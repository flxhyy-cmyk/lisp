;;; ============================================================
;;; 属性块-柜增减.lsp                              命令：GKZ
;;; ------------------------------------------------------------
;;; 干什么用：
;;;   在一排柜里「加一台 / 减一台」，右侧的块自动让位。
;;;
;;;   增 (A)：
;;;     1) 选定柜紧右边插入一台新柜（底宽 35 的同型号块定义）；
;;;     2) 柜型 / 柜尺寸 / 柜用途 照抄选定柜，柜号 +1，其余属性留空；
;;;     3) 柜顶用「占位块.dwg」模板补盖一枚空白占位块（顶离柜顶 3）；
;;;     4) 右侧的块整体右移 35 腾位（表头块跟着走，占位块随宿主）。
;;;
;;;   减 (D)：
;;;     1) 删掉选定柜，连同它头顶那枚块；
;;;     2) 右侧的块整体左移「被删柜的当前宽度」。
;;;
;;; 柜号联动（增 / 减 同一个口径）：
;;;   只动「被移动的那一批柜」，并且同时满足 ——
;;;     柜号前缀与选定柜一致 + 有末尾数字 + 序号 >= 选定序号 + 1
;;;   才 +1 / -1；前缀不一致、或没有末尾数字的，一律不动。
;;;   位数保持：001 -> 002，009 -> 010。
;;;   例：选定 D1 插入 -> 新柜 D2，原 D2/D3 变 D3/D4；G1、BYQ 不动。
;;;
;;; 为什么必须用「改动前的布局快照」：
;;;   判「谁在本块右侧」要看原始布局。改完一个块再判下一个，
;;;   位置已经变了，会越推越乱（这条在「柜宽缩放」里踩过）。
;;;
;;; 安全：
;;;   * 整次改动包在一个 UNDO 组里 —— 不满意一句 U 全撤回；
;;;   * 一排只剩一台柜时拒绝删；
;;;   * 图框永不参与位移（判据: 谁的包围盒把柜块整个包住）；
;;;   * 校验全部前置，校验不过绝不动图。
;;;
;;; 依赖：无（自包含）。让位逻辑 / 判定口径与
;;;       「属性块-柜宽缩放.lsp」的 gkw:* 同源，改一处请同步另一处。
;;;
;;; 编码：ANSI/GBK 无 BOM
;;; ============================================================
(vl-load-com)

;; ------------------------------------------------------------
;; 可调参数
;; ------------------------------------------------------------
(setq *gkz:cab-prefix*  "开关柜")     ; 柜块名里必须含这个（挡掉别的块）
(setq *gkz:new-w*       35.0)         ; 新柜默认底宽
(setq *gkz:tag-no*      "柜号")        ; 柜号属性标签
(setq *gkz:copy-tags*   '("柜型" "柜尺寸" "柜用途"))  ; 照抄的属性
(setq *gkz:ph-block*    "占位块")      ; 占位块定义名
(setq *gkz:ph-dwg*      "D:\\kk三部曲\\148.CAD插件研究\\占位块.dwg")
(setq *gkz:ph-gap*      3.0)          ; 占位块顶离柜顶的净距
(setq *gkz:frame-lyr*   "BOARD")      ; 图框图层
(setq *gkz:frame-w*     420.0)        ; A3 图框宽
(setq *gkz:frame-h*     297.0)        ; A3 图框高
(setq *gkz:max-per-frame* 9)          ; 一框最多几台柜（只用于报警告）

;; ------------------------------------------------------------
;; 基础小工具
;; ------------------------------------------------------------
(defun gkz:trim (s)
  (if s (vl-string-trim " \t" s) ""))

;; 实体包围盒 -> (最小点 最大点)；读不到就按「插入点 + 定义几何 x 缩放」推算
;;   实测图里那几枚 BLK_* 方案图块调 GetBoundingBox 会直接报 eInvalidInput，
;;   不加保护整条命令当场崩。
(defun gkz:bb (e / o r mn mx bn b ee de ip sc dx0 dy0 dx1 dy1 x y)
  (setq o (vlax-ename->vla-object e))
  (setq r (vl-catch-all-apply 'vla-GetBoundingBox (list o 'mn 'mx)))
  (if (not (vl-catch-all-error-p r))
    (list (vlax-safearray->list mn) (vlax-safearray->list mx))
    (progn
      (setq bn (cdr (assoc 2 (entget e))) ip (cdr (assoc 10 (entget e))))
      (setq b (tblobjname "BLOCK" bn) ee (if b (entnext b) nil))
      (setq dx0 1e9 dy0 1e9 dx1 -1e9 dy1 -1e9)
      (while (and ee (/= "ENDBLK" (cdr (assoc 0 (setq de (entget ee))))))
        (foreach pr de
          (if (and (member (car pr) '(10 11)) (listp (cdr pr)))
            (progn
              (setq x (car (cdr pr)) y (cadr (cdr pr)))
              (if (and (numberp x) (numberp y))
                (progn
                  (if (< x dx0) (setq dx0 x)) (if (> x dx1) (setq dx1 x))
                  (if (< y dy0) (setq dy0 y)) (if (> y dy1) (setq dy1 y))
                )
              )
            )
          )
        )
        (setq ee (entnext ee))
      )
      (if (> dx0 dx1)
        nil
        (progn
          (setq sc (cdr (assoc 41 (entget e))) sc (if (and sc (> sc 0.0)) sc 1.0))
          (list (list (+ (car ip) (* dx0 sc)) (+ (cadr ip) (* dy0 sc)) 0.0)
                (list (+ (car ip) (* dx1 sc)) (+ (cadr ip) (* dy1 sc)) 0.0))
        )
      )
    )
  )
)

;; 块定义几何 -> (最小x 最小y 宽 高)；找不到返回 nil
(defun gkz:defbox (bn / b e ed dx0 dy0 dx1 dy1 x y)
  (setq b (tblobjname "BLOCK" bn) e (if b (entnext b) nil))
  (setq dx0 1e9 dy0 1e9 dx1 -1e9 dy1 -1e9)
  (while (and e (/= "ENDBLK" (cdr (assoc 0 (setq ed (entget e))))))
    (foreach pr ed
      (if (and (member (car pr) '(10 11)) (listp (cdr pr)))
        (progn
          (setq x (car (cdr pr)) y (cadr (cdr pr)))
          (if (and (numberp x) (numberp y))
            (progn
              (if (< x dx0) (setq dx0 x)) (if (> x dx1) (setq dx1 x))
              (if (< y dy0) (setq dy0 y)) (if (> y dy1) (setq dy1 y))
            )
          )
        )
      )
    )
    (setq e (entnext e))
  )
  (if (> dx0 dx1)
    nil
    (list dx0 dy0 (- dx1 dx0) (- dy1 dy0))
  )
)

;; 块定义顶层有没有 ATTDEF —— 也就是「是不是属性块」
(defun gkz:attrp (bn / b e ed r)
  (setq b (tblobjname "BLOCK" bn) e (if b (entnext b) nil) r nil)
  (while (and e (null r) (/= "ENDBLK" (cdr (assoc 0 (setq ed (entget e))))))
    (if (= "ATTDEF" (cdr (assoc 0 ed))) (setq r T))
    (setq e (entnext e))
  )
  r
)

;; 谁的包围盒把柜块整个包住 —— 那就是图框 / 图纸边框，永不位移
(defun gkz:frame (mn mx mn2 mx2)
  (and (<= (car mn2) (+ (car mn) 0.01))
       (>= (car mx2) (- (car mx) 0.01))
       (<= (cadr mn2) (+ (cadr mn) 0.01))
       (>= (cadr mx2) (- (cadr mx) 0.01)))
)

;; 给实体数据里所有组码 10 / 11 的 x 统一加 dd
(defun gkz:addx (ed dd / out x p)
  (setq out nil)
  (foreach x ed
    (if (or (= 10 (car x)) (= 11 (car x)))
      (progn
        (setq p (cdr x))
        (setq out (cons (cons (car x) (cons (+ (car p) dd) (cdr p))) out))
      )
      (setq out (cons x out))
    )
  )
  (reverse out)
)

;; 平移一个块参照（连同它的属性）
(defun gkz:move (e dd / ed p sub)
  (setq ed (entget e) p (cdr (assoc 10 ed)))
  (entmod (subst (cons 10 (cons (+ (car p) dd) (cdr p))) (assoc 10 ed) ed))
  (setq sub (entnext e))
  (while (and sub (= "ATTRIB" (cdr (assoc 0 (setq ed (entget sub))))))
    (entmod (gkz:addx ed dd))
    (setq sub (entnext sub))
  )
)

;; 平移一个块参照（x / y 双向，属性一起走）
(defun gkz:movexy (e dx dy / ed p sub sub2)
  (setq ed (entget e) p (cdr (assoc 10 ed)))
  (entmod (subst (cons 10 (list (+ (car p) dx) (+ (cadr p) dy) (caddr p)))
                 (assoc 10 ed) ed))
  (setq sub (entnext e))
  (while (and sub (= "ATTRIB" (cdr (assoc 0 (setq ed (entget sub))))))
    (setq sub2 nil)
    (foreach pr ed
      (if (and (member (car pr) '(10 11)) (listp (cdr pr)))
        (setq sub2 (cons (cons (car pr) (list (+ (car (cdr pr)) dx)
                                             (+ (cadr (cdr pr)) dy)
                                             (caddr (cdr pr)))) sub2))
        (setq sub2 (cons pr sub2))
      )
    )
    (entmod (reverse sub2))
    (setq sub (entnext sub))
  )
)

;; 取块参照上的属性值（按 TAG）
(defun gkz:tag (e tag / sub ed out)
  (setq sub (entnext e) out nil)
  (while (and sub (= "ATTRIB" (cdr (assoc 0 (setq ed (entget sub))))))
    (if (= tag (cdr (assoc 2 ed))) (setq out (cdr (assoc 1 ed))))
    (setq sub (entnext sub))
  )
  out
)

;; 写块参照上的属性值（按 TAG）
(defun gkz:setatt (e tag val / sub ed)
  (setq sub (entnext e))
  (while (and sub (= "ATTRIB" (cdr (assoc 0 (setq ed (entget sub))))))
    (if (= tag (cdr (assoc 2 ed)))
      (entmod (subst (cons 1 val) (assoc 1 ed) ed))
    )
    (setq sub (entnext sub))
  )
  (princ)
)

;; 列出块参照上的全部属性标签
(defun gkz:tags (e / sub ed out)
  (setq sub (entnext e) out nil)
  (while (and sub (= "ATTRIB" (cdr (assoc 0 (setq ed (entget sub))))))
    (setq out (cons (cdr (assoc 2 ed)) out))
    (setq sub (entnext sub))
  )
  (reverse out)
)

;; 从块定义里找某 TAG 的属性定义（ATTDEF）
(defun gkz:attdef (bn tag / b e ed res)
  (setq b (tblobjname "BLOCK" bn) e (if b (entnext b) nil) res nil)
  (while (and e (null res) (/= "ENDBLK" (cdr (assoc 0 (setq ed (entget e))))))
    (if (and (= "ATTDEF" (cdr (assoc 0 ed))) (= tag (cdr (assoc 2 ed))))
      (setq res ed)
    )
    (setq e (entnext e))
  )
  res
)

;; 按块定义把参照的属性位置对齐回去（替代不可靠的 -ATTSYNC）
;;   前提：参照无旋转、无缩放
(defun gkz:relayout (e / ed bn ins sub ed2 ad p10 p11 out)
  (setq ed (entget e) bn (cdr (assoc 2 ed)) ins (cdr (assoc 10 ed)))
  (setq sub (entnext e) out 0)
  (while (and sub (= "ATTRIB" (cdr (assoc 0 (setq ed2 (entget sub))))))
    (setq ad (gkz:attdef bn (cdr (assoc 2 ed2))))
    (if ad
      (progn
        (setq p10 (cdr (assoc 10 ad)) p11 (cdr (assoc 11 ad)))
        (setq ed2 (subst (cons 10 (list (+ (car ins) (car p10))
                                        (+ (cadr ins) (cadr p10))
                                        (+ (caddr ins) (caddr p10))))
                         (assoc 10 ed2) ed2))
        (if (assoc 11 ed2)
          (setq ed2 (subst (cons 11 (list (+ (car ins) (car p11))
                                          (+ (cadr ins) (cadr p11))
                                          (+ (caddr ins) (caddr p11))))
                           (assoc 11 ed2) ed2))
        )
        (entmod ed2)
        (setq out (1+ out))
      )
    )
    (setq sub (entnext sub))
  )
  out
)

;; ------------------------------------------------------------
;; 柜号解析 / 递增
;;   末尾连续数字 = 序号，其前 = 前缀。中文按字节取前缀不会切错位：
;;   数字是 ASCII，扫描停下的位置必然落在字符边界上。
;; ------------------------------------------------------------
(defun gkz:serial (s / i n)
  (setq s (gkz:trim s))
  (if (= s "")
    nil
    (progn
      (setq i (strlen s) n 0)
      (while (and (> i 0) (wcmatch (substr s i 1) "#"))
        (setq i (1- i) n (1+ n))
      )
      (if (= n 0)
        nil
        (list (substr s 1 i) (atoi (substr s (1+ i))) n)
      )
    )
  )
)

;; 补零到 n 位（超了就不补）
(defun gkz:pad (v n / s)
  (setq s (itoa v))
  (while (< (strlen s) n) (setq s (strcat "0" s)))
  s
)

;; 候选柜号该不该 +1：前缀一致 + 序号 >= 选定序号 + 1 -> 返回新柜号，否则 nil
(defun gkz:up-no (sel cno / tp)
  (setq tp (gkz:serial cno))
  (if (and tp (= (car tp) (car sel)) (>= (cadr tp) (1+ (cadr sel))))
    (strcat (car tp) (gkz:pad (1+ (cadr tp)) (caddr tp)))
    nil
  )
)

;; 候选柜号该不该 -1
(defun gkz:dn-no (sel cno / tp)
  (setq tp (gkz:serial cno))
  (if (and tp (= (car tp) (car sel)) (>= (cadr tp) (1+ (cadr sel))))
    (strcat (car tp) (gkz:pad (1- (cadr tp)) (caddr tp)))
    nil
  )
)

;; ------------------------------------------------------------
;; 快照 / 占位块 / 让位
;; ------------------------------------------------------------
;; 开工前把所有块参照的包围盒拍个快照 (图元名 最小点 最大点)
(defun gkz:snapshot ( / ss i n lst e bbx)
  (setq ss (ssget "X" '((0 . "INSERT"))) i 0 n (if ss (sslength ss) 0) lst nil)
  (while (< i n)
    (setq e (ssname ss i) bbx (gkz:bb e))
    (if bbx (setq lst (cons (list e (car bbx) (cadr bbx)) lst)))
    (setq i (1+ i))
  )
  lst
)

;; 依据快照，找出「被该柜块罩住」的纯几何块参照（占位块 / 方案图块）
;;   判据：① 不是属性块；② 不是把柜块整个包住的外框；
;;         ③ 包围盒中心落在柜块包围盒内；④ 高度 >= 柜高 10%
;;   ④ 只看纵向尺寸：横向会被柜宽缩放影响，缩回时就认不出了。
(defun gkz:holders (e snap / itm mn mx res o2 mn2 mx2)
  (setq itm (assoc e snap) res nil)
  (if itm
    (progn
      (setq mn (cadr itm) mx (caddr itm))
      (foreach o2 snap
        (if (not (eq (car o2) e))
          (progn
            (setq mn2 (cadr o2) mx2 (caddr o2))
            (if (and (not (gkz:attrp (cdr (assoc 2 (entget (car o2))))))
                     (not (gkz:frame mn mx mn2 mx2))
                     (>= (/ (+ (car mn2) (car mx2)) 2.0) (- (car mn) 1e-6))
                     (<= (/ (+ (car mn2) (car mx2)) 2.0) (+ (car mx) 1e-6))
                     (>= (/ (+ (cadr mn2) (cadr mx2)) 2.0) (- (cadr mn) 1e-6))
                     (<= (/ (+ (cadr mn2) (cadr mx2)) 2.0) (+ (cadr mx) 1e-6))
                     (>= (- (cadr mx2) (cadr mn2)) (* 0.1 (- (cadr mx) (cadr mn)))))
              (setq res (cons (car o2) res))
            )
          )
        )
      )
    )
  )
  res
)

;; 全图所有「已被某个属性块罩住」的占位块 —— 推块时要跳过它们
;;   否则会被推两遍：一遍作为宿主的随从，一遍作为「自己也在右边」的独立块
(defun gkz:allholders (snap / res itm)
  (setq res nil)
  (foreach itm snap
    (if (gkz:attrp (cdr (assoc 2 (entget (car itm)))))
      (foreach h (gkz:holders (car itm) snap)
        (if (not (member h res)) (setq res (cons h res)))
      )
    )
  )
  res
)

;; 平移一个柜块，连它罩住的占位块一起走
(defun gkz:move2 (e dd snap / cnt)
  (gkz:move e dd)
  (setq cnt 1)
  (foreach h (gkz:holders e snap)
    (gkz:move h dd)
    (setq cnt (1+ cnt))
  )
  cnt
)

;; 把原始布局里位于该块右侧、且同排的块，按当前位置平移 dd（dd 可负）
;;   外框永不被推；占位块不单独推（跟着宿主走）
(defun gkz:push (e dd snap hs / itm mn mx o2 mn2 mx2 cnt)
  (setq itm (assoc e snap) cnt 0)
  (if itm
    (progn
      (setq mn (cadr itm) mx (caddr itm))
      (foreach o2 snap
        (if (and (not (eq (car o2) e)) (not (member (car o2) hs)))
          (progn
            (setq mn2 (cadr o2) mx2 (caddr o2))
            (if (and (not (gkz:frame mn mx mn2 mx2))
                     (>= (car mn2) (- (car mx) 0.01))
                     (> (- (min (cadr mx) (cadr mx2))
                           (max (cadr mn) (cadr mn2))) 0.01))
              (progn (gkz:move2 (car o2) dd snap) (setq cnt (1+ cnt)))
            )
          )
        )
      )
    )
  )
  cnt
)

;; 同排的柜（y 区间有重叠 + 有柜号属性）—— 用于「只剩一台不许删」的判定
(defun gkz:rowcabs (e snap / itm mn mx res o2 mn2 mx2 cno)
  (setq itm (assoc e snap) res nil)
  (if itm
    (progn
      (setq mn (cadr itm) mx (caddr itm))
      (foreach o2 snap
        (setq cno (gkz:tag (car o2) *gkz:tag-no*))
        (if (and cno (/= (gkz:trim cno) ""))
          (progn
            (setq mn2 (cadr o2) mx2 (caddr o2))
            (if (> (- (min (cadr mx) (cadr mx2)) (max (cadr mn) (cadr mn2))) 0.01)
              (setq res (cons (car o2) res))
            )
          )
        )
      )
    )
  )
  res
)

;; ------------------------------------------------------------
;; 建新柜（复制选定柜 -> 换定义 -> 挪位置 -> 重排属性 -> 填值）
;; ------------------------------------------------------------
(defun gkz:copycab (src newbn x y / o r newo)
  (setq o (vlax-ename->vla-object src))
  (setq r (vl-catch-all-apply 'vla-Copy (list o)))
  (if (or (null r) (vl-catch-all-error-p r))
    nil
    (progn
      (setq newo r)
      (vl-catch-all-apply 'vla-put-Name (list newo newbn))
      (vl-catch-all-apply 'vla-put-InsertionPoint
                          (list newo (vlax-3d-point x y 0.0)))
      (vlax-vla-object->ename newo)
    )
  )
)

;; 找 35 宽的同型号块定义
;;   ① 块名去掉 "-柜号" 后缀 = 型号基名；
;;   ② 基名本身若正好 35 宽 -> 用它；
;;   ③ 否则在图上已有的块定义里找「名字与基名同型 + 35 宽」的那一个。
(defun gkz:base35 (selbn cno / base n suff hit ss i bn g0 w)
  (setq base selbn)
  (if (and cno (/= cno ""))
    (progn
      (setq suff (strcat "-" cno) n (strlen suff))
      (if (and (> (strlen selbn) n)
               (= (substr selbn (- (strlen selbn) n -1)) suff))
        (setq base (substr selbn 1 (- (strlen selbn) n)))
      )
    )
  )
  (setq hit nil)
  (setq g0 (gkz:defbox base))
  (if (and g0 (equal (caddr g0) *gkz:new-w* 0.5))
    (setq hit base)
  )
  (if (null hit)
    (progn
      (setq ss (ssget "_X" '((0 . "INSERT"))) i 0)
      (while (and (null hit) ss (< i (sslength ss)))
        (setq bn (cdr (assoc 2 (entget (ssname ss i)))) i (1+ i))
        (if (and (or (= bn base)
                     (wcmatch bn (strcat base "*"))
                     (wcmatch base (strcat bn "*")))
                 (gkz:attrp bn))
          (progn
            (setq g0 (gkz:defbox bn))
            (if (and g0 (equal (caddr g0) *gkz:new-w* 0.5))
              (setq hit bn)
            )
          )
        )
      )
    )
  )
  (list hit base)
)

;; 在新柜顶上盖一枚空白占位块
;;   模板实测口径（占位块.dwg）：几何 x0=174.481 y0=119.225 w=35 h=51.582，
;;   基点 (0,0) 不在几何角点上 —— 所以一律「插完读包围盒、反推挪正」：
;;       占位块几何左下 x = 柜几何左 x        占位块几何顶 y = 柜顶 - 3
;;   图上有「占位块」定义就直接插；没有就用 _.-INSERT 从模板 dwg 导入一次
;;   （导入的那一枚就是新柜要的那一枚，不再另外建）。
;;   注：vla-Import 在本机报「实参太少」，走命令行 _.-INSERT 加引号吃中文路径。
(defun gkz:stamp (cabbe / doc sp x y2 y r ph bb dx dy bn oldos)
  (setq x (car (car cabbe)))
  (setq y2 (- (cadr (cadr cabbe)) *gkz:ph-gap*))
  (setq ph nil)
  (if (tblsearch "BLOCK" *gkz:ph-block*)
    (progn
      (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
      (setq sp (vla-get-ModelSpace doc))
      (setq r (vl-catch-all-apply 'vla-InsertBlock
                                  (list sp (vlax-3d-point x y2 0.0) *gkz:ph-block*
                                        1.0 1.0 1.0 0.0)))
      (if (not (or (null r) (vl-catch-all-error-p r)))
        (setq ph (vlax-vla-object->ename r))
      )
    )
    (if (findfile *gkz:ph-dwg*)
      (progn
        (setq oldos (getvar "OSMODE"))
        (setvar "OSMODE" 0)
        (command "_.-INSERT" (strcat "\"" *gkz:ph-dwg* "\"")
                 (list x y2 0.0) 1.0 1.0 0.0)
        (setvar "OSMODE" oldos)
        (setq ph (entlast))
        (if (and ph (= "INSERT" (cdr (assoc 0 (entget ph)))))
          (progn
            (setq bn (cdr (assoc 2 (entget ph))))
            (if (and bn (/= bn *gkz:ph-block*)) (setq *gkz:ph-block* bn))
          )
          (setq ph nil)
        )
      )
      (princ (strcat "\n[GKZ] 找不到占位块模板 " *gkz:ph-dwg* "，本次不盖章。"))
    )
  )
  (if ph
    (progn
      (setq bb (gkz:bb ph))
      (if bb
        (progn
          (setq dx (- x (car (car bb))) dy (- y2 (cadr (cadr bb))))
          (if (or (> (abs dx) 1e-6) (> (abs dy) 1e-6))
            (gkz:movexy ph dx dy)
          )
          ph
        )
      )
    )
    nil
  )
)

;; ------------------------------------------------------------
;; 图框体检（只数、只报警告，不动图框）
;; ------------------------------------------------------------
(defun gkz:frame-of (selbb / ss i en bb fbb w h)
  (setq fbb nil)
  (if (setq ss (ssget "_X" (list '(0 . "INSERT") (cons 8 *gkz:frame-lyr*))))
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq en (ssname ss i) i (1+ i) bb (gkz:bb en))
        (if bb
          (progn
            (setq w (- (car (cadr bb)) (car (car bb)))
                  h (- (cadr (cadr bb)) (cadr (car bb))))
            (if (and (equal w *gkz:frame-w* 5.0) (equal h *gkz:frame-h* 5.0))
              (if (and (>= (car (car selbb)) (- (car (car bb)) 1.0))
                       (<= (car (cadr selbb)) (+ (car (cadr bb)) 1.0))
                       (>= (cadr (car selbb)) (- (cadr (car bb)) 1.0)))
                (setq fbb bb)
              )
            )
          )
        )
      )
    )
  )
  fbb
)

(defun gkz:frame-warn (fbb / ss i en bb n mx cno)
  (if fbb
    (progn
      (setq n 0 mx -1e9)
      (if (setq ss (ssget "_X" '((0 . "INSERT"))))
        (progn
          (setq i 0)
          (while (< i (sslength ss))
            (setq en (ssname ss i) i (1+ i))
            (setq cno (gkz:tag en *gkz:tag-no*))
            (if (and cno (/= (gkz:trim cno) ""))
              (progn
                (setq bb (gkz:bb en))
                (if (and bb
                         (>= (car (car bb)) (- (car (car fbb)) 1.0))
                         (<= (car (cadr bb)) (+ (car (cadr fbb)) 1.0)))
                  (progn
                    (setq n (1+ n))
                    (if (> (car (cadr bb)) mx) (setq mx (car (cadr bb))))
                  )
                )
              )
            )
          )
        )
      )
      (if (> n *gkz:max-per-frame*)
        (princ (strcat "\n[警告] 本框已有 " (itoa n) " 台柜，超出一框 "
                       (itoa *gkz:max-per-frame*) " 台，请自行分框。"))
      )
      (if (> mx (- (car (cadr fbb)) 5.0))
        (princ (strcat "\n[警告] 内容右边界 " (rtos mx 2 2)
                       " 已顶到 / 超出图框右边界 " (rtos (car (cadr fbb)) 2 2)
                       "，请自行调框。"))
      )
    )
  )
  (princ)
)

;; ------------------------------------------------------------
;; 主逻辑：增
;; ------------------------------------------------------------
(defun gkz:add (e / selbn cno sel res newbn base g0 cls selbb selins insx insy
                   snap hs newe n w ok warns)
  (setq selbn (cdr (assoc 2 (entget e))))
  (setq cno (gkz:trim (gkz:tag e *gkz:tag-no*)))
  (setq sel (gkz:serial cno))
  (if (null sel)
    (progn
      (princ (strcat "\n[停止] 选定柜的柜号「" cno "」没有末尾数字，算不出新柜号。"))
      nil
    )
    (progn
      (setq res (gkz:base35 selbn cno))
      (setq newbn (car res) base (cadr res))
      (if (null newbn)
        (progn
          (princ (strcat "\n[停止] 找不到 35 宽的同型号柜块定义（型号基名「" base
                         "」）。请先在图里放一台该型号的底宽柜，或把它的定义加进来。"))
          nil
        )
        (progn
          (setq g0 (gkz:defbox newbn))
          (setq selbb (gkz:bb e))
          (if (or (null g0) (null selbb))
            (progn (princ "\n[停止] 读不到柜块几何，命令中止。") nil)
            (progn
              ;; ---- 校验通过，开始改图（整次一个 UNDO 组）----
              (command "_.UNDO" "_BE")
              (setq snap (gkz:snapshot))
              (setq hs (gkz:allholders snap))

              ;; 1) 先推右侧腾位（用改动前的快照）
              (setq n (gkz:push e *gkz:new-w* snap hs))

              ;; 2) 建新柜：几何左边界贴选定柜右边界，底边对齐
              ;;    用「插入点 + 定义几何」推，不靠包围盒 —— 属性文字外溢也不会算歪
              (setq cls (gkz:defbox selbn))
              (setq selins (cdr (assoc 10 (entget e))))
              (if cls
                (progn
                  (setq insx (- (+ (car selins) (car cls) (caddr cls)) (car g0)))
                  (setq insy (- (+ (cadr selins) (cadr cls)) (cadr g0)))
                )
                (progn
                  (setq insx (- (car (cadr selbb)) (car g0)))
                  (setq insy (- (cadr (car selbb)) (cadr g0)))
                )
              )
              (setq newe (gkz:copycab e newbn insx insy))
              (if (null newe)
                (progn
                  (princ "\n[出错] 复制柜块失败，本次改动请按 U 撤回。")
                  (command "_.UNDO" "_E")
                  nil
                )
                (progn
                  ;; 3) 属性按新定义重排 + 填值
                  (gkz:relayout newe)
                  (if (null (gkz:tags newe))
                    (princ "\n[GKZ] 注意：新柜上没读到属性（属性没跟着复制过来？）请手工检查。")
                  )
                  (foreach tg (gkz:tags newe)
                    (cond
                      ((= tg *gkz:tag-no*)
                       (gkz:setatt newe tg (strcat (car sel)
                                                  (gkz:pad (1+ (cadr sel)) (caddr sel)))))
                      ((member tg *gkz:copy-tags*) nil)
                      (t (gkz:setatt newe tg ""))
                    )
                  )

                  ;; 4) 柜顶补盖一枚占位块
                  (gkz:stamp (gkz:bb newe))

                  ;; 5) 右侧那批柜的柜号 +1（按改动前的快照判位置）
                  (setq ok 0)
                  (foreach itm snap
                    (if (and (not (eq (car itm) e))
                             (>= (car (cadr itm)) (- (car (cadr selbb)) 0.01))
                             (> (- (min (cadr (cadr selbb)) (cadr (caddr itm)))
                                   (max (cadr (car selbb)) (cadr (cadr itm)))) 0.01))
                      (progn
                        (setq w (gkz:up-no sel (gkz:tag (car itm) *gkz:tag-no*)))
                        (if w
                          (progn (gkz:setatt (car itm) *gkz:tag-no* w)
                                 (setq ok (1+ ok)))
                        )
                      )
                    )
                  )

                  (command "_.UNDO" "_E")
                  (princ (strcat "\n[GKZ] 已加柜：新柜号 "
                                 (strcat (car sel) (gkz:pad (1+ (cadr sel)) (caddr sel)))
                                 "，定义「" newbn "」，右侧 " (itoa n) " 个块右移 "
                                 (rtos *gkz:new-w* 2 2) "，联动改号 " (itoa ok) " 台。"))
                  (gkz:frame-warn (gkz:frame-of (gkz:bb newe)))
                  (princ "\n（不满意输入 U 可整次撤回）")
                  T
                )
              )
            )
          )
        )
      )
    )
  )
)

;; ------------------------------------------------------------
;; 主逻辑：减
;; ------------------------------------------------------------
(defun gkz:del (e / cno sel selbb w snap hs n ok fbb i)
  (setq cno (gkz:trim (gkz:tag e *gkz:tag-no*)))
  (setq sel (gkz:serial cno))
  (setq selbb (gkz:bb e))
  (if (null selbb)
    (progn (princ "\n[停止] 读不到柜块几何，命令中止。") nil)
    (progn
      (setq snap (gkz:snapshot))
      (if (<= (length (gkz:rowcabs e snap)) 1)
        (progn (princ "\n[停止] 这一排只剩一台柜，不删。") nil)
        (progn
          (setq fbb (gkz:frame-of selbb))
          (setq w (- (car (cadr selbb)) (car (car selbb))))   ; 被删柜当前宽度

          (command "_.UNDO" "_BE")

          ;; 1) 右侧整体左移「被删柜宽度」
          (setq hs (gkz:allholders snap))
          (setq n (gkz:push e (- w) snap hs))

          ;; 2) 右侧那批柜的柜号 -1（按改动前的快照判位置）
          (setq ok 0)
          (if sel
            (foreach itm snap
              (if (and (not (eq (car itm) e))
                       (>= (car (cadr itm)) (- (car (cadr selbb)) 0.01))
                       (> (- (min (cadr (cadr selbb)) (cadr (caddr itm)))
                             (max (cadr (car selbb)) (cadr (cadr itm)))) 0.01))
                (progn
                  (setq i (gkz:dn-no sel (gkz:tag (car itm) *gkz:tag-no*)))
                  (if i
                    (progn (gkz:setatt (car itm) *gkz:tag-no* i)
                           (setq ok (1+ ok)))
                  )
                )
              )
            )
          )

          ;; 3) 删柜 —— 连它头顶那枚块一起删，不留孤儿
          (foreach h (gkz:holders e snap) (entdel h))
          (entdel e)

          (command "_.UNDO" "_E")
          (princ (strcat "\n[GKZ] 已删柜：柜号「" cno "」，宽 "
                         (rtos w 2 2) "，右侧 " (itoa n) " 个块左移，联动改号 "
                         (itoa ok) " 台。"))
          (if fbb (gkz:frame-warn fbb))
          (princ "\n（不满意输入 U 可整次撤回）")
          T
        )
      )
    )
  )
)

;; ------------------------------------------------------------
;; 命令外壳
;; ------------------------------------------------------------
(defun c:GKZ ( / e bn cno k oldecho r)
  (vl-load-com)
  (princ "\n选择要增减的柜属性块: ")
  (setq e (car (entsel)))
  (cond
    ((null e)
     (princ "\n没选到对象。"))
    ((/= "INSERT" (cdr (assoc 0 (entget e))))
     (princ "\n[停止] 选的不是块参照。"))
    ((null (gkz:tag e *gkz:tag-no*))
     (princ (strcat "\n[停止] 这个块上没有「" *gkz:tag-no*
                    "」属性 —— 大概是选到表头块或占位块了。")))
    ((not (wcmatch (setq bn (cdr (assoc 2 (entget e))))
                   (strcat "*" *gkz:cab-prefix* "*")))
     (princ (strcat "\n[停止] 块名「" bn "」看着不是柜属性块。")))
    (T
     (setq cno (gkz:trim (gkz:tag e *gkz:tag-no*)))
     (initget "A D")
     (setq k (getkword "\n[增(A)/减(D)] <增>: "))
     (if (null k) (setq k "A"))
     (setq oldecho (getvar "CMDECHO"))
     (setvar "CMDECHO" 0)
     (if (= k "A")
       (gkz:add e)
       (gkz:del e)
     )
     (setvar "CMDECHO" oldecho)
    )
  )
  (princ)
)

(princ "\n[属性块-柜增减] 已加载，命令：GKZ")
(princ)
