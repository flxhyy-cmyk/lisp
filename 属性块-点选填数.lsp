;;; ============================================================
;;; 属性块-点选填数.lsp                                      命令：DTF
;;; ------------------------------------------------------------
;;; 作用
;;;   点图上某个位置，程序自己认出这是「哪台柜 / 哪个槽 / 哪枚表头」，
;;;   弹出对话框让人填数，按「写入」落图。给人在图上零散补数用 ——
;;;   不用写清单，不用记槽号。
;;;
;;; 只管两种属性块（不管图签，也不管普通图元）
;;;   ① 柜属性块（认「柜号」标签）
;;;        · 槽位区 -> 该槽（名称 / 型号 / 规格 / 数量，按块里实际有的属性给）
;;;            该槽表头已有值 -> 表头值只读显示，只填本柜
;;;            该槽表头是空的 -> 名称 / 型号 可填，写入时同时落到表头块
;;;        · 柜级区 -> 柜号 / 柜用途 / 柜型 / 柜尺寸 / 容量功率 /
;;;                    分支电流 / 备注
;;;   ② 表头属性块（认「主母线规格」标签）
;;;        · 槽位区 -> 该槽 名称 / 型号
;;;        · 信息区 -> 主母线规格 / 电压等级
;;;
;;; 认块不认块名
;;;   GKC / GGH 会把块名滚成 柜块模版-排(1)-AH01 这种，所以一律靠标签认。
;;;
;;; 位置怎么认（两条独立判据，不用包围盒）
;;;   ① 先按「块定义几何 x 区间 + 插入点」定在哪台柜 / 哪枚表头。
;;;      不用 vla-GetBoundingBox：属性文字超宽会把包围盒撑大，隔壁
;;;      那台柜会被串进来。
;;;   ② 再按「点到该参照每个属性的 y 距离」定在哪个槽 —— 取最近的那个
;;;      属性所属的槽；非槽位属性（柜号 / 主母线规格 …）归「组 0」。
;;;      最近距离超过 9 就当成没点在块上（约 1.5 倍槽行距 6）。
;;;
;;; 表头怎么配
;;;   柜块槽位填数时，表头取「插入点 x 离点击点最近」的那一枚 ——
;;;   KYN28 两框、箱变两段都自然对上（段 1 表头在左、段 2 在右）。
;;;
;;; 字段不是写死的
;;;   一个槽的输入框按「该槽在块里实际有哪些属性」生成：柜块通常
;;;   元件NN规格 / 元件NN数量，表头块是 元件NN名称 / 元件NN型号。
;;;   规格类多行标签（元件NN规格1..M，XBT 加过行高留下的）合成一个
;;;   输入框，写入时按 PLT 同一套宽度规则拆行落位。
;;;
;;; 不存盘
;;;   与 PLT / GGH 同规矩：只改内存，存盘 / 撤销归用户。
;;;
;;; 界面（DCL）
;;;   每次弹窗前把 DCL 写一份到 %TEMP%\dtf_dlg.dcl（字段数随位置变，
;;;   所以必须现生成），不改工程目录里的任何文件。
;;;
;;; 编码：ANSI/GBK 无 BOM
;;; 依赖：属性块-批量填数.lsp（plt: 那套内核），本文件自己 load
;;; ============================================================
(vl-load-com)
(if (null SEP) (setq SEP (chr 10)))

;; ---------------- 依赖（PLT 内核） ----------------
(setq dtf:dep "D:/kk三部曲/148.CAD插件研究/lisp/属性块-批量填数.lsp")

;; 内核不在就 load 一次。返回 T / nil。
(defun dtf:ensure ( / p)
  (if (atoms-family 1 '("PLT:PUT"))
    T
    (progn
      (setq p (vl-catch-all-apply 'load (list dtf:dep)))
      (if (and (not (vl-catch-all-error-p p)) (atoms-family 1 '("PLT:PUT")))
        T
        nil)
    )
  )
)

;; ================================================================
;; 一、小工具
;; ================================================================

;; nil -> ""（edit_box 只吃字符串）
(defun dtf:nz (s)
  (if s s "")
)

;; 从属性标签解析槽号：元件NNxxx -> NN；非槽位标签（柜号 / 主母线规格 …）返回 nil
;;   AutoLISP 的 strlen / substr 按字节算，「元件」= 4 字节，槽号固定 2 字节数字
(defun dtf:slotno (tg / s c1 c2)
  (if (and tg (>= (strlen tg) 6) (= (substr tg 1 4) *plt:prefix*))
    (progn
      (setq s (substr tg 5 2))
      (setq c1 (ascii (substr s 1 1)) c2 (ascii (substr s 2 1)))
      (if (and (>= c1 48) (<= c1 57) (>= c2 48) (<= c2 57))
        (atoi s)
        nil)
    )
    nil)
)

;; 标签后半段的字段名：元件04规格1 -> 规格1（「元件04」= 6 字节）
(defun dtf:suffix (tg)
  (substr tg 7)
)

;; 是不是「规格类」标签（元件NN规格 / 元件NN规格1..M）
(defun dtf:specp (tg / k)
  (setq k (dtf:slotno tg))
  (if (and k (vl-string-search "规格" tg)) T nil)
)

;; 移除表里第一个等于 x 的元素（vl-remove 会删掉全部，这里只要一个）
(defun dtf:rem1 (x lst / out hit)
  (setq out nil hit nil)
  (foreach e lst
    (if (and (null hit) (equal e x))
      (setq hit T)
      (setq out (cons e out))
    )
  )
  (reverse out)
)

;; 字段排序权重：名称 型号 规格 数量，其余排后面
(defun dtf:ordf (nm)
  (cond
    ((= nm "名称") 1)
    ((= nm "型号") 2)
    ((= nm "规格") 3)
    ((= nm "数量") 4)
    (T 9)
  )
)

;; 选择排序（稳定）。★ 不用 vl-sort：它会把「相等」的元素去掉，字段会丢
(defun dtf:sortflds (lst / out best bv)
  (setq out nil)
  (while lst
    (setq best (car lst) bv (dtf:ordf (cadr best)))
    (foreach x lst
      (if (< (dtf:ordf (cadr x)) bv)
        (setq best x bv (dtf:ordf (cadr x)))
      )
    )
    (setq out (cons best out))
    (setq lst (dtf:rem1 best lst))
  )
  (reverse out)
)

;; 多行规格标签（规格1..M）合成一行文本；有单行「规格」就直接用它
(defun dtf:joinspec (specs / one lst r)
  (setq one nil lst nil)
  (foreach s specs
    (if (= (car s) "规格")
      (setq one (cdr s))
      (setq lst (cons s lst))
    )
  )
  (if one
    one
    (progn
      (setq lst (vl-sort lst '(lambda (a b)
                                (< (atoi (substr (car a) 5)) (atoi (substr (car b) 5))))))
      (setq r "")
      (foreach s lst
        (setq r (if (= r "") (cdr s) (strcat r " " (cdr s))))
      )
      r
    )
  )
)

;; ================================================================
;; 二、图上扫描（只收柜属性块 / 表头属性块）
;; ================================================================

(setq *dtf:blks* nil)

;; 扫一遍模型空间 -> *dtf:blks*
;;   元素 = (参照 类型 插入点 定义信息)，类型 "cab" 柜 / "hdr" 表头
(defun dtf:scanblks ( / ss i n e ed bn atts typ out)
  (setq out nil)
  (setq ss (ssget "_X" '((0 . "INSERT") (410 . "Model"))))
  (if ss
    (progn
      (setq i 0 n (sslength ss))
      (while (< i n)
        (setq e (ssname ss i) ed (entget e) bn (cdr (assoc 2 ed)))
        (setq atts (plt:atts e) typ nil)
        (cond
          ((assoc *plt:tag-cab* atts) (setq typ "cab"))
          ((assoc *plt:tag-hdr* atts) (setq typ "hdr"))
        )
        (if typ
          (setq out (cons (list e typ (cdr (assoc 10 ed)) (plt:definfo bn)) out))
        )
        (setq i (1+ i))
      )
    )
  )
  (setq *dtf:blks* (reverse out))
)

;; 某个块的 x 区间（定义几何 + 插入点，不含属性文字溢出）
;;   ★ plt:definfo 返回 (ATTDEF数 宽 高 最小x 最小y) —— 最小x 是【第 4 项】，
;;     第 5 项是最小y；下标写错会静默把 y 当 x 用（横竖差几百，台台串号）
(defun dtf:xrng (it / p di x0)
  (setq p (caddr it) di (cadddr it) x0 (+ (car p) (nth 3 di)))
  (list x0 (+ x0 (nth 1 di)))
)

;; 点落在哪台柜 / 哪枚表头：x 区间包含点的取「中心最近」的那个
;;   （柜块与表头块 x 紧挨甚至重叠，取中心最近才不串台）
(defun dtf:pickblk (p / best bd it xr cx d)
  (setq best nil bd 1e9)
  (foreach it *dtf:blks*
    (setq xr (dtf:xrng it))
    (if (and (>= (car p) (car xr)) (<= (car p) (cadr xr)))
      (progn
        (setq cx (/ (+ (car xr) (cadr xr)) 2.0))
        (setq d (abs (- (car p) cx)))
        (if (< d bd) (setq bd d best it))
      )
    )
  )
  best
)

;; 表头块里，插入点 x 离点击点最近的那一枚
;;   KYN28 两框、箱变两段都自然对上（段 1 表头在左、段 2 表头在右）
(defun dtf:pickhdr (p / best bd d it)
  (setq best nil bd 1e9)
  (foreach it *dtf:blks*
    (if (= "hdr" (cadr it))
      (progn
        (setq d (abs (- (car p) (car (caddr it)))))
        (if (< d bd) (setq bd d best it))
      )
    )
  )
  best
)

;; 这枚表头在图上排第几（按插入点 x 从左到右）
(defun dtf:hdrno (it / hx xs i n)
  (if (null it)
    0
    (progn
      (setq hx (car (caddr it)) xs nil)
      (foreach b *dtf:blks*
        (if (= "hdr" (cadr b)) (setq xs (cons (car (caddr b)) xs)))
      )
      (setq xs (vl-sort xs '<) i 0 n 0)
      (while (< i (length xs))
        (if (< (nth i xs) (- hx 0.01)) (setq n (1+ n)))
        (setq i (1+ i))
      )
      (1+ n)
    )
  )
)

;; ================================================================
;; 三、位置 -> 目标（哪个参照、哪个槽、界面给哪些字段）
;; ================================================================
;; 目标 = (类型 参照 槽号 标题 提示 组表)
;;   类型 "cab" 柜 / "hdr" 表头 / "miss" 没认出来
;;   组表 = ((组标题 (字段...)) ...)
;;   字段 = (标签 显示名 值 可写 参照)

;; 参照的属性按槽号分组 -> ((槽号 ymin ymax 属性表) ...)，槽号 0 = 非槽位属性
(defun dtf:grps (ref / out item k y a)
  (setq out nil)
  (foreach a (plt:atts ref)
    (setq k (dtf:slotno (car a)) k (if k k 0))
    (setq y (caddr (assoc 10 (entget (cdr a)))))
    (setq item (assoc k out))
    (if item
      (setq out (subst (list k (min (cadr item) y) (max (caddr item) y)
                             (append (nth 3 item) (list a))) item out))
      (setq out (cons (list k y y (list a)) out))
    )
  )
  (reverse out)
)

;; 点 y 离哪个槽最近（按该槽每个属性的 y 算距离），最近 > 9 视为没点在块上
(defun dtf:ngroup (grps py / best bd g a y d)
  (setq best nil bd 1e9)
  (foreach g grps
    (foreach a (nth 3 g)
      (setq y (caddr (assoc 10 (entget (cdr a)))))
      (setq d (abs (- y py)))
      (if (< d bd) (setq bd d best (car g)))
    )
  )
  (if (< bd 9.0) best nil)
)

;; 非槽位属性的字段（柜级 7 项 / 表头信息区 2 项），按块里定义顺序
(defun dtf:flds0 (atts ref / out)
  (setq out nil)
  (foreach a atts
    (if (null (dtf:slotno (car a)))
      (setq out (cons (list (car a) (car a) (plt:val atts (car a)) T ref) out))
    )
  )
  (reverse out)
)

;; 槽 k 的字段：名称 / 型号 / 规格（多行合一） / 数量 / 其他
(defun dtf:flds (atts ref k / out specs suf v)
  (setq out nil specs nil)
  (foreach a atts
    (if (= k (dtf:slotno (car a)))
      (progn
        (setq suf (dtf:suffix (car a)) v (plt:atext (cdr a)))
        (if (vl-string-search "规格" suf)
          (setq specs (cons (cons suf v) specs))
          (progn
            ;; 数量空着时给个默认 1（元件绝大多数是 1 台）—— 看得见，可改
            (if (and (= suf "数量") (= v "")) (setq v "1"))
            (setq out (cons (list (car a) suf v T ref) out))
          )
        )
      )
    )
  )
  (if specs
    (setq out (cons (list (plt:tagk k "规格") "规格" (dtf:joinspec specs) T ref) out))
  )
  (dtf:sortflds (reverse out))
)

;; 槽 k 的字段（表头块只有 名称 / 型号）
(defun dtf:fldsh (atts ref k / out)
  (setq out nil)
  (foreach a atts
    (if (= k (dtf:slotno (car a)))
      (setq out (cons (list (car a) (dtf:suffix (car a)) (plt:atext (cdr a))
                           T ref) out))
    )
  )
  (dtf:sortflds (reverse out))
)

;; 主入口：点 -> 目标
(defun dtf:target (p / blk it typ ref atts grps k hdr hidx hatt href cabno mx f1 f2
                     hgot hname hmod ttl hint)
  (setq blk (dtf:pickblk p))
  (cond
    ;; ---------- 没点在柜 / 表头的 x 区间里 ----------
    ((null blk)
     (list "miss" nil 0 "这个位置不在柜 / 表头块上（图签和空白处不管），再点一次。" nil nil))
    (T
     (setq it blk typ (cadr blk) ref (car it) atts (plt:atts ref))
     (setq grps (dtf:grps ref) k (dtf:ngroup grps (cadr p)))
     (setq mx 0)
     (foreach g grps (if (> (car g) mx) (setq mx (car g))))
     (cond
       ((null k)
        (list "miss" nil 0
              "点在块上了，但离最近的数据行太远（x 对了、y 不在可填区），再点一次。"
              nil nil))
       ;; ---------- 组 0：柜级 / 表头信息区 ----------
       ((= k 0)
        (if (= typ "cab")
          (progn
            (setq cabno (dtf:nz (plt:val atts *plt:tag-cab*)))
            (list "cab" ref 0 (strcat "柜 " cabno " · 柜级信息")
                  "柜级字段（改完按「写入」）"
                  (list (list "柜级字段" (dtf:flds0 atts ref)))))
          (list "hdr" ref 0 "表头 · 信息区"
                "表头信息区（母排 / 电压等级，改的是全排表头）"
                (list (list "表头信息区" (dtf:flds0 atts ref))))))
       ;; ---------- 表头块槽位区 ----------
       ((= typ "hdr")
        (list "hdr" ref k (strcat "表头 " (itoa (dtf:hdrno it)) " · 槽 " (itoa k))
              (strcat "表头块槽位 —— 改的是本排表头（共 " (itoa mx) " 槽）")
              (list (list (strcat "表头槽 " (itoa k) " / " (itoa mx))
                          (dtf:fldsh atts ref k)))))
       ;; ---------- 柜块槽位区 ----------
       (T
        ;; 先配最近的那枚表头
        ;;   ★ pickhdr 给的是「条目表」，plt:atts 要的是实体图元名 -> 必须 (car hdr)
        (setq hdr (dtf:pickhdr p) hidx (dtf:hdrno hdr) href (if hdr (car hdr) nil))
        (setq cabno (dtf:nz (plt:val atts *plt:tag-cab*)))
        (setq ttl (strcat "柜 " cabno " · 槽 " (itoa k) " / " (itoa mx)))
        (setq f2 (dtf:flds atts ref k))
        (cond
          ((null hdr)
           (list "cab" ref k ttl
                 "图上没有表头块 —— 只写本柜该槽"
                 (list (list (strcat "本柜 " cabno " · 槽 " (itoa k)) f2))))
          (T
           (setq hatt (plt:atts href))
           (setq hname (dtf:nz (plt:val hatt (plt:tagk k "名称"))))
           (setq hmod  (dtf:nz (plt:val hatt (plt:tagk k "型号"))))
           (setq hgot (if (or (/= hname "") (/= hmod "")) T nil))
           (setq f1 (dtf:fldsh hatt href k))
           (if hgot
             (progn
               ;; 表头已有 -> 只读展示，字段全部不可写
               (setq f1 (mapcar '(lambda (f)
                                   (list (car f) (cadr f) (nth 2 f) nil href)) f1))
               (setq hint "该槽表头已有值，上面只读显示；下面填的是本柜该槽数据")
             )
             (setq hint "该槽表头为空 —— 填名称 / 型号会同时写进表头块")
           )
           (list "cab" ref k ttl hint
                 (list
                   (list (strcat "表头 " (itoa hidx)
                                 (if hgot "（已有，只读）" "（本槽暂无，填写即定义）"))
                         f1)
                   (list (strcat "本柜 " cabno " · 槽 " (itoa k)) f2))))
        )
       )
     )
    )
  )
)

;; ================================================================
;; 四、界面（DCL）
;; ================================================================

(setq *dtf:keys* nil *dtf:flat* nil *dtf:vals* nil *dtf:ok* nil)

;; DCL 落到 %TEMP%（字段数随位置变，必须现生成）
(defun dtf:dclpath ( / d c)
  (setq d (getvar "TEMPPREFIX"))
  (if (or (null d) (= d ""))
    (setq d (vl-filename-mktemp "dtf"))
  )
  (setq c (substr d (strlen d) 1))
  (if (and (/= c "\\") (/= c "/")) (setq d (strcat d "\\")))
  (strcat d "dtf_dlg.dcl")
)

;; DCL 字符串里不能出现裸引号 / 反斜杠 —— 换掉，否则整份 DCL 解析失败、弹窗静默不出
(defun dtf:esc (s / x)
  (cond
    ((null s) (setq x ""))
    ((= (type s) 'STR) (setq x s))
    ((= (type s) 'INT) (setq x (itoa s)))
    ((= (type s) 'REAL) (setq x (rtos s 2 2)))
    (T (setq x (vl-princ-to-string s)))
  )
  (vl-string-translate "\"\\" "  " x)
)

;; 生成 DCL 文本
(defun dtf:mkdcl (fn ttl hint grps / h i g f)
  (setq h (open fn "w"))
  (if (null h)
    nil
    (progn
      (write-line "dtfdlg : dialog {" h)
      (write-line (strcat "  label = \"" (dtf:esc ttl) "\";") h)
      (write-line "  : text { key = \"hint\"; width = 58; }" h)
      (setq i 0)
      (foreach g grps
        (write-line "  : boxed_column {" h)
        (write-line (strcat "    label = \"" (dtf:esc (car g)) "\";") h)
        (foreach f (cadr g)
          (write-line "    : row {" h)
          (write-line (strcat "      : text { label = \"" (dtf:esc (cadr f)) "\"; width = 12; }") h)
          (write-line (strcat "      : edit_box { key = \"e" (itoa i) "\"; edit_width = 32; }") h)
          (write-line "    }" h)
          (setq i (1+ i))
        )
        (write-line "  }" h)
      )
      (write-line "  : row {" h)
      (write-line "    : button { key = \"bok\"; label = \"写入\"; is_default = true; }" h)
      (write-line "    : button { key = \"bno\"; label = \"取消\"; is_cancel = true; }" h)
      (write-line "  }" h)
      (write-line "}" h)
      (close h)
      T
    )
  )
)

;; 收集各框的值（与 *dtf:flat* 同序）
(defun dtf:pull ( / o k)
  (setq o nil)
  (foreach k *dtf:keys* (setq o (cons (get_tile k) o)))
  (reverse o)
)

;; 弹窗；返回 (值...) 或 nil（取消）
(defun dtf:ui (ttl hint grps / fn id r i f)
  (setq *dtf:keys* nil *dtf:flat* nil *dtf:vals* nil *dtf:ok* nil i 0)
  (foreach g grps
    (foreach f (cadr g)
      (setq *dtf:keys* (cons (strcat "e" (itoa i)) *dtf:keys*))
      (setq *dtf:flat* (cons f *dtf:flat*))
      (setq i (1+ i))
    )
  )
  (setq *dtf:keys* (reverse *dtf:keys*) *dtf:flat* (reverse *dtf:flat*))
  (if (null *dtf:flat*)
    (progn (princ "\n这个位置没认出可填的格。") nil)
    (progn
      (setq fn (dtf:dclpath))
      (if (null (dtf:mkdcl fn ttl hint grps))
        (progn (princ (strcat "\n写不出 DCL 文件：" fn)) nil)
        (progn
          (setq id (load_dialog fn))
          (if (< id 0)
            (progn (princ "\n加载对话框失败。") nil)
            (progn
              (if (not (new_dialog "dtfdlg" id))
                (progn (unload_dialog id) (princ "\n打开对话框失败。") nil)
                (progn
                  (set_tile "hint" (dtf:nz hint))
                  (setq i 0)
                  (foreach f *dtf:flat*
                    (set_tile (nth i *dtf:keys*) (dtf:nz (nth 2 f)))
                    (if (null (nth 3 f)) (mode_tile (nth i *dtf:keys*) 1))
                    (setq i (1+ i))
                  )
                  (action_tile "bok" "(progn (setq *dtf:ok* T *dtf:vals* (dtf:pull)) (done_dialog 1))")
                  (action_tile "bno" "(done_dialog 0)")
                  ;; start_dialog 万一抛异常 -> 出人话提示，别让命令默默中断
                  (setq r (vl-catch-all-apply 'start_dialog nil))
                  (unload_dialog id)
                  (if (vl-catch-all-error-p r)
                    (progn (princ (strcat "\n对话框异常："
                                          (vl-catch-all-error-message r))) nil)
                    (if (and (= r 1) *dtf:ok*) *dtf:vals* nil)
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

;; ================================================================
;; 五、写回
;; ================================================================

;; 把界面上收到的值写回图 -> 报告文字
;;   ★ 每个字段写【它自己的参照】(nth 4 f)，不是一律写主参照：
;;     柜槽界面里「表头组」写表头块、「本柜组」写柜块。写成主参照的话
;;     「表头为空时填名称/型号」会往柜块上写 —— 柜块没这标签，静默落空。
(defun dtf:write (tg vals / i n rep f lab nm v wok rf atts rw k)
  (setq i 0 n 0 rep "")
  (foreach f *dtf:flat*
    (setq lab (car f) nm (cadr f) v (dtf:nz (nth i vals))
          wok (nth 3 f) rf (nth 4 f))
    (if (and wok rf)
      (progn
        (cond
          ;; 规格类：按 PLT 同一套宽度规则拆行；多行格只填前几行时整组居中
          ((dtf:specp lab)
           (setq k (dtf:slotno lab) atts (plt:atts rf))
           (setq rw (plt:putspec atts k v T))
           (setq n (+ n (car rw)))
           (if (and (> (cadr rw) 0) (> (caddr rw) 0) (< (caddr rw) (cadr rw)))
             (plt:midrow rf k (cadr rw) (caddr rw))
           )
           (setq rep (strcat rep (if (= rep "") "" " / ") nm)))
          ;; 其余普通属性（名称 / 型号 / 数量 / 柜级 7 项 / 母排 / 电压）
          (T
           (setq atts (plt:atts rf))
           (if (plt:put atts lab v T)
             (progn
               (setq n (1+ n))
               (setq rep (strcat rep (if (= rep "") "" " / ") nm))
             )
             (setq rep (strcat rep (if (= rep "") "" " / ") nm "（图上没这格）"))
           )
          )
        )
      )
    )
    (setq i (1+ i))
  )
  (strcat "已写 " (itoa n) " 格：" (if (= rep "") "（没有可写的格）" rep)
          "。没存盘，改动都在内存里。")
)

;; ================================================================
;; 六、入口
;; ================================================================

;; 数一数某类型的块有几枚
(defun dtf:cnt (ty / n)
  (setq n 0)
  (foreach b *dtf:blks* (if (= ty (cadr b)) (setq n (1+ n))))
  n
)

(defun c:DTF ( / p tg v go)
  (if (null (dtf:ensure))
    (princ "\n[DTF] 内核没就位：先 load 属性块-批量填数.lsp（本文件同目录）。")
    (progn
      (dtf:scanblks)
      (if (null *dtf:blks*)
        (princ "\n图上没有柜块 / 表头块（先跑 PGT 铺图）。")
        (progn
          (princ (strcat "\nDTF：点一个位置 -> 弹窗填数。柜 " (itoa (dtf:cnt "cab"))
                         " 台 / 表头 " (itoa (dtf:cnt "hdr")) " 枚。"))
          (setq go T)
          (while go
            (setq p (getpoint "\n点选要填写的位置（回车结束）: "))
            (if (null p)
              (setq go nil)
              (progn
                (setq tg (dtf:target p))
                (if (= (car tg) "miss")
                  (princ (strcat "\n" (caddr tg)))
                  (progn
                    (setq v (dtf:ui (nth 3 tg) (nth 4 tg) (nth 5 tg)))
                    (if v
                      (princ (strcat "\n" (dtf:write tg v)))
                      (princ "\n取消了，图没动。")
                    )
                  )
                )
              )
            )
          )
          (princ "\nDTF 结束。")
        )
      )
    )
  )
  (princ)
)

(princ "\n属性块-点选填数.lsp 已加载。命令 DTF —— 点一个位置，弹窗填数。")
(princ)
