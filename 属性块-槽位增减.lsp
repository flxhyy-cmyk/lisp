;;; ============================================================
;;; 属性块-槽位增减.lsp                                      命令：GKC
;;; ------------------------------------------------------------
;;; 作用
;;;   给「一排」属性块（柜块 + 表头块）在行区【底部】一次增减 N
;;;   个标准槽位行。整排（含表头）同步增减 —— 不必一块一块地点。
;;;
;;; 用法（人工操作）
;;;   1. APPLOAD 加载本文件，命令行输入  GKC
;;;   2. 点选「一排」属性块里的【任意一个】（柜块、表头块都行）——
;;;      程序自己判断整排范围（同一水平带、左右紧贴的属性块，含表头）
;;;   3. 弹出对话框：点【增】一次 +1 行，点【减】一次 -1 行，可反复点，
;;;      增减互相抵消，【归零】清零。对话框实时显示：
;;;        · 本次要增 / 减的行数
;;;        · 本排每个块：现有槽数 -> 执行后槽数
;;;        · 离图框下沿还剩多少（会超框会提示）
;;;        · 减不了的原因（最底几行有数据 / 已到槽数下限），此时
;;;          【确定】按钮灰掉，防止误操作
;;;   4. 点【确定】才真正执行；点【取消】图一个字不动
;;;   5. 整个操作是一个撤销步骤，不满意 Ctrl+Z（或 U）一步退回
;;;
;;; 自动化入口（人用不到，保留给外部工具，逻辑没动）
;;;   (gkc:plan   "句柄,句柄,..." N)   只读清单，图不动
;;;   (gkc:expand "句柄,句柄,..." N)   执行；N 负数 = 减槽
;;;
;;; 「本排」怎么圈（只对手动入口有意义）
;;;   先按 y 区间圈出同一水平带的所有属性块，再按「x 相接」从
;;;   选中的那块向两侧扩展 —— 只要紧贴在一起就算本排，不再按
;;;   槽数是否相同来切分。槽数不同的块并进同一排后，增/减时
;;;   各块按自身现有槽数分别 +N / -N，互不影响。
;;;
;;; 纵向几何口径（全部实测得来，不写死数字）
;;;   ① 柜块顶部（顶部数据容器）、列标题行、以及【已有全部槽位】
;;;      一律原地不动；
;;;   ② 增减那 N 行占的空间从【行区底部】腾出 / 收回 ——
;;;        增：底部区（备注行 / 容量功率行 / 空白行）整体下移 N 个
;;;            行距，N 个新槽插在行区最底，块底跟着向下长 N 个行距；
;;;        减：底部区整体上移 N 个行距，行区最底 N 槽被删掉，
;;;            块底跟着向上收 N 个行距。
;;;      说白了就是「备注行、功率行上下移动，柜顶恒定」。
;;;   ③ 所有图元共用一条判定规则（LINE / LWPOLYLINE / ATTDEF /
;;;      TEXT / MTEXT 一律照此办理）：
;;;          点的 y <= 行区底线 B  ->  增则 -N*pitch，减则 +N*pitch
;;;      于是柜框底边、分栏竖线底端、底部区那几条横线、备注与
;;;      容量功率属性自动跟着走；顶边与上部属性天然不动。
;;;   ④ 行区底线 B 那条横线：
;;;        增 N 行时【原地保留】（它变成最上那个新槽的顶线），
;;;        另在 B-pitch, B-2*pitch, ... B-N*pitch 处补画 N 条
;;;        （各新槽的底线）；
;;;        减 N 行时把最底 N 条（B, B+pitch, ... B+(N-1)*pitch）删掉。
;;;   ⑤ 行区分栏竖线（穿过槽位行的 LINE 竖线，如「规格 | 数量」之间
;;;      那根）：下端一律吸附到行区底线上 ——
;;;        增 N 行时下端下移 N 个行距（不跟就会缺：新槽那一段没竖线），
;;;        减 N 行时下端上移 N 个行距（不跟就会拖到已删掉的行区里去）。
;;;      上端（行区顶）不动：加槽只往行区底部加，行区顶不挪。
;;;
;;; 一次到位
;;;   增 / 减 N 槽在【一趟重建】里做完，不循环调 N 次 —— 循环会让
;;;   参照重造 N 轮、句柄变 N 轮，表头块 of= 还得每轮追着改名。
;;;   K=1 时与「一次一槽」的老写法逐字等价。
;;;
;;; 槽位与槽号
;;;   槽位属性 tag 形如「元件NN规格」「元件NN数量」（表头块是
;;;   「元件NN名称」「元件NN型号」）。新槽号 = 现有最大 +1，两位
;;;   数补零。第 01 槽在一期图上占 3 个子行（18 高，规格1/2/3 +
;;;   数量），它【永远不参与】增减，也永远不当模板；模板固定取
;;;   【行区最底那一槽】（标准行）。
;;;
;;; 减的两道闸门（整排级）
;;;   ① 本排【任意】一个属性块、最底 N 槽的【任意】一个属性只要有
;;;      非空值 -> 整次拒绝，一个块都不动；
;;;   ② 减完剩下的槽数低于 *gkc:min*（默认 1）时拒绝。
;;;
;;; 槽数不必一致
;;;   本排各属性块的最大槽号可以不同，增/减时各块按自身现有
;;;   槽数分别处理，互不影响，不再要求先对齐。
;;;
;;; 共享定义
;;;   块定义一律「克隆成新名 + 把本排参照改指过去」的方式重建，
;;;   原定义原封不动 —— 别的排就算共用同一个定义也一个字不变。
;;;   新定义名 = 原名 + "-" + 该参照的柜号（表头块用「表头」）。
;;;   原定义的自描述串（Comments）一并带走，表头块 of= 里的柜块名同步
;;;   改成新名；slots 数值 ±N、slotrows 补 / 去 N 项。少了这一步，
;;;   MCP 侧 acad_att_bom 认不到表头块、acad_att_panel 会判「没建过」重画。
;;;
;;; 报告
;;;   命令提示与执行结果都写详细：逐块列出槽数、新旧定义名、
;;;   同步了几个参照、底部区移了多少、拒绝的具体原因与所在块。
;;;
;;; 编码：ANSI/GBK 无 BOM
;;; ============================================================
(vl-load-com)
(setq SEP (chr 10))

;; ---------------- 常量 ----------------
(setq *gkc:pfx* "元件")   ; 槽位属性前缀
(setq *gkc:min* 1)        ; 至少保留几个槽（低于它拒绝再减）

;; ================================================================
;; 一、基础小工具
;; ================================================================

;; 去首尾空白
(defun gkc:trim (s / r)
  (if s
    (progn
      (setq r s)
      (while (and (> (strlen r) 0) (<= (ascii (substr r 1 1)) 32))
        (setq r (substr r 2)))
      (while (and (> (strlen r) 0) (<= (ascii (substr r (strlen r) 1)) 32))
        (setq r (substr r 1 (1- (strlen r)))))
      r)
    "")
)

;; 两位数补零：1 -> "01"
(defun gkc:pad2 (n)
  (if (< n 10) (strcat "0" (itoa n)) (itoa n))
)

;; 拼槽位 tag：元件 + NN + 后缀
(defun gkc:mktag (n sfx)
  (strcat *gkc:pfx* (gkc:pad2 n) sfx)
)

;; 从属性 tag 解析槽号：元件NNxxx -> NN；不是槽位属性返回 nil
;;   注意 AutoLISP 的 strlen / substr 按【字节】算，一个汉字 2 字节：
;;   「元件」= 4 字节，槽号固定 2 字节数字 —— 只取第 5、6 字节这个
;;   纯 ASCII 片段做 substr 永远安全（DBCS 陷阱就在这儿）。
(defun gkc:slotno (tg / s c1 c2)
  (if (and tg (>= (strlen tg) 6) (= (substr tg 1 4) *gkc:pfx*))
    (progn
      (setq s (substr tg 5 2))
      (setq c1 (ascii (substr s 1 1)) c2 (ascii (substr s 2 1)))
      (if (and (>= c1 48) (<= c1 57) (>= c2 48) (<= c2 57))
        (atoi s)
        nil))
    nil)
)

;; 取槽号之后的字段名：元件01规格1 -> 规格1 / 元件12数量 -> 数量
(defun gkc:suffix (tg)
  (substr tg 7)
)

;; 字符串列表拼成一段
(defun gkc:join (lst sep / r)
  (setq r "")
  (foreach s lst (setq r (if (= r "") s (strcat r sep s))))
  r
)

;; 输出报告（*gkc:quiet* 为真时一声不响 —— MCP 内部调用走这条）
(defun gkc:say (s)
  (if (not *gkc:quiet*)
    (progn
      (if s (princ s))
      (princ)
    )
  )
)

;; 块定义里所有图元的实体数据（按定义顺序）
(defun gkc:ents (bn / b e ed res)
  (setq b (tblobjname "BLOCK" bn) e (if b (entnext b) nil) res nil)
  (while (and e (/= "ENDBLK" (cdr (assoc 0 (setq ed (entget e))))))
    (setq res (cons ed res))
    (setq e (entnext e))
  )
  (reverse res)
)

;; 块定义里所有 ATTDEF 的实体数据
(defun gkc:attdefs (bn / res ed)
  (setq res nil)
  (foreach ed (gkc:ents bn)
    (if (= "ATTDEF" (cdr (assoc 0 ed))) (setq res (cons ed res)))
  )
  (reverse res)
)

;; 块定义里有没有 ATTDEF —— 也就是「是不是属性块」
(defun gkc:attrp (bn / b e ed r)
  (setq b (tblobjname "BLOCK" bn) e (if b (entnext b) nil) r nil)
  (while (and e (null r) (/= "ENDBLK" (cdr (assoc 0 (setq ed (entget e))))))
    (if (= "ATTDEF" (cdr (assoc 0 ed))) (setq r T))
    (setq e (entnext e))
  )
  r
)

;; ================================================================
;; 二、槽位几何（全部实测）
;; ================================================================

;; 槽位分组 -> ((槽号 最小y ATTDEF列表) ...)，按槽号升序
(defun gkc:slots (bn / res ed n y item)
  (setq res nil)
  (foreach ed (gkc:attdefs bn)
    (setq n (gkc:slotno (cdr (assoc 2 ed))))
    (if n
      (progn
        (setq y (caddr (assoc 10 ed)))
        (setq item (assoc n res))
        (if item
          (setq res (subst (list n (min (cadr item) y) (append (caddr item) (list ed)))
                           item res))
          (setq res (cons (list n y (list ed)) res))
        )
      )
    )
  )
  (vl-sort res '(lambda (a b) (< (car a) (car b))))
)

;; 行距：所有槽位属性 y 的相邻差里、出现次数最多的那个
;;   （实测柜块 = 6；第 01 槽内部子行也是 6，不会被带偏）
(defun gkc:pitch (bn / ys u d prev dd v cnt item best mx)
  (setq ys nil)
  (foreach ed (gkc:attdefs bn)
    (if (gkc:slotno (cdr (assoc 2 ed)))
      (setq ys (cons (caddr (assoc 10 ed)) ys))))
  (setq ys (vl-sort ys '<))
  (setq u nil)
  (foreach v ys
    (if (null (vl-remove-if '(lambda (x) (equal x v 0.01)) u))
      (setq u (cons v u))))
  (setq ys (vl-sort u '<))
  (setq d nil prev nil)
  (foreach v ys
    (if prev
      (progn
        (setq dd (- v prev))
        (if (> dd 0.01) (setq d (cons (atof (rtos dd 2 2)) d)))
      )
    )
    (setq prev v)
  )
  (setq cnt nil)
  (foreach v d
    (setq item (assoc v cnt))
    (if item
      (setq cnt (subst (cons v (1+ (cdr item))) item cnt))
      (setq cnt (cons (cons v 1) cnt))
    )
  )
  (setq best nil mx 0)
  (foreach v cnt
    (if (> (cdr v) mx) (progn (setq mx (cdr v)) (setq best (car v)))))
  best
)

;; 找 y 附近的那条横线（两端等高的 LINE），返回它的实体数据
(defun gkc:hline-ed (bn y / b e ed res best d)
  (setq b (tblobjname "BLOCK" bn) e (if b (entnext b) nil) res nil best 1e9)
  (while (and e (/= "ENDBLK" (cdr (assoc 0 (setq ed (entget e))))))
    (if (and (= "LINE" (cdr (assoc 0 ed)))
             (equal (caddr (assoc 10 ed)) (caddr (assoc 11 ed)) 1e-6))
      (progn
        (setq d (abs (- (caddr (assoc 10 ed)) y)))
        (if (< d best) (setq best d res ed))
      )
    )
    (setq e (entnext e))
  )
  (if (< best 1.0) res nil)
)

;; 一次算齐纵向几何 -> (行区底线B 行距P 最大槽号 槽表 最底槽 底线图元)
(defun gkc:geo (bn / sls low p y h off ln)
  (setq sls (gkc:slots bn))
  (if (null sls)
    nil
    (progn
      (setq p (gkc:pitch bn) p (if p p 6.0))
      (setq low (car (vl-sort sls '(lambda (a b) (< (cadr a) (cadr b))))))
      (setq y (cadr low))
      (setq h (cdr (assoc 40 (car (caddr low)))) h (if h h 3.0))
      (setq off (/ (- p h) 2.0))
      (setq B (- y off))
      (setq ln (gkc:hline-ed bn B))
      (if ln (setq B (caddr (assoc 10 ln))))
      (list B p (apply 'max (mapcar 'car sls)) sls low ln)
    )
  )
)

;; 某个槽的全部属性 tag
(defun gkc:slot-tags (bn n / res ed s)
  (setq res nil)
  (foreach ed (gkc:attdefs bn)
    (setq s (gkc:slotno (cdr (assoc 2 ed))))
    (if (and s (= s n)) (setq res (cons (cdr (assoc 2 ed)) res))))
  (reverse res)
)

;; 一段槽（lo..hi，闭区间）的全部属性 tag —— 减 K 槽时一次拿全，不够 K 个就少拿
(defun gkc:slot-tags-r (bn lo hi / res ed s)
  (setq res nil)
  (foreach ed (gkc:attdefs bn)
    (setq s (gkc:slotno (cdr (assoc 2 ed))))
    (if (and s (>= s lo) (<= s hi)) (setq res (cons (cdr (assoc 2 ed)) res))))
  (reverse res)
)

;; ================================================================
;; 三、包围盒 / 同排判定
;; ================================================================

;; 包围盒（读不到就按「插入点 + 定义几何」自己推）
(defun gkc:bb (e / o mn mx r bn b ee de ip sc dx0 dy0 dx1 dy1 x y)
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
          (if (and (member (car pr) '(10 11))
                   (listp (cdr pr)) (numberp (car (cdr pr))))
            (progn
              (setq x (car (cdr pr)) y (cadr (cdr pr)))
              (if (< x dx0) (setq dx0 x)) (if (> x dx1) (setq dx1 x))
              (if (< y dy0) (setq dy0 y)) (if (> y dy1) (setq dy1 y))
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

;; 是不是「把柜块整个包住的外框」（图框）—— 这类块绝不参与判定
(defun gkc:frame (mn mx mn2 mx2)
  (and (<= (car mn2) (+ (car mn) 0.01))
       (>= (car mx2) (- (car mx) 0.01))
       (<= (cadr mn2) (+ (cadr mn) 0.01))
       (>= (cadr mx2) (- (cadr mx) 0.01)))
)

;; 开工前把所有块参照的包围盒拍个快照（实体名 . (最小点 最大点)）
(defun gkc:snapshot ( / ss i n lst e bbx)
  (setq ss (ssget "X" '((0 . "INSERT"))) i 0 lst nil
        n (if ss (sslength ss) 0))
  (while (< i n)
    (setq e (ssname ss i) bbx (gkc:bb e))
    (if bbx (setq lst (cons (list e (car bbx) (cadr bbx)) lst)))
    (setq i (1+ i))
  )
  lst
)

;; 本排：y 区间与 e 重叠的【属性块】参照（含 e 自己），不含图框
;; 定义的最大槽号（不是属性块 / 没有槽位 -> nil）
(defun gkc:maxslot (bn / g0)
  (setq g0 (gkc:geo bn))
  (if g0 (caddr g0) nil)
)

;; 相邻两块能不能连成同一段：只看 x 是否相接，不再要求槽数一致
;;   —— 紧贴在一起就算同一排，槽数不同也并成一排、统一增减
;;   （各按自身槽数分别处理）。n0 参数保留仅为兼容调用方，不再使用。
;;   【2026-09-23 起已不再被调用】只比「紧邻两项」会把链断在夹在
;;   块内的窄块上（实测 CCSYM01101501），gkc:row 已改用「段并集」；
;;   本函数保留备查，不要再拿它做整排判定。
(defun gkc:adjoin (aa bb n0)
  (and (<= (car (cadr bb)) (+ (car (caddr aa)) 0.5))
       (>= (car (caddr bb)) (- (car (cadr aa)) 0.5)))
)

;; 本排：先按 y 重叠圈出「同一水平带」的属性块（且必须是【带槽位】的
;;   属性块），再按「x 相接」从选中块向两侧扩展，切出这一段 ——
;;   紧贴在一起就算同一排，不再按槽数是否相同来切分。槽数不同的块
;;   并进同一排后，增/减时各自按自身现有槽数分别处理，互不影响。
;;   扩展用【段并集】推进：维护当前段的 x 区间 [L,R]，反复扫带内成员，
;;   凡与 [L,R] 交叠（间隙 ≤ 0.5）就并入并把区间撑大，直到一轮没有新增。
;;   —— 旧写法只比「紧邻两项」，一旦中间夹着被别的块包住的窄块
;;   （2026-09-23 实测 CCSYM01101501 包围盒 295.8~313.2 落在柜块
;;   285~320 内部），链就在那里断掉，最右的表头块进不来
;;   （实测：整排 12 块只认出 7 块、点最右表头只认 1 块）。
;;   带内成员必须带槽位：无槽位的属性块（符号类）增减不了，混进来
;;   只会把 x 链搅断，同样是 2026-09-23 实测。
;;   实体一律按【句柄】认 —— 本机实测不同途径取到的实体名用 eq
;;   比会失手（段会切空），句柄是字符串，稳。
(defun gkc:row (e snap / eh itm mn mx band o2 mn2 mx2 n0 res L R grew xl xr bh ehs bn)
  (setq eh (cdr (assoc 5 (entget e))) itm nil)
  (foreach o2 snap
    (if (and (null itm) (= (cdr (assoc 5 (entget (car o2)))) eh)) (setq itm o2))
  )
  (if (null itm)
    nil
    (progn
      (setq mn (cadr itm) mx (caddr itm) band (list itm))
      ;; 自己也算带内一员，但要【先跳过自己】再判「谁包住谁」——
      ;; 自己的包围盒当然包住自己，不跳过会被 frame 判据剔掉（踩过，
      ;; 表现就是段里没有自己、整段切空）。
      (foreach o2 snap
        (setq bn (cdr (assoc 2 (entget (car o2)))) mn2 (cadr o2) mx2 (caddr o2))
        (if (and (/= (cdr (assoc 5 (entget (car o2)))) eh)
                 (gkc:attrp bn)
                 (gkc:maxslot bn)
                 (not (gkc:frame mn mx mn2 mx2))
                 (> (- (min (cadr mx) (cadr mx2)) (max (cadr mn) (cadr mn2))) 0.01))
          (setq band (cons o2 band))
        )
      )
      (setq n0 (gkc:maxslot (cdr (assoc 2 (entget e)))))
      (if (null n0)
        nil
        (progn
          ;; 从选中块自己的 x 区间起步，一轮轮把「与当前段交叠」的成员吃进来
          (setq L (car mn) R (car mx) res (list itm) ehs (list eh) grew T)
          (while grew
            (setq grew nil)
            (foreach o2 band
              (setq bh (cdr (assoc 5 (entget (car o2))))
                    xl (car (cadr o2)) xr (car (caddr o2)))
              (if (and (not (member bh ehs))
                       (<= xl (+ R 0.5))
                       (>= xr (- L 0.5)))
                (progn
                  (setq res (cons o2 res) ehs (cons bh ehs))
                  (if (< xl L) (setq L xl))
                  (if (> xr R) (setq R xr))
                  (setq grew T)
                )
              )
            )
          )
          ;; 按 x 左->右给出（次键用句柄，防同 x 被 vl-sort 当重复项吃掉）
          (mapcar 'car
            (vl-sort res
              '(lambda (a b)
                 (if (equal (car (cadr a)) (car (cadr b)) 1e-6)
                   (< (cdr (assoc 5 (entget (car a)))) (cdr (assoc 5 (entget (car b)))))
                   (< (car (cadr a)) (car (cadr b)))
                 )
               )
            )
          )
        )
      )
    )
  )
)



;; 按几何把「紧密相接的一整行」补齐 —— MCP 口给句柄时也必须过这道。
;;   给一批参照 -> (整行参照列表 本次新补进来的参照列表)
;;   手动口单点选择走的就是 gkc:row；MCP 口以前不走 → 只给半排句柄就
;;   只扩半排；而 plan 的「漏参照」自检只比【同名块定义】，于是紧贴成
;;   一行的高低两段（块名不同）被漏掉时一声不响（2026-09-18 实测）。
;;   只读：只用 entget / 包围盒，图一个字节都不动。
(defun gkc:rowfill (refs / snap out seen add eh r x)
  (setq snap (gkc:snapshot) out nil seen nil add nil)
  (foreach e refs
    (setq eh (cdr (assoc 5 (entget e))))
    (if (not (member eh seen))
      (progn (setq seen (cons eh seen)) (setq out (cons e out))))
  )
  (foreach e refs
    (foreach x (gkc:row e snap)
      (setq eh (cdr (assoc 5 (entget x))))
      (if (not (member eh seen))
        (progn (setq seen (cons eh seen)) (setq add (cons x add)) (setq out (cons x out))))
    )
  )
  (list (reverse out) (reverse add))
)

;; 按块定义把本排参照分组 -> ((块名 参照1 参照2 ...) ...)
(defun gkc:groups (row / bydef bn rs)
  (setq bydef nil)
  (foreach rb row
    (setq bn (cdr (assoc 2 (entget rb))))
    (setq rs (assoc bn bydef))
    (if rs
      (setq bydef (subst (cons bn (append (cdr rs) (list rb))) rs bydef))
      (setq bydef (cons (list bn rb) bydef))
    )
  )
  bydef
)

;; ================================================================
;; 四、参照上的属性读写
;; ================================================================

;; 读参照的某个属性值
(defun gkc:tag (e tag / sub ed out)
  (setq sub (entnext e) out nil)
  (while (and sub (= "ATTRIB" (cdr (assoc 0 (setq ed (entget sub))))))
    (if (= tag (cdr (assoc 2 ed))) (setq out (cdr (assoc 1 ed))))
    (setq sub (entnext sub))
  )
  out
)

;; 用 -INSERT 重造参照：属性交给 CAD 自己实例化，再把旧值按 tag 迁回去
;;   为什么不是「给旧参照补属性」—— 本机 COM 没有 BlockReference.AddAttribute
;;   （实测「未知名称: AddAttribute」），entmake 单发 ATTRIB 也被拒；
;;   只有命令通道 _. -INSERT 会正常实例化属性。代价：参照句柄会变。
;;   drop = 不迁值的 tag 列表（减模式：已删掉的那一槽）
(defun gkc:newref (e newbn drop / pe lay rot scx scy p3 sub ed kv kv2 tag nv ed2 ne nc cl at nz nh)
  (setq pe (entget e))
  (setq nz (cdr (assoc 210 pe)))
  (if (and nz (not (equal nz '(0.0 0.0 1.0) 1e-6)))
    nil
    (progn
      (setq lay (cdr (assoc 8 pe)))
      (setq rot (cdr (assoc 50 pe)) rot (if rot rot 0.0))
      (setq scx (cdr (assoc 41 pe)) scx (if scx scx 1.0))
      (setq scy (cdr (assoc 42 pe)) scy (if scy scy 1.0))
      (setq p3 (cdr (assoc 10 pe)))
      (if (null (caddr p3)) (setq p3 (list (car p3) (cadr p3) 0.0)))
      ;; 旧参照的 tag -> 值
      (setq kv nil sub (entnext e))
      (while (and sub (= "ATTRIB" (cdr (assoc 0 (setq ed (entget sub))))))
        (setq kv (cons (cons (cdr (assoc 2 ed)) (cdr (assoc 1 ed))) kv))
        (setq sub (entnext sub))
      )
      (setq cl (getvar "CLAYER") at (getvar "ATTREQ"))
      (setvar "CLAYER" lay)
      (setvar "ATTREQ" 0)
      (command "_.-INSERT" newbn p3 scx scy rot)
      (setvar "ATTREQ" at)
      (setvar "CLAYER" cl)
      (setq ne (entlast) nc 0)
      (if (and ne (= "INSERT" (cdr (assoc 0 (entget ne))))
               (equal (cdr (assoc 2 (entget ne))) newbn))
        (progn
          (setq sub (entnext ne))
          (while (and sub (= "ATTRIB" (cdr (assoc 0 (setq ed (entget sub))))))
            (setq nc (1+ nc)) (setq sub (entnext sub))
          )
          ;; 防御：定义里有 ATTDEF、插出来却 0 属性 —— 块头少「含非常量属性」位。
          ;; 视为失败：删掉刚插的、保留旧参照，宁可不动也不丢数据。
          (if (and (gkc:attdefs newbn) (= nc 0))
            (progn (entdel ne) nil)
            (progn
              (setq sub (entnext ne))
              (while (and sub (= "ATTRIB" (cdr (assoc 0 (setq ed (entget sub))))))
                (setq tag (cdr (assoc 2 ed)) kv2 (assoc tag kv))
                (if (and kv2 (not (member tag drop)))
                  (progn
                    (setq nv (cdr kv2) ed2 nil)
                    (foreach q ed (setq ed2 (cons (if (= (car q) 1) (cons 1 nv) q) ed2)))
                    (entmod (reverse ed2))
                  )
                )
                (setq sub (entnext sub))
              )
              ;; 句柄必须在 entdel 之前取 —— 删了就问不出来了。
              ;; 返回【新句柄】而不是实体名：MCP 侧要拿它改写后续写入的目标。
              (setq nh (cdr (assoc 5 (entget ne))))
              (entdel e)
              nh
            )
          )
        )
        nil
      )
    )
  )
)

;; 由参照的柜号推一个不重名的块名（表头块用「表头」）
(defun gkc:newname (bn e / tg cand i)
  (setq tg (gkc:trim (gkc:tag e "柜号")))
  (if (= tg "") (setq tg (if (wcmatch bn "*表头*") "表头" "排")))
  (setq cand (strcat bn "-" tg) i 1)
  (while (tblobjname "BLOCK" cand)
    (setq cand (strcat bn "-" tg "(" (itoa i) ")") i (1+ i))
  )
  cand
)

;; ================================================================
;; 四之二、块定义元数据（Comments）维护
;; ================================================================
;; 块定义 Comments 里存着 MCP 侧的自描述串：
;;   柜块    "ACAD_BOM_CABINET|size=..|slots=15|prefix=元件|slotrows=1,2,.."
;;   表头块  "ACAD_BOM_HEADER|of=<柜块名>|size=..|slots=15|.."
;; acad_att_bom 靠 of= 认表头块、acad_att_panel 靠 slots / slotrows 判
;; 「这套参数是不是已经建过」。克隆块定义时必须把它带走 ——
;;   丢了 = 表头认不到（表头参照会被当柜计出幽灵行）、
;;          att_panel 误判「没建过」→ 重画，本次加减槽白干。
;; （2026-09-18 修：原来块头 entmake 写死 '(4 . "") 直接清空。）

;; 串里最后一个逗号的位置，没有则 nil
(defun gkc:lastcomma (s / i r)
  (setq i 1 r nil)
  (while (<= i (strlen s))
    (if (= (substr s i 1) ",") (setq r i))
    (setq i (1+ i))
  )
  r
)

;; 按【字节】在 s 里找 ASCII 子串 pat，回 1 基字节下标（找不到 nil）。
;;   ★ 不用 vl-string-search：它回 0 基、串里带中文时索引单位还未必是字节，
;;   而 substr 一律按字节 —— 两套混用会错位。这里自己走字节，
;;   只用来定位 "of=" / "slots=" / "slotrows=" / "|" 这类 ASCII 标记。
(defun gkc:bfind (s pat / i n m r)
  (setq i 1 n (strlen s) m (strlen pat) r nil)
  (while (and (null r) (<= i (- n m -1)))
    (if (= (substr s i m) pat) (setq r i))
    (setq i (1+ i))
  )
  r
)

;; "键=" 后面那段值的区间 -> (值首字节下标 值后第一个字节下标)，均 1 基
(defun gkc:kvspan (r key / b e n)
  (setq b (gkc:bfind r key) n (strlen key))
  (if b
    (progn
      (setq e (gkc:bfind (substr r (+ b n)) "|"))
      (list (+ b n) (if e (+ b n e -1) (1+ (strlen r))))
    )
    nil
  )
)

;; 元数据串维护（只动三处，其余一字不改地带走）：
;;   pairs = ((旧块名 . 新块名) ...) —— 只用于 of= 的精确改名
;;   delta = +K 增 K 槽 / -K 减 K 槽 / 0 不动
;;         —— 改 slots 数值 + slotrows 补 / 去 K 项
(defun gkc:metafix (desc pairs delta / r sp v w n)
  (setq r desc)
  (if (and r (/= r ""))
    (progn
      ;; of= 精确改名：取 of= 的完整值，**等于**某个旧名才换。
      ;; 别用 vl-string-subst 做子串替换 —— 柜块名互为前缀时（「开关柜」对
      ;; 「开关柜-高压」）会把 of=开关柜-高压 误改成 of=开关柜-AH1-高压。
      (setq sp (gkc:kvspan r "of="))
      (if sp
        (progn
          (setq v (substr r (car sp) (- (cadr sp) (car sp))))
          (foreach pr pairs
            (if (and (car pr) (cdr pr) (= v (car pr)))
              (setq v (cdr pr))
            )
          )
          (setq r (strcat (substr r 1 (1- (car sp)))
                          v (substr r (cadr sp))))
        )
      )
      (if (/= delta 0)
        (progn
          ;; slots=NN —— 槽数
          (setq sp (gkc:kvspan r "slots="))
          (if sp
            (progn
              (setq v (atoi (substr r (car sp) (- (cadr sp) (car sp)))))
              (setq v (max 1 (+ v delta)))
              (setq r (strcat (substr r 1 (1- (car sp)))
                              (itoa v) (substr r (cadr sp))))
            )
          )
          ;; slotrows=1,2,3 —— 每槽行数；新槽是空槽 = 1 行，增 K 槽补 K 项、
          ;;   减 K 槽去 K 项（一项都不能少，否则 MCP 侧按它推的槽高会错位）
          (setq sp (gkc:kvspan r "slotrows="))
          (if sp
            (progn
              (setq v (substr r (car sp) (- (cadr sp) (car sp))))
              (if (> delta 0)
                (progn
                  (setq n 0)
                  (while (< n delta)
                    (setq v (strcat v ",1"))
                    (setq n (1+ n))
                  )
                )
                (progn
                  (setq n (- delta))
                  (while (> n 0)
                    (setq w (gkc:lastcomma v))
                    (if w (setq v (substr v 1 (1- w))))
                    (setq n (1- n))
                  )
                )
              )
              (setq r (strcat (substr r 1 (1- (car sp)))
                              v (substr r (cadr sp))))
            )
          )
        )
      )
    )
  )
  r
)

;; ================================================================
;; 五、定义侧：克隆重建（增 / 减一行）
;; ================================================================

;; 把点 p 的 y 换成 ny，z 有就留着、没有也不硬塞
;;   LWPOLYLINE 的顶点是 2D 点 (x y)，硬补一个 nil 会让 entmake
;;   报「DXF 组不正确: (10 0.0 -6.0 nil)」—— 踩过。
(defun gkc:setY (p ny)
  (cons (car p) (cons ny (cddr p)))
)

;; entmake 一个图元（去掉句柄 / 所有者这类会冲突的组码）
(defun gkc:emit (ed / out q)
  (setq out nil)
  (foreach q ed
    (if (not (member (car q) '(5 330 102 360)))
      (setq out (cons q out))))
  (entmake (reverse out))
)

;; 给「局部 y < B」的点整体 ±K*P；不需要动的原样返回（B 那条底线本身不动）
;;   K 省略 = 1（老调用方一个字节都不用改）
(defun gkc:yshift (ed mode B p k / need out q pt dy)
  (setq k (if (and k (> k 0)) k 1))
  (setq dy (if (= mode "A") (- (* p k)) (* p k)) need nil)
  (foreach q ed
    (if (and (member (car q) '(10 11)) (listp (cdr q)) (numberp (car (cdr q))))
      (if (< (cadr (cdr q)) (- B 1e-6)) (setq need T))))
  (if (not need)
    ed
    (progn
      (setq out nil)
      (foreach q ed
        (if (and (member (car q) '(10 11)) (listp (cdr q)) (numberp (car (cdr q))))
          (progn
            (setq pt (cdr q))
            (if (< (cadr pt) (- B 1e-6))
              (setq q (cons (car q) (gkc:setY pt (+ (cadr pt) dy)))))))
        (setq out (cons q out)))
      (reverse out)))
)

;; 行区分栏竖线（如「规格 | 数量」之间那根）：下端一律应当落在【行区
;;   底线】上。行区底线移动多少、它下端就跟着移多少 ——
;;     增 N 行：下端 -> B - N*P（不跟就会缺：新槽那一段没有竖线）
;;     减 N 行：下端 -> B + N*P（不跟就会拖到已删掉的行区里去）
;;   上端（行区顶）一律不动：加槽只往行区底部加，行区顶不挪。
;;   判据 = 「竖线（LINE、两端 x 相等）穿过了槽位行」：y 区间里落得下
;;   至少一个槽的 y。用这条而不是「下端 == B」是有原因的 —— 早期版本
;;   扩容时压根没管竖线，图上已经留下「下端落后若干个行距」的块，等值
;;   判据救不回来；「穿过槽位行」把这一段历史欠账一次补齐。
;;   ★ 必须放在 yshift 之后调。yshift 只挪「严格低于 B」的点，下端正好
;;     == B 的那一端它一个字节都不碰；顺序反过来 vfix 先改好的端点会被
;;     yshift 当成「低于 B 的点」再挪一次（增行变下移 2N 个行距）。
(defun gkc:vslot? (sls lo hi / hit)
  (setq hit nil)
  (foreach s sls
    (if (and (>= (cadr s) lo) (<= (cadr s) hi)) (setq hit T)))
  hit
)

(defun gkc:vfix (ed mode B p k sls / x1 y1 x2 y2 lo hi nlo out q)
  (if (/= "LINE" (cdr (assoc 0 ed)))
    ed
    (progn
      (setq k (if (and k (> k 0)) k 1))
      (setq x1 (cadr (assoc 10 ed)) y1 (caddr (assoc 10 ed))
            x2 (cadr (assoc 11 ed)) y2 (caddr (assoc 11 ed)))
      (setq lo (min y1 y2) hi (max y1 y2))
      (if (and (equal x1 x2 1e-6) (> hi (+ B 0.01)) (gkc:vslot? sls lo hi))
        (progn
          (setq nlo (if (= mode "A") (- B (* p k)) (+ B (* p k))))
          (setq out nil)
          (foreach q ed
            (cond
              ;; 只动【下端】那一端：它的 y 就是 lo。上端（hi）一个字不碰。
              ;; ★ 比 y 要用 (caddr q) —— q 是带组码的 (10 x y z)，
              ;;   写成 (caddr (cdr q)) 取到的是 z=0.0，判据永不成立，
              ;;   于是整条线被逐元素原样重建 = 看着像"没命中"（踩过）。
              ((and (= 10 (car q)) (listp (cdr q)) (equal (caddr q) lo 1e-6))
               (setq out (cons (cons 10 (gkc:setY (cdr q) nlo)) out)))
              ((and (= 11 (car q)) (listp (cdr q)) (equal (caddr q) lo 1e-6))
               (setq out (cons (cons 11 (gkc:setY (cdr q) nlo)) out)))
              (T (setq out (cons q out)))
            )
          )
          (reverse out)
        )
        ed
      )
    )
  )
)

;; 减 K 行时要丢掉的图元：
;;   ① 最底 K 槽（槽号 maxn-K+1 .. maxn）的全部 ATTDEF；
;;   ② 行区最底 K 条横线 —— B, B+P, ..., B+(K-1)P。
;;      （行区横线自底线 B 起向上每 P 一条，所以最底 K 条就是这个集合。）
;;   K 省略 = 1 —— 与改前「只删槽 maxn + 只删 B 那条线」完全等价。
(defun gkc:drop? (ed mode B maxn k / ty s lo j y hit)
  (if (/= mode "D")
    nil
    (progn
      (setq k (if (and k (> k 0)) k 1))
      (setq ty (cdr (assoc 0 ed)) lo (- maxn k -1))
      (cond
        ((= ty "ATTDEF")
         (setq s (gkc:slotno (cdr (assoc 2 ed))))
         (if (and s (>= s lo) (<= s maxn)) T nil))
        ((= ty "LINE")
         (if (equal (caddr (assoc 10 ed)) (caddr (assoc 11 ed)) 1e-6)
           (progn
             (setq y (caddr (assoc 10 ed)) hit nil)
             (setq j 0)
             (while (and (< j k) (null hit))
               (if (equal y (+ B (* j p)) 0.01) (setq hit T))
               (setq j (1+ j))
             )
             hit
           )
           nil
         ))
        (T nil))
    )
  )
)

;; 复制一条 ATTDEF 给新槽：tag 换号、默认值清空、点按 dy 平移
(defun gkc:mkat (d nnew dy / tg sfx out q pt)
  (setq tg (cdr (assoc 2 d)) sfx (gkc:suffix tg))
  (setq out nil)
  (foreach q d
    (cond
      ((member (car q) '(5 330 102 360)) nil)
      ((= 2 (car q)) (setq out (cons (cons 2 (gkc:mktag nnew sfx)) out)))
      ((= 3 (car q)) (setq out (cons (cons 3 (gkc:mktag nnew sfx)) out)))
      ((= 1 (car q)) (setq out (cons (cons 1 "") out)))
      ((and (member (car q) '(10 11)) (listp (cdr q)))
       (setq pt (cdr q))
       (setq out (cons (cons (car q) (gkc:setY pt (+ (cadr pt) dy))) out)))
      (T (setq out (cons q out)))
    )
  )
  (reverse out)
)

;; 复制一条线到新的 y
(defun gkc:mkline (d newy / out q pt)
  (setq out nil)
  (foreach q d
    (cond
      ((member (car q) '(5 330 102 360)) nil)
      ((and (member (car q) '(10 11)) (listp (cdr q)))
       (setq pt (cdr q))
       (setq out (cons (cons (car q) (gkc:setY pt newy)) out)))
      (T (setq out (cons q out)))
    )
  )
  (reverse out)
)

;; 重建一个「已经增减过 K 行」的新定义
;;   mode = "A" 增 / "D" 减；K 省略 = 1（与改前逐字等价）
;;   返回 (新定义名 新最大槽号 新槽ATTDEF列表)，失败返回 nil
(defun gkc:rebuild (bn newbn mode pairs k / bh base flags desc g0 B p maxn sls low hl addl newdefs ed d2 res j)
  (setq g0 (gkc:geo bn))
  (if (null g0)
    nil
    (progn
      (setq k (if (and k (> k 0)) k 1))
      (setq B (car g0) p (cadr g0) maxn (caddr g0) sls (nth 3 g0))
      (setq low (nth 4 g0) hl (nth 5 g0))
      (setq addl nil newdefs nil)
      (if (= mode "A")
        (progn
          ;; ① 最底槽那一套 ATTDEF 复制 K 份 —— 第 j 份给新槽 maxn+j，
          ;;    整体下移 j 个行距（j=1 那轮就是改前的那一次，逐字一致）
          (setq j 0)
          (while (< j k)
            (setq j (1+ j))
            (foreach ed (caddr low)
              (setq d2 (gkc:mkat ed (+ maxn j) (- (* p j))))
              (setq newdefs (cons d2 newdefs))
              (setq addl (cons d2 addl))
            )
            ;; ② 行区底线那条横线复制 K 条到 B-P, B-2P, ... B-K*P
            ;;    （各新槽的底线；B 那条原地留用，变成最上那个新槽的顶线）
            (if hl (setq addl (cons (gkc:mkline hl (- B (* p j))) addl)))
          )
        )
      )
      (setq bh (entget (tblobjname "BLOCK" bn)))
      (setq base (cdr (assoc 10 bh)) flags (cdr (assoc 70 bh)))
      ;; 原定义的 Comments（MCP 侧自描述串）必须带走 —— 见 gkc:metafix
      (setq desc (gkc:metafix (cdr (assoc 4 bh)) pairs (if (= mode "A") k (- k))))
      (entmake (list '(0 . "BLOCK") '(100 . "AcDbEntity") '(8 . "0")
                     '(100 . "AcDbBlockBegin") (cons 2 newbn) (cons 3 newbn)
                     ;; 70 的 bit2 = 「含非常量属性定义」——少了它，_.-INSERT
                     ;; 插这个定义出来一个属性都不带（实测踩过），必须补上
                     (cons 70 (logior (if flags flags 0) 2))
                     (cons 10 base) (cons 4 (if desc desc ""))))
      (foreach ed (gkc:ents bn)
        (if (not (gkc:drop? ed mode B maxn k))
          ;; 先 yshift（底部区整体平移），再把分栏竖线下端吸附到新的行区底线
          (gkc:emit (gkc:vfix (gkc:yshift ed mode B p k) mode B p k sls))
        )
      )
      (foreach ed addl (gkc:emit ed))
      (entmake '((0 . "ENDBLK") (100 . "AcDbEntity") (8 . "0") (100 . "AcDbBlockEnd")))
      (list newbn (+ maxn k) (reverse newdefs))
    )
  )
)

;; ================================================================
;; 六、主逻辑
;; ================================================================

;; 公共准备：算本排、按定义分组、算几何、查槽数一致
;;   返回 (pinfo groups maxn) 或 nil（pinfo 为 ((块名 B P 槽表 参照列表) ...)）
(defun gkc:prep (e row / snap gs pinfo nset g0)
  (if (null row) (setq row (gkc:row e (gkc:snapshot))))
  (if (null row)
    nil
    (progn
      (setq gs (gkc:groups row) pinfo nil)
      (foreach g gs
        (setq g0 (gkc:geo (car g)))
        (if g0 (setq pinfo (cons (list (car g) (car g0) (cadr g0) (nth 3 g0) (cdr g))
                                 pinfo))))
      (if (null pinfo)
        nil
        (progn
          (setq nset (mapcar '(lambda (z) (apply 'max (mapcar 'car (nth 3 z)))) pinfo))
          (list (reverse pinfo) gs nset)
        )
      )
    )
  )
)

;; ---------- 增 ----------
;; K 省略 = 1（老交互路径与改前一致）
(defun gkc:add (e row k / pr pinfo gs nset rep maxn nnew g nb B p r newbn res cnt pairs oh nh mapped)
  (setq k (if (and k (> k 0)) k 1))
  (setq rep "" mapped nil pairs nil)
  (setq pr (gkc:prep e row))
  (cond
    ((null pr) (setq rep (strcat SEP "[GKC] 选中的不是属性块，或本排没有槽位属性块 —— 退出。")))
    (T
     (setq pinfo (car pr) gs (cadr pr) nset (caddr pr))
     (setq rep (strcat SEP "[GKC] 整排增 " (itoa k) " 槽 —— 本排 " (itoa (length pinfo)) " 个块定义"
                       (if (apply '= nset)
                         ""
                         (strcat "（各块槽数不同，以下各按自身现有槽数分别 +" (itoa k) "，互不影响）"))))
     ;; 先把本排各定义的新名一次性算好（映射表）—— rebuild 要拿它把 meta 里的
     ;; of=（表头块指向柜块）一起改名。必须一次算完：等第一组建好新定义后
     ;; gkc:newname 再查重就会给出带 (1) 的名字，两块的名字就对不上了。
     (setq pairs nil)
     (foreach g pinfo
       (setq pairs (cons (cons (car g) (gkc:newname (car g) (car (nth 4 g)))) pairs))
     )
     (setq pairs (reverse pairs))
     (foreach g pinfo
       (setq nb (car g) B (cadr g) p (caddr g) r (nth 4 g))
       (setq maxn (apply 'max (mapcar 'car (nth 3 g))) nnew (+ maxn k))
       (setq newbn (cdr (assoc nb pairs)))
       (setq res (gkc:rebuild nb newbn "A" pairs k))
       (if res
         (progn
            (setq cnt 0)
            ;; 旧句柄必须在 newref 之前取 —— 它内部会把旧参照 entdel 掉。
            ;; 收集成 (旧句柄 新句柄 新块名)：MCP 侧靠它改写写入目标。
            (foreach x r
              (setq oh (cdr (assoc 5 (entget x))))
              (setq nh (gkc:newref x newbn nil))
              (if nh
                (progn
                  (setq cnt (1+ cnt))
                  (setq mapped (cons (list oh nh newbn) mapped))
                )
              )
            )
           (setq rep (strcat rep SEP "  [" nb "] 槽 " (itoa maxn) " -> " (itoa nnew)
                             "；新定义 " newbn
                             "；同步 " (itoa cnt) " 个参照（重造，句柄已变）"
                             "；底部区下移 " (rtos (* p k) 2 2)))
         )
         (setq rep (strcat rep SEP "  [" nb "] 定义重建失败，本块未动"))
       )
     )
     (setq rep (strcat rep SEP "[GKC] 完成。整排已 +" (itoa k) " 槽（各块按自身槽数分别计算），量一下图框是否有空间放高出来的部分。"))
    )
  )
  (gkc:say rep)
  ;; (报告串 参照映射 定义映射)
  ;;   参照映射 = ((旧句柄 新句柄 新块名) ...)   —— MCP 侧按它改写写入目标
  ;;   定义映射 = ((旧块名 . 新块名) ...)        —— MCP 侧按它重认 block / header
  (list rep (reverse mapped) (reverse pairs))
)

;; ---------- 减 ----------
;; K 省略 = 1（老交互路径与改前一致）
(defun gkc:del (e row k / pr pinfo gs nset rep minn bad g nb B p r tags newbn res cnt kv maxn pairs oh nh mapped)
  (setq k (if (and k (> k 0)) k 1))
  (setq rep "" mapped nil pairs nil)
  (setq pr (gkc:prep e row))
  (cond
    ((null pr) (setq rep (strcat SEP "[GKC] 选中的不是属性块，或本排没有槽位属性块 —— 退出。")))
    (T
     (setq pinfo (car pr) gs (cadr pr) nset (caddr pr))
     (setq minn (apply 'min nset))
     (cond
       ;; 减 K 槽后剩下的必须还有 *gkc:min* 个（K=1 时与改前那条 <= 完全等价）
       ((< (- minn k) *gkc:min*)
        (setq rep (strcat SEP "[GKC] 本排至少有一个块只剩 " (itoa minn) " 个槽位，减 "
                          (itoa k) " 个就到下限以下了，拒绝（下限 "
                          (itoa *gkc:min*) "）。")))
       (T
        ;; 闸门：整排每个块各自最底 K 槽的任意属性有非空值 -> 整次拒绝
        (setq bad nil)
        (foreach g pinfo
          (setq maxn (apply 'max (mapcar 'car (nth 3 g))))
          (foreach x (nth 4 g)
            (foreach tg (gkc:slot-tags-r (car g) (- maxn k -1) maxn)
              (setq kv (gkc:trim (gkc:tag x tg)))
              (if (/= kv "")
                (setq bad (cons (list (car g) (cdr (assoc 2 (entget x))) tg kv) bad))
              )
            )
          )
        )
        (if bad
          (setq rep (strcat SEP "[GKC] 减被拒绝 —— 本排至少一个块的最底 " (itoa k)
                            " 个槽还有数据，按规矩一个块都不动。共 " (itoa (length bad)) " 处：" SEP
                            (apply 'strcat
                                   (mapcar '(lambda (z)
                                              (strcat "   [" (car z) "] 句柄 " (cadr z)
                                                      "  " (caddr z) " = " (cadddr z) SEP))
                                           (reverse bad)))))
          (progn
            (setq rep (strcat SEP "[GKC] 整排减 " (itoa k) " 槽 —— 本排 " (itoa (length pinfo)) " 个块定义"
                              (if (apply '= nset)
                                (strcat "，删最底 " (itoa k) " 槽（槽号 " (itoa (- (car nset) k -1))
                                        ".." (itoa (car nset)) "，已确认该段全空）")
                                (strcat "（各块槽数不同，各自删自身最底 " (itoa k)
                                        " 槽，已确认该段全空）"))))
            ;; 新名映射一次算完（理由同 gkc:add —— rebuild 要拿它改 meta 的 of=）
            (setq pairs nil)
            (foreach g pinfo
              (setq pairs (cons (cons (car g) (gkc:newname (car g) (car (nth 4 g)))) pairs))
            )
            (setq pairs (reverse pairs))
            (foreach g pinfo
              (setq nb (car g) B (cadr g) p (caddr g) r (nth 4 g))
              (setq maxn (apply 'max (mapcar 'car (nth 3 g))))
              (setq tags (gkc:slot-tags-r nb (- maxn k -1) maxn))
              (setq newbn (cdr (assoc nb pairs)))
              (setq res (gkc:rebuild nb newbn "D" pairs k))
              (if res
                (progn
                    (setq cnt 0)
                    (foreach x r
                      (setq oh (cdr (assoc 5 (entget x))))
                      (setq nh (gkc:newref x newbn tags))
                      (if nh
                        (progn
                          (setq cnt (1+ cnt))
                          (setq mapped (cons (list oh nh newbn) mapped))
                        )
                      )
                    )
                  (setq rep (strcat rep SEP "  [" nb "] 槽 " (itoa maxn) ".."
                                    (itoa (- maxn k -1)) " 已删（"
                                    (itoa (length tags)) " 条属性）"
                                    "；新定义 " newbn
                                    "；同步 " (itoa cnt) " 个参照（重造，句柄已变）"
                                    "；底部区上移 " (rtos (* p k) 2 2)))
                )
                (setq rep (strcat rep SEP "  [" nb "] 定义重建失败，本块未动"))
              )
            )
            (setq rep (strcat rep SEP "[GKC] 完成。整排已 -" (itoa k) " 槽（各块按自身槽数分别计算）。"))
          )
        )
       )
     )
    )
  )
  (gkc:say rep)
  ;; (报告串 参照映射 定义映射) —— 口径同 gkc:add
  (list rep (reverse mapped) (reverse pairs))
)

;; ================================================================
;; 七、按句柄扩容（MCP 导入工具专用入口）
;; ================================================================
;; 谁找块：MCP 侧。attkit import 比对差异时，导入模板里本来就有块名 /
;;         柜号 / 句柄，AI 从差异里直接就知道「哪一排、要加几个槽」。
;;         所以这一节【不做几何识别来挑块】，只做「按给定参照 + 给定
;;         槽数，一次扩到位」；但给进来的参照会先过一遍 gkc:rowfill，
;;         把「紧密相接的一整行」补齐（见下）。
;;
;; 两个入口（都能被 acad_run_lisp 直接调）：
;;   (gkc:plan   "332,35B,384,3AD,3D6,3FF,428" 2)   只读清单，图不动；返回报告串
;;       （两者都先按几何补齐整行；漏给的句柄会被自动纳入并点名）
;;   (gkc:expand "332,35B,384,3AD,3D6,3FF,428" 2)   执行；返回
;;       "[GKC-RESULT]报告…|@defs=旧名>新名;…|@refs=旧柄>新柄>新块名;…|@bad=…"
;;       MCP 侧靠 @refs 改写写入目标（句柄全变）、靠 @defs 重认块名
;;       （扩容后本排参照换成「原名-柜号」，旧名字一个参照都找不到了）
;;   K 给负数 = 减槽（减的闸门照旧：该段有值就整次拒绝）
;;
;; ★ 一次只传【一排】的句柄。两排万一共用同一个块定义，一起传会把两排
;;   并成一个新定义（新定义名只取一个柜号）—— 想处理两排就调两次。
;;
;; 句柄少给几个【按几何自动补】：整行（同 y 带 + x 相接）一块不落。
;; 以前不补，只给半排句柄就静默只扩一半 —— 高低压紧贴成一行时最典型：
;; 给低压 4 个句柄，高压两块原地不动、底边错开一个行距，而 plan 的
;; 「漏参照」自检只比同名块定义，一条警告都不出（2026-09-18 踩到）。
;; 补齐后报告里点名新纳入的块；传的句柄横跨两排时两排都会被纳入。
;; —— 两排共用【同一个块定义】时仍要小心：一起传会把两排并成一个新
;; 定义（新定义名只取一个柜号），这种情况分两次调。

;; 句柄串 "332,35B" 或 ("332" "35B") -> 句柄字符串列表
(defun gkc:tokens (s / res cur i ch)
  (if (listp s)
    s
    (progn
      (setq res nil cur "" i 1)
      (while (<= i (strlen s))
        (setq ch (substr s i 1))
        (if (or (= ch ",") (= ch ";") (= ch " "))
          (progn
            (if (/= cur "") (setq res (cons cur res)))
            (setq cur "")
          )
          (setq cur (strcat cur ch))
        )
        (setq i (1+ i))
      )
      (if (/= cur "") (setq res (cons cur res)))
      (reverse res)
    )
  )
)

;; 句柄串 -> (实体名列表 无效句柄列表)
;;   句柄是本机唯一稳的定位方式：柜号改了不怕，但重造参照后句柄必变。
(defun gkc:refs-from (s / res bad h e)
  (setq res nil bad nil)
  (foreach h (gkc:tokens s)
    (setq e (handent h))
    (if (and e (= "INSERT" (cdr (assoc 0 (entget e)))))
      (setq res (cons e res))
      (setq bad (cons h bad))
    )
  )
  (list (reverse res) (reverse bad))
)

;; 参照列表里有没有这个句柄
(defun gkc:inh (h lst / r)
  (setq r nil)
  (foreach e lst
    (if (= h (cdr (assoc 5 (entget e)))) (setq r T))
  )
  r
)

;; 本排被哪个块整包住（就是图框）？返回 (下边距 图框块名)，算不出 nil。
;;   下边距 = 本排最低边 - 图框最低边。扩槽让块往下长，它就变小 ——
;;   变成负数就是破框，所以要在动手【之前】报出来。
;;   包住本排的块可能不止一个（外框套内框），取面积最小的那个。
(defun gkc:clearance (refs / x0 y0 x1 y1 bb e snap best mn2 mx2 o2 ar a1 a2)
  (setq x0 1e9 y0 1e9 x1 -1e9 y1 -1e9)
  (foreach e refs
    (setq bb (gkc:bb e))
    (if bb
      (progn
        (if (< (car (car bb)) x0) (setq x0 (car (car bb))))
        (if (< (cadr (car bb)) y0) (setq y0 (cadr (car bb))))
        (if (> (car (cadr bb)) x1) (setq x1 (car (cadr bb))))
        (if (> (cadr (cadr bb)) y1) (setq y1 (cadr (cadr bb))))
      )
    )
  )
  (if (> x0 x1)
    nil
    (progn
      (setq snap (gkc:snapshot) best nil ar nil)
      (foreach o2 snap
        (setq mn2 (cadr o2) mx2 (caddr o2))
        (if (and (<= (car mn2) (+ x0 0.01)) (>= (car mx2) (- x1 0.01))
                 (<= (cadr mn2) (+ y0 0.01)) (>= (cadr mx2) (- y1 0.01)))
          (progn
            (setq a1 (* (- (car mx2) (car mn2)) (- (cadr mx2) (cadr mn2))))
            (if (or (null best) (< a1 ar))
              (progn (setq best o2 ar a1))
            )
          )
        )
      )
      (if best
        (list (- y0 (cadr (cadr best))) (cdr (assoc 2 (entget (car best)))))
        nil
      )
    )
  )
)

;; 只读预演：列出将动哪些定义、槽数怎么变、块底移多少、离图框还剩多少。
;;   全用只读调用（gkc:bb / gkc:snapshot / handent），图一个字节都不动。
(defun gkc:plan (s k / kk rf refs bad f1 addd pr pinfo rep g nb r maxn all nall i e2 miss cl pv after)
  (setq kk (if (and k (/= k 0)) k 1))
  (setq rf (gkc:refs-from s) refs (car rf) bad (cadr rf))
  (setq rep (strcat "[GKC 预演] 拟 " (if (> kk 0) "+" "") (itoa kk) " 槽；只读，图不会动。"))
  (if bad
    (setq rep (strcat rep SEP "  无效句柄 " (itoa (length bad)) " 个：" (gkc:join bad ","))))
  ;; 先按几何补齐整行（只读）：手动口单点走的就是 gkc:row，MCP 口以前
  ;; 不走 —— 只给半排句柄就静默只扩一半，故这里补上，并把补进来的点名。
  (setq f1 (gkc:rowfill refs) refs (car f1) addd (cadr f1))
  (if refs
    (setq rep (strcat rep SEP "  句柄 " (itoa (- (length refs) (length addd)))
                      " 个 → 按几何补齐整行 " (itoa (length refs)) " 个"
                      (if addd
                        (strcat "；补进来的 " (itoa (length addd)) " 个："
                                (gkc:join
                                  (mapcar '(lambda (x) (strcat (cdr (assoc 5 (entget x))) "="
                                                               (cdr (assoc 2 (entget x)))))
                                          addd) " "))
                        "（给的就是整行）"))))
  (cond
    ((null refs)
     (setq rep (strcat rep SEP "  没有有效参照，没法继续。")))
    (T
     (setq pr (gkc:prep (car refs) refs))
     (if (null pr)
       (setq rep (strcat rep SEP "  给出的参照里没有带槽位的属性块"
                         "（认的是块定义里的「" *gkc:pfx* "NN」属性）—— 没法扩容。"))
       (progn
         (setq pinfo (car pr))
         (setq pv (caddr (car pinfo)))
         (foreach g pinfo
           (setq nb (car g) r (nth 4 g) maxn (apply 'max (mapcar 'car (nth 3 g))))
           (setq rep (strcat rep SEP "  [" nb "] 槽 " (itoa maxn) " -> " (itoa (+ maxn kk))
                             "；行距 " (rtos (caddr g) 2 2)
                             "；块底" (if (> kk 0) "下移 " "上移 ")
                             (rtos (abs (* (caddr g) kk)) 2 2)
                             "；重造 " (itoa (length r)) " 个参照（句柄会变）"))
           ;; 同定义的参照有没有漏在清单外
           (setq all (ssget "X" (list '(0 . "INSERT") (cons 2 nb))))
           (setq nall (if all (sslength all) 0) miss 0 i 0)
           (while (< i nall)
             (setq e2 (ssname all i))
             (if (not (gkc:inh (cdr (assoc 5 (entget e2))) r)) (setq miss (1+ miss)))
             (setq i (1+ i))
           )
           (if (> miss 0)
             (setq rep (strcat rep SEP "      ! 同定义还有 " (itoa miss)
                               " 个参照没在清单里 —— 它们会留在旧定义上，槽数跟本排对不上")))
         )
         (setq cl (gkc:clearance refs))
         (if cl
           (progn
             (setq after (- (car cl) (* pv kk)))
             (setq rep (strcat rep SEP "  图框 [" (cadr cl) "] 下边距 "
                               (rtos (car cl) 2 2) " -> " (rtos after 2 2)))
             (if (< after 0.0)
               (setq rep (strcat rep SEP "      ! 扩完会破框（低于图框下边 "
                                 (rtos (abs after) 2 2) "）—— 先挪框 / 改图幅，别急着加")))
           )
           (setq rep (strcat rep SEP "  没找到包住本排的图框，破框与否自己看图"))
         )
       )
     )
    )
  )
  (gkc:say rep)
  rep
)

;; 执行：按句柄对整排一次扩 K 槽（K 负 = 减）。
;; 返回【定界字符串】而不是 LISP 表 —— acad_run_lisp 是用 vl-princ-to-string
;; 把结果写文件回传的，直接回表会变成带转义引号的打印格式，Python 侧没法读。
;; 格式（人读报告在前，|@ 开头的是机器段）：
;;   [GKC-RESULT]报告…|@defs=旧块名>新块名;…|@refs=旧句柄>新句柄>新块名;…|@bad=句柄,…
(defun gkc:expand (s k / kk rf refs bad f1 addd frep row raw res rep mapped pairs out)
  (setq kk (if (and k (/= k 0)) k 1))
  (setq rf (gkc:refs-from s) refs (car rf) bad (cadr rf))
  (setq rep "" mapped nil pairs nil frep "")
  ;; 补齐整行（口径同 plan）—— 只给半排句柄时以前静默只扩一半
  (setq f1 (gkc:rowfill refs) refs (car f1) addd (cadr f1))
  (if addd
    (setq frep (strcat "[GKC] 按几何补齐整行：新纳入 " (itoa (length addd)) " 个（"
                       (gkc:join
                         (mapcar '(lambda (x) (strcat (cdr (assoc 5 (entget x))) "="
                                                      (cdr (assoc 2 (entget x)))))
                                 addd) " ")
                       "）" SEP)))
  (cond
    ((null refs)
     (setq rep "[GKC] 没有有效参照（句柄无效或已删除），图没动。"))
    (T
     (setq row refs)
     ;; 静默执行；quiet 的恢复放在 vl-catch-all-apply 之后 ——
     ;; add/del 内部报错也不会把全局开关留在 T 上（否则下次手动调用就哑了）。
     (setq *gkc:quiet* T)
     (setq raw (vl-catch-all-apply
                 (if (> kk 0) 'gkc:add 'gkc:del)
                 (list (car row) row (if (> kk 0) kk (- kk)))))
     (setq *gkc:quiet* nil)
     (if (vl-catch-all-error-p raw)
       (setq rep (strcat "[GKC] 执行报错，图可能只改了一部分 —— 核对后重来："
                         (vl-catch-all-error-message raw)))
       (setq res raw rep (car res) mapped (cadr res) pairs (caddr res))
     )
    )
  )
  (setq out (strcat "[GKC-RESULT]" frep rep
                    "|@defs=" (if pairs
                                (apply 'strcat
                                       (mapcar '(lambda (z) (strcat (car z) ">" (cdr z) ";"))
                                               pairs))
                                "")
                    "|@refs=" (if mapped
                                (apply 'strcat
                                       (mapcar '(lambda (z)
                                                  (strcat (car z) ">" (cadr z) ">" (caddr z) ";"))
                                               mapped))
                                "")
                    "|@bad=" (if bad (gkc:join bad ",") "")))
  out
)

;; ================================================================
;; 八、人机界面（命令 GKC）
;; ================================================================
;; 流程：GKC -> 点选本排任意一个属性块 -> 程序自己认出整排 -> 弹窗
;;       点【增】/【减】累计行数（实时显示）-> 确定 -> 一次执行

;; ---------- 现场保护 ----------
;; 执行期间要临时改的系统变量：
;;   OSMODE  —— 关对象捕捉，否则 -INSERT 的插入点会被吸到附近的端点上，整排错位
;;   CMDECHO —— 关命令回显，命令行不刷屏
;;   非世界坐标系时临时切回 WCS（-INSERT 的点按当前 UCS 解释，而图元里取的是 WCS）
;;   UNDO 编组 —— 整次操作只算一个撤销步骤
(setq *gkc:sv* nil *gkc:sv-ucs* nil *gkc:sv-undo* nil)

(defun gkc:env-save ()
  (setq *gkc:sv* (list (getvar "OSMODE") (getvar "CMDECHO")
                       (getvar "ATTREQ") (getvar "CLAYER")))
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)
  (command "_.UNDO" "_BEGIN")
  (setq *gkc:sv-undo* T)
  (if (= 0 (getvar "WORLDUCS"))
    (progn (command "_.UCS" "_W") (setq *gkc:sv-ucs* T)))
  (princ)
)

(defun gkc:env-restore ()
  (if *gkc:sv*
    (progn
      (repeat 3 (if (> (getvar "CMDACTIVE") 0) (command)))
      (if *gkc:sv-ucs* (command "_.UCS" "_P"))
      (if *gkc:sv-undo* (command "_.UNDO" "_END"))
      (setvar "ATTREQ" (nth 2 *gkc:sv*))
      (vl-catch-all-apply 'setvar (list "CLAYER" (nth 3 *gkc:sv*)))
      (setvar "OSMODE" (nth 0 *gkc:sv*))
      (setvar "CMDECHO" (nth 1 *gkc:sv*))
      (setq *gkc:sv* nil *gkc:sv-ucs* nil *gkc:sv-undo* nil)
    )
  )
  (princ)
)

;; ---------- 点选 ----------
;; 点一个块参照；点空 / 点到不带槽位的块 -> 提示后重点；回车或 Esc 退出。
;; 返回实体名，退出返回 nil。
(defun gkc:pick ( / sel e bn done res)
  (setq done nil res nil)
  (while (not done)
    (setvar "ERRNO" 0)
    (setq sel (entsel "\n请点选本排任意一个属性块 <退出>: "))
    (cond
      (sel
       (setq e (car sel))
       (if (= "INSERT" (cdr (assoc 0 (entget e))))
         (progn
           (setq bn (cdr (assoc 2 (entget e))))
           (if (gkc:maxslot bn)
             (setq res e done T)
             (princ (strcat "\n  块 [" bn "] 里没有槽位属性（" *gkc:pfx* "NN...），请重新点选。"))))
         (princ "\n  点到的不是块参照，请重新点选。")))
      ((= (getvar "ERRNO") 7) (princ "\n  没点中对象，请重试。"))
      (T (setq done T))
    )
  )
  res
)

;; ---------- 减的数据闸门（界面实时用，规则与 gkc:del 里那道完全一致）----------
;; 返回有数据的位置列表 ((柜号 属性tag) ...)，全空返回 nil
(defun gkc:del-bad (pinfo k / bad g maxn x tg tags)
  (setq bad nil)
  (foreach g pinfo
    (setq maxn (apply 'max (mapcar 'car (nth 3 g))))
    (setq tags (gkc:slot-tags-r (car g) (- maxn k -1) maxn))
    (foreach x (nth 4 g)
      (foreach tg tags
        (if (/= "" (gkc:trim (gkc:tag x tg)))
          (setq bad (cons (list (gkc:trim (gkc:tag x "柜号")) tg) bad))
        )
      )
    )
  )
  (reverse bad)
)

;; ---------- 对话框（DCL 运行时写成临时文件）----------
(defun gkc:mkdcl ( / fn f ln)
  (setq fn (vl-filename-mktemp "gkc" nil ".dcl"))
  (setq f (open fn "w"))
  (if (null f)
    nil
    (progn
      (foreach ln
        '("gkc_dlg : dialog {"
          "  label = \"属性块 - 槽位增减\";"
          "  : column {"
          "    : text { key = \"head\"; width = 68; }"
          "    : list_box { key = \"lst\"; width = 68; height = 8; }"
          "    : text { label = \"每点一次[增]或[减]改变 1 行，可反复点，增减相互抵消；[归零]重来。\"; }"
          "    : row {"
          "      alignment = centered;"
          "      : button { key = \"plus\"; label = \"增  (+1 行)\"; width = 16; fixed_width = true; }"
          "      : button { key = \"minus\"; label = \"减  (-1 行)\"; width = 16; fixed_width = true; }"
          "      : button { key = \"reset\"; label = \"归零\"; width = 10; fixed_width = true; }"
          "    }"
          "    : boxed_column {"
          "      label = \"本次变化\";"
          "      : text { key = \"delta\"; alignment = centered; width = 60; }"
          "    }"
          "    : text { key = \"frame\"; width = 68; }"
          "    : text { key = \"warn\"; width = 68; }"
          "    : text { key = \"warn2\"; width = 68; }"
          "  }"
          "  : row {"
          "    alignment = centered;"
          "    : button { key = \"accept\"; label = \"确定\"; is_default = true; width = 12; fixed_width = true; }"
          "    : button { key = \"cancel\"; label = \"取消\"; is_cancel = true; width = 12; fixed_width = true; }"
          "  }"
          "}")
        (write-line ln f)
      )
      (close f)
      fn
    )
  )
)

;; 界面状态（回调函数只能读全局变量）
;;   *gkc:dlg-k*      累计增减行数（正 = 增，负 = 减）
;;   *gkc:dlg-msg*    点不动时的提示
;;   *gkc:dlg-pinfo*  gkc:prep 算出的分组几何
;;   *gkc:dlg-ents*   每个块一项 (柜号 块名 现有槽数或nil)
;;   *gkc:dlg-minn* / *gkc:dlg-maxn*  本排各定义槽数的最小 / 最大
;;   *gkc:dlg-pv*     本排最大行距
;;   *gkc:dlg-cl*     (图框下边距 图框块名) 或 nil
(setq *gkc:dlg-k* 0 *gkc:dlg-msg* "" *gkc:dlg-pinfo* nil *gkc:dlg-ents* nil
      *gkc:dlg-minn* 1 *gkc:dlg-maxn* 1 *gkc:dlg-pv* 6.0 *gkc:dlg-cl* nil)

;; 按当前累计行数刷新整个界面
(defun gkc:dlg-refresh ( / d i z bad after lab0)
  (setq d *gkc:dlg-k*)
  ;; ① 本次变化
  (set_tile "delta"
    (cond
      ((> d 0) (strcat "本次： 增加 " (itoa d) " 行"))
      ((< d 0) (strcat "本次： 减少 " (itoa (- d)) " 行"))
      (T "本次： 0 行（未改动）")))
  ;; ② 每个块：现有槽数 -> 执行后槽数
  (start_list "lst")
  (setq i 0)
  (foreach z *gkc:dlg-ents*
    (setq i (1+ i))
    (add_list
      (strcat (itoa i) ". 柜号 " (if (= (car z) "") "-" (car z))
              "   [" (cadr z) "]   "
              (cond
                ((null (caddr z)) "无槽位，不受影响")
                ((= d 0) (strcat "槽数 " (itoa (caddr z))))
                (T (strcat "槽数 " (itoa (caddr z)) " -> " (itoa (+ (caddr z) d))))))))
  (end_list)
  ;; ③ 图框余量（增会变小，减会变大）
  (if *gkc:dlg-cl*
    (progn
      (setq after (- (car *gkc:dlg-cl*) (* *gkc:dlg-pv* d)))
      (set_tile "frame"
        (strcat "图框 [" (cadr *gkc:dlg-cl*) "] 下边距： "
                (rtos (car *gkc:dlg-cl*) 2 2) " -> " (rtos after 2 2)
                (if (< after 0.0) "   (会超出图框！)" ""))))
    (set_tile "frame" "没找到包住本排的图框（会不会超框请自己看图）"))
  ;; ④ 减的数据闸门 + 提示
  (setq bad (if (< d 0) (gkc:del-bad *gkc:dlg-pinfo* (- d)) nil))
  (cond
    (bad
     (setq lab0 (car (car bad)))
     (set_tile "warn" (strcat "不能减：最底 " (itoa (- d)) " 行还有数据（共 "
                              (itoa (length bad)) " 处）"))
     (set_tile "warn2" (strcat "例如：柜号 " (if (= lab0 "") "-" lab0)
                               " 的 " (cadr (car bad)) " 不是空的")))
    (T
     (set_tile "warn" *gkc:dlg-msg*)
     (set_tile "warn2" "")))
  ;; ⑤ 确定键：有改动且没被闸门挡住才可点
  (mode_tile "accept" (if (and (/= d 0) (null bad)) 0 1))
)

;; 点一次增(+1) / 减(-1)
(defun gkc:dlg-step (s / nk)
  (setq nk (+ *gkc:dlg-k* s))
  (setq *gkc:dlg-msg* "")
  (cond
    ((and (< nk 0) (< (+ *gkc:dlg-minn* nk) *gkc:min*))
     (setq *gkc:dlg-msg*
           (strcat "已到槽数下限（至少保留 " (itoa *gkc:min*) " 个槽），不能再减")))
    ((> (+ *gkc:dlg-maxn* nk) 99)
     (setq *gkc:dlg-msg* "槽号最大 99，不能再增"))
    (T (setq *gkc:dlg-k* nk))
  )
  (gkc:dlg-refresh)
)

;; 归零
(defun gkc:dlg-reset ()
  (setq *gkc:dlg-k* 0 *gkc:dlg-msg* "")
  (gkc:dlg-refresh)
)

;; 弹窗。返回累计行数（>0 增，<0 减，0 = 取消或没改）
(defun gkc:dialog (row / pr nset cache x bn lab nn dcl id r)
  (setq pr (gkc:prep (car row) row))
  (if (null pr)
    (progn (princ "\n本排没有带槽位的属性块，退出。") 0)
    (progn
      (setq nset (caddr pr))
      (setq *gkc:dlg-pinfo* (car pr)
            *gkc:dlg-minn* (apply 'min nset)
            *gkc:dlg-maxn* (apply 'max nset)
            *gkc:dlg-pv* (apply 'max (mapcar 'caddr (car pr)))
            *gkc:dlg-k* 0
            *gkc:dlg-msg* ""
            *gkc:dlg-ents* nil
            *gkc:dlg-cl* (gkc:clearance row))
      ;; 本排每个块一项，顺序 = 从左到右
      (setq cache nil nn 0)
      (foreach x row
        (setq bn (cdr (assoc 2 (entget x))))
        (if (not (assoc bn cache))
          (setq cache (cons (cons bn (gkc:maxslot bn)) cache)))
        (if (cdr (assoc bn cache)) (setq nn (1+ nn)))
        (setq lab (gkc:trim (gkc:tag x "柜号")))
        (setq *gkc:dlg-ents*
              (append *gkc:dlg-ents* (list (list lab bn (cdr (assoc bn cache))))))
      )
      (setq dcl (gkc:mkdcl))
      (if (null dcl)
        (progn (princ "\n临时对话框文件写不出来（检查 TEMP 目录权限），退出。") 0)
        (progn
          (setq id (load_dialog dcl))
          (if (not (new_dialog "gkc_dlg" id))
            (progn (princ "\n对话框加载失败，退出。") (setq r 0))
            (progn
              (set_tile "head"
                (strcat "已识别本排共 " (itoa (length row)) " 个属性块（"
                        (itoa nn) " 个带槽位），当前槽数 "
                        (if (= *gkc:dlg-minn* *gkc:dlg-maxn*)
                          (itoa *gkc:dlg-minn*)
                          (strcat (itoa *gkc:dlg-minn*) " ~ " (itoa *gkc:dlg-maxn*) "（各块不同）"))))
              (action_tile "plus" "(gkc:dlg-step 1)")
              (action_tile "minus" "(gkc:dlg-step -1)")
              (action_tile "reset" "(gkc:dlg-reset)")
              (action_tile "accept" "(done_dialog 1)")
              (action_tile "cancel" "(done_dialog 0)")
              (gkc:dlg-refresh)
              (setq r (start_dialog))
            )
          )
          (unload_dialog id)
          (vl-file-delete dcl)
          (if (= r 1) *gkc:dlg-k* 0)
        )
      )
    )
  )
)

;; ---------- 命令 ----------
(defun c:GKC ( / *error* e row k res)
  (defun *error* (msg)
    (gkc:env-restore)
    (if (and msg (not (member msg '("Function cancelled" "quit / exit abort" "console break"))))
      (princ (strcat "\n[GKC] 出错：" msg "  （已恢复系统变量；若图已被改动，可 U 撤销）")))
    (princ)
  )
  (setq *gkc:quiet* nil)
  (princ "\n[GKC] 属性块槽位增减 —— 点选本排任意一个属性块，弹窗里点 增 / 减 累计行数，确定后执行。")
  (setq e (gkc:pick))
  (if e
    (progn
      (setq row (gkc:row e (gkc:snapshot)))
      (if (null row)
        (princ "\n没能识别出本排，退出。")
        (progn
          (setq k (gkc:dialog row))
          (if (= k 0)
            (princ "\n未改动，图没动。")
            (progn
              (gkc:env-save)
              (setq res (if (> k 0)
                          (gkc:add (car row) row k)
                          (gkc:del (car row) row (- k))))
              (gkc:env-restore)
              (if (and res (cadr res))
                (princ "\n可用 U / Ctrl+Z 一步撤销本次操作。"))
            )
          )
        )
      )
    )
    (princ "\n已退出。")
  )
  (princ)
)

(princ "\n[属性块-槽位增减] 已加载，命令：GKC（点选本排任意一块 -> 弹窗点 增 / 减 -> 确定）")
(princ)
