;;; ============================================================
;;; 属性块-点排填数.lsp                      命令：DTP / DTPA
;;; ------------------------------------------------------------
;;; 作用
;;;   点一台柜 -> 把【这一排】柜的柜级字段摊成一张表，一个面板集中改：
;;;     柜号 / 柜型 / 用途 / 尺寸 / 分支电流 / 功率 / 备注
;;;   一列 = 一台柜，按插入点 x 从左往右排（与图上排列方向一致）；
;;;   逐格改完按「写入」一次落图。柜号也在表里，能改。
;;;   DTPA = 不用点，全图所有柜一次列出
;;;          （排序 = y 降序、x 升序，即从上往下、每排从左往右）。
;;;
;;; 一排怎么认
;;;   点哪台柜，就拿哪台的 y 当基准；|y - 基准| < 8 的柜都算这一排。
;;;   上下两排的 y 通常差几十往上，不会串台。
;;;   判「点在哪个柜」用块定义几何 x 区间 + 插入点（与 DTF 同口径，
;;;   不用包围盒 —— 属性文字超宽会把包围盒撑大，隔壁柜会被串进来）。
;;;
;;; 分页
;;;   每页最多 8 台（*dtq:perpage*）—— 列再多就顶出屏幕了。
;;;   列宽按本页台数自动收缩，8 台也塞得进屏幕。
;;;   翻页时本页输入先存内存，「写入」才落图；来回翻不丢数据。
;;;
;;; 认块 / 读写
;;;   与 PLT / DTF 同一套内核：认「柜号」标签认柜块，
;;;   读写走 plt:atts / plt:val / plt:put。块名被 GKC 之类滚过也不受影响。
;;;
;;; 不存盘
;;;   与 PLT / DTF 同规矩：只改内存，存盘 / 撤销归用户。
;;;
;;; 界面（DCL）
;;;   每次弹窗前把 DCL 写一份到 %TEMP%\dtq_dlg.dcl（列数随排变，必须现生成），
;;;   不改工程目录里的任何文件。
;;;
;;; 界面为什么不卡（2026-09-25 修「框关不掉」）
;;;   ① 按钮回调里【只】调 done_dialog —— 回调表达式不可能抛错，
;;;      也就不会出现「点了没反应、框关不掉」。
;;;      取值挪到 start_dialog 返回之后、unload_dialog 之前做。
;;;      旧写法在回调里先 dtq:pull 再 done_dialog，pull 一抛错
;;;      done_dialog 就永远执行不到，框就卡在那儿。
;;;   ② 每个编辑框都绑 action 实时存值（*dtq:live*）——
;;;      在格子里打完字不按回车、直接点按钮，也能拿到最新值。
;;;      旧写法只靠 get_tile，DCL 没绑 action 的格子不提交，会取到旧值。
;;;   ③ 写入 / 取消按钮的 key 直接用 DCL 内置的 accept / cancel，
;;;      回车、ESC 和鼠标点是同一条路 —— ESC 也能关框。
;;;      旧写法按钮 key 自造成 bok / bno，ESC 走内置 cancel 却没绑，ESC 无效。
;;;   ④ 取值函数整体 catch，某个键对不上也不炸。
;;;   ⑤ 弹窗时把焦点给第一个输入框，点一下就落在控件上。
;;;
;;; 自动编柜号（右侧固定区按钮，2026-09-25 新增）
;;;   面板右侧新开一列固定区（不随翻页消失），放「自动编柜号」按钮。
;;;   点一下：以本页第 1 台（最左列）当前的柜号为基准，
;;;     拆成「前缀 + 末尾数字」，后面每一列 = 前缀不变、数字依次 +1，
;;;     位数不够补前导 0（如 A01 -> A02、A03……）。
;;;   只改本页格子里的实时显示值，不越页、不落图 —— 仍要按「写入」才生效。
;;;   第一台柜号结尾没有数字（比如没填、或不是数字收尾）时会弹提示，不动数据。
;;;
;;; 自动计算分支电流（右侧固定区按钮，新增）
;;;   点「自动计算」弹子框：先选电压（35 / 20 / 10 / 6 / 0.4KV 五档常用值，
;;;     外加自定义 1 档可自己填数，默认选中 10KV；电压档位一点即记，
;;;     换了档位再点「计算」一定按新档位算，不会停在旧档位），
;;;     下面自动列出本页「容量功率」能读出数值的柜号（没数据的不列），
;;;     默认全选，不想算的自己去掉勾。
;;;   按「计算」：对勾中的每台柜，按 I = 容量或功率(kVA/kW) / (根号3 × 电压(kV))
;;;     算出额定电流，填进该柜「分支电流」的实时格子（不动没勾的柜、
;;;     不越页、不落图，仍要按「写入」才生效）。
;;;   容量功率格子里带单位文字（如 "50kW"）也认，只取开头的数字。
;;;
;;; 一键清空（右侧固定区按钮，新增）
;;;   点「一键清空」弹子框：按面板行标题列 6 个按钮（柜型 / 用途 / 尺寸 /
;;;     分支电流 / 功率 / 备注），外加一个「全部」按钮——「柜号」不在其中，
;;;     避免连认柜的依据一起清掉。
;;;   点哪行清哪行（本页所有柜的该字段清空）；点「全部」= 本页除柜号外
;;;     其余各行一次清空。只改实时值，不越页、不落图，仍要按「写入」才生效。
;;;
;;; 编码：ANSI/GBK 无 BOM
;;; 依赖：属性块-批量填数.lsp（同目录，本文件自己 load）
;;; ============================================================
(vl-load-com)
(if (null SEP) (setq SEP (chr 10)))

;; ---------------- 依赖（PLT 内核） ----------------
(setq dtq:dep "D:/kk三部曲/148.CAD插件研究/lisp/属性块-批量填数.lsp")

;; 内核不在就 load 一次。返回 T / nil。
(defun dtq:ensure ( / p)
  (if (atoms-family 1 '("PLT:PUT"))
    T
    (progn
      (setq p (vl-catch-all-apply 'load (list dtq:dep)))
      (if (and (not (vl-catch-all-error-p p)) (atoms-family 1 '("PLT:PUT")))
        T
        nil)
    )
  )
)

;; ================================================================
;; 一、常量与小工具
;; ================================================================

(setq *dtq:perpage* 8)     ; 每页最多几台柜（列数 = 台数，太多会顶出屏幕）
(setq *dtq:roweq*  8.0)    ; 判「同一排」的 y 容差
(setq *dtq:tw*     10)     ; 字段名列宽（字符）
(setq *dtq:cw*     13)     ; 每列输入框宽（字符）—— 默认值，弹窗前按台数覆盖

;; 柜级字段：属性标签 / 面板上的显示名（显示名比标签短，省横向空间）
(setq *dtq:flds* '(
  ("柜号"     "柜号")
  ("柜型"     "柜型")
  ("柜用途"   "用途")
  ("柜尺寸"   "尺寸")
  ("分支电流" "分支电流")
  ("容量功率" "功率")
  ("备注"     "备注")))

;; nil -> ""（set_tile / edit_box 只吃字符串）
(defun dtq:nz (s) (if s s ""))

;; 列宽随台数收缩，保证整排塞得进屏幕（DCL 约 6px/字符）
(defun dtq:fitcw (n)
  (cond
    ((<= n 5) 14)
    ((= n 6)  13)
    ((= n 7)  12)
    ((= n 8)  11)
    (T        10)
  )
)

;; 移除表里第一个等于 x 的元素（vl-remove 会删掉全部，排序时只要删一个）
(defun dtq:rem1 (x lst / out hit)
  (setq out nil hit nil)
  (foreach e lst
    (if (and (null hit) (equal e x))
      (setq hit T)
      (setq out (cons e out))
    )
  )
  (reverse out)
)

;; 把表里第 n 个（0 起）元素换成 v
(defun dtq:setn (lst n v / out i)
  (setq out nil i 0)
  (foreach e lst
    (setq out (cons (if (= i n) v e) out))
    (setq i (1+ i))
  )
  (reverse out)
)

;; ================================================================
;; 二、图上扫描 / 定位
;; ================================================================

;; 模型空间所有柜块参照 -> ((图元名 x y) ...)，顺序不定
;;   靠「柜号」标签认柜，不认块名（GKC 扩容会把块名滚花）
(defun dtq:cabs ( / ss i n e ed p out)
  (setq out nil)
  (setq ss (ssget "_X" '((0 . "INSERT") (410 . "Model"))))
  (if ss
    (progn
      (setq i 0 n (sslength ss))
      (while (< i n)
        (setq e (ssname ss i) ed (entget e))
        (if (assoc *plt:tag-cab* (plt:atts e))
          (progn
            (setq p (cdr (assoc 10 ed)))
            (setq out (cons (list e (car p) (cadr p)) out))
          )
        )
        (setq i (1+ i))
      )
    )
  )
  (reverse out)
)

;; 柜的 x 区间与高度（块定义几何 + 插入点，不含属性文字溢出）
;;   plt:definfo 返回 (ATTDEF数 宽 高 最小x 最小y) —— 最小x 是【第 4 项】
(defun dtq:xrng (it / di x0)
  (setq di (plt:definfo (cdr (assoc 2 (entget (car it))))))
  (setq x0 (+ (cadr it) (nth 3 di)))
  (list x0 (+ x0 (nth 1 di)) (nth 2 di))
)

;; 点 -> 哪台柜：x 落在柜宽内、y 在柜高内，取中心最近的那台；
;;   都没中再按平面距离兜底（半径 60），超过就当成没点着。
(defun dtq:pick (cabs p / best bd it xr cx d hy)
  (setq best nil bd 1e9)
  (foreach it cabs
    (setq xr (dtq:xrng it) hy (caddr xr))
    (if (< hy 40.0) (setq hy 40.0))
    (if (and (>= (car p) (car xr)) (<= (car p) (cadr xr))
             (<= (abs (- (cadr p) (caddr it))) hy))
      (progn
        (setq cx (/ (+ (car xr) (cadr xr)) 2.0))
        (setq d (abs (- (car p) cx)))
        (if (< d bd) (setq bd d best it))
      )
    )
  )
  (if (null best)
    (progn
      (setq bd 60.0)
      (foreach it cabs
        (setq d (sqrt (+ (expt (- (car p) (cadr it)) 2.0)
                         (expt (- (cadr p) (caddr it)) 2.0))))
        (if (< d bd) (setq bd d best it))
      )
    )
  )
  best
)

;; 按插入点 x 升序排（稳定 —— 不用 vl-sort，它会吃掉相等元素）
(defun dtq:sortx (lst / out mn)
  (setq out nil)
  (while lst
    (setq mn (car lst))
    (foreach p lst (if (< (cadr p) (cadr mn)) (setq mn p)))
    (setq out (cons mn out))
    (setq lst (dtq:rem1 mn lst))
  )
  (reverse out)
)

;; 按 y 降序、同 y 按 x 升序排 = 从上往下、每排从左往右
(defun dtq:sortall (lst / out mn)
  (setq out nil)
  (while lst
    (setq mn (car lst))
    (foreach p lst
      (if (or (> (caddr p) (caddr mn))
              (and (equal (caddr p) (caddr mn) 1e-6)
                   (< (cadr p) (cadr mn))))
        (setq mn p)
      )
    )
    (setq out (cons mn out))
    (setq lst (dtq:rem1 mn lst))
  )
  (reverse out)
)

;; 取与 it 同一排的柜（|y - 基准| < 容差），排内按 x 升序
(defun dtq:row (cabs it / y0 out)
  (setq y0 (caddr it) out nil)
  (foreach c (dtq:sortx cabs)
    (if (< (abs (- (caddr c) y0)) *dtq:roweq*)
      (setq out (cons c out))
    )
  )
  (reverse out)
)

;; ================================================================
;; 三、面板数据（初值 / 取值 / 写回）
;; ================================================================

;; 柜表 -> 记录表 ((图元名 x y (值...)) ...)，值与 *dtq:flds* 同序
(defun dtq:mkrecs (rows / out atts)
  (setq out nil)
  (foreach c rows
    (setq atts (plt:atts (car c)))
    (setq out (cons (list (car c) (cadr c) (caddr c)
                          (mapcar '(lambda (f)
                                     (dtq:nz (plt:val atts (car f))))
                                  *dtq:flds*))
                    out)
    )
  )
  (reverse out)
)

;; 写回：每台柜逐字段写它自己的参照；空值也照写（面板清空 = 图上清空）
(defun dtq:write (recs / n chg atts i v old)
  (setq n 0 chg 0)
  (foreach r recs
    (setq atts (plt:atts (car r)) i 0)
    (foreach f *dtq:flds*
      (setq v (nth i (cadddr r)))
      (setq old (dtq:nz (plt:val atts (car f))))
      (if (plt:put atts (car f) v T)
        (progn
          (setq n (1+ n))
          (if (/= old v) (setq chg (1+ chg)))
        )
      )
      (setq i (1+ i))
    )
  )
  (strcat "已写 " (itoa n) " 格（实际变 " (itoa chg) " 格），覆盖 "
          (itoa (length recs)) " 台柜。没存盘，改动都在内存里。")
)

;; 总页数
(defun dtq:npage (n)
  (if (< n 1) 1 (1+ (/ (1- n) *dtq:perpage*)))
)

;; 第 pg 页（0 起）的记录
(defun dtq:page (recs pg / i n out)
  (setq i (* pg *dtq:perpage*) n (length recs) out nil)
  (while (and (< i n) (< (length out) *dtq:perpage*))
    (setq out (cons (nth i recs) out) i (1+ i))
  )
  (reverse out)
)

;; ================================================================
;; 四、界面（DCL）
;; ================================================================

(setq *dtq:recs* nil *dtq:keys* nil *dtq:ok* nil *dtq:live* nil *dtq:curn* 0 *dtq:curpg* 0 *dtq:selst* nil *dtq:pick1* nil *dtq:calcsel* nil *dtq:calcidx* nil)

;; DCL 落到 %TEMP%（列数随排变，必须现生成）
(defun dtq:tmppath (name / d c)
  (setq d (getvar "TEMPPREFIX"))
  (if (or (null d) (= d ""))
    (setq d (vl-filename-mktemp "dtq"))
  )
  (setq c (substr d (strlen d) 1))
  (if (and (/= c "\\") (/= c "/")) (setq d (strcat d "\\")))
  (strcat d name)
)
(defun dtq:dclpath () (dtq:tmppath "dtq_dlg.dcl"))
(defun dtq:selpath () (dtq:tmppath "dtq_sel.dcl"))

;; DCL 字符串里不能出现裸引号 / 反斜杠 —— 换掉，否则整份 DCL 解析失败、弹窗静默不出
(defun dtq:esc (s / x)
  (cond
    ((null s) (setq x ""))
    ((= (type s) 'STR) (setq x s))
    ((= (type s) 'INT) (setq x (itoa s)))
    ((= (type s) 'REAL) (setq x (rtos s 2 2)))
    (T (setq x (vl-princ-to-string s)))
  )
  (vl-string-translate "\"\\" "  " x)
)

;; 拆「柜号」：前缀（非数字部分）+ 末尾数字串（保留原位数）
;;   结尾没有数字 -> 返回 nil（没法自动编号）
(defun dtq:splitno (s / n i)
  (setq n (strlen s) i n)
  (while (and (> i 0) (wcmatch (substr s i 1) "#"))
    (setq i (1- i))
  )
  (if (= i n)
    nil
    (list (substr s 1 i) (substr s (1+ i)) (- n i))
  )
)

;; 数字补前导 0 到 w 位（超过 w 位就照实际位数出，不截断）
(defun dtq:padnum (v w / s)
  (setq s (itoa v))
  (while (< (strlen s) w) (setq s (strcat "0" s)))
  s
)

;; 按标签找字段在 *dtq:flds* 里的序号（0 起），找不到返回 nil
(defun dtq:fldidx (tag / i lst)
  (setq i 0 lst *dtq:flds*)
  (while (and lst (/= (caar lst) tag))
    (setq lst (cdr lst) i (1+ i))
  )
  (if lst i nil)
)

;; 取本页第 i 列（0 起）、字段序号 fi 的「当前」值：
;;   优先用实时编辑值（*dtq:live*）；没编辑过就用面板原值（来自 *dtq:recs*，
;;   已经是图上真实值）——不问 get_tile，没被激活过的框读它可能是空串。
(defun dtq:curval (fi i / k v r)
  (setq k (strcat "e" (itoa fi) "_" (itoa i)))
  (setq v (cdr (assoc k *dtq:live*)))
  (if (null v)
    (progn
      (setq r (nth (+ (* *dtq:curpg* *dtq:perpage*) i) *dtq:recs*))
      (setq v (if r (dtq:nz (nth fi (cadddr r))) ""))
    )
  )
  v
)

;; 自动编柜号：先弹一个「选起止范围」子框（本页每台一个勾选框，默认全选，
;;   带「全选」「全部取消」，复用 dtq:selui）。确定后，以勾选里最靠前的一台
;;   为基准，柜号依次 +1、+2……只发给后面同样被勾选的柜；没勾的跳过不动，
;;   相当于「起止点之间」——把范围两头以外的柜取消勾选，就等于圈定了起止点。
;;   只改本页正在编辑的格子（实时值），不动别的字段、不落图。
(defun dtq:autonum ( / fi i first cnt base sp pre num w k v)
  (setq fi (dtq:fldidx "柜号"))
  (cond
    ((< *dtq:curn* 2)
     (alert "本页只有一台柜，没有后续柜号可编。")
    )
    ((null fi)
     (alert "面板里没有「柜号」这个字段。")
    )
    ((dtq:selui "选自动编号范围（默认全选）" fi)
     (setq i 0 first nil cnt 0)
     (while (< i *dtq:curn*)
       (if (= (nth i *dtq:selst*) "1")
         (progn
           (if (null first) (setq first i))
           (setq cnt (1+ cnt))
         )
       )
       (setq i (1+ i))
     )
     (cond
       ((null first)
        (alert "一台都没选，没法自动编号。")
       )
       ((< cnt 2)
        (alert "只选中了一台，没有后续柜号可编。")
       )
       (T
        (setq base (dtq:curval fi first))
        (cond
          ((= base "")
           (alert "基准那台（第一个被选中的）柜号是空的，先填好再自动编号。")
          )
          ((null (setq sp (dtq:splitno base)))
           (alert (strcat "基准柜号「" base "」结尾没有数字，没法自动递增。"))
          )
          (T
           (setq pre (nth 0 sp) num (atoi (nth 1 sp)) w (nth 2 sp))
           (setq i (1+ first) cnt 1)
           (while (< i *dtq:curn*)
             (if (= (nth i *dtq:selst*) "1")
               (progn
                 (setq k (strcat "e" (itoa fi) "_" (itoa i)))
                 (setq v (strcat pre (dtq:padnum (+ num cnt) w)))
                 (vl-catch-all-apply 'set_tile (list k v))
                 (dtq:live1 k)
                 (setq cnt (1+ cnt))
               )
             )
             (setq i (1+ i))
           )
          )
        )
       )
     )
    )
  )
)

;; 通用「统一某字段」：把本页第 1 台（第 0 列）该字段的值，原样复制给本页其余各列。
;;   只改本页正在编辑的格子（实时值），不动其它字段、不落图。
(defun dtq:unifyfld (tag / fi base i k)
  (setq fi (dtq:fldidx tag))
  (if (null fi)
    (alert (strcat "面板里没有「" tag "」这个字段。"))
    (progn
      (setq base (dtq:curval fi 0))
      (cond
        ((< *dtq:curn* 2)
         (alert "本页只有一台柜，没有其他柜可统一。")
        )
        ((= base "")
         (alert (strcat "第一台的「" tag "」是空的，先填好第一台再统一。"))
        )
        (T
         (setq i 1)
         (while (< i *dtq:curn*)
           (setq k (strcat "e" (itoa fi) "_" (itoa i)))
           (vl-catch-all-apply 'set_tile (list k base))
           (dtq:live1 k)
           (setq i (1+ i))
         )
        )
      )
    )
  )
)

;; 柜尺寸统一：按钮回调用——以本页第 1 台的柜尺寸为准，其余柜照抄
(defun dtq:unifysize () (dtq:unifyfld "柜尺寸"))

;; ---------------- 柜型统一：先勾选范围，再统一 ----------------

;; 本页各列的勾选状态，("1"/"0" ...)，下标 = 列号（0 起）
;; *dtq:pick1*：起止点两次点选的「第 1 次」列号；nil = 当前没有未完成的点选
;;   （刚打开子框、或上一轮起止点已经点完两次）。

;; 点一次柜（勾选框被点）—— 用两次点选定义一段连续的起止范围：
;;   第 1 次点：不管点哪台，先把全部选中清空，只把这一台记成「起点」，
;;     等第 2 次点（对应「改起止点就清空重选」）；
;;   第 2 次点：与第 1 次点之间（含两头、不分先后）整段设为选中，
;;     范围外的全部清空——起止点就此重新定义完成，*dtq:pick1* 归空，
;;     再点一次又重新开始下一轮。
(defun dtq:rangeclick (i k / lo hi j on)
  (if (null *dtq:pick1*)
    (progn
      (setq j 0)
      (while (< j *dtq:curn*)
        (setq on (if (= j i) "1" "0"))
        (vl-catch-all-apply 'set_tile (list (strcat "s" (itoa j)) on))
        (setq *dtq:selst* (dtq:setn *dtq:selst* j on))
        (setq j (1+ j))
      )
      (setq *dtq:pick1* i)
    )
    (progn
      (setq lo (min *dtq:pick1* i) hi (max *dtq:pick1* i))
      (setq j 0)
      (while (< j *dtq:curn*)
        (setq on (if (and (>= j lo) (<= j hi)) "1" "0"))
        (vl-catch-all-apply 'set_tile (list (strcat "s" (itoa j)) on))
        (setq *dtq:selst* (dtq:setn *dtq:selst* j on))
        (setq j (1+ j))
      )
      (setq *dtq:pick1* nil)
    )
  )
)

;; 全选 / 全部取消：直接改勾选框显示 + 同步记录，不关子框；
;;   同时把待定的起止点点选清空——按了这两个按钮，就不算在两次点选的半途中。
(defun dtq:selall ( / i)
  (setq i 0)
  (while (< i *dtq:curn*)
    (vl-catch-all-apply 'set_tile (list (strcat "s" (itoa i)) "1"))
    (setq *dtq:selst* (dtq:setn *dtq:selst* i "1"))
    (setq i (1+ i))
  )
  (setq *dtq:pick1* nil)
)
(defun dtq:selnone ( / i)
  (setq i 0)
  (while (< i *dtq:curn*)
    (vl-catch-all-apply 'set_tile (list (strcat "s" (itoa i)) "0"))
    (setq *dtq:selst* (dtq:setn *dtq:selst* i "0"))
    (setq i (1+ i))
  )
  (setq *dtq:pick1* nil)
)

;; 弹「选统一范围」子对话框：本页每台柜一个勾选框（默认全选中，带当前 fi
;;   字段预览方便对照），配「全选」「全部取消」。
;;   返回 T = 用户按了「确定」（此时 *dtq:selst* 就是本页各列的勾选结果）；
;;   返回 nil = 取消 / 出错（*dtq:selst* 不作数）。
(defun dtq:selui (ttl fi / fn h i v id r ok)
  (setq *dtq:selst* nil *dtq:pick1* nil i 0)
  (while (< i *dtq:curn*) (setq *dtq:selst* (cons "1" *dtq:selst*)) (setq i (1+ i)))
  (setq *dtq:selst* (reverse *dtq:selst*))
  (setq fn (dtq:selpath))
  (setq h (open fn "w"))
  (if (null h)
    (progn (princ (strcat "\n写不出选择框 DCL：" fn)) nil)
    (progn
      (write-line "dtqsel : dialog {" h)
      (write-line (strcat "  label = \"" (dtq:esc ttl) "\";") h)
      (write-line "  : text { label = \"默认整排全选；点两台柜定起止点，中间自动整段选中（再点会重新定义）\"; }" h)
      (setq i 0)
      (while (< i *dtq:curn*)
        (setq v (if fi (dtq:curval fi i) ""))
        (write-line (strcat "  : toggle { key = \"s" (itoa i)
                            "\"; value = \"1\"; label = \""
                            (itoa (1+ i)) ": " (dtq:esc v) "\"; }") h)
        (setq i (1+ i))
      )
      (write-line "  : row {" h)
      (write-line "    : button { key = \"selall\"; label = \"全选\"; }" h)
      (write-line "    : button { key = \"selnone\"; label = \"全部取消\"; }" h)
      (write-line "  }" h)
      (write-line "  : row {" h)
      (write-line "    : button { key = \"accept\"; label = \"确定\"; is_default = true; }" h)
      (write-line "    : button { key = \"cancel\"; label = \"取消\"; is_cancel = true; }" h)
      (write-line "  }" h)
      (write-line "}" h)
      (close h)
      (setq id (load_dialog fn))
      (if (< id 0)
        (progn (princ "\n加载选择框失败。") nil)
        (progn
          (setq ok nil)
          (if (not (new_dialog "dtqsel" id))
            (progn (unload_dialog id) (princ "\n打开选择框失败。") nil)
            (progn
              (setq i 0)
              (while (< i *dtq:curn*)
                (vl-catch-all-apply 'action_tile
                  (list (strcat "s" (itoa i))
                        (strcat "(dtq:rangeclick " (itoa i) " \"s" (itoa i) "\")")))
                (setq i (1+ i))
              )
              (vl-catch-all-apply 'action_tile
                (list "selall" "(vl-catch-all-apply 'dtq:selall nil)"))
              (vl-catch-all-apply 'action_tile
                (list "selnone" "(vl-catch-all-apply 'dtq:selnone nil)"))
              (action_tile "accept" "(done_dialog 1)")
              (action_tile "cancel" "(done_dialog 0)")
              (setq r (vl-catch-all-apply 'start_dialog nil))
              (unload_dialog id)
              (if (and (not (vl-catch-all-error-p r)) (= r 1)) (setq ok T))
              ok
            )
          )
        )
      )
    )
  )
)

;; 柜型统一：按钮回调用——先弹选择框，再以「第一个被选中的」为基准，
;;   把基准值抄给其余被选中的柜；没勾的一律不动。
(defun dtq:typeunify ( / fi i first base k)
  (setq fi (dtq:fldidx "柜型"))
  (if (null fi)
    (alert "面板里没有「柜型」这个字段。")
    (if (dtq:selui "选柜型统一范围" fi)
      (progn
        (setq i 0 first nil)
        (while (and (< i *dtq:curn*) (null first))
          (if (= (nth i *dtq:selst*) "1") (setq first i))
          (setq i (1+ i))
        )
        (cond
          ((null first)
           (alert "一台都没选，没法统一。")
          )
          (T
           (setq base (dtq:curval fi first))
           (if (= base "")
             (alert "基准那台（第一个被选中的）柜型是空的，先填好再统一。")
             (progn
               (setq i 0)
               (while (< i *dtq:curn*)
                 (if (and (/= i first) (= (nth i *dtq:selst*) "1"))
                   (progn
                     (setq k (strcat "e" (itoa fi) "_" (itoa i)))
                     (vl-catch-all-apply 'set_tile (list k base))
                     (dtq:live1 k)
                   )
                 )
                 (setq i (1+ i))
               )
             )
           )
          )
        )
      )
    )
  )
)

;; ---------------- 自动计算分支电流：按电压 + 容量/功率算额定电流 ----------------

;; 去掉字符串首尾空格
(defun dtq:trim (s / n a b)
  (setq n (strlen s) a 1 b n)
  (while (and (<= a b) (= (substr s a 1) " ")) (setq a (1+ a)))
  (while (and (<= a b) (= (substr s b 1) " ")) (setq b (1- b)))
  (if (> a b) "" (substr s a (1+ (- b a))))
)

;; 挑出字符串开头的数字（允许前导负号、一个小数点），后面带的单位文字
;;   （kW / kVA / 千瓦……）直接忽略不管；开头就不是数字 -> 返回 nil，
;;   代表这一格没法自动提取数值（自动计算时会跳过，不出现在清单里）。
(defun dtq:parsenum (s / s2 n i c out dot)
  (setq s2 (dtq:trim s))
  (setq n (strlen s2) i 1 out "" dot nil)
  (if (and (> n 0) (= (substr s2 1 1) "-"))
    (progn (setq out "-" i 2))
  )
  (while (and (<= i n)
              (setq c (substr s2 i 1))
              (or (wcmatch c "#") (and (= c ".") (null dot)))
         )
    (if (= c ".") (setq dot T))
    (setq out (strcat out c))
    (setq i (1+ i))
  )
  (if (or (= out "") (= out "-") (= out "."))
    nil
    (atof out)
  )
)

;; 额定电流（简化算法，按 I = 容量或功率(kVA/kW) / (根号3 × 电压(kV))，
;;   不单独扣功率因数——容量/功率、电压都用 kVA/kW/kV，算出来直接就是 A）
(defun dtq:calcamp (s uk) (/ s (* (sqrt 3) uk)))

;; 电流按大小自动收精度：<10A 留 2 位小数，10~100A 留 1 位，>=100A 取整
(defun dtq:fmtamp (v / w)
  (cond
    ((< v 10.0) (setq w 2))
    ((< v 100.0) (setq w 1))
    (T (setq w 0))
  )
  (rtos v 2 w)
)

;; 勾选框刚被点 -> 记录它自己的最新状态（子框里的框互不影响，独立勾选，
;;   不是「自动编号」那种起止点两次点选）
(defun dtq:calcclick (si k)
  (setq *dtq:calcsel* (dtq:setn *dtq:calcsel* si (dtq:gts k)))
)

;; 电压单选按钮被点 -> 立刻记下点的是哪个键（与主面板编辑框 dtq:live1
;;   同一思路：点选那一刻就落地，不等对话框关闭后再读，避免读到旧档位）
(defun dtq:calcvsel (k) (setq *dtq:calcvolt* k))

;; 自定义电压格子实时存值（格子里打完字不按回车、直接点「计算」，
;;   也能拿到最新填的数）
(defun dtq:calcecv () (setq *dtq:calcec* (dtq:gts "ec")))

;; 弹「自动计算」子对话框：电压单选（35/20/10/6/0.4KV 五档常用值 +
;;   自定义1/2/3 三个可自己填数的档，默认选中 10KV）+ 本页里「容量功率」
;;   能读出数值的柜号勾选框（默认全选，没数据的柜不列出，也不会被算）。
;;   返回 (电压kV . ((列号 数值) ...))，取消 / 没数据 / 自定义电压没填对
;;   一律返回 nil。
(defun dtq:calcui (fi_p / fn h i v id r items n si uk errmsg)
  (setq *dtq:calcidx* nil *dtq:calcsel* nil items nil i 0)
  (while (< i *dtq:curn*)
    (setq v (dtq:parsenum (dtq:curval fi_p i)))
    (if (and v (> v 0.0))
      (setq items (cons (list i v) items))
    )
    (setq i (1+ i))
  )
  (setq items (reverse items))
  (if (null items)
    (progn (alert "本页「容量/功率」都没有能识别的数值，没法自动计算。") nil)
    (progn
      (setq n (length items) si 0)
      (while (< si n) (setq *dtq:calcsel* (cons "1" *dtq:calcsel*)) (setq si (1+ si)))
      (setq *dtq:calcsel* (reverse *dtq:calcsel*))
      (setq fn (dtq:selpath))
      (setq h (open fn "w"))
      (if (null h)
        (progn (princ (strcat "\n写不出选择框 DCL：" fn)) nil)
        (progn
          (write-line "dtqsel : dialog {" h)
          (write-line "  label = \"自动计算分支电流\";" h)
          (write-line "  : boxed_radio_column {" h)
          (write-line "    label = \"电压等级（kV）\";" h)
          (write-line "    : radio_button { key = \"v35\"; label = \"35KV\"; }" h)
          (write-line "    : radio_button { key = \"v20\"; label = \"20KV\"; }" h)
          (write-line "    : radio_button { key = \"v10\"; label = \"10KV\"; value = \"1\"; }" h)
          (write-line "    : radio_button { key = \"v6\";  label = \"6KV\"; }" h)
          (write-line "    : radio_button { key = \"v04\"; label = \"0.4KV\"; }" h)
          (write-line "    : radio_button { key = \"vc\";  label = \"自定义\"; }" h)
          (write-line "  }" h)
          (write-line "  : row {" h)
          (write-line "    : text { label = \"自定义电压(kV)：\"; }" h)
          (write-line "    : edit_box { key = \"ec\"; edit_width = 8; }" h)
          (write-line "  }" h)
          (write-line "  : text { label = \"以下是有容量/功率数据的柜号，默认全选（去掉勾的不算）：\"; }" h)
          (setq si 0)
          (foreach it items
            (write-line (strcat "  : toggle { key = \"c" (itoa si)
                                "\"; value = \"1\"; label = \""
                                (itoa (1+ (car it))) ": " (dtq:esc (cadr it)) "\"; }") h)
            (setq si (1+ si))
          )
          (write-line "  : row {" h)
          (write-line "    : button { key = \"accept\"; label = \"计算\"; is_default = true; }" h)
          (write-line "    : button { key = \"cancel\"; label = \"取消\"; is_cancel = true; }" h)
          (write-line "  }" h)
          (write-line "}" h)
          (close h)
          (setq id (load_dialog fn))
          (if (< id 0)
            (progn (princ "\n加载选择框失败。") nil)
            (if (not (new_dialog "dtqsel" id))
              (progn (unload_dialog id) (princ "\n打开选择框失败。") nil)
              (progn
                (setq *dtq:calcvolt* "v10" *dtq:calcec* "")
                (setq si 0)
                (while (< si n)
                  (vl-catch-all-apply 'action_tile
                    (list (strcat "c" (itoa si))
                          (strcat "(dtq:calcclick " (itoa si) " \"c" (itoa si) "\")")))
                  (setq si (1+ si))
                )
                ;; 电压 6 个单选键：点哪个就立刻记哪个（dtq:calcvsel），
                ;;   换档位马上生效，不用等对话框关闭再读
                (foreach vk '("v35" "v20" "v10" "v6" "v04" "vc")
                  (vl-catch-all-apply 'action_tile
                    (list vk (strcat "(dtq:calcvsel \"" vk "\")")))
                )
                ;; 自定义电压格子实时存值
                (vl-catch-all-apply 'action_tile (list "ec" "(dtq:calcecv)"))
                (action_tile "accept" "(done_dialog 1)")
                (action_tile "cancel" "(done_dialog 0)")
                (setq r (vl-catch-all-apply 'start_dialog nil))
                (if (or (vl-catch-all-error-p r) (/= r 1))
                  (progn (unload_dialog id) nil)
                  (progn
                    ;; 电压：优先用点按钮那一刻实时记下的 *dtq:calcvolt*
                    ;;   （dtq:calcvsel，与主面板编辑框 dtq:live1 同一思路）——
                    ;;   不再等对话框关闭后才用 get_tile 逐个读，从根上避免
                    ;;   「换了档位、取值时又读回旧档位」（这正是之前改电压
                    ;;   不生效的根子）。万一 action 没绑上（极端情况）才退
                    ;;   回去直接读 get_tile 兜底。自定义电压同理，格子里打
                    ;;   完字不回车也能靠 dtq:calcecv 实时拿到最新值。
                    ;;   取值必须在 unload_dialog 之前做（兜底分支要读 dcl）。
                    (if (or (null *dtq:calcvolt*) (= *dtq:calcvolt* ""))
                      (setq *dtq:calcvolt*
                            (cond ((= (dtq:gts "v35") "1") "v35")
                                  ((= (dtq:gts "v20") "1") "v20")
                                  ((= (dtq:gts "v6")  "1") "v6")
                                  ((= (dtq:gts "v04") "1") "v04")
                                  ((= (dtq:gts "vc")  "1") "vc")
                                  (T "v10"))
                      )
                    )
                    (if (or (null *dtq:calcec*) (= *dtq:calcec* ""))
                      (setq *dtq:calcec* (dtq:gts "ec"))
                    )
                    (unload_dialog id)
                    (setq uk nil errmsg nil)
                    (cond
                      ((= *dtq:calcvolt* "v35") (setq uk 35.0))
                      ((= *dtq:calcvolt* "v20") (setq uk 20.0))
                      ((= *dtq:calcvolt* "v10") (setq uk 10.0))
                      ((= *dtq:calcvolt* "v6")  (setq uk 6.0))
                      ((= *dtq:calcvolt* "v04") (setq uk 0.4))
                      ((= *dtq:calcvolt* "vc")
                       (setq uk (dtq:parsenum *dtq:calcec*))
                       (if (or (null uk) (<= uk 0.0))
                         (setq errmsg "「自定义」没填对电压数值（要大于 0 的数字）。")
                       )
                      )
                      (T (setq uk 10.0))
                    )
                    (if errmsg
                      (progn (alert errmsg) nil)
                      (progn
                        (setq *dtq:calcidx* (mapcar 'car items))
                        (cons uk items)
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
  )
)

;; 自动计算：按钮回调用 —— 选电压 + 选柜号后，逐台把「容量功率」按
;;   I = 值 / (根号3 × 电压) 算出来，填进「分支电流」（只改实时值，不落图）
(defun dtq:autocalc ( / fi_p fi_c res uk items si it col val amp k)
  (setq fi_p (dtq:fldidx "容量功率") fi_c (dtq:fldidx "分支电流"))
  (cond
    ((null fi_p) (alert "面板里没有「容量功率」这个字段。"))
    ((null fi_c) (alert "面板里没有「分支电流」这个字段。"))
    (T
     (setq res (dtq:calcui fi_p))
     (if res
       (progn
         (setq uk (car res) items (cdr res) si 0)
         (foreach it items
           (if (= (nth si *dtq:calcsel*) "1")
             (progn
               (setq col (car it) val (cadr it))
               (setq amp (dtq:fmtamp (dtq:calcamp val uk)))
               (setq k (strcat "e" (itoa fi_c) "_" (itoa col)))
               (vl-catch-all-apply 'set_tile (list k amp))
               (dtq:live1 k)
             )
           )
           (setq si (1+ si))
         )
       )
     )
    )
  )
)

;; ---------------- 一键清空：按行（字段）或整页清空当前页的实时文本框 ----------------

;; 把第 fi 号字段在本页所有列的实时显示值清空（清成空串）；
;;   只改实时值，不越页、不落图，仍要按主面板「写入」才真正落到图上。
(defun dtq:clearfld (fi / i k)
  (setq i 0)
  (while (< i *dtq:curn*)
    (setq k (strcat "e" (itoa fi) "_" (itoa i)))
    (vl-catch-all-apply 'set_tile (list k ""))
    (dtq:live1 k)
    (setq i (1+ i))
  )
)

;; 一键清空：弹子框，按面板行标题列按钮（跳过「柜号」，避免连柜号一起清没
;;   —— 柜号是认柜的依据，清空按钮不碰它），外加一个「全部」按钮
;;   （全部 = 清掉除柜号外的其余各行）。点哪个按钮就清哪个，随即关子框。
(defun dtq:clearui ( / fn h id r idxs f)
  (setq idxs nil f 0)
  (foreach fld *dtq:flds*
    (if (/= (car fld) "柜号")
      (setq idxs (cons f idxs))
    )
    (setq f (1+ f))
  )
  (setq idxs (reverse idxs))
  (setq fn (dtq:selpath))
  (setq h (open fn "w"))
  (if (null h)
    (progn (princ (strcat "\n写不出选择框 DCL：" fn)) nil)
    (progn
      (write-line "dtqsel : dialog {" h)
      (write-line "  label = \"一键清空\";" h)
      (write-line "  : text { label = \"点哪行清哪行（本页），或点「全部」整页清空：\"; }" h)
      (foreach fi idxs
        (write-line (strcat "  : button { key = \"b" (itoa fi) "\"; label = \""
                            (dtq:esc (cadr (nth fi *dtq:flds*))) "\"; }") h)
      )
      (write-line "  : button { key = \"ball\"; label = \"全部\"; }" h)
      (write-line "  : button { key = \"cancel\"; label = \"取消\"; is_cancel = true; }" h)
      (write-line "}" h)
      (close h)
      (setq id (load_dialog fn))
      (if (< id 0)
        (progn (princ "\n加载选择框失败。") nil)
        (if (not (new_dialog "dtqsel" id))
          (progn (unload_dialog id) (princ "\n打开选择框失败。") nil)
          (progn
            (foreach fi idxs
              (vl-catch-all-apply 'action_tile
                (list (strcat "b" (itoa fi)) (strcat "(done_dialog " (itoa (1+ fi)) ")")))
            )
            (action_tile "ball" "(done_dialog 999)")
            (action_tile "cancel" "(done_dialog 0)")
            (setq r (vl-catch-all-apply 'start_dialog nil))
            (unload_dialog id)
            (cond
              ((vl-catch-all-error-p r) nil)
              ((= r 0) nil)
              ((= r 999) (foreach fi idxs (dtq:clearfld fi)))
              (T (dtq:clearfld (1- r)))
            )
          )
        )
      )
    )
  )
)

;; 生成 DCL 文本：首行 = 柜序号，再逐字段一行（标签 + N 个输入框）；
;;   右侧另起一列固定区，放「自动编柜号」按钮，不随翻页消失。
;;   按钮 key 直接用内置 accept / cancel —— 回车 / ESC / 鼠标点是同一条路
(defun dtq:mkdcl (fn ttl pg npage cols / h i j)
  (setq h (open fn "w"))
  (if (null h)
    nil
    (progn
      (write-line "dtqdcl : dialog {" h)
      (write-line (strcat "  label = \"" (dtq:esc ttl) "\";") h)
      (write-line "  : text { key = \"hint\"; width = 64; }" h)
      ;; 外层一行：左边是整张表（表头 + 各字段行），右边是固定的操作区
      (write-line "  : row {" h)
      (write-line "    : column {" h)
      ;; 表头行：空标签 + 各列柜序号（跨页时序号接着上一页排）
      (write-line "      : row {" h)
      (write-line (strcat "        : text { label = \" \"; width = " (itoa *dtq:tw*) "; }") h)
      (setq i 0)
      (foreach c cols
        (write-line (strcat "        : text { label = \""
                            (itoa (+ (* pg *dtq:perpage*) i 1))
                            "\"; width = " (itoa *dtq:cw*)
                            "; alignment = center; }") h)
        (setq i (1+ i))
      )
      (write-line "      }" h)
      ;; 字段行
      (setq j 0)
      (foreach f *dtq:flds*
        (write-line "      : row {" h)
        (write-line (strcat "        : text { label = \"" (dtq:esc (cadr f))
                            "\"; width = " (itoa *dtq:tw*) "; }") h)
        (setq i 0)
        (foreach c cols
          (write-line (strcat "        : edit_box { key = \"e" (itoa j) "_" (itoa i)
                              "\"; edit_width = " (itoa *dtq:cw*)
                              "; width = " (itoa (+ *dtq:cw* 2)) "; }") h)
          (setq i (1+ i))
        )
        (write-line "      }" h)
        (setq j (1+ j))
      )
      (write-line "    }" h)
      ;; 右侧固定区：批量操作按钮（自动编柜号 / 柜尺寸统一 / 柜型统一……）
      (write-line "    : column {" h)
      (write-line "      : text { label = \"批量\"; }" h)
      (write-line "      : button { key = \"bauto\"; label = \"自动编柜号\"; }" h)
      (write-line "      : button { key = \"bsize\"; label = \"柜尺寸统一\"; }" h)
      (write-line "      : button { key = \"btype\"; label = \"柜型统一\"; }" h)
      (write-line "      : button { key = \"bcalc\"; label = \"自动计算\"; }" h)
      (write-line "      : button { key = \"bclear\"; label = \"一键清空\"; }" h)
      (write-line "    }" h)
      (write-line "  }" h)
      ;; 按钮行
      (write-line "  : row {" h)
      (if (> npage 1)
        (progn
          (if (> pg 0)
            (write-line "    : button { key = \"bprev\"; label = \"< 上一页\"; }" h)
          )
          (if (< pg (1- npage))
            (write-line "    : button { key = \"bnext\"; label = \"下一页 >\"; }" h)
          )
        )
      )
      (write-line "    : button { key = \"accept\"; label = \"写入\"; is_default = true; }" h)
      (write-line "    : button { key = \"cancel\"; label = \"取消\"; is_cancel = true; }" h)
      (write-line "  }" h)
      (write-line "}" h)
      (close h)
      T
    )
  )
)

;; 安全的 get_tile：键不存在 / 对话框已卸载都不抛错，一律当空串
(defun dtq:gts (k / r)
  (setq r (vl-catch-all-apply 'get_tile (list k)))
  (if (vl-catch-all-error-p r) "" (dtq:nz r))
)

;; 编辑框实时存值：*dtq:live* = ((键 . 值) ...)，最新的在最前
;;   ★ 必须给每个编辑框绑 action —— DCL 里没绑 action 的格子，
;;     用户在格内打完字不按回车直接点按钮时，值不会提交，
;;     get_tile 会取到旧值。
(defun dtq:live1 (k)
  (setq *dtq:live* (cons (cons k (dtq:gts k)) *dtq:live*))
)

;; start_dialog 返回后、unload_dialog 之前取值回写 *dtq:recs*
;;   ★ 只有真被编辑过（*dtq:live* 里有记录）的格子才用最新值；
;;     没编辑过的格子直接保留面板原值，不问 get_tile —— 没被激活过的
;;     edit_box，AutoCAD 的 get_tile 读出来可能是空串（不是旧值），
;;     照它写回去就会把没碰过的柜号 / 字段全部清空。整体不抛错。
(defun dtq:snap ( / kv k i j v r)
  (foreach kv *dtq:keys*
    (setq k (car kv) i (cadr kv) j (caddr kv))
    (setq r (nth i *dtq:recs*))
    (if r
      (progn
        (setq v (cdr (assoc k *dtq:live*)))
        (if (null v) (setq v (nth j (cadddr r))))
        (setq *dtq:recs* (dtq:setn *dtq:recs* i
                                  (list (car r) (cadr r) (caddr r)
                                        (dtq:setn (cadddr r) j v))))
      )
    )
  )
)

;; 弹窗（分页循环）。返回 T = 用户按了「写入」，nil = 取消 / 出错
;;   ★ 按钮回调里【只有 done_dialog】—— 表达式绝不抛错，框一定关得掉。
;;     取值在 start_dialog 返回之后做（那时 DCL 资源还在，unload 之前有效）。
(defun dtq:ui (recs ttl / pg npage go r fn id cols i j kv rv cw)
  (setq *dtq:recs* recs *dtq:ok* nil)
  (setq npage (dtq:npage (length recs)) pg 0 go T)
  (while go
    (setq cols (dtq:page *dtq:recs* pg))
    (setq cw (dtq:fitcw (length cols)) *dtq:cw* cw)
    (setq *dtq:curn* (length cols) *dtq:curpg* pg)
    ;; 键序必须与 dtq:mkdcl 的生成顺序一致：外层字段、内层列
    (setq *dtq:keys* nil i 0)
    (while (< i (length *dtq:flds*))
      (setq j 0)
      (while (< j (length cols))
        (setq *dtq:keys* (cons (list (strcat "e" (itoa i) "_" (itoa j))
                                     (+ (* pg *dtq:perpage*) j) i)
                               *dtq:keys*))
        (setq j (1+ j))
      )
      (setq i (1+ i))
    )
    (setq *dtq:keys* (reverse *dtq:keys*))
    (setq fn (dtq:dclpath))
    (if (null (dtq:mkdcl fn ttl pg npage cols))
      (progn (princ (strcat "\n写不出 DCL 文件：" fn)) (setq go nil))
      (progn
        (setq id (load_dialog fn))
        (if (< id 0)
          (progn (princ "\n加载对话框失败。") (setq go nil))
          (progn
            (if (not (new_dialog "dtqdcl" id))
              (progn (unload_dialog id) (princ "\n打开对话框失败。") (setq go nil))
              (progn
                (setq *dtq:live* nil)
                (set_tile "hint"
                          (strcat "第 " (itoa (1+ pg)) "/" (itoa npage) " 页 · 共 "
                                  (itoa (length *dtq:recs*))
                                  " 台柜 · 按「写入」一次性落图（不存盘）"))
                (foreach kv *dtq:keys*
                  (setq rv (nth (cadr kv) *dtq:recs*))
                  (set_tile (car kv) (dtq:nz (nth (caddr kv) (cadddr rv))))
                  ;; 每个编辑框绑实时存值（绑不上也不影响主流程）
                  (vl-catch-all-apply 'action_tile
                    (list (car kv) (strcat "(dtq:live1 \"" (car kv) "\")")))
                )
                ;; 按钮回调：只调 done_dialog，绝不夹带别的动作
                (action_tile "accept" "(done_dialog 1)")
                (action_tile "cancel" "(done_dialog 0)")
                ;; 自动编柜号 / 柜尺寸统一 / 柜型统一：不关框、只改本页格子；
                ;;   绑定本身也 catch 兜底，绝不能让它拖累下面的
                ;;   mode_tile / start_dialog（否则整个对话框都进不了模态，
                ;;   点「写入」会像没反应一样）
                (vl-catch-all-apply 'action_tile
                  (list "bauto" "(vl-catch-all-apply 'dtq:autonum nil)"))
                (vl-catch-all-apply 'action_tile
                  (list "bsize" "(vl-catch-all-apply 'dtq:unifysize nil)"))
                (vl-catch-all-apply 'action_tile
                  (list "btype" "(vl-catch-all-apply 'dtq:typeunify nil)"))
                (vl-catch-all-apply 'action_tile
                  (list "bcalc" "(vl-catch-all-apply 'dtq:autocalc nil)"))
                (vl-catch-all-apply 'action_tile
                  (list "bclear" "(vl-catch-all-apply 'dtq:clearui nil)"))
                (if (> pg 0)
                  (action_tile "bprev" "(done_dialog 2)")
                )
                (if (< pg (1- npage))
                  (action_tile "bnext" "(done_dialog 3)")
                )
                ;; 焦点给第一个输入框（点一下就落在控件上）
                (vl-catch-all-apply 'mode_tile
                  (list (car (car *dtq:keys*)) 2))
                ;; start_dialog 万一抛异常 -> 出人话提示，别让命令默默中断
                (setq r (vl-catch-all-apply 'start_dialog nil))
                (cond
                  ((vl-catch-all-error-p r)
                   ;; 兜底：对话框可能还活着，能关就关掉
                   (vl-catch-all-apply 'done_dialog (list 0))
                   (princ (strcat "\n对话框异常："
                                  (vl-catch-all-error-message r)))
                   (setq go nil))
                  (T
                   ;; 关闭后（1 写入 / 2 上页 / 3 下页）才取值 —— 此时 DCL 资源还在
                   (if (member r '(1 2 3))
                     (vl-catch-all-apply 'dtq:snap nil)
                   )
                   (cond
                     ((= r 1) (setq *dtq:ok* T go nil))
                     ((= r 2) (setq pg (1- pg)))
                     ((= r 3) (setq pg (1+ pg)))
                     (T (setq go nil))
                   )
                  )
                )
                (unload_dialog id)
              )
            )
          )
        )
      )
    )
  )
  *dtq:ok*
)

;; ================================================================
;; 五、入口
;; ================================================================

;; 有记录 -> 弹窗 -> 写入；返回报告文字（取消时返回 nil）
(defun dtq:go (rows ttl / r)
  (if (null rows)
    (progn (princ "\n这一排没认出柜块。") nil)
    (progn
      (if (dtq:ui (dtq:mkrecs rows) ttl)
        (progn
          (princ (strcat "\n" (dtq:write *dtq:recs*)))
          T
        )
        (progn (princ "\n取消了，图没动。") nil)
      )
    )
  )
)

;; DTP —— 点一台柜，弹这一排
(defun c:DTP ( / cabs p it row)
  (if (null (dtq:ensure))
    (princ "\n[DTP] 内核没就位：先 load 属性块-批量填数.lsp（本文件同目录）。")
    (progn
      (setq cabs (dtq:cabs))
      (cond
        ((null cabs)
         (princ "\n图上没有柜块（先跑 PGT 铺图）。"))
        (T
         (princ (strcat "\nDTP：点一台柜 -> 弹【这一排】的数据表。图上共 "
                        (itoa (length cabs)) " 台柜。"))
         (setq p (getpoint "\n点这排上任意一台柜（回车结束）: "))
         (if (null p)
           (princ "\n取消了，什么也没做。")
           (progn
             (setq it (dtq:pick cabs p))
             (if (null it)
               (princ "\n这个位置附近没有柜块，再点一次。")
               (progn
                 (setq row (dtq:row cabs it))
                 (dtq:go row (strcat "柜体数据 · 本排 " (itoa (length row)) " 台"))
               )
             )
           )
         )
        )
      )
    )
  )
  (princ)
)

;; DTPA —— 不点，全图所有柜（上->下、左->右）
(defun c:DTPA ( / cabs)
  (if (null (dtq:ensure))
    (princ "\n[DTPA] 内核没就位：先 load 属性块-批量填数.lsp（本文件同目录）。")
    (progn
      (setq cabs (dtq:cabs))
      (if (null cabs)
        (princ "\n图上没有柜块（先跑 PGT 铺图）。")
        (progn
          (setq cabs (dtq:sortall cabs))
          (princ (strcat "\nDTPA：全图 " (itoa (length cabs))
                         " 台柜（上->下、每排左->右）。"))
          (dtq:go cabs (strcat "柜体数据 · 全图 " (itoa (length cabs)) " 台"))
        )
      )
    )
  )
  (princ)
)

(princ "\n属性块-点排填数.lsp 已加载。命令 DTP —— 点一台柜，整排数据摊到一个面板里集中改。")
(princ)
