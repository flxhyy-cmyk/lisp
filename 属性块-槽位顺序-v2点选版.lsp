;;; ============================================================
;;; 属性块-槽位顺序.lsp   v2（点选交互版）     命令：CWJH / CWJHN
;;; ------------------------------------------------------------
;;; 作用：把两个槽位的内容对调 —— 表头块（名称/型号）和各柜块
;;;       （规格/数量）一起换。只换格子里的字，不动任何几何：
;;;       槽位行高、行位置、块宽、插入点全部保持原样。
;;;
;;; 用法（点选版，推荐）：
;;;   命令: CWJH
;;;   1) 点选【第一个】槽位的任意一格属性文字（名称/型号/规格/数量）
;;;      -> 全图属于该槽的格子全部高亮，并报「几枚块有此槽」
;;;   2) 点选【第二个】槽位的任意一格属性文字，同样高亮 + 报数
;;;   3) 屏幕打印两槽逐字段对照 + 演练结果：要换几枚、跳过几枚
;;;      （此时一个字都没写）
;;;   4) 回答 Y 才真正互换；回答 N 直接放弃
;;;   5) 可接着换下一对，不用重开命令
;;;   两边报数不一样是正常的（分框后槽数可能不齐），差额就是会被
;;;   跳过的那些块，演练结果里逐枚列出来。
;;;
;;; 用法（老的敲键版，保留）：
;;;   命令: CWJHN
;;;   要交换的槽位(如 1 3): 3 4        <- 槽 3 与槽 4 对调
;;;   多组一次说完：3 4,7 9            <- 先 3<->4，再 7<->9
;;;
;;; 自动识别目标：模型空间里属性含「元件01…」的块参照都算
;;;   （柜块是 元件NN规格/数量，表头块是 元件NN名称/型号），
;;;   点选只用来指定"换哪两个槽"，改动仍然覆盖全图所有槽位块。
;;;
;;; 安全闸门：
;;;   1) 某块没有这两个槽 -> 该块跳过（分框后两框槽数可能不同，
;;;      比如第 1 框扩到 12 槽、第 2 框还是 11 槽），报告里列出
;;;      跳过几枚；其余块照常换。
;;;   2) 两槽的「规格」行数不一致 -> 整批一处都不写 —— 行高不同还
;;;      硬换，内容会串到隔壁行、看着错位。这条刻意保留，不做几何重排。
;;;
;;; 非交互入口（供脚本/外部调用）：
;;;   (cwjh:run "3 4")   直接换
;;;   (cwjh:dry "3 4")   只演练，不动图
;;;   (cwjh:show "3 4")  只读核对这两个槽的当前值
;;;
;;; 换完记得：Excel 那张属性表的槽序就过期了，要用先重新导出。
;;;
;;; 编码：ANSI/GBK 无 BOM
;;; ============================================================
(vl-load-com)

;; ------------------------------------------------------------
;; 槽号 -> 标签段（1..9 补零成 01..09）
;; ------------------------------------------------------------
(defun cwjh:nn (k)
  (if (< k 10) (strcat "0" (itoa k)) (itoa k))
)

;; ------------------------------------------------------------
;; 参照的属性表 ((标签 . 图元名) ...)
;; ------------------------------------------------------------
(defun cwjh:atts (ref / e ed out)
  (setq e (entnext ref) out nil)
  (while (and e (setq ed (entget e)) (= "ATTRIB" (cdr (assoc 0 ed))))
    (setq out (cons (cons (cdr (assoc 2 ed)) e) out))
    (setq e (entnext e))
  )
  (reverse out)
)

;; ------------------------------------------------------------
;; 是否槽位块：属性里有以「元件01」开头的标签
;; 注意：AutoLISP 的 strlen / substr 按「字节」算（"元件" = 4 字节、
;;    "元件01" = 6 字节），中文前缀不能用 (substr s 1 n) 比 —— 会
;;    切成半个字。改判 vl-string-search 的位置 = 0（命中开头）。
;; ------------------------------------------------------------
(defun cwjh:hasp (atts / r)
  (setq r nil)
  (foreach a atts (if (= 0 (vl-string-search "元件01" (car a))) (setq r T)))
  r
)

;; ------------------------------------------------------------
;; 块名
;; ------------------------------------------------------------
(defun cwjh:bname (ref / d)
  (setq d (entget ref))
  (cdr (assoc 2 d))
)

;; ------------------------------------------------------------
;; 槽 k 的字段表 ((后缀 . 图元名) ...)   后缀如 "规格" / "规格2" / "数量"
;; ------------------------------------------------------------
(defun cwjh:slotmap (atts k / pre L out)
  (setq pre (strcat "元件" (cwjh:nn k)) L (strlen pre) out nil)
  (foreach a atts
    (if (and (> (strlen (car a)) L) (= pre (substr (car a) 1 L)))
      (setq out (cons (cons (substr (car a) (1+ L)) (cdr a)) out))
    )
  )
  (reverse out)
)

;; ------------------------------------------------------------
;; 该槽的「规格」占几行（同理用 vl-string-search 判中文前缀）
;; ------------------------------------------------------------
(defun cwjh:rows (sm / n)
  (setq n 0)
  (foreach x sm (if (= 0 (vl-string-search "规格" (car x))) (setq n (1+ n))))
  n
)

;; ------------------------------------------------------------
;; 读 / 写属性值（组码 1；只改值不碰坐标）
;; ------------------------------------------------------------
(defun cwjh:getv (e / d)
  (setq d (entget e))
  (if (assoc 1 d) (cdr (assoc 1 d)) "")
)

(defun cwjh:setv (e v / d)
  (setq d (entget e))
  (entmod (subst (cons 1 v) (assoc 1 d) d))
)

;; ------------------------------------------------------------
;; 在一个参照内对调 a / b 两槽的全部字段（规格 / 数量 / 名称 / 型号）
;; 返回改动的格数（0 = 两边本来一样或没写）
;; ------------------------------------------------------------
(defun cwjh:swap1 (ref a b write / atts sa sb n x y va vb)
  (setq atts (cwjh:atts ref)
        sa   (cwjh:slotmap atts a)
        sb   (cwjh:slotmap atts b)
        n    0)
  (foreach x sa
    (setq y (assoc (car x) sb))
    (if y
      (progn
        (setq va (cwjh:getv (cdr x)) vb (cwjh:getv (cdr y)))
        (if (/= va vb)
          (progn
            (if write
              (progn (cwjh:setv (cdr x) vb) (cwjh:setv (cdr y) va))
            )
            (setq n (+ n 2))
          )
        )
      )
    )
  )
  (if (and write (> n 0)) (entupd ref))
  n
)

;; ------------------------------------------------------------
;; 全图目标参照（模型空间里带「元件01…」的块参照）
;; ------------------------------------------------------------
(defun cwjh:targets ( / ss n i ref out)
  (setq ss (ssget "_X" '((0 . "INSERT") (410 . "Model")))
        n  (if ss (sslength ss) 0)
        i  0
        out nil)
  (while (< i n)
    (setq ref (ssname ss i))
    (if (cwjh:hasp (cwjh:atts ref)) (setq out (cons ref out)))
    (setq i (1+ i))
  )
  (reverse out)
)

;; ------------------------------------------------------------
;; 参照类别：带「名称」的是表头块，带「规格」的是柜块
;;   （不用块名 —— 实测图上块名被历史改名污染过：
;;    "开关柜-AH01-AH01-AH01-AH01-AH01"，按名汇总没法看）
;; ------------------------------------------------------------
(defun cwjh:kind (ref / sm)
  (setq sm (cwjh:slotmap (cwjh:atts ref) 1))
  (cond
    ((assoc "名称" sm) "表头块")
    ((assoc "规格" sm) "柜块")
    (t "其它")
  )
)

;; ------------------------------------------------------------
;; 按类别汇总参照数  ->  (("表头块" . 2) ("柜块" . 10))
;; ------------------------------------------------------------
(defun cwjh:kindcount (refs / a k r)
  (setq a nil)
  (foreach ref refs
    (setq k (cwjh:kind ref))
    (if (setq r (assoc k a))
      (setq a (subst (cons k (1+ (cdr r))) r a))
      (setq a (cons (cons k 1) a))
    )
  )
  (reverse a)
)

;; ------------------------------------------------------------
;; 参照标签：柜块取「柜号」，表头块没有柜号 -> "表头"
;; ------------------------------------------------------------
(defun cwjh:label (ref / atts a)
  (setq atts (cwjh:atts ref) a (assoc "柜号" atts))
  (if a (cwjh:getv (cdr a)) "表头")
)

;; ------------------------------------------------------------
;; 只读核对：列出这些槽在各块里的当前值
;;   (cwjh:show "3 4")
;; ------------------------------------------------------------
(defun cwjh:show (s / pairs refs out ref atts r p k sm x)
  (setq pairs (cwjh:parse s) refs (cwjh:targets) out "")
  (foreach ref refs
    (setq atts (cwjh:atts ref) r "")
    (foreach p pairs
      (foreach k p
        (setq sm (cwjh:slotmap atts k))
        (foreach x sm
          (setq r (strcat r "槽" (cwjh:nn k) (car x) "=" (cwjh:getv (cdr x)) " "))
        )
      )
    )
    (if (/= r "")
      (setq out (strcat out (cwjh:kind ref) "-" (cwjh:label ref) "| " r "\n"))
    )
  )
  (princ out)
  out
)

;; ------------------------------------------------------------
;; 解析 "3 4,7 9" -> ((3 4) (7 9))
;; ------------------------------------------------------------
(defun cwjh:parse (s / i ch num nums out)
  (setq i 1 num "" nums nil)
  (while (<= i (strlen s))
    (setq ch (substr s i 1))
    (if (and (>= (ascii ch) 48) (<= (ascii ch) 57))
      (setq num (strcat num ch))
      (if (/= num "") (setq nums (cons (atoi num) nums) num ""))
    )
    (setq i (1+ i))
  )
  (if (/= num "") (setq nums (cons (atoi num) nums)))
  (setq nums (reverse nums) out nil)
  (while (>= (length nums) 2)
    (setq out (cons (list (car nums) (cadr nums)) out)
          nums (cddr nums))
  )
  (reverse out)
)

;; ------------------------------------------------------------
;; 主流程：pairs + write(T=真写 / nil=演练)
;; ------------------------------------------------------------
(defun cwjh:go (pairs write / refs bad atts p ref sa sb bs total nc l msg part skip skl ok)
  (setq msg "")
  (if (null pairs)
    (setq msg "\n没解析到槽位对。")
    (progn
      (setq refs (cwjh:targets))
      (if (null refs)
        (setq msg "\n没找到槽位属性块（属性里要有「元件01…」标签）。")
        (progn
          ;; --- 预检（逐块）---
          ;;   缺槽     -> 该块跳过：分框后两框槽数可能不同（如一框已扩
          ;;               到 12 槽、另一框还是 11 槽），硬要它参与没道理
          ;;   行数不等 -> 记入 bad，整批一处都不写（硬换会让内容串行）
          (setq bad nil part nil skip 0 skl nil)
          (foreach ref refs
            (setq atts (cwjh:atts ref) ok T)
            (foreach p pairs
              (setq sa (cwjh:slotmap atts (car p))
                    sb (cwjh:slotmap atts (cadr p)))
              (cond
                ((or (null sa) (null sb))
                 (setq ok nil)
                 (setq bs (strcat (cwjh:kind ref) "-" (cwjh:label ref)))
                 (if (not (member bs skl)) (setq skl (cons bs skl))))
                ((/= (cwjh:rows sa) (cwjh:rows sb))
                 (setq ok nil)
                 (setq bs (strcat (cwjh:kind ref) "-" (cwjh:label ref) "：槽"
                                  (itoa (car p)) "/槽" (itoa (cadr p)) " 规格行数不等"))
                 (if (not (member bs bad)) (setq bad (cons bs bad))))
              )
            )
            (if ok (setq part (cons ref part)) (setq skip (1+ skip)))
          )
          (setq part (reverse part) skl (reverse skl))
          (if bad
            (progn
              (setq msg "\n预检不过，一处都没写：")
              (foreach b bad (setq msg (strcat msg "\n  " b)))
            )
            (progn
              ;; --- 执行（只对同时具备这两个槽的块） ---
              (setq total 0)
              (foreach ref part
                (setq l 0)
                (foreach p pairs (setq l (+ l (cwjh:swap1 ref (car p) (cadr p) write))))
                (setq total (+ total l))
              )
              (setq nc (cwjh:kindcount part))
              (setq msg (strcat (if write "\n完成：" "\n演练（未写）：")
                                (apply 'strcat
                                       (mapcar '(lambda (p) (strcat (itoa (car p)) "<->" (itoa (cadr p)) " "))
                                               pairs))
                                "\n槽位块 " (itoa (length part)) " 枚（"
                                (apply 'strcat
                                       (mapcar '(lambda (x) (strcat (car x) " " (itoa (cdr x)) " / "))
                                               nc))
                                "）"))
              (if (> skip 0)
                (progn
                  (setq msg (strcat msg "\n跳过 " (itoa skip) " 枚（没有这些槽位）："))
                  (foreach b skl (setq msg (strcat msg "\n  " b)))
                )
              )
              (setq msg (strcat msg "\n改动 " (itoa total) " 格"))
              (if (= 0 (length part))
                (setq msg (strcat msg "\n（没有块同时具备这些槽位，什么都没做）"))
              )
              (if (and write (> total 0))
                (setq msg (strcat msg "\n注意：Excel 属性表的槽序已过期，要用请重新导出。"))
              )
            )
          )
        )
      )
    )
  )
  (princ msg)
  msg
)

(defun cwjh:run (s) (cwjh:go (cwjh:parse s) T))
(defun cwjh:dry (s) (cwjh:go (cwjh:parse s) nil))

;; ============================================================
;; 以下为 v2 新增：点选交互
;; ============================================================

;; ------------------------------------------------------------
;; 属性标签 -> 槽号   "元件03规格2" -> 3   不是槽位标签则返回 nil
;;   （"元件" 占 4 字节，数字从第 5 字节起，取到非数字为止）
;; ------------------------------------------------------------
(defun cwjh:tag->slot (tag / s i ch num)
  (if (= 0 (vl-string-search "元件" tag))
    (progn
      (setq s (substr tag 5) num "" i 1)
      (while (and (<= i (strlen s))
                  (setq ch (substr s i 1))
                  (>= (ascii ch) 48)
                  (<= (ascii ch) 57))
        (setq num (strcat num ch) i (1+ i))
      )
      (if (/= num "") (atoi num))
    )
  )
)

;; ------------------------------------------------------------
;; 属性所属的块参照：先用组码 330（宿主），取不到再全图反查
;; ------------------------------------------------------------
(defun cwjh:owner (e / o out)
  (setq o (cdr (assoc 330 (entget e))))
  (if (and o (= "INSERT" (cdr (assoc 0 (entget o)))))
    o
    (progn
      (setq out nil)
      (foreach ref (cwjh:targets)
        (if (and (null out) (member e (mapcar 'cdr (cwjh:atts ref))))
          (setq out ref)
        )
      )
      out
    )
  )
)

;; ------------------------------------------------------------
;; 高亮 / 取消高亮：全图属于槽 k 的所有格子
;; ------------------------------------------------------------
;; 返回「带这个槽」的块数 —— 分框后各框槽数可能不同，先让人看见
;; 到底几枚块会参与
(defun cwjh:hl-on (refs k / x sm n)
  (setq n 0)
  (foreach ref refs
    (setq sm (cwjh:slotmap (cwjh:atts ref) k))
    (if sm (setq n (1+ n)))
    (foreach x sm
      (redraw (cdr x) 3)
      (setq cwjh:*hl* (cons (cdr x) cwjh:*hl*))
    )
  )
  n
)

(defun cwjh:hl-off ( / e)
  (foreach e cwjh:*hl* (if (entget e) (redraw e 4)))
  (setq cwjh:*hl* nil)
)

;; ------------------------------------------------------------
;; 点选一个槽：返回 (槽号 . 属性图元)，用户回车/右键取消返回 nil
;;   点错了（点到框线、点到非槽位属性）会提示并让重点
;; ------------------------------------------------------------
(defun cwjh:pick (msg / r e ed k done res)
  (setq done nil res nil)
  (while (not done)
    (setq r (nentsel msg))
    (if (null r)
      (setq done T)
      (progn
        (setq e (car r) ed (entget e))
        (if (= "ATTRIB" (cdr (assoc 0 ed)))
          (progn
            (setq k (cwjh:tag->slot (cdr (assoc 2 ed))))
            (if k
              (setq res (cons k e) done T)
              (princ (strcat "\n  这格不是槽位字段（标签 "
                             (cdr (assoc 2 ed))
                             "，要形如 元件03规格），请重选。"))
            )
          )
          (princ "\n  请点属性文字本身（名称/型号/规格/数量），别点框线或其它图元。")
        )
      )
    )
  )
  res
)

;; ------------------------------------------------------------
;; 挑一枚「同时具备 a、b 两槽」的块做对照样本：
;;   优先用点中的那枚；它要是缺另一个槽（分框后槽数不齐很常见），
;;   就顺着全图找第一枚齐的，找不到返回 nil
;; ------------------------------------------------------------
(defun cwjh:pvref (refs a b prefer / atts out)
  (setq out nil)
  (if (and prefer
           (setq atts (cwjh:atts prefer))
           (cwjh:slotmap atts a)
           (cwjh:slotmap atts b))
    (setq out prefer)
    (foreach ref refs
      (if (null out)
        (progn
          (setq atts (cwjh:atts ref))
          (if (and (cwjh:slotmap atts a) (cwjh:slotmap atts b)) (setq out ref))
        )
      )
    )
  )
  out
)

;; ------------------------------------------------------------
;; 逐字段对照预览（拿样本块举例，看得最直观）
;; ------------------------------------------------------------
(defun cwjh:preview (ref a b / atts sa sb x y)
  (if (null ref)
    (princ (strcat "\n--- 没有块同时具备槽 " (cwjh:nn a) " 与槽 " (cwjh:nn b)
                   "，无从对照 ---"))
    (progn
      (setq atts (cwjh:atts ref)
            sa   (cwjh:slotmap atts a)
            sb   (cwjh:slotmap atts b))
      (princ (strcat "\n--- 对照预览（以 " (cwjh:kind ref) "-" (cwjh:label ref) " 为例）---"))
      (foreach x sa
        (setq y (assoc (car x) sb))
        (if y
          (princ (strcat "\n  " (car x)
                         "：槽" (cwjh:nn a) "「" (cwjh:getv (cdr x)) "」"
                         "  <->  "
                         "槽" (cwjh:nn b) "「" (cwjh:getv (cdr y)) "」"))
        )
      )
    )
  )
  (princ)
)

;; ------------------------------------------------------------
;; 命令：CWJH —— 点选两槽，确认后互换
;; ------------------------------------------------------------
(defun c:CWJH ( / *error* refs pa pb a b na nb ref msg ans more)
  (defun *error* (m)
    (cwjh:hl-off)
    (if (and m (/= m "Function cancelled") (/= m "quit / exit abort"))
      (princ (strcat "\n出错：" m))
    )
    (princ "\n已中断，图面未改动。")
    (princ)
  )
  (setq cwjh:*hl* nil)
  (setq refs (cwjh:targets))
  (if (null refs)
    (princ "\n没找到槽位属性块（属性里要有「元件01…」标签）。")
    (progn
      (princ (strcat "\n全图槽位块 " (itoa (length refs)) " 枚。点选要互换的两个槽（回车取消）。"))
      (setq more T)
      (while more
        (setq more nil)
        (setq pa (cwjh:pick "\n点选【第一个】槽位的任意一格属性文字："))
        (if (null pa)
          (princ "\n已取消。")
          (progn
            (setq a (car pa))
            (setq na (cwjh:hl-on refs a))
            (princ (strcat "\n  -> 槽 " (cwjh:nn a) " 已高亮（"
                           (itoa na) " 枚块有此槽）。"))
            (setq pb (cwjh:pick "\n点选【第二个】槽位的任意一格属性文字："))
            (cond
              ((null pb) (princ "\n已取消。"))
              ((= (car pb) a) (princ "\n两次点的是同一个槽，本次取消。"))
              (t
                (setq b (car pb))
                (setq nb (cwjh:hl-on refs b))
                (princ (strcat "\n  -> 槽 " (cwjh:nn b) " 已高亮（"
                               (itoa nb) " 枚块有此槽）。"))
                (setq ref (cwjh:pvref refs a b (cwjh:owner (cdr pa))))
                (cwjh:preview ref a b)
                ;; 先演练一遍：
                ;;   预检不过（规格行数不等）-> 整批不写，到此为止
                ;;   没有块同时具备两槽      -> 无事可做，不必问
                ;;   演练里报的「跳过 N 枚」是正常情况，照常问
                (setq msg (cwjh:go (list (list a b)) nil))
                (if (or (vl-string-search "预检不过" msg)
                        (vl-string-search "什么都没做" msg))
                  (princ "\n本次不执行。")
                  (progn
                    (initget "Y N")
                    (setq ans (getkword (strcat "\n确认把槽 " (cwjh:nn a)
                                                " 与槽 " (cwjh:nn b)
                                                " 互换? [是(Y)/否(N)] <Y>：")))
                    (if (or (null ans) (= ans "Y"))
                      (cwjh:go (list (list a b)) T)
                      (princ "\n已放弃，图面未改动。")
                    )
                  )
                )
              )
            )
            (cwjh:hl-off)
            (initget "Y N")
            (setq ans (getkword "\n还要换下一对吗? [是(Y)/否(N)] <N>："))
            (if (= ans "Y") (setq more T))
          )
        )
      )
    )
  )
  (cwjh:hl-off)
  (princ)
)

;; ------------------------------------------------------------
;; 命令：CWJHN —— 老的敲槽号方式（保留，支持一次多组）
;; ------------------------------------------------------------
(defun c:CWJHN ( / s)
  (setq s (getstring T "\n要交换的槽位(如 1 3；多组 1 3,5 6)："))
  (if (= s "")
    (princ "\n已取消。")
    (cwjh:go (cwjh:parse s) T)
  )
  (princ)
)

(princ "\n属性块-槽位顺序 v2 已加载：CWJH（点选两槽互换）/ CWJHN（敲槽号）")
(princ)
