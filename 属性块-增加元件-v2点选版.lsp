;;; ============================================================
;;; 属性块-增加元件.lsp   v2（点选柜 + 对话框）   命令：JYJ / JYJN
;;; ------------------------------------------------------------
;;; 作用
;;;   给某台柜加一个元件（名称 / 型号 / 规格 / 数量），自动落图：
;;;     1 表头里已有同「名称 + 型号」的槽 -> 复用，不动表头；
;;;     2 表头里没有 -> 占第一个空槽（名称与型号都空的那个），
;;;        把名称 / 型号写进表头；
;;;     3 表头一个空槽都没有 -> 整排 +1 槽（调 属性块-槽位增减.lsp
;;;        的 gkc:expand），新槽号 = 原最大 +1；
;;;     4 该柜目标槽原有值 -> 直接覆盖（规格 / 数量），名称型号不动。
;;;
;;; 用法（点选版，推荐）
;;;   命令: JYJ
;;;   1) 直接在图上点那台柜的属性块 -> 柜号自动读出来，不用敲
;;;   2) 弹出对话框，四个参数一次填完：名称 / 型号 / 规格 / 数量
;;;      · 上方列表列出「表头已有元件」，点一下就把名称+型号填进去
;;;        （若本柜该槽已有规格/数量，也一并带出来，方便改）
;;;      · 也可以完全不理列表，四个框自己手输 —— 就是新元件
;;;   3) 确定后先打印一遍落点说明，再写图
;;;
;;; 用法（老的敲键版，保留）
;;;   命令: JYJN      逐项提示，直接回车取默认
;;;
;;; 两个脚本入口（未改动）
;;;   (jyj:add "AH03|电流互感器|LZZBJ9-12|300/5A|3")   直接写
;;;   (jyj:dry "AH03|电流互感器|LZZBJ9-12|300/5A|3")   只预演
;;;   字段顺序 = 柜号 | 名称 | 型号 | 规格 | 数量，数量可省 = 1。
;;;
;;; 依赖
;;;   属性块-槽位增减.lsp（gkc:* 一族）。本文件自己找它：先
;;;   findfile 同名文件，再试项目写死路径；都找不到就拒绝执行 ——
;;;   「整排加槽」那一步自己做不了（要克隆块定义 + 重造参照）。
;;;   对话框 DCL 由本文件运行时临时生成，用完即删，不用额外配文件。
;;;
;;; 为什么要整排加槽
;;;   表头块是「槽位级」的（元件名称 / 型号全排共用一份槽位表），
;;;   柜块只有规格 / 数量。只给一台柜加槽会让本排槽数不齐、表头
;;;   与柜块对不上，所以加槽一律走整排（与 GKC 同一口径）。
;;;
;;; 副作用（只有走到「加槽」那步才会发生）
;;;   整排参照重造 -> 句柄全变；块定义克隆成新名（原名-柜号）；
;;;   行区底部多一行，底部区（备注 / 容量功率行）整体下移一个行距，
;;;   柜块整体向下长高 —— 量一下图框下边距够不够。
;;;
;;; 编码：ANSI/GBK 无 BOM
;;; ============================================================
(vl-load-com)
(if (not (boundp 'SEP)) (setq SEP (chr 10)))

;; ================================================================
;; 一、依赖
;; ================================================================

;; 确保 属性块-槽位增减.lsp 已加载（gkc:expand 在就算在）；成了返回 T
(defun jyj:ensure ( / f)
  (if (not (member 'GKC:EXPAND (atoms-family 0)))
    (progn
      (setq f (findfile "属性块-槽位增减.lsp"))
      (if (null f)
        (setq f (findfile "D:/kk三部曲/148.CAD插件研究/lisp/属性块-槽位增减.lsp"))
      )
      (if f (load f))
    )
  )
  (if (member 'GKC:EXPAND (atoms-family 0)) T nil)
)

;; ================================================================
;; 二、小工具
;; ================================================================

;; 去首尾半角空格（只切 ASCII 空格，不会切坏中文）
(defun jyj:trim (s / n)
  (setq s (if s s ""))
  (while (and (> (strlen s) 0) (= 32 (ascii (substr s 1 1))))
    (setq s (substr s 2))
  )
  (setq n (strlen s))
  (while (and (> n 0) (= 32 (ascii (substr s n 1))))
    (setq s (substr s 1 (1- n)) n (1- n))
  )
  s
)

;; "a|b|c" -> ("a" "b" "c")（| 是 ASCII，按它切不会切坏中文）
(defun jyj:split (s / res p)
  (setq res nil s (if s s ""))
  (while (setq p (vl-string-search "|" s))
    (setq res (cons (substr s 1 p) res)
          s (substr s (+ p 2)))
  )
  (reverse (cons s res))
)

;; "柜号|名称|型号|规格|数量" -> 五个字段（缺的给 nil）
(defun jyj:parses (s / f)
  (setq f (jyj:split s))
  (list (nth 0 f) (nth 1 f) (nth 2 f) (nth 3 f) (nth 4 f))
)

;; 槽号补零：3 -> "03"
(defun jyj:nn (k)
  (if (< k 10) (strcat "0" (itoa k)) (itoa k))
)

;; ================================================================
;; 三、定位
;; ================================================================

;; 按柜号找柜块参照 —— 只认「有槽位属性的属性块」里的柜号
(defun jyj:bycab (cab / ss i n e res)
  (setq res nil i 0
        ss (ssget "_X" '((0 . "INSERT") (410 . "Model")))
        n (if ss (sslength ss) 0))
  (while (< i n)
    (setq e (ssname ss i))
    (if (and (null res) (gkc:maxslot (cdr (assoc 2 (entget e)))))
      (if (= (jyj:trim (gkc:tag e "柜号")) cab) (setq res e))
    )
    (setq i (1+ i))
  )
  res
)

;; 这个块定义是不是表头块 —— 槽位属性后缀里出现「名称」即是
(defun jyj:ishead (bn / r ed)
  (setq r nil)
  (foreach ed (gkc:attdefs bn)
    (if (= "名称" (gkc:suffix (cdr (assoc 2 ed)))) (setq r T))
  )
  r
)

;; 从整排参照里挑出表头块（一框一枚）
(defun jyj:heads (row / res e)
  (setq res nil)
  (foreach e row
    (if (jyj:ishead (cdr (assoc 2 (entget e)))) (setq res (cons e res)))
  )
  (reverse res)
)

;; ================================================================
;; 四、槽位判定
;; ================================================================

;; 表头里 名称 + 型号 都相同的槽号；没有返回 nil
(defun jyj:same (he name model / k mx r a b)
  (setq k 1 r nil mx (gkc:maxslot (cdr (assoc 2 (entget he)))))
  (while (and (<= k mx) (null r))
    (setq a (jyj:trim (gkc:tag he (gkc:mktag k "名称")))
          b (jyj:trim (gkc:tag he (gkc:mktag k "型号"))))
    (if (and (= a name) (= b model)) (setq r k))
    (setq k (1+ k))
  )
  r
)

;; 表头里第一个空槽号（名称与型号都空）；没有返回 nil
(defun jyj:empty (he / k mx r a b)
  (setq k 1 r nil mx (gkc:maxslot (cdr (assoc 2 (entget he)))))
  (while (and (<= k mx) (null r))
    (setq a (jyj:trim (gkc:tag he (gkc:mktag k "名称")))
          b (jyj:trim (gkc:tag he (gkc:mktag k "型号"))))
    (if (and (= "" a) (= "" b)) (setq r k))
    (setq k (1+ k))
  )
  r
)

;; ================================================================
;; 五、写值
;; ================================================================

;; 改参照上某个属性的值（entmod 改组码 1）；属性不存在返回 nil
;;   注意：AutoLISP 的 substr / strlen 按字节算，但这里 tag 是整串
;;   比较，不切中文，安全。
(defun jyj:setref (e tag val / sub ed hit)
  (setq sub (entnext e) hit nil)
  (while (and sub (= "ATTRIB" (cdr (assoc 0 (setq ed (entget sub))))))
    (if (= tag (cdr (assoc 2 ed)))
      (progn
        (if (not (equal (cdr (assoc 1 ed)) val))
          (entmod (subst (cons 1 val) (assoc 1 ed) ed))
        )
        (setq hit T)
      )
    )
    (setq sub (entnext sub))
  )
  hit
)

;; ================================================================
;; 六、定目标槽
;; ================================================================

;; 返回 (槽号 要不要写表头 报告串)；第一步就失败时 槽号 = nil
(defun jyj:resolve (cab name model / e row he s k hs ev mx nl e2 row2 he2)
  (setq e (jyj:bycab cab))
  (if (null e)
    (list nil nil (strcat "[JYJ] 图上没找到柜号「" cab "」的柜块。"))
    (progn
      (setq row (gkc:row e (gkc:snapshot)))
      (setq he (car (jyj:heads row)))
      (if (null he)
        (list nil nil (strcat "[JYJ] 柜号「" cab
                              "」所在那一排没认到表头块（槽位属性后缀要有「名称」）。"))
        (progn
          (setq s (jyj:same he name model))
          (if s
            (list s nil (strcat "表头已有同「名称+型号」-> 复用槽 " (itoa s) "，表头不动"))
            (progn
              (setq k (jyj:empty he))
              (if k
                (list k T (strcat "表头无此元件 -> 占空槽 " (itoa k) "，写名称/型号"))
                (progn
                  ;; 表头满了 -> 整排 +1 槽（走 GKC，参照会重造）
                  (setq mx (gkc:maxslot (cdr (assoc 2 (entget he)))))
                  (setq hs (gkc:join
                             (mapcar '(lambda (x) (cdr (assoc 5 (entget x)))) row)
                             ","))
                  (setq ev (gkc:expand hs 1))
                  (setq e2 (jyj:bycab cab))
                  (if (null e2)
                    (list nil nil (strcat "[JYJ] 整排加槽后找不到柜块了，先查图。GKC 回包：" ev))
                    (progn
                      (setq row2 (gkc:row e2 (gkc:snapshot)))
                      (setq he2 (car (jyj:heads row2)))
                      (setq nl (if he2 (gkc:maxslot (cdr (assoc 2 (entget he2)))) (1+ mx)))
                      (list nl T
                            (strcat "表头已满 -> 整排 +1 槽（" (itoa mx) " 到 " (itoa nl)
                                    "），本排参照已重造、句柄全变" SEP "      " ev))
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

;; ================================================================
;; 七、主流程
;; ================================================================

;; write = T 真写 / nil 只预演。返回报告串。
(defun jyj:main (cab name model spec qty write / r s needhead rep e row he evo evq sc)
  (setq cab (jyj:trim cab) name (jyj:trim name)
        model (jyj:trim model) spec (jyj:trim spec) qty (jyj:trim qty))
  (if (= "" qty) (setq qty "1"))
  (cond
    ((= "" cab) (setq rep "[JYJ] 柜号空着，图没动。"))
    ((= "" name) (setq rep "[JYJ] 元件名称空着，图没动。"))
    ((null (jyj:ensure))
     (setq rep (strcat "[JYJ] 缺依赖：找不到「属性块-槽位增减.lsp」——"
                       "两个文件放同一目录，或改 jyj:ensure 里的兜底路径。")))
    (T
     (setq r (jyj:resolve cab name model))
     (if (null (car r))
       (setq rep (caddr r))
       (progn
         (setq s (car r) needhead (cadr r))
         (setq e (jyj:bycab cab)
               row (gkc:row e (gkc:snapshot))
               he (car (jyj:heads row)))
         (setq evo (jyj:trim (gkc:tag e (gkc:mktag s "规格")))
               evq (jyj:trim (gkc:tag e (gkc:mktag s "数量"))))
         (setq rep (strcat "[JYJ] " (if write "" "预演（图不动）：")
                           cab " 槽 " (itoa s) " <- " name " / " model
                           " / " spec " / " qty SEP "      " (caddr r)))
         (if (and (/= "" evo) (or (/= evo spec) (/= evq qty)))
           (setq rep (strcat rep SEP "      覆盖：原「" evo " / " evq
                             "」-> 「" spec " / " qty "」"))
         )
         (if write
           (progn
             (setq sc 0)
             (if needhead
               (progn
                 (if (jyj:setref he (gkc:mktag s "名称") name) (setq sc (1+ sc)))
                 (if (jyj:setref he (gkc:mktag s "型号") model) (setq sc (1+ sc)))
               )
             )
             (if (jyj:setref e (gkc:mktag s "规格") spec) (setq sc (1+ sc)))
             (if (jyj:setref e (gkc:mktag s "数量") qty) (setq sc (1+ sc)))
             (setq rep (strcat rep SEP "      已写 " (itoa sc) " 格"
                               (if needhead "（表头 2 + 柜块 2）" "（柜块 2，表头没动）")))
           )
           (setq rep (strcat rep SEP "      预演结束，图没动。"))
         )
       )
     )
    )
  )
  rep
)

;; ================================================================
;; 八、入口
;; ================================================================

;; "AH03|电流互感器|LZZBJ9-12|300/5A|3"
(defun jyj:add (s / f)
  (setq f (jyj:parses s))
  (jyj:main (nth 0 f) (nth 1 f) (nth 2 f) (nth 3 f) (nth 4 f) T)
)

;; 同上，只预演不动图
(defun jyj:dry (s / f)
  (setq f (jyj:parses s))
  (jyj:main (nth 0 f) (nth 1 f) (nth 2 f) (nth 3 f) (nth 4 f) nil)
)

;; ================================================================
;; 九、v2 新增：点选柜块
;; ================================================================

;; 参照的属性表 ((标签 . 值) ...)（不依赖 gkc，点选阶段就能用）
(defun jyj:atts (ref / e ed out)
  (setq e (entnext ref) out nil)
  (while (and e (setq ed (entget e)) (= "ATTRIB" (cdr (assoc 0 ed))))
    (setq out (cons (cons (cdr (assoc 2 ed)) (cdr (assoc 1 ed))) out))
    (setq e (entnext e))
  )
  (reverse out)
)

;; 是不是槽位属性块（标签里有以「元件01」开头的）
;;   中文前缀不能用 substr 比（strlen 按字节算，会切半个字），
;;   统一判 vl-string-search 命中位置 = 0。
(defun jyj:isslot (atts / r a)
  (setq r nil)
  (foreach a atts (if (= 0 (vl-string-search "元件01" (car a))) (setq r T)))
  r
)

;; 点一台柜：返回柜号串；取消或点错返回 nil
(defun jyj:pickcab ( / r e ed atts cab done res)
  (setq done nil res nil)
  (while (not done)
    (setq r (entsel "\n点选要加元件的那台柜的属性块（回车取消）："))
    (if (null r)
      (setq done T)
      (progn
        (setq e (car r) ed (entget e) atts nil)
        (cond
          ((/= "INSERT" (cdr (assoc 0 ed)))
           (princ "\n  这不是块参照，请点柜块的框线或它的属性文字。"))
          ((not (jyj:isslot (setq atts (jyj:atts e))))
           (princ "\n  这个块没有槽位属性（标签要形如 元件01规格），请重选。"))
          ((= "" (setq cab (jyj:trim (cdr (assoc "柜号" atts)))))
           (princ "\n  这个块读不到「柜号」（多半点到表头块了），请点柜块。"))
          (t (setq res cab done T))
        )
      )
    )
  )
  res
)

;; ================================================================
;; 十、v2 新增：对话框
;; ================================================================

;; 表头已有元件清单 -> ((槽号 名称 型号 本柜规格 本柜数量) ...)
(defun jyj:opts (he cabref / k mx nm md sp qt out)
  (setq mx (gkc:maxslot (cdr (assoc 2 (entget he)))) k 1 out nil)
  (while (<= k mx)
    (setq nm (jyj:trim (gkc:tag he (gkc:mktag k "名称")))
          md (jyj:trim (gkc:tag he (gkc:mktag k "型号"))))
    (if (or (/= "" nm) (/= "" md))
      (progn
        (setq sp (if cabref (jyj:trim (gkc:tag cabref (gkc:mktag k "规格"))) "")
              qt (if cabref (jyj:trim (gkc:tag cabref (gkc:mktag k "数量"))) ""))
        (setq out (cons (list k nm md sp qt) out))
      )
    )
    (setq k (1+ k))
  )
  (reverse out)
)

;; 清单项的显示行
(defun jyj:optline (x)
  (strcat "槽" (jyj:nn (car x)) "  " (cadr x) " | " (caddr x)
          (if (/= "" (nth 3 x))
            (strcat "    [本柜现有 " (nth 3 x) " x " (nth 4 x) "]")
            ""
          )
  )
)

;; 生成临时 DCL，返回文件名
(defun jyj:dcl ( / fn fp)
  (setq fn (vl-filename-mktemp "jyjdlg" nil ".dcl"))
  (setq fp (open fn "w"))
  (foreach s
    (list
      "jyjdlg : dialog {"
      "  label = \"增加元件\";"
      "  : text { key = \"cab\"; }"
      "  : boxed_column {"
      "      label = \"表头已有元件（点一下带入名称/型号）\";"
      "      : list_box { key = \"lst\"; width = 56; height = 10; }"
      "    }"
      "  : boxed_column {"
      "      label = \"本次要写的四个参数\";"
      "      : edit_box { key = \"nm\"; label = \"名称：\"; edit_width = 36; }"
      "      : edit_box { key = \"md\"; label = \"型号：\"; edit_width = 36; }"
      "      : edit_box { key = \"sp\"; label = \"规格：\"; edit_width = 36; }"
      "      : edit_box { key = \"qt\"; label = \"数量：\"; edit_width = 10; }"
      "    }"
      "  : text { key = \"tip\"; }"
      "  ok_cancel;"
      "  : errtile {}"
      "}"
    )
    (write-line s fp)
  )
  (close fp)
  fn
)

;; 列表选中：把名称/型号（及本柜现有规格/数量）带进编辑框
(defun jyj:onsel (v / x)
  (setq x (nth (atoi v) jyj:*opts*))
  (if x
    (progn
      (set_tile "nm" (cadr x))
      (set_tile "md" (caddr x))
      (if (/= "" (nth 3 x)) (set_tile "sp" (nth 3 x)))
      (if (/= "" (nth 4 x)) (set_tile "qt" (nth 4 x)))
      (set_tile "error" "")
    )
  )
  (princ)
)

;; 收集编辑框的值；名称必填，空则报错留在对话框
(defun jyj:grab ( / nm)
  (setq jyj:*nm* (jyj:trim (get_tile "nm"))
        jyj:*md* (jyj:trim (get_tile "md"))
        jyj:*sp* (jyj:trim (get_tile "sp"))
        jyj:*qt* (jyj:trim (get_tile "qt")))
  (if (= "" jyj:*qt*) (setq jyj:*qt* "1"))
  (setq nm jyj:*nm*)
  (if (= "" nm)
    (progn (set_tile "error" "名称不能为空。") nil)
    T
  )
)

;; 弹框。成功返回 (名称 型号 规格 数量)，取消返回 nil
(defun jyj:dialog (cab opts / fn id ok x)
  (setq jyj:*opts* opts jyj:*nm* "" jyj:*md* "" jyj:*sp* "" jyj:*qt* "1")
  (setq fn (jyj:dcl) id (load_dialog fn))
  (if (< id 0)
    (progn (princ "\n[JYJ] 对话框加载失败，改用命令行逐项输入。") nil)
    (progn
      (if (not (new_dialog "jyjdlg" id))
        (progn (unload_dialog id) (princ "\n[JYJ] 对话框打不开，改用命令行逐项输入。") nil)
        (progn
          (set_tile "cab" (strcat "柜号：" cab "（点选得到，不用改）"))
          (set_tile "tip" (strcat "表头已有 " (itoa (length opts))
                                  " 个元件；名称+型号与已有完全相同则复用原槽，否则占空槽，没空槽则整排 +1 槽。"))
          (start_list "lst")
          (foreach x opts (add_list (jyj:optline x)))
          (end_list)
          (set_tile "qt" "1")
          (action_tile "lst" "(jyj:onsel $value)")
          (action_tile "accept" "(if (jyj:grab) (done_dialog 1))")
          (action_tile "cancel" "(done_dialog 0)")
          (setq ok (start_dialog))
          (unload_dialog id)
          (vl-file-delete fn)
          (if (= ok 1)
            (list jyj:*nm* jyj:*md* jyj:*sp* jyj:*qt*)
            nil
          )
        )
      )
    )
  )
)

;; ================================================================
;; 十一、命令外壳
;; ================================================================

;; 点选柜 + 对话框
(defun c:JYJ ( / cab e row he opts v)
  (if (null (jyj:ensure))
    (princ (strcat "\n[JYJ] 缺依赖：找不到「属性块-槽位增减.lsp」——"
                   "两个文件放同一目录，或改 jyj:ensure 里的兜底路径。"))
    (progn
      (setq cab (jyj:pickcab))
      (if (null cab)
        (princ "\n已取消。")
        (progn
          (princ (strcat "\n  -> 柜号 " cab))
          (setq e (jyj:bycab cab))
          (setq row (if e (gkc:row e (gkc:snapshot))))
          (setq he (if row (car (jyj:heads row))))
          (if (null he)
            (princ (strcat "\n[JYJ] 柜号「" cab
                           "」所在那一排没认到表头块（槽位属性后缀要有「名称」）。"))
            (progn
              (setq opts (jyj:opts he e))
              (setq v (jyj:dialog cab opts))
              (if (null v)
                (princ "\n已取消，图没动。")
                (princ (jyj:main cab (car v) (cadr v) (caddr v) (cadddr v) T))
              )
            )
          )
        )
      )
    )
  )
  (princ)
)

;; 老的命令行逐项版（保留）
(defun c:JYJN ( / cab name model spec qty)
  (princ "\n[JYJ] 给某台柜加一个元件 —— 表头同名就复用，没有空槽就整排 +1 槽")
  (princ "\n       字段顺序：柜号 | 名称 | 型号 | 规格 | 数量（数量可省 = 1）")
  (princ "\n       非交互入口（脚本 / 导入工具用，注意用 | 分段）：")
  (princ "\n         (jyj:add \"AH03|电流互感器|LZZBJ9-12|300/5A|3\")   直接写")
  (princ "\n         (jyj:dry \"AH03|电流互感器|LZZBJ9-12|300/5A|3\")   只预演")
  (setq cab (getstring "\n柜号 <AH01>: "))
  (if (= cab "") (setq cab "AH01"))
  (setq name (getstring T "\n元件名称: "))
  (setq model (getstring T "\n型号: "))
  (setq spec (getstring T "\n规格（可带空格）: "))
  (setq qty (getstring T "\n数量 <1>: "))
  (princ (jyj:main cab name model spec qty T))
  (princ)
)

(princ "\n[属性块-增加元件 v2] 已加载：JYJ（点柜 + 弹框）/ JYJN（命令行）")
(princ)
