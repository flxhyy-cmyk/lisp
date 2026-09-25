;;; ============================================================
;;; 属性块-铺图-面板版.lsp                          命令：PGT（弹面板）
;;; ------------------------------------------------------------
;;; 作用
;;;   按「槽位数 + 柜号前缀 + 柜总数」铺一张一次系统图空骨架，其余数据全空：
;;;     1 每框插一份图框（图框模版.dwg）并炸开；
;;;     2 每框插一枚表头块（槽位级，全排共用一份槽位表）；
;;;     3 逐台插柜属性块，只写「柜号」，其余属性一律留空；
;;;     4 每台柜盖一枚占位块，落在系统图位置上。
;;;
;;; 两个入口
;;;   1 面板（给人用）：命令 PGT，弹出面板，填：
;;;        槽位数      空 / 0 = 用模板自带的槽位数
;;;        每框台数    空 / 0 = 按框宽自动
;;;        柜号前缀    如 AH（可留空）
;;;        柜总数      如 12 -> AH01 ~ AH12
;;;                    序号从 1 起，至少 2 位；柜总数 >= 100 时补成 3 位
;;;                    （如 120 台 -> AH001 ~ AH120）
;;;        只预演      勾上 = 只算不铺
;;;      上次填的值，本次 CAD 会话内记住。
;;;   2 口述（给脚本 / 外部调用，原接口保留）：
;;;        (pgt:run 9 "AH01,AH02,AH03")   槽位数 + 柜号清单，真铺
;;;        (pgt:dry 9 "AH01,AH02,AH03")   同上，只算不铺
;;;      槽位数给 nil 或 0 -> 用模板自带的槽位数；
;;;      柜号既可以是 "AH01,AH02"，也可以是 ("AH01" "AH02")。
;;;
;;; 依赖
;;;   属性块-槽位增减.lsp（gkc:* 一族；槽位数与模板不同时整排调一次）
;;;   本文件和上面这个 lsp 放在同一处（CAD 支持路径里能 findfile 到）
;;;   模板 dwg（lisp 所在目录的上一级，或 CAD 支持路径能找到）：
;;;       图框模版.dwg  柜块模版.dwg  表头块模版.dwg  占位块.dwg
;;;   找不到模板就拒绝执行 —— 块定义只活在图纸自身里，
;;;   换一张图铺图必须靠模板 dwg 把定义带进来。
;;;
;;; 副作用
;;;   1 块定义要先「带进本图」才能量几何、才能插（插远处再删，图元不留）；
;;;     这一步是唯一走命令行 -INSERT 的地方（只用来把 dwg 带进来），
;;;     期间把 ATTREQ 压成 0，免得属性块索要输入；
;;;   2 槽位数与模板不同 -> 在远地造一份新定义（整排重造：句柄全变、
;;;      块名克隆成「原名-柜号/表头」），临时参照随即删掉；
;;;   3 铺进来的块定义留在这张图里（换图要重新铺一次）；
;;;   4 不存盘 —— 落不落盘你自己定。
;;;
;;; 插块为什么走 COM 而不是 (command "_.-INSERT" …)
;;;   命令行插「带属性的块」时 AutoCAD 会逐个属性索要输入（表头 32 个 /
;;;   柜块 37 个），(command …) 就永远等不到下一句 —— 实测卡死两次，
;;;   连 ESC 都注入不进去，得人工到 CAD 窗口按 ESC 才回来。
;;;   COM 的 InsertBlock 一个提示都不弹，属性按 ATTDEF 默认值带入；
;;;   server.py 主路径用的也是它（5964 / 6744 / 9041）。
;;;   炸开同样走 COM（BlockReference.Explode 不删源对象，要手动删）。
;;;
;;; 版面口径（全部从图上量出来的，不是猜的）
;;;   图框：定义里那个匿名块参照（*U3）就是框；它的几何 = 420x297，
;;;         框左下角在定义内的位置 = 锚块插入点 + 锚块几何最小角
;;;         -> 插入点要反向补偿这两项之和（锚块几何起于 0 时退化成只减插入点）
;;;   柜块：几何 x[0,35] y[-36,178]（15 槽）；槽位增减只动下半截，几何顶不变
;;;   表头块：几何 x[0,40]，与柜块同一套 y
;;;   占位块：几何 x[174.48,209.48] y[119.23,170.81]（基点离几何很远）
;;;   表头几何左 = 框左 + 35；第 1 台柜几何左 = 表头几何左 + 表头宽
;;;   柜块几何顶 = 框顶 - 14；占位块几何顶 = 柜块几何顶 - 3
;;;   第 2 框 = 第 1 框 + 框宽 + 50
;;;
;;; 编码：ANSI/GBK 无 BOM
;;; ============================================================
(vl-load-com)
(if (not (boundp 'SEP)) (setq SEP (chr 10)))

;; 表头几何左离框左 / 柜块几何顶离框顶 / 框间距 / 占位块顶离柜块顶
(setq *pgt:padl* 35.0)
(setq *pgt:padt* 14.0)
(setq *pgt:cgap* 50.0)
(setq *pgt:phgap* 3.0)

;; 造副本定义时临时摆在多远处（离正式图够远，gkc 不会认成同一排）
(setq *pgt:far* 20000.0)

;; ================================================================
;; 一、路径与依赖
;; ================================================================

(defun pgt:dir ( / f)
  (setq f (or (findfile "属性块-铺图-面板版.lsp") (findfile "属性块-铺图.lsp")))
  (if f (vl-filename-directory f) nil)
)

;; 模板 dwg 在哪：先看 lisp 上一级，再试写死路径
(defun pgt:tpldir ( / d)
  (setq d (pgt:dir))
  (if d (setq d (vl-filename-directory d)))
  (cond
    ((and d (findfile (strcat d "/图框模版.dwg"))) d)
    ((findfile "D:/kk三部曲/148.CAD插件研究/图框模版.dwg")
     "D:/kk三部曲/148.CAD插件研究")
    (T d)
  )
)

(defun pgt:tpl (n / d f)
  (setq d (pgt:tpldir))
  (if d (setq f (findfile (strcat d "/" n))))
  (if (null f) (setq f (findfile n)))
  f
)

;; 确保 属性块-槽位增减.lsp 已加载（gkc:expand 在就算在）
(defun pgt:ensure ( / f)
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
(defun pgt:trim (s / n)
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

;; "AH01,AH02 AH03" -> ("AH01" "AH02" "AH03")
;; 按字节逐个走：中文柜号被切成单字节再拼回去，字节序列不变，结果正确
(defun pgt:split (s / res cur i n ch)
  (setq res nil cur "" s (pgt:trim (if s s "")) n (strlen s) i 1)
  (while (<= i n)
    (setq ch (substr s i 1))
    (if (member ch (list "," " " (chr 9) ";" (chr 10)))
      (progn (if (/= cur "") (setq res (cons cur res))) (setq cur ""))
      (setq cur (strcat cur ch))
    )
    (setq i (1+ i))
  )
  (if (/= cur "") (setq res (cons cur res)))
  (reverse res)
)

;; 向上取整 a/b（整数相除前先转实数 —— AutoLISP 的 / 对整数是整除）
(defun pgt:ceil (a b)
  (fix (/ (+ a (1- b)) (float b)))
)

;; 块定义是否存在
(defun pgt:defp (bn)
  (if (tblobjname "BLOCK" bn) T nil)
)

;; 量块定义内部几何范围 -> (minx miny maxx maxy)；块内实体坐标是相对基点的
;; 不用 COM 包围盒：LISP 侧拿到的是 safearray 不是 variant，容易炸
(defun pgt:rng (bn / e ed x q minx miny maxx maxy)
  (setq e (tblobjname "BLOCK" bn))
  (if (null e)
    nil
    (progn
      (setq e (entnext e) minx 1e9 miny 1e9 maxx -1e9 maxy -1e9)
      (while (and e (/= "ENDBLK" (cdr (assoc 0 (entget e)))))
        (setq ed (entget e))
        (foreach x ed
          (setq q (cdr x))
          (if (and (member (car x) (list 10 11)) (listp q))
            (progn
              (if (< (car q) minx) (setq minx (car q)))
              (if (< (cadr q) miny) (setq miny (cadr q)))
              (if (> (car q) maxx) (setq maxx (car q)))
              (if (> (cadr q) maxy) (setq maxy (cadr q)))
            )
          )
        )
        (setq e (entnext e))
      )
      (list minx miny maxx maxy)
    )
  )
)

;; 图框模版定义里的「锚块」：那个匿名块参照（名以 * 开头），
;; 它的位置就是框左下角相对基点的偏移。返回 (块名 锚点x 锚点y)
(defun pgt:fanchor ( / e ed bn p r)
  (setq e (tblobjname "BLOCK" "图框模版") r nil)
  (if e
    (progn
      (setq e (entnext e))
      (while (and e (/= "ENDBLK" (cdr (assoc 0 (entget e)))))
        (setq ed (entget e))
        (if (= "INSERT" (cdr (assoc 0 ed)))
          (progn
            (setq bn (cdr (assoc 2 ed)) p (cdr (assoc 10 ed)))
            (if (and bn p (= "*" (substr bn 1 1)))
              (setq r (list bn (car p) (cadr p)))
            )
          )
        )
        (setq e (entnext e))
      )
    )
  )
  r
)

;; 取 GKC 回包里 "|标记值|" 的那一段（标记是 ASCII）
;; 注意：一律用 gkc:bfind 定位 —— 它按字节回 1 基下标，配 substr 不会错位
(defun pgt:kv (s key / p q)
  (setq p (gkc:bfind s key))
  (if p
    (progn
      (setq p (+ p (strlen key)))
      (setq q (gkc:bfind (substr s p) "|"))
      (substr s p (if q (1- q) 9999))
    )
    nil
  )
)

;; 从 "@defs=旧>新;旧>新;" 里找指定旧名对应的新名
(defun pgt:newdef (s obn / r p q seg)
  (setq r nil s (if s s ""))
  (while (setq p (gkc:bfind s ";"))
    (setq seg (substr s 1 (1- p)) s (substr s (1+ p)))
    (setq q (gkc:bfind seg ">"))
    (if (and q (= obn (substr seg 1 (1- q))))
      (setq r (substr seg (1+ q)))
    )
  )
  (if (> (strlen s) 0)
    (progn
      (setq q (gkc:bfind s ">"))
      (if (and q (= obn (substr s 1 (1- q))))
        (setq r (substr s (1+ q)))
      )
    )
  )
  r
)

;; 删掉 *pgt:far* 附近的块参照（±500）—— 造副本定义留下的临时件
;; 注意：用「区间」而不是「大于」：正式图起点万一摆得很大，按 > 判会误删
(defun pgt:purgef (c / ss n i e ed p cnt lo hi)
  (setq ss (ssget "_X" '((0 . "INSERT") (410 . "Model")))
        n (if ss (sslength ss) 0) i 0 cnt 0
        lo (- c 500.0) hi (+ c 500.0))
  (while (< i n)
    (setq e (ssname ss i) ed (entget e) p (cdr (assoc 10 ed)))
    (if (and p (> (car p) lo) (< (car p) hi))
      (progn (entdel e) (setq cnt (1+ cnt)))
    )
    (setq i (1+ i))
  )
  cnt
)

;; ================================================================
;; 三、插块（全走 COM）
;; ================================================================
;; 为什么不用 (command "_.-INSERT" …) 插正式块：见文件头。COM 不问任何东西。
;; 变体 / 安全数组 -> 普通表（COM 在 LISP 侧两种都可能吐出来）
(defun pgt:aslist (r / v)
  (cond
    ((= (type r) 'safearray)
     (setq v (vl-catch-all-apply 'vlax-safearray->list (list r)))
     (if (vl-catch-all-error-p v) nil v))
    ((= (type r) 'variant)
     (setq v (vl-catch-all-apply 'vlax-variant-value (list r)))
     (if (vl-catch-all-error-p v)
       nil
       (if (= (type v) 'safearray)
         (progn
           (setq v (vl-catch-all-apply 'vlax-safearray->list (list v)))
           (if (vl-catch-all-error-p v) nil v))
         nil)))
    ((listp r) r)
    (T nil)
  )
)

;; 当前文档的模型空间（COM），拿不到返回 nil
(defun pgt:ms ( / app d)
  (setq app (vl-catch-all-apply 'vlax-get-acad-object nil))
  (if (vl-catch-all-error-p app)
    nil
    (progn
      (setq d (vl-catch-all-apply 'vla-get-ActiveDocument (list app)))
      (if (vl-catch-all-error-p d)
        nil
        (progn
          (setq d (vl-catch-all-apply 'vla-get-ModelSpace (list d)))
          (if (vl-catch-all-error-p d) nil d)
        )
      )
    )
  )
)

;; 插一个块参照（按块名，块定义必须已在本图）-> VLA 对象；失败 nil
;; 先按 6 参（x y z 比例 + 转角），不行再退 5 参（x y 比例 + 转角）——
;;   不同版本的 InsertBlock 签名不一样，两条都试一次比赌一条稳
(defun pgt:ins (bn pt / ms p r)
  (setq ms (pgt:ms))
  (if (null ms)
    nil
    (progn
      (setq p (vlax-3d-point (car pt) (cadr pt) 0.0))
      ;; 必须走直连包装 vla-InsertBlock，6 参：插入点 + 块名 + x y z 比例 + 转角
      ;;   vlax-invoke 那条路在本机不通（实测 2026-09-20）：
      ;;     6 参回「发生意外。」，5 参回「无效的参数数目。」
      ;;     —— 和当初 vlax-invoke 'Move 报「发生意外」是同一类毛病
      ;;   插入点也必须用 vlax-3d-point：传普通表会回
      ;;     「此类型的 LISP 值不能强制转换成 VARIANT」
      (setq r (vl-catch-all-apply 'vla-InsertBlock
                (list ms p bn 1.0 1.0 1.0 0.0)))
      (if (vl-catch-all-error-p r) (setq r nil))
      r
    )
  )
)

;; 命令行 -INSERT，只用来把「模板 dwg 文件」带进本图（按路径插，插完就删）
;;   COM 的 InsertBlock 不吃文件路径（server.py 8693 记过这条），所以这里
;;   是全文唯一保留命令行插块的地方 —— 这条路实测是通的（临时件不带属性值）。
(defun pgt:insf (path pt / before e oa)
  (setq before (entlast))
  (setq oa (getvar "ATTREQ"))
  (setvar "ATTREQ" 0)
  (command "_.-INSERT" (strcat "\"" path "\"") pt 1 1 0)
  (setvar "ATTREQ" oa)
  (setq e (entlast))
  (if (equal e before) nil e)
)

;; 炸开一层（COM）+ 删源对象：COM 的 Explode 不像 EXPLODE 命令那样删源
(defun pgt:exp (ref / r)
  (if ref
    (progn
      (setq r (vl-catch-all-apply 'vla-Explode (list ref)))
      (if (vl-catch-all-error-p r) (setq r nil))
      (vl-catch-all-apply 'vla-Delete (list ref))
      (pgt:aslist r)
    )
  )
)

;; 写属性值（按标签精确定位，只改这一个）
(defun pgt:setatt (ref tg val / lst a tgv)
  (if ref
    (progn
      (setq lst (pgt:aslist (vl-catch-all-apply 'vla-GetAttributes
                               (list ref))))
      (foreach a lst
        (if (= (type a) 'VLA-OBJECT)
          (progn
            (setq tgv (vl-catch-all-apply 'vla-get-TagString (list a)))
            (if (and (not (vl-catch-all-error-p tgv)) (= tg tgv))
              (vl-catch-all-apply 'vla-put-TextString (list a val))
            )
          )
        )
      )
    )
  )
  val
)

;; 把模板 dwg 的块定义带进本图（已经有就不动它）；成了返回 T
(defun pgt:import (dwg bn / f e)
  (if (pgt:defp bn)
    T
    (progn
      (setq f (pgt:tpl dwg))
      (if (null f)
        nil
        (progn
          (setq e (pgt:insf f (list *pgt:far* 0.0)))
          (if e (entdel e))
          (pgt:defp bn)
        )
      )
    )
  )
)

;; ================================================================
;; 四、定槽：拿到「槽数 = n」的柜块 / 表头块定义名
;; ================================================================
;; 模板是固定槽数的。用户要别的槽数时，在远地插一对（1 表头 + 1 柜），
;; 调一次 gkc:expand 让整排变成 n 槽，记下新定义名，再把临时参照删掉。
;; 这样「调槽」整张图只发生一次，正式铺图直接用现成的定义。
;; 返回 (柜块名 表头块名 实际槽数)
;; 注意：临时件摆成「相邻」（表头在 20000、柜块在 20040，表头宽正好 40），
;;   gkc:rowfill 才会按几何把它们判成同一排，一起 ±k；分开摆会只变一半。
(defun pgt:defs (n cab0 hdr0 / cur r e1 e2 h1 h2 nc nh)
  (setq cur (gkc:maxslot cab0))
  (if (or (null n) (<= n 0) (null cur) (= cur n))
    (list cab0 hdr0 cur)
    (progn
      (setq e1 (pgt:ins hdr0 (list *pgt:far* 0.0)))
      (setq e2 (pgt:ins cab0 (list (+ *pgt:far* 40.0) 0.0)))
      (if (or (null e1) (null e2))
        (list cab0 hdr0 cur)
        (progn
          (setq h1 (vl-catch-all-apply 'vla-get-Handle (list e1)))
          (setq h2 (vl-catch-all-apply 'vla-get-Handle (list e2)))
          (if (or (vl-catch-all-error-p h1) (vl-catch-all-error-p h2))
            (progn (pgt:purgef *pgt:far*) (list cab0 hdr0 cur))
            (progn
              (setq r (gkc:expand (strcat h1 "," h2) (- n cur)))
              (setq nc (pgt:newdef (pgt:kv r "@defs=") cab0))
              (setq nh (pgt:newdef (pgt:kv r "@defs=") hdr0))
              (pgt:purgef *pgt:far*)
              (if (and nc nh)
                (list nc nh n)
                (list cab0 hdr0 cur)
              )
            )
          )
        )
      )
    )
  )
)

;; ================================================================
;; 五、主流程
;; ================================================================
;; slots = 槽位数（nil/0 = 用模板的）   cabs = 柜号表
;; per   = 每框台数（nil = 按框宽自动）  bx by = 第 1 框左下角
;; write = T 真铺 / nil 只算不铺
(defun pgt:go (slots cabs per bx by write / ok msg dfs cabn hdrn n
               fa ar fr cr hr pr fw fh step hw cw
               crx cry hrx hry phx phy
               m k nfr fi fi2 j idx left cno e tpl tot
               fok hok cok pok
               ftop cabx caby hdrx hdry phxx phyy cabht)
  (cond
    ((null (pgt:ensure))
     "缺 属性块-槽位增减.lsp（找不到或加载失败），没动图。")
    ((null cabs)
     "柜号清单是空的，没动图。")
    (T
     ;; --- 模板文件齐不齐 ---
     (setq msg "")
     (foreach tpl (list "图框模版.dwg" "柜块模版.dwg" "表头块模版.dwg" "占位块.dwg")
       (if (null (pgt:tpl tpl)) (setq msg (strcat msg "  缺模板：" tpl SEP)))
     )
     (cond
       ((/= msg "")
        (strcat "模板不齐，没动图：" SEP msg
                "  模板目录：" (if (pgt:tpldir) (pgt:tpldir) "?")))
       ((and write (null (pgt:ms)))
        "拿不到模型空间（COM 没通），没动图。")
       (T
        ;; --- 把四个块定义带进本图（只加定义，不加图元）---
        (pgt:import "图框模版.dwg" "图框模版")
        (pgt:import "柜块模版.dwg" "柜块模版")
        (pgt:import "表头块模版.dwg" "表头块模版")
        (pgt:import "占位块.dwg" "占位块")
        (cond
          ((or (null (pgt:defp "图框模版")) (null (pgt:defp "柜块模版"))
               (null (pgt:defp "表头块模版")) (null (pgt:defp "占位块")))
           "四个块定义没全进到本图（模板插不进来？），没动图。")
          (T
           ;; --- 定槽 ---
           (setq dfs (pgt:defs slots "柜块模版" "表头块模版"))
           (setq cabn (car dfs) hdrn (cadr dfs) n (caddr dfs))
           ;; --- 量几何 ---
           (setq fa (pgt:fanchor))
           (setq ar (if fa (pgt:rng (car fa))))
           (setq cr (pgt:rng cabn)
                 hr (pgt:rng hdrn)
                 pr (pgt:rng "占位块"))
           (cond
             ((null fa)
              "图框模版里找不到锚块（那个 * 开头的匿名块参照），没动图。")
             ((or (null ar) (null cr) (null hr) (null pr))
              "量不到块定义几何（图框 / 柜块 / 表头 / 占位块 有一个缺），没动图。")
             (T
              (setq fw (- (nth 2 ar) (car ar)))        ; 框宽 420
              (setq fh (- (nth 3 ar) (nth 1 ar)))      ; 框高 297
              (setq step (+ fw *pgt:cgap*))            ; 框间距 470
              (setq hw (- (nth 2 hr) (car hr)))        ; 表头块宽 40
              (setq cw (- (nth 2 cr) (car cr)))        ; 柜块宽 35
              (setq crx (car cr) cry (nth 3 cr))       ; 柜块几何 左 / 顶
              (setq hrx (car hr) hry (nth 3 hr))       ; 表头几何 左 / 顶
              (setq phx (car pr) phy (nth 3 pr))       ; 占位块几何 左 / 顶
              (setq cabht (- cry (nth 1 cr)))          ; 柜块几何高
              ;; 每框几台：框内净宽 / 柜宽
              (setq k (if (and per (> per 0)) per
                        (fix (/ (- fw (+ *pgt:padl* hw)) (float cw)))))
              (if (< k 1) (setq k 1))
              (setq m (length cabs))
              (setq nfr (pgt:ceil m k))
              ;; 与第几台无关的那几个 y / 偏移，循环外算一次
              (setq ftop (+ by fh))                          ; 框顶
              (setq caby (- ftop *pgt:padt* cry))            ; 柜块插入点 y
              (setq hdry (- ftop *pgt:padt* hry))            ; 表头插入点 y
              (setq phyy (- ftop *pgt:padt* *pgt:phgap* phy)) ; 占位块插入点 y
              ;; --- 逐框铺 ---
              (setq tot 0 fi 0 fok 0 hok 0 cok 0 pok 0)
              (while (< fi nfr)
                (setq left (+ bx (* fi step)))               ; 本框左下角 x
                (if write
                  (progn
                    ;; 图框：COM 插 + COM 炸开（框左下角在定义内要反向补偿）
                    (setq e (pgt:ins "图框模版"
                                     (list (- left (cadr fa) (car ar))
                                           (- by (caddr fa) (nth 1 ar)))))
                    (if e (progn (setq fok (1+ fok)) (pgt:exp e)))
                    ;; 本框表头，只一枚
                    (if (pgt:ins hdrn (list (- (+ left *pgt:padl*) hrx) hdry))
                      (setq hok (1+ hok))
                    )
                  )
                )
                ;; 本框的柜 + 占位块
                (setq j 0)
                (while (and (< j k) (< (+ (* fi k) j) m))
                  (setq idx (+ (* fi k) j))
                  (setq cno (nth idx cabs))
                  (setq cabx (+ left *pgt:padl* hw (* j cw)))   ; 本台柜几何左
                  (if write
                    (progn
                      (setq e (pgt:ins cabn (list (- cabx crx) caby)))
                      (if e (progn (setq cok (1+ cok)) (pgt:setatt e "柜号" cno)))
                      (if (pgt:ins "占位块" (list (- cabx phx) phyy))
                        (setq pok (1+ pok))
                      )
                    )
                  )
                  (setq tot (1+ tot))
                  (setq j (1+ j))
                )
                (setq fi (1+ fi))
              )
              ;; --- 报告 ---
              (strcat
                (if write "已铺" "【只算没铺】")
                "：槽位 " (itoa n)
                "（" (if (= cabn "柜块模版") "用模板定义" "现场克隆了新定义") "，柜块几何高 "
                (rtos cabht 2 0) "）" SEP
                "  柜 " (itoa m) " 台 / 每框 " (itoa k) " 台 / 共 " (itoa nfr) " 框" SEP
                (if write
                  (strcat "  实插：图框 " (itoa fok) "/" (itoa nfr)
                          "，表头 " (itoa hok) "/" (itoa nfr)
                          "，柜块 " (itoa cok) "/" (itoa tot)
                          "，占位块 " (itoa pok) "/" (itoa tot) SEP)
                  (strcat "  只算没铺：一个字没写。" SEP))
                "  框 " (rtos fw 2 0) "x" (rtos fh 2 0)
                "，间距 " (rtos step 2 0)
                "；柜块插入点 y = " (rtos caby 2 1)
                "，第 1 台柜几何左 = " (rtos (+ bx *pgt:padl* hw) 2 1) SEP
                (if write "  没存盘，改动都在内存里。" "")
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
;; 六、脚本入口（原接口保留，给外部调用）
;; ================================================================

;; 柜号可以给字符串（"AH01,AH02"）也可以直接给表（("AH01" "AH02")）
(defun pgt:cl (c)
  (if (listp c) c (pgt:split c))
)

;; 每框台数要走指定值时，直接调 (pgt:go 槽数 柜号表 每框台数 bx by write)
(defun pgt:run (slots cabs)
  (pgt:go slots (pgt:cl cabs) nil 0.0 0.0 T)
)

(defun pgt:dry (slots cabs)
  (pgt:go slots (pgt:cl cabs) nil 0.0 0.0 nil)
)


;; ================================================================
;; 七、面板与命令 PGT（给人用）
;; ================================================================
;; 面板是运行时现写的临时 DCL（写到临时目录，用完即删），
;; 所以整套功能只有这一个 .lsp，不用另带 .dcl。

;; 面板上次填的值：本次 CAD 会话内记住（重载本文件不清）
(if (null *pgt:u-slots*)  (setq *pgt:u-slots*  ""))
(if (null *pgt:u-per*)    (setq *pgt:u-per*    ""))
(if (null *pgt:u-prefix*) (setq *pgt:u-prefix* "AH"))
(if (null *pgt:u-total*)  (setq *pgt:u-total*  "10"))
(if (null *pgt:u-dry*)    (setq *pgt:u-dry*    "0"))

;; 全是半角数字才算（按字节判：中文 / 全角数字的字节都 >= 128，判不过）
(defun pgt:digitp (s / n i ok)
  (setq n (strlen s) ok (> n 0) i 1)
  (while (and ok (<= i n))
    (if (not (<= 48 (ascii (substr s i 1)) 57)) (setq ok nil))
    (setq i (1+ i))
  )
  ok
)

;; 序号补零到 w 位
(defun pgt:pad (i w / s)
  (setq s (itoa i))
  (while (< (strlen s) w) (setq s (strcat "0" s)))
  s
)

;; 第 i 台的柜号 = 前缀 + 序号
;; 序号至少 2 位；柜总数 m >= 100 时补成 3 位，依此类推
(defun pgt:cno (pf i m)
  (strcat pf (pgt:pad i (max 2 (strlen (itoa m)))))
)

;; 前缀 + 柜总数 -> 柜号表 ("AH01" "AH02" ...)
(defun pgt:mklist (pf m / i res)
  (setq i m res nil)
  (while (>= i 1)
    (setq res (cons (pgt:cno pf i m) res) i (1- i))
  )
  res
)

;; 把面板写成临时 DCL，返回文件路径；写不出来返回 nil
(defun pgt:dcl ( / fn f ln)
  (setq fn (vl-filename-mktemp "pgt.dcl"))
  (setq f (open fn "w"))
  (if f
    (progn
      (foreach ln
        (list
          "pgt_dlg : dialog {"
          "  label = \"属性块 - 铺图\";"
          "  : boxed_column {"
          "    label = \"版面\";"
          "    : edit_box { key = \"slots\"; label = \"槽位数（空 = 用模板自带）\"; edit_width = 8; edit_limit = 3; }"
          "    : edit_box { key = \"per\"; label = \"每框台数（空 = 按框宽自动）\"; edit_width = 8; edit_limit = 3; }"
          "  }"
          "  : boxed_column {"
          "    label = \"柜号 = 前缀 + 序号\";"
          "    : edit_box { key = \"prefix\"; label = \"柜号前缀\"; edit_width = 14; edit_limit = 20; }"
          "    : edit_box { key = \"total\"; label = \"柜总数\"; edit_width = 8; edit_limit = 3; }"
          "    : text { label = \"序号从 1 起，至少 2 位；柜总数 >= 100 时自动补成 3 位。\"; }"
          "    : text { key = \"pv\"; label = \"\"; width = 46; }"
          "  }"
          "  : toggle { key = \"dry\"; label = \"只预演（只算，不插入任何图元）\"; }"
          "  : text { key = \"error\"; label = \"\"; width = 46; }"
          "  : row {"
          "    alignment = centered;"
          "    : button { key = \"accept\"; label = \"铺图\"; is_default = true; width = 12; fixed_width = true; }"
          "    : button { key = \"cancel\"; label = \"取消\"; is_cancel = true; width = 12; fixed_width = true; }"
          "  }"
          "}"
        )
        (write-line ln f)
      )
      (close f)
      fn
    )
    nil
  )
)

;; 预览：柜号范围（前缀 / 柜总数改动后刷新）
(defun pgt:dlg-pv ( / pf tt m)
  (setq pf (pgt:trim (get_tile "prefix"))
        tt (pgt:trim (get_tile "total")))
  (set_tile "pv"
    (if (and (pgt:digitp tt) (> (atoi tt) 0))
      (progn
        (setq m (atoi tt))
        (strcat "将生成 " (itoa m) " 台：" (pgt:cno pf 1 m)
                (if (> m 1) (strcat " ~ " (pgt:cno pf m m)) "")))
      "（填好柜总数后，这里显示柜号范围）"))
)

;; 点「铺图」：先校验，不合格就留在面板里提示，合格才关面板
(defun pgt:dlg-accept ( / sl pe pf tt err bad)
  (setq sl (pgt:trim (get_tile "slots"))
        pe (pgt:trim (get_tile "per"))
        pf (pgt:trim (get_tile "prefix"))
        tt (pgt:trim (get_tile "total"))
        err nil bad nil)
  (cond
    ((and (/= sl "") (not (pgt:digitp sl)))
     (setq err "槽位数请填正整数（半角数字），或留空。" bad "slots"))
    ((and (/= pe "") (not (pgt:digitp pe)))
     (setq err "每框台数请填正整数（半角数字），或留空。" bad "per"))
    ((not (pgt:digitp tt))
     (setq err "柜总数请填正整数（半角数字）。" bad "total"))
    ((< (atoi tt) 1)
     (setq err "柜总数至少为 1。" bad "total"))
  )
  (if err
    (progn (set_tile "error" err) (mode_tile bad 2))
    (progn
      (setq *pgt:u-slots* sl *pgt:u-per* pe
            *pgt:u-prefix* pf *pgt:u-total* tt
            *pgt:u-dry* (get_tile "dry"))
      (done_dialog 1)
    )
  )
)

;; 弹面板；点「铺图」返回 T，取消 / 出错返回 nil
(defun pgt:dialog ( / fn id r)
  (setq fn (pgt:dcl))
  (cond
    ((null fn)
     (princ "\n写不出面板文件（临时目录不可写？），没动图。")
     nil)
    ((<= (setq id (load_dialog fn)) 0)
     (vl-file-delete fn)
     (princ "\n面板文件加载失败，没动图。")
     nil)
    ((not (new_dialog "pgt_dlg" id))
     (unload_dialog id)
     (vl-file-delete fn)
     (princ "\n面板打不开，没动图。")
     nil)
    (T
     (set_tile "slots" *pgt:u-slots*)
     (set_tile "per" *pgt:u-per*)
     (set_tile "prefix" *pgt:u-prefix*)
     (set_tile "total" *pgt:u-total*)
     (set_tile "dry" *pgt:u-dry*)
     (pgt:dlg-pv)
     (action_tile "prefix" "(pgt:dlg-pv)")
     (action_tile "total" "(pgt:dlg-pv)")
     (action_tile "accept" "(pgt:dlg-accept)")
     (action_tile "cancel" "(done_dialog 0)")
     (mode_tile "prefix" 2)
     (setq r (start_dialog))
     (unload_dialog id)
     (vl-file-delete fn)
     (if (= r 1)
       T
       (progn (princ "\n已取消。") nil))
    )
  )
)

(defun c:PGT ( / sl pe lst)
  (if (pgt:dialog)
    (progn
      (setq sl (atoi *pgt:u-slots*)
            pe (atoi *pgt:u-per*)
            lst (pgt:mklist *pgt:u-prefix* (atoi *pgt:u-total*)))
      (princ "\n正在处理，请稍候...")
      (princ (strcat SEP
                     (pgt:go (if (> sl 0) sl nil)
                             lst
                             (if (> pe 0) pe nil)
                             0.0 0.0
                             (if (= *pgt:u-dry* "1") nil T))))
    )
  )
  (princ)
)

(princ "\n属性块-铺图-面板版.lsp 已加载。命令 PGT（弹出面板）。")
(princ)
