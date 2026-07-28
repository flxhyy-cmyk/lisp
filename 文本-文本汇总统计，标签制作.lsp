;;; ============================================================
;;; DBQ.LSP  v1.0
;;; CAD 标签自动排版系统
;;; 适用：配电柜/控制柜/铭牌批量标签生成
;;; ============================================================
;;; 使用方法：
;;;   1. 将本文件拷贝到 AutoCAD 支持路径
;;;   2. APPLOAD 加载 DBQ.LSP
;;;   3. 命令行输入 DBQ 启动
;;; ============================================================

(vl-load-com)

;;; ─────────────────────────────────────────────────────────
;;; 初始化全局常量
;;; ─────────────────────────────────────────────────────────
(defun dbq:init ()
  ;; 标签尺寸（内框为主，外框自动跟随）
  (setq dbq:iw  (if (and (boundp '*dbq-iw*) *dbq-iw*) *dbq-iw* 79)  ; 文字矩形宽（主）
        dbq:ih  (if (and (boundp '*dbq-ih*) *dbq-ih*) *dbq-ih* 20)  ; 文字矩形高（主）
        ;; 外框尺寸：优先读用户输入，否则按内框+2派生
        dbq:ow  (if (and (boundp '*dbq-ow*) *dbq-ow*) *dbq-ow* (+ dbq:iw 2))
        dbq:oh  (if (and (boundp '*dbq-oh*) *dbq-oh*) *dbq-oh* (+ dbq:ih 2))
        dbq:io  (/ (- dbq:ow dbq:iw) 2.0)  ; 内外框间隙（动态计算）
        dbq:th  (if (and (boundp '*dbq-th*) *dbq-th*) *dbq-th* 10) ; 文字高度
        dbq:ns  (if (and (boundp '*dbq-ns*) *dbq-ns*) *dbq-ns* 5)  ; 压缩阈值（≤N字用WF=1居中）
        dbq:cb  (if (and (boundp '*dbq-cb*) *dbq-cb*) *dbq-cb* 5.0) ; 压缩基准（填满N字宽）
  )
  ;; 纸张参数（可配置：A4=297x210, A3=420x297, 自定义）
  (setq dbq:pw (if (and (boundp '*dbq-pw*) *dbq-pw*) *dbq-pw* 297)
        dbq:ph (if (and (boundp '*dbq-ph*) *dbq-ph*) *dbq-ph* 210)
        dbq:amx  7.0
        dbq:amy  7.0
  )
  ;; 排版参数 ― 按纸张可用空间智能计算列数/行数
  (setq dbq:hg  (if (and (boundp '*dbq-hg*) *dbq-hg*) *dbq-hg* 7)
        dbq:vg  (if (and (boundp '*dbq-vg*) *dbq-vg*) *dbq-vg* 7)
        ;; 可用区 = 纸张尺寸 - 2×最小边距
        dbq:nc  (max 1 (fix (/ (+ (- dbq:pw (* 2 dbq:amx)) dbq:hg) (+ dbq:ow dbq:hg))))
        dbq:nr  (max 1 (fix (/ (+ (- dbq:ph (* 2 dbq:amy)) dbq:vg) (+ dbq:oh dbq:vg))))
        dbq:pg  (* dbq:nc dbq:nr)
        dbq:hs  (+ dbq:ow dbq:hg)
        dbq:vs  (+ dbq:oh dbq:vg)
        dbq:cw  (+ (* dbq:nc dbq:ow) (* (1- dbq:nc) dbq:hg))
        dbq:ch  (+ (* dbq:nr dbq:oh) (* (1- dbq:nr) dbq:vg))
        dbq:ox  (/ (- dbq:pw (float dbq:cw)) 2.0)
        dbq:oy  (/ (- dbq:ph (float dbq:ch)) 2.0)
        dbq:gh  (+ dbq:ph 50)
  )
  ;; 全局数据容器
  (if (not (boundp '*dbq-data*)) (setq *dbq-data* nil))
  (if (not (boundp '*dbq-path*)) (setq *dbq-path* ""))
  (if (not (boundp '*dbq-title*)) (setq *dbq-title* ""))
  (if (not (boundp '*dbq-paper*)) (setq *dbq-paper* 0))
  (if (not (boundp '*dbq-pw*)) (setq *dbq-pw* 297))
  (if (not (boundp '*dbq-ph*)) (setq *dbq-ph* 210))
  (if (not (boundp '*dbq-summary-only*)) (setq *dbq-summary-only* 0))
  (if (not (boundp '*dbq-smart-merge*)) (setq *dbq-smart-merge* 0))
  (if (not (boundp '*dbq-smart-merge-row*)) (setq *dbq-smart-merge-row* 0))
  (if (not (boundp '*dbq-merge-rows*)) (setq *dbq-merge-rows* 3))
  (if (not (boundp '*dbq-append*)) (setq *dbq-append* 0))
  (if (not (boundp '*dbq-backup-data*)) (setq *dbq-backup-data* nil))
  (if (not (and (boundp '*dbq-rowtol*) *dbq-rowtol* (= (type *dbq-rowtol*) 'REAL))) (setq *dbq-rowtol* 0.5))
  (if (not (boundp '*dbq-match-height*)) (setq *dbq-match-height* 0))
  (if (not (boundp '*dbq-captured-height*)) (setq *dbq-captured-height* nil))
  (if (not (boundp '*dbq-configs*)) (setq *dbq-configs* (dbq:load-configs)))
  (dbq:load-state)
)

;;; ─────────────────────────────────────────────────────────
;;; 字符计数（中英混合）
;;; 中文双字节字符（GBK/ANSI）= 1个视觉字符
;;; 英文单字节字符             = 1个视觉字符
;;; ─────────────────────────────────────────────────────────
(defun dbq:nchar (s / i n c)
  (setq i 1 n 0)
  (while (<= i (strlen s))
    (setq c (ascii (substr s i 1)))
    (if (>= c 128)
      (setq i (+ i 2))   ; 中文双字节，跳2
      (setq i (+ i 1))   ; 英文单字节，跳1
    )
    (setq n (1+ n))
  )
  n
)

;;; ─────────────────────────────────────────────────────────
;;; 读取TXT文件
;;; 支持：ANSI / UTF-8 BOM
;;; 自动过滤：空行、\r、首尾空格
;;; ─────────────────────────────────────────────────────────
(defun dbq:read-txt (fn / fp ln lst)
  (setq lst nil)
  (setq fp (open fn "r"))
  (if (null fp)
    (progn (alert (strcat "无法打开文件：\n" fn)) nil)
    (progn
      (while (setq ln (read-line fp))
        ;; 去除 UTF-8 BOM (0xEF=239, 0xBB=187, 0xBF=191)
        (if (and (>= (strlen ln) 1)
                 (= (ascii (substr ln 1 1)) 239))
          (setq ln (substr ln 4))
        )
        ;; 去除首尾空白和回车
        (setq ln (vl-string-trim " \t\r" ln))
        ;; 非空行加入列表
        (if (/= ln "")
          (setq lst (cons ln lst))
        )
      )
      (close fp)
      (reverse lst)
    )
  )
)

(defun dbq:write-txt (fn lines append-mode / fp mode)
  (setq mode (if (= append-mode 1) "a" "w"))
  (setq fp (open fn mode))
  (if (null fp)
    (progn (alert (strcat "无法写入文件：\n" fn)) nil)
    (progn
      (foreach ln lines
        (progn
          (princ ln fp)
          (princ "\n" fp)
        )
      )
      (close fp)
      t
    )
  )
)

(defun dbq:split-lines (s / p part rest out)
  (setq rest s out nil)
  ;; 统一换行符: \P (MTEXT 原始码) 转为 \n
  (while (setq p (vl-string-search "\\P" rest))
    (setq rest (strcat (substr rest 1 p) "\n" (substr rest (+ p 3))))
  )
  ;; 按 \n 拆分
  (while (setq p (vl-string-search "\n" rest))
    (setq part (substr rest 1 p)
          rest (substr rest (+ p 2))
          part (vl-string-trim " \t\r" part)
    )
    (if (/= part "") (setq out (append out (list part))))
  )
  (setq rest (vl-string-trim " \t\r" rest))
  (if (/= rest "") (setq out (append out (list rest))))
  out
)

(defun dbq:clean-mtext-string (s / i n ch nx q out)
  (setq i 1
        n (strlen s)
        out "")
  (while (<= i n)
    (setq ch (substr s i 1))
    (if (= ch "\\")
      (progn
        (if (< i n)
          (progn
            (setq nx (substr s (+ i 1) 1))
            (cond
              ;; MTEXT new paragraph
              ((= nx "P")
               (setq out (strcat out "\n")
                     i (+ i 2)))
              ;; non-breaking space
              ((= nx "~")
               (setq out (strcat out " ")
                     i (+ i 2)))
              ;; escaped literal chars
              ((or (= nx "\\") (= nx "{") (= nx "}"))
               (setq out (strcat out nx)
                     i (+ i 2)))
               ;; formatting code, consume until ';'
               ;; 已知码优先处理，另外对字母开头的控制码做兜底清理
               ((or (= nx "C") (= nx "H") (= nx "W") (= nx "T") (= nx "F")
                    (= nx "A") (= nx "Q") (= nx "L") (= nx "O") (= nx "K")
                    (= nx "S") (= nx "U") (= nx "o") (= nx "p")
                    (wcmatch nx "[A-Za-z]"))
                (setq q (+ i 2))
                (while (and (<= q n) (/= (substr s q 1) ";"))
                  (setq q (1+ q)))
               (if (<= q n)
                 (setq i (1+ q))
                 (setq i (+ i 2))))
              ;; 未知转义序列，保留反斜杠
              (t
               (setq out (strcat out ch)
                     i (1+ i)))
            )
          )
          (setq i (1+ i))
        )
      )
      (progn
        (setq out (strcat out ch)
              i (1+ i))
      )
    )
  )
  out
)

(defun dbq:get-selected-text-lines (/ ss i en ed tp txt lines h vobj clean)
  (setq lines nil)
  (prompt "\n请选择需要写回TXT的文字对象（TEXT/MTEXT），回车结束：")
  (setq ss (ssget '((0 . "TEXT,MTEXT"))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq en (ssname ss i)
              ed (entget en)
              tp (if (assoc 0 ed) (cdr (assoc 0 ed)) "")
              txt (if (assoc 1 ed) (cdr (assoc 1 ed)) "")
        )
        (if (and txt (/= (vl-string-trim " \t\r\n" txt) ""))
          (progn
            ;; 捕获文字高度
            (if (= i 0)
              (progn
                (cond
                  ((= tp "MTEXT")
                   (progn
                     (setq h 0.0)
                     (vl-catch-all-apply
                       (function
                         (lambda ()
                           (setq vobj (vlax-ename->vla-object en))
                           (setq h (vlax-get vobj 'Height))
                           (vlax-release-object vobj))))
                     (if (not (and (numberp h) (> h 0.0))) (setq h (if (assoc 40 ed) (cdr (assoc 40 ed)) nil)))
                   )
                  )
                  (t (setq h (if (assoc 40 ed) (cdr (assoc 40 ed)) nil)))
                )
                (if (and (numberp h) (> h 0.0)) (setq *dbq-captured-height* h))
              )
            )
            ;; MTEXT: 多行合并为一行（"-"连接）; TEXT: 直接使用
            (if (= tp "MTEXT")
              (progn
                (setq vobj nil clean nil)
                (vl-catch-all-apply
                  (function (lambda ()
                    (setq vobj (vlax-ename->vla-object en))
                    (setq clean (vla-get-textstring vobj)))))
                (if vobj (vlax-release-object vobj))
                (if (and clean (= (type clean) 'STR))
                  (setq lines (append lines
                    (list (dbq:_join-no-sep (dbq:split-lines (dbq:clean-mtext-string clean))))))
                  ;; fallback: 若 vla-get-textstring 失败，用 DXF 1 值
                  (setq lines (append lines (list (vl-string-trim " \t\r\n" txt))))
                )
              )
              (setq lines (append lines (list (vl-string-trim " \t\r\n" txt))))
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )
  lines
)

(defun dbq:_join-with-hyphen (lst / out)
  (if (null lst)
    ""
    (progn
      (setq out (car lst)
            lst (cdr lst))
      (foreach s lst
        (setq out (strcat out "-" s))
      )
      out
    )
  )
)

(defun dbq:_join-no-sep (lst / out)
  (if (null lst)
    ""
    (progn
      (setq out (car lst)
            lst (cdr lst))
      (foreach s lst
        (setq out (strcat out s))
      )
      out
    )
  )
)

;;; 收集各行文本统计：((text count) ...)
(defun dbq:_collect-text-stats (rows / counts r it txt found pair)
  (setq counts nil)
  (foreach r rows
    (foreach it r
      (setq txt (nth 3 it) found nil)
      (foreach pair counts
        (if (= (car pair) txt) (setq found pair))
      )
      (if found
        (setq counts
          (mapcar '(lambda (p)
                     (if (= (car p) txt)
                       (cons (car p) (1+ (cdr p)))
                       p))
                   counts))
        (setq counts (append counts (list (cons txt 1))))
      )
    )
  )
  counts
)

;;; 结构模板：字母=L，数字=D，中文/双字节=C，常见分隔符保留，其它=S；连续同类压缩
(defun dbq:text-structure-template (s / i n ch code cat prev run out)
  (setq i 1 n (strlen s) prev "" run 0 out "")
  (while (<= i n)
    (setq ch (substr s i 1)
          code (ascii ch))
    (cond
      ((or (= ch "-") (= ch "_") (= ch "/") (= ch ".")) (setq cat ch))
      ((and (>= code 48) (<= code 57)) (setq cat "D"))
      ((or (and (>= code 65) (<= code 90)) (and (>= code 97) (<= code 122))) (setq cat "L"))
      ((>= code 128) (setq cat "C"))
      (t (setq cat "S"))
    )
    (if (= cat prev)
      (setq run (1+ run))
      (progn
        (if (/= prev "") (setq out (strcat out prev (itoa run))))
        (setq prev cat
              run 1)
      )
    )
    (if (>= code 128)
      (setq i (+ i 2))
      (setq i (1+ i))
    )
  )
  (if (/= prev "") (setq out (strcat out prev (itoa run))))
  out
)

;;; 基于首列结构模板，返回需要默认选中的排除索引串（如 "1 3 5"）
(defun dbq:_median (nums / sorted n)
  (setq sorted (vl-sort nums '<)
        n (length sorted))
  (cond
    ((= n 0) 0.0)
    ((= (rem n 2) 1) (nth (/ n 2) sorted))
    (t (/ (+ (nth (/ n 2) sorted) (nth (1- (/ n 2)) sorted)) 2.0))
  )
)

(defun dbq:_row-count-mode (rows / counts r n pair best best-count)
  (setq counts nil)
  (foreach r rows
    (setq n (length r)
          pair (assoc n counts))
    (if pair
      (setq counts (subst (cons n (1+ (cdr pair))) pair counts))
      (setq counts (cons (cons n 1) counts))
    )
  )
  (setq best nil best-count -1)
  (foreach pair counts
    (if (or (> (cdr pair) best-count)
            (and (= (cdr pair) best-count) (or (null best) (< (car pair) best))))
      (setq best (car pair) best-count (cdr pair))
    )
  )
  best
)

(defun dbq:_item-cx (it)
  (if (and (nth 4 it) (numberp (nth 4 it))) (nth 4 it) (car it))
)

(defun dbq:_build-col-x-refs (rows n / c xs cxs r it refs)
  (setq refs nil c 0)
  (while (< c n)
    (setq xs nil cxs nil)
    (foreach r rows
      (setq it (nth c r))
      (if it
        (progn
          (setq xs (cons (car it) xs))
          (setq cxs (cons (dbq:_item-cx it) cxs))
        )
      )
    )
    (setq refs (append refs (list (list (dbq:_median xs) (dbq:_median cxs)))))
    (setq c (1+ c))
  )
  refs
)

(defun dbq:_x-align-score (it ref / dx dcx)
  (setq dx (abs (- (car it) (car ref)))
        dcx (abs (- (dbq:_item-cx it) (cadr ref))))
  (+ (* 0.4 dx) (* 0.6 dcx))
)

(defun dbq:_best-kept-indexes-by-x (items refs / best best-score n)
  (setq best nil
        best-score nil
        n (length refs))
  (dbq:_best-kept-indexes-by-x-rec items refs 0 0 nil 0.0 n)
  best
)

(defun dbq:_best-kept-indexes-by-x-rec (items refs item-idx col-idx chosen score n / remain-items remain-cols it new-score)
  (setq remain-items (- (length items) item-idx)
        remain-cols (- n col-idx))
  (cond
    ((= col-idx n)
     (if (or (null best-score) (< score best-score))
       (setq best-score score
             best (reverse chosen))
     )
    )
    ((< remain-items remain-cols) nil)
    (t
      (setq it (nth item-idx items)
            new-score (+ score (dbq:_x-align-score it (nth col-idx refs))))
      (dbq:_best-kept-indexes-by-x-rec items refs (1+ item-idx) (1+ col-idx) (cons item-idx chosen) new-score n)
      (if (> remain-items remain-cols)
        (dbq:_best-kept-indexes-by-x-rec items refs (1+ item-idx) col-idx chosen score n)
      )
    )
  )
)

(defun dbq:auto-exclude-indexes-by-x-align (rows vals / target-row max-len r txt pairs p found maxc picktexts idx out n normal-rows refs texts keep it)
  ;; 新逻辑优先：取“数量最多行”，按该行文本频次自动预选（并列最多全部选中）
  (setq target-row nil
        max-len 0)
  (foreach r rows
    (if (> (length r) max-len)
      (progn
        (setq max-len (length r))
        (setq target-row r)
      )
    )
  )
  (if (and target-row (> max-len 0))
    (progn
      (setq pairs nil)
      (foreach it target-row
        (setq txt (nth 3 it))
        (if (and txt (/= (vl-string-trim " \t\r\n" txt) ""))
          (progn
            (setq found nil)
            (setq pairs
              (mapcar
                '(lambda (q)
                   (if (= (car q) txt)
                     (progn (setq found t) (cons (car q) (1+ (cdr q))))
                     q))
                pairs))
            (if (not found) (setq pairs (cons (cons txt 1) pairs)))
          )
        )
      )
      (setq maxc 0)
      (foreach p pairs
        (if (> (cdr p) maxc) (setq maxc (cdr p)))
      )
      (setq picktexts nil)
      (if (> maxc 0)
        (foreach p pairs
          (if (= (cdr p) maxc)
            (setq picktexts (cons (car p) picktexts))
          )
        )
      )
      (setq idx 0 out "")
      (foreach p vals
        (if (member (car p) picktexts)
          (progn
            (if (/= out "") (setq out (strcat out " ")))
            (setq out (strcat out (itoa idx)))
          )
        )
        (setq idx (1+ idx))
      )
      (if (/= out "")
        out
        ;; 方案3：新逻辑为空时，回退旧 x 对齐逻辑
        (progn
          (setq n (dbq:_row-count-mode rows))
          (if (or (null n) (<= n 0))
            ""
            (progn
              (setq normal-rows nil)
              (foreach r rows
                (if (= (length r) n) (setq normal-rows (cons r normal-rows)))
              )
              (if (null normal-rows)
                ""
                (progn
                  (setq refs (dbq:_build-col-x-refs normal-rows n)
                        texts nil)
                  (foreach r rows
                    (if (> (length r) n)
                      (progn
                        (setq keep (dbq:_best-kept-indexes-by-x r refs)
                              idx 0)
                        (foreach it r
                          (if (not (member idx keep))
                            (if (not (member (nth 3 it) texts))
                              (setq texts (cons (nth 3 it) texts))
                            )
                          )
                          (setq idx (1+ idx))
                        )
                      )
                    )
                  )
                  (setq idx 0 out "")
                  (foreach p vals
                    (if (member (car p) texts)
                      (progn
                        (if (/= out "") (setq out (strcat out " ")))
                        (setq out (strcat out (itoa idx)))
                      )
                    )
                    (setq idx (1+ idx))
                  )
                  out
                )
              )
            )
          )
        )
      )
    )
    ""
  )
)

(defun dbq:auto-exclude-indexes-by-first-col (rows vals / first-templates r first-it txt tpl idx out)
  (setq first-templates nil)
  (foreach r rows
    (if (and r (setq first-it (car r)) (setq txt (nth 3 first-it)) (/= (vl-string-trim " \t\r\n" txt) ""))
      (progn
        (setq tpl (dbq:text-structure-template txt))
        (if (and (/= tpl "") (not (member tpl first-templates)))
          (setq first-templates (cons tpl first-templates))
        )
      )
    )
  )
  (if (null first-templates)
    ""
    (progn
      (setq idx 0 out "")
      (foreach p vals
        (setq tpl (dbq:text-structure-template (car p)))
        (if (not (member tpl first-templates))
          (progn
            (if (/= out "") (setq out (strcat out " ")))
            (setq out (strcat out (itoa idx)))
          )
        )
        (setq idx (1+ idx))
      )
      out
    )
  )
)

;;; 调试：返回首列结构模板集合
(defun dbq:first-col-template-list (rows / first-templates r first-it txt tpl)
  (setq first-templates nil)
  (foreach r rows
    (if (and r (setq first-it (car r)) (setq txt (nth 3 first-it)) (/= (vl-string-trim " \t\r\n" txt) ""))
      (progn
        (setq tpl (dbq:text-structure-template txt))
        (if (and (/= tpl "") (not (member tpl first-templates)))
          (setq first-templates (cons tpl first-templates))
        )
      )
    )
  )
  first-templates
)

;;; 排除文本 DCL 对话框，返回用户选定排除的文本列表，nil=取消
(defun dbq:write-exclude-dcl (text-stats / fn fp)
  (setq fn (strcat (getenv "TEMP") "\\dbq_ex.dcl"))
  (if (not (findfile fn))
    (progn
      (setq fp (open fn "w"))
      (foreach s (list
    "dbq_exclude : dialog {"
    "  label = \"选择要排除的文本\";"
    "  : list_box {"
    "    key         = \"lb_texts\";"
    "    multiple_select = true;"
    "    width       = 60;"
    "    height      = 20;"
    "  }"
    "  spacer_1;"
    "  : row {"
    "    : button { key = \"b_selall\"; label = \"全选\";   fixed_width = true; width = 8; }"
    "    : button { key = \"b_desel\"; label = \"全不选\"; fixed_width = true; width = 8; }"
    "    spacer_0;"
    "    ok_only;"
    "  }"
    "}"
  )
      (write-line s fp)
    )
    (close fp)
  )
  )
  fn
)

(defun dbq:do-smart-merge-exclude-dialog (rows / dcl-fn dcl-id vals val i idx excluded auto-excluded tpl-debug dlgret)
  (setq vals (dbq:_collect-text-stats rows))
  (if (null vals) (progn (alert "没有可排除的文本。") nil)
    (progn
      (setq dcl-fn (dbq:write-exclude-dcl vals))
      (setq dcl-id (load_dialog dcl-fn))
      (if (or (null dcl-id) (minusp dcl-id))
        (progn (alert "DCL 加载失败！") nil)
        (progn
          (if (not (new_dialog "dbq_exclude" dcl-id))
            (progn (alert "对话框初始化失败！") (unload_dialog dcl-id) nil)
            (progn
              ;; 填充列表
              (setq val nil i 0)
              (foreach p vals
                (setq val (append val (list (strcat (car p) "  (含此文本的行: " (itoa (cdr p)) ")"))))
                (setq i (1+ i))
              )
              (start_list "lb_texts")
              (mapcar 'add_list val)
              (end_list)

              ;; 自动预选：结构不符合首列模板的项
              (setq auto-excluded (dbq:auto-exclude-indexes-by-x-align rows vals))
              (prompt (strcat "\n[DBQ-DEBUG] x-align auto indexes='" auto-excluded "'"))
              (if (/= auto-excluded "") (set_tile "lb_texts" auto-excluded))
              ;; 按钮回调
              (action_tile "b_selall"
                (vl-prin1-to-string
                  '(progn
                     (setq excluded "")
                     (setq idx 0)
                     (foreach p vals
                       (if (> idx 0) (setq excluded (strcat excluded " ")))
                       (setq excluded (strcat excluded (itoa idx)))
                       (setq idx (1+ idx))
                     )
                     (set_tile "lb_texts" excluded)
                   )
                )
              )
              (action_tile "b_desel" "(set_tile \"lb_texts\" \"\")")
              (action_tile "accept"
                "(progn (setq excluded (get_tile \"lb_texts\")) (done_dialog 1))"
              )
              (action_tile "cancel"
                "(progn (setq excluded nil) (done_dialog 0))"
              )
              (setq excluded auto-excluded)
              (setq dlgret (start_dialog))
              (unload_dialog dcl-id)
              (cond
                ;; 用户取消
                ((/= dlgret 1) nil)
                ;; 用户确认但未选择任何排除项：返回空列表，而不是 nil
                ;; 这样外层可区分“取消”和“确认无排除”
                ((or (null excluded) (= excluded "")) (list))
                (t
                 (progn
                   (setq excluded (dbq:_split-comma excluded))
                   (setq excluded (mapcar '(lambda (idx-str)
                                             (car (nth (atoi idx-str) vals)))
                                           excluded))
                   excluded
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

(defun dbq:_split-comma (s / p out)
  (setq out nil)
  (while (setq p (vl-string-search " " s))
    (if (> p 0) (setq out (append out (list (substr s 1 p)))))
    (setq s (substr s (+ p 2)))
  )
  (if (/= s "") (setq out (append out (list s))))
  out
)

(defun dbq:get-selected-text-lines-smart-merge (/ ss i en ed tp txt pt items k heights sorted_heights n_h H_rep eps min_eps max_eps overlap_thresh rows it y foundrow r row_y ri yi hi it_h min1 max1 min2 max2 ov out cols ok c vals existing sumy cnt new_y new_row h vobj clean cx *dbq:_excluded* *dbq:_retry* _retry-count _retry-max _last-item-count)
  (setq items nil
        ;; 将用户输入的 *dbq-rowtol* 作为比例因子 k（默认 0.5）
        ;; 必须用 type 守卫阻止 DCL 遗留的空字符串炸掉 (> "" 0.0)
        k (if (and (boundp '*dbq-rowtol*) *dbq-rowtol* (= (type *dbq-rowtol*) 'REAL) (> *dbq-rowtol* 0.0))
            *dbq-rowtol* 0.5))
  (prompt "\n请选择需要写回TXT的文字对象（TEXT/MTEXT），回车结束：")
  (setq ss (ssget '((0 . "TEXT,MTEXT"))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq en (ssname ss i)
              ed (entget en)
              tp (if (assoc 0 ed) (cdr (assoc 0 ed)) "")
              txt (if (assoc 1 ed) (cdr (assoc 1 ed)) "")
              pt  (if (assoc 10 ed) (cdr (assoc 10 ed)) nil)
        )
        (if (and txt pt (/= (vl-string-trim " \t\r\n" txt) ""))
          (progn
            ;; 捕获第一个文字的高度
            (if (= i 0)
              (progn
                (cond
                  ((= tp "MTEXT")
                   (progn
                     (setq h 0.0)
                     (vl-catch-all-apply
                       (function
                         (lambda ()
                           (setq vobj (vlax-ename->vla-object en))
                           (setq h (vlax-get vobj 'Height))
                           (vlax-release-object vobj))))
                     (if (not (and (numberp h) (> h 0.0))) (setq h (if (assoc 40 ed) (cdr (assoc 40 ed)) nil)))
                   )
                  )
                  (t (setq h (if (assoc 40 ed) (cdr (assoc 40 ed)) nil)))
                )
                (if (and (numberp h) (> h 0.0)) (setq *dbq-captured-height* h))
              )
            )
            ;; 计算实体高度：TEXT 使用 assoc 40，MTEXT 尝试用 vla-getboundingbox
            (cond
              ((= tp "MTEXT")
               (progn
                 (setq h 0.0)
                 (vl-catch-all-apply
                   (function
                      (lambda ()
                        (setq vobj (vlax-ename->vla-object en))
                        (vla-getboundingbox vobj 'dbq:bbmin 'dbq:bbmax)
                        (setq dbq:bbmin (vlax-safearray->list dbq:bbmin)
                              dbq:bbmax (vlax-safearray->list dbq:bbmax)
                              h (abs (- (cadr dbq:bbmax) (cadr dbq:bbmin))))
                        (vlax-release-object vobj))))
                 ;; 如果获取失败，退回到默认文字高度
                 (if (not (and (numberp h) (> h 0.0)))
                   (setq h (if (and (boundp '*dbq-th*) (numberp *dbq-th*) (> *dbq-th* 0)) *dbq-th* 10)))
               )
              )
               (t (setq h (if (assoc 40 ed) (cdr (assoc 40 ed)) nil)))
             )
               ;; 将文本拆分为单行项（MTEXT 多行合并为一行）并记录 (x y height text)
              (setq cx (car pt))
              (vl-catch-all-apply
                (function
                  (lambda ()
                    (setq vobj (vlax-ename->vla-object en))
                    (vla-getboundingbox vobj 'dbq:bbmin 'dbq:bbmax)
                    (setq dbq:bbmin (vlax-safearray->list dbq:bbmin)
                          dbq:bbmax (vlax-safearray->list dbq:bbmax)
                          cx (/ (+ (car dbq:bbmin) (car dbq:bbmax)) 2.0))
                    (vlax-release-object vobj))))
              (if (= tp "MTEXT")
                (progn
                  (setq vobj nil clean nil)
                  (vl-catch-all-apply
                    (function (lambda ()
                      (setq vobj (vlax-ename->vla-object en))
                      (setq clean (vla-get-textstring vobj)))))
                  (if vobj (vlax-release-object vobj))
                  (if (and clean (= (type clean) 'STR))
                    (setq items (append items
                      (list (list (car pt) (cadr pt) (if h h 0.0)
                        (dbq:_join-no-sep (dbq:split-lines (dbq:clean-mtext-string clean))) cx))))
                    ;; fallback: 使用 DXF 1 值
                    (setq items (append items
                      (list (list (car pt) (cadr pt) (if h h 0.0)
                        (vl-string-trim " \t\r\n" txt) cx))))
                  )
                )
                (setq items (append items
                  (list (list (car pt) (cadr pt) (if h h 0.0)
                    (vl-string-trim " \t\r\n" txt) cx))))
              )
          )
        )
        (setq i (1+ i))
      )

      (if (null items)
        nil
        (progn
          ;; 按 Y(降序) 与 X(升序) 排序项
          (setq items (vl-sort items '(lambda (a b) (if (/= (cadr a) (cadr b)) (> (cadr a) (cadr b)) (< (car a) (car b))))))

          ;; 计算代表高度（中位数），用以确定自适应容差 eps
          (setq heights (mapcar '(lambda (it) (nth 2 it)) items))
          (setq sorted_heights (vl-sort heights '<))
          (setq n_h (length sorted_heights))
          (if (> n_h 0)
            (progn
              (if (= (rem n_h 2) 1)
                (setq H_rep (nth (/ n_h 2) sorted_heights))
                (setq H_rep (/ (+ (nth (/ n_h 2) sorted_heights) (nth (1- (/ n_h 2)) sorted_heights)) 2.0))
              )
            )
            (setq H_rep 1.0)
          )
          (if (<= H_rep 0.0) (setq H_rep 1.0))
          (setq eps (* H_rep k))
          ;; 限定 eps 的上下界，防止极端值
          (setq min_eps (* H_rep 0.1))
          (setq max_eps (* H_rep 2.0))
          (if (< eps min_eps) (setq eps min_eps))
          (if (> eps max_eps) (setq eps max_eps))
          (setq overlap_thresh 0.3)

          ;; 分组循环：支持排除文本后重新计算
          (setq *dbq:_excluded* nil *dbq:_retry* t)
          (setq _retry-count 0 _retry-max 20 _last-item-count (length items))
          (while *dbq:_retry*
            (setq _retry-count (1+ _retry-count))
            (if (> _retry-count _retry-max)
              (progn
                (prompt "\n[DBQ] 自动排除重试达到上限，已退出智能重试。")
                (setq *dbq:_retry* nil)
              )
            )
            ;; 按自适应 eps 与垂直重叠进行行分组
            (setq rows nil)
            (foreach it items
              (setq y (nth 1 it) foundrow nil)
              (foreach r rows
                (if (not foundrow)
                  (progn
                    (setq row_y (car r))
                    (if (<= (abs (- y row_y)) eps)
                      (setq foundrow r)
                      (foreach ri (cdr r)
                        (if (not foundrow)
                          (progn
                            (setq yi (nth 1 ri)
                                  hi (nth 2 ri)
                                  it_h (nth 2 it)
                                  min1 (- yi (/ hi 2.0))
                                  max1 (+ yi (/ hi 2.0))
                                  min2 (- y (/ it_h 2.0))
                                  max2 (+ y (/ it_h 2.0))
                                  ov (- (min max1 max2) (max min1 min2))
                            )
                            (if (> ov 0.0)
                              (progn
                                (setq ov_ratio (/ ov (min hi it_h)))
                                (if (>= ov_ratio overlap_thresh) (setq foundrow r))
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
              (if foundrow
                (progn
                  (setq existing (cdr foundrow)
                        sumy 0 cnt 0)
                  (foreach ex existing (setq sumy (+ sumy (nth 1 ex)) cnt (1+ cnt)))
                  (setq new_y (/ (+ sumy y) (1+ cnt)))
                  (setq new_row (cons new_y (append existing (list it))))
                  (setq rows (subst new_row foundrow rows))
                )
                (setq rows (append rows (list (cons y (list it)))))
              )
            )
            ;; 将 rows 转为行项列表并按 X 排序每行内项
            (setq rows (vl-sort rows '(lambda (a b) (> (car a) (car b)))))
            (setq rows (mapcar '(lambda (r) (vl-sort (cdr r) '(lambda (a b) (if (/= (car a) (car b)) (< (car a) (car b)) (< (cadr a) (cadr b)))))) rows))
            ;; 一致性检查
            (if (null rows)
              (setq ok nil)
              (progn
                (setq cols (length (car rows)) ok t)
                (foreach r rows (if (/= (length r) cols) (setq ok nil)))
              )
            )
            (if ok
              (setq *dbq:_retry* nil)
              (progn
                ;; 弹出排除对话框
                (setq *dbq:_excluded* (dbq:do-smart-merge-exclude-dialog rows))
                (if (null *dbq:_excluded*)
                  ;; 取消：退出智能合并流程
                  (setq *dbq:_retry* nil)
                  (progn
                    ;; 用户确认（包含“无排除项”）：仅在有排除项时过滤
                    (if (> (length *dbq:_excluded*) 0)
                      (progn
                        (setq items
                          (vl-remove-if
                            '(lambda (it) (member (nth 3 it) *dbq:_excluded*))
                            items))
                        ;; 重新计算 eps
                        (setq heights (mapcar '(lambda (it) (nth 2 it)) items))
                        (setq sorted_heights (vl-sort heights '<))
                        (setq n_h (length sorted_heights))
                        (if (> n_h 0)
                          (progn
                            (if (= (rem n_h 2) 1)
                              (setq H_rep (nth (/ n_h 2) sorted_heights))
                              (setq H_rep (/ (+ (nth (/ n_h 2) sorted_heights) (nth (1- (/ n_h 2)) sorted_heights)) 2.0))
                            )
                          )
                          (setq H_rep 1.0)
                        )
                        (if (<= H_rep 0.0) (setq H_rep 1.0))
                        (setq eps (* H_rep k))
                        (setq min_eps (* H_rep 0.1))
                        (setq max_eps (* H_rep 2.0))
                        (if (< eps min_eps) (setq eps min_eps))
                        (if (> eps max_eps) (setq eps max_eps))
                        (if (null items)
                          (progn (alert "排除后无剩余文本。") (setq *dbq:_retry* nil))
                          ;; 有排除且仍有数据：继续 while，重新分组/重算 ok
                          (setq *dbq:_retry* t)
                        )
                      )
                      ;; 用户确认但未选择排除项：结束重试，避免重复弹窗
                      (setq *dbq:_retry* nil)
                    )
                  )
                )
              )
            )
            ;; 防止重试过程无变化导致循环反复
            (if (and *dbq:_retry* (= (length items) _last-item-count))
              (progn
                (prompt "\n[DBQ] 本轮排除未改变数据，已自动退出重试。")
                (setq *dbq:_retry* nil)
              )
              (setq _last-item-count (length items))
            )
          )
          ;; 输出结果
          (if (and ok rows)
            (progn
              (setq out nil c 0)
              (while (< c cols)
                (setq vals nil)
                (foreach r rows
                  (setq vals (append vals (list (nth 3 (nth c r)))))
                )
                (setq out (append out (list (dbq:_join-with-hyphen vals))))
                (setq c (1+ c))
              )
              out
            )
            (dbq:get-selected-text-lines)
          )
        )
      )
    )
  )
)

(defun dbq:get-selected-text-lines-smart-merge-row (/ n ss i en ed tp txt pt y h lines result j chunk vobj clean)
  (setq n (if (and (boundp '*dbq-merge-rows*) *dbq-merge-rows* (= (type *dbq-merge-rows*) 'INT) (> *dbq-merge-rows* 1)) *dbq-merge-rows* 3))
  (setq lines nil)
  (prompt "\n请选择需要写回TXT的文字对象（TEXT/MTEXT），回车结束：")
  (setq ss (ssget '((0 . "TEXT,MTEXT"))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq en (ssname ss i)
              ed (entget en)
              tp (if (assoc 0 ed) (cdr (assoc 0 ed)) "")
              txt (if (assoc 1 ed) (cdr (assoc 1 ed)) "")
              pt (if (assoc 10 ed) (cdr (assoc 10 ed)) nil)
              y  (if (and pt (listp pt) (> (length pt) 1)) (cadr pt) 0.0)
        )
        (if (and txt (/= (vl-string-trim " \t\r\n" txt) ""))
          (progn
            ;; 捕获第一个文字的高度
            (if (= i 0)
              (progn
                (cond
                  ((= tp "MTEXT")
                   (progn
                     (setq h 0.0)
                     (vl-catch-all-apply
                       (function
                         (lambda ()
                           (setq vobj (vlax-ename->vla-object en))
                           (setq h (vlax-get vobj 'Height))
                           (vlax-release-object vobj))))
                     (if (not (and (numberp h) (> h 0.0))) (setq h (if (assoc 40 ed) (cdr (assoc 40 ed)) nil)))
                   )
                  )
                  (t (setq h (if (assoc 40 ed) (cdr (assoc 40 ed)) nil)))
                )
                (if (and (numberp h) (> h 0.0)) (setq *dbq-captured-height* h))
              )
            )
            (if (= tp "MTEXT")
              (setq txt (dbq:_join-no-sep (dbq:split-lines (dbq:clean-mtext-string txt))))
              (setq txt (vl-string-trim " \t\r\n" txt))
            )
            ;; 保存为 (y text)
            (setq lines (append lines (list (list y txt))))
          )
        )
        (setq i (1+ i))
      )
      ;; 稳定排序：按视觉从上到下（Y 从大到小）
      (setq lines (vl-sort lines (function (lambda (a b) (> (car a) (car b))))))
      (setq result nil i 0)
      (while (< i (length lines))
        (setq chunk nil j 0)
        (while (and (< j n) (< (+ i j) (length lines)))
          (setq chunk (append chunk (list (cadr (nth (+ i j) lines)))))
          (setq j (1+ j))
        )
        (setq result (append result (list (dbq:_join-with-hyphen chunk))))
        (setq i (+ i n))
      )
      result
    )
    nil
  )
)

;;; ─────────────────────────────────────────────────────────
;;; 绘制封闭矩形 LWPOLYLINE
;;; blx,bly = 左下角坐标   w = 宽   h = 高
;;; ─────────────────────────────────────────────────────────
(defun dbq:rect (blx bly w h / x2 y2)
  (setq x2 (+ blx w)
        y2 (+ bly h))
  (entmakex
    (list
      (cons  0 "LWPOLYLINE")
      (cons 100 "AcDbEntity")
      (cons 100 "AcDbPolyline")
      (cons  90 4)               ; 顶点数
      (cons  70 1)               ; 闭合标志
      (cons  10 (list blx bly))
      (cons  10 (list x2  bly))
      (cons  10 (list x2  y2 ))
      (cons  10 (list blx y2 ))
    )
  )
)

;;; ─────────────────────────────────────────────────────────
;;; 绘制单行文字 TEXT（MC 居中对齐）
;;; cx,cy = 文字中心点   s = 内容   h = 高度   wf = 宽度因子
;;; ─────────────────────────────────────────────────────────
(defun dbq:text (cx cy s h wf)
  (entmakex
    (list
      (cons   0 "TEXT")
      (cons 100 "AcDbEntity")
      (cons 100 "AcDbText")
      (cons  10 (list cx cy 0.0))  ; 初始插入点
      (cons  40 h)                  ; 文字高度
      (cons   1 s)                  ; 文字内容
      (cons  41 wf)                 ; 宽度因子
      (cons  72 1)                  ; 水平对齐：1=Center
      (cons  11 (list cx cy 0.0))  ; MC对齐基准点
      (cons 100 "AcDbText")
      (cons  73 2)                  ; 垂直对齐：2=Middle
    )
  )
)

;;; ─────────────────────────────────────────────────────────
;;; 绘制单个双线矩形标签
;;;
;;; lx = 外框左边X坐标
;;; ly = 外框顶边Y坐标（左上角）
;;; txt = 标签文字内容
;;;
;;; 外框：81×22（blx=lx, bly=ly-22）
;;; 内框：79×20（blx=lx+1, bly=ly-21）
;;; 文字：中心点=(lx+40.5, ly-11)
;;;
;;; 字符数≤5：WF=1.0，居中
;;; 字符数>5：WF=5/n（保持与5字等宽，等比压缩）
;;; ─────────────────────────────────────────────────────────
(defun dbq:label (lx ly txt / bly n wf cx cy)
  (setq bly (- ly dbq:oh))
  ;; 外框 81×22
  (dbq:rect lx bly dbq:ow dbq:oh)
  ;; 内框 79×20（四边各偏移1）
  (dbq:rect
    (+ lx dbq:io)
    (+ bly dbq:io)
    dbq:iw
    dbq:ih
  )
  ;; 计算文字参数
  (setq n  (dbq:nchar txt)
        wf (if (<= n dbq:ns)
             1.0
             (/ dbq:cb (float n))   ; 布满压缩：WF=cb/n
           )
        cx (+ lx (/ (float dbq:ow) 2.0))   ; 标签水平中心
        cy (- ly (/ (float dbq:oh) 2.0))   ; 标签垂直中心
  )
  (dbq:text cx cy txt dbq:th wf)
)

;;; ─────────────────────────────────────────────────────────
;;; 增强排序辅助函数（移植自 VBA 增强升序排列）
;;; 从文本末尾提取连续数字，剩余部分作为前缀
;;; 先按前缀(字符串)升序，前缀相同时按数字(数值)升序
;;; 示例："断路器10" → ("断路器" 10)  "接触器" → ("接触器" 0)
;;; ─────────────────────────────────────────────────────────
(defun dbq:parse-trailing-number (text / codes n i numcodes numlen)
  (setq codes (vl-string->list text)
        n     (length codes)
        i     (1- n)
        numcodes nil)
  (while (>= i 0)
    (if (and (>= (nth i codes) 48) (<= (nth i codes) 57))
      (setq numcodes (cons (nth i codes) numcodes)
            i (1- i))
      (setq i -1)))
  (if numcodes
    (progn
      (setq numlen (length numcodes))
      (list (substr text 1 (- (strlen text) numlen))
            (atoi (vl-list->string numcodes))))
    (list text 0)))

(defun dbq:enhanced-less-p (a b / pa pb)
  (setq pa (dbq:parse-trailing-number a)
        pb (dbq:parse-trailing-number b))
  (cond
    ((< (car pa) (car pb)) t)
    ((> (car pa) (car pb)) nil)
    (t (< (cadr pa) (cadr pb)))))

;;; ─────────────────────────────────────────────────────────
;;; 统计标签分类
;;; 返回关联列表：((分类名 . 数量) ...)
;;; 分类规则：保留原始文本，不按冒号截断
;;; 结果按增强升序排列（前缀+数字）
;;; ─────────────────────────────────────────────────────────
(defun dbq:classify-labels (data / result cat found)
  (setq result nil)
  (foreach txt data
    ;; 提取分类：保留原始文本，不按冒号截断
    (setq cat txt)
    ;; 去除首尾空格
    (setq cat (vl-string-trim " \t" cat))
    ;; 统计
    (setq found nil)
    (setq result
      (mapcar
        '(lambda (pair)
           (if (= (car pair) cat)
             (progn
               (setq found t)
               (cons (car pair) (1+ (cdr pair)))
             )
             pair
           )
         )
        result
      )
    )
    (if (not found)
      (setq result (append result (list (cons cat 1))))
    )
  )
  ;; 增强升序排序（前缀+数字）
  (vl-sort result '(lambda (a b) (dbq:enhanced-less-p (car a) (car b))))
)

;;; ─────────────────────────────────────────────────────────
;;; 绘制分类汇总
;;; rx = 汇总区域左边X坐标（通常是第一页纸张框右侧）
;;; ry = 汇总区域顶部Y坐标（与第一页纸张框顶部对齐）
;;; stats = 分类统计列表 ((分类名 . 数量) ...)
;;; use-captured-height = 是否使用捕获的文字高度
;;; ─────────────────────────────────────────────────────────
(defun dbq:draw-summary (rx ry stats use-captured-height / y line-h total num-x maxw row-idx row-color title-h text-h text-spacing total-spacing sn-x name-x sn-maxw i)
  ;; 确定文字高度和间距（智能匹配）
  (if (and use-captured-height *dbq-captured-height* (> *dbq-captured-height* 0.0))
    (progn
      ;; 使用捕获的字高进行智能计算
      ;; ─────────────────────────────────────────────
      ;; 智能匹配规则：
      ;; 1. 标题高度 = 字高 × 1.2（比正文大20%）
      ;; 2. 行间距 = 字高 × 1.5（字高 + 字高的50%作为行距）
      ;; 3. 文本间距 = 字高 × 1.6（约1.5-2个字符宽度）
      ;; 4. 总计前间距 = 字高 × 0.3（小间隔分隔）
      ;; ─────────────────────────────────────────────
      (setq text-h *dbq-captured-height*
            title-h (* *dbq-captured-height* 1.2)
            line-h (* *dbq-captured-height* 1.5)
            text-spacing (* *dbq-captured-height* 1.6)
            total-spacing (* *dbq-captured-height* 0.3))
    )
    (progn
      ;; 使用默认值（字高10的情况）
      (setq text-h 10
            title-h 12
            line-h 15
            text-spacing 16.0
            total-spacing 5)
    )
  )
  (setq maxw 0.0)
  (foreach pair stats
    (setq maxw (max maxw
      ((lambda (s h / tb)
         (setq tb (textbox (list (cons 40 h) (cons 41 1.0) (cons 1 s))))
         (if tb (- (caadr tb) (caar tb)) 0.0)
       ) (car pair) text-h)
    ))
  )
  (setq maxw (max maxw
    ((lambda (s h / tb)
       (setq tb (textbox (list (cons 40 h) (cons 41 1.0) (cons 1 s))))
       (if tb (- (caadr tb) (caar tb)) 0.0)
     ) "总计" text-h)
  ))
  ;; 计算序号列最大宽度
  (setq sn-maxw 0.0
        i 1)
  (foreach pair stats
    (setq sn-maxw (max sn-maxw
      ((lambda (s h / tb)
         (setq tb (textbox (list (cons 40 h) (cons 41 1.0) (cons 1 s))))
         (if tb (- (caadr tb) (caar tb)) 0.0)
       ) (itoa i) text-h)
    ))
    (setq i (1+ i))
  )
  (setq y ry
        total 0
        sn-x rx
        name-x (+ rx sn-maxw text-spacing)
        num-x (+ name-x maxw text-spacing)
  )
  ;; 标题
  (entmakex
    (list
      (cons   0 "TEXT")
      (cons 100 "AcDbEntity")
      (cons  62 1)  ; 颜色：红色
      (cons 100 "AcDbText")
      (cons  10 (list rx y 0.0))
      (cons  40 title-h)
      (cons   1 "【分类汇总】")
      (cons  41 1.0)
      (cons  72 0)
      (cons  73 0)
    )
  )
  (setq y (- y line-h))
  
  ;; 分类明细
  (setq row-idx 0
        i 1)
  (foreach pair stats
    (setq total (+ total (cdr pair)))
    (setq row-color (if (= (rem row-idx 2) 0) 7 3))
    ;; 左侧序号（独立文本）
    (entmakex
      (list
        (cons   0 "TEXT")
        (cons 100 "AcDbEntity")
        (cons  62 row-color)
        (cons 100 "AcDbText")
        (cons  10 (list sn-x y 0.0))
        (cons  40 text-h)
        (cons   1 (itoa i))
        (cons  41 1.0)
        (cons  72 0)
        (cons  73 0)
      )
    )
    ;; 分类名文本
    (entmakex
      (list
        (cons   0 "TEXT")
        (cons 100 "AcDbEntity")
        (cons  62 row-color)
        (cons 100 "AcDbText")
        (cons  10 (list name-x y 0.0))
        (cons  40 text-h)
        (cons   1 (car pair))
        (cons  41 1.0)
        (cons  72 0)
        (cons  73 0)
      )
    )
    ;; 右侧数量文本（统一中心对齐到同一基准）
    (entmakex
      (list
        (cons   0 "TEXT")
        (cons 100 "AcDbEntity")
        (cons  62 row-color)
        (cons 100 "AcDbText")
        (cons  10 (list num-x y 0.0))
        (cons  40 text-h)
        (cons   1 (itoa (cdr pair)))
        (cons  41 1.0)
        (cons  72 1)
        (cons  11 (list num-x y 0.0))
        (cons 100 "AcDbText")
        (cons  73 0)
      )
    )
    (setq y (- y line-h))
    (setq row-idx (1+ row-idx))
    (setq i (1+ i))
  )
  
  ;; 总计
  (setq y (- y total-spacing))
  (entmakex
    (list
      (cons   0 "TEXT")
      (cons 100 "AcDbEntity")
      (cons  62 1)  ; 颜色：红色
      (cons 100 "AcDbText")
      (cons  10 (list name-x y 0.0))
      (cons  40 text-h)
      (cons   1 "总计")
      (cons  41 1.0)
      (cons  72 0)
      (cons  73 0)
    )
  )
  (entmakex
    (list
      (cons   0 "TEXT")
      (cons 100 "AcDbEntity")
      (cons  62 1)
      (cons 100 "AcDbText")
      (cons  10 (list num-x y 0.0))
      (cons  40 text-h)
      (cons   1 (itoa total))
      (cons  41 1.0)
      (cons  72 1)
      (cons  11 (list num-x y 0.0))
      (cons 100 "AcDbText")
      (cons  73 0)
    )
  )
)

;;; ─────────────────────────────────────────────────────────
;;; 绘制纸张包围框
;;; a4x,a4y = 本页纸张框左上角坐标
;;;
;;; 纸张框左下角：(a4x, a4y - ph)
;;; ─────────────────────────────────────────────────────────
(defun dbq:a4-box (a4x a4y)
  (dbq:rect
    a4x
    (- a4y dbq:ph)
    dbq:pw
    dbq:ph
  )
)

;;; ─────────────────────────────────────────────────────────
;;; 排版主逻辑
;;; sp = 用户点选的起点（A4框左上角）
;;;
;;; 位置计算（第i个标签，0起）：
;;;   组号  g = i / pg
;;;   组内序 k = i mod pg
;;;   行号  r = k / nc
;;;   列号  c = k mod nc
;;;
;;;   A4框左上角   a4x = sx
;;;                a4y = sy - g × gh
;;;
;;;   标签阵列左上角（居中后）：
;;;     content_lx = a4x + ox      (ox = 水平居中偏移)
;;;     content_ly = a4y - oy      (oy = 垂直居中偏移)
;;;
;;;   标签左上角 lx = content_lx + c × hs
;;;              ly = content_ly - r × vs
;;; ─────────────────────────────────────────────────────────
(defun dbq:layout (sp use-captured-height / sx sy i g r c lx ly a4x a4y clx cly cur-grp ngrp stats)
  (setq sx      (car sp)
        sy      (cadr sp)
        i       0
        cur-grp -1
        ngrp    0
        stats   (dbq:classify-labels *dbq-data*)  ; 统计分类
  )
  (foreach txt *dbq-data*
    (setq g   (/ i dbq:pg)
          r   (/ (rem i dbq:pg) dbq:nc)
          c   (rem i dbq:nc)
          ;; 当前组的A4框左上角
          a4x sx
          a4y (- sy (* g dbq:gh))
          ;; 标签内容区域左上角（水平居中）
          clx (+ a4x dbq:ox)
          cly (- a4y dbq:oy)
          ;; 当前标签左上角
          lx  (+ clx (* c dbq:hs))
          ly  (- cly (* r dbq:vs))
    )
    ;; 新组时先绘制A4包围框
    (if (/= g cur-grp)
      (progn
        (dbq:a4-box a4x a4y)
        ;; 如果是第一页（g=0），在右侧绘制分类汇总
        (if (= g 0)
          (dbq:draw-summary (+ a4x dbq:pw 20) a4y stats use-captured-height)
        )
        (setq cur-grp g
              ngrp    (1+ ngrp))
      )
    )
    ;; 绘制标签
    (dbq:label lx ly txt)
    (setq i (1+ i))
  )
  ngrp   ; 返回实际组数（页数）
)

;;; ─────────────────────────────────────────────────────────
;;; 将 DCL 对话框定义写入临时文件
;;; ─────────────────────────────────────────────────────────
(defun dbq:write-dcl (/ fn fp)
  (setq fn (strcat (getenv "TEMP") "\\dbq.dcl"))
  (if (not (findfile fn))
    (progn
      (setq fp (open fn "w"))
      (foreach s (list
    "dbq_dlg : dialog {"
    "  label = \"CAD 标签排版系统（DBQ）\";"
    "  : boxed_row {"
    "    label = \"TXT 文件\";"
    "    : edit_box {"
    "      key        = \"epath\";"
    "      width      = 44;"
    "      is_enabled = false;"
    "    }"
    "    : button {"
    "      key         = \"bsel\";"
    "      label       = \"选择TXT\";"
    "      width       = 10;"
    "      fixed_width = true;"
    "    }"
    "    : button {"
    "      key         = \"bwrite\";"
    "      label       = \"写回TXT\";"
    "      width       = 10;"
    "      fixed_width = true;"
    "    }"
    "  }"
    "  : edit_box {"
    "      key         = \"e_title\";"
    "      label       = \"标题说明:\";"
    "      width       = 48;"
    "    }"
    "  spacer_1;"
    "  : boxed_row {"
    "    label = \"纸张\";"
    "    : radio_row {"
    "      : radio_button { key = \"rb_a4\"; label = \"A4\"; }"
    "      : radio_button { key = \"rb_a3\"; label = \"A3\"; }"
    "      : radio_button { key = \"rb_custom\"; label = \"自定义\"; }"
    "    }"
    "    : edit_box { key = \"e_pw\"; label = \"宽:\"; width = 8; edit_width = 6; is_enabled = false; }"
    "    : edit_box { key = \"e_ph\"; label = \"高:\"; width = 8; edit_width = 6; is_enabled = false; }"
    "  }"
    "  spacer_1;"
    "  : boxed_row {"
    "    label = \"文字矩形\";"
    "    : edit_box {"
    "      key         = \"e_iw\";"
    "      label       = \"内框宽(W):\";"
    "      width       = 8;"
    "      edit_width  = 6;"
    "    }"
    "    : edit_box {"
    "      key         = \"e_ih\";"
    "      label       = \"内框高(H):\";"
    "      width       = 8;"
    "      edit_width  = 6;"
    "    }"
    "  }"
    "  : boxed_row {"
    "    label = \"外框尺寸\";"
    "    : edit_box {"
    "      key         = \"e_ow\";"
    "      label       = \"外框宽(W):\";"
    "      width       = 8;"
    "      edit_width  = 6;"
    "    }"
    "    : edit_box {"
    "      key         = \"e_oh\";"
    "      label       = \"外框高(H):\";"
    "      width       = 8;"
    "      edit_width  = 6;"
    "    }"
    "  }"
    "  : boxed_row {"
    "    label = \"文字压缩\";"
    "    : edit_box {"
    "      key         = \"e_ns\";"
    "      label       = \"阈值(字):\";"
    "      width       = 8;"
    "      edit_width  = 6;"
    "    }"
    "    : edit_box {"
    "      key         = \"e_cb\";"
    "      label       = \"压缩基准:\";"
    "      width       = 8;"
    "      edit_width  = 6;"
    "    }"
    "    : edit_box {"
    "      key         = \"e_th\";"
    "      label       = \"字高:\";"
    "      width       = 6;"
    "      edit_width  = 5;"
    "    }"
    "  }"
    "  : boxed_row {"
    "    label = \"标签间距\";"
    "    : edit_box {"
    "      key         = \"e_hg\";"
    "      label       = \"左右间距:\";"
    "      width       = 10;"
    "      edit_width  = 6;"
    "    }"
    "    : edit_box {"
    "      key         = \"e_vg\";"
    "      label       = \"上下间距:\";"
    "      width       = 10;"
    "      edit_width  = 6;"
    "    }"
    "  }"
    "  : boxed_row {"
    "    label = \"配置管理\";"
    "    : edit_box {"
    "      key         = \"e_cfgname\";"
    "      label       = \"名称:\";"
    "      width       = 14;"
    "    }"
    "    : button {"
    "      key         = \"b_cfgsave\";"
    "      label       = \"保存\";"
    "      width       = 6;"
    "      fixed_width = true;"
    "    }"
    "    : button {"
    "      key         = \"b_cfgdel\";"
    "      label       = \"删除\";"
    "      width       = 6;"
    "      fixed_width = true;"
    "    }"
    "  }"
    "  : popup_list {"
    "    key         = \"e_cfglist\";"
    "    width       = 46;"
    "  }"
     "  : toggle {"
     "    key         = \"cb_summary_only\";"
     "    label       = \"仅生成分类汇总\";"
     "    value       = \"0\";"
     "  }"
     "  : toggle {"
     "    key         = \"cb_match_height\";"
     "    label       = \"字高匹配（使用写回TXT时捕获的文字高度）\";"
     "    value       = \"0\";"
     "  }"
     "  : row {"
     "    : toggle {"
     "      key         = \"cb_smart_merge\";"
     "      label       = \"写回TXT启用智能按列合并\";"
     "      value       = \"0\";"
     "    }"
      "    : toggle {"
      "      key         = \"cb_append\";"
      "      label       = \"追加写回\";"
      "      value       = \"0\";"
      "    }"
      "    : text {"
      "      key         = \"t_file_lines\";"
      "      label       = \"\";"
      "      width       = 6;"
      "      fixed_width = true;"
      "    }"
      "  }"
      "  : row {"
     "    : toggle {"
     "      key         = \"cb_smart_merge_row\";"
     "      label       = \"写回TXT启用智能按行合并\";"
     "      value       = \"0\";"
     "    }"
     "    : edit_box {"
     "      key         = \"e_merge_rows\";"
     "      label       = \"合并行数:\";"
     "      width       = 10;"
     "      edit_width  = 4;"
     "      value       = \"3\";"
     "    }"
     "  }"
      "  : edit_box {"
      "    key         = \"e_rowtol\";"
      "    label       = \"分行容差 (k):\";"
      "    width       = 12;"
      "    edit_width  = 8;"
      "  }"
      "  : text {"
      "    key         = \"t_rowtol_hint\";"
      "    label       = \"k 为高度比例，默认 0.5，建议 0.3-0.7（范围 0.1-2.0）\";"
      "    width       = 50;"
      "  }"
    "  spacer_1;"
    "  : text {"
    "    key         = \"t_preview\";"
    "    label       = \"\";"
    "    width       = 50;"
    "    alignment   = centered;"
    "  }"
    "  spacer_1;"
    "  : row {"
    "    : button {"
    "      key         = \"accept\";"
    "      label       = \"确定\";"
    "      is_default  = true;"
    "      width       = 10;"
    "      fixed_width = true;"
    "    }"
    "    : button {"
    "      key         = \"cancel\";"
    "      label       = \"取消\";"
    "      is_cancel   = true;"
    "      width       = 10;"
    "      fixed_width = true;"
    "    }"
    "    : button {"
    "      key         = \"b_undo_append\";"
    "      label       = \"撤销上次追加\";"
    "      width       = 16;"
    "      fixed_width = true;"
    "    }"
    "  }"
    "}"
  )
      (write-line s fp)
    )
    (close fp)
  )
  )
  fn
)

;;; ─────────────────────────────────────────────────────────
;;; 自动计算纸张高度
;;; 根据宽度、标签尺寸、间距和数据量计算最小所需高度
;;; ─────────────────────────────────────────────────────────
(defun dbq:auto-calc-height (/ pw ow oh hg vg nc ntotal nr ph-min)
  (setq pw (atoi (get_tile "e_pw"))
        ow (atoi (get_tile "e_ow"))
        oh (atoi (get_tile "e_oh"))
        hg (atoi (get_tile "e_hg"))
        vg (atoi (get_tile "e_vg"))
  )
  ;; 计算列数
  (setq nc (max 1 (fix (/ (+ (- pw 14) hg) (+ ow hg)))))
  
  ;; 获取标签总数
  (setq ntotal (if (and (boundp '*dbq-data*) *dbq-data*)
                 (length *dbq-data*)
                 0)
  )
  
  ;; 如果有数据，计算所需行数
  (if (> ntotal 0)
    (progn
      ;; 所需行数 = 向上取整(总数 / 列数)
      (setq nr (fix (+ (/ (float ntotal) nc) 0.999999)))
      ;; 最小高度 = 上下边距 + 行数×标签高 + (行数-1)×垂直间距
      (setq ph-min (+ 14 (* nr oh) (* (max 0 (1- nr)) vg)))
      ;; 填入高度
      (set_tile "e_ph" (itoa ph-min))
      (dbq:update-preview)
    )
    ;; 没有数据时，使用默认高度210
    (progn
      (set_tile "e_ph" "210")
      (dbq:update-preview)
    )
  )
)

;;; ─────────────────────────────────────────────────────────
;;; 更新排版预览摘要
;;; 根据当前对话框参数计算并显示：X列×X行=X个/页，共X页
;;; ─────────────────────────────────────────────────────────
(defun dbq:update-preview (/ pw ph ow oh hg vg nc nr pg npg ntotal preview-str)
  (setq pw (atoi (get_tile "e_pw"))
        ph (atoi (get_tile "e_ph"))
        ow (atoi (get_tile "e_ow"))
        oh (atoi (get_tile "e_oh"))
        hg (atoi (get_tile "e_hg"))
        vg (atoi (get_tile "e_vg"))
  )
  ;; 计算列数、行数、每页标签数
  (setq nc (max 1 (fix (/ (+ (- pw 14) hg) (+ ow hg))))
        nr (max 1 (fix (/ (+ (- ph 14) vg) (+ oh vg))))
        pg (* nc nr)
  )
  ;; 计算总页数（如果已加载数据）
  (setq ntotal (if (and (boundp '*dbq-data*) *dbq-data*)
                 (length *dbq-data*)
                 0)
        npg (if (> ntotal 0)
              (fix (+ (/ (float ntotal) pg) 0.999999))  ; 向上取整
              0)
  )
  ;; 构建预览字符串
  (setq preview-str
    (if (> ntotal 0)
      (strcat (itoa nc) "列×" (itoa nr) "行=" (itoa pg) "个/页，共" (itoa ntotal) "个标签，" (itoa npg) "页")
      (strcat (itoa nc) "列×" (itoa nr) "行=" (itoa pg) "个/页")
    )
  )
  (set_tile "t_preview" preview-str)
)

;;; 更新文件行数标签（对话框实时显示用）
(defun dbq:update-file-lines-label (/ n)
  (if (/= (get_tile "cb_append") "1")
    (set_tile "t_file_lines" "")
    (progn
      (setq n (if (and (boundp '*dbq-data*) *dbq-data*) (length *dbq-data*) 0))
      (set_tile "t_file_lines" (itoa n))
    )
  )
)

;;; 毫秒延时（不阻塞对话框刷新）
(defun dbq:delay-ms (ms / start)
  (setq start (getvar "date"))
  (while (< (* (- (getvar "date") start) 86400000.0) ms))
)

;;; 行数标签闪烁2次（隐藏/显示交替，模拟红绿闪烁）
(defun dbq:flash-lines-label (/ i label)
  (setq label (get_tile "t_file_lines"))
  (setq i 0)
  (while (< i 4)
    (set_tile "t_file_lines" "")
    (dbq:delay-ms 150)
    (set_tile "t_file_lines" label)
    (dbq:delay-ms 150)
    (setq i (1+ i))
  )
)

;;; 撤销上一次追加写回
(defun dbq:undo-append (/ n)
  (if (not *dbq-backup-data*)
    (alert "没有可撤销的追加操作")
    (progn
      (setq *dbq-data* (mapcar '(lambda (x) x) *dbq-backup-data*)
            *dbq-backup-data* nil)
      (if (dbq:write-txt *dbq-path* *dbq-data* 0)
        (progn
          (setq *dbq-data* (dbq:read-txt *dbq-path*))
          (setq n (length *dbq-data*))
          (set_tile "t_file_lines" (itoa n))
          (dbq:flash-lines-label)
        )
        (alert "撤销失败：无法写入文件")
      )
    )
  )
)

;;; ─────────────────────────────────────────────────────────
;;; 主命令入口：DBQ
;;; ─────────────────────────────────────────────────────────
(defun c:DBQ (/ dcl-fn dcl-id ret sp ngrp old-echo stats _target _lines *error*)
  ;; 初始化
  (dbq:init)
  (setq old-echo (getvar "CMDECHO"))
  ;; 本地错误处理
  (defun *error* (msg)
    (if old-echo (setvar "CMDECHO" old-echo))
    (if dcl-id (unload_dialog dcl-id))
    (if (and msg (/= msg "Function cancelled") (/= msg "quit / exit abort"))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )
  ;; ── 对话框循环（ret=2 时重新打开对话框，代替递归） ──
  (setq ret 1)
  (while (>= ret 1)
    ;; 重新初始化 DCL
    (setq dcl-fn (dbq:write-dcl)
          dcl-id (load_dialog dcl-fn))
    (if (or (null dcl-id) (minusp dcl-id))
      (progn (alert "DCL 加载失败！请检查 TEMP 目录权限。") (setq dcl-id nil))
    )
    (if (and dcl-id (not (new_dialog "dbq_dlg" dcl-id)))
      (progn
        (alert "对话框初始化失败！")
        (unload_dialog dcl-id)
        (setq dcl-id nil)
      )
    )

    (if dcl-id
      (progn
        ;; 显示上次选择的路径
        (if (and *dbq-path* (/= *dbq-path* ""))
          (set_tile "epath" *dbq-path*)
        )
        ;; 设置标题说明
        (set_tile "e_title" (if (and (boundp '*dbq-title*) *dbq-title*) *dbq-title* ""))
        ;; 设置文字矩形宽高初始值
        (set_tile "e_iw" (itoa (if (and (boundp '*dbq-iw*) *dbq-iw*) *dbq-iw* 79)))
        (set_tile "e_ih" (itoa (if (and (boundp '*dbq-ih*) *dbq-ih*) *dbq-ih* 20)))
        (set_tile "e_ow" (itoa (if (and (boundp '*dbq-ow*) *dbq-ow*) *dbq-ow* 81)))
        (set_tile "e_oh" (itoa (if (and (boundp '*dbq-oh*) *dbq-oh*) *dbq-oh* 22)))
        ;; 设置文字压缩参数初始值
        (set_tile "e_ns" (itoa (if (and (boundp '*dbq-ns*) *dbq-ns*) *dbq-ns* 5)))
        (set_tile "e_cb" (rtos  (if (and (boundp '*dbq-cb*) *dbq-cb*) *dbq-cb* 5.0) 2 1))
        (set_tile "e_th" (itoa (if (and (boundp '*dbq-th*) *dbq-th*) *dbq-th* 10)))
        (set_tile "e_hg" (itoa (if (and (boundp '*dbq-hg*) *dbq-hg*) *dbq-hg* 7)))
        (set_tile "e_vg" (itoa (if (and (boundp '*dbq-vg*) *dbq-vg*) *dbq-vg* 7)))
        ;; 设置纸张选择
        (set_tile (nth *dbq-paper* '("rb_a4" "rb_a3" "rb_custom")) "1")
        (set_tile "e_pw" (itoa *dbq-pw*))
        (set_tile "e_ph" (itoa *dbq-ph*))
        ;; 根据纸张类型设置输入框启用/禁用状态
        (if (= *dbq-paper* 2)
          (progn (mode_tile "e_pw" 0) (mode_tile "e_ph" 0))
          (progn (mode_tile "e_pw" 1) (mode_tile "e_ph" 1))
        )
        ;; 填充配置下拉框，默认选中第一个
        (dbq:populate-cfglist)
        (if *dbq-configs* (dbq:apply-config 0))
        (set_tile "cb_summary_only" (itoa (if (and (boundp '*dbq-summary-only*) *dbq-summary-only*) *dbq-summary-only* 0)))
        (set_tile "cb_match_height" (itoa (if (and (boundp '*dbq-match-height*) *dbq-match-height*) *dbq-match-height* 0)))
        (set_tile "cb_smart_merge" (itoa (if (and (boundp '*dbq-smart-merge*) *dbq-smart-merge*) *dbq-smart-merge* 0)))
        (set_tile "cb_smart_merge_row" (itoa (if (and (boundp '*dbq-smart-merge-row*) *dbq-smart-merge-row*) *dbq-smart-merge-row* 0)))
        (set_tile "e_merge_rows" (itoa (if (and (boundp '*dbq-merge-rows*) *dbq-merge-rows*) *dbq-merge-rows* 3)))
        (set_tile "cb_append" (itoa (if (and (boundp '*dbq-append*) *dbq-append*) *dbq-append* 0)))
        (set_tile "e_rowtol" (rtos (if (and (boundp '*dbq-rowtol*) *dbq-rowtol* (= (type *dbq-rowtol*) 'REAL)) *dbq-rowtol* 0.5) 2 2))
        ;; 更新提示文本
        (set_tile "t_rowtol_hint" (if (and (boundp '*dbq-rowtol*) *dbq-rowtol* (= (type *dbq-rowtol*) 'REAL))
                                       (strcat "k=" (rtos *dbq-rowtol* 2 2) " (height ratio)")
                                       "k=0.5 (height ratio, default)"))
        (dbq:update-file-lines-label)
        (if (= (get_tile "cb_append") "1")
          (mode_tile "b_undo_append" 0)
          (mode_tile "b_undo_append" 1)
        )
        (if (= (get_tile "cb_summary_only") "1")
          (progn
            (mode_tile "e_title" 1) (mode_tile "e_iw" 1) (mode_tile "e_ih" 1)
            (mode_tile "e_ow" 1) (mode_tile "e_oh" 1) (mode_tile "e_ns" 1)
            (mode_tile "e_cb" 1) (mode_tile "e_th" 1) (mode_tile "e_hg" 1) (mode_tile "e_vg" 1)
          )
        )
        ;; 互锁初始状态
        (if (= (get_tile "cb_smart_merge_row") "1")
          (mode_tile "e_merge_rows" 0)
          (mode_tile "e_merge_rows" 1)
        )
        ;; 初始化预览摘要
        (dbq:update-preview)
        ;; ── action_tiles ──────────────────────────
        (action_tile "bsel"
          (vl-prin1-to-string
            '(progn
               (setq *dbq-title* (get_tile "e_title"))
               (setq *dbq-iw* (atoi (get_tile "e_iw"))
                     *dbq-ih* (atoi (get_tile "e_ih"))
                     *dbq-ow* (atoi (get_tile "e_ow"))
                     *dbq-oh* (atoi (get_tile "e_oh")))
               (setq *dbq-ns* (atoi (get_tile "e_ns"))
                     *dbq-cb* (atof (get_tile "e_cb"))
                     *dbq-th* (atoi (get_tile "e_th"))
                     *dbq-hg* (atoi (get_tile "e_hg"))
                     *dbq-vg* (atoi (get_tile "e_vg")))
               (setq *dbq-paper* (cond ((= (get_tile "rb_a4") "1") 0)
                                       ((= (get_tile "rb_a3") "1") 1)
                                       (t 2))
                     *dbq-pw* (atoi (get_tile "e_pw"))
                     *dbq-ph* (atoi (get_tile "e_ph")))
               (setq _fn (getfiled "请选择标签TXT文件" *dbq-path* "txt" 0))
               (if _fn
                 (progn
                   (setq *dbq-path* _fn)
                   (setq *dbq-data* (dbq:read-txt _fn))
                    (set_tile "epath" _fn)
                    (if (= (get_tile "rb_custom") "1")
                      (dbq:auto-calc-height)
                      (dbq:update-preview)
                    )
                    (dbq:update-file-lines-label)
                  )
                )
              )
           )
        )
        (action_tile "rb_a4" "(mode_tile \"e_pw\" 1)(mode_tile \"e_ph\" 1)(set_tile \"e_pw\" \"297\")(set_tile \"e_ph\" \"210\")(dbq:update-preview)")
        (action_tile "rb_a3" "(mode_tile \"e_pw\" 1)(mode_tile \"e_ph\" 1)(set_tile \"e_pw\" \"420\")(set_tile \"e_ph\" \"297\")(dbq:update-preview)")
        (action_tile "rb_custom" "(mode_tile \"e_pw\" 0)(mode_tile \"e_ph\" 0)")
        (action_tile "e_pw" "(if (and (= (get_tile \"rb_custom\") \"1\") (boundp '*dbq-data*) *dbq-data*) (dbq:auto-calc-height) (dbq:update-preview))")
        (action_tile "e_ph" "(dbq:update-preview)")
        (action_tile "e_ow" "(if (and (= (get_tile \"rb_custom\") \"1\") (boundp '*dbq-data*) *dbq-data*) (dbq:auto-calc-height) (dbq:update-preview))")
        (action_tile "e_oh" "(if (and (= (get_tile \"rb_custom\") \"1\") (boundp '*dbq-data*) *dbq-data*) (dbq:auto-calc-height) (dbq:update-preview))")
        (action_tile "e_hg" "(if (and (= (get_tile \"rb_custom\") \"1\") (boundp '*dbq-data*) *dbq-data*) (dbq:auto-calc-height) (dbq:update-preview))")
        (action_tile "e_vg" "(if (and (= (get_tile \"rb_custom\") \"1\") (boundp '*dbq-data*) *dbq-data*) (dbq:auto-calc-height) (dbq:update-preview))")
        (action_tile "cb_summary_only"
          "(if (= (get_tile \"cb_summary_only\") \"1\") (progn (mode_tile \"e_title\" 1)(mode_tile \"e_iw\" 1)(mode_tile \"e_ih\" 1)(mode_tile \"e_ow\" 1)(mode_tile \"e_oh\" 1)(mode_tile \"e_ns\" 1)(mode_tile \"e_cb\" 1)(mode_tile \"e_th\" 1)(mode_tile \"e_hg\" 1)(mode_tile \"e_vg\" 1)) (progn (mode_tile \"e_title\" 0)(mode_tile \"e_iw\" 0)(mode_tile \"e_ih\" 0)(mode_tile \"e_ow\" 0)(mode_tile \"e_oh\" 0)(mode_tile \"e_ns\" 0)(mode_tile \"e_cb\" 0)(mode_tile \"e_th\" 0)(mode_tile \"e_hg\" 0)(mode_tile \"e_vg\" 0)))"
        )
        (action_tile "cb_smart_merge"
          "(if (= (get_tile \"cb_smart_merge\") \"1\") (set_tile \"cb_smart_merge_row\" \"0\"))"
        )
        (action_tile "cb_smart_merge_row"
          "(if (= (get_tile \"cb_smart_merge_row\") \"1\") (progn (set_tile \"cb_smart_merge\" \"0\") (mode_tile \"e_merge_rows\" 0)) (mode_tile \"e_merge_rows\" 1))"
        )
        (action_tile "cb_append"
          "(dbq:update-file-lines-label)(if (= (get_tile \"cb_append\") \"1\") (mode_tile \"b_undo_append\" 0) (mode_tile \"b_undo_append\" 1))"
        )
        (action_tile "b_undo_append"
          "(dbq:undo-append)"
        )
        (action_tile "bwrite"
          (vl-prin1-to-string
            '(progn
               (setq *dbq-title* (get_tile "e_title"))
               (setq *dbq-summary-only* (atoi (get_tile "cb_summary_only")))
               (setq *dbq-match-height* (atoi (get_tile "cb_match_height")))
               (setq *dbq-smart-merge* (atoi (get_tile "cb_smart_merge")))
               (setq *dbq-smart-merge-row* (atoi (get_tile "cb_smart_merge_row")))
               (setq *dbq-merge-rows* (max 2 (atoi (get_tile "e_merge_rows"))))
               (setq *dbq-append* (atoi (get_tile "cb_append")))
                (setq *dbq-rowtol* (atof (get_tile "e_rowtol")))
                (if (<= *dbq-rowtol* 0.0) (setq *dbq-rowtol* 0.5))
                (dbq:save-state)
                (setq *dbq-iw* (atoi (get_tile "e_iw"))
                     *dbq-ih* (atoi (get_tile "e_ih"))
                     *dbq-ow* (atoi (get_tile "e_ow"))
                     *dbq-oh* (atoi (get_tile "e_oh"))
                     *dbq-ns* (atoi (get_tile "e_ns"))
                     *dbq-cb* (atof (get_tile "e_cb"))
                     *dbq-th* (atoi (get_tile "e_th"))
                     *dbq-hg* (atoi (get_tile "e_hg"))
                     *dbq-vg* (atoi (get_tile "e_vg")))
                (setq *dbq-paper* (cond ((= (get_tile "rb_a4") "1") 0)
                                        ((= (get_tile "rb_a3") "1") 1)
                                        (t 2))
                      *dbq-pw* (atoi (get_tile "e_pw"))
                      *dbq-ph* (atoi (get_tile "e_ph")))
                (done_dialog 2)
             )
          )
        )
        (action_tile "accept"
          (vl-prin1-to-string
            '(progn
               (setq *dbq-title* (get_tile "e_title"))
               (setq *dbq-iw* (atoi (get_tile "e_iw"))
                     *dbq-ih* (atoi (get_tile "e_ih"))
                     *dbq-ow* (atoi (get_tile "e_ow"))
                     *dbq-oh* (atoi (get_tile "e_oh")))
               (setq *dbq-ns* (atoi (get_tile "e_ns"))
                     *dbq-cb* (atof (get_tile "e_cb"))
                     *dbq-th* (atoi (get_tile "e_th"))
                     *dbq-hg* (atoi (get_tile "e_hg"))
                     *dbq-vg* (atoi (get_tile "e_vg")))
               (setq *dbq-paper* (cond ((= (get_tile "rb_a4") "1") 0)
                                       ((= (get_tile "rb_a3") "1") 1)
                                       (t 2))
                     *dbq-pw* (atoi (get_tile "e_pw"))
                     *dbq-ph* (atoi (get_tile "e_ph")))
               (setq *dbq-summary-only* (atoi (get_tile "cb_summary_only")))
               (setq *dbq-match-height* (atoi (get_tile "cb_match_height")))
               (setq *dbq-smart-merge* (atoi (get_tile "cb_smart_merge")))
               (setq *dbq-smart-merge-row* (atoi (get_tile "cb_smart_merge_row")))
               (setq *dbq-merge-rows* (max 2 (atoi (get_tile "e_merge_rows"))))
               (setq *dbq-append* (atoi (get_tile "cb_append")))
                (setq *dbq-rowtol* (atof (get_tile "e_rowtol")))
                (if (<= *dbq-rowtol* 0.0) (setq *dbq-rowtol* 0.5))
                (dbq:save-state)
                (if (or (null *dbq-data*) (zerop (length *dbq-data*)))
                  (alert "请先选择TXT文件，且文件中至少有一行内容！")
                  (done_dialog 1)
                )
             )
          )
        )
        (action_tile "e_cfglist"
          (vl-prin1-to-string
            '(progn (dbq:apply-config (atoi (get_tile "e_cfglist"))))
          )
        )
        (action_tile "b_cfgsave"
          (vl-prin1-to-string
            '(progn
               (setq _cfgname (get_tile "e_cfgname"))
               (if (/= _cfgname "")
                 (progn
                   (setq _cfg (list _cfgname
                                    (atoi (get_tile "e_iw"))
                                    (atoi (get_tile "e_ih"))
                                    (atoi (get_tile "e_ow"))
                                    (atoi (get_tile "e_oh"))
                                    (atoi (get_tile "e_ns"))
                                    (atof (get_tile "e_cb"))
                                    (atoi (get_tile "e_th"))
                                    (atoi (get_tile "e_hg"))
                                    (atoi (get_tile "e_vg"))
                                    (get_tile "e_title")))
                   (setq *dbq-configs* (dbq:update-config *dbq-configs* _cfg))
                   (dbq:save-configs *dbq-configs*)
                   (dbq:populate-cfglist)
                   (set_tile "e_cfglist" (itoa (dbq:find-config *dbq-configs* _cfgname)))
                 )
                 (alert "请先输入配置名称！")
               )
             )
          )
        )
        (action_tile "b_cfgdel"
          (vl-prin1-to-string
            '(progn
               (setq _idx (atoi (get_tile "e_cfglist")))
               (if (and *dbq-configs* (>= _idx 0) (< _idx (length *dbq-configs*)))
                 (progn
                   (setq *dbq-configs* (dbq:remove-config *dbq-configs* _idx))
                   (dbq:save-configs *dbq-configs*)
                   (dbq:populate-cfglist)
                   (if *dbq-configs*
                     (dbq:apply-config 0)
                     (set_tile "e_cfgname" "")
                   )
                 )
               )
             )
          )
        )
        (action_tile "cancel" "(dbq:save-state)(done_dialog 0)")

        (setq ret (start_dialog))
        (unload_dialog dcl-id)
      )
      (setq ret 0)
    )

    ;; ── 处理对话框结果 ──────────────────────────────
    (cond
      ((= ret 1)
        ;; 排版模式
        (dbq:init)
        (setvar "CMDECHO" 0)
        (initget 1)
        (if (= *dbq-summary-only* 1)
          (progn
            (setq sp (getpoint "\n请在CAD中点击分类汇总左上角起点："))
            (if sp
              (progn
                (setq sp (trans sp 1 0))
                (setq stats (dbq:classify-labels *dbq-data*))
                (dbq:draw-summary (car sp) (cadr sp) stats (= *dbq-match-height* 1))
                (princ (strcat "\n[OK] 分类汇总完成！共 " (itoa (length *dbq-data*)) " 个标签。"))
              )
            )
          )
          (progn
            (setq sp (getpoint "\n请在CAD中点击标签阵列左上角起点："))
            (if sp
              (progn
                (setq sp (trans sp 1 0))
                (setq ngrp (dbq:layout sp (= *dbq-match-height* 1)))
                (if (and *dbq-title* (/= *dbq-title* ""))
                  (progn
                    (setq dbq:ts (strcat *dbq-title* " 共" (itoa (length *dbq-data*)) "个")
                          dbq:th 12
                          dbq:twf 1.0)
                    (setq dbq:tb (textbox (list (cons 40 dbq:th) (cons 41 dbq:twf) (cons 1 dbq:ts)))
                          dbq:tw (- (caadr dbq:tb) (caar dbq:tb)))
                    (if (> dbq:tw 297)
                      (progn
                        (setq dbq:me (entmakex (list (cons 0 "MTEXT") (cons 100 "AcDbEntity") (cons 62 1)
                                                     (cons 100 "AcDbMText")
                                                     (cons 10 (list (car sp) (+ (cadr sp) 10) 0.0))
                                                     (cons 40 dbq:th) (cons 1 dbq:ts) (cons 41 297.0)
                                                     (cons 71 1) (cons 72 1))))
                        (setq dbq:vo (vlax-ename->vla-object dbq:me))
                        (vla-getboundingbox dbq:vo 'dbq:pn 'dbq:px)
                        (setq dbq:pn (vlax-safearray->list dbq:pn)
                              dbq:px (vlax-safearray->list dbq:px)
                              dbq:mh (- (cadr dbq:px) (cadr dbq:pn)))
                        (vla-move dbq:vo
                          (vlax-3d-point 0 0 0)
                          (vlax-3d-point 0 dbq:mh 0))
                        (vlax-release-object dbq:vo)
                      )
                      (entmakex (list (cons 0 "TEXT") (cons 100 "AcDbEntity") (cons 62 1)
                                     (cons 100 "AcDbText")
                                     (cons 10 (list (car sp) (+ (cadr sp) 10) 0.0))
                                     (cons 40 dbq:th) (cons 1 dbq:ts) (cons 41 dbq:twf)
                                     (cons 72 0) (cons 73 0)))
                    )
                  )
                )
                (princ (strcat "\n[OK] 排版完成！共 "
                               (itoa (length *dbq-data*))
                               " 个标签，"
                               (itoa ngrp)
                               " 组（A4页）。"))
              )
            )
          )
        )
        (setvar "CMDECHO" old-echo)
        (setq ret 0)  ;; 退出循环
      )
      ((= ret 2)
        ;; 写回TXT模式
        (setq _target
          (if (and *dbq-path* (/= *dbq-path* ""))
            *dbq-path*
            ;; 未指定 TXT 文件时，在临时空间创建一个临时文件
            (strcat (getenv "TEMP") "\\dbq_temp_"
                    (itoa (fix (* 1e6 (- (getvar "DATE") (fix (getvar "DATE"))))))
                    ".txt")))
        (if _target
          (progn
            (cond
              ((= *dbq-smart-merge* 1)
               (setq _lines (dbq:get-selected-text-lines-smart-merge)))
              ((= *dbq-smart-merge-row* 1)
               (setq _lines (dbq:get-selected-text-lines-smart-merge-row)))
              (t
               (setq _lines (dbq:get-selected-text-lines)))
            )
            (if (and _lines (> (length _lines) 0))
               (if (dbq:write-txt _target _lines *dbq-append*)
                  (progn
                    (if (= *dbq-append* 1)
                      (setq *dbq-backup-data* (mapcar '(lambda (x) x) *dbq-data*))
                    )
                    (setq *dbq-path* _target
                          *dbq-data* (if (= *dbq-append* 1)
                                      (append *dbq-data* _lines)
                                      _lines))
                  (princ (strcat "\n[OK] 已写回TXT：" _target "，共 " (itoa (length _lines)) " 行。"))
                )
              )
              (alert "未选择任何可写回的文字对象（TEXT/MTEXT）。")
            )
          )
        )
        ;; ret 保持 2，继续循环打开对话框
      )
    )
  )

  (princ)
)

;;; ─────────────────────────────────────────────────────────
;;; DBV ― 写回 TXT + 立即排版（一键完成，不显示主界面）
;;; ─────────────────────────────────────────────────────────
(defun c:DBV (/ old-echo _target _lines sp stats ngrp *error*)
  (dbq:init)
  (setq old-echo (getvar "CMDECHO"))
  (defun *error* (msg)
    (if old-echo (setvar "CMDECHO" old-echo))
    (if (and msg (/= msg "Function cancelled") (/= msg "quit / exit abort"))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )
  ;; ── Step 1: 写回TXT ──
  (setq _target
    (if (and *dbq-path* (/= *dbq-path* ""))
      *dbq-path*
      (strcat (getenv "TEMP") "\\dbq_temp_"
              (itoa (fix (* 1e6 (- (getvar "DATE") (fix (getvar "DATE"))))))
              ".txt")))
  (if _target
    (progn
      (cond
        ((= *dbq-smart-merge* 1)
         (setq _lines (dbq:get-selected-text-lines-smart-merge)))
        ((= *dbq-smart-merge-row* 1)
         (setq _lines (dbq:get-selected-text-lines-smart-merge-row)))
        (t
         (setq _lines (dbq:get-selected-text-lines)))
      )
      (if (and _lines (> (length _lines) 0))
        (if (dbq:write-txt _target _lines *dbq-append*)
           (progn
             (setq *dbq-path* _target
                   *dbq-data* (if (= *dbq-append* 1)
                               (append *dbq-data* _lines)
                               _lines))
            (princ (strcat "\n[OK] 已写回TXT：" _target "，共 " (itoa (length _lines)) " 行。"))
          )
        )
        (alert "未选择任何可写回的文字对象（TEXT/MTEXT）。")
      )
    )
  )
  ;; ── Step 2: 排版（对应主界面确定按钮） ──
  (setvar "CMDECHO" 0)
  (initget 1)
  (if (= *dbq-summary-only* 1)
    (progn
      (setq sp (getpoint "\n请在CAD中点击分类汇总左上角起点："))
      (if sp
        (progn
          (setq sp (trans sp 1 0))
          (setq stats (dbq:classify-labels *dbq-data*))
          (dbq:draw-summary (car sp) (cadr sp) stats (= *dbq-match-height* 1))
          (princ (strcat "\n[OK] 分类汇总完成！共 " (itoa (length *dbq-data*)) " 个标签。"))
        )
      )
    )
    (progn
      (setq sp (getpoint "\n请在CAD中点击标签阵列左上角起点："))
      (if sp
        (progn
          (setq sp (trans sp 1 0))
          (setq ngrp (dbq:layout sp (= *dbq-match-height* 1)))
          (if (and *dbq-title* (/= *dbq-title* ""))
            (progn
              (setq dbq:ts (strcat *dbq-title* " 共" (itoa (length *dbq-data*)) "个")
                    dbq:th 12
                    dbq:twf 1.0)
              (setq dbq:tb (textbox (list (cons 40 dbq:th) (cons 41 dbq:twf) (cons 1 dbq:ts)))
                    dbq:tw (- (caadr dbq:tb) (caar dbq:tb)))
              (if (> dbq:tw 297)
                (progn
                  (setq dbq:me (entmakex (list (cons 0 "MTEXT") (cons 100 "AcDbEntity") (cons 62 1)
                                               (cons 100 "AcDbMText")
                                               (cons 10 (list (car sp) (+ (cadr sp) 10) 0.0))
                                               (cons 40 dbq:th) (cons 1 dbq:ts) (cons 41 297.0)
                                               (cons 71 1) (cons 72 1))))
                  (setq dbq:vo (vlax-ename->vla-object dbq:me))
                  (vla-getboundingbox dbq:vo 'dbq:pn 'dbq:px)
                  (setq dbq:pn (vlax-safearray->list dbq:pn)
                        dbq:px (vlax-safearray->list dbq:px)
                        dbq:mh (- (cadr dbq:px) (cadr dbq:pn)))
                  (vla-move dbq:vo
                    (vlax-3d-point 0 0 0)
                    (vlax-3d-point 0 dbq:mh 0))
                  (vlax-release-object dbq:vo)
                )
                (entmakex (list (cons 0 "TEXT") (cons 100 "AcDbEntity") (cons 62 1)
                               (cons 100 "AcDbText")
                               (cons 10 (list (car sp) (+ (cadr sp) 10) 0.0))
                               (cons 40 dbq:th) (cons 1 dbq:ts) (cons 41 dbq:twf)
                               (cons 72 0) (cons 73 0)))
              )
            )
          )
          (princ (strcat "\n[OK] 排版完成！共 "
                         (itoa (length *dbq-data*))
                         " 个标签，"
                         (itoa ngrp)
                         " 组（A4页）。"))
        )
      )
    )
  )
  (setvar "CMDECHO" old-echo)
  (princ)
)

;;; ─────────────────────────────────────────────────────────
;;; 配置管理 ― 辅助函数
;;; ─────────────────────────────────────────────────────────

(defun dbq:cfg-file ()
  (strcat (getenv "TEMP") "\\dbq_configs.dat")
)

;; 加载配置列表
(defun dbq:load-configs (/ fn fp line lst parts)
  (setq lst nil)
  (if (setq fp (open (dbq:cfg-file) "r"))
    (progn
      (while (setq line (read-line fp))
        ;; 按 | 拆分
        (setq parts nil)
        (while (vl-string-search "|" line)
          (setq parts (append parts (list (substr line 1 (vl-string-search "|" line))))
                line (substr line (+ (vl-string-search "|" line) 2))
          )
        )
        (setq parts (append parts (list line)))
        ;; parts = (name iw ih ns cb th hg vg)
        (cond
          ;; 最新版：11字段 (name iw ih ow oh ns cb th hg vg title)
          ((>= (length parts) 11)
           (setq lst (append lst (list (list (nth 0 parts)
                                             (atoi (nth 1 parts))
                                             (atoi (nth 2 parts))
                                             (atoi (nth 3 parts))
                                             (atoi (nth 4 parts))
                                             (atoi (nth 5 parts))
                                             (atof (nth 6 parts))
                                             (atoi (nth 7 parts))
                                             (atoi (nth 8 parts))
                                             (atoi (nth 9 parts))
                                             (nth 10 parts)
                                       )))
           )
          )
          ;; 10字段版 (name iw ih ow oh ns cb th hg vg)，补title=""
          ((>= (length parts) 10)
           (setq lst (append lst (list (list (nth 0 parts)
                                             (atoi (nth 1 parts))
                                             (atoi (nth 2 parts))
                                             (atoi (nth 3 parts))
                                             (atoi (nth 4 parts))
                                             (atoi (nth 5 parts))
                                             (atof (nth 6 parts))
                                             (atoi (nth 7 parts))
                                             (atoi (nth 8 parts))
                                             (atoi (nth 9 parts))
                                             ""
                                       )))
           )
          )
          ;; 中版：8字段 (name iw ih ns cb th hg vg)，补ow/oh=iw+2, ih+2
          ((>= (length parts) 8)
           (setq lst (append lst (list (list (nth 0 parts)
                                             (atoi (nth 1 parts))
                                             (atoi (nth 2 parts))
                                             (+ (atoi (nth 1 parts)) 2)  ; ow
                                             (+ (atoi (nth 2 parts)) 2)  ; oh
                                             (atoi (nth 3 parts))
                                             (atof (nth 4 parts))
                                             (atoi (nth 5 parts))
                                             (atoi (nth 6 parts))
                                             (atoi (nth 7 parts))
                                             ""
                                       )))
           )
          )
          ;; 旧版：6字段，补ow/oh、间距7、title=""
          ((>= (length parts) 6)
           (setq lst (append lst (list (list (nth 0 parts)
                                             (atoi (nth 1 parts))
                                             (atoi (nth 2 parts))
                                             (+ (atoi (nth 1 parts)) 2)  ; ow
                                             (+ (atoi (nth 2 parts)) 2)  ; oh
                                             (atoi (nth 3 parts))
                                             (atof (nth 4 parts))
                                             (atoi (nth 5 parts))
                                             7  ; hg
                                             7  ; vg
                                             ""
                                       )))
           )
          )
        )
      )
      (close fp)
    )
  )
  lst
)

;; 保存配置列表
(defun dbq:save-configs (cfgs / fn fp)
  (setq fp (open (dbq:cfg-file) "w"))
  (foreach c cfgs
    (write-line
      (strcat (car c) "|"
              (itoa (cadr c)) "|"   ; iw
              (itoa (caddr c)) "|"  ; ih
              (itoa (cadddr c)) "|" ; ow
              (itoa (nth 4 c)) "|"  ; oh
              (itoa (nth 5 c)) "|"  ; ns
              (rtos (nth 6 c) 2 1) "|"  ; cb
              (itoa (nth 7 c)) "|"  ; th
              (itoa (nth 8 c)) "|"  ; hg
              (itoa (nth 9 c)) "|"  ; vg
              (nth 10 c)            ; title
      )
      fp
    )
  )
  (close fp)
)

;; 更新（同名覆盖）或添加配置
(defun dbq:update-config (cfgs cfg / name found rest)
  (setq name (car cfg) found nil rest nil)
  (foreach c cfgs
    (if (= (car c) name)
      (setq found t rest (append rest (list cfg)))
      (setq rest (append rest (list c)))
    )
  )
  (if (not found)
    (setq rest (append rest (list cfg)))
  )
  rest
)

;; 按索引删除配置
(defun dbq:remove-config (cfgs idx / i rest)
  (setq i 0 rest nil)
  (foreach c cfgs
    (if (/= i idx)
      (setq rest (append rest (list c)))
    )
    (setq i (1+ i))
  )
  rest
)

;; 查找配置名对应的索引
(defun dbq:find-config (cfgs name / i result)
  (setq i 0 result -1)
  (foreach c cfgs
    (if (= (car c) name) (setq result i))
    (setq i (1+ i))
  )
  result
)

;; 刷新下拉框
(defun dbq:populate-cfglist (/)
  (start_list "e_cfglist")
  (if *dbq-configs*
    (foreach c *dbq-configs* (add_list (car c)))
    (add_list "")
  )
  (end_list)
)

;; 将下拉框选中配置的数值填入各文本框
(defun dbq:apply-config (idx / cfg)
  (if (and *dbq-configs* (>= idx 0) (< idx (length *dbq-configs*)))
    (progn
      (setq cfg (nth idx *dbq-configs*))
      (set_tile "e_cfgname" (car cfg))
      (set_tile "e_iw" (itoa (nth 1 cfg)))
      (set_tile "e_ih" (itoa (nth 2 cfg)))
      (set_tile "e_ow" (itoa (nth 3 cfg)))
      (set_tile "e_oh" (itoa (nth 4 cfg)))
      (set_tile "e_ns" (itoa (nth 5 cfg)))
      (set_tile "e_cb" (rtos (nth 6 cfg) 2 1))
      (set_tile "e_th" (itoa (nth 7 cfg)))
      (set_tile "e_hg" (itoa (nth 8 cfg)))
      (set_tile "e_vg" (itoa (nth 9 cfg)))
      (set_tile "e_title" (nth 10 cfg))
      (dbq:update-preview)
    )
  )
)

;;; ─────────────────────────────────────────────────────────
;;; 复选框状态长久保存
;;; ─────────────────────────────────────────────────────────
(defun dbq:state-file ()
  (strcat (getenv "TEMP") "\\dbq_state.dat")
)

(defun dbq:load-state (/ fn fp line p key val)
  (if (not (boundp '*dbq-summary-only*)) (setq *dbq-summary-only* 0))
  (if (not (boundp '*dbq-match-height*)) (setq *dbq-match-height* 0))
  (if (not (boundp '*dbq-smart-merge*)) (setq *dbq-smart-merge* 0))
  (if (not (boundp '*dbq-smart-merge-row*)) (setq *dbq-smart-merge-row* 0))
  (if (not (boundp '*dbq-append*)) (setq *dbq-append* 0))
  (if (not (boundp '*dbq-merge-rows*)) (setq *dbq-merge-rows* 3))
  (if (not (and (boundp '*dbq-rowtol*) *dbq-rowtol* (= (type *dbq-rowtol*) 'REAL))) (setq *dbq-rowtol* 0.5))
  (setq fp (open (dbq:state-file) "r"))
  (if fp
    (progn
      (while (setq line (read-line fp))
        (setq p (vl-string-search "=" line))
        (if p
          (progn
            (setq key (substr line 1 p)
                  val (substr line (+ p 2))
            )
            (cond
              ((= key "summary_only") (setq *dbq-summary-only* (atoi val)))
              ((= key "match_height") (setq *dbq-match-height* (atoi val)))
              ((= key "smart_merge") (setq *dbq-smart-merge* (atoi val)))
              ((= key "smart_merge_row") (setq *dbq-smart-merge-row* (atoi val)))
              ((= key "append") (setq *dbq-append* (atoi val)))
              ((= key "merge_rows") (setq *dbq-merge-rows* (max 2 (atoi val))))
              ((= key "rowtol") (setq *dbq-rowtol* (max 0.1 (min 2.0 (atof val)))))
            )
          )
        )
      )
      (close fp)
    )
  )
)

(defun dbq:save-state (/ fp)
  (setq fp (open (dbq:state-file) "w"))
  (if fp
    (progn
      (write-line (strcat "summary_only=" (itoa (if (boundp '*dbq-summary-only*) *dbq-summary-only* 0))) fp)
      (write-line (strcat "match_height=" (itoa (if (boundp '*dbq-match-height*) *dbq-match-height* 0))) fp)
      (write-line (strcat "smart_merge=" (itoa (if (boundp '*dbq-smart-merge*) *dbq-smart-merge* 0))) fp)
      (write-line (strcat "smart_merge_row=" (itoa (if (boundp '*dbq-smart-merge-row*) *dbq-smart-merge-row* 0))) fp)
      (write-line (strcat "append=" (itoa (if (boundp '*dbq-append*) *dbq-append* 0))) fp)
      (write-line (strcat "merge_rows=" (itoa (if (boundp '*dbq-merge-rows*) *dbq-merge-rows* 3))) fp)
      (write-line (strcat "rowtol=" (rtos (if (and (boundp '*dbq-rowtol*) *dbq-rowtol* (= (type *dbq-rowtol*) 'REAL)) *dbq-rowtol* 0.5) 2 2)) fp)
      (close fp)
    )
  )
)

;;; ─────────────────────────────────────────────────────────
(princ "\n[ DBQ ] 标签排版系统已加载。输入 DBQ 启动程序。\n")
(princ)
;;; ─────────────────────────────────────────────────────────
;;; END OF DBQ.LSP
;;; ─────────────────────────────────────────────────────────
