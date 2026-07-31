;@name 查找定位文本
;@group 文本编辑
;@desc 查找图纸中的文字并快速定位。支持全图查找和框选范围查找，点击结果可缩放定位到文字位置(约1/30屏)，按空格返回列表，按ESC退出。全图查找支持缓存加速，支持从剪贴板批量导入关键词。支持自定义标记：定位时按数字键给关键词打标记，并可按标记统计和复制关键词
;@require ModelSpace
;@require Selection

(vl-load-com)

;; ---------------- 全局变量 ----------------
(setq *wtf-dcl-file* (strcat (getenv "TEMP") "\\wtf_dialog.dcl"))
;; 搜索结果列表，每项: (文字内容 X Y Z 实体名)
(setq *wtf-results* nil)
;; 上次搜索关键词
(setq *wtf-last-search* "")
;; 全图文字缓存列表(每项格式同 *wtf-results*: 文字内容 X Y Z 实体名)
(setq *wtf-cache* nil)
;; 缓存是否有效
(setq *wtf-cache-valid* nil)
;; 批量关键词列表(每项为字符串)
(setq *wtf-keyword-list* nil)
;; 关键词到最后选择匹配项句柄的映射(关联列表: (("关键词" . "句柄") ...))
(setq *wtf-last-selection* nil)
;; 关键词列表滚动位置(上次选中索引，用于恢复滚动位置)
(setq *wtf-keyword-top-idx* "")
;; 标记定义列表(每项为标记名称字符串，最多9个)
(setq *wtf-mark-defs* nil)
;; 标记功能开关("1"=启用 "0"=关闭)
(setq *wtf-mark-enabled* "0")
;; 关键词到标记索引列表的映射(关联列表: (("关键词" 1 2) ...)，cdr为标记序号列表)
(setq *wtf-keyword-marks* nil)
;; 标记统计窗口当前显示的关键词列表
(setq *wtf-stat-current* nil)
;; 标记统计窗口上次显示的标记序号(用于数量定义后刷新列表)
(setq *wtf-stat-last-idx* nil)
;; 关键词到数量的映射(关联列表: (("关键词" . "数量") ...)，由数量定义功能写入)
(setq *wtf-keyword-qty* nil)

;; ============================================================
;;  生成 DCL 对话框
;; ============================================================
(defun wtf:write-dcl ( / f idx cnt)
  (if (findfile *wtf-dcl-file*)
    (vl-file-delete *wtf-dcl-file*)
  )
  (setq f (open *wtf-dcl-file* "w"))

  (write-line "wtf_dialog : dialog {" f)
  (write-line "  label = \"查找定位文本\";" f)
  (write-line "  : row {" f)
  ;; ---- 左侧: 批量关键词列表区 ----
  (write-line "    : column {" f)
  (write-line "      : button {" f)
  (write-line "        key = \"btn_add_keywords\";" f)
  (write-line "        label = \"批量添加(剪贴板)\";" f)
  (write-line "        width = 20;" f)
  (write-line "      }" f)
  (write-line "      : button {" f)
  (write-line "        key = \"btn_clear_data\";" f)
  (write-line "        label = \"清除缓存数据\";" f)
  (write-line "        width = 20;" f)
  (write-line "      }" f)
  (write-line "      : row {" f)
  (write-line "        : button { key = \"btn_mark_def\"; label = \"标记\"; width = 9; }" f)
  (write-line "        : button { key = \"btn_mark_stat\"; label = \"标记统计\"; width = 9; }" f)
  (write-line "      }" f)
  (write-line "      : toggle {" f)
  (write-line "        key = \"tg_mark_enable\";" f)
  (write-line "        label = \"启用标记\";" f)
  (write-line "      }" f)
  (write-line "      : list_box {" f)
  (write-line "        key = \"keyword_list\";" f)
  (write-line "        label = \"关键词列表 (点击查找):\";" f)
  (write-line "        height = 18;" f)
  (write-line "        width = 20;" f)
  (write-line "      }" f)
  (write-line "    }" f)
  ;; ---- 右侧: 查找区域 ----
  (write-line "    : column {" f)
  (write-line "      : edit_box {" f)
  (write-line "        key = \"search_text\";" f)
  (write-line "        label = \"查找内容:\";" f)
  (write-line "        edit_width = 38;" f)
  (write-line "        edit_limit = 200;" f)
  (write-line "      }" f)
  (write-line "      : row {" f)
  (write-line "        : radio_button { key = \"rb_all\"; label = \"全图查找\"; }" f)
  (write-line "        : radio_button { key = \"rb_window\"; label = \"框选范围查找\"; }" f)
  (write-line "      }" f)
  (write-line "      : button {" f)
  (write-line "        key = \"btn_search\";" f)
  (write-line "        label = \"查找\";" f)
  (write-line "        width = 12;" f)
  (write-line "        is_default = true;" f)
  (write-line "        alignment = centered;" f)
  (write-line "      }" f)
  (write-line "      : list_box {" f)
  (write-line "        key = \"result_list\";" f)
  (write-line "        label = \"结果列表 (点击项可定位):\";" f)
  (write-line "        height = 15;" f)
  (write-line "        width = 55;" f)
  (write-line "      }" f)
  (write-line "      : text {" f)
  (write-line "        key = \"status\";" f)
  (write-line "        label = \"\";" f)
  (write-line "        alignment = centered;" f)
  (write-line "      }" f)
  (write-line "      spacer_1;" f)
  (write-line "      : row {" f)
  (write-line "        : button { key = \"btn_locate\"; label = \"定位\"; width = 12; }" f)
  (write-line "        : button { key = \"btn_exit\"; label = \"退出\"; width = 12; is_cancel = true; }" f)
  (write-line "      }" f)
  (write-line "    }" f)
  (write-line "  }" f)
  (write-line "}" f)

  ;; ---- 标记定义对话框 ----
  (write-line "wtf_mark_def : dialog {" f)
  (write-line "  label = \"定义标记\";" f)
  (write-line "  : edit_box {" f)
  (write-line "    key = \"mark_input\";" f)
  (write-line "    label = \"标记(用&分隔):\";" f)
  (write-line "    edit_width = 40;" f)
  (write-line "    edit_limit = 500;" f)
  (write-line "  }" f)
  (write-line "  : text { label = \"示例: 重要&待核对&已完成 (最多35个，快捷键1-9和A-Z)\"; }" f)
  (write-line "  ok_cancel;" f)
  (write-line "}" f)

  ;; ---- 标记统计对话框(按钮按当前标记定义动态生成) ----
  (write-line "wtf_mark_stat : dialog {" f)
  (write-line "  label = \"标记统计\";" f)
  (if *wtf-mark-defs*
    (progn
      (setq idx 1)
      (setq cnt 0)
      (foreach mk *wtf-mark-defs*
        (if (= cnt 0) (write-line "  : row {" f))
        (write-line (strcat "    : button { key = \"stat_btn_" (itoa idx)
                            "\"; label = \"" (wtf:idx->key idx) "." mk "\"; width = 14; }") f)
        (setq cnt (1+ cnt))
        (if (= cnt 3)
          (progn (write-line "  }" f) (setq cnt 0)))
        (setq idx (1+ idx))
      )
      (if (/= cnt 0) (write-line "  }" f))
    )
    (write-line "  : text { label = \"尚未定义标记，请先点击【标记】按钮定义\"; }" f)
  )
  (write-line "  : list_box {" f)
  (write-line "    key = \"stat_list\";" f)
  (write-line "    label = \"含此标记的关键词:\";" f)
  (write-line "    height = 15;" f)
  (write-line "    width = 44;" f)
  (write-line "  }" f)
  (write-line "  : text { key = \"stat_status\"; label = \"\"; width = 44; }" f)
  (write-line "  : row {" f)
  (write-line "    : button { key = \"btn_qty_def\"; label = \"数量定义\"; width = 12; }" f)
  (write-line "    : button { key = \"btn_stat_copy\"; label = \"复制\"; width = 12; }" f)
  (write-line "    : button { key = \"btn_stat_close\"; label = \"关闭\"; width = 12; is_cancel = true; }" f)
  (write-line "  }" f)
  (write-line "}" f)

  ;; ---- 数量定义核对对话框 ----
  (write-line "wtf_qty_confirm : dialog {" f)
  (write-line "  label = \"数量对应核对\";" f)
  (write-line "  : list_box {" f)
  (write-line "    key = \"qty_list\";" f)
  (write-line "    label = \"关键词与数量对应关系(请核实):\";" f)
  (write-line "    height = 20;" f)
  (write-line "    width = 50;" f)
  (write-line "  }" f)
  (write-line "  : text { key = \"qty_status\"; label = \"\"; width = 50; }" f)
  (write-line "  ok_cancel;" f)
  (write-line "}" f)

  (close f)
  (princ)
)

;; ============================================================
;;  辅助函数
;; ============================================================

;; 获取文字内容
(defun wtf:get-text-content (ent / ed etype)
  (setq ed (entget ent))
  (setq etype (cdr (assoc 0 ed)))
  (cond
    ((= etype "TEXT") (cdr (assoc 1 ed)))
    ((= etype "MTEXT") (wtf:clean-mtext (cdr (assoc 1 ed))))
    (t "")
  )
)

;; 简单清理 MTEXT 格式码 (换行符等)
(defun wtf:clean-mtext (str)
  (while (vl-string-search "\\P" str)
    (setq str (vl-string-subst " " "\\P" str))
  )
  (while (vl-string-search "\\p" str)
    (setq str (vl-string-subst " " "\\p" str))
  )
  (while (vl-string-search "\\~" str)
    (setq str (vl-string-subst " " "\\~" str))
  )
  str
)

;; 生成列表显示行
(defun wtf:make-display-line (item / content x y)
  (setq content (nth 0 item))
  (setq x (nth 1 item))
  (setq y (nth 2 item))
  (if (> (strlen content) 45)
    (setq content (strcat (substr content 1 42) "..."))
  )
  (strcat content "  |  (" (rtos x 2 1) ", " (rtos y 2 1) ")")
)

;; 更新列表框显示
(defun wtf:update-list ( / )
  (start_list "result_list")
  (foreach item *wtf-results*
    (add_list (wtf:make-display-line item))
  )
  (end_list)
)

;; 更新状态栏
(defun wtf:update-status ( / cnt)
  (setq cnt (length *wtf-results*))
  (if (= cnt 0)
    (set_tile "status" "暂无结果，请输入关键词后点击查找")
    (set_tile "status" (strcat "共找到 " (itoa cnt) " 个结果"))
  )
)

;; ============================================================
;;  剪贴板读取 (通过 htmlfile COM 对象)
;;  返回剪贴板文本字符串，失败返回空字符串
;; ============================================================
(defun wtf:get-clipboard ( / htmlfile parent clip result)
  (setq result "")
  (setq htmlfile (vl-catch-all-apply 'vlax-create-object (list "htmlfile")))
  (if (not (vl-catch-all-error-p htmlfile))
    (progn
      (vl-catch-all-apply
        '(lambda ()
           (setq parent (vlax-get-property htmlfile 'ParentWindow))
           (setq clip (vlax-get-property parent 'ClipBoardData))
           (setq result (vlax-invoke clip 'GetData "Text"))))
      (vl-catch-all-apply 'vlax-release-object (list htmlfile))
    )
  )
  (if (null result) (setq result ""))
  result
)

;; ============================================================
;;  剪贴板写入 (通过 htmlfile COM 对象)
;;  成功返回 T，失败返回 nil
;; ============================================================
(defun wtf:set-clipboard (str / htmlfile parent clip ok)
  (setq ok nil)
  (setq htmlfile (vl-catch-all-apply 'vlax-create-object (list "htmlfile")))
  (if (not (vl-catch-all-error-p htmlfile))
    (progn
      (vl-catch-all-apply
        '(lambda ()
           (setq parent (vlax-get-property htmlfile 'ParentWindow))
           (setq clip (vlax-get-property parent 'ClipBoardData))
           (vlax-invoke clip 'SetData "Text" str)
           (setq ok T)))
      (vl-catch-all-apply 'vlax-release-object (list htmlfile))
    )
  )
  ok
)

;; ============================================================
;;  字符串分割/连接工具
;; ============================================================
(defun wtf:split-string (str delim / pos lst token)
  (setq lst nil)
  (if (and str (/= str ""))
    (progn
      (while (setq pos (vl-string-search delim str))
        (setq token (substr str 1 pos))
        (if (/= token "") (setq lst (append lst (list token))))
        (setq str (substr str (+ pos 1 (strlen delim))))
      )
      (if (/= str "") (setq lst (append lst (list str))))
    )
  )
  lst
)

(defun wtf:join-strings (lst delim / result)
  (setq result "")
  (foreach s lst
    (if (= result "")
      (setq result s)
      (setq result (strcat result delim s))
    )
  )
  result
)

;; 去除字符串首尾空白(半角空格、全角空格、制表符)
;; 防止重定义标记时多打空格导致名称不匹配、标记迁移丢失
(defun wtf:trim-string (str)
  (vl-string-trim " 	　" str)
)

;; 按换行符分割文本为行列表(忽略空行，兼容\r\n和\n)
(defun wtf:split-lines (text / lines pos start ch)
  (setq lines nil)
  (if (and text (/= text ""))
    (progn
      (setq pos 1)
      (setq start 1)
      (while (<= pos (strlen text))
        (setq ch (substr text pos 1))
        (if (or (= ch "\n") (= ch "\r"))
          (progn
            (if (> pos start)
              (setq lines (append lines (list (substr text start (- pos start)))))
            )
            (while (and (<= pos (strlen text))
                        (or (= (substr text pos 1) "\n")
                            (= (substr text pos 1) "\r")))
              (setq pos (1+ pos))
            )
            (setq start pos)
          )
          (setq pos (1+ pos))
        )
      )
      (if (<= start (strlen text))
        (setq lines (append lines (list (substr text start))))
      )
    )
  )
  lines
)

;; ============================================================
;;  关键词标记读写
;;  标记映射每项: ("关键词" 1 2)，cdr 为标记序号列表
;; ============================================================
(defun wtf:get-keyword-marks (keyword / pair)
  (setq pair (assoc keyword *wtf-keyword-marks*))
  (if pair (cdr pair) nil)
)

(defun wtf:set-keyword-marks (keyword idx-list / filtered)
  (setq filtered
    (vl-remove-if '(lambda (pair) (= (car pair) keyword)) *wtf-keyword-marks*))
  (if idx-list
    (setq *wtf-keyword-marks* (append filtered (list (cons keyword idx-list))))
    (setq *wtf-keyword-marks* filtered)
  )
)

;; 标记序号列表 -> 标记名称字符串(逗号分隔)
(defun wtf:mark-names (idx-list / names mk)
  (setq names nil)
  (foreach i idx-list
    (setq mk (nth (1- i) *wtf-mark-defs*))
    (if mk (setq names (append names (list mk))))
  )
  (wtf:join-strings names ",")
)

;; ============================================================
;;  标记快捷键映射: 序号1-9对应数字键，序号10-35对应字母A-Z
;; ============================================================
;; 标记序号 -> 快捷键字符(1-9、A-Z)
(defun wtf:idx->key (n)
  (if (<= n 9)
    (itoa n)
    (chr (+ 65 (- n 10)))
  )
)

;; 快捷键字符 -> 标记序号(大小写不敏感)，无效返回 nil
(defun wtf:key->idx (ch / code)
  (setq ch (strcase ch))
  (setq code (ascii ch))
  (cond
    ((and (>= code 49) (<= code 57)) (- code 48))   ;; 1-9
    ((and (>= code 65) (<= code 90)) (+ 10 (- code 65)))  ;; A-Z -> 10-35
    (t nil)
  )
)

;; ============================================================
;;  解析定位时输入的快捷键序列并应用标记
;;  字符: 数字1-9、字母A-Z对应标记序号
;;  规则: 以最后一次输入为准整体替换; 含0则清除该关键词标记
;; ============================================================
(defun wtf:apply-mark-input (keyword digits / i ch n new-marks has-zero)
  (setq new-marks nil)
  (setq has-zero nil)
  (setq i 1)
  (while (<= i (strlen digits))
    (setq ch (substr digits i 1))
    (setq n (wtf:key->idx ch))
    (cond
      ((= ch "0") (setq has-zero T))
      ((and n (>= n 1) (<= n (length *wtf-mark-defs*)))
        (if (not (member n new-marks))
          (setq new-marks (append new-marks (list n))))
      )
    )
    (setq i (1+ i))
  )
  (cond
    (has-zero
      (wtf:set-keyword-marks keyword nil)
      (princ (strcat "\n[WTF] 已清除【" keyword "】的标记"))
    )
    (new-marks
      (wtf:set-keyword-marks keyword new-marks)
      (princ (strcat "\n[WTF] 【" keyword "】标记为: " (wtf:mark-names new-marks)))
    )
    (t (princ "\n[WTF] 输入的快捷键无对应标记，保持原标记"))
  )
  (wtf:save-data)
)

;; ============================================================
;;  从剪贴板批量加载关键词
;;  按行分割，每行一个关键词，忽略空行
;;  结果存入全局变量 *wtf-keyword-list*
;; ============================================================
(defun wtf:load-keywords-from-clipboard ( / clip-text lines pos start ch)
  (setq clip-text (wtf:get-clipboard))
  (setq *wtf-keyword-list* nil)
  (setq lines '())

  (if (and clip-text (/= clip-text ""))
    (progn
      (setq pos 1)
      (setq start 1)
      (while (<= pos (strlen clip-text))
        (setq ch (substr clip-text pos 1))
        (if (or (= ch "\n") (= ch "\r"))
          (progn
            ;; 提取当前行(非空)
            (if (> pos start)
              (setq lines (append lines (list (substr clip-text start (- pos start)))))
            )
            ;; 跳过连续换行符
            (while (and (<= pos (strlen clip-text))
                        (or (= (substr clip-text pos 1) "\n")
                            (= (substr clip-text pos 1) "\r")))
              (setq pos (1+ pos))
            )
            (setq start pos)
          )
          (setq pos (1+ pos))
        )
      )
      ;; 最后一个关键词(无换行符结尾)
      (if (<= start (strlen clip-text))
        (setq lines (append lines (list (substr clip-text start))))
      )
      (setq *wtf-keyword-list* lines)
    )
  )

  (princ (strcat "\n[WTF] 从剪贴板加载了 " (itoa (length *wtf-keyword-list*)) " 个关键词"))
  *wtf-keyword-list*
)

;; ============================================================
;;  更新关键词列表显示
;; ============================================================
(defun wtf:update-keyword-list ( / idx kw-idx half-vis scroll-idx kw-len mks line)
  (setq idx 1)
  (start_list "keyword_list")
  (foreach kw *wtf-keyword-list*
    (setq line (strcat (itoa idx) ". " kw))
    ;; 附加已打标记名称
    (setq mks (wtf:get-keyword-marks kw))
    (if mks (setq line (strcat line " [" (wtf:mark-names mks) "]")))
    (add_list line)
    (setq idx (1+ idx))
  )
  (end_list)
  ;; 恢复关键词列表滚动位置(居中显示)
  (setq kw-len (length *wtf-keyword-list*))
  (if (and *wtf-keyword-top-idx*
           (/= *wtf-keyword-top-idx* "")
           *wtf-keyword-list*
           (< (atoi *wtf-keyword-top-idx*) kw-len))
    (progn
      (setq kw-idx (atoi *wtf-keyword-top-idx*))
      (setq half-vis 9)  ;; keyword_list height=18, 半屏=9
      ;; 计算滚动锚点: 目标项+半屏，选中此项使列表向下滚动(锚点出现在可视区底部)
      (setq scroll-idx (+ kw-idx half-vis))
      ;; 边界检查: 不超过列表末尾
      (if (>= scroll-idx kw-len)
        (setq scroll-idx (1- kw-len)))
      ;; 第一步: 选中锚点项，触发列表向下滚动
      (if (and (/= scroll-idx kw-idx) (>= scroll-idx 0) (< scroll-idx kw-len))
        (set_tile "keyword_list" (itoa scroll-idx)))
      ;; 第二步: 选中目标项(此时目标项已在可见区域居中位置)
      (set_tile "keyword_list" (itoa kw-idx))
    )
  )
)

;; ============================================================
;;  保存关键词列表当前选中项索引(用于恢复滚动位置)
;; ============================================================
(defun wtf:save-keyword-pos ( / kw-str)
  (setq kw-str (get_tile "keyword_list"))
  (if (and kw-str (/= kw-str ""))
    (setq *wtf-keyword-top-idx* kw-str)
  )
)

;; ============================================================
;;  记录关键词的最后选择(保存实体句柄，跨会话稳定)
;;  句柄(组码5)随DWG持久化，不随ssget顺序变化而偏移
;; ============================================================
(defun wtf:save-selection (keyword ent / handle filtered)
  ;; 获取实体句柄(组码5，随DWG持久化)
  (setq handle (cdr (assoc 5 (entget ent))))
  (if handle
    (progn
      (setq filtered
        (vl-remove-if '(lambda (pair) (= (car pair) keyword)) *wtf-last-selection*))
      (setq *wtf-last-selection* (append filtered (list (cons keyword handle))))))
)

;; ============================================================
;;  获取关键词的最后选择匹配项句柄
;; ============================================================
(defun wtf:get-last-selection (keyword / pair)
  (setq pair (assoc keyword *wtf-last-selection*))
  (if pair (cdr pair) nil)
)

;; ============================================================
;;  在结果列表中按句柄查找匹配项
;;  返回匹配的 item，未找到返回 nil
;; ============================================================
(defun wtf:find-result-by-handle (handle / found item ent h)
  (setq found nil)
  (if (and handle (/= handle "") *wtf-results*)
    (foreach item *wtf-results*
      (if (not found)
        (progn
          (setq ent (nth 4 item))
          (if ent
            (progn
              (setq h (cdr (assoc 5 (entget ent))))
              (if (and h (= h handle))
                (setq found item))))))))
  found
)

;; ============================================================
;;  数据持久化 (存储到图纸字典，随DWG保存)
;; ============================================================

;; 获取或创建WTF数据字典
(defun wtf:get-data-dict ( / dictName dictEnt)
  (setq dictName "WTF_DATA")
  (setq dictEnt (dictsearch (namedobjdict) dictName))
  (if dictEnt
    (cdr (assoc -1 dictEnt))
    (progn
      (setq dictEnt (entmakex '((0 . "DICTIONARY") (100 . "AcDbDictionary"))))
      (if dictEnt
        (dictadd (namedobjdict) dictName dictEnt)
      )
    )
  )
)

;; 保存数据到图纸
(defun wtf:save-data ( / dict)
  (setq dict (wtf:get-data-dict))
  (if dict
    (progn
      (vlax-ldata-put dict "keyword-list" *wtf-keyword-list*)
      (vlax-ldata-put dict "last-selection" *wtf-last-selection*)
      (vlax-ldata-put dict "last-search" *wtf-last-search*)
      (vlax-ldata-put dict "keyword-top-idx" *wtf-keyword-top-idx*)
      (vlax-ldata-put dict "mark-defs" *wtf-mark-defs*)
      (vlax-ldata-put dict "mark-enabled" *wtf-mark-enabled*)
      (vlax-ldata-put dict "keyword-marks" *wtf-keyword-marks*)
      (vlax-ldata-put dict "keyword-qty" *wtf-keyword-qty*)
    )
  )
)

;; 从图纸加载数据
(defun wtf:load-data ( / dict)
  (setq dict (wtf:get-data-dict))
  (if dict
    (progn
      (setq *wtf-keyword-list* (vlax-ldata-get dict "keyword-list" nil))
      (setq *wtf-last-selection* (vlax-ldata-get dict "last-selection" nil))
      (setq *wtf-last-search* (vlax-ldata-get dict "last-search" ""))
      (setq *wtf-keyword-top-idx* (vlax-ldata-get dict "keyword-top-idx" ""))
      (setq *wtf-mark-defs* (vlax-ldata-get dict "mark-defs" nil))
      (setq *wtf-mark-enabled* (vlax-ldata-get dict "mark-enabled" "0"))
      (setq *wtf-keyword-marks* (vlax-ldata-get dict "keyword-marks" nil))
      (setq *wtf-keyword-qty* (vlax-ldata-get dict "keyword-qty" nil))
      (if (null *wtf-keyword-list*) (setq *wtf-keyword-list* nil))
      (if (null *wtf-last-selection*) (setq *wtf-last-selection* nil))
      (if (null *wtf-last-search*) (setq *wtf-last-search* ""))
      (if (null *wtf-keyword-top-idx*) (setq *wtf-keyword-top-idx* ""))
      (if (null *wtf-mark-enabled*) (setq *wtf-mark-enabled* "0"))
    )
  )
)

;; 清除图纸中保存的数据
(defun wtf:clear-data ( / )
  (dictremove (namedobjdict) "WTF_DATA")
  (setq *wtf-keyword-list* nil)
  (setq *wtf-last-selection* nil)
  (setq *wtf-last-search* "")
  (setq *wtf-keyword-top-idx* "")
  (setq *wtf-mark-defs* nil)
  (setq *wtf-mark-enabled* "0")
  (setq *wtf-keyword-marks* nil)
  (setq *wtf-keyword-qty* nil)
  (setq *wtf-cache* nil)
  (setq *wtf-cache-valid* nil)
)

;; ============================================================
;;  构建全图文字缓存
;;  遍历所有 TEXT/MTEXT，提取文字内容和插入点缓存到全局列表
;;  后续全图搜索直接从缓存匹配，无需重复 ssget
;; ============================================================
(defun wtf:build-cache ( / ss i ent content ins-pt filter)
  (setq *wtf-cache* nil)
  (setq filter (list '(-4 . "<OR") '(0 . "TEXT") '(0 . "MTEXT") '(-4 . "OR>")))
  (setq ss (ssget "_X" filter))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq content (wtf:get-text-content ent))
        (setq ins-pt (cdr (assoc 10 (entget ent))))
        (setq *wtf-cache*
          (append *wtf-cache*
            (list (list content (car ins-pt) (cadr ins-pt) (caddr ins-pt) ent))))
        (setq i (1+ i))
      )
    )
  )
  (setq *wtf-cache-valid* T)
  (princ (strcat "\n[WTF] 缓存已构建，共 " (itoa (length *wtf-cache*)) " 条文字记录"))
  *wtf-cache*
)

;; ============================================================
;;  搜索文字
;; ============================================================
;; scope: 'all 全图查找(使用缓存) / 'window 框选范围查找
;; search-text: 搜索关键词，为空时列出所有文字
(defun wtf:do-search (search-text scope / ss i ent content ins-pt pt1 pt2 filter
                               cache-list use-cache)
  (setq *wtf-results* nil)
  (setq use-cache nil)

  (cond
    ((= scope 'all)
      ;; 全图查找: 使用缓存加速
      (if (not *wtf-cache-valid*)
        (wtf:build-cache)
      )
      (setq cache-list *wtf-cache*)
      (setq use-cache T)
    )
    ((= scope 'window)
      ;; 框选查找: 不使用缓存，实时选择
      (setq filter (list '(-4 . "<OR") '(0 . "TEXT") '(0 . "MTEXT") '(-4 . "OR>")))
      (setq pt1 (getpoint "\n选择查找范围第一点: "))
      (if pt1
        (progn
          (setq pt2 (getcorner pt1 "\n选择查找范围第二点: "))
          (if pt2
            (setq ss (ssget "_W" pt1 pt2 filter))
          )
        )
      )
    )
  )

  (if use-cache
    ;; 从缓存匹配(快速，无需访问实体)
    (progn
      (foreach item cache-list
        (setq content (nth 0 item))
        (if (or (= search-text "")
                (vl-string-search (strcase search-text) (strcase content)))
          (setq *wtf-results* (append *wtf-results* (list item)))
        )
      )
      (princ (strcat "\n[WTF] 从缓存匹配，找到 " (itoa (length *wtf-results*)) " 个结果"))
    )
    ;; 框选模式: 从选择集匹配
    (progn
      (if ss
        (progn
          (setq i 0)
          (while (< i (sslength ss))
            (setq ent (ssname ss i))
            (setq content (wtf:get-text-content ent))
            (if (or (= search-text "")
                    (vl-string-search (strcase search-text) (strcase content)))
              (progn
                (setq ins-pt (cdr (assoc 10 (entget ent))))
                (setq *wtf-results*
                  (append *wtf-results*
                    (list (list content (car ins-pt) (cadr ins-pt) (caddr ins-pt) ent))))
              )
            )
            (setq i (1+ i))
          )
        )
      )
      (princ (strcat "\n[WTF] 找到 " (itoa (length *wtf-results*)) " 个结果"))
    )
  )

  *wtf-results*
)

;; ============================================================
;;  缩放定位到文字位置（约1/30屏）
;; ============================================================
(defun wtf:zoom-to-text (item / ent obj result minpt maxpt height center target-size bb-width bb-height)
  (setq ent (nth 4 item))

  (setq center nil)
  (setq height nil)
  (setq bb-width nil)
  (setq bb-height nil)

  ;; 尝试用边界框获取中心点(不取高度,避免旋转文字比例失衡)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
  (if (not (vl-catch-all-error-p obj))
    (progn
      (setq result (vl-catch-all-apply 'vla-getboundingbox (list obj 'minpt 'maxpt)))
      (if (not (vl-catch-all-error-p result))
        (progn
          (setq minpt (vlax-safearray->list minpt))
          (setq maxpt (vlax-safearray->list maxpt))
          (setq center (list (/ (+ (car minpt) (car maxpt)) 2.0)
                             (/ (+ (cadr minpt) (cadr maxpt)) 2.0)
                             0.0))
          ;; 记录边界框宽高(仅用于回退)
          (setq bb-width (abs (- (car maxpt) (car minpt))))
          (setq bb-height (abs (- (cadr maxpt) (cadr minpt))))
        )
      )
    )
  )

  ;; 回退到插入点(组码10)
  (if (null center)
    (setq center (list (nth 1 item) (nth 2 item) (nth 3 item)))
  )
  ;; 优先使用文字高度组码40(不受旋转影响,水平/垂直文字均正确)
  (setq height (cdr (assoc 40 (entget ent))))
  ;; 回退: 边界框宽高较小值(近似字符高度)
  (if (or (null height) (<= height 0.01))
    (if (and bb-width bb-height)
      (setq height (min bb-width bb-height))
      (setq height nil)
    )
  )
  ;; 最终回退
  (if (or (null height) (<= height 0.01))
    (setq height 10.0)
  )

  ;; 目标视图高度 = 文字高度 * 30 (文字占约1/30屏)
  (setq target-size (* height 30.0))

  (command "_.ZOOM" "_C" "_non" center target-size)
  (princ)
)

;; ============================================================
;;  等待用户按键：空格返回，ESC退出
;;  启用标记时: 数字键累积输入，空格/ESC时应用标记(整体替换，0=清除)
;;  返回 'back 或 'exit
;; ============================================================
(defun wtf:wait-for-key (keyword / input done char result mark-active digits opts idx cur)
  (setq done nil)
  (setq result nil)
  (setq digits "")
  (setq mark-active (and (= *wtf-mark-enabled* "1")
                         *wtf-mark-defs*
                         keyword
                         (/= keyword "")))

  (princ "\n[WTF] 已定位到文字")
  (princ "\n按【空格】返回列表选择下一个，按【ESC】退出")
  (if mark-active
    (progn
      ;; 显示标记选项(每个选项前带快捷键: 1-9、A-Z)
      (setq opts "0.清除")
      (setq idx 1)
      (foreach mk *wtf-mark-defs*
        (setq opts (strcat opts "  " (wtf:idx->key idx) "." mk))
        (setq idx (1+ idx))
      )
      (princ (strcat "\n[WTF] 标记选项: " opts))
      (princ (strcat "\n按快捷键给【" keyword "】打标记(可多个如1A，以最后输入为准)，不输入则保持原标记"))
      (setq cur (wtf:get-keyword-marks keyword))
      (if cur
        (princ (strcat "\n[WTF] 当前标记: " (wtf:mark-names cur)))
      )
    )
  )

  (while (not done)
    ;; grread nil = 不跟踪鼠标移动，只等待按键或点击
    (setq input (vl-catch-all-apply 'grread (list nil)))
    (if (vl-catch-all-error-p input)
      ;; ESC 可能触发错误，视为退出
      (progn
        (setq result 'exit)
        (setq done T)
      )
      (if (= (car input) 2)
        ;; 键盘输入: grread 返回值可能是整数(ASCII码)或字符串
        (progn
          (setq char (cadr input))
          (cond
            ;; 整数类型: 用 ASCII 码判断
            ((= (type char) 'INT)
              (cond
                ((= char 32)  ;; 空格
                  (setq result 'back)
                  (setq done T)
                )
                ((= char 27)  ;; ESC
                  (setq result 'exit)
                  (setq done T)
                )
                ;; 数字键0-9、字母键A-Z: 启用标记时累积输入(统一转大写)
                ((and mark-active
                      (or (and (>= char 48) (<= char 57))
                          (and (>= char 65) (<= char 90))
                          (and (>= char 97) (<= char 122))))
                  (setq digits (strcat digits (strcase (chr char))))
                  (princ (strcat "\n[WTF] 已输入: " digits))
                )
              )
            )
            ;; 字符串类型: 用字符串比较
            ((= (type char) 'STR)
              (cond
                ((or (= char " ") (= (strcase char) "SPACE"))
                  (setq result 'back)
                  (setq done T)
                )
                ((= (strcase char) "ESC")
                  (setq result 'exit)
                  (setq done T)
                )
                ((and mark-active (= (strlen char) 1)
                      (or (and (>= char "0") (<= char "9"))
                          (and (>= (strcase char) "A") (<= (strcase char) "Z"))))
                  (setq digits (strcat digits (strcase char)))
                  (princ (strcat "\n[WTF] 已输入: " digits))
                )
              )
            )
          )
        )
      )
    )
  )
  ;; 退出等待前应用标记输入(未输入则保持原标记)
  (if (and mark-active (/= digits ""))
    (wtf:apply-mark-input keyword digits)
  )
  result
)

;; ============================================================
;;  标记定义变更后迁移已有关键词标记
;;  按标记名称对齐(先trim再比较): 旧序号->旧名称->新序号，
;;  名称仍存在的标记保留，被删除的名称才从关键词标记中移除，
;;  每一条丢弃都在命令行明确报告，不静默丢数据
;; ============================================================
(defun wtf:remap-keyword-marks (old-defs new-defs / remapped trimmed-new kw old-idxs new-idxs name pos dropped)
  (setq remapped nil)
  (setq dropped 0)
  ;; 新定义名称先trim，消除首尾空格差异
  (setq trimmed-new (mapcar 'wtf:trim-string new-defs))
  (foreach pair *wtf-keyword-marks*
    (setq kw (car pair))
    (setq old-idxs (cdr pair))
    (setq new-idxs nil)
    (foreach i old-idxs
      ;; 旧序号取旧名称，再在新定义中按trim后名称找新序号
      (setq name (nth (1- i) old-defs))
      (if name
        (progn
          (setq pos (vl-position (wtf:trim-string name) trimmed-new))
          (if pos
            (if (not (member (1+ pos) new-idxs))
              (setq new-idxs (append new-idxs (list (1+ pos))))
            )
            (progn
              ;; 明确报告被丢弃的标记，便于发现名称不一致问题
              (setq dropped (1+ dropped))
              (princ (strcat "\n[WTF] 关键词【" kw "】的标记【" name "】在新定义中不存在，已移除"))
            )
          )
        )
      )
    )
    (if new-idxs
      (setq remapped (append remapped (list (cons kw new-idxs))))
    )
  )
  (setq *wtf-keyword-marks* remapped)
  (if (> dropped 0)
    (princ (strcat "\n[WTF] 本次重定义共移除 " (itoa dropped) " 条关键词标记(标记名在新定义中未找到)"))
  )
)

;; ============================================================
;;  标记定义对话框
;;  输入用&分隔的标记串，以最后一次定义为准；变更后已有关键词标记
;;  按名称迁移保留，仅被删除的标记名失效
;; ============================================================
(defun wtf:mark-def-dialog ( / dcl_id ret input new-defs old-defs trimmed i)
  (wtf:write-dcl)
  (setq dcl_id (load_dialog *wtf-dcl-file*))
  (if (not (new_dialog "wtf_mark_def" dcl_id))
    (princ "\n[WTF] 标记定义对话框加载失败")
    (progn
      (setq input (wtf:join-strings *wtf-mark-defs* "&"))
      (set_tile "mark_input" input)
      (mode_tile "mark_input" 2)
      (action_tile "accept" "(setq input (get_tile \"mark_input\")) (done_dialog 1)")
      (action_tile "cancel" "(done_dialog 0)")
      (setq ret (start_dialog))
      (if (= ret 1)
        (progn
          ;; 解析并trim每个标记名(去首尾空格)，trim后为空的丢弃
          (setq new-defs
            (vl-remove "" (mapcar 'wtf:trim-string (wtf:split-string input "&"))))
          ;; 最多35个(快捷键1-9、A-Z)
          (if (> (length new-defs) 35)
            (progn
              (setq trimmed nil)
              (setq i 0)
              (foreach mk new-defs
                (if (< i 35) (setq trimmed (append trimmed (list mk))))
                (setq i (1+ i))
              )
              (setq new-defs trimmed)
              (princ "\n[WTF] 标记最多支持35个，已截取前35个")
            )
          )
          (if (not (equal new-defs *wtf-mark-defs*))
            (progn
              (setq old-defs *wtf-mark-defs*)
              (setq *wtf-mark-defs* new-defs)
              ;; 按名称迁移已有关键词标记(名称仍存在则保留，被删除的失效)
              (wtf:remap-keyword-marks old-defs new-defs)
              (princ (strcat "\n[WTF] 已定义 " (itoa (length *wtf-mark-defs*)) " 个标记，同名标记的关键词数据已保留"))
            )
            (princ "\n[WTF] 标记定义未变更")
          )
          (wtf:save-data)
        )
      )
    )
  )
  (unload_dialog dcl_id)
  (princ)
)

;; ============================================================
;;  标记统计: 显示含指定标记的关键词(已定义数量时附加显示)
;; ============================================================
(defun wtf:get-keyword-qty (keyword / pair)
  (setq pair (assoc keyword *wtf-keyword-qty*))
  (if pair (cdr pair) nil)
)

(defun wtf:stat-show (idx / mks qty line)
  (setq *wtf-stat-last-idx* idx)
  (setq *wtf-stat-current* nil)
  (foreach kw *wtf-keyword-list*
    (setq mks (wtf:get-keyword-marks kw))
    (if (member idx mks)
      (setq *wtf-stat-current* (append *wtf-stat-current* (list kw)))
    )
  )
  (start_list "stat_list")
  (foreach kw *wtf-stat-current*
    (setq line kw)
    ;; 附加显示数量(由数量定义功能写入)
    (setq qty (wtf:get-keyword-qty kw))
    (if qty (setq line (strcat line "  |  " qty)))
    (add_list line)
  )
  (end_list)
  (set_tile "stat_status"
    (strcat "【" (nth (1- idx) *wtf-mark-defs*) "】共 "
            (itoa (length *wtf-stat-current*)) " 个关键词"))
)

;; ============================================================
;;  标记统计: 复制当前显示的关键词到剪贴板(每行一个，含数量)
;;  首行自动添加表头(编号/数量)，末尾附加汇总统计行(数量按数值累加)
;; ============================================================
(defun wtf:stat-copy ( / lines qty val total txt total-str)
  (if *wtf-stat-current*
    (progn
      ;; 首行表头；后续每行: 关键词 + 制表符 + 数量(未定义则只有关键词)，同时累加汇总
      (setq lines (list "编号\t数量"))
      (setq total 0.0)
      (foreach kw *wtf-stat-current*
        (setq qty (wtf:get-keyword-qty kw))
        (if qty
          (progn
            (setq lines (append lines (list (strcat kw "\t" qty))))
            (setq val (distof qty))
            (if val (setq total (+ total val)))
          )
          (setq lines (append lines (list kw)))
        )
      )
      ;; 汇总行: 整数去小数点，非整数保留两位
      (setq total-str
        (if (equal total (float (fix total)) 1e-6)
          (itoa (fix total))
          (rtos total 2 2)
        )
      )
      (setq lines (append lines (list (strcat "汇总统计\t" total-str))))
      (setq txt (wtf:join-strings lines "\r\n"))
      (if (wtf:set-clipboard txt)
        (set_tile "stat_status"
          (strcat "已复制 " (itoa (length *wtf-stat-current*))
                  " 个关键词(含表头和数量)，汇总 " total-str " 台"))
        (set_tile "stat_status" "复制失败")
      )
    )
    (set_tile "stat_status" "无可复制内容，请先点击上方标记按钮")
  )
)

;; ============================================================
;;  数量定义: 从剪贴板读取数量(每行一条)，按顺序与关键词列表一一对应，
;;  弹出核对对话框，用户确认后写入 *wtf-keyword-qty* 并刷新统计列表
;;  (从标记统计对话框内嵌套调用，复用已加载的 dcl_id)
;; ============================================================
(defun wtf:qty-define-dialog (dcl_id / lines pairs kw-cnt qty-cnt i kw qty ret)
  (setq lines (mapcar 'wtf:trim-string (wtf:split-lines (wtf:get-clipboard))))
  (setq lines (vl-remove "" lines))
  (cond
    ((null *wtf-keyword-list*)
      (set_tile "stat_status" "关键词列表为空，无法定义数量")
    )
    ((null lines)
      (set_tile "stat_status" "剪贴板无数据，请先复制数量(每行一条)")
    )
    (t
      ;; 按顺序一一对应: 第i个关键词 <- 第i行数量(行不够则无数量，多余行忽略)
      (setq kw-cnt (length *wtf-keyword-list*))
      (setq qty-cnt (length lines))
      (setq pairs nil)
      (setq i 0)
      (foreach kw *wtf-keyword-list*
        (setq qty (if (< i qty-cnt) (nth i lines) nil))
        (if qty
          (setq pairs (append pairs (list (cons kw qty))))
        )
        (setq i (1+ i))
      )
      ;; 弹出核对对话框(嵌套)
      (if (not (new_dialog "wtf_qty_confirm" dcl_id))
        (set_tile "stat_status" "数量核对对话框加载失败")
        (progn
          (start_list "qty_list")
          (setq i 1)
          (foreach kw *wtf-keyword-list*
            (setq qty (wtf:get-keyword-qty-from-pairs kw pairs))
            (add_list (strcat (itoa i) ". " kw "  =  " (if qty qty "(无)")))
            (setq i (1+ i))
          )
          (end_list)
          (if (= kw-cnt qty-cnt)
            (set_tile "qty_status"
              (strcat "共 " (itoa kw-cnt) " 个关键词，数量行数匹配，确认后生效"))
            (set_tile "qty_status"
              (strcat "注意: 关键词 " (itoa kw-cnt) " 个，剪贴板数量 " (itoa qty-cnt)
                      " 行，不一致(缺少的无数量，多余的忽略)"))
          )
          (action_tile "accept" "(done_dialog 1)")
          (action_tile "cancel" "(done_dialog 0)")
          (setq ret (start_dialog))
          (if (= ret 1)
            (progn
              ;; 确认: 整体替换数量映射并持久化
              (setq *wtf-keyword-qty* pairs)
              (wtf:save-data)
              ;; 刷新统计列表(若已选过标记则重新显示，带数量)
              (if *wtf-stat-last-idx*
                (wtf:stat-show *wtf-stat-last-idx*)
                (set_tile "stat_status"
                  (strcat "已定义 " (itoa (length pairs)) " 条数量，点击标记按钮查看"))
              )
            )
            (set_tile "stat_status" "已取消，数量定义未生效")
          )
        )
      )
    )
  )
)

;; 从临时对应表中取关键词数量(核对阶段使用，未确认前不动全局变量)
(defun wtf:get-keyword-qty-from-pairs (keyword pairs / pair)
  (setq pair (assoc keyword pairs))
  (if pair (cdr pair) nil)
)

;; ============================================================
;;  标记统计对话框
;;  每个标记生成一个按钮，点击后列表区显示含此标记的关键词
;; ============================================================
(defun wtf:mark-stat-dialog ( / dcl_id i)
  (setq *wtf-stat-current* nil)
  (setq *wtf-stat-last-idx* nil)
  (wtf:write-dcl)
  (setq dcl_id (load_dialog *wtf-dcl-file*))
  (if (not (new_dialog "wtf_mark_stat" dcl_id))
    (princ "\n[WTF] 标记统计对话框加载失败")
    (progn
      ;; 为每个标记按钮绑定动作
      (setq i 1)
      (repeat (length *wtf-mark-defs*)
        (action_tile (strcat "stat_btn_" (itoa i))
                     (strcat "(wtf:stat-show " (itoa i) ")"))
        (setq i (1+ i))
      )
      (action_tile "btn_qty_def" "(wtf:qty-define-dialog dcl_id)")
      (action_tile "btn_stat_copy" "(wtf:stat-copy)")
      (action_tile "btn_stat_close" "(done_dialog 0)")
      (set_tile "stat_status" "点击上方标记按钮查看对应关键词")
      (start_dialog)
    )
  )
  (unload_dialog dcl_id)
  (princ)
)

;; ============================================================
;;  主命令
;; ============================================================
(defun c:WTF ( / dcl_id code old-cmdecho old-osmode
                search-text scope sel-str sel-idx item key-result
                kw-sel-str kw-idx last-sel)
  ;; 从图纸加载持久化数据
  (wtf:load-data)
  (setq old-cmdecho (getvar "CMDECHO"))
  (setq old-osmode (getvar "OSMODE"))
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)

  (setq search-text *wtf-last-search*)
  (if (null search-text)
    (setq search-text "")
  )
  (setq scope 'all)
  (setq code 999)

  (while (/= code 0)
    (wtf:write-dcl)
    (setq dcl_id (load_dialog *wtf-dcl-file*))

    (if (not (new_dialog "wtf_dialog" dcl_id))
      (progn
        (princ "\n[WTF] 对话框加载失败")
        (unload_dialog dcl_id)
        (setq code 0)
      )
      (progn
        ;; 设置初始值
        (set_tile "search_text" search-text)
        (if (= scope 'all)
          (progn
            (set_tile "rb_all" "1")
            (set_tile "rb_window" "0")
          )
          (progn
            (set_tile "rb_all" "0")
            (set_tile "rb_window" "1")
          )
        )

        ;; 显示已有结果、关键词列表和状态
        (wtf:update-list)
        (wtf:update-keyword-list)
        (wtf:update-status)

        ;; 标记功能开关初始状态
        (set_tile "tg_mark_enable" *wtf-mark-enabled*)

        ;; 动作绑定
        (action_tile "rb_all" "(setq scope 'all)")
        (action_tile "rb_window" "(setq scope 'window)")

        (action_tile "btn_add_keywords" "(wtf:save-keyword-pos) (done_dialog 4)")
        (action_tile "btn_clear_data" "(wtf:save-keyword-pos) (done_dialog 5)")
        (action_tile "btn_mark_def" "(wtf:save-keyword-pos) (done_dialog 6)")
        (action_tile "btn_mark_stat" "(wtf:save-keyword-pos) (done_dialog 7)")
        (action_tile "tg_mark_enable"
          "(setq *wtf-mark-enabled* $value) (wtf:save-data)")

        (action_tile "keyword_list"
          "(setq kw-sel-str (get_tile \"keyword_list\"))
           (if (/= kw-sel-str \"\")
             (progn
               (setq *wtf-keyword-top-idx* kw-sel-str)
               (done_dialog 3)))")

        (action_tile "btn_search"
          "(wtf:save-keyword-pos)
           (setq search-text (get_tile \"search_text\"))
           (setq *wtf-last-search* search-text)
           (done_dialog 2)")

        (action_tile "result_list"
          "(wtf:save-keyword-pos)
           (setq sel-str (get_tile \"result_list\"))
           (if (/= sel-str \"\")
             (done_dialog 1))")

        (action_tile "btn_locate"
          "(wtf:save-keyword-pos)
           (setq sel-str (get_tile \"result_list\"))
           (if (/= sel-str \"\")
             (done_dialog 1)
             (princ \"\\n[WTF] 请先选择一个结果\"))")

        (action_tile "btn_exit" "(wtf:save-keyword-pos) (done_dialog 0)")

        ;; 焦点设到搜索框
        (mode_tile "search_text" 2)
        (setq code (start_dialog))
        (unload_dialog dcl_id)

        (cond
          ;; 批量添加关键词(从剪贴板)
          ((= code 4)
            (wtf:load-keywords-from-clipboard)
            (wtf:save-data)
          )
          ;; 清除缓存数据
          ((= code 5)
            (wtf:clear-data)
            (setq search-text "")
          )
          ;; 标记定义
          ((= code 6)
            (wtf:mark-def-dialog)
          )
          ;; 标记统计
          ((= code 7)
            (wtf:mark-stat-dialog)
          )
          ;; 点击关键词列表项: 填充到查找框并搜索
          ((= code 3)
            (setq kw-idx (atoi kw-sel-str))
            (if (and *wtf-keyword-list* (< kw-idx (length *wtf-keyword-list*)))
              (progn
                (setq search-text (nth kw-idx *wtf-keyword-list*))
                (setq *wtf-last-search* search-text)
                (wtf:do-search search-text scope)
                ;; 通过句柄查找上次选择的结果项(跨会话稳定)
                (setq last-sel (wtf:get-last-selection search-text))
                (if last-sel
                  (setq item (wtf:find-result-by-handle last-sel))
                  (setq item nil))
                (if item
                  (progn
                    (wtf:zoom-to-text item)
                    ;; 等待按键(启用标记时可输入数字打标记)
                    (setq key-result (wtf:wait-for-key search-text))
                    (if (eq key-result 'exit)
                      (setq code 0)
                    )
                  )
                )
              )
              (princ "\n[WTF] 无效的关键词选择")
            )
          )
          ;; 查找
          ((= code 2)
            (wtf:do-search search-text scope)
            (wtf:save-data)
          )
          ;; 定位
          ((= code 1)
            (setq sel-idx (atoi sel-str))
            (if (and *wtf-results* (< sel-idx (length *wtf-results*)))
              (progn
                (setq item (nth sel-idx *wtf-results*))
                (wtf:zoom-to-text item)
                ;; 记录当前关键词的最后选择(保存实体句柄)
                (wtf:save-selection search-text (nth 4 item))
                ;; 保存数据到图纸
                (wtf:save-data)
                ;; 等待按键(启用标记时可输入数字打标记)
                (setq key-result (wtf:wait-for-key search-text))
                (if (eq key-result 'exit)
                  (setq code 0)
                  ;; 空格返回: code保持非0，继续循环重新显示对话框
                )
              )
              (princ "\n[WTF] 无效的选择")
            )
          )
        )
      )
    )
  )

  ;; 确保数据(含滚动位置)保存到图纸
  (wtf:save-data)
  (setvar "OSMODE" old-osmode)
  (setvar "CMDECHO" old-cmdecho)
  (princ)
)

(princ "\n[WTF] 已加载。输入 WTF 启动文本查找定位。")
(princ)
