;@name 查找定位文本
;@group 文本编辑
;@desc 查找图纸中的文字并快速定位。支持全图查找和框选范围查找，点击结果可缩放定位到文字位置(约1/30屏)，按空格返回列表，按ESC退出。全图查找支持缓存加速，支持从剪贴板批量导入关键词。支持自定义标记：定位时按数字键给关键词打标记，并可按标记统计和复制关键词。支持双关联：开启后每个关键词可关联最多2个目标，定位时按空格在两目标间来回切换、右键返回，看全部时按鼠标左键定义关联目标
;@require ModelSpace
;@require Selection

(vl-load-com)

;; ---------------- 全局变量 ----------------
(setq *wtf-dcl-file* (strcat (getenv "TEMP") "\\wtf_dialog.dcl"))
;; 减少标记对话框的临时DCL文件(按当前标记动态生成行)
(setq *wtf-mark-del-dcl-file* (strcat (getenv "TEMP") "\\wtf_mark_del.dcl"))
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
;; 双关联开关("1"=启用 "0"=关闭)，启用时一个关键词可关联最多2个目标
(setq *wtf-dual-enabled* "0")
;; 双关联映射: (("关键词" 句柄1 句柄2 最后停留序号) ...)
;; 句柄2可为nil(仅1个目标)；最后停留序号 1或2(0=尚未定位过，默认从目标1开始)
(setq *wtf-dual-selection* nil)
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
;; 标记统计窗口点击列表项选中的待定位关键词(为nil时表示非定位退出)
(setq *wtf-stat-locate-kw* nil)
;; 标记统计窗口列表上次选中索引(定位返回后恢复列表滚动位置)
(setq *wtf-stat-last-sel* nil)
;; 关键词到数量的映射(关联列表: (("关键词" . "数量") ...)，由数量定义功能写入)
(setq *wtf-keyword-qty* nil)
;; 备份列表: ((备份名 关键词列表 标记定义 标记开关 标记映射 数量映射 上次搜索 位置映射 滚动位置 双关联映射) ...)
;; 备份名用关键词数量命名，如 "15个关键词"
(setq *wtf-backups* nil)
;; 定位缩放比例分母(文字约占屏1/N)，默认1/30，可在主界面点击"定位"按钮从预设值中修改
(setq *wtf-zoom-denominator* 30)

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
  (write-line "      : boxed_column {" f)
  (write-line "        label = \"备份数据 (点击切换):\";" f)
  (write-line "        : list_box {" f)
  (write-line "          key = \"backup_list\";" f)
  (write-line "          height = 6;" f)
  (write-line "          width = 20;" f)
  (write-line "        }" f)
  (write-line "      }" f)
  (write-line "      : button {" f)
  (write-line "        key = \"btn_add_keywords\";" f)
  (write-line "        label = \"批量添加(剪贴板)\";" f)
  (write-line "        width = 20;" f)
  (write-line "      }" f)
  (write-line "      : list_box {" f)
  (write-line "        key = \"keyword_list\";" f)
  (write-line "        label = \"关键词列表 (点击查找):\";" f)
  (write-line "        height = 15;" f)
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
  (write-line "        : toggle { key = \"tg_mark_enable\"; label = \"启用标记\"; }" f)
  (write-line "        : toggle { key = \"tg_dual_link\"; label = \"双关联\"; }" f)
  (write-line "      }" f)
  (write-line "      : row {" f)
  (write-line "        : button {" f)
  (write-line "          key = \"btn_clear_data\";" f)
  (write-line "          label = \"清除缓存数据\";" f)
  (write-line "          width = 9;" f)
  (write-line "        }" f)
  (write-line "        : button { key = \"btn_mark_def\"; label = \"标记\"; width = 9; }" f)
  (write-line "        : button { key = \"btn_mark_stat\"; label = \"标记统计\"; width = 9; }" f)
  (write-line "        : button {" f)
  (write-line "          key = \"btn_search\";" f)
  (write-line "          label = \"查找\";" f)
  (write-line "          width = 12;" f)
  (write-line "          is_default = true;" f)
  (write-line "        }" f)
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
      (write-line (strcat "        : button { key = \"btn_locate\"; label = \"定位 1/"
                      (itoa *wtf-zoom-denominator*)
                      "\"; width = 14; }") f)
      (write-line "        : button { key = \"btn_view_all\"; label = \"看全部\"; width = 14; }" f)
      (write-line "        : button { key = \"btn_exit\"; label = \"退出\"; width = 12; is_cancel = true; }" f)
      (write-line "      }" f)
  (write-line "    }" f)
  (write-line "  }" f)
  ;; 底部状态条: 显示选中关键词的完整标记信息(横跨整个对话框底部)
  (write-line "  : text {" f)
  (write-line "    key = \"keyword_status\";" f)
  (write-line "    label = \"\";" f)
  (write-line "    alignment = left;" f)
  (write-line "    width = 55;" f)
  (write-line "    height = 2;" f)
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
  (write-line "  : row {" f)
  (write-line "    : button { key = \"btn_mark_add\"; label = \"增加标记\"; width = 14; }" f)
  (write-line "    : button { key = \"btn_mark_del\"; label = \"减少标记\"; width = 14; }" f)
  (write-line "  }" f)
  (write-line "  : text { key = \"mark_status\"; label = \"\"; width = 44; }" f)
  (write-line "  ok_cancel;" f)
  (write-line "}" f)

  ;; ---- 增加标记输入对话框(嵌套调用) ----
  (write-line "wtf_mark_add : dialog {" f)
  (write-line "  label = \"增加标记\";" f)
  (write-line "  : edit_box {" f)
  (write-line "    key = \"mark_add_input\";" f)
  (write-line "    label = \"新标记(用&分隔):\";" f)
  (write-line "    edit_width = 40;" f)
  (write-line "    edit_limit = 100;" f)
  (write-line "  }" f)
  (write-line "  : text { key = \"mark_add_status\"; label = \"\"; width = 44; }" f)
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

  ;; ---- 管理员密码确认对话框(清除数据前) ----
  (write-line "wtf_admin_confirm : dialog {" f)
  (write-line "  label = \"管理员确认\";" f)
  (write-line "  : text { label = \"此操作将清除全部已保存的数据，请输入管理员密码确认:\"; alignment = left; }" f)
  (write-line "  : edit_box {" f)
  (write-line "    key = \"admin_pwd\";" f)
  (write-line "    label = \"密码:\";" f)
  (write-line "    password_char = \"*\";" f)
  (write-line "    edit_width = 16;" f)
  (write-line "  }" f)
  (write-line "  : row {" f)
  (write-line "    : button { key = \"admin_ok\"; label = \"确定\"; width = 10; is_default = true; }" f)
  (write-line "    : button { key = \"admin_cancel\"; label = \"取消\"; width = 10; is_cancel = true; }" f)
  (write-line "  }" f)
  (write-line "}" f)

  ;; ---- 定位比例设置对话框 ----
  (write-line "wtf_zoom_set : dialog {" f)
  (write-line "  label = \"设置定位比例\";" f)
  (write-line "  : boxed_radio_column {" f)
  (write-line "    label = \"选择文字占屏比例:\";" f)
  (write-line "    : radio_button { key = \"z1_20\"; label = \"1/20\"; }" f)
  (write-line "    : radio_button { key = \"z1_30\"; label = \"1/30\"; }" f)
  (write-line "    : radio_button { key = \"z1_40\"; label = \"1/40\"; }" f)
  (write-line "    : radio_button { key = \"z1_50\"; label = \"1/50\"; }" f)
  (write-line "    : radio_button { key = \"z1_60\"; label = \"1/60\"; }" f)
  (write-line "    : radio_button { key = \"z1_80\"; label = \"1/80\"; }" f)
  (write-line "    : radio_button { key = \"z1_100\"; label = \"1/100\"; }" f)
  (write-line "  }" f)
  (write-line "  : text { key = \"zoom_status\"; label = \"\"; alignment = centered; }" f)
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

;; 清理 MTEXT 格式码
;; 换行码\P\p、非断行空格\~ 替换为空格，其余格式码(字体\f、字号\H、颜色\C、对齐\A、
;; 堆叠\S、宽度\W、行距\T、斜角\Q、下划线\L\l、上划线\O\o、删除线\K\k、列\N、Unicode\U+ 等)全部删除
(defun wtf:clean-mtext (str / out i nch nstr j skip)
  (while (vl-string-search "\\P" str)
    (setq str (vl-string-subst " " "\\P" str))
  )
  (while (vl-string-search "\\p" str)
    (setq str (vl-string-subst " " "\\p" str))
  )
  (while (vl-string-search "\\~" str)
    (setq str (vl-string-subst " " "\\~" str))
  )
  (setq out "")
  (setq i 1)
  (while (<= i (strlen str))
    (setq nch (substr str i 1))
    (if (/= (ascii nch) 92)
      (progn
        (setq out (strcat out nch))
        (setq i (1+ i))
      )
      (progn
        (setq nstr (substr str (+ i 1) 1))
        (cond
          ;; Unicode 转义 \U+XXXX 整体删除
          ((and nstr (= nstr "U") (= (substr str (+ i 2) 1) "+"))
            (setq i (+ i 2))
            (while (and (<= i (strlen str))
                        (or (and (>= (substr str i 1) "0") (<= (substr str i 1) "9"))
                            (and (>= (substr str i 1) "A") (<= (substr str i 1) "F"))
                            (and (>= (substr str i 1) "a") (<= (substr str i 1) "f"))))
              (setq i (1+ i))
            )
          )
          ;; 单字符控制码(无分号结尾): 反斜杠+该字符整体删除
          ((and nstr (vl-string-search nstr "\\Pp~LlOoKkUN e"))
            (setq i (+ i 2))
          )
          ;; 以分号结尾的格式码(如 \f...; \H...; \C...; \A...; \S...; \W...; \T...; \Q...): 整体删除
          (t
            (setq skip nil)
            (setq j (+ i 2))
            (while (and (not skip) (<= j (strlen str)))
              (if (= (substr str j 1) ";")
                (setq skip T)
                (setq j (1+ j))
              )
            )
            (if skip
              (setq i (+ j 1))
              (setq i (+ i 1))
            )
          )
        )
      )
    )
  )
  out
)

;; 搜索结果排序: Y值从大到小，X值从小到大
(defun wtf:sort-results (results / )
  (vl-sort results
    (function
      (lambda (a b)
        (cond
          ;; Y值不同: 从大到小
          ((> (nth 2 a) (nth 2 b)) T)
          ((< (nth 2 a) (nth 2 b)) nil)
          ;; Y值相同: X值从小到大
          (T (< (nth 1 a) (nth 1 b)))
        )
      )
    )
  )
)

;; 生成列表显示行 (idx 为结果序号，从1开始)
(defun wtf:make-display-line (idx item / content x y)
  (setq content (nth 0 item))
  (setq x (nth 1 item))
  (setq y (nth 2 item))
  (if (> (strlen content) 45)
    (setq content (strcat (substr content 1 42) "..."))
  )
  (strcat (itoa idx) ". " content "  |  (" (rtos x 2 1) ", " (rtos y 2 1) ")")
)

;; 更新列表框显示 (带序号)
(defun wtf:update-list ( / idx)
  (setq idx 1)
  (start_list "result_list")
  (foreach item *wtf-results*
    (add_list (wtf:make-display-line idx item))
    (setq idx (1+ idx))
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

;; 更新关键词状态条: 显示当前选中关键词的完整标记信息
(defun wtf:update-keyword-status ( / idx kw mks qty line)
  (setq idx (get_tile "keyword_list"))
  (if (and idx (/= idx ""))
    (progn
      (setq idx (atoi idx))
      (if (and *wtf-keyword-list* (>= idx 0) (< idx (length *wtf-keyword-list*)))
        (progn
          (setq kw (nth idx *wtf-keyword-list*))
          (setq line (strcat "关键词: " kw))
          (setq mks (wtf:get-keyword-marks kw))
          (if mks
            (setq line (strcat line "    标记: " (wtf:mark-names mks))))
          (setq qty (wtf:get-keyword-qty kw))
          (if qty (setq line (strcat line "    数量: " qty)))
          (set_tile "keyword_status" line)
        )
        (set_tile "keyword_status" "")
      )
    )
    (set_tile "keyword_status" "")
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
  (wtf:sync-backup-marks)
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
;;  备份功能
;;  备份当前缓存数据，备份名用关键词数量命名
;;  用于批量添加替换前自动备份，以及主界面切换备份数据
;; ============================================================

;; 根据当前关键词数量生成备份名
(defun wtf:backup-name (n)
  (strcat (itoa n) "个关键词")
)

;; 备份当前数据，若已有同名备份则覆盖
(defun wtf:backup-current ( / n name new-list item)
  (if (and *wtf-keyword-list* (> (length *wtf-keyword-list*) 0))
    (progn
      (setq n (length *wtf-keyword-list*))
      (setq name (wtf:backup-name n))
      ;; 移除同名备份
      (setq new-list nil)
      (foreach item *wtf-backups*
        (if (/= (car item) name)
          (setq new-list (append new-list (list item)))
        )
      )
      (setq new-list (append new-list
        (list (list name *wtf-keyword-list* *wtf-mark-defs* *wtf-mark-enabled*
                    *wtf-keyword-marks* *wtf-keyword-qty* *wtf-last-search*
                    *wtf-last-selection* *wtf-keyword-top-idx*
                    *wtf-dual-selection*))))
      (setq *wtf-backups* new-list)
      (princ (strcat "\n[WTF] 已备份当前数据: " name))
      T
    )
    (progn
      (princ "\n[WTF] 当前无关键词数据，无需备份")
      nil
    )
  )
)

;; 检查当前数据是否已有备份
(defun wtf:has-backup ( / name)
  (if (and *wtf-keyword-list* (> (length *wtf-keyword-list*) 0))
    (progn
      (setq name (wtf:backup-name (length *wtf-keyword-list*)))
      (if (assoc name *wtf-backups*) T nil)
    )
    T
  )
)

;; 恢复指定备份为当前数据，返回备份名
(defun wtf:restore-backup (name / item)
  (setq item (assoc name *wtf-backups*))
  (if item
    (progn
      (setq *wtf-keyword-list* (nth 1 item))
      (setq *wtf-mark-defs* (nth 2 item))
      (setq *wtf-mark-enabled* (nth 3 item))
      (setq *wtf-keyword-marks* (nth 4 item))
      (setq *wtf-keyword-qty* (nth 5 item))
      (setq *wtf-last-search* (nth 6 item))
      (setq *wtf-last-selection* (nth 7 item))
      (setq *wtf-keyword-top-idx* (nth 8 item))
      ;; 双关联映射(第10个字段，旧备份无此字段时为nil)
      (setq *wtf-dual-selection* (nth 9 item))
      (if (null *wtf-last-selection*) (setq *wtf-last-selection* nil))
      (if (null *wtf-keyword-top-idx*) (setq *wtf-keyword-top-idx* ""))
      (if (null *wtf-dual-selection*) (setq *wtf-dual-selection* nil))
      (princ (strcat "\n[WTF] 已恢复备份数据: " name))
      T
    )
    (progn
      (princ (strcat "\n[WTF] 未找到备份: " name))
      nil
    )
  )
)

;; 同步当前数据的标记映射与数量映射到对应备份项
;; 打标记/改标记定义/定义数量后调用，确保不同备份拥有各自独立的标记组与数量组
(defun wtf:sync-backup-marks ( / n name new-list item updated)
  (setq updated nil)
  (if (and *wtf-keyword-list* (> (length *wtf-keyword-list*) 0))
    (progn
      (setq n (length *wtf-keyword-list*))
      (setq name (wtf:backup-name n))
      (setq new-list nil)
      (foreach item *wtf-backups*
        (if (and (= (car item) name) (>= (length item) 9))
          (progn
            (setq updated T)
            (setq new-list (append new-list
              (list (list name (nth 1 item) (nth 2 item) (nth 3 item)
                          *wtf-keyword-marks* *wtf-keyword-qty* (nth 6 item)
                          (nth 7 item) (nth 8 item) (nth 9 item)))))
          )
          (setq new-list (append new-list (list item)))
        )
      )
      (if updated
        (setq *wtf-backups* new-list)
      )
    )
  )
  (if updated (wtf:save-data))
)

;; 更新主界面左侧备份列表显示，并选中当前数据对应的备份项
(defun wtf:update-backup-list ( / names item cur-name cur-idx i)
  (setq names nil)
  (setq cur-name nil)
  (if (and *wtf-keyword-list* (> (length *wtf-keyword-list*) 0))
    (setq cur-name (wtf:backup-name (length *wtf-keyword-list*)))
  )
  (setq cur-idx nil)
  (setq i 0)
  (foreach item *wtf-backups*
    (setq names (append names (list (car item))))
    (if (and cur-name (= (car item) cur-name))
      (setq cur-idx i)
    )
    (setq i (1+ i))
  )
  (start_list "backup_list")
  (if (and names (> (length names) 0))
    (foreach name names
      (add_list name)
    )
    (add_list "（无备份）")
  )
  (end_list)
  ;; 若当前数据对应的备份存在，则选中该项
  (if cur-idx
    (set_tile "backup_list" (itoa cur-idx))
  )
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
;;  双关联读写
;;  双关联映射每项: ("关键词" 句柄1 句柄2 最后停留序号)
;;  句柄2可为nil(仅1个目标)；最后停留序号 1或2(0=尚未定位过)
;; ============================================================
;; 获取关键词的双关联记录: 返回 (句柄1 句柄2 最后停留序号)，无记录返回 nil
(defun wtf:get-dual-selection (keyword / pair)
  (setq pair (assoc keyword *wtf-dual-selection*))
  (if pair (cdr pair) nil)
)

;; 将目标句柄加入双关联记录(FIFO最多2个，第3个自动替换较早记住的)
;; 已关联过的目标仅更新最后停留序号，不重复添加
;; 返回新的记录列表 (句柄1 句柄2 最后停留序号)
(defun wtf:dual-add (keyword handle / rec h1 h2 last-idx new-rec)
  (if (and handle (/= handle ""))
    (progn
      (setq rec (wtf:get-dual-selection keyword))
      (if rec
        (progn
          (setq h1 (nth 0 rec))
          (setq h2 (nth 1 rec))
          (setq last-idx (nth 2 rec))
          (cond
            ((= handle h1) (setq last-idx 1))
            ((and h2 (= handle h2)) (setq last-idx 2))
            ((null h2)
              ;; 仅1个目标，追加为第2个
              (setq h2 handle)
              (setq last-idx 2)
            )
            (t
              ;; 已有2个目标，替换较早记住的(句柄1)，新目标作为最后
              (setq h1 h2)
              (setq h2 handle)
              (setq last-idx 2)
            )
          )
          (setq new-rec (list h1 h2 last-idx))
        )
        (setq new-rec (list handle nil 1))
      )
      ;; 写回映射(替换同名关键词的旧记录)
      (setq *wtf-dual-selection*
        (append (vl-remove-if '(lambda (p) (= (car p) keyword)) *wtf-dual-selection*)
                (list (cons keyword new-rec))))
      new-rec
    )
    nil
  )
)

;; 更新关键词双关联的最后停留目标序号(1或2)
(defun wtf:dual-set-last (keyword last-idx / rec)
  (setq rec (wtf:get-dual-selection keyword))
  (if rec
    (progn
      (setq rec (list (nth 0 rec) (nth 1 rec) last-idx))
      (setq *wtf-dual-selection*
        (append (vl-remove-if '(lambda (p) (= (car p) keyword)) *wtf-dual-selection*)
                (list (cons keyword rec))))
    )
  )
)

;; 按句柄查找实体并构造定位item (文字内容 X Y Z 实体名)
;; 实体已删除或句柄无效时返回 nil
(defun wtf:dual-find-item (handle / ent content ins-pt)
  (if (and handle (/= handle ""))
    (progn
      (setq ent (handent handle))
      (if (and ent (entget ent))
        (progn
          (setq content (wtf:get-text-content ent))
          (setq ins-pt (cdr (assoc 10 (entget ent))))
          (list content (car ins-pt) (cadr ins-pt) (caddr ins-pt) ent))
        nil
      )
    )
    nil
  )
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
      (vlax-ldata-put dict "dual-enabled" *wtf-dual-enabled*)
      (vlax-ldata-put dict "dual-selection" *wtf-dual-selection*)
      (vlax-ldata-put dict "last-search" *wtf-last-search*)
      (vlax-ldata-put dict "keyword-top-idx" *wtf-keyword-top-idx*)
      (vlax-ldata-put dict "mark-defs" *wtf-mark-defs*)
      (vlax-ldata-put dict "mark-enabled" *wtf-mark-enabled*)
      (vlax-ldata-put dict "keyword-marks" *wtf-keyword-marks*)
      (vlax-ldata-put dict "keyword-qty" *wtf-keyword-qty*)
      (vlax-ldata-put dict "backups" *wtf-backups*)
      (vlax-ldata-put dict "zoom-denominator" *wtf-zoom-denominator*)
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
      (setq *wtf-dual-enabled* (vlax-ldata-get dict "dual-enabled" "0"))
      (setq *wtf-dual-selection* (vlax-ldata-get dict "dual-selection" nil))
      (setq *wtf-last-search* (vlax-ldata-get dict "last-search" ""))
      (setq *wtf-keyword-top-idx* (vlax-ldata-get dict "keyword-top-idx" ""))
      (setq *wtf-mark-defs* (vlax-ldata-get dict "mark-defs" nil))
      (setq *wtf-mark-enabled* (vlax-ldata-get dict "mark-enabled" "0"))
      (setq *wtf-keyword-marks* (vlax-ldata-get dict "keyword-marks" nil))
      (setq *wtf-keyword-qty* (vlax-ldata-get dict "keyword-qty" nil))
      (setq *wtf-backups* (vlax-ldata-get dict "backups" nil))
      (setq *wtf-zoom-denominator* (vlax-ldata-get dict "zoom-denominator" 30))
      (if (null *wtf-keyword-list*) (setq *wtf-keyword-list* nil))
      (if (null *wtf-last-selection*) (setq *wtf-last-selection* nil))
      (if (null *wtf-dual-enabled*) (setq *wtf-dual-enabled* "0"))
      (if (null *wtf-dual-selection*) (setq *wtf-dual-selection* nil))
      (if (null *wtf-last-search*) (setq *wtf-last-search* ""))
      (if (null *wtf-keyword-top-idx*) (setq *wtf-keyword-top-idx* ""))
      (if (null *wtf-mark-enabled*) (setq *wtf-mark-enabled* "0"))
      (if (null *wtf-backups*) (setq *wtf-backups* nil))
    )
  )
)

;; 清除当前使用的数据，保留备份区域其他备份
(defun wtf:clear-data ( / cur-name new-list item)
  ;; 从备份列表中移除当前数据对应的备份项(如果有)
  (setq cur-name nil)
  (if (and *wtf-keyword-list* (> (length *wtf-keyword-list*) 0))
    (setq cur-name (wtf:backup-name (length *wtf-keyword-list*)))
  )
  (if cur-name
    (progn
      (setq new-list nil)
      (foreach item *wtf-backups*
        (if (/= (car item) cur-name)
          (setq new-list (append new-list (list item)))
        )
      )
      (setq *wtf-backups* new-list)
    )
  )
  (setq *wtf-keyword-list* nil)
  (setq *wtf-last-selection* nil)
  (setq *wtf-dual-enabled* "0")
  (setq *wtf-dual-selection* nil)
  (setq *wtf-last-search* "")
  (setq *wtf-keyword-top-idx* "")
  (setq *wtf-mark-defs* nil)
  (setq *wtf-mark-enabled* "0")
  (setq *wtf-keyword-marks* nil)
  (setq *wtf-keyword-qty* nil)
  (setq *wtf-cache* nil)
  (setq *wtf-cache-valid* nil)
  ;; 重新保存数据以持久化剩余的备份
  (wtf:save-data)
)

;; 管理员密码确认: 弹出密码输入对话框，输入admin正确才返回T
(defun wtf:admin-verify ( / dcl_id ret)
  (setq ret 0)
  (setq dcl_id (load_dialog *wtf-dcl-file*))
  (if (not (new_dialog "wtf_admin_confirm" dcl_id))
    (princ "\n[WTF] 管理员确认对话框加载失败")
    (progn
      (action_tile "admin_ok"
        "(if (= (get_tile \"admin_pwd\") \"admin\") (done_dialog 1) (set_tile \"admin_pwd\" \"\"))")
      (action_tile "admin_cancel" "(done_dialog 0)")
      (mode_tile "admin_pwd" 2)
      (setq ret (start_dialog))
    )
  )
  (unload_dialog dcl_id)
  (= ret 1)
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

  ;; 按坐标排序: Y值从大到小，X值从小到大
  (setq *wtf-results* (wtf:sort-results *wtf-results*))

  *wtf-results*
)

;; 定位提示圆相关全局变量
(setq *wtf-indicator-ent* nil)    ; 本次新建的全部圆实体列表(用于删除)
(setq *wtf-indicator-front* nil)  ; 前(左)圆实体列表
(setq *wtf-indicator-back* nil)   ; 后(右)圆实体列表
(setq *wtf-indicator-top* nil)    ; 上圆实体列表(文本中央上方)
(setq *wtf-indicator-bottom* nil) ; 下圆实体列表(文本中央下方)
(setq *wtf-indicator-toggle* nil) ; 颜色互换状态

;; 看全部功能相关全局变量
;; 注意: 2026-08-06起看全部只对当前查看的目标绘制定位圆(红绿闪烁)，
;; 不再为所有结果批量画圆，以下 *wtf-view-all-* 系列保留仅为兼容(不再填充)
(setq *wtf-view-all-ents* nil)      ; 所有结果的实心圆实体列表
(setq *wtf-view-all-front* nil)     ; 所有结果的前(左)圆实体列表
(setq *wtf-view-all-back* nil)      ; 所有结果的后(右)圆实体列表
(setq *wtf-view-all-top* nil)       ; 所有结果的上圆实体列表
(setq *wtf-view-all-bottom* nil)    ; 所有结果的下圆实体列表
(setq *wtf-view-all-toggle* nil)    ; 颜色互换状态
(setq *wtf-view-all-idx* 0)         ; 当前显示的结果索引
(setq *wtf-view-all-selected* nil)  ; 用户选择的结果(item)
(setq *wtf-view-all-seq-ent* nil)   ; 序号文字实体(看全部时显示当前序号)

;; 给实体设置颜色(无62组则追加)，并立即刷新显示
(defun wtf:set-color (e col / ed)
  (if (and e (setq ed (entget e)))
    (progn
      (if (assoc 62 ed)
        (entmod (subst (cons 62 col) (assoc 62 ed) ed))
        (entmod (append ed (list (cons 62 col))))
      )
      (redraw e)
    )
  )
  (princ)
)

;; 设置实体可见性: flag=T可见, nil不可见 (用ActiveX vla-put-visible 更可靠)
(defun wtf:set-vis (e flag / obj)
  (if (and e (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list e))))
    (progn
      (vl-catch-all-apply 'vla-put-visible (list obj (if flag :vlax-true :vlax-false)))
      (redraw e)
    )
  )
  (princ)
)

;; 显示或隐藏四个提示圆 (flag=T 显示, nil 隐藏)
(defun wtf:set-circles-visible (flag)
  (foreach e *wtf-indicator-front*
    (wtf:set-vis e flag)
  )
  (foreach e *wtf-indicator-back*
    (wtf:set-vis e flag)
  )
  (foreach e *wtf-indicator-top*
    (wtf:set-vis e flag)
  )
  (foreach e *wtf-indicator-bottom*
    (wtf:set-vis e flag)
  )
  (princ)
)

;; 简单延时(毫秒)，用于保持圆可见窗口(纯忙等，不涉及command)
(defun wtf:sleep (ms / t0)
  (setq t0 (getvar "MILLISECS"))
  (while (< (- (getvar "MILLISECS") t0) ms))
  (princ)
)

;; 在文字前后上下各画一个与文字同字号(直径=文字高度)的实心圆提示
;; 前(左)后(右)上下各一：前/上红，后/下绿；定位显示期间颜色循环互换
;; DONUT 可能生成多个实体，故收集本次全部新建实体存入全局列表，返回时统一删除
(defun wtf:draw-indicator (ent height / obj result minpt maxpt radius cx cy cxmid cy1 cy2 before e lst px py dx dy ed)
  (wtf:erase-indicator)
  (setq before (entlast))
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
  (if (not (vl-catch-all-error-p obj))
    (progn
      (setq result (vl-catch-all-apply 'vla-getboundingbox (list obj 'minpt 'maxpt)))
      (if (not (vl-catch-all-error-p result))
        (progn
          ;; 输出参数可能未被赋值(异常实体)，安全转换避免 consp nil 错误
          (setq minpt (vl-catch-all-apply 'vlax-safearray->list (list minpt)))
          (if (vl-catch-all-error-p minpt) (setq minpt nil))
          (setq maxpt (vl-catch-all-apply 'vlax-safearray->list (list maxpt)))
          (if (vl-catch-all-error-p maxpt) (setq maxpt nil))
          (if (and minpt maxpt)
            (progn
              (setq radius (/ (float height) 2.0))
          (if (or (null radius) (<= radius 0.0))
            (setq radius 2.5)
          )
          (setq cxmid (/ (+ (car minpt) (car maxpt)) 2.0))
          (setq cy (/ (+ (cadr minpt) (cadr maxpt)) 2.0))
          ;; 前后圆：左右各一(垂直居中)；上下圆：中央上/下各一
          (setq cx1 (- (car minpt) (* radius 2.0)))
          (setq cx2 (+ (car maxpt) (* radius 2.0)))
          (setq cy1 (+ (cadr maxpt) (* radius 2.0)))
          (setq cy2 (- (cadr minpt) (* radius 2.0)))
          ;; 用 DONUT(内径0)画四个实心圆
          (command "_.DONUT" 0.0 (* radius 2.0) "_non" (list cx1 cy 0.0) "")
          (command "_.DONUT" 0.0 (* radius 2.0) "_non" (list cx2 cy 0.0) "")
          (command "_.DONUT" 0.0 (* radius 2.0) "_non" (list cxmid cy1 0.0) "")
          (command "_.DONUT" 0.0 (* radius 2.0) "_non" (list cxmid cy2 0.0) "")
          ;; 收集 before 之后新建的全部实体(可能不止一个)
          (setq lst nil)
          (setq e (entnext before))
          (while e
            (setq lst (cons e lst))
            (if (equal e (entlast))
              (setq e nil)
              (setq e (entnext e))
            )
          )
          (setq *wtf-indicator-ent* lst)
          ;; 按位置划分四组：以水平/垂直中心为界，偏移大的方向决定归属
          (setq *wtf-indicator-front* nil)
          (setq *wtf-indicator-back* nil)
          (setq *wtf-indicator-top* nil)
          (setq *wtf-indicator-bottom* nil)
          (foreach e lst
            (if (and e (setq ed (entget e)))
              (progn
                (setq px (car (cdr (assoc 10 ed))))
                (setq py (cadr (cdr (assoc 10 ed))))
                (setq dx (abs (- px cxmid)))
                (setq dy (abs (- py cy)))
                (cond
                  ((>= dx dy)
                    (if (< px cxmid)
                      (setq *wtf-indicator-front* (cons e *wtf-indicator-front*))
                      (setq *wtf-indicator-back* (cons e *wtf-indicator-back*))
                    )
                  )
                  (T
                    (if (> py cy)
                      (setq *wtf-indicator-top* (cons e *wtf-indicator-top*))
                      (setq *wtf-indicator-bottom* (cons e *wtf-indicator-bottom*))
                    )
                  )
                )
              )
            )
          )
          ;; 初始配色：前/上红，后/下绿
          (foreach e *wtf-indicator-front* (wtf:set-color e 1))
          (foreach e *wtf-indicator-back* (wtf:set-color e 3))
          (foreach e *wtf-indicator-top* (wtf:set-color e 1))
          (foreach e *wtf-indicator-bottom* (wtf:set-color e 3))
          (setq *wtf-indicator-toggle* nil)
            )
          )
        )
      )
    )
  )
  (princ)
)

;; 互换四圆颜色(前/上红后/下绿 <-> 前/上绿后/下红)
(defun wtf:swap-indicator-colors ()
  (setq *wtf-indicator-toggle* (not *wtf-indicator-toggle*))
  (if *wtf-indicator-toggle*
    (progn
      (foreach e *wtf-indicator-front* (wtf:set-color e 3))
      (foreach e *wtf-indicator-back* (wtf:set-color e 1))
      (foreach e *wtf-indicator-top* (wtf:set-color e 3))
      (foreach e *wtf-indicator-bottom* (wtf:set-color e 1))
    )
    (progn
      (foreach e *wtf-indicator-front* (wtf:set-color e 1))
      (foreach e *wtf-indicator-back* (wtf:set-color e 3))
      (foreach e *wtf-indicator-top* (wtf:set-color e 1))
      (foreach e *wtf-indicator-bottom* (wtf:set-color e 3))
    )
  )
  (princ)
)

;; 删除定位提示圆(若存在)
(defun wtf:erase-indicator ()
  (if *wtf-indicator-ent*
    (foreach e *wtf-indicator-ent*
      (if (and e (entget e))
        (entdel e)
      )
    )
  )
  (setq *wtf-indicator-ent* nil)
  (setq *wtf-indicator-front* nil)
  (setq *wtf-indicator-back* nil)
  (setq *wtf-indicator-top* nil)
  (setq *wtf-indicator-bottom* nil)
  (setq *wtf-indicator-toggle* nil)
  (princ)
)

;; 删除看全部功能的所有实心圆(若存在)
(defun wtf:erase-view-all ()
  (if *wtf-view-all-ents*
    (foreach e *wtf-view-all-ents*
      (if (and e (entget e))
        (entdel e)
      )
    )
  )
  ;; 删除序号文字实体
  (if (and *wtf-view-all-seq-ent* (entget *wtf-view-all-seq-ent*))
    (entdel *wtf-view-all-seq-ent*)
  )
  (setq *wtf-view-all-ents* nil)
  (setq *wtf-view-all-front* nil)
  (setq *wtf-view-all-back* nil)
  (setq *wtf-view-all-top* nil)
  (setq *wtf-view-all-bottom* nil)
  (setq *wtf-view-all-toggle* nil)
  (setq *wtf-view-all-seq-ent* nil)
  (princ)
)

;; 为单个实体绘制实心圆(看全部功能使用)
(defun wtf:draw-indicator-for-item (ent height / obj result minpt maxpt radius cx cy cxmid cy1 cy2 before e px py dx dy ed)
  (setq before (entlast))
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
  (if (not (vl-catch-all-error-p obj))
    (progn
      (setq result (vl-catch-all-apply 'vla-getboundingbox (list obj 'minpt 'maxpt)))
      (if (not (vl-catch-all-error-p result))
        (progn
          (setq minpt (vlax-safearray->list minpt))
          (setq maxpt (vlax-safearray->list maxpt))
          (setq radius (/ (float height) 2.0))
          (if (or (null radius) (<= radius 0.0))
            (setq radius 2.5)
          )
          (setq cxmid (/ (+ (car minpt) (car maxpt)) 2.0))
          (setq cy (/ (+ (cadr minpt) (cadr maxpt)) 2.0))
          ;; 前后圆：左右各一(垂直居中)；上下圆：中央上/下各一
          (setq cx1 (- (car minpt) (* radius 2.0)))
          (setq cx2 (+ (car maxpt) (* radius 2.0)))
          (setq cy1 (+ (cadr maxpt) (* radius 2.0)))
          (setq cy2 (- (cadr minpt) (* radius 2.0)))
          ;; 用 DONUT(内径0)画四个实心圆
          (command "_.DONUT" 0.0 (* radius 2.0) "_non" (list cx1 cy 0.0) "")
          (command "_.DONUT" 0.0 (* radius 2.0) "_non" (list cx2 cy 0.0) "")
          (command "_.DONUT" 0.0 (* radius 2.0) "_non" (list cxmid cy1 0.0) "")
          (command "_.DONUT" 0.0 (* radius 2.0) "_non" (list cxmid cy2 0.0) "")
          ;; 收集 before 之后新建的全部实体(可能不止一个)
          (setq e (entnext before))
          (while e
            ;; 添加到全局列表
            (setq *wtf-view-all-ents* (cons e *wtf-view-all-ents*))
            ;; 按位置划分四组
            (if (and e (setq ed (entget e)))
              (progn
                (setq px (car (cdr (assoc 10 ed))))
                (setq py (cadr (cdr (assoc 10 ed))))
                (setq dx (abs (- px cxmid)))
                (setq dy (abs (- py cy)))
                (cond
                  ((>= dx dy)
                    (if (< px cxmid)
                      (setq *wtf-view-all-front* (cons e *wtf-view-all-front*))
                      (setq *wtf-view-all-back* (cons e *wtf-view-all-back*))
                    )
                  )
                  (T
                    (if (> py cy)
                      (setq *wtf-view-all-top* (cons e *wtf-view-all-top*))
                      (setq *wtf-view-all-bottom* (cons e *wtf-view-all-bottom*))
                    )
                  )
                )
              )
            )
            (if (equal e (entlast))
              (setq e nil)
              (setq e (entnext e))
            )
          )
        )
      )
    )
  )
  (princ)
)

;; 看全部功能: 缩放笼罩所有结果，空格逐个展示，右键确定选择/返回
;; keyword: 当前关键词(双关联开启时用于按Y关联目标，可传nil)
(defun wtf:view-all-results (keyword / item flash-t input done char
                                 idx cnt dual-mode d-rec d-cnt)
  ;; 清除之前的实心圆
  (wtf:erase-view-all)
  (setq *wtf-view-all-selected* nil)
  ;; 双关联模式: 启用开关且提供了当前关键词
  (setq dual-mode (and (= *wtf-dual-enabled* "1") keyword (/= keyword "")))
  
  ;; 检查是否有结果
  (if (null *wtf-results*)
    (progn
      (princ "\n[WTF] 没有搜索结果，请先执行查找")
      (princ)
    )
    (progn
      (setq cnt (length *wtf-results*))

      ;; 只对当前查看的目标绘制定位提示圆(由wtf:zoom-to-text内部绘制)，
      ;; 其他非当前目标不画圆，避免全部目标都被红绿圆标记
      
      ;; 初始化当前索引为0，并缩放到第一项
      (setq *wtf-view-all-idx* 0)
      (if (and *wtf-results* (> cnt 0))
        (progn
          (setq item (nth 0 *wtf-results*))
          (wtf:zoom-to-text item 1 cnt)
          (princ (strcat "\n[WTF] 当前: 1/" (itoa cnt)))
        )
      )
      
      (princ (strcat "\n[WTF] 共 " (itoa cnt) " 个结果，空格逐个查看"))
      (if dual-mode
        (princ "\n【空格】下一个  【左键】关联当前目标  【右键】返回  【ESC】退出")
        (princ "\n【空格】下一个  【右键】确定选择  【ESC】退出")
      )
      
      ;; 等待用户按键
      (setq flash-t (getvar "MILLISECS"))
      (setq done nil)
      (while (not done)
        ;; 每300ms互换当前目标定位圆的颜色(红绿闪烁)
        (if (>= (- (getvar "MILLISECS") flash-t) 300)
          (progn
            (setq flash-t (getvar "MILLISECS"))
            (wtf:swap-indicator-colors)
          )
        )
        ;; 检测输入
        (setq input (vl-catch-all-apply 'grread (list T)))
        (if (vl-catch-all-error-p input)
          ;; ESC 可能触发错误，视为退出
          (progn
            (setq done T)
            (setq *wtf-view-all-selected* nil)
          )
          (cond
            ;; 鼠标左键: 双关联模式关联当前目标(点击任意位置即关联当前显示的结果)
            ((= (car input) 3)
              (if dual-mode
                (progn
                  (if (and *wtf-results* (>= *wtf-view-all-idx* 0) (< *wtf-view-all-idx* cnt))
                    (progn
                      (setq item (nth *wtf-view-all-idx* *wtf-results*))
                      (setq d-rec (wtf:dual-add keyword
                        (cdr (assoc 5 (entget (nth 4 item))))))
                      (if d-rec
                        (progn
                          (wtf:save-data)
                          (wtf:sync-backup-marks)
                          (setq d-cnt (if (nth 1 d-rec) 2 1))
                          (princ (strcat "\n[WTF] 已将当前目标关联到【" keyword
                                         "】(共 " (itoa d-cnt) "/2 个)"))
                        )
                      )
                    )
                  )
                )
                ;; 非双关联模式: 左键不响应
                nil
              )
            )
            ;; 鼠标右键: 双关联模式返回界面，否则确定当前选择
            ((= (car input) 25)
              (progn
                (setq done T)
                (if dual-mode
                  ;; 双关联模式: 右键=返回(不选择、不关联)
                  (setq *wtf-view-all-selected* nil)
                  ;; 保存当前选择的结果
                  (if (and *wtf-results* (>= *wtf-view-all-idx* 0) (< *wtf-view-all-idx* cnt))
                    (setq *wtf-view-all-selected* (nth *wtf-view-all-idx* *wtf-results*))
                    (setq *wtf-view-all-selected* nil)
                  )
                )
              )
            )
            ;; 键盘输入
            ((= (car input) 2)
              (progn
                (setq char (cadr input))
                (cond
                  ;; 整数类型: 用 ASCII 码判断
                  ((= (type char) 'INT)
                    (cond
                      ((= char 32)  ;; 空格
                        (progn
                          ;; 下一个
                          (setq *wtf-view-all-idx* (1+ *wtf-view-all-idx*))
                          (if (>= *wtf-view-all-idx* cnt)
                            (setq *wtf-view-all-idx* 0)
                          )
                          ;; 缩放到当前结果
                          (if (and (>= *wtf-view-all-idx* 0) (< *wtf-view-all-idx* cnt))
                            (progn
                              (setq item (nth *wtf-view-all-idx* *wtf-results*))
                              (wtf:zoom-to-text item (1+ *wtf-view-all-idx*) cnt)
                              (princ (strcat "\n[WTF] 当前: " (itoa (1+ *wtf-view-all-idx*)) "/" (itoa cnt)))
                            )
                          )
                        )
                      )
                      ((= char 27)  ;; ESC
                        (progn
                          (setq done T)
                          (setq *wtf-view-all-selected* nil)
                        )
                      )
                    )
                  )
                  ;; 字符串类型: 用字符串比较
                  ((= (type char) 'STR)
                    (cond
                      ((or (= char " ") (= (strcase char) "SPACE"))
                        (progn
                          ;; 下一个
                          (setq *wtf-view-all-idx* (1+ *wtf-view-all-idx*))
                          (if (>= *wtf-view-all-idx* cnt)
                            (setq *wtf-view-all-idx* 0)
                          )
                          ;; 缩放到当前结果
                          (if (and (>= *wtf-view-all-idx* 0) (< *wtf-view-all-idx* cnt))
                            (progn
                              (setq item (nth *wtf-view-all-idx* *wtf-results*))
                              (wtf:zoom-to-text item (1+ *wtf-view-all-idx*) cnt)
                              (princ (strcat "\n[WTF] 当前: " (itoa (1+ *wtf-view-all-idx*)) "/" (itoa cnt)))
                            )
                          )
                        )
                      )
                      ((= (strcase char) "ESC")
                        (progn
                          (setq done T)
                          (setq *wtf-view-all-selected* nil)
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
      
      ;; 删除所有实心圆
      (wtf:erase-view-all)
      ;; 删除当前结果的定位圆(由wtf:zoom-to-text中的wtf:draw-indicator绘制)
      (wtf:erase-indicator)
      
      ;; 如果用户选择了结果，建立映射关系并返回
      (if *wtf-view-all-selected*
        (progn
          (princ (strcat "\n[WTF] 已选择: " (nth 0 *wtf-view-all-selected*)))
          ;; 返回选中的item，由主循环处理映射关系
          *wtf-view-all-selected*
        )
        (progn
          (princ "\n[WTF] 已退出看全部模式")
          nil
        )
      )
    )
  )
)

;; ============================================================
;;  缩放定位到文字位置（约1/30屏）
;; ============================================================
(defun wtf:zoom-to-text (item seq-num total / ent obj result minpt maxpt height center target-size bb-width bb-height seq-text-hgt seq-text-pos)
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
          ;; 输出参数可能未被赋值(异常实体)，安全转换避免 consp nil 错误
          (setq minpt (vl-catch-all-apply 'vlax-safearray->list (list minpt)))
          (if (vl-catch-all-error-p minpt) (setq minpt nil))
          (setq maxpt (vl-catch-all-apply 'vlax-safearray->list (list maxpt)))
          (if (vl-catch-all-error-p maxpt) (setq maxpt nil))
          (if (and minpt maxpt)
            (progn
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

  ;; 目标视图高度 = 文字高度 * 比例分母 (文字占约1/N屏)
  (setq target-size (* height (float *wtf-zoom-denominator*)))

  (command "_.ZOOM" "_C" "_non" center target-size)
  ;; 定位后在文字前面画实心圆提示，返回时删除
  (wtf:draw-indicator ent height)

  ;; 如果有序号参数，在定位圆顶部区域显示序号文字
  (if (and seq-num (>= seq-num 1))
    (progn
      ;; 先删除之前的序号文字
      (if (and *wtf-view-all-seq-ent* (entget *wtf-view-all-seq-ent*))
        (entdel *wtf-view-all-seq-ent*)
      )
      (setq *wtf-view-all-seq-ent* nil)
      ;; 序号文字高度为定位圆直径的1.5倍
      (setq seq-text-hgt (* height 1.5))
      ;; 序号文字位置: 定位圆区域顶部(上圆上方)
      (setq seq-text-pos (list (car center)
                                (+ (cadr center) (* height 3.0))
                                0.0))
      ;; 创建序号文字实体
      (setq *wtf-view-all-seq-ent*
        (entmakex
          (list
            (cons 0 "TEXT")
            (cons 10 seq-text-pos)
            (cons 40 seq-text-hgt)
            (cons 1 (strcat "[" (itoa seq-num) "/" (itoa total) "]"))
            (cons 7 (getvar "TEXTSTYLE"))
            (cons 62 7)  ; 颜色7(白色/黑色)
            (cons 72 1)  ; 水平对齐: 居中
            (cons 11 seq-text-pos)
          )
        )
      )
    )
  )

  (princ)
)

;; ============================================================
;;  等待用户按键：空格/鼠标右键返回，ESC退出
;;  启用标记时: 数字键累积输入，空格/鼠标右键/ESC时应用标记(整体替换，0=清除)
;;  dual-items: 双关联目标item列表(最多2个)，nil表示非双关联模式
;;  cur-idx: 当前显示目标在dual-items中的索引(0或1)
;;  双关联模式: 空格在两目标间来回切换，右键返回，ESC退出
;;  返回 'back 或 'exit
;; ============================================================
(defun wtf:wait-for-key (keyword dual-items cur-idx / input done char result
                             mark-active digits opts idx cur flash-t d-len d-item)
  (setq done nil)
  (setq result nil)
  (setq digits "")
  (setq d-len (if dual-items (length dual-items) 0))
  (setq mark-active (and (= *wtf-mark-enabled* "1")
                         *wtf-mark-defs*
                         keyword
                         (/= keyword "")))

  (princ "\n[WTF] 已定位到文字")
  (if (> d-len 1)
    (princ "\n按【空格】切换目标，鼠标右键返回列表，按【ESC】退出")
    (princ "\n按【空格】或鼠标右键返回列表选择下一个，按【ESC】退出")
  )
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

  (setq flash-t (getvar "MILLISECS"))
  (while (not done)
    ;; 每300ms互换颜色(前/上红后/下绿<->前/上绿后/下红)
    (if (>= (- (getvar "MILLISECS") flash-t) 300)
      (progn
        (setq flash-t (getvar "MILLISECS"))
        (wtf:swap-indicator-colors)
      )
    )
    ;; grread T 跟踪模式：检测输入
    (setq input (vl-catch-all-apply 'grread (list T)))
    (if (vl-catch-all-error-p input)
      ;; ESC 可能触发错误，视为退出
      (progn
        (setq result 'exit)
        (setq done T)
      )
      (cond
        ;; 鼠标右键点击: 等同空格，返回列表
        ((= (car input) 25)
          (setq result 'back)
          (setq done T)
        )
        ;; 键盘输入: grread 返回值可能是整数(ASCII码)或字符串
        ((= (car input) 2)
          (progn
            (setq char (cadr input))
            (cond
              ;; 整数类型: 用 ASCII 码判断
              ((= (type char) 'INT)
                (cond
                  ((= char 32)  ;; 空格
                    (if (> d-len 1)
                      ;; 双关联模式: 在两目标间来回切换
                      (progn
                        (setq cur-idx (if (= cur-idx 0) 1 0))
                        (setq d-item (nth cur-idx dual-items))
                        (if d-item
                          (progn
                            (wtf:zoom-to-text d-item nil nil)
                            (wtf:dual-set-last keyword (1+ cur-idx))
                            (wtf:save-data)
                            (wtf:sync-backup-marks)
                            (princ (strcat "\n[WTF] 已切换到目标 "
                                           (itoa (1+ cur-idx)) "/" (itoa d-len)))
                          )
                        )
                      )
                      (progn
                        (setq result 'back)
                        (setq done T)
                      )
                    )
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
                    (if (> d-len 1)
                      ;; 双关联模式: 在两目标间来回切换
                      (progn
                        (setq cur-idx (if (= cur-idx 0) 1 0))
                        (setq d-item (nth cur-idx dual-items))
                        (if d-item
                          (progn
                            (wtf:zoom-to-text d-item nil nil)
                            (wtf:dual-set-last keyword (1+ cur-idx))
                            (wtf:save-data)
                            (wtf:sync-backup-marks)
                            (princ (strcat "\n[WTF] 已切换到目标 "
                                           (itoa (1+ cur-idx)) "/" (itoa d-len)))
                          )
                        )
                      )
                      (progn
                        (setq result 'back)
                        (setq done T)
                      )
                    )
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
  )
  ;; 退出等待前应用标记输入(未输入则保持原标记)
  (if (and mark-active (/= digits ""))
    (wtf:apply-mark-input keyword digits)
  )
  ;; 返回前删除定位提示圆
  (wtf:erase-indicator)
  result
)

;; ============================================================
;;  统一定位入口
;;  双关联开启时: 有双关联记录→定位+空格切换/右键返回
;;                无记录且auto-view-all=T→自动进入看全部模式(左键定义目标)
;;                无记录且auto-view-all=nil→退回单关联原逻辑
;;  双关联关闭时: 始终走单关联原逻辑
;;  返回 'exit(退出程序) / 'back(返回界面)
;; ============================================================
(defun wtf:locate-keyword (keyword auto-view-all / key-result)
  (if (= *wtf-dual-enabled* "1")
    (progn
      (setq key-result (wtf:dual-locate keyword auto-view-all))
      (if (null key-result)
        (setq key-result (wtf:locate-single keyword))
      )
      key-result
    )
    (wtf:locate-single keyword)
  )
)

;; 单关联定位(原逻辑): 定位到最后选择或第一个匹配结果，空格/右键返回
;; 返回 'exit 或 'back
(defun wtf:locate-single (keyword / last-sel item key-result)
  (setq last-sel (wtf:get-last-selection keyword))
  (if last-sel
    (setq item (wtf:find-result-by-handle last-sel))
    (setq item nil)
  )
  ;; 无上次记录时跳转到第一个匹配结果
  (if (not item)
    (if (and *wtf-results* (> (length *wtf-results*) 0))
      (setq item (nth 0 *wtf-results*))
    )
  )
  (if item
    (progn
      (wtf:zoom-to-text item nil nil)
      ;; 记录当前关键词的最后选择(保存实体句柄)
      (wtf:save-selection keyword (nth 4 item))
      ;; 保存数据到图纸
      (wtf:save-data)
      ;; 等待按键(启用标记时可输入数字打标记)
      (setq key-result (wtf:wait-for-key keyword nil 0))
      (if (eq key-result 'exit) 'exit 'back)
    )
    'back
  )
)

;; 双关联定位: 关键词有双关联记录时定位+空格切换/右键返回，记住最后停留目标
;; auto-view-all: 无记录时是否自动进入看全部模式(该模式支持左键定义双关联目标)
;; 返回 'exit / 'back / nil(无记录且未进入看全部，由调用方退化为单关联)
(defun wtf:dual-locate (keyword auto-view-all / rec d1 d2 d-items start-idx item key-result)
  (setq rec (wtf:get-dual-selection keyword))
  (setq d-items nil)
  (if rec
    (progn
      ;; 按句柄找回两个目标实体(失效的句柄跳过)
      (setq d1 (wtf:dual-find-item (nth 0 rec)))
      (if d1 (setq d-items (append d-items (list d1))))
      (setq d2 (wtf:dual-find-item (nth 1 rec)))
      (if d2 (setq d-items (append d-items (list d2))))
    )
  )
  (cond
    (d-items
      ;; 有有效目标: 从最后停留的目标开始(无记录或仅1个目标时从目标1开始)
      (setq start-idx
        (if (and (= (nth 2 rec) 2) d2) 1 0)
      )
      (setq item (nth start-idx d-items))
      (wtf:zoom-to-text item nil nil)
      ;; 等待按键: 空格在两目标间切换，右键返回
      (setq key-result (wtf:wait-for-key keyword d-items start-idx))
      (if (eq key-result 'exit) 'exit 'back)
    )
    ((and auto-view-all (= *wtf-dual-enabled* "1"))
      ;; 无双关联记录: 自动进入看全部模式(支持左键定义双关联目标)
      (wtf:view-all-results keyword)
      'back
    )
    (t nil)
  )
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
;;  标记定义辅助: 从标记定义框读取当前标记列表(trim后去除空项)
;; ============================================================
(defun wtf:mark-current-list ( / input)
  (setq input (get_tile "mark_input"))
  (vl-remove "" (mapcar 'wtf:trim-string (wtf:split-string input "&")))
)

;; ============================================================
;;  增加标记: 嵌套对话框输入新标记(用&分隔)，合并到标记定义框
;;  从标记定义对话框内嵌套调用，复用已加载的 dcl_id
;; ============================================================
(defun wtf:mark-add-dialog (dcl_id / add-input add-ret new-marks cur-marks added)
  (if (not (new_dialog "wtf_mark_add" dcl_id))
    (princ "\n[WTF] 增加标记对话框加载失败")
    (progn
      (setq add-input "")
      (set_tile "mark_add_input" "")
      (set_tile "mark_add_status" "输入新标记，多个用&分隔")
      (mode_tile "mark_add_input" 2)
      (action_tile "accept" "(setq add-input (get_tile \"mark_add_input\")) (done_dialog 1)")
      (action_tile "cancel" "(done_dialog 0)")
      (setq add-ret (start_dialog))
      (if (= add-ret 1)
        (progn
          (setq new-marks (vl-remove "" (mapcar 'wtf:trim-string (wtf:split-string add-input "&"))))
          (if (null new-marks)
            (set_tile "mark_status" "未输入有效标记，未增加")
            (progn
              (setq cur-marks (wtf:mark-current-list))
              (setq added 0)
              (foreach mk new-marks
                (if (not (member mk cur-marks))
                  (progn
                    (setq cur-marks (append cur-marks (list mk)))
                    (setq added (1+ added))
                  )
                )
              )
              (set_tile "mark_input" (wtf:join-strings cur-marks "&"))
              (set_tile "mark_status" (strcat "增加 " (itoa added)
                                               " 个标记，当前共 " (itoa (length cur-marks)) " 个"))
            )
          )
        )
      )
    )
  )
)

;; ============================================================
;;  减少标记: 动态生成DCL列出当前标记，每个标记后附删除按钮，
;;  点击删除即时移除，点确定后按格式回填标记定义框
;; ============================================================
;; 检查指定标记名是否被关键词标记应用，返回应用该标记的关键词数量
;; 通过 *wtf-keyword-marks* (("关键词" 序号...) ...) 与 *wtf-mark-defs* 序号对应
(defun wtf:mark-in-use-count (mark-name / i idx cnt mk pair)
  (setq idx nil)
  (setq cnt 0)
  (setq i 1)
  (foreach mk *wtf-mark-defs*
    (if (= mk mark-name) (setq idx i))
    (setq i (1+ i))
  )
  (if idx
    (foreach pair *wtf-keyword-marks*
      (if (member idx (cdr pair))
        (setq cnt (1+ cnt))
      )
    )
  )
  cnt
)

;; 从列表中移除第 n 个元素(0起)
(defun wtf:remove-nth (lst n / result i)
  (setq result nil)
  (setq i 0)
  (foreach item lst
    (if (/= i n)
      (setq result (append result (list item)))
    )
    (setq i (1+ i))
  )
  result
)
(defun wtf:write-mark-del-dcl (marks / f i mk)
  (if (findfile *wtf-mark-del-dcl-file*)
    (vl-file-delete *wtf-mark-del-dcl-file*)
  )
  (setq f (open *wtf-mark-del-dcl-file* "w"))
  (write-line "wtf_mark_del : dialog {" f)
  (write-line "  label = \"减少标记\";" f)
  (write-line "  : column {" f)
  (setq i 0)
  (foreach mk marks
    (write-line (strcat "    : row { : text { label = \"" (itoa (1+ i)) ". " mk
                        "\"; width = 30; alignment = left; }"
                        " : button { key = \"del_btn_" (itoa i)
                        "\"; label = \"删除\"; width = 8; } }") f)
    (setq i (1+ i))
  )
  (write-line "  }" f)
  (write-line "  : text { key = \"del_status\"; label = \"点击删除移除标记，点确定后生效\"; width = 44; }" f)
  (write-line "  : row {" f)
  (write-line "    : button { key = \"del_ok\"; label = \"确定\"; width = 10; is_default = true; }" f)
  (write-line "    : button { key = \"del_cancel\"; label = \"取消\"; width = 10; is_cancel = true; }" f)
  (write-line "  }" f)
  (write-line "}" f)
  ;; 删除标记前确认对话框(被关键词应用时弹出)
  (write-line "wtf_mark_del_confirm : dialog {" f)
  (write-line "  label = \"确认删除标记\";" f)
  (write-line "  : text { key = \"confirm_msg\"; label = \"\"; width = 52; alignment = left; }" f)
  (write-line "  : row {" f)
  (write-line "    : button { key = \"confirm_yes\"; label = \"继续删除\"; width = 12; is_default = true; }" f)
  (write-line "    : button { key = \"confirm_no\"; label = \"取消\"; width = 12; is_cancel = true; }" f)
  (write-line "  }" f)
  (write-line "}" f)
  (close f)
  *wtf-mark-del-dcl-file*
)

(defun wtf:mark-del-dialog ( / marks del-file dcl_id del-ret del-idx i result del-mark in-use cfm-id cfm-ret)
  (setq marks (wtf:mark-current-list))
  (if (null marks)
    (set_tile "mark_status" "当前没有标记可删除")
    (progn
      (setq result nil)
      (setq del-ret 999)
      (while (= del-ret 999)
        ;; 动态生成对话框
        (wtf:write-mark-del-dcl marks)
        (setq dcl_id (load_dialog *wtf-mark-del-dcl-file*))
        (if (not (new_dialog "wtf_mark_del" dcl_id))
          (progn
            (princ "\n[WTF] 减少标记对话框加载失败")
            (setq del-ret 0)
          )
          (progn
            ;; 为每个删除按钮绑定动作
            (setq i 0)
            (foreach mk marks
              (action_tile (strcat "del_btn_" (itoa i))
                           (strcat "(setq del-idx " (itoa i) ") (done_dialog 2)"))
              (setq i (1+ i))
            )
            (action_tile "del_ok" "(done_dialog 1)")
            (action_tile "del_cancel" "(done_dialog 0)")
            (setq del-ret (start_dialog))
            (unload_dialog dcl_id)
            (cond
              ((= del-ret 2)
                ;; 点击删除: 检查该标记是否被关键词应用，有应用则弹出确认
                (setq del-mark (nth del-idx marks))
                (setq in-use (if del-mark (wtf:mark-in-use-count del-mark) 0))
                (if (> in-use 0)
                  (progn
                    ;; 被应用，重新加载DCL并弹出确认对话框(嵌套)
                    (setq cfm-id (load_dialog *wtf-mark-del-dcl-file*))
                    (if (not (new_dialog "wtf_mark_del_confirm" cfm-id))
                      (princ "\n[WTF] 删除确认对话框加载失败")
                      (progn
                        (set_tile "confirm_msg"
                          (strcat "标记 \"" del-mark "\" 已被 " (itoa in-use)
                                  " 个关键词应用。\n删除后这些关键词将失去该标记，确定删除?"))
                        (action_tile "confirm_yes" "(done_dialog 1)")
                        (action_tile "confirm_no" "(done_dialog 0)")
                        (setq cfm-ret (start_dialog))
                        (if (= cfm-ret 1)
                          (setq marks (wtf:remove-nth marks del-idx))
                          nil
                        )
                      )
                    )
                    (unload_dialog cfm-id)
                  )
                  ;; 未被应用，直接删除
                  (setq marks (wtf:remove-nth marks del-idx))
                )
                (if (null marks)
                  (progn
                    (set_tile "mark_input" "")
                    (set_tile "mark_status" "所有标记已删除")
                    (setq del-ret 0)
                  )
                  (setq del-ret 999)
                )
              )
              ((= del-ret 1)
                ;; 确定: 按格式回填标记定义框
                (set_tile "mark_input" (wtf:join-strings marks "&"))
                (set_tile "mark_status" (strcat "删除后剩余 " (itoa (length marks)) " 个标记"))
              )
              (t
                ;; 取消: 不做任何修改
                (set_tile "mark_status" "已取消，标记未修改")
              )
            )
          )
        )
      )
    )
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
      (action_tile "btn_mark_add" "(wtf:mark-add-dialog dcl_id)")
      (action_tile "btn_mark_del" "(wtf:mark-del-dialog)")
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
        (wtf:sync-backup-marks)
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

;; ============================================================
;;  滚动列表框使目标项显示在可视区中间位置
;;  与主界面关键词列表返回时的居中逻辑一致:
;;  先选中"目标项+半屏"的锚点项触发列表向下滚动(锚点出现在可视区底部)，
;;  再选中目标项(此时目标项已在中间位置)
;;  key: 列表框 key; idx: 目标项索引; len: 列表项总数; half: 半屏可见项数
;; ============================================================
(defun wtf:center-list (key idx len half / scroll-idx)
  (if (and idx (>= idx 0) (< idx len))
    (progn
      (setq scroll-idx (+ idx half))
      (if (>= scroll-idx len)
        (setq scroll-idx (1- len)))
      (if (and (/= scroll-idx idx) (>= scroll-idx 0) (< scroll-idx len))
        (set_tile key (itoa scroll-idx)))
      (set_tile key (itoa idx))
    )
  )
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
              ;; 确认: 整体替换数量映射并同步到当前备份项
              (setq *wtf-keyword-qty* pairs)
              (wtf:sync-backup-marks)
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
;;  点击列表项可定位到该关键词(与主界面关键词列表行为一致)，返回码8
;; ============================================================
(defun wtf:mark-stat-dialog ( / dcl_id i ret)
  ;; 重置当前显示列表和待定位关键词，保留 *wtf-stat-last-idx* 和 *wtf-stat-last-sel*
  ;; (定位返回后重新打开时恢复上次标记列表及滚动位置)
  (setq *wtf-stat-current* nil)
  (setq *wtf-stat-locate-kw* nil)
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
      (action_tile "stat_list"
        "(setq stat-sel-str (get_tile \"stat_list\"))
         (if (/= stat-sel-str \"\")
           (progn
             (setq stat-sel-idx (atoi stat-sel-str))
             (setq *wtf-stat-last-sel* stat-sel-idx)
             (if (and *wtf-stat-current*
                      (>= stat-sel-idx 0)
                      (< stat-sel-idx (length *wtf-stat-current*)))
               (progn
                 (setq *wtf-stat-locate-kw* (nth stat-sel-idx *wtf-stat-current*))
                 (done_dialog 8))
                  (setq *wtf-stat-locate-kw* nil))))")
      (action_tile "btn_qty_def" "(wtf:qty-define-dialog dcl_id)")
      (action_tile "btn_stat_copy" "(wtf:stat-copy)")
      (action_tile "btn_stat_close" "(done_dialog 0)")
      ;; 定位返回后重新打开对话框: 自动恢复上次标记对应的关键词列表及滚动位置
      (if (and *wtf-stat-last-idx*
               (> *wtf-stat-last-idx* 0)
               (<= *wtf-stat-last-idx* (length *wtf-mark-defs*)))
        (progn
          (wtf:stat-show *wtf-stat-last-idx*)
          ;; 恢复列表选中项并滚动到中间位置(与主界面关键词列表行为一致)
          ;; stat_list 高度15，半屏=7
          (wtf:center-list "stat_list" *wtf-stat-last-sel*
                           (length *wtf-stat-current*) 7)
        )
        (set_tile "stat_status" "点击上方标记按钮查看对应关键词，点击列表项可定位")
      )
      (setq ret (start_dialog))
    )
  )
  (unload_dialog dcl_id)
  ret
)

;; ============================================================
;;  定位比例设置
;;  独立对话框(自行加载DCL)。选择后更新 *wtf-zoom-denominator* 并保存，
;;  主循环随后重开主界面，按钮标签按最新比例重绘。
;; ============================================================
;; 读取当前选中的比例分母(DCL对话框仍激活时调用)
(defun wtf:zoom-selected-val ( / v)
  (setq v 30)
  (if (= (get_tile "z1_20") "1") (setq v 20))
  (if (= (get_tile "z1_30") "1") (setq v 30))
  (if (= (get_tile "z1_40") "1") (setq v 40))
  (if (= (get_tile "z1_50") "1") (setq v 50))
  (if (= (get_tile "z1_60") "1") (setq v 60))
  (if (= (get_tile "z1_80") "1") (setq v 80))
  (if (= (get_tile "z1_100") "1") (setq v 100))
  v
)

(defun wtf:zoom-dialog ( / dcl_id ret zoom-new)
  (wtf:write-dcl)
  (setq dcl_id (load_dialog *wtf-dcl-file*))
  (if (not (new_dialog "wtf_zoom_set" dcl_id))
    (princ "\n[WTF] 定位比例对话框加载失败")
    (progn
      ;; 预选当前比例
      (if (= *wtf-zoom-denominator* 20) (set_tile "z1_20" "1"))
      (if (= *wtf-zoom-denominator* 30) (set_tile "z1_30" "1"))
      (if (= *wtf-zoom-denominator* 40) (set_tile "z1_40" "1"))
      (if (= *wtf-zoom-denominator* 50) (set_tile "z1_50" "1"))
      (if (= *wtf-zoom-denominator* 60) (set_tile "z1_60" "1"))
      (if (= *wtf-zoom-denominator* 80) (set_tile "z1_80" "1"))
      (if (= *wtf-zoom-denominator* 100) (set_tile "z1_100" "1"))
      (set_tile "zoom_status" (strcat "当前: 1/" (itoa *wtf-zoom-denominator*)))
      (setq zoom-new *wtf-zoom-denominator*)
      ;; 在 accept 回调内(done_dialog之前)读取选中项，对话框结束后 get_tile 值不可靠
      (action_tile "accept"
        "(setq zoom-new (wtf:zoom-selected-val)) (done_dialog 1)")
      (action_tile "cancel" "(done_dialog 0)")
      (setq ret (start_dialog))
      (if (= ret 1)
        (progn
          (setq *wtf-zoom-denominator* zoom-new)
          (wtf:save-data)
          (princ (strcat "\n[WTF] 定位比例设为 1/" (itoa *wtf-zoom-denominator*)))
        )
      )
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
                kw-sel-str kw-idx last-sel stat-ret bk-sel-str bk-name)
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
        (wtf:update-keyword-status)
        ;; 显示备份列表
        (wtf:update-backup-list)

        ;; 标记/双关联功能开关初始状态
        (set_tile "tg_mark_enable" *wtf-mark-enabled*)
        (set_tile "tg_dual_link" *wtf-dual-enabled*)

        ;; 动作绑定
        (action_tile "rb_all" "(setq scope 'all)")
        (action_tile "rb_window" "(setq scope 'window)")

        (action_tile "btn_add_keywords" "(wtf:save-keyword-pos) (done_dialog 4)")
        (action_tile "btn_clear_data" "(wtf:save-keyword-pos) (done_dialog 5)")
        (action_tile "btn_mark_def" "(wtf:save-keyword-pos) (done_dialog 6)")
        (action_tile "btn_mark_stat" "(wtf:save-keyword-pos) (done_dialog 7)")
        (action_tile "tg_mark_enable"
          "(setq *wtf-mark-enabled* $value) (wtf:save-data)")
        (action_tile "tg_dual_link"
          "(setq *wtf-dual-enabled* $value) (wtf:save-data)")

        (action_tile "keyword_list"
          "(setq kw-sel-str (get_tile \"keyword_list\"))
           (if (/= kw-sel-str \"\")
             (progn
               (setq *wtf-keyword-top-idx* kw-sel-str)
               (wtf:update-keyword-status)
               (setq kw-idx (atoi kw-sel-str))
               (if (and *wtf-keyword-list* (< kw-idx (length *wtf-keyword-list*)))
                 (set_tile \"search_text\" (nth kw-idx *wtf-keyword-list*)))
               (done_dialog 3)))")

        (action_tile "backup_list"
          "(setq bk-sel-str (get_tile \"backup_list\"))
           (if (/= bk-sel-str \"\")
             (progn
               (setq bk-name (nth (atoi bk-sel-str) (mapcar 'car *wtf-backups*)))
               (if bk-name
                 (done_dialog 8))))")

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

        ;; 点击定位按钮: 返回9，由主循环弹出比例选择对话框(主界面重开重绘按钮标签)
        (action_tile "btn_locate" "(wtf:save-keyword-pos) (done_dialog 9)")

        ;; 点击看全部按钮: 返回10，对所有结果执行定位显示实心圆
        (action_tile "btn_view_all" "(wtf:save-keyword-pos) (done_dialog 10)")

        (action_tile "btn_exit" "(wtf:save-keyword-pos) (done_dialog 0)")

        ;; 焦点设到搜索框
        (mode_tile "search_text" 2)
        (setq code (start_dialog))
        (unload_dialog dcl_id)

        (cond
          ;; 批量添加关键词(从剪贴板): 列表非空时替换会覆盖已有数据，先备份再替换；列表为空则直接导入
          ((= code 4)
            (if (and *wtf-keyword-list* (> (length *wtf-keyword-list*) 0))
              (progn
                ;; 替换前先备份当前数据(以关键词数量命名)
                (wtf:backup-current)
                (wtf:load-keywords-from-clipboard)
                (wtf:save-data)
              )
              (progn
                (wtf:load-keywords-from-clipboard)
                (wtf:save-data)
              )
            )
          )
          ;; 点击备份数据: 切换加载所选备份为当前数据
          ;; 切换前检查当前数据是否有备份，无则先备份再替换
          ((= code 8)
            (if (wtf:has-backup)
              nil
              (wtf:backup-current)
            )
            (if (wtf:restore-backup bk-name)
              (progn
                (setq search-text *wtf-last-search*)
                (setq *wtf-last-search* search-text)
                (wtf:save-data)
                (wtf:do-search search-text scope)
              )
            )
          )
          ;; 清除缓存数据(需管理员密码确认)
          ((= code 5)
            (if (wtf:admin-verify)
              (progn
                (wtf:clear-data)
                (setq search-text "")
              )
              (princ "\n[WTF] 密码错误或已取消，未清除数据")
            )
          )
          ;; 标记定义
          ((= code 6)
            (wtf:mark-def-dialog)
          )
          ;; 定位比例设置(独立对话框，返回后主界面重开重绘按钮标签)
          ((= code 9)
            (wtf:zoom-dialog)
          )
          ;; 标记统计
          ((= code 7)
            (setq stat-ret (wtf:mark-stat-dialog))
            ;; 循环: 从标记统计界面点击列表项定位，返回后重新打开统计对话框
            ;; 恢复上次标记对应的关键词列表及滚动位置(从哪来回到哪)
            (while (and (= stat-ret 8) *wtf-stat-locate-kw* (/= code 0))
              (progn
                (setq search-text *wtf-stat-locate-kw*)
                (setq *wtf-last-search* search-text)
                (wtf:do-search search-text scope)
                ;; 统一定位入口(双关联开启且有关联记录时定位+空格切换，无记录不自动进看全部)
                (setq key-result (wtf:locate-keyword search-text nil))
                (if (eq key-result 'exit)
                  (setq code 0)
                )
                ;; 定位返回(空格/右键)后重新打开统计对话框，恢复上次列表状态
                (if (/= code 0)
                  (setq stat-ret (wtf:mark-stat-dialog))
                )
              )
            )
          )
          ;; 点击关键词列表项: 填充到查找框并搜索
          ;; 双关联开启: 有双关联记录→定位+空格切换/右键返回；无记录→自动进入看全部模式定义目标
          ;; 双关联关闭: 优先跳转到上次定位的位置(句柄映射，跨会话稳定)，无记录时跳转第一个匹配结果
          ((= code 3)
            (setq kw-idx (atoi kw-sel-str))
            (if (and *wtf-keyword-list* (< kw-idx (length *wtf-keyword-list*)))
              (progn
                (setq search-text (nth kw-idx *wtf-keyword-list*))
                (setq *wtf-last-search* search-text)
                (wtf:do-search search-text scope)
                ;; 统一定位入口(双关联开启时无记录自动进看全部定义目标)
                (setq key-result (wtf:locate-keyword search-text T))
                (if (eq key-result 'exit)
                  (setq code 0)
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
                (wtf:zoom-to-text item nil nil)
                ;; 双关联开启时不写单关联映射(由双关联记录接管)
                (if (/= *wtf-dual-enabled* "1")
                  (wtf:save-selection search-text (nth 4 item))
                )
                ;; 保存数据到图纸
                (wtf:save-data)
                ;; 等待按键(启用标记时可输入数字打标记)
                (setq key-result (wtf:wait-for-key search-text nil 0))
                (if (eq key-result 'exit)
                  (setq code 0)
                  ;; 空格返回: code保持非0，继续循环重新显示对话框
                )
              )
              (princ "\n[WTF] 无效的选择")
            )
          )
          ;; 看全部: 对所有结果执行定位，显示实心圆
          ;; 双关联开启: 左键关联当前目标(最多2个)，右键返回；关闭时右键确定选择建立单关联映射
          ((= code 10)
            (setq item (wtf:view-all-results search-text))
            ;; 如果用户选择了结果，建立映射关系
            (if item
              (progn
                ;; 记录当前关键词的最后选择(保存实体句柄)
                (wtf:save-selection search-text (nth 4 item))
                ;; 保存数据到图纸
                (wtf:save-data)
                (princ (strcat "\n[WTF] 已将【" search-text "】映射到选择的结果"))
              )
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
