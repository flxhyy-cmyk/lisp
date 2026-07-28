;@name 查找定位文本
;@group 文本编辑
;@desc 查找图纸中的文字并快速定位。支持全图查找和框选范围查找，点击结果可缩放定位到文字位置(约1/30屏)，按空格返回列表，按ESC退出。全图查找支持缓存加速，支持从剪贴板批量导入关键词
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

;; ============================================================
;;  生成 DCL 对话框
;; ============================================================
(defun wtf:write-dcl ( / f)
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
(defun wtf:update-keyword-list ( / idx kw-idx half-vis scroll-idx kw-len)
  (setq idx 1)
  (start_list "keyword_list")
  (foreach kw *wtf-keyword-list*
    (add_list (strcat (itoa idx) ". " kw))
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
      (if (null *wtf-keyword-list*) (setq *wtf-keyword-list* nil))
      (if (null *wtf-last-selection*) (setq *wtf-last-selection* nil))
      (if (null *wtf-last-search*) (setq *wtf-last-search* ""))
      (if (null *wtf-keyword-top-idx*) (setq *wtf-keyword-top-idx* ""))
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
;;  返回 'back 或 'exit
;; ============================================================
(defun wtf:wait-for-key ( / input done char result)
  (setq done nil)
  (setq result nil)

  (princ "\n[WTF] 已定位到文字")
  (princ "\n按【空格】返回列表选择下一个，按【ESC】退出")

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
              )
            )
          )
        )
      )
    )
  )
  result
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

        ;; 动作绑定
        (action_tile "rb_all" "(setq scope 'all)")
        (action_tile "rb_window" "(setq scope 'window)")

        (action_tile "btn_add_keywords" "(wtf:save-keyword-pos) (done_dialog 4)")
        (action_tile "btn_clear_data" "(wtf:save-keyword-pos) (done_dialog 5)")

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
                    ;; 等待按键
                    (setq key-result (wtf:wait-for-key))
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
                ;; 等待按键
                (setq key-result (wtf:wait-for-key))
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
