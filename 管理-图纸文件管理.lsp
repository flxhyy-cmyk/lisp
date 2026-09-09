;@name 打开DWG文件
;@group 文件工具
;@desc 从指定文件夹选择DWG文件，双击或确定打开文件。WGG关闭所有WDG打开的文件。
;@order 1

;; 加载 Visual LISP 扩展
(vl-load-com)

;; ============================================================
;;  全局变量：记录通过WDG打开的文档完整路径
;; ============================================================
(setq wdg_opened_docs nil)

;; ============================================================
;;  文件夹路径持久化（跨会话保存）
;; ============================================================

;; 保存文件夹路径到环境变量（跨会话持久）
(defun save-folder-wdg (folder)
  (setenv "WDG_LAST_FOLDER" folder)
)

;; 读取上次保存的文件夹路径，没有则返回nil
(defun load-folder-wdg ( / saved)
  (setq saved (getenv "WDG_LAST_FOLDER"))
  (if (and saved (/= saved ""))
    saved
    nil
  )
)


;; ============================================================
;;  收藏夹持久化（分段存储，防止单键超 REG_SZ 上限 32767 字节）
;;  每段最多 8000 字节（约 80~100 个路径），自动分片写入 WDG_FAV_0..N
;; ============================================================

;; 每段最大字节数（留足余量，REG_SZ 上限 32767）
(setq wdg_seg_max 8000)

;; 保存收藏夹列表到注册表（分段存储）
(defun wdg-save-favorites (fav-list / joined total seg-count idx chunk key)
  (setq joined (wdg-join-with-sep fav-list "|"))
  (setq total (strlen joined))
  ;; 计算段数
  (setq seg-count 0)
  (if (> total 0)
    (progn
      (setq seg-count (/ (+ total wdg_seg_max -1) wdg_seg_max))
      ;; 写入段数
      (vl-registry-write
        "HKEY_CURRENT_USER\\Software\\WDG_Favorites"
        "WDG_FAV_COUNT"
        (itoa seg-count)
      )
      ;; 按段写入
      (setq idx 0)
      (while (< idx seg-count)
        (setq chunk (substr joined (+ 1 (* idx wdg_seg_max)) wdg_seg_max))
        (setq key (strcat "WDG_FAV_" (itoa idx)))
        (vl-registry-write
          "HKEY_CURRENT_USER\\Software\\WDG_Favorites"
          key
          chunk
        )
        (setq idx (1+ idx))
      )
    )
    ;; 空列表：段数写 0
    (vl-registry-write
      "HKEY_CURRENT_USER\\Software\\WDG_Favorites"
      "WDG_FAV_COUNT"
      "0"
    )
  )
)

;; 从注册表加载收藏夹列表（分段读取并拼接，兼容旧版单键格式）
(defun wdg-load-favorites ( / seg-count joined idx key chunk)
  ;; 先尝试分段格式
  (setq seg-count (vl-registry-read
    "HKEY_CURRENT_USER\\Software\\WDG_Favorites"
    "WDG_FAV_COUNT"
  ))
  (if seg-count
    (progn
      (setq seg-count (atoi seg-count))
      (if (> seg-count 0)
        (progn
          (setq joined "")
          (setq idx 0)
          (while (< idx seg-count)
            (setq key (strcat "WDG_FAV_" (itoa idx)))
            (setq chunk (vl-registry-read
              "HKEY_CURRENT_USER\\Software\\WDG_Favorites"
              key
            ))
            (if chunk
              (setq joined (strcat joined chunk))
            )
            (setq idx (1+ idx))
          )
          (if (/= joined "")
            (wdg-split-by-sep joined "|")
            '()
          )
        )
        '()
      )
    )
    ;; 回退：尝试旧版单键格式 WDG_FAVORITES
    (progn
      (setq joined (vl-registry-read
        "HKEY_CURRENT_USER\\Software\\WDG_Favorites"
        "WDG_FAVORITES"
      ))
      (if (and joined (/= joined ""))
        (wdg-split-by-sep joined "|")
        '()
      )
    )
  )
)

;; 用分隔符连接字符串列表为单字符串（如 ("a" "b") "|" → "a|b"）
(defun wdg-join-with-sep (lst sep / result)
  (if (not lst) ""
    (progn
      (setq result (car lst))
      (setq lst (cdr lst))
      (while lst
        (setq result (strcat result sep (car lst)))
        (setq lst (cdr lst))
      )
      result
    )
  )
)

;; 按分隔符切分字符串为列表（如 "a|b" "|" → ("a" "b")）
(defun wdg-split-by-sep (str sep / result sep-len total i ch part done)
  (setq result '())
  (if (or (not str) (= str ""))
    result
    (progn
      (setq sep-len (strlen sep))
      (setq total (strlen str))
      (setq i 1)
      (setq part "")
      (setq done nil)
      (while (and (not done) (<= i total))
        (if (and (<= (+ i (- sep-len 1)) total)
                 (= (substr str i sep-len) sep))
          (progn
            (setq result (append result (list part)))
            (setq part "")
            (setq i (+ i sep-len))
          )
          (progn
            (setq part (strcat part (substr str i 1)))
            (setq i (1+ i))
          )
        )
      )
      (setq result (append result (list part)))
      result
    )
  )
)

;; 获取收藏夹中文件夹的显示名称（取最后一级文件夹名）
(defun wdg-fav-display-name (folder / parts last-part)
  ;; 去掉末尾反斜杠
  (setq folder (vl-string-right-trim "\\" folder))
  ;; 取最后一个 \ 之后的部分
  (setq last-part folder)
  (while (vl-string-search "\\" last-part)
    (setq last-part (substr last-part (+ (vl-string-search "\\" last-part) 2)))
  )
  (if (or (not last-part) (= last-part ""))
    folder
    last-part
  )
)

;; 刷新收藏夹列表显示（带文件夹图标）
(defun wdg-refresh-fav-list ( / fav-list)
  (setq fav-list (wdg-load-favorites))
  (setq wdg_favorites fav-list)
  (start_list "fav_list")
  (foreach f fav-list
    (add_list (strcat "  \004  " (wdg-fav-display-name f) "  "))
  )
  (end_list)
)

;; ============================================================
;;  DWG文件缓存（收藏夹文件夹及子文件夹内所有DWG文件名）
;; ============================================================

;; 搜索模式标记
(setq wdg_is_searching nil)
;; 搜索结果列表：完整 DWG 路径字符串列表
(setq wdg_search_results nil)

;; 获取缓存文件路径
(defun wdg-get-cache-path ()
  (strcat (getvar "TEMPPREFIX") "wdg_dwg_cache.txt")
)

;; 递归扫描文件夹中所有DWG文件（返回完整路径列表）
(defun wdg-scan-folder-recursive (folder / result files f full-path subfolders sub sub-path)
  (setq result '())
  (if (vl-file-directory-p folder)
    (progn
      (if (/= (substr folder (strlen folder)) "\\")
        (setq folder (strcat folder "\\"))
      )
      ;; 收集当前文件夹的DWG文件（仅文件名）
      (setq files (vl-directory-files folder "*.dwg" 1))
      (foreach f files
        (setq full-path (strcat folder f))
        (setq result (cons full-path result))
      )
      ;; 递归子文件夹
      (setq subfolders (vl-directory-files folder "*" -1))
      (foreach sub subfolders
        (if (and (/= sub ".") (/= sub ".."))
          (progn
            (setq sub-path (strcat folder sub))
            (if (vl-file-directory-p sub-path)
              (setq result (append result
                (wdg-scan-folder-recursive (strcat sub-path "\\"))))
            )
          )
        )
      )
    )
  )
  result
)

;; 构建缓存：扫描所有收藏夹文件夹及子文件夹
(defun wdg-build-cache ( / fav-list all-dwgs total cache-path fh)
  (setq fav-list (wdg-load-favorites))
  (if (not fav-list)
    (set_tile "status_text" "收藏夹为空，请先添加收藏夹！")
    (progn
      (setq all-dwgs '())
      (setq total 0)
      (foreach fav fav-list
        (if (vl-file-directory-p fav)
          (progn
            (setq sub-dwgs (wdg-scan-folder-recursive fav))
            (setq all-dwgs (append all-dwgs sub-dwgs))
            (setq total (+ total (length sub-dwgs)))
          )
        )
      )
      ;; 排序（按完整路径不区分大小写）
      (setq all-dwgs (vl-sort all-dwgs
        (function (lambda (a b) (< (strcase a) (strcase b))))))
      ;; 写入缓存文件
      (setq cache-path (wdg-get-cache-path))
      (setq fh (open cache-path "w"))
      (foreach path all-dwgs
        (write-line path fh)
      )
      (close fh)
      (set_tile "status_text"
        (strcat "缓存已更新：" (itoa total) " 个DWG（来自 "
                (itoa (length fav-list)) " 个收藏夹）"))
    )
  )
)

;; 加载缓存文件，返回完整路径列表
(defun wdg-load-cache ( / cache-path fh line result)
  (setq cache-path (wdg-get-cache-path))
  (setq result '())
  (if (findfile cache-path)
    (progn
      (setq fh (open cache-path "r"))
      (while (setq line (read-line fh))
        (if (/= line "")
          (setq result (cons line result))
        )
      )
      (close fh)
      (reverse result)
    )
    '()
  )
)

;; 执行搜索：根据搜索文本筛选缓存中的文件
(defun wdg-do-search (search-text / cache-all matches display-name path)
  (if (or (not search-text) (= search-text ""))
    ;; 搜索为空：恢复原始目录显示
    (progn
      (setq wdg_is_searching nil)
      (setq wdg_search_results nil)
      (wdg_refresh_dir_list (get_tile "folder_edit"))
      (if (and wdg_sub_folder (vl-file-directory-p wdg_sub_folder))
        (wdg_refresh_sub_list wdg_sub_folder)
      )
    )
    ;; 有搜索文本：过滤缓存
    (progn
      (setq wdg_is_searching T)
      (setq cache-all (wdg-load-cache))
      (if (not cache-all)
        (progn
          (start_list "dir_list")
          (end_list)
          (set_tile "status_text" "缓存为空，请先点击\"缓存\"按钮生成缓存")
          (start_list "sub_file_list")
          (end_list)
        )
        (progn
          (setq search-text (strcase search-text))
          (setq matches '())
          (foreach path cache-all
            ;; 提取文件名（不含扩展名）用于匹配
            (setq display-name (vl-filename-base path))
            ;; 文件名或完整路径匹配搜索文本（不区分大小写）
            (if (or (vl-string-search search-text (strcase display-name))
                    (vl-string-search search-text (strcase path)))
              (setq matches (cons path matches))
            )
          )
          (setq matches (reverse matches))
          (setq wdg_search_results matches)
          (start_list "dir_list")
          (foreach path matches
            (add_list (strcat "  " (vl-filename-base path) ".dwg"))
          )
          (end_list)
          (set_tile "status_text"
            (strcat "搜索 \"" (get_tile "search_edit") "\"：找到 "
                    (itoa (length matches)) " 个文件"))
          (start_list "sub_file_list")
          (end_list)
        )
      )
    )
  )
)

;; ============================================================
;;  安全打开DWG文件（绕过command函数对含空格路径的解析问题）
;; ============================================================

;; 获取当前文档标识：已保存返回完整路径，未保存返回 *UNSAVED*:DrawingX.dwg
(defun wdg-current-dwg-path ( / name)
  (setq name (getvar "DWGNAME"))
  (cond
    ((or (not name) (= name "")) nil)
    ((wcmatch (strcase name) "DRAWING*.DWG")
     (strcat "*UNSAVED*:" name))
    (T (strcat (getvar "DWGPREFIX") name))
  )
)

;; 写入切换对文件，供 WDV 使用
;; 格式: source_id|dest_path   source_id 可为完整路径或 *UNSAVED*:DrawingX.dwg
(defun wdg-write-switch-pair (source-path dest-path / pair-path fh)
  (if dest-path
    (progn
      (if (not source-path)
        (setq source-path (strcat "*UNSAVED*:" (getvar "DWGNAME")))
      )
      (setq pair-path (strcat (getvar "TEMPPREFIX") "wdg_switch_pair.txt"))
      (setq fh (open pair-path "w"))
      (write-line (strcat source-path "|" dest-path) fh)
      (close fh)
    )
  )
)

;; 打开DWG文件，优先 vla-Open（返回 COM 对象，WGG 可直接关闭），
;; 失败则回退到脚本方式（适用于含空格和中文的深层路径）
;; 打开前记录源路径→目标路径切换对（供 WDV 使用）
(defun wdg-open-dwg-safe (filepath / new-doc scr-path scr-file
                                    docs doc dname safe-name src-path)
  (if (not (findfile filepath))
    (princ (strcat "\n[WDG] 错误：文件不存在 - " filepath))
    (progn
      (princ (strcat "\n[WDG] 正在打开文件: " filepath))
      ;; 记录切换对：源文件(当前) → 目标文件
      (setq src-path (wdg-current-dwg-path))
      (wdg-write-switch-pair src-path filepath)
      ;; 优先用 vla-Open（返回 COM 对象，WGG 可直接 vla-Close）
      (setq new-doc
        (vl-catch-all-apply 'vla-Open
          (list (vla-get-Documents (vlax-get-acad-object)) filepath)))
      (if (vl-catch-all-error-p new-doc)
        ;; vla-Open 失败（深层中文路径/空格），回退到脚本方式
        (progn
          (setq scr-path (strcat (getvar "TEMPPREFIX") "wdg_open.scr"))
          (setq scr-file (open scr-path "w"))
          (write-line (strcat "_.OPEN \"" filepath "\"") scr-file)
          (close scr-file)
          (command "_.SCRIPT" scr-path)
          ;; 脚本执行后，按文件名查找新打开的文档对象
          (setq safe-name
            (strcase (strcat (vl-filename-base filepath)
                             (vl-filename-extension filepath))))
          (setq new-doc nil)
          (setq docs (vla-get-Documents (vlax-get-acad-object)))
          (vlax-for doc docs
            (if (= (strcase (vl-catch-all-apply 'vla-get-Name (list doc)))
                   safe-name)
              (setq new-doc doc)
            )
          )
        )
      )
      ;; 记录 COM 文档对象（供 WGG 关闭使用）
      (if (and new-doc (not (vl-catch-all-error-p new-doc)))
        (setq wdg_opened_docs (cons new-doc wdg_opened_docs))
      )
    )
  )
)

;; ============================================================
;;  临时DCL文件管理
;; ============================================================

;; 创建临时DCL文件，返回文件路径（固定文件名，已存在则复用）
(defun create-temp-dcl-wdg ( / tmp-path dcl-lines fh)
  (setq tmp-path (strcat (getvar "TEMPPREFIX") "wdg_dialog.dcl"))
  ;; 删除旧文件，确保每次都用最新的DCL定义
  (if (findfile tmp-path)
    (vl-file-delete tmp-path)
  )
  (setq dcl-lines (list
        "dwg_insert : dialog {"
        "  label = \"WDG - DWG文件管理\";"
        "  initial_focus = \"dir_list\";"
        ""
        "  : row {"
        "    : column {"
        "      : row {"
        "        : edit_box {"
        "          label = \"文件夹:\";"
        "          key = \"folder_edit\";"
        "          edit_width = 95;"
        "          allow_accept = false;"
        "        }"
        "        : button {"
        "          label = \"浏览...\";"
        "          key = \"browse_btn\";"
        "          fixed_width = true;"
        "          width = 8;"
        "        }"
        "        : button {"
        "          label = \"打开\";"
        "          key = \"open_folder_btn\";"
        "          fixed_width = true;"
        "          width = 6;"
        "        }"
        "        : button {"
        "          label = \"保存\";"
        "          key = \"save_btn\";"
        "          fixed_width = true;"
        "          width = 6;"
        "        }"
        "        : button {"
        "          label = \"缓存\";"
        "          key = \"cache_btn\";"
        "          fixed_width = true;"
        "          width = 6;"
        "        }"
        "      }"
        ""
        "      : row {"
        "        : edit_box {"
        "          label = \"搜索:\";"
        "          key = \"search_edit\";"
        "          edit_width = 95;"
        "          allow_accept = false;"
        "        }"
        "      }"
        ""
        "      : row {"
        "        : list_box {"
        "          label = \"子文件夹 / DWG文件:\";"
        "          key = \"dir_list\";"
        "          height = 35;"
        "          width = 55;"
        "          multiple_select = false;"
        "          allow_accept = true;"
        "        }"
        "        : list_box {"
        "          label = \"子文件夹中的DWG文件 (双击打开):\";"
        "          key = \"sub_file_list\";"
        "          height = 35;"
        "          width = 55;"
        "          multiple_select = false;"
        "          allow_accept = true;"
        "        }"
        "      }"
        ""
        "      : text {"
        "        label = \"双击或选择文件后点确定打开文件\";"
        "        key = \"status_text\";"
        "        alignment = left;"
        "      }"
        "    }"
        ""
        "    : column {"
        "      : text {"
        "        label = \"收藏夹\";"
        "        alignment = centered;"
        "        width = 28;"
        "      }"
        "      : list_box {"
        "        key = \"fav_list\";"
        "        height = 30;"
        "        width = 28;"
        "        multiple_select = false;"
        "      }"
        "      : spacer { height = 1; }"
        "      : button {"
        "        label = \"加入收藏\";"
        "        key = \"fav_add_btn\";"
        "        fixed_width = true;"
        "        width = 26;"
        "      }"
        "      : spacer { height = 0; }"
        "      : button {"
        "        label = \"删除选中\";"
        "        key = \"fav_del_btn\";"
        "        fixed_width = true;"
        "        width = 26;"
        "      }"
        "      : spacer { height = 0; }"
        "      : button {"
        "        label = \"导出配置\";"
        "        key = \"export_btn\";"
        "        fixed_width = true;"
        "        width = 26;"
        "      }"
        "    }"
        "  }"
        ""
        "  : row {"
        "    : button {"
        "      label = \"确定(打开)\";"
        "      key = \"accept\";"
        "      is_default = false;"
        "      width = 14;"
        "    }"
        "    : button {"
        "      label = \"取消\";"
        "      key = \"cancel\";"
        "      is_cancel = true;"
        "      width = 12;"
        "    }"
        "  }"
        "}"
      ))
      (setq fh (open tmp-path "w"))
      (foreach line dcl-lines (write-line line fh))
      (close fh)
      tmp-path
)



;; ============================================================
;;  文件夹浏览
;; ============================================================

(defun browse-folder-wdg ( / shell-app folder-obj folder-path result)
  (setq shell-app (vla-getinterfaceobject (vlax-get-acad-object) "Shell.Application"))
  (if shell-app
    (progn
      ;; 打开文件夹选择对话框，参数512=BIF_NEWDIALOGSTYLE显示新建文件夹按钮
      (setq folder-obj (vlax-invoke-method shell-app 'BrowseForFolder 0 "请选择文件夹" 512))
      (if folder-obj
        (progn
          (setq folder-path (vlax-get-property (vlax-get-property folder-obj 'Self) 'Path))
          (vlax-release-object folder-obj)
          (if (and folder-path (/= folder-path ""))
            (progn
              (if (/= (substr folder-path (strlen folder-path)) "\\")
                (setq folder-path (strcat folder-path "\\"))
              )
              (setq result folder-path)
            )
          )
        )
      )
      (vlax-release-object shell-app)
    )
  )
  result
)

;; ============================================================
;;  打开文件夹（用资源管理器）
;; ============================================================

(defun wdg-open-folder (folder)
  (if (and folder (/= folder "") (vl-file-directory-p folder))
    (progn
      ;; 用 explorer 打开文件夹，路径含空格需加引号
      (startapp (strcat "explorer \"" folder "\""))
    )
    (set_tile "status_text" "文件夹路径无效，无法打开！")
  )
)

;; ============================================================
;;  覆盖确认对话框（通过 Windows Script Host 弹出是/否弹窗）
;;  返回 T=覆盖  nil=取消
;; ============================================================

(defun wdg-confirm-overwrite (filename / wsh result)
  (setq wsh (vlax-create-object "WScript.Shell"))
  (if wsh
    (progn
      (setq result
        (vlax-invoke-method wsh 'Popup
          (strcat "目标文件已存在：\n" filename "\n\n是否覆盖？")
          0
          "WDG - 文件已存在"
          4  ;; 4 = vbYesNo
        )
      )
      (vlax-release-object wsh)
      ;; Popup 返回值: 6=Yes, 7=No, -1=超时
      (= result 6)
    )
    ;; wsh 创建失败，默认不覆盖
    nil
  )
)

;; ============================================================
;;  保存当前DWG文件到地址栏文件夹（复制方式，同名询问覆盖）
;; ============================================================

(defun wdg-save-current-dwg ( / src-path folder-path target-folder
                                target-base target-ext target-path idx)
  (setq src-path (strcat (getvar "DWGPREFIX") (getvar "DWGNAME")))
  (if (or (not src-path) (= src-path ""))
    (set_tile "status_text" "无法获取当前图纸路径！")
    (progn
      ;; 读取地址栏路径
      (setq folder-path (get_tile "folder_edit"))
      (if (or (not folder-path) (= folder-path ""))
        (set_tile "status_text" "地址栏路径为空！")
        (progn
          ;; 确保路径以 \\ 结尾
          (if (/= (substr folder-path (strlen folder-path)) "\\")
            (setq folder-path (strcat folder-path "\\"))
          )
          ;; 检查左列是否选中了子文件夹
          (setq wdg_tmp_idx (get_tile "dir_list"))
          (if (and wdg_tmp_idx (/= wdg_tmp_idx ""))
            (progn
              (setq wdg_idx (atoi wdg_tmp_idx))
              (if (< wdg_idx (length wdg_sub_folders))
                ;; 选中了子文件夹
                (setq target-folder (strcat folder-path (nth wdg_idx wdg_sub_folders) "\\"))
                ;; 选中的是DWG文件
                (setq target-folder folder-path)
              )
            )
            ;; 左列没选中
            (setq target-folder folder-path)
          )
              ;; 确保目标文件夹存在
              (if (vl-file-directory-p target-folder)
                (progn
                  (setq target-base (vl-filename-base src-path))
                  (setq target-ext  (vl-filename-extension src-path))
                  ;; 目标完整路径（原名）
                  (setq target-path (strcat target-folder target-base target-ext))
                  ;; 检查原名是否已存在
                  (if (findfile target-path)
                    ;; 已存在，弹出确认框询问是否覆盖
                    (progn
                      (setq wdg_overwrite_answer
                        (wdg-confirm-overwrite (strcat target-base target-ext)))
                      (if wdg_overwrite_answer
                        (progn
                          (if (vl-file-copy src-path target-path T)
                            (progn
                              (set_tile "status_text"
                                (strcat "已覆盖保存: " target-base target-ext))
                              ;; 刷新文件列表
                              (wdg_refresh_dir_list (get_tile "folder_edit"))
                              (if (and wdg_sub_folder (vl-file-directory-p wdg_sub_folder))
                                (wdg_refresh_sub_list wdg_sub_folder)
                              )
                            )
                            (set_tile "status_text" "覆盖失败！")
                          )
                        )
                        (set_tile "status_text"
                          (strcat "已取消覆盖: " target-base target-ext))
                      )
                    )
                    ;; 不存在，执行复制
                    (progn
                      (if (vl-file-copy src-path target-path)
                        (progn
                          (set_tile "status_text"
                            (strcat "已保存到: " target-base target-ext))
                          ;; 刷新文件列表
                          (wdg_refresh_dir_list (get_tile "folder_edit"))
                          (if (and wdg_sub_folder (vl-file-directory-p wdg_sub_folder))
                            (wdg_refresh_sub_list wdg_sub_folder)
                          )
                        )
                        (set_tile "status_text" "复制失败！")
                      )
                    )
                  )
                )
                (set_tile "status_text" "目标文件夹不存在！")
              )
        )
      )
    )
  )
)


;; ============================================================
;;  导出配置到REG文件
;; ============================================================

(defun wdg-export-config ( / fav-list last-folder reg-lines fh save-path
                            joined total seg-count idx chunk escaped-chunk)
  ;; 读取收藏夹列表
  (setq fav-list (wdg-load-favorites))
  ;; 读取上次文件夹路径
  (setq last-folder (getenv "WDG_LAST_FOLDER"))

  ;; 构建REG文件内容
  (setq reg-lines (list
    "Windows Registry Editor Version 5.00"
    ""
    "[HKEY_CURRENT_USER\\Software\\WDG_Favorites]"
  ))

  ;; 将所有收藏夹路径用 | 连接，按段写入 REG（防单键超 32767 字节）
  (setq joined (wdg-join-with-sep fav-list "|"))
  (setq total (strlen joined))
  (setq seg-count (if (> total 0) (/ (+ total wdg_seg_max -1) wdg_seg_max) 0))
  ;; 段数
  (setq reg-lines (append reg-lines (list
    (strcat "\"WDG_FAV_COUNT\"=\"" (itoa seg-count) "\"")
  )))
  ;; 按段写入
  (setq idx 0)
  (while (< idx seg-count)
    (setq chunk (substr joined (+ 1 (* idx wdg_seg_max)) wdg_seg_max))
    (setq escaped-chunk (wdg-escape-reg-string chunk))
    (setq reg-lines (append reg-lines (list
      (strcat "\"WDG_FAV_" (itoa idx) "\"=\"" escaped-chunk "\"")
    )))
    (setq idx (1+ idx))
  )

  ;; 添加上次文件夹路径（作为注释行，REG文件中;是注释）
  (setq reg-lines (append reg-lines (list
    ""
    "; WDG上次打开的文件夹路径（存储在AutoCAD环境变量中）"
    "; 导入此REG文件后，还需在AutoCAD中执行: (setenv \"WDG_LAST_FOLDER\" \"路径\")"
    (strcat "; WDG_LAST_FOLDER = " (wdg-escape-reg-string last-folder))
  )))

  ;; 让用户选择保存位置（getfiled对话框）
  ;; FILEDIA已设为0，但getfiled在DCL回调中可能不弹对话框
  ;; 所以先关闭当前对话框，在对话框外执行保存
  (setq save-path nil)

  ;; 将REG内容写入全局变量，在对话框关闭后再执行保存
  (setq wdg_export_reg_lines reg-lines)

  ;; 关闭对话框，标记为导出操作
  (done_dialog 3)
)

;; REG字符串转义：将\替换为\\，将"替换为\"
(defun wdg-escape-reg-string (str / result pos ch)
  (if (or (not str) (= str ""))
    ""
    (progn
      (setq result "")
      (setq pos 1)
      (while (<= pos (strlen str))
        (setq ch (substr str pos 1))
        (if (= ch "\\")
          (setq result (strcat result "\\\\"))
          (if (= ch "\"")
            (setq result (strcat result "\\\""))
            (setq result (strcat result ch))
          )
        )
        (setq pos (1+ pos))
      )
      result
    )
  )
)

;; ============================================================
;;  主命令 WDG
;; ============================================================

(defun c:WDG ( / dcl-path dcl-id dwg-list selected-file
                      dwg-fullpath folder-path selected-index
                      result old-cmdecho old-osmode old-filedia
                      save-path fh)

  ;; 初始化全局变量
  (setq wdg_folder nil)
  (setq wdg_sel_index nil)
  (setq wdg_favorites nil)
  (setq wdg_fav_sel_index nil)
  (setq wdg_sub_folders nil)    ;; 当前文件夹的子文件夹名列表
  (setq wdg_cur_dwgs nil)       ;; 当前文件夹的DWG文件列表
  (setq wdg_sub_dwgs nil)       ;; 选中子文件夹的DWG文件列表
  (setq wdg_sub_folder nil)     ;; 当前选中的子文件夹完整路径
  (setq wdg_dir_sel_index nil)  ;; 左列选中索引
  (setq wdg_sub_sel_index nil)  ;; 右列选中索引
  (setq wdg_open_source nil)    ;; 打开来源: "sub"=右列, "dir"=左列DWG文件, "search"=搜索结果
  (setq wdg_is_searching nil)   ;; 搜索模式标记
  (setq wdg_search_results nil) ;; 搜索结果列表
  (setq wdg_search_sel_index nil) ;; 搜索结果选中索引
  (setq wdg_last_search_text "") ;; 搜索去重：上次搜索文本

  (setq old-cmdecho (getvar "CMDECHO"))
  (setq old-osmode  (getvar "OSMODE"))
  (setq old-filedia (getvar "FILEDIA"))
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)
  (setvar "FILEDIA" 0)

  ;; 创建临时DCL文件
  (setq dcl-path (create-temp-dcl-wdg))
  (setq dcl-id (load_dialog dcl-path))

  (if (< dcl-id 0)
    (progn
      (princ "\n[WDG] 无法加载对话框。")
      (setvar "CMDECHO" old-cmdecho)
      (setvar "OSMODE"  old-osmode)
      (setvar "FILEDIA" old-filedia)
      (princ)
    )
  )

  ;; ---------- 默认文件夹：优先读取上次保存的路径 ----------
  (setq wdg_folder (load-folder-wdg))
  ;; 没有上次记录，用当前图纸所在文件夹
  (if (not wdg_folder)
    (progn
      (setq wdg_folder (getvar "DWGPREFIX"))
      (if (or (not wdg_folder) (= wdg_folder ""))
        (setq wdg_folder (getvar "SAVEFILEPATH"))
      )
      (if (or (not wdg_folder) (= wdg_folder ""))
        (setq wdg_folder ".\\")
      )
    )
  )

  (if (not (new_dialog "dwg_insert" dcl-id))
    (progn
      (princ "\n[WDG] 无法显示对话框。")
      (unload_dialog dcl-id)
      (setvar "CMDECHO" old-cmdecho)
      (setvar "OSMODE"  old-osmode)
      (setvar "FILEDIA" old-filedia)
      (princ)
    )
  )

  ;; ---------- 初始化 ----------
  (set_tile "folder_edit" wdg_folder)
  (wdg_refresh_dir_list wdg_folder)
  (wdg-refresh-fav-list)

  ;; ---------- 回调 ----------

  ;; 浏览按钮
  (action_tile "browse_btn"
    "(setq wdg_tmp (browse-folder-wdg))
     (if wdg_tmp
       (progn
         (set_tile \"folder_edit\" wdg_tmp)
         (setq wdg_folder wdg_tmp)
         (wdg_refresh_dir_list wdg_tmp)
         (start_list \"sub_file_list\")
         (end_list)
       )
     )"
  )

  ;; 打开按钮：用资源管理器打开地址栏中的文件夹
  (action_tile "open_folder_btn"
    "(wdg-open-folder (get_tile \"folder_edit\"))"
  )

  ;; 保存按钮：复制当前DWG到地址栏文件夹
  (action_tile "save_btn"
    "(wdg-save-current-dwg)"
  )

  ;; 缓存按钮：扫描所有收藏夹文件夹及子文件夹中的DWG文件
  (action_tile "cache_btn"
    "(wdg-build-cache)"
  )

  ;; 搜索框：实时筛选缓存中的DWG文件
  ;; 注意：焦点丢失也会触发此回调，用去重变量避免重复刷新导致选择丢失
  (action_tile "search_edit"
    "(if (/= $value wdg_last_search_text)
       (progn
         (setq wdg_last_search_text $value)
         (setq wdg_search_text $value)
         (wdg-do-search wdg_search_text)
       )
     )"
  )

  ;; 文件夹路径编辑框：修改时刷新列表
  (action_tile "folder_edit"
    "(setq wdg_folder (get_tile \"folder_edit\"))
     (wdg_refresh_dir_list wdg_folder)
     (start_list \"sub_file_list\")
     (end_list)"
  )

  ;; 左列：子文件夹+DWG文件列表（或搜索结果）
  ;; 单击文件夹：刷新右列；双击DWG文件：打开
  (action_tile "dir_list"
    "(setq wdg_dir_sel_index (get_tile \"dir_list\"))
     (if (and wdg_dir_sel_index (/= wdg_dir_sel_index \"\"))
       (progn
         (setq wdg_idx (atoi wdg_dir_sel_index))
         (if wdg_is_searching
           ;; 搜索模式：所有项都是DWG文件
           (progn
             (setq wdg_open_source \"search\")
             (setq wdg_search_sel_index wdg_dir_sel_index)
             (if (= $reason 4)
               (progn
                 (setq wdg_folder (get_tile \"folder_edit\"))
                 (done_dialog 2)
               )
             )
           )
           ;; 正常模式：子文件夹 + DWG文件
           (progn
             (if (< wdg_idx (length wdg_sub_folders))
               ;; 是子文件夹：刷新右列
               (progn
                 (setq wdg_cur_folder (get_tile \"folder_edit\"))
                 (if (/= (substr wdg_cur_folder (strlen wdg_cur_folder)) \"\\\\\")
                   (setq wdg_cur_folder (strcat wdg_cur_folder \"\\\\\"))
                 )
                 (setq wdg_sub_folder (strcat wdg_cur_folder (nth wdg_idx wdg_sub_folders) \"\\\\\"))
                 (wdg_refresh_sub_list wdg_sub_folder)
                 (set_tile \"status_text\" (strcat \"子文件夹: \" (nth wdg_idx wdg_sub_folders)))
               )
               ;; 是DWG文件：记录来源为左列
               (progn
                 (setq wdg_open_source \"dir\")
                 (setq wdg_sel_index (itoa (- wdg_idx (length wdg_sub_folders))))
                 (if (= $reason 4)
                   (progn
                     (setq wdg_folder (get_tile \"folder_edit\"))
                     (done_dialog 2)
                   )
                 )
               )
             )
           )
         )
       )
     )"
  )

  ;; 右列：子文件夹中的DWG文件列表，双击=result 2(打开文件)
  (action_tile "sub_file_list"
    "(setq wdg_sub_sel_index (get_tile \"sub_file_list\"))
     (if (/= wdg_sub_sel_index \"\")
       (progn
         (setq wdg_open_source \"sub\")
         (if (= $reason 4)
           (progn
             (setq wdg_folder (get_tile \"folder_edit\"))
             (done_dialog 2)
           )
         )
       )
     )"
  )

  ;; 确定按钮=result 1(打开文件)
  (action_tile "accept"
    "(setq wdg_folder (get_tile \"folder_edit\"))
     (setq wdg_sub_sel_index (get_tile \"sub_file_list\"))
     (setq wdg_dir_sel_index (get_tile \"dir_list\"))
     (if wdg_is_searching
       ;; 搜索模式：打开选中的搜索结果
       (progn
         (if wdg_dir_sel_index
           (setq wdg_open_source \"search\"
                 wdg_search_sel_index wdg_dir_sel_index)
         )
       )
       ;; 正常模式
       (progn
         (if (and wdg_sub_sel_index (/= wdg_sub_sel_index \"\"))
           (setq wdg_open_source \"sub\")
           (if (and wdg_dir_sel_index (/= wdg_dir_sel_index \"\"))
             (progn
               (setq wdg_idx2 (atoi wdg_dir_sel_index))
               (if (>= wdg_idx2 (length wdg_sub_folders))
                 (progn
                   (setq wdg_open_source \"dir\")
                   (setq wdg_sel_index (itoa (- wdg_idx2 (length wdg_sub_folders))))
                 )
               )
             )
           )
         )
       )
     )
     (done_dialog 1)"
  )

  ;; 收藏夹列表：单击切换文件夹
  (action_tile "fav_list"
    "(setq wdg_fav_sel_index (get_tile \"fav_list\"))
     (if wdg_fav_sel_index
       (progn
         (setq wdg_fav_folder (nth (atoi wdg_fav_sel_index) wdg_favorites))
         (if (and wdg_fav_folder (vl-file-directory-p wdg_fav_folder))
           (progn
             (set_tile \"folder_edit\" wdg_fav_folder)
             (setq wdg_folder wdg_fav_folder)
             (wdg_refresh_dir_list wdg_fav_folder)
             (start_list \"sub_file_list\")
             (end_list)
           )
           (set_tile \"status_text\" \"收藏夹路径无效！\")
         )
       )
     )"
  )

  ;; 加入收藏按钮
  (action_tile "fav_add_btn"
    "(setq wdg_cur_folder (get_tile \"folder_edit\"))
     (if (and wdg_cur_folder (/= wdg_cur_folder \"\") (vl-file-directory-p wdg_cur_folder))
       (progn
         ;; 确保以\\结尾
         (if (/= (substr wdg_cur_folder (strlen wdg_cur_folder)) \"\\\\\")
           (setq wdg_cur_folder (strcat wdg_cur_folder \"\\\\\"))
         )
         (setq wdg_favorites (wdg-load-favorites))
         ;; 检查是否已存在
         (if (not (member (strcase wdg_cur_folder) (mapcar 'strcase wdg_favorites)))
           (progn
             (setq wdg_favorites (append wdg_favorites (list wdg_cur_folder)))
             (wdg-save-favorites wdg_favorites)
             (wdg-refresh-fav-list)
             (set_tile \"status_text\" (strcat \"已加入收藏: \" (wdg-fav-display-name wdg_cur_folder)))
           )
           (set_tile \"status_text\" \"该文件夹已在收藏夹中\")
         )
       )
       (set_tile \"status_text\" \"当前文件夹路径无效\")
     )"
  )

  ;; 删除收藏按钮
  (action_tile "fav_del_btn"
    "(setq wdg_fav_sel_index (get_tile \"fav_list\"))
     (if (and wdg_fav_sel_index (/= wdg_fav_sel_index \"\"))
       (progn
         (setq wdg_favorites (wdg-load-favorites))
         (setq wdg_fav_to_del (nth (atoi wdg_fav_sel_index) wdg_favorites))
         (if wdg_fav_to_del
           (progn
             (setq wdg_favorites
               (vl-remove-if
                 (function (lambda (x) (equal (strcase x) (strcase wdg_fav_to_del))))
                 wdg_favorites
               )
             )
             (wdg-save-favorites wdg_favorites)
             (wdg-refresh-fav-list)
             (set_tile \"status_text\" (strcat \"已删除收藏: \" (wdg-fav-display-name wdg_fav_to_del)))
           )
         )
       )
       (set_tile \"status_text\" \"请先选择要删除的收藏项\")
     )"
  )

  ;; 取消按钮=result 0
  (action_tile "cancel" "(done_dialog 0)")

  ;; 导出配置按钮：关闭对话框后执行保存
  (action_tile "export_btn"
    "(wdg-export-config)"
  )

  ;; ---------- 显示对话框 ----------
  (setq result (start_dialog))

  ;; ---------- 清理对话框 ----------
  (unload_dialog dcl-id)

  ;; ---------- 处理导出配置操作（result=3） ----------
  (if (= result 3)
    (progn
      ;; 先恢复系统变量，避免递归调用时保存错误的值
      (setvar "CMDECHO" old-cmdecho)
      (setvar "OSMODE"  old-osmode)
      (setvar "FILEDIA" old-filedia)
      ;; 恢复FILEDIA，让getfiled能弹出保存对话框
      (setvar "FILEDIA" 1)
      (setq save-path (getfiled "导出WDG配置" "" "reg" 1))
      (setvar "FILEDIA" old-filedia)
      (if save-path
        (progn
          (setq fh (open save-path "w"))
          (foreach line wdg_export_reg_lines (write-line line fh))
          (close fh)
          (princ (strcat "\n[WDG] 配置已导出到: " save-path))
        )
        (princ "\n[WDG] 导出取消。")
      )
      ;; 导出完成后，重新打开WDG对话框
      (c:WDG)
      ;; 递归调用后直接返回，不再执行后续逻辑
      (princ)
    )
  )

  ;; ---------- 如果不是导出操作，继续正常逻辑 ----------
  (if (/= result 3)
    (progn

  ;; ---------- 读取选择 ----------
  (setq folder-path wdg_folder)

  ;; ---------- 保存文件夹路径（持久化）----------
  (if (and folder-path (/= folder-path ""))
    (save-folder-wdg folder-path)
  )

  ;; ---------- 准备打开文件（但不立即打开）----------
  (setq wdg_pending_open nil)

  (if (/= result 0)
    (cond
      ;; 搜索模式：从缓存结果打开文件
      ((and (equal wdg_open_source "search")
            wdg_search_sel_index (/= wdg_search_sel_index "")
            wdg_search_results)
        (setq wdg_tmp_idx2 (atoi wdg_search_sel_index))
        (if (< wdg_tmp_idx2 (length wdg_search_results))
          (setq wdg_pending_open (nth wdg_tmp_idx2 wdg_search_results))
        )
      )
      ;; 从右列（子文件夹）打开文件
      ((and (equal wdg_open_source "sub")
            wdg_sub_sel_index (/= wdg_sub_sel_index "")
            wdg_sub_folder)
        (setq selected-file (nth (atoi wdg_sub_sel_index) wdg_sub_dwgs))
        (if selected-file
          (setq wdg_pending_open (strcat wdg_sub_folder selected-file))
        )
      )
      ;; 从左列（当前文件夹DWG文件）打开
      ((and (equal wdg_open_source "dir")
            wdg_sel_index (/= wdg_sel_index "")
            folder-path)
        (if (/= (substr folder-path (strlen folder-path)) "\\")
          (setq folder-path (strcat folder-path "\\"))
        )
        (setq selected-file (nth (atoi wdg_sel_index) wdg_cur_dwgs))
        (if selected-file
          (setq wdg_pending_open (strcat folder-path selected-file))
        )
      )
      ;; 所有分支都不匹配时才提示已取消
      (T (princ "\n[WDG] 已取消。"))
    )
  )

  ;; ---------- 恢复系统变量（必须在打开文件之前完成）----------
  (setvar "CMDECHO" old-cmdecho)
  (setvar "OSMODE"  old-osmode)
  (setvar "FILEDIA" old-filedia)

  ;; ---------- 最后一步：打开DWG文件 ----------
  ;; 通过临时脚本文件打开，完美支持含空格和中文的深层路径
  (if wdg_pending_open
    (wdg-open-dwg-safe wdg_pending_open)
  )

    ) ;; end of (if (/= result 3) progn
  ) ;; end of (if (/= result 3))

  (princ)
)


;; ============================================================
;;  刷新文件列表（对话框回调函数）
;; ============================================================

;; 刷新左列：子文件夹 + DWG文件
(defun wdg_refresh_dir_list (folder / subfolders dwg-list)
  (if (vl-file-directory-p folder)
    (progn
      (setq subfolders (get-subfolders-wdg folder))
      (setq dwg-list (get-dwg-list-wdg folder))
      (setq wdg_sub_folders subfolders)
      (setq wdg_cur_dwgs dwg-list)
      (start_list "dir_list")
      (foreach f subfolders (add_list (strcat "  \004  " f "  ")))
      (foreach f dwg-list (add_list (strcat "  " f "  ")))
      (end_list)
      (set_tile "status_text"
        (strcat (itoa (length subfolders)) " 个子文件夹，"
                (itoa (length dwg-list)) " 个DWG文件")
      )
    )
    (progn
      (start_list "dir_list")
      (end_list)
      (start_list "sub_file_list")
      (end_list)
      (set_tile "status_text" "文件夹路径无效！")
    )
  )
)

;; 刷新右列：指定子文件夹中的DWG文件
(defun wdg_refresh_sub_list (folder / dwg-list)
  (if (vl-file-directory-p folder)
    (progn
      (setq dwg-list (get-dwg-list-wdg folder))
      (setq wdg_sub_dwgs dwg-list)
      (start_list "sub_file_list")
      (foreach f dwg-list (add_list (strcat "  " f "  ")))
      (end_list)
    )
    (progn
      (start_list "sub_file_list")
      (end_list)
    )
  )
)


;; ============================================================
;;  获取指定文件夹中的子文件夹列表（纯名称，不含路径）
;; ============================================================

(defun get-subfolders-wdg (folder / all-items result full-path)
  ;; 确保路径以 \\ 结尾
  (if (/= (substr folder (strlen folder)) "\\")
    (setq folder (strcat folder "\\"))
  )
  ;; 获取所有条目（模式 -1 = 文件和目录）
  (setq all-items (vl-directory-files folder "*" -1))
  (setq result '())
  (foreach item all-items
    (if (and (/= item ".") (/= item ".."))
      (progn
        (setq full-path (strcat folder item))
        (if (vl-file-directory-p full-path)
          (setq result (cons item result))
        )
      )
    )
  )
  (if result
    (vl-sort result (function (lambda (a b) (< (strcase a) (strcase b)))))
    result
  )
)


;; ============================================================
;;  获取指定文件夹中的DWG文件列表（不含路径，纯文件名）
;;  过滤掉当前图纸本身（按完整路径判断，不同路径的同名文件不过滤）
;; ============================================================

(defun get-dwg-list-wdg (folder / files result cur-full base-name full-path)
  ;; 确保 folder 以 \ 结尾，用于拼接完整路径
  (if (/= (substr folder (strlen folder)) "\\")
    (setq folder (strcat folder "\\"))
  )
  (setq files (vl-directory-files folder "*.dwg"))
  (setq result '())
  ;; 当前图纸的完整路径，用于精确过滤
  (setq cur-full (strcase (strcat (getvar "DWGPREFIX") (getvar "DWGNAME"))))
  (foreach f files
    ;; 拼接文件完整路径
    (setq full-path (strcase (strcat folder f)))
    (if (not (equal full-path cur-full))
      (setq result (cons f result))
    )
  )
  (if result
    (vl-sort result (function (lambda (a b) (< (strcase a) (strcase b)))))
    result
  )
)


;; ============================================================
;;  WDV 命令：在源文件和WDG打开的目标文件之间切换
;;  读取 %TEMP%\wdg_switch_pair.txt，判断当前在源还是目标
;;  支持未保存图纸（标识 *UNSAVED*:DrawingX.dwg）
;;  两个文件都已打开（WDG 保证），只用 vla-put-ActiveDocument 激活
;;  不涉及任何文件打开操作
;; ============================================================

;; 在已打开的文档集合中按完整路径或未保存标识查找文档对象
;; target-id: 完整路径 或  *UNSAVED*:DrawingX.dwg
(defun wdv-find-open-doc (target-id / docs doc dp dname result unsaved-name)
  (setq result nil)
  (if (and target-id (/= target-id ""))
    (progn
      (setq docs (vla-get-Documents (vlax-get-acad-object)))
      (if (wcmatch (strcase target-id) "*UNSAVED*:*")
        ;; 未保存图纸：按文档名称匹配
        (progn
          (setq unsaved-name (strcase (substr target-id 11)))
          (vlax-for doc docs
            (setq dname (vl-catch-all-apply 'vla-get-Name (list doc)))
            (if (and (not (vl-catch-all-error-p dname))
                     (= (strcase dname) unsaved-name))
              (setq result doc)
            )
          )
        )
        ;; 已保存：按完整路径匹配
        (vlax-for doc docs
          (setq dp (vl-catch-all-apply 'vla-get-FullName (list doc)))
          (if (and (not (vl-catch-all-error-p dp))
                   (= (strcase dp) (strcase target-id)))
            (setq result doc)
          )
        )
      )
    )
  )
  result
)

(defun c:WDV ( / pair-path fh line sep-pos src-path dest-path cur-path target-doc)
  (setq pair-path (strcat (getvar "TEMPPREFIX") "wdg_switch_pair.txt"))
  (if (not (findfile pair-path))
    (princ "\n[WDV] 没有切换记录。请先通过WDG打开一个文件。")
    (progn
      (setq fh (open pair-path "r"))
      (setq line (read-line fh))
      (close fh)
      (if (not line)
        (princ "\n[WDV] 切换记录为空。")
        (progn
          (setq sep-pos (vl-string-search "|" line))
          (if (not sep-pos)
            (princ "\n[WDV] 切换记录格式错误。")
            (progn
              (setq src-path (substr line 1 sep-pos))
              (setq dest-path (substr line (+ sep-pos 2)))
              (setq cur-path (wdg-current-dwg-path))
              (setq target-doc nil)
              (cond
                ;; 当前是源 → 激活目标
                ((and cur-path (= (strcase cur-path) (strcase src-path)))
                 (setq target-doc (wdv-find-open-doc dest-path))
                 (if target-doc
                   (progn
                     (princ (strcat "\n[WDV] 切换到目标: " (vla-get-Name target-doc)))
                     (vla-put-ActiveDocument (vlax-get-acad-object) target-doc)
                   )
                   (princ "\n[WDV] 目标文件未在CAD中打开。")
                 )
                )
                ;; 当前是目标 → 激活源（支持未保存源）
                ((and cur-path (= (strcase cur-path) (strcase dest-path)))
                 (setq target-doc (wdv-find-open-doc src-path))
                 (if target-doc
                   (progn
                     (princ (strcat "\n[WDV] 切回源文件: " (vla-get-Name target-doc)))
                     (vla-put-ActiveDocument (vlax-get-acad-object) target-doc)
                   )
                   (princ "\n[WDV] 源文件未在CAD中打开。")
                 )
                )
                ;; 当前不在切换对中
                (T
                 (princ "\n[WDV] 当前文件不在切换对中。")
                 (princ (strcat "\n  源: " src-path))
                 (princ (strcat "\n  目标: " dest-path))
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


;; ============================================================
;;  WGG 命令：关闭所有通过WDG打开的文件
;; ============================================================

(defun c:WGG ( / doc count closed-count)

  (if (not wdg_opened_docs)
    (progn
      (princ "\n[WGG] 没有通过WDG打开的文件。")
      (princ)
    )
  )

  (setq count 0)
  (setq closed-count 0)

  (foreach doc wdg_opened_docs
    (setq count (1+ count))
    ;; 检查文档是否仍然在打开状态
    (if (not (vl-catch-all-error-p
               (vl-catch-all-apply 'vlax-get (list doc 'FullName))))
      ;; 文档仍然打开，关闭它（不保存）
      (progn
        (princ (strcat "\n[WGG] 关闭: " (vlax-get doc 'Name)))
        (vl-catch-all-apply 'vla-Close (list doc :vlax-false))
        (setq closed-count (1+ closed-count))
      )
      ;; 文档已关闭或不可用
      (princ (strcat "\n[WGG] 跳过(已关闭或不可用): 第" (itoa count) "个文件"))
    )
  )

  ;; 清空记录列表
  (setq wdg_opened_docs nil)

  (princ (strcat "\n[WGG] 完成。共关闭 " (itoa closed-count) " 个文件。"))
  (princ)
)

(princ "\n[WDG] 命令已加载。WDG 打开文件，WDV 在源/目标文件间切换，WGG 关闭所有WDG打开的文件。")
(princ)
