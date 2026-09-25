;; AutoCAD Automatic Oriented Array Tool - Enhanced Multi-Block Version
;; Command: bsd
;; Features: Multiple block selection, individual quantity settings

;; Global variables for dialog interaction
(setq *selected_blocks* nil)
(setq *block_quantities* nil)
(setq *temp_quantities* nil)
;; Dynamic quantity variables will be created as needed

;; Global variables for BSDW (library selection)
(setq *library_blocks* nil)      ;; List of available block files from library
(setq *selected_library_blocks* nil)  ;; List of selected block names from library
(setq *selected_library_block_info* nil)  ;; List of selected block info (name, file_path, subfolder)
(setq *library_quantities* nil)  ;; Quantities for library blocks
(setq *library_path* nil)        ;; Path to block library folder
(setq *insertion_scale* nil)     ;; Insertion scale (1x or 0.1x) - loaded from saved settings on first use, defaults to 1x
(setq *library_subfolders* nil)  ;; List of subfolders in library
(setq *current_subfolder* nil)   ;; Currently selected subfolder
(setq *subfolder_blocks* nil)    ;; Blocks organized by subfolder
(setq *library_tabs_as_list* nil) ;; T when subfolder tabs are shown as a scrollable list instead of button rows
(setq *library_favorites* nil)   ;; 当前已加载的图块库收藏夹路径列表
(setq *bsdw_fav_seg_max* 8000)   ;; 收藏夹注册表分段存储：每段最大字节数（REG_SZ 上限 32767）
(setq *bsdw_pending_name* nil)   ;; 创建图块命名对话框中暂存的名称（支持"拾取文字"刷新对话框后保留）
(setq *bsdw_pending_basepoint* nil) ;; 创建图块命名对话框中暂存的基点选择："bp_tl"/"bp_tr"/"bp_bl"/"bp_br"/"bp_center"，默认左上

(defun InitializeQuantityVariables (block_count / i var_name)
  "Initialize quantity variables dynamically based on block count"
  (setq i 1)
  (repeat block_count
    (setq var_name (read (strcat "*q" (itoa i) "*")))
    (set var_name "1")  ;; Set default value
    (setq i (+ i 1))
  )
)

(defun GetQuantityVariable (index / var_name)
  "Get quantity variable value by index"
  (setq var_name (read (strcat "*q" (itoa index) "*")))
  (if (boundp var_name)
    (eval var_name)
    "1"  ;; Default value if variable doesn't exist
  )
)

(defun c:bsdw (/ old_osmode old_cmdecho)
  "Main command function for library-based block selection and array"
  
  ;; Error handler
  (defun *error* (msg)
    (if old_osmode
      (setvar "OSMODE" old_osmode)
    )
    (if old_cmdecho
      (setvar "CMDECHO" old_cmdecho)
    )
    (if (not (wcmatch msg "quit / *cancel*"))
      (princ (strcat "\n错误：" msg))
    )
    (princ)
  )
  
  ;; Save system variables
  (setq old_osmode (getvar "OSMODE"))
  (setq old_cmdecho (getvar "CMDECHO"))
  
  ;; Reset selections
  (setq *selected_library_blocks* nil)
  (setq *selected_library_block_info* nil)
  (setq *library_quantities* nil)
  (setq *library_blocks* nil)
  (setq *library_subfolders* nil)
  (setq *current_subfolder* nil)
  (setq *subfolder_blocks* nil)
  
  ;; Initialize insertion scale: keep current session value if already set,
  ;; otherwise load the last saved preference, otherwise default to 1x
  (if (not *insertion_scale*)
    (if (not (LoadInsertionScale))
      (setq *insertion_scale* "1.0")
    )
  )
  
  ;; Get library path and scan for blocks
  (if (ScanBlockLibrary)
    (progn
      ;; Show library selection dialog
      (ShowLibraryDialog)
    )
    (princ "\n未找到图块库或无可用图块。")
  )
  
  ;; Restore system variables
  (setvar "OSMODE" old_osmode)
  (setvar "CMDECHO" old_cmdecho)
  
  (princ)
)

(defun ScanBlockLibrary (/ script_path library_path dir_list file_list block_name current_dir lisp_path subfolder_list subfolder_name subfolder_path subfolder_files saved_subfolder)
  "Scan block library folder for available block files and subfolders"
  
  ;; First try to load saved library path
  (if (not *library_path*)
    (LoadLibraryPath)
  )
  
  ;; Use the manually set or loaded library path if available
  (if *library_path*
    (setq library_path *library_path*)
    (progn
      ;; Try to get the directory where this LISP file is located
      ;; Method 1: Check if we can get it from the load path
      (setq lisp_path nil)
      
      ;; Method 2: Ask user for the library path if we can't find it automatically
      (setq library_path nil)
      
      ;; Try current drawing directory first
      (setq current_dir (getvar "DWGPREFIX"))
      (princ (strcat "\n检查当前图纸目录：" current_dir))
      
      ;; Check if blocks subfolder exists in current drawing directory
      (setq script_path (strcat current_dir "blocks\\"))
      (if (vl-file-directory-p script_path)
        (setq library_path script_path)
        (princ "\n当前图纸目录中无 blocks 子文件夹")
      )
      
      ;; If not found, check current drawing directory for DWG files
      (if (not library_path)
        (progn
          (setq dir_list (vl-directory-files current_dir "*.dwg" 1))
          (if dir_list
            (progn
              (setq library_path current_dir)
              (princ "\n使用当前图纸目录")
            )
            (princ "\n当前图纸目录中无 DWG 文件")
          )
        )
      )
      
      ;; If still not found, let user specify the path
      (if (not library_path)
        (progn
          (princ "\n无法自动定位图块库。")
          (princ "\n请先使用 BSDSETPATH 命令设置库路径。")
          (setq library_path nil)
        )
      )
    )
  )
  
  ;; Debug information
  (princ (strcat "\n使用库路径：" (if library_path library_path "无")))
  
  (if library_path
    (progn
      (setq *library_path* library_path)
      
      ;; Initialize subfolder variables
      (setq *library_subfolders* nil)
      (setq *subfolder_blocks* nil)
      (setq *current_subfolder* nil)
      
      ;; Get list of subdirectories
      (setq subfolder_list (vl-directory-files library_path nil -1))
      
      ;; Filter out "." and ".." and add "Main" for root directory
      (setq *library_subfolders* (list "Main"))
      (foreach subfolder_name subfolder_list
        (if (and (not (equal subfolder_name ".")) 
                 (not (equal subfolder_name "..")))
          (setq *library_subfolders* (append *library_subfolders* (list subfolder_name)))
        )
      )
      
      ;; Scan blocks in main directory (root)
      (setq dir_list (vl-directory-files library_path "*.dwg" 1))
      (setq main_blocks nil)
      (if dir_list
        (foreach file_name dir_list
          (setq block_name (vl-filename-base file_name))
          (setq main_blocks (append main_blocks (list (list block_name file_name))))
        )
      )
      (setq *subfolder_blocks* (append *subfolder_blocks* (list (list "Main" main_blocks))))
      
      ;; Scan blocks in each subdirectory
      (foreach subfolder_name (cdr *library_subfolders*) ;; Skip "Main" as it's already processed
        (setq subfolder_path (strcat library_path subfolder_name "\\"))
        (if (vl-file-directory-p subfolder_path)
          (progn
            (setq subfolder_files (vl-directory-files subfolder_path "*.dwg" 1))
            (setq subfolder_blocks nil)
            (if subfolder_files
              (foreach file_name subfolder_files
                (setq block_name (vl-filename-base file_name))
                (setq subfolder_blocks (append subfolder_blocks (list (list block_name (strcat subfolder_name "\\" file_name)))))
              )
            )
            (setq *subfolder_blocks* (append *subfolder_blocks* (list (list subfolder_name subfolder_blocks))))
          )
        )
      )
      
      ;; Restore the last-used category if it still exists in this library,
      ;; otherwise fall back to "Main"
      (setq saved_subfolder (LoadCurrentSubfolder))
      (if (and saved_subfolder (member saved_subfolder *library_subfolders*))
        (setq *current_subfolder* saved_subfolder)
        (setq *current_subfolder* "Main")
      )
      (setq *library_blocks* (GetBlocksForSubfolder *current_subfolder*))
      
      ;; Debug information
      (princ (strcat "\n找到 " (itoa (length *library_subfolders*)) " 个文件夹"))
      (foreach subfolder_info *subfolder_blocks*
        (setq subfolder_name (car subfolder_info))
        (setq subfolder_blocks (cadr subfolder_info))
        (princ (strcat "\n  " subfolder_name ": " (itoa (length subfolder_blocks)) " 个图块"))
      )
      
      T
    )
    (progn
      (princ "\n无可用库路径。请先使用 BSDSETPATH。")
      nil
    )
  )
)

(defun GetBlocksForSubfolder (subfolder_name / subfolder_info)
  "Get blocks list for specified subfolder"
  (setq subfolder_info nil)
  (foreach folder_info *subfolder_blocks*
    (if (equal (car folder_info) subfolder_name)
      (setq subfolder_info (cadr folder_info))
    )
  )
  (if subfolder_info subfolder_info nil)
)

(defun GetAdaptiveListHeight (/ screen_size screen_height calc_height)
  "Calculate the BASE list_box height (in DCL units) purely from the current
   AutoCAD drawing window / screen resolution. This is the static height
   used for the first column's library_list, which does NOT change with
   the number of category tab rows (that column already grows on its own
   because the tab button rows sit above it and push the column taller)."
  (setq screen_size (getvar "SCREENSIZE"))
  (setq screen_height (cadr screen_size))
  ;; Roughly ~22 pixels of drawing-window height per DCL height unit at typical DPI/font sizes
  (setq calc_height (fix (/ screen_height 22.0)))
  ;; Clamp the raw screen-based baseline
  (if (< calc_height 18) (setq calc_height 18))
  (if (> calc_height 45) (setq calc_height 45))
  calc_height
)

(defun GetGrowingListHeight (base_height tab_rows / calc_height extra)
  "Calculate the list_box height (in DCL units) for the second and third
   columns (selected_list and fav_list), which have no tab buttons of
   their own. Their list must grow along with the number of category tab
   rows shown in the first column, so that all three columns' bottom
   edges stay aligned near the bottom of the container as it grows taller.
   base_height is the static baseline from GetAdaptiveListHeight; tab_rows
   is the number of category tab rows shown above the first column's list
   (already capped elsewhere at *library_max_tab_rows*, since beyond that
   the tab area switches to a fixed-height scrollable list and stops
   growing, so these lists should stop growing with it too)."
  (if (not tab_rows) (setq tab_rows 1))
  ;; Add ~2 units for each tab row beyond the first, since each extra row
  ;; of category buttons in column 1 pushes the whole container taller.
  (setq extra (* (max 0 (- tab_rows 1)) 2))
  (setq calc_height (+ base_height extra))
  ;; Never let the list shrink to an unusable size
  (if (< calc_height 12) (setq calc_height 12))
  calc_height
)

(defun CreateLibraryDCLFile (filename / dcl_file i tab_index list_height list_height2
                              tabs_per_row max_tab_rows total_subfolders needed_rows capped_rows)
  "Create DCL file for library block selection dialog with tabs"
  (setq dcl_file (open filename "w"))
  (setq tabs_per_row 5)
  (setq max_tab_rows 5)
  (setq total_subfolders (if *library_subfolders* (length *library_subfolders*) 0))
  (setq needed_rows (if (> total_subfolders 0) (1+ (fix (/ (float (- total_subfolders 1)) tabs_per_row))) 0))
  (setq capped_rows (min needed_rows max_tab_rows))
  ;; Once the categories would need more than max_tab_rows of button rows,
  ;; stop growing the dialog height and show them in a fixed-height,
  ;; internally scrollable list instead.
  (setq *library_tabs_as_list* (> needed_rows max_tab_rows))
  ;; Column 1 (library_list): static height, independent of tab row count -
  ;; that column already grows on its own because the tab rows sit above it.
  (setq list_height (GetAdaptiveListHeight))
  ;; Columns 2 & 3 (selected_list, fav_list): no tab rows of their own, so
  ;; grow their list to track column 1's row count and keep bottoms aligned.
  (setq list_height2 (GetGrowingListHeight list_height (max 1 capped_rows)))
  (if dcl_file
    (progn
      (write-line "bsdw_library : dialog {" dcl_file)
      (write-line "  label = \"图块库选择\";" dcl_file)
      (write-line "  : row {" dcl_file)
      (write-line "    : boxed_column {" dcl_file)
      (write-line "      label = \"可用图块\";" dcl_file)
      (write-line "      width = 25;" dcl_file)
      
      (cond
        ;; Too many categories for button rows - show a fixed-height,
        ;; scrollable list instead so the dialog height stops growing.
        (*library_tabs_as_list*
          (write-line "      : list_box {" dcl_file)
          (write-line "        key = \"subfolder_list\";" dcl_file)
          (write-line "        width = 25;" dcl_file)
          (write-line (strcat "        height = " (itoa (* max_tab_rows 2)) ";") dcl_file)
          (write-line "        multiple_select = false;" dcl_file)
          (write-line "      }" dcl_file)
        )
        ;; Otherwise, add tab buttons for subfolders with multi-row layout (max 5 per row)
        (*library_subfolders*
          (progn
            (setq tab_index 1)
            (setq current_row_count 0)
            (setq row_started nil)
            
            (foreach subfolder_name *library_subfolders*
              ;; Start new row if needed
              (if (or (not row_started) (>= current_row_count tabs_per_row))
                (progn
                  ;; Close previous row if exists
                  (if row_started
                    (write-line "      }" dcl_file)
                  )
                  ;; Start new row
                  (write-line "      : row {" dcl_file)
                  (write-line (strcat "        key = \"tab_row" (itoa (/ (- tab_index 1) tabs_per_row)) "\";") dcl_file)
                  (setq current_row_count 0)
                  (setq row_started T)
                )
              )
              
              ;; Add tab button
              (write-line "        : button {" dcl_file)
              (write-line (strcat "          key = \"tab_" (itoa tab_index) "\";") dcl_file)
              (write-line (strcat "          label = \"" subfolder_name "\";") dcl_file)
              (write-line "          width = 8;" dcl_file)
              (write-line "          fixed_width = true;" dcl_file)
              (if (equal subfolder_name *current_subfolder*)
                (write-line "          is_default = true;" dcl_file)
              )
              (write-line "        }" dcl_file)
              
              (setq tab_index (+ tab_index 1))
              (setq current_row_count (+ current_row_count 1))
            )
            
            ;; Close the last row
            (if row_started
              (write-line "      }" dcl_file)
            )
          )
        )
      )
      
      (write-line "      : list_box {" dcl_file)
      (write-line "        key = \"library_list\";" dcl_file)
      (write-line "        width = 20;" dcl_file)
      (write-line (strcat "        height = " (itoa list_height) ";") dcl_file)
      (write-line "        alignment = \"fill\";" dcl_file)
      (write-line "        multiple_select = false;" dcl_file)
      (write-line "      }" dcl_file)
      (write-line "      : boxed_column {" dcl_file)
      (write-line "        label = \"插入比例\";" dcl_file)
      (write-line "        width = 25;" dcl_file)
      (write-line "        : radio_column {" dcl_file)
      (write-line "          key = \"scale_group\";" dcl_file)
      (write-line "          : radio_button {" dcl_file)
      (write-line "            key = \"scale_1x\";" dcl_file)
      (write-line "            label = \"1x 插入（原始尺寸）\";" dcl_file)
      (write-line "            value = \"1\";" dcl_file)
      (write-line "          }" dcl_file)
      (write-line "          : radio_button {" dcl_file)
      (write-line "            key = \"scale_01x\";" dcl_file)
      (write-line "            label = \"0.1x 插入（缩小尺寸）\";" dcl_file)
      (write-line "            value = \"0\";" dcl_file)
      (write-line "          }" dcl_file)
      (write-line "        }" dcl_file)
      (write-line "      }" dcl_file)
      (write-line "    }" dcl_file)
      (write-line "    : boxed_column {" dcl_file)
      (write-line "      label = \"已选图块\";" dcl_file)
      (write-line "      width = 30;" dcl_file)
      (write-line "      : list_box {" dcl_file)
      (write-line "        key = \"selected_list\";" dcl_file)
      (write-line "        width = 25;" dcl_file)
      (write-line (strcat "        height = " (itoa list_height2) ";") dcl_file)
      (write-line "        alignment = \"fill\";" dcl_file)
      (write-line "        multiple_select = false;" dcl_file)
      (write-line "      }" dcl_file)
      (write-line "      : text {" dcl_file)
      (write-line "        key = \"selected_status\";" dcl_file)
      (write-line "        label = \"未选择图块\";" dcl_file)
      (write-line "      }" dcl_file)
      (write-line "    }" dcl_file)
      (write-line "    : boxed_column {" dcl_file)
      (write-line "      label = \"收藏夹\";" dcl_file)
      (write-line "      width = 22;" dcl_file)
      (write-line "      : list_box {" dcl_file)
      (write-line "        key = \"fav_list\";" dcl_file)
      (write-line "        width = 20;" dcl_file)
      (write-line (strcat "        height = " (itoa list_height2) ";") dcl_file)
      (write-line "        alignment = \"fill\";" dcl_file)
      (write-line "        multiple_select = false;" dcl_file)
      (write-line "      }" dcl_file)
      (write-line "      : spacer { height = 1; }" dcl_file)
      (write-line "      : button {" dcl_file)
      (write-line "        key = \"fav_add_btn\";" dcl_file)
      (write-line "        label = \"加入收藏\";" dcl_file)
      (write-line "        fixed_width = true;" dcl_file)
      (write-line "        width = 20;" dcl_file)
      (write-line "      }" dcl_file)
      (write-line "      : spacer { height = 0; }" dcl_file)
      (write-line "      : button {" dcl_file)
      (write-line "        key = \"fav_del_btn\";" dcl_file)
      (write-line "        label = \"删除选中\";" dcl_file)
      (write-line "        fixed_width = true;" dcl_file)
      (write-line "        width = 20;" dcl_file)
      (write-line "      }" dcl_file)
      (write-line "    }" dcl_file)
      (write-line "  }" dcl_file)
      (write-line "  : row {" dcl_file)
      (write-line "    : button {" dcl_file)
      (write-line "      key = \"set_path\";" dcl_file)
      (write-line "      label = \"设置库路径\";" dcl_file)
      (write-line "      width = 15;" dcl_file)
      (write-line "    }" dcl_file)
      (write-line "    : button {" dcl_file)
      (write-line "      key = \"open_path\";" dcl_file)
      (write-line "      label = \"打开\";" dcl_file)
      (write-line "      width = 8;" dcl_file)
      (write-line "    }" dcl_file)
      (write-line "    : button {" dcl_file)
      (write-line "      key = \"clear_selection\";" dcl_file)
      (write-line "      label = \"全部清除\";" dcl_file)
      (write-line "      width = 12;" dcl_file)
      (write-line "    }" dcl_file)
      (write-line "    : button {" dcl_file)
      (write-line "      key = \"create_block_btn\";" dcl_file)
      (write-line "      label = \"创建图块\";" dcl_file)
      (write-line "      width = 12;" dcl_file)
      (write-line "    }" dcl_file)
      (write-line "    : spacer { width = 1; }" dcl_file)
      (write-line "    ok_cancel;" dcl_file)
      (write-line "  }" dcl_file)
      (write-line "}" dcl_file)
      (close dcl_file)
      T
    )
    nil
  )
)

(defun SwitchToSubfolder (subfolder_name)
  "Switch to specified subfolder and update block list"
  (if (member subfolder_name *library_subfolders*)
    (progn
      (setq *current_subfolder* subfolder_name)
      (setq *library_blocks* (GetBlocksForSubfolder subfolder_name))
      ;; Remember this category so BSDW reopens on it next time
      (SaveCurrentSubfolder subfolder_name)
      ;; Update the library list in-place - no need to rebuild/reload the dialog
      (UpdateLibraryList)
    )
  )
)

(defun HandleSubfolderListSelection (selection_value / selected_index subfolder_name)
  "Handle a click on the scrollable category list (used when there are too
   many subfolders to show as tab-button rows)"
  (setq selected_index (atoi selection_value))
  (if (and (>= selected_index 0) (< selected_index (length *library_subfolders*)))
    (progn
      (setq subfolder_name (nth selected_index *library_subfolders*))
      (SwitchToSubfolder subfolder_name)
    )
  )
)

(defun UpdateLibraryList (/ block_list i block_info)
  "Update the library block list display"
  (if *library_blocks*
    (progn
      (setq block_list "")
      (setq i 0)
      (foreach block_info *library_blocks*
        (if (> i 0)
          (setq block_list (strcat block_list "\n"))
        )
        (setq block_list (strcat block_list (car block_info)))
        (setq i (+ i 1))
      )
      (start_list "library_list")
      (foreach block_info *library_blocks*
        (add_list (car block_info))
      )
      (end_list)
    )
  )
)

(defun UpdateSelectedLibraryBlocks (/ status_text i block_name block_info subfolder display_name)
  "Update selected blocks display - support duplicate block names"
  (if *selected_library_blocks*
    (progn
      (setq status_text (strcat (itoa (length *selected_library_blocks*)) " 个图块已选"))
      (set_tile "selected_status" status_text)
      
      ;; Populate the selected blocks list box
      (start_list "selected_list")
      (setq i 1)
      (foreach block_name *selected_library_blocks*
        ;; Get subfolder info if available
        (if (and *selected_library_block_info* 
                 (>= (length *selected_library_block_info*) i))
          (progn
            (setq block_info (nth (- i 1) *selected_library_block_info*))
            (setq subfolder (nth 2 block_info))
            ;; Create display name with subfolder info
            (if (and subfolder (not (equal subfolder "Main")))
              (setq display_name (strcat (itoa i) ". " block_name " [" subfolder "]"))
              (setq display_name (strcat (itoa i) ". " block_name))
            )
          )
          ;; Fallback if no block info available
          (setq display_name (strcat (itoa i) ". " block_name))
        )
        
        (add_list display_name)
        (setq i (+ i 1))
      )
      (end_list)
    )
    (progn
      (setq status_text "未选择图块")
      (set_tile "selected_status" status_text)
      (start_list "selected_list")
      (end_list)
    )
  )
)

(defun HandleLibrarySelection (selection_value / selected_index block_name block_info file_path complete_block_info existing_pos i entry)
  "Handle library list click - toggle selection: 1st click adds to the
   selected list, 2nd click on the SAME item removes it again, and so on."
  (setq selected_index (atoi selection_value))
  (if (and (>= selected_index 0) (< selected_index (length *library_blocks*)))
    (progn
      ;; Check whether this library item (by its index) is already selected
      (setq existing_pos nil)
      (setq i 1)
      (foreach entry *selected_library_block_info*
        (if (and (not existing_pos) (equal (nth 3 entry) selected_index))
          (setq existing_pos i)
        )
        (setq i (+ i 1))
      )
      (if existing_pos
        ;; Already selected -> this click removes it from the selection
        (RemoveLibrarySelection existing_pos)
        ;; Not selected yet -> this click adds it to the selection
        (progn
          (setq block_info (nth selected_index *library_blocks*))
          (setq block_name (car block_info))
          (setq file_path (cadr block_info))
          
          ;; Create complete block info with current subfolder context,
          ;; plus the source index so a later click can find it again
          (setq complete_block_info (list block_name file_path *current_subfolder* selected_index))
          
          (setq *selected_library_blocks* (append *selected_library_blocks* (list block_name)))
          (setq *selected_library_block_info* (append *selected_library_block_info* (list complete_block_info)))
          (setq *library_quantities* (append *library_quantities* (list 1)))
          ;; Update the selected list in-place - no need to rebuild/reload the dialog
          (UpdateSelectedLibraryBlocks)
        )
      )
    )
  )
)

(defun SetLibraryPathFromDialog (/ new_path)
  "Set library path from dialog using folder selection"
  ;; Use Windows folder selection dialog
  (setq new_path (SelectFolderDialog "选择图块库文件夹"))
  
  (if new_path
    (progn
      ;; Add trailing backslash if missing
      (if (not (= (substr new_path (strlen new_path)) "\\"))
        (setq new_path (strcat new_path "\\"))
      )
      
      ;; Set the new path
      (setq *library_path* new_path)
      
      ;; Save path permanently
      (SaveLibraryPath new_path)
      
      ;; Rescan the library
      (RescanLibrary)
      
      ;; Clear current selections since we have a new library
      (setq *selected_library_blocks* nil)
      (setq *library_quantities* nil)
      
      ;; Refresh dialog to show new library
      (done_dialog 2)
    )
    (progn
      ;; User cancelled folder selection
      (princ "\n文件夹选择已取消。")
    )
  )
)

(defun OpenLibraryFolder ( / )
  "Open the currently set library path folder in Windows Explorer"
  (cond
    ((not *library_path*)
     (princ "\n尚未设置库路径，请先点击\"设置库路径\"。")
    )
    ((not (vl-file-directory-p *library_path*))
     (princ (strcat "\n路径不存在或不是文件夹：" *library_path*))
    )
    (T
     (if (vl-catch-all-error-p
           (vl-catch-all-apply 'startapp (list "explorer.exe" *library_path*)))
       (princ (strcat "\n无法打开文件夹：" *library_path*))
       (princ (strcat "\n已打开文件夹：" *library_path*))
     )
    )
  )
  (princ)
)

(defun SelectFolderDialog (title / shell folder_obj selected_folder)
  "Show Windows folder selection dialog"
  ;; Try to use Windows Shell.Application to show folder dialog
  (if (not (vl-catch-all-error-p 
             (vl-catch-all-apply 'vlax-create-object (list "Shell.Application"))))
    (progn
      (setq shell (vlax-create-object "Shell.Application"))
      (setq folder_obj (vlax-invoke shell 'BrowseForFolder 0 title 1))
      
      (if folder_obj
        (progn
          (setq selected_folder (vlax-get-property (vlax-get-property folder_obj 'Self) 'Path))
          (vlax-release-object folder_obj)
        )
      )
      
      (vlax-release-object shell)
      selected_folder
    )
    ;; Fallback to manual input if COM object fails
    (progn
      (princ "\n无法使用文件夹对话框。请手动输入路径：")
      (getstring "\n输入图块库文件夹的完整路径：")
    )
  )
)

(defun RescanLibrary (/ dir_list block_name)
  "Rescan the current library path for blocks"
  (if *library_path*
    (progn
      ;; Get list of DWG files in library folder
      (setq dir_list (vl-directory-files *library_path* "*.dwg" 1))
      
      (if dir_list
        (progn
          (setq *library_blocks* nil)
          (foreach file_name dir_list
            ;; Use the filename without extension as the display name
            (setq block_name (vl-filename-base file_name))
            (setq *library_blocks* (append *library_blocks* (list (list block_name file_name))))
          )
          (princ (strcat "\n库已重新扫描：" (itoa (length *library_blocks*)) " 个图块"))
          T
        )
        (progn
          (setq *library_blocks* nil)
          (princ "\n所选目录中未找到 DWG 文件")
          nil
        )
      )
    )
    (progn
      (princ "\n未设置库路径")
      nil
    )
  )
)

;; ============================================================
;;  图块库收藏夹（分段存储路径列表，防止单键超 REG_SZ 上限 32767 字节）
;; ============================================================

;; 用分隔符连接字符串列表为单字符串（如 ("a" "b") "|" -> "a|b"）
(defun JoinFavoritesWithSep (lst sep / result)
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

;; 按分隔符切分字符串为列表（如 "a|b" "|" -> ("a" "b")）
(defun SplitFavoritesBySep (str sep / result sep-len total i part done)
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

;; 保存图块库收藏夹列表到注册表（分段存储）
(defun SaveLibraryFavorites (fav-list / joined total seg-count idx chunk key)
  (setq joined (JoinFavoritesWithSep fav-list "|"))
  (setq total (strlen joined))
  (setq seg-count 0)
  (if (> total 0)
    (progn
      (setq seg-count (/ (+ total *bsdw_fav_seg_max* -1) *bsdw_fav_seg_max*))
      (vl-registry-write
        "HKEY_CURRENT_USER\\Software\\AutoCAD\\BSDW_Favorites"
        "BSDW_FAV_COUNT"
        (itoa seg-count)
      )
      (setq idx 0)
      (while (< idx seg-count)
        (setq chunk (substr joined (+ 1 (* idx *bsdw_fav_seg_max*)) *bsdw_fav_seg_max*))
        (setq key (strcat "BSDW_FAV_" (itoa idx)))
        (vl-registry-write
          "HKEY_CURRENT_USER\\Software\\AutoCAD\\BSDW_Favorites"
          key
          chunk
        )
        (setq idx (1+ idx))
      )
    )
    (vl-registry-write
      "HKEY_CURRENT_USER\\Software\\AutoCAD\\BSDW_Favorites"
      "BSDW_FAV_COUNT"
      "0"
    )
  )
)

;; 从注册表加载图块库收藏夹列表（分段读取并拼接）
(defun LoadLibraryFavorites ( / seg-count joined idx key chunk)
  (setq seg-count (vl-registry-read
    "HKEY_CURRENT_USER\\Software\\AutoCAD\\BSDW_Favorites"
    "BSDW_FAV_COUNT"
  ))
  (if seg-count
    (progn
      (setq seg-count (atoi seg-count))
      (if (> seg-count 0)
        (progn
          (setq joined "")
          (setq idx 0)
          (while (< idx seg-count)
            (setq key (strcat "BSDW_FAV_" (itoa idx)))
            (setq chunk (vl-registry-read
              "HKEY_CURRENT_USER\\Software\\AutoCAD\\BSDW_Favorites"
              key
            ))
            (if chunk
              (setq joined (strcat joined chunk))
            )
            (setq idx (1+ idx))
          )
          (if (/= joined "")
            (SplitFavoritesBySep joined "|")
            '()
          )
        )
        '()
      )
    )
    '()
  )
)

;; 获取收藏夹中文件夹的显示名称（取最后一级文件夹名）
(defun GetFavoriteDisplayName (folder / last-part)
  (setq folder (vl-string-right-trim "\\" folder))
  (setq last-part folder)
  (while (vl-string-search "\\" last-part)
    (setq last-part (substr last-part (+ (vl-string-search "\\" last-part) 2)))
  )
  (if (or (not last-part) (= last-part ""))
    folder
    last-part
  )
)

;; 刷新收藏夹列表显示（带文件夹图标），并高亮与当前库路径匹配的收藏项
(defun RefreshFavoriteList ( / fav-list i cur-norm f-norm)
  (setq fav-list (LoadLibraryFavorites))
  (setq *library_favorites* fav-list)
  (start_list "fav_list")
  (foreach f fav-list
    (add_list (strcat "  \004  " (GetFavoriteDisplayName f) "  "))
  )
  (end_list)
  ;; 若当前库路径就是某个收藏夹，刷新后重新选中该项，避免选中状态丢失
  (if *library_path*
    (progn
      (setq cur-norm (strcase (vl-string-right-trim "\\" *library_path*)))
      (setq i 0)
      (foreach f fav-list
        (setq f-norm (strcase (vl-string-right-trim "\\" f)))
        (if (equal f-norm cur-norm)
          (set_tile "fav_list" (itoa i))
        )
        (setq i (1+ i))
      )
    )
  )
)

;; 收藏夹列表：单击切换图块库路径（刷新后会自动重新高亮该项，见 RefreshFavoriteList）
(defun HandleFavoriteListSelection (selection_value / selected_index fav_path)
  "Switch the block library path to the selected favorite folder"
  (setq selected_index (atoi selection_value))
  (setq fav_path (nth selected_index *library_favorites*))
  (if (and fav_path (vl-file-directory-p fav_path))
    (progn
      (setq *library_path* fav_path)
      (SaveLibraryPath fav_path)
      (if (ScanBlockLibrary)
        (progn
          (setq *selected_library_blocks* nil)
          (setq *selected_library_block_info* nil)
          (setq *library_quantities* nil)
          ;; Refresh dialog to show the newly switched library
          (done_dialog 2)
        )
        (princ "\n该收藏夹中未找到可用图块。")
      )
    )
    (princ "\n收藏夹路径无效。")
  )
)

;; 加入收藏按钮：将当前图块库路径加入收藏夹
(defun AddCurrentPathToFavorites ( / fav-list)
  (if (and *library_path* (vl-file-directory-p *library_path*))
    (progn
      (setq fav-list (LoadLibraryFavorites))
      (if (not (member (strcase *library_path*) (mapcar 'strcase fav-list)))
        (progn
          (setq fav-list (append fav-list (list *library_path*)))
          (SaveLibraryFavorites fav-list)
          (RefreshFavoriteList)
          (princ (strcat "\n已加入收藏：" (GetFavoriteDisplayName *library_path*)))
        )
        (princ "\n该文件夹已在收藏夹中。")
      )
    )
    (princ "\n当前库路径无效，无法加入收藏。")
  )
)

;; 删除收藏按钮：删除收藏夹列表中选中的项
(defun RemoveSelectedFavorite ( / selected_index fav_to_del fav-list)
  (setq selected_index (get_tile "fav_list"))
  (if (and selected_index (/= selected_index ""))
    (progn
      (setq fav-list (LoadLibraryFavorites))
      (setq fav_to_del (nth (atoi selected_index) fav-list))
      (if fav_to_del
        (progn
          (setq fav-list
            (vl-remove-if
              (function (lambda (x) (equal (strcase x) (strcase fav_to_del))))
              fav-list
            )
          )
          (SaveLibraryFavorites fav-list)
          (RefreshFavoriteList)
          (princ (strcat "\n已删除收藏：" (GetFavoriteDisplayName fav_to_del)))
        )
      )
    )
    (princ "\n请先选择要删除的收藏项。")
  )
)

;; ============================================================
;;  从图纸选择创建新图块，保存到当前选定的库分类文件夹
;; ============================================================

;; ============================================================
;;  图块命名对话框（支持手动输入 / 拾取图纸中的文字作为名称）
;; ============================================================

;; 生成命名对话框的DCL文件
(defun CreateNameInputDCLFile (filename / dcl_file)
  (setq dcl_file (open filename "w"))
  (if dcl_file
    (progn
      (write-line "bsdw_name_input : dialog {" dcl_file)
      (write-line "  label = \"输入图块名称\";" dcl_file)
      (write-line "  : row {" dcl_file)
      (write-line "    : edit_box {" dcl_file)
      (write-line "      key = \"name_edit\";" dcl_file)
      (write-line "      label = \"名称:\";" dcl_file)
      (write-line "      edit_width = 30;" dcl_file)
      (write-line "    }" dcl_file)
      (write-line "    : button {" dcl_file)
      (write-line "      key = \"pick_txt_btn\";" dcl_file)
      (write-line "      label = \"拾取文字\";" dcl_file)
      (write-line "      fixed_width = true;" dcl_file)
      (write-line "      width = 10;" dcl_file)
      (write-line "    }" dcl_file)
      (write-line "  }" dcl_file)
      (write-line "  : row {" dcl_file)
      (write-line "    : text {" dcl_file)
      (write-line "      label = \"基点:\";" dcl_file)
      (write-line "    }" dcl_file)
      (write-line "    : radio_row {" dcl_file)
      (write-line "      key = \"basepoint_group\";" dcl_file)
      (write-line "      : radio_button {" dcl_file)
      (write-line "        key = \"bp_tl\";" dcl_file)
      (write-line "        label = \"左上\";" dcl_file)
      (write-line "      }" dcl_file)
      (write-line "      : radio_button {" dcl_file)
      (write-line "        key = \"bp_tr\";" dcl_file)
      (write-line "        label = \"右上\";" dcl_file)
      (write-line "      }" dcl_file)
      (write-line "      : radio_button {" dcl_file)
      (write-line "        key = \"bp_bl\";" dcl_file)
      (write-line "        label = \"左下\";" dcl_file)
      (write-line "      }" dcl_file)
      (write-line "      : radio_button {" dcl_file)
      (write-line "        key = \"bp_br\";" dcl_file)
      (write-line "        label = \"右下\";" dcl_file)
      (write-line "      }" dcl_file)
      (write-line "      : radio_button {" dcl_file)
      (write-line "        key = \"bp_center\";" dcl_file)
      (write-line "        label = \"几何中心\";" dcl_file)
      (write-line "      }" dcl_file)
      (write-line "    }" dcl_file)
      (write-line "  }" dcl_file)
      (write-line "  : row {" dcl_file)
      (write-line "    : button {" dcl_file)
      (write-line "      key = \"ok\";" dcl_file)
      (write-line "      label = \"确定\";" dcl_file)
      (write-line "      is_default = true;" dcl_file)
      (write-line "      width = 10;" dcl_file)
      (write-line "    }" dcl_file)
      (write-line "    : button {" dcl_file)
      (write-line "      key = \"cancel\";" dcl_file)
      (write-line "      label = \"取消\";" dcl_file)
      (write-line "      is_cancel = true;" dcl_file)
      (write-line "      width = 10;" dcl_file)
      (write-line "    }" dcl_file)
      (write-line "  }" dcl_file)
      (write-line "}" dcl_file)
      (close dcl_file)
      T
    )
    nil
  )
)

;; 去除MTEXT字符串中的常见格式控制码（\A、\f、\C、\H、\W 等及 {} 分组），
;; 便于把选中的文字内容用作干净的图块名称
(defun CleanMTextFormatting (str / result i ch len)
  (setq result "")
  (setq i 1)
  (setq len (strlen str))
  (while (<= i len)
    (setq ch (substr str i 1))
    (cond
      ;; 转义反斜杠 \\  -> 字面反斜杠
      ((and (= ch "\\") (<= (1+ i) len) (= (substr str (1+ i) 1) "\\"))
       (setq result (strcat result "\\"))
       (setq i (+ i 2))
      )
      ;; 段落换行 \P 或 \p -> 当作空格处理
      ((and (= ch "\\") (<= (1+ i) len) (member (substr str (1+ i) 1) (list "P" "p")))
       (setq result (strcat result " "))
       (setq i (+ i 2))
      )
      ;; 其它格式控制码：\<字母>...; -> 整段跳过
      ((and (= ch "\\") (<= (1+ i) len))
       (setq i (+ i 2))
       (while (and (<= i len) (/= (substr str i 1) ";"))
         (setq i (1+ i))
       )
       (setq i (1+ i))
      )
      ;; 花括号分组符号 -> 丢弃
      ((or (= ch "{") (= ch "}"))
       (setq i (1+ i))
      )
      (T
       (setq result (strcat result ch))
       (setq i (1+ i))
      )
    )
  )
  result
)

;; 将字符串中不允许出现在图块/文件名中的字符替换为下划线
(defun SanitizeBlockName (str / result ch i len bad)
  (setq bad (list " " "<" ">" "/" "\\" "\"" ":" ";" "?" "*" "|" "," "=" "`"))
  (setq result "")
  (setq i 1)
  (setq len (strlen str))
  (while (<= i len)
    (setq ch (substr str i 1))
    (if (member ch bad)
      (setq result (strcat result "_"))
      (setq result (strcat result ch))
    )
    (setq i (1+ i))
  )
  result
)

;; 拾取图纸中的一个文字对象(TEXT/MTEXT/属性定义)，返回其文字内容
(defun PickTextForBlockName ( / ent_result ent edata etype text_str)
  (setq ent_result (entsel "\n请选择一个文字对象(TEXT/MTEXT)，将以其内容作为图块名称: "))
  (if ent_result
    (progn
      (setq ent (car ent_result))
      (setq edata (entget ent))
      (setq etype (cdr (assoc 0 edata)))
      (if (member etype (list "TEXT" "MTEXT" "ATTDEF"))
        (progn
          (setq text_str (cdr (assoc 1 edata)))
          (if (= etype "MTEXT")
            (setq text_str (CleanMTextFormatting text_str))
          )
          (setq text_str (vl-string-trim " \t" text_str))
          (if (= text_str "")
            (progn
              (princ "\n所选文字对象内容为空。")
              nil
            )
            text_str
          )
        )
        (progn
          (princ "\n所选对象不是文字(TEXT/MTEXT)对象。")
          nil
        )
      )
    )
    nil
  )
)

;; 显示命名对话框，让用户手动输入或点击"拾取文字"从图纸中选取文字作为名称
;; 返回校验通过的图块名称，取消则返回 nil
(defun GetBlockNameFromDialog ( / dcl_id result continue_loop temp_file entered_name picked_text)
  (if (not *bsdw_pending_name*)
    (setq *bsdw_pending_name* "")
  )
  (if (not *bsdw_pending_basepoint*)
    (setq *bsdw_pending_basepoint* "bp_tl")
  )
  (setq continue_loop T)
  (setq entered_name nil)
  (while continue_loop
    (setq temp_file (strcat (getvar "TEMPPREFIX") "bsdw_name_dialog.dcl"))
    (CreateNameInputDCLFile temp_file)
    (setq dcl_id (load_dialog temp_file))
    (if (< dcl_id 0)
      (progn
        (alert "无法加载命名对话框。")
        (setq continue_loop nil)
      )
      (progn
        (if (not (new_dialog "bsdw_name_input" dcl_id))
          (progn
            (alert "无法初始化命名对话框。")
            (setq continue_loop nil)
          )
          (progn
            (set_tile "name_edit" *bsdw_pending_name*)
            ;; 回填基点单选框的选中状态（5选1：左上/右上/左下/右下/几何中心）
            (set_tile "bp_tl" (if (= *bsdw_pending_basepoint* "bp_tl") "1" "0"))
            (set_tile "bp_tr" (if (= *bsdw_pending_basepoint* "bp_tr") "1" "0"))
            (set_tile "bp_bl" (if (= *bsdw_pending_basepoint* "bp_bl") "1" "0"))
            (set_tile "bp_br" (if (= *bsdw_pending_basepoint* "bp_br") "1" "0"))
            (set_tile "bp_center" (if (= *bsdw_pending_basepoint* "bp_center") "1" "0"))
            (action_tile "bp_tl" "(setq *bsdw_pending_basepoint* \"bp_tl\")")
            (action_tile "bp_tr" "(setq *bsdw_pending_basepoint* \"bp_tr\")")
            (action_tile "bp_bl" "(setq *bsdw_pending_basepoint* \"bp_bl\")")
            (action_tile "bp_br" "(setq *bsdw_pending_basepoint* \"bp_br\")")
            (action_tile "bp_center" "(setq *bsdw_pending_basepoint* \"bp_center\")")
            ;; 拾取文字：先保存当前编辑框内容，再关闭对话框以便在图纸上拾取
            (action_tile "pick_txt_btn"
              "(setq *bsdw_pending_name* (get_tile \"name_edit\")) (done_dialog 2)"
            )
            (action_tile "ok"
              "(setq *bsdw_pending_name* (get_tile \"name_edit\")) (done_dialog 1)"
            )
            (action_tile "cancel" "(done_dialog 0)")
            (setq result (start_dialog))
            (cond
              ((= result 1)
                (cond
                  ((or (not *bsdw_pending_name*) (= *bsdw_pending_name* ""))
                   (alert "名称不能为空，请重新输入。")
                  )
                  ((not (snvalid *bsdw_pending_name*))
                   (alert "名称中含有非法字符，请重新输入。")
                  )
                  (T
                   (setq entered_name *bsdw_pending_name*)
                   (setq continue_loop nil)
                  )
                )
              )
              ((= result 2)
                ;; 拾取文字按钮：对话框已关闭，可安全在图纸上拾取对象
                (setq picked_text (PickTextForBlockName))
                (if picked_text
                  (setq *bsdw_pending_name* (SanitizeBlockName picked_text))
                )
                ;; 循环继续，重新打开命名对话框并回填拾取到的内容
              )
              (T
                (setq continue_loop nil)
              )
            )
          )
        )
        (unload_dialog dcl_id)
        (vl-file-delete temp_file)
      )
    )
  )
  entered_name
)

;; 根据选择集中所有对象的整体包围盒，按命名对话框中选择的基点模式计算插入基点
;; mode: "bp_tl"=左上  "bp_tr"=右上  "bp_bl"=左下  "bp_br"=右下  "bp_center"=几何中心
;; 若选择集为空、或所有对象的包围盒计算均失败，则返回 nil（由调用方决定后备方案）
(defun ComputeLibraryBlockBasePoint (ss mode / i n ent vla_obj min_pt max_pt
                                          box_min box_max got_box
                                          overall_min_x overall_min_y overall_min_z
                                          overall_max_x overall_max_y overall_max_z
                                          mid_z)
  (setq n (sslength ss))
  (setq i 0)
  (setq got_box nil)
  (while (< i n)
    (setq ent (ssname ss i))
    (setq vla_obj (vlax-ename->vla-object ent))
    (if (not (vl-catch-all-error-p
               (vl-catch-all-apply 'vla-GetBoundingBox (list vla_obj 'min_pt 'max_pt))))
      (progn
        (setq box_min (vlax-safearray->list min_pt))
        (setq box_max (vlax-safearray->list max_pt))
        (if (not got_box)
          (progn
            (setq overall_min_x (car box_min))
            (setq overall_min_y (cadr box_min))
            (setq overall_min_z (caddr box_min))
            (setq overall_max_x (car box_max))
            (setq overall_max_y (cadr box_max))
            (setq overall_max_z (caddr box_max))
            (setq got_box T)
          )
          (progn
            (if (< (car box_min) overall_min_x) (setq overall_min_x (car box_min)))
            (if (< (cadr box_min) overall_min_y) (setq overall_min_y (cadr box_min)))
            (if (< (caddr box_min) overall_min_z) (setq overall_min_z (caddr box_min)))
            (if (> (car box_max) overall_max_x) (setq overall_max_x (car box_max)))
            (if (> (cadr box_max) overall_max_y) (setq overall_max_y (cadr box_max)))
            (if (> (caddr box_max) overall_max_z) (setq overall_max_z (caddr box_max)))
          )
        )
      )
    )
    (setq i (1+ i))
  )
  (if (not got_box)
    nil
    (progn
      (setq mid_z (/ (+ overall_min_z overall_max_z) 2.0))
      (cond
        ((= mode "bp_tl") (list overall_min_x overall_max_y mid_z))
        ((= mode "bp_tr") (list overall_max_x overall_max_y mid_z))
        ((= mode "bp_bl") (list overall_min_x overall_min_y mid_z))
        ((= mode "bp_br") (list overall_max_x overall_min_y mid_z))
        ((= mode "bp_center")
         (list (/ (+ overall_min_x overall_max_x) 2.0)
               (/ (+ overall_min_y overall_max_y) 2.0)
               mid_z))
        (T (list overall_min_x overall_max_y mid_z)) ;; 未知模式时默认左上
      )
    )
  )
)

(defun CreateLibraryBlockFromSelection ( / ss block_name target_folder
                                          save_path base_pt old_filedia old_cmdecho
                                          overwrite_answer)
  "Prompt the user to select objects, name them, and save the selection as
   a new block DWG file inside the currently selected library category"
  (if (not (and *library_path* (vl-file-directory-p *library_path*)))
    (princ "\n尚未设置有效的图块库路径，请先设置库路径。")
    (progn
      (princ "\n请选择要制作成图块的对象：")
      (setq ss (ssget))
      (if (not ss)
        (princ "\n未选择任何对象，操作已取消。")
        (progn
          ;; Ask for a valid block/file name via a small dialog (supports
          ;; picking a TXT/MTEXT object's content as the name)
          (setq block_name (GetBlockNameFromDialog))
          (if (not block_name)
            (princ "\n已取消，未输入图块名称。")
            (progn
          (setq target_folder *library_path*)
          (if (and *current_subfolder* (/= *current_subfolder* "Main"))
            (setq target_folder (strcat *library_path* *current_subfolder* "\\"))
          )
          (if (not (vl-file-directory-p target_folder))
            (vl-mkdir target_folder)
          )
          (setq save_path (strcat target_folder block_name ".dwg"))
          ;; Confirm overwrite if a block with this name already exists
          (setq overwrite_answer "Y")
          (if (findfile save_path)
            (progn
              (initget "Yes No")
              (setq overwrite_answer
                (getkword (strcat "\n\"" block_name "\" 已存在于该分类中，是否覆盖？[Yes/No] <No>: ")))
              (if (not overwrite_answer) (setq overwrite_answer "No"))
              (setq overwrite_answer (if (= overwrite_answer "Yes") "Y" "N"))
            )
          )
          (if (/= overwrite_answer "Y")
            (princ "\n已取消，未覆盖现有图块。")
            (progn
              (setq base_pt (ComputeLibraryBlockBasePoint ss *bsdw_pending_basepoint*))
              (if (not base_pt)
                (progn
                  (setq base_pt (getpoint "\n无法自动计算基点，请手动指定图块插入基点: "))
                  (if (not base_pt) (setq base_pt (list 0 0 0)))
                )
              )
              (setq old_filedia (getvar "FILEDIA"))
              (setq old_cmdecho (getvar "CMDECHO"))
              (setvar "FILEDIA" 0)
              (setvar "CMDECHO" 0)
              (if (findfile save_path)
                (vl-file-delete save_path)
              )
              ;; -WBLOCK: output file, "" = define new drawing, base point, selection set, "" ends selection
              (command "_.-WBLOCK" save_path "" base_pt ss "")
              ;; -WBLOCK erases the selected objects from the current drawing after
              ;; writing them out; OOPS restores them exactly as they were, so the
              ;; original objects stay untouched and are NOT converted into a block
              (command "_.OOPS")
              (setvar "FILEDIA" old_filedia)
              (setvar "CMDECHO" old_cmdecho)
              (if (findfile save_path)
                (progn
                  (princ (strcat "\n图块已保存：" save_path))
                  ;; Rescan so the new block shows up immediately (keeps the
                  ;; last-used category, see SaveCurrentSubfolder/LoadCurrentSubfolder)
                  (ScanBlockLibrary)
                )
                (princ "\n图块保存失败。")
              )
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

(defun ClearLibrarySelection ()
  "Clear all selected library blocks"
  (setq *selected_library_blocks* nil)
  (setq *selected_library_block_info* nil)
  (setq *library_quantities* nil)
  ;; Update the selected list in-place - no need to rebuild/reload the dialog
  (UpdateSelectedLibraryBlocks)
)

(defun RemoveLibrarySelection (index / new_blocks new_block_info new_quantities i)
  "Remove a single selected library block at specific index"
  (if (and (>= index 1) (<= index (length *selected_library_blocks*)))
    (progn
      ;; Rebuild lists without the item at the specified index
      (setq new_blocks nil)
      (setq new_block_info nil)
      (setq new_quantities nil)
      (setq i 1)
      
      ;; Copy all items except the one to be removed
      (foreach block_name *selected_library_blocks*
        (if (not (= i index))
          (progn
            (setq new_blocks (append new_blocks (list block_name)))
            (setq new_block_info (append new_block_info (list (nth (- i 1) *selected_library_block_info*))))
            (setq new_quantities (append new_quantities (list (nth (- i 1) *library_quantities*))))
          )
        )
        (setq i (+ i 1))
      )
      
      ;; Update global variables
      (setq *selected_library_blocks* new_blocks)
      (setq *selected_library_block_info* new_block_info)
      (setq *library_quantities* new_quantities)
      
      ;; Update the selected list in-place - no need to rebuild/reload the dialog
      (UpdateSelectedLibraryBlocks)
    )
  )
)

(defun c:bsdquantitytest ()
  "Test function to check quantity updates"
  (princ "\n=== 数量测试 ===")
  (princ (strcat "\n已选图块数量：" (itoa (if *selected_library_blocks* (length *selected_library_blocks*) 0))))
  (princ (strcat "\n数量项数量：" (itoa (if *library_quantities* (length *library_quantities*) 0))))
  
  (if (and *selected_library_blocks* *library_quantities*)
    (progn
      (setq i 1)
      (foreach block_name *selected_library_blocks*
        (setq qty (nth (- i 1) *library_quantities*))
        (princ (strcat "\n" (itoa i) ". " block_name " -> " (itoa qty)))
        (setq i (+ i 1))
      )
    )
  )
  
  (princ "\n=== 测试结束 ===")
  (princ)
)

(defun ExecuteLibraryArrays (/ i block_name block_info file_path insert_pt doc all_block_groups base_y current_y spacing block_group)
  "Execute array operations for selected library blocks"
  (if (and *selected_library_blocks* *library_quantities* *selected_library_block_info*)
    (progn
      (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
      (vla-StartUndoMark doc)
      
      ;; Get insertion point from user
      (setq insert_pt (getpoint "\n指定插入点："))
      
      (if insert_pt
        (progn
          ;; Initialize variables
          (setq all_block_groups nil)
          (setq base_y (cadr insert_pt))
          (setq current_y base_y)
          (setq spacing 3.0) ;; Default vertical spacing between different block types
          
          (setq i 0)
          (foreach block_name *selected_library_blocks*
            ;; Get the complete block info for this selection
            (setq block_info (nth i *selected_library_block_info*))
            (setq file_path (GetBlockFilePathFromInfo block_info))
            
            (if file_path
              (progn
                ;; Calculate Y position for this row (vertical arrangement)
                (if (> i 0)
                  (setq current_y (- current_y spacing))
                )
                
                ;; Insert and array this block type
                (setq block_group (InsertAndArrayLibraryBlockAdvanced 
                                    block_name 
                                    file_path 
                                    (list (car insert_pt) current_y) 
                                    (nth i *library_quantities*)))
                
                ;; Store this block group
                (if block_group
                  (setq all_block_groups (append all_block_groups (list block_group)))
                )
              )
              (progn
                (princ (strcat "\n警告：找不到图块的文件路径：" block_name))
              )
            )
            (setq i (+ i 1))
          )
          
          ;; Realign all block groups horizontally (same as BSD)
          (if all_block_groups
            (RealignLibraryBlockGroupsHorizontally all_block_groups base_y)
          )
        )
      )
      
      (vla-EndUndoMark doc)
    )
  )
)

(defun InsertAndArrayLibraryBlockAdvanced (block_name file_path base_pt quantity / 
                                          i insert_pt block_group first_ent vla_obj array_blocks temp_insert_pt scale_factor block_width)
  "Insert block from library and create horizontal array, return all created blocks - OPTIMIZED VERSION"
  
  (setq block_group nil)
  
  ;; Get scale factor from global variable
  (setq scale_factor (atof *insertion_scale*))
  
  ;; For single block, use simple insertion
  (if (= quantity 1)
    (progn
      (setq temp_insert_pt (list (car base_pt) (cadr base_pt)))
      (command "_.INSERT" file_path temp_insert_pt scale_factor scale_factor 0)
      (setq first_ent (entlast))
      (if first_ent
        (progn
          (setq vla_obj (vlax-ename->vla-object first_ent))
          (setq block_group (list vla_obj))
        )
      )
    )
    ;; For multiple blocks, use optimized array method
    (progn
      ;; Insert first block to get its width for proper spacing
      (setq temp_insert_pt (list (car base_pt) (cadr base_pt)))
      (command "_.INSERT" file_path temp_insert_pt scale_factor scale_factor 0)
      
      ;; Get the first inserted entity and convert to VLA object
      (setq first_ent (entlast))
      (if first_ent
        (progn
          (setq vla_obj (vlax-ename->vla-object first_ent))
          
          ;; Get block width for spacing calculation
          (setq block_width (GetBlockWidth vla_obj))
          
          ;; Use optimized array method based on quantity
          (if (> quantity 10)
            ;; For large quantities, use AutoCAD's ARRAY command
            (setq block_group (CreateLargeArrayOptimized vla_obj quantity block_width base_pt))
            ;; For smaller quantities, use the original method but optimized
            (setq block_group (CreateSmallArrayOptimized vla_obj quantity block_width base_pt file_path scale_factor))
          )
        )
      )
    )
  )
  
  block_group ;; Return the list of created block objects
)

(defun CreateLargeArrayOptimized (base_block quantity block_width base_pt / 
                                 doc space block_group ss i ent vla_obj new_ent new_pt)
  "Create large array using VLA Copy method for better performance"
  
  (setq block_group (list base_block))
  
  ;; For very large quantities, use batched approach
  (if (> quantity 50)
    (progn
      ;; Process in batches to avoid memory issues
      (setq batch_size 25)
      (setq batches (/ (- quantity 1) batch_size))
      (setq remaining (rem (- quantity 1) batch_size))
      
      (setq i 1)
      ;; Process full batches
      (repeat batches
        (repeat batch_size
          ;; Calculate position for this block
          (setq new_pt (list (+ (car base_pt) (* i block_width)) (cadr base_pt) 0.0))
          
          ;; Use VLA Copy method
          (setq new_ent (vla-Copy base_block))
          (vla-put-InsertionPoint new_ent (vlax-3d-point new_pt))
          
          ;; Add to block group
          (setq block_group (append block_group (list new_ent)))
          (setq i (+ i 1))
        )
        ;; Small pause between batches
        (princ (strcat "\r处理中：" (itoa i) "/" (itoa quantity)))
      )
      
      ;; Process remaining blocks
      (repeat remaining
        (setq new_pt (list (+ (car base_pt) (* i block_width)) (cadr base_pt) 0.0))
        (setq new_ent (vla-Copy base_block))
        (vla-put-InsertionPoint new_ent (vlax-3d-point new_pt))
        (setq block_group (append block_group (list new_ent)))
        (setq i (+ i 1))
      )
      (princ "\r完成！                    ")
    )
    ;; For moderate quantities, use simple loop
    (progn
      (setq i 1)
      (repeat (- quantity 1)
        ;; Calculate position for this block
        (setq new_pt (list (+ (car base_pt) (* i block_width)) (cadr base_pt) 0.0))
        
        ;; Use VLA Copy method
        (setq new_ent (vla-Copy base_block))
        (vla-put-InsertionPoint new_ent (vlax-3d-point new_pt))
        
        ;; Add to block group
        (setq block_group (append block_group (list new_ent)))
        (setq i (+ i 1))
      )
    )
  )
  
  block_group
)

(defun CreateSmallArrayOptimized (base_block quantity block_width base_pt file_path scale_factor / 
                                 block_group i insert_pt new_ent vla_obj new_pt)
  "Create small array using optimized copy method"
  
  (setq block_group (list base_block))
  
  ;; Use VLA Copy method which is faster than INSERT command for existing blocks
  (setq i 1)
  (repeat (- quantity 1)
    ;; Calculate position for this block
    (setq new_pt (list (+ (car base_pt) (* i block_width)) (cadr base_pt) 0.0))
    
    ;; Use VLA Copy method instead of INSERT command
    (setq new_ent (vla-Copy base_block))
    
    ;; Move the copied block to the correct position using VLA method
    (vla-put-InsertionPoint new_ent (vlax-3d-point new_pt))
    
    ;; Add to block group (use cons for better performance, then reverse at end)
    (setq block_group (cons new_ent block_group))
    
    (setq i (+ i 1))
  )
  
  ;; Reverse to maintain left-to-right order
  (reverse block_group)
)

(defun RealignLibraryBlockGroupsHorizontally (all_block_groups target_y / i current_x group_width block_group j block_obj block_insert_pt new_x block_width)
  "Realign all library block groups horizontally on the same line"
  
  (setq current_x nil) ;; Will be set from first block
  
  (setq i 0)
  (repeat (length all_block_groups)
    (setq block_group (nth i all_block_groups))
    
    ;; For first group, get starting X and properly space the blocks
    (if (= i 0)
      (progn
        ;; Get starting X from first block
        (setq block_obj (nth 0 block_group))
        (setq block_insert_pt (GetBlockInsertionPoint block_obj))
        (setq current_x (car block_insert_pt))
        
        ;; Properly space all blocks in the first group
        (setq j 0)
        (repeat (length block_group)
          (setq block_obj (nth j block_group))
          (setq block_width (GetBlockWidth block_obj))
          
          ;; Calculate new X position for this block
          (setq new_x (+ current_x (* j block_width)))
          
          ;; Move block to correct position
          (MoveBlockToXYPosition block_obj new_x target_y)
          
          (setq j (+ j 1))
        )
        
        ;; Calculate width of first group for next group positioning
        (setq group_width (CalculateLibraryGroupWidth block_group))
        (setq current_x (+ current_x group_width))
      )
      (progn
        ;; Move subsequent groups to current_x position and target Y
        (setq j 0)
        (repeat (length block_group)
          (setq block_obj (nth j block_group))
          (setq block_width (GetBlockWidth block_obj))
          
          ;; Calculate new X position for this block within the group
          (setq new_x (+ current_x (* j block_width)))
          
          ;; Move block to new position
          (MoveBlockToXYPosition block_obj new_x target_y)
          
          (setq j (+ j 1))
        )
        
        ;; Update current_x for next group
        (setq group_width (CalculateLibraryGroupWidth block_group))
        (setq current_x (+ current_x group_width))
      )
    )
    
    (setq i (+ i 1))
  )
)

(defun CalculateLibraryGroupWidth (block_group / total_width i block_obj block_width)
  "Calculate total width of a library block group"
  (setq total_width 0.0)
  (setq i 0)
  (repeat (length block_group)
    (setq block_obj (nth i block_group))
    (setq block_width (GetBlockWidth block_obj))
    (setq total_width (+ total_width block_width))
    (setq i (+ i 1))
  )
  total_width
)

(defun GetBlockFilePathFromInfo (block_info / block_name file_path subfolder)
  "Get full file path from block info structure"
  (setq block_name (nth 0 block_info))
  (setq file_path (nth 1 block_info))
  (setq subfolder (nth 2 block_info))
  
  (if file_path
    (progn
      ;; Check if file_path already contains subfolder path
      (if (wcmatch file_path "*\\*")
        ;; File is in subfolder, use as is
        (strcat *library_path* file_path)
        ;; File is in main directory
        (strcat *library_path* file_path)
      )
    )
    nil
  )
)

(defun GetBlockFilePath (block_name / file_info)
  "Get full file path for a block name (legacy function for compatibility)"
  (foreach block_info *library_blocks*
    (if (equal (car block_info) block_name)
      (setq file_info (cadr block_info))
    )
  )
  (if file_info
    (progn
      ;; Check if file_info already contains subfolder path
      (if (wcmatch file_info "*\\*")
        ;; File is in subfolder, use as is
        (strcat *library_path* file_info)
        ;; File is in main directory
        (strcat *library_path* file_info)
      )
    )
    nil
  )
)


(defun ShowLibraryDialog (/ dcl_id result continue_loop temp_file i tile_key tab_index tab_key)
  "Display dialog for library block selection"
  
  ;; Dialog loop with refresh capability
  (setq continue_loop T)
  
  (while continue_loop
    ;; Create temporary DCL file (recreate each time for dynamic content)
    (setq temp_file (strcat (getvar "TEMPPREFIX") "bsdw_library_dialog.dcl"))
    
    ;; Write DCL content
    (CreateLibraryDCLFile temp_file)
    
    ;; Load dialog
    (setq dcl_id (load_dialog temp_file))
    
    (if (< dcl_id 0)
      (progn
        (alert "加载库对话框文件失败。")
        (princ "\n错误：无法加载 DCL 文件。")
        (setq continue_loop nil)
      )
      (progn
        ;; Initialize dialog
        (if (not (new_dialog "bsdw_library" dcl_id))
          (progn
            (alert "初始化库对话框失败。")
            (setq continue_loop nil)
          )
          (progn
            ;; Update displays
            (UpdateLibraryList)
            (UpdateSelectedLibraryBlocks)
            (RefreshFavoriteList)
            
            ;; Populate the scrollable category list when there are too many
            ;; subfolders to show as tab-button rows
            (if *library_tabs_as_list*
              (progn
                (start_list "subfolder_list")
                (foreach subfolder_name *library_subfolders*
                  (add_list subfolder_name)
                )
                (end_list)
                ;; Highlight the currently active category, if found
                (setq i 0)
                (foreach subfolder_name *library_subfolders*
                  (if (equal subfolder_name *current_subfolder*)
                    (set_tile "subfolder_list" (itoa i))
                  )
                  (setq i (+ i 1))
                )
              )
            )
            
            ;; Initialize scale radio buttons
            (if (equal *insertion_scale* "1.0")
              (progn
                (set_tile "scale_1x" "1")
                (set_tile "scale_01x" "0")
              )
              (progn
                (set_tile "scale_1x" "0")
                (set_tile "scale_01x" "1")
              )
            )
            
            ;; Handle scale radio button changes - also persist the choice
            (action_tile "scale_1x" "(SetInsertionScale \"1.0\")")
            (action_tile "scale_01x" "(SetInsertionScale \"0.1\")")
            
            ;; Handle category selection - either the scrollable list (when
            ;; there are too many subfolders) or the individual tab buttons
            (if *library_tabs_as_list*
              (action_tile "subfolder_list" "(HandleSubfolderListSelection $value)")
              (if *library_subfolders*
                (progn
                  (setq tab_index 1)
                  (foreach subfolder_name *library_subfolders*
                    (setq tab_key (strcat "tab_" (itoa tab_index)))
                    (action_tile tab_key (strcat "(SwitchToSubfolder \"" subfolder_name "\")"))
                    (setq tab_index (+ tab_index 1))
                  )
                )
              )
            )
            
            ;; Handle library list selection
            (action_tile "library_list" "(HandleLibrarySelection $value)")
            
            ;; Handle set path button
            (action_tile "set_path" "(SetLibraryPathFromDialog)")
            
            ;; Handle open path button
            (action_tile "open_path" "(OpenLibraryFolder)")
            
            ;; Handle clear selection button
            (action_tile "clear_selection" "(ClearLibrarySelection)")
            
            ;; Handle favorites list selection - switch library path
            ;; (RefreshFavoriteList re-highlights the matching item after reload)
            (action_tile "fav_list" "(HandleFavoriteListSelection $value)")
            
            ;; Handle add-to-favorites button
            (action_tile "fav_add_btn" "(AddCurrentPathToFavorites)")
            
            ;; Handle remove-favorite button
            (action_tile "fav_del_btn" "(RemoveSelectedFavorite)")
            
            ;; Handle create-block button: must close the modal dialog first
            ;; so the user can pick objects on screen (done via result 3 below)
            (action_tile "create_block_btn" "(done_dialog 3)")
            
            ;; Handle click on a selected block in the list - removes that selection
            (action_tile "selected_list" "(RemoveLibrarySelection (1+ (atoi $value)))")
            
            ;; Handle OK button
            (action_tile "ok" "(done_dialog 1)")
            
            ;; Handle Cancel button
            (action_tile "cancel" "(done_dialog 0)")
            
            ;; Show dialog
            (setq result (start_dialog))
            
            ;; Process result
            (cond
              ((= result 1)
                ;; OK button clicked
                (if *selected_library_blocks*
                  (progn
                    (ExecuteLibraryArrays)
                    (setq continue_loop nil)
                  )
                  (progn
                    (alert "请至少从库中选择一个图块。")
                  )
                )
              )
              ((= result 2)
                ;; Refresh dialog (continue loop)
                T
              )
              ((= result 3)
                ;; Create-block button: dialog is now closed, safe to select
                ;; objects on screen; reopen dialog afterwards (continue loop)
                (CreateLibraryBlockFromSelection)
              )
              (T
                ;; Cancel or close
                (princ "\n操作已取消。")
                (setq continue_loop nil)
              )
            )
          )
        )
        
        ;; Unload dialog and cleanup
        (unload_dialog dcl_id)
        (vl-file-delete temp_file)
      )
    )
  )
)

(defun c:bsdsubfoldertest (/ i block_info block_name file_path subfolder folder_info folder_name folder_blocks)
  "Test function to check subfolder functionality"
  (princ "\n=== 子文件夹测试 ===")
  (princ (strcat "\n库路径：" (if *library_path* *library_path* "nil")))
  (princ (strcat "\n子文件夹数量：" (itoa (if *library_subfolders* (length *library_subfolders*) 0))))
  
  (if *library_subfolders*
    (progn
      (princ "\n子文件夹：")
      (foreach subfolder_name *library_subfolders*
        (princ (strcat "\n  " subfolder_name))
      )
    )
  )
  
  (princ (strcat "\n当前子文件夹：" (if *current_subfolder* *current_subfolder* "nil")))
  (princ (strcat "\n当前图块数量：" (itoa (if *library_blocks* (length *library_blocks*) 0))))
  
  (princ (strcat "\n已选图块数量：" (itoa (if *selected_library_blocks* (length *selected_library_blocks*) 0))))
  (princ (strcat "\n已选图块信息数量：" (itoa (if *selected_library_block_info* (length *selected_library_block_info*) 0))))
  
  (if *selected_library_block_info*
    (progn
      (princ "\n已选图块信息：")
      (setq i 1)
      (foreach block_info *selected_library_block_info*
        (setq block_name (nth 0 block_info))
        (setq file_path (nth 1 block_info))
        (setq subfolder (nth 2 block_info))
        (princ (strcat "\n  " (itoa i) ". " block_name " 来自 [" subfolder "] -> " file_path))
        (setq i (+ i 1))
      )
    )
  )
  
  (if *subfolder_blocks*
    (progn
      (princ "\n子文件夹图块结构：")
      (foreach folder_info *subfolder_blocks*
        (setq folder_name (car folder_info))
        (setq folder_blocks (cadr folder_info))
        (princ (strcat "\n  " folder_name ": " (itoa (length folder_blocks)) " 个图块"))
        (if (< (length folder_blocks) 3)
          (foreach block_info folder_blocks
            (princ (strcat "\n    - " (car block_info) " (" (cadr block_info) ")"))
          )
        )
      )
    )
  )
  
  (princ "\n=== 子文件夹测试结束 ===")
  (princ)
)

(defun c:bsdremovetest ()
  "Test function to check remove functionality"
  (princ "\n=== 删除测试 ===")
  (princ (strcat "\n已选图块数量：" (itoa (if *selected_library_blocks* (length *selected_library_blocks*) 0))))
  (princ (strcat "\n数量项数量：" (itoa (if *library_quantities* (length *library_quantities*) 0))))
  
  (if (and *selected_library_blocks* *library_quantities*)
    (progn
      (princ "\n当前选择：")
      (setq i 1)
      (foreach block_name *selected_library_blocks*
        (setq qty (nth (- i 1) *library_quantities*))
        (princ (strcat "\n  " (itoa i) ". " block_name " (qty: " (itoa qty) ")"))
        (setq i (+ i 1))
      )
    )
  )
  
  (princ "\n=== 删除测试结束 ===")
  (princ)
)

(defun c:bsdperformancetest (/ test_quantities i quantity start_time end_time elapsed)
  "Test performance of array operations with different quantities"
  
  (princ "\n=== 阵列性能测试 ===")
  (princ "\n此测试将测量阵列创建性能。")
  (princ "\n请选择一个图块进行测试：")
  
  ;; Pick a test block
  (setq test_block (car (entsel "\n选择一个图块进行测试：")))
  
  (if test_block
    (progn
      (setq test_vla (vlax-ename->vla-object test_block))
      (setq test_quantities '(5 10 25 50 100))
      
      (princ "\n\n测试不同数量：")
      (princ "\n数量 | 时间（秒）")
      (princ "\n---------|---------------")
      
      (foreach quantity test_quantities
        ;; Record start time
        (setq start_time (getvar "MILLISECS"))
        
        ;; Execute array operation
        (setq result_blocks (ExecuteArrayAndReturnBlocks test_vla quantity))
        
        ;; Record end time
        (setq end_time (getvar "MILLISECS"))
        (setq elapsed (/ (- end_time start_time) 1000.0))
        
        ;; Display results
        (princ (strcat "\n   " (rtos quantity 2 0) "    |   " (rtos elapsed 2 3)))
        
        ;; Clean up created blocks
        (foreach block result_blocks
          (if (not (vlax-object-released-p block))
            (vla-Delete block)
          )
        )
      )
      
      (princ "\n\n测试完成！")
    )
    (princ "\n未选择图块。测试已取消。")
  )
  
  (princ "\n=== 性能测试结束 ===")
  (princ)
)

(defun c:bsdscaletest ()
  "Test function to check scale variable"
  (princ "\n=== 比例测试 ===")
  (princ (strcat "\n*insertion_scale*: " (if *insertion_scale* *insertion_scale* "nil")))
  (princ (strcat "\n比例因子：" (rtos (atof (if *insertion_scale* *insertion_scale* "1.0")) 2 2)))
  (princ "\n=== 比例测试结束 ===")
  (princ)
)

(defun c:bsddebug ()
  "Debug function to check library variables"
  (princ "\n=== BSDW 调试信息 ===")
  (princ (strcat "\n*library_path*: " (if *library_path* *library_path* "nil")))
  (princ (strcat "\n*library_blocks* length: " (itoa (if *library_blocks* (length *library_blocks*) 0))))
  
  (if *library_blocks*
    (progn
      (princ "\n*library_blocks* 内容：")
      (setq i 1)
      (foreach block_info *library_blocks*
        (princ (strcat "\n  " (itoa i) ": " (vl-prin1-to-string block_info)))
        (setq i (+ i 1))
      )
    )
  )
  
  (princ (strcat "\n*selected_library_blocks* length: " (itoa (if *selected_library_blocks* (length *selected_library_blocks*) 0))))
  
  (if *selected_library_blocks*
    (progn
      (princ "\n*selected_library_blocks* 内容：")
      (setq i 1)
      (foreach block_name *selected_library_blocks*
        (princ (strcat "\n  " (itoa i) ": " (vl-prin1-to-string block_name)))
        (setq i (+ i 1))
      )
    )
  )
  
  (princ "\n=== 调试结束 ===")
  (princ)
)

(defun c:bsdsetpath (/ user_path test_files)
  "Set the block library path manually and save it permanently"
  
  ;; Use folder selection dialog
  (setq user_path (SelectFolderDialog "选择图块库文件夹"))
  
  (if user_path
    (progn
      ;; Add trailing backslash if missing
      (if (not (= (substr user_path (strlen user_path)) "\\"))
        (setq user_path (strcat user_path "\\"))
      )
      
      ;; Check if path exists
      (if (vl-file-directory-p user_path)
        (progn
          (setq *library_path* user_path)
          (princ (strcat "\n库路径已设置为：" user_path))
          
          ;; Save path permanently to registry
          (SaveLibraryPath user_path)
          
          ;; Test scan and populate *library_blocks*
          (setq test_files (vl-directory-files user_path "*.dwg" 1))
          (princ (strcat "\n找到 " (itoa (if test_files (length test_files) 0)) " 个 DWG 文件"))
          
          ;; Populate the library blocks list
          (setq *library_blocks* nil)
          (if test_files
            (progn
              (foreach file_name test_files
                (setq block_name (vl-filename-base file_name))
                (setq *library_blocks* (append *library_blocks* (list (list block_name file_name))))
                (princ (strcat "\n  " file_name " -> " block_name))
              )
              (princ (strcat "\n库图块已填充：" (itoa (length *library_blocks*)) " 个图块"))
              (princ "\n路径已永久保存。")
            )
            (princ "\n未找到 DWG 文件来填充库")
          )
        )
        (princ "\n错误：所选路径不存在！")
      )
    )
    (princ "\n文件夹选择已取消。")
  )
  (princ)
)

(defun SaveCurrentSubfolder (subfolder_name / reg_key)
  "Save the last-used library category/subfolder to the registry"
  (setq reg_key "HKEY_CURRENT_USER\\Software\\AutoCAD\\BSDW")
  (vl-catch-all-apply 'vl-registry-write (list reg_key "CurrentSubfolder" subfolder_name))
  (princ)
)

(defun LoadCurrentSubfolder ( / reg_key saved)
  "Load the last-used library category/subfolder from the registry"
  (setq reg_key "HKEY_CURRENT_USER\\Software\\AutoCAD\\BSDW")
  (setq saved nil)
  (if (not (vl-catch-all-error-p
             (vl-catch-all-apply 'vl-registry-read (list reg_key "CurrentSubfolder"))))
    (setq saved (vl-registry-read reg_key "CurrentSubfolder"))
  )
  saved
)

(defun SaveLibraryPath (path_string / reg_key)
  "Save library path to Windows registry"
  (setq reg_key "HKEY_CURRENT_USER\\Software\\AutoCAD\\BSDW")
  
  ;; Try to save to registry using vl-registry-write
  (if (vl-catch-all-error-p 
        (vl-catch-all-apply 'vl-registry-write 
          (list reg_key "LibraryPath" path_string)))
    (progn
      ;; If registry fails, try to save to a config file
      (SaveLibraryPathToFile path_string)
    )
    (princ "\n路径已保存到注册表。")
  )
)

(defun SaveLibraryPathToFile (path_string / config_file config_path)
  "Save library path to a configuration file as backup"
  (setq config_path (strcat (getvar "ROAMABLEROOTPREFIX") "BSDW_Config.txt"))
  (setq config_file (open config_path "w"))
  
  (if config_file
    (progn
      (write-line path_string config_file)
      (close config_file)
      (princ (strcat "\n路径已保存到配置文件：" config_path))
    )
    (princ "\n警告：无法永久保存路径。")
  )
)

(defun LoadLibraryPath (/ reg_key saved_path config_path config_file)
  "Load library path from registry or config file"
  (setq reg_key "HKEY_CURRENT_USER\\Software\\AutoCAD\\BSDW")
  (setq saved_path nil)
  
  ;; Try to load from registry first
  (if (not (vl-catch-all-error-p 
             (vl-catch-all-apply 'vl-registry-read (list reg_key "LibraryPath"))))
    (setq saved_path (vl-registry-read reg_key "LibraryPath"))
  )
  
  ;; If registry fails, try to load from config file
  (if (not saved_path)
    (progn
      (setq config_path (strcat (getvar "ROAMABLEROOTPREFIX") "BSDW_Config.txt"))
      (if (findfile config_path)
        (progn
          (setq config_file (open config_path "r"))
          (if config_file
            (progn
              (setq saved_path (read-line config_file))
              (close config_file)
            )
          )
        )
      )
    )
  )
  
  ;; Validate and set the loaded path
  (if (and saved_path (vl-file-directory-p saved_path))
    (progn
      (setq *library_path* saved_path)
      (princ (strcat "\n已加载保存的库路径：" saved_path))
      T
    )
    nil
  )
)

(defun SetInsertionScale (scale_string)
  "Set the current insertion scale and permanently save it as the preference"
  (setq *insertion_scale* scale_string)
  (SaveInsertionScale scale_string)
  (princ)
)

(defun SaveInsertionScale (scale_string / reg_key)
  "Save insertion scale to Windows registry"
  (setq reg_key "HKEY_CURRENT_USER\\Software\\AutoCAD\\BSDW")
  
  ;; Try to save to registry using vl-registry-write
  (if (vl-catch-all-error-p 
        (vl-catch-all-apply 'vl-registry-write 
          (list reg_key "InsertionScale" scale_string)))
    (progn
      ;; If registry fails, try to save to a config file
      (SaveInsertionScaleToFile scale_string)
    )
    (princ "\n插入比例已保存。")
  )
)

(defun SaveInsertionScaleToFile (scale_string / config_file config_path)
  "Save insertion scale to a configuration file as backup"
  (setq config_path (strcat (getvar "ROAMABLEROOTPREFIX") "BSDW_ScaleConfig.txt"))
  (setq config_file (open config_path "w"))
  
  (if config_file
    (progn
      (write-line scale_string config_file)
      (close config_file)
      (princ (strcat "\n插入比例已保存到配置文件：" config_path))
    )
    (princ "\n警告：无法永久保存插入比例。")
  )
)

(defun LoadInsertionScale (/ reg_key saved_scale config_path config_file)
  "Load insertion scale from registry or config file"
  (setq reg_key "HKEY_CURRENT_USER\\Software\\AutoCAD\\BSDW")
  (setq saved_scale nil)
  
  ;; Try to load from registry first
  (if (not (vl-catch-all-error-p 
             (vl-catch-all-apply 'vl-registry-read (list reg_key "InsertionScale"))))
    (setq saved_scale (vl-registry-read reg_key "InsertionScale"))
  )
  
  ;; If registry fails, try to load from config file
  (if (not saved_scale)
    (progn
      (setq config_path (strcat (getvar "ROAMABLEROOTPREFIX") "BSDW_ScaleConfig.txt"))
      (if (findfile config_path)
        (progn
          (setq config_file (open config_path "r"))
          (if config_file
            (progn
              (setq saved_scale (read-line config_file))
              (close config_file)
            )
          )
        )
      )
    )
  )
  
  ;; Validate and set the loaded scale (only accept the known valid values)
  (if (and saved_scale (or (equal saved_scale "1.0") (equal saved_scale "0.1")))
    (progn
      (setq *insertion_scale* saved_scale)
      (princ (strcat "\n已加载保存的插入比例：" saved_scale))
      T
    )
    nil
  )
)

(defun c:bsdtest ()
  "Test function to check library scanning"
  (princ "\n=== BSDW 库测试 ===")
  (princ (strcat "\nDWGPREFIX: " (getvar "DWGPREFIX")))
  (princ (strcat "\nACADPREFIX: " (getvar "ACADPREFIX")))
  
  ;; Test directory existence
  (setq test_dir1 (strcat (getvar "DWGPREFIX") "blocks\\"))
  (setq test_dir2 (getvar "DWGPREFIX"))
  
  (princ (strcat "\n测试 blocks 子文件夹：" test_dir1))
  (princ (strcat "\n  存在：" (if (vl-file-directory-p test_dir1) "是" "否")))
  
  (princ (strcat "\n测试当前目录：" test_dir2))
  (princ (strcat "\n  存在：" (if (vl-file-directory-p test_dir2) "是" "否")))
  
  ;; Test file listing
  (setq files1 (vl-directory-files test_dir1 "*.dwg" 1))
  (setq files2 (vl-directory-files test_dir2 "*.dwg" 1))
  
  (princ (strcat "\nblocks 子文件夹中的 DWG 文件：" (itoa (if files1 (length files1) 0))))
  (if files1
    (foreach f files1 (princ (strcat "\n  " f)))
  )
  
  (princ (strcat "\n当前目录中的 DWG 文件：" (itoa (if files2 (length files2) 0))))
  (if files2
    (foreach f files2 (princ (strcat "\n  " f)))
  )
  
  (princ "\n=== 测试结束 ===")
  (princ)
)

(defun c:bsd (/ old_osmode old_cmdecho)
  
  ;; Error handler
  (defun *error* (msg)
    (if old_osmode
      (setvar "OSMODE" old_osmode)
    )
    (if old_cmdecho
      (setvar "CMDECHO" old_cmdecho)
    )
    (if (not (wcmatch msg "quit / *cancel*"))
      (princ (strcat "\n错误：" msg))
    )
    (princ)
  )
  
  ;; Save system variables
  (setq old_osmode (getvar "OSMODE"))
  (setq old_cmdecho (getvar "CMDECHO"))
  
  ;; Reset selections
  (setq *selected_blocks* nil)
  (setq *block_quantities* nil)
  (setq *temp_quantities* nil)
  
  ;; Directly pick blocks first
  (PickMultipleBlocks)
  
  ;; Only show dialog if blocks were selected
  (if *selected_blocks*
    (progn
      ;; Initialize quantity variables dynamically
      (InitializeQuantityVariables (length *selected_blocks*))
      (ShowArrayDialog)
    )
    (princ "\n未选择图块。命令已取消。")
  )
  
  ;; Restore system variables
  (setvar "OSMODE" old_osmode)
  (setvar "CMDECHO" old_cmdecho)
  
  (princ)
)

(defun ShowArrayDialog (/ dcl_id result continue_loop temp_file temp_quantities i tile_key temp_qty parsed_qty)
  "Display dialog for array settings"
  
  ;; Create temporary DCL file with embedded content
  (setq temp_file (strcat (getvar "TEMPPREFIX") "bsd_multi_dialog.dcl"))
  
  ;; Write DCL content directly
  (CreateDCLFile temp_file)
  
  ;; Load dialog
  (setq dcl_id (load_dialog temp_file))
  
  (if (< dcl_id 0)
    (progn
      (alert "加载对话框文件失败。")
      (princ "\n错误：无法加载 DCL 文件。")
    )
    (progn
      ;; Dialog loop
      (setq continue_loop T)
      
      (while continue_loop
        ;; Initialize dialog
        (if (not (new_dialog "bsd_multi_array" dcl_id))
          (progn
            (alert "初始化对话框失败。")
            (setq continue_loop nil)
          )
          (progn
            ;; Update block list display
            (UpdateBlockList)
            
            ;; Add individual action_tile for each edit_box dynamically
            (if *selected_blocks*
              (progn
                (setq i 1)
                (repeat (length *selected_blocks*)
                  (setq tile_key (strcat "quantity" (itoa i)))
                  (setq var_name (strcat "*q" (itoa i) "*"))
                  (action_tile tile_key (strcat "(setq " var_name " $value)"))
                  (setq i (+ i 1))
                )
              )
            )
            
            ;; Handle OK button - Keep it simple, only close dialog
            (action_tile "ok" "(done_dialog 1)")
            
            ;; Handle Cancel button
            (action_tile "cancel" "(done_dialog 0)")
            
            ;; Show dialog
            (setq result (start_dialog))
            
            ;; Process result
            (cond
              ((= result 1)
                ;; OK button clicked - Build quantities from captured variables
                ;; Build quantities list from dynamic variables
                (setq *temp_quantities* nil)
                (setq i 0)
                (while (< i (length *selected_blocks*))
                  (setq temp_qty (GetQuantityVariable (+ i 1)))
                  (setq *temp_quantities* (append *temp_quantities* (list (atoi temp_qty))))
                  (setq i (+ i 1))
                )
                
                (setq *block_quantities* *temp_quantities*)
                
                (ExecuteMultipleArrays)
                (setq continue_loop nil)
              )
              (T
                ;; Cancel or close
                (princ "\n操作已取消。")
                (setq continue_loop nil)
              )
            )
          )
        )
      )
      
      ;; Unload dialog and cleanup
      (unload_dialog dcl_id)
      (vl-file-delete temp_file)
    )
  )
)

(defun CreateDCLFile (filename / dcl_file i)
  "Create DCL file with embedded content for multiple blocks - dynamic quantity based on selected blocks"
  (setq dcl_file (open filename "w"))
  (if dcl_file
    (progn
      (write-line "bsd_multi_array : dialog {" dcl_file)
      (write-line "  label = \"多图块阵列工具\";" dcl_file)
      (write-line "  : boxed_column {" dcl_file)
      (write-line "    label = \"已选图块\";" dcl_file)
      (write-line "    : text {" dcl_file)
      (write-line "      key = \"block_status\";" dcl_file)
      (write-line "      label = \"未选择图块\";" dcl_file)
      (write-line "      width = 40;" dcl_file)
      (write-line "    }" dcl_file)
      (write-line "  }" dcl_file)
      (write-line "  : boxed_column {" dcl_file)
      (write-line "    label = \"图块数量\";" dcl_file)
      
      ;; Dynamic content based on actual selected blocks count
      (if *selected_blocks*
        (progn
          (setq i 1)
          (repeat (length *selected_blocks*)
            (write-line (strcat "    : row {") dcl_file)
            (write-line (strcat "      key = \"row" (itoa i) "\";") dcl_file)
            (write-line (strcat "      : text {") dcl_file)
            (write-line (strcat "        key = \"block_name" (itoa i) "\";") dcl_file)
            (write-line (strcat "        label = \"图块 " (itoa i) "：\";") dcl_file)
            (write-line (strcat "        width = 20;") dcl_file)
            (write-line (strcat "      }") dcl_file)
            (write-line (strcat "      : edit_box {") dcl_file)
            (write-line (strcat "        key = \"quantity" (itoa i) "\";") dcl_file)
            (write-line (strcat "        value = \"1\";") dcl_file)
            (write-line (strcat "        width = 8;") dcl_file)
            (write-line (strcat "        edit_width = 8;") dcl_file)
            (write-line (strcat "      }") dcl_file)
            (write-line (strcat "    }") dcl_file)
            (setq i (+ i 1))
          )
        )
        ;; If no blocks selected, show a placeholder message
        (progn
          (write-line "    : text {" dcl_file)
          (write-line "      label = \"未选择图块。请先选择图块。\";" dcl_file)
          (write-line "      width = 40;" dcl_file)
          (write-line "    }" dcl_file)
        )
      )
      
      (write-line "  }" dcl_file)
      (write-line "  ok_cancel;" dcl_file)
      (write-line "}" dcl_file)
      (close dcl_file)
      T
    )
    nil
  )
)

(defun PickMultipleBlocks (/ ss i ent vla_obj block_name)
  "Pick multiple blocks from drawing"
  
  (princ "\n选择图块（完成后按回车）：")
  (setq ss (ssget (list (cons 0 "INSERT"))))
  
  (if ss
    (progn
      (setq *selected_blocks* nil)
      (setq *block_quantities* nil)
      
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq vla_obj (vlax-ename->vla-object ent))
        (setq block_name (vla-get-Name vla_obj))
        
        ;; Add to lists
        (setq *selected_blocks* (append *selected_blocks* (list vla_obj)))
        (setq *block_quantities* (append *block_quantities* (list 1)))
        
        (setq i (+ i 1))
      )
    )
  )
)


(defun UpdateBlockList (/ i block_name status_text current_qty)
  "Update the block list display in dialog - dynamic version"
  
  ;; Update status text
  (if *selected_blocks*
    (setq status_text (strcat (itoa (length *selected_blocks*)) " 个图块已选"))
    (setq status_text "未选择图块")
  )
  (set_tile "block_status" status_text)
  
  ;; Update block rows dynamically
  (if *selected_blocks*
    (progn
      (setq i 1)
      (repeat (length *selected_blocks*)
        ;; Show this row
        (setq block_name (vla-get-Name (nth (- i 1) *selected_blocks*)))
        (setq current_qty (itoa (nth (- i 1) *block_quantities*)))
        (set_tile (strcat "block_name" (itoa i)) (strcat "图块 " (itoa i) "：" block_name))
        (set_tile (strcat "quantity" (itoa i)) current_qty)
        
        ;; Sync global variable with current value
        (setq var_name (read (strcat "*q" (itoa i) "*")))
        (set var_name current_qty)
        
        (setq i (+ i 1))
      )
    )
  )
)



(defun sublist (lst start len / result i)
  "Extract sublist from list"
  (setq result nil)
  (setq i 0)
  (foreach item lst
    (if (and (>= i start) (< i (+ start len)))
      (setq result (append result (list item)))
    )
    (setq i (+ i 1))
  )
  result
)

(defun ExecuteMultipleArrays (/ doc i vla_obj quantity base_y current_y block_height spacing all_block_groups)
  "Execute array operation for all selected blocks with vertical arrangement, then horizontal realignment"
  
  (if (and *selected_blocks* *block_quantities*)
    (progn
      ;; Get active document
      (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
      
      ;; Start undo group
      (vla-StartUndoMark doc)
      
      ;; Check if this is BSDG edit mode (blocks to delete exist)
      (if (and (boundp '*blocks_to_delete*) *blocks_to_delete*)
        (progn
          
          ;; Rebuild the original grouped sequence to maintain position order
          ;; This ensures same-name blocks in different positions are treated separately
          (setq kept_blocks nil)
          (setq delete_count 0)
          
          ;; Sort all blocks by X coordinate to get original left-to-right order
          (setq all_blocks_sorted nil)
          (foreach block_obj *blocks_to_delete*
            (if (not (vlax-object-released-p block_obj))
              (progn
                (setq block_name (vla-get-Name block_obj))
                (setq insert_pt (GetBlockInsertionPoint block_obj))
                (setq x_coord (car insert_pt))
                (setq all_blocks_sorted (append all_blocks_sorted (list (list x_coord block_name block_obj))))
              )
            )
          )
          (setq all_blocks_sorted (vl-sort all_blocks_sorted '(lambda (a b) (< (car a) (car b)))))
          
          ;; Rebuild consecutive groups (same as AnalyzeExistingArray logic)
          (setq consecutive_groups nil)
          (setq current_group_blocks nil)
          (setq current_group_name nil)
          (setq current_group_count 0)
          
          (foreach block_info all_blocks_sorted
            (setq x_coord (nth 0 block_info))
            (setq block_name (nth 1 block_info))
            (setq block_obj (nth 2 block_info))
            
            (if (equal block_name current_group_name)
              ;; Same block type, add to current group
              (progn
                (setq current_group_count (+ current_group_count 1))
                (setq current_group_blocks (append current_group_blocks (list block_obj)))
              )
              ;; Different block type, save previous group and start new one
              (progn
                ;; Save previous group if exists
                (if current_group_blocks
                  (setq consecutive_groups (append consecutive_groups 
                    (list (list current_group_name current_group_count current_group_blocks))))
                )
                ;; Start new group
                (setq current_group_name block_name)
                (setq current_group_count 1)
                (setq current_group_blocks (list block_obj))
              )
            )
          )
          
          ;; Don't forget the last group
          (if current_group_blocks
            (setq consecutive_groups (append consecutive_groups 
              (list (list current_group_name current_group_count current_group_blocks))))
          )
          
          ;; Now process each consecutive group according to its corresponding quantity
          (setq group_index 0)
          (foreach group consecutive_groups
            (setq group_name (nth 0 group))
            (setq group_count (nth 1 group))
            (setq group_blocks (nth 2 group))
            (setq new_quantity (nth group_index *block_quantities*))
            
            (if (and new_quantity (> new_quantity 0))
              (progn
                ;; Keep the first (leftmost) block of this group if quantity > 0
                (setq leftmost_block (car group_blocks))
                (setq kept_blocks (append kept_blocks (list leftmost_block)))
                
                ;; Delete the rest of this group
                (setq j 1)
                (while (< j (length group_blocks))
                  (setq block_to_delete (nth j group_blocks))
                  (if (not (vlax-object-released-p block_to_delete))
                    (progn
                      (vla-Delete block_to_delete)
                      (setq delete_count (+ delete_count 1))
                    )
                  )
                  (setq j (+ j 1))
                )
              )
              (progn
                ;; Delete all blocks of this group if quantity = 0
                (setq j 0)
                (while (< j (length group_blocks))
                  (setq block_to_delete (nth j group_blocks))
                  (if (not (vlax-object-released-p block_to_delete))
                    (progn
                      (vla-Delete block_to_delete)
                      (setq delete_count (+ delete_count 1))
                    )
                  )
                  (setq j (+ j 1))
                )
              )
            )
            
            (setq group_index (+ group_index 1))
          )
          
          ;; Update *selected_blocks* to use the kept blocks and filter quantities > 0
          (setq *selected_blocks* kept_blocks)
          
          ;; Filter *block_quantities* to remove zero quantities
          (setq filtered_quantities nil)
          (foreach qty *block_quantities*
            (if (and qty (> qty 0))
              (setq filtered_quantities (append filtered_quantities (list qty)))
            )
          )
          (setq *block_quantities* filtered_quantities)
          
          (setq *blocks_to_delete* nil)
        )
      )
      
      ;; Get base Y position from first block
      (setq base_insert_pt (GetBlockInsertionPoint (nth 0 *selected_blocks*)))
      (setq base_y (cadr base_insert_pt))
      (setq current_y base_y)
      
      ;; Initialize list to store all block groups
      (setq all_block_groups nil)
      
      (setq i 0)
      (repeat (length *selected_blocks*)
        (setq vla_obj (nth i *selected_blocks*))
        (setq quantity (nth i *block_quantities*))
        
        ;; Move block to vertical position (except first block)
        (if (> i 0)
          (progn
            ;; Get height of previous block for spacing calculation
            (setq block_height (GetBlockHeight (nth (- i 1) *selected_blocks*)))
            ;; Use minimum spacing of 2.0 units or 1.5 times block height
            (setq spacing (max 2.0 (* block_height 1.5)))
            (setq current_y (- current_y spacing))
            (MoveBlockToPosition vla_obj current_y)
          )
        )
        
        ;; Execute horizontal array for this block and collect all blocks in this group
        (setq block_group (list vla_obj)) ;; Start with original block
        (if (> quantity 1)
          (progn
            (setq array_blocks (ExecuteArrayAndReturnBlocks vla_obj quantity))
            (setq block_group (append block_group array_blocks))
          )
        )
        
        ;; Store this block group
        (setq all_block_groups (append all_block_groups (list block_group)))
        
        (setq i (+ i 1))
      )
      
      ;; Now realign all block groups horizontally
      (RealignBlockGroupsHorizontally all_block_groups base_y)
      
      ;; End undo group
      (vla-EndUndoMark doc)
    )
  )
)

(defun ExecuteArrayAndReturnBlocks (vla_obj n / width rotation insert_pt dX dY i new_pt new_ent created_blocks)
  "Execute the array operation for a single block and return all created blocks - OPTIMIZED VERSION"
  
  (setq created_blocks nil)
  
  (if (and vla_obj (numberp n) (> n 1))
    (progn
      ;; Get block properties using VLA methods (faster than entity data)
      (setq insert_pt (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint vla_obj))))
      (setq rotation (vla-get-Rotation vla_obj))
      
      ;; Handle nil rotation
      (if (not rotation)
        (setq rotation 0.0)
      )
      
      ;; Calculate block width using bounding box
      (setq width (GetBlockWidth vla_obj))
      
      (if (and (numberp width) (> width 0))
        (progn
          ;; Calculate offset increments
          (setq dX (* width (cos rotation)))
          (setq dY (* width (sin rotation)))
          
          ;; Choose optimization strategy based on quantity
          (if (> n 20)
            ;; For large quantities, use batch VLA operations
            (setq created_blocks (CreateLargeArrayVLA vla_obj n dX dY insert_pt))
            ;; For smaller quantities, use optimized VLA copy
            (setq created_blocks (CreateSmallArrayVLA vla_obj n dX dY insert_pt))
          )
        )
      )
    )
  )
  
  created_blocks
)

(defun CreateLargeArrayVLA (base_obj n dX dY insert_pt / 
                           created_blocks i new_ent new_pt batch_size batch_count remaining progress_interval)
  "Create large array using batched VLA operations with progress display and memory management"
  
  (setq created_blocks nil)
  (setq batch_size 20) ;; Optimal batch size for memory management
  (setq batch_count (/ (- n 1) batch_size))
  (setq remaining (rem (- n 1) batch_size))
  (setq progress_interval (max 1 (/ batch_count 10))) ;; Update progress every 10%
  
  (princ (strcat "\n正在分批创建 " (itoa (- n 1)) " 个副本..."))
  
  ;; Process full batches
  (setq i 1)
  (setq batch_num 0)
  (repeat batch_count
    (setq batch_num (+ batch_num 1))
    
    ;; Show progress for large operations
    (if (= (rem batch_num progress_interval) 0)
      (princ (strcat "\r进度：" (itoa (/ (* batch_num 100) batch_count)) "%"))
    )
    
    ;; Process current batch
    (repeat batch_size
      ;; Calculate new insertion point
      (setq new_pt (list
        (+ (nth 0 insert_pt) (* dX i))
        (+ (nth 1 insert_pt) (* dY i))
        (nth 2 insert_pt)
      ))
      
      ;; Copy block using VLA method
      (setq new_ent (vla-Copy base_obj))
      
      ;; Set new insertion point using VLA method (faster than entmod)
      (vla-put-InsertionPoint new_ent (vlax-3d-point new_pt))
      
      ;; Add to created blocks list (use cons for better performance)
      (setq created_blocks (cons new_ent created_blocks))
      
      (setq i (+ i 1))
    )
    
    ;; Memory management: Force garbage collection for very large operations
    (if (and (> n 200) (= (rem batch_num 10) 0))
      (gc)
    )
  )
  
  ;; Process remaining blocks
  (repeat remaining
    ;; Calculate new insertion point
    (setq new_pt (list
      (+ (nth 0 insert_pt) (* dX i))
      (+ (nth 1 insert_pt) (* dY i))
      (nth 2 insert_pt)
    ))
    
    ;; Copy block using VLA method
    (setq new_ent (vla-Copy base_obj))
    
    ;; Set new insertion point using VLA method
    (vla-put-InsertionPoint new_ent (vlax-3d-point new_pt))
    
    ;; Add to created blocks list
    (setq created_blocks (cons new_ent created_blocks))
    
    (setq i (+ i 1))
  )
  
  (princ "\r完成！                    ")
  
  ;; Reverse to maintain order (cons builds list in reverse)
  (reverse created_blocks)
)

(defun CreateSmallArrayVLA (base_obj n dX dY insert_pt / 
                           created_blocks i new_ent new_pt)
  "Create small array using optimized VLA operations"
  
  (setq created_blocks nil)
  
  ;; Execute array (n-1 copies)
  (setq i 1)
  (repeat (- n 1)
    ;; Calculate new insertion point
    (setq new_pt (list
      (+ (nth 0 insert_pt) (* dX i))
      (+ (nth 1 insert_pt) (* dY i))
      (nth 2 insert_pt)
    ))
    
    ;; Copy block using VLA method (faster than entity operations)
    (setq new_ent (vla-Copy base_obj))
    
    ;; Set new insertion point using VLA method (faster than entmod)
    (vla-put-InsertionPoint new_ent (vlax-3d-point new_pt))
    
    ;; Add to created blocks list (use cons for better performance)
    (setq created_blocks (cons new_ent created_blocks))
    
    (setq i (+ i 1))
  )
  
  ;; Reverse to maintain left-to-right order
  (reverse created_blocks)
)

(defun ExecuteArray (vla_obj n / width rotation insert_pt dX dY i new_pt new_ent ent_data new_ent_name new_ent_data)
  "Execute the array operation for a single block"
  
  (if (and vla_obj (numberp n) (> n 1))
    (progn
      ;; Get block properties using entity data
      (setq ent_data (entget (vlax-vla-object->ename vla_obj)))
      (setq insert_pt (cdr (assoc 10 ent_data)))
      (setq rotation (cdr (assoc 50 ent_data)))
      
      ;; Handle nil rotation
      (if (not rotation)
        (setq rotation 0.0)
      )
      
      ;; Ensure rotation is a number
      (if (not (numberp rotation))
        (setq rotation 0.0)
      )
      
      ;; Ensure insertion point is valid
      (if (not (and (listp insert_pt) (>= (length insert_pt) 2)))
        (setq insert_pt '(0.0 0.0 0.0))
        ;; Add Z coordinate if missing
        (if (= (length insert_pt) 2)
          (setq insert_pt (append insert_pt '(0.0)))
        )
      )
      
      ;; Calculate block width using bounding box
      (setq width (GetBlockWidth vla_obj))
      
      (if (and (numberp width) (> width 0))
        (progn
          ;; Calculate offset increments
          (setq dX (* width (cos rotation)))
          (setq dY (* width (sin rotation)))
          
          ;; Execute array (n-1 copies)
          (setq i 1)
          (repeat (- n 1)
            ;; Calculate new insertion point
            (setq new_pt (list
              (+ (nth 0 insert_pt) (* dX i))
              (+ (nth 1 insert_pt) (* dY i))
              (nth 2 insert_pt)
            ))
            
            ;; Copy block
            (setq new_ent (vla-Copy vla_obj))
            
            ;; Set new insertion point using entity modification
            (setq new_ent_name (vlax-vla-object->ename new_ent))
            (setq new_ent_data (entget new_ent_name))
            (setq new_ent_data (subst (cons 10 new_pt) (assoc 10 new_ent_data) new_ent_data))
            (entmod new_ent_data)
            
            (setq i (+ i 1))
          )
        )
      )
    )
  )
)

(defun RealignBlockGroupsHorizontally (all_block_groups target_y / i current_x group_width block_group j block_obj block_insert_pt new_x)
  "Realign all block groups horizontally on the same line"
  
  (setq current_x nil) ;; Will be set from first block
  
  (setq i 0)
  (repeat (length all_block_groups)
    (setq block_group (nth i all_block_groups))
    
    ;; For first group, just move to target Y and get starting X
    (if (= i 0)
      (progn
        ;; Move first group to target Y, keep X unchanged
        (setq j 0)
        (repeat (length block_group)
          (setq block_obj (nth j block_group))
          (setq block_insert_pt (GetBlockInsertionPoint block_obj))
          (if (= j 0)
            (setq current_x (car block_insert_pt)) ;; Set starting X from first block
          )
          (MoveBlockToPosition block_obj target_y)
          (setq j (+ j 1))
        )
        ;; Calculate width of first group
        (setq group_width (CalculateGroupWidth block_group))
        (setq current_x (+ current_x group_width))
      )
      (progn
        ;; Move subsequent groups to current_x position and target Y
        (setq j 0)
        (repeat (length block_group)
          (setq block_obj (nth j block_group))
          (setq block_insert_pt (GetBlockInsertionPoint block_obj))
          
          ;; Calculate new X position for this block within the group
          (setq new_x (+ current_x (* j (GetBlockWidth block_obj))))
          
          ;; Move block to new position
          (MoveBlockToXYPosition block_obj new_x target_y)
          
          (setq j (+ j 1))
        )
        
        ;; Update current_x for next group
        (setq group_width (CalculateGroupWidth block_group))
        (setq current_x (+ current_x group_width))
      )
    )
    
    (setq i (+ i 1))
  )
)

(defun CalculateGroupWidth (block_group / total_width i block_obj block_width)
  "Calculate total width of a block group"
  (setq total_width 0.0)
  (setq i 0)
  (repeat (length block_group)
    (setq block_obj (nth i block_group))
    (setq block_width (GetBlockWidth block_obj))
    (setq total_width (+ total_width block_width))
    (setq i (+ i 1))
  )
  total_width
)

(defun MoveBlockToXYPosition (vla_obj new_x new_y / ent_data insert_pt new_pt new_ent_name new_ent_data)
  "Move block to new X,Y position while keeping Z coordinate"
  
  ;; Get current insertion point using entity data
  (setq ent_data (entget (vlax-vla-object->ename vla_obj)))
  (setq insert_pt (cdr (assoc 10 ent_data)))
  
  ;; Ensure insertion point is valid
  (if (not (and (listp insert_pt) (>= (length insert_pt) 2)))
    (setq insert_pt '(0.0 0.0 0.0))
    ;; Add Z coordinate if missing
    (if (= (length insert_pt) 2)
      (setq insert_pt (append insert_pt '(0.0)))
    )
  )
  
  ;; Create new position
  (setq new_pt (list new_x new_y (nth 2 insert_pt)))
  
  ;; Update insertion point using entity modification
  (setq new_ent_name (vlax-vla-object->ename vla_obj))
  (setq new_ent_data (entget new_ent_name))
  (setq new_ent_data (subst (cons 10 new_pt) (assoc 10 new_ent_data) new_ent_data))
  (entmod new_ent_data)
)

(defun GetBlockInsertionPoint (vla_obj / ent_data insert_pt)
  "Get block insertion point"
  (setq ent_data (entget (vlax-vla-object->ename vla_obj)))
  (setq insert_pt (cdr (assoc 10 ent_data)))
  
  ;; Ensure insertion point is valid
  (if (not (and (listp insert_pt) (>= (length insert_pt) 2)))
    '(0.0 0.0 0.0)
    (progn
      ;; Add Z coordinate if missing
      (if (= (length insert_pt) 2)
        (append insert_pt '(0.0))
        insert_pt
      )
    )
  )
)

(defun GetBlockHeight (vla_obj / min_pt max_pt height)
  "Calculate block height from bounding box"
  (if (vl-catch-all-error-p 
        (vl-catch-all-apply 'vla-GetBoundingBox (list vla_obj 'min_pt 'max_pt)))
    1.0  ;; Default height if bounding box fails
    (progn
      (setq height (- (cadr (vlax-safearray->list max_pt))
                      (cadr (vlax-safearray->list min_pt))))
      (if (numberp height) height 1.0)
    )
  )
)

(defun MoveBlockToPosition (vla_obj new_y / ent_data insert_pt new_pt new_ent_name new_ent_data)
  "Move block to new Y position while keeping X and Z coordinates"
  
  ;; Get current insertion point using entity data
  (setq ent_data (entget (vlax-vla-object->ename vla_obj)))
  (setq insert_pt (cdr (assoc 10 ent_data)))
  
  ;; Ensure insertion point is valid
  (if (not (and (listp insert_pt) (>= (length insert_pt) 2)))
    (setq insert_pt '(0.0 0.0 0.0))
    ;; Add Z coordinate if missing
    (if (= (length insert_pt) 2)
      (setq insert_pt (append insert_pt '(0.0)))
    )
  )
  
  ;; Create new position (keep X and Z, change Y)
  (setq new_pt (list
    (nth 0 insert_pt)  ;; Keep X
    new_y              ;; New Y
    (nth 2 insert_pt)  ;; Keep Z
  ))
  
  ;; Update insertion point using entity modification
  (setq new_ent_name (vlax-vla-object->ename vla_obj))
  (setq new_ent_data (entget new_ent_name))
  (setq new_ent_data (subst (cons 10 new_pt) (assoc 10 new_ent_data) new_ent_data))
  (entmod new_ent_data)
)

(defun GetBlockWidth (vla_obj / min_pt max_pt width)
  "Calculate block width from bounding box"
  (if (vl-catch-all-error-p 
        (vl-catch-all-apply 'vla-GetBoundingBox (list vla_obj 'min_pt 'max_pt)))
    0.0
    (progn
      (setq width (- (car (vlax-safearray->list max_pt))
                     (car (vlax-safearray->list min_pt))))
      (if (numberp width) width 0.0)
    )
  )
)

(defun c:bsdg (/ old_osmode old_cmdecho)
  "Edit existing array - simplified version"
  
  ;; Error handler
  (defun *error* (msg)
    (if old_osmode
      (setvar "OSMODE" old_osmode)
    )
    (if old_cmdecho
      (setvar "CMDECHO" old_cmdecho)
    )
    (if (not (wcmatch msg "quit / *cancel*"))
      (princ (strcat "\n错误：" msg))
    )
    (princ)
  )
  
  ;; Save system variables
  (setq old_osmode (getvar "OSMODE"))
  (setq old_cmdecho (getvar "CMDECHO"))
  
  ;; Reset selections
  (setq *selected_blocks* nil)
  (setq *block_quantities* nil)
  (setq *temp_quantities* nil)
  (setq *blocks_to_delete* nil)
  
  ;; Analyze existing array and collect blocks for deletion
  (if (AnalyzeExistingArray)
    (progn
      ;; Initialize quantity variables dynamically
      (InitializeQuantityVariables (length *selected_blocks*))
      (ShowArrayDialog)
    )
    (princ "\n未选择有效图块或分析失败。")
  )
  
  ;; Restore system variables
  (setvar "OSMODE" old_osmode)
  (setvar "CMDECHO" old_cmdecho)
  
  (princ)
)

(defun AnalyzeExistingArray (/ ss i ent vla_obj block_data sorted_blocks grouped_blocks current_group current_name current_count j block_info block_name insert_pt)
  "Analyze selected blocks and group consecutive identical blocks"
  
  (princ "\n从现有阵列中选择图块：")
  (setq ss (ssget (list (cons 0 "INSERT"))))
  
  (if ss
    (progn
      (setq block_data nil)
      
      ;; Collect all blocks with their positions
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq vla_obj (vlax-ename->vla-object ent))
        (setq block_name (vla-get-Name vla_obj))
        (setq insert_pt (GetBlockInsertionPoint vla_obj))
        
        ;; Store block info: (X-coordinate, block-name, vla-object)
        (setq block_data (append block_data (list (list (car insert_pt) block_name vla_obj))))
        (setq i (+ i 1))
      )
      
      ;; Sort blocks by X coordinate (left to right)
      (setq sorted_blocks (vl-sort block_data '(lambda (a b) (< (car a) (car b)))))
      
      ;; Group consecutive identical blocks
      (setq grouped_blocks nil)
      (setq current_group nil)
      (setq current_name nil)
      (setq current_count 0)
      
      (setq j 0)
      (foreach block_info sorted_blocks
        (setq block_name (nth 1 block_info))
        (setq vla_obj (nth 2 block_info))
        
        (if (equal block_name current_name)
          ;; Same block type, add to current group
          (progn
            (setq current_count (+ current_count 1))
            (setq current_group (append current_group (list vla_obj)))
          )
          ;; Different block type, start new group
          (progn
            ;; Save previous group if exists
            (if current_group
              (progn
                (setq grouped_blocks (append grouped_blocks (list (list current_name current_count (car current_group)))))
              )
            )
            ;; Start new group
            (setq current_name block_name)
            (setq current_count 1)
            (setq current_group (list vla_obj))
          )
        )
        (setq j (+ j 1))
      )
      
      ;; Don't forget the last group
      (if current_group
        (progn
          (setq grouped_blocks (append grouped_blocks (list (list current_name current_count (car current_group)))))
        )
      )
      
      ;; Build selection lists for dialog
      (setq *selected_blocks* nil)
      (setq *block_quantities* nil)
      (setq *blocks_to_delete* nil)
      
      (foreach group grouped_blocks
        (setq block_name (nth 0 group))
        (setq quantity (nth 1 group))
        (setq representative_block (nth 2 group))
        
        (setq *selected_blocks* (append *selected_blocks* (list representative_block)))
        (setq *block_quantities* (append *block_quantities* (list quantity)))
      )
      
      ;; Store all selected blocks for deletion
      (foreach block_info sorted_blocks
        (setq *blocks_to_delete* (append *blocks_to_delete* (list (nth 2 block_info))))
      )
      
      T
    )
    (progn
      (princ "\n未选择图块。")
      nil
    )
  )
)

(defun c:bsdheighttest ()
  "Test function to verify the increased height of library list"
  (princ "\n=== 界面高度测试 ===")
  
  ;; Create test DCL to show the height difference
  (setq test_dcl_file (strcat (getvar "TEMPPREFIX") "test_height.dcl"))
  
  ;; Set up some test data
  (setq *library_subfolders* '("Main" "Test1" "Test2"))
  (setq *current_subfolder* "Main")
  (setq *selected_library_blocks* nil)
  
  (if (CreateLibraryDCLFile test_dcl_file)
    (progn
      (princ (strcat "\n[√] 测试 DCL 文件已创建：" test_dcl_file))
      (princ "\n高度设置：")
      (princ "\n  库列表高度：30（从 12 增大）")
      (princ "\n  已选列高度：15（未变）")
      (princ "\n库列表区域现在可以一次显示更多图块！")
    )
    (princ "\n[×] DCL 文件创建失败")
  )
  
  (princ "\n=== 高度测试结束 ===")
  (princ)
)

(defun c:bsdtabtest ()
  "Test function to check multi-row tab layout with many subfolders"
  (princ "\n=== 标签页布局测试 ===")
  
  ;; Create test data with many subfolders to test multi-row layout
  (setq *library_subfolders* '("Main" "Electrical" "Mechanical" "Plumbing" "HVAC" "Structural" "Furniture" "Landscape" "Symbols" "Details" "Templates" "Custom"))
  (setq *current_subfolder* "Main")
  
  (princ (strcat "\n测试子文件夹数量：" (itoa (length *library_subfolders*))))
  (princ "\n子文件夹：")
  (foreach subfolder_name *library_subfolders*
    (princ (strcat "\n  " subfolder_name))
  )
  
  ;; Test DCL creation
  (setq test_dcl_file (strcat (getvar "TEMPPREFIX") "test_tabs.dcl"))
  (if (CreateLibraryDCLFile test_dcl_file)
    (progn
      (princ (strcat "\n[√] DCL 文件创建成功：" test_dcl_file))
      (princ "\n您可以查看 DCL 文件以验证多行标签页布局。")
      
      ;; Show expected layout
      (princ "\n预期布局：")
      (princ "\n第 1 行：Main, Electrical, Mechanical, Plumbing, HVAC")
      (princ "\n第 2 行：Structural, Furniture, Landscape, Symbols, Details")
      (princ "\n第 3 行：Templates, Custom")
    )
    (princ "\n[×] DCL 文件创建失败")
  )
  
  (princ "\n=== 标签页测试结束 ===")
  (princ)
)

;; Load commands
(princ "\n多图块阵列工具已加载 - 优化版本。")
(princ "\n命令：")
(princ "\n  BSD  - 创建新阵列（交互选择）")
(princ "\n  BSDW - 创建新阵列（库选择）")
(princ "\n  BSDG - 编辑现有阵列")
(princ "\n  BSDSETPATH - 设置并永久保存库路径")
(princ "\n  BSDTEST - 测试库路径和文件检测")
(princ "\n  BSDDEBUG - 调试库变量")
(princ "\n  BSDQUANTITYTEST - 测试数量设置")
(princ "\n  BSDSCALETEST - 测试比例变量")
(princ "\n  BSDREMOVETEST - 测试删除功能")
(princ "\n  BSDSUBFOLDERTEST - 测试子文件夹功能")
(princ "\n  BSDPERFORMANCETEST - 测试阵列性能")
(princ "\n  BSDTABTEST - 测试多行标签页布局")
(princ "\n  BSDHEIGHTTEST - 测试增加的界面高度")
(princ "\n")
(princ "\n性能优化：")
(princ "\n  - 数量 1-10：标准 VLA 复制方法")
(princ "\n  - 数量 11-20：优化 VLA 操作")
(princ "\n  - 数量 21+：分批处理并显示进度")
(princ "\n")
(princ "\n界面改进：")
(princ "\n  - 标签页布局：每行最多 5 个标签页，超过自动多行")
(princ "\n  - 库列表高度：增至 30（原为 12）以提升可见性")
(princ)