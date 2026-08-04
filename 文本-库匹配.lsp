;; WWE.LSP - Text Pick and Display Tool
;; Function: Interactive selection and viewing of text objects (TEXT/MTEXT)

;; Global variable to store text list
(setq *wwe-text-list* nil)
(setq *wwe-text-height* 2.5)
(setq *wwe-entity-groups* nil)
(setq *wwe-checkbox-states* nil)
(setq *wwe-numbers-added* nil)
(setq *wwe-transformer-data* nil)
(setq *wwe-selected-transformer* nil)
(setq *wwe-lsp-path* nil)
(if (not (boundp '*wwe-last-text-data*))
  (setq *wwe-last-text-data* nil))
(if (not (boundp '*wwe-last-text-height*))
  (setq *wwe-last-text-height* 2.5))
(if (not (boundp '*wwe-last-entity-groups*))
  (setq *wwe-last-entity-groups* nil))

;; Store the LSP file path when loaded
(if (and (getvar "LASTPROMPT") (not *wwe-lsp-path*))
  (progn
    ;; Try to get the path from the loaded files list
    (setq *wwe-lsp-path* (findfile "WWE.lsp"))
    (if (not *wwe-lsp-path*)
      ;; If not found, user needs to set it manually or we use a workaround
      (setq *wwe-lsp-path* nil)
    )
  )
)

;; Read CSV file and parse transformer data
(defun wwe:read-csv (/ csv-path f line data-list fields lsp-dir csv-file-obj reg-path)
  (setq csv-path nil)
  
  ;; Try to read saved path from Windows registry
  (vl-load-com)
  (setq reg-path (vl-registry-read "HKEY_CURRENT_USER\\Software\\WWE_LISP" "CSVPath"))
  (if (and reg-path (findfile reg-path))
    (setq csv-path reg-path)
  )
  
  ;; Method 1: Use stored LSP path
  (if (and (not csv-path) *wwe-lsp-path*)
    (progn
      ;; Try both Chinese and English filenames
      (if (findfile (strcat (vl-filename-directory *wwe-lsp-path*) "\\transformer.csv"))
        (setq csv-path (strcat (vl-filename-directory *wwe-lsp-path*) "\\transformer.csv"))
        (if (findfile (strcat (vl-filename-directory *wwe-lsp-path*) "\\workbook1.csv"))
          (setq csv-path (strcat (vl-filename-directory *wwe-lsp-path*) "\\workbook1.csv"))
        )
      )
    )
  )
  
  ;; Method 2: Try to find WWE.lsp in search paths
  (if (not csv-path)
    (progn
      (setq *wwe-lsp-path* (findfile "WWE.lsp"))
      (if *wwe-lsp-path*
        (progn
          ;; Try both filenames
          (if (findfile (strcat (vl-filename-directory *wwe-lsp-path*) "\\transformer.csv"))
            (setq csv-path (strcat (vl-filename-directory *wwe-lsp-path*) "\\transformer.csv"))
            (if (findfile (strcat (vl-filename-directory *wwe-lsp-path*) "\\workbook1.csv"))
              (setq csv-path (strcat (vl-filename-directory *wwe-lsp-path*) "\\workbook1.csv"))
            )
          )
        )
      )
    )
  )
  
  ;; Method 3: Prompt user to select the CSV file
  (if (not csv-path)
    (progn
      (princ "\nCannot locate CSV file automatically.")
      (princ "\nPlease select the transformer CSV file (transformer.csv or workbook1.csv)...")
      (setq csv-file-obj (getfiled "Select Transformer CSV File" "" "csv" 8))
      (if csv-file-obj
        (progn
          (setq csv-path csv-file-obj)
          ;; Store the directory for future use
          (setq *wwe-lsp-path* (strcat (vl-filename-directory csv-path) "\\WWE.lsp"))
          ;; Save to registry for permanent storage
          (vl-registry-write "HKEY_CURRENT_USER\\Software\\WWE_LISP" "CSVPath" csv-path)
          (princ (strcat "\nCSV path saved: " csv-path))
        )
      )
    )
    ;; Save found path to registry
    (if csv-path
      (vl-registry-write "HKEY_CURRENT_USER\\Software\\WWE_LISP" "CSVPath" csv-path)
    )
  )
  
  (if (not csv-path)
    (progn
      (princ "\nNo CSV file selected")
      nil
    )
    (progn
      (princ (strcat "\nUsing CSV file: " csv-path))
      
      (setq f (open csv-path "r"))
      (if (not f)
        (progn
          (princ "\nFailed to open CSV file")
          ;; Clear invalid registry entry
          (vl-registry-delete "HKEY_CURRENT_USER\\Software\\WWE_LISP" "CSVPath")
          nil
        )
        (progn
          (setq data-list '())
          
          ;; Skip header line
          (read-line f)
          
          ;; Read all data lines
          (while (setq line (read-line f))
            (if (/= line "")
              (progn
                ;; Split by comma
                (setq fields (wwe:split-string line ","))
                (if (>= (length fields) 6)
                  (setq data-list (append data-list (list fields)))
                )
              )
            )
          )
          
          (close f)
          (princ (strcat "\nLoaded " (itoa (length data-list)) " transformer records"))
          data-list
        )
      )
    )
  )
)

;; Split string by delimiter
(defun wwe:split-string (str delim / pos result)
  (setq result '())
  (while (setq pos (vl-string-search delim str))
    (setq result (append result (list (substr str 1 pos))))
    (setq str (substr str (+ pos 2)))
  )
  (setq result (append result (list str)))
  result
)

;; Write transformer selection DCL
(defun wwe:write-transformer-dcl (/ dcl-file f i)
  (setq dcl-file (strcat (getenv "TEMP") "\\wwe_transformer.dcl"))
  (setq f (open dcl-file "w"))
  
  (write-line "wwe_transformer : dialog {" f)
  (write-line "  label = \"ѡ   ѹ  \";" f)
  (write-line "  : boxed_column {" f)
  (write-line "    label = \"  ѹ   б \";" f)
  (write-line "    : list_box {" f)
  (write-line "      key = \"transformer_list\";" f)
  (write-line "      width = 40;" f)
  (write-line "      height = 15;" f)
  (write-line "      fixed_width = true;" f)
  (write-line "      fixed_height = true;" f)
  (write-line "    }" f)
  (write-line "  }" f)
  (write-line "  : row {" f)
  (write-line "    fixed_width = true;" f)
  (write-line "    alignment = centered;" f)
  (write-line "    : button {" f)
  (write-line "      key = \"accept\";" f)
  (write-line "      label = \"ȷ  \";" f)
  (write-line "      width = 12;" f)
  (write-line "      fixed_width = true;" f)
  (write-line "      is_default = true;" f)
  (write-line "    }" f)
  (write-line "    : button {" f)
  (write-line "      key = \"cancel\";" f)
  (write-line "      label = \"ȡ  \";" f)
  (write-line "      width = 12;" f)
  (write-line "      fixed_width = true;" f)
  (write-line "      is_cancel = true;" f)
  (write-line "    }" f)
  (write-line "  }" f)
  (write-line "}" f)
  
  (close f)
  dcl-file
)

;; Show transformer selection dialog
(defun wwe:show-transformer-dialog (/ dcl-file dcl-id dialog-result capacity-list i selected-index)
  ;; Load transformer data if not already loaded
  (if (not *wwe-transformer-data*)
    (setq *wwe-transformer-data* (wwe:read-csv))
  )
  
  (if (not *wwe-transformer-data*)
    (progn
      (alert "Failed to load transformer data from CSV file!")
      nil
    )
    (progn
      ;; Build capacity list
      (setq capacity-list '())
      (foreach item *wwe-transformer-data*
        (setq capacity-list (append capacity-list (list (car item))))
      )
      
      ;; Write DCL file
      (setq dcl-file (wwe:write-transformer-dcl))
      (setq dcl-id (load_dialog dcl-file))
      
      (if (not (new_dialog "wwe_transformer" dcl-id))
        (progn
          (alert "Error: Cannot create transformer dialog!")
          (unload_dialog dcl-id)
          nil
        )
        (progn
          ;; Set list box items
          (start_list "transformer_list")
          (mapcar 'add_list capacity-list)
          (end_list)
          
          ;; Set default selection
          (set_tile "transformer_list" "0")
          
          ;; OK button action
          (action_tile "accept"
            "(progn (setq selected-index (atoi (get_tile \"transformer_list\"))) (done_dialog 1))"
          )
          
          ;; Cancel button action
          (action_tile "cancel"
            "(done_dialog 0)"
          )
          
          ;; Show dialog
          (setq dialog-result (start_dialog))
          (unload_dialog dcl-id)
          
          ;; Return selected transformer data
          (if (= dialog-result 1)
            (nth selected-index *wwe-transformer-data*)
            nil
          )
        )
      )
    )
  )
)

;; Write DCL content to temporary file with dynamic rows
(defun wwe:write-dcl (row-count / dcl-file f i)
  (setq dcl-file (strcat (getenv "TEMP") "\\wwe_dialog.dcl"))
  (setq f (open dcl-file "w"))
  (write-line "wwe_dialog : dialog {" f)
  (write-line "  label = \" ı ѡ 񹤾  -  б ༭  \";" f)
  (write-line "  : row {" f)
  (write-line "    : boxed_column {" f)
  (write-line "      label = \"  ѹ   б \";" f)
  (write-line "      : list_box {" f)
  (write-line "        key = \"transformer_list\";" f)
  (write-line "        width = 25;" f)
  (write-line "        height = 5;" f)
  (write-line "        fixed_width = true;" f)
  (write-line "      }" f)
  (write-line "    }" f)
  (write-line "    : column {" f)
  (write-line "      : text {" f)
  (write-line "        label = \" ı    ݣ ÿ пɱ༭\";" f)
  (write-line "      }" f)
  (write-line "      : text {" f)
  (write-line "        key = \"info_label\";" f)
  (write-line "        label = \"δѡ   ѹ  \";" f)
  (write-line "        alignment = left;" f)
  (write-line "      }" f)
  (write-line "      : column {" f)
  (write-line "        width = 70;" f)
  (write-line "        fixed_width = true;" f)
  
  ;; Create scrollable area with edit boxes
  (write-line "      : boxed_column {" f)
  (write-line "        label = \"\";" f)
  (write-line "        height = 5;" f)
  
  ;; Generate edit boxes for each row with checkbox
  (setq i 0)
  (while (< i row-count)
    (write-line "        : row {" f)
    (write-line "          : text {" f)
    (write-line (strcat "            label = \"" (itoa (1+ i)) ".\";") f)
    (write-line "            width = 3;" f)
    (write-line "            fixed_width = true;" f)
    (write-line "            alignment = right;" f)
    (write-line "          }" f)
    (write-line "          : edit_box {" f)
    (write-line (strcat "            key = \"line_" (itoa i) "\";") f)
    (write-line "            edit_width = 55;" f)
    (write-line "            fixed_width = true;" f)
    (write-line "          }" f)
    (write-line "          : toggle {" f)
    (write-line (strcat "            key = \"chk_" (itoa i) "\";") f)
    (write-line "            label = \"\";" f)
    (write-line "            width = 3;" f)
    (write-line "            fixed_width = true;" f)
    (write-line "          }" f)
    (write-line "        }" f)
    (setq i (1+ i))
  )
  
  (write-line "      }" f)
  (write-line "      }" f)
  (write-line "    }" f)
  (write-line "  }" f)
  
  ;; Row 1: text editing & data operations
  (write-line "  : row {" f)
  (write-line "    fixed_width = true;" f)
  (write-line "    alignment = centered;" f)
  (write-line "    : button {" f)
  (write-line "      key = \"btn_extend\";" f)
  (write-line "      label = \"    ·  \";" f)
  (write-line "      width = 8;" f)
  (write-line "      fixed_width = true;" f)
  (write-line "    }" f)
  (write-line "    : button {" f)
  (write-line "      key = \"btn_copy\";" f)
  (write-line "      label = \"    \";" f)
  (write-line "      width = 6;" f)
  (write-line "      fixed_width = true;" f)
  (write-line "    }" f)
  (write-line "    : button {" f)
  (write-line "      key = \"btn_paste\";" f)
  (write-line "      label = \"ճ  \";" f)
  (write-line "      width = 6;" f)
  (write-line "      fixed_width = true;" f)
  (write-line "    }" f)
  (write-line "    : button {" f)
  (write-line "      key = \"btn_swap\";" f)
  (write-line "      label = \"    \";" f)
  (write-line "      width = 6;" f)
  (write-line "      fixed_width = true;" f)
  (write-line "    }" f)
  (write-line "    : button {" f)
  (write-line "      key = \"btn_add_numbers\";" f)
  (write-line "      label = \" ӱ  \";" f)
  (write-line "      width = 6;" f)
  (write-line "      fixed_width = true;" f)
  (write-line "    }" f)
  (write-line "  }" f)

  ;; Row 2: output & dialog control
  (write-line "  : row {" f)
  (write-line "    fixed_width = true;" f)
  (write-line "    alignment = centered;" f)
  (write-line "    : button {" f)
  (write-line "      key = \"btn_write\";" f)
  (write-line "      label = \"д  CAD\";" f)
  (write-line "      width = 8;" f)
  (write-line "      fixed_width = true;" f)
  (write-line "    }" f)
  (write-line "    : button {" f)
  (write-line "      key = \"btn_pick\";" f)
  (write-line "      label = \"ѡ     ı \";" f)
  (write-line "      width = 8;" f)
  (write-line "      fixed_width = true;" f)
  (write-line "    }" f)
  ;; Text height edit box: auto-shows recorded height, remembers last change
  (write-line "    : edit_box {" f)
  (write-line "      key = \"eb_height\";" f)
  (write-line "      label = \" ָ :\";" f)
  (write-line "      width = 12;" f)
  (write-line "      edit_width = 6;" f)
  (write-line "      fixed_width = true;" f)
  (write-line "    }" f)
  (write-line "    : button {" f)
  (write-line "      key = \"btn_ok\";" f)
  (write-line "      label = \"ȷ  \";" f)
  (write-line "      width = 6;" f)
  (write-line "      fixed_width = true;" f)
  (write-line "      is_default = true;" f)
  (write-line "    }" f)
  ;; Visible cancel button: also handles ESC / close-X cancel
  (write-line "    : button {" f)
  (write-line "      key = \"btn_cancel\";" f)
  (write-line "      label = \"ȡ  \";" f)
  (write-line "      width = 6;" f)
  (write-line "      fixed_width = true;" f)
  (write-line "      is_cancel = true;" f)
  (write-line "    }" f)
  (write-line "  }" f)
  (write-line "}" f)
  (close f)
  dcl-file
)

;; Save last text data to temp file (cross-document persistence)
(defun wwe:save-last-data-to-file (/ f data-file)
  (setq data-file (strcat (getenv "TEMP") "\\wwe_last_data.txt"))
  (setq f (open data-file "w"))
  (if f
    (progn
      (write-line (rtos *wwe-last-text-height* 2 4) f)
      (foreach item *wwe-last-text-data*
        (write-line (caddr item) f))
      (close f)
    )
  )
)

;; Load last text data from temp file (cross-document persistence)
(defun wwe:load-last-data-from-file (/ f line text-list height data-list i gid data-file)
  (setq data-file (strcat (getenv "TEMP") "\\wwe_last_data.txt"))
  (setq f (open data-file "r"))
  (if f
    (progn
      (setq height (atof (read-line f)))
      (setq text-list '())
      (while (setq line (read-line f))
        (setq text-list (append text-list (list line)))
      )
      (close f)
      (if (and text-list (> (length text-list) 0))
        (progn
          (setq data-list '())
          (setq *wwe-last-entity-groups* '())
          (setq i 0)
          (while (< i (length text-list))
            (setq data-list (append data-list
              (list (list nil 0 (nth i text-list) i))))
            (setq *wwe-last-entity-groups* (append *wwe-last-entity-groups* (list i)))
            (setq i (1+ i))
          )
          (setq *wwe-last-text-data* data-list)
          (setq *wwe-last-text-height* height)
          T
        )
        nil
      )
    )
    nil
  )
)

;;        
(defun c:WWD ()
  (setq text-data nil)
  (setq result-list nil)
  (setq result-str nil)
  (setq *wwe-numbers-added* nil)  ; Reset flag at start
  (princ "\nText Pick Tool started...")
  
  ;; Select text objects (returns list of (entity y-coord text))
  (setq text-data (wwe:pick-text))
  
  ;; If no new text picked, always load from file for latest cross-document data
  (if (not text-data)
    (progn
      (wwe:load-last-data-from-file)
      (if *wwe-last-text-data*
        (progn
          (setq text-data *wwe-last-text-data*)
          (setq *wwe-text-height* *wwe-last-text-height*)
          (setq *wwe-entity-groups* (append '() *wwe-last-entity-groups*))
          (princ "\nNo new text selected, showing last selected text...")
        )
        (princ "\nNo text object selected, operation cancelled")
      )
    )
  )
  
  ;; Show dialog if we have text data (either new or recalled)
  (if text-data
    (progn
      ;; Show dialog and get final result
      (setq result-list (wwe:show-dialog text-data))
      
      (if result-list
        (progn
          ;; Combine all text with line breaks for output
          (setq result-str "")
          (foreach txt result-list
            (if (/= txt "")
              (if (= result-str "")
                (setq result-str txt)
                (setq result-str (strcat result-str "\n" txt))
              )
            )
          )
          (princ (strcat "\nFinal text content:\n" result-str))
        )
        (princ "\nOperation cancelled")
      )
    )
  )
  
  (princ)
)

;; Interactive text entity selection (supports multiple selection)
(defun wwe:pick-text (/ ss ss-len i ent-obj ent-type text-str y-coord text-height result-list sorted-list old-nomutt group-id)
  ;; Save and set NOMUTT to suppress selection messages
  (setq old-nomutt (getvar "NOMUTT"))
  (setvar "NOMUTT" 1)
  
  (princ "\nSelect text objects (TEXT/MTEXT) - Window/Crossing selection supported: ")
  (setq ss (ssget '((0 . "TEXT,MTEXT"))))
  
  ;; Restore NOMUTT
  (setvar "NOMUTT" old-nomutt)
  
  (if ss
    (progn
      (setq ss-len (sslength ss))
      (setq result-list '())
      (setq *wwe-entity-groups* '())
      (setq i 0)
      (setq *wwe-text-height* 2.5)
      (setq group-id 0)
      
      ;; Loop through all selected entities
      (while (< i ss-len)
        (setq ent-obj (ssname ss i))
        (setq ent-type (cdr (assoc 0 (entget ent-obj))))
        
        ;; Extract text from each entity
        (if (or (= ent-type "TEXT") (= ent-type "MTEXT"))
          (progn
            (setq text-str (wwe:get-text-from-ent ent-obj))
            ;; Get Y coordinate (10 for TEXT, 10 for MTEXT)
            (setq y-coord (cadr (cdr (assoc 10 (entget ent-obj)))))
            ;; Get text height (40 for both TEXT and MTEXT)
            (setq text-height (cdr (assoc 40 (entget ent-obj))))
            
            ;; Store first text height
            (if (= i 0)
              (setq *wwe-text-height* text-height)
            )
            
            ;; Check if text-str is a list (multi-line MTEXT) or string
            (if (listp text-str)
              ;; Multi-line MTEXT - add each line with same group-id
              (progn
                (foreach line text-str
                  (if line
                    (progn
                      (setq result-list (append result-list (list (list ent-obj y-coord line group-id))))
                      (setq *wwe-entity-groups* (append *wwe-entity-groups* (list group-id)))
                    )
                  )
                )
                (setq group-id (1+ group-id))
              )
              ;; Single line text - unique group-id
              (if text-str
                (progn
                  (setq result-list (append result-list (list (list ent-obj y-coord text-str group-id))))
                  (setq *wwe-entity-groups* (append *wwe-entity-groups* (list group-id)))
                  (setq group-id (1+ group-id))
                )
              )
            )
          )
        )
        
        (setq i (1+ i))
      )
      
      ;; Clear selection set to remove highlight
      (command "_.SELECT" "_P" "")
      
      ;; Keep pick order (display in selection order, not sorted by Y)
      (if result-list
        (progn
          (setq *wwe-entity-groups* '())
          (foreach item result-list
            (setq *wwe-entity-groups* (append *wwe-entity-groups* (list (cadddr item))))
          )
          result-list
        )
        nil
      )
    )
    nil
  )
)

;; Extract text string from entity
(defun wwe:get-text-from-ent (ent-obj / ent-data ent-type text-str)
  (setq ent-data (entget ent-obj))
  (setq ent-type (cdr (assoc 0 ent-data)))
  (setq text-str (cdr (assoc 1 ent-data)))
  
  ;; If MTEXT, clean format codes (may return list of lines)
  (if (= ent-type "MTEXT")
    (setq text-str (wwe:strip-mtext-codes text-str))
  )
  
  text-str
)

;; Clean MTEXT format codes and split by paragraphs
(defun wwe:strip-mtext-codes (text-str / result i len ch in-brace ch-code lines current-line)
  (setq current-line "")
  (setq lines '())
  (setq i 0)
  (setq len (strlen text-str))
  (setq in-brace 0)
  
  (while (< i len)
    (setq ch (substr text-str (1+ i) 1))
    
    (cond
      ;; Handle escape sequences
      ((= ch "\\")
       (setq i (1+ i))
       (if (< i len)
         (progn
           (setq ch (substr text-str (1+ i) 1))
           (setq ch-code (ascii ch))
           (cond
             ;; \P paragraph break - create new line
             ((= ch "P")
              (setq lines (append lines (list current-line)))
              (setq current-line "")
             )
             ;; \xxx; format escape sequences (letter start)
             ((or (and (>= ch-code 65) (<= ch-code 90))
                  (and (>= ch-code 97) (<= ch-code 122)))
              ;; Skip until semicolon
              (setq i (1+ i))
              (while (and (< i len) (/= (substr text-str (1+ i) 1) ";"))
                (setq i (1+ i))
              )
             )
             ;; Other \ control codes, skip
             (t nil)
           )
         )
       )
      )
      
      ;; Handle braces
      ((= ch "{")
       (setq in-brace (1+ in-brace))
      )
      
      ((= ch "}")
       (if (> in-brace 0)
         (setq in-brace (1- in-brace))
       )
      )
      
      ;; Normal character
      (t
       (setq current-line (strcat current-line ch))
      )
    )
    
    (setq i (1+ i))
  )
  
  ;; Add last line if not empty
  (if (/= current-line "")
    (setq lines (append lines (list current-line)))
  )
  
  ;; Return list of lines or single string if only one line
  (if (> (length lines) 1)
    lines
    (if (= (length lines) 1)
      (car lines)
      ""
    )
  )
)

;; Write edited text back to original CAD entities (with grouping support)
(defun wwe:update-original-entities (text-data edited-list / processed-groups i group-id ent-name group-lines combined-text ent-data ent-type valid-count)
  (setq valid-count 0)
  (setq processed-groups '())
  (setq i 0)
  
  (while (< i (length edited-list))
    (if (< i (length *wwe-entity-groups*))
      (progn
        (setq group-id (nth i *wwe-entity-groups*))
        
        ;; Check if this group has been processed
        (if (not (member group-id processed-groups))
          (progn
            ;; Collect all lines belonging to this group
            (setq group-lines '())
            (setq j 0)
            (while (< j (length edited-list))
              (if (and (< j (length *wwe-entity-groups*))
                       (= (nth j *wwe-entity-groups*) group-id))
                (setq group-lines (append group-lines (list (nth j edited-list))))
              )
              (setq j (1+ j))
            )
            
            ;; Get entity for this group
            (if (< i (length text-data))
              (progn
                (setq ent-name (car (nth i text-data)))
                
                ;; Check if entity is valid
                (if (and ent-name (setq ent-data (entget ent-name)))
                  (progn
                    (setq ent-type (cdr (assoc 0 ent-data)))
                    
                    ;; Combine lines based on entity type
                    (if (= ent-type "MTEXT")
                      ;; MTEXT: combine with \P
                      (progn
                        (setq combined-text "")
                        (foreach line group-lines
                          (if (/= line "")
                            (if (= combined-text "")
                              (setq combined-text line)
                              (setq combined-text (strcat combined-text "\\P" line))
                            )
                          )
                        )
                      )
                      ;; TEXT: use first line only
                      (setq combined-text (car group-lines))
                    )
                    
                    ;; Update entity
                    (if (/= combined-text "")
                      (progn
                        (setq ent-data (subst (cons 1 combined-text) (assoc 1 ent-data) ent-data))
                        (entmod ent-data)
                        (setq valid-count (1+ valid-count))
                      )
                    )
                  )
                )
              )
            )
            
            ;; Mark group as processed
            (setq processed-groups (append processed-groups (list group-id)))
          )
        )
      )
    )
    
    (setq i (1+ i))
  )
  
  (if (> valid-count 0)
    (princ (strcat "\n" (itoa valid-count) " text object(s) updated in CAD"))
    (princ "\nNo valid entities to update")
  )
  valid-count
)

;; Create new MTEXT with all edited content
(defun wwe:create-new-mtext (edited-list / i line-val combined-text insert-pt)
  ;; Collect all non-empty lines
  (setq combined-text "")
  (setq i 0)
  
  (while (< i (length edited-list))
    (setq line-val (nth i edited-list))
    (if (/= line-val "")
      (if (= combined-text "")
        (setq combined-text line-val)
        (setq combined-text (strcat combined-text "\\P" line-val))
      )
    )
    (setq i (1+ i))
  )
  
  (if (/= combined-text "")
    (progn
      ;; Prompt user to pick insertion point
      (setq insert-pt (getpoint "\nSpecify insertion point for new MTEXT: "))
      
      (if insert-pt
        (progn
          ;; Create new MTEXT entity with same text height
          (entmake
            (list
              '(0 . "MTEXT")
              '(100 . "AcDbEntity")
              '(100 . "AcDbMText")
              (cons 10 insert-pt)
              (cons 1 combined-text)
              '(7 . "Standard")
              (cons 40 *wwe-text-height*)
              '(41 . 0.0)
              '(71 . 1)
              '(72 . 5)
            )
          )
          (princ "\nNew MTEXT created successfully!")
          T
        )
        (progn
          (princ "\nInsertion point not specified, operation cancelled")
          nil
        )
      )
    )
    (progn
      (princ "\nNo content to write!")
      nil
    )
  )
)

;; Copy text to clipboard using COM method
(defun wwe:copy-to-clipboard (text-list / combined-text html-obj)
  ;; Combine all non-empty lines
  (setq combined-text "")
  (foreach line text-list
    (if (/= line "")
      (if (= combined-text "")
        (setq combined-text line)
        (setq combined-text (strcat combined-text "\r\n" line))
      )
    )
  )
  
  (if (/= combined-text "")
    (progn
      (vl-load-com)
      
      ;; Try to set clipboard text using COM object
      (if (not (vl-catch-all-error-p
        (setq html-obj (vl-catch-all-apply 'vlax-create-object '("htmlfile")))))
        (progn
          (if (not (vl-catch-all-error-p
            (vl-catch-all-apply 'vlax-invoke 
              (list (vlax-get-property 
                (vlax-get-property html-obj 'ParentWindow) 
                'ClipboardData) 
                'SetData 
                "Text"
                combined-text))))
            (progn
              (vlax-release-object html-obj)
              (princ "\nText copied to clipboard!")
              T
            )
            (progn
              (vlax-release-object html-obj)
              (princ "\nFailed to copy to clipboard!")
              nil
            )
          )
        )
        (progn
          (princ "\nFailed to access clipboard!")
          nil
        )
      )
    )
    (progn
      (princ "\nNo content to copy!")
      nil
    )
  )
)

;; Paste text from clipboard using COM method
(defun wwe:paste-from-clipboard (/ html-obj clip-text pasted-lines char-list i ch prev-ch current-line)
  (setq pasted-lines '())
  (setq current-line "")
  
  (vl-load-com)
  
  ;; Try to get clipboard text using COM object
  (if (not (vl-catch-all-error-p
    (setq html-obj (vl-catch-all-apply 'vlax-create-object '("htmlfile")))))
    (progn
      (if (not (vl-catch-all-error-p
        (setq clip-text (vl-catch-all-apply 'vlax-invoke 
          (list (vlax-get-property 
            (vlax-get-property html-obj 'ParentWindow) 
            'ClipboardData) 
            'GetData 
            "Text")))))
        (progn
          ;; Process character by character to handle line breaks
          (setq i 1)
          (setq prev-ch "")
          (while (<= i (strlen clip-text))
            (setq ch (substr clip-text i 1))
            (cond
              ;; Handle \r\n (Windows line break)
              ((and (= prev-ch "\r") (= ch "\n"))
               (setq pasted-lines (append pasted-lines (list current-line)))
               (setq current-line "")
              )
              ;; Handle standalone \n (Unix line break)
              ((and (= ch "\n") (/= prev-ch "\r"))
               (setq pasted-lines (append pasted-lines (list current-line)))
               (setq current-line "")
              )
              ;; Handle standalone \r (Mac line break)
              ((and (= ch "\r") (or (= i (strlen clip-text)) (/= (substr clip-text (1+ i) 1) "\n")))
               (setq pasted-lines (append pasted-lines (list current-line)))
               (setq current-line "")
              )
              ;; Skip \r if followed by \n (already handled above)
              ((= ch "\r")
               nil
              )
              ;; Normal character
              (t
               (setq current-line (strcat current-line ch))
              )
            )
            (setq prev-ch ch)
            (setq i (1+ i))
          )
          
          ;; Add last line if not empty
          (if (/= current-line "")
            (setq pasted-lines (append pasted-lines (list current-line)))
          )
          
          (vlax-release-object html-obj)
          
          (if (> (length pasted-lines) 0)
            (progn
              (princ (strcat "\n" (itoa (length pasted-lines)) " line(s) pasted!"))
              pasted-lines
            )
            (progn
              (princ "\nClipboard is empty!")
              nil
            )
          )
        )
        (progn
          (vlax-release-object html-obj)
          (princ "\nFailed to get clipboard data!")
          nil
        )
      )
    )
    (progn
      (princ "\nFailed to access clipboard!")
      nil
    )
  )
)

;; Swap two lines based on checkbox selection
(defun wwe:swap-lines (text-data edited-list checkbox-states / selected-indices i idx1 idx2 temp-text temp-entity temp-ycoord temp-group new-text-list)
  ;; Collect selected line indices
  (setq selected-indices '())
  (setq i 0)
  (while (< i (length checkbox-states))
    (if (= (nth i checkbox-states) "1")
      (setq selected-indices (append selected-indices (list i)))
    )
    (setq i (1+ i))
  )
  
  ;; Check if exactly 2 lines are selected
  (if (/= (length selected-indices) 2)
    (progn
      (alert "Please select exactly 2 lines to swap!")
      nil
    )
    (progn
      (setq idx1 (car selected-indices))
      (setq idx2 (cadr selected-indices))
      
      ;; Create new text list with swapped content AND entity info
      (setq new-text-list '())
      (setq i 0)
      (while (< i (length edited-list))
        (cond
          ;; Swap position 1: gets BOTH content AND entity info from position 2
          ((= i idx1)
           (if (< idx2 (length text-data))
             (setq new-text-list (append new-text-list 
               (list (list (car (nth idx2 text-data))      ; Position 2 entity
                           (cadr (nth idx2 text-data))     ; Position 2 y-coord
                           (nth idx2 edited-list)          ; Position 2 text content
                           (cadddr (nth idx2 text-data)))))) ; Position 2 group-id
             (setq new-text-list (append new-text-list 
               (list (list nil (- 0 idx1) (nth idx2 edited-list) (+ 10000 idx1)))))
           )
          )
          ;; Swap position 2: gets BOTH content AND entity info from position 1
          ((= i idx2)
           (if (< idx1 (length text-data))
             (setq new-text-list (append new-text-list 
               (list (list (car (nth idx1 text-data))      ; Position 1 entity
                           (cadr (nth idx1 text-data))     ; Position 1 y-coord
                           (nth idx1 edited-list)          ; Position 1 text content
                           (cadddr (nth idx1 text-data)))))) ; Position 1 group-id
             (setq new-text-list (append new-text-list 
               (list (list nil (- 0 idx2) (nth idx1 edited-list) (+ 10000 idx2)))))
           )
          )
          ;; Other positions remain unchanged
          (t
           (if (< i (length text-data))
             (setq new-text-list (append new-text-list 
               (list (list (car (nth i text-data)) 
                           (cadr (nth i text-data)) 
                           (nth i edited-list)
                           (cadddr (nth i text-data))))))
             (if (/= (nth i edited-list) "")
               (setq new-text-list (append new-text-list 
                 (list (list nil (- 0 i) (nth i edited-list) (+ 10000 i)))))
             )
           )
          )
        )
        (setq i (1+ i))
      )
      
      ;; Update entity groups
      (setq *wwe-entity-groups* '())
      (foreach item new-text-list
        (setq *wwe-entity-groups* (append *wwe-entity-groups* (list (cadddr item))))
      )
      
      (princ (strcat "\nSwapped line " (itoa (1+ idx1)) " with line " (itoa (1+ idx2))))
      new-text-list
    )
  )
)

;; Show dialog and handle user interaction
(defun wwe:show-dialog (text-data / dcl-file dcl-id dialog-result new-data row-count i result-list line-val text-list pasted-lines new-text-list j checkbox-states swapped-data capacity-list csv-file-obj)
  ;; Store text data in global variable
  (setq *wwe-text-list* text-data)
  
  ;; Save text data for recall (strip entity references to avoid stale handles)
  (setq *wwe-last-text-data* '())
  (foreach item text-data
    (setq *wwe-last-text-data* (append *wwe-last-text-data*
      (list (list nil (cadr item) (caddr item) (cadddr item)))))
  )
  (setq *wwe-last-text-height* *wwe-text-height*)
  (setq *wwe-last-entity-groups* (append '() *wwe-entity-groups*))
  (wwe:save-last-data-to-file)
  
  ;; Initialize numbers-added flag if not set
  (if (not *wwe-numbers-added*)
    (setq *wwe-numbers-added* nil)
  )
  
  ;; Extract text strings from data
  (setq text-list '())
  (foreach item text-data
    (setq text-list (append text-list (list (caddr item))))
  )
  
  ;; Determine row count: actual text count + 5 extra empty lines
  (setq row-count (+ (length text-list) 5))
  
  ;; Write DCL content to temporary file
  (setq dcl-file (wwe:write-dcl row-count))
  
  (if (not dcl-file)
    (progn
      (alert "Error: Cannot create DCL file!")
      nil
    )
    (progn
      ;; Load DCL file
      (setq dcl-id (load_dialog dcl-file))
      
      (if (not (new_dialog "wwe_dialog" dcl-id))
        (progn
          (alert "Error: Cannot create dialog!")
          (unload_dialog dcl-id)
          nil
        )
        (progn
          ;; Load transformer data if not already loaded
          (if (not *wwe-transformer-data*)
            (setq *wwe-transformer-data* (wwe:read-csv))
          )
          
          ;; Populate transformer list box
          (if *wwe-transformer-data*
            (progn
              (setq capacity-list '())
              (foreach item *wwe-transformer-data*
                (setq capacity-list (append capacity-list (list (car item))))
              )
              (start_list "transformer_list")
              (mapcar 'add_list capacity-list)
              (end_list)
            )
          )
          
          ;; Set initial values for each edit box and checkbox
          (setq i 0)
          (while (< i row-count)
            (if (< i (length text-list))
              (set_tile (strcat "line_" (itoa i)) (nth i text-list))
              (set_tile (strcat "line_" (itoa i)) "")
            )
            ;; Initialize all checkboxes to unchecked
            (set_tile (strcat "chk_" (itoa i)) "0")
            (setq i (1+ i))
          )
          
          ;; Show current recorded text height in the height box (real-time display)
          (set_tile "eb_height" (rtos *wwe-text-height* 2 2))
          
          ;; Update info label if transformer is selected
          (if *wwe-selected-transformer*
            (set_tile "info_label" 
              (strcat (car *wwe-selected-transformer*) "    ѹ           " 
                      (cadr *wwe-selected-transformer*) "A     ѹ  " 
                      (caddr *wwe-selected-transformer*) "A"))
            (set_tile "info_label" "δѡ   ѹ  ")
          )
          
          ;; Add action callbacks for each checkbox to highlight edit box and control selection limit
          (setq i 0)
          (while (< i row-count)
            (action_tile (strcat "chk_" (itoa i))
              (strcat 
                "(progn "
                ;; Highlight/unhighlight the edit box
                "  (if (= $value \"1\") "
                "    (mode_tile \"line_" (itoa i) "\" 2) "
                "    (mode_tile \"line_" (itoa i) "\" 0)) "
                ;; Count selected checkboxes
                "  (setq selected-count 0) "
                "  (setq j 0) "
                "  (while (< j row-count) "
                "    (if (= (get_tile (strcat \"chk_\" (itoa j))) \"1\") "
                "      (setq selected-count (1+ selected-count))) "
                "    (setq j (1+ j))) "
                ;; Enable/disable checkboxes based on count
                "  (setq k 0) "
                "  (while (< k row-count) "
                "    (if (>= selected-count 2) "
                ;; If 2 or more selected, disable unchecked boxes
                "      (if (= (get_tile (strcat \"chk_\" (itoa k))) \"0\") "
                "        (mode_tile (strcat \"chk_\" (itoa k)) 1)) "
                ;; If less than 2 selected, enable all boxes
                "      (mode_tile (strcat \"chk_\" (itoa k)) 0)) "
                "    (setq k (1+ k))) "
                ")")
            )
            (setq i (1+ i))
          )
          
          ;; Transformer list selection action - apply data to lines 1-3 on selection
          (action_tile "transformer_list"
            (strcat
              "(progn "
              "  (setq trans-idx (atoi (get_tile \"transformer_list\"))) "
              "  (if (and *wwe-transformer-data* (< trans-idx (length *wwe-transformer-data*))) "
              "    (progn "
              "      (setq *wwe-selected-transformer* (nth trans-idx *wwe-transformer-data*)) "
              "      (if (>= row-count 1) (set_tile \"line_0\" (nth 4 *wwe-selected-transformer*))) "
              "      (if (>= row-count 2) (set_tile \"line_1\" (nth 5 *wwe-selected-transformer*))) "
              "      (if (>= row-count 3) (set_tile \"line_2\" (nth 3 *wwe-selected-transformer*))) "
              "      (set_tile \"info_label\" "
              "        (strcat (car *wwe-selected-transformer*) \"    ѹ           \" "
              "                (cadr *wwe-selected-transformer*) \"A     ѹ  \" "
              "                (caddr *wwe-selected-transformer*) \"A\")) "
              "    ) "
              "  ) "
              ")"
            )
          )
          
          ;; Redefine CSV path button action (formerly Extend button)
          (action_tile "btn_extend"
            "(progn (setq result-list '()) (setq i 0) (while (< i row-count) (setq result-list (append result-list (list (get_tile (strcat \"line_\" (itoa i)))))) (setq i (1+ i))) (done_dialog 9))"
          )
          
          ;; Copy button action - copy without closing dialog
          (action_tile "btn_copy"
            "(progn (setq result-list '()) (setq i 0) (while (< i row-count) (setq result-list (append result-list (list (get_tile (strcat \"line_\" (itoa i)))))) (setq i (1+ i))) (wwe:copy-to-clipboard result-list))"
          )
          
          ;; Paste button action
          (action_tile "btn_paste"
            "(done_dialog 5)"
          )
          
          ;; Swap button action
          (action_tile "btn_swap"
            "(progn (setq result-list '()) (setq checkbox-states '()) (setq i 0) (while (< i row-count) (setq result-list (append result-list (list (get_tile (strcat \"line_\" (itoa i)))))) (setq checkbox-states (append checkbox-states (list (get_tile (strcat \"chk_\" (itoa i)))))) (setq i (1+ i))) (done_dialog 7))"
          )
          
          ;; Add Numbers button action
          (action_tile "btn_add_numbers"
            "(progn (setq result-list '()) (setq i 0) (while (< i row-count) (setq result-list (append result-list (list (get_tile (strcat \"line_\" (itoa i)))))) (setq i (1+ i))) (done_dialog 6))"
          )
          
          ;; Text height box action - real-time update and remember last value
          (action_tile "eb_height"
            (strcat
              "(progn "
              "  (setq tmp-h (distof $value 2)) "
              "  (if (and tmp-h (> tmp-h 0)) "
              "    (progn "
              "      (setq *wwe-text-height* tmp-h) "
              "      (setq *wwe-last-text-height* tmp-h) "
              "      (wwe:save-last-data-to-file) "
              "    ) "
              ;; Invalid input: restore current recorded height
              "    (set_tile \"eb_height\" (rtos *wwe-text-height* 2 2)) "
              "  ) "
              ")"
            )
          )
          
          ;; Write to CAD button action - Create new MTEXT
          (action_tile "btn_write"
            "(progn (setq tmp-h (distof (get_tile \"eb_height\") 2)) (if (and tmp-h (> tmp-h 0)) (progn (setq *wwe-text-height* tmp-h) (setq *wwe-last-text-height* tmp-h) (wwe:save-last-data-to-file))) (setq result-list '()) (setq i 0) (while (< i row-count) (setq result-list (append result-list (list (get_tile (strcat \"line_\" (itoa i)))))) (setq i (1+ i))) (done_dialog 4))"
          )
          
          ;; OK button action - Update original entities
          (action_tile "btn_ok"
            "(progn (setq result-list '()) (setq i 0) (while (< i row-count) (setq result-list (append result-list (list (get_tile (strcat \"line_\" (itoa i)))))) (setq i (1+ i))) (done_dialog 1))"
          )
          
          ;; Pick new text button action
          (action_tile "btn_pick"
            "(done_dialog 2)"
          )
          
          ;; Cancel button action: click / ESC / close-X exits with status 0
          (action_tile "btn_cancel"
            "(done_dialog 0)"
          )
          
          ;; Show dialog and get return code
          (setq dialog-result (start_dialog))
          
          ;; Unload dialog
          (unload_dialog dcl-id)
          
          ;; Handle based on return code
          (cond
            ;; Return code 0: Cancel/Close - exit dialog
            ((= dialog-result 0)
             (princ "\nDialog closed")
             '()  ; Return empty list to exit
            )
            
            ;; Return code 1: OK - Update original entities
            ((= dialog-result 1)
             (wwe:update-original-entities *wwe-text-list* result-list)
             result-list
            )
            
            ;; Return code 2: Pick new text
            ((= dialog-result 2)
             (setq new-data (wwe:pick-text))
             (if new-data
               (wwe:show-dialog new-data)
               (wwe:show-dialog text-data)
             )
            )
            
            ;; Return code 4: Write to CAD - Create new MTEXT
            ((= dialog-result 4)
             (wwe:create-new-mtext result-list)
             result-list
            )
            
            ;; Return code 5: Paste from clipboard
            ((= dialog-result 5)
             (setq pasted-lines (wwe:paste-from-clipboard))
             (if pasted-lines
               ;; Replace text content but keep original entity references and groups
               (progn
                 (setq new-text-list '())
                 (setq i 0)
                 (foreach line pasted-lines
                   ;; Keep original entity, y-coord, and group-id if available
                   (if (< i (length text-data))
                     (setq new-text-list (append new-text-list 
                       (list (list (car (nth i text-data)) 
                                   (cadr (nth i text-data)) 
                                   line
                                   (cadddr (nth i text-data))))))
                     ;; If more pasted lines than original, create new entries with unique group-id
                     (setq new-text-list (append new-text-list 
                       (list (list nil (- 0 i) line (+ 10000 i)))))
                   )
                   (setq i (1+ i))
                 )
                 ;; Update entity groups
                 (setq *wwe-entity-groups* '())
                 (foreach item new-text-list
                   (setq *wwe-entity-groups* (append *wwe-entity-groups* (list (cadddr item))))
                 )
                 (wwe:show-dialog new-text-list)
               )
               (wwe:show-dialog text-data)
             )
            )
            
            ;; Return code 6: Add Numbers (toggle on/off with auto-detection)
            ((= dialog-result 6)
             (setq new-text-list '())
             (setq i 0)
             
             ;; Auto-detect if numbers are already present (only on first click)
             (if (not *wwe-numbers-added*)
               (progn
                 (setq has-numbers nil)
                 (setq check-count 0)
                 (setq numbered-count 0)
                 (setq j 0)
                 ;; Check first few non-empty lines
                 (while (and (< j (length result-list)) (< check-count 3))
                   (setq check-line (nth j result-list))
                   (if (and check-line (/= check-line ""))
                     (progn
                       (setq check-count (1+ check-count))
                       ;; Check if line starts with number pattern
                       (if (and (>= (strlen check-line) 2)
                                (wcmatch check-line "[0-9]*.*"))
                         (progn
                           ;; Verify it's actually "digit(s)." pattern
                           (setq verify-pos 0)
                           (setq found-dot nil)
                           (setq all-digits T)
                           (while (and (< verify-pos (strlen check-line)) (not found-dot))
                             (setq ch (substr check-line (1+ verify-pos) 1))
                             (if (= ch ".")
                               (setq found-dot T)
                               (if (not (and (>= (ascii ch) 48) (<= (ascii ch) 57)))
                                 (setq all-digits nil)
                               )
                             )
                             (setq verify-pos (1+ verify-pos))
                           )
                           (if (and found-dot all-digits (> verify-pos 1))
                             (setq numbered-count (1+ numbered-count))
                           )
                         )
                       )
                     )
                   )
                   (setq j (1+ j))
                 )
                 ;; If more than half of checked lines have numbers, assume numbers exist
                 (if (and (> check-count 0) (>= numbered-count (/ check-count 2)))
                   (setq *wwe-numbers-added* T)
                   (setq *wwe-numbers-added* nil)
                 )
               )
             )
             
             ;; Toggle the numbers-added flag
             (if *wwe-numbers-added*
               ;; Remove numbers
               (progn
                 (while (< i (length result-list))
                   (setq line-val (nth i result-list))
                   (if (and line-val (/= line-val ""))
                     (progn
                       ;; Check if line starts with number pattern (e.g., "1.", "12.", "123.")
                       (setq cleaned-line line-val)
                       (if (and (>= (strlen line-val) 2)
                                (wcmatch line-val "[0-9]*.*"))
                         (progn
                           ;; Find the position of first dot after numbers
                           (setq dot-pos 0)
                           (setq found-dot nil)
                           (while (and (< dot-pos (strlen line-val)) (not found-dot))
                             (if (= (substr line-val (1+ dot-pos) 1) ".")
                               (progn
                                 (setq found-dot T)
                                 ;; Remove "number." prefix
                                 (if (< (1+ dot-pos) (strlen line-val))
                                   (setq cleaned-line (substr line-val (+ dot-pos 2)))
                                   (setq cleaned-line "")
                                 )
                               )
                               (progn
                                 ;; Check if current char is a digit
                                 (setq ch (substr line-val (1+ dot-pos) 1))
                                 (if (not (and (>= (ascii ch) 48) (<= (ascii ch) 57)))
                                   (setq found-dot T)  ; Stop if not a digit
                                   (setq dot-pos (1+ dot-pos))
                                 )
                               )
                             )
                           )
                         )
                       )
                       
                       ;; Add to new list
                       (if (< i (length text-data))
                         (setq new-text-list (append new-text-list 
                           (list (list (car (nth i text-data)) 
                                       (cadr (nth i text-data)) 
                                       cleaned-line
                                       (cadddr (nth i text-data))))))
                         (if (/= cleaned-line "")
                           (setq new-text-list (append new-text-list 
                             (list (list nil (- 0 i) cleaned-line (+ 10000 i)))))
                         )
                       )
                     )
                     ;; Keep empty lines if they're part of original text-data
                     (if (< i (length text-data))
                       (setq new-text-list (append new-text-list 
                         (list (list (car (nth i text-data)) 
                                     (cadr (nth i text-data)) 
                                     line-val
                                     (cadddr (nth i text-data))))))
                     )
                   )
                   (setq i (1+ i))
                 )
                 (setq *wwe-numbers-added* nil)
                 (princ "\nNumbers removed from lines")
               )
               ;; Add numbers
               (progn
                 (setq line-num 1)
                 (while (< i (length result-list))
                   (setq line-val (nth i result-list))
                   (if (and line-val (/= line-val ""))
                     (progn
                       ;; Add number prefix to non-empty lines
                       (if (< i (length text-data))
                         (setq new-text-list (append new-text-list 
                           (list (list (car (nth i text-data)) 
                                       (cadr (nth i text-data)) 
                                       (strcat (itoa line-num) "." line-val)
                                       (cadddr (nth i text-data))))))
                         (setq new-text-list (append new-text-list 
                           (list (list nil (- 0 i) (strcat (itoa line-num) "." line-val) (+ 10000 i)))))
                       )
                       (setq line-num (1+ line-num))
                     )
                     ;; Only keep empty lines if they're part of original text-data
                     (if (< i (length text-data))
                       (setq new-text-list (append new-text-list 
                         (list (list (car (nth i text-data)) 
                                     (cadr (nth i text-data)) 
                                     line-val
                                     (cadddr (nth i text-data))))))
                     )
                   )
                   (setq i (1+ i))
                 )
                 (setq *wwe-numbers-added* T)
                 (princ (strcat "\nAdded numbers to " (itoa (1- line-num)) " line(s)"))
               )
             )
             
             ;; Update entity groups
             (setq *wwe-entity-groups* '())
             (foreach item new-text-list
               (setq *wwe-entity-groups* (append *wwe-entity-groups* (list (cadddr item))))
             )
             (wwe:show-dialog new-text-list)
            )
            
            ;; Return code 7: Swap selected lines
            ((= dialog-result 7)
             (setq swapped-data (wwe:swap-lines text-data result-list checkbox-states))
             (if swapped-data
               (wwe:show-dialog swapped-data)
               (wwe:show-dialog text-data)
             )
            )
            
            ;; Return code 9: Redefine CSV path
            ((= dialog-result 9)
             ;; Save current edit box values to new-text-list
             (setq new-text-list '())
             (setq i 0)
             (while (< i (length result-list))
               (if (< i (length text-data))
                 (setq new-text-list (append new-text-list
                   (list (list (car (nth i text-data))
                               (cadr (nth i text-data))
                               (nth i result-list)
                               (cadddr (nth i text-data))))))
                 (if (/= (nth i result-list) "")
                   (setq new-text-list (append new-text-list
                     (list (list nil (- 0 i) (nth i result-list) (+ 10000 i))))))
               )
               (setq i (1+ i))
             )
             ;; Update entity groups
             (setq *wwe-entity-groups* '())
             (foreach item new-text-list
               (setq *wwe-entity-groups* (append *wwe-entity-groups* (list (cadddr item))))
             )
             
             ;; Prompt user to select new CSV file
             (setq csv-file-obj (getfiled "ѡ   ѹ     ݱ  " "" "csv" 8))
             (if csv-file-obj
               (progn
                 ;; Save to registry
                 (vl-registry-write "HKEY_CURRENT_USER\\Software\\WWE_LISP" "CSVPath" csv-file-obj)
                 (princ (strcat "\nCSV path saved: " csv-file-obj))
                 
                 ;; Clear and reload transformer data
                 (setq *wwe-transformer-data* nil)
                 (setq *wwe-transformer-data* (wwe:read-csv))
                 (setq *wwe-selected-transformer* nil)
               )
             )
             
             ;; Reopen dialog with current data
             (wwe:show-dialog new-text-list)
            )
            
            ;; Other cases
            (t nil)
          )
        )
      )
    )
  )
)

;; Load message
(princ "\nText Pick Tool loaded!")
(princ "\nCommand name: WWE")
(princ "\nFunction: Interactive selection and viewing of TEXT/MTEXT objects")
(princ "\nFeatures: Multi-line editor with proper MTEXT grouping support")
(princ)
