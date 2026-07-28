;;; ADZ - Quick Text Filter Command
;;; Function: Filter and select only TEXT and MTEXT objects within user-defined window

;; Global variable: Save last selection
(setq *ADZ_LAST_SELECTION* nil)

;@name 快速筛选文字
;@group 文本编辑
;@desc 通过框选区域，自动筛选并只选中TEXT和MTEXT对象，过滤其他类型的图元
;@require Selection
;@require ModelSpace
(defun c:ADZ ()
  (setq ss nil)
  (setq text-ss nil)
  (setq text-count 0)
  
  (princ "\n=== ADZ Quick Text Filter ===")
  
  ;; Prompt user to select objects with window
  (princ "\nSelect area to filter text objects (TEXT/MTEXT only): ")
  (setq ss (ssget))
  
  (cond
    ;; User selected objects
    (ss
     (setq text-ss (filter-text-objects ss))
     (if text-ss
       (progn
         (setq text-count (sslength text-ss))
         (princ (strcat "\nFiltered " (itoa text-count) " text object(s)"))
         
         ;; Save selection for future use
         (setq *ADZ_LAST_SELECTION* text-ss)
         
         ;; Set as current selection (keeps objects selected)
         (sssetfirst nil text-ss)
         (princ "\nText objects filtered and selected.")
       )
       (princ "\nNo TEXT or MTEXT objects found in selection!")
     )
    )
    
    ;; No selection
    (t (princ "\nNo objects selected."))
  )
  
  (princ)
)

;; Filter only TEXT and MTEXT objects
(defun filter-text-objects (ss / i ent ent-type text-ss text-list)
  (setq text-list '())
  (setq i 0)
  
  ;; Loop through selection set
  (repeat (sslength ss)
    (setq ent (ssname ss i))
    (setq ent-type (cdr (assoc 0 (entget ent))))
    
    ;; Check if entity is TEXT or MTEXT
    (if (or (= ent-type "TEXT") (= ent-type "MTEXT"))
      (setq text-list (cons ent text-list))
    )
    
    (setq i (1+ i))
  )
  
  ;; Create new selection set with filtered text objects
  (if text-list
    (progn
      (setq text-ss (ssadd))
      (foreach ent (reverse text-list)
        (ssadd ent text-ss)
      )
      text-ss
    )
    nil
  )
)

;; Get statistics of selection
(defun get-text-statistics (ss / i ent ent-type text-count mtext-count)
  (setq text-count 0)
  (setq mtext-count 0)
  (setq i 0)
  
  (repeat (sslength ss)
    (setq ent (ssname ss i))
    (setq ent-type (cdr (assoc 0 (entget ent))))
    
    (cond
      ((= ent-type "TEXT") (setq text-count (1+ text-count)))
      ((= ent-type "MTEXT") (setq mtext-count (1+ mtext-count)))
    )
    
    (setq i (1+ i))
  )
  
  (princ (strcat "\n  TEXT: " (itoa text-count)))
  (princ (strcat "\n  MTEXT: " (itoa mtext-count)))
)

(princ "\nCommand ADZ loaded. Type ADZ to start.")
(princ)
