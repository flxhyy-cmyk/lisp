;;; ADX - Vertical Text Alignment Command
;;; Function: Automatically arrange multiple text objects vertically with spacing

;; Global variable: Save last data
(setq *ADX_LAST_DATA* nil)

;@name 对齐多个文字
;@group 文本编辑
;@desc 一次选择多个文字对象，按垂直方向排列并调整间距。文字基点之间的距离默认为1.3倍文字高度
;@require Selection
;@require ModelSpace
(defun c:ADX ()
  (setq ss nil)
  (setq texts nil)
  (setq sorted-data nil)
  
  (princ "\n=== ADX Vertical Text Alignment ===")
  
  ;; Select text objects
  (if *ADX_LAST_DATA*
    (progn
      (princ "\nSelect text objects [Press ENTER to use last selection]: ")
      (setq ss (ssget))
    )
    (progn
      (princ "\nSelect text objects: ")
      (setq ss (ssget))
    )
  )
  
  (cond
    ;; User pressed ENTER and last data exists
    ((and (not ss) *ADX_LAST_DATA*)
     (if (validate-previous-data-adx *ADX_LAST_DATA*)
       (progn
         (princ "\nUsing last selection...")
         (align-vertical-texts-with-data *ADX_LAST_DATA*)
       )
       (progn
         (princ "\nLast selection is invalid. Please select again.")
         (setq *ADX_LAST_DATA* nil)
       )
     )
    )
    
    ;; User selected new objects
    (ss
     (setq texts (filter-texts ss))
     (if (>= (length texts) 2)
       (align-vertical-texts texts)
       (princ "\nAt least 2 text objects required!")
     )
    )
    
    ;; Invalid selection
    (t (princ "\nNo objects selected."))
  )
  
  (princ)
)

;; Filter text objects
(defun filter-texts (ss / i ent ent-type texts count)
  (setq texts '())
  (setq count 0)
  (setq i 0)
  
  (repeat (sslength ss)
    (setq ent (ssname ss i))
    (setq ent-type (cdr (assoc 0 (entget ent))))
    
    (if (or (= ent-type "TEXT") (= ent-type "MTEXT"))
      (progn
        (setq texts (cons ent texts))
        (setq count (1+ count))
      )
    )
    (setq i (1+ i))
  )
  
  (princ (strcat "\nFound " (itoa count) " text objects"))
  (reverse texts)
)

;; Prepare text data
(defun prepare-text-data (texts / data ent insert-pt bbox-info top-y left-x height)
  (setq data '())
  
  (foreach ent texts
    (setq insert-pt (get-text-insertion-point ent))
    (setq bbox-info (get-text-bbox-info ent))
    
    (if bbox-info
      (progn
        (setq top-y (cadr (cadr bbox-info)))  ; Max Y of bounding box
        (setq left-x (car (car bbox-info)))   ; Min X of bounding box
        (setq height (caddr bbox-info))       ; Height of bounding box
        
        (setq data (cons (list ent insert-pt top-y left-x height) data))
      )
    )
  )
  
  (reverse data)
)

;; Sort by Y coordinate (top to bottom)
(defun sort-by-y-adx (data / )
  (vl-sort data
    '(lambda (a b) (> (caddr a) (caddr b)))
  )
)

;; Execute vertical alignment
(defun execute-vertical-alignment-texts (sorted-data / 
  min-height line-spacing first-text current-y 
  prev-text prev-top-y prev-height
  ent insert-pt top-y left-x height
  align-x target-y move-y vla-obj pt1 pt2)
  
  ;; Calculate minimum text height
  (setq min-height (apply 'min (mapcar 'last sorted-data)))
  (setq line-spacing (* min-height 0.5))
  
  (princ (strcat "\nMin text height: " (rtos min-height 2 2)))
  (princ (strcat "\nLine spacing: " (rtos line-spacing 2 2)))
  
  ;; First text as reference
  (setq first-text (car sorted-data))
  (setq align-x (cadddr first-text))  ; Left X of first text
  
  (princ "\nAligning...")
  
  ;; Process first text (no move)
  (setq prev-text first-text)
  (setq prev-top-y (caddr first-text))
  (setq prev-height (last first-text))
  
  ;; Process remaining texts
  (foreach text-data (cdr sorted-data)
    (setq ent (car text-data))
    (setq insert-pt (cadr text-data))
    (setq top-y (caddr text-data))
    (setq left-x (cadddr text-data))
    (setq height (last text-data))
    
    ;; Calculate target position
    (setq target-y (- prev-top-y prev-height line-spacing))
    (setq move-y (- target-y top-y))
    
    ;; Move text
    (setq vla-obj (vlax-ename->vla-object ent))
    (setq pt1 (vlax-3d-point (list (car insert-pt) (cadr insert-pt) 0)))
    (setq pt2 (vlax-3d-point (list (- align-x left-x) move-y 0)))
    
    (vla-move vla-obj pt1 
              (vlax-3d-point 
                (list (+ (car insert-pt) (- align-x left-x))
                      (+ (cadr insert-pt) move-y)
                      0)))
    
    ;; Update previous text info
    (setq prev-top-y target-y)
    (setq prev-height height)
  )
  
  (princ "\nAlignment completed!")
)

;; Align newly selected texts
(defun align-vertical-texts (texts / text-data sorted-data)
  (setq text-data (prepare-text-data texts))
  
  (if (< (length text-data) 2)
    (princ "\nCannot get enough text bounding box info!")
    (progn
      (setq sorted-data (sort-by-y-adx text-data))
      (execute-vertical-alignment-texts sorted-data)
      
      ;; Save data
      (setq *ADX_LAST_DATA* 
        (list 
          (cons 'sorted-data sorted-data)
          (cons 'first-text (car sorted-data))
          (cons 'line-spacing-factor 0.5)
        )
      )
    )
  )
)

;; Realign using last data
(defun align-vertical-texts-with-data (last-data / 
  sorted-data updated-data ent new-bbox-info new-insert-pt)
  
  (setq sorted-data (cdr (assoc 'sorted-data last-data)))
  (setq updated-data '())
  
  ;; Update current bounding box info for all texts
  (foreach text-data sorted-data
    (setq ent (car text-data))
    (setq new-insert-pt (get-text-insertion-point ent))
    (setq new-bbox-info (get-text-bbox-info ent))
    
    (if new-bbox-info
      (setq updated-data 
        (cons 
          (list ent 
                new-insert-pt
                (cadr (cadr new-bbox-info))  ; top-y
                (car (car new-bbox-info))    ; left-x
                (caddr new-bbox-info))       ; height
          updated-data))
    )
  )
  
  (setq updated-data (reverse updated-data))
  
  (if (>= (length updated-data) 2)
    (progn
      (execute-vertical-alignment-texts updated-data)
      
      ;; Update saved data
      (setq *ADX_LAST_DATA* 
        (list 
          (cons 'sorted-data updated-data)
          (cons 'first-text (car updated-data))
          (cons 'line-spacing-factor 0.5)
        )
      )
    )
    (princ "\nCannot update text info!")
  )
)

;; Validate previous data
(defun validate-previous-data-adx (last-data / sorted-data valid)
  (setq sorted-data (cdr (assoc 'sorted-data last-data)))
  (setq valid T)
  
  (foreach text-data sorted-data
    (if (not (entget (car text-data)))
      (setq valid nil)
    )
  )
  
  valid
)

;; Get text insertion point
(defun get-text-insertion-point (ent / )
  (cdr (assoc 10 (entget ent)))
)

;; Get text bounding box info
(defun get-text-bbox-info (ent / result min-pt max-pt height)
  (setq result 
    (vl-catch-all-apply 'vla-getboundingbox
      (list (vlax-ename->vla-object ent) 'min-pt 'max-pt)))
  
  (if (not (vl-catch-all-error-p result))
    (progn
      (setq min-pt (vlax-safearray->list min-pt))
      (setq max-pt (vlax-safearray->list max-pt))
      (setq height (- (cadr max-pt) (cadr min-pt)))
      (list min-pt max-pt height)
    )
    nil
  )
)

;; Get text actual height
(defun get-text-actual-height (ent / bbox-info)
  (setq bbox-info (get-text-bbox-info ent))
  (if bbox-info
    (caddr bbox-info)
    (get-text-height ent)
  )
)

;; Get text height property
(defun get-text-height (ent / )
  (cdr (assoc 40 (entget ent)))
)

(princ "\nCommand ADX loaded. Type ADX to start.")
(princ)
