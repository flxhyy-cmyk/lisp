;;; ABCD - Rectangle Area Annotation Command
;;; Function: Calculate rectangle area and write it to the center

;; Global variable: Save last data
(setq *ABCD_LAST_DATA* nil)
(setq *ABCD_LENGTH_FACTOR* 100.0) ; Default: drawing length / 100 = meters

;@name 矩形面积标注
;@group 文本编辑
;@desc 选择矩形后自动计算面积并写入中心位置，文字高度为矩形较短边的0.5倍
;@require Selection
;@require ModelSpace
;@order 10
(defun c:ABCD ()
  (setq ss nil)
  (setq rects nil)
  
  (princ "\n=== ABCD Rectangle Area Annotation ===")
  
  ;; Select rectangle objects
  (if *ABCD_LAST_DATA*
    (progn
      (princ "\nSelect rectangles [Press ENTER to use last selection]: ")
      (setq ss (ssget))
    )
    (progn
      (princ "\nSelect rectangles: ")
      (setq ss (ssget))
    )
  )
  
  (cond
    ;; User pressed ENTER and last data exists
    ((and (not ss) *ABCD_LAST_DATA*)
     (if (validate-previous-data-abcd *ABCD_LAST_DATA*)
       (progn
         (princ "\nUsing last selection...")
         (annotate-rectangles-with-data *ABCD_LAST_DATA*)
       )
       (progn
         (princ "\nLast selection is invalid. Please select again.")
         (setq *ABCD_LAST_DATA* nil)
       )
     )
    )
    
    ;; User selected new objects
    (ss
     (setq rects (filter-rectangles-abcd ss))
     (if (>= (length rects) 1)
       (annotate-rectangles rects)
       (princ "\nNo valid rectangles selected!")
     )
    )
    
    ;; Invalid selection
    (t (princ "\nNo objects selected."))
  )
  
  (princ)
)

;; Filter rectangle objects (LWPOLYLINE with 4 vertices and closed)
(defun filter-rectangles-abcd (ss / i ent ent-type ent-data points closed-p rects count)
  (setq rects '())
  (setq count 0)
  (setq i 0)
  
  (repeat (sslength ss)
    (setq ent (ssname ss i))
    (setq ent-type (cdr (assoc 0 (entget ent))))
    
    ;; Check if it's a closed polyline with 4 vertices
    (if (= ent-type "LWPOLYLINE")
      (progn
        (setq ent-data (entget ent))
        (setq closed-p (cdr (assoc 70 ent-data)))
        (setq points (get-polyline-vertices ent-data))
        
        (if (and 
              (or (= closed-p 1) (= closed-p 129))
              (or (= (length points) 4) (= (length points) 5)))
          (progn
            (if (= (length points) 5)
              (setq points (butlast points))
            )
            (if (is-rectangle points)
              (progn
                (setq rects (cons (list ent points) rects))
                (setq count (1+ count))
              )
            )
          )
        )
      )
    )
    (setq i (1+ i))
  )
  
  (princ (strcat "\nFound " (itoa count) " rectangles"))
  (reverse rects)
)

;; Remove last element from list
(defun butlast (lst / result)
  (setq result '())
  (foreach item (reverse (cdr (reverse lst)))
    (setq result (cons item result))
  )
  result
)

;; Check if 4 points form a rectangle
(defun is-rectangle (points / p1 p2 p3 p4 v1 v2 v3 v4 tol)
  (setq tol 0.0001)
  (if (= (length points) 4)
    (progn
      (setq p1 (nth 0 points))
      (setq p2 (nth 1 points))
      (setq p3 (nth 2 points))
      (setq p4 (nth 3 points))
      
      (setq v1 (list (- (car p2) (car p1)) (- (cadr p2) (cadr p1))))
      (setq v2 (list (- (car p3) (car p2)) (- (cadr p3) (cadr p2))))
      (setq v3 (list (- (car p4) (car p3)) (- (cadr p4) (cadr p3))))
      (setq v4 (list (- (car p1) (car p4)) (- (cadr p1) (cadr p4))))
      
      ;; Check if opposite sides are parallel and adjacent sides are perpendicular
      (and
        (vectors-parallel-p v1 v3 tol)
        (vectors-parallel-p v2 v4 tol)
        (vectors-perpendicular-p v1 v2 tol)
      )
    )
    nil
  )
)

;; Check if two vectors are parallel
(defun vectors-parallel-p (v1 v2 tol / cross)
  (setq cross (abs (- (* (car v1) (cadr v2)) (* (cadr v1) (car v2)))))
  (< cross tol)
)

;; Check if two vectors are perpendicular
(defun vectors-perpendicular-p (v1 v2 tol / dot len1 len2)
  (setq dot (+ (* (car v1) (car v2)) (* (cadr v1) (cadr v2))))
  (setq len1 (sqrt (+ (* (car v1) (car v1)) (* (cadr v1) (cadr v1)))))
  (setq len2 (sqrt (+ (* (car v2) (car v2)) (* (cadr v2) (cadr v2)))))
  (if (and (> len1 0) (> len2 0))
    (< (abs dot) (* tol len1 len2))
    nil
  )
)

;; Get polyline vertices
(defun get-polyline-vertices (ent-data / vertices pt)
  (setq vertices '())
  (foreach item ent-data
    (if (= (car item) 10)
      (setq vertices (cons (cdr item) vertices))
    )
  )
  (reverse vertices)
)

;; Calculate rectangle properties
(defun get-rectangle-info (points / p1 p2 p3 p4 width height area center min-x max-x min-y max-y)
  (setq p1 (nth 0 points))
  (setq p2 (nth 1 points))
  (setq p3 (nth 2 points))
  (setq p4 (nth 3 points))
  
  ;; Calculate width and height
  (setq width (distance p1 p2))
  (setq height (distance p2 p3))
  
  ;; Convert length to meters first, then calculate square meters
  (setq area (* (/ width *ABCD_LENGTH_FACTOR*) (/ height *ABCD_LENGTH_FACTOR*)))
  
  ;; Calculate center
  (setq min-x (apply 'min (mapcar 'car points)))
  (setq max-x (apply 'max (mapcar 'car points)))
  (setq min-y (apply 'min (mapcar 'cadr points)))
  (setq max-y (apply 'max (mapcar 'cadr points)))
  (setq center (list (/ (+ min-x max-x) 2.0) (/ (+ min-y max-y) 2.0) 0.0))
  
  (list width height area center)
)



;; Annotate rectangles with area

;; Format number automatically (integer or decimal)
(defun format-number-auto (num)
  (if (= (fix num) num)
    (itoa (fix num))
    (rtos num 2 2)
  )
)
(defun annotate-rectangles (rects / rect-data ent points rect-info width height area center area-text text-ent text-width existing-text-list user-confirm)
  (setq rect-data '())
  
  ;; Check for existing text inside rectangles
  (setq existing-text-list (check-existing-text-in-rectangles rects))
  
  ;; Get user confirmation if existing text found
  (setq user-confirm (get-user-confirmation (length existing-text-list)))
  
  ;; Delete existing text only if user confirmed
  (if (and user-confirm (> (length existing-text-list) 0))
    (delete-existing-text existing-text-list)
  )
  
  ;; Create new annotations (regardless of user's delete choice)
  (foreach rect rects
    (setq ent (car rect))
    (setq points (cadr rect))
    (setq rect-info (get-rectangle-info points))
    
    (setq width (car rect-info))
    (setq height (cadr rect-info))
    (setq area (caddr rect-info))
    (setq center (cadddr rect-info))
    
    ;; Create area text
    (setq text-ent (create-text-labels center nil width height))
    
    (setq rect-data (cons (list ent points center text-ent) rect-data))
    
    (princ (strcat "\nRectangle area: " (format-number-auto area) " 平方米"))
  )
  
  ;; Save data for re-annotation
  (setq *ABCD_LAST_DATA* rect-data)
  
  (princ "\nArea annotation completed!")
)

;; Ensure "宋体" text style exists
(defun ensure-songti-style (/ style-name font-file style-data)
  (setq style-name "宋体")
  (setq font-file "SimSun.ttf")
  
  (if (not (tblsearch "STYLE" style-name))
    (entmake
      (list
        (cons 0 "STYLE")
        (cons 100 "AcDbSymbolTableRecord")
        (cons 100 "AcDbTextStyleTableRecord")
        (cons 2 style-name)
        (cons 70 0)
        (cons 40 0.0)
        (cons 41 1.0)
        (cons 50 0.0)
        (cons 71 0)
        (cons 42 2.5)
        (cons 3 font-file)
        (cons 4 "")
      )
    )
  )
  style-name
)

;; Create three separate TEXT entities for precise layout control
(defun create-text-labels (center text-content rect-width rect-height / style-name text-lines line-count max-line-len 
                               text-height-for-h text-height-for-w final-text-height 
                               line-spacing total-height y-offset 
                               area-str width-str height-str width-m height-m)
  (setq style-name (ensure-songti-style))

  ;; --- The Definitive Height Calculation --- 

  ;; Split the content into three separate strings
  (setq width-m (/ rect-width *ABCD_LENGTH_FACTOR*))
  (setq height-m (/ rect-height *ABCD_LENGTH_FACTOR*))
  (setq area-str (strcat "面积=" (format-number-auto (* width-m height-m)) " 平方米"))
  (setq width-str (strcat "宽=" (format-number-auto width-m) " 米"))
  (setq height-str (strcat "深=" (format-number-auto height-m) " 米"))
  (setq text-lines (list area-str width-str height-str))
  
  ;; 1. Calculate max text height based on the RECTANGLE'S HEIGHT.
  ;; The factor 4.0 is a safe value for 3 lines of text including standard line spacing.
  (setq text-height-for-h (/ (* rect-height 0.95) 4.0))

  ;; 2. Calculate max text height based on the RECTANGLE'S WIDTH.
  ;; Find the longest of the three strings to use for width calculation.
  (setq max-line-len 0)
  (foreach line text-lines
    (if (> (strlen line) max-line-len)
      (setq max-line-len (strlen line))
    )
  )
  (setq text-height-for-w (/ (* rect-width 0.8) (* max-line-len 0.7)))

  ;; 3. Use the SMALLER of the two calculated heights.
  ;; This ensures BOTH height and width constraints are met for all lines.
  (setq final-text-height (min text-height-for-h text-height-for-w))

  ;; --- Precise Positioning --- 
  (setq line-spacing (* final-text-height 1.5)) ; Vertical distance between text baselines

  ;; Create the middle text ("Width") at the exact center
  (entmake
    (list
      (cons 0 "TEXT") (cons 100 "AcDbEntity") (cons 100 "AcDbText")
      (cons 10 center) (cons 40 final-text-height) (cons 1 width-str)
      (cons 7 style-name) (cons 62 1) (cons 72 1) (cons 73 2)
      (cons 11 center)
    )
  )

  ;; Create the top text ("Area")
  (setq y-offset (list (car center) (+ (cadr center) line-spacing) (caddr center)))
  (entmake
    (list
      (cons 0 "TEXT") (cons 100 "AcDbEntity") (cons 100 "AcDbText")
      (cons 10 y-offset) (cons 40 final-text-height) (cons 1 area-str)
      (cons 7 style-name) (cons 62 1) (cons 72 1) (cons 73 2)
      (cons 11 y-offset)
    )
  )

  ;; Create the bottom text ("Height")
  (setq y-offset (list (car center) (- (cadr center) line-spacing) (caddr center)))
  (entmake
    (list
      (cons 0 "TEXT") (cons 100 "AcDbEntity") (cons 100 "AcDbText")
      (cons 10 y-offset) (cons 40 final-text-height) (cons 1 height-str)
      (cons 7 style-name) (cons 62 1) (cons 72 1) (cons 73 2)
      (cons 11 y-offset)
    )
  )

  ;; Return the entity name of the middle text for reference
  (entlast)
)

;; Re-annotate using last data
(defun annotate-rectangles-with-data (last-data / updated-data ent points rect-info center width height area area-text text-ent text-width old-text rects existing-text-list user-confirm)
  (setq updated-data '())
  (setq rects '())
  
  ;; Convert last-data to rects format for checking
  (foreach rect-data last-data
    (setq rects (cons (list (car rect-data) (cadr rect-data)) rects))
  )
  
  ;; Check for existing text inside rectangles
  (setq existing-text-list (check-existing-text-in-rectangles rects))
  
  ;; Get user confirmation if existing text found
  (setq user-confirm (get-user-confirmation (length existing-text-list)))
  
  ;; Delete existing text only if user confirmed
  (if (and user-confirm (> (length existing-text-list) 0))
    (delete-existing-text existing-text-list)
  )
  
  ;; Process each rectangle (regardless of user's delete choice)
  (foreach rect-data last-data
    (setq ent (car rect-data))
    (setq points (cadr rect-data))
    (setq center (caddr rect-data))
    (setq old-text (cadddr rect-data))
    
    ;; Delete old text if exists
    (if (and old-text (entget old-text))
      (entdel old-text)
    )
    
    ;; Recalculate rectangle info
    (setq rect-info (get-rectangle-info points))
    (setq width (car rect-info))
    (setq height (cadr rect-info))
    (setq area (caddr rect-info))
    
    ;; Create new area text
    (setq text-ent (create-text-labels center nil width height))
    
    (setq updated-data (cons (list ent points center text-ent) updated-data))
    
    (princ (strcat "\nRectangle area: " (format-number-auto area) " 平方米"))
  )
  
  ;; Update saved data
  (setq *ABCD_LAST_DATA* (reverse updated-data))
  
  (princ "\nRe-annotation completed!")
)

;; Check for existing text inside rectangles
(defun check-existing-text-in-rectangles (rects / existing-text-list rect ent points center min-x max-x min-y max-y text-ents ss-text i text-ent text-point)
  (setq existing-text-list '())
  
  (foreach rect rects
    (setq ent (car rect))
    (setq points (cadr rect))
    
    ;; Get rectangle bounds
    (setq min-x (apply 'min (mapcar 'car points)))
    (setq max-x (apply 'max (mapcar 'car points)))
    (setq min-y (apply 'min (mapcar 'cadr points)))
    (setq max-y (apply 'max (mapcar 'cadr points)))
    
    ;; Select all text objects in the drawing
    (setq ss-text (ssget "_X" '((0 . "MTEXT,TEXT"))))
    
    (if ss-text
      (progn
        (setq i 0)
        (repeat (sslength ss-text)
          (setq text-ent (ssname ss-text i))
          (setq text-point (cdr (assoc 10 (entget text-ent))))
          
          ;; Check if text is inside rectangle
          (if (and 
                (>= (car text-point) min-x)
                (<= (car text-point) max-x)
                (>= (cadr text-point) min-y)
                (<= (cadr text-point) max-y)
              )
            (setq existing-text-list (cons text-ent existing-text-list))
          )
          (setq i (1+ i))
        )
      )
    )
  )
  
  existing-text-list
)

;; Get user confirmation for deleting existing text
(defun get-user-confirmation (existing-text-count / user-input)
  (if (> existing-text-count 0)
    (progn
      (princ (strcat "\nFound " (itoa existing-text-count) " existing text objects inside rectangles."))
      (princ "\nDelete existing text? [Y/N] <Y>: ")
      (setq user-input (getstring))
      (or (= user-input "") (= (strcase user-input) "Y") (= (strcase user-input) "YES"))
    )
    T  ; No existing text, proceed automatically
  )
)

;; Delete existing text objects
(defun delete-existing-text (existing-text-list)
  (foreach text-ent existing-text-list
    (if (entget text-ent)
      (entdel text-ent)
    )
  )
  (princ (strcat "\nDeleted " (itoa (length existing-text-list)) " existing text objects."))
)

;; Validate previous data
(defun validate-previous-data-abcd (last-data / valid ent)
  (setq valid T)
  
  (foreach rect-data last-data
    (setq ent (car rect-data))
    (if (not (entget ent))
      (setq valid nil)
    )
  )
  
  valid
)

(princ "\nCommand ABCD loaded. Type ABCD to start.")
(princ)
