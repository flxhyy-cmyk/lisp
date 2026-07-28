;;; XXZ - Construction Line Auto-Trim within Rectangle
;;; Function: Get two points from user and auto-trim line to rectangle bounds

;@name 矩形内构造线自动裁剪
;@group 图形编辑
;@desc 用户点击两个点，程序自动创建直线并找到包含起点的矩形进行截断
;@require ModelSpace
;@order 20
(defun c:XXZ ()
  (princ "\n=== XXZ Construction Line Auto-Trim ===")
  (princ "\nClick first point: ")
  
  ;; Get first point from user
  (setq pt1 (getpoint))
  
  (if pt1
    (progn
      (princ "\nClick second point: ")
      
      ;; Get second point from user
      (setq pt2 (getpoint pt1))
      
      (if pt2
        (progn
          (princ "\nCreating line and trimming to rectangle...")
          
          ;; Calculate direction vector
          (setq direction (list (- (car pt2) (car pt1))
                               (- (cadr pt2) (cadr pt1))))
          
          ;; Find rectangle and trim
          (xxz-find-and-trim-by-line pt1 direction pt2)
        )
        (princ "\nNo second point selected.")
      )
    )
    (princ "\nNo first point selected.")
  )
  
  (princ)
)

;; Find rectangle containing the line point and trim
(defun xxz-find-and-trim-by-line (line-pt direction pt2 / ent ent-type rect-data rect-points intersections pt-extended candidates best-rect rect-count distance-to-rect)
  (princ "\nSearching for containing rectangle...")
  (princ (strcat "\nLine point 1: " (xxz-point-to-string line-pt)))
  (princ (strcat "\nLine point 2: " (xxz-point-to-string pt2)))
  
  (setq candidates '())
  (setq rect-count 0)
  
  ;; Calculate extended point on the line (for intersection calculation)
  (setq pt-extended (list (+ (car line-pt) (* 10000 (car direction)))
                         (+ (cadr line-pt) (* 10000 (cadr direction)))))
  
  ;; Scan all entities using entnext
  (setq ent (entnext))
  (while ent
    (setq ent-type (cdr (assoc 0 (entget ent))))
    
    ;; Check if it's a LWPOLYLINE
    (if (= ent-type "LWPOLYLINE")
      (progn
        (setq rect-data (entget ent))
        
        ;; Check if it has 4 vertices (rectangle)
        (if (= (cdr (assoc 90 rect-data)) 4)
          (progn
            (setq rect-count (1+ rect-count))
            (setq rect-points (extract-rectangle-points rect-data))
            
            ;; Find intersections with rectangle edges
            (setq intersections (find-line-rect-intersections line-pt pt-extended rect-points))
            
            ;; If exactly 2 intersections, add to candidates
            (if (= (length intersections) 2)
              (progn
                ;; Check if line-pt is strictly inside this rectangle (not on edge)
                (if (point-in-rectangle line-pt rect-points)
                  ;; If start point is strictly inside, distance is 0 (highest priority)
                  (setq distance-to-rect 0)
                  ;; Otherwise, use distance to closest intersection point
                  (setq distance-to-rect (min (distance line-pt (nth 0 intersections))
                                             (distance line-pt (nth 1 intersections))))
                )
                
                ;; Store candidate with its distance
                (setq candidates (cons (list ent intersections distance-to-rect) candidates))
              )
            )
          )
        )
      )
    )
    
    (setq ent (entnext ent))
  )
  
  (princ (strcat "\nTotal rectangles checked: " (itoa rect-count)))
  (princ (strcat "\nValid candidates found: " (itoa (length candidates))))
  
  ;; Find the rectangle with minimum distance
  (if candidates
    (progn
      ;; Sort candidates by distance (ascending)
      (setq candidates (vl-sort candidates '(lambda (a b) (< (nth 2 a) (nth 2 b)))))
      
      ;; Get the first rectangle (closest distance)
      (setq best-rect (nth 0 candidates))
      
      (if best-rect
        (progn
          (princ (strcat "\nFound best rectangle! Distance: " (rtos (nth 2 best-rect) 2 2)))
          (xxz-create-trimmed-line line-pt (nth 1 best-rect))
        )
        (princ "\nNo valid rectangle found!")
      )
    )
    (princ "\nNo intersecting rectangle found!")
  )
)

;; Extract rectangle corner points from LWPOLYLINE entity data
(defun extract-rectangle-points (ent-data / points)
  (setq points '())
  
  (foreach item ent-data
    (if (= (car item) 10)
      (setq points (cons (cdr item) points))
    )
  )
  
  (reverse points)
)

;; Check if a point is strictly inside the rectangle (not on edge)
(defun point-in-rectangle (pt rect-points / x y x-min x-max y-min y-max tolerance)
  (setq x (car pt))
  (setq y (cadr pt))
  (setq tolerance 0.01)
  
  ;; Get rectangle bounds
  (setq x-min (apply 'min (mapcar 'car rect-points)))
  (setq x-max (apply 'max (mapcar 'car rect-points)))
  (setq y-min (apply 'min (mapcar 'cadr rect-points)))
  (setq y-max (apply 'max (mapcar 'cadr rect-points)))
  
  ;; Check if point is strictly inside (with tolerance for edge detection)
  (and (> x (+ x-min tolerance)) 
       (< x (- x-max tolerance)) 
       (> y (+ y-min tolerance)) 
       (< y (- y-max tolerance)))
)

;; Create trimmed line between two intersection points
(defun xxz-create-trimmed-line (line-pt intersections / trimmed-pt1 trimmed-pt2)
  (princ "\n\nCreating trimmed line...")
  (princ (strcat "\nIntersection 1: " (xxz-point-to-string (nth 0 intersections))))
  (princ (strcat "\nIntersection 2: " (xxz-point-to-string (nth 1 intersections))))
  
  ;; Use the two intersection points as endpoints
  (setq trimmed-pt1 (nth 0 intersections))
  (setq trimmed-pt2 (nth 1 intersections))
  
  ;; Create new line segment between the two intersection points
  (entmake (list
    (cons 0 "LINE")
    (cons 10 trimmed-pt1)
    (cons 11 trimmed-pt2)
  ))
  
  (princ "\nLine created from " (xxz-point-to-string trimmed-pt1) " to " (xxz-point-to-string trimmed-pt2))
)

;; Find intersection points between line and rectangle edges
(defun find-line-rect-intersections (line-pt1 line-pt2 rect-points / intersections i edge-start edge-end int-pt)
  (setq intersections '())
  (setq i 0)
  
  ;; Check intersection with each rectangle edge
  (repeat 4
    (setq edge-start (nth i rect-points))
    (setq edge-end (nth (rem (1+ i) 4) rect-points))
    
    (setq int-pt (line-intersection line-pt1 line-pt2 edge-start edge-end))
    (if int-pt
      (setq intersections (cons int-pt intersections))
    )
    
    (setq i (1+ i))
  )
  
  intersections
)

;; Calculate line-line intersection point
(defun line-intersection (p1 p2 p3 p4 / x1 y1 x2 y2 x3 y3 x4 y4 denom t1)
  (setq x1 (car p1) y1 (cadr p1))
  (setq x2 (car p2) y2 (cadr p2))
  (setq x3 (car p3) y3 (cadr p3))
  (setq x4 (car p4) y4 (cadr p4))
  
  (setq denom (- (* (- x1 x2) (- y3 y4)) (* (- y1 y2) (- x3 x4))))
  
  (if (not (equal denom 0 0.0001))
    (progn
      (setq t1 (/ (- (* (- x1 x3) (- y3 y4)) (* (- y1 y3) (- x3 x4))) denom))
      (list (+ x1 (* t1 (- x2 x1))) (+ y1 (* t1 (- y2 y1))))
    )
    nil
  )
)

;; Calculate distance between two points
(defun distance (pt1 pt2)
  (sqrt (+ (expt (- (car pt2) (car pt1)) 2)
           (expt (- (cadr pt2) (cadr pt1)) 2)))
)

;; Convert point to string for display
(defun xxz-point-to-string (pt)
  (strcat "(" (rtos (car pt) 2 2) ", " (rtos (cadr pt) 2 2) ")")
)

(princ "\nCommand XXZ loaded. Type XXZ to start.")
(princ)
