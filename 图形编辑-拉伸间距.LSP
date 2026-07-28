;@name 拉伸间距 / 拉伸间距_单线版
;@group 图形编辑
;@desc STE: 选择两条垂直直线，根据X坐标判断增减间距，自动拉伸横线、移动右侧实体、居中文字。STZ: 选择一条垂直线，移动右侧实体、拉伸穿过线的水平线、复制该线并移动（字母前缀跳过复制）。支持回车重用上次选择
;@require Selection
;@require ModelSpace
(defun c:STE ()
  (if (and *ST_LAST_LINE1* *ST_LAST_LINE2*)
    (princ "\nSelect first vertical line (or press Enter to use last selection): ")
    (princ "\nSelect first vertical line: ")
  )
  
  (setq ent1 (entsel))
  
  (if (not ent1)
    (progn
      (if (and *ST_LAST_LINE1* *ST_LAST_LINE2*)
        (progn
          (princ "\nUsing last selection...")
          (setq ent1 (list *ST_LAST_LINE1*))
          (setq ent2 (list *ST_LAST_LINE2*))
          (setq use-last T)
        )
        (progn
          (princ "\nNo line selected.")
          (princ)
          (setq use-last nil)
        )
      )
    )
    (setq use-last nil)
  )
  
  (if (or use-last ent1)
    (progn
      (if (not use-last)
        (progn
          (princ "\nSelect second vertical line: ")
          (setq ent2 (entsel))
        )
      )
      
      (if (not ent2)
        (progn (princ "\nNo line selected.") (princ))
        (progn
          (initget 6)
          (setq stretch-dist (getdist "\nEnter stretch distance <20>: "))
          (if (not stretch-dist)
            (setq stretch-dist 20)
          )
          
          (setq data1 (entget (car ent1)))
          (setq data2 (entget (car ent2)))
          (setq p1a (cdr (assoc 10 data1)))
          (setq p1b (cdr (assoc 11 data1)))
          (setq p2a (cdr (assoc 10 data2)))
          (setq p2b (cdr (assoc 11 data2)))
          
          (setq x1 (car p1a))
          (setq x2 (car p2a))
          
          (if (< x1 x2)
            (progn
              (setq left-line ent1)
              (setq right-line ent2)
              (setq left-x x1)
              (setq right-x x2)
              (setq left-data data1)
              (setq right-data data2)
              (setq increase T)
            )
            (progn
              (setq left-line ent2)
              (setq right-line ent1)
              (setq left-x x2)
              (setq right-x x1)
              (setq left-data data2)
              (setq right-data data1)
              (setq increase nil)
            )
          )
          
          (setq left-p1 (cdr (assoc 10 left-data)))
          (setq left-p2 (cdr (assoc 11 left-data)))
          (setq y-min (min (cadr left-p1) (cadr left-p2)))
          (setq y-max (max (cadr left-p1) (cadr left-p2)))
          
          (if increase
            (setq move-dist stretch-dist)
            (setq move-dist (- stretch-dist))
          )
          
          (princ "\nFinding rectangles and text...")
          (setq rectangles (find-rectangles-between-lines (car left-line) (car right-line) y-min y-max))
          
          (setq rectangle-texts (find-text-in-rectangles rectangles))
          
          (setq corner1 (list right-x y-min))
          (setq corner2 (list 999999 y-max))
          
          (setq ss-move (ssget "_W" corner1 corner2))
          
          (setq narrow-corner1 (list (- right-x 1) y-min))
          (setq narrow-corner2 (list (+ right-x 1) y-max))
          
          (setq ss-intersect (ssget "_C" narrow-corner1 narrow-corner2 '((0 . "LINE"))))
          
          (if ss-intersect
            (progn
              (setq i 0)
              (while (< i (sslength ss-intersect))
                (setq ent (ssname ss-intersect i))
                (if (not (eq ent (car right-line)))
                  (progn
                    (setq data (entget ent))
                    (setq pa (cdr (assoc 10 data)))
                    (setq pb (cdr (assoc 11 data)))
                    (setq is-horizontal (< (abs (- (cadr pa) (cadr pb))) 0.01))
                    
                    (if is-horizontal
                      (progn
                        (setq xa (car pa))
                        (setq xb (car pb))
                        (setq crosses (and (<= (min xa xb) (+ right-x 0.01))
                                          (>= (max xa xb) (- right-x 0.01))))
                        
                        (if crosses
                          (progn
                            (if (> xa xb)
                              (progn
                                (setq new-pa (list (+ (car pa) move-dist) (cadr pa) (caddr pa)))
                                (setq data (subst (cons 10 new-pa) (assoc 10 data) data))
                              )
                              (progn
                                (setq new-pb (list (+ (car pb) move-dist) (cadr pb) (caddr pb)))
                                (setq data (subst (cons 11 new-pb) (assoc 11 data) data))
                              )
                            )
                            (entmod data)
                            (entupd ent)
                          )
                        )
                      )
                    )
                  )
                )
                (setq i (1+ i))
              )
            )
          )
          
          (if (and ss-move (> (sslength ss-move) 0))
            (command "_.MOVE" ss-move "" (list 0 0 0) (list move-dist 0 0))
          )
          
          (if rectangle-texts
            (progn
              (setq text-count 0)
              (foreach rect-text rectangle-texts
                (setq rect-data (car rect-text))
                (setq texts (cadr rect-text))
                
                (setq v-left (nth 0 rect-data))
                (setq v-right (nth 1 rect-data))
                (setq h-top (nth 2 rect-data))
                (setq h-bottom (nth 3 rect-data))
                
                (setq new-center (calculate-rect-center v-left v-right h-top h-bottom))
                
                (if new-center
                  (foreach text-ent texts
                    (stretch-move-text-to-center text-ent new-center)
                    (setq text-count (1+ text-count))
                  )
                )
              )
              (princ (strcat "\nCentered " (itoa text-count) " text(s)."))
            )
          )
          
          (if increase
            (princ (strcat "\nSpacing increased by: " (rtos stretch-dist 2 2)))
            (princ (strcat "\nSpacing decreased by: " (rtos stretch-dist 2 2)))
          )
          
          (setq *ST_LAST_LINE1* (car ent1))
          (setq *ST_LAST_LINE2* (car ent2))
          
          (princ)
        )
      )
    )
  )
  (princ)
)

(defun find-rectangles-between-lines (left-line right-line y-min y-max / all-lines rectangles)
  (setq rectangles (list))
  (setq all-lines (ssget "_X" '((0 . "LINE"))))
  
  (if all-lines
    (progn
      (setq h-lines (list))
      (setq i 0)
      (while (< i (sslength all-lines))
        (setq ent (ssname all-lines i))
        (setq data (entget ent))
        (setq pa (cdr (assoc 10 data)))
        (setq pb (cdr (assoc 11 data)))
        
        (setq is-horizontal (< (abs (- (cadr pa) (cadr pb))) 0.01))
        (setq in-y-range (and (>= (cadr pa) (- y-min 0.01))
                             (<= (cadr pa) (+ y-max 0.01))))
        
        (if (and is-horizontal in-y-range)
          (setq h-lines (append h-lines (list ent)))
        )
        (setq i (1+ i))
      )
      
      (setq h-lines-sorted (vl-sort h-lines 
        '(lambda (a b) 
          (> (cadr (cdr (assoc 10 (entget a)))) 
             (cadr (cdr (assoc 10 (entget b))))))))
      
      (setq i 0)
      (while (< i (- (length h-lines-sorted) 1))
        (setq h-top (nth i h-lines-sorted))
        (setq h-bottom (nth (+ i 1) h-lines-sorted))
        
        (setq rect (list left-line right-line h-top h-bottom))
        (setq rectangles (append rectangles (list rect)))
        
        (setq i (1+ i))
      )
    )
  )
  rectangles
)

(defun find-text-in-rectangles (rectangles / all-texts rectangle-texts)
  (setq rectangle-texts (list))
  (setq all-texts (ssget "X" (list (cons 0 "TEXT,MTEXT"))))
  
  (if (and rectangles all-texts)
    (progn
      (setq i 0)
      (while (< i (sslength all-texts))
        (setq text-ent (ssname all-texts i))
        (setq text-data (entget text-ent))
        (setq text-pos (cdr (assoc 10 text-data)))
        
        (setq smallest-rect nil)
        (setq smallest-area 999999999.0)
        
        (foreach rect rectangles
          (setq bounds (calculate-rect-bounds-from-lines rect))
          
          (if bounds
            (progn
              (setq min-x (nth 0 bounds))
              (setq max-x (nth 1 bounds))
              (setq min-y (nth 2 bounds))
              (setq max-y (nth 3 bounds))
              
              (if (and (>= (car text-pos) min-x)
                      (<= (car text-pos) max-x)
                      (>= (cadr text-pos) min-y)
                      (<= (cadr text-pos) max-y))
                (progn
                  (setq area (* (- max-x min-x) (- max-y min-y)))
                  (if (< area smallest-area)
                    (progn
                      (setq smallest-area area)
                      (setq smallest-rect rect)
                    )
                  )
                )
              )
            )
          )
        )
        
        (if smallest-rect
          (progn
            (setq found nil)
            (setq new-list (list))
            (foreach rt rectangle-texts
              (if (equal (car rt) smallest-rect)
                (progn
                  (setq found T)
                  (setq new-list (append new-list (list (list (car rt) (append (cadr rt) (list text-ent))))))
                )
                (setq new-list (append new-list (list rt)))
              )
            )
            (if found
              (setq rectangle-texts new-list)
              (setq rectangle-texts (append rectangle-texts (list (list smallest-rect (list text-ent)))))
            )
          )
        )
        
        (setq i (1+ i))
      )
    )
  )
  rectangle-texts
)

(defun calculate-rect-bounds-from-lines (rect / v-left v-right h-top h-bottom)
  (setq v-left (nth 0 rect))
  (setq v-right (nth 1 rect))
  (setq h-top (nth 2 rect))
  (setq h-bottom (nth 3 rect))
  
  (setq left-data (entget v-left))
  (setq right-data (entget v-right))
  (setq top-data (entget h-top))
  (setq bottom-data (entget h-bottom))
  
  (setq left-x (car (cdr (assoc 10 left-data))))
  (setq right-x (car (cdr (assoc 10 right-data))))
  (setq top-y (cadr (cdr (assoc 10 top-data))))
  (setq bottom-y (cadr (cdr (assoc 10 bottom-data))))
  
  (list left-x right-x bottom-y top-y)
)

(defun calculate-rect-center (v-left v-right h-top h-bottom / left-data right-data top-data bottom-data)
  (setq left-data (entget v-left))
  (setq right-data (entget v-right))
  (setq top-data (entget h-top))
  (setq bottom-data (entget h-bottom))
  
  (setq left-p1 (cdr (assoc 10 left-data)))
  (setq left-p2 (cdr (assoc 11 left-data)))
  (setq right-p1 (cdr (assoc 10 right-data)))
  (setq right-p2 (cdr (assoc 11 right-data)))
  (setq top-p1 (cdr (assoc 10 top-data)))
  (setq top-p2 (cdr (assoc 11 top-data)))
  (setq bottom-p1 (cdr (assoc 10 bottom-data)))
  (setq bottom-p2 (cdr (assoc 11 bottom-data)))
  
  (setq corner-tl (inters left-p1 left-p2 top-p1 top-p2 nil))
  (setq corner-tr (inters right-p1 right-p2 top-p1 top-p2 nil))
  (setq corner-br (inters right-p1 right-p2 bottom-p1 bottom-p2 nil))
  (setq corner-bl (inters left-p1 left-p2 bottom-p1 bottom-p2 nil))
  
  (if (and corner-tl corner-tr corner-br corner-bl)
    (list (/ (+ (car corner-tl) (car corner-tr) (car corner-br) (car corner-bl)) 4.0)
          (/ (+ (cadr corner-tl) (cadr corner-tr) (cadr corner-br) (cadr corner-bl)) 4.0)
          0)
    nil
  )
)

(defun stretch-move-text-to-center (text-ent new-center / text-data text-type)
  (setq text-data (entget text-ent))
  (setq text-type (cdr (assoc 0 text-data)))
  
  (if (= text-type "TEXT")
    (progn
      (setq text-data (vl-remove (assoc 72 text-data) text-data))
      (setq text-data (vl-remove (assoc 73 text-data) text-data))
      (setq text-data (vl-remove (assoc 11 text-data) text-data))
      
      (setq text-data (append text-data (list (cons 72 1))))
      (setq text-data (append text-data (list (cons 73 2))))
      (setq text-data (append text-data (list (cons 11 new-center))))
      
      (setq text-data (subst (cons 10 new-center) (assoc 10 text-data) text-data))
    )
    (progn
      (setq text-data (vl-remove (assoc 71 text-data) text-data))
      (setq text-data (append text-data (list (cons 71 5))))
      (setq text-data (subst (cons 10 new-center) (assoc 10 text-data) text-data))
    )
  )
  
  (entmod text-data)
  (entupd text-ent)
)


;; ── STZ: 单线版拉伸间距 ──────────────────────────────────
;; Select one vertical line, move right entities, stretch crossing horizontals, copy line.
;; Prefix distance with a letter (e.g. "a20") to skip line copy.
(defun c:STZ (/ sel-line input-str skip-copy first-char num-str move-distance
               line-data p1 p2 line-x y-min y-max
               corner1 corner2 ss-move
               narrow-corner1 narrow-corner2 ss-intersect
               i ent data pa pb is-horizontal xa xb crosses new-pa new-pb
               new-line-data new-p1 new-p2)
  (princ "\nSelect a vertical line: ")
  (setq sel-line (entsel))
  
  (if (not sel-line)
    (progn (princ "\nNo line selected.") (princ))
    (progn
      (initget 6)
      (setq input-str (getstring "\nEnter move distance <20>: "))
      (setq skip-copy nil)
      
      (if (and input-str (> (strlen input-str) 0))
        (progn
          (setq first-char (substr input-str 1 1))
          (if (or (and (>= (ascii first-char) 65) (<= (ascii first-char) 90))
                  (and (>= (ascii first-char) 97) (<= (ascii first-char) 122)))
            (progn
              (setq skip-copy T)
              (setq num-str (substr input-str 2))
              (if (> (strlen num-str) 0)
                (setq move-distance (atof num-str))
                (setq move-distance 20)
              )
            )
            (setq move-distance (atof input-str))
          )
        )
        (setq move-distance 20)
      )
      
      (if (not move-distance)
        (setq move-distance 20)
      )
      
      (setq line-data (entget (car sel-line)))
      (setq p1 (cdr (assoc 10 line-data)))
      (setq p2 (cdr (assoc 11 line-data)))
      
      (setq line-x (car p1))
      (setq y-min (min (cadr p1) (cadr p2)))
      (setq y-max (max (cadr p1) (cadr p2)))
      
      ;; OP1: move all entities to the right (excluding selected line)
      (setq corner1 (list line-x y-min))
      (setq corner2 (list 999999 y-max))
      (setq ss-move (ssget "_W" corner1 corner2))
      
      (if (and ss-move (> (sslength ss-move) 0))
        (progn
          (ssdel (car sel-line) ss-move)
          (if (> (sslength ss-move) 0)
            (command "_.MOVE" ss-move "" (list 0 0 0) (list move-distance 0 0))
          )
        )
      )
      
      ;; OP2: stretch horizontal lines crossing the reference line
      (setq narrow-corner1 (list (- line-x 1) y-min))
      (setq narrow-corner2 (list (+ line-x 1) y-max))
      (setq ss-intersect (ssget "_C" narrow-corner1 narrow-corner2 '((0 . "LINE"))))
      
      (if ss-intersect
        (progn
          (setq i 0)
          (while (< i (sslength ss-intersect))
            (setq ent (ssname ss-intersect i))
            (if (not (eq ent (car sel-line)))
              (progn
                (setq data (entget ent))
                (setq pa (cdr (assoc 10 data)))
                (setq pb (cdr (assoc 11 data)))
                (setq is-horizontal (< (abs (- (cadr pa) (cadr pb))) 0.01))
                
                (if is-horizontal
                  (progn
                    (setq xa (car pa))
                    (setq xb (car pb))
                    (setq crosses (and (<= (min xa xb) (+ line-x 0.01))
                                      (>= (max xa xb) (- line-x 0.01))))
                    
                    (if crosses
                      (progn
                        ;; positive: stretch right endpoint; negative: stretch left endpoint
                        (if (>= move-distance 0)
                          (progn
                            (if (> xa xb)
                              (progn
                                (setq new-pa (list (+ (car pa) move-distance) (cadr pa) (caddr pa)))
                                (setq data (subst (cons 10 new-pa) (assoc 10 data) data))
                              )
                              (progn
                                (setq new-pb (list (+ (car pb) move-distance) (cadr pb) (caddr pb)))
                                (setq data (subst (cons 11 new-pb) (assoc 11 data) data))
                              )
                            )
                          )
                          (progn
                            (if (> xa xb)
                              (progn
                                (setq new-pa (list (+ (car pa) move-distance) (cadr pa) (caddr pa)))
                                (setq data (subst (cons 10 new-pa) (assoc 10 data) data))
                              )
                              (progn
                                (setq new-pb (list (+ (car pb) move-distance) (cadr pb) (caddr pb)))
                                (setq data (subst (cons 11 new-pb) (assoc 11 data) data))
                              )
                            )
                          )
                        )
                        (entmod data)
                        (entupd ent)
                      )
                    )
                  )
                )
              )
            )
            (setq i (1+ i))
          )
        )
      )
      
      ;; OP3: copy reference line and move to new position
      (if (not skip-copy)
        (progn
          (setq new-line-data (entget (car sel-line)))
          (setq new-p1 (list (+ (car p1) move-distance) (cadr p1) (caddr p1)))
          (setq new-p2 (list (+ (car p2) move-distance) (cadr p2) (caddr p2)))
          
          (setq new-line-data (subst (cons 10 new-p1) (assoc 10 new-line-data) new-line-data))
          (setq new-line-data (subst (cons 11 new-p2) (assoc 11 new-line-data) new-line-data))
          
          (entmake new-line-data)
          (if (>= move-distance 0)
            (princ "\nLine copied and moved right.")
            (princ "\nLine copied and moved left.")
          )
        )
      )
      
      (if (>= move-distance 0)
        (princ (strcat "\nMoved entities and stretched lines by: " (rtos move-distance 2 2)))
        (princ (strcat "\nMoved entities and stretched lines by: " (rtos move-distance 2 2)))
      )
      (if skip-copy
        (princ "\nSkipped line copy operation.")
      )
      (princ)
    )
  )
  (princ)
)


(if (not *ST_LAST_LINE1*) (setq *ST_LAST_LINE1* nil))
(if (not *ST_LAST_LINE2*) (setq *ST_LAST_LINE2* nil))
