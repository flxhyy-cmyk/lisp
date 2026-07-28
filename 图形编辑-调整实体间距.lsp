;@name 调整实体间距
;@group 图形编辑
;@desc 调整两个实体之间的正交距离为指定值（默认25），并且对齐。使用边界框精确计算距离
;@require Selection
;@require ModelSpace
(defun c:AD ()
  (setq ss1 nil ss2 nil bounds1 nil bounds2 nil center1 nil center2 nil dx nil dy nil 
        target-dist nil rect1 nil rect2 nil use-rect nil ss1-with-rect nil ss2-with-rect nil)
  
  (princ "\nSelect first entity (window selection): ")
  (setq ss1 (ssget))
  (if (or (not ss1) (= (sslength ss1) 0))
    (progn (princ "\nNo entity selected.") (princ))
    (progn
      ;; 获取第一个选择集的边界
      (setq bounds1 (get-selection-set-bounds ss1))
      (if (not bounds1)
        (progn (princ "\nCannot calculate first entity bounds.") (princ))
        (progn
          ;; 判断是否需要创建边界框
          (setq use-rect (has-geometry ss1))
          
          (if use-rect
            (progn
              ;; 创建边界框并加入选择集
              (setq rect1 (create-boundary-rectangle bounds1 1))
              (setq ss1-with-rect (ssadd))
              ;; 复制原选择集到新选择集
              (setq i 0)
              (while (< i (sslength ss1))
                (ssadd (ssname ss1 i) ss1-with-rect)
                (setq i (1+ i))
              )
              ;; 添加边界框
              (ssadd rect1 ss1-with-rect)
              (setq ss1 ss1-with-rect)
              (princ "\nFirst boundary rectangle created.")
            )
          )
          
          (princ "\nSelect second entity (window selection): ")
          (setq ss2 (ssget))
          (if (or (not ss2) (= (sslength ss2) 0))
            (progn (princ "\nNo entity selected.") (princ))
            (progn
              ;; 获取第二个选择集的边界
              (setq bounds2 (get-selection-set-bounds ss2))
              (if (not bounds2)
                (progn (princ "\nCannot calculate second entity bounds.") (princ))
                (progn
                  ;; 判断是否需要创建边界框
                  (if (or use-rect (has-geometry ss2))
                    (progn
                      (setq use-rect T)
                      ;; 创建边界框并加入选择集
                      (setq rect2 (create-boundary-rectangle bounds2 1))
                      (setq ss2-with-rect (ssadd))
                      ;; 复制原选择集到新选择集
                      (setq i 0)
                      (while (< i (sslength ss2))
                        (ssadd (ssname ss2 i) ss2-with-rect)
                        (setq i (1+ i))
                      )
                      ;; 添加边界框
                      (ssadd rect2 ss2-with-rect)
                      (setq ss2 ss2-with-rect)
                      (princ "\nSecond boundary rectangle created.")
                    )
                  )
                  
                  (initget 6)
                  (setq target-dist (getdist "\nEnter target distance <25>: "))
                  (if (not target-dist)
                    (setq target-dist 25)
                  )
                  
                  (if use-rect
                    ;; 使用边界框方法
                    (adjust-with-rectangles ss1 ss2 bounds1 bounds2 target-dist rect1 rect2)
                    ;; 使用原有方法（直接计算边界）
                    (adjust-direct ss1 ss2 bounds1 bounds2 target-dist)
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

;; 检查选择集是否包含实体几何（LINE, CIRCLE, ARC, POLYLINE等）
(defun has-geometry (ss / i ent etype has-geo)
  (setq has-geo nil)
  (setq i 0)
  (while (and (< i (sslength ss)) (not has-geo))
    (setq ent (ssname ss i))
    (setq etype (cdr (assoc 0 (entget ent))))
    (if (or (= etype "LINE") (= etype "CIRCLE") (= etype "ARC") 
            (= etype "LWPOLYLINE") (= etype "POLYLINE"))
      (setq has-geo T)
    )
    (setq i (1+ i))
  )
  has-geo
)

;; 使用边界框方法调整距离
(defun adjust-with-rectangles (ss1 ss2 bounds1 bounds2 target-dist rect1 rect2 / 
                                center1 center2 dx dy 
                                rect-bounds1 rect-bounds2 move-vec align-dist
                                minx1 miny1 maxx1 maxy1
                                minx2 miny2 maxx2 maxy2
                                ss2-original i ent
                                current-dist move-dist)
  ;; 获取边界框的边界
  (setq rect-bounds1 (get-rectangle-bounds-from-entity rect1))
  (setq rect-bounds2 (get-rectangle-bounds-from-entity rect2))
  
  ;; 计算中心点
  (setq center1 (list (/ (+ (car (car rect-bounds1)) (car (cadr rect-bounds1))) 2.0)
                      (/ (+ (cadr (car rect-bounds1)) (cadr (cadr rect-bounds1))) 2.0)
                      0))
  (setq center2 (list (/ (+ (car (car rect-bounds2)) (car (cadr rect-bounds2))) 2.0)
                      (/ (+ (cadr (car rect-bounds2)) (cadr (cadr rect-bounds2))) 2.0)
                      0))
  
  (setq dx (abs (- (car center2) (car center1))))
  (setq dy (abs (- (cadr center2) (cadr center1))))
  
  (setq minx1 (car (car rect-bounds1)))
  (setq miny1 (cadr (car rect-bounds1)))
  (setq maxx1 (car (cadr rect-bounds1)))
  (setq maxy1 (cadr (cadr rect-bounds1)))
  
  (setq minx2 (car (car rect-bounds2)))
  (setq miny2 (cadr (car rect-bounds2)))
  (setq maxx2 (car (cadr rect-bounds2)))
  (setq maxy2 (cadr (cadr rect-bounds2)))
  
  ;; 保存原始ss2（不含边界框）用于最后调整
  (setq ss2-original (ssadd))
  (setq i 0)
  (while (< i (sslength ss2))
    (setq ent (ssname ss2 i))
    (if (not (equal ent rect2))
      (ssadd ent ss2-original)
    )
    (setq i (1+ i))
  )
  
  (if (> dx dy)
    (progn
      ;; 水平方向调整
      ;; 第一步：计算当前边界框之间的距离
      (setq current-dist (- minx2 maxx1))
      (if (< (car center2) (car center1))
        (setq current-dist (- minx1 maxx2))
      )
      
      ;; 第二步：移动边界框距离到 target-dist - 2（考虑边界框偏移）
      (setq move-dist (- (- target-dist 2) current-dist))
      
      ;; 根据方向设置移动向量
      (if (< (car center2) (car center1))
        (setq move-vec (list (- move-dist) 0 0))
        (setq move-vec (list move-dist 0 0))
      )
      
      ;; 移动ss2（含边界框）
      (command "_.MOVE" ss2 "" '(0 0 0) move-vec)
      (command)
      
      ;; 第三步：顶部对齐
      (setq align-dist (- maxy1 maxy2))
      (command "_.MOVE" ss2 "" '(0 0 0) (list 0 align-dist 0))
      (command)
      
      ;; 第四步：删除边界框
      (command "_.ERASE" rect1 "")
      (command "_.ERASE" rect2 "")
      
      (princ "\nHorizontal spacing adjusted with boundary rectangles. Top aligned.")
    )
    (progn
      ;; 垂直方向调整
      ;; 第一步：计算当前边界框之间的距离
      (setq current-dist (- miny2 maxy1))
      (if (< (cadr center2) (cadr center1))
        (setq current-dist (- miny1 maxy2))
      )
      
      ;; 第二步：移动边界框距离到 target-dist - 2（考虑边界框偏移）
      (setq move-dist (- (- target-dist 2) current-dist))
      
      ;; 根据方向设置移动向量
      (if (< (cadr center2) (cadr center1))
        (setq move-vec (list 0 (- move-dist) 0))
        (setq move-vec (list 0 move-dist 0))
      )
      
      ;; 移动ss2（含边界框）
      (command "_.MOVE" ss2 "" '(0 0 0) move-vec)
      (command)
      
      ;; 第三步：右侧对齐
      (setq align-dist (- maxx1 maxx2))
      (command "_.MOVE" ss2 "" '(0 0 0) (list align-dist 0 0))
      (command)
      
      ;; 第四步：删除边界框
      (command "_.ERASE" rect1 "")
      (command "_.ERASE" rect2 "")
      
      (princ "\nVertical spacing adjusted with boundary rectangles. Right aligned.")
    )
  )
  
  (princ (strcat "\nDistance adjusted to: " (rtos target-dist 2 2)))
  (princ)
)

;; 直接计算边界调整距离（原有方法）
(defun adjust-direct (ss1 ss2 bounds1 bounds2 target-dist / 
                      center1 center2 dx dy current-dist move-dist move-vec align-dist)
  (setq center1 (list (/ (+ (car (car bounds1)) (car (cadr bounds1))) 2.0)
                      (/ (+ (cadr (car bounds1)) (cadr (cadr bounds1))) 2.0)
                      0))
  (setq center2 (list (/ (+ (car (car bounds2)) (car (cadr bounds2))) 2.0)
                      (/ (+ (cadr (car bounds2)) (cadr (cadr bounds2))) 2.0)
                      0))
  
  (setq dx (abs (- (car center2) (car center1))))
  (setq dy (abs (- (cadr center2) (cadr center1))))
  
  (if (> dx dy)
    (progn
      ;; 水平方向调整
      (setq current-dist (- (car (car bounds2)) (car (cadr bounds1))))
      (if (< (car center2) (car center1))
        (setq current-dist (- (car (car bounds1)) (car (cadr bounds2))))
      )
      (setq move-dist (- target-dist current-dist))
      (if (< (car center2) (car center1))
        (setq move-vec (list (- move-dist) 0 0))
        (setq move-vec (list move-dist 0 0))
      )
      (command "_.MOVE" ss2 "" '(0 0 0) move-vec)
      (command)
      
      ;; 顶部对齐
      (setq align-dist (- (cadr (cadr bounds1)) (cadr (cadr bounds2))))
      (command "_.MOVE" ss2 "" '(0 0 0) (list 0 align-dist 0))
      (command)
      (princ "\nHorizontal spacing adjusted. Top aligned.")
    )
    (progn
      ;; 垂直方向调整
      (setq current-dist (- (cadr (car bounds2)) (cadr (cadr bounds1))))
      (if (< (cadr center2) (cadr center1))
        (setq current-dist (- (cadr (car bounds1)) (cadr (cadr bounds2))))
      )
      (setq move-dist (- target-dist current-dist))
      (if (< (cadr center2) (cadr center1))
        (setq move-vec (list 0 (- move-dist) 0))
        (setq move-vec (list 0 move-dist 0))
      )
      (command "_.MOVE" ss2 "" '(0 0 0) move-vec)
      (command)
      
      ;; 右侧对齐
      (setq align-dist (- (car (cadr bounds1)) (car (cadr bounds2))))
      (command "_.MOVE" ss2 "" '(0 0 0) (list align-dist 0 0))
      (command)
      (princ "\nVertical spacing adjusted. Right aligned.")
    )
  )
  
  (princ (strcat "\nDistance adjusted to: " (rtos target-dist 2 2)))
  (princ)
)

;; 创建边界矩形（参考自动边界框.txt）
(defun create-boundary-rectangle (bounds offset / minx miny maxx maxy x1 y1 x2 y2 p1 p2 p3 p4 cmd ent1 ent2 ent3 ent4)
  (setq minx (car (car bounds)))
  (setq miny (cadr (car bounds)))
  (setq maxx (car (cadr bounds)))
  (setq maxy (cadr (cadr bounds)))
  
  (setq x1 (- minx offset))
  (setq y1 (- miny offset))
  (setq x2 (+ maxx offset))
  (setq y2 (+ maxy offset))
  
  (setq p1 (list x1 y1 0))
  (setq p2 (list x2 y1 0))
  (setq p3 (list x2 y2 0))
  (setq p4 (list x1 y2 0))
  
  (setq cmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  
  (entmake (list (cons 0 "LINE") (cons 10 p1) (cons 11 p2) (cons 62 1)))
  (setq ent1 (entlast))
  (entmake (list (cons 0 "LINE") (cons 10 p2) (cons 11 p3) (cons 62 1)))
  (setq ent2 (entlast))
  (entmake (list (cons 0 "LINE") (cons 10 p3) (cons 11 p4) (cons 62 1)))
  (setq ent3 (entlast))
  (entmake (list (cons 0 "LINE") (cons 10 p4) (cons 11 p1) (cons 62 1)))
  (setq ent4 (entlast))
  
  (command "_.PEDIT" ent1 "_Y" "_J" ent2 ent3 ent4 "" "")
  (command)
  
  (setvar "CMDECHO" cmd)
  
  (entlast)
)

;; 获取矩形实体的边界
(defun get-rectangle-bounds-from-entity (rect-ent / data minx miny maxx maxy pt)
  (setq data (entget rect-ent))
  (setq minx nil)
  (setq miny nil)
  (setq maxx nil)
  (setq maxy nil)
  
  (foreach item data
    (if (= (car item) 10)
      (progn
        (setq pt (cdr item))
        (if (or (not minx) (< (car pt) minx))
          (setq minx (car pt))
        )
        (if (or (not miny) (< (cadr pt) miny))
          (setq miny (cadr pt))
        )
        (if (or (not maxx) (> (car pt) maxx))
          (setq maxx (car pt))
        )
        (if (or (not maxy) (> (cadr pt) maxy))
          (setq maxy (cadr pt))
        )
      )
    )
  )
  
  (list (list minx miny) (list maxx maxy))
)

;; 获取矩形边界内的所有实体（参考调整距离_矩形框版.txt）
(defun get-entities-in-bounds (bounds / minx miny maxx maxy ss-result)
  (setq minx (car (car bounds)))
  (setq miny (cadr (car bounds)))
  (setq maxx (car (cadr bounds)))
  (setq maxy (cadr (cadr bounds)))
  
  ;; 使用窗口选择获取边界内的所有实体
  (setq ss-result (ssget "_W" (list minx miny) (list maxx maxy)))
  
  (if ss-result
    ss-result
    (ssadd)
  )
)

;; 获取选择集的整体边界
(defun get-selection-set-bounds (ss / i ent bounds minx miny maxx maxy)
  (if (and ss (> (sslength ss) 0))
    (progn
      (setq i 0)
      (setq minx nil)
      (setq miny nil)
      (setq maxx nil)
      (setq maxy nil)
      
      ;; 遍历选择集中的所有实体
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq bounds (get-entity-bounds-distance ent))
        
        (if bounds
          (progn
            ;; 更新最小最大边界
            (if (or (not minx) (< (car (car bounds)) minx))
              (setq minx (car (car bounds)))
            )
            (if (or (not miny) (< (cadr (car bounds)) miny))
              (setq miny (cadr (car bounds)))
            )
            (if (or (not maxx) (> (car (cadr bounds)) maxx))
              (setq maxx (car (cadr bounds)))
            )
            (if (or (not maxy) (> (cadr (cadr bounds)) maxy))
              (setq maxy (cadr (cadr bounds)))
            )
          )
        )
        (setq i (1+ i))
      )
      
      (if (and minx miny maxx maxy)
        (list (list minx miny) (list maxx maxy))
        nil
      )
    )
    nil
  )
)

(defun get-entity-bounds-distance (ent / data etype p1 p2 minx miny maxx maxy r)
  (setq data (entget ent))
  (setq etype (cdr (assoc 0 data)))
  
  (cond
    ((= etype "LINE")
     (setq p1 (cdr (assoc 10 data)))
     (setq p2 (cdr (assoc 11 data)))
     (setq minx (min (car p1) (car p2)))
     (setq miny (min (cadr p1) (cadr p2)))
     (setq maxx (max (car p1) (car p2)))
     (setq maxy (max (cadr p1) (cadr p2)))
     (list (list minx miny) (list maxx maxy))
    )
    ((= etype "CIRCLE")
     (setq p1 (cdr (assoc 10 data)))
     (setq r (cdr (assoc 40 data)))
     (setq minx (- (car p1) r))
     (setq miny (- (cadr p1) r))
     (setq maxx (+ (car p1) r))
     (setq maxy (+ (cadr p1) r))
     (list (list minx miny) (list maxx maxy))
    )
    ((= etype "ARC")
     (setq p1 (cdr (assoc 10 data)))
     (setq r (cdr (assoc 40 data)))
     (setq minx (- (car p1) r))
     (setq miny (- (cadr p1) r))
     (setq maxx (+ (car p1) r))
     (setq maxy (+ (cadr p1) r))
     (list (list minx miny) (list maxx maxy))
    )
    ((= etype "LWPOLYLINE")
     (get-polyline-bounds-distance data)
    )
    ((= etype "POLYLINE")
     (get-polyline-bounds-distance data)
    )
    (T
     nil
    )
  )
)

(defun get-polyline-bounds-distance (data / minx miny maxx maxy pt)
  (setq minx nil)
  (setq miny nil)
  (setq maxx nil)
  (setq maxy nil)
  
  (foreach item data
    (if (= (car item) 10)
      (progn
        (setq pt (cdr item))
        (if (or (not minx) (< (car pt) minx))
          (setq minx (car pt))
        )
        (if (or (not miny) (< (cadr pt) miny))
          (setq miny (cadr pt))
        )
        (if (or (not maxx) (> (car pt) maxx))
          (setq maxx (car pt))
        )
        (if (or (not maxy) (> (cadr pt) maxy))
          (setq maxy (cadr pt))
        )
      )
    )
  )
  
  (if (and minx miny maxx maxy)
    (list (list minx miny) (list maxx maxy))
    nil
  )
)
