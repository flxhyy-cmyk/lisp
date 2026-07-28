; ADV - 对齐多个矩形和图块
; 性能优化说明：
; - 混合选择策略：视图内使用快速窗口选择，视图外使用可靠全局选择
; - 列表构建优化：使用 cons 代替 append，提升列表操作性能
; - 直接对象操作：使用 vla-move 代替 command，减少命令行开销
; - 自动清理：对齐完成后自动删除临时生成的矩形描边
; 全局变量，用于记住上次的排序数据
; 结构：(list direction sorted-data first-rect)
(if (not *ADV_LAST_DATA*) (setq *ADV_LAST_DATA* nil))

; 全局变量，用于记录自动生成的矩形描边
; 结构：(list (rect-ent . block-ent) ...)
(if (not *ADV_AUTO_RECTS*) (setq *ADV_AUTO_RECTS* nil))

;@name 对齐多个矩形
;@group 图形编辑
;@desc 一次选择多个矩形或图块，按指定方向排列并调整间距，或执行对齐操作。F水平排列（顶对齐），V垂直排列（左对齐），A左对齐（以最上面矩形为基准），D右对齐（以最上面矩形为基准），W顶对齐（以最左边矩形为基准），S底对齐（以最左边矩形为基准）。自动为图块生成矩形描边，识别矩形内元素并整体移动
;@require Selection
;@require ModelSpace
(defun c:ADV ()
  (setq ss nil)
  (setq direction nil)
  (setq target-dist nil)
  (setq rects nil)
  (setq use-previous nil)
  
  ; 清空上次自动生成的矩形记录
  (setq *ADV_AUTO_RECTS* nil)
  
  (princ "\nSelect rectangles and blocks")
  (if *ADV_LAST_DATA*
    (princ " (Press Enter to use previous selection)")
  )
  (princ ": ")
  
  (setq ss (ssget))
  
  ; 如果没有选择且存在上次的数据，验证并使用上次的数据
  (if (and (not ss) *ADV_LAST_DATA*)
    (progn
      (if (validate-previous-data (cadr *ADV_LAST_DATA*))
        (progn
          (setq use-previous t)
          (princ "\nUsing previous selection with saved order.")
        )
        (progn
          (princ "\nPrevious selection is invalid (entities may have been deleted).")
          (princ "\nPlease select rectangles again.")
          (setq *ADV_LAST_DATA* nil)
          (princ)
          (exit)
        )
      )
    )
  )
  
  (if (and (not ss) (not use-previous))
    (progn (princ "\nNo entity selected.") (princ))
    (progn
      ; 如果不是使用上次数据，则重新过滤和准备
      (if (not use-previous)
        (progn
          (setq rects (filter-rectangles ss))
          
          (if (< (length rects) 2)
            (progn 
              (princ "\nAt least 2 rectangles/blocks required.") 
              (princ)
              (exit)
            )
          )
          
          ; 显示统计信息
          (if *ADV_AUTO_RECTS*
            (princ (strcat "\nGenerated " (itoa (length *ADV_AUTO_RECTS*)) " bounding rectangle(s) for block(s)."))
          )
          (princ (strcat "\nTotal " (itoa (length rects)) " rectangle(s) for alignment."))
        )
      )
      
      ; 获取用户输入
      (initget "F V A D W S")
      (setq direction (getkword "\nOptions [F(horizontal)/V(vertical)/A(left)/D(right)/W(top)/S(bottom)] <F>: "))
      (if (not direction)
        (setq direction "F")
      )
      
      ; 判断是排列还是对齐
      (setq is-alignment (member direction '("A" "D" "W" "S")))
      
      ; 如果是排列（F/V），需要输入距离
      (if (not is-alignment)
        (progn
          ; 计算默认距离：根据是否使用上一次数据选择不同的矩形列表
          (if use-previous
            (setq default-dist (calculate-default-distance-from-data (cadr *ADV_LAST_DATA*) direction))
            (setq default-dist (calculate-default-distance rects direction))
          )
          
          (initget 6)
          (setq target-dist (getdist (strcat "\nEnter target distance <" (rtos default-dist 2 2) ">: ")))
          (if (not target-dist)
            (setq target-dist default-dist)
          )
        )
      )
      
      ; 执行操作
      (if is-alignment
        (progn
          ; 执行对齐操作（不需要距离）
          (cond
            ((= direction "A") (align-left rects))
            ((= direction "D") (align-right rects))
            ((= direction "W") (align-top rects))
            ((= direction "S") (align-bottom rects))
          )
        )
        (progn
          ; 执行排列操作
          (if use-previous
            (progn
              ; 使用保存的数据直接对齐
              (if (= direction "F")
                (align-horizontal-with-data (cadr *ADV_LAST_DATA*) target-dist)
                (align-vertical-with-data (cadr *ADV_LAST_DATA*) (caddr *ADV_LAST_DATA*) target-dist)
              )
            )
            (progn
              ; 正常流程：准备数据并对齐
              (if (= direction "F")
                (align-horizontal rects target-dist)
                (align-vertical rects target-dist)
              )
            )
          )
        )
      )
      
      ; 对齐完成后，自动删除自动生成的矩形描边
      (cleanup-auto-rectangles)
    )
  )
  (princ)
)

;@hidden true
(defun calculate-default-distance (rects direction / first-rect bounds rect-width rect-height)
  ; 计算默认距离：使用第一个矩形的宽度或高度的1/10
  (if (and rects (> (length rects) 0))
    (progn
      (setq first-rect (car rects))
      (setq bounds (get-rectangle-bounds first-rect))
      
      (if bounds
        (progn
          (setq rect-width (abs (- (car (cadr bounds)) (car (car bounds)))))
          (setq rect-height (abs (- (cadr (cadr bounds)) (cadr (car bounds)))))
          
          ; 根据排列方向选择宽度或高度
          (if (= direction "F")
            (/ rect-width 10.0)   ; 水平排列使用宽度的1/10
            (/ rect-height 10.0)  ; 垂直排列使用高度的1/10
          )
        )
        25  ; 如果无法获取边界，返回默认值25
      )
    )
    25  ; 如果没有矩形，返回默认值25
  )
)

;@hidden true
(defun calculate-default-distance-from-data (sorted-data direction / first-data bounds rect-width rect-height)
  ; 从保存的数据中计算默认距离
  (if (and sorted-data (> (length sorted-data) 0))
    (progn
      (setq first-data (car sorted-data))
      (setq bounds (cadr first-data))  ; 从保存的数据中获取边界
      
      (if bounds
        (progn
          (setq rect-width (abs (- (car (cadr bounds)) (car (car bounds)))))
          (setq rect-height (abs (- (cadr (cadr bounds)) (cadr (car bounds)))))
          
          ; 根据排列方向选择宽度或高度
          (if (= direction "F")
            (/ rect-width 10.0)   ; 水平排列使用宽度的1/10
            (/ rect-height 10.0)  ; 垂直排列使用高度的1/10
          )
        )
        25  ; 如果无法获取边界，返回默认值25
      )
    )
    25  ; 如果没有数据，返回默认值25
  )
)

;@hidden true
(defun cleanup-auto-rectangles (/ deleted-count rect-ent)
  ; 自动清理生成的矩形描边
  (if (and *ADV_AUTO_RECTS* (> (length *ADV_AUTO_RECTS*) 0))
    (progn
      (setq deleted-count 0)
      (foreach pair *ADV_AUTO_RECTS*
        (setq rect-ent (car pair))
        (if (and rect-ent (entget rect-ent))
          (progn
            (entdel rect-ent)
            (setq deleted-count (1+ deleted-count))
          )
        )
      )
      (setq *ADV_AUTO_RECTS* nil)
      (princ (strcat "\nAuto-generated rectangle(s) cleaned up."))
    )
  )
)

;@hidden true
(defun filter-rectangles (ss / i ent etype rects-list vertex-count block-count rect-count new-rect)
  (setq rects-list nil)
  (setq i 0)
  (setq block-count 0)
  (setq rect-count 0)
  
  (if ss
    (while (< i (sslength ss))
      (setq ent (ssname ss i))
      (if ent
        (progn
          (setq etype (cdr (assoc 0 (entget ent))))
          (cond
            ; 处理多段线（矩形）
            ((or (= etype "LWPOLYLINE") (= etype "POLYLINE"))
              (progn
                ; 检查顶点数量，矩形应该有4个顶点（或5个顶点如果闭合）
                (setq vertex-count (count-vertices ent))
                (if (or (= vertex-count 4) (= vertex-count 5))
                  (progn
                    (setq rects-list (cons ent rects-list))  ; 使用 cons 代替 append
                    (setq rect-count (1+ rect-count))
                  )
                )
              )
            )
            ; 处理图块（包括属性块）
            ((= etype "INSERT")
              (progn
                ; 为图块创建矩形描边
                (setq new-rect (create-bounding-rectangle ent))
                (if new-rect
                  (progn
                    (setq rects-list (cons new-rect rects-list))  ; 使用 cons 代替 append
                    (setq block-count (1+ block-count))
                    ; 记录自动生成的矩形和对应的图块
                    (setq *ADV_AUTO_RECTS* (cons (cons new-rect ent) *ADV_AUTO_RECTS*))  ; 使用 cons 代替 append
                  )
                  (princ (strcat "\nWarning: Failed to create bounding rectangle for block at index " (itoa i)))
                )
              )
            )
          )
        )
      )
      (setq i (1+ i))
    )
  )
  
  (if (> block-count 0)
    (princ (strcat "\nFound " (itoa rect-count) " rectangle(s) and " (itoa block-count) " block(s)."))
    (princ (strcat "\nFound " (itoa rect-count) " rectangle(s)."))
  )
  
  (reverse rects-list)  ; 反转列表以保持原始顺序
)

;@hidden true
(defun count-vertices (ent / data count)
  (setq data (entget ent))
  (setq count 0)
  
  (foreach item data
    (if (= (car item) 10)
      (setq count (1+ count))
    )
  )
  
  count
)

;@hidden true
(defun create-bounding-rectangle (block-ent / bbox minpt maxpt p1 p2 p3 p4 rect-ent entdata)
  ; 为图块创建矩形描边
  ; 返回新创建的矩形实体名称
  (setq bbox (get-entity-bbox block-ent))
  
  (if bbox
    (progn
      (setq minpt (car bbox))
      (setq maxpt (cadr bbox))
      
      ; 构建矩形的四个顶点（逆时针）
      (setq p1 (list (car minpt) (cadr minpt) 0.0))      ; 左下
      (setq p2 (list (car maxpt) (cadr minpt) 0.0))      ; 右下
      (setq p3 (list (car maxpt) (cadr maxpt) 0.0))      ; 右上
      (setq p4 (list (car minpt) (cadr maxpt) 0.0))      ; 左上
      
      ; 使用 entmake 创建 LWPOLYLINE（更快，不依赖命令行）
      (setq entdata
        (list
          (cons 0 "LWPOLYLINE")
          (cons 100 "AcDbEntity")
          (cons 100 "AcDbPolyline")
          (cons 90 4)           ; 顶点数量
          (cons 70 1)           ; 闭合标志
          (cons 10 (list (car p1) (cadr p1)))
          (cons 10 (list (car p2) (cadr p2)))
          (cons 10 (list (car p3) (cadr p3)))
          (cons 10 (list (car p4) (cadr p4)))
          (cons 62 1)           ; 颜色：红色（便于识别自动生成的矩形）
        )
      )
      
      (if (entmake entdata)
        (progn
          (setq rect-ent (entlast))
          rect-ent
        )
        nil
      )
    )
    nil
  )
)

;@hidden true
(defun align-horizontal (rects target-dist / rect-data-list sorted-data first-rect top-y)
  (setq rect-data-list (prepare-rect-data rects))
  
  (if (not rect-data-list)
    (progn (princ "\nFailed to prepare rectangle data.") (princ))
    (progn
      ; 按X值排序，找到X最小的矩形作为第一个不动的矩形
      (setq sorted-data (sort-by-x rect-data-list))
      (setq first-rect (car sorted-data))
      
      ; 使用第一个矩形的顶部Y值作为对齐基准
      (setq top-y (cadr (cadr (cadr first-rect))))
      
      (execute-horizontal-alignment sorted-data target-dist top-y)
      
      ; 保存排序后的数据供下次使用
      ; 结构：(list direction sorted-data first-rect)
      (setq *ADV_LAST_DATA* (list "F" sorted-data first-rect))
      
      (princ (strcat "\nHorizontal alignment completed. Distance: " (rtos target-dist 2 2)))
    )
  )
)

;@hidden true
(defun align-horizontal-with-data (sorted-data target-dist / first-rect top-y updated-sorted-data i curr-data curr-ent new-bounds curr-ss)
  ; 使用已保存的排序数据直接对齐
  
  ; 重新获取 sorted-data 中所有矩形的当前边界
  (setq updated-sorted-data nil)
  (setq i 0)
  (while (< i (length sorted-data))
    (setq curr-data (nth i sorted-data))
    (setq curr-ent (car curr-data))
    (setq curr-ss (caddr curr-data))
    
    ; 重新获取当前边界
    (setq new-bounds (get-rectangle-bounds curr-ent))
    
    (if new-bounds
      (setq updated-sorted-data (cons (list curr-ent new-bounds curr-ss) updated-sorted-data))  ; 使用 cons
      (progn
        (princ (strcat "\nWarning: Failed to get bounds for entity at index " (itoa i)))
      )
    )
    
    (setq i (+ i 1))
  )
  
  (setq updated-sorted-data (reverse updated-sorted-data))  ; 反转以保持顺序
  
  (if updated-sorted-data
    (progn
      (setq first-rect (car updated-sorted-data))
      (setq top-y (cadr (cadr (cadr first-rect))))
      
      (execute-horizontal-alignment updated-sorted-data target-dist top-y)
      
      (princ (strcat "\nHorizontal alignment completed. Distance: " (rtos target-dist 2 2)))
    )
    (progn
      (princ "\nError: Failed to update bounds data.")
    )
  )
)

;@hidden true
(defun align-vertical (rects target-dist / rect-data-list sorted-by-x sorted-data first-rect left-x)
  (setq rect-data-list (prepare-rect-data rects))
  
  (if (not rect-data-list)
    (progn (princ "\nFailed to prepare rectangle data.") (princ))
    (progn
      ; 按X值排序，找到X最小的矩形作为第一个不动的矩形
      (setq sorted-by-x (sort-by-x rect-data-list))
      (setq first-rect (car sorted-by-x))
      
      ; 使用第一个矩形的左侧X值作为对齐基准
      (setq left-x (car (car (cadr first-rect))))
      
      ; 按Y值排序用于垂直排列
      (setq sorted-data (sort-by-y rect-data-list))
      
      (execute-vertical-alignment sorted-data target-dist left-x first-rect)
      
      ; 保存排序后的数据供下次使用
      ; 结构：(list direction sorted-data first-rect)
      (setq *ADV_LAST_DATA* (list "V" sorted-data first-rect))
      
      (princ (strcat "\nVertical alignment completed. Distance: " (rtos target-dist 2 2)))
    )
  )
)

;@hidden true
(defun align-vertical-with-data (sorted-data first-rect target-dist / left-x first-ent updated-bounds updated-sorted-data i curr-data curr-ent new-bounds curr-ss)
  ; 使用已保存的排序数据直接对齐
  ; sorted-data 已经是按Y排序的结果
  ; first-rect 是第一个不动的矩形
  
  ; 重新获取 first-rect 的当前边界（因为第一次执行后位置可能已改变）
  (setq first-ent (car first-rect))
  (setq updated-bounds (get-rectangle-bounds first-ent))
  
  (if (not updated-bounds)
    (progn
      (princ "\nError: Failed to get bounds for first rectangle.")
      (exit)
    )
  )
  
  (setq left-x (car (car updated-bounds)))
  
  ; 创建更新后的 first-rect 数据
  (setq first-rect (list first-ent updated-bounds (caddr first-rect)))
  
  ; 重新获取 sorted-data 中所有矩形的当前边界
  (setq updated-sorted-data nil)
  (setq i 0)
  (while (< i (length sorted-data))
    (setq curr-data (nth i sorted-data))
    (setq curr-ent (car curr-data))
    (setq curr-ss (caddr curr-data))
    
    ; 重新获取当前边界
    (setq new-bounds (get-rectangle-bounds curr-ent))
    
    (if new-bounds
      (setq updated-sorted-data (cons (list curr-ent new-bounds curr-ss) updated-sorted-data))  ; 使用 cons
      (progn
        (princ (strcat "\nWarning: Failed to get bounds for entity at index " (itoa i)))
      )
    )
    
    (setq i (+ i 1))
  )
  
  (setq updated-sorted-data (reverse updated-sorted-data))  ; 反转以保持顺序
  
  (if updated-sorted-data
    (progn
      (execute-vertical-alignment updated-sorted-data target-dist left-x first-rect)
      (princ (strcat "\nVertical alignment completed. Distance: " (rtos target-dist 2 2)))
    )
    (progn
      (princ "\nError: Failed to update bounds data.")
    )
  )
)

;@hidden true
(defun prepare-rect-data (rects / rect-data-list ent bounds ss-ent fast-count slow-count)
  (setq rect-data-list nil)
  (setq fast-count 0)
  (setq slow-count 0)
  
  (foreach ent rects
    (setq bounds (get-rectangle-bounds ent))
    (if bounds
      (progn
        ; 统计使用的方法
        (if (is-rect-in-view bounds)
          (setq fast-count (1+ fast-count))
          (setq slow-count (1+ slow-count))
        )
        
        (setq ss-ent (get-entities-in-rectangle ent bounds))
        (if ss-ent
          (setq rect-data-list (cons (list ent bounds ss-ent) rect-data-list))  ; 使用 cons 代替 append
          (princ (strcat "\nWarning: Failed to create selection set for entity"))
        )
      )
      (princ (strcat "\nWarning: Failed to get bounds for entity"))
    )
  )
  
  ; 显示性能统计
  (if (> slow-count 0)
    (princ (strcat "\nProcessing: " (itoa fast-count) " in view (fast), " (itoa slow-count) " out of view (reliable)."))
  )
  
  (reverse rect-data-list)  ; 反转列表以保持原始顺序
)

;@hidden true
(defun get-rectangle-bounds (ent / data minx miny maxx maxy pt)
  (setq data (entget ent))
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

;@hidden true
(defun is-rect-in-view (bounds / viewctr viewsize viewwidth viewheight minx miny maxx maxy view-minx view-miny view-maxx view-maxy)
  ; 检查矩形是否在当前视图范围内
  (setq viewctr (getvar "VIEWCTR"))
  (setq viewsize (getvar "VIEWSIZE"))
  (setq viewwidth (* viewsize (/ (car (getvar "SCREENSIZE")) (cadr (getvar "SCREENSIZE")))))
  (setq viewheight viewsize)
  
  ; 计算视图边界
  (setq view-minx (- (car viewctr) (/ viewwidth 2.0)))
  (setq view-maxx (+ (car viewctr) (/ viewwidth 2.0)))
  (setq view-miny (- (cadr viewctr) (/ viewheight 2.0)))
  (setq view-maxy (+ (cadr viewctr) (/ viewheight 2.0)))
  
  ; 计算矩形边界
  (setq minx (car (car bounds)))
  (setq miny (cadr (car bounds)))
  (setq maxx (car (cadr bounds)))
  (setq maxy (cadr (cadr bounds)))
  
  ; 判断矩形是否与视图相交
  (and
    (< minx view-maxx)
    (> maxx view-minx)
    (< miny view-maxy)
    (> maxy view-miny)
  )
)

;@hidden true
(defun get-entities-in-rectangle (rect-ent bounds / minx miny maxx maxy ss-result block-ent p1 p2 ss-temp use-fast-method i ent ss-all bbox)
  (setq minx (car (car bounds)))
  (setq miny (cadr (car bounds)))
  (setq maxx (car (cadr bounds)))
  (setq maxy (cadr (cadr bounds)))
  
  (setq ss-result (ssadd))
  (ssadd rect-ent ss-result)
  
  ; 检查这个矩形是否是自动生成的，如果是，添加对应的图块
  (if *ADV_AUTO_RECTS*
    (progn
      (foreach pair *ADV_AUTO_RECTS*
        (if (equal (car pair) rect-ent)
          (progn
            (setq block-ent (cdr pair))
            (if (and block-ent (entget block-ent))
              (ssadd block-ent ss-result)
            )
          )
        )
      )
    )
  )
  
  ; 判断使用哪种选择方法
  (setq use-fast-method (is-rect-in-view bounds))
  
  (if use-fast-method
    (progn
      ; 方法1：快速方法 - 使用窗口选择（适用于视图内的矩形）
      (setq p1 (list (- minx 0.01) (- miny 0.01)))
      (setq p2 (list (+ maxx 0.01) (+ maxy 0.01)))
      
      (setq ss-temp (ssget "_C" p1 p2))
      
      (if (and ss-temp (= (type ss-temp) 'PICKSET))
        (progn
          (setq i 0)
          (while (< i (sslength ss-temp))
            (setq ent (ssname ss-temp i))
            (if (and ent (not (is-auto-rect-ent ent)))
              (ssadd ent ss-result)
            )
            (setq i (+ i 1))
          )
        )
      )
    )
    (progn
      ; 方法2：可靠方法 - 使用全局选择（适用于视图外的矩形）
      (setq ss-all (ssget "_X" 
        (list 
          (cons -4 "<OR")
            (cons 0 "LINE")
            (cons 0 "CIRCLE")
            (cons 0 "ARC")
            (cons 0 "ELLIPSE")
            (cons 0 "LWPOLYLINE")
            (cons 0 "POLYLINE")
            (cons 0 "SPLINE")
            (cons 0 "TEXT")
            (cons 0 "MTEXT")
            (cons 0 "INSERT")
            (cons 0 "DIMENSION")
            (cons 0 "LEADER")
            (cons 0 "MLEADER")
            (cons 0 "HATCH")
            (cons 0 "SOLID")
            (cons 0 "REGION")
            (cons 0 "OLE2FRAME")
            (cons 0 "IMAGE")
          (cons -4 "OR>")
        )
      ))
      
      ; 精确检查每个实体的边界框是否在矩形范围内
      (if (and ss-all (= (type ss-all) 'PICKSET))
        (progn
          (setq i 0)
          (while (< i (sslength ss-all))
            (setq ent (ssname ss-all i))
            (if ent
              (progn
                (setq bbox (get-entity-bbox ent))
                (if (and bbox (bbox-intersects bbox bounds) (not (is-auto-rect-ent ent)))
                  (ssadd ent ss-result)
                )
              )
            )
            (setq i (+ i 1))
          )
        )
      )
    )
  )
  
  ss-result
)

;@hidden true
(defun is-auto-rect-ent (ent / result)
  ; 检查某个实体是否是有描边矩形的图块（即它作为一个图纸的外框，有自己的描边矩形）
  (setq result nil)
  (if (and *ADV_AUTO_RECTS* ent)
    (foreach pair *ADV_AUTO_RECTS*
      (if (equal (cdr pair) ent)
        (setq result t)
      )
    )
  )
  result
)

;@hidden true
(defun get-entity-bbox (ent / vla-obj minpt maxpt result)
  ; 获取实体的边界框（使用 Visual LISP 的 ActiveX 接口）
  ; 支持所有类型的实体，包括 OLE 对象
  (setq result nil)
  (if ent
    (progn
      (if (setq vla-obj (vlax-ename->vla-object ent))
        (progn
          (if (not (vl-catch-all-error-p 
                (vl-catch-all-apply 'vla-getBoundingBox 
                  (list vla-obj 'minpt 'maxpt))))
            (progn
              (if (and minpt maxpt)
                (setq result
                  (list 
                    (vlax-safearray->list minpt)
                    (vlax-safearray->list maxpt)
                  )
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

;@hidden true
(defun validate-previous-data (data / valid ent)
  ; 验证保存的数据是否仍然有效（实体是否存在）
  (setq valid t)
  (if (and data (listp data))
    (progn
      (foreach rect-data data
        (if valid
          (progn
            (setq ent (car rect-data))
            ; 检查实体是否仍然存在
            (if (not (and ent (entget ent)))
              (setq valid nil)
            )
          )
        )
      )
    )
    (setq valid nil)
  )
  valid
)

;@hidden true
(defun bbox-intersects (bbox1 bbox2 / min1x min1y max1x max1y min2x min2y max2x max2y)
  ; 判断两个边界框是否相交或包含
  (setq min1x (car (car bbox1)))
  (setq min1y (cadr (car bbox1)))
  (setq max1x (car (cadr bbox1)))
  (setq max1y (cadr (cadr bbox1)))
  
  (setq min2x (car (car bbox2)))
  (setq min2y (cadr (car bbox2)))
  (setq max2x (car (cadr bbox2)))
  (setq max2y (cadr (cadr bbox2)))
  
  ; 两个矩形相交的条件：
  ; bbox1的左边 <= bbox2的右边 AND bbox1的右边 >= bbox2的左边
  ; bbox1的下边 <= bbox2的上边 AND bbox1的上边 >= bbox2的下边
  (and
    (<= min1x max2x)
    (>= max1x min2x)
    (<= min1y max2y)
    (>= max1y min2y)
  )
)

;@hidden true
(defun sort-by-x (rect-data-list / sorted)
  ; 按X值升序排序，如果X值相同则按Y值降序排序（Y大的在前）
  (setq sorted (vl-sort rect-data-list 
    (function (lambda (a b) 
      (setq x-a (car (car (cadr a))))
      (setq x-b (car (car (cadr b))))
      (setq y-a (cadr (cadr (cadr a))))
      (setq y-b (cadr (cadr (cadr b))))
      (if (= x-a x-b)
        (> y-a y-b)  ; X相同时，Y大的在前
        (< x-a x-b)  ; X不同时，X小的在前
      )
    ))
  ))
  sorted
)

;@hidden true
(defun sort-by-y (rect-data-list / sorted)
  (setq sorted (vl-sort rect-data-list (function (lambda (a b) (> (cadr (cadr (cadr a))) (cadr (cadr (cadr b))))))))
  sorted
)

;@hidden true
(defun execute-horizontal-alignment (sorted-data target-dist top-y / i move-plans curr-data prev-bounds curr-bounds target-x move-x align-y curr-ss new-bounds ent vla-obj)
  (setq move-plans nil)
  (setq i 1)  ; 初始化 i，从第二个矩形开始（第一个不动）
  
  ; 第一个矩形（X最小）保持不动，从第二个开始排列
  ; prev-bounds 始终保持为前一个矩形移动后的位置
  (setq prev-bounds (cadr (car sorted-data)))
  
  (while (< i (length sorted-data))
    (setq curr-bounds (cadr (nth i sorted-data)))
    
    ; 计算目标位置：前一个矩形的右边界 + 目标间距
    (setq target-x (+ (car (cadr prev-bounds)) target-dist))
    (setq move-x (- target-x (car (car curr-bounds))))
    (setq align-y (- top-y (cadr (cadr curr-bounds))))
    
    (setq move-plans (cons (list i move-x align-y) move-plans))  ; 使用 cons 代替 append
    
    ; 计算当前矩形移动后的新边界，作为下一个矩形的参考
    (setq new-bounds (list 
      (list target-x (+ (cadr (car curr-bounds)) align-y))
      (list (+ target-x (- (car (cadr curr-bounds)) (car (car curr-bounds)))) (+ (cadr (cadr curr-bounds)) align-y))
    ))
    
    ; 更新prev-bounds为当前矩形移动后的位置
    (setq prev-bounds new-bounds)
    
    (setq i (+ i 1))
  )
  
  ; 反转 move-plans 以保持正确顺序
  (setq move-plans (reverse move-plans))
  
  ; 批量移动：使用 vla-move 代替 command，速度更快
  (foreach plan move-plans
    (setq curr-data (nth (car plan) sorted-data))
    (setq move-x (cadr plan))
    (setq align-y (caddr plan))
    (setq curr-ss (caddr curr-data))
    
    (if (and curr-ss (= (type curr-ss) 'PICKSET))
      (progn
        ; 使用 VLA 对象移动，比 command 快
        (setq i 0)
        (while (< i (sslength curr-ss))
          (setq ent (ssname curr-ss i))
          (if ent
            (progn
              (setq vla-obj (vlax-ename->vla-object ent))
              (if vla-obj
                (vla-move vla-obj 
                  (vlax-3d-point '(0 0 0))
                  (vlax-3d-point (list move-x align-y 0))
                )
              )
            )
          )
          (setq i (+ i 1))
        )
      )
      (princ (strcat "\nError: Invalid selection set at index " (itoa (car plan))))
    )
  )
)

;@hidden true
(defun execute-vertical-alignment (sorted-data target-dist left-x first-rect / i move-plans curr-data prev-bounds curr-bounds target-y move-y align-x curr-ss first-ent curr-ent rect-height rect-width ent vla-obj)
  (setq move-plans nil)
  (setq i 0)  ; 初始化 i
  (setq first-ent (car first-rect))
  
  ; 找到第一个不动的矩形在sorted-data中的位置，从它开始排列
  (setq prev-bounds (cadr first-rect))
  
  (while (< i (length sorted-data))
    (setq curr-data (nth i sorted-data))
    (setq curr-ent (car curr-data))
    (setq curr-bounds (cadr curr-data))
    
    ; 判断是否是第一个不动的矩形
    (if (equal curr-ent first-ent)
      (progn
        ; 是第一个不动的矩形，不移动，但更新 prev-bounds 为它的当前边界
        (setq prev-bounds (cadr first-rect))
      )
      (progn
        ; 不是第一个矩形，需要移动
        ; 计算当前矩形的高度和宽度
        (setq rect-height (- (cadr (cadr curr-bounds)) (cadr (car curr-bounds))))
        (setq rect-width (- (car (cadr curr-bounds)) (car (car curr-bounds))))
        
        ; 目标Y位置：前一个矩形的底部 - 目标间距 - 当前矩形高度
        (setq target-y (- (cadr (car prev-bounds)) target-dist rect-height))
        (setq move-y (- target-y (cadr (car curr-bounds))))
        (setq align-x (- left-x (car (car curr-bounds))))
        
        (setq move-plans (cons (list i move-y align-x) move-plans))  ; 使用 cons 代替 append
        
        ; 更新 prev-bounds 为当前矩形移动后的新位置
        ; 新的底部Y = target-y，新的顶部Y = target-y + rect-height
        (setq prev-bounds (list 
          (list left-x target-y)
          (list (+ left-x rect-width) (+ target-y rect-height))
        ))
      )
    )
    
    (setq i (+ i 1))
  )
  
  ; 反转 move-plans 以保持正确顺序
  (setq move-plans (reverse move-plans))
  
  ; 批量移动：使用 vla-move 代替 command，速度更快
  (foreach plan move-plans
    (setq curr-data (nth (car plan) sorted-data))
    (setq move-y (cadr plan))
    (setq align-x (caddr plan))
    (setq curr-ss (caddr curr-data))
    
    (if (and curr-ss (= (type curr-ss) 'PICKSET))
      (progn
        ; 使用 VLA 对象移动，比 command 快
        (setq i 0)
        (while (< i (sslength curr-ss))
          (setq ent (ssname curr-ss i))
          (if ent
            (progn
              (setq vla-obj (vlax-ename->vla-object ent))
              (if vla-obj
                (vla-move vla-obj 
                  (vlax-3d-point '(0 0 0))
                  (vlax-3d-point (list align-x move-y 0))
                )
              )
            )
          )
          (setq i (+ i 1))
        )
      )
      (princ (strcat "\nError: Invalid selection set at index " (itoa (car plan))))
    )
  )
)

;@hidden true
(defun align-left (rects / rect-data-list baseline rect-data curr-ent curr-bounds baseline-left move-x curr-ss ent vla-obj j)
  ; 左对齐：以最上面的矩形为基准，所有矩形左边缘对齐
  (setq rect-data-list (prepare-rect-data rects))
  
  (if (not rect-data-list)
    (progn (princ "\nFailed to prepare rectangle data.") (princ))
    (progn
      ; 找到最上面的矩形（Y值最大的）
      (setq baseline (car (vl-sort rect-data-list (function (lambda (a b) (> (cadr (cadr (cadr a))) (cadr (cadr (cadr b)))))))))
      (setq baseline-left (car (car (cadr baseline))))
      
      ; 执行移动
      (foreach rect-data rect-data-list
        (setq curr-ent (car rect-data))
        (setq curr-bounds (cadr rect-data))
        (setq curr-ss (caddr rect-data))
        
        ; 如果是基准矩形，不移动
        (if (not (equal curr-ent (car baseline)))
          (progn
            (setq move-x (- baseline-left (car (car curr-bounds))))
            
            ; 执行移动
            (if (and curr-ss (= (type curr-ss) 'PICKSET))
              (progn
                (setq j 0)
                (while (< j (sslength curr-ss))
                  (setq ent (ssname curr-ss j))
                  (if ent
                    (progn
                      (setq vla-obj (vlax-ename->vla-object ent))
                      (if vla-obj
                        (vla-move vla-obj 
                          (vlax-3d-point '(0 0 0))
                          (vlax-3d-point (list move-x 0 0))
                        )
                      )
                    )
                  )
                  (setq j (+ j 1))
                )
              )
            )
          )
        )
      )
      
      (princ "\nLeft alignment completed.")
    )
  )
  
  ; 对齐完成后，自动删除自动生成的矩形描边
  (cleanup-auto-rectangles)
)

;@hidden true
(defun align-right (rects / rect-data-list baseline rect-data curr-ent curr-bounds baseline-right move-x curr-ss ent vla-obj j)
  ; 右对齐：以最上面的矩形为基准，所有矩形右边缘对齐
  (setq rect-data-list (prepare-rect-data rects))
  
  (if (not rect-data-list)
    (progn (princ "\nFailed to prepare rectangle data.") (princ))
    (progn
      ; 找到最上面的矩形（Y值最大的）
      (setq baseline (car (vl-sort rect-data-list (function (lambda (a b) (> (cadr (cadr (cadr a))) (cadr (cadr (cadr b)))))))))
      (setq baseline-right (car (cadr (cadr baseline))))
      
      ; 执行移动
      (foreach rect-data rect-data-list
        (setq curr-ent (car rect-data))
        (setq curr-bounds (cadr rect-data))
        (setq curr-ss (caddr rect-data))
        
        ; 如果是基准矩形，不移动
        (if (not (equal curr-ent (car baseline)))
          (progn
            (setq move-x (- baseline-right (car (cadr curr-bounds))))
            
            ; 执行移动
            (if (and curr-ss (= (type curr-ss) 'PICKSET))
              (progn
                (setq j 0)
                (while (< j (sslength curr-ss))
                  (setq ent (ssname curr-ss j))
                  (if ent
                    (progn
                      (setq vla-obj (vlax-ename->vla-object ent))
                      (if vla-obj
                        (vla-move vla-obj 
                          (vlax-3d-point '(0 0 0))
                          (vlax-3d-point (list move-x 0 0))
                        )
                      )
                    )
                  )
                  (setq j (+ j 1))
                )
              )
            )
          )
        )
      )
      
      (princ "\nRight alignment completed.")
    )
  )
  
  ; 对齐完成后，自动删除自动生成的矩形描边
  (cleanup-auto-rectangles)
)

;@hidden true
(defun align-top (rects / rect-data-list baseline rect-data curr-ent curr-bounds baseline-top move-y curr-ss ent vla-obj j)
  ; 顶对齐：以最左边的矩形为基准，所有矩形顶部对齐
  (setq rect-data-list (prepare-rect-data rects))
  
  (if (not rect-data-list)
    (progn (princ "\nFailed to prepare rectangle data.") (princ))
    (progn
      ; 找到最左边的矩形（X值最小的）
      (setq baseline (car (vl-sort rect-data-list (function (lambda (a b) (< (car (car (cadr a))) (car (car (cadr b)))))))))
      (setq baseline-top (cadr (cadr (cadr baseline))))
      
      ; 执行移动
      (foreach rect-data rect-data-list
        (setq curr-ent (car rect-data))
        (setq curr-bounds (cadr rect-data))
        (setq curr-ss (caddr rect-data))
        
        ; 如果是基准矩形，不移动
        (if (not (equal curr-ent (car baseline)))
          (progn
            (setq move-y (- baseline-top (cadr (cadr curr-bounds))))
            
            ; 执行移动
            (if (and curr-ss (= (type curr-ss) 'PICKSET))
              (progn
                (setq j 0)
                (while (< j (sslength curr-ss))
                  (setq ent (ssname curr-ss j))
                  (if ent
                    (progn
                      (setq vla-obj (vlax-ename->vla-object ent))
                      (if vla-obj
                        (vla-move vla-obj 
                          (vlax-3d-point '(0 0 0))
                          (vlax-3d-point (list 0 move-y 0))
                        )
                      )
                    )
                  )
                  (setq j (+ j 1))
                )
              )
            )
          )
        )
      )
      
      (princ "\nTop alignment completed.")
    )
  )
  
  ; 对齐完成后，自动删除自动生成的矩形描边
  (cleanup-auto-rectangles)
)

;@hidden true
(defun align-bottom (rects / rect-data-list baseline rect-data curr-ent curr-bounds baseline-bottom move-y curr-ss ent vla-obj j)
  ; 底对齐：以最左边的矩形为基准，所有矩形底部对齐
  (setq rect-data-list (prepare-rect-data rects))
  
  (if (not rect-data-list)
    (progn (princ "\nFailed to prepare rectangle data.") (princ))
    (progn
      ; 找到最左边的矩形（X值最小的）
      (setq baseline (car (vl-sort rect-data-list (function (lambda (a b) (< (car (car (cadr a))) (car (car (cadr b)))))))))
      (setq baseline-bottom (cadr (car (cadr baseline))))
      
      ; 执行移动
      (foreach rect-data rect-data-list
        (setq curr-ent (car rect-data))
        (setq curr-bounds (cadr rect-data))
        (setq curr-ss (caddr rect-data))
        
        ; 如果是基准矩形，不移动
        (if (not (equal curr-ent (car baseline)))
          (progn
            (setq move-y (- baseline-bottom (cadr (car curr-bounds))))
            
            ; 执行移动
            (if (and curr-ss (= (type curr-ss) 'PICKSET))
              (progn
                (setq j 0)
                (while (< j (sslength curr-ss))
                  (setq ent (ssname curr-ss j))
                  (if ent
                    (progn
                      (setq vla-obj (vlax-ename->vla-object ent))
                      (if vla-obj
                        (vla-move vla-obj 
                          (vlax-3d-point '(0 0 0))
                          (vlax-3d-point (list 0 move-y 0))
                        )
                      )
                    )
                  )
                  (setq j (+ j 1))
                )
              )
            )
          )
        )
      )
      
      (princ "\nBottom alignment completed.")
    )
  )
  
  ; 对齐完成后，自动删除自动生成的矩形描边
  (cleanup-auto-rectangles)
)