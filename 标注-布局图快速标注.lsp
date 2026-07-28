;@name 智能矩形标注
;@group 标注工具
;@desc 选择矩形后自动标注宽度和高度，标注值乘以10显示，精度2位小数但.00不显示，智能调整使标注规整
;@require ModelSpace
;@order 1
(defun c:WBB ()
  (setq old-cmdecho (getvar "CMDECHO"))
  (setq old-dimstyle (getvar "DIMSTYLE"))
  (setvar "CMDECHO" 0)
  
  ;; 设置STANDARD为当前标注样式
  (command "_.DIMSTYLE" "_R" "STANDARD")
  
  ;; 提示用户选择矩形
  (princ "\n请选择矩形（可多选）: ")
  (setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE"))))
  
  (if ss
    (progn
      ;; 清除原有标注
      (clear-existing-dimensions-wbb ss)
      
      ;; 收集所有矩形信息
      (setq rect-list '())
      (setq i 0)
      (repeat (sslength ss)
        (setq rect-ent (ssname ss i))
        (setq rect-bounds (get-rectangle-bounds-wbb rect-ent))
        
        (if rect-bounds
          (progn
            (setq minx (car (car rect-bounds)))
            (setq miny (cadr (car rect-bounds)))
            (setq maxx (car (cadr rect-bounds)))
            (setq maxy (cadr (cadr rect-bounds)))
            (setq width (- maxx minx))
            (setq height (- maxy miny))
            
            ;; 保存矩形信息：(minx miny maxx maxy width height)
            (setq rect-list (cons (list minx miny maxx maxy width height) rect-list))
          )
        )
        (setq i (1+ i))
      )
      
      ;; 按高度分组（同高矩形竖直标注只保留最左边一个）
      (setq height-groups (group-by-height-wbb rect-list))
      
      ;; 按宽度分组（同宽矩形水平标注只保留最下方一个）
      (setq width-groups (group-by-width-wbb rect-list))
      
      ;; 预先计算哪些矩形是"外框"（包含其他矩形）
      ;; 外框的标注放在外部，被包裹的小矩形标注放在内部
      (setq outer-rects '())
      (foreach rect-info rect-list
        (setq c-minx (nth 0 rect-info))
        (setq c-miny (nth 1 rect-info))
        (setq c-maxx (nth 2 rect-info))
        (setq c-maxy (nth 3 rect-info))
        (setq has-inner nil)
        (foreach other rect-list
          (if (not (equal other rect-info))
            (if (and (> (nth 0 other) c-minx) (< (nth 2 other) c-maxx)
                     (> (nth 1 other) c-miny) (< (nth 3 other) c-maxy))
              ;; other 完全在 rect-info 内部 → rect-info 是外框
              (setq has-inner T)
            )
          )
        )
        (if has-inner
          (setq outer-rects (cons (nth 0 rect-info) outer-rects))  ;; 用minx作为标识
        )
      )

      ;; 标注所有矩形
      (foreach rect-info rect-list
        (setq minx (nth 0 rect-info))
        (setq miny (nth 1 rect-info))
        (setq maxx (nth 2 rect-info))
        (setq maxy (nth 3 rect-info))
        (setq width (nth 4 rect-info))
        (setq height (nth 5 rect-info))

        ;; 判断此矩形是否是外框（包含了其他矩形）
        (setq is-outer-rect (member minx outer-rects))
        
        ;; 定义四个角点（使用原始边界）
        (setq p1 (list minx miny 0))
        (setq p2 (list maxx miny 0))
        (setq p3 (list maxx maxy 0))
        (setq p4 (list minx maxy 0))
        
        ;; 计算缩放因子
        (setq scale-factor (/ (+ width height) 2.0 1000.0))
        
        ;; 配置标注样式参数
        (setup-wbb-dimstyle scale-factor)
        
        ;; 计算标注偏移距离
        (setq text-height (getvar "DIMTXT"))
        (setq h-offset-dist (* text-height 2.0))
        (setq v-offset-dist (* text-height 1.5))
        
        ;; 添加水平标注（只标注最下方的）
        (if (is-bottommost-in-group-wbb miny width width-groups)
          (if is-outer-rect
            ;; 外框矩形：标注放在外部（底部外侧），定义点用底部两角
            (progn
              (setq h-dim-y (- miny h-offset-dist))
              (add-dimension-wbb p1 p2 (list (/ (+ minx maxx) 2.0) h-dim-y 0) T)
            )
            ;; 被包裹的内部矩形：标注放到矩形内部（顶部内侧）
            ;; 定义点改用顶部两角(p4,p3)，避免尺寸界线从底部拉到顶部
            (progn
              (setq h-dim-y (- maxy h-offset-dist))
              (add-dimension-wbb p4 p3 (list (/ (+ minx maxx) 2.0) h-dim-y 0) T)
            )
          )
        )
        
        ;; 检查是否需要添加竖直标注（只标注最左边的）
        (if (is-leftmost-in-group-wbb minx height height-groups)
          (if is-outer-rect
            ;; 外框矩形：竖直标注放在外部（左侧外侧），定义点用左侧两角
            (progn
              (setq v-dim-x (- minx v-offset-dist))
              (add-dimension-wbb p1 p4 (list v-dim-x (/ (+ miny maxy) 2.0) 0) nil)
            )
            ;; 被包裹的内部矩形：竖直标注放到内部（右侧内侧）
            ;; 定义点改用右侧两角(p2,p3)，避免尺寸界线从左拉到右
            (progn
              (setq v-dim-x (- maxx v-offset-dist))
              (add-dimension-wbb p2 p3 (list v-dim-x (/ (+ miny maxy) 2.0) 0) nil)
            )
          )
        )
      )
      
      ;; 标注矩形之间的间距
      (annotate-gaps-wbb rect-list)
      
      (princ (strcat "\n已标注 " (itoa (length rect-list)) " 个矩形。"))
    )
    (princ "\n未选择对象。")
  )
  
  ;; 恢复设置
  (setvar "CMDECHO" old-cmdecho)
  (princ)
)

;; 按高度分组矩形（同高、Y范围重叠、X方向相邻的才归为一组）
;; 组结构：(height group-minx group-miny group-maxy group-maxx)
;; 修复：foreach 中 setq 循环变量不修改原列表，改为重建列表
(defun group-by-height-wbb (rect-list /
       height-groups height found minx miny maxx maxy width
       x-gap matched-group rest-groups g)
  (setq height-groups '())

  (foreach rect rect-list
    (setq height (nth 5 rect))
    (setq minx (nth 0 rect))
    (setq miny (nth 1 rect))
    (setq maxx (nth 2 rect))
    (setq maxy (nth 3 rect))
    (setq width (nth 4 rect))

    (setq found nil)
    (setq matched-group nil)
    (setq rest-groups '())

    (foreach g height-groups
      (if (and (not found)
               (< (abs (- (car g) height)) 0.1)
               ;; Y范围必须重叠，才属于同一水平行
               (< (max miny (nth 2 g)) (min maxy (nth 3 g)))
               ;; X方向必须相邻（间隙小于宽度的一半）
               (progn
                 (if (>= minx (nth 4 g))
                   (setq x-gap (- minx (nth 4 g)))
                   (if (<= maxx (nth 1 g))
                     (setq x-gap (- (nth 1 g) maxx))
                     (setq x-gap 0)))
                 (< x-gap (/ width 2.0))))
        ;; 匹配成功，合并到组
        (progn
          (setq found T)
          (setq matched-group
            (list (car g)
                  (min (nth 1 g) minx)
                  (min (nth 2 g) miny)
                  (max (nth 3 g) maxy)
                  (max (nth 4 g) maxx))))
        ;; 不匹配，保留到 rest-groups
        (setq rest-groups (cons g rest-groups))
      )
    )

    (if found
      (setq height-groups (cons matched-group rest-groups))
      (setq height-groups (cons (list height minx miny maxy maxx) height-groups))
    )
  )

  height-groups
)

;; 按宽度分组矩形（同宽、X范围重叠、Y方向相邻的才归为一组）
;; 组结构：(width group-miny group-minx group-maxx group-maxy)
;; 修复：foreach 中 setq 循环变量不修改原列表，改为重建列表
(defun group-by-width-wbb (rect-list /
       width-groups width found minx miny maxx maxy height
       y-gap matched-group rest-groups g)
  (setq width-groups '())

  (foreach rect rect-list
    (setq width (nth 4 rect))
    (setq minx (nth 0 rect))
    (setq miny (nth 1 rect))
    (setq maxx (nth 2 rect))
    (setq maxy (nth 3 rect))
    (setq height (nth 5 rect))

    (setq found nil)
    (setq matched-group nil)
    (setq rest-groups '())

    (foreach g width-groups
      (if (and (not found)
               (< (abs (- (car g) width)) 0.1)
               ;; X范围必须重叠，才属于同一竖直列
               (< (max minx (nth 2 g)) (min maxx (nth 3 g)))
               ;; Y方向必须相邻（间隙小于高度的一半）
               (progn
                 (if (>= miny (nth 4 g))
                   (setq y-gap (- miny (nth 4 g)))
                   (if (<= maxy (nth 1 g))
                     (setq y-gap (- (nth 1 g) maxy))
                     (setq y-gap 0)))
                 (< y-gap (/ height 2.0))))
        ;; 匹配成功，合并到组
        (progn
          (setq found T)
          (setq matched-group
            (list (car g)
                  (min (nth 1 g) miny)
                  (min (nth 2 g) minx)
                  (max (nth 3 g) maxx)
                  (max (nth 4 g) maxy))))
        ;; 不匹配，保留到 rest-groups
        (setq rest-groups (cons g rest-groups))
      )
    )

    (if found
      (setq width-groups (cons matched-group rest-groups))
      (setq width-groups (cons (list width miny minx maxx maxy) width-groups))
    )
  )

  width-groups
)

;; 判断是否是该高度组中最左边的矩形
(defun is-leftmost-in-group-wbb (minx height height-groups / found)
  (setq found nil)
  
  (foreach group height-groups
    (if (< (abs (- (car group) height)) 0.1)
      (if (< (abs (- (cadr group) minx)) 0.1)
        (setq found T)
      )
    )
  )
  
  found
)

;; 判断是否是该宽度组中最下方的矩形
(defun is-bottommost-in-group-wbb (miny width width-groups / found)
  (setq found nil)
  
  (foreach group width-groups
    (if (< (abs (- (car group) width)) 0.1)
      (if (< (abs (- (cadr group) miny)) 0.1)
        (setq found T)
      )
    )
  )
  
  found
)

;; 标注矩形之间的间距
(defun annotate-gaps-wbb (rect-list / rect1 rect2 minx1 miny1 maxx1 maxy1 minx2 miny2 maxx2 maxy2
                                   inner-rects annotated-pairs min-gap gap-info has-intermediate
                                   annotated-gap-values gap-key skipped-gap-values
                                   used-horizontal-ys used-vertical-xs overlap-threshold
                                   returned-y returned-x)
  (setq annotated-pairs '())  ;; 记录已标注的矩形对
  (setq annotated-gap-values '())  ;; 记录已标注的（方向+距离）组合
  (setq skipped-gap-values '())    ;; 记录因中间矩形被跳过的总间隙（方向+距离），用于推导性冗余去重
  (setq used-horizontal-ys '())  ;; 已使用的水平标注Y位置
  (setq used-vertical-xs '())    ;; 已使用的垂直标注X位置
  (setq overlap-threshold 30)      ;; 标注重叠检测阈值
  
  ;; 遍历所有矩形对
  (foreach rect1 rect-list
    (setq minx1 (nth 0 rect1))
    (setq miny1 (nth 1 rect1))
    (setq maxx1 (nth 2 rect1))
    (setq maxy1 (nth 3 rect1))
    
    ;; 收集在rect1内部的所有矩形
    (setq inner-rects '())
    (foreach rect2 rect-list
      (if (not (equal rect1 rect2))
        (progn
          (setq minx2 (nth 0 rect2))
          (setq miny2 (nth 1 rect2))
          (setq maxx2 (nth 2 rect2))
          (setq maxy2 (nth 3 rect2))
          
          ;; 检查rect2是否在rect1内部
          (if (and (>= minx2 minx1) (<= maxx2 maxx1)
                   (>= miny2 miny1) (<= maxy2 maxy1)
                   (or (> minx2 minx1) (< maxx2 maxx1) (> miny2 miny1) (< maxy2 maxy1)))
            (setq inner-rects (cons rect2 inner-rects))
          )
        )
      )
    )
    
    ;; 如果有内部矩形，标注最靠近边界的矩形
    (if inner-rects
      (annotate-nearest-to-edges-wbb minx1 miny1 maxx1 maxy1 inner-rects)
    )
    
    ;; 标注相邻矩形的最小间隙（排除有中间矩形的情况）
    (foreach rect2 rect-list
      (if (and (not (equal rect1 rect2))
               (not (is-pair-annotated-wbb rect1 rect2 annotated-pairs)))
        (progn
          (setq minx2 (nth 0 rect2))
          (setq miny2 (nth 1 rect2))
          (setq maxx2 (nth 2 rect2))
          (setq maxy2 (nth 3 rect2))
          
          ;; 计算两个矩形之间的最小间隙
          (setq min-gap (find-minimum-gap-wbb minx1 miny1 maxx1 maxy1 minx2 miny2 maxx2 maxy2))
          
          ;; 检查是否有中间矩形
          (setq has-intermediate (has-intermediate-rect-wbb rect1 rect2 rect-list))
          
          ;; 如果有有效间隙（大于0.1）
          (if (and min-gap (> (car min-gap) 0.1))
            (progn
              (setq gap-info (cdr min-gap))
              (setq gap-key (create-gap-key-wbb rect1 rect2 gap-info (car min-gap)))

              (if has-intermediate
                ;; 有中间矩形：不标注，但记录被跳过的总间隙值
                ;; （供后续推导性冗余去重：若其他无遮挡矩形对的间隙与此总间隙一致，也不标）
                (setq skipped-gap-values (cons gap-key skipped-gap-values))
                ;; 无中间矩形：检查是否与已标注间隙或被跳过的总间隙重复
                (if (and (not (is-gap-value-annotated-wbb gap-key annotated-gap-values))
                         (not (is-gap-value-annotated-wbb gap-key skipped-gap-values)))
                  (progn
                    ;; gap-info: (type x1 y1 x2 y2 overlap-min overlap-max)
                    ;; type: "horizontal" 或 "vertical"
                    (if (equal (car gap-info) "horizontal")
                      (progn
                        (setq returned-y (annotate-horizontal-gap-wbb
                          (nth 1 gap-info) (nth 3 gap-info)
                          (nth 5 gap-info) (nth 6 gap-info)
                          (car min-gap)
                          used-horizontal-ys
                          overlap-threshold))
                        (setq used-horizontal-ys (cons returned-y used-horizontal-ys))
                      )
                      (progn
                        (setq returned-x (annotate-vertical-gap-wbb
                          (nth 2 gap-info) (nth 4 gap-info)
                          (nth 5 gap-info) (nth 6 gap-info)
                          (car min-gap)
                          used-vertical-xs
                          overlap-threshold))
                        (setq used-vertical-xs (cons returned-x used-vertical-xs))
                      )
                    )
                    ;; 记录已标注的矩形对和间隙值
                    (setq annotated-pairs (cons (list rect1 rect2) annotated-pairs))
                    (setq annotated-gap-values (cons gap-key annotated-gap-values))
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

;; 创建间隙键值（用于检测相同间隙）
;; 键值包含位置信息，避免不同位置的相同距离被误去重
;; 返回：(方向类型 距离值 位置1 位置2)
(defun create-gap-key-wbb (rect1 rect2 gap-info gap-distance
                            / gap-type rounded-distance pos1 pos2)
  (setq gap-type (car gap-info))
  (setq rounded-distance (fix (+ gap-distance 0.5)))
  ;; 根据方向取位置坐标
  (if (equal gap-type "horizontal")
    (progn
      (setq pos1 (nth 1 gap-info))  ;; 左矩形右边x
      (setq pos2 (nth 3 gap-info))  ;; 右矩形左边x
    )
    (progn
      (setq pos1 (nth 2 gap-info))  ;; 下矩形上边y
      (setq pos2 (nth 4 gap-info))  ;; 上矩形下边y
    )
  )
  (list gap-type rounded-distance pos1 pos2)
)

;; 检查相同方向、距离、位置的间隙是否已被标注
;; 键值格式：(方向类型 四舍五入距离 位置1 位置2)
(defun is-gap-value-annotated-wbb (gap-key annotated-list / found)
  (setq found nil)
  (foreach annotated-gap annotated-list
    (if (and (equal (car annotated-gap) (car gap-key))
             (< (abs (- (cadr annotated-gap) (cadr gap-key))) 1)
             (< (abs (- (nth 2 annotated-gap) (nth 2 gap-key))) 0.1)
             (< (abs (- (nth 3 annotated-gap) (nth 3 gap-key))) 0.1))
      (setq found T)
    )
  )
  found
)

;; 检查两个矩形之间是否有中间矩形
(defun has-intermediate-rect-wbb (rect1 rect2 rect-list 
                                   / minx1 miny1 maxx1 maxy1 minx2 miny2 maxx2 maxy2
                                   rect3 minx3 miny3 maxx3 maxy3
                                   overlap-v-min overlap-v-max overlap-h-min overlap-h-max
                                   is-horizontal is-vertical has-intermediate)
  (setq minx1 (nth 0 rect1))
  (setq miny1 (nth 1 rect1))
  (setq maxx1 (nth 2 rect1))
  (setq maxy1 (nth 3 rect1))
  (setq minx2 (nth 0 rect2))
  (setq miny2 (nth 1 rect2))
  (setq maxx2 (nth 2 rect2))
  (setq maxy2 (nth 3 rect2))
  
  (setq has-intermediate nil)
  
  ;; 计算rect1和rect2的重叠范围
  (setq overlap-v-min (max miny1 miny2))
  (setq overlap-v-max (min maxy1 maxy2))
  (setq overlap-h-min (max minx1 minx2))
  (setq overlap-h-max (min maxx1 maxx2))
  
  ;; 判断rect1和rect2的相对位置
  (setq is-horizontal (< overlap-v-min overlap-v-max))  ;; 垂直方向有重叠，水平排列
  (setq is-vertical (< overlap-h-min overlap-h-max))    ;; 水平方向有重叠，垂直排列
  
  ;; 检查是否有中间矩形
  (foreach rect3 rect-list
    (if (and (not (equal rect3 rect1)) (not (equal rect3 rect2)))
      (progn
        (setq minx3 (nth 0 rect3))
        (setq miny3 (nth 1 rect3))
        (setq maxx3 (nth 2 rect3))
        (setq maxy3 (nth 3 rect3))
        
        ;; 如果是水平排列（左右关系）
        (if is-horizontal
          (progn
            ;; 检查rect3是否在rect1和rect2之间
            ;; rect3的垂直范围要与rect1和rect2有重叠
            (if (and (< (max miny1 miny2 miny3) (min maxy1 maxy2 maxy3))
                     ;; rect3在rect1和rect2的水平范围之间
                     (or (and (< maxx1 minx2) (>= minx3 maxx1) (<= maxx3 minx2))
                         (and (< maxx2 minx1) (>= minx3 maxx2) (<= maxx3 minx1))))
              (setq has-intermediate T)
            )
          )
        )
        
        ;; 如果是垂直排列（上下关系）
        (if is-vertical
          (progn
            ;; 检查rect3是否在rect1和rect2之间
            ;; rect3的水平范围要与rect1和rect2有重叠
            (if (and (< (max minx1 minx2 minx3) (min maxx1 maxx2 maxx3))
                     ;; rect3在rect1和rect2的垂直范围之间
                     (or (and (< maxy1 miny2) (>= miny3 maxy1) (<= maxy3 miny2))
                         (and (< maxy2 miny1) (>= miny3 maxy2) (<= maxy3 miny1))))
              (setq has-intermediate T)
            )
          )
        )
      )
    )
  )
  
  has-intermediate
)

;; 检查矩形对是否已标注
(defun is-pair-annotated-wbb (rect1 rect2 annotated-pairs / found)
  (setq found nil)
  (foreach pair annotated-pairs
    (if (or (and (equal (car pair) rect1) (equal (cadr pair) rect2))
            (and (equal (car pair) rect2) (equal (cadr pair) rect1)))
      (setq found T)
    )
  )
  found
)

;; 找出两个矩形之间的最小间隙
(defun find-minimum-gap-wbb (minx1 miny1 maxx1 maxy1 minx2 miny2 maxx2 maxy2 
                              / h-gap-left h-gap-right v-gap-bottom v-gap-top
                              overlap-h-min overlap-h-max overlap-v-min overlap-v-max
                              min-gap gap-type gap-info)
  
  (setq min-gap nil)
  
  ;; 计算水平和垂直方向的重叠范围
  (setq overlap-v-min (max miny1 miny2))
  (setq overlap-v-max (min maxy1 maxy2))
  (setq overlap-h-min (max minx1 minx2))
  (setq overlap-h-max (min maxx1 maxx2))
  
  ;; 检查水平间隙（左右方向）
  ;; rect1在rect2左边
  (if (and (< maxx1 minx2) (< overlap-v-min overlap-v-max))
    (progn
      (setq h-gap-left (- minx2 maxx1))
      (if (or (not min-gap) (< h-gap-left (car min-gap)))
        (setq min-gap (cons h-gap-left 
                           (list "horizontal" maxx1 0 minx2 0 overlap-v-min overlap-v-max)))
      )
    )
  )
  
  ;; rect1在rect2右边
  (if (and (< maxx2 minx1) (< overlap-v-min overlap-v-max))
    (progn
      (setq h-gap-right (- minx1 maxx2))
      (if (or (not min-gap) (< h-gap-right (car min-gap)))
        (setq min-gap (cons h-gap-right 
                           (list "horizontal" maxx2 0 minx1 0 overlap-v-min overlap-v-max)))
      )
    )
  )
  
  ;; 检查垂直间隙（上下方向）
  ;; rect1在rect2下方
  (if (and (< maxy1 miny2) (< overlap-h-min overlap-h-max))
    (progn
      (setq v-gap-bottom (- miny2 maxy1))
      (if (or (not min-gap) (< v-gap-bottom (car min-gap)))
        (setq min-gap (cons v-gap-bottom 
                           (list "vertical" 0 maxy1 0 miny2 overlap-h-min overlap-h-max)))
      )
    )
  )
  
  ;; rect1在rect2上方
  (if (and (< maxy2 miny1) (< overlap-h-min overlap-h-max))
    (progn
      (setq v-gap-top (- miny1 maxy2))
      (if (or (not min-gap) (< v-gap-top (car min-gap)))
        (setq min-gap (cons v-gap-top 
                           (list "vertical" 0 maxy2 0 miny1 overlap-h-min overlap-h-max)))
      )
    )
  )
  
  min-gap
)

;; 检查间隙是否已被标注（已废弃，保留以防兼容性）
(defun is-gap-annotated-wbb (pos1 pos2 annotated-list / found)
  (setq found nil)
  (foreach gap annotated-list
    (if (and (< (abs (- (car gap) pos1)) 0.1)
             (< (abs (- (cadr gap) pos2)) 0.1))
      (setq found T)
    )
  )
  found
)

;; 标注最靠近外部矩形边界的内部矩形
(defun annotate-nearest-to-edges-wbb (outer-minx outer-miny outer-maxx outer-maxy inner-rects 
                                       / nearest-left nearest-right nearest-top nearest-bottom
                                       min-left-gap min-right-gap min-top-gap min-bottom-gap
                                       inner-minx inner-miny inner-maxx inner-maxy
                                       left-gap right-gap top-gap bottom-gap scale-factor)
  
  ;; 初始化最小间隙和最近矩形
  (setq min-left-gap 1e10)
  (setq min-right-gap 1e10)
  (setq min-top-gap 1e10)
  (setq min-bottom-gap 1e10)
  (setq nearest-left nil)
  (setq nearest-right nil)
  (setq nearest-top nil)
  (setq nearest-bottom nil)
  
  ;; 找出每个方向最靠近边界的矩形
  (foreach inner-rect inner-rects
    (setq inner-minx (nth 0 inner-rect))
    (setq inner-miny (nth 1 inner-rect))
    (setq inner-maxx (nth 2 inner-rect))
    (setq inner-maxy (nth 3 inner-rect))
    
    ;; 左边距离
    (setq left-gap (- inner-minx outer-minx))
    (if (< left-gap min-left-gap)
      (progn
        (setq min-left-gap left-gap)
        (setq nearest-left inner-rect)
      )
    )
    
    ;; 右边距离
    (setq right-gap (- outer-maxx inner-maxx))
    (if (< right-gap min-right-gap)
      (progn
        (setq min-right-gap right-gap)
        (setq nearest-right inner-rect)
      )
    )
    
    ;; 底部距离
    (setq bottom-gap (- inner-miny outer-miny))
    (if (< bottom-gap min-bottom-gap)
      (progn
        (setq min-bottom-gap bottom-gap)
        (setq nearest-bottom inner-rect)
      )
    )
    
    ;; 顶部距离
    (setq top-gap (- outer-maxy inner-maxy))
    (if (< top-gap min-top-gap)
      (progn
        (setq min-top-gap top-gap)
        (setq nearest-top inner-rect)
      )
    )
  )
  
  ;; 标注左侧最近的矩形
  (if (and nearest-left (> min-left-gap 0.1))
    (progn
      (setq inner-minx (nth 0 nearest-left))
      (setq inner-miny (nth 1 nearest-left))
      (setq inner-maxy (nth 3 nearest-left))
      (setq scale-factor (/ min-left-gap 1000.0))
      (if (< scale-factor 0.01) (setq scale-factor 0.01))
      (setup-wbb-dimstyle scale-factor)
      (command "_.DIMLINEAR" 
               (list outer-minx (/ (+ inner-miny inner-maxy) 2.0) 0)
               (list inner-minx (/ (+ inner-miny inner-maxy) 2.0) 0)
               "_H"
               (list (/ (+ outer-minx inner-minx) 2.0) (/ (+ inner-miny inner-maxy) 2.0) 0))
    )
  )
  
  ;; 标注右侧最近的矩形
  (if (and nearest-right (> min-right-gap 0.1))
    (progn
      (setq inner-maxx (nth 2 nearest-right))
      (setq inner-miny (nth 1 nearest-right))
      (setq inner-maxy (nth 3 nearest-right))
      (setq scale-factor (/ min-right-gap 1000.0))
      (if (< scale-factor 0.01) (setq scale-factor 0.01))
      (setup-wbb-dimstyle scale-factor)
      (setvar "DIMTAD" 3)
      (command "_.DIMLINEAR" 
               (list inner-maxx (/ (+ inner-miny inner-maxy) 2.0) 0)
               (list outer-maxx (/ (+ inner-miny inner-maxy) 2.0) 0)
               "_H"
               (list (/ (+ inner-maxx outer-maxx) 2.0) (/ (+ inner-miny inner-maxy) 2.0) 0))
      (setvar "DIMTAD" 0)
    )
  )
  
  ;; 标注底部最近的矩形
  (if (and nearest-bottom (> min-bottom-gap 0.1))
    (progn
      (setq inner-minx (nth 0 nearest-bottom))
      (setq inner-maxx (nth 2 nearest-bottom))
      (setq inner-miny (nth 1 nearest-bottom))
      (setq scale-factor (/ min-bottom-gap 1000.0))
      (if (< scale-factor 0.01) (setq scale-factor 0.01))
      (setup-wbb-dimstyle scale-factor)
      (setvar "DIMTAD" 3)
      (command "_.DIMLINEAR" 
               (list (/ (+ inner-minx inner-maxx) 2.0) outer-miny 0)
               (list (/ (+ inner-minx inner-maxx) 2.0) inner-miny 0)
               "_V"
               (list (/ (+ inner-minx inner-maxx) 2.0) (/ (+ outer-miny inner-miny) 2.0) 0))
      (setvar "DIMTAD" 0)
    )
  )
  
  ;; 标注顶部最近的矩形
  (if (and nearest-top (> min-top-gap 0.1))
    (progn
      (setq inner-minx (nth 0 nearest-top))
      (setq inner-maxx (nth 2 nearest-top))
      (setq inner-maxy (nth 3 nearest-top))
      (setq scale-factor (/ min-top-gap 1000.0))
      (if (< scale-factor 0.01) (setq scale-factor 0.01))
      (setup-wbb-dimstyle scale-factor)
      (setvar "DIMTAD" 3)
      (command "_.DIMLINEAR" 
               (list (/ (+ inner-minx inner-maxx) 2.0) inner-maxy 0)
               (list (/ (+ inner-minx inner-maxx) 2.0) outer-maxy 0)
               "_V"
               (list (/ (+ inner-minx inner-maxx) 2.0) (/ (+ inner-maxy outer-maxy) 2.0) 0))
      (setvar "DIMTAD" 0)
    )
  )
)

;; 标注位置冲突检测辅助函数

;; 检查Y位置是否与已用位置冲突（阈值：threshold）
(defun is-y-conflicting-wbb (y used-ys threshold / result)
  (setq result nil)
  (foreach used-y used-ys
    (if (< (abs (- y used-y)) threshold)
      (setq result T)
    )
  )
  result
)

;; 获取安全的水平标注Y位置（避免与已有标注重叠）
;; 冲突时交替上下偏移，最多尝试6次
(defun get-safe-horizontal-y-wbb (preferred-y used-ys threshold / try-y attempt offset)
  (setq try-y preferred-y)
  (setq attempt 0)
  (while (and (is-y-conflicting-wbb try-y used-ys threshold) (< attempt 6))
    (setq offset (* threshold (if (= (rem attempt 2) 0)
                         (/ (+ attempt 2) 2)
                         (- (/ (+ attempt 2) 2)))))
    (setq try-y (+ preferred-y offset))
    (setq attempt (1+ attempt))
  )
  try-y
)

;; 检查X位置是否与已用位置冲突
(defun is-x-conflicting-wbb (x used-xs threshold / result)
  (setq result nil)
  (foreach used-x used-xs
    (if (< (abs (- x used-x)) threshold)
      (setq result T)
    )
  )
  result
)

;; 获取安全的垂直标注X位置（避免与已有标注重叠）
(defun get-safe-vertical-x-wbb (preferred-x used-xs threshold / try-x attempt offset)
  (setq try-x preferred-x)
  (setq attempt 0)
  (while (and (is-x-conflicting-wbb try-x used-xs threshold) (< attempt 6))
    (setq offset (* threshold (if (= (rem attempt 2) 0)
                         (/ (+ attempt 2) 2)
                         (- (/ (+ attempt 2) 2)))))
    (setq try-x (+ preferred-x offset))
    (setq attempt (1+ attempt))
  )
  try-x
)

;; 标注水平间隙
;; used-ys: 已使用的Y位置列表（调用方维护）
;; threshold: Y方向最小间距（默认30）
(defun annotate-horizontal-gap-wbb (x1 x2 y-min y-max gap used-ys threshold
                             / y-mid safe-y p1 p2 dim-point scale-factor)
  (setq y-mid (/ (+ y-min y-max) 2.0))
  ;; 计算安全的Y位置（避免与已有标注重叠）
  (setq safe-y (get-safe-horizontal-y-wbb y-mid used-ys threshold))
  (setq p1 (list x1 safe-y 0))
  (setq p2 (list x2 safe-y 0))
  (setq dim-point (list (/ (+ x1 x2) 2.0) safe-y 0))
  
  ;; 设置标注样式
  (setq scale-factor (/ gap 1000.0))
  (if (< scale-factor 0.01) (setq scale-factor 0.01))
  (setup-wbb-dimstyle scale-factor)
  
  (command "_.DIMLINEAR" p1 p2 "_H" dim-point)
  ;; 返回实际使用的Y位置，供调用方添加到used-ys
  safe-y
)

;; 标注垂直间隙
;; used-xs: 已使用的X位置列表（调用方维护）
;; threshold: X方向最小间距（默认30）
(defun annotate-vertical-gap-wbb (y1 y2 x-min x-max gap used-xs threshold
                           / x-mid safe-x p1 p2 dim-point scale-factor)
  (setq x-mid (/ (+ x-min x-max) 2.0))
  ;; 计算安全的X位置（避免与已有标注重叠）
  (setq safe-x (get-safe-vertical-x-wbb x-mid used-xs threshold))
  (setq p1 (list safe-x y1 0))
  (setq p2 (list safe-x y2 0))
  (setq dim-point (list safe-x (/ (+ y1 y2) 2.0) 0))
  
  ;; 设置标注样式
  (setq scale-factor (/ gap 1000.0))
  (if (< scale-factor 0.01) (setq scale-factor 0.01))
  (setup-wbb-dimstyle scale-factor)
  
  (command "_.DIMLINEAR" p1 p2 "_V" dim-point)
  ;; 返回实际使用的X位置，供调用方添加到used-xs
  safe-x
)

;; 清除选中矩形相关的标注
(defun clear-existing-dimensions-wbb (ss / i rect-ent rect-bounds minx miny maxx maxy 
                                       all-dims j dim-ent dim-data dim-p1 dim-p2 dim-p3
                                       deleted-count is-related)
  (setq deleted-count 0)
  
  ;; 获取所有标注对象
  (setq all-dims (ssget "_X" '((0 . "DIMENSION"))))
  
  (if all-dims
    (progn
      ;; 遍历所有标注
      (setq j 0)
      (repeat (sslength all-dims)
        (setq dim-ent (ssname all-dims j))
        
        (if (and dim-ent (entget dim-ent))
          (progn
            (setq dim-data (entget dim-ent))
            
            ;; 获取标注的关键点
            (setq dim-p1 (cdr (assoc 13 dim-data)))  ;; 第一个定义点
            (setq dim-p2 (cdr (assoc 14 dim-data)))  ;; 第二个定义点
            (setq dim-p3 (cdr (assoc 10 dim-data)))  ;; 标注文字位置
            
            ;; 检查标注是否与任何选中的矩形相关
            (setq is-related nil)
            (setq i 0)
            (repeat (sslength ss)
              (setq rect-ent (ssname ss i))
              (setq rect-bounds (get-rectangle-bounds-wbb rect-ent))
              
              (if rect-bounds
                (progn
                  (setq minx (car (car rect-bounds)))
                  (setq miny (cadr (car rect-bounds)))
                  (setq maxx (car (cadr rect-bounds)))
                  (setq maxy (cadr (cadr rect-bounds)))
                  
                  ;; 检查标注的定义点是否在矩形边界上或附近（容差5）
                  (if (or
                        ;; 检查第一个定义点
                        (and dim-p1
                             (is-point-on-rect-edge-wbb (car dim-p1) (cadr dim-p1) 
                                                         minx miny maxx maxy 5))
                        ;; 检查第二个定义点
                        (and dim-p2
                             (is-point-on-rect-edge-wbb (car dim-p2) (cadr dim-p2) 
                                                         minx miny maxx maxy 5))
                        ;; 检查标注文字是否在矩形附近（容差50）
                        (and dim-p3
                             (>= (car dim-p3) (- minx 50))
                             (<= (car dim-p3) (+ maxx 50))
                             (>= (cadr dim-p3) (- miny 50))
                             (<= (cadr dim-p3) (+ maxy 50))))
                    (setq is-related T)
                  )
                )
              )
              (setq i (1+ i))
            )
            
            ;; 如果标注与选中矩形相关，删除它
            (if is-related
              (progn
                (entdel dim-ent)
                (setq deleted-count (1+ deleted-count))
              )
            )
          )
        )
        (setq j (1+ j))
      )
      
      (if (> deleted-count 0)
        (princ (strcat "\n已清除 " (itoa deleted-count) " 个相关标注。"))
      )
    )
  )
)

;; 检查点是否在矩形边界上或附近
(defun is-point-on-rect-edge-wbb (px py minx miny maxx maxy tolerance / on-edge)
  (setq on-edge nil)
  
  ;; 检查是否在左边界附近
  (if (and (< (abs (- px minx)) tolerance)
           (>= py (- miny tolerance))
           (<= py (+ maxy tolerance)))
    (setq on-edge T)
  )
  
  ;; 检查是否在右边界附近
  (if (and (< (abs (- px maxx)) tolerance)
           (>= py (- miny tolerance))
           (<= py (+ maxy tolerance)))
    (setq on-edge T)
  )
  
  ;; 检查是否在下边界附近
  (if (and (< (abs (- py miny)) tolerance)
           (>= px (- minx tolerance))
           (<= px (+ maxx tolerance)))
    (setq on-edge T)
  )
  
  ;; 检查是否在上边界附近
  (if (and (< (abs (- py maxy)) tolerance)
           (>= px (- minx tolerance))
           (<= px (+ maxx tolerance)))
    (setq on-edge T)
  )
  
  on-edge
)

;; 设置WBB标注样式（自适应）
(defun setup-wbb-dimstyle (scale-factor / style-name font-file base-text-height base-arrow-size new-text-height new-arrow-size)
  ;; 确保宋体样式存在
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
  
  ;; 基础参数（参考尺寸1000时的值）
  (setq base-text-height 25)  ;; 文字基础值：25
  (setq base-arrow-size 16)   ;; 箭头基础值：16（文字的2倍）
  
  ;; 计算新的参数（根据矩形尺寸自适应）
  (setq new-text-height (* base-text-height scale-factor))
  (setq new-arrow-size (* base-arrow-size scale-factor))
  
  ;; 确保最小值
  (if (< new-text-height 5)
    (setq new-text-height 5)
  )
  (if (< new-arrow-size 2)  ;; 箭头最小值调整为2
    (setq new-arrow-size 2)
  )
  
  ;; 设置标注参数（自适应）
  (setvar "DIMTXSTY" style-name)            ;; 文字样式：宋体
  (setvar "DIMTXT" new-text-height)         ;; 文字高度：自适应
  (setvar "DIMASZ" new-arrow-size)          ;; 箭头大小：自适应
  (setvar "DIMGAP" (* new-text-height 0.5)) ;; 文字间隙：文字高度的0.5倍
  (setvar "DIMEXE" (* new-arrow-size 1.0))  ;; 延伸线超出：箭头大小的1倍（原来2倍的一半）
  (setvar "DIMEXO" (* new-arrow-size 1.5))  ;; 延伸线偏移：箭头大小的1.5倍
  
  ;; 设置精度和格式
  (setvar "DIMDEC" 2)                      ;; 精度：2位小数
  (setvar "DIMZIN" 12)                     ;; 消零：12（抑制后续零和0英尺）
  
  ;; 设置比例因子为10
  (setvar "DIMLFAC" 10)                    ;; 线性比例因子：10
)

;; 获取矩形边界
(defun get-rectangle-bounds-wbb (ent / ent-data ent-type p1 p2 minx miny maxx maxy)
  (setq ent-data (entget ent))
  (setq ent-type (cdr (assoc 0 ent-data)))
  
  (cond
    ;; LWPOLYLINE类型
    ((= ent-type "LWPOLYLINE")
     (setq p1 (cdr (assoc 10 ent-data)))
     (setq minx (car p1))
     (setq miny (cadr p1))
     (setq maxx (car p1))
     (setq maxy (cadr p1))
     
     ;; 遍历所有顶点
     (foreach item ent-data
       (if (= (car item) 10)
         (progn
           (setq pt (cdr item))
           (if (< (car pt) minx) (setq minx (car pt)))
           (if (< (cadr pt) miny) (setq miny (cadr pt)))
           (if (> (car pt) maxx) (setq maxx (car pt)))
           (if (> (cadr pt) maxy) (setq maxy (cadr pt)))
         )
       )
     )
     (list (list minx miny) (list maxx maxy))
    )
    
    ;; POLYLINE类型
    ((= ent-type "POLYLINE")
     (setq minx nil)
     (setq miny nil)
     (setq maxx nil)
     (setq maxy nil)
     
     (setq vertex-ent (entnext ent))
     (while (and vertex-ent (= (cdr (assoc 0 (entget vertex-ent))) "VERTEX"))
       (setq vertex-data (entget vertex-ent))
       (setq pt (cdr (assoc 10 vertex-data)))
       
       (if (not minx)
         (progn
           (setq minx (car pt))
           (setq miny (cadr pt))
           (setq maxx (car pt))
           (setq maxy (cadr pt))
         )
         (progn
           (if (< (car pt) minx) (setq minx (car pt)))
           (if (< (cadr pt) miny) (setq miny (cadr pt)))
           (if (> (car pt) maxx) (setq maxx (car pt)))
           (if (> (cadr pt) maxy) (setq maxy (cadr pt)))
         )
       )
       
       (setq vertex-ent (entnext vertex-ent))
     )
     
     (if minx
       (list (list minx miny) (list maxx maxy))
       nil
     )
    )
    
    ;; 其他类型，尝试使用边界框
    (T
     (setq bbox (vla-get-boundingbox 
                  (vlax-ename->vla-object ent)
                  'minpt
                  'maxpt))
     (if bbox
       (list 
         (list (vlax-safearray-get-element minpt 0)
               (vlax-safearray-get-element minpt 1))
         (list (vlax-safearray-get-element maxpt 0)
               (vlax-safearray-get-element maxpt 1))
       )
       nil
     )
    )
  )
)

;; 添加标注
(defun add-dimension-wbb (p1 p2 dim-point is-horizontal)
  (if is-horizontal
    (command "_.DIMLINEAR" p1 p2 "_H" dim-point)
    (command "_.DIMLINEAR" p1 p2 "_V" dim-point)
  )
)

;; 智能调整尺寸值，使标注后以00或50结尾
;; is-width: T表示宽度（只能增大），nil表示高度（只能缩小）
(defun adjust-dim-value-wbb (dimension is-width / scaled-value last-two-digits remainder target-50 target-00 diff-50 diff-00)
  ;; 将尺寸乘以10（因为标注比例是1:10）
  (setq scaled-value (* dimension 10))
  
  ;; 获取最后两位数字
  (setq last-two-digits (rem (fix scaled-value) 100))
  
  ;; 检查是否已经以00或50结尾
  (if (or (= last-two-digits 0) (= last-two-digits 50))
    dimension
    (progn
      (setq remainder (rem (fix scaled-value) 50))
      
      (if is-width
        ;; 宽度：只能增大
        (progn
          ;; 计算下一个50结尾的值
          (setq target-50 (+ (- (fix scaled-value) remainder) 50))
          
          ;; 计算下一个00结尾的值
          (setq target-00 (+ (- (fix scaled-value) remainder) 100))
          
          ;; 选择最近的目标
          (setq diff-50 (- target-50 (fix scaled-value)))
          (setq diff-00 (- target-00 (fix scaled-value)))
          
          (if (<= diff-50 diff-00)
            (/ target-50 10.0)
            (/ target-00 10.0)
          )
        )
        ;; 高度：只能缩小
        (progn
          ;; 计算前一个50结尾的值
          (if (> remainder 0)
            (setq target-50 (- (fix scaled-value) remainder))
            (setq target-50 (- (fix scaled-value) 50))
          )
          
          ;; 计算前一个00结尾的值
          (if (>= remainder 50)
            (setq target-00 (- (fix scaled-value) remainder))
            (setq target-00 (- (fix scaled-value) remainder 50))
          )
          
          ;; 选择最近的目标
          (setq diff-50 (- (fix scaled-value) target-50))
          (setq diff-00 (- (fix scaled-value) target-00))
          
          (if (<= diff-50 diff-00)
            (/ target-50 10.0)
            (/ target-00 10.0)
          )
        )
      )
    )
  )
)

(princ "\nWBB命令已加载。输入 WBB 开始智能标注矩形。")
(princ)
