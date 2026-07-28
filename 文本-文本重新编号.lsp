;@name 文本重新编号
;@group 文本编辑
;@desc 选择多个文本，按位置从左往右（或从右往左）重新编号，自动识别编号格式并居中到表格单元格

;; 将数字 n 格式化为至少 len 位的字符串，不足时前补零
(defun pad-number (n len / s)
  (setq s (itoa n))
  (while (< (strlen s) len)
    (setq s (strcat "0" s))
  )
  s
)

(defun c:RNT ()
  (setq ss nil)
  (setq direction nil)
  (setq texts-data nil)
  (setq sorted-texts nil)
  (setq prefix nil)
  (setq suffix nil)
  (setq start-num nil)
  (setq i nil)
  (setq ent nil)
  (setq ent-data nil)
  (setq ent-type nil)
  (setq text-content nil)
  (setq text-pos nil)
  (setq text-x nil)
  (setq pattern nil)
  (setq new-content nil)
  (setq centered-count nil)
  
  (princ "\nSelect text entities to renumber: ")
  (setq ss (ssget '((0 . "TEXT,MTEXT"))))
  
  (if ss
    (progn
      (initget "Left Right E D")
      (setq direction (getkword "\nRenumber direction [Left/Right/E(top-down)/D(bottom-up)] <L>: "))
      (if (not direction)
        (setq direction "Left")
      )
      
      (princ "\nCollecting text data...")
      (setq texts-data (list))
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq ent-data (entget ent))
        (setq ent-type (cdr (assoc 0 ent-data)))
        
        (if (= ent-type "TEXT")
          (setq text-content (cdr (assoc 1 ent-data)))
          (setq text-content (cdr (assoc 1 ent-data)))
        )
        
        (setq text-pos (cdr (assoc 10 ent-data)))
        (setq text-x (car text-pos))
        (setq text-y (cadr text-pos))
        
        (setq texts-data (append texts-data 
          (list (list ent ent-data text-content text-x text-y))))
        
        (setq i (1+ i))
      )
      
      (princ "\nAnalyzing numbering pattern...")
      (setq pattern (analyze-number-pattern (nth 2 (car texts-data))))
      
      (if pattern
        (progn
          (setq prefix (nth 0 pattern))
          (setq suffix (nth 1 pattern))
          (setq detected-num (atoi (nth 2 pattern)))
          (setq num-len (nth 3 pattern))
          (princ (strcat "\nDetected pattern: \"" prefix "N" suffix "\""))
          
          (princ "\nSorting texts by position...")
          (cond
            ((= direction "Right")
             (setq sorted-texts (vl-sort texts-data 
               '(lambda (a b) (> (nth 3 a) (nth 3 b))))))
            ((= direction "E")
             (setq sorted-texts (vl-sort texts-data 
               '(lambda (a b) (> (nth 4 a) (nth 4 b))))))
            ((= direction "D")
             (setq sorted-texts (vl-sort texts-data 
               '(lambda (a b) (< (nth 4 a) (nth 4 b))))))
            (T
             (setq sorted-texts (vl-sort texts-data 
               '(lambda (a b) (< (nth 3 a) (nth 3 b))))))
          )
          
          (princ "\nRenumbering texts...")
          (setq start-num (getint (strcat "\nEnter starting number <" (itoa detected-num) ">: ")))
          (if (not start-num)
            (setq start-num detected-num)
          )
          (foreach text-item sorted-texts
            (setq ent (nth 0 text-item))
            (setq ent-data (nth 1 text-item))
            (setq new-content (strcat prefix (pad-number start-num num-len) suffix))
            
            (setq ent-data (subst (cons 1 new-content) (assoc 1 ent-data) ent-data))
            (entmod ent-data)
            (entupd ent)
            
            (setq start-num (1+ start-num))
          )
          
          (princ (strcat "\nRenumbered " (itoa (length sorted-texts)) " text(s) " 
                        (cond
                          ((= direction "Right") "from right to left.")
                          ((= direction "E") "from top to bottom.")
                          ((= direction "D") "from bottom to top.")
                          (T "from left to right."))))
        )
        (princ "\nError: Cannot detect numbering pattern in text.")
      )
    )
    (princ "\nNo text selected.")
  )
  (princ)
)

(defun analyze-number-pattern (text / num-start num-end i char prefix suffix number in-number num-length)
  (setq num-start nil)
  (setq num-end nil)
  (setq i 1)
  (setq char nil)
  (setq prefix nil)
  (setq suffix nil)
  (setq number nil)
  (setq in-number nil)
  (setq num-length 0)
  
  ;; 遍历整个字符串，找到最后一组连续数字（字母后面的数字优先）
  (while (<= i (strlen text))
    (setq char (substr text i 1))
    (if (and (>= (ascii char) 48) (<= (ascii char) 57))
      (progn
        ;; 遇到数字
        (if (not in-number)
          (progn
            ;; 新数字组开始：覆盖之前记录的组，始终保留最后一组
            (setq num-start i)
            (setq in-number T)
            (setq num-length 1)
          )
          (progn
            ;; 数字继续
            (setq num-length (1+ num-length))
          )
        )
        (setq num-end i)
      )
      (progn
        ;; 遇到非数字：结束当前数字组，继续扫描寻找更靠右的数字组
        (setq in-number nil)
      )
    )
    (setq i (1+ i))
  )
  
  (if (and num-start (numberp num-end) (> num-length 0))
    (progn
      (if (> num-start 1)
        (setq prefix (substr text 1 (1- num-start)))
        (setq prefix "")
      )
      (setq number (substr text num-start num-length))
      (if (< num-end (strlen text))
        (setq suffix (substr text (1+ num-end)))
        (setq suffix "")
      )
      (list prefix suffix number num-length)
    )
    nil
  )
)

(princ "\nType RNT to renumber texts.")
(princ)

(defun center-texts-efficient (sorted-texts / text-item text-ent text-data text-pos text-count i v-left v-right h-top h-bottom left-data right-data top-data bottom-data left-x right-x top-y bottom-y rect-center)
  (setq centered-count 0)
  (setq text-count (length sorted-texts))
  
  (princ "\nCentering texts in table cells...")
  
  ;; 遍历每个文字，找到其所在单元格的四条边界
  (setq i 0)
  (while (< i text-count)
    (princ (strcat "\nProcessing text " (itoa (+ i 1)) " of " (itoa text-count)))
    
    (setq text-item (nth i sorted-texts))
    (setq text-ent (nth 0 text-item))
    (setq text-data (entget text-ent))
    (setq text-pos (cdr (assoc 10 text-data)))
    (setq text-x (car text-pos))
    (setq text-y (cadr text-pos))
    
    (princ (strcat " at (" (rtos text-x 2 2) ", " (rtos text-y 2 2) ")"))
    
    ;; 从文字位置向四个方向找最近的边界线
    (setq v-left (find-line-in-direction text-x text-y -1 0 "V" nil nil))
    (setq v-right (find-line-in-direction text-x text-y 1 0 "V" nil nil))
    (setq h-top (find-line-in-direction text-x text-y 0 1 "H" nil nil))
    (setq h-bottom (find-line-in-direction text-x text-y 0 -1 "H" nil nil))
    
    ;; 检查是否找到完整的四条边界
    (if (and v-left v-right h-top h-bottom)
      (progn
        (setq left-data (entget v-left))
        (setq right-data (entget v-right))
        (setq top-data (entget h-top))
        (setq bottom-data (entget h-bottom))
        
        (setq left-x (car (cdr (assoc 10 left-data))))
        (setq right-x (car (cdr (assoc 10 right-data))))
        (setq top-y (cadr (cdr (assoc 10 top-data))))
        (setq bottom-y (cadr (cdr (assoc 10 bottom-data))))
        
        ;; 计算矩形中心
        (setq rect-center (calculate-rect-center-rnt v-left v-right h-top h-bottom))
        
        (if rect-center
          (progn
            (stretch-move-text-to-center-rnt text-ent rect-center)
            (setq centered-count (1+ centered-count))
          )
        )
      )
    )
    
    (setq i (1+ i))
  )
  
  (princ (strcat "\nCentered " (itoa centered-count) " of " (itoa text-count) " text(s)."))
  centered-count
)

(defun find-line-in-direction (start-x start-y dir-x dir-y line-type limit-x limit-y / all-lines closest-line closest-dist i ent data pa pb is-horizontal is-vertical is-correct-type line-y line-x-min line-x-max dist line-x line-y-min line-y-max)
  (setq all-lines (ssget "_X" '((0 . "LINE"))))
  (setq closest-line nil)
  (setq closest-dist 999999999.0)
  
  (if all-lines
    (progn
      (setq i 0)
      (while (< i (sslength all-lines))
        (setq ent (ssname all-lines i))
        (setq data (entget ent))
        (setq pa (cdr (assoc 10 data)))
        (setq pb (cdr (assoc 11 data)))
        
        (setq is-horizontal (< (abs (- (cadr pa) (cadr pb))) 0.01))
        (setq is-vertical (< (abs (- (car pa) (car pb))) 0.01))
        
        ;; 检查是否是要找的直线类型
        (setq is-correct-type (or (and (= line-type "H") is-horizontal)
                                  (and (= line-type "V") is-vertical)))
        
        (if is-correct-type
          (progn
            ;; 根据方向判断是否在正确的一侧
            (if (= line-type "H")
              (progn
                ;; 水平线：检查Y方向，并验证直线覆盖文字的X坐标
                (setq line-y (cadr pa))
                (setq line-x-min (min (car pa) (car pb)))
                (setq line-x-max (max (car pa) (car pb)))
                
                ;; 检查文字X坐标是否在直线的X范围内
                (if (and (>= start-x line-x-min) (<= start-x line-x-max))
                  (progn
                    (if (and (> dir-y 0) (> line-y start-y))
                      ;; 向上找，线在上方
                      (progn
                        (if (and limit-y (> line-y limit-y))
                          (progn
                            (setq closest-line nil)
                            (setq i (sslength all-lines))
                          )
                          (progn
                            (setq dist (- line-y start-y))
                            (if (< dist closest-dist)
                              (progn
                                (setq closest-dist dist)
                                (setq closest-line ent)
                              )
                            )
                          )
                        )
                      )
                    )
                    (if (and (< dir-y 0) (< line-y start-y))
                      ;; 向下找，线在下方
                      (progn
                        (if (and limit-y (< line-y limit-y))
                          (progn
                            (setq closest-line nil)
                            (setq i (sslength all-lines))
                          )
                          (progn
                            (setq dist (- start-y line-y))
                            (if (< dist closest-dist)
                              (progn
                                (setq closest-dist dist)
                                (setq closest-line ent)
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
              (progn
                ;; 竖直线：检查X方向
                ;; 注意：不检查Y范围，因为我们要找所有在指定X方向上的竖线
                (setq line-x (car pa))
                
                (if (and (> dir-x 0) (> line-x start-x))
                  ;; 向右找，线在右方
                  (progn
                    (if (and limit-x (> line-x limit-x))
                      (progn
                        (setq closest-line nil)
                        (setq i (sslength all-lines))
                      )
                      (progn
                        (setq dist (- line-x start-x))
                        (if (< dist closest-dist)
                          (progn
                            (setq closest-dist dist)
                            (setq closest-line ent)
                          )
                        )
                      )
                    )
                  )
                )
                (if (and (< dir-x 0) (< line-x start-x))
                  ;; 向左找，线在左方
                  (progn
                    (if (and limit-x (< line-x limit-x))
                      (progn
                        (setq closest-line nil)
                        (setq i (sslength all-lines))
                      )
                      (progn
                        (setq dist (- start-x line-x))
                        (if (< dist closest-dist)
                          (progn
                            (setq closest-dist dist)
                            (setq closest-line ent)
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
        
        (setq i (1+ i))
      )
    )
  )
  
  closest-line
)

(defun calculate-rect-center-rnt (v-left v-right h-top h-bottom / left-data right-data top-data bottom-data left-p1 left-p2 right-p1 right-p2 top-p1 top-p2 bottom-p1 bottom-p2 corner-tl corner-tr corner-br corner-bl center-x center-y)
  (setq left-data (entget v-left))
  (setq right-data (entget v-right))
  (setq top-data (entget h-top))
  (setq bottom-data (entget h-bottom))
  
  ;; 获取四条直线的端点
  (setq left-p1 (cdr (assoc 10 left-data)))
  (setq left-p2 (cdr (assoc 11 left-data)))
  (setq right-p1 (cdr (assoc 10 right-data)))
  (setq right-p2 (cdr (assoc 11 right-data)))
  (setq top-p1 (cdr (assoc 10 top-data)))
  (setq top-p2 (cdr (assoc 11 top-data)))
  (setq bottom-p1 (cdr (assoc 10 bottom-data)))
  (setq bottom-p2 (cdr (assoc 11 bottom-data)))
  
  ;; 计算四条直线的交点，得到矩形的四个角
  ;; 左上角：左竖线 与 上横线的交点
  (setq corner-tl (inters left-p1 left-p2 top-p1 top-p2 nil))
  ;; 右上角：右竖线 与 上横线的交点
  (setq corner-tr (inters right-p1 right-p2 top-p1 top-p2 nil))
  ;; 右下角：右竖线 与 下横线的交点
  (setq corner-br (inters right-p1 right-p2 bottom-p1 bottom-p2 nil))
  ;; 左下角：左竖线 与 下横线的交点
  (setq corner-bl (inters left-p1 left-p2 bottom-p1 bottom-p2 nil))
  
  ;; 验证四个角都找到了
  (if (and corner-tl corner-tr corner-br corner-bl)
    (progn
      ;; 计算矩形的几何中心（四个角的平均值）
      (setq center-x (/ (+ (car corner-tl) (car corner-tr) (car corner-br) (car corner-bl)) 4.0))
      (setq center-y (/ (+ (cadr corner-tl) (cadr corner-tr) (cadr corner-br) (cadr corner-bl)) 4.0))
      
      (list center-x center-y 0)
    )
    (progn
      (princ "\nError: Cannot calculate rectangle corners")
      nil
    )
  )
)

(defun stretch-move-text-to-center-rnt (text-ent new-center / text-data text-type)
  (setq text-data (entget text-ent))
  (setq text-type (cdr (assoc 0 text-data)))
  
  (if (= text-type "MTEXT")
    (progn
      ;; MTEXT：直接修改插入点
      (setq text-data (subst (cons 10 new-center) (assoc 10 text-data) text-data))
      ;; 设置为中心对齐
      (setq text-data (subst (cons 71 5) (assoc 71 text-data) text-data))
      (entmod text-data)
      (entupd text-ent)
    )
    (progn
      ;; TEXT：设置为中心对齐
      (setq text-data (vl-remove (assoc 72 text-data) text-data))
      (setq text-data (vl-remove (assoc 73 text-data) text-data))
      (setq text-data (append text-data (list (cons 72 1))))
      (setq text-data (append text-data (list (cons 73 2))))
      
      ;; 设置代码10和代码11都为矩形中心
      (setq text-data (subst (cons 10 new-center) (assoc 10 text-data) text-data))
      
      ;; 添加或修改代码11为对齐点
      (if (assoc 11 text-data)
        (setq text-data (subst (cons 11 new-center) (assoc 11 text-data) text-data))
        (setq text-data (append text-data (list (cons 11 new-center))))
      )
      
      (entmod text-data)
      (entupd text-ent)
    )
  )
)