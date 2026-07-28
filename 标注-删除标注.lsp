;;; WSD - Delete Dimensions from Selection
;;; Function: Guide user to select objects, filter and delete all dimensions
;;; Command: WSD

;@name 删除标注
;@group 标注工具
;@desc 引导用户选择（单选/框选），自动筛选并删除其中所有标注对象
;@require Selection
;@require ModelSpace
(defun c:WSD ()
  (setq ss nil)
  (setq dim-count 0)
  
  (princ "\n=== WSD 删除标注 ===")
  (princ "\n请选择要删除的标注（可单选或框选）: ")
  
  ;; Select and filter only dimension entities
  ;; *DIMENSION matches DIMENSION, ARC_DIMENSION, etc.
  (setq ss (ssget '((0 . "*DIMENSION"))))
  
  (cond
    (ss
      (setq dim-count (sslength ss))
      (princ (strcat "\n找到 " (itoa dim-count) " 个标注"))
      
      ;; Delete all selected dimensions
      (command "_.ERASE" ss "")
      
      (princ (strcat "\n已删除 " (itoa dim-count) " 个标注。"))
    )
    (t
      (princ "\n未选择任何标注对象。")
    )
  )
  
  (princ)
)

(princ "\n命令 WSD 已加载。输入 WSD 选择并删除标注。")
(princ)
