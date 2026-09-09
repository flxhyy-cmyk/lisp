;;; ============================================================
;;; 命令名称: QH  (圈号 - 给数字文本添加外圆圈)
;;; 功能说明:
;;;   1. 启动命令后提示用户输入"圆圈直径/文字高度"的比例系数(可直接回车用默认值)
;;;   2. 引导用户选择需要加圈的文字对象(支持 TEXT 与 MTEXT，可多选/框选)
;;;   3. 依据每个文字对象自身的文字高度，按比例智能计算圆的半径
;;;   4. 以文字外包框的几何中心为圆心，生成对应的外圆圈(CIRCLE)
;;; ============================================================

(defun c:QH ( / ratioInput ratio ss ssCount i ent entData entType
              obj minp maxp cen h halfW halfH r layerName doneCount)

  (vl-load-com)

  ;; 默认比例系数：圆半径 = 文字高度 * ratio
  ;; 0.75 大致对应常见的"圈号"效果(圆直径约为字高的1.5倍)
  (setq ratio 0.75)

  (initget 6) ; 禁止输入 0 或负数，允许直接回车使用默认值
  (setq ratioInput
    (getreal
      (strcat "\n请输入圆圈半径相对文字高度的比例系数 <" (rtos ratio 2 2) ">: ")
    )
  )
  (if ratioInput (setq ratio ratioInput))

  (princ "\n请选择需要添加外圆圈的数字文本(可框选/多选，支持 TEXT 和 MTEXT): ")
  (setq ss (ssget '((0 . "TEXT,MTEXT"))))

  (cond
    ((not ss)
     (princ "\n未选择任何文字对象，命令结束。")
    )
    (t
     (setq ssCount (sslength ss))
     (setq doneCount 0)
     (setq i 0)
     (repeat ssCount

       (setq ent (ssname ss i))
       (setq entData (entget ent))
       (setq entType (cdr (assoc 0 entData)))
       (setq layerName (cdr (assoc 8 entData)))

       ;; 用 VLA 对象获取精确的几何外包框(适用于 TEXT 与 MTEXT，
       ;; 不受对齐方式/旋转角度影响，圆心始终取包围盒中心)
       (setq obj (vlax-ename->vla-object ent))

       (if
         (not
           (vl-catch-all-error-p
             (vl-catch-all-apply 'vla-GetBoundingBox (list obj 'minp 'maxp))
           )
         )
         (progn
           (setq minp (vlax-safearray->list minp))
           (setq maxp (vlax-safearray->list maxp))

           (setq halfW (/ (- (car maxp) (car minp)) 2.0))
           (setq halfH (/ (- (cadr maxp) (cadr minp)) 2.0))

           (setq cen
             (list
               (+ (car minp) halfW)
               (+ (cadr minp) halfH)
               0.0
             )
           )

           ;; 优先取文字自身的"文字高度"字段(组码40)；
           ;; 若取不到(极少数情况)，兜底改用外包框高度
           (setq h (cdr (assoc 40 entData)))
           (if (or (not h) (<= h 0.0))
             (setq h (* halfH 2.0))
           )

           (setq r (* h ratio))

           (if (> r 0.0)
             (progn
               (entmake
                 (list
                   '(0 . "CIRCLE")
                   '(100 . "AcDbEntity")
                   (cons 8 layerName)
                   '(100 . "AcDbCircle")
                   (cons 10 cen)
                   '(210 0.0 0.0 1.0)
                   (cons 40 r)
                 )
               )
               (setq doneCount (1+ doneCount))
             )
           )
         )
       )

       (setq i (1+ i))
     )

     (princ
       (strcat "\n处理完成，共选中 " (itoa ssCount)
               " 个文字对象，成功添加 " (itoa doneCount) " 个外圆圈。")
     )
    )
  )

  (princ)
)

(princ "\n命令已加载：输入 QH 并回车，即可为选中的数字文本按比例添加外圆圈。")
(princ)
