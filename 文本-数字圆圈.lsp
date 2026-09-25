;;; ============================================================
;;; 命令名称: QH  (圈号 - 给数字文本添加外圆圈)
;;; 功能说明:
;;;   1. 引导用户选择需要加圈的文字对象(支持 TEXT 与 MTEXT，可多选/框选)
;;;   2. 自动读取每个文字对象的外包框宽度与高度
;;;   3. 外径按包围盒最大对角线考虑：取外包框对角线长度作为基础尺寸
;;;      (对角线同时覆盖了文字的宽度和高度，避免多字符文本宽度超出圆圈)，
;;;      再按 "对角线 * (1 + extraRatio)" 预留富裕外径
;;;   4. 以文字外包框的几何中心为圆心，生成对应的外圆圈(CIRCLE)
;;;   5. 自动获取文字的颜色设置(含随层/随块/真彩色/颜色簿)，
;;;      原样复制到新生成的圆圈上，确保圆圈颜色与文字完全一致
;;; ============================================================

(defun c:QH ( / extraRatio ss ssCount i ent entData obj minp maxp cen
              h halfW halfH bboxW bboxH bboxDiag diameter r layerName
              circleEnt colorCode trueColorVal colorName doneCount)

  (vl-load-com)

  ;; 富裕量系数：外径 = 外包框对角线长度 * (1 + extraRatio)
  ;; extraRatio = 0.2 表示在(综合宽高得出的)基础尺寸上再预留20%作为外径余量
  (setq extraRatio 0.2)

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

           ;; 按包围盒最大对角线考虑：矩形包围盒只有一条对角线长度
           ;; (两条对角线等长)，取该对角线长度作为基础尺寸，
           ;; 可同时保证宽、高方向都被圆圈完整覆盖，
           ;; 不会因为多字符文本(如"12"/"88")宽度较大而超出圆圈范围。
           (setq bboxW (* halfW 2.0))
           (setq bboxH (* halfH 2.0))
           (setq bboxDiag (sqrt (+ (* bboxW bboxW) (* bboxH bboxH))))

           ;; 极少数情况下外包框宽高都取不到(bboxDiag为0)，
           ;; 兜底改用文字自身的"文字高度"字段(组码40)作为对角线基础尺寸
           (if (<= bboxDiag 0.0)
             (progn
               (setq h (cdr (assoc 40 entData)))
               (if (or (not h) (<= h 0.0))
                 (setq h bboxH)
               )
               (setq bboxDiag h)
             )
           )

           ;; 外径 = 包围盒对角线长度 * (1 + extraRatio)；半径 = 外径 / 2
           (setq diameter (* bboxDiag (+ 1.0 extraRatio)))
           (setq r (/ diameter 2.0))

           (if (> r 0.0)
             (progn
               (setq circleEnt
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

               ;; ---- 自动获取文字颜色，确保圆圈颜色与文字保持一致 ----
               ;; 直接复制源文字实体上的颜色相关 DXF 组码：
               ;;   62  = 颜色号(ACI，含 0=随块 / 256=随层)
               ;;   420 = 真彩色(RGB)
               ;;   430 = 颜色簿/颜色名称
               ;; 若文字本身未显式设置颜色(即随层)，这些组码不存在，
               ;; 圆圈与文字同图层即已保证颜色一致，无需额外处理。
               (setq colorCode (assoc 62 entData))
               (if colorCode
                 (setq circleEnt (append circleEnt (list colorCode)))
               )
               (setq trueColorVal (assoc 420 entData))
               (if trueColorVal
                 (setq circleEnt (append circleEnt (list trueColorVal)))
               )
               (setq colorName (assoc 430 entData))
               (if colorName
                 (setq circleEnt (append circleEnt (list colorName)))
               )

               (entmake circleEnt)
               (setq doneCount (1+ doneCount))
             )
           )
         )
       )

       (setq i (1+ i))
     )

     (princ
       (strcat "\n处理完成，共选中 " (itoa ssCount)
               " 个文字对象，成功添加 " (itoa doneCount)
               " 个外圆圈(颜色已自动匹配文字，外径按包围盒最大对角线预留"
               (rtos (* extraRatio 100.0) 2 0) "%)。")
     )
    )
  )

  (princ)
)

(princ "\n命令已加载：输入 QH 并回车，即可为选中的数字文本自动添加外圆圈(颜色与文字一致，外径按包围盒最大对角线计算)。")
(princ)
