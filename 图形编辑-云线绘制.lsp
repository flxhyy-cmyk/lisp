;;; ============================================================
;;; YunXian.lsp
;;; 功能：根据用户框选的矩形区域，自动绘制修订云线（Revision Cloud）
;;;       弧的弦长（波浪比例）根据框选区域的对角线长度自适应计算，
;;;       并用最小/最大弧数做双重限制，保证无论区域大小、长宽比
;;;       如何变化，云线波浪都保持在"看得清、不过密、不过疏"的
;;;       合理范围内。
;;;
;;; 命令：YUNXIAN（完整名） / YX（简写别名）
;;;
;;; 可调全局参数（运行前用 (setq ...) 覆盖默认值即可自定义）：
;;;   *WD:CloudRatio*        弦长 = 对角线 * 该比例，默认 0.105（0.035的3倍）
;;;   *WD:CloudMinArcs*      弧段数下限，默认 6
;;;   *WD:CloudMaxArcs*      弧段数上限，默认 120
;;;   *WD:CloudBulgeFactor*  弧的凸度系数（越大弧越鼓），默认 0.55
;;;   *WD:BulgeSign*         凸度方向，默认 -1.0（向外鼓）
;;;                          如果画出来的弧朝里凹，改成 1.0 即可反向
;;;   *WD:CloudColor*        颜色索引（ACI），默认 1（红色）
;;; ============================================================

;; 数值限幅工具函数
(defun WD:Clamp (v lo hi)
  (cond
    ((< v lo) lo)
    ((> v hi) hi)
    (t v)
  )
)

;; 生成矩形边界路径（闭合，按逆时针：左下->右下->右上->左上->回到左下）
(defun WD:RectPath (xmin ymin xmax ymax z)
  (list
    (list xmin ymin z)
    (list xmax ymin z)
    (list xmax ymax z)
    (list xmin ymax z)
    (list xmin ymin z)
  )
)

;; 在闭合折线路径 path 上，取距起点弧长为 dist 处的点（线性插值）
(defun WD:PtAtDist (path dist / plist p0 p1 seglen acc pct pt)
  (setq plist path)
  (setq acc 0.0)
  (setq pt nil)
  (while (and (not pt) (cdr plist))
    (setq p0 (car plist))
    (setq p1 (cadr plist))
    (setq seglen (distance p0 p1))
    (if (<= dist (+ acc seglen))
      (progn
        (setq pct (if (> seglen 1e-9) (/ (- dist acc) seglen) 0.0))
        (setq pt
          (list
            (+ (car p0) (* pct (- (car p1) (car p0))))
            (+ (cadr p0) (* pct (- (cadr p1) (cadr p0))))
            (+ (caddr p0) (* pct (- (caddr p1) (caddr p0))))
          )
        )
      )
      (progn
        (setq acc (+ acc seglen))
        (setq plist (cdr plist))
      )
    )
  )
  pt
)

;; 主命令
(defun C:YUNXIAN ( / p1 p2 xmin xmax ymin ymax zval w h shortside
                      perim diag basechord chordlo chordhi chordv
                      arcn chord bulge path pts k dist pt
                      vtxlist entlist)

  (defun *error* (msg)
    (if (and msg
             (/= msg "Function cancelled")
             (/= msg "quit / exit abort")
        )
      (princ (strcat "\n云线绘制中断：" msg))
    )
    (princ)
  )

  ;; 首次加载时初始化默认参数（已设置过的不会被覆盖）
  (if (not *WD:CloudRatio*) (setq *WD:CloudRatio* 0.105))
  (if (not *WD:CloudMinArcs*) (setq *WD:CloudMinArcs* 6))
  (if (not *WD:CloudMaxArcs*) (setq *WD:CloudMaxArcs* 120))
  (if (not *WD:CloudBulgeFactor*) (setq *WD:CloudBulgeFactor* 0.55))
  (if (not *WD:BulgeSign*) (setq *WD:BulgeSign* -1.0))
  (if (not *WD:CloudColor*) (setq *WD:CloudColor* 1))

  (setq p1 (getpoint "\n指定云线区域第一角点: "))
  (if p1
    (setq p2 (getcorner p1 "\n指定对角点: "))
  )

  (cond

    ((not (and p1 p2))
     (princ "\n已取消，未指定有效区域。")
    )

    (t
     (setq xmin (min (car p1) (car p2)))
     (setq xmax (max (car p1) (car p2)))
     (setq ymin (min (cadr p1) (cadr p2)))
     (setq ymax (max (cadr p1) (cadr p2)))
     (setq zval (if (caddr p1) (caddr p1) 0.0))
     (setq w (- xmax xmin))
     (setq h (- ymax ymin))
     (setq diag (sqrt (+ (* w w) (* h h))))

     (if (< diag 1e-6)
       (princ "\n所选区域过小或无效，已取消绘制。")

       (progn
         (setq perim (* 2.0 (+ w h)))
         (setq shortside (min w h))

         ;; 基准弦长：区域对角线的固定比例
         (setq basechord (* diag *WD:CloudRatio*))

         ;; 弦长上下限：由弧数上下限反推，保证弧段数量不会过多或过少
         (setq chordlo (/ perim (float *WD:CloudMaxArcs*)))
         (setq chordhi (/ perim (float *WD:CloudMinArcs*)))

         ;; 再叠加一层限制：弦长不超过短边的90%，避免细长区域时
         ;; 弧过大导致短边方向看不出波浪
         (setq chordhi (min chordhi (* shortside 0.9)))
         (if (> chordlo chordhi) (setq chordlo (* chordhi 0.5)))

         (setq chordv (WD:Clamp basechord chordlo chordhi))

         ;; 由弦长反推弧段数量，四舍五入取整
         (setq arcn (fix (+ 0.5 (/ perim chordv))))
         (if (< arcn *WD:CloudMinArcs*) (setq arcn *WD:CloudMinArcs*))
         (if (> arcn *WD:CloudMaxArcs*) (setq arcn *WD:CloudMaxArcs*))

         ;; 用最终弧段数重新计算精确弦长，保证首尾均匀闭合
         (setq chord (/ perim (float arcn)))

         ;; bulge 是无量纲比值，会随弦长自动等比例缩放弧高，
         ;; 因此无需再单独按尺寸调整凸度
         (setq bulge (* *WD:CloudBulgeFactor* *WD:BulgeSign*))

         (setq path (WD:RectPath xmin ymin xmax ymax zval))

         (setq pts nil)
         (setq k 0)
         (while (< k arcn)
           (setq dist (* (float k) chord))
           (setq pt (WD:PtAtDist path dist))
           (setq pts (append pts (list pt)))
           (setq k (1+ k))
         )

         (setq vtxlist nil)
         (foreach pt pts
           (setq vtxlist
             (append vtxlist
               (list (cons 10 (list (car pt) (cadr pt))) (cons 42 bulge))
             )
           )
         )

         (setq entlist
           (append
             (list
               (cons 0 "LWPOLYLINE")
               (cons 100 "AcDbEntity")
               (cons 8 (getvar "CLAYER"))
               (cons 62 *WD:CloudColor*)
               (cons 100 "AcDbPolyline")
               (cons 90 arcn)
               (cons 70 1)
               (cons 38 zval)
             )
             vtxlist
           )
         )

         (entmake entlist)

         (princ
           (strcat
             "\n云线绘制完成：共 " (itoa arcn) " 段弧，弦长约 "
             (rtos chord 2 2) "，区域对角线 " (rtos diag 2 2)
           )
         )
       )
     )
    )
  )
  (princ)
)

;; 简写别名
(defun C:YX () (C:YUNXIAN))

(princ "\n已加载云线命令，输入 YUNXIAN 或 YX 执行。")
(princ)
