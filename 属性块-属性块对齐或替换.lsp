;;;========================================================
;;; PPZ v4 - 属性块水平排列(底部对齐)
;;; PPD v1 - 属性块水平排列(顶部对齐)
;;; PPG v2 - 属性块顶部中心对齐复制(支持一对多)
;;; PPH v2 - 两个块位置互换(顶部中心对齐, 自动互换所属属性块值)
;;; Command: PPZ / PPD / PPG / PPH
;;;
;;; PPZ: 左下角对齐前一个的右下角(底对齐)
;;; PPD: 左上角对齐前一个的右上角(顶对齐)
;;; PPG: 选择1个源块，再框选多个目标块，
;;;      删除所有目标块，以顶部中心为基准复制源块到每个目标位置
;;; PPH: 选择两个块，以顶部中心为基准互换位置
;;;      交换前检测基点35半径内最近属性块，若两块分属不同属性块则同时互换属性值
;;;========================================================

(vl-load-com)

;;;--------------------------------------------------------
;;; 辅助函数: 直接获取实体包围盒(回退用)
;;; 返回: (minX minY maxX maxY) 或 nil
;;;--------------------------------------------------------
(defun PPZ:GetBBox (ent / obj result ll ur minpt maxpt)
  (setq obj (vlax-ename->vla-object ent))
  (setq result (vl-catch-all-apply 'vla-getboundingbox (list obj 'll 'ur)))
  (if (vl-catch-all-error-p result)
    nil
    (progn
      (setq minpt (vlax-safearray->list ll))
      (setq maxpt (vlax-safearray->list ur))
      (list (car minpt) (cadr minpt) (car maxpt) (cadr maxpt))
    )
  )
)


;;;--------------------------------------------------------
;;; 辅助函数: 获取块定义中图形实体的包围盒(排除ATTDEF)
;;; 返回: (minX minY maxX maxY) 或 nil
;;;--------------------------------------------------------
(defun PPZ:GetBlockDefBBox (blkDef / minX minY maxX maxY result ll ur minpt maxpt eName)
  (setq minX nil minY nil maxX nil maxY nil)
  (vlax-for e blkDef
    (setq eName (vla-get-ObjectName e))
    (if (and (/= eName "AcDbAttributeDefinition")
             (/= eName "AcDbBlockBegin")
             (/= eName "AcDbBlockEnd"))
      (progn
        (setq result (vl-catch-all-apply 'vla-getboundingbox (list e 'll 'ur)))
        (if (not (vl-catch-all-error-p result))
          (progn
            (setq minpt (vlax-safearray->list ll))
            (setq maxpt (vlax-safearray->list ur))
            (if (null minX)
              (setq minX (car minpt) minY (cadr minpt) maxX (car maxpt) maxY (cadr maxpt))
              (setq minX (min minX (car minpt))
                    minY (min minY (cadr minpt))
                    maxX (max maxX (car maxpt))
                    maxY (max maxY (cadr maxpt)))
            )
          )
        )
      )
    )
  )
  (if minX (list minX minY maxX maxY) nil)
)


;;;--------------------------------------------------------
;;; 辅助函数: 获取块参照在世界坐标系中的精确包围盒
;;; 返回: (minX minY maxX maxY) 或 nil
;;;--------------------------------------------------------
(defun PPZ:GetBlockBBox (ent / ed obj blkName doc blkDef bbox
                         insPt insX insY rot xs ys
                         cosR sinR
                         lx ly wx wy rx ry
                         minX minY maxX maxY)

  (setq ed (entget ent))
  (setq insPt (cdr (assoc 10 ed)))
  (setq insX (car insPt))
  (setq insY (cadr insPt))
  (setq rot (cdr (assoc 50 ed)))
  (if (null rot) (setq rot 0.0))
  (setq xs (cdr (assoc 41 ed)))
  (if (null xs) (setq xs 1.0))
  (setq ys (cdr (assoc 42 ed)))
  (if (null ys) (setq ys 1.0))
  (setq blkName (cdr (assoc 2 ed)))

  (if (or (null blkName) (= blkName ""))
    (PPZ:GetBBox ent)
    (progn
      (setq obj (vlax-ename->vla-object ent))
      (setq doc (vla-get-Document obj))
      (setq blkDef (vl-catch-all-apply 'vla-item (list (vla-get-Blocks doc) blkName)))
      (if (vl-catch-all-error-p blkDef)
        (setq blkDef nil))

      (setq bbox nil)
      (if blkDef
        (setq bbox (PPZ:GetBlockDefBBox blkDef)))

      (if (not bbox)
        (PPZ:GetBBox ent)
        (progn
          (setq cosR (cos rot))
          (setq sinR (sin rot))

          (setq minX nil minY nil maxX nil maxY nil)

          (foreach corner (list
            (list (nth 0 bbox) (nth 1 bbox))
            (list (nth 2 bbox) (nth 1 bbox))
            (list (nth 2 bbox) (nth 3 bbox))
            (list (nth 0 bbox) (nth 3 bbox)))

            (setq lx (car corner))
            (setq ly (cadr corner))

            (setq wx (* lx xs))
            (setq wy (* ly ys))

            (setq rx (+ (* wx cosR) (* wy (- sinR))))
            (setq ry (+ (* wx sinR) (* wy cosR)))

            (setq rx (+ rx insX))
            (setq ry (+ ry insY))

            (if (null minX)
              (setq minX rx minY ry maxX rx maxY ry)
              (setq minX (min minX rx)
                    minY (min minY ry)
                    maxX (max maxX rx)
                    maxY (max maxY ry))
            )
          )

          (list minX minY maxX maxY)
        )
      )
    )
  )
)


;;;--------------------------------------------------------
;;; 辅助函数: 获取块顶部中心坐标
;;; 返回: (centerX maxY) 或 nil
;;;--------------------------------------------------------
(defun PPZ:GetTopCenter (ent / bbox)
  (setq bbox (PPZ:GetBlockBBox ent))
  (if bbox
    (list (/ (+ (nth 0 bbox) (nth 2 bbox)) 2.0) (nth 3 bbox))
    nil
  )
)


;;;--------------------------------------------------------
;;; 辅助函数: 按 minX 升序排序(从左到右)
;;;--------------------------------------------------------
(defun PPZ:SortX (a b)
  (< (car a) (car b))
)


;;;========================================================
;;; 命令: PPZ - 属性块水平排列(底部对齐)
;;; 左下角对齐前一个的右下角
;;;========================================================
(defun c:PPZ (/ ss i ent bbox lst k currX currY offsetX offsetY obj newBBox)

  (prompt "\n请选择需要排列的属性块(底对齐): ")
  (setq ss (ssget '((0 . "INSERT"))))

  (if ss
    (progn

      (setq lst '())
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq bbox (PPZ:GetBlockBBox ent))
        (if bbox
          (setq lst
            (cons
              (list (nth 0 bbox) ent (nth 1 bbox) (nth 2 bbox) (nth 3 bbox))
              lst
            )
          )
        )
        (setq i (1+ i))
      )

      (if (>= (length lst) 2)
        (progn

          (setq lst (vl-sort lst 'PPZ:SortX))

          ;; 底对齐: currY = minY (索引2)
          (setq currX (nth 3 (nth 0 lst)))
          (setq currY (nth 2 (nth 0 lst)))

          (setq k 1)
          (repeat (- (length lst) 1)

            ;; 底对齐: offsetY = currY - minY (索引2)
            (setq offsetX (- currX (nth 0 (nth k lst))))
            (setq offsetY (- currY (nth 2 (nth k lst))))

            (setq ent (cadr (nth k lst)))
            (setq obj (vlax-ename->vla-object ent))
            (vla-Move obj
              (vlax-3d-point 0.0 0.0 0.0)
              (vlax-3d-point offsetX offsetY 0.0)
            )
            (vla-Update obj)
            (entupd ent)

            (setq newBBox (PPZ:GetBlockBBox ent))
            (if newBBox
              (setq currX (nth 2 newBBox))
              (setq currX (+ currX (- (nth 3 (nth k lst)) (nth 0 (nth k lst)))))
            )

            (setq k (1+ k))
          )

          (command "_.REGEN")

          (princ
            (strcat
              "\n排列完成(底对齐): 共排列 "
              (itoa (length lst))
              " 个属性块。"
            )
          )
        )
        (prompt "\n至少需要选择 2 个属性块才能排列。")
      )
    )
    (prompt "\n没有选择任何对象。")
  )

  (princ)
)


;;;========================================================
;;; 命令: PPD - 属性块水平排列(顶部对齐)
;;; 左上角对齐前一个的右上角
;;;========================================================
(defun c:PPD (/ ss i ent bbox lst k currX currY offsetX offsetY obj newBBox)

  (prompt "\n请选择需要排列的属性块(顶对齐): ")
  (setq ss (ssget '((0 . "INSERT"))))

  (if ss
    (progn

      (setq lst '())
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq bbox (PPZ:GetBlockBBox ent))
        (if bbox
          (setq lst
            (cons
              (list (nth 0 bbox) ent (nth 1 bbox) (nth 2 bbox) (nth 3 bbox))
              lst
            )
          )
        )
        (setq i (1+ i))
      )

      (if (>= (length lst) 2)
        (progn

          (setq lst (vl-sort lst 'PPZ:SortX))

          ;; 顶对齐: currY = maxY (索引4)
          (setq currX (nth 3 (nth 0 lst)))
          (setq currY (nth 4 (nth 0 lst)))

          (setq k 1)
          (repeat (- (length lst) 1)

            ;; 顶对齐: offsetY = currY - maxY (索引4)
            (setq offsetX (- currX (nth 0 (nth k lst))))
            (setq offsetY (- currY (nth 4 (nth k lst))))

            (setq ent (cadr (nth k lst)))
            (setq obj (vlax-ename->vla-object ent))
            (vla-Move obj
              (vlax-3d-point 0.0 0.0 0.0)
              (vlax-3d-point offsetX offsetY 0.0)
            )
            (vla-Update obj)
            (entupd ent)

            (setq newBBox (PPZ:GetBlockBBox ent))
            (if newBBox
              (setq currX (nth 2 newBBox))
              (setq currX (+ currX (- (nth 3 (nth k lst)) (nth 0 (nth k lst)))))
            )

            (setq k (1+ k))
          )

          (command "_.REGEN")

          (princ
            (strcat
              "\n排列完成(顶对齐): 共排列 "
              (itoa (length lst))
              " 个属性块。"
            )
          )
        )
        (prompt "\n至少需要选择 2 个属性块才能排列。")
      )
    )
    (prompt "\n没有选择任何对象。")
  )

  (princ)
)


;;;========================================================
;;; 命令: PPG - 属性块顶部中心对齐复制(一对多)
;;;
;;; 流程:
;;; 1. 选择源块 -> 获取顶部中心坐标 tc1
;;; 2. 框选多个目标块 -> 逐个获取顶部中心坐标
;;; 3. 逐个: 删除目标块，复制源块到该目标位置
;;;========================================================
(defun c:PPG (/ ent1 ss i ent tc1 tc2 offsetX offsetY obj1 newObj oldcmd cnt)

  (setq oldcmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  ;; 选择源块
  (setq ent1 (car (entsel "\n选择源块: ")))
  (if ent1
    (if (/= (cdr (assoc 0 (entget ent1))) "INSERT")
      (progn
        (prompt "\n所选对象不是块参照(INSERT)，请重试。")
        (setvar "CMDECHO" oldcmd)
        (princ)
        (exit)
      )
    )
    (progn
      (prompt "\n未选择任何对象。")
      (setvar "CMDECHO" oldcmd)
      (princ)
      (exit)
    )
  )

  ;; 获取源块顶部中心坐标
  (setq tc1 (PPZ:GetTopCenter ent1))
  (if (null tc1)
    (progn
      (prompt "\n无法获取源块包围盒。")
      (setvar "CMDECHO" oldcmd)
      (princ)
      (exit)
    )
  )

  ;; 框选多个目标块
  (prompt "\n选择需要替换的目标块(可多选): ")
  (setq ss (ssget '((0 . "INSERT"))))

  (if ss
    (progn

      (setq cnt 0)
      (setq i 0)

      ;; 逐个处理目标块
      (repeat (sslength ss)
        (setq ent (ssname ss i))

        ;; 跳过源块自身(如果误选)
        (if (/= ent ent1)
          (progn
            ;; 获取目标块顶部中心坐标
            (setq tc2 (PPZ:GetTopCenter ent))
            (if tc2
              (progn
                ;; 计算偏移量: tc2 - tc1
                (setq offsetX (- (car tc2) (car tc1)))
                (setq offsetY (- (cadr tc2) (cadr tc1)))

                ;; 删除目标块
                (entdel ent)

                ;; 复制源块并移动到目标位置
                (setq obj1 (vlax-ename->vla-object ent1))
                (setq newObj (vla-Copy obj1))
                (vla-Move newObj
                  (vlax-3d-point 0.0 0.0 0.0)
                  (vlax-3d-point offsetX offsetY 0.0)
                )
                (vla-Update newObj)

                (setq cnt (1+ cnt))
              )
            )
          )
        )

        (setq i (1+ i))
      )

      ;; 刷新显示
      (command "_.REGEN")

      (setvar "CMDECHO" oldcmd)

      (princ
        (strcat
          "\n替换完成: 以顶部中心为基准，共替换 "
          (itoa cnt)
          " 个目标块。"
        )
      )
    )
    (progn
      (prompt "\n未选择任何目标块。")
      (setvar "CMDECHO" oldcmd)
    )
  )

  (princ)
)


;;;--------------------------------------------------------
;;; 辅助函数: 获取属性块的 (TAG . VAL) 点对列表
;;; 参数: ent - INSERT 实体名(带属性)
;;; 返回: ((TAG1 . VAL1) (TAG2 . VAL2) ...) 或 nil
;;;--------------------------------------------------------
(defun PPH:GetAttribAlist (ent / attr ed result)
  (setq attr (entnext ent) result nil)
  (while (and attr
              (= (cdr (assoc 0 (setq ed (entget attr)))) "ATTRIB"))
    (setq result (cons (cons (cdr (assoc 2 ed)) (cdr (assoc 1 ed))) result))
    (setq attr (entnext attr)))
  (reverse result))


;;;--------------------------------------------------------
;;; 辅助函数: 按 TAG 匹配设置属性值
;;; 参数: ent  - 目标块实体名
;;;       tags - TAG 名称列表
;;;       vals - 对应的值列表
;;;--------------------------------------------------------
(defun PPH:SetAttrValues (ent tags vals / attr ed tag pos val)
  (setq attr (entnext ent))
  (while (and attr
              (= (cdr (assoc 0 (setq ed (entget attr)))) "ATTRIB"))
    (setq tag (cdr (assoc 2 ed)))
    (setq pos (vl-position tag tags))
    (if pos
      (progn
        (setq val (nth pos vals))
        (if (null val) (setq val ""))
        (setq ed (subst (cons 1 val) (assoc 1 ed) ed))
        (entmod ed)))
    (setq attr (entnext attr)))
  (entupd ent))


;;;--------------------------------------------------------
;;; 辅助函数: 计算点到包围盒的距离
;;; 点在包围盒内返回0, 否则返回到最近边的距离
;;; 参数: pt   - 点 (x y)
;;;       bbox - (minX minY maxX maxY)
;;; 返回: 距离值
;;;--------------------------------------------------------
(defun PPH:PointToBBoxDist (pt bbox / px py minX minY maxX maxY dx dy)
  (setq px (car pt) py (cadr pt))
  (setq minX (nth 0 bbox) minY (nth 1 bbox)
        maxX (nth 2 bbox) maxY (nth 3 bbox))
  (cond
    ((< px minX) (setq dx (- minX px)))
    ((> px maxX) (setq dx (- px maxX)))
    (t (setq dx 0.0)))
  (cond
    ((< py minY) (setq dy (- minY py)))
    ((> py maxY) (setq dy (- py maxY)))
    (t (setq dy 0.0)))
  (sqrt (+ (* dx dx) (* dy dy))))


;;;--------------------------------------------------------
;;; 辅助函数: 寻找基点所属的最近属性块
;;; 以基点到属性块包围盒的距离为判断依据:
;;;   - 基点在包围盒内 -> 距离0(最佳匹配)
;;;   - 基点在包围盒外 -> 到最近边的距离
;;; 搜索全图所有带属性的块参照, 取距离<=radius的最近一个
;;; 参数: pt     - 基准点 (x y)
;;;       radius - 搜索半径
;;;       excl1  - 排除实体1
;;;       excl2  - 排除实体2
;;; 返回: 最近的属性块实体名 或 nil
;;;--------------------------------------------------------
(defun PPH:FindNearestAttrBlock (pt radius excl1 excl2 /
                                  ss i ent bbox dist bestEnt bestDist)
  (setq ss (ssget "X" '((0 . "INSERT") (66 . 1))))
  (if ss
    (progn
      (setq bestEnt nil bestDist nil i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq i (1+ i))
        (if (and (/= ent excl1) (/= ent excl2))
          (progn
            (setq bbox (PPZ:GetBlockBBox ent))
            (if bbox
              (progn
                (setq dist (PPH:PointToBBoxDist pt bbox))
                (if (<= dist radius)
                  (if (or (null bestDist) (< dist bestDist))
                    (setq bestEnt ent bestDist dist))))))))
      bestEnt)
    nil))


;;;========================================================
;;; 命令: PPH v2 - 两个块位置互换(顶部中心对齐)
;;;
;;; 增强功能: 交换前检测每个块基点35半径内最近的属性块,
;;; 若两个块分别属于不同的属性块,则交换位置后同时对
;;; 这两个属性块执行属性值互换(TZV逻辑)
;;;
;;; 流程:
;;; 1. 选择块1 -> 获取顶部中心坐标 tc1(基点)
;;; 2. 选择块2 -> 获取顶部中心坐标 tc2(基点)
;;; 3. 交换前: 以tc1搜索35内最近属性块A, 以tc2搜索35内最近属性块B
;;; 4. 块1以顶部中心为基准移动到tc2, 块2移动到tc1
;;; 5. 若属性块A和B都存在且不同 -> 互换两者的属性值
;;;========================================================
(defun c:PPH (/ ent1 ent2 tc1 tc2 offX offY obj1 obj2 oldcmd
               attrA attrB alistA alistB)

  (setq oldcmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  ;; 选择块1
  (setq ent1 (car (entsel "\n选择第一个块: ")))
  (if ent1
    (if (/= (cdr (assoc 0 (entget ent1))) "INSERT")
      (progn
        (prompt "\n所选对象不是块参照(INSERT)，请重试。")
        (setvar "CMDECHO" oldcmd)
        (princ)
        (exit)
      )
    )
    (progn
      (prompt "\n未选择任何对象。")
      (setvar "CMDECHO" oldcmd)
      (princ)
      (exit)
    )
  )

  ;; 选择块2
  (setq ent2 (car (entsel "\n选择第二个块: ")))
  (if ent2
    (if (/= (cdr (assoc 0 (entget ent2))) "INSERT")
      (progn
        (prompt "\n所选对象不是块参照(INSERT)，请重试。")
        (setvar "CMDECHO" oldcmd)
        (princ)
        (exit)
      )
    )
    (progn
      (prompt "\n未选择任何对象。")
      (setvar "CMDECHO" oldcmd)
      (princ)
      (exit)
    )
  )

  ;; 获取两个块的顶部中心坐标(基点)
  (setq tc1 (PPZ:GetTopCenter ent1))
  (if (null tc1)
    (progn
      (prompt "\n无法获取第一个块包围盒。")
      (setvar "CMDECHO" oldcmd)
      (princ)
      (exit)
    )
  )

  (setq tc2 (PPZ:GetTopCenter ent2))
  (if (null tc2)
    (progn
      (prompt "\n无法获取第二个块包围盒。")
      (setvar "CMDECHO" oldcmd)
      (princ)
      (exit)
    )
  )

  ;; 交换前: 搜索基点35半径内最近的属性块
  (setq attrA (PPH:FindNearestAttrBlock tc1 35.0 ent1 ent2))
  (setq attrB (PPH:FindNearestAttrBlock tc2 35.0 ent1 ent2))

  (if attrA
    (prompt (strcat "\n块1附近找到属性块(句柄: "
                    (cdr (assoc 5 (entget attrA))) ")"))
    (prompt "\n块1附近35半径内未找到属性块。"))

  (if attrB
    (prompt (strcat "\n块2附近找到属性块(句柄: "
                    (cdr (assoc 5 (entget attrB))) ")"))
    (prompt "\n块2附近35半径内未找到属性块。"))

  ;; 计算互换偏移量(基于原始位置)
  (setq offX (- (car tc2) (car tc1)))
  (setq offY (- (cadr tc2) (cadr tc1)))

  ;; 块1移动到块2的位置
  (setq obj1 (vlax-ename->vla-object ent1))
  (vla-Move obj1
    (vlax-3d-point 0.0 0.0 0.0)
    (vlax-3d-point offX offY 0.0)
  )
  (vla-Update obj1)
  (entupd ent1)

  ;; 块2移动到块1的原始位置(反向偏移)
  (setq obj2 (vlax-ename->vla-object ent2))
  (vla-Move obj2
    (vlax-3d-point 0.0 0.0 0.0)
    (vlax-3d-point (- offX) (- offY) 0.0)
  )
  (vla-Update obj2)
  (entupd ent2)

  ;; 判断是否执行属性值互换(TZV)
  (if (and attrA attrB (not (equal attrA attrB)))
    (progn
      ;; 读取两个属性块的数据
      (setq alistA (PPH:GetAttribAlist attrA))
      (setq alistB (PPH:GetAttribAlist attrB))
      (if (and alistA alistB)
        (progn
          ;; 交换写入: A的值给B, B的值给A
          (PPH:SetAttrValues attrB (mapcar 'car alistA) (mapcar 'cdr alistA))
          (PPH:SetAttrValues attrA (mapcar 'car alistB) (mapcar 'cdr alistB))
          (prompt "\n属性值已互换(TZV)。"))
        (prompt "\n属性块无属性数据，跳过属性值互换。")))
    (prompt "\n条件不齐全，仅执行位置互换。"))

  ;; 刷新显示
  (command "_.REGEN")

  (setvar "CMDECHO" oldcmd)

  (princ
    (strcat
      "\n位置互换完成。"
      "\n偏移量: X=" (rtos offX 2 3) " Y=" (rtos offY 2 3)
    )
  )

  (princ)
)


(princ "\nPPZ v4 / PPD v1 / PPG v2 / PPH v2 已加载。输入 PPZ 底对齐排列，输入 PPD 顶对齐排列，输入 PPG 顶部中心复制(一对多)，输入 PPH 位置互换(自动检测属性块并互换属性值)。")
(princ)
