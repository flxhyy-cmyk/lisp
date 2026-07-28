;;;========================================================
;;; WLP v5 - 属性排序 + 自动建块
;;; Command: WLP (属性排序 + 建块)
;;; Command: MB  (仅建块)
;;;
;;; 功能:
;;; 1. 用户框选对象（支持所有类型）
;;; 2. 自动提取 ATTDEF，按 Y 坐标重排属性顺序
;;; 3. 重排完成后，将所有选中对象自动创建为块
;;; 4. MB 命令可单独使用，仅执行建块功能
;;;
;;; 建块说明:
;;; - 块名自动生成: BLK_YYYYMMDD_HHMMSS
;;; - 基点自动计算: 所选对象包围盒左下角
;;; - 创建后自动删除原对象并插入块参照
;;;========================================================

(vl-load-com)

;;;--------------------------------------------------------
;;; 辅助函数: 十六进制转整数
;;;--------------------------------------------------------
(defun WLP:HexToInt (s / chars result i pos)
  (setq chars "0123456789ABCDEF")
  (setq result 0)
  (setq i 1)
  (setq s (strcase s))

  (while (<= i (strlen s))
    (setq pos (vl-string-search (substr s i 1) chars))
    (if pos
      (setq result (+ (* result 16) pos))
    )
    (setq i (1+ i))
  )
  result
)


;;;--------------------------------------------------------
;;; 辅助函数: 获取实体句柄(整数)
;;;--------------------------------------------------------
(defun WLP:GetHandle (ent)
  (WLP:HexToInt (cdr (assoc 5 (entget ent))))
)


;;;--------------------------------------------------------
;;; 辅助函数: 移动实体到指定点
;;;--------------------------------------------------------
(defun WLP:SetPoint (ent pt / obj old10)
  (setq obj (vlax-ename->vla-object ent))
  (setq old10 (cdr (assoc 10 (entget ent))))
  (vla-Move obj (vlax-3d-point old10) (vlax-3d-point pt))
  (vla-Update obj)
  (entupd ent)
)


;;;--------------------------------------------------------
;;; 辅助函数: 按句柄降序排序
;;;--------------------------------------------------------
(defun WLP:Sort (a b)
  (> (car a) (car b))
)


;;;--------------------------------------------------------
;;; 辅助函数: 按 Y 坐标降序排序
;;;--------------------------------------------------------
(defun WLP:SortY (a b / ya yb)
  (setq ya (nth 3 a))
  (setq yb (nth 3 b))
  (if (equal ya yb 0.0001)
    (< (car (nth 4 a)) (car (nth 4 b)))
    (> ya yb)
  )
)


;;;--------------------------------------------------------
;;; 辅助函数: 设置 ATTDEF 内容
;;;--------------------------------------------------------
(defun WLP:SetContent (ent tag prompt value height width align / obj)
  (setq obj (vlax-ename->vla-object ent))
  (vla-put-TagString obj tag)
  (vla-put-PromptString obj prompt)
  (vla-put-TextString obj value)
  (vla-put-Height obj height)
  (vla-put-ScaleFactor obj width)
  (vla-put-Alignment obj align)
  (vla-Update obj)
  (entupd ent)
)


;;;--------------------------------------------------------
;;; 辅助函数: 生成块名 BLK_YYYYMMDD_HHMMSS
;;; 使用 CAD 系统时间变量 DATE 通过 edtime 格式化
;;;--------------------------------------------------------
(defun WLP:MakeBlockName (/ tm)
  (setq tm (menucmd "M=$(edtime,$(getvar,date),YYYYMODD_HHMMSS)"))
  (strcat "BLK_" tm)
)


;;;--------------------------------------------------------
;;; 辅助函数: 计算选择集包围盒
;;; 参数: ss - 选择集
;;; 返回: (MinX MinY MaxX MaxY) 或 nil
;;;--------------------------------------------------------
(defun WLP:GetMinMax (ss / i ent obj result ll ur minpt maxpt minx miny maxx maxy)
  (setq i 0 minx nil miny nil maxx nil maxy nil)
  (repeat (sslength ss)
    (setq ent (ssname ss i))
    (setq obj (vlax-ename->vla-object ent))
    (setq result (vl-catch-all-apply 'vla-getboundingbox (list obj 'll 'ur)))
    (if (not (vl-catch-all-error-p result))
      (progn
        (setq minpt (vlax-safearray->list ll))
        (setq maxpt (vlax-safearray->list ur))
        (if (null minx)
          (setq minx (car minpt)
                miny (cadr minpt)
                maxx (car maxpt)
                maxy (cadr maxpt))
          (setq minx (min minx (car minpt))
                miny (min miny (cadr minpt))
                maxx (max maxx (car maxpt))
                maxy (max maxy (cadr maxpt)))
        )
      )
    )
    (setq i (1+ i))
  )
  (if minx
    (list minx miny maxx maxy)
    nil
  )
)


;;;--------------------------------------------------------
;;; 辅助函数: 创建块
;;; 参数: ss - 选择集
;;; 返回: 块名(字符串) 或 nil(失败)
;;;
;;; 流程:
;;; 1. 生成唯一块名
;;; 2. 计算包围盒左下角作为基点
;;; 3. 用 -BLOCK 创建块定义(自动删除原对象)
;;; 4. 用 -INSERT 插入块参照到原位置
;;;--------------------------------------------------------
(defun WLP:CreateBlock (ss / blkname bbox basept oldcmd olderr)
  (setq oldcmd (getvar "CMDECHO"))
  (setq olderr *error*)

  (defun *error* (msg)
    (setvar "CMDECHO" oldcmd)
    (setq *error* olderr)
    (if (not (wcmatch (strcase msg t) "*break*,*cancel*,*exit*"))
      (princ (strcat "\n建块失败: " msg))
    )
    (princ)
  )

  ;; 生成唯一块名
  (setq blkname (WLP:MakeBlockName))
  (while (tblsearch "BLOCK" blkname)
    (setq blkname (WLP:MakeBlockName))
  )

  ;; 计算包围盒
  (setq bbox (WLP:GetMinMax ss))
  (if (null bbox)
    (progn
      (setvar "CMDECHO" oldcmd)
      (setq *error* olderr)
      (princ "\n无法计算包围盒，建块失败。")
      nil
    )
    (progn
      (setq basept (list (nth 0 bbox) (nth 1 bbox) 0.0))

      ;; 关闭命令回显
      (setvar "CMDECHO" 0)

      ;; 创建块定义(原对象自动删除)
      (command "_.-BLOCK" blkname basept ss "")

      ;; 插入块参照到原位置
      (command "_.-INSERT" blkname basept 1 1 0)

      ;; 恢复设置
      (setvar "CMDECHO" oldcmd)
      (setq *error* olderr)

      (princ (strcat "\n块创建完成: " blkname))
      blkname
    )
  )
)


;;;========================================================
;;; 主命令: WLP - 属性排序 + 自动建块
;;;
;;; 流程:
;;; 1. 框选所有对象
;;; 2. 提取 ATTDEF 子集进行 Y 坐标排序
;;; 3. 排序完成后将所有对象创建为块
;;;========================================================
(defun c:WLP (/ ss ssAtt i ent obj data lst lstY lstH content k pt)

  (prompt "\n请选择对象（含属性定义）: ")
  (setq ss (ssget))

  (if ss
    (progn

      ;; 提取 ATTDEF 子集
      (setq ssAtt (ssadd))
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (if (= (cdr (assoc 0 (entget ent))) "ATTDEF")
          (ssadd ent ssAtt)
        )
        (setq i (1+ i))
      )

      ;; 属性排序（如果存在 ATTDEF）
      (if (> (sslength ssAtt) 0)
        (progn

          (setq lst '())
          (setq i 0)

          ;; 读取每个 ATTDEF 的数据
          (repeat (sslength ssAtt)
            (setq ent (ssname ssAtt i))
            (setq obj (vlax-ename->vla-object ent))
            (setq data (entget ent))

            (setq lst
              (cons
                (list
                  (WLP:GetHandle ent)
                  ent
                  (vla-get-Height obj)
                  (cadr (cdr (assoc 10 data)))
                  (cdr (assoc 10 data))
                  (cdr (assoc 2 data))
                  (cdr (assoc 3 data))
                  (cdr (assoc 1 data))
                  (if (assoc 41 data) (cdr (assoc 41 data)) 1.0)
                  (vla-get-Alignment obj)
                )
                lst
              )
            )

            (setq i (1+ i))
          )

          ;; 按 Y 坐标排序
          (setq lstY (vl-sort lst 'WLP:SortY))

          ;; 按 Handle 排序
          (setq lstH (vl-sort lst 'WLP:Sort))

          ;; 内容重分配: 第k个(Handle序) <- 第k个(Y序)的内容
          (setq k 0)
          (repeat (length lstH)
            (setq ent (cadr (nth k lstH)))
            (setq content (nth k lstY))
            (WLP:SetContent
              ent
              (nth 5 content)
              (nth 6 content)
              (nth 7 content)
              (nth 2 content)
              (nth 8 content)
              (nth 9 content)
            )
            (setq k (1+ k))
          )

          ;; 位置重分配: 第k个(Handle序)移到第k个(Y序)的位置
          (setq k 0)
          (repeat (length lstH)
            (setq ent (cadr (nth k lstH)))
            (setq pt (nth 4 (nth k lstY)))
            (WLP:SetPoint ent pt)
            (setq k (1+ k))
          )

          ;; 刷新显示
          (command "_.REGEN")

          (princ
            (strcat
              "\n属性排序完成: 共处理 "
              (itoa (length lstH))
              " 个属性定义。"
            )
          )
        )
        (prompt "\n未选中 ATTDEF，跳过属性排序。")
      )

      ;; 自动建块（使用全部选中对象）
      (WLP:CreateBlock ss)
    )
    (prompt "\n没有选择任何对象。")
  )

  (princ)
)


;;;========================================================
;;; 独立命令: MB - 仅自动建块
;;;
;;; 流程:
;;; 1. 框选对象
;;; 2. 自动生成块名、计算基点
;;; 3. 创建块并替换原对象
;;;========================================================
(defun c:MB (/ ss)

  (prompt "\n请选择需要创建块的对象: ")
  (setq ss (ssget))

  (if ss
    (WLP:CreateBlock ss)
    (prompt "\n没有选择任何对象。")
  )

  (princ)
)


(princ "\nWLP v5 已加载，输入 WLP 进行属性排序+建块，输入 MB 仅建块。")
(princ)
