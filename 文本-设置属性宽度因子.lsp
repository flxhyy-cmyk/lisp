;;; ============================================================
;;; 功能：批量修改选中对象中文字的宽度因子
;;;       支持：普通单行文字、块参照的属性文字、以及块定义内部的
;;;             非属性文字（普通 TEXT / 属性定义 ATTDEF，含嵌套块）
;;; 命令：SETATTWIDTH
;;; 用法：加载后在命令行输入 SETATTWIDTH，
;;;       1) 输入宽度因子（直接回车则默认使用 0.7）；
;;;       2) 选择需要修改的对象（可框选，支持属性块和普通文字，可混选）；
;;;       选择完成后按回车即可批量执行。
;;; 注意：块内部的非属性文字属于“块定义”的一部分，修改后该块的
;;;       所有引用都会同步改变，这是 AutoCAD 的固有机制。
;;;       外部参照(XREF)内部的文字不做修改。
;;; ============================================================

;; 直接改写实体 DXF 组码 41（宽度因子），成功返回 T
;; 只对 TEXT / ATTRIB / ATTDEF 生效；MTEXT 的 41 是框宽，不能动
(defun wf:put-width (ent wf / ed)
  (setq ed (entget ent))
  (if (assoc 41 ed)
    (progn
      (entmod (subst (cons 41 wf) (assoc 41 ed) ed))
      T
    )
    nil
  )
)

;; 处理块参照上的属性（ATTRIB 是块参照的子实体，随参照走）
(defun wf:do-attributes (ent wf / sub cnt ed)
  (setq cnt 0)
  (setq sub (entnext ent))
  (while (and sub
              (setq ed (entget sub))
              (/= (cdr (assoc 0 ed)) "SEQEND")
         )
    (if (= (cdr (assoc 0 ed)) "ATTRIB")
      (if (wf:put-width sub wf) (setq cnt (1+ cnt)))
    )
    (setq sub (entnext sub))
  )
  (entupd ent)
  cnt
)

;; 递归处理块定义内部的非属性文字（TEXT / ATTDEF），含嵌套块
;; 依赖动态作用域中的 wf:seen 防止重复处理同一块定义
(defun wf:do-blockdef (bname wf / tbl bent ed etype cnt flag)
  (setq cnt 0)
  (if (and bname
           (not (member (strcase bname) wf:seen))
           (setq tbl (tblsearch "BLOCK" bname))
      )
    (progn
      (setq wf:seen (cons (strcase bname) wf:seen))
      (setq flag (cdr (assoc 70 tbl)))
      (if (null flag) (setq flag 0))
      ;; 位 4(值4)/位 5(值8) = 外部参照及其依赖，跳过不改
      (if (zerop (logand flag 12))
        (progn
          (setq bent (entnext (tblobjname "BLOCK" bname)))
          (while (and bent
                      (setq ed (entget bent))
                      (/= (cdr (assoc 0 ed)) "ENDBLK")
                 )
            (setq etype (cdr (assoc 0 ed)))
            (cond
              ((or (= etype "TEXT") (= etype "ATTDEF"))
               (if (wf:put-width bent wf) (setq cnt (1+ cnt)))
              )
              ((= etype "INSERT")
               (setq cnt (+ cnt (wf:do-blockdef (cdr (assoc 2 ed)) wf)))
              )
            )
            (setq bent (entnext bent))
          )
        )
      )
    )
  )
  cnt
)

(defun c:SETATTWIDTH (/ wf ss i n ent ed etype cnt cntb wf:seen)
  (vl-load-com)
  (setq cnt 0 cntb 0 wf:seen nil)

  ;; 交互获取宽度因子，直接回车则使用默认值 0.7
  (initget 6) ;; 禁止输入 0 或负数
  (setq wf (getreal "\n请输入宽度因子 <0.7>: "))
  (if (null wf) (setq wf 0.7))

  (princ "\n请选择需要修改的对象（属性块 / 普通文字，可框选多个），选择完成后按回车确认：")
  (setq ss (ssget '((-4 . "<OR")
                     (0 . "INSERT")
                     (0 . "TEXT")
                     (0 . "ATTDEF")
                     (-4 . "OR>")
                   )
           )
  )

  (if ss
    (progn
      (setq n (sslength ss))
      (setq i 0)
      (while (< i n)
        (setq ent (ssname ss i))
        (setq ed (entget ent))
        (setq etype (cdr (assoc 0 ed)))
        (cond
          ;; 块参照：1) 改属性文字  2) 进块定义改非属性文字
          ((= etype "INSERT")
           (setq cnt (+ cnt (wf:do-attributes ent wf)))
           (setq cntb (+ cntb (wf:do-blockdef (cdr (assoc 2 ed)) wf)))
           (entupd ent)
          )
          ;; 模型空间里的普通文字 / 属性定义
          ((or (= etype "TEXT") (= etype "ATTDEF"))
           (if (wf:put-width ent wf)
             (progn
               (entupd ent)
               (setq cnt (1+ cnt))
             )
           )
          )
        )
        (setq i (1+ i))
      )
      ;; 块定义被改动后必须重生成，所有引用才会在屏幕上立即刷新
      (if (> (+ cnt cntb) 0)
        (vla-Regen (vla-get-ActiveDocument (vlax-get-acad-object)) acAllViewports)
      )
      (princ (strcat "\n处理完成：属性/独立文字 " (itoa cnt)
                     " 处，块内非属性文字 " (itoa cntb)
                     " 处，宽度因子已设为 " (rtos wf 2 2) "。"))
      (if (> cntb 0)
        (princ "\n提示：块内文字属于块定义，同名块的其它引用也会同步改变。")
      )
    )
    (princ "\n未选择任何对象，命令已取消。")
  )

  (princ)
)

(princ "\n已加载命令：SETATTWIDTH —— 批量修改属性文字/块内非属性文字/普通文字的宽度因子（默认0.7）")
(princ)
