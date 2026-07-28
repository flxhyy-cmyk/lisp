;@name 标注值转文本
;@group 文本编辑
;@desc 选择标注获取尺寸值，再选择文本将尺寸值赋予该文本
(defun c:ZSR (/ dim-ent dim-ed dim-txt dim-val val-str result
               txt-ent txt-ed txt-type idx pos1 pos2 ch)
  ;; ---- 1. 选择标注 ----
  (setq dim-ent (car (entsel "\n选择标注: ")))
  (if dim-ent
    (progn
      (setq dim-ed (entget dim-ent))
      (if (wcmatch (cdr (assoc 0 dim-ed)) "DIMENSION*")
        (progn
          ;; ---- 2. 获取尺寸文本 ----
          (setq dim-txt (cdr (assoc 1 dim-ed)))
          (setq dim-val (cdr (assoc 42 dim-ed)))
          (setq val-str (rtos dim-val 2 (getvar "DIMDEC")))

          (cond
            ;; 文字为空或 "<>" -> 用实际测量值
            ((or (not dim-txt) (= dim-txt "") (= dim-txt "<>"))
             (setq result val-str))
            ;; 文字含 "<>" -> 纯 LISP 拆分拼接
            ((wcmatch dim-txt "*<>*")
             (progn
               ;; 找 "<>" 位置 (1-indexed, substr 的语义)
               (setq idx 1)
               (setq pos1 nil)
               (while (and (<= idx (strlen dim-txt)) (not pos1))
                 (setq ch (substr dim-txt idx 1))
                 (if (and (= ch "<")
                          (<= (+ idx 1) (strlen dim-txt))
                          (= (substr dim-txt (+ idx 1) 1) ">"))
                   (setq pos1 idx)
                 )
                 (setq idx (1+ idx))
               )
               (if pos1
                 (progn
                   (setq pos2 (+ pos1 1))
                   (setq result
                          (strcat
                            (substr dim-txt 1 (- pos1 1))
                            val-str
                            (substr dim-txt (+ pos2 1)
                                    (- (strlen dim-txt) pos2)))))
                 (setq result val-str))))
            ;; 手动改过文字 -> 直接用
            (T
             (setq result dim-txt)))
          (princ (strcat "\n尺寸值: " result))

          ;; ---- 3. 选择目标文本 ----
          (setq txt-ent (car (entsel "\n选择目标文本: ")))
          (if txt-ent
            (progn
              (setq txt-ed (entget txt-ent))
              (setq txt-type (cdr (assoc 0 txt-ed)))
              (if (or (= txt-type "TEXT") (= txt-type "MTEXT"))
                (progn
                  ;; ---- 4. 修改文本内容 ----
                  (setq txt-ed (subst (cons 1 result)
                                      (assoc 1 txt-ed) txt-ed))
                  (entmod txt-ed)
                  (princ (strcat "\n已将文本修改为: " result))
                )
                (princ "\n选择的不是文本实体。")
              )
            )
            (princ "\n已取消。")
          )
        )
        (princ "\n选择的不是标注实体。")
      )
    )
    (princ "\n已取消。")
  )
  (princ)
)
