;@name 标注值转文本
;@group 文本编辑
;@desc 依次选择2个标注获取尺寸值，再依次选择2个文本，将对应尺寸值赋予文本

; 辅助: 提取标注的显示文本
(defun zsr-get-dim-text (dim-ent / ed tp txt val val-str idx
                          pos1 pos2 ch result)
  (if dim-ent
    (progn
      (setq ed (entget dim-ent))
      (setq tp (cdr (assoc 0 ed)))
      (if (= tp "DIMENSION")
        (progn
          (setq txt (cdr (assoc 1 ed)))
          (setq val (cdr (assoc 42 ed)))
          (setq val-str (rtos val 2 (getvar "DIMDEC")))
          (cond
            ((or (not txt) (= txt "") (= txt "<>")
                 (not (wcmatch txt "*<>*")))
             (setq result val-str))
            (T
             (progn
               (setq idx 1)
               (setq pos1 nil)
               (while (and (<= idx (strlen txt)) (not pos1))
                 (setq ch (substr txt idx 1))
                 (if (and (= ch "<")
                          (<= (+ idx 1) (strlen txt))
                          (= (substr txt (+ idx 1) 1) ">"))
                   (setq pos1 idx))
                 (setq idx (1+ idx)))
               (if pos1
                 (progn
                   (setq pos2 (+ pos1 1))
                   (setq result
                          (strcat (substr txt 1 (- pos1 1))
                                  val-str
                                  (substr txt (+ pos2 1)
                                          (- (strlen txt) pos2)))))
                 (setq result val-str)))))
          result))
      result))
  result)

; 辅助: 将文本写入 TEXT/MTEXT
(defun zsr-set-text (txt-ent val-str / ed tp)
  (if (and txt-ent val-str)
    (progn
      (setq ed (entget txt-ent))
      (setq tp (cdr (assoc 0 ed)))
      (if (or (= tp "TEXT") (= tp "MTEXT"))
        (progn
          (setq ed (subst (cons 1 val-str) (assoc 1 ed) ed))
          (if (entmod ed)
            T
            nil))
        nil))
    nil))

; 辅助: 选择实体并校验类型
(defun zsr-pick (prompt type-pattern / ent ed tp)
  (setq ent (car (entsel prompt)))
  (if ent
    (progn
      (setq ed (entget ent))
      (setq tp (cdr (assoc 0 ed)))
      (if (wcmatch tp type-pattern)
        ent
        nil))
    nil))

; 主命令
(defun c:ZSR (/ dim1 dim2 txt1 txt2 v1 v2 r1 r2)
  (if (not (setq dim1 (zsr-pick "\n选择第 1 个标注: " "DIMENSION*")))
    (progn (princ "\n已取消。") (exit))
  )
  (if (not (setq dim2 (zsr-pick "\n选择第 2 个标注: " "DIMENSION*")))
    (progn (princ "\n已取消。") (exit))
  )
  (if (not (setq txt1 (zsr-pick "\n选择第 1 个目标文本: " "TEXT,MTEXT")))
    (progn (princ "\n已取消。") (exit))
  )
  (if (not (setq txt2 (zsr-pick "\n选择第 2 个目标文本: " "TEXT,MTEXT")))
    (progn (princ "\n已取消。") (exit))
  )
  (setq v1 (zsr-get-dim-text dim1))
  (setq v2 (zsr-get-dim-text dim2))
  (setq r1 (zsr-set-text txt1 v1))
  (setq r2 (zsr-set-text txt2 v2))
  (if r1
    (princ (strcat "\n文本1 已更新: " v1))
    (princ "\n文本1 写入失败。")
  )
  (if r2
    (princ (strcat "\n文本2 已更新: " v2))
    (princ "\n文本2 写入失败。")
  )
  ;; 两次写入都成功时, 调用 REGEN 重生成图形
  (if (and r1 r2)
    (progn
      (command "_.REGEN")
      (princ "\n已重生成图形。"))
  )
  (princ)
)
