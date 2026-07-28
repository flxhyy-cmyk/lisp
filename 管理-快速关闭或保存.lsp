;;; QM.lsp - 快速关闭DWG管理工具
;;; QM  - 全部不保存关闭非活动文档
;;; QMB - 有路径则保存关闭，无路径则不保存关闭，并记录关闭列表
;;; QMF - 重新打开最近一次 QMB 关闭的文件

;;; ================================
;;; 全局变量：记录 QMB 关闭的文件
;;; ================================
(setq *QMB-closed-files* nil)

;;; ========== QM ==========
(defun c:QM (/ acad docs n i doclist activeDoc doc closed)
  (setq acad      (vlax-get-acad-object)
        docs      (vla-get-documents acad)
        n         (vla-get-count docs)
        activeDoc (vla-get-activedocument acad)
  )

  (if (<= n 1)
    (progn
      (princ "\n没有可关闭的其他文档。")
      (princ)
    )
    (progn
      (setq doclist '() i 0)
      (repeat n
        (setq doclist (append doclist (list (vla-item docs i))))
        (setq i (1+ i))
      )

      (setq closed 0)
      (foreach doc doclist
        (if (not (eq doc activeDoc))
          (progn
            (vl-catch-all-apply 'vla-Close (list doc :vlax-false))
            (setq closed (1+ closed))
          )
        )
      )

      (princ (strcat "\n已关闭 " (itoa closed) " 个文档（未保存）。"))
      (princ)
    )
  )
)

;;; ========== QMB ==========
(defun c:QMB (/ acad docs n i doclist activeDoc doc path saved unsaved closed)
  (setq acad      (vlax-get-acad-object)
        docs      (vla-get-documents acad)
        n         (vla-get-count docs)
        activeDoc (vla-get-activedocument acad)
        *QMB-closed-files* nil
  )

  (if (<= n 1)
    (progn
      (princ "\n没有可关闭的其他文档。")
      (princ)
    )
    (progn
      (setq doclist '() i 0)
      (repeat n
        (setq doclist (append doclist (list (vla-item docs i))))
        (setq i (1+ i))
      )

      (setq closed 0 saved 0 unsaved 0)

      (foreach doc doclist
        (if (not (eq doc activeDoc))
          (progn
            (setq path (vlax-get-property doc 'FullName))

            ;; 已保存文件
            (if (and path (/= path ""))
              (progn
                (vl-catch-all-apply 'vla-Save (list doc))
                (setq *QMB-closed-files*
                      (cons path *QMB-closed-files*))
                (vl-catch-all-apply 'vla-Close (list doc :vlax-true))
                (setq saved (1+ saved))
              )
              ;; 未保存文件
              (progn
                (vl-catch-all-apply 'vla-Close (list doc :vlax-false))
                (setq unsaved (1+ unsaved))
              )
            )

            (setq closed (1+ closed))
          )
        )
      )

      (princ
        (strcat "\nQMB完成：关闭 "
                (itoa closed)
                " 个文档（已保存 "
                (itoa saved)
                "，未保存 "
                (itoa unsaved)
                "）"))
      (princ)
    )
  )
)

;;; ========== QMF ==========
(defun c:QMF (/ acad)
  (setq acad (vlax-get-acad-object))

  (if (null *QMB-closed-files*)
    (progn
      (princ "\n没有可恢复的QMB关闭记录。")
      (princ)
    )
    (progn
      (foreach f *QMB-closed-files*
        (if (findfile f)
          (vl-catch-all-apply
            'vla-open
            (list (vla-get-documents acad) f)
          )
          (princ (strcat "\n文件不存在：" f))
        )
      )

      (princ (strcat "\n已尝试恢复 " (itoa (length *QMB-closed-files*)) " 个文件。"))
      (princ)
    )
  )
)

(princ "\nQM / QMB / QMF 已加载完成")
(princ)