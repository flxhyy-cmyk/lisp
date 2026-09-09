;;; ====================================================================
;;; 功能: 排查CAD图纸"明明有图形,打印却是空白"的常见原因
;;; 用法: 在命令行输入 CHKPLOT 并回车
;;; 说明: 本程序只做只读检测,不会修改图纸任何内容
;;; ====================================================================

(defun c:CHKPLOT (/ doc lays lay lname frozen off noplot ss cnt issues
                   tilemode ctab layoutObj styleSheet ssAll hiddenCnt
                   i n obj vis blk xrefPath xrefIssue ent edata vpid
                   vplayer vpstatus vpCount)

  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq issues '())

  (princ "\n========== 打印为空排查开始 ==========")

  ;; ---------------------------------------------------------------
  ;; 1. 当前空间 / 布局信息
  ;; ---------------------------------------------------------------
  (setq tilemode (getvar "TILEMODE"))
  (setq ctab (getvar "CTAB"))
  (princ (strcat "\n[信息] 当前布局标签: " ctab))
  (princ (strcat "\n[信息] TILEMODE = " (itoa tilemode)
                 (if (= tilemode 1)
                   "  (当前处于模型空间标签页)"
                   "  (当前处于图纸/布局空间)")))

  ;; ---------------------------------------------------------------
  ;; 2. 图层检查: 冻结 / 关闭 / 设置为不打印
  ;; ---------------------------------------------------------------
  (princ "\n\n---- 图层检查 ----")
  (setq lays (vla-get-Layers doc))
  (vlax-for lay lays
    (setq lname (vla-get-Name lay))
    (setq frozen (eq (vla-get-Freeze lay) :vlax-true))
    (setq off (eq (vla-get-LayerOn lay) :vlax-false))
    (setq n (vl-catch-all-apply 'vla-get-Plottable (list lay)))
    (setq noplot (and (not (vl-catch-all-error-p n)) (eq n :vlax-false)))
    (if (or frozen off noplot)
      (progn
        (setq ss (ssget "_X" (list (cons 8 lname))))
        (setq cnt (if ss (sslength ss) 0))
        (if (> cnt 0)
          (progn
            (princ (strcat "\n[问题] 图层 \"" lname "\" 上有 "
                           (itoa cnt) " 个图元, 但该图层"))
            (if frozen (princ " 被冻结(Freeze)"))
            (if off (princ " 已关闭(On=否)"))
            (if noplot (princ " 被设置为不打印(Plottable=否)"))
            (setq issues (cons (strcat "图层[" lname "]状态异常且含有图元") issues))
          )
        )
      )
    )
  )
  (if (not issues) (princ "\n[正常] 未发现处于冻结/关闭/不打印状态且含图元的图层"))

  ;; ---------------------------------------------------------------
  ;; 3. 布局视口基本状态检查(仅当处于图纸空间时)
  ;; ---------------------------------------------------------------
  (if (= tilemode 0)
    (progn
      (princ "\n\n---- 当前布局视口检查 ----")
      (setq ss (ssget "_X" (list (cons 0 "VIEWPORT") (cons 410 ctab))))
      (if ss
        (progn
          (setq vpCount 0)
          (setq i 0)
          (repeat (sslength ss)
            (setq ent (ssname ss i))
            (setq edata (entget ent))
            (setq vpid (cdr (assoc 69 edata)))
            (setq vplayer (cdr (assoc 8 edata)))
            (setq vpstatus (cdr (assoc 68 edata)))
            (if (and vpid (/= vpid 1))
              (progn
                (setq vpCount (1+ vpCount))
                (princ (strcat "\n  视口(ID=" (itoa vpid) ") 所在图层: " vplayer))
                (if (and vpstatus (= vpstatus 0))
                  (princ "    [问题] 该视口状态字段显示为关闭, 不显示模型内容")
                )
              )
            )
            (setq i (1+ i))
          )
          (if (= vpCount 0)
            (princ "\n[信息] 当前布局中只有图纸空间本身, 未找到独立视口")
          )
        )
        (princ "\n[信息] 未在当前布局检测到视口实体")
      )
    )
  )

  ;; ---------------------------------------------------------------
  ;; 4. 单独隐藏的图元检查(如通过 隐藏对象/ISOLATEOBJECTS 命令)
  ;; ---------------------------------------------------------------
  (princ "\n\n---- 隐藏对象检查 ----")
  (setq ssAll (ssget "_X"))
  (setq hiddenCnt 0)
  (if ssAll
    (progn
      (setq i 0)
      (repeat (sslength ssAll)
        (setq obj (vlax-ename->vla-object (ssname ssAll i)))
        (setq vis (vl-catch-all-apply 'vla-get-Visible (list obj)))
        (if (and (not (vl-catch-all-error-p vis)) (eq vis :vlax-false))
          (setq hiddenCnt (1+ hiddenCnt))
        )
        (setq i (1+ i))
      )
    )
  )
  (if (> hiddenCnt 0)
    (progn
      (princ (strcat "\n[问题] 发现 " (itoa hiddenCnt)
                     " 个图元被单独设置为不可见(可能使用过 隐藏对象/ISOLATEOBJECTS 命令)"))
      (setq issues (cons "存在被单独隐藏(Visible=否)的图元" issues))
    )
    (princ "\n[正常] 未发现被单独隐藏的图元")
  )

  ;; ---------------------------------------------------------------
  ;; 5. 外部参照(XREF)有效性检查
  ;; ---------------------------------------------------------------
  (princ "\n\n---- 外部参照(XREF)检查 ----")
  (setq xrefIssue nil)
  (vlax-for blk (vla-get-Blocks doc)
    (if (and (vlax-property-available-p blk 'IsXRef)
             (eq (vla-get-IsXRef blk) :vlax-true))
      (progn
        (setq xrefPath (vl-catch-all-apply 'vla-get-Path (list blk)))
        (if (or (vl-catch-all-error-p xrefPath)
                (not xrefPath)
                (= xrefPath "")
                (not (findfile xrefPath)))
          (progn
            (setq xrefIssue T)
            (princ (strcat "\n[问题] 外部参照 \"" (vla-get-Name blk)
                           "\" 路径无效或文件缺失, 其图元可能无法打印"))
            (setq issues (cons (strcat "外部参照[" (vla-get-Name blk) "]路径失效") issues))
          )
        )
      )
    )
  )
  (if (not xrefIssue) (princ "\n[正常] 未发现路径失效的外部参照"))

  ;; ---------------------------------------------------------------
  ;; 6. 图元总数检查(确认数据库中是否真的存在图元)
  ;; ---------------------------------------------------------------
  (princ "\n\n---- 图元总数检查 ----")
  (if ssAll
    (princ (strcat "\n[信息] 整个图形数据库共有 " (itoa (sslength ssAll)) " 个图元"))
    (progn
      (princ "\n[严重问题] 整个图形数据库中没有任何图元, 图纸实际为空")
      (setq issues (cons "图形数据库中不存在任何图元" issues))
    )
  )

  ;; ---------------------------------------------------------------
  ;; 7. 打印样式表信息
  ;; ---------------------------------------------------------------
  (princ "\n\n---- 打印样式表信息 ----")
  (setq styleSheet
    (vl-catch-all-apply
      (function
        (lambda ()
          (setq layoutObj (vla-item (vla-get-Layouts doc) ctab))
          (vla-get-StyleSheet layoutObj)
        )
      )
    )
  )
  (if (or (vl-catch-all-error-p styleSheet) (not styleSheet) (= styleSheet ""))
    (princ "\n[信息] 未能读取到当前布局的打印样式表名称(或未指定)")
    (progn
      (princ (strcat "\n[信息] 当前布局使用的打印样式表: " styleSheet))
      (princ "\n[提示] 请在该CTB/STB文件中确认相关颜色/图层对应的打印样式")
      (princ "\n       是否被设置为'不打印'、线宽为0且透明, 或打印颜色为白色")
    )
  )

  ;; ---------------------------------------------------------------
  ;; 8. 总结
  ;; ---------------------------------------------------------------
  (princ "\n\n========== 排查结束 ==========")
  (if issues
    (progn
      (princ "\n以下是本次检测发现的可能原因:")
      (foreach it issues (princ (strcat "\n  - " it)))
    )
    (princ "\n未发现明显的图层/隐藏/外部参照/数据库层面问题.")
  )
  (princ "\n如以上均无问题, 建议进一步检查: 打印范围/窗口选择、打印偏移、")
  (princ "\n打印比例, 以及CTB/STB打印样式表内的颜色到打印颜色映射设置.")
  (princ)
  (princ)
)

;;; ====================================================================
;;; 功能: 自动修复 CHKPLOT 检测出的两类常见问题
;;;       1. 图层被设置为"不打印"(Plottable=否) -> 改回"可打印"
;;;       2. 图元被单独隐藏(Visible=否, 如 隐藏对象/ISOLATEOBJECTS) -> 恢复可见
;;; 用法: 在命令行输入 FIXPLOT 并回车
;;; 说明: 本程序会修改图纸内容, 执行前请确认已保存图纸备份
;;; ====================================================================

(defun c:FIXPLOT (/ doc lays lay lname n noplot fixedLayerCnt
                   ssAll obj vis i fixedHiddenCnt)

  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  (princ "\n========== 打印为空自动修复开始 ==========")

  ;; ---------------------------------------------------------------
  ;; 1. 修复图层 Plottable = 否 的情况
  ;; ---------------------------------------------------------------
  (princ "\n\n---- 修复图层'不打印'状态 ----")
  (setq fixedLayerCnt 0)
  (setq lays (vla-get-Layers doc))
  (vlax-for lay lays
    (setq lname (vla-get-Name lay))
    (setq n (vl-catch-all-apply 'vla-get-Plottable (list lay)))
    (setq noplot (and (not (vl-catch-all-error-p n)) (eq n :vlax-false)))
    (if noplot
      (progn
        (vl-catch-all-apply 'vla-put-Plottable (list lay :vlax-true))
        (setq fixedLayerCnt (1+ fixedLayerCnt))
        (princ (strcat "\n[已修复] 图层 \"" lname "\" 已改为可打印(Plottable=是)"))
      )
    )
  )
  (if (= fixedLayerCnt 0)
    (princ "\n[信息] 未发现设置为不打印的图层, 无需修复")
  )

  ;; ---------------------------------------------------------------
  ;; 2. 修复被单独隐藏的图元(Visible = 否)
  ;; ---------------------------------------------------------------
  (princ "\n\n---- 修复被单独隐藏的图元 ----")
  (setq fixedHiddenCnt 0)
  (setq ssAll (ssget "_X"))
  (if ssAll
    (progn
      (setq i 0)
      (repeat (sslength ssAll)
        (setq obj (vlax-ename->vla-object (ssname ssAll i)))
        (setq vis (vl-catch-all-apply 'vla-get-Visible (list obj)))
        (if (and (not (vl-catch-all-error-p vis)) (eq vis :vlax-false))
          (progn
            (vl-catch-all-apply 'vla-put-Visible (list obj :vlax-true))
            (setq fixedHiddenCnt (1+ fixedHiddenCnt))
          )
        )
        (setq i (1+ i))
      )
    )
  )
  (if (> fixedHiddenCnt 0)
    (princ (strcat "\n[已修复] 恢复了 " (itoa fixedHiddenCnt) " 个图元的可见性(Visible=是)"))
    (princ "\n[信息] 未发现被单独隐藏的图元, 无需修复")
  )

  ;; ---------------------------------------------------------------
  ;; 3. 总结
  ;; ---------------------------------------------------------------
  (princ "\n\n========== 修复结束 ==========")
  (princ (strcat "\n共修复图层 " (itoa fixedLayerCnt)
                 " 个, 恢复隐藏图元 " (itoa fixedHiddenCnt) " 个"))
  (princ "\n建议重新运行 CHKPLOT 命令确认问题已消除, 再执行 REGEN 刷新显示.")
  (command "_.REGEN")
  (princ)
  (princ)
)

(princ "\n已加载: 输入 CHKPLOT 排查打印为空原因, 输入 FIXPLOT 自动修复已知问题.")
(princ)
