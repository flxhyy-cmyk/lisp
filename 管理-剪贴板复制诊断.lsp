;;; =====================================================================
;;; 剪贴板复制诊断工具 v2  CHKCLIP.LSP
;;; 用途：当 AutoCAD 弹出"无法复制到剪贴板"消息框时，
;;;       1) 排查选择集内部原因（代理对象/OLE/图像/复杂实体等）；
;;;       2) 检测最常见的外部系统级原因：
;;;          - 远程桌面剪贴板代理进程 rdpclip.exe 崩溃
;;;          - 其它剪贴板管理/录屏类程序占用剪贴板
;;;          - AutoCAD 以管理员权限运行导致的剪贴板隔离
;;;       3) 对 rdpclip.exe 崩溃这种最常见情况提供一键自动修复（重启该进程）。
;;; 用法：命令行输入 CHKCLIP，选择你原本要复制的对象即可。
;;; 说明：AutoCAD 弹出的"无法复制到剪贴板"是 Win32 OpenClipboard/
;;;       SetClipboardData 调用失败时的系统级提示，并不会作为
;;;       LISP 错误传回 (command) 调用，因此单靠 vl-catch-all-apply
;;;       无法捕获到它——这本身就说明问题出在 AutoCAD 进程之外。
;;; =====================================================================

(vl-load-com)

;; ---------------------------------------------------------------------
;; 全局标志：记录"上一次" WTF:RUN-CAPTURE 调用的系统命令是否被意外中断。
;; 意外中断包含两种情况：
;;   1) Shell.Run 调用本身被拦截/失败（比如被安全软件阻止启动 cmd.exe）；
;;   2) Run 调用没有报错，但重定向输出的临时文件却完全没有生成
;;      （正常情况下哪怕命令本身报错，cmd 重定向也会生成一个文件，
;;       哪怕内容只是错误信息；完全没有文件通常说明 cmd.exe 进程
;;       没有正常跑完，比如被安全软件拦截、异常终止或权限被拒绝）。
;; 这个标志让调用方能区分"命令正常执行但确实没查到东西"和"命令
;; 根本没跑完/被中断"这两种性质完全不同的空结果，从而在被中断时
;; 给出明确提示，并继续往下走完剩余的检查阶段，而不是把中断误当成
;; 正常的空结果静默略过。
;; ---------------------------------------------------------------------
(setq *WTF-LAST-CMD-INTERRUPTED* nil)

;; ---------------------------------------------------------------------
;; 工具函数：用 WScript.Shell 同步执行一条命令，并把标准输出重定向到
;; 临时文件后读回，返回文件全部文本行组成的列表。
;; 用于在没有外部脚本引擎的情况下，从 AutoLISP 里拿到 tasklist / whoami
;; 等命令的结果。
;; 调用后请检查 *WTF-LAST-CMD-INTERRUPTED*：为 T 表示本次系统命令执行
;; 被意外中断，返回的空列表不代表"确实没查到"，调用方应据此提示用户
;; 并继续执行后续检查阶段，不应中止整个诊断流程。
;; ---------------------------------------------------------------------
(defun WTF:RUN-CAPTURE (cmdline / shell tmpfile fullcmd rc f line lines got-file)
  (setq lines nil)
  (setq *WTF-LAST-CMD-INTERRUPTED* nil)
  (setq tmpfile (strcat (vl-filename-mktemp) ".txt"))
  (setq fullcmd (strcat "cmd /c " cmdline " > \"" tmpfile "\" 2>&1"))
  (setq shell (vl-catch-all-apply 'vlax-create-object (list "WScript.Shell")))
  (if (and shell (not (vl-catch-all-error-p shell)))
    (progn
      (setq rc
        (vl-catch-all-apply
          'vlax-invoke-method
          (list shell 'Run fullcmd 0 :vlax-true)
        )
      )
      (vl-catch-all-apply 'vlax-release-object (list shell))
      ;; rc 是错误对象：说明 Run 调用本身被中断（外壳启动失败/被拦截等）
      (if (vl-catch-all-error-p rc)
        (setq *WTF-LAST-CMD-INTERRUPTED* T)
      )
      (setq got-file nil)
      (if (findfile tmpfile)
        (progn
          (setq got-file T)
          (setq f (open tmpfile "r"))
          (if f
            (progn
              (while (setq line (read-line f))
                (setq lines (cons line lines))
              )
              (close f)
            )
          )
          (vl-catch-all-apply 'vl-file-delete (list tmpfile))
        )
      )
      ;; Run 调用没报错，但输出文件完全没生成：同样视为命令被意外中断
      (if (and (not got-file) (not (vl-catch-all-error-p rc)))
        (setq *WTF-LAST-CMD-INTERRUPTED* T)
      )
    )
    ;; 连 WScript.Shell 对象都创建失败，属于更早一步的中断
    (setq *WTF-LAST-CMD-INTERRUPTED* T)
  )
  (reverse lines)
)

;; ---------------------------------------------------------------------
;; 判断某进程名是否出现在 tasklist 输出的行列表中（忽略大小写）
;; ---------------------------------------------------------------------
(defun WTF:PROC-RUNNING-P (lines procname / found)
  (setq found nil)
  (foreach ln lines
    (if (and ln (wcmatch (strcase ln) (strcat "*" (strcase procname) "*")))
      (setq found T)
    )
  )
  found
)

;; ---------------------------------------------------------------------
;; 判断当前 AutoCAD 进程是否以管理员（高完整性级别）运行
;; 通过 whoami /groups 输出中是否包含高完整性级别标识判断
;; ---------------------------------------------------------------------
(defun WTF:IS-ELEVATED-P ( / lines elevated)
  (setq lines (WTF:RUN-CAPTURE "whoami /groups"))
  (setq elevated nil)
  (foreach ln lines
    (if (and ln
             (or (wcmatch (strcase ln) "*S-1-16-12288*")
                 (wcmatch ln "*High Mandatory Level*")
                 (wcmatch ln "*高完整性级别*")
             )
        )
      (setq elevated T)
    )
  )
  elevated
)

;; ---------------------------------------------------------------------
;; 尝试重启 rdpclip.exe（远程桌面剪贴板代理），这是"无法复制到剪贴板"
;; 在远程桌面场景下最常见、也最容易一键解决的原因。
;; ---------------------------------------------------------------------
(defun WTF:FIX-RDPCLIP ( / shell)
  (WTF:RUN-CAPTURE "taskkill /f /im rdpclip.exe")
  (setq shell (vl-catch-all-apply 'vlax-create-object (list "WScript.Shell")))
  (if (and shell (not (vl-catch-all-error-p shell)))
    (progn
      (vl-catch-all-apply 'vlax-invoke-method (list shell 'Run "rdpclip.exe" 0 :vlax-false))
      (vl-catch-all-apply 'vlax-release-object (list shell))
      T
    )
    nil
  )
)

;; ---------------------------------------------------------------------
;; 重启 explorer.exe：修复"剪贴板查看器链损坏"这一经典 Windows 级
;; 剪贴板故障（某个曾监听剪贴板的老程序异常退出、未正确从链中摘除，
;; 导致此后所有程序 OpenClipboard/SetClipboardData 间歇性失败）。
;; ---------------------------------------------------------------------
(defun WTF:FIX-EXPLORER ( / shell)
  (WTF:RUN-CAPTURE "taskkill /f /im explorer.exe")
  (setq shell (vl-catch-all-apply 'vlax-create-object (list "WScript.Shell")))
  (if (and shell (not (vl-catch-all-error-p shell)))
    (progn
      (vl-catch-all-apply 'vlax-invoke-method (list shell 'Run "explorer.exe" 1 :vlax-false))
      (vl-catch-all-apply 'vlax-release-object (list shell))
      T
    )
    nil
  )
)

;; ---------------------------------------------------------------------
;; 读取注册表某项的值（借助 reg query 命令行输出解析）
;; 返回字符串形式的值，读取失败或不存在返回 nil
;; ---------------------------------------------------------------------
(defun WTF:REG-QUERY (keypath valuename / lines val pos)
  (setq lines (WTF:RUN-CAPTURE (strcat "reg query \"" keypath "\" /v " valuename)))
  (setq val nil)
  (foreach ln lines
    (if (and ln (wcmatch ln (strcat "*" valuename "*REG_*")))
      (progn
        (setq pos (vl-string-search "0x" ln))
        (if pos (setq val (substr ln (1+ pos))))
      )
    )
  )
  val
)

;; ---------------------------------------------------------------------
;; 全库扫描：递归遍历模型空间/图纸空间以及所有块定义内部的实体
;; （而不只是用户选中的对象），用于定位"只在这个文件才出问题"这类
;; 文件级/数据库级异常——哪怕问题实体没被用户选中，只要它存在于
;; 图形数据库里，也可能是文件本身状态不正常的信号。
;; ---------------------------------------------------------------------
(defun WTF:SCAN-CHAIN (start-ent depth / e edata etype bname bename)
  (setq e start-ent)
  (while e
    (setq edata (vl-catch-all-apply 'entget (list e)))
    (if (and edata (not (vl-catch-all-error-p edata)))
      (progn
        (setq etype (cdr (assoc 0 edata)))
        (cond
          ((wcmatch (strcase etype) "*PROXY*")
           (setq *WTF-G-PROXY* (1+ *WTF-G-PROXY*))
          )
          ((wcmatch (strcase etype) "OLE2FRAME")
           (setq *WTF-G-OLE* (1+ *WTF-G-OLE*))
          )
          ((wcmatch (strcase etype) "IMAGE,RASTERIMAGE")
           (setq *WTF-G-IMG* (1+ *WTF-G-IMG*))
          )
        )
        (setq *WTF-G-TOTAL* (1+ *WTF-G-TOTAL*))
        ;; 递归进入嵌套块定义（限制递归深度，避免异常循环引用死循环）
        (if (and (= etype "INSERT") (< depth 20))
          (progn
            (setq bname (cdr (assoc 2 edata)))
            (setq bename (vl-catch-all-apply 'tblobjname (list "BLOCK" bname)))
            (if (and bename (not (vl-catch-all-error-p bename)))
              (WTF:SCAN-CHAIN (entnext bename) (1+ depth))
            )
          )
        )
      )
      ;; entget 失败：说明该实体本身已损坏，无法正常访问
      (setq *WTF-G-BAD* (1+ *WTF-G-BAD*))
    )
    (setq e (entnext e))
  )
)

(defun WTF:SCAN-WHOLE-DB ( / bname bename)
  (setq *WTF-G-PROXY* 0 *WTF-G-OLE* 0 *WTF-G-IMG* 0
        *WTF-G-TOTAL* 0 *WTF-G-BAD* 0)
  ;; 遍历所有块表记录（含 *Model_Space、各布局的 *Paper_Space 以及
  ;; 所有具名块定义），对每个块定义内部做一次实体链扫描
  (setq bname (tblnext "BLOCK" T))
  (while bname
    (setq bename (vl-catch-all-apply 'tblobjname (list "BLOCK" (cdr (assoc 2 bname)))))
    (if (and bename (not (vl-catch-all-error-p bename)))
      (WTF:SCAN-CHAIN (entnext bename) 0)
    )
    (setq bname (tblnext "BLOCK"))
  )
  (list *WTF-G-TOTAL* *WTF-G-PROXY* *WTF-G-OLE* *WTF-G-IMG* *WTF-G-BAD*)
)

;; ---------------------------------------------------------------------
;; 列出图形中已注册的 APPID（应用程序标识表）。第三方插件（如天正建筑、
;; 浩辰CAD、探索者TSSD等）在写入自定义对象/扩展数据时通常会注册自己的
;; APPID，借此可以反推出代理对象最可能来自哪个第三方应用。
;; ---------------------------------------------------------------------
(defun WTF:LIST-APPIDS ( / rec names)
  (setq names nil)
  (setq rec (tblnext "APPID" T))
  (while rec
    (setq names (cons (cdr (assoc 2 rec)) names))
    (setq rec (tblnext "APPID"))
  )
  (reverse names)
)

;; ---------------------------------------------------------------------
;; 在数据库中找出第一个代理对象，尝试读取其原始类名相关信息
;; （群组 91/92/93 等）以及是否带有第三方 XDATA，辅助定位来源应用
;; ---------------------------------------------------------------------
(defun WTF:SAMPLE-PROXY-INFO (start-ent depth / e edata etype found result bname bename)
  (setq result nil)
  (setq e start-ent)
  (while (and e (not result))
    (setq edata (vl-catch-all-apply 'entget (list e '("*"))))
    (if (and edata (not (vl-catch-all-error-p edata)))
      (progn
        (setq etype (cdr (assoc 0 edata)))
        (if (wcmatch (strcase etype) "*PROXY*")
          (setq result edata)
          (if (and (= etype "INSERT") (< depth 20))
            (progn
              (setq bname (cdr (assoc 2 edata)))
              (setq bename (vl-catch-all-apply 'tblobjname (list "BLOCK" bname)))
              (if (and bename (not (vl-catch-all-error-p bename)))
                (setq result (WTF:SAMPLE-PROXY-INFO (entnext bename) (1+ depth)))
              )
            )
          )
        )
      )
    )
    (if (not result) (setq e (entnext e)))
  )
  result
)


(defun WTF:RUN-AUDIT ( / old-logmode old-cmdecho logpath before-count
                          f ln lines summary old-error result)
  ;; 说明：LOGFILENAME 是只读系统变量，(setvar "LOGFILENAME" ...) 会被
  ;; AutoCAD 直接拒绝（命令行提示"变量设置被拒绝"），这正是之前"执行到
  ;; AUDIT这一步就终止/无输出"的真正原因——不是取消，而是这一句setvar
  ;; 从设计上就不可能成功，日志根本没有被重定向到临时文件，导致后面
  ;; 读取 tmplog 永远读到空文件。
  ;; 修正思路：不再尝试修改 LOGFILENAME（只读取，读取是允许的），而是
  ;; 记录AUDIT执行前当前日志文件已有的行数，AUDIT结束后只取新增的那部分
  ;; 行来解析摘要；同时强制 CMDECHO=1，这样不管摘要解析是否成功，
  ;; AUDIT的原始输出都会直接显示在命令行，用户始终能看到结果。
  (setq old-logmode (getvar "LOGFILEMODE"))
  (setq old-cmdecho (getvar "CMDECHO"))
  (setq logpath (getvar "LOGFILENAME"))
  (setq before-count 0)
  (if (and logpath (findfile logpath))
    (progn
      (setq f (open logpath "r"))
      (if f
        (progn
          (while (read-line f) (setq before-count (1+ before-count)))
          (close f)
        )
      )
    )
  )
  ;; 接管*error*：防止AUDIT弹出的确认/报告对话框被取消(Esc)时，
  ;; LOGFILEMODE/CMDECHO残留未还原、且整个诊断被静默中断
  (setq old-error *error*)
  (defun *error* (msg)
    (setq *error* old-error)
    (vl-catch-all-apply 'setvar (list "LOGFILEMODE" old-logmode))
    (vl-catch-all-apply 'setvar (list "CMDECHO" old-cmdecho))
    (princ "\n  [WTF:RUN-AUDIT] AUDIT 执行被中断")
    (if (and msg (/= (strcase msg) "FUNCTION CANCELLED"))
      (princ (strcat "：" msg))
      (princ "（很可能是AUDIT弹出的确认/报告对话框被关闭或按了Esc；"
             "请在AUDIT弹窗出现时点\"确定\"而不是按Esc关闭）。")
    )
    (princ)
  )
  (vl-catch-all-apply 'setvar (list "CMDECHO" 1))
  (vl-catch-all-apply 'setvar (list "LOGFILEMODE" 1))
  (setq result (vl-catch-all-apply 'command (list "._AUDIT" "Y")))
  (setq *error* old-error)
  (vl-catch-all-apply 'setvar (list "LOGFILEMODE" old-logmode))
  (vl-catch-all-apply 'setvar (list "CMDECHO" old-cmdecho))
  ;; 尝试从当前日志文件里提取本次AUDIT新增写入的内容，作为摘要
  (setq lines nil)
  (if (and logpath (findfile logpath))
    (progn
      (setq f (open logpath "r"))
      (if f
        (progn
          (while (setq ln (read-line f)) (setq lines (cons ln lines)))
          (close f)
        )
      )
    )
  )
  (setq lines (reverse lines))
  (if (> (length lines) before-count)
    (setq lines (nthcdr before-count lines))
    (setq lines nil)
  )
  (setq summary nil)
  (foreach ln lines
    (if (and ln (or (wcmatch ln "*Errors found*") (wcmatch ln "*发现*错误*")
                     (wcmatch ln "*fixed*") (wcmatch ln "*修复*")))
      (setq summary (cons ln summary))
    )
  )
  ;; 返回 (成功标志 摘要行列表)；成功标志仅表示command调用本身未被
  ;; 捕获到LISP层错误，不代表AUDIT一定没发现问题
  (list (not (vl-catch-all-error-p result)) (reverse summary))
)




;; ---------------------------------------------------------------------
;; 尝试把选择集中的代理对象(ACAD_PROXY_ENTITY)炸开为标准 AutoCAD 实体。
;; 原理：EXPLODE 命令对代理对象的处理，是把它当前的"代理图形"
;; (proxy graphics，由生成该对象的第三方 ARX/对象增强器提供)拆解成
;; 组成它的标准图元(直线/圆弧/多段线等)。
;; 前提条件：本机必须已加载对应第三方应用或其 Object Enabler，否则
;; 代理对象没有可用的代理图形，EXPLODE 会失败或什么都不发生。
;; 注意：炸开后对象会丢失原有的"智能"属性(如天正墙的宽度/材质等参数)，
;; 只剩纯几何图元，此操作不可逆，务必先另存/备份图形。
;; ---------------------------------------------------------------------
(defun WTF:TRY-EXPLODE-PROXY (ss / i n ent edata etype proxy-list ok-count
                                fail-count old-cmdecho res pe)
  (setq proxy-list nil)
  (setq n (sslength ss))
  (setq i 0)
  (while (< i n)
    (setq ent (ssname ss i))
    (setq edata (vl-catch-all-apply 'entget (list ent)))
    (if (and edata (not (vl-catch-all-error-p edata)))
      (progn
        (setq etype (cdr (assoc 0 edata)))
        (if (wcmatch (strcase etype) "*PROXY*")
          (setq proxy-list (cons ent proxy-list))
        )
      )
    )
    (setq i (1+ i))
  )
  (setq proxy-list (reverse proxy-list))

  (if (not proxy-list)
    (progn
      (princ "\n  选择集中未发现代理对象，无需转换。")
      (list 0 0)
    )
    (progn
      (setq ok-count 0 fail-count 0)
      (setq old-cmdecho (getvar "CMDECHO"))
      (setvar "CMDECHO" 0)
      (foreach pe proxy-list
        (setq edata (vl-catch-all-apply 'entget (list pe)))
        (if (and edata (not (vl-catch-all-error-p edata)))
          (progn
            (setq res (vl-catch-all-apply 'command (list "._EXPLODE" pe "")))
            (if (vl-catch-all-error-p res)
              (setq fail-count (1+ fail-count))
              (setq ok-count (1+ ok-count))
            )
          )
          ;; 实体已不存在（可能是前面某次炸开的连带反应导致失效），跳过
          nil
        )
      )
      (setvar "CMDECHO" old-cmdecho)
      (list ok-count fail-count)
    )
  )
)

;; ---------------------------------------------------------------------
;; 命令 PROXY2STD：批量把选中对象中的代理对象炸开为标准实体
;; 用法：命令行输入 PROXY2STD，选择包含代理对象的对象（可直接框选，
;; 非代理对象会被自动忽略），确认后执行。
;; ---------------------------------------------------------------------
(defun c:PROXY2STD ( / ss result ok-count fail-count ans)
  (princ "\n===== 代理对象批量转换为标准实体 (PROXY2STD) =====")
  (princ "\n【重要提示】")
  (princ "\n  1) 此操作会调用 EXPLODE，把代理对象拆解为其当前的\"代理图形\"，")
  (princ "\n     炸开后将丢失原对象的专业属性（如天正墙的厚度/材质、设备的")
  (princ "\n     型号参数等），只剩下纯几何图元(线/弧/多段线等)。")
  (princ "\n  2) 此操作不可逆（关闭文件不保存除外），强烈建议先执行 SAVEAS")
  (princ "\n     另存一份备份，再运行本命令。")
  (princ "\n  3) 若本机未加载对应第三方应用/对象增强器，代理对象可能没有")
  (princ "\n     可用的代理图形，此时 EXPLODE 会失败或没有任何效果——这种")
  (princ "\n     情况下本命令帮不上忙，请改用原厂软件(如天正建筑)自带的")
  (princ "\n     \"转换为标准 AutoCAD 图元\"功能。")

  (setq ans (getstring "\n\n是否已完成备份，确认继续？[Y/N] <N>: "))
  (if (/= (strcase ans) "Y")
    (progn
      (princ "\n已取消。请先备份图形后再运行 PROXY2STD。")
      (princ)
    )
    (progn
      (princ "\n请选择要转换的对象（可直接框选，非代理对象会自动忽略）：")
      (setq ss (ssget "_I"))
      (if (not ss) (setq ss (ssget)))
      (if (not ss)
        (progn
          (princ "\n[提示] 未选中任何对象，操作结束。")
          (princ)
        )
        (progn
          (setq result (WTF:TRY-EXPLODE-PROXY ss))
          (setq ok-count (nth 0 result))
          (setq fail-count (nth 1 result))
          (princ (strcat "\n\n转换完成：成功炸开 " (itoa ok-count)
                          " 个，失败 " (itoa fail-count) " 个。"))
          (if (> ok-count 0)
            (princ "\n建议：转换后请人工核对图形是否正确，无误后再保存；如需批量")
          )
          (if (> fail-count 0)
            (progn
              (princ "\n\n仍有失败的代理对象，说明它们当前没有可用的代理图形。")
              (princ "\n请改用以下任一方式彻底解决：")
              (princ "\n  1) 安装/加载该对象对应的第三方应用或 Object Enabler 后重试；")
              (princ "\n  2) 用原厂软件(如天正建筑)打开本图，使用其自带的")
              (princ "\n     \"转换为标准 CAD 图层/输出为标准图形\"等功能。")
            )
          )
          (princ)
        )
      )
    )
  )
)

(defun c:CHKCLIP ( / ss i ent obj otype
                     total-count proxy-count ole-count image-count
                     xref-count big-count complex-count
                     old-cmdecho copy-result copy-err
                     proc-lines suspect-procs p elevated ans retry found
                     kw-hit clip-hist gpo-rdp arx-list a
                     db-scan audit-summary audit-result audit-ok ln f tmplog
                     appid-names nm sample-proxy pr ms-ename explode-result
                     old-error)

  (princ "\n===== 剪贴板复制诊断工具 v2 =====")
  (princ "\n请选择你原本要复制的对象（若已有选择集将自动使用）：")

  ;; 重置"本次诊断中被意外中断的系统命令检查"汇总列表，供结论部分统一提示
  (setq *WTF-INTERRUPTED-CHECKS* nil)

  ;; 顶层*error*保护：AUDIT/COPYCLIP等步骤内部通过(command)调用系统命令，
  ;; 若过程中弹出的对话框被关闭/按Esc取消，会直接中断整个c:CHKCLIP并
  ;; 退回命令行(vl-catch-all-apply拦截不到这种取消)。这里接管*error*，
  ;; 确保无论在哪一步被中断，CMDECHO都会被还原，并打印明确提示而不是
  ;; 静默终止。
  (setq old-error *error*)
  (defun *error* (msg)
    (setq *error* old-error)
    (if old-cmdecho (vl-catch-all-apply 'setvar (list "CMDECHO" old-cmdecho)))
    (princ "\n\n[CHKCLIP] 诊断被中断")
    (if (and msg (/= (strcase msg) "FUNCTION CANCELLED"))
      (princ (strcat "：" msg))
      (princ "（很可能是某个系统命令弹出的对话框被关闭或按了Esc，"
             "请留意弹窗并点\"确定\"/\"是\"而不是直接按Esc）。")
    )
    (princ)
  )

  (setq ss (ssget "_I"))
  (if (not ss)
    (setq ss (ssget))
  )

  (if (not ss)
    (progn
      (princ "\n[提示] 未选中任何对象，诊断结束。")
      (princ)
    )
    (progn
      (setq total-count (sslength ss))
      (setq proxy-count 0 ole-count 0 image-count 0
            xref-count 0 complex-count 0 big-count 0)

      ;; ---------------- 遍历选择集，逐个分类统计 ----------------
      (setq i 0)
      (while (< i total-count)
        (setq ent (ssname ss i))
        (setq otype "未知")

        (if (and ent (vl-catch-all-apply 'entget (list ent)))
          (progn
            (setq otype (cdr (assoc 0 (entget ent))))
            (cond
              ((wcmatch (strcase otype) "*PROXY*")
               (setq proxy-count (1+ proxy-count))
              )
              ((wcmatch (strcase otype) "OLE2FRAME")
               (setq ole-count (1+ ole-count))
              )
              ((wcmatch (strcase otype) "IMAGE,RASTERIMAGE")
               (setq image-count (1+ image-count))
              )
              ((and (= otype "INSERT")
                    (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
                    (not (vl-catch-all-error-p obj))
                    (vl-catch-all-apply 'vlax-get-property (list obj 'IsXRef)))
               (if (eq (vl-catch-all-apply 'vlax-get-property (list obj 'IsXRef)) :vlax-true)
                 (setq xref-count (1+ xref-count))
               )
              )
            )
            (if (wcmatch (strcase otype) "3DSOLID,REGION,BODY,MESH,PLANESURFACE,SURFACE")
              (setq complex-count (1+ complex-count))
            )
          )
          (setq proxy-count (1+ proxy-count))
        )
        (setq i (1+ i))
      )
      (if (> total-count 3000) (setq big-count total-count))

      ;; ---------------- 对象统计报告 ----------------
      (princ (strcat "\n\n[1/4 选择集对象统计] 共 " (itoa total-count) " 个"))
      (princ (strcat "\n  代理/无法识别对象：" (itoa proxy-count)
                      (if (> proxy-count 0) "  <== 可疑" "  (正常)")))
      (princ (strcat "\n  OLE 嵌入对象：" (itoa ole-count)
                      (if (> ole-count 0) "  <== 可疑" "  (正常)")))
      (princ (strcat "\n  光栅图像对象：" (itoa image-count)
                      (if (> image-count 0) "  <== 可疑" "  (正常)")))
      (princ (strcat "\n  外部参照插入：" (itoa xref-count)))
      (princ (strcat "\n  三维/曲面复杂对象：" (itoa complex-count)
                      (if (> complex-count 0) "  <== 可疑" "  (正常)")))
      (if (> big-count 0)
        (princ (strcat "\n  [警告] 对象数量较多(" (itoa total-count) ")"))
      )
      (if (and (= proxy-count 0) (= ole-count 0) (= image-count 0)
               (= complex-count 0) (= big-count 0))
        (princ "\n  => 选择集本身干净，未发现对象层面的可疑原因。")
      )

      ;; ---------------- 系统变量 ----------------
      (princ "\n\n[2/5 系统变量]")
      (princ (strcat "\n  OLEHIDE=" (vl-princ-to-string (getvar "OLEHIDE"))
                      "  PICKFIRST=" (vl-princ-to-string (getvar "PICKFIRST"))
                      "  PICKADD=" (vl-princ-to-string (getvar "PICKADD"))))

      ;; ---------------- 文件级检查（针对"只有这一个文件才出问题"）----------------
      (princ "\n\n[3/5 文件级检查 —— 这是'只有此文件失效'的重点排查方向]")
      (princ "\n  正在扫描整个图形数据库（含所有块定义内部，而不只是本次选中的对象）...")
      (setq db-scan (WTF:SCAN-WHOLE-DB))
      (princ (strcat "\n  数据库中共遍历 " (itoa (nth 0 db-scan)) " 个实体（含块定义内部）"))
      (princ (strcat "\n    其中代理对象：" (itoa (nth 1 db-scan))
                      (if (> (nth 1 db-scan) 0) "  <== 全库存在代理对象" "")))
      (princ (strcat "\n    其中 OLE 对象：" (itoa (nth 2 db-scan))))
      (princ (strcat "\n    其中图像对象：" (itoa (nth 3 db-scan))))
      (princ (strcat "\n    无法访问/已损坏的实体：" (itoa (nth 4 db-scan))
                      (if (> (nth 4 db-scan) 0)
                        "  <== 强烈提示图形数据库存在损坏，是本文件独有问题的最可能原因"
                        ""
                      )))

      ;; 若全库存在代理对象，进一步尝试定位来源应用（APPID + 原始类信息）
      (if (> (nth 1 db-scan) 0)
        (progn
          (princ "\n\n  [重点] 检测到全库存在代理对象，这是最可能导致'仅此文件'")
          (princ "\n  复制失败的原因——复制时 AutoCAD 需要为全部代理对象重新生成")
          (princ "\n  代理图形，若对应第三方应用/对象增强器未正确加载，该步骤可能失败。")
          (princ "\n  正在读取 APPID 注册表（辅助判断代理对象来源应用）：")
          (setq appid-names (WTF:LIST-APPIDS))
          (if appid-names
            (foreach nm appid-names
              (if (not (member (strcase nm) '("ACAD")))
                (princ (strcat "\n    APPID: " nm))
              )
            )
            (princ "\n    （未发现自定义 APPID）")
          )
          (princ "\n  正在提取一个代理对象样本的原始信息...")
          (setq ms-ename (vl-catch-all-apply 'tblobjname (list "BLOCK" "*Model_Space")))
          (setq sample-proxy nil)
          (if (and ms-ename (not (vl-catch-all-error-p ms-ename)))
            (setq sample-proxy (WTF:SAMPLE-PROXY-INFO (entnext ms-ename) 0))
          )
          (if sample-proxy
            (progn
              (princ (strcat "\n    实体类型: " (vl-princ-to-string (cdr (assoc 0 sample-proxy)))))
              (foreach pr sample-proxy
                (if (member (car pr) '(1001 1000 91 92 93 90))
                  (princ (strcat "\n    DXF " (itoa (car pr)) ": " (vl-princ-to-string (cdr pr))))
                )
              )
            )
            (princ "\n    未能定位到具体样本（可能全部代理对象都在无法直接遍历的层级）。")
          )
          (princ "\n  提示：也可把系统变量 PROXYNOTICE 设为 1 后重新打开本文件，")
          (princ "\n  AutoCAD 会在打开时弹出对话框，直接写明缺失的是哪个第三方应用。")
        )
      )

      ;; 若本次选中的对象里就包含代理对象，提供一键尝试转换为标准实体
      (if (> proxy-count 0)
        (progn
          (princ (strcat "\n\n  [提示] 本次选中的 " (itoa total-count)
                          " 个对象中有 " (itoa proxy-count) " 个是代理对象。"))
          (princ "\n  可尝试用 EXPLODE 把它们炸开为标准实体（前提是本机已加载对应")
          (princ "\n  第三方应用/对象增强器，否则可能无效；炸开会丢失专业属性，")
          (princ "\n  且不可逆，请确保已备份）。")
          (setq ans (getstring "\n  是否现在尝试转换？[Y/N] <N>: "))
          (if (= (strcase ans) "Y")
            (progn
              (setq explode-result (WTF:TRY-EXPLODE-PROXY ss))
              (princ (strcat "\n  转换完成：成功 " (itoa (nth 0 explode-result))
                              " 个，失败 " (itoa (nth 1 explode-result)) " 个。"))
              (if (> (nth 1 explode-result) 0)
                (princ "\n  仍失败的对象请改用原厂软件(如天正建筑)自带的转换功能。")
              )
            )
          )
        )
      )

      (princ "\n\n  正在运行 AUDIT 检查图形数据库完整性（下面紧接着就是AUDIT命令本身的原始输出）...")
      (setq audit-result (WTF:RUN-AUDIT))
      (setq audit-ok (car audit-result))
      (setq audit-summary (cadr audit-result))
      (if (not audit-ok)
        (princ "\n    [异常] AUDIT 未能正常完成，请查看上方的中断提示。")
        (if audit-summary
          (foreach ln audit-summary (princ (strcat "\n    " ln)))
          (princ "\n    AUDIT已执行完毕。未能从日志中自动提取到摘要，请直接查看上方AUDIT命令本身打印的原始输出。")
        )
      )

      (princ (strcat "\n\n  当前图形版本(DWG格式) sysvar ACADVER: "
                      (vl-princ-to-string (getvar "ACADVER"))))

      ;; ---------------- 进程与权限级别检测 ----------------
      (princ "\n\n[4/5 外部系统级检测]")
      (princ "\n  正在读取进程列表...")
      (setq proc-lines (WTF:RUN-CAPTURE "tasklist"))
      (if *WTF-LAST-CMD-INTERRUPTED*
        (progn
          (princ "\n  [检测到系统命令被意外中断] tasklist 未能正常执行完成，")
          (princ "\n  本项（可疑进程/终端安全软件扫描）跳过，自动继续后续检查...")
          (setq *WTF-INTERRUPTED-CHECKS*
            (cons "进程列表(tasklist)" *WTF-INTERRUPTED-CHECKS*))
        )
      )

      (if (not proc-lines)
        (if (not *WTF-LAST-CMD-INTERRUPTED*)
          (princ "\n  未获取到任何进程信息（tasklist 正常返回但结果为空）。")
        )
        (progn
          (setq suspect-procs
            '("RDPCLIP.EXE" "TEAMVIEWER" "ANYDESK" "SUNLOGIN" "TODESK"
              "DITTO" "CLIPX" "CLCL" "CLIPDIARY"
              "SNAGIT" "BANDICAM" "OBS64" "OBS32" "CAMTASIA")
          )
          (setq found nil)
          (foreach p suspect-procs
            (if (WTF:PROC-RUNNING-P proc-lines p)
              (progn
                (princ (strcat "\n  [发现] 进程 " p " 正在运行"))
                (setq found T)
              )
            )
          )
          (if (not found)
            (princ "\n  未发现已知的剪贴板管理/录屏类可疑进程。")
          )

          ;; 关键词扫描：企业终端安全/DLP类软件厂商众多，无法穷举，
          ;; 用关键词做模糊命中提示，命中不代表一定是元凶，仅供人工确认
          (setq kw-hit nil)
          (foreach ln proc-lines
            (if (and ln
                     (or (wcmatch (strcase ln) "*GUARD*")
                         (wcmatch (strcase ln) "*DLP*")
                         (wcmatch (strcase ln) "*SAFEDOG*")
                         (wcmatch (strcase ln) "*DOMAIN*")
                         (wcmatch (strcase ln) "*AUDIT*")
                         (wcmatch (strcase ln) "*EDR*")
                     )
                )
              (progn
                (princ (strcat "\n  [疑似终端安全/DLP软件] " ln))
                (setq kw-hit T)
              )
            )
          )
          (if kw-hit
            (progn
              (princ "\n  以上进程名包含常见终端安全/数据防泄露(DLP)软件关键词，")
              (princ "\n  此类软件常在底层监控/拦截剪贴板以防止数据外发，")
              (princ "\n  是企业环境下'无法复制到剪贴板'的高发原因，建议联系")
              (princ "\n  IT/安全管理员确认是否对 AutoCAD 或 dwg 内容做了剪贴板管控。")
            )
          )
        )
      )

      ;; ---------------- Windows 剪贴板历史/云剪贴板 ----------------
      (princ "\n\n  正在检测 Windows 剪贴板历史(Win+V)/云剪贴板设置...")
      (setq clip-hist (WTF:REG-QUERY "HKCU\\Software\\Microsoft\\Clipboard" "EnableClipboardHistory"))
      (cond
        (*WTF-LAST-CMD-INTERRUPTED*
         (princ "\n  [检测到系统命令被意外中断] reg query 未能正常执行完成，")
         (princ "\n  本项跳过，自动继续后续检查...")
         (setq *WTF-INTERRUPTED-CHECKS*
           (cons "剪贴板历史注册表(reg query)" *WTF-INTERRUPTED-CHECKS*))
        )
        (clip-hist
         (princ (strcat "\n  EnableClipboardHistory = " clip-hist
                         (if (wcmatch clip-hist "*0x1*")
                           "  <== 已开启，个别 Windows 版本与部分程序存在冲突，可尝试关闭"
                           "  (已关闭)"
                         )))
        )
        (T
         (princ "\n  未读取到该注册表项（可能是默认值/被策略隐藏）。")
        )
      )

      (princ "\n\n  正在检测远程桌面剪贴板重定向策略...")
      (setq gpo-rdp (WTF:REG-QUERY "HKLM\\SOFTWARE\\Policies\\Microsoft\\Windows NT\\Terminal Services" "fDisableClipboardRedirection"))
      (cond
        (*WTF-LAST-CMD-INTERRUPTED*
         (princ "\n  [检测到系统命令被意外中断] reg query 未能正常执行完成，")
         (princ "\n  本项跳过，自动继续后续检查...")
         (setq *WTF-INTERRUPTED-CHECKS*
           (cons "组策略注册表(reg query)" *WTF-INTERRUPTED-CHECKS*))
        )
        (gpo-rdp
         (princ (strcat "\n  组策略 fDisableClipboardRedirection = " gpo-rdp
                         (if (wcmatch gpo-rdp "*0x1*")
                           "  <== 组策略已禁止剪贴板重定向！"
                           ""
                         )))
        )
        (T
         (princ "\n  未检测到该组策略限制（或非受限环境）。")
        )
      )

      ;; ---------------- 已加载的第三方 ARX 应用 ----------------
      (princ "\n\n  当前已加载的第三方 ARX/应用模块：")
      (setq arx-list (vl-catch-all-apply 'arx (list)))
      (if (and arx-list (not (vl-catch-all-error-p arx-list)) arx-list)
        (foreach a arx-list (princ (strcat "\n    " a)))
        (princ "\n    （无自定义 ARX 模块加载，或获取失败）")
      )
      (princ "\n  若上方存在非 Autodesk 官方命名的模块，不能排除其挂钩了")
      (princ "\n  剪贴板/OLE 相关接口，可尝试逐一 (command \"ARXUNLOAD\" \"模块名\") 卸载后测试。")

      (princ "\n  正在检测 AutoCAD 运行权限级别...")
      (setq elevated (WTF:IS-ELEVATED-P))
      (if *WTF-LAST-CMD-INTERRUPTED*
        (progn
          (princ "\n  [检测到系统命令被意外中断] whoami /groups 未能正常执行完成，")
          (princ "\n  权限级别判断结果可能不准确，已跳过此项，自动继续后续检查...")
          (setq *WTF-INTERRUPTED-CHECKS*
            (cons "权限级别(whoami)" *WTF-INTERRUPTED-CHECKS*))
        )
      )
      (cond
        ((eq elevated T)
         (princ "\n  [警告] 当前 AutoCAD 以管理员（高完整性级别）身份运行。")
         (princ "\n         若目标粘贴程序(如资源管理器/Word/微信等)不是管理员权限，")
         (princ "\n         Windows 会因完整性级别不同阻止剪贴板数据传递，")
         (princ "\n         表现就是 AutoCAD 端直接提示'无法复制到剪贴板'。")
        )
        ((eq elevated nil)
         (princ "\n  未检测到管理员权限运行（或无法判断，可能被策略限制读取）。")
        )
      )

      ;; ---------------- 实际尝试复制 ----------------
      (princ "\n\n[5/5 实际执行 COPYCLIP]")
      (setq old-cmdecho (getvar "CMDECHO"))
      (setvar "CMDECHO" 0)
      (setq copy-result
        (vl-catch-all-apply 'command (list "._COPYCLIP" ss ""))
      )
      (setvar "CMDECHO" old-cmdecho)

      (if (vl-catch-all-error-p copy-result)
        (progn
          (setq copy-err (vl-catch-all-error-message copy-result))
          (princ (strcat "\n  [LISP 捕获到错误] " copy-err))
        )
        (progn
          (princ "\n  命令已正常返回。")
          (princ "\n  注意：AutoCAD 的'无法复制到剪贴板'消息框是系统级弹窗，")
          (princ "\n  并不会作为 LISP 错误传回，所以即使刚才又弹出了该提示，")
          (princ "\n  这里也只会显示'命令已正常返回'——这恰恰说明问题不在")
          (princ "\n  LISP/选择集层面，而在 AutoCAD 进程之外（见上方 3/4 检测项）。")
        )
      )

      ;; ---------------- 一键修复：重启 rdpclip.exe ----------------
      (if (and proc-lines (WTF:PROC-RUNNING-P proc-lines "RDPCLIP.EXE"))
        (progn
          (setq ans
            (getstring "\n检测到处于远程桌面会话，是否尝试自动重启 rdpclip.exe 后重新复制？[Y/N] <Y>: ")
          )
          (if (or (= ans "") (= (strcase ans) "Y"))
            (progn
              (princ "\n  正在重启 rdpclip.exe ...")
              (WTF:FIX-RDPCLIP)
              (princ "\n  已重启，正在重新尝试复制...")
              (setvar "CMDECHO" 0)
              (setq retry (vl-catch-all-apply 'command (list "._COPYCLIP" ss "")))
              (setvar "CMDECHO" old-cmdecho)
              (if (vl-catch-all-error-p retry)
                (princ (strcat "\n  [仍失败] " (vl-catch-all-error-message retry)))
                (princ "\n  已重新执行 COPYCLIP，请查看是否还弹出错误提示。")
              )
              (princ "\n  若仍提示失败，再粘贴到目标程序验证一次。")
            )
          )
        )
        ;; 未处于远程桌面场景时，提供"重启资源管理器"这一通用修复选项
        ;; （修复经典的"剪贴板查看器链损坏"问题，对所有程序的复制都有效，
        ;;  这也解释了为什么连 Ctrl+C 这种最基础的复制方式也会失败）
        (progn
          (setq ans
            (getstring "\n是否尝试重启 explorer.exe 以修复可能损坏的剪贴板查看器链？[Y/N] <N>: ")
          )
          (if (= (strcase ans) "Y")
            (progn
              (princ "\n  正在重启 explorer.exe（桌面/任务栏会短暂消失属正常现象）...")
              (WTF:FIX-EXPLORER)
              (princ "\n  已重启。请重新用 Ctrl+C 或本命令测试复制。")
            )
          )
        )
      )

      ;; ---------------- 结论 ----------------
      (princ "\n\n[结论与建议]")
      (if *WTF-INTERRUPTED-CHECKS*
        (progn
          (princ "\n- [提示] 本次诊断过程中，以下系统命令检查被意外中断，")
          (princ "\n  结果可能不完整（程序已自动跳过并继续走完其余检查项，")
          (princ "\n  未中止整个诊断）：")
          (foreach nm (reverse *WTF-INTERRUPTED-CHECKS*)
            (princ (strcat "\n    * " nm))
          )
          (princ "\n  如需确认这些项目的真实情况，可重新运行 CHKCLIP，")
          (princ "\n  或手动在命令提示符中执行对应命令核实。")
        )
      )
      (if (> (nth 1 db-scan) 0)
        (progn
          (princ "\n- [根本原因，优先处理] 全库存在代理对象(PROXY)，这是本文件")
          (princ "\n  '独有'复制故障的最可能原因。请对照上方 APPID 列表 / 样本DXF")
          (princ "\n  信息判断这些对象来自哪个第三方应用（国内常见的是天正建筑、")
          (princ "\n  浩辰CAD、探索者TSSD、理正等建筑/结构/电气插件对象），然后：")
          (princ "\n  1) 在本机安装/加载对应的对象增强器(Object Enabler)，或直接")
          (princ "\n     用原厂软件(如天正建筑)打开本图，并使用其自带的")
          (princ "\n     '天正转标准CAD图层/输出为AutoCAD图形'等功能，把自定义")
          (princ "\n     对象彻底转换为标准 AutoCAD 实体，一劳永逸消除代理依赖；")
          (princ "\n  2) 或者把系统变量 PROXYNOTICE 设为 1 后重新打开本文件，")
          (princ "\n     AutoCAD 会直接弹窗告知缺失的第三方应用名称；")
          (princ "\n  3) 临时验证：新建空白图纸，WBLOCK 输出为新文件后打开，若")
          (princ "\n     代理对象在转换过程中被正确处理/丢弃，复制通常就会恢复正常。")
        )
      )
      (if (> proxy-count 0)
        (progn
          (princ "\n- 存在代理/无法识别对象：多为第三方插件生成的专有实体，")
          (princ "\n  本机未加载对应模块时无法转换为剪贴板格式，建议加载对应")
          (princ "\n  应用后再复制，或先炸开/转换为标准实体。")
        )
      )
      (if (> ole-count 0)
        (princ "\n- 存在 OLE 嵌入对象：确认源程序(Word/Excel等)未崩溃/未被占用。")
      )
      (if (> big-count 0)
        (princ "\n- 选中对象过多：建议分批复制，或用 WBLOCK 输出为文件再插入。")
      )
      (if (eq elevated T)
        (princ "\n- [重点排查] AutoCAD 以管理员身份运行，建议改为普通权限启动。")
      )
      (if kw-hit
        (princ "\n- [重点排查] 检测到疑似终端安全/DLP软件进程，请找IT确认剪贴板管控策略。")
      )
      (princ "\n\n- 你提到只有这一个文件复制失败，其它文件用 Ctrl+C 都正常：")
      (princ "\n  这基本排除了操作系统/驱动/权限/常驻软件这些'环境类'原因")
      (princ "\n  （否则应该是所有文件都受影响），问题大概率出在这个 DWG")
      (princ "\n  文件自身的数据库状态上。请重点看上方[3/5 文件级检查]的结果：")
      (if (> (nth 4 db-scan) 0)
        (progn
          (princ "\n  * 已检测到无法正常访问的损坏实体，这是本文件独有问题")
          (princ "\n    的直接证据。建议：")
        )
        (princ "\n  * 本次全库扫描未发现明显损坏实体，仍建议按以下步骤处理：")
      )
      (princ "\n  1) [推荐首选] 新建一张空白图纸，用 INSERT 或 XREF 方式把本")
      (princ "\n     图整体插入进去成为一个块，或者用 WBLOCK 把 * (整个图形)")
      (princ "\n     输出为新文件，再打开新文件测试复制——这一步相当于让")
      (princ "\n     AutoCAD 重新生成一份干净的数据库，能解决绝大多数'仅此")
      (princ "\n     文件'的复制/剪切故障；")
      (princ "\n  2) 查看上方 AUDIT 的输出，如果提示发现并修复了错误，保存后")
      (princ "\n     重新测试；如果 AUDIT 无法完全修复，尝试关闭文件后用")
      (princ "\n     文件 -> 图形实用工具 -> 修复(RECOVER) 重新打开该文件；")
      (princ "\n  3) 如果该文件是从其它版本 AutoCAD、其它厂商 CAD 软件另存")
      (princ "\n     /转换而来，也常带入不完全兼容的数据结构，可尝试 DXFOUT")
      (princ "\n     导出为 DXF 再 DXFIN 导入，相当于对数据库做一次'清洗'；")
      (princ "\n  4) 若该图形包含大量已删除但未清理的对象，执行 PURGE(全部)")
      (princ "\n     后再 AUDIT 一次，减少数据库中的冗余/悬挂引用。")

      (princ "\n\n===== 诊断结束 =====")
      (princ)
    )
  )
  (setq *error* old-error)
  (princ)
)

(princ "\n[已加载] 输入 CHKCLIP 开始诊断剪贴板复制问题；输入 PROXY2STD 可单独批量转换代理对象。")
(princ)
