;;; ======================================================================
;;; WCF.lsp  -  文本批量替换工具
;;; 命令: WCF
;;;
;;; 功能:
;;;   1. 通过 DCL 动态生成对话框(写入系统临时文件夹), 每页含 20 个
;;;      文本输入框, 每个输入框左侧有"拾取"按钮、右侧有"选取赋值"
;;;      按钮; 支持多页, 页数随数据量动态变化, 理论无上限。
;;;   2. 翻页时不关闭对话框, 直接替换文本框内容显示; 上一页/下一页
;;;      按钮始终存在于界面中, 上一页在第1页时禁用, 下一页始终启用
;;;      (允许用户翻到新页添加内容); 页码通过独立文本控件动态显示。
;;;   3. 程序会记住关闭前所在的页码, 下次运行 WCF 时直接进入上次关闭
;;;      前所在的那一页。
;;;   4. 点击"选取赋值"按钮后:
;;;        - 对话框关闭, 用户可框选/连续选取多个文字/多行文字/属性
;;;          对象(可多选, 回车结束选择), 将文字内容批量赋给所有选中
;;;          对象;
;;;        - 赋值后停留在绘图区, 用户能看到替换结果并继续选取;
;;;        - 无选择(空格/回车)时才返回对话框界面。
;;;   5. "清空本页"按钮: 点击时先把当前页 20 个输入框的内容, 按文本框
;;;      顺序逐行写入系统剪贴板(每个文本框一行, 空的也占一行), 然后
;;;      再清空这 20 个输入框; 不会关闭对话框。
;;;   6. "粘贴"按钮: 读取系统剪贴板中的内容, 按行拆分, 依次逐行填入
;;;      当前页的每个文本框(第 1 行 -> 第 1 个框, 第 2 行 -> 第 2 个
;;;      框, 以此类推); 剪贴板行数不足 20 行时, 多余的文本框会被清
;;;      空; 不会关闭对话框。
;;;   7. 因为标准 DCL 对话框是模态的, 无法在对话框显示时直接拾取图元,
;;;      本程序在需要拾取图元时采用"关闭对话框 -> 拾取图元 -> 重新打开
;;;      对话框(保留已填内容)"的方式; 翻页操作不需要拾取图元, 因此
;;;      翻页时不关闭对话框, 直接替换文本框内容, 提升响应速度。
;;;   8. 所有输入框内容以及当前页码, 均会在每次操作后自动保存到临时
;;;      文件夹的数据文件中, 下次运行自动读取并回显。
;;;   9. 弹药箱功能:
;;;        - "设置"按钮: 选择军火库文件夹, 路径保存到配置文件, 下次
;;;          自动读取。
;;;        - "弹药箱"按钮: 从军火库中选择文本文件, 按行装填到所有
;;;          文本框(先清空全部内容, 再从文件逐行填充, 空行对应文本框
;;;          留空; 无数量上限, 超出每页 20 条的部分自动分页显示)。
;;;        - "入库"按钮: 将当前所有文本框的内容逐行写入新文本文件,
;;;          保存到军火库文件夹, 用户指定文件名。
;;;  10. 每个文本输入框前的"拾取"按钮: 点击后对话框关闭, 引导用户
;;;       选择一个文字对象(文字/多行文字/属性), 程序自动提取其文字
;;;       内容, 清理 MTEXT 格式控制码(字体/高度/颜色/换行符/堆叠等),
;;;       将纯文本填入对应的输入框; 多行文字的换行替换为空格, 连续
;;;       空格合并为一个。
;;;  11. "总拾取"按钮(位于"设置"按钮右侧): 点击后对话框关闭, 引导
;;;       用户选择一系列文字对象(文字/多行文字/属性); 既可单击逐个
;;;       拾取, 也可窗口/交叉框选一次性拾取多个, 并可分多轮选择,
;;;       直到某一轮不再选择任何对象(直接空格/回车)才结束。若拾取
;;;       内容中存在完全相同的重复文本, 自动去重(保留首次出现的顺序);
;;;       去重后复用"整理"按钮的增强排序逻辑对结果重新排序; 随后
;;;       清空全部文本框, 按排序后的顺序从第 1 页第 1 个文本框开始
;;;       整体覆盖填入; 无数量上限, 超出 20 条自动分页。
;;; ======================================================================

(vl-load-com)

;; ---------------- 全局路径与常量 ----------------
(setq *wcf-data-file*  (strcat (getenv "TEMP") "\\wcf_data.dat"))
(setq *wcf-page-file*  (strcat (getenv "TEMP") "\\wcf_page.dat"))
(setq *wcf-dcl-file*   (strcat (getenv "TEMP") "\\wcf_dialog.dcl"))
(setq *wcf-clip-out*   (strcat (getenv "TEMP") "\\wcf_clip_out.txt"))
(setq *wcf-clip-in*    (strcat (getenv "TEMP") "\\wcf_clip_in.txt"))
(setq *wcf-ammo-config* (strcat (getenv "TEMP") "\\wcf_ammo.cfg"))
(setq *wcf-per-page* 20)                                 ;; 每页输入框数量

;; ---------------- 计算总页数(根据数据长度动态计算) ----------------
(defun wcf:calc-pages ( / n)
  (setq n (length *wcf-vals*))
  (if (= n 0)
    1
    (if (= (rem n *wcf-per-page*) 0)
      (/ n *wcf-per-page*)
      (1+ (/ n *wcf-per-page*))
    )
  )
)

;; ---------------- 确保数据列表足够长(至少能覆盖到指定索引) ----------------
(defun wcf:ensure-vals-length (needed / )
  (while (< (length *wcf-vals*) needed)
    (setq *wcf-vals* (append *wcf-vals* (list "")))
  )
)

;; ---------------- 数据持久化: 读取上次保存的文本内容 ----------------
(defun wcf:load-values ( / f line lst)
  (setq lst '())
  (if (setq f (open *wcf-data-file* "r"))
    (progn
      (while (setq line (read-line f))
        (setq lst (append lst (list line)))
      )
      (close f)
    )
  )
  ;; 确保至少有一页的数据(20条)
  (while (< (length lst) *wcf-per-page*)
    (setq lst (append lst (list "")))
  )
  lst
)

;; ---------------- 数据持久化: 保存当前文本内容 ----------------
(defun wcf:save-values (lst / f)
  (setq f (open *wcf-data-file* "w"))
  (foreach v lst
    (write-line (if v v "") f)
  )
  (close f)
  (princ)
)

;; ---------------- 页码持久化: 读取上次关闭前所在的页 ----------------
(defun wcf:load-page ( / f line n)
  (setq n 0)
  (if (setq f (open *wcf-page-file* "r"))
    (progn
      (setq line (read-line f))
      (close f)
      (if (and line (numberp (setq n (atoi line))))
        (progn
          (if (< n 0) (setq n 0))
        )
        (setq n 0)
      )
    )
  )
  n
)

;; ---------------- 页码持久化: 保存当前所在页 ----------------
(defun wcf:save-page (page / f)
  (setq f (open *wcf-page-file* "w"))
  (write-line (itoa page) f)
  (close f)
  (princ)
)

;; ---------------- 军火库路径持久化: 读取 ----------------
(defun wcf:load-ammo-path ( / f line)
  (setq line "")
  (if (setq f (open *wcf-ammo-config* "r"))
    (progn
      (setq line (read-line f))
      (close f)
      (if (null line) (setq line ""))
    )
  )
  line
)

;; ---------------- 军火库路径持久化: 保存 ----------------
(defun wcf:save-ammo-path (path / f)
  (setq f (open *wcf-ammo-config* "w"))
  (write-line path f)
  (close f)
  (princ)
)

;; ---------------- 通过 WScript.Shell 同步执行命令行(阻塞等待完成) ----------------
(defun wcf:shell-run-wait (cmd / wsh)
  (setq wsh (vlax-create-object "WScript.Shell"))
  (vlax-invoke-method wsh 'Run cmd 0 :vlax-true)
  (vlax-release-object wsh)
  (princ)
)

;; ---------------- 清理空白字符: 换行/制表符转空格, 合并连续空格, 去首尾空格 ----------------
(defun wcf:clean-whitespace (str / result len i ch c prev-space)
  (setq result "")
  (setq len (strlen str))
  (setq i 1)
  (setq prev-space nil)
  (while (<= i len)
    (setq ch (substr str i 1))
    (setq c (ascii ch))
    (cond
      ;; 换行(10) / 回车(13) -> 直接跳过(不插入空格)
      ((or (= c 10) (= c 13))
        )
      ;; 制表符(9) -> 空格
      ((= c 9)
        (if (not prev-space)
          (setq result (strcat result " ") prev-space T)))
      ;; 空格(32)
      ((= c 32)
        (if (not prev-space)
          (setq result (strcat result " ") prev-space T)))
      ;; 其他字符(含中文)
      (t
        (setq result (strcat result ch) prev-space nil)))
    (setq i (1+ i)))
  (vl-string-trim " " result))

;; ---------------- 清理 MTEXT 格式符号, 提取纯文本内容 ----------------
;; 处理 MTEXT 中的格式控制码:
;;   \P             -> 段落换行, 直接跳过(不插入空格)
;;   \f...; \F...;  -> 字体设置, 移除
;;   \H...; \h...;  -> 文字高度, 移除
;;   \C...; \c...;  -> 颜色, 移除
;;   \Q...; \q...;  -> 倾斜角, 移除
;;   \T...; \t...;  -> 字符间距, 移除
;;   \W...; \w...;  -> 宽度因子, 移除
;;   \A0; \A1; \A2; -> 对齐, 移除
;;   \S...;         -> 堆叠/分数, 保留内部文本
;;   \L \l \O \o \K \k -> 下划线/上划线/删除线开关, 移除
;;   \\  -> 字面反斜杠
;;   \{ \} -> 字面花括号
;;   \~  -> 不换行空格, 替换为空格
;;   { } -> 分组花括号, 移除
;;   换行符/制表符 -> 空格; 连续空格 -> 合并
(defun wcf:clean-mtext (str / result len i ch ch2 temp)
  (setq result "")
  (setq len (strlen str))
  (setq i 1)
  (while (<= i len)
    (setq ch (substr str i 1))
    (cond
      ;; ---- 反斜杠转义序列 ----
      ((= ch "\\")
        (if (< i len)
          (progn
            (setq ch2 (substr str (+ i 1) 1))
            (cond
              ;; \\ -> 字面反斜杠
              ((= ch2 "\\")
                (setq result (strcat result "\\"))
                (setq i (+ i 2)))
              ;; \{ -> 字面 {
              ((= ch2 "{")
                (setq result (strcat result "{"))
                (setq i (+ i 2)))
              ;; \} -> 字面 }
              ((= ch2 "}")
                (setq result (strcat result "}"))
                (setq i (+ i 2)))
              ;; \P -> 段落换行, 直接跳过(不插入空格)
              ((= ch2 "P")
                (setq i (+ i 2)))
              ;; \~ -> 不换行空格
              ((= ch2 "~")
                (setq result (strcat result " "))
                (setq i (+ i 2)))
              ;; \S...; -> 堆叠/分数, 保留内部文本
              ((= ch2 "S")
                (setq i (+ i 2))
                (setq temp "")
                (while (and (<= i len) (/= (substr str i 1) ";"))
                  (setq temp (strcat temp (substr str i 1)))
                  (setq i (1+ i)))
                (if (<= i len) (setq i (1+ i)))
                (setq result (strcat result temp)))
              ;; 单字符开关码: \L \l \O \o \K \k
              ((member ch2 '("L" "l" "O" "o" "K" "k"))
                (setq i (+ i 2)))
              ;; 带分号的格式码: \A \f \F \H \h \C \c \Q \q \T \t \W \w \U \M \Z
              ((member ch2 '("A" "f" "F" "H" "h" "C" "c" "Q" "q" "T" "t" "W" "w" "U" "M" "Z"))
                (setq i (+ i 2))
                (while (and (<= i len) (/= (substr str i 1) ";"))
                  (setq i (1+ i)))
                (if (<= i len) (setq i (1+ i))))
              ;; 未知转义: 输出反斜杠后的字符
              (t
                (setq result (strcat result ch2))
                (setq i (+ i 2)))))
          ;; 反斜杠在字符串末尾: 跳过
          (setq i (1+ i))))
      ;; ---- 分组花括号: 移除 ----
      ((or (= ch "{") (= ch "}"))
        (setq i (1+ i)))
      ;; ---- 普通字符 ----
      (t
        (setq result (strcat result ch))
        (setq i (1+ i)))))
  ;; 清理空白字符, 合并连续空格, 去首尾空格
  (wcf:clean-whitespace result))

;; ---------------- 拾取文字: 引导用户选择一个文字对象, 返回清理后的纯文本 ----------------
;; 支持 TEXT / MTEXT / ATTRIB / ATTDEF
;; 多行文字会清理 MTEXT 格式控制码, 换行直接拼接(不插入空格)
;; 用户按回车取消时返回 nil
(defun wcf:pick-text ( / ent obj etype txt done)
  (setq done nil)
  (while (not done)
    (setq ent (entsel "\n请选择一个文字对象(文字/多行文字/属性), 或按回车取消: "))
    (if (null ent)
      (setq done T)
      (progn
        (setq obj (vlax-ename->vla-object (car ent)))
        (if obj
          (progn
            (setq etype (vla-get-ObjectName obj))
            (if (member etype '("AcDbText" "AcDbMText" "AcDbAttribute" "AcDbAttributeDefinition"))
              (progn
                (setq txt (wcf:clean-mtext (vla-get-TextString obj)))
                (setq done T))
              (princ "\n选中的不是文字对象, 请重新选择。")))
          (princ "\n无法获取对象信息, 请重新选择。")))))
  txt
)

;; ---------------- 动态生成 DCL 文件(固定结构, 含上一页/下一页/页码显示) ----------------
;; DCL 结构固定, 不再按页码条件生成按钮; 翻页时通过 set_tile 替换文本框内容,
;; 通过 mode_tile 启用/禁用按钮, 不需要重新加载对话框。
(defun wcf:write-dcl ( / f i)
  (setq f (open *wcf-dcl-file* "w"))
  (write-line "wcf_dialog : dialog {" f)
  (write-line "  label = \"文本批量替换工具\";" f)
  (write-line "  : column {" f)
  (setq i 1)
  (while (<= i *wcf-per-page*)
    (write-line "    : row {" f)
    (write-line
      (strcat "      : button { key = \"pick" (itoa i)
              "\"; label = \"拾取\"; fixed_width = true; width = 8; }")
      f)
    (write-line
      (strcat "      : edit_box { key = \"edit" (itoa i)
              "\"; edit_width = 50; }")
      f)
    (write-line
      (strcat "      : button { key = \"btn" (itoa i)
              "\"; label = \"选取赋值\"; fixed_width = true; width = 10; }")
      f)
    (write-line "    }" f)
    (setq i (1+ i))
  )
  (write-line "    spacer_1;" f)
  ;; 页码显示行
  (write-line "    : row {" f)
  (write-line "      : text { key = \"page_label\"; label = \"\"; width = 30; alignment = centered; }" f)
  (write-line "    }" f)
  ;; 导航与功能按钮行
  (write-line "    : row {" f)
  (write-line "      : button { key = \"prev\"; label = \"上一页\"; width = 9; }" f)
  (write-line "      : button { key = \"next\"; label = \"下一页\"; width = 9; }" f)
  (write-line "      : button { key = \"clear\"; label = \"清空本页\"; width = 9; }" f)
  (write-line "      : button { key = \"paste\"; label = \"粘贴\"; width = 9; }" f)
  (write-line "      : button { key = \"sort\"; label = \"整理\"; width = 9; }" f)
  (write-line "      : button { key = \"close\"; label = \"关闭\"; is_cancel = true; width = 9; }" f)
  (write-line "    }" f)
  (write-line "    : row {" f)
  (write-line "      : button { key = \"ammo\"; label = \"弹药箱\"; width = 9; }" f)
  (write-line "      : button { key = \"setammo\"; label = \"设置\"; width = 9; }" f)
  (write-line "      : button { key = \"pickall\"; label = \"总拾取\"; width = 9; }" f)
  (write-line "      : button { key = \"saveammo\"; label = \"入库\"; width = 9; }" f)
  (write-line "    }" f)
  (write-line "  }" f)
  (write-line "}" f)
  (close f)
  (princ)
)

;; ---------------- 将文字内容批量赋值给用户选中的对象(支持多选, 可连续操作) ----------------
;; 有选择: 执行赋值并停留在绘图区, 用户可继续选取
;; 无选择(空格/回车): 退出循环, 返回对话框界面
(defun wcf:assign-to-selected (str / ss i n ent obj etype cnt)
  (princ "\n请选择需要替换文字内容的对象(文字/多行文字/属性, 可多选, 回车结束选择): ")
  (while (setq ss (ssget '((0 . "TEXT,MTEXT,ATTRIB,ATTDEF"))))
    (setq cnt 0)
    (setq i 0)
    (setq n (sslength ss))
    (while (< i n)
      (setq ent (ssname ss i))
      (setq obj (vlax-ename->vla-object ent))
      (setq etype (vla-get-ObjectName obj))
      (if (member etype '("AcDbText" "AcDbMText" "AcDbAttribute" "AcDbAttributeDefinition"))
        (progn
          (vla-put-TextString obj str)
          (setq cnt (1+ cnt))
        )
      )
      (setq i (1+ i))
    )
    (princ (strcat "\n已将 " (itoa cnt) " 个对象的文字替换为: " str))
    (princ "\n可继续选取对象, 或按空格/回车返回界面。")
  )
  (princ "\n返回界面...")
  (princ)
)

;; ---------------- 在对话框内点击按钮/翻页时: 抓取当前页所有输入框内容 ----------------
;; offset: 当前页在总列表中的起始索引(第1页=0, 第2页=20, 以此类推)
(defun wcf:capture-values (offset / i)
  (wcf:ensure-vals-length (+ offset *wcf-per-page*))
  (setq i 0)
  (while (< i *wcf-per-page*)
    (setq *wcf-vals*
      (wcf:list-set *wcf-vals* (+ offset i) (get_tile (strcat "edit" (itoa (1+ i)))))
    )
    (setq i (1+ i))
  )
  (princ)
)

;; ---------------- 显示指定页内容(只刷新文本框, 不重建界面) ----------------
(defun wcf:show-page ( / offset i total-pages)
  (setq offset (* *wcf-page* *wcf-per-page*))
  (setq total-pages (wcf:calc-pages))
  (set_tile "page_label" (strcat "第 " (itoa (1+ *wcf-page*)) " / " (itoa total-pages) " 页"))
  (setq i 0)
  (while (< i *wcf-per-page*)
    (set_tile (strcat "edit" (itoa (1+ i))) (nth (+ offset i) *wcf-vals*))
    (setq i (1+ i))
  )
  (if (> *wcf-page* 0)
    (mode_tile "prev" 0)
    (mode_tile "prev" 1)
  )
  (mode_tile "next" 0)
  (princ)
)

;; ---------------- 翻页(不关闭对话框, 直接替换文本框内容) ----------------
;; new-page: 目标页码(0基)
;; 流程: 先抓取当前页内容 -> 确保数据足够长 -> 切换页码 -> 更新页码显示 ->
;;       填入新页文本框内容 -> 启用/禁用翻页按钮
(defun wcf:goto-page (new-page / offset i total-pages)
  (if (< new-page 0)
    (princ)  ;; 不允许翻到负数页
    (progn
      ;; 先抓取当前页所有输入框内容
      (wcf:capture-values (* *wcf-page* *wcf-per-page*))
      ;; 确保数据列表足够长, 能覆盖目标页
      (wcf:ensure-vals-length (* (1+ new-page) *wcf-per-page*))
      ;; 更新页码
      (setq *wcf-page* new-page)
      ;; 只刷新当前文本框内容，不重新生成DCL界面
      (wcf:show-page)
    )
  )
  (princ)
)

;; ---------------- 清空本页: 先把当前页内容按行写入剪贴板, 再清空界面 ----------------
(defun wcf:copy-page-to-clipboard-and-clear ( / i lines f)
  ;; 读取当前页 20 个输入框内容
  (setq lines '())
  (setq i 1)
  (while (<= i *wcf-per-page*)
    (setq lines (append lines (list (get_tile (strcat "edit" (itoa i))))))
    (setq i (1+ i))
  )
  ;; 写入临时文件, 每个文本框一行
  (setq f (open *wcf-clip-out* "w"))
  (foreach v lines (write-line (if v v "") f))
  (close f)
  ;; 通过系统 clip 命令把临时文件内容送入剪贴板
  (wcf:shell-run-wait (strcat "cmd.exe /c clip < \"" *wcf-clip-out* "\""))
  ;; 清空界面上的 20 个输入框
  (setq i 1)
  (while (<= i *wcf-per-page*)
    (set_tile (strcat "edit" (itoa i)) "")
    (setq i (1+ i))
  )
  (princ)
)

;; ---------------- 粘贴: 读取剪贴板内容, 按行填入当前页每个输入框 ----------------
(defun wcf:paste-clipboard-into-page ( / f line lines i cmd)
  ;; 通过 PowerShell 把剪贴板文本原样写到临时文件(使用系统默认 ANSI 编码, 避免中文乱码)
  (setq cmd
    (strcat
      "powershell -NoProfile -Command \"$c = Get-Clipboard -Raw; "
      "if ($null -eq $c) { $c = '' }; "
      "[System.IO.File]::WriteAllText('" *wcf-clip-in* "', $c, [System.Text.Encoding]::Default)\""
    )
  )
  (wcf:shell-run-wait cmd)
  ;; 读取临时文件, 按行拆分
  (setq lines '())
  (if (setq f (open *wcf-clip-in* "r"))
    (progn
      (while (setq line (read-line f))
        (setq lines (append lines (list line)))
      )
      (close f)
    )
  )
  ;; 逐行填入当前页每个输入框, 不足的清空
  (setq i 1)
  (while (<= i *wcf-per-page*)
    (set_tile
      (strcat "edit" (itoa i))
      (if (<= i (length lines)) (nth (1- i) lines) "")
    )
    (setq i (1+ i))
  )
  (princ)
)

;; ---------------- 增强排序辅助函数（从 DBQ.lsp 移植） ----------------
(defun wcf:parse-trailing-number (text / codes n i numcodes numlen)
  (setq codes (vl-string->list text)
        n     (length codes)
        i     (1- n)
        numcodes nil)
  (while (>= i 0)
    (if (and (>= (nth i codes) 48) (<= (nth i codes) 57))
      (setq numcodes (cons (nth i codes) numcodes)
            i (1- i))
      (setq i -1)))
  (if numcodes
    (progn
      (setq numlen (length numcodes))
      (list (substr text 1 (- (strlen text) numlen))
            (atoi (vl-list->string numcodes))))
    (list text 0)))

(defun wcf:enhanced-less-p (a b / pa pb)
  (setq pa (wcf:parse-trailing-number a)
        pb (wcf:parse-trailing-number b))
  (cond
    ((< (car pa) (car pb)) t)
    ((> (car pa) (car pb)) nil)
    (t (< (cadr pa) (cadr pb)))))

;; ---------------- 整理: 读取当前页内容, 增强排序后重新填入 ----------------
(defun wcf:sort-current-page ( / i lines)
  ;; 读取当前页 20 个输入框内容
  (setq lines '())
  (setq i 1)
  (while (<= i *wcf-per-page*)
    (setq lines (append lines (list (get_tile (strcat "edit" (itoa i))))))
    (setq i (1+ i))
  )
  ;; 排除空行
  (setq lines (vl-remove-if '(lambda (s) (= (vl-string-trim " \t\r\n" s) "")) lines))
  ;; 增强排序
  (setq lines (vl-sort lines 'wcf:enhanced-less-p))
  ;; 清空界面上的 20 个输入框
  (setq i 1)
  (while (<= i *wcf-per-page*)
    (set_tile (strcat "edit" (itoa i)) "")
    (setq i (1+ i))
  )
  ;; 将排序后的内容逐行填入
  (setq i 1)
  (while (<= i (length lines))
    (set_tile (strcat "edit" (itoa i)) (nth (1- i) lines))
    (setq i (1+ i))
  )
  (princ)
)

;; ---------------- 辅助函数: 替换列表中指定索引的元素 ----------------
(defun wcf:list-set (lst idx val / i result)
  (setq i 0 result '())
  (foreach item lst
    (setq result (append result (list (if (= i idx) val item))))
    (setq i (1+ i))
  )
  result
)

;; ---------------- 弹药箱: 设置军火库文件夹路径 ----------------
;; 优化说明: 原实现每次点击"设置"按钮都要把一段 ps1 脚本写到磁盘,
;; 再启动一个全新的 powershell.exe 进程去执行它, 脚本里还要
;; Add-Type 加载 System.Windows.Forms 这个 .NET 程序集才能弹出
;; 文件夹选择框。"新建进程 + 加载 .NET 程序集 + 执行策略检查"这一
;; 整套流程通常要耗时 1~3 秒甚至更久, 这正是点击设置按钮后界面
;; 半天弹不出来的根本原因。
;; 现改为直接在 AutoCAD 进程内通过系统自带的 Shell.Application
;; COM 组件调用 BrowseForFolder 方法弹出文件夹选择框, 不再新建
;; 外部进程, 弹窗速度可提升到几乎瞬时。
;; 若此前已设置过军火库路径, 先用 WScript.Shell.Popup 弹一个
;; 是/否询问框(该方法直接调用系统消息框 API, 不新建进程, 不影响
;; 速度):
;;   选"是" -> 把上次路径作为 RootFolder 传入 BrowseForFolder,
;;             对话框直接定位/展开到该文件夹(适合只是想换成里面
;;             的另一个子文件夹这种高频场景);
;;   选"否" -> 不传 RootFolder, 完全自由导航(可以选任意盘符/
;;             任意新路径, 适合彻底更换军火库位置这种场景)。
;; 若此前从未设置过路径(第一次使用), 直接进入完全自由导航, 不
;; 弹询问框。
(defun wcf:set-ammo ( / oldpath wsh answer shellapp folderobj folderitem path)
  (setq path nil)
  (setq oldpath (wcf:load-ammo-path))
  (setq answer 7)  ;; 默认按"否"处理(自由导航), 对应无历史路径的情况
  (if (and oldpath (/= oldpath "") (vl-file-directory-p oldpath))
    (progn
      (setq wsh (vlax-create-object "WScript.Shell"))
      (setq answer
        (vlax-invoke-method wsh 'Popup
          (strcat "是否进入已设置的军火库文件夹?\n" oldpath
                  "\n\n[是] 在该文件夹范围内选择\n[否] 重新选择全新的军火库位置")
          0 "军火库设置" 36
        )
      )
      (vlax-release-object wsh)
    )
  )
  (setq shellapp (vlax-create-object "Shell.Application"))
  (if (= answer 6)
    ;; 用户选"是": 定位到已设置的文件夹, 浏览范围限制在其内部
    (setq folderobj (vlax-invoke-method shellapp 'BrowseForFolder 0 "选择军火库文件夹" 0 oldpath))
    ;; 用户选"否", 或从未设置过路径: 完全自由导航
    (setq folderobj (vlax-invoke-method shellapp 'BrowseForFolder 0 "选择军火库文件夹" 0))
  )
  (if folderobj
    (progn
      (setq folderitem (vlax-get-property folderobj 'Self))
      (setq path (vlax-get-property folderitem 'Path))
      (vlax-release-object folderitem)
      (vlax-release-object folderobj)
    )
  )
  (vlax-release-object shellapp)
  (if (and path (/= path ""))
    (progn
      (wcf:save-ammo-path path)
      (princ (strcat "\n军火库设置为: " path))
    )
    (princ "\n未选择文件夹, 操作取消。")
  )
  (princ)
)


;; ---------------- 总拾取: 支持框选拾取多个文字对象, 去重排序后整体覆盖填入全部文本框 ----------------
;; 引导用户选择文字/多行文字/属性对象: 既支持单击逐个拾取, 也支持
;; 窗口/交叉框选一次性拾取多个; 可分多次选择, 直到某一轮不再选择
;; 任何对象(直接空格/回车)才结束。去重后复用"整理"按钮的增强排序
;; 逻辑重新排序, 再整体覆盖填入(先清空全部内容, 按排序后的顺序填入;
;; 无数量上限, 超出每页 20 条的部分自动分页显示)。
(defun wcf:pick-all-text ( / texts uniq ss n i ent obj etype txt cnt)
  (setq texts '())
  (princ "\n请选择文字对象(文字/多行文字/属性): 可单击逐个拾取, 也可窗口/交叉框选一次拾取多个, 可分多轮选择; 全部完成后按空格/回车结束: ")
  (while (setq ss (ssget '((0 . "TEXT,MTEXT,ATTRIB,ATTDEF"))))
    (setq n (sslength ss))
    (setq i 0)
    (while (< i n)
      (setq ent (ssname ss i))
      (setq obj (vlax-ename->vla-object ent))
      (setq etype (vla-get-ObjectName obj))
      (if (member etype '("AcDbText" "AcDbMText" "AcDbAttribute" "AcDbAttributeDefinition"))
        (progn
          (setq txt (wcf:clean-mtext (vla-get-TextString obj)))
          (setq texts (append texts (list txt)))
        )
      )
      (setq i (1+ i))
    )
    (princ (strcat "\n本轮选中 " (itoa n) " 个, 累计拾取 " (itoa (length texts)) " 个。"))
    (princ "\n可继续选择(点选/框选), 或按空格/回车结束拾取: ")
  )
  ;; 去重: 保留首次出现的顺序, 完全相同的文本内容只保留一份
  (setq uniq '())
  (foreach v texts
    (if (not (member v uniq))
      (setq uniq (append uniq (list v)))
    )
  )
  ;; 复用"整理"按钮的增强排序逻辑, 对去重后的内容重新排序
  (setq uniq (vl-sort uniq 'wcf:enhanced-less-p))
  (if (= (length uniq) 0)
    (princ "\n未拾取任何文字对象, 操作取消。")
    (progn
      ;; 清空全部数据
      (setq *wcf-vals* '())
      ;; 按排序后的顺序整体覆盖填入, 无数量上限
      (setq cnt 0)
      (while (< cnt (length uniq))
        (setq *wcf-vals* (append *wcf-vals* (list (nth cnt uniq))))
        (setq cnt (1+ cnt))
      )
      ;; 确保至少有一页数据
      (wcf:ensure-vals-length *wcf-per-page*)
      ;; 回到第1页
      (setq *wcf-page* 0)
      (if (< (length uniq) (length texts))
        (princ
          (strcat "\n总拾取完成: 共拾取 " (itoa (length texts)) " 个, 去重后 "
                  (itoa (length uniq)) " 个, 已按整理规则排序后整体覆盖填入 "
                  (itoa (length uniq)) " 个文本框。")
        )
        (princ
          (strcat "\n总拾取完成: 共拾取 " (itoa (length texts))
                  " 个(无重复), 已按整理规则排序后整体覆盖填入 " (itoa (length uniq)) " 个文本框。")
        )
      )
    )
  )
  (princ)
)

;; ---------------- 弹药箱: 从军火库文件装填子弹到全部文本框 ----------------
;; 清空全部数据, 再从文件逐行填充(空行对应文本框留空), 无数量上限
(defun wcf:load-from-ammo ( / ammo-path filepath f line lines i cnt)
  (setq ammo-path (wcf:load-ammo-path))
  (if (or (null ammo-path) (= ammo-path ""))
    (princ "\n请先点击\"设置\"按钮指定军火库文件夹。")
    (progn
      (setq filepath (getfiled "选择弹药箱文件" (strcat ammo-path "\\") "txt" 4))
      (if (null filepath)
        (princ "\n未选择文件, 装填取消。")
        (progn
          ;; 读取文件所有行
          (setq lines '())
          (if (setq f (open filepath "r"))
            (progn
              (while (setq line (read-line f))
                (setq lines (append lines (list line)))
              )
              (close f)
            )
          )
          ;; 清空全部数据
          (setq *wcf-vals* '())
          ;; 从文件逐行填充, 无数量上限
          (setq i 0)
          (while (< i (length lines))
            (setq *wcf-vals* (append *wcf-vals* (list (nth i lines))))
            (setq i (1+ i))
          )
          ;; 确保至少有一页数据
          (wcf:ensure-vals-length *wcf-per-page*)
          ;; 回到第1页
          (setq *wcf-page* 0)
          (setq cnt (length lines))
          (princ (strcat "\n已从弹药箱装填 " (itoa cnt) " 发子弹。"))
        )
      )
    )
  )
  (princ)
)

;; ---------------- 弹药箱: 将全部内容入库到军火库新文件 ----------------
;; 读取所有文本框内容, 逐行写入用户指定的新文件
(defun wcf:save-to-ammo ( / ammo-path filepath f i total)
  (setq ammo-path (wcf:load-ammo-path))
  (if (or (null ammo-path) (= ammo-path ""))
    (princ "\n请先点击\"设置\"按钮指定军火库文件夹。")
    (progn
      (setq filepath (getfiled "入库到弹药箱" (strcat ammo-path "\\") "txt" 1))
      (if (null filepath)
        (princ "\n取消入库。")
        (progn
          (setq f (open filepath "w"))
          (setq i 0)
          (setq total (length *wcf-vals*))
          (while (< i total)
            (write-line (if (nth i *wcf-vals*) (nth i *wcf-vals*) "") f)
            (setq i (1+ i))
          )
          (close f)
          (princ (strcat "\n已入库 " (itoa total) " 发子弹到: " filepath))
        )
      )
    )
  )
  (princ)
)

;; ---------------- 主命令 ----------------
(defun c:WCF ( / dcl_id code i str offset idx dynold picked-text total-pages)
  (setq *wcf-vals* (wcf:load-values))
  (setq *wcf-page* (wcf:load-page))
  ;; 确保页码不越界
  (setq total-pages (wcf:calc-pages))
  (if (>= *wcf-page* total-pages)
    (setq *wcf-page* (1- total-pages))
  )
  (if (< *wcf-page* 0)
    (setq *wcf-page* 0)
  )
  ;; 确保数据足够覆盖当前页
  (wcf:ensure-vals-length (* (1+ *wcf-page*) *wcf-per-page*))
  (setq dynold (getvar "DYNMODE"))
  (setvar "DYNMODE" 0)
  (setq code 999)
  (while (/= code 0)
    (wcf:write-dcl)
    ;; 确保数据足够覆盖当前页(每次循环都检查, 防止数据被外部操作修改后越界)
    (wcf:ensure-vals-length (* (1+ *wcf-page*) *wcf-per-page*))
    (setq offset (* *wcf-page* *wcf-per-page*))
    (setq dcl_id (load_dialog *wcf-dcl-file*))
    (if (not (new_dialog "wcf_dialog" dcl_id))
      (progn
        (princ "\n对话框加载失败。")
        (unload_dialog dcl_id)
        (setq code 0)
      )
      (progn
        ;; 回显当前页保存的内容
        (setq i 0)
        (while (< i *wcf-per-page*)
          (set_tile (strcat "edit" (itoa (1+ i))) (nth (+ offset i) *wcf-vals*))
          (setq i (1+ i))
        )
        ;; 显示当前页码
        (setq total-pages (wcf:calc-pages))
        (set_tile "page_label" (strcat "第 " (itoa (1+ *wcf-page*)) " / " (itoa total-pages) " 页"))
        ;; 根据页码边界启用/禁用翻页按钮
        (if (> *wcf-page* 0)
          (mode_tile "prev" 0)
          (mode_tile "prev" 1)
        )
        ;; 下一页始终启用(允许用户翻到新页添加内容)
        (mode_tile "next" 0)
        ;; 绑定每个"选取赋值"按钮: 先抓取本页全部输入内容, 再以 100+序号 的编码关闭对话框
        (setq i 1)
        (while (<= i *wcf-per-page*)
          (action_tile
            (strcat "btn" (itoa i))
            (strcat "(wcf:capture-values " (itoa offset) ")(done_dialog " (itoa (+ 100 i)) ")")
          )
          (setq i (1+ i))
        )
        ;; 绑定每个"拾取"按钮: 先抓取本页全部输入内容, 再以 200+序号 的编码关闭对话框
        (setq i 1)
        (while (<= i *wcf-per-page*)
          (action_tile
            (strcat "pick" (itoa i))
            (strcat "(wcf:capture-values " (itoa offset) ")(done_dialog " (itoa (+ 200 i)) ")")
          )
          (setq i (1+ i))
        )
        ;; 翻页按钮: 不关闭对话框, 直接替换文本框内容
        (action_tile "prev" "(wcf:goto-page (1- *wcf-page*))")
        (action_tile "next" "(wcf:goto-page (1+ *wcf-page*))")
        ;; 清空按钮: 先复制到剪贴板再清空界面; 粘贴按钮: 从剪贴板按行填回; 整理按钮: 增强排序后重新填入; 均不关闭对话框
        (action_tile "clear" "(wcf:copy-page-to-clipboard-and-clear)")
        (action_tile "paste" "(wcf:paste-clipboard-into-page)")
        (action_tile "sort" "(wcf:sort-current-page)")
        (action_tile "close" (strcat "(wcf:capture-values " (itoa offset) ")(done_dialog 0)"))
        ;; 弹药箱按钮: 600=设置军火库, 601=装填弹药, 602=入库, 603=总拾取; 均先抓取当前页再关闭对话框
        (action_tile "setammo"  (strcat "(wcf:capture-values " (itoa offset) ")(done_dialog 600)"))
        (action_tile "ammo"     (strcat "(wcf:capture-values " (itoa offset) ")(done_dialog 601)"))
        (action_tile "saveammo" (strcat "(wcf:capture-values " (itoa offset) ")(done_dialog 602)"))
        (action_tile "pickall"  (strcat "(wcf:capture-values " (itoa offset) ")(done_dialog 603)"))
        (redraw)
        (mode_tile "btn1" 2)
        (setq code (start_dialog))
        (unload_dialog dcl_id)
        ;; 每次关闭对话框(无论何种方式)都立即保存文本内容, 保证内容不丢失
        (wcf:save-values *wcf-vals*)
        (cond
          ;; 翻页操作不再通过 done_dialog 处理(已改为不关闭对话框, 直接替换文本框内容)
          ;; 设置军火库路径
          ((= code 600) (wcf:set-ammo))
          ;; 从弹药箱装填子弹
          ((= code 601) (wcf:load-from-ammo))
          ;; 入库到弹药箱文件
          ((= code 602) (wcf:save-to-ammo))
          ;; 总拾取: 按顺序拾取多个文字对象, 去重后整体覆盖填入全部文本框
          ((= code 603) (wcf:pick-all-text))
          ;; 点击了某个"选取赋值"按钮: 对话框已关闭, 可以自由拾取图元(支持多选)
          ((and (>= code 100) (< code 200))
           (setq idx (+ offset (1- (- code 100))))
           (setq str (nth idx *wcf-vals*))
           (wcf:assign-to-selected str)
          )
          ;; 点击了某个"拾取"按钮: 引导用户选择文字对象, 清理后填入对应文本框
          ((and (>= code 200) (< code 300))
           (setq idx (+ offset (1- (- code 200))))
           (setq picked-text (wcf:pick-text))
           (if picked-text
             (setq *wcf-vals* (wcf:list-set *wcf-vals* idx picked-text))
           )
          )
        )
        ;; 保存当前所在页, 供下次启动直接进入
        (wcf:save-page *wcf-page*)
      )
    )
  )
  (wcf:save-values *wcf-vals*)
  (wcf:save-page *wcf-page*)
  (setvar "DYNMODE" dynold)
  (princ "\nWCF 工具已关闭, 内容与页码已保存, 下次启动将自动回显并进入原页。")
  (princ)
)

(princ "\nWCF 命令已加载, 输入 WCF 启动文本批量替换工具。")
(princ)
