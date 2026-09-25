;;; ============================================================
;;; 属性块数据管理器 ABM (Attribute Block Manager)
;;; 启动命令: TAG
;;; 兼容: AutoCAD 2016 / Windows 11 64位
;;; 单文件: ABM.lsp（DCL 已内嵌，无需单独 DCL 文件）
;;; 功能: 批量读取/编辑/回写 CAD 属性块数据，支持 Excel 互导
;;; 说明: 不依赖第三方插件，直接加载 LSP 运行
;;; ============================================================

(vl-load-com)

;; ============================================================
;; 全局变量
;; ============================================================
(setq *ABM-TAGS*      nil)   ; TAG 名称列表（按块定义顺序）
(setq *ABM-ROWS*      nil)   ; 数据行: ((handle (val1 val2 ...)) ...)
(setq *ABM-BLOCKS*    nil)   ; 选中块实体列表
(setq *ABM-TAE-HANDLES* nil) ; TAE 选块时记住的 handle 列表（供 TAR 无表格路径回写用）
(setq *ABM-FROM-BUTTON* nil) ; 从TAG按钮调用TAE的标志（T=按钮调用，用已有选块；nil=命令行调用，引导选块）
(setq *ABM-EXCEL-APP* nil)   ; 当前 Excel 应用对象（用于错误清理）
(setq abm:old-error   nil)   ; 原 *error* 函数

;; ============================================================
;; 错误处理：防止 Excel 进程残留
;; ============================================================
(defun abm:error (msg)
  ;; 出错时强制释放 Excel
  (if *ABM-EXCEL-APP*
    (vl-catch-all-apply
      '(lambda ()
         (if (boundp 'msxl-Quit)
           (msxl-Quit *ABM-EXCEL-APP*)
           (vlax-invoke-method *ABM-EXCEL-APP* 'Quit))
         (vlax-release-object *ABM-EXCEL-APP*))))
  (setq *ABM-EXCEL-APP* nil)
  ;; 恢复原 *error*
  (if abm:old-error (setq *error* abm:old-error))
  ;; 提示错误（忽略用户取消）
  (if (and msg
           (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*,*函数已取消*")))
    (princ (strcat "\nABM 错误: " msg)))
  (princ))

(defun abm:install-error ()
  (setq abm:old-error *error*)
  (setq *error* abm:error))

(defun abm:uninstall-error ()
  (if abm:old-error
    (setq *error* abm:old-error))
  (setq abm:old-error nil))

;; ============================================================
;; 辅助函数
;; ============================================================

;; 选择集转实体列表
(defun abm:ss->list (ss / i lst)
  (setq i 0 lst nil)
  (while (< i (sslength ss))
    (setq lst (cons (ssname ss i) lst)
          i   (1+ i)))
  (reverse lst))

;; 按插入点 X 坐标升序排序（从左到右），用于稳定 handle 列顺序
;; INSERT 实体 DXF group 10 = 插入点 (X Y Z)，取 car 得 X
(defun abm:sort-blocks-by-x (blocks)
  (vl-sort blocks
    '(lambda (a b)
       (< (car (cdr (assoc 10 (entget a))))
          (car (cdr (assoc 10 (entget b))))))))

;; 生成重复 n 次的字符串（替代不存在的 vl-string-fill）
(defun abm:repeat-str (str n / result)
  (setq result "")
  (while (> n 0)
    (setq result (strcat result str)
          n      (1- n)))
  result)

;; 计算字符串显示宽度（中文占 2，英文占 1）
(defun abm:str-width (str / w i n ch)
  (if (null str) (setq str ""))
  (setq str (vl-princ-to-string str))
  (setq w 0 i 1 n (strlen str))
  (while (<= i n)
    (setq ch (ascii (substr str i 1)))
    (if (> ch 127)
      (setq w (+ w 2))
      (setq w (+ w 1)))
    (setq i (1+ i)))
  w)

;; 右侧补空格到指定显示宽度
(defun abm:pad-right (str width / pad)
  (if (null str) (setq str ""))
  (setq str (vl-princ-to-string str))
  (setq pad (- width (abm:str-width str)))
  (if (> pad 0)
    (strcat str (abm:repeat-str " " pad))
    str))

;; 文件是否被占用（以追加方式测试）
(defun abm:file-locked? (path / f)
  (setq f (open path "a"))
  (if f
    (progn (close f) nil)
    T))

;; 数字转字符串（整数不带小数点）
(defun abm:num-to-str (n)
  (cond
    ((= (type n) 'INT) (itoa n))
    ((= (type n) 'REAL)
     (if (equal n (fix n) 1e-9)
       (itoa (fix n))
       (rtos n 2 6)))
    (t (vl-princ-to-string n))))

;; ============================================================
;; 属性块处理
;; ============================================================

;; 获取块定义中 ATTDEF 的 TAG 列表（按定义顺序）
;; 参数: ent - INSERT 实体名
;; 返回: ("TAG1" "TAG2" ...)
(defun abm:get-attdef-tags (ent / blk-name blk ent2 tags ed)
  (setq blk-name (cdr (assoc 2 (entget ent))))
  (setq blk (tblobjname "BLOCK" blk-name))
  (setq tags nil)
  (if blk
    (progn
      (setq ent2 blk)
      (while (setq ent2 (entnext ent2))
        (setq ed (entget ent2))
        (if (= (cdr (assoc 0 ed)) "ATTDEF")
          (setq tags (append tags (list (cdr (assoc 2 ed)))))))))
  tags)

;; 获取块定义中 ATTDEF 的 TAG -> Prompt 关联表（按定义顺序）
;; Prompt 存在于 ATTDEF 的 DXF group 3（ATTRIB 中没有）
;; 参数: ent - INSERT 实体名
;; 返回: (("TAG1" . "prompt1") ("TAG2" . "prompt2") ...)
(defun abm:get-attdef-prompts (ent / blk-name blk ent2 ed tag prompt result)
  (setq blk-name (cdr (assoc 2 (entget ent))))
  (setq blk (tblobjname "BLOCK" blk-name))
  (setq result nil)
  (if blk
    (progn
      (setq ent2 blk)
      (while (setq ent2 (entnext ent2))
        (setq ed (entget ent2))
        (if (= (cdr (assoc 0 ed)) "ATTDEF")
          (progn
            (setq tag     (cdr (assoc 2 ed))
                  prompt  (cdr (assoc 3 ed)))
            (setq result
                   (cons (cons tag (if prompt prompt ""))
                         result)))))))
  (reverse result))

;; 获取 INSERT 的所有 ATTRIB，返回 (tag . value) 关联表
(defun abm:get-attrib-alist (ent / attr ed result)
  (setq attr (entnext ent) result nil)
  (while (and attr
              (= (cdr (assoc 0 (setq ed (entget attr)))) "ATTRIB"))
    (setq result (cons (cons (cdr (assoc 2 ed)) (cdr (assoc 1 ed))) result))
    (setq attr (entnext attr)))
  (reverse result))

;; 获取 INSERT 引用中 ATTRIB 的 TAG 列表（按 ATTRIB 实体顺序）
;; 此顺序与 AutoCAD 原生"增强属性编辑器/块属性管理器"显示顺序一致
;; 参数: ent - INSERT 实体名
;; 返回: ("TAG1" "TAG2" ...)
(defun abm:get-attrib-tags (ent / attr ed tags)
  (setq attr (entnext ent) tags nil)
  (while (and attr
              (= (cdr (assoc 0 (setq ed (entget attr)))) "ATTRIB"))
    (setq tags (append tags (list (cdr (assoc 2 ed)))))
    (setq attr (entnext attr)))
  tags)

;; 检查所有块属性结构是否一致，返回共同 TAG 列表或 nil
;; TAG 顺序取自 INSERT 的 ATTRIB 实体顺序（与属性编辑界面一致）
(defun abm:get-common-tags (blocks / first-order ok order)
  (if (null blocks)
    nil
    (progn
      (setq first-order (abm:get-attrib-tags (car blocks))
            ok T)
      (foreach blk (cdr blocks)
        (setq order (abm:get-attrib-tags blk))
        (if (not (equal order first-order))
          (setq ok nil)))
      (if ok first-order nil))))

;; 按 TAG 顺序读取一个块的属性值
(defun abm:read-row (ent tags / alist result)
  (setq alist (abm:get-attrib-alist ent)
        result nil)
  (foreach tag tags
    (setq result (append result (list (cdr (assoc tag alist))))))
  result)

;; 读取所有块数据
(defun abm:read-all-data (blocks tags / rows handle vals)
  (setq rows nil)
  (foreach ent blocks
    (setq handle (cdr (assoc 5 (entget ent)))
          vals   (abm:read-row ent tags))
    (setq rows (append rows (list (list handle vals)))))
  rows)

;; 设置一个块的属性值（按 TAG 匹配）
(defun abm:set-attr-values (ent tags vals / attr ed tag pos val)
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

;; ============================================================
;; Excel COM 操作
;; ============================================================

;; 获取 Excel 导出路径: DWG目录\DWG名_属性数据.xlsx
(defun abm:get-excel-path (/ prefix name)
  (setq prefix (getvar "DWGPREFIX"))
  (setq name (vl-filename-base (getvar "DWGNAME")))
  (strcat prefix name "_属性数据.xlsx"))

;; ----------------------------------------------------------
;; DCL 内容（内嵌，无需外部 DCL 文件）
;; 运行时写入临时文件供 load_dialog 加载，用完即删
;; ----------------------------------------------------------

;; 字符串列表按分隔符拼接
(defun abm:join-strings (lst sep / result)
  (setq result "" sep (if sep sep ""))
  (while lst
    (setq result (strcat result (car lst))
          lst    (cdr lst))
    (if lst (setq result (strcat result sep))))
  result)

;; 返回 DCL 内容字符串
(defun abm:get-dcl-content (/ lines)
  (setq lines
    (list
      "// 属性块数据管理器 DCL（内嵌生成）"
      "abm_dialog : dialog {"
      "    label = \"属性块数据管理器\";"
      "    spacer;"
      ""
      "    : text {"
      "        key = \"info\";"
      "        label = \"\";"
      "        alignment = centered;"
      "        width = 100;"
      "    }"
      ""
      "    : boxed_column {"
      "        label = \"属性数据\";"
      "        : list_box {"
      "            key = \"data_list\";"
      "            label = \"Handle | 属性1 | 属性2 | ...\";"
      "            multiple_select = false;"
      "            height = 24;"
      "            width = 110;"
      "            fixed_width_font = true;"
      "        }"
      "    }"
      ""
      "    : row {"
      "        : button {"
      "            key = \"open_excel\";"
      "            label = \"打开Excel\";"
      "            width = 16;"
      "            fixed_width = true;"
      "        }"
      "        : button {"
      "            key = \"read_excel\";"
      "            label = \"读取Excel\";"
      "            width = 16;"
      "            fixed_width = true;"
      "        }"
      "        : button {"
      "            key = \"refresh\";"
      "            label = \"刷新数据\";"
      "            width = 16;"
      "            fixed_width = true;"
      "        }"
      "        : button {"
      "            key = \"write_back\";"
      "            label = \"写回CAD\";"
      "            width = 16;"
      "            fixed_width = true;"
      "        }"
      "        : button {"
      "            key = \"draw_table\";"
      "            label = \"绘制表格\";"
      "            width = 16;"
      "            fixed_width = true;"
      "        }"
      "        : spacer {"
      "            width = 2;"
      "        }"
      "        : button {"
      "            key = \"cancel\";"
      "            label = \"关闭\";"
      "            is_cancel = true;"
      "            width = 12;"
      "            fixed_width = true;"
      "        }"
      "    }"
      "    spacer;"
      "}"))
  (abm:join-strings lines "\n"))

;; 将 DCL 内容写入临时文件，返回文件路径；失败返回 nil
(defun abm:write-temp-dcl (/ path f content)
  (setq path (vl-filename-mktemp "ABM.dcl"))
  (setq f (open path "w"))
  (if f
    (progn
      (setq content (abm:get-dcl-content))
      (write-line content f)
      (close f)
      path)
    nil))

;; 解包 variant（某些 AutoCAD 版本下 COM 返回值会被 variant 包装）
;; 直接对象原样返回；variant 取其 value
;; 注意：导入 Excel 类型库后，强类型调用一般不再返回 variant，但保留此函数作为保险
(defun abm:unwrap (x)
  (if (= (type x) 'variant)
    (vlax-variant-value x)
    x))

;; ----------------------------------------------------------
;; Excel 类型库导入（核心修复）
;; 移植自 gg.lsp 的 vlxls-app-init，精简为单一中文版本
;; 作用：让 AutoCAD 知道 Excel 所有方法/属性/常量的签名
;;       解决 "实参太少"、"no function definition"、variant 包装等问题
;; 用 msxlc-xl24HourClock 常量是否存在判断是否已导入，避免重复导入
;; 成功返回 T，失败返回 nil
;; ----------------------------------------------------------
(defun abm:xl-init (/ ggg tlb osvar keys)
  (if (boundp 'msxlc-xl24HourClock)
    T
    (progn
      (setq keys (list
        "HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\Excel.EXE"
        "HKEY_LOCAL_MACHINE\\SOFTWARE\\WOW6432Node\\Microsoft\\Windows\\CurrentVersion\\App Paths\\Excel.EXE"))
      (setq ggg nil)
      (while (and keys (null ggg))
        (setq ggg (vl-registry-read (car keys) "Path"))
        (if ggg
          (setq ggg (strcat ggg "Excel.EXE"))
          (setq keys (cdr keys))))
      (if ggg
        (progn
          (setq ggg (strcase ggg))
          (foreach osvar '("SYSTEMROOT" "WINDIR" "WINBOOTDIR" "SYSTEMDRIVE"
                           "USERNAME" "COMPUTERNAME" "HOMEDRIVE" "HOMEPATH"
                           "PROGRAMFILES")
            (if (vl-string-search (strcat "%" osvar "%") ggg)
              (setq ggg (vl-string-subst (strcase (getenv osvar))
                                         (strcat "%" osvar "%") ggg))))
          (setq tlb ggg)
          (if (findfile tlb)
            (progn
              (princ "\n正在导入 Excel 类型库...")
              (vlax-import-type-library
                :tlb-filename      tlb
                :methods-prefix    "msxl-"
                :properties-prefix "msxlp-"
                :constants-prefix  "msxlc-")
              (if (boundp 'msxlc-xl24HourClock)
                (progn (princ " [成功]") T)
                (progn (princ " [失败]") nil)))
            (progn
              (princ (strcat "\n警告: 未找到 Excel 可执行文件: " tlb))
              nil)))
        (progn
          (princ "\n警告: 注册表中未检测到 Excel 安装路径，跳过类型库导入。")
          nil)))))

;; 写 Excel 单元格
(defun abm:xl-set-cell (ws row col val / cells cell)
  (if (null val) (setq val ""))
  (setq val (cond
              ((= (type val) 'INT) (itoa val))
              ((= (type val) 'REAL) (abm:num-to-str val))
              (t (vl-princ-to-string val))))
  (setq cells (abm:unwrap (vlax-get-property ws 'Cells))
        cell  (abm:unwrap (vlax-get-property cells 'Item row col)))
  (vl-catch-all-apply '(lambda () (vlax-put-property cell 'Value2 val)))
  (vlax-release-object cell)
  (vlax-release-object cells))

;; 读 Excel 单元格
;; 修复：类型库导入后 vlax-get-property 可能返回 variant，必须解包后再分类
(defun abm:xl-get-cell (ws row col / cells cell val)
  (setq cells (abm:unwrap (vlax-get-property ws 'Cells))
        cell  (abm:unwrap (vlax-get-property cells 'Item row col)))
  (setq val (vl-catch-all-apply '(lambda () (vlax-get-property cell 'Value2))))
  (vlax-release-object cell)
  (vlax-release-object cells)
  (if (vl-catch-all-error-p val)
    ""
    (progn
      ;; 先解包 variant（可能嵌套，循环解到非 variant）
      (while (= (type val) 'variant)
        (setq val (vlax-variant-value val)))
      (cond
        ((null val) "")
        ((= (type val) 'VLA-OBJECT) "")
        ((= (type val) 'REAL) (abm:num-to-str val))
        ((= (type val) 'INT) (itoa val))
        ((= (type val) 'STR) val)
        (t (vl-princ-to-string val))))))

;; ----------------------------------------------------------
;; Excel COM 对象创建
;; 使用 vlax-get-or-create-object：先获取已运行实例，无则新建
;; 创建前先调用 abm:xl-init 导入类型库（修复 SaveAs "实参太少" 等问题的核心）
;; 布尔属性用 :vlax-False / :vlax-true（导入类型库后的标准写法）
;; 成功返回 Excel.Application 对象，失败返回 nil
;; ----------------------------------------------------------
(defun abm:get-excel-app (/ obj err)
  (if (null (abm:xl-init))
    nil
    (progn
      (setq err (vl-catch-all-apply
                  '(lambda () (vlax-get-or-create-object "Excel.Application"))))
      (if (vl-catch-all-error-p err)
        nil
        err))))

;; 尝试保存工作簿，依次试 xlsx(51)、默认格式、xls(56)
;; 成功返回 (T . savedpath)；失败返回 (nil . errormsg)
;; 使用 msxl-SaveAs 强类型包装函数，避免 vlax-invoke-method 的 optional 参数问题
(defun abm:try-save-wb (wb filepath / fmts fmt result err savedpath errmsg)
  (if (not (boundp 'msxlc-xl24HourClock))
    (cons nil "Excel 类型库未正确导入，无法调用 SaveAs。请检查 Office 是否完整安装、AutoCAD/Excel 位数是否一致。")
    (progn
      (setq fmts (list (list 51 filepath)
                       (list nil filepath)
                       (list 56 (strcat (vl-filename-directory filepath) "\\"
                                        (vl-filename-base filepath) ".xls"))))
      (setq result nil savedpath filepath err nil errmsg nil)
      (while (and fmts (null result))
        (setq fmt (car fmts)
              fmts (cdr fmts)
              savedpath (cadr fmt))
        ;; msxl-SaveAs 是 vlax-import-type-library 生成的强类型包装函数，
        ;; 对类型库中无 default value 标记的可选参数必须显式传 :vlax-missing 占位，
        ;; 不能像晚绑定那样省略尾部可选参数（否则报"实参太少"）。
        ;; Workbook.SaveAs 完整签名: Filename FileFormat Password WriteResPassword
        ;;   ReadOnlyRecommended CreateBackup AccessMode ConflictResolution
        ;;   AddToMru TextCodepage TextVisualLayout Local (共12个参数)
        (setq result (vl-catch-all-apply
                       '(lambda ()
                          (msxl-SaveAs wb
                                       (cadr fmt)                            ; Filename (required)
                                       (if (car fmt) (car fmt) :vlax-missing) ; FileFormat
                                       :vlax-missing :vlax-missing :vlax-missing
                                       :vlax-missing :vlax-missing :vlax-missing
                                       :vlax-missing :vlax-missing :vlax-missing))))
        (if (vl-catch-all-error-p result)
          (progn
            (setq err result)
            (setq errmsg (vl-catch-all-error-message err))
            (princ (strcat "\n  [SaveAs诊断] 格式=" (if (car fmt) (itoa (car fmt)) "默认")
                           " 路径=" savedpath " -> 失败: " errmsg))
            (setq result nil))
          (progn
            (princ (strcat "\n  [SaveAs诊断] 格式=" (if (car fmt) (itoa (car fmt)) "默认")
                           " 路径=" savedpath " -> "
                           (if (findfile savedpath) "成功" "无异常但文件未生成")))
            (if (findfile savedpath)
              (setq result T)
              (setq result nil)))))
      (if result
        (cons T savedpath)
        (cons nil (if errmsg errmsg "SaveAs 调用失败，但未返回错误信息"))))))


;; 导出数据到 Excel 文件
;; 采用横向展开结构（与"绘制表格"功能一致）：
;;   表头行: 标记(TAG) | 提示(Prompt) | Handle1 | Handle2 | ...
;;   数据行: 每行 = TAG Prompt val1 val2 ... valn（每个TAG占一行，每个块占一值列）
;; 提示(Prompt)来自块定义 ATTDEF 的 DXF group 3，值来自属性引用 ATTRIB
;; 导出后 Excel 可见并保持打开，交由用户编辑
(defun abm:export-excel
       (filepath / xlApp wb ws row col save-result
                  first-ent prompt-alist handles n m
                  data-rows i j tag prompt-val vals r)
  (princ "\n正在导出 Excel...")
  ;; ---- 构造横向展开数据（与绘制表格功能一致）----
  (setq first-ent (handent (car (car *ABM-ROWS*))))
  (setq prompt-alist (if first-ent (abm:get-attdef-prompts first-ent) nil))
  (setq handles (mapcar 'car *ABM-ROWS*)
        n       (length *ABM-ROWS*)
        m       (length *ABM-TAGS*))
  ;; data-rows: 每行 = (TAG Prompt val1 val2 ... valn)
  (setq data-rows nil i 0)
  (repeat m
    (setq tag        (nth i *ABM-TAGS*)
          prompt-val (cdr (assoc tag prompt-alist)))
    (setq vals nil)
    (foreach r *ABM-ROWS*
      (setq vals (append vals (list (nth i (cadr r))))))
    (setq data-rows
           (append data-rows
                   (list (cons tag
                               (cons (if prompt-val prompt-val "")
                                     (mapcar '(lambda (v) (if v v ""))
                                             vals))))))
    (setq i (1+ i)))
  ;; ---- Excel COM 导出 ----
  (setq xlApp (abm:get-excel-app))
  (if (null xlApp)
    (progn
      (alert "无法启动 Excel！\n\n可能原因:\n1. 未安装 Microsoft Excel\n2. AutoCAD 与 Excel 位数不一致\n3. Excel COM 组件未注册")
      nil)
    (progn
      (setq *ABM-EXCEL-APP* xlApp)
      (vl-catch-all-apply '(lambda ()
        (vlax-put-property xlApp 'Visible :vlax-False)
        (vlax-put-property xlApp 'ScreenUpdating :vlax-False)
        (vlax-put-property xlApp 'DisplayAlerts :vlax-False)))
      (setq wb (vl-catch-all-apply
                 '(lambda ()
                    (msxl-Add
                      (abm:unwrap (vlax-get-property xlApp 'Workbooks))))))
      (if (vl-catch-all-error-p wb)
        (progn
          (vl-catch-all-apply '(lambda ()
            (msxl-Quit xlApp)
            (vlax-release-object xlApp)))
          (setq *ABM-EXCEL-APP* nil)
          (alert
            (strcat "Excel 启动失败：无法创建工作簿。\n\n"
                    "具体错误: " (vl-catch-all-error-message wb) "\n\n"
                    "常见原因：\n"
                    "1. AutoCAD 与 Excel 位数不一致（如 32位 CAD + 64位 Excel）\n"
                    "2. Excel 加载项/COM 加载项冲突\n"
                    "3. Excel 未完成首次激活或配置\n"
                    "4. Excel 与 AutoCAD 运行权限不同\n"
                    "5. Microsoft Store 版 Office 限制 COM 自动化\n\n"
                    "建议排查：\n"
                    "1. 手动打开 Excel，确认能新建空白工作簿\n"
                    "2. 任务管理器\"详细信息\"页确认无 EXCEL.EXE 残留\n"
                    "3. Excel 中禁用所有 COM 加载项\n"
                    "4. 以管理员身份运行 AutoCAD 再试\n"
                    "5. 检查 Office 是否为 Microsoft Store 版本"))
          nil)
        (progn
          (setq ws (abm:unwrap (vlax-get-property wb 'ActiveSheet)))
          ;; 表头行（第1行）：标记(TAG) | 提示(Prompt) | Handle1 | Handle2 | ...
          (abm:xl-set-cell ws 1 1 "标记(TAG)")
          (abm:xl-set-cell ws 1 2 "提示(Prompt)")
          (setq col 3 j 0)
          (repeat n
            (abm:xl-set-cell ws 1 col (nth j handles))
            (setq col (1+ col) j (1+ j)))
          ;; 数据行（第2行起）：TAG Prompt val1 val2 ... valn
          (setq row 2)
          (foreach r data-rows
            (abm:xl-set-cell ws row 1 (nth 0 r))   ; 标记(TAG)
            (abm:xl-set-cell ws row 2 (nth 1 r))   ; 提示(Prompt)
            (setq col 3 j 0)
            (repeat n
              (abm:xl-set-cell ws row col
                               (if (nth (+ 2 j) r)
                                 (nth (+ 2 j) r)
                                 ""))
              (setq col (1+ col) j (1+ j)))
            (setq row (1+ row)))
          ;; 列宽自适应
          (vl-catch-all-apply
            '(lambda ()
               (msxl-AutoFit
                 (abm:unwrap
                   (vlax-get-property
                     (abm:unwrap (vlax-get-property ws 'UsedRange)) 'EntireColumn)))))
          ;; 删除已存在文件（若被占用则忽略，后续 SaveAs 会回弹覆盖）
          (if (findfile filepath)
            (vl-catch-all-apply '(lambda () (vl-file-delete filepath))))
          ;; 尝试多种格式保存
          (setq save-result (abm:try-save-wb wb filepath))
          (if (null (car save-result))
            (progn
              ;; 全部失败：清理并弹框
              (vl-catch-all-apply '(lambda ()
                (msxl-Close wb :vlax-False :vlax-missing :vlax-missing)
                (vlax-release-object ws)
                (vlax-release-object wb)
                (msxl-Quit xlApp)
                (vlax-release-object xlApp)))
              (setq *ABM-EXCEL-APP* nil)
              (alert (strcat "保存 Excel 文件失败:\n" filepath
                             "\n\n具体错误: " (cdr save-result)
                             "\n\n常见原因:\n"
                             "1. 文件被占用（同名文件已打开）\n"
                             "2. 目录不存在或不可写\n"
                             "3. 当前 Office 不支持 xlsx 格式（已尝试 xls 回退）\n"
                             "4. AutoCAD 与 Excel 运行权限不同"))
              nil)
            (progn
              (setq filepath (cdr save-result))
              (vlax-put-property xlApp 'ScreenUpdating :vlax-true)
              (vlax-put-property xlApp 'Visible :vlax-true)
              (vlax-put-property xlApp 'DisplayAlerts :vlax-true)
              (vlax-release-object ws)
              (vlax-release-object wb)
              (vlax-release-object xlApp)
              (setq *ABM-EXCEL-APP* nil)
              (princ (strcat "\n已导出: " filepath))
              T)))))))

;; 从 Excel 文件读取数据
;; 读取横向展开结构（与导出/绘制表格一致）：
;;   表头行: 标记(TAG) | 提示(Prompt) | Handle1 | Handle2 | ...
;;   数据行: TAG Prompt val1 val2 ... valn
;; 建立 tag -> (h1_val h2_val ...) 映射后，按 *ABM-TAGS* 顺序重组每个 handle 的值
;; 返回: ((handle v1 v2 ...) ...)（平铺，与原接口一致），失败返回 nil
(defun abm:import-excel
       (filepath / xlApp wb ws usedrange
                  rows cols rowcount colcount
                  handles tag-map tag vals r c val handle result)
  (princ "\n正在读取 Excel...")
  (setq xlApp (abm:get-excel-app))
  (if (null xlApp)
    (progn
      (alert "无法启动 Excel！\n\n可能原因:\n1. 未安装 Microsoft Excel\n2. AutoCAD 与 Excel 位数不一致\n3. Excel COM 组件未注册")
      nil)
    (progn
      (setq *ABM-EXCEL-APP* xlApp)
      (vl-catch-all-apply '(lambda ()
        (vlax-put-property xlApp 'Visible :vlax-False)
        (vlax-put-property xlApp 'DisplayAlerts :vlax-False)))
      (setq wb (vl-catch-all-apply
                 '(lambda ()
                    ;; Workbooks.Open 完整签名15个参数:
                    ;;   Filename UpdateLinks ReadOnly Format Password WriteResPassword
                    ;;   IgnoreReadOnlyRecommended Origin Delimiter Editable Notify
                    ;;   Converter AddToMru Local CorruptLoad
                    ;; 强类型包装函数同样要求补齐 :vlax-missing
                    (msxl-Open
                      (abm:unwrap (vlax-get-property xlApp 'Workbooks))
                      filepath        ; Filename (required)
                      0               ; UpdateLinks
                      :vlax-true      ; ReadOnly
                      :vlax-missing :vlax-missing :vlax-missing :vlax-missing
                      :vlax-missing :vlax-missing :vlax-missing :vlax-missing
                      :vlax-missing :vlax-missing :vlax-missing))))
      (if (vl-catch-all-error-p wb)
        (progn
          (vl-catch-all-apply '(lambda ()
            (msxl-Quit xlApp)
            (vlax-release-object xlApp)))
          (setq *ABM-EXCEL-APP* nil)
          (alert (strcat "无法打开 Excel 文件:\n" filepath
                         "\n\n可能原因: 文件被占用或格式错误。"))
          nil)
        (progn
          (setq ws (abm:unwrap (vlax-get-property wb 'ActiveSheet)))
          (setq usedrange (abm:unwrap (vlax-get-property ws 'UsedRange))
                rows      (abm:unwrap (vlax-get-property usedrange 'Rows))
                cols      (abm:unwrap (vlax-get-property usedrange 'Columns))
                rowcount  (vlax-get-property rows 'Count)
                colcount  (vlax-get-property cols 'Count))
          ;; 读取表头行（第1行）：第3列起为 handles
          (setq handles nil c 3)
          (while (<= c colcount)
            (setq handles (append handles (list (abm:xl-get-cell ws 1 c))))
            (setq c (1+ c)))
          ;; 读取数据行（第2行起），建立 tag -> (h1_val h2_val ...) 映射
          (setq tag-map nil r 2)
          (while (<= r rowcount)
            (setq tag (abm:xl-get-cell ws r 1))
            (if (and tag (/= tag ""))
              (progn
                (setq vals nil c 3)
                (while (<= c colcount)
                  (setq vals (append vals (list (abm:xl-get-cell ws r c))))
                  (setq c (1+ c)))
                (setq tag-map (cons (cons tag vals) tag-map))))
            (setq r (1+ r)))
          (msxl-Close wb :vlax-False :vlax-missing :vlax-missing)
          (vlax-release-object usedrange)
          (vlax-release-object ws)
          (vlax-release-object wb)
          (msxl-Quit xlApp)
          (vlax-release-object xlApp)
          (setq *ABM-EXCEL-APP* nil)
          ;; 重建 ((handle v1 v2 ...) ...)，值按 *ABM-TAGS* 顺序对齐
          ;; 对每个 handle 列，依次取 *ABM-TAGS* 中各 TAG 在该列的值
          (setq result nil c 0)
          (foreach handle handles
            (setq vals nil)
            (foreach tag *ABM-TAGS*
              (setq val (nth c (cdr (assoc tag tag-map))))
              (setq vals (append vals (list (if val val "")))))
            (setq result (append result (cons handle vals)))
            (setq c (1+ c)))
          (princ "\nExcel 读取完成。")
          result)))))

;; 用 Excel 数据更新内存中的行数据
(defun abm:update-rows-from-excel (excel-data / excel-map new-rows handle vals)
  ;; 建立 handle -> vals 映射
  (setq excel-map nil)
  (foreach row excel-data
    (if (>= (length row) 1)
      (setq excel-map (cons (cons (car row) (cdr row)) excel-map))))
  ;; 按原 *ABM-ROWS* 顺序更新
  (setq new-rows nil)
  (foreach r *ABM-ROWS*
    (setq handle (car r))
    (if (setq vals (cdr (assoc handle excel-map)))
      ;; 用 Excel 数据替换，但保证列数与 TAG 数一致
      (setq new-rows (append new-rows
                             (list (list handle (abm:fit-vals vals (length *ABM-TAGS*))))))
      (setq new-rows (append new-rows (list r)))))
  (setq *ABM-ROWS* new-rows))

;; 调整值列表长度到 n（多则截断，少则补空）
(defun abm:fit-vals (vals n / result i)
  (setq result nil i 0)
  (while (< i n)
    (setq result (append result (list (nth i vals))))
    (setq i (1+ i)))
  result)

;; ============================================================
;; DCL 界面
;; ============================================================

;; 计算各列显示宽度
(defun abm:calc-col-widths (/ all-rows transposed)
  (setq all-rows
         (cons (cons "Handle" *ABM-TAGS*)
               (mapcar '(lambda (r) (cons (car r) (cadr r))) *ABM-ROWS*)))
  (setq transposed (apply 'mapcar (cons 'list all-rows)))
  (mapcar '(lambda (col)
             (+ 2 (apply 'max
                         (mapcar 'abm:str-width
                                 (mapcar '(lambda (x) (if x x "")) col)))))
          transposed))

;; 格式化一行（按列宽补齐）
(defun abm:format-row (row widths / line i)
  (setq line "" i 0)
  (foreach v row
    (setq line (strcat line (abm:pad-right (if v v "") (nth i widths)) " "))
    (setq i (1+ i)))
  line)

;; 刷新 DCL 列表显示
(defun abm:fill-list (/ widths header-line lines)
  (setq widths (abm:calc-col-widths))
  (setq header-line (abm:format-row (cons "Handle" *ABM-TAGS*) widths))
  (setq lines (list header-line))
  (foreach r *ABM-ROWS*
    (setq lines (append lines
                        (list (abm:format-row (cons (car r) (cadr r)) widths)))))
  (start_list "data_list")
  (foreach line lines (add_list line))
  (end_list)
  ;; 设置对话框标题显示块数量
  (if (and *ABM-TAGS* *ABM-ROWS*)
    (set_tile "info"
              (strcat "共 " (itoa (length *ABM-ROWS*)) " 个属性块  |  "
                      (itoa (length *ABM-TAGS*)) " 个属性字段"))))

;; 显示 DCL 对话框（DCL 内容内嵌，运行时写入临时文件）
;; 采用循环结构：绘制表格按钮需要关闭对话框去 CAD 选点，绘制完重新打开
(defun abm:show-dialog (/ dclfile dclid dlg-result keep-going)
  (setq dclfile (abm:write-temp-dcl)
        dclid   0)
  (cond
    ((not dclfile)
     (princ "\n无法创建临时 DCL 文件。"))
    ((<= (setq dclid (load_dialog dclfile)) 0)
     (princ "\n无法加载 DCL（内嵌定义可能已损坏）。")
     (vl-file-delete dclfile))
    (T
     (setq keep-going T)
     (while keep-going
       (cond
         ((not (new_dialog "abm_dialog" dclid))
          (princ "\n无法创建对话框，请检查 DCL 定义。")
          (setq keep-going nil))
         (T
          ;; 填充数据
          (abm:fill-list)
          ;; 按钮回调
          ;; 打开Excel/写回CAD 改为关闭对话框，外层调用 TAE/TAR 命令
          ;; （这两个命令需要 CAD 交互，对话框打开时无法执行）
          (action_tile "open_excel"  "(done_dialog 5)")
          (action_tile "read_excel"  "(abm:cmd-read-excel)")
          (action_tile "refresh"     "(abm:cmd-refresh)")
          (action_tile "write_back"  "(done_dialog 6)")
          ;; 绘制表格：关闭对话框返回状态码 4，外层循环据此调用绘制函数
          (action_tile "draw_table"  "(done_dialog 4)")
          (action_tile "cancel"      "(done_dialog 0)")
          (setq dlg-result (start_dialog))
          (cond
            ((= dlg-result 4)
             ;; 对话框已关闭，可在 CAD 中交互选点并绘制表格
             ;; 绘制完成后直接结束，不返回主对话框
             (abm:cmd-draw-table)
             (setq keep-going nil))
            ((= dlg-result 5)
             ;; 打开Excel：从按钮调用 TAE，设标志让其用 TAG 已选的块（不引导选块）
             (setq *ABM-FROM-BUTTON* T)
             (c:TAE)
             (setq keep-going nil))
            ((= dlg-result 6)
             ;; 写回CAD：执行 TAR 命令（有表格更新表格+回写，无表格直接回写）
             (c:TAR)
             (setq keep-going nil))
            (T
             (setq keep-going nil))))))
     (unload_dialog dclid)
     ;; 删除临时 DCL 文件
     (vl-file-delete dclfile)))
  (princ))

;; ============================================================
;; 按钮命令
;; ============================================================

;; 打开Excel：导出 + 用 Excel 打开显示
(defun abm:cmd-open-excel (/ path ok)
  (setq path (abm:get-excel-path))
  ;; 检查文件是否被占用
  (if (and (findfile path) (abm:file-locked? path))
    (alert "Excel 文件被占用！\n请先关闭已打开的 Excel 文件，再点击\"打开Excel\"。")
    (progn
      (setq ok (abm:export-excel path))
      (if ok
        (progn
          (princ (strcat "\n已导出并打开: " path))
          (alert
            (strcat "已导出到:\n" path
                    "\n\n请在 Excel 中编辑数据，"
                    "\n编辑完成后保存并关闭 Excel，"
                    "\n然后点击\"读取Excel\"。")))))))

;; 读取Excel：读取文件数据更新内存并刷新显示
(defun abm:cmd-read-excel (/ path data)
  (setq path (abm:get-excel-path))
  (cond
    ((not (findfile path))
     (alert (strcat "Excel 文件不存在:\n" path
                    "\n\n请先点击\"打开Excel\"导出数据。")))
    ((abm:file-locked? path)
     (alert "Excel 文件被占用！\n请先保存并关闭 Excel，再点击\"读取Excel\"。"))
    (T
     (setq data (abm:import-excel path))
     (if data
       (progn
         (abm:update-rows-from-excel data)
         (abm:fill-list)
         (princ "\n已读取 Excel 数据并更新列表。"))
       (princ "\nExcel 读取失败或无数据。")))))

;; 刷新数据：从 CAD 重新读取属性
(defun abm:cmd-refresh ()
  (if *ABM-BLOCKS*
    (progn
      (setq *ABM-ROWS* (abm:read-all-data *ABM-BLOCKS* *ABM-TAGS*))
      (abm:fill-list)
      (princ "\n已从 CAD 重新读取属性数据。"))
    (princ "\n没有可刷新的数据。")))

;; 写回CAD：把内存数据写回属性块
(defun abm:cmd-write-back (/ ent handle updated missing)
  (setq updated 0 missing 0)
  (foreach r *ABM-ROWS*
    (setq handle (car r))
    (setq ent (handent handle))
    (if ent
      (progn
        (abm:set-attr-values ent *ABM-TAGS* (cadr r))
        (setq updated (1+ updated)))
      (setq missing (1+ missing))))
  ;; 刷新视图（重生成以显示更新后的属性，acAllViewports=1）
  (vl-catch-all-apply
    '(lambda ()
       (vla-Regen (vlax-get-property (vlax-get-acad-object) 'ActiveDocument) 1)))
  (princ
    (strcat "\n已更新 " (itoa updated) " 个属性块。"
            (if (> missing 0)
              (strcat " " (itoa missing) " 个 Handle 未找到对应块（可能已删除）。")
              "")))
  (alert
    (strcat "写回完成！\n\n更新: " (itoa updated) " 个属性块"
            (if (> missing 0)
              (strcat "\n未找到: " (itoa missing) " 个")
              ""))))

;; ============================================================
;; 绘制表格到 CAD
;; ============================================================

;; 获取当前活动空间（模型/图纸）的 Block 对象
;; 用 TILEMODE 系统变量判断最可靠：1=模型空间，0=图纸空间
(defun abm:get-current-space (/ doc)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (if (= (getvar "TILEMODE") 1)
    (vla-get-ModelSpace doc)
    (vla-get-PaperSpace doc)))

;; 绘制表格：将 *ABM-ROWS* 和 *ABM-TAGS* 数据以表格形式绘制到 CAD 中
;; 表格结构（横向展开）：标记(TAG) | 提示(Prompt) | Handle1 | Handle2 | ...
;; 标记和提示列共用（所有块 TAG 顺序一致），每个块占一值列，列名用 Handle 区分
;; 行数 = 2(标题+表头) + TAG数；列数 = 2(标记+提示) + 块数
;; 标记/提示来自块定义的 ATTDEF（提示用 DXF group 3），值来自属性引用 ATTRIB
(defun abm:cmd-draw-table
       (/ pt textH unit-w rowH ncols nrows tbl col i j
          first-ent prompt-alist prompt-val
          handles n m data-rows row tag vals
          w-tag w-prompt handle-widths col-vals w
          col-widths doc space)
  (if (or (null *ABM-TAGS*) (null *ABM-ROWS*))
    (progn
      (princ "\n没有可绘制的数据。")
      (alert "没有可绘制的数据。\n\n请先选择属性块并读取数据。"))
    (progn
      ;; 提示选择插入点（对话框已关闭，可在 CAD 中交互）
      (princ "\n请在绘图区域选择表格插入点...")
      (setq pt (getpoint "\n表格插入点: "))
      (if (null pt)
        (princ "\n已取消绘制表格。")
        (progn
          ;; 从第一个块读取 TAG -> Prompt 关联表（用 handent 反查实体名）
          (setq first-ent (handent (car (car *ABM-ROWS*))))
          (setq prompt-alist
                 (if first-ent
                   (abm:get-attdef-prompts first-ent)
                   nil))
          ;; handles 列表、块数 n、属性数 m
          (setq handles (mapcar 'car *ABM-ROWS*)
                n       (length *ABM-ROWS*)
                m       (length *ABM-TAGS*))
          ;; 构造数据行：每行 = (TAG Prompt val1 val2 ... valn)
          ;; 第 i 行对应第 i 个 TAG，valj = 第 j 个块的第 i 个属性值
          (setq data-rows nil
                i 0)
          (repeat m
            (setq tag        (nth i *ABM-TAGS*)
                  prompt-val (cdr (assoc tag prompt-alist)))
            ;; 收集各块的第 i 个值（按 *ABM-ROWS* 顺序）
            (setq vals nil)
            (foreach r *ABM-ROWS*
              (setq vals (append vals (list (nth i (cadr r))))))
            (setq data-rows
                   (append data-rows
                           (list (cons tag
                                       (cons (if prompt-val prompt-val "")
                                             (mapcar '(lambda (v) (if v v ""))
                                                     vals))))))
            (setq i (1+ i)))
          ;; 文字高度：取当前 TEXTSIZE，过小则回退到 3.5
          (setq textH (getvar "TEXTSIZE"))
          (if (or (null textH) (<= textH 0))
            (setq textH 3.5))
          ;; 单位字符宽度（abm:str-width 中英文各 2/1 → 实际宽度）
          (setq unit-w (/ textH 2.0)
                rowH   (* textH 1.6))
          ;; 计算各列最大显示宽度（含表头，每列加 2 字符余量）
          ;; 标记列：表头 + 所有 TAG
          (setq w-tag (apply 'max
                             (cons (abm:str-width "标记(TAG)")
                                   (mapcar 'abm:str-width *ABM-TAGS*))))
          ;; 提示列：表头 + 所有 Prompt（data-rows 每行第 1 个元素）
          (setq w-prompt (apply 'max
                                (cons (abm:str-width "提示(Prompt)")
                                      (mapcar 'abm:str-width
                                              (mapcar 'cadr data-rows)))))
          ;; 每个 Handle 值列：表头(handle) + 该列所有值
          (setq handle-widths nil
                j 0)
          (repeat n
            (setq col-vals (mapcar '(lambda (row) (nth (+ 2 j) row))
                                    data-rows))
            (setq w (apply 'max
                           (cons (abm:str-width (nth j handles))
                                 (mapcar '(lambda (v)
                                            (abm:str-width (if v v "")))
                                         col-vals))))
            (setq handle-widths (append handle-widths (list w)))
            (setq j (1+ j)))
          ;; 列宽列表（字符宽度 → 实际宽度，每列加 2 字符余量）
          (setq col-widths
                 (cons (* (+ w-tag 2) unit-w)
                       (cons (* (+ w-prompt 2) unit-w)
                             (mapcar '(lambda (w) (* (+ w 2) unit-w))
                                     handle-widths))))
          (setq ncols (+ 2 n)
                nrows (+ 2 m))
          ;; 获取当前空间
          (setq doc   (vla-get-ActiveDocument (vlax-get-acad-object))
                space (abm:get-current-space))
          ;; 创建表格
          (setq tbl
                 (vla-AddTable space
                               (vlax-3d-point pt)
                               nrows ncols rowH
                               (apply 'max col-widths)))
          ;; 标题行
          (vl-catch-all-apply
            '(lambda () (vla-SetText tbl 0 0 "属性块数据表")))
          ;; 表头行：标记(TAG) | 提示(Prompt) | Handle1 | Handle2 | ...
          (vl-catch-all-apply
            '(lambda ()
               (vla-SetText tbl 1 0 "标记(TAG)")
               (vla-SetText tbl 1 1 "提示(Prompt)")
               (setq j 0)
               (repeat n
                 (vla-SetText tbl 1 (+ 2 j) (nth j handles))
                 (setq j (1+ j)))))
          ;; 数据行：每行 = TAG Prompt val1 val2 ... valn
          (setq i 2)
          (foreach row data-rows
            (vl-catch-all-apply
              '(lambda ()
                 (vla-SetText tbl i 0 (nth 0 row))   ; 标记
                 (vla-SetText tbl i 1 (nth 1 row))   ; 提示
                 (setq j 0)
                 (repeat n
                   (vla-SetText tbl i (+ 2 j)
                                (if (nth (+ 2 j) row)
                                  (nth (+ 2 j) row)
                                  ""))
                   (setq j (1+ j)))))
            (setq i (1+ i)))
          ;; 逐列设置列宽
          (setq col 0)
          (foreach w col-widths
            (vl-catch-all-apply
              '(lambda () (vla-SetColumnWidth tbl col w)))
            (setq col (1+ col)))
          ;; 设置各行类型的文字高度（acTitleRow=1, acHeaderRow=2, acDataRow=4）
          ;; 失败不影响主流程，表格会用默认样式
          (vl-catch-all-apply
            '(lambda ()
               (vla-SetTextHeight tbl 1 (* textH 1.2))
               (vla-SetTextHeight tbl 2 textH)
               (vla-SetTextHeight tbl 4 textH)))
          (princ
            (strcat "\n已绘制表格: " (itoa m) " 个属性 × "
                    (itoa n) " 个块，共 " (itoa nrows) " 行 × "
                    (itoa ncols) " 列。")))))))

;; ============================================================
;; 主命令 TAG
;; ============================================================
(defun c:TAG (/ ss blocks tags old-cmdecho)
  (abm:install-error)
  (setq old-cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (princ "\n=== 属性块数据管理器 ===")
  (princ "\n请选择属性块（可框选）...")
  ;; 选择带属性的 INSERT 对象
  (setq ss (ssget '((0 . "INSERT") (66 . 1))))
  (if (or (null ss) (= (sslength ss) 0))
    (progn
      (princ "\n未选择任何属性块。")
      (setvar "CMDECHO" old-cmdecho)
      (abm:uninstall-error))
    (progn
      (setq blocks (abm:ss->list ss))
      ;; 按插入点 X 坐标排序（从左到右），保证表格/Excel 列顺序与图纸一致
      (setq blocks (abm:sort-blocks-by-x blocks))
      ;; 检查属性结构一致性
      (setq tags (abm:get-common-tags blocks))
      (if (null tags)
        (progn
          (princ "\n检测到属性结构不一致，请选择相同属性结构的属性块。")
          (alert "检测到属性结构不一致！\n\n请选择具有相同属性数量、TAG名称和顺序的属性块。")
          (setvar "CMDECHO" old-cmdecho)
          (abm:uninstall-error))
        (progn
          ;; 读取数据
          (setq *ABM-TAGS* tags
                *ABM-ROWS* (abm:read-all-data blocks tags)
                *ABM-BLOCKS* blocks)
          (princ (strcat "\n已读取 " (itoa (length blocks)) " 个属性块，"
                         (itoa (length tags)) " 个属性字段。"))
          ;; 显示对话框
          (abm:show-dialog)
          (setvar "CMDECHO" old-cmdecho)
          (abm:uninstall-error)))))
  (princ))

;; ============================================================
;; 从表格回写属性块（TAA 命令）
;; 用户在 CAD 中编辑表格单元格的值后，输入 TAA 自动把修改回写到对应属性块
;; 表格由"绘制表格"生成，结构：行0=标题，行1=表头(标记|提示|Handle1|...)，
;;   行2..=数据(TAG|Prompt|val1|val2|...)
;; 回写时按表头列名(Handle)定位块，按数据行第0列(TAG)定位属性
;; ============================================================

;; 查找图纸中标题为"属性块数据表"的表格
;; 返回: vla-object 列表（可能为空）
(defun abm:find-data-tables (/ ss i tbl title result)
  (setq result nil)
  (setq ss (ssget "X" '((0 . "ACAD_TABLE"))))
  (if (and ss (> (sslength ss) 0))
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq tbl (vlax-ename->vla-object (ssname ss i)))
        (setq title
               (vl-catch-all-apply '(lambda () (vla-GetText tbl 0 0))))
        (if (and (not (vl-catch-all-error-p title))
                 (= title "属性块数据表"))
          (setq result (cons tbl result)))
        (setq i (1+ i)))))
  (reverse result))

;; 读取表格单元格文本（容错：失败返回空串）
(defun abm:cell-text (tbl row col / v)
  (setq v (vl-catch-all-apply '(lambda () (vla-GetText tbl row col))))
  (if (vl-catch-all-error-p v) "" (if v v "")))

;; 从指定表格读取数据并回写到对应属性块
(defun abm:update-from-table (tbl / nrows ncols n m handles tags
                                 c r handle ent vals updated missing)
  (setq nrows (vlax-get-property tbl 'Rows)
        ncols (vlax-get-property tbl 'Columns))
  (setq n (- ncols 2)   ; 块数 = 列数 - 2(标记+提示)
        m (- nrows 2))  ; 属性数 = 行数 - 2(标题+表头)
  (if (or (< n 1) (< m 1))
    (progn
      (princ "\n表格结构异常，无法回写。")
      (alert "表格结构异常：列数或行数不足。\n请确认这是由 TAG 命令\"绘制表格\"生成的数据表。"))
    (progn
      ;; 读取 handles（行1，列2..n+1）
      (setq handles nil c 2)
      (repeat n
        (setq handles (append handles (list (abm:cell-text tbl 1 c))))
        (setq c (1+ c)))
      ;; 读取 TAGs（列0，行2..m+1）
      (setq tags nil r 2)
      (repeat m
        (setq tags (append tags (list (abm:cell-text tbl r 0))))
        (setq r (1+ r)))
      ;; 对每个 handle 列，收集该列所有值并回写
      (setq updated 0 missing 0 c 0)
      (foreach handle handles
        (setq ent (handent handle))
        (if ent
          (progn
            (setq vals nil r 2)
            (repeat m
              (setq vals (append vals (list (abm:cell-text tbl r (+ 2 c)))))
              (setq r (1+ r)))
            (abm:set-attr-values ent tags vals)
            (setq updated (1+ updated)))
          (setq missing (1+ missing)))
        (setq c (1+ c)))
      ;; 刷新视图（acAllViewports=1）
      (vl-catch-all-apply
        '(lambda ()
           (vla-Regen
             (vlax-get-property (vlax-get-acad-object) 'ActiveDocument) 1)))
      (princ
        (strcat "\n已从表格回写 " (itoa updated) " 个属性块"
                "（每个 " (itoa m) " 个属性）。"
                (if (> missing 0)
                  (strcat " " (itoa missing) " 个 Handle 未找到对应块（可能已删除）。")
                  ""))))))

(defun c:TAA (/ old-cmdecho tables choice)
  (abm:install-error)
  (setq old-cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (princ "\n=== 从表格回写属性块 ===")
  (setq tables (abm:find-data-tables))
  (cond
    ((null tables)
     (princ "\n未找到\"属性块数据表\"。")
     (alert "未找到数据表。\n\n请先用 TAG 命令选择属性块并点击\"绘制表格\"生成数据表，\n在表中修改值后再运行 TAA 回写。"))
    ((= (length tables) 1)
     (abm:update-from-table (car tables)))
    (T
     (princ (strcat "\n找到 " (itoa (length tables)) " 个数据表，请选择一个。"))
     (setq choice (entsel "\n请选择要回写的表格: "))
     (if choice
       (abm:update-from-table (vlax-ename->vla-object (car choice)))
       (princ "\n已取消。"))))
  (setvar "CMDECHO" old-cmdecho)
  (abm:uninstall-error)
  (princ))

;; ============================================================
;; TAE 命令：自动找表格 → 导出到 Excel 打开编辑
;; 复用 abm:export-excel（横向展开结构）
;; ============================================================
(defun c:TAE (/ old-cmdecho tables choice tbl
               nrows ncols n m handles tags
               c r handle vals rows tmp-file result
               old-tags old-rows temp-dir
               ss blocks from-button)
  (abm:install-error)
  (setq old-cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (princ "\n=== 表格导出到 Excel 编辑 ===")
  ;; 捕获并重置"从按钮调用"标志（防止残留影响下次命令行调用）
  (setq from-button *ABM-FROM-BUTTON*)
  (setq *ABM-FROM-BUTTON* nil)
  (setq tables (abm:find-data-tables))
  (cond
    ((null tables)
     ;; 无表格：按钮调用且已有选块→直接用；否则引导选块
     (if (and from-button *ABM-BLOCKS* (> (length *ABM-BLOCKS*) 0))
       ;; 从TAG按钮调用：直接用已有的 *ABM-BLOCKS*/*ABM-TAGS*/*ABM-ROWS* 导出，不引导选块
       (progn
         (setq blocks *ABM-BLOCKS*
               tags *ABM-TAGS*)
         (setq *ABM-TAE-HANDLES* (mapcar 'car *ABM-ROWS*))
         (princ (strcat "\n使用 TAG 已选的 " (itoa (length blocks)) " 个属性块直接导出。"))
         (setq temp-dir (getenv "TEMP"))
         (if (or (null temp-dir) (= temp-dir ""))
           (setq temp-dir "."))
         (setq tmp-file (strcat temp-dir "\\abm_table_edit.xlsx"))
         (princ (strcat "\n正在导出到 Excel: " tmp-file))
         (setq result (abm:export-excel tmp-file))
         (if result
           (princ "\n已在 Excel 中打开，编辑后保存即可。")
           (princ "\n导出失败，请检查 Excel 环境或命令行错误信息。")))
       ;; 命令行调用或无选块：引导选块（原逻辑）
       (progn
         (princ "\n未找到\"属性块数据表\"，将直接选择属性块导出到 Excel。")
         (princ "\n请选择属性块（可框选）...")
         (setq ss (ssget '((0 . "INSERT") (66 . 1))))
         (if (or (null ss) (= (sslength ss) 0))
           (progn
             (princ "\n未选择任何属性块，导出取消。")
             (alert "未选择任何属性块，导出取消。"))
           (progn
            (setq blocks (abm:ss->list ss))
            (setq blocks (abm:sort-blocks-by-x blocks))
            (setq tags (abm:get-common-tags blocks))
             (if (null tags)
               (progn
                 (princ "\n检测到属性结构不一致，请选择相同属性结构的属性块。")
                 (alert "检测到属性结构不一致！\n\n请选择具有相同属性数量、TAG名称和顺序的属性块。"))
               (progn
                 (setq *ABM-TAGS* tags
                       *ABM-ROWS* (abm:read-all-data blocks tags)
                       *ABM-BLOCKS* blocks)
                 (setq *ABM-TAE-HANDLES* (mapcar 'car *ABM-ROWS*))
                 (princ (strcat "\n已读取 " (itoa (length blocks)) " 个属性块，"
                                (itoa (length tags)) " 个属性字段。"))
                 (princ (strcat "\n已记住这 " (itoa (length *ABM-TAE-HANDLES*)) " 个属性块，TAR 将直接更新它们。"))
                 (setq temp-dir (getenv "TEMP"))
                 (if (or (null temp-dir) (= temp-dir ""))
                   (setq temp-dir "."))
                 (setq tmp-file (strcat temp-dir "\\abm_table_edit.xlsx"))
                 (princ (strcat "\n正在导出到 Excel: " tmp-file))
                 (setq result (abm:export-excel tmp-file))
                 (if result
                   (princ "\n已在 Excel 中打开，编辑后保存即可。")
                   (princ "\n导出失败，请检查 Excel 环境或命令行错误信息。")))))))))
    (T
     ;; 多个表格时让用户选
     (if (= (length tables) 1)
       (setq tbl (car tables))
       (progn
         (princ (strcat "\n找到 " (itoa (length tables)) " 个数据表，请选择一个。"))
         (setq choice (entsel "\n请选择要导出到 Excel 的表格: "))
         (if choice
           (setq tbl (vlax-ename->vla-object (car choice)))
           (setq tbl nil))))
     (if tbl
       (progn
         (setq nrows (vlax-get-property tbl 'Rows)
               ncols (vlax-get-property tbl 'Columns)
               n     (- ncols 2)
               m     (- nrows 2))
         (if (or (< n 1) (< m 1))
           (progn
             (princ "\n表格结构异常，无法导出。")
             (alert "表格结构异常：列数或行数不足。\n请确认这是由 TAG 命令\"绘制表格\"生成的数据表。"))
           (progn
             ;; 读 handles（行1，列2..n+1）
             (setq handles nil c 2)
             (repeat n
               (setq handles (append handles (list (abm:cell-text tbl 1 c))))
               (setq c (1+ c)))
             ;; 读 TAGs（列0，行2..m+1）
             (setq tags nil r 2)
             (repeat m
               (setq tags (append tags (list (abm:cell-text tbl r 0))))
               (setq r (1+ r)))
             ;; 构造 *ABM-ROWS* = ((handle (v1 v2 ...)) ...)
             ;; 对每个 handle 列，按 TAG 行顺序收集值
             (setq rows nil c 0)
             (foreach handle handles
               (setq vals nil r 2)
               (repeat m
                 (setq vals (append vals (list (abm:cell-text tbl r (+ 2 c)))))
                 (setq r (1+ r)))
               (setq rows (append rows (list (list handle vals))))
               (setq c (1+ c)))
            ;; 临时保存原全局变量，调用 export 后恢复
            (setq old-tags *ABM-TAGS*
                  old-rows *ABM-ROWS*
                  *ABM-TAGS* tags
                  *ABM-ROWS* rows)
            ;; 记住本次表格导出对应的 handle 列表，供 TAR 有表格路径回写用
            (setq *ABM-TAE-HANDLES* (mapcar 'car rows))
            (princ (strcat "\n已记住表格中 " (itoa (length *ABM-TAE-HANDLES*)) " 个属性块 handle，TAR 将直接用它们回写。"))
             ;; 临时文件路径（系统 TEMP 目录）
             (setq temp-dir (getenv "TEMP"))
             (if (or (null temp-dir) (= temp-dir ""))
               (setq temp-dir "."))
             (setq tmp-file (strcat temp-dir "\\abm_table_edit.xlsx"))
             (princ (strcat "\n正在导出到 Excel: " tmp-file))
             (setq result (abm:export-excel tmp-file))
             ;; 恢复原全局变量
             (setq *ABM-TAGS* old-tags
                   *ABM-ROWS* old-rows)
             (if result
               (princ "\n已在 Excel 中打开，编辑后保存即可。")
               (princ "\n导出失败，请检查 Excel 环境或命令行错误信息。"))))))))
  (setvar "CMDECHO" old-cmdecho)
  (abm:uninstall-error)
  (princ))

;; ============================================================
;; TAR 命令：从 Excel 文件更新 CAD 中的属性块数据表
;; 工作流: TAE导出Excel → Excel编辑保存 → TAR更新CAD表格 → TAA回写属性块
;; ============================================================

;; 从 Excel 文件读取数据，更新指定 CAD 表格的单元格内容
;; Excel 结构(横向展开): 行1=表头(标记|提示|Handle1|...), 行2+=数据(TAG|Prompt|val1|...)
;; CAD 表格结构: 行0=标题, 行1=表头, 行2+=数据
;; 映射: Excel行r → CAD行r(行0标题不动), Excel列c → CAD列(c-1)
;; 成功返回更新的单元格数，失败返回 nil
;; 从 Excel 读取数据，应用到 CAD：更新表格(若有) + 回写属性块
;; tbl 为 nil 时只回写属性块；tbl 非 nil 时同时更新 CAD 表格单元格
;; mem-handles 非 nil 时用它作为回写目标 handle 列表（TAE 选块时记住的），
;;   否则从 Excel 表头第3列起读 handle（兼容旧逻辑）
;; 返回 (updated-cells updated-blocks missing-blocks attr-count)，失败返回 nil
(defun abm:apply-excel-data (filepath tbl mem-handles / xlApp wb ws usedrange
                                       xlrows xlcols r c val
                                       tbl-nrows tbl-ncols updated-cells
                                       handles tags handle ent vals
                                       updated-blocks missing-blocks m col-idx
                                       xl-handle-count)
  (setq xlApp (abm:get-excel-app))
  (if (null xlApp)
    (progn
      (alert "无法启动 Excel！")
      nil)
    (progn
      (vl-catch-all-apply '(lambda ()
        (vlax-put-property xlApp 'Visible :vlax-False)
        (vlax-put-property xlApp 'DisplayAlerts :vlax-False)))
      (setq wb (vl-catch-all-apply
                 '(lambda ()
                    (msxl-Open
                      (abm:unwrap (vlax-get-property xlApp 'Workbooks))
                      filepath 0 :vlax-true
                      :vlax-missing :vlax-missing :vlax-missing :vlax-missing
                      :vlax-missing :vlax-missing :vlax-missing :vlax-missing
                      :vlax-missing :vlax-missing :vlax-missing))))
      (if (vl-catch-all-error-p wb)
        (progn
          (vl-catch-all-apply '(lambda () (msxl-Quit xlApp) (vlax-release-object xlApp)))
          (alert (strcat "无法打开 Excel 文件:\n" filepath
                         "\n\n可能原因: 文件被占用或不存在。"))
          nil)
        (progn
          (setq ws (abm:unwrap (vlax-get-property wb 'ActiveSheet)))
          (setq usedrange (abm:unwrap (vlax-get-property ws 'UsedRange)))
          (setq xlrows (vlax-get-property
                         (abm:unwrap (vlax-get-property usedrange 'Rows)) 'Count)
                xlcols (vlax-get-property
                         (abm:unwrap (vlax-get-property usedrange 'Columns)) 'Count))
          ;; ---- 步骤1: 若有 CAD 表格，更新表格单元格 ----
          (setq updated-cells 0)
          (if tbl
            (progn
              (setq tbl-nrows (vlax-get-property tbl 'Rows)
                    tbl-ncols (vlax-get-property tbl 'Columns))
              ;; 从 r=2 开始，跳过表头行(行1含handle，不应被Excel覆盖)
              ;; 行0=标题(不动) 行1=表头(不动) 行2+=数据(从Excel更新)
              (setq r 2)
              (while (<= r xlrows)
                (if (< r tbl-nrows)
                  (progn
                    (setq c 1)
                    (while (<= c xlcols)
                      (if (< (- c 1) tbl-ncols)
                        (progn
                          (setq val (abm:xl-get-cell ws r c))
                          (vl-catch-all-apply
                            '(lambda () (vla-SetText tbl r (- c 1) val)))
                          (setq updated-cells (1+ updated-cells))))
                      (setq c (1+ c)))))
                (setq r (1+ r)))))
          ;; ---- 步骤2: 回写属性块 ----
          ;; [诊断] 确认步骤2被执行
          (princ "\n[诊断] 步骤2: 回写属性块 开始")
          ;; handles 优先用 mem-handles（TAE 选块时记住的），否则从 Excel 表头读
          ;; Excel 表头: 列1=标记 列2=提示 列3+=Handle
          (if (and mem-handles (> (length mem-handles) 0))
            (progn
              (setq handles mem-handles)
              (princ (strcat "\n使用 TAE 记住的 " (itoa (length handles)) " 个属性块 handle 回写。")))
            (progn
              (setq handles nil c 3)
              (while (<= c xlcols)
                (setq handles (append handles (list (abm:xl-get-cell ws 1 c))))
                (setq c (1+ c)))
              (princ (strcat "\n从 Excel 表头读取 " (itoa (length handles)) " 个 handle 回写。"))))
          ;; [诊断] 显示 handles 内容
          (princ (strcat "\n[诊断] handles=" (vl-prin1-to-string handles)))
          ;; Excel 中实际 handle 值列数（用于回写时边界保护）
          (setq xl-handle-count (- xlcols 2))
          ;; tags = Excel 行2..xlrows 列1
          (setq tags nil r 2)
          (while (<= r xlrows)
            (setq tags (append tags (list (abm:xl-get-cell ws r 1))))
            (setq r (1+ r)))
          (setq m (length tags))
          ;; [诊断] 显示 tags 内容
          (princ (strcat "\n[诊断] tags=" (vl-prin1-to-string tags) " m=" (itoa m)))
          (setq updated-blocks 0 missing-blocks 0 col-idx 0)
          (foreach handle handles
            (setq ent (handent handle))
            ;; [诊断] 显示每个 handle 的 handent 结果
            (princ (strcat "\n[诊断] handle=\"" handle "\" handent=" (if ent "成功" "nil")))
            (if ent
              (progn
                (setq vals nil r 2)
                (repeat m
                  ;; 边界保护：col-idx 超出 Excel 值列时用空串
                  (if (< col-idx xl-handle-count)
                    (setq vals (append vals (list (abm:xl-get-cell ws r (+ 3 col-idx)))))
                    (setq vals (append vals (list ""))))
                  (setq r (1+ r)))
                ;; [诊断] 显示 vals
                (princ (strcat " vals=" (vl-prin1-to-string vals)))
                (abm:set-attr-values ent tags vals)
                (setq updated-blocks (1+ updated-blocks)))
              (setq missing-blocks (1+ missing-blocks)))
            (setq col-idx (1+ col-idx)))
          ;; [诊断] 最终统计
          (princ (strcat "\n[诊断] 步骤2完成: updated-blocks=" (itoa updated-blocks)
                         " missing-blocks=" (itoa missing-blocks)))
          ;; ---- 清理 ----
          (vl-catch-all-apply '(lambda ()
            (msxl-Close wb :vlax-False :vlax-missing :vlax-missing)))
          (vlax-release-object usedrange)
          (vlax-release-object ws)
          (vlax-release-object wb)
          (vl-catch-all-apply '(lambda () (msxl-Quit xlApp)))
          (vlax-release-object xlApp)
          (vla-Regen (vlax-get-property (vlax-get-acad-object) 'ActiveDocument) 1)
          (list updated-cells updated-blocks missing-blocks m))))))

(defun c:TAR (/ old-cmdecho tables tbl choice filepath result)
  (setq old-cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (abm:install-error)
  (princ "\n=== 从 Excel 更新（表格 + 属性块）===")
  (setq tables (abm:find-data-tables))
  (cond
    ((null tables)
     ;; 无表格：用 TAE 记住的 handles 直接从 Excel 回写属性块
     (princ "\n未找到\"属性块数据表\"，将直接从 Excel 回写属性块。")
     (if (or (null *ABM-TAE-HANDLES*) (= (length *ABM-TAE-HANDLES*) 0))
       (progn
         (princ "\n尚未记住任何属性块（请先执行 TAE 选择属性块导出）。")
         (alert "未找到数据表，且没有记住的属性块。\n\n请先执行 TAE 选择属性块导出到 Excel，编辑后再执行 TAR 回写。"))
       (progn
         (setq filepath (strcat (getenv "TEMP") "\\abm_table_edit.xlsx"))
         (if (null (findfile filepath))
           (setq filepath (getfiled "选择要读取的 Excel 文件" "" "xlsx;xls" 8)))
         (if (null filepath)
           (princ "\n未选择文件，已取消。")
           (progn
             (princ (strcat "\n正在从 Excel 读取: " filepath))
             (setq result (abm:apply-excel-data filepath nil *ABM-TAE-HANDLES*))
             (if result
               (princ (strcat "\n已回写 " (itoa (cadr result)) " 个属性块"
                              "（每个 " (itoa (nth 3 result)) " 个属性）。"
                              (if (> (caddr result) 0)
                                (strcat " " (itoa (caddr result)) " 个 Handle 未找到对应块。")
                                "")))
               (princ "\n更新失败，请检查命令行错误信息。")))))))
    (T
     ;; 有表格：同时更新表格 + 回写属性块（一步到位，无需再跑 TAA）
     (if (= (length tables) 1)
       (setq tbl (car tables))
       (progn
         (princ (strcat "\n找到 " (itoa (length tables)) " 个数据表，请选择一个。"))
         (setq choice (entsel "\n请选择要更新的表格: "))
         (if choice
           (setq tbl (vlax-ename->vla-object (car choice)))
           (setq tbl nil))))
     (if tbl
       (progn
         (setq filepath (strcat (getenv "TEMP") "\\abm_table_edit.xlsx"))
         (if (null (findfile filepath))
           (setq filepath (getfiled "选择要读取的 Excel 文件" "" "xlsx;xls" 8)))
         (if (null filepath)
           (princ "\n未选择文件，已取消。")
           (progn
             (princ (strcat "\n正在从 Excel 读取: " filepath))
             (setq result (abm:apply-excel-data filepath tbl *ABM-TAE-HANDLES*))
             (if result
               (princ (strcat "\n已更新表格 " (itoa (car result)) " 个单元格，"
                              "并回写 " (itoa (cadr result)) " 个属性块"
                              "（每个 " (itoa (nth 3 result)) " 个属性）。"
                              (if (> (caddr result) 0)
                                (strcat " " (itoa (caddr result)) " 个 Handle 未找到对应块。")
                                "")))
               (princ "\n更新失败，请检查命令行错误信息。")))))))
  (setvar "CMDECHO" old-cmdecho)
  (abm:uninstall-error)
  (princ)))

;; ============================================================
;; TZR 命令: 属性值复制（源块 → 目标块，支持框选多个）
;; 按 TAG 名称对应赋值，不要求顺序完全一致
;; 借用 TZV 思路：直接传全部 TAG/VAL，set-attr-values 自动按 TAG 匹配
;; ============================================================
(defun c:TZR (/ old-cmdecho src-ent src-alist src-handle
              ss i tgt-ent tgt-tags n-matched
              success-count skip-count)
  (setq old-cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (princ "\n=== 属性值复制 TZR（源块 -> 目标块）===")

  ;; 1. 选源块
  (setq src-ent (entsel "\n请选择源属性块: "))
  (if (null src-ent)
    (princ "\n未选择源属性块，已取消。")
    (progn
      (setq src-ent (car src-ent))
      (setq src-alist (abm:get-attrib-alist src-ent))
      (if (null src-alist)
        (princ "\n源属性块没有属性数据。")
        (progn
          (princ (strcat "\n源块属性: " (abm:join-strings (mapcar 'car src-alist) ", ")))
          (setq src-handle (cdr (assoc 5 (entget src-ent))))

          ;; 2. 框选目标块（支持窗选/交叉窗选多个）
          (princ "\n请框选目标属性块: ")
          (setq ss (ssget '((0 . "INSERT") (66 . 1))))
          (if (null ss)
            (princ "\n未选择目标块。")
            (progn
              (setq success-count 0 skip-count 0 i 0)
              ;; 3. 遍历选择集
              (while (< i (sslength ss))
                (setq tgt-ent (ssname ss i)
                      i (1+ i))
                ;; 排除源块自身
                (if (/= src-handle (cdr (assoc 5 (entget tgt-ent))))
                  (progn
                    ;; 计算匹配数（仅用于提示）
                    (setq tgt-tags (abm:get-attrib-tags tgt-ent)
                          n-matched 0)
                    (foreach pair src-alist
                      (if (member (car pair) tgt-tags)
                        (setq n-matched (1+ n-matched))))
                    (if (= n-matched 0)
                      (progn
                        (princ "\n  无共同属性，跳过此块。")
                        (setq skip-count (1+ skip-count)))
                      (progn
                        ;; 直接传全部 TAG/VAL，set-attr-values 自动匹配
                        (abm:set-attr-values tgt-ent (mapcar 'car src-alist) (mapcar 'cdr src-alist))
                        (princ (strcat "\n  已复制 " (itoa n-matched) " 个属性值。"))
                        (setq success-count (1+ success-count)))))))
              ;; 4. 输出统计
              (princ (strcat "\n复制完成: 成功 " (itoa success-count) " 个"
                             (if (> skip-count 0)
                               (strcat "，跳过 " (itoa skip-count) " 个（无共同属性）")
                               "")))))))))
  (setvar "CMDECHO" old-cmdecho)
  (princ))

;; ============================================================
;; TZV 命令: 属性值互换（块A <-> 块B，双向交换）
;; 本质 = 两次 TZR：先读两块数据到内存，再交换写入
;; 无需计算交集——abm:set-attr-values 内部按 TAG 匹配，
;; 传入全部 TAG/VAL 即可，不存在的自动跳过
;; ============================================================
(defun c:TZV (/ old-cmdecho entA entB alistA alistB)
  (setq old-cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setq entA (entsel "\n请选择第一个属性块: "))
  (if (null entA)
    (princ "\n已取消。")
    (progn
      (setq entA (car entA))
      (setq alistA (abm:get-attrib-alist entA))
      (if (null alistA)
        (princ "\n第一个属性块没有属性数据。")
        (progn
          (setq entB (entsel "\n请选择第二个属性块: "))
          (if (null entB)
            (princ "\n已取消。")
            (progn
              (setq entB (car entB))
              (if (equal entA entB)
                (princ "\n同一个块，无需互换。")
                (progn
                  (setq alistB (abm:get-attrib-alist entB))
                  (if (null alistB)
                    (princ "\n第二个属性块没有属性数据。")
                    (progn
                      ;; 交换写入：A的全部值给B，B的全部值给A
                      ;; set-attr-values 遍历目标块ATTRIB，按TAG匹配，不存在的自动跳过
                      (abm:set-attr-values entB (mapcar 'car alistA) (mapcar 'cdr alistA))
                      (abm:set-attr-values entA (mapcar 'car alistB) (mapcar 'cdr alistB))
                      (princ "\nTZV 完成: 属性值已互换。")))))))))))
  (setvar "CMDECHO" old-cmdecho)
  (princ))

;; ============================================================
;; 自动加载提示
;; ============================================================
(princ "\n属性块数据管理器已加载。TAG 启动；绘制表格后可输入 TAE 导出到 Excel 编辑，TAR 从 Excel 更新表格/属性块，TAA 从表格回写属性块，TZR 复制属性值（源块->目标块），TZV 互换两个属性块的属性值。")
(princ)

