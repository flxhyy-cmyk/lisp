;;; 文本-计算和.lsp
;;; 命令: CS  - 选择文本求和，结果界面可做加减乘除/等差
;;; 命令: CSC - 直接进入等差流程（先选方向，再确认首数字与公差）
;;; 命令: CSS - 直接进入字母等差流程（先选方向，再确认首字母与公差）
;;; 命令: CSW - 在CSC基础上智能分列/分行等差：
;;;             选择"从上往下/从下往上"时，先按X坐标智能判断能否分成多列；
;;;             选择"从左往右/从右往左"时，先按Y坐标智能判断能否分成多行；
;;;             能分组时：组内再按所选方向重新排序，各组"排序后第一个"
;;;             实体的原始数字自动作为该组首数字(不再交互确认)；
;;;             公差在分组确定后整体只询问一次，所有组共用；
;;;             判断不出明显的列/行结构时，退化为常规CSC(整体一次等差,
;;;             仍交互确认首数字与公差)。
;;; 功能: 选择文本,提取数字并求和.
;;;       多数字文本使用对话框点击选择;
;;;       复选框仅对“数字个数相同”的后续文本自动应用相同序号(默认勾选);
;;;       结果界面按行显示参与计算的数字(每行5个);
;;;       结果界面提供加减乘除与等差按钮,可将运算结果写回原文本(保持原精度,成功不弹窗).
;;;       等差时按坐标方向排序,首项可交互修改(默认使用方向首个数字),公差默认1.
;;;       字母等差(CSS): 提取字母,按方向排序后写回字母等差序列(保持大小写,公差默认1,循环A-Z).

(vl-load-com)

;;------------------------------------------------------------
;; 从字符串中提取所有数字(支持整数与小数)
;;------------------------------------------------------------
(defun CS:ExtractNums (s / lst i n c buf)
  (setq lst '() i 1 n (strlen s) buf "")
  (while (<= i (1+ n))
    (setq c (if (<= i n) (substr s i 1) " "))
    (cond
      ((wcmatch c "[0-9]") (setq buf (strcat buf c)))
      ((and (= c ".") (/= buf "") (not (vl-string-search "." buf)))
       (setq buf (strcat buf c)))
      (T
       (if (and (/= buf "") (/= buf "."))
         (setq lst (append lst (list (atof buf))))
       )
       (setq buf "")
      )
    )
    (setq i (1+ i))
  )
  lst
)

;;------------------------------------------------------------
;; 从字符串中提取所有字母 (A-Z a-z 单字符)
;;------------------------------------------------------------
(defun CS:ExtractLetters (s / lst i n c)
  (setq lst '() i 1 n (strlen s))
  (while (<= i n)
    (setq c (substr s i 1))
    (if (wcmatch c "[A-Za-z]")
      (setq lst (append lst (list c)))
    )
    (setq i (1+ i))
  )
  lst
)

;;------------------------------------------------------------
;; 获取 / 设置 实体文本
;;------------------------------------------------------------
(defun CS:GetTextContent (en / obj)
  (setq obj (vlax-ename->vla-object en))
  (cond
    ((= (vla-get-ObjectName obj) "AcDbText")  (vla-get-TextString obj))
    ((= (vla-get-ObjectName obj) "AcDbMText") (vla-get-TextString obj))
    (T "")
  )
)

(defun CS:SetTextContent (en newstr / obj)
  (setq obj (vlax-ename->vla-object en))
  (cond
    ((= (vla-get-ObjectName obj) "AcDbText")  (vla-put-TextString obj newstr))
    ((= (vla-get-ObjectName obj) "AcDbMText") (vla-put-TextString obj newstr))
  )
)

;;------------------------------------------------------------
;; 获取实体插入点 (DXF 10，兼容 TEXT / MTEXT)
;;------------------------------------------------------------
(defun CS:GetInsPt (en / ed)
  (setq ed (entget en))
  (if ed (cdr (assoc 10 ed)))
)

;;------------------------------------------------------------
;; 定位原数字子串 & 按原格式写回
;;------------------------------------------------------------
;; 按"出现顺序序号"(occIdx，1-based)定位第几个数字子串，
;; 而不是按数值反查——避免同一文本内出现重复数值时
;; (如 "5 组 5 台") 永远命中最靠左那个、导致写错位置的问题。
(defun CS:FindNumStr (str occIdx / i n c buf start result cnt)
  (setq i 1 n (strlen str) buf "" start 0 result nil cnt 0)
  (while (and (<= i (1+ n)) (null result))
    (setq c (if (<= i n) (substr str i 1) " "))
    (cond
      ((wcmatch c "[0-9]")
       (if (= buf "") (setq start i))
       (setq buf (strcat buf c)))
      ((and (= c ".") (/= buf "") (not (vl-string-search "." buf)))
       (setq buf (strcat buf c)))
      (T
       (if (and (/= buf "") (/= buf "."))
         (progn
           (setq cnt (1+ cnt))
           (if (= cnt occIdx)
             (setq result (list start buf)))
         )
       )
       (setq buf ""))
    )
    (setq i (1+ i))
  )
  result
)

(defun CS:FormatLike (newnum oldstr / hasdot decs)
  (setq hasdot (vl-string-search "." oldstr))
  (if hasdot
    (progn (setq decs (- (strlen oldstr) hasdot 1)) (rtos newnum 2 decs))
    (if (equal newnum (fix newnum) 1e-8)
      (itoa (fix newnum))
      (rtos newnum 2 1)
    )
  )
)

(defun CS:ReplaceNumInStr (str occIdx newnum / found pos oldstr newstr)
  (setq found (CS:FindNumStr str occIdx))
  (if found
    (progn
      (setq pos (car found) oldstr (cadr found) newstr (CS:FormatLike newnum oldstr))
      (strcat (substr str 1 (1- pos)) newstr (substr str (+ pos (strlen oldstr))))
    )
    str
  )
)

;;------------------------------------------------------------
;; 定位原字母 & 写回
;;------------------------------------------------------------
(defun CS:FindLetterStr (str oldch / i n c result)
  (setq i 1 n (strlen str) result nil)
  (while (and (<= i n) (null result))
    (setq c (substr str i 1))
    (if (= c oldch)
      (setq result (list i c))
    )
    (setq i (1+ i))
  )
  result
)

(defun CS:ReplaceLetterInStr (str oldch newch / found pos)
  (setq found (CS:FindLetterStr str oldch))
  (if found
    (progn
      (setq pos (car found))
      (strcat (substr str 1 (1- pos)) newch (substr str (1+ pos)))
    )
    str
  )
)

;;------------------------------------------------------------
;; 字母偏移与转换 (0-25, 循环)
;;------------------------------------------------------------
(defun CS:IsUpper (ch)
  (and (>= (ascii ch) 65) (<= (ascii ch) 90))
)

(defun CS:LetterToOffset (ch / code)
  (setq code (ascii ch))
  (cond
    ((and (>= code 65) (<= code 90)) (- code 65))
    ((and (>= code 97) (<= code 122)) (- code 97))
    (T 0)
  )
)

(defun CS:OffsetToLetter (offset upper / off)
  (setq off (rem offset 26))
  (if (< off 0) (setq off (+ off 26)))
  (if upper
    (chr (+ 65 off))
    (chr (+ 97 off))
  )
)

;;------------------------------------------------------------
;; 选择数字对话框 (默认勾选应用到相同个数)
;;------------------------------------------------------------
(defun CS:AskOneDlg (txt nums / dclpath dcl_id key res idx applyflg showtxt i selidx)
  (setq res nil idx "0" applyflg "1"
        showtxt (if (> (strlen txt) 60) (strcat (substr txt 1 57) "...") txt)
        showtxt (vl-string-subst " " "\n" showtxt))
  (setq dclpath (vl-filename-mktemp "csone" nil ".dcl"))
  (if (setq dcl_id (open dclpath "w"))
    (progn
      (write-line "cs_one : dialog {" dcl_id)
      (write-line "  label = \"选择要纳入计算的数字\";" dcl_id)
      (write-line "  : text {" dcl_id)
      (write-line "    key = \"tip\";" dcl_id)
      (write-line (strcat "    label = \"文本: " showtxt "\";") dcl_id)
      (write-line "  }" dcl_id)
      (write-line "  : list_box {" dcl_id)
      (write-line "    key = \"numlist\"; label = \"请点击选择数字:\";" dcl_id)
      (write-line "    height = 8; width = 40; fixed_width_font = true;" dcl_id)
      (write-line "  }" dcl_id)
      (write-line "  : toggle {" dcl_id)
      (write-line "    key = \"applyrest\";" dcl_id)
      (write-line "    label = \"将此选择应用到后续相同数字个数的文本\";" dcl_id)
      (write-line "    value = \"1\";" dcl_id)
      (write-line "  }" dcl_id)
      (write-line "  : row { fixed_width = true; alignment = centered;" dcl_id)
      (write-line "    : button { key = \"accept\"; label = \"确定\"; is_default = true; width = 12; }" dcl_id)
      (write-line "    : button { key = \"cancel\"; label = \"取消\"; is_cancel = true; width = 12; }" dcl_id)
      (write-line "  }" dcl_id)
      (write-line "}" dcl_id)
      (close dcl_id)
      (if (and (>= (setq dcl_id (load_dialog dclpath)) 0) (new_dialog "cs_one" dcl_id))
        (progn
          (start_list "numlist")
          (setq i 0)
          (foreach x nums (setq i (1+ i)) (add_list (strcat (itoa i) ". " (rtos x 2 1))))
          (end_list)
          (set_tile "numlist" "0") (set_tile "applyrest" "1")
          (action_tile "numlist" "(setq idx $value)")
          (action_tile "applyrest" "(setq applyflg $value)")
          (action_tile "accept" "(done_dialog 1)")
          (action_tile "cancel" "(done_dialog 0)")
          (setq key (start_dialog))
          (unload_dialog dcl_id)
          (if (= key 1)
            (progn
              (if (not idx) (setq idx "0"))
              (setq selidx (1+ (atoi idx)))
              (setq res (list (nth (atoi idx) nums) (= applyflg "1") selidx))
            )
          )
        )
      )
      (vl-file-delete dclpath)
    )
  )
  res
)

;;------------------------------------------------------------
;; 选择字母对话框 (默认勾选应用到相同个数)
;;------------------------------------------------------------
(defun CS:AskOneLetterDlg (txt letters / dclpath dcl_id key res idx applyflg showtxt i selidx)
  (setq res nil idx "0" applyflg "1"
        showtxt (if (> (strlen txt) 60) (strcat (substr txt 1 57) "...") txt)
        showtxt (vl-string-subst " " "\n" showtxt))
  (setq dclpath (vl-filename-mktemp "csoneL" nil ".dcl"))
  (if (setq dcl_id (open dclpath "w"))
    (progn
      (write-line "cs_oneL : dialog {" dcl_id)
      (write-line "  label = \"选择要纳入计算的字母\";" dcl_id)
      (write-line "  : text {" dcl_id)
      (write-line "    key = \"tip\";" dcl_id)
      (write-line (strcat "    label = \"文本: " showtxt "\";") dcl_id)
      (write-line "  }" dcl_id)
      (write-line "  : list_box {" dcl_id)
      (write-line "    key = \"numlist\"; label = \"请点击选择字母:\";" dcl_id)
      (write-line "    height = 8; width = 40; fixed_width_font = true;" dcl_id)
      (write-line "  }" dcl_id)
      (write-line "  : toggle {" dcl_id)
      (write-line "    key = \"applyrest\";" dcl_id)
      (write-line "    label = \"将此选择应用到后续相同字母个数的文本\";" dcl_id)
      (write-line "    value = \"1\";" dcl_id)
      (write-line "  }" dcl_id)
      (write-line "  : row { fixed_width = true; alignment = centered;" dcl_id)
      (write-line "    : button { key = \"accept\"; label = \"确定\"; is_default = true; width = 12; }" dcl_id)
      (write-line "    : button { key = \"cancel\"; label = \"取消\"; is_cancel = true; width = 12; }" dcl_id)
      (write-line "  }" dcl_id)
      (write-line "}" dcl_id)
      (close dcl_id)
      (if (and (>= (setq dcl_id (load_dialog dclpath)) 0) (new_dialog "cs_oneL" dcl_id))
        (progn
          (start_list "numlist")
          (setq i 0)
          (foreach x letters (setq i (1+ i)) (add_list (strcat (itoa i) ". " x)))
          (end_list)
          (set_tile "numlist" "0") (set_tile "applyrest" "1")
          (action_tile "numlist" "(setq idx $value)")
          (action_tile "applyrest" "(setq applyflg $value)")
          (action_tile "accept" "(done_dialog 1)")
          (action_tile "cancel" "(done_dialog 0)")
          (setq key (start_dialog))
          (unload_dialog dcl_id)
          (if (= key 1)
            (progn
              (if (not idx) (setq idx "0"))
              (setq selidx (1+ (atoi idx)))
              (setq res (list (nth (atoi idx) letters) (= applyflg "1") selidx))
            )
          )
        )
      )
      (vl-file-delete dclpath)
    )
  )
  res
)

;;------------------------------------------------------------
;; 数字列表按每行5个拆成字符串列表
;;------------------------------------------------------------
(defun CS:NumListToRows (lst / rows line i x)
  (setq rows '() line "" i 0)
  (foreach x lst
    (setq i (1+ i))
    (if (= line "") (setq line (rtos x 2 1)) (setq line (strcat line "   " (rtos x 2 1))))
    (if (or (= (rem i 5) 0) (= i (length lst)))
      (setq rows (append rows (list line)) line "")
    )
  )
  rows
)

;;------------------------------------------------------------
;; 方向选择对话框 (十字架布局 + 方向符号 + 特殊复选框)
;; 中间行: 左=从左往右  右=从右往左
;; 返回: (list dir specialflg)  dir:1=从上往下 2=从左往右 3=从下往上 4=从右往左 0=取消
;;       specialflg: T=勾选特殊(第4项强制N/n)  nil=未勾选
;;------------------------------------------------------------
(defun CS:AskDirection ( / dclpath dcl_id key specialflg)
  (setq dclpath (vl-filename-mktemp "csdir" nil ".dcl") key 0 specialflg "1")
  (if (setq dcl_id (open dclpath "w"))
    (progn
      (write-line "cs_dir : dialog {" dcl_id)
      (write-line "  label = \"选择等差方向\";" dcl_id)
      (write-line "  : text { label = \"请选择排列方向:\"; }" dcl_id)
      (write-line "  : spacer { height = 0.3; }" dcl_id)
      (write-line "  : row { alignment = centered;" dcl_id)
      (write-line "    : button { key = \"d_down\"; label = \"↓ 从上往下\"; width = 16; }" dcl_id)
      (write-line "  }" dcl_id)
      (write-line "  : spacer { height = 0.2; }" dcl_id)
      (write-line "  : row { alignment = centered;" dcl_id)
      (write-line "    : button { key = \"d_right\"; label = \"→ 从左往右\"; width = 16; }" dcl_id)
      (write-line "    : spacer { width = 2; }" dcl_id)
      (write-line "    : button { key = \"d_left\";  label = \"← 从右往左\"; width = 16; }" dcl_id)
      (write-line "  }" dcl_id)
      (write-line "  : spacer { height = 0.2; }" dcl_id)
      (write-line "  : row { alignment = centered;" dcl_id)
      (write-line "    : button { key = \"d_up\"; label = \"↑ 从下往上\"; width = 16; }" dcl_id)
      (write-line "  }" dcl_id)
      (write-line "  : spacer { height = 0.3; }" dcl_id)
      (write-line "  : toggle {" dcl_id)
      (write-line "    key = \"special\";" dcl_id)
      (write-line "    label = \"特殊 (第4个字母强制为 N/n)\";" dcl_id)
      (write-line "    value = \"1\";" dcl_id)
      (write-line "  }" dcl_id)
      (write-line "  : spacer { height = 0.3; }" dcl_id)
      (write-line "  : row { alignment = centered;" dcl_id)
      (write-line "    : button { key = \"cancel\"; label = \"取消\"; is_cancel = true; width = 12; }" dcl_id)
      (write-line "  }" dcl_id)
      (write-line "}" dcl_id)
      (close dcl_id)

      (if (and (>= (setq dcl_id (load_dialog dclpath)) 0) (new_dialog "cs_dir" dcl_id))
        (progn
          (set_tile "special" "1")
          (action_tile "special" "(setq specialflg $value)")
          (action_tile "d_down"  "(done_dialog 1)")
          (action_tile "d_right" "(done_dialog 2)")
          (action_tile "d_up"    "(done_dialog 3)")
          (action_tile "d_left"  "(done_dialog 4)")
          (action_tile "cancel"  "(done_dialog 0)")
          (setq key (start_dialog))
          (unload_dialog dcl_id)
        )
      )
      (vl-file-delete dclpath)
    )
  )
  (if (and (numberp key) (> key 0))
    (list key (= specialflg "1"))
    (list 0 nil)
  )
)

;;------------------------------------------------------------
;; 结果对话框
;; 返回: 0=确定/取消  1=+  2=-  3=*  4=/  5=等差  6=转电流
;;------------------------------------------------------------
(defun CS:ShowResultDlg (sum cnt numlst / dclpath dcl_id key rows i10 i04 sqrt3)
  (setq sqrt3 1.7320508
        i10   (/ sum (* sqrt3 10.0))
        i04   (/ sum (* sqrt3 0.4))
        rows  (CS:NumListToRows numlst))
  (setq dclpath (vl-filename-mktemp "csres" nil ".dcl") key 0)
  (if (setq dcl_id (open dclpath "w"))
    (progn
      (write-line "cs_res : dialog {" dcl_id)
      (write-line "  label = \"计算结果\";" dcl_id)
      (write-line (strcat "  : text { key = \"t1\"; label = \"参与计算的数字个数: " (itoa cnt) "\"; }") dcl_id)
      (write-line "  : text { key = \"t2\"; label = \"参与计算的数字 (每行5个):\"; }" dcl_id)
      (write-line "  : list_box { key = \"numlist\"; height = 8; width = 48; fixed_width_font = true; }" dcl_id)
      (write-line (strcat "  : text { key = \"t3\"; label = \"数字之和 (kW / kVA): " (rtos sum 2 1) "\"; }") dcl_id)
      (write-line (strcat "  : text { key = \"t4\"; label = \"10kV 电流: " (rtos i10 2 1) " A    0.4kV 电流: " (rtos i04 2 1) " A\"; }") dcl_id)
      (write-line "  : spacer { height = 0.5; }" dcl_id)
      (write-line "  : row { alignment = centered;" dcl_id)
      (write-line "    : button { key = \"op_add\"; label = \"+\"; width = 7; }" dcl_id)
      (write-line "    : button { key = \"op_sub\"; label = \"-\"; width = 7; }" dcl_id)
      (write-line "    : button { key = \"op_mul\"; label = \"*\"; width = 7; }" dcl_id)
      (write-line "    : button { key = \"op_div\"; label = \"/\"; width = 7; }" dcl_id)
      (write-line "  }" dcl_id)
      (write-line "  : spacer { height = 0.3; }" dcl_id)
      (write-line "  : row { fixed_width = true; alignment = centered;" dcl_id)
      (write-line "    : button { key = \"op_seq\"; label = \"等差\"; width = 9; }" dcl_id)
      (write-line "    : button { key = \"op_cur\"; label = \"转电流\"; width = 9; }" dcl_id)
      (write-line "    : button { key = \"accept\"; label = \"确定\"; is_default = true; width = 12; }" dcl_id)
      (write-line "    : button { key = \"cancel\"; label = \"取消\"; is_cancel = true; width = 12; }" dcl_id)
      (write-line "  }" dcl_id)
      (write-line "}" dcl_id)
      (close dcl_id)

      (if (and (>= (setq dcl_id (load_dialog dclpath)) 0) (new_dialog "cs_res" dcl_id))
        (progn
          (start_list "numlist")
          (foreach x rows (add_list x))
          (end_list)
          (action_tile "op_add" "(done_dialog 1)")
          (action_tile "op_sub" "(done_dialog 2)")
          (action_tile "op_mul" "(done_dialog 3)")
          (action_tile "op_div" "(done_dialog 4)")
          (action_tile "op_seq" "(done_dialog 5)")
          (action_tile "op_cur" "(done_dialog 6)")
          (action_tile "accept" "(done_dialog 0)")
          (action_tile "cancel" "(done_dialog 0)")
          (setq key (start_dialog))
          (unload_dialog dcl_id)
        )
      )
      (vl-file-delete dclpath)
    )
  )
  (if (numberp key) key 0)
)

;;------------------------------------------------------------
;; 四则运算写回
;;------------------------------------------------------------
(defun CS:ApplyOpAndWrite (reclst op / x en val occIdx newval operand oldtxt newtxt)
  (setq operand nil)
  (initget 1)
  (setq operand (getreal "\n请输入要参与运算的数字: "))
  (if (null operand)
    (princ "\n已取消运算.")
    (if (and (= op 4) (equal operand 0.0 1e-10))
      (alert "除数不能为0.")
      (progn
        (foreach x reclst
          (setq en (car x) val (cadr x) occIdx (caddr x))
          (cond
            ((= op 1) (setq newval (+ val operand)))
            ((= op 2) (setq newval (- val operand)))
            ((= op 3) (setq newval (* val operand)))
            ((= op 4) (setq newval (/ val operand)))
          )
          (setq oldtxt (CS:GetTextContent en)
                newtxt (CS:ReplaceNumInStr oldtxt occIdx newval))
          (if (/= newtxt oldtxt) (CS:SetTextContent en newtxt))
        )
        (princ "\n写回完成.")
      )
    )
  )
)

;;------------------------------------------------------------
;; 选择电压等级 (用于"转电流")
;; 返回: 0.4 或 10.0；取消返回 nil
;;------------------------------------------------------------
(defun CS:AskVoltageLevel ( / dclpath dcl_id key)
  (setq dclpath (vl-filename-mktemp "csvlt" nil ".dcl") key 0)
  (if (setq dcl_id (open dclpath "w"))
    (progn
      (write-line "cs_vlt : dialog {" dcl_id)
      (write-line "  label = \"选择电压等级\";" dcl_id)
      (write-line "  : text { label = \"请选择用于换算电流的电压等级:\"; }" dcl_id)
      (write-line "  : spacer { height = 0.3; }" dcl_id)
      (write-line "  : row { alignment = centered;" dcl_id)
      (write-line "    : button { key = \"v04\"; label = \"0.4kV\"; width = 12; }" dcl_id)
      (write-line "    : button { key = \"v10\"; label = \"10kV\"; width = 12; }" dcl_id)
      (write-line "  }" dcl_id)
      (write-line "  : spacer { height = 0.3; }" dcl_id)
      (write-line "  : row { alignment = centered;" dcl_id)
      (write-line "    : button { key = \"cancel\"; label = \"取消\"; is_cancel = true; width = 12; }" dcl_id)
      (write-line "  }" dcl_id)
      (write-line "}" dcl_id)
      (close dcl_id)

      (if (and (>= (setq dcl_id (load_dialog dclpath)) 0) (new_dialog "cs_vlt" dcl_id))
        (progn
          (action_tile "v04"    "(done_dialog 1)")
          (action_tile "v10"    "(done_dialog 2)")
          (action_tile "cancel" "(done_dialog 0)")
          (setq key (start_dialog))
          (unload_dialog dcl_id)
        )
      )
      (vl-file-delete dclpath)
    )
  )
  (cond
    ((= key 1) 0.4)
    ((= key 2) 10.0)
    (T nil)
  )
)

;;------------------------------------------------------------
;; 判断字符串去除首尾空白后是否为"纯数字"(只含数字和最多一个小数点，
;; 不含其他字符如单位)——用于"转电流"时判断是否需要自动附加单位 A
;;------------------------------------------------------------
(defun CS:IsPureNumberStr (str / trimmed n i c hasDot hasDigit ok)
  (setq trimmed (vl-string-trim " \t" str) ok T hasDot nil hasDigit nil)
  (if (= trimmed "")
    nil
    (progn
      (setq n (strlen trimmed) i 1)
      (while (and ok (<= i n))
        (setq c (substr trimmed i 1))
        (cond
          ((wcmatch c "[0-9]") (setq hasDigit T))
          ((and (= c ".") (not hasDot)) (setq hasDot T))
          (T (setq ok nil))
        )
        (setq i (1+ i))
      )
      (and ok hasDigit)
    )
  )
)

;;------------------------------------------------------------
;; 转电流写回：把选中数字按 I = P / (√3 × U) 换算成对应电压等级电流，
;; 若原文本是纯数字(不含其他字符)，换算后自动附加单位 "A"
;;------------------------------------------------------------
(defun CS:ApplyCurrentAndWrite (reclst / sqrt3 u x en val occIdx newval oldtxt newtxt)
  (setq sqrt3 1.7320508)
  (setq u (CS:AskVoltageLevel))
  (if (null u)
    (princ "\n已取消转电流.")
    (progn
      (foreach x reclst
        (setq en (car x) val (cadr x) occIdx (caddr x)
              newval (/ val (* sqrt3 u))
              oldtxt (CS:GetTextContent en)
              newtxt (CS:ReplaceNumInStr oldtxt occIdx newval))
        (if (CS:IsPureNumberStr oldtxt)
          (setq newtxt (strcat newtxt "A"))
        )
        (if (/= newtxt oldtxt) (CS:SetTextContent en newtxt))
      )
      (princ "\n转电流写回完成.")
    )
  )
)

;;------------------------------------------------------------
;; 按方向对 reclst 排序
;; dir: 1=从上往下(Y降) 2=从左往右(X升) 3=从下往上(Y升) 4=从右往左(X降)
;;------------------------------------------------------------
(defun CS:SortByDir (reclst dir / lst pt en val)
  (setq lst '())
  (foreach x reclst
    (if (and (listp x) (car x) (cdr x))
      (progn
        (setq en (car x) val (cdr x) pt (CS:GetInsPt en))
        (if (and pt (listp pt) (car pt) (cadr pt))
          (setq lst (cons (list pt en val) lst))
        )
      )
    )
  )
  (setq lst (reverse lst))
  (if lst
    (progn
      (setq lst
        (vl-sort lst
          (function
            (lambda (a b / pa pb)
              (setq pa (car a) pb (car b))
              (cond
                ((= dir 1) (> (cadr pa) (cadr pb)))
                ((= dir 2) (< (car pa)  (car pb)))
                ((= dir 3) (< (cadr pa) (cadr pb)))
                ((= dir 4) (> (car pa)  (car pb)))
                (T nil)
              )
            )
          )
        )
      )
      (mapcar '(lambda (x) (cons (cadr x) (caddr x))) lst)
    )
    '()
  )
)

;;------------------------------------------------------------
;; 智能分列/分行聚类 (供 CSW 使用)
;; 思路: 按坐标轴(X用于分列,Y用于分行)对各实体插入点排序,
;;       比较相邻间距,若存在明显"突变"的大间距(远大于其余间距),
;;       则以该处为分界拆分成多组(列/行)；否则视为无法划分,返回单一整组.
;;------------------------------------------------------------

;; 在按坐标排序后的 items( (coord en val occIdx) ... ) 中寻找间距突变点并拆分.
;; 找不到明显突变时返回 (list 整组), 即退化为单一整体(等价于不拆分).
(defun CS:SplitByGap (items / n gaps i g gdesc numRatios k ratio bestk bestratio
                              eps boundaryIdx clusters cur item)
  (setq n (length items) eps 1e-6 gaps '())
  (setq i 0)
  (repeat (1- n)
    (setq g (- (car (nth (1+ i) items)) (car (nth i items))))
    (setq gaps (append gaps (list (list g i))))  ;; (间距值 间距所在索引)
    (setq i (1+ i))
  )
  ;; 按间距值从大到小排序
  (setq gdesc (vl-sort gaps (function (lambda (a b) (> (car a) (car b))))))
  ;; 在相邻(排序后)间距之间寻找最大的"跳变比例"(即分界所在)
  (setq bestk -1 bestratio 0.0 numRatios (1- (length gdesc)))
  (setq k 0)
  (repeat (max 0 numRatios)
    (setq ratio (/ (car (nth k gdesc)) (max eps (car (nth (1+ k) gdesc)))))
    (if (> ratio bestratio) (setq bestratio ratio bestk k))
    (setq k (1+ k))
  )
  ;; 跳变比例不够明显(<1.8)或最大间距本身接近0，判定无法划分
  (if (or (< bestk 0) (< bestratio 1.8) (< (car (nth bestk gdesc)) eps))
    (list (mapcar '(lambda (it) (list (cadr it) (caddr it) (cadddr it))) items))
    (progn
      ;; 前 bestk+1 个最大间距即为分界点
      (setq boundaryIdx '() k 0)
      (repeat (1+ bestk)
        (setq boundaryIdx (cons (cadr (nth k gdesc)) boundaryIdx))
        (setq k (1+ k))
      )
      (setq clusters '() cur '() i 0)
      (foreach item items
        (setq cur (append cur (list (list (cadr item) (caddr item) (cadddr item)))))
        (if (member i boundaryIdx)
          (progn (setq clusters (append clusters (list cur))) (setq cur '()))
        )
        (setq i (1+ i))
      )
      (if (> (length cur) 0) (setq clusters (append clusters (list cur))))
      clusters
    )
  )
)

;; 从 reclst( (en val occIdx) ... ) 按轴聚类. axis: 0=按X分列 1=按Y分行.
;; 返回聚类列表, 每个元素仍是 (en val occIdx) 格式的子列表；
;; 若坐标点不足或聚类不出多组, 返回 (list reclst) 即单一整组.
(defun CS:BuildClusters (reclst axis / items x en val occIdx pt coord clusters)
  (setq items '())
  (foreach x reclst
    (setq en (car x) val (cadr x) occIdx (caddr x) pt (CS:GetInsPt en))
    (if (and pt (listp pt) (car pt) (cadr pt))
      (setq coord (if (= axis 0) (car pt) (cadr pt))
            items (append items (list (list coord en val occIdx))))
    )
  )
  (if (< (length items) 3)
    (list reclst)
    (progn
      (setq items (vl-sort items (function (lambda (a b) (< (car a) (car b))))))
      (setq clusters (CS:SplitByGap items))
      ;; 分列(axis=0)按X升序即"从左到右"，符合习惯，保持不变；
      ;; 分行(axis=1)聚类后是Y升序("从下到上")，反转为"从上到下"更符合阅读习惯。
      (if (= axis 1) (reverse clusters) clusters)
    )
  )
)

;;------------------------------------------------------------
;; 收集参与计算的数字与实体 (供 CS / CSC 共用)
;; 返回 (list reclst numlst sum cnt) 或 nil
;;------------------------------------------------------------
(defun CS:CollectData ( / ss i en txt nums multiLst singleSum sum cnt item
                          chosen applyrest fixedRules numcnt selidx val result pair
                          numlst reclst)
  (princ "\n请选择要处理的文本对象...")
  (setq ss (ssget '((0 . "TEXT,MTEXT"))))
  (if (not ss)
    (progn (princ "\n未选择任何文本.") nil)
    (progn
      (setq multiLst '() singleSum 0.0 sum 0.0 cnt 0
            fixedRules '() numlst '() reclst '())
      (setq i 0)
      (repeat (sslength ss)
        (setq en (ssname ss i) txt (CS:GetTextContent en) nums (CS:ExtractNums txt))
        (cond
          ((= (length nums) 0) )
          ((= (length nums) 1)
           (setq singleSum (+ singleSum (car nums))
                 cnt (1+ cnt)
                 numlst (append numlst (list (car nums)))
                 reclst (append reclst (list (list en (car nums) 1))))
          )
          (T (setq multiLst (append multiLst (list (list en txt nums)))))
        )
        (setq i (1+ i))
      )
      (setq sum singleSum)
      (foreach item multiLst
        (setq en (car item) txt (cadr item) nums (caddr item)
              numcnt (length nums) pair (assoc numcnt fixedRules))
        (cond
          (pair
           (setq selidx (cdr pair))
           (if (<= selidx numcnt)
             (setq val (nth (1- selidx) nums)
                   sum (+ sum val) cnt (1+ cnt)
                   numlst (append numlst (list val))
                   reclst (append reclst (list (list en val selidx))))
           )
          )
          (T
           (setq result (CS:AskOneDlg txt nums))
           (if result
             (progn
               (setq chosen (car result) applyrest (cadr result) selidx (caddr result)
                     sum (+ sum chosen) cnt (1+ cnt)
                     numlst (append numlst (list chosen))
                     reclst (append reclst (list (list en chosen selidx))))
               (if applyrest
                 (setq fixedRules (cons (cons numcnt selidx) fixedRules))
               )
             )
           )
          )
        )
      )
      (if (> cnt 0)
        (list reclst numlst sum cnt)
        (progn (alert "没有可计算的数字.") nil)
      )
    )
  )
)

;;------------------------------------------------------------
;; 收集参与计算的字母与实体 (供 CSS 共用)
;; 返回 (list reclst) 或 nil
;;------------------------------------------------------------
(defun CS:CollectLetterData ( / ss i en txt letters multiLst cnt item
                                chosen applyrest fixedRules letcnt selidx val result pair
                                reclst)
  (princ "\n请选择要处理的文本对象...")
  (setq ss (ssget '((0 . "TEXT,MTEXT"))))
  (if (not ss)
    (progn (princ "\n未选择任何文本.") nil)
    (progn
      (setq multiLst '() cnt 0 fixedRules '() reclst '())
      (setq i 0)
      (repeat (sslength ss)
        (setq en (ssname ss i) txt (CS:GetTextContent en) letters (CS:ExtractLetters txt))
        (cond
          ((= (length letters) 0) )
          ((= (length letters) 1)
           (setq cnt (1+ cnt)
                 reclst (append reclst (list (cons en (car letters)))))
          )
          (T (setq multiLst (append multiLst (list (list en txt letters)))))
        )
        (setq i (1+ i))
      )
      (foreach item multiLst
        (setq en (car item) txt (cadr item) letters (caddr item)
              letcnt (length letters) pair (assoc letcnt fixedRules))
        (cond
          (pair
           (setq selidx (cdr pair))
           (if (<= selidx letcnt)
             (setq val (nth (1- selidx) letters)
                   cnt (1+ cnt)
                   reclst (append reclst (list (cons en val))))
           )
          )
          (T
           (setq result (CS:AskOneLetterDlg txt letters))
           (if result
             (progn
               (setq chosen (car result) applyrest (cadr result) selidx (caddr result)
                     cnt (1+ cnt)
                     reclst (append reclst (list (cons en chosen))))
               (if applyrest
                 (setq fixedRules (cons (cons letcnt selidx) fixedRules))
               )
             )
           )
          )
        )
      )
      (if (> cnt 0)
        (list reclst)
        (progn (alert "没有可计算的字母.") nil)
      )
    )
  )
)

;;------------------------------------------------------------
;; 等差数列写回（首数字可改，公差默认1）
;;------------------------------------------------------------
;; 等差写回核心逻辑：已确定方向dir，对reclst排序后交互首数字/公差并写回。
;; label: 提示前缀(如 "[第1列] ")，用于CSW区分多组；单组场景传 nil 或 "" 即可。
(defun CS:ApplySeqCore (reclst dir label / sorted d a0 a0in din i en val occIdx newval oldtxt newtxt)
  (if (null label) (setq label ""))
  (setq sorted (CS:SortByDir reclst dir))
  (if (or (null sorted) (= (length sorted) 0))
    (princ (strcat "\n" label "无可用数字."))
    (progn
      (setq a0 (cadr (car sorted)))
      ;; 首数字交互，默认显示程序获取到的值
      (setq a0in (getreal (strcat "\n" label "首数字 <" (rtos a0 2 4) ">: ")))
      (if a0in (setq a0 a0in))
      ;; 公差默认1
      (setq din (getreal (strcat "\n" label "请输入等差值 (公差) <1>: ")))
      (setq d (if din din 1.0))
      (setq i 0)
      (foreach x sorted
        (setq en (car x)
              val (cadr x)
              occIdx (caddr x)
              newval (+ a0 (* i d))
              i (1+ i)
              oldtxt (CS:GetTextContent en)
              newtxt (CS:ReplaceNumInStr oldtxt occIdx newval)
        )
        (if (/= newtxt oldtxt)
          (CS:SetTextContent en newtxt)
        )
      )
      (princ (strcat "\n" label "写回完成."))
    )
  )
)

;; 等差写回(自动首数字版): 已确定方向dir与公差d(整体共用,不再交互),
;; 排序后直接取第一个实体自身原始数字作为首数字(不询问/不弹窗)，随后写回。
;; 供CSW在成功分列/分行时,对每一组分别调用。
(defun CS:ApplySeqAuto (reclst dir d label / sorted a0 i en val occIdx newval oldtxt newtxt)
  (if (null label) (setq label ""))
  (setq sorted (CS:SortByDir reclst dir))
  (if (or (null sorted) (= (length sorted) 0))
    (princ (strcat "\n" label "无可用数字."))
    (progn
      (setq a0 (cadr (car sorted)))
      (princ (strcat "\n" label "首数字自动取: " (rtos a0 2 4)))
      (setq i 0)
      (foreach x sorted
        (setq en (car x)
              val (cadr x)
              occIdx (caddr x)
              newval (+ a0 (* i d))
              i (1+ i)
              oldtxt (CS:GetTextContent en)
              newtxt (CS:ReplaceNumInStr oldtxt occIdx newval)
        )
        (if (/= newtxt oldtxt)
          (CS:SetTextContent en newtxt)
        )
      )
      (princ (strcat "\n" label "写回完成."))
    )
  )
)

(defun CS:ApplySeqAndWrite (reclst / dirret dir)
  (setq dirret (CS:AskDirection)
        dir (if (listp dirret) (car dirret) dirret))
  (if (or (null dir) (not (numberp dir)) (= dir 0))
    (princ "\n已取消等差.")
    (CS:ApplySeqCore reclst dir nil)
  )
)

;;------------------------------------------------------------
;; 字母等差写回（首字母可改，公差默认1，循环A-Z）
;; 若勾选特殊复选框，则不询问首字母与公差：
;;   首字母强制为 A/a（保持原大小写），公差1，第4个强制为 N/n
;;------------------------------------------------------------
(defun CS:ApplyLetterSeqAndWrite (reclst / dirret dir special sorted a0 a0in din d i en val newval oldtxt newtxt upper)
  (setq dirret (CS:AskDirection)
        dir (if (listp dirret) (car dirret) dirret)
        special (if (and (listp dirret) (cadr dirret)) T nil))
  (if (or (null dir) (not (numberp dir)) (= dir 0))
    (princ "\n已取消字母等差.")
    (progn
      (setq sorted (CS:SortByDir reclst dir))
      (if (or (null sorted) (= (length sorted) 0))
        (princ "\n无可用字母.")
        (progn
          (setq a0 (cdr (car sorted)))
          (if special
            ;; 勾选特殊时：不询问，首字母强制 A/a（按原大小写），公差1
            (progn
              (setq upper (CS:IsUpper a0))
              (setq a0 (if upper "A" "a"))
              (setq d 1)
            )
            (progn
              ;; 未勾选时：首字母交互，默认显示程序获取到的值
              (setq a0in (getstring (strcat "\n首字母 <" a0 ">: ")))
              (if (and a0in (/= a0in "") (wcmatch (substr a0in 1 1) "[A-Za-z]"))
                (setq a0 (substr a0in 1 1))
              )
              ;; 公差默认1
              (setq din (getint "\n请输入等差值 (公差) <1>: "))
              (setq d (if din din 1))
              (setq upper (CS:IsUpper a0))
            )
          )
          (setq i 0)
          (foreach x sorted
            (setq en (car x)
                  val (cdr x)
                  newval (CS:OffsetToLetter (+ (CS:LetterToOffset a0) (* i d)) upper)
            )
            ;; 特殊: 第4个(索引3)强制为 N 或 n
            (if (and special (= i 3))
              (setq newval (if upper "N" "n"))
            )
            (setq i (1+ i)
                  oldtxt (CS:GetTextContent en)
                  newtxt (CS:ReplaceLetterInStr oldtxt val newval)
            )
            (if (/= newtxt oldtxt)
              (CS:SetTextContent en newtxt)
            )
          )
          (princ "\n写回完成.")
        )
      )
    )
  )
)

;;------------------------------------------------------------
;; 主命令 CS
;;------------------------------------------------------------
(defun c:CS ( / data reclst numlst sum cnt op)
  (princ "\n【文本-计算和】")
  (setq data (CS:CollectData))
  (if data
    (progn
      (setq reclst (car data) numlst (cadr data) sum (caddr data) cnt (cadddr data))
      (setq op (CS:ShowResultDlg sum cnt numlst))
      (cond
        ((and (numberp op) (> op 0) (< op 5)) (CS:ApplyOpAndWrite reclst op))
        ((= op 5) (CS:ApplySeqAndWrite reclst))
        ((= op 6) (CS:ApplyCurrentAndWrite reclst))
      )
    )
  )
  (princ)
)

;;------------------------------------------------------------
;; 命令 CSC - 直接进入等差流程
;;------------------------------------------------------------
(defun c:CSC ( / data reclst)
  (princ "\n【文本-等差】")
  (setq data (CS:CollectData))
  (if data
    (progn
      (setq reclst (car data))
      (CS:ApplySeqAndWrite reclst)
    )
  )
  (princ)
)

;;------------------------------------------------------------
;; 命令 CSS - 直接进入字母等差流程
;;------------------------------------------------------------
(defun c:CSS ( / data reclst)
  (princ "\n【文本-字母等差】")
  (setq data (CS:CollectLetterData))
  (if data
    (progn
      (setq reclst (car data))
      (CS:ApplyLetterSeqAndWrite reclst)
    )
  )
  (princ)
)

;;------------------------------------------------------------
;; 命令 CSW - 在CSC基础上智能分列/分行等差
;; 上下方向(1/3): 按X坐标智能判断能否分列，能分则逐列独立执行等差；
;; 左右方向(2/4): 按Y坐标智能判断能否分行，能分则逐行独立执行等差；
;; 判断不出明显分组时，按常规CSC(整体一次等差)处理。
;;------------------------------------------------------------
(defun c:CSW ( / data reclst dirret dir clusters axis kind idx total cl label din d)
  (princ "\n【文本-智能分列/分行等差】")
  (setq data (CS:CollectData))
  (if data
    (progn
      (setq reclst (car data))
      (setq dirret (CS:AskDirection)
            dir (if (listp dirret) (car dirret) dirret))
      (if (or (null dir) (not (numberp dir)) (= dir 0))
        (princ "\n已取消等差.")
        (progn
          (setq axis (if (or (= dir 1) (= dir 3)) 0 1)
                kind (if (= axis 0) "列" "行"))
          (setq clusters (CS:BuildClusters reclst axis))
          (cond
            ((<= (length clusters) 1)
             (princ (strcat "\n未检测到可拆分的" kind ",按常规等差处理."))
             (CS:ApplySeqCore reclst dir nil)
            )
            (T
             (princ (strcat "\n检测到 " (itoa (length clusters)) " " kind ",将分别执行等差(各" kind "首数字自动识别)."))
             ;; 公差整体只问一次，所有列/行共用；各组首数字各自按排序结果自动取值，不再交互
             (setq din (getreal "\n请输入等差值 (公差,所有列/行共用) <1>: "))
             (setq d (if din din 1.0))
             (setq idx 0 total (length clusters))
             (foreach cl clusters
               (setq idx (1+ idx)
                     label (strcat "[第" (itoa idx) kind "] "))
               (princ (strcat "\n--- " label "共 " (itoa (length cl)) " 个 ---"))
               (CS:ApplySeqAuto cl dir d label)
             )
             (princ "\n全部写回完成.")
            )
          )
        )
      )
    )
  )
  (princ)
)

(princ "\n文本-计算和 已加载. 命令: CS / CSC / CSS / CSW")
(princ)
