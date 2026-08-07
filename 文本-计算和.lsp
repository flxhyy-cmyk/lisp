;;; ============================================================
;;; 文本-计算和.lsp
;;; 命令: CS
;;; 功能: 选择文字对象, 自动提炼文本内容, 提取其中的数字并求和
;;;   1. 提示用户单选/框选文字(TEXT/MTEXT)
;;;   2. 自动提炼文本(去除MTEXT格式码), 提取所有数字(含小数)
;;;   3. 只含1个数字的文本自动纳入计算
;;;   4. 含多个数字的文本按数字数量分组交互选择:
;;;      数字个数相同的文本归为一组, 每组分别弹出
;;;      选择弹窗(单选框)确定该组统一用第几位数字
;;;   5. 计算所有数字之和, 弹窗显示结果(焦点在确定按钮)
;;; DCL 内嵌生成, 单一代码文件
;;; 编码: ANSI (GBK)
;;; ============================================================

(vl-load-com)

;; ---------- 通用: 数字转显示字符串(去掉尾零) ----------
(defun csk-rtos (v / s)
  (setq s (rtos v 2 4))
  (if (vl-string-search "." s)
    (progn
      (while (and (> (strlen s) 1)
                  (= (substr s (strlen s)) "0"))
        (setq s (substr s 1 (1- (strlen s)))))
      (if (= (substr s (strlen s)) ".")
        (setq s (substr s 1 (1- (strlen s)))))))
  s)

;; ---------- MTEXT 格式码清理 ----------
(defun csk-strip-mtext (str / out i n c nc j brace)
  (setq out "" i 1 n (strlen str) brace nil)
  (while (<= i n)
    (setq c (substr str i 1))
    (cond
      ;; 反斜杠转义控制码
      ((= c "\\")
       (setq nc (substr str (1+ i) 1))
       (cond
         ;; \P 段落换行 -> 空格
         ((= nc "P")
          (if (not brace) (setq out (strcat out " ")))
          (setq i (+ i 2)))
         ;; \S 堆叠文本 -> 跳过直到分号
         ((= nc "S")
          (setq j (vl-string-search ";" str i))
          (if j
            (setq i (+ j 2))
            (setq i (1+ n))))
         ;; 其他控制码 -> 跳过控制字符
         (t (setq i (+ i 2))))
       (if (> i n) (setq i (1+ n))))
      ;; 大括号格式段 {格式码;显示文本}
      ((= c "{") (setq brace T) (setq i (1+ i)))
      ((= c "}") (setq brace nil) (setq i (1+ i)))
      ;; 格式段内遇到分号 -> 之后为显示文本
      ((and brace (= c ";"))
       (setq brace nil) (setq i (1+ i)))
      ;; 格式段内的内容不输出
      (brace (setq i (1+ i)))
      ;; 普通字符输出
      (t (setq out (strcat out c)) (setq i (1+ i)))))
  out)

;; ---------- 清洗数字串(去掉多余小数点, 纯点返回nil) ----------
(defun csk-clean-num (s / i n c out hasdigit)
  (setq i 1 n (strlen s) out "" hasdigit nil)
  (while (<= i n)
    (setq c (substr s i 1))
    (if (= c ".")
      (if (vl-string-search "." out)
        nil
        (setq out (strcat out c)))
      (progn
        (setq out (strcat out c))
        (setq hasdigit T)))
    (setq i (1+ i)))
  (if hasdigit (atof out) nil))

;; ---------- 提取字符串中的数字(含小数) ----------
(defun csk-extract-nums (str / i n c cur nums v hasdot)
  (setq i 1 n (strlen str) cur "" nums nil hasdot nil)
  (while (<= i n)
    (setq c (substr str i 1))
    (cond
      ((= c ".")
       (if hasdot
         ;; 第二个小数点: 先提交当前数字, 再以该点为起点
         (progn
           (setq v (csk-clean-num cur))
           (if v (setq nums (append nums (list v))))
           (setq cur "." hasdot nil))
         (progn
           (setq cur (strcat cur c))
           (setq hasdot T))))
      ((and (>= (ascii c) 48) (<= (ascii c) 57))
       (setq cur (strcat cur c)))
      (t
       (if (/= cur "")
         (progn
           (setq v (csk-clean-num cur))
           (if v (setq nums (append nums (list v))))
           (setq cur "" hasdot nil)))))
    (setq i (1+ i)))
  (if (/= cur "")
    (progn
      (setq v (csk-clean-num cur))
      (if v (setq nums (append nums (list v))))))
  nums)

;; ---------- 生成 DCL 文件 ----------
(defun csk-write-dcl (dclfile / f i)
  (setq f (open dclfile "w"))
  ;; 分组选择对话框(单选框选择第几位)
  (write-line "all_dlg : dialog {" f)
  (write-line "  label = \"文本-计算和 - 选择数字\";" f)
  (write-line "  : text { key = \"info\"; alignment = centered; width = 60; height = 12; }" f)
  (write-line "  : text { label = \"请选择本组统一采用的数字位数:\"; }" f)
  (write-line "  : radio_column { key = \"rc\";" f)
  (setq i 1)
  (while (<= i csk-max-num)
    (write-line (strcat "    : radio_button { key = \"rb" (itoa (1- i)) "\"; label = \"第" (itoa i) "个\"; }") f)
    (setq i (1+ i)))
  (write-line "  }" f)
  (write-line "  ok_cancel;" f)
  (write-line "}" f)
  ;; 结果对话框(加高, 同时显示和与计算电流)
  (write-line "res_dlg : dialog {" f)
  (write-line "  label = \"文本-计算和\";" f)
  (write-line "  : text { key = \"info\"; alignment = centered; width = 50; height = 6; }" f)
  (write-line "  ok_only;" f)
  (write-line "}" f)
  (close f))

;; ---------- 数字列表拼接显示 ----------
(defun csk-join-nums (nums / s first)
  (setq s "" first T)
  (foreach n nums
    (if first
      (setq s (csk-rtos n) first nil)
      (setq s (strcat s " / " (csk-rtos n)))))
  s)

;; ---------- 读取单选框选中的位数索引 ----------
(defun csk-radio-idx (/ i)
  (setq csk-radio-result 0 i 0)
  (while (< i csk-max-num)
    (if (= (get_tile (strcat "rb" (itoa i))) "1")
      (setq csk-radio-result i i csk-max-num)
      (setq i (1+ i))))
  csk-radio-result)

;; ---------- 分组选择弹窗(同组文本数字数量相同, 单选框选第几位) ----------
(defun csk-pick-group-dlg (items cnt / dclfile dclid rslt lines i item)
  (setq dclfile (strcat (getenv "TEMP") "\\cs_calc.dcl"))
  (setq csk-max-num cnt)
  (csk-write-dcl dclfile)
  (setq dclid (load_dialog dclfile))
  (setq csk-pick-all-result nil)
  (if (and (> dclid 0) (new_dialog "all_dlg" dclid))
    (progn
      ;; 说明: 列出本组每个文本的数字
      (setq lines (strcat "本组 " (itoa (length items)) " 个文本均含 " (itoa cnt) " 个数字:\n"))
      (setq i 0)
      (foreach item items
        (setq lines (strcat lines "第" (itoa (1+ i)) "个: " (csk-join-nums (cadr item)) "\n"))
        (setq i (1+ i)))
      (set_tile "info" lines)
      ;; 默认选第1个
      (set_tile "rb0" "1")
      (action_tile "accept" "(csk-radio-idx) (done_dialog 1)")
      (action_tile "cancel" "(done_dialog 0)")
      (setq rslt (start_dialog))
      (if (= rslt 1)
        (setq csk-pick-all-result csk-radio-result)))
    (alert "DCL 加载失败!"))
  (unload_dialog dclid)
  csk-pick-all-result)

;; ---------- 结果弹窗(焦点在确定按钮) ----------
;; 功率按 KW 处理, 电压按 0.4KV, 三相电流 I = P(KW)*1000 / (1.732*400*cosf)
(defun csk-result-dlg (total n sum / dclfile dclid msg cur)
  (setq dclfile (strcat (getenv "TEMP") "\\cs_calc.dcl"))
  (csk-write-dcl dclfile)
  (setq dclid (load_dialog dclfile))
  (if (and (> dclid 0) (new_dialog "res_dlg" dclid))
    (progn
      (setq cur (/ (* sum 1000.0) (* 1.7320508 400.0 0.85)))
      (setq msg (strcat "数字之和 = " (csk-rtos sum) " KW"
                        "\n电压 = 0.4 KV"
                        "\n计算电流 = " (csk-rtos cur) " A"))
      (set_tile "info" msg)
      (action_tile "accept" "(done_dialog 1)")
      (start_dialog))
    (alert "DCL 加载失败!"))
  (unload_dialog dclid))

;; ---------- 主命令 ----------
(defun c:CS (/ ss i ent etype content nums item v k sum cnt cancel pick-idx num-cnt idx2 g)
  (vl-load-com)
  (princ "\n请选择要计算的文字对象(可框选): ")
  (setq ss (ssget '((0 . "TEXT,MTEXT"))))
  (if (null ss)
    (alert "未选择任何文字!")
    (progn
      ;; 1. 提炼文本内容并提取数字
      (setq csk-texts nil)
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq etype (cdr (assoc 0 (entget ent))))
        (setq content (cdr (assoc 1 (entget ent))))
        (if (= etype "MTEXT")
          (setq content (csk-strip-mtext content)))
        (setq nums (csk-extract-nums content))
        (if (> (length nums) 0)
          (setq csk-texts (append csk-texts (list (list content nums)))))
        (setq i (1+ i)))
      (if (null csk-texts)
        (alert "所选文字中未提取到数字!")
        (progn
          ;; 2. 分类: 单数字自动纳入 / 多数字需要选择
          (setq csk-auto nil csk-need nil)
          (foreach item csk-texts
            (if (= (length (cadr item)) 1)
              (setq csk-auto (append csk-auto (list (car (cadr item)))))
              (setq csk-need (append csk-need (list item)))))
          ;; 3. 按数字数量分组, 每组弹窗选择第几位
          (setq cancel nil csk-picked csk-auto)
          (setq csk-groups nil)
          (foreach item csk-need
            (setq num-cnt (length (cadr item)))
            (setq g (assoc num-cnt csk-groups))
            (if g
              (setq csk-groups (subst (cons num-cnt (append (cdr g) (list item))) g csk-groups))
              (setq csk-groups (append csk-groups (list (cons num-cnt (list item)))))))
          ;; 每组分别弹窗交互, 组内统一应用所选位数
          (foreach g csk-groups
            (setq pick-idx (csk-pick-group-dlg (cdr g) (car g)))
            (if pick-idx
              (progn
                (foreach item (cdr g)
                  ;; 取第 pick-idx 位数字(0基), 越界取最后一个
                  (setq idx2 (min pick-idx (1- (length (cadr item)))))
                  (setq v (nth idx2 (cadr item)))
                  (if v (setq csk-picked (append csk-picked (list v))))))
              (setq cancel T)))
          ;; 4. 求和并弹窗显示
          (if (not cancel)
            (progn
              (setq sum 0.0 cnt 0)
              (foreach v csk-picked
                (setq sum (+ sum v) cnt (1+ cnt)))
              (csk-result-dlg (length csk-texts) cnt sum)))))))
  (princ))

;; ---------- 加载提示 ----------
(princ "\n>> 文本-计算和 已加载, 命令: CS <<")
(princ)
