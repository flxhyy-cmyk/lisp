(vl-load-com)
;;;*SysVarNL* 常用系统变量表
(setq *SysVarNL*
       (list 'AUNITS      'AUPREC	     'ATTDIA	   'ATTREQ
	 'BLIPMODE    'DIMZIN	     'CECOLOR	   'CELTYPE
	 'CLAYER      'CMDECHO     'TRIMMODE	   'EXPERT
	 'HIGHLIGHT   'LUNITS	     'LUPREC	   'EDGEMODE
	 'OSMODE      'ORTHOMODE   'TEXTSTYLE   'PLINEWID 'PLINEGEN
	 'FILEDIA     'PICKBOX     'QAFLAGS	   'UCSAXISANG
	 'CELTSCALE 'NOMUTT 'PEDITACCEPT 'Mirrtext 'limcheck 'REGENMODE
	)
) ;_ setq

;;;常量定义
(setq *Acad*	   (vlax-get-acad-object)
      *AcDocument* (vla-get-activedocument *Acad*)
      *Model-Space* (vla-get-modelspace *AcDocument*)
      *Paper-Space* (vla-get-PaperSpace *AcDocument*)
      pi2	   (* pi 0.5)
      pi4	   (* pi 0.25)
      2pi	   (* pi 2.)
      3pi2	   (* 1.5 pi)
      3pi4   (+ pi2 pi4)
      5pi4   (+ pi pi4)
      7pi4 (+ 3pi2 pi4)
      #ZJWS# 2
      *jd* 0.00001
      en2obj vlax-ename->vla-object
      obj2en vlax-vla-object->ename
      *Space* (vlax-get-property *AcDocument* (if (= 1 (getvar 'CVPORT)) 'PaperSpace 'ModelSpace))
)
;;;增加内置函数
(mapcar	'vl-arx-import
	'(ACAD_COLORDLG	     ACAD_truecolordlg	ACAD_STRLSORT
	  INITDIA	     ACAD-POP-DBMOD	ACAD-PUSH-DBMOD
	  STARTAPP	     layoutlist Bpoly
	 )
)


;;;*************函数 gxl-midpoint.lsp  *************
;;;==================================================================
;;;gxl-MidPoint 表操作函数,计算两点的中点
;;;计算两点的中点
;;;==================================================================
(defun gxl-MidPoint (p1 p2)
  (mapcar '(lambda (x) (* x 0.5)) (mapcar '+ p1 p2))
  )
;;;***************** 函数 gxl-MidPoint*****************


;;;*************函数 gxl-dcl-addlist.lsp  *************

;;;填充List gxl-dcl-AddList
(defun gxl-dcl-AddList (key val item)
  (start_list key)
  (foreach n val
    (add_list n)
  )
  (end_list)
  (if item
  (if (= 'INT (type item))
  (set_tile key (itoa item))
    (if (= 'STR (type item))
    (set_tile key item)
      )
    )
    )
)
;;;***************** 函数 gxl-dcl-AddList*****************


;;;*************函数 gxl-strisnum.lsp  *************

;;;==================================================================
;;; (gxl-strIsNum str)	确定字符串是否为数字
;;;------------------------------------------------------------------
;;; 参数:
;;;		Str			str 检查的字符串
;;;------------------------------------------------------------------
;;;	Returns:
;;;		[LOG]	T ：字符串为数字, F 字符串不是数字
;;;		Example: 	(setq a "+.123a") (atof "a")(atoi "-.12")
;;;					(setq b '1B5')
;;;				(gxl-strIsNum a) 返回 T
;;;					(gxl-strIsNum b) 返回 F
;;;==================================================================
(defun gxl-strIsNum (str)
  (numberp (VL-CATCH-ALL-APPLY
	     'read
	     (list (cond
		     ((= "." (substr str 1 1))
		      (strcat "0" str)
		     )
		     ((and
			(= "-." (substr str 1 2))
			(> (strlen str) 2)
			)
		      (strcat "-0" (substr str 2))
		     )
		     ((and
			(= "+." (substr str 1 2))
			(> (strlen str) 2)
			)
		      (strcat "+0" (substr str 2))
		     )
		     (t
		      str
		     )
		   )
	     )
	   )
  )
)
;;;***************** 函数 gxl-strIsNum*****************


;;;*************函数 gxl-strisint.lsp  *************

;;(gxl-StrIsInt str)	确定字符串是否为整数 (gxl-StrIsInt "-1.2")
(defun gxl-StrIsInt (str)
  (and (gxl-strIsNum str)
       (not
	 (or
	   (= "." (substr str 1 1))
	   (= "-." (substr str 1 2))
	   (= "+." (substr str 1 2))
	   )
	 )
       (= 'int (type (read str)))
       )
  )
;;;***************** 函数 gxl-StrIsInt*****************


;;;*************函数 gxl-strisreal.lsp  *************
;;(gxl-StrIsReal str)	确定字符串是否为实数
(defun gxl-StrIsReal (str)
  (and (gxl-strIsNum str)
       (or
	   (= "." (substr str 1 1))
	   (= "-." (substr str 1 2))
	   (= "+." (substr str 1 2))
	   (= 'int (type (read str)))
	   (= 'REAL (type (read str)))
	   )
       
       )
  )
;;;***************** 函数 gxl-StrIsReal*****************


;;;*************函数 gxl-chkreal.lsp  *************
;;;==================================================================
;;;gxl-chkreal 对话框输入检查是否为实型数函数,返回实型数或nil
;;;==================================================================
;|(defun gxl-chkreal (a *key* / chk_flag)
  (if (= (substr a 1 1) ".") (setq a (strcat "0" a)))
  (setq a (read a))
  (if (or (= 'INT (type a)) (= 'REAL (type a)))
    (progn
      (setq chk_flag t)
      a
    )					;progn
    (progn
      (alert "请输入实型数!")
      (mode_tile *key* 2)
      (setq chk_flag nil)
    )					;progn
  )					;if
)|;
(defun gxl-chkreal (a *key*)
  (cond
    ((gxl-STRISINT a) (atoi a))
    ((gxl-STRISREAL a) (atof a))
    (t (alert "请输入实型数!")
     (mode_tile *key* 2)
     nil
     )
    )
)
;;;***************** 函数 gxl-chkreal*****************


;;;*************函数 gxl-chkrealp.lsp  *************
;;(gxl-chkrealp val key bits) 对话框输入检查是否为实型数函数,返回实型数或nil,bits 按位标志，1 = 非正数 2 = 非零 4 = 非负
(defun gxl-chkrealp (val key bits / chkFlag)
  (setq chkflag (gxl-chkreal val key))
  (if (and chkflag
	   (= 1 (logand bits 1)) ;_ 1 = 非正数
	   )
    (progn
      (if (> (atof val) 0)
	(progn
	  (setq chkflag nil)
	  (alert "请输入非正数!")
	  (mode_tile key 2)
	  )
	)
      )
    )
  (if (and chkflag
	   (= 2 (logand bits 2)) ;_ 2 = 非零
	   )
    (progn
      (if (zerop (atof val))
	(progn
	  (setq chkflag nil)
	  (alert "请输入非零数!")
	  (mode_tile key 2)
	  )
	)
      )
    )
  (if (and chkflag
	   (= 4 (logand bits 4)) ;_ 4 = 非负数
	   )
    (progn
      (if (minusp (atof val))
	(progn
	  (setq chkflag nil)
	  (alert "请输入非负数!")
	  (mode_tile key 2)
	  )
	)
      )
    )
  (if chkflag
    (progn
      (if (gxl-STRISINT val)
	(atoi val)
	(atof val)
	)
      )
    )
  )
;;;***************** 函数 gxl-chkrealp*****************


;;;*************函数 vlxls-app-open.lsp  *************
 
;|Examples:

(setq *xlapp* (vlxls-app-new T)) è #<VLA-OBJECT _Application 001db27c>
 

 

Excel Application Session Progress Function
 
Name
 (vlxls-app-open XLSfilename ShowExcelFlag)
 
Usage
 Open a new Excel session to start existing XLS file.
 
Input
 STR
 XLS file name with full path, ".XLS" not needed.
 
BOOLEAN
 T for display, nil for hide
 
RetVal
 True
 VLOBJ
 Excel Session vla-object
 
Fail
 BOOLEAN
 NIL
 |;
(Defun vlxls-app-open

       (XLSFile UnHide / ExcelApp WorkSheet Sheets ActiveSheet Rtn)

  (setq XLSFile (strcase XLSFile))

  (if (null (wcmatch XLSFile "*`.XLS,*`.XLSX"))

    (setq XLSFile (strcat XLSFile ".XLS"))

  )

  (if (and (findfile XLSFile)

          (setq Rtn (vlax-get-or-create-object "Excel.Application"))

      )

    (progn
     (if UnHide

       (vla-put-visible Rtn 1)

       (vla-put-visible Rtn 0)

      )
      (vlax-invoke-method

       (vlax-get-property Rtn 'WorkBooks)

       'Open

       XLSFile

      )

      

    )

  )

  Rtn

)
;;;***************** 函数 vlxls-app-open*****************


;;;*************函数 vlxls-variant-》list.lsp  *************


;| 参数 Key：
    ActiveWindow.SelectedSheets.HPageBreaks.Add Before:=ActiveCell  插入分页符
    With ActiveSheet.PageSetup
        .PrintTitleRows = "$1:$3"  工作表顶端标题行
        .PrintTitleColumns = ""    工作表左端标题列
    End With
    ActiveSheet.PageSetup.PrintArea = "$C$1:$H$255"   工作表打印区域
    With ActiveSheet.PageSetup
        .LeftHeader = ""    左页眉
        .CenterHeader = ""  中页眉
        .RightHeader = ""   右页眉
        .LeftFooter = "&P"  左页脚
        .CenterFooter = "&N" 中页脚
        .RightFooter = "aaaaaaaaa"  右页脚
        .LeftMargin = Application.InchesToPoints(0.62)   左边距
        .RightMargin = Application.InchesToPoints(0.748031496062992) 右边距
        .TopMargin = Application.InchesToPoints(0.984251968503937)
        .BottomMargin = Application.InchesToPoints(0.393700787401575)
        .HeaderMargin = Application.InchesToPoints(0.511811023622047)
        .FooterMargin = Application.InchesToPoints(0.511811023622047)   
        .PrintHeadings = False
        .PrintGridlines = False
        .PrintComments = xlPrintNoComments
        .CenterHorizontally = False
        .CenterVertically = False
        .Orientation = xlPortrait
        .Draft = False
        .PaperSize = xlPaperA4
        .FirstPageNumber = xlAutomatic  打印起始页
        .Order = xlDownThenOver
        .BlackAndWhite = False
        .Zoom = 100
        .PrintErrors = xlPrintErrorsDisplayed
    End With
End Sub
|;


;|Copyright(C) 1994-2005 by KozMos Inc.

 

Permission to use, copy, modify, and distribute this software for any purpose and without fee is hereby
granted, provided that the above copyright notice appears in all copies and that both that copyright notice
and the limited warranty and restricted rights notice below appear in all supporting documentation.
KozMos PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS. KozMos SPECIFICALLY DISCLAIMS ANY IMPLIED
WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE. KozMos, INC. DOES NOT WARRANT THAT THE OPERATION
OF THE PROGRAM WILL BE UNINTERRUPTED OR ERROR FREE.

 

Public Function
 
Name
 (vlxls-variant->list VariantValue)
 
Usage
 Convert a variant into normal Visual LISP LIST data, nested Variant and safearray will also be converted.
 
Input
 VARIANT
 Input Variant
 
RetVal
 True
 LIST
 Valid Visual LISP variable value
 
Fail
 STR
 ""
 |;
(Defun vlxls-variant->list (VarX / Run Item Rtn)
  (setq Run T)
  (while
    Run
     (cond ((= (type VarX) 'SAFEARRAY)
           (setq VarX (vlax-safearray->list VarX))
          )
          ((= (type VarX) 'VARIANT)
           (if    (member (vlax-variant-type VarX) (list 5 4 3 2))
             (setq VarX (vlax-variant-change-type Varx vlax-vbString))
           )
           (setq VarX (vlax-variant-value VarX))
          )
          (t (setq Run nil))
     )
  )
  (cond  ((= (type VarX) 'LIST)
        (foreach Item VarX
          (setq Item (vlxls-variant->list Item)
               Rtn  (append Rtn (list Item))
          )
        )
       )
       ((= VarX nil) (setq Rtn ""))
       (t (setq Rtn VarX))
  )
  Rtn
)
;;;***************** 函数 vlxls-variant->list*****************


;;;*************函数 vlxls-get-property.lsp  *************
 
;|Examples:

(vlxls-rangevalue->safearray '(("A1" . "aaa")("B4" . "ccc"))) è#<safearray...>

(vlxls-variant->list (vlxls-rangevalue->safearray '(("A1" . "aaa")("B4" . "ccc"))))è(("aaa" "") ("" "") ("" "") ("" "ccc"))
 

 

Public Function
 
Name
 (vlxls-get-property TopVLAObject NestPropertyString)
 
Usage
 Get the property of a nested VLA-Object from the main top vla-object. Use same property indicator as VBA.
 
Input
 VLOBJ
 The Top vla-object
 
STRING
 The Property combination string, divided with ".", ordered from top to inner.
 
RetVal
 True
 ANY
 The value of the most nested property.
 
Fail
 BOOLEAN
 NIL
 |;
(Defun vlxls-get-property (top prop / vlstring->list item Rtn)

  (Defun vlstring->list (str st / lst e)

    (setq str (strcat str st))

    (while (vl-string-search st str)

      (setq

       lst

        (append lst (list (substr str 1 (vl-string-search st str))))

      )

      (setq

       str

        (substr str (+ (1+ (strlen st)) (vl-string-search st str)))

      )

    )

    (if    lst

      (mapcar '(lambda (e) (vl-string-trim " " e)) lst)

    )

  )

  (cond  ((= (type prop) 'sym)

        (setq Rtn (vlax-get-property top prop))

       )

       ((= (type prop) 'str)

        (if (null (vl-string-search "." prop))

          (setq Rtn (vlax-get-property top prop))

          (foreach item (vlstring->list prop ".")

            (if (null Rtn)

              (setq Rtn (vlax-get-property top item))

              (setq Rtn (vlax-get-property Rtn item))

            )

          )

        )

       )

  )

  (cond  ((= (type Rtn) 'variant)

        (setq Rtn (vlax-variant-value Rtn))

       )

       ((= (type Rtn) 'safearray)

        (setq Rtn (vlxls-variant->list Rtn))

       )

  )       

  Rtn

)
;;;***************** 函数 vlxls-get-property*****************


;;;*************函数 gxl-strparse.lsp  *************
 ;_ end of defun

;;;==================================================================
;;; (gxl-StrParse Str Delimiter) 将具有分隔符的字符串解析为列表
;;;------------------------------------------------------------------
;;; 参数:
;;;		Str			要解析的字符串
;;;		Delimiter	要搜索的分隔符
;;;------------------------------------------------------------------
;;;	返回:
;;;		一个字符串列表。
;;;		示例:
;;;		(setq a "Harp,Guiness,Black and Tan")
;;;		(gxl-StrParse a ",")
;;;		返回:
;;;		("Harp" "Guiness" "Black and Tan")
;;;------------------------------------------------------------------
;;; 相关主题: (StringToList) (gxl-StrParse "asd    asdasda   1" "  ")
;;;==================================================================
;|(defun gxl-StrParse	(Str Delimiter / SearchStr StringLen return n char nn)
	(setq SearchStr Str)
	(setq StringLen (strlen SearchStr) nn StringLen)
	(setq return '())


	(while (> StringLen 0)
		(setq n 1)
		(setq char (substr SearchStr 1 1))
		(while (and (/= char Delimiter) (<= n StringLen))
			(setq n (1+ n))
			(setq char (substr SearchStr n 1))
		) ;_ end of while
		(setq return (cons (substr SearchStr 1 (1- n)) return))
		(setq SearchStr (substr SearchStr (1+ n) StringLen))
		(setq StringLen (strlen SearchStr))
	) ;_ end of while
  
     (if (= " " Delimiter)
       (setq return (vl-remove  "" return))
       )
      (reverse return)
	;(reverse (vl-remove  "" return))
) ;_ end of defun
|;
;;str = "" ，返回 nil
(defun gxl-StrParse (str del / pos lst)
  (if (/= "" str)
    (progn
  (while (setq pos (vl-string-search del str))
    (setq lst (cons (substr str 1 pos) lst)
	  str (substr str (+ pos 1 (strlen del)))
    )
  )
  ;(vl-remove "" (reverse (cons str lst)))
  
  (if (= " " Del)
    (vl-remove "" (reverse (cons str lst)))
    (reverse (cons str lst))
  )
  )
    )
)
;;;***************** 函数 gxl-StrParse*****************


;;;*************函数 gxl-strparse1.lsp  *************
;;;"" 返回 '("")
(defun gxl-StrParse1 (str del / pos lst)
  (while (setq pos (vl-string-search del str))
    (setq lst (cons (substr str 1 pos) lst)
	  str (substr str (+ pos 1 (strlen del)))
    )
  )
  ;(vl-remove "" (reverse (cons str lst)))

  (if (= " " Del)
    (vl-remove "" (reverse (cons str lst)))
    (reverse (cons str lst))
  )
)
;;;***************** 函数 gxl-StrParse1*****************


;;;*************函数 gxl-strparsebylst.lsp  *************
;; | ----------------------------------------------------------------------------
;; | (gxl-StrParseByLst 字串 分隔符表)
;; | ----------------------------------------------------------------------------
;; | Function : Splits up a string into a list of tokens delimited by a list of
;; |            delimiters and returns all white spaces in between the dlimiters
;; | Argument : [lstr]     - The String
;; |            [DelimLst] - The list of possible delimiters
;; | Return   : A list of delimited strings
;; | Updated  : January 29, 1999
;; | e-mail   : rakesh.rao@4d-technologies.com 
;; | Web      : www.4d-technologies.com
;; | ----------------------------------------------------------------------------
;;;(gxl-StrParseByLst "asdas;asdsad,asdasd.sad" '(";" "," ".")) 返回 '("asdas" "asdsad" "asdasd" "sad")
;|(defun gxl-StrParseByLst ( lstr DelimLst / cnt len Lst str Char LastWasWhite )
(setq
	len           (strlen lstr)
	cnt           1
	Lst          '()
	LastWasWhite nil
	str          ""
)

(repeat len
	(setq
		Char (substr lstr cnt 1)
		cnt (1+ cnt)
	)

	(if (not (member Char DelimLst))
	(setq str (strcat str Char))
	  (if (/= str "")
	(setq
		LastWasWhite T
		Lst          (append Lst (list str))
		str          ""
	)
	    (setq
		LastWasWhite T
	)
	    )
	  )
)
(if (not (equal str ""))
	(setq Lst (append Lst (list str)))
)
 Lst
)|;
(defun gxl-StrParseByLst ( lstr DelimLst )
  (setq lstr (list lstr))
  (foreach del DelimLst
    (setq lstr (apply 'append (mapcar '(lambda (x) (gxl-STRPARSE1 x del)) lstr)))
    )
  (if (member " " DelimLst)
    (vl-remove "" lstr)
  lstr
    )
)
;;;***************** 函数 gxl-StrParseByLst*****************


;;;*************函数 gxl-sys-progress-init.lsp  *************
;;;================================================================================================
;;; 进程条初始化 (gxl-Sys-Progress-Init 提示 进程总数)
;;; 进程步进 (gxl-Sys-Progress 进程总数 -1)
;;; 进程结束 (gxl-Sys-Progress-Done)
(setq *ProgressID* 0
      *ProgressPrompt* ""
      *ProgressBFB* "  0%")

(defun gxl-Sys-Progress-Init (str to)
    (if *FlagINIT* (alert "上一次进程条没有结束！"))
    (setq *ProgressID* 0
	  *ProgressTo* to
	  *ProgressPrompt* str
	  *ProgressBFB* 2
	  *FlagINIT* T)
    )
;;;***************** 函数 gxl-Sys-Progress-Init*****************


;;;*************函数 gxl-str-space.lsp  *************
;|(defun c:tt (/ oldos oldfill flag strTxt entext pt pt1)
  (setq oldos (getvar "osmode"))
  (setq oldfill (getvar "fillmode"))
  (setvar "osmode" 0)
  (setvar "fillmode" 1)
  (setvar "cmdecho" 0)

  (setq flag t)
  (while flag
    (setq entext (entsel "\n请选择源文字："))
    (if	(= "TEXT" (cdr (assoc 0 (entget (car entext)))))
      (setq flag nil)
    ) ;_ 结束if
  ) ;_ 结束while
  (while (not (setq zg (getreal "\n输入字高："))))
  (setq	strTxt (cdr (assoc 1 (entget (car entext))))
	pt     (cdr (assoc 10 (entget (car entext))))
  ) ;_ 结束setq

  (while (setq pt1 (getpoint pt "\n要复制的位置："))
    (setq strTxt (gxl-StrNumNext strTxt 1))
    (command "text" pt1 1 0 strTxt)
  ) ;_ 结束while
  (setvar "osmode" oldos)
  (setvar "fillmode" oldfill)

) ;_ 结束defun

|;
		      

;;; gxl-Str-Space 制造空格字串
(defun gxl-Str-Space (n / zf)
  (if (< n 1)
   (setq zf "")
    (progn
  (setq zf "")
  
  (repeat n
    (setq zf (strcat " " zf))
    )
  );progn
    );if
  )
;;;***************** 函数 gxl-Str-Space*****************


;;;*************函数 gxl-sys-progress.lsp  *************

;;;进程条函数,to 为进程总数，i为已到达进程数
;;;第一次使用 i应为1，以后 i = 负数 为步进数，也可以为已到达进程数
;|(defun gxl-Sys-Progress (to i / CS_TEXT MYI bfb corstate LL)
					;(setq cs_text ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
					;(setq corstate (getvar "coords"))
					;(setvar "coords" 0)
					;(setq cs_text "||||||||||||||||||||||||||||||"
					;	LL (strlen cs_text)
  (if (not *FlagINIT*) (gxl-Sys-Progress-Init "" 100))
  (if (and *FlagINIT* *ProgressTo*)
    (setq to *ProgressTo*)
  )
  (setq	cs_text	"████████████████████"
	LL	(strlen cs_text)
  )
  (if (<= i 0)
    (setq i (- *ProgressID* i)
	  *ProgressID* i
    )
    (setq *ProgressID* i)
  )
  (if (> i to)
    (setq i to)
  )
  (setq	myi (fix (/ (* (strlen cs_text) i) to))
	myi (* 2 (/ myi 2))
  )
  (if (= 0 myi)
    (setq myi 2)
  )
  (if (/= *ProgressBFB* myi)
    (progn
      (setq
	cs_text	(substr cs_text 1 myi)
	cs_text	(strcat cs_text (gxl-Str-Space (- LL myi)))
      )

      (setq bfb (fix (* 100 i (/ 1.0 to))))
      (setq bfb (itoa bfb))
      (cond
	((= 1 (strlen bfb))
	 (setq bfb (strcat "  " bfb "% "))
	)
	((= 2 (strlen bfb)) (setq bfb (strcat " " bfb "% ")))
	((= 3 (strlen bfb)) (setq bfb (strcat bfb "% ")))
      )
					;(grtext -1 (strcat "已完成" cs_text bfb))
      (setvar "modemacro"
	      (strcat *ProgressPrompt*
		      "已完成"
		      cs_text
		      bfb
	      )
      )
      (setq *ProgressBFB* myi)

    )
    (if	(= 2 myi)
      (progn
	(setvar	"modemacro"
		(strcat	*ProgressPrompt*
			"已完成"
			"|                                        "
			"1%"
		)
	)
      )					;progn
    )					;if
  )
					;(setvar "coords" corstate)
)|;
(defun gxl-Sys-Progress	(to i / CS_TEXT MYI bfb corstate LL)
					;(setq cs_text ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
					;(setq corstate (getvar "coords"))
					;(setvar "coords" 0)
					;(setq cs_text "||||||||||||||||||||||||||||||"
					;	LL (strlen cs_text)
  (if (not *FlagINIT*)
    (gxl-Sys-Progress-Init "" 100)
  )
  (if (and *FlagINIT* *ProgressTo*)
    (setq to *ProgressTo*)
  )
  (setq	cs_text	"████████████████████"
	LL	(strlen cs_text)
  )
  (if (<= i 0)
    (setq i	       (- *ProgressID* i)
	  *ProgressID* i
    )
    (setq *ProgressID* i)
  )
  (if (> i to)
    (setq i to)
  )
  (setq	myi (fix (/ (* (strlen cs_text) i) to))
	myi (* 2 (/ myi 2))
  )
  (if (= 0 myi)
    (setq myi 2)
  )

  (setq bfb (fix (* 100 i (/ 1.0 to))))
  (if (/= *ProgressBFB* bfb)
    (progn
      (setq *ProgressBFB* bfb)
      (setq
	cs_text	(substr cs_text 1 myi)
	cs_text	(strcat cs_text (gxl-Str-Space (- LL myi)))
      )


      (setq bfb (itoa bfb))
      (cond
	((= 1 (strlen bfb))
	 (setq bfb (strcat "  " bfb "% "))
	)
	((= 2 (strlen bfb)) (setq bfb (strcat " " bfb "% ")))
	((= 3 (strlen bfb)) (setq bfb (strcat bfb "% ")))
      )
      (setvar "modemacro"
	      (strcat *ProgressPrompt*
		      "已完成"
		      cs_text
		      bfb
	      )
      )

    )
  )


)
;;;***************** 函数 gxl-Sys-Progress*****************


;;;*************函数 vlxls-rangeid.lsp  *************
;| 
Examples:

(vlxls-cellid '(3 14)) è ("C14" "")

(vlxls-cellid "23") è ("D23" "")

(vlxls-cellid "C12:F3") è ("C3" "F12")

(vlxls-cellid "F15:G22") è ("F15" "G22")
 

 

Excel Cell and Range Progress Function
 
Name
 (vlxls-rangeid CellIDStringOrList)
 
Usage
 VLXLS treats Excel Cell ID in two types: AutoCAD LIST and Excel simple Cell ID String. This function is used to convert Cell ID between the two types.
 
Input
 STR/LIST
 The Cell ID list or string
 
RetVal
 True
 STR/LIST
 Cell ID value in another VLXLS ID type
 
Fail
 BOOLEAN
 NIL
 
 |;
;;(vlxls-rangeid "A1")
(Defun vlxls-rangeid (id / str->list list->str xid->str Rtn)

  (Defun str->list (str / ii xk xv rr pos x y)

    (setq rr (strlen str))

    (foreach ii     '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9")

      (if (setq pos (vl-string-search ii str))

       (setq rr (min pos rr))

      )

    )

    (setq x (substr str 1 rr)

         y (substr str (1+ rr))

    )

    (if    (= (strlen x) 2)

      (setq xk (- (ascii (substr x 1 1)) 64)

           xv (- (ascii (substr x 2)) 64)

      )

      (setq xk 0

           xv (- (ascii x) 64)

      )

    )

    (list (+ (* xk 26) xv) (read y))

  )

  (Defun xid->str (IntNum / PosNum Nm-One)

    (setq Nm-One (1- IntNum)

         PosNum (/ Nm-One 26)

    )

    (if    (= PosNum 0)

      (chr (+ 65 (rem Nm-One 26)))

      (strcat (chr (+ 64 PosNum)) (chr (+ 65 (rem Nm-One 26))))

    )

  )

  (Defun list->str (idr / x y)

    (setq x (car idr)

         y (cadr idr)

         x (xid->str x)

         y (itoa y)

    )

    (strcat x y)

  )

  (cond  ((= (type id) 'str) (setq Rtn (str->list id)))

       ((= (type id) 'list) (setq Rtn (list->str id)))

  )

  Rtn

)
;;;***************** 函数 vlxls-rangeid*****************


;;;*************函数 vlxls-color-eci-》aci.lsp  *************
 
;|Examples:

(vlxls-color-eci->truecolor 0) è16711935

(vlxls-color-eci->truecolor 1)è 0

(vlxls-color-eci->truecolor 12)è 8355584

(vlxls-color-eci->truecolor 120) è16711935
 

 

Color Transfer Function
 
Name
 (vlxls-color-eci->aci ExcelColorIndexNumber)
 
Usage
 Convert Excel ColorIndex number into most matched AutoCAD ACI Integer number.
 
Input
 INT
 Excel ColorIndex integer (0 to 56)
 
RetVal
 True
 INT
 Valid AutoCAD ACI Integer number (0 to 256)
 (vlxls-color-eci->aci 249)
Fail
 INT
 256 for BYLAYER
 |;
(Defun vlxls-color-eci->aci (Color / Rtn)

(if (null (setq Rtn (cdr (assoc Color *xls-color*))))

  (setq Rtn 256)

    (setq Rtn (nth 0 Rtn))

  )

  Rtn

)
;;;***************** 函数 vlxls-color-eci->aci*****************


;;;*************函数 vlxls-color-eci-》truecolor.lsp  *************
;|Examples:

NONE
 

 

Color Transfer Function
 
Name
 (vlxls-color-eci->truecolor ExcelColorIndexNumber)
 
Usage
 Convert Excel ColorIndex number into most matched AutoCAD2004+ truecolor number (stored by DXF420).
 
Input
 INT
 Excel ColorIndex integer (0 to 56)
 (vlxls-color-ECI->truecolor 28)
RetVal
 True
 INT
 Valid AutoCAD 2004+ truecolor number
 
Fail
 INT
 16711935 for None|;
 
(Defun vlxls-color-ECI->truecolor (Color / Rtn)

  (if (setq Rtn (cdr (assoc Color *xls-color*)))

    (setq Rtn (nth 1 Rtn))

  )

  (if (null Rtn)

    (setq Rtn 16711935)

  )

  Rtn

)
;;;***************** 函数 vlxls-color-ECI->truecolor*****************


;;;*************函数 gxl-catchapply.lsp  *************
;;;(gxl-CatchApply fun args) 重定义 VL-CATCH-ALL-APPLY ,如函数错误返回nil
;;;(gxl-CatchApply vla-offset (list (vlax-ename->vla-object (car(entsel))) 10)) 
(defun gxl-CatchApply ( fun args / result )
  ;; ?Lee Mac 2010
  (if
    (not
      (vl-catch-all-error-p
        (setq result
          (vl-catch-all-apply (if (= 'SYM (type fun)) fun (function fun)) args)
        )
      )
    )
    result
  )
)
;;;***************** 函数 gxl-CatchApply*****************


;;;*************函数 gxl-str-》singleonly.lsp  *************
;;;(gxl-str->singleonly str) 中英文拆分为单独文字
;;;(gxl-str->singleonly "12我 的\n顾晓林")
(defun gxl-str->singleonly (str / strlst strlst1 hz_str e_str )
  (setq	strlst	(vl-string->list str)
	strlst1	'()
  ) ;_ setq
  (while strlst
    (cond
      ((and (not hz_str)
	    (> (car strlst) 159)
	    )
       (setq e_str nil)
       (setq hz_str (list (car strlst))) 
       (setq strlst (cdr strlst))
        ;_ if
      )
      ((and hz_str
	    (> (car strlst) 159)
	    )
       (setq e_str nil)
       (setq hz_str (append hz_str(list  (car strlst)))) 
       (setq strlst (cdr strlst))
       (setq strlst1 (append strlst1 (list hz_str))
	     hz_str nil)
        ;_ if
      )
      ((< (car strlst) 159)
       (setq hz_str nil)
        ;_ if
       (if strlst1
	   (setq strlst1 (append strlst1 (list (list (car strlst)))))
	   (setq strlst1 (list (list (car strlst))))
	 )
       (setq strlst (cdr strlst))
      )
    ) ;_ cond
  ) ;_ while
  ;(mapcar 'vl-list->string (mapcar 'reverse strlst1))
  
  (mapcar 'vl-list->string strlst1)
)
;;;***************** 函数 gxl-str->singleonly*****************


;;;*************函数 vlxls-cellid.lsp  *************
;| 
Examples:

(vlxls-sheet-get-usedrange *xlapp* "Sheet1") è T

(vlxls-sheet- get-usedrange *xlapp* "NewSheet") è T
 

 

Excel Cell and Range Progress Function
 
Name
 (vlxls-cellid CellIDStringOrList)
 
Usage
 Divide complex Excel Cell ID into a two-string-item list, contain the Left-Upper and Right-Lower Cell ID.
If only one Cell ID is provided, set the Right-Lower Cell ID to "".
 
Input
 STR/LIST
 Complex Excel Cell ID string or simple Cell ID string/list.
 
RetVal
 True
 LIST
 List of Left-Upper and Right-Lower Cell ID
 
Fail
 BOOLEAN
 NIL
 |;
(Defun vlxls-cellid (id / xx id1 id2 Rtn)

  (if (= (type id) 'list)

    (setq id (vlxls-rangeid id))

  )

  (setq id (strcase id))

  (if (null (setq xx (vl-string-search ":" id)))

    (setq Rtn (list id ""))

    (setq id1 (substr id 1 xx)

         id2 (substr id (+ xx 2))

         id1 (vlxls-rangeid id1)

         id2 (vlxls-rangeid id2)

         Rtn (list (vlxls-rangeid

                    (list (min (car id1) (car id2))

                         (min (cadr id1) (cadr id2))

                    )

                  )

                  (vlxls-rangeid

                    (list (max (car id1) (car id2))

                         (max (cadr id1) (cadr id2))

                    )

                  )

             )

    )

  )

  Rtn

)
;;;***************** 函数 vlxls-cellid*****************


;;;*************函数 vlxls-range-getid.lsp  *************
 
;|Examples:

(vlxls-cell-get-mergeid *xlapp* "C12:F14") è"B9:G19"

(vlxls-cell-get-mergeid *xlapp* "E14") è"A11:G19"
 

 

Excel Cell and Range Progress Function
 
Name
 (vlxls-range-getid RangeObject)
 
Usage
 Get the Left-Upper and Right-Lower Cell ID of a range object.
 
Input
 VLOBJ
 The Excel Range vla-object
 
RetVal
 True
 STRING
 A string contain Left-Upper and Right-Lower cells’ ID
 
Fail
 BOOLEAN
 NIL
 |;
(Defun vlxls-range-getID (range / col row dx dy)
  (setq   dx  (vlxls-get-property range "MergeArea.Rows.Count")
       dy  (vlxls-get-property range "MergeArea.Columns.Count")
       row (vlxls-get-property range "MergeArea.Row")
       col (vlxls-get-property range "MergeArea.Column")
  )
  (strcat (vlxls-rangeid (list col row))
         ":"
         (vlxls-rangeid (list (1- (+ col dy)) (1- (+ row dx))))
  )
)
;;;***************** 函数 vlxls-range-getID*****************


;;;*************函数 gxl-listp.lsp  *************
;;;(gxl-listp lst) 判断表是否为真正的表,非nil、非点对表
;;;(gxl-listp nil) nil (gxl-listp '(1 . 2)) (gxl-listp '(1  2))
(defun gxl-listp (lst)
  (and (vl-consp lst)
       (vl-list-length lst)
       )
  )
;;;***************** 函数 gxl-listp*****************


;;;*************函数 gxl-dxf.lsp  *************
;;;==================================================================
;;;(gxl-dxf ent i )取出图元索引i对应的值
;;;==================================================================
  ;|(defun gxl-dxf (ent i)
    (cond ((= (type ent) 'ename)
	    (cdr (assoc i (entget ent '("*"))))
	     )
	  ((= (type ent) 'list)
	   (cdr (assoc i ent))
	   )
    ) ;_ if
  )|;
(defun gxl-dxf	(ent i)
  (if (= (type ent) 'ename)
    (setq ent (entget ent '("*")))
    )
  (cond	((atom i)
	 (cdr (assoc i ent))
	 )
	((gxl-listp i)
	 (mapcar '(lambda (x) (cdr (assoc x ent))) i)
	 )
	)
  )
;;;***************** 函数 gxl-dxf*****************


;;;*************函数 gxl-ch_ent.lsp  *************
;;;==================================================================
;;;(gxl-CH_Ent ent i pt) 用新值pt更新图元ent索引i对应的值
;;;==================================================================

(defun gxl-CH_Ent (ent i pt / en)
  (if (assoc i (setq en (entget ent)))
    (setq en (subst (cons i pt) (assoc i en) en))
    (setq en (append en (list (cons i pt))))
    )
    (entmod en)
  (entupd ent)
  )
;;;***************** 函数 gxl-CH_Ent*****************


;;;*************函数 gxl-regexsearch.lsp  *************
;;(gxl-RegExSearch string Express key) 正则表达式搜索字串
;;参数 string = 字串
;;     Express = 正则表达式
;;     key = 字母 i I m M g G的组合字串
;;          i/I = 忽略大小写 m/M = 多行搜索 g/G = 全文搜索

(defun gxl-RegExSearch (STRING EXPRESS KEY / REGEX S POS LEN STR L)
  (setq RegEx (vlax-create-object "Vbscript.RegExp"))
  (if (and key (WCMATCH key "*g*,*G*"))
    (vlax-put-property regex "Global" 1)
    (vlax-put-property regex "Global" 0)
  )
  (if (and key (WCMATCH key "*i*,*I*"))
    (vlax-put-property regex "IgnoreCase" 1)
    (vlax-put-property regex "IgnoreCase" 0)
  )
  (if (and key (WCMATCH key "*m*,*M*"))
    (vlax-put-property regex "Multiline" 1)
    (vlax-put-property regex "Multiline" 0)
  )
  (vlax-put-property regex "Pattern" ExPress)
  (setq s (vlax-invoke regex 'Execute string))
  (vlax-for o s
    (setq pos (vlax-get-property o "FirstIndex")
	  len (vlax-get-property o "Length")
	  str (vlax-get-property o "value")
    )
    (setq l (cons (list pos len str) l))
    ;|(princ (strcat "\n文字位置 = "
		   (itoa pos)
		   "  文字长度 = "
		   (itoa len)
		   "  值 = \""
		   str
		   "\""
	   )
    )|;
  )
  (vlax-release-object RegEx)
  (reverse l)
)
;;;***************** 函数 gxl-RegExSearch*****************


;;;*************函数 gxl-regexreplace.lsp  *************
;;(gxl-RegExSearch string Express key) 正则表达式替换字串
;;参数 string = 字串
;;     Newstr = 替换的字串
;;     Express = 正则表达式
;;     key = 字母 i I m M g G的组合字串
;;          i/I = 忽略大小写 m/M = 多行搜索 g/G = 全文搜索
;;(gxl-RegExRePlace "ASasd32424\r\nasaf" "搜索" "as" "mg")
(defun gxl-RegExRePlace (STRING NEWSTR EXPRESS KEY / REGEX S)
  (setq RegEx (vlax-create-object "Vbscript.RegExp"))
  (if (and key (WCMATCH key "*g*,*G*"))
    (vlax-put-property regex "Global" 1)
    (vlax-put-property regex "Global" 0)
  )
  (if (and key (WCMATCH key "*i*,*I*"))
    (vlax-put-property regex "IgnoreCase" 1)
    (vlax-put-property regex "IgnoreCase" 0)
  )
  (if (and key (WCMATCH key "*m*,*M*"))
    (vlax-put-property regex "Multiline" 1)
    (vlax-put-property regex "Multiline" 0)
  )
  (vlax-put-property regex "Pattern" ExPress)
  (setq s (vlax-invoke regex 'RePlace string Newstr))
  (vlax-release-object RegEx)
  s
)
;;;***************** 函数 gxl-RegExRePlace*****************


;;;*************函数 gxl-str-padleft.lsp  *************
 ;_ defun
;;;(gxl-Str-PadLeft Str len char) 参数：字串 长度 填充字符  功能：字符串以len长度在前面填充字符
(defun gxl-Str-PadLeft (Str len char / _len Str1 diff)
  (setq
    _len (strlen Str)
    diff (- len _len)
    Str1 ""
  ) ;_ setq
  (if (>= diff 0)
    (repeat diff
      (setq Str1 (strcat Str1 char))
    ) ;_ repeat
  ) ;_ if
    (setq Str (strcat Str1 Str))
)
;;;***************** 函数 gxl-Str-PadLeft*****************


;;;*************函数 gxl-rgb-》aci.lsp  *************
;;(gxl-HSL->RGB h s l) HSL -> RGB - Lee Mac 2011
;;;*AcadAcCmColor* 颜色对象
(setq *AcadAcCmColor* (gxl-CatchApply vla-getinterfaceobject (list (vlax-get-acad-object) (strcat "AutoCAD.AcCmColor." (substr (getvar "acadver") 1 2)))))

;;;(gxl-RGB->ACI R G B) RGB转AutoCAD 256色颜色索引
;;;(gxl-RGB->ACI 204 0 255)
(defun gxl-RGB->ACI (R G B)
  (vla-setrgb *AcadAcCmColor* R G B)
  (vla-get-ColorIndex *AcadAcCmColor*)
  )
;;;***************** 函数 gxl-RGB->ACI*****************


;;;*************函数 gxl-num-base-》decimal.lsp  *************


;;;==================================================================
;;;gxl-Num-Base->Decimal 将一个字符串按BASE的做为基数的进制转换为十进制的整数值。
;|功能
将一个字符串按BASE的做为基数的进制转换为十进制的整数值。
参数
base:一个代表所要转换的进制(BASE2、BASE8等)基数整数。
val:一个进行转换的字符串。
返回值
十进制的整数值。
示例 (gxl-Num-Base->Decimal 2 "0011011")
(gxl-Num-Base->Decimal 16 "71063301")(gxl-Num-Base->Decimal 16 "1234567890abcdef")
|;

;;;==================================================================
(defun gxl-Num-Base->Decimal (base val / pos power result tmp)
  (setq	pos    (1+ (strlen val))
	power  -1
	result 0
	val    (strcase val)
  )
  (while (> (setq pos (1- pos)) 0)
    (setq
      result (+	result
		(* (if (> (setq tmp (ascii (substr val pos 1))) 64)
		     (- tmp 55)
		     (- tmp 48)
		   )
		   (expt base (setq power (1+ power)))
		)
	     )
    )
  )
  result
)
;;;***************** 函数 gxl-Num-Base->Decimal*****************


;;;*************函数 gxl-hex-》aci.lsp  *************
;;;(gxl-Hex->ACI Hex) 16位颜色转AutoCAD 256色颜色索引
;;;(gxl-Hex->ACI "#CC00FF") 
(defun gxl-Hex->ACI (Hex)
  (setq Hex (gxl-Str-PadLeft (VL-STRING-Left-TRIM "#" Hex) 6 "0"))
  (apply
    'gxl-RGB->ACI
    (list (gxl-Num-Base->Decimal 16 (substr Hex 1 2))
	  (gxl-Num-Base->Decimal 16 (substr Hex 3 2))
	  (gxl-Num-Base->Decimal 16 (substr Hex 5 2))
    )
  )
)
;;;***************** 函数 gxl-Hex->ACI*****************


;;;*************函数 gxl-item.lsp  *************
;;;(gxl-Item coll item ) 重定义 Item
;;(gxl-Item (vla-get-blocks (vla-get-ActiveDocument (vlax-get-acad-object))) "001")
(defun gxl-Item  ( coll item )
  (if
    (not
      (vl-catch-all-error-p
        (setq item
          (vl-catch-all-apply
            (function vla-item) (list coll item)
          )
        )
      )
    )
    item
  )
)
;;;***************** 函数 gxl-Item*****************


;;;*************函数 gxl-sys-progress-done.lsp  *************
(defun gxl-Sys-Progress-Done ()
    (setq *ProgressID* 0
	  *ProgressTo* nil
	  *ProgressPrompt* ""
	  *ProgressBFB* 2
	  *FlagINIT* nil)
    (setvar "modemacro" "")
    )
;;;***************** 函数 gxl-Sys-Progress-Done*****************


;;;*************函数 gxl-reucs.lsp  *************
 ;_ defun


;;;(gxl-ReUcs) 恢复ucs坐标系
(defun gxl-ReUcs (/ objucs)
  (if
    (setq objucs (gxl-CATCHAPPLY vla-item (list (vla-get-UserCoordinateSystems *AcDocument*) "OldUCS")))
    (vla-put-ActiveUCS *AcDocument* objucs)
    ;(command "_.ucs" "w")
    )
  (princ)
  )
;;;***************** 函数 gxl-ReUcs*****************


;;;*************函数 reerr.lsp  *************
;;;(ReErr)
(defun ReErr ()
    ;;;恢复ucs坐标系
  (gxl-REUCS)
  (if (or (= 'LIST (type *error*))
	  (= 'SUBR (type *error*))
	  (= 'USUBR (type *error*))
      )
    (PROGN
      ;|(MAPCAR (function
		(lambda (a b) (VL-CATCH-ALL-APPLY 'setvar (list a b)))
	      )
	      *SYSVARNL*
	      (REVERSE *SVARL*)
      )|;
      (foreach a
      (apply 'mapcar (list 'list *SYSVARNL* (REVERSE *SVARL*)))
	
	(setq bb a aa (VL-CATCH-ALL-APPLY 'setvar a))
	)
      (SETQ *Error* MyOld*error*)

    )
    (ALERT "ERROR  : NO (SETIERR)!")
  )
  (if (= 8 (LOGAND (getvar "undoctl") 8)) (vla-endUndoMark *ACDOCUMENT*))
    ;;;恢复图层状态
  ;(gxl-restoreslayers)
;;;释放内存
  (gc)
 (PRINC)
   )
;;;***************** 函数 ReErr*****************


;;;*************函数 gxl-pt-》3d.lsp  *************
;;; (gxl-pt->3d p) 无条件转换为3维点，
;|(gxl-pt->3d p)
参数
p
点或数
返回值
点
示例
命令: (gxl-pt->3d '(2. 2.))
(2.0 2.0)
命令: (gxl-pt->3d '(5))
(5.0 0.0)
命令: (gxl-pt->3d 6)
(6.0 0.0 )
命令: (gxl-pt->3d '(6 7. 3. 4. ))
(6.0 7.0)
|;
(defun gxl-pt->3d (p)
  (cond ((= 'LIST (type p))
	 (if (= 1 (length p))
	   (list (if (= 'REAL (type (car p))) (car p) (atof (itoa (car p))))  0.0 0.0)
	   (if (= 2 (length p))
	   (list (if (= 'REAL (type (car p))) (car p) (atof (itoa (car p))))
		 (if (= 'REAL (type (cadr p))) (cadr p) (atof (itoa (cadr p))))
		 0.0
		 )
	     (list (if (= 'REAL (type (car p))) (car p) (atof (itoa (car p))))
		 (if (= 'REAL (type (cadr p))) (cadr p) (atof (itoa (cadr p))))
		 (if (= 'REAL (type (caddr p))) (caddr p) (atof (itoa (caddr p))))
		 )
	     )
	   )
	 )
	((= 'REAL (type p))
	 (list p 0.0 0.0)
	 )
	((= 'INT (type p))
	 (list (atof (itoa p)) 0.0 0.0)
	 )
	(t nil)
	)
  )
;;;***************** 函数 gxl-pt->3d*****************


;;;*************函数 gxl-pt-》shift.lsp  *************
;;;(gxl-pt->shift pt @pt) 点相对位移 ,参数：点 相对位移距离点 返回三维点 (gxl-pt->shift '(0 0 ) 1 )
(defun gxl-pt->shift(pt @pt)
(setq pt (gxl-pt->3d pt)
      @pt (gxl-pt->3d @pt))
  (apply 'mapcar (cons '+ (list pt @pt)))
)
;;;***************** 函数 gxl-pt->shift*****************


;;;*************函数 gxl-setwcs.lsp  *************
;;;(gxl-SetWcs) 保存坐标系并设置wcs坐标系
;(gxl-SetWcs)
;(vla-Item (vla-get-UserCoordinateSystems *ACDOCUMENT*)0)
;(vla-add (vla-get-UserCoordinateSystems *ACDOCUMENT*) (vlax-3d-point '(0 0 0)) (vlax-3d-point '(0 0 0)) (vlax-3d-point '(0 0 0)) "old")
(defun gxl-SetWcs (/ objucs ucsorg ucsxdir ucsydir)
  (if (= 0 (getvar "worlducs"))
    (progn
      
      (setq ucsorg (getvar "ucsorg")
	    ucsxdir (gxl-PT->SHIFT ucsorg (getvar "ucsxdir"))
	    ucsydir (gxl-PT->SHIFT ucsorg (getvar "ucsydir"))
	    )
      (setq objucs (VL-CATCH-ALL-APPLY 'vla-item (list (vla-get-UserCoordinateSystems *AcDocument*) "OldUCS")))
      (vl-cmdf "_.ucs" "")
      (if (not (VL-CATCH-ALL-ERROR-P objucs))
	(vla-delete objucs)
	)
      (VL-CATCH-ALL-APPLY 'vla-add
	(list
	 (vla-get-UserCoordinateSystems *AcDocument*)
	      (vlax-3d-point ucsorg)
	      (vlax-3d-point ucsxdir)
	      (vlax-3d-point ucsydir)
	      "OldUCS"
	 )
      ) ;_ vla-add
      
    ) ;_ progn
    (progn
      (setq objucs (VL-CATCH-ALL-APPLY 'vla-item (list (vla-get-UserCoordinateSystems *AcDocument*) "OldUCS")))
      (if (not (VL-CATCH-ALL-ERROR-P objucs))
	(VL-CATCH-ALL-APPLY 'vla-delete (list objucs))
	)
      )
  ) ;_ if
  (princ)
)
;;;***************** 函数 gxl-SetWcs*****************


;;;*************函数 setierr.lsp  *************
;;;(SetIErr) DEFUN ERROR FUNCTION
(DEFUN SetIErr (/ sv 0lay)
    ;;;取消选择集
 ; (sssetfirst nil)
;(if (not (gxl-Data-DictGetData "初始化环境" "初始化环境")) (c:Gxl_InitEstate))
  ;;;保存图层状态
  ;(gxl-storeslayers)
  (vla-put-lock (gxl-item (vla-get-layers *ACDOCUMENT*) "0") :vlax-false)
  (if (or (= 'LIST (type *Error*))(= 'USUBR (type *Error*)))
	 (alert "ERROR  :THE LAST (SETiERR) FUNCTION HAS NO (ReErr)!")
	(PROGN
	  (SETQ *SVARL* '())
	  (FOREACH SV *SYSVARNL*
	    (SETQ *SVARL* (CONS (GETVAR SV) *SVARL*))
	    )
	  (FOREACH SV '("ATTDIA" "ATTREQ" "BLIPMODE" "CMDECHO" "DIMZIN"
			"ORTHOMODE" "MIRRTEXT")
	    (SETVAR SV 0)
	    )
	  ;(command "_undo" "_BE")
	  (vla-StartUndoMark *ACDOCUMENT*)
	  (SETVAR "EXPERT" 5)
         (SETVAR "CECOLOR" "BYLAYER")
         (SETVAR "celtype" "BYLAYER")
	  (SETVAR "LWDISPLAY" 1)
	  (SETVAR "CELTSCALE" 1)
	  (SETVAR "PLINEGEN" 1)
	  
        ; (if SetScale () (InitMap))
	  (setq MyOld*error* *error*)
	  (defun *error* (st)
	    (while (/= (getvar 'CMDACTIVE) 0)
	      (command "")
	    )
	    (gxl-sys-progress-done)
	    (if	ErrSel
	      (command "erase" ErrSel "")
	    )
	    (vla-endUndoMark *ACDOCUMENT*)
	    (reerr)
	    (princ st)
	  )
	  )
	 )
     ;;;保存坐标系并设置wcs坐标系
           (gxl-SetWcs)
  (setq *Model-Space* (vlax-get-property *AcDocument* (if (= 1 (getvar 'CVPORT)) 'PaperSpace 'ModelSpace)))
    )
;;;***************** 函数 SetIErr*****************


;;;*************函数 gxl-table.lsp  *************
;;;==================================================================
;;;gxl-table 返回包含在指定符号表中的所有元素
;|功能
返回包含在指定符号表中的所有元素
参数
一个符号表名称
示例
(gxl-table "ltype") 
|;
;;;==================================================================
(defun gxl-table (s / d r)
  (while (setq d (tblnext s (null d)))
    (setq r (cons (cdr (assoc 2 d)) r))
  )
  (reverse r)
)
;;;***************** 函数 gxl-table*****************


;;;*************函数 vlxls-app-init.lsp  *************
 
;|Examples:

(vlxls-color-aci-> truecolor 0) è 16711935 

(vlxls-color-aci->truecolor 1)è 16711680

(vlxls-color-aci-> truecolor 12)è 16711935

(vlxls-color-aci-> truecolor 120) è 16711935
 

 

Excel Application Session Progress Function
 
Name
 x2c 
 
Usage
 Import Microsoft Excel Type Library, set prefix of "msxl-" for all of the :methods-prefix; :properties-prefix
 & :constants-prefix. This function can detect Excel’s installation path automatically from Windows registry so
 that it can run smoothly on any language platform of Windows and Office.
 
Input
 NONE
 No Arguments
 
RetVal
 True
 BOOLEAN
 msxlc-xl24HourClock
 
Fail
 BOOLEAN
 NIL
 |;
(Defun vlxls-app-Init

       (/ OSVar GGG Olb8 Olb9 Olb10 TLB Out msg msg1 msg2)
  (if *Chinese*
    (setq msg  "\n 初始化微软Excel "
	  msg1 "\042初始化Excel错误\042"
	  msg2 (strcat
		 "\042 警告"
		 "\n ===="
		 "\n 无法在您的计算机上检测到微软Excel软件"
		 "\n 如果您确认已经安装Excel, 请发送电子邮"
		 "\n 件到kozmosovia@hotmail.com获取更多的解决方案\042")
	  )
    (setq msg  "\n Initializing Microsoft Excel "
	  msg1 "\042Initialization Error\042"
	  msg2 (strcat
		 "\042 WARNING"	"\n ======="
		 "\n Can NOT detect Excel97/200X/XP in your computer"
		 "\n If you already have Excel installed, please email"
		 "\n us to get more solution via GuXiaolin@hxch.com.cn\042")
	  )
    )
  (if (null msxlc-xl24HourClock)
    (progn
      (if (and (setq GGG
		      (vl-registry-read
			"HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\Excel.EXE"
			"Path"
			)
		     )
	       (setq GGG (strcase (strcat GGG "Excel.EXE")))
	       )
	(progn
	  (foreach OSVar  (list	"SYSTEMROOT"	  "WINDIR"
				"WINBOOTDIR"	  "SYSTEMDRIVE"
				"USERNAME"	  "COMPUTERNAME"
				"HOMEDRIVE"	  "HOMEPATH"
				"PROGRAMFILES")
	    (if	(vl-string-search (strcat "%" OSVar "%") GGG)
	      (setq GGG	(vl-string-subst
			  (strcase (getenv OSVar))
			  (strcat "%" OSVar "%")
			  GGG
			  )
		    )
	      )
	    )
	  (setq	Olb8  (findfile
			(vl-string-subst "EXCEL8.OLB" "EXCEL.EXE" GGG))
		Olb9  (findfile
			(vl-string-subst "EXCEL9.OLB" "EXCEL.EXE" GGG))
		Olb10 (findfile	(vl-string-subst
				  "EXCEL10.OLB"
				  "EXCEL.EXE"
				  GGG))
		)
	  (cond
	    ((= (vl-filename-base (vl-filename-directory GGG))
		    "OFFICE15"
		 )
		 (setq TLB GGG
		       Out "2013"
		 )
		)
	    ((= (vl-filename-base (vl-filename-directory GGG))
		    "OFFICE14"
		 )
		 (setq TLB GGG
		       Out "2010"
		 )
		)
	    ((= (vl-filename-base (vl-filename-directory GGG))
		    "OFFICE12"
		 )
		 (setq TLB GGG
		       Out "2007"
		 )
		)
	    ((=	(vl-filename-base (vl-filename-directory GGG))
		"OFFICE11"
		)
	     (setq TLB GGG
		   Out "2003"
		   )
	     )
	    ((=	(vl-filename-base (vl-filename-directory GGG))
		"OFFICE10"
		)
	     (setq TLB GGG
		   Out "XP"
		   )
	     )
	    (Olb9
	     (setq TLB Olb9
		   Out "2000"
		   )
	     )
	    (Olb8
	     (setq TLB Olb8
		   Out "97"
		   )
	     )
	    (t (setq TLB GGG Out "Version Unknown"))
	    )
	  (if TLB
	    (progn
	      (princ (strcat MSG Out "..."))
	      (vlax-import-type-library
		:tlb-filename	   TLB
		:methods-prefix	   "msxl-"
		:properties-prefix "msxlp-"
		:constants-prefix  "msxlc-")
	      )
	    )
	  )
	(progn
	  ;|(if vldcl-msgbox
	    (vldcl-msgbox "x" msg1 msg2)
	    (alert (read msg2))
	    )|;
	  (alert msg2)
	  (exit)
	  )
	)
      )
    )
  msxlc-xl24HourClock
  )
;;;***************** 函数 vlxls-app-Init*****************


;;;*************函数 gxl-sel-entnextall.lsp  *************
;;;gxl-Sel-EntNextAll en 返回 en 之后的所有物体选择集，无则返回 nil,en为nil返回图形全部图元

(defun gxl-Sel-EntNextAll (ent / ss)
  (if (not ent)
    (progn
      (setq ent (entnext))
      (if ent
        (setq ss (ssadd ent))
        (setq ss (ssadd))
        )
      )
    (setq ss (ssadd))
    )
  (while (setq ent (entnext ent))
    (if (not (member (cdr (assoc 0 (entget ent)))
                     '("ATTRIB" "VERTEX" "SEQEND")
                     )
             )
      (ssadd ent ss)
      )
    )
  (if (= 0 (sslength ss))
    nil
    ss
    )
  )
;;;***************** 函数 gxl-Sel-EntNextAll*****************


;;;*************函数 gxl-sel-ss-》vla.lsp  *************
;;;;;;gxl-sel-ss->vla 选择集转为Vla列表
(defun gxl-sel-ss->vla (ss / i l)
  (if ss
    (repeat (setq i (sslength ss))
      (setq
        l (cons (vlax-ename->vla-object (ssname ss (setq i (1- i))))
                l
                )
        )
      )
    )
  )
;;;***************** 函数 gxl-sel-ss->vla*****************


;;;*************函数 gxl-getsortentstable.lsp  *************
;;;(gxl-GetSortentsTable *MODEL-SPACE*) 获取AcDbSortentsTable 对象，参数：space 为ModelSpace or PaperSpace
(defun gxl-GetSortentsTable ( space / dict ) (vl-load-com)
  
  (cond
    (
      (gxl-CATCHAPPLY vla-item
        (list
          (setq dict
            (vla-GetExtensionDictionary space)
          )
         "ACAD_SORTENTS"
        )
      )
    )
    (
      (gxl-CATCHAPPLY vla-AddObject
        (list dict "ACAD_SORTENTS" "AcDbSortentsTable")
      )
    )
  )
)
;;;***************** 函数 gxl-GetSortentsTable*****************


;;;*************函数 gxl-num-objectvariant.lsp  *************

;;-------------------=={ Object Variant }==-------------------;;
;;                                                            ;;
;;  Creates a populated Object Variant                        ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright ?2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  lst - list of VLA Objects to populate the Variant.        ;;
;;------------------------------------------------------------;;
;;  Returns:  VLA Object Variant                              ;;
;;------------------------------------------------------------;;
;;;(gxl-num-ObjectVariant vla对象表) 创建vla变体
(defun gxl-num-ObjectVariant ( lst )
  ;(gxl-num-SafearrayVariant vlax-vbobject lst)
  (vlax-make-variant
    (vlax-safearray-fill
      (vlax-make-safearray vlax-vbobject
        (cons 0 (1- (length lst)))
      )
      lst
    )    
  )
)
;;;***************** 函数 gxl-num-ObjectVariant*****************


;;;*************函数 gxl-movetobottom.lsp  *************
;|;;;移到最上显示 By Gu_xl  示例： (MovetoTop (ssget)) 
(defun MovetoTop (ss / ObjLst Sortents dict i)
  (cond	((= 'pickset (type ss))
	 (repeat (setq i (sslength ss))
	   (setq ObjLst	(cons (vlax-ename->vla-object
				(ssname ss (setq i (1- i)))
			      ) ;_ vlax-ename->vla-object
			      ObjLst
			) ;_ cons
	   ) ;_ setq
	 ) ;_ repeat
	)
	((= 'ename (type ss))
	 (setq ObjLst (list (vlax-ename->vla-object ss)))
	)
	((= 'vla-object (type ss)) (setq ObjLst (list ss)))
  ) ;_ cond
  (if
    (cond
      ((not
	 (VL-CATCH-ALL-ERROR-P
	   (setq Sortents
		  (VL-CATCH-ALL-APPLY
		    'vla-item
		    (list
		      (setq dict
			     (vla-GetExtensionDictionary
			       (vla-ObjectIDtoObject
				 (vla-get-ActiveDocument (vlax-get-acad-object))
				 (vla-get-OwnerID (car ObjLst))
			       ) ;_ vla-ObjectIDtoObject
			     ) ;_ vla-GetExtensionDictionary
		      ) ;_ setq
		      "ACAD_SORTENTS"
		    ) ;_ list
		  ) ;_ gxl-CATCHAPPLY
	   ) ;_ setq
	 ) ;_ VL-CATCH-ALL-ERROR-P
       ) ;_ not
      )
      ((not
	 (VL-CATCH-ALL-ERROR-P
	   (setq Sortents
		  (VL-CATCH-ALL-APPLY
		    'vla-AddObject
		    (list dict "ACAD_SORTENTS" "AcDbSortentsTable")
		  ) ;_ gxl-CATCHAPPLY
	   ) ;_ setq
	 ) ;_ VL-CATCH-ALL-ERROR-P
       ) ;_ not
      )
    ) ;_ cond
     (not (vla-MovetoTop
	    Sortents
	    (vlax-make-variant
	      (vlax-safearray-fill
		(vlax-make-safearray
		  vlax-vbobject
		  (cons 0 (1- (length ObjLst)))
		) ;_ vlax-make-safearray
		ObjLst
	      ) ;_ vlax-safearray-fill
	    ) ;_ vlax-make-variant
	  ) ;_ vla-MovetoTop
     ) ;_ not
  ) ;_ if
) ;_ defun
|;

;;;(gxl-MovetoBottom ss) 选择集或图元Ename、Vla对象 移到数据库最下显示 ，成功返回 T 失败返回 nil
;;;(gxl-MovetoBottom (ssget))
(defun gxl-MovetoBottom ( ss / ObjLst Sortents ) (vl-load-com)
  (if vla-MoveToBottom
    (progn
  (cond ((= 'pickset (type ss)) (setq ObjLst (gxl-SEL-SS->VLA ss)))
	((= 'ename (type ss)) (setq ObjLst (list (vlax-ename->vla-object ss))))
	((= 'vla-object (type ss)) (setq ObjLst (list ss)))
	)
  (if
    (setq Sortents
      (gxl-GETSORTENTSTABLE
        (vla-ObjectIDtoObject *ACDOCUMENT* (vla-get-OwnerID (car ObjLst)))
      )
    )
    (not (vla-MovetoBottom Sortents (gxl-num-ObjectVariant ObjLst)))
  )
  )
    (progn
      (if (= 'vla-object (type ss)) (setq ss (vlax-vla-object->ename ss)))
    (command "draworder" ss "" "b")
    )
    )
)
;;;***************** 函数 gxl-MovetoBottom*****************


;;;*************函数 gxl-sel-ss-》ax：array.lsp  *************

;;;===================================================================
;;;gxl-Sel-SS->AX:Array 转换选择集为变体数组
;|功能  
转换选择集为变体数组  
语法  
(selectionsetToArray ss)  
参数  
ss: 选择集  
返回值  
变体数组  
样例  
(selectionsetToArray mySS)  
说明  
使用该函数可以将选择集转换为数组传递给ActiveX函数。
如果需要其它的子类型，只需更改引用vlax-vbObject。  
|;
;;;===================================================================
(defun gxl-Sel-SS->AX:Array (ss / c r en)
  (repeat (setq c (sslength ss))
    (setq en (ssname ss (setq c (1- c))))
    (if	(entget en)
      (setq r (cons en r))
    )
  )
  (vlax-safearray-fill
    (vlax-make-safearray
      vlax-vbObject
      (cons 0 (1- (length r)))
    )
    (mapcar 'vlax-ename->vla-object r)
  )
)
;;;***************** 函数 gxl-Sel-SS->AX:Array*****************


;;;*************函数 gxl-ax：addunnamegroup.lsp  *************
;;;(gxl-AX:AddUnNameGroup ss) 创建无名组 (gxl-AX:AddUnNameGroup (ssget))
(defun gxl-AX:AddUnNameGroup (ss / objGroup)
  (vla-AppendItems (setq objGroup (vla-add (vla-get-Groups *ACDOCUMENT*) "*")) (gxl-Sel-SS->AX:Array ss))
  objGroup
  )
;;;***************** 函数 gxl-AX:AddUnNameGroup*****************


;;;*************函数 gxl-ax：addblock.lsp  *************
;;;===================================================================
;;;(gxl-AX:AddBlock InsPt Name) 增加块定义(做块头)，返回块定义 OBJ
(defun gxl-AX:AddBlock (InsPt Name)
  (if (= (substr (strcase name) 1 2) "*U") (setq name "*U"))
(vla-add (vla-get-Blocks *AcDocument*) (vlax-3d-point InsPt) Name)
  )
;;;***************** 函数 gxl-AX:AddBlock*****************


;;;*************函数 gxl-str-subst.lsp  *************
 ;_ end of defun
;;; (gxl-Str-Subst New Old Str) 替换字符串中的某些字符为其它字符
;;;(gxl-Str-Subst  ",." ".." "123..456..789")
(defun gxl-Str-Subst (New Old Str / str1 n)
  (setq n (strlen old))
  (cond	((> (strlen str) n)
	 (setq str1 (substr str 1 n))
	 (if (= str1 old)
	   (strcat new (gxl-Str-Subst new old (substr str (1+ n))))
	   (strcat (substr str 1 1) (gxl-Str-Subst new old (substr str 2)))
	   )
	)
	((= (strlen str) n)
	 (if (= old str)
	   new
	   str
	   )
	)
	(t
	 str
	)
  ) ;_ 结束cond
  )
;;;***************** 函数 gxl-Str-Subst*****************


;;;*************函数 gxl-blk-check.lsp  *************
;|(defun c:BlockIn0 (/ f)
  (setierr)
  (foreach f (gxl-sort (gxl-file-Dos_dir "E:\\lisp\\MySurvey编译\\symbol\\*.dwg") '<)
    (command "-insert" (strcat "E:\\lisp\\MySurvey编译\\symbol\\" f) '(0 0 0) 1 1  0)
    )
  (reerr)
  )

(defun c:BlockIn1 (/ f)
  (setierr)
  (foreach f (gxl-sort (gxl-file-Dos_dir "E:\\lisp\\房产CAD工具软件\\EstateCADTools\\support\\图廓\\*.dwg") '<)
    (command "-insert" (strcat "E:\\lisp\\房产CAD工具软件\\EstateCADTools\\support\\图廓\\" f) '(0 0 0) 1 1  0)
    )
  (reerr)
  )

(defun c:BlockIn2 (/ f)
  (setierr)
  (foreach f (gxl-sort (gxl-file-Dos_dir "E:\\lisp\\房产CAD工具软件\\EstateCADTools\\support\\符号\\*.dwg") '<)
    (command "-insert" (strcat "E:\\lisp\\房产CAD工具软件\\EstateCADTools\\support\\符号\\" f) '(0 0 0) 1 1  0)
    )
  (reerr)
  )

(defun c:BlockIn3 (/ f)
  (setierr)
  (foreach f (gxl-sort (gxl-file-Dos_dir "c:\\tk\\*.dwg") '<)
    (command "-insert" (strcat "c:\\tk\\" f) '(0 0 0) 1 1  0)
    )
  (reerr)
  )
(defun c:BlockIn4 (/ f)
  (setierr)
  (foreach f (gxl-sort (gxl-file-Dos_dir "E:\\lisp\\MySurvey编译\\symbol\\新增符号\\*.dwg") '<)
    (command "-insert" (strcat "E:\\lisp\\MySurvey编译\\symbol\\新增符号\\" f) '(0 0 0) 1 1  0)
    )
  (reerr)
  )
  |;
;;; gxl-Blk-Check 检查定义图块
(defun gxl-Blk-Check (B_Name / $PROMPT B_NAME1 CURLAY ERR)
  (if (or (= 'SUBR (type MakeBlock-001))
	  (= 'USUBR (type MakeBlock-001))
	  )
    ()
    (setq $prompt (load "MakeBlockSymbol.vlx" "未找到MakeBlockSymbol.vlx文件"))
    )
  (if (= $prompt "未找到MakeBlockSymbol.vlx文件")
    (setq $prompt (load "E:\\lisp\\房产CAD工具软件\\lisp\\MakeBlockSymbol.vlx" "未找到MakeBlockSymbol.vlx文件"))
    )
  ;(if (= $prompt "未找到MakeBlockSymbol.vlx文件") (progn (princ "\n未找到MakeBlockSymbol.vlx文件") (exit)))
  (if (= $prompt "未找到MakeBlockSymbol.vlx文件")
    B_Name
    (progn
  (setq B_Name1 (gxl-Str-Subst "]" ")" (gxl-Str-Subst "[" "(" B_Name)))
  (setq curlay (getvar "Clayer"))
  (setq err (VL-CATCH-ALL-APPLY 'vla-Item (list (vla-get-Blocks *ACDOCUMENT*) B_Name)))
  (if (VL-CATCH-ALL-ERROR-P err) ;(not (member B_Name (gxl-TABLE "block")))
    (progn
      (if (or (= 'USUBR (type (eval(read (strcat "MakeBlock-" B_Name1)))))
	      (= 'SUBR (type (eval(read (strcat "MakeBlock-" B_Name1)))))
	      )
	(eval (read (strcat "(MakeBlock-" B_Name1 ")")))
	)
      )
    )
  (setvar "clayer" curlay)
  B_Name
  )
    )
  )
;;;***************** 函数 gxl-Blk-Check*****************


;;;*************函数 gxl-ax：insertblock.lsp  *************
;;;====================================ACTIVEX方法=============================================
;;;插入块
;;;(gxl-AX:InsertBlock InsPt Name Xscale Yscale ZScale Rotation) 插入图块,返回BlockREf
;;;(gxl-AX:InsertBlock  (getpoint) "GC200" 1 1 1 0)
(defun gxl-AX:InsertBlock (InsPt Name Xscale Yscale ZScale Rotation)
  (gxl-BLK-CHECK Name)
  (setvar "insname" (VL-FILENAME-BASE name))
  (VL-CATCH-ALL-APPLY 'vla-InsertBlock (list *MODEL-SPACE* (vlax-3d-point (trans InsPt 1 0)) Name Xscale Yscale ZScale Rotation))
  )
;;;***************** 函数 gxl-AX:InsertBlock*****************


;;;*************函数 gxl-blk-unblockbase.lsp  *************
 ;_ defun
;;(gxl-BLK-UnBlockBase ss base) 制作无名块，base 为图块基点 或 0 = 中心 1 = 左下 2 = 右下 3 = 右上 4 = 左上 ，默认值为0
;;(gxl-BLK-UnBlockBase (ssget) 3)
(defun gxl-BLK-UnBlockBase (ss base / obj blkName obj1 cp ll ur)
  (if (> (sslength ss) 0)
    (progn
  (setq blkName "*U")
  (setq ss (gxl-Sel-SS->AX:Array ss))
  (setq obj (gxl-AX:AddBlock '(0 0 0) blkName))
  (vla-CopyObjects *AcDocument* ss obj)
  (foreach ent (vlax-safearray->list ss)
    (VL-CATCH-ALL-APPLY 'vla-Delete (list ent))
  ) ;_ foreach
  (setq obj1 (gxl-AX:InsertBlock '(0 0 0) (vla-get-name obj) 1 1 1 0))
  ;;计算基点
  (cond
    ((= 'list (type base))
     (setq cp base)
     )
    ((or (null base)
	 (= 0 base)
	 )
     (vla-GetBoundingBox obj1 'll 'ur)
     (setq ll (vlax-safearray->list ll)
	   ur (vlax-safearray->list ur)
	   )
     (setq cp (gxl-MIDPOINT ll ur))
     )
    ((= 1 base)
     (vla-GetBoundingBox obj1 'll 'ur)
     (setq ll (vlax-safearray->list ll)
	   ;ur (vlax-safearray->list ur)
	   )
     (setq cp ll)
     )
    ((= 2 base)
     (vla-GetBoundingBox obj1 'll 'ur)
     (setq ll (vlax-safearray->list ll)
	   ur (vlax-safearray->list ur)
	   )
     (setq cp (list (car ur) (cadr ll) 0))
     )
    ((= 3 base)
     (vla-GetBoundingBox obj1 'll 'ur)
     (setq ;ll (vlax-safearray->list ll)
	   ur (vlax-safearray->list ur)
	   )
     (setq cp ur)
     )
    ((= 4 base)
     (vla-GetBoundingBox obj1 'll 'ur)
     (setq ll (vlax-safearray->list ll)
	   ur (vlax-safearray->list ur)
	   )
     (setq cp (list (car ll) (cadr ur) 0))
     )
    )
  ;;修改图块基点
  (vla-put-Origin obj (vlax-3d-point cp))
  (vla-move obj1 (vlax-3d-point '(0 0 0)) (vlax-3d-point cp))
  ;;************
  obj1
  )
    )
)
;;;***************** 函数 gxl-BLK-UnBlockBase*****************


;;;*************函数 vlxls-app-new.lsp  *************
 
;|Examples:

(vlxls-app-init)è 33
 

 

Excel Application Session Progress Function
 
Name
 (vlxls-app-new ShowExcelFlag)
 
Usage
 Open a new Excel session and start a new workbook.
 
Input
 BOOLEAN
 T for display, nil for hide
 
RetVal
 True
 VLOBJ
 Excel Session vla-object
 
Fail
 BOOLEAN
 NIL
 |;
(Defun vlxls-app-New (UnHide / Rtn)
  (if (vlxls-app-init)
    (progn
      (if *Chinese*
           (princ "\n 新建Excel工作表...")
       (princ "\n Creating new Excel Spreadsheet file...")
      )
      (if (not (VL-CATCH-ALL-ERROR-P (setq Rtn (VL-CATCH-ALL-APPLY 'vlax-get-or-create-object '("Excel.Application")))))
       (progn
         (vlax-invoke-method
           (vlax-get-property Rtn 'WorkBooks)
           'Add
         )
         (if UnHide
           (vla-put-visible Rtn 1)
           (vla-put-visible Rtn 0)
         )
       )
      )
    )
  )
  Rtn
)
;;;***************** 函数 vlxls-app-New*****************


;;;*************函数 gxl-onelist.lsp  *************
;;;展开嵌套表 gxl-onelist lst
;;;(gxl-onelist '(((1 2 3 ( 4 (5 6))) 7) (8 (9 10)) 11 (12(13 14) (15 (16 17))))) (gxl-onelist nil)
;|'(((1 2 3 ( 4 (5 6))) 7) (8 (9 10)) 11 (12(13 14) (15 (16 17))))
(defun gxl-onelist (l / return onelist1 a)
    (foreach a l
    (cond ((atom a)
	 (setq return (append return (list a)))
	 )
	(t
	 (setq return (append return  (gxl-onelist a)))
	)
	)
    )
  return
) ;_ 结束deufn(atom nil)
|;
(defun gxl-onelist ( l )
  (if l
  (if (atom l) (list l)
      (append (gxl-onelist (car l)) (gxl-onelist (cdr l)))
    )
    )
)
;;;***************** 函数 gxl-onelist*****************


;;;*************函数 vlxls-cellid-calc.lsp  *************

 
;|Examples:

(vlxls-cell-put-value *xlapp* "C12" "xx") è #<VLA-OBJECT Range 093a7764>

(vlxls-cell-put-value *xlapp* "C12:F3" "5") è #<VLA-OBJECT Range 43c5ac64>

(vlxls-cell-put-value *xlapp* "C12:D13" '(("zz" 2)("xx" 31))) è #<VLA-OBJECT Range 1b8f2a64>
 

 

Excel Cell and Range Progress Function
 
Name
 (vlxls-cellid-calc BaseCellId XOffset YOffset)
 
Usage
 Calculate a new Cell ID for given delta X and Y from base Cell ID.
 
Input
 STR/LIST
 Base Cell ID string or list
 
INT
 X offset integer of Cell ID
 
INT
 Y offset integer of Cell ID
 
RetVal
 True
 STRING
 An Excel Complex Cell ID format contain the base Cell ID and target Cell ID.
 
Fail
 BOOLEAN
 NIL
 |;
(Defun vlxls-cellid-calc (id x y / idx)

  (setq   id  (car (vlxls-cellid id))

       idx (vlxls-rangeid id)

       x   (+ x (car idx))

       x   (if    (< x 1)

             1

             x

           )

       y   (+ y (cadr idx))

       y   (if    (< y 1)

             1

             y

           )

       idx (vlxls-rangeid (list x y))

       id  (vlxls-cellid (strcat id ":" idx))

       id  (strcat (car id) ":" (cadr id))

  )

  id

)
;;;***************** 函数 vlxls-cellid-calc*****************


;;;*************函数 vlxls-cell-put-value.lsp  *************
;| 
Examples:

(vlxls-cell-get-value *xlapp* "C12") è "g"

(vlxls-cell-get-value *xlapp* "C12:C12") è "g"

(vlxls-cell-get-value *xlapp* "C12:C15") è (("g") ("") ("") (""))

(vlxls-cell-get-value *xlapp* "C12:F12") è (("g" "ds" "" ""))

(vlxls-cell-get-value *xlapp* "C12:F15") è (("g" "ds" "" "") ("" "" "g" "") ("" "" "" "") ("" "" "" ""))
 

 

Excel Cell and Range Progress Function
 
Name
 (vlxls-cell-put-value ExcelSessionVLA-OBJECT CellIDStringOrList DataList)
 
Usage
 Pass a 1 dimension or a 2 dimension string list into Excel, started at certain Cell ID.
 
Input
 VLOBJ
 The Excel Session vla-object
 
STR/LIST
 The start Cell ID [Left-Upper] list or string
 
STR/LIST
 If this argument is a string, VLXLS will fill same string to all cells.

Or the argument should be a 1 dimension list or a 2 dimension list to fill in Excel. If the data list can NOT match the
given cell ID, VLXLS will only fill first cell, fill to other cells will be ignored.
 
RetVal
 True
 VLOBJ
 All Excel Range vla-object that just be filled in by given data list
 
Fail
 BOOLEAN
 NIL
 |;
(Defun vlxls-cell-put-value

       (xl id Data / vllist-explode idx xx yy ary Rtn)
;|    (Defun vllist-explode  (lst)

	(cond

	    ((not lst) nil)

	    ((atom lst) (list lst))

	    ((append (vllist-explode (car lst))

		     (vllist-explode (cdr lst))

		     )

	     )

	    )

	)
|;
    (if	(null id)
	(setq id "A1")
	)
    (if	(= (type id) 'list)
	(setq id (vlxls-rangeid id))
	)
  (if (atom data) (setq data (list data)))
    (if	(= (type (car Data)) 'LIST)
	(setq ARY (vlax-make-safearray
		      vlax-vbVariant;vlax-vbstring
		      (cons 0 (1- (length Data)))
		      (cons 1 (length (car Data)))
		      )

	      XX  (1- (length (car Data)))
	      YY  (1- (length Data))
	      )
	(setq
	    ARY	(vlax-make-safearray
		    vlax-vbVariant;vlax-vbstring
		    (cons 0 1)
		    (cons 1 (length Data))
		    )
	    XX	(1- (length Data))
	    YY	0
	    )
	)
    (if	(= xx yy 0)
	(msxlp-put-VALUE2
	    (setq Rtn (msxlp-get-range xl id))
	    (car (gxl-ONELIST data))
	    )
	(progn
	    (setq id (vlxls-cellid-calc id xx yy))
	    (msxlp-put-VALUE2
		(setq Rtn (msxlp-get-range xl id))
		(vlax-safearray-fill ary data)
		)
	    )
	)
    Rtn
    )
;;;***************** 函数 vlxls-cell-put-value*****************


;;;*************函数 vlxls-range-autofit.lsp  *************
 
;|Examples:

(vlxls-rangeid '(3 14)) è "C14"

(vlxls-rangeid "D23") è (4 23)

(vlxls-rangeid "DD23") è (108 23)
 

 

Excel Cell and Range Progress Function
 
Name
 (vlxls-range-autofit RangeVLA_OBJECT)
 
Usage
 Autofit the column width of a certain range object.
 
Input
 VLOBJ
 The Excel Range vla-object
 
RetVal
 True
 BOOLEAN
 T
 
Fail
 BOOLEAN
 NIL
 |;
(Defun vlxls-range-autofit (range)

  (equal (vlax-variant-value

          (msxl-autofit

            (msxlp-get-columns (msxlp-get-Cells range))

          )

        )

        :vlax-true

  )

)
;;;***************** 函数 vlxls-range-autofit*****************


;;;*************函数 vlxls-sheet-get-usedrange.lsp  *************
;| 
Examples:

(vlxls-sheet-put-active *xlapp* "Sheet1") è T

(vlxls-sheet-put-active *xlapp* "NewSheet") è T
 

 

Excel Sheet Progress Function
 
Name
 (vlxls-sheet-get-usedrange ExcelSessionVLA-OBJECT SheetName)
 
Usage
 Get all used range of certain Excel sheet. If sheet name not exist, return NIL.
 
Input
 VLOBJ 
 Excel session vla-object
 
STRING
 Excel sheet name string, NIL for current active sheet.
 
RetVal
 True
 VLOBJ
 Excel Range vla-object
 
Fail
 BOOLEAN
 NIL
 |;
(Defun vlxls-sheet-get-UsedRange (xlapp Name / sh Rtn)

  (if (null Name)

    (setq Name (vlax-get-property (msxlp-get-ActiveSheet Xlapp) 'Name))

  )
  
  (vlax-for sh (vlax-get-property Xlapp "sheets")

    (if    (= (vlax-get-property sh "Name") Name)

      (setq Rtn (vlax-get-property sh "UsedRange"))

    )

  )

  Rtn

)
;;;***************** 函数 vlxls-sheet-get-UsedRange*****************


;;;*************函数 vlxls-app-quit.lsp  *************
 
;|Examples:

(vlxls-app-saveas *xlapp* nil) è "C:/Temp-Folder/XLS.XLS"

(vlxls-app-saveas *xlapp* "C:/Temp-Folder/XLS.XLS") è "C:/Temp-Folder/XLS.XLS"

(vlxls-app-saveas *xlapp* nil) è NIL
 

 

Excel Application Session Progress Function
 
Name
 (vlxls-app-quit ExcelSessionVLA-OBJECT SavedFlag)
 
Usage
 Quit active workbook of Excel session and release Excel application.
 
Input
 VLOBJ
 Excel session vla-object
 
BOOLEAN
 Save Excel active workwook flag, T for save, NIL for unsave
 
RetVal
 True
 BOOLEAN
 NIL
 
Fail
 BOOLEAN
 NIL
 |;
(Defun vlxls-app-quit (ExlObj SaveYN)

  (if SaveYN

    (vlax-invoke-method

      (vlax-get-property ExlObj "ActiveWorkbook")

      'Close

    )

    (vlax-invoke-method

      (vlax-get-property ExlObj "ActiveWorkbook")

      'Close

      :vlax-False

    )

  )
  (if (= 0
	 (vlax-get-property
	   (vlax-get-property ExlObj "Workbooks")
	   "count"
	 )
      )
    (progn
      (vlax-invoke-method ExlObj 'QUIT)

      (vlax-release-object ExlObj)

      (setq ExlObj nil)
    )
  )


  (gc)

)
;;;***************** 函数 vlxls-app-quit*****************
(setq *xls-color*
       (list (list 1 18 0)
	     (list 2 7 1677215)
	     (list 3 1 16711680)
	     (list 4 3 65280)
	     (list 5 5 255)
	     (list 6 2 16776960)
	     (list 7 6 16711935)
	     (list 8 4 65535)
	     (list 9 16 8323072)
	     (list 10 96 32512)
	     (list 11 176 127)
	     (list 12 56 8355584)
	     (list 13 216 8323199)
	     (list 14 136 32639)
	     (list 15 9 12566463)
	     (list 16 8 8355711)
	     (list 17 161 9476095)
	     (list 18 237 9449568)
	     (list 19 7 1677167)
	     (list 20 254 12648447)
	     (list 21 218 6291552)
	     (list 22 11 16744319)
	     (list 23 152 24768)
	     (list 24 254 13617407)
	     (list 25 176 127)
	     (list 26 6 16711935)
	     (list 27 2 16776960)
	     (list 28 4 65535)
	     (list 29 216 8323199)
	     (list 30 16 8323072)
	     (list 31 136 32639)
	     (list 32 5 255)
	     (list 33 140 51455)
	     (list 34 254 12648447)
	     (list 35 254 13631439)
	     (list 36 51 16777104)
	     (list 37 151 9488639)
	     (list 38 221 16750799)
	     (list 39 191 13605119)
	     (list 40 31 16763024)
	     (list 41 150 3105023)
	     (list 42 132 3131584)
	     (list 43 62 9488384)
	     (list 44 40 16762880)
	     (list 45 30 16750336)
	     (list 46 30 16738048)
	     (list 47 165 6317968)
	     (list 48 252 9475984)
	     (list 49 148 12384)
	     (list 50 105 3184736)
	     (list 51 98 12032)
	     (list 52 48 3158016)
	     (list 53 24 9449472)
	     (list 54 237 9449311)
	     (list 55 177 3158160)
	     (list 56 250 3092527)
       )
  *Chinese* T
)

;;;*************************************************
(defun c:ev (/	*XLAPP*	      ACT_ANNOCOLOR ACT_BLAYER	  ACT_CELLCOLOR
		ACT_GETFILE   ACT_GROUP	    ACT_KEEPTHEIGHT
		ACT_MERGE     ACT_NONE	    ACT_PAGESETUP ACT_PRINTAREA
		ACT_TLAYER    ACT_UBLOCK    ACT_USED	  ACT_USER
		BASEPOINT     CELLS	    COL		  CURPT
		DCLCODE	      DD	    DEFAULTHEIGHT DRAWPAGESETUP
		DXF40	      DXF420	    DXF62	  DXF7
		DXF71	      DXF71DATA	    ECODE	  ENDENT
		FONT	      GET9JUSTPTS   GETRANGETEXTSTYLE
		GRIDSCALE     HEIGHT	    HEIGHT1
		HORIZONTALALIGNMENT	    HORLINE	  HPAGEBREAKS
		INTERIORCOLOR INTERIORTRUECOLOR		  LAYERS
		MERGEID	      MERGEP	    MKTMPDCL	  
		OLDHEIGHT     OLDROW	    P0		  P1
		P2	      P3	    PAGE	  PAGEMARGIN
		PAGESETUP     PRINTAREA	    PRINTTITLEROWS
		RANGE	      RANGEFONT	    RIGHTTOPPT	  ROW
		S1	      SCALE	    SELECTION	  SHEET
		SS	      STANDARDFONT  STANDARDFONTSIZE
		START_XL2X    STARTPOINT    TEXT	  TEXTFONT
		TEXTPT	      TEXTVERFLAG   TMP		  TO
		TOTALHEIGHT   TOTALPAGE	    TOTALWIDE	  USEDRANGE
		VERLINES      VERTICALALIGNMENT		  WIDTH
		WIDTH1	      WORKBOOK	    WORKBOOKS	  ACT_RANGE
		ACT_THEIGHT   BLAYER	    CAPTION	  CFONT
		CHAR	      CHARFONT	    F
		HORIZONTALALIGNMEN	    I		  II
		INTERIORCOLOR1		    KD		  SSTITLE
		TLAYER	      TMPPT	    VERLINE	  TITLEROWS
		TMP	      TMP1	    *DRAWRANGE*	  *CELLCOLOR*
		*ANNOCOLOR*   *OPRATE*	    *MERGE*	  *THEIGHT*
 *KEEPTHEIGHT* *PAGESETUP*   *DEFAULTCOLOR* *ROWHEIGHT* *TABLEWIDTH* *BASEPOINTPOS*
		TEXTHEIGHT
		TBLHEIGHT
		SSSolid
	       )
  ;;计算九宫格点
(defun Get9JustPts (LL UR / tmp BC BL BR MC ML MR TC TL TR)
    (setq
      LL (list (car LL) (cadr LL) 0.0)
      UR (list (car UR) (cadr UR) 0.0)
      BL LL
      TR UR
      MC (GXL-MIDPOINT BL TR)
      TL (list (car BL) (cadr TR) 0.0)
      TC (list (car MC) (cadr TR) 0.0)
      MR (list (car TR) (cadr MC) 0.0)
      BR (list (car TR) (cadr BL) 0.0)
      BC (list (car MC) (cadr BL) 0.0)
      ML (list (car BL) (cadr MC) 0.0)
      )
    (list TL TC TR ML MC MR BL BC BR)
    )
 ;;创建临时对话框 
 (defun mkTmpDcl (dclname / tmpdcl f _GetSavePath)
   (DEFUN _GETSAVEPATH (/ TMP)
     (COND ((SETQ TMP (GETVAR (QUOTE ROAMABLEROOTPREFIX)))
            (OR (EQ "\\" (SUBSTR TMP (STRLEN TMP)))
                (SETQ TMP (STRCAT TMP "\\"))
                )
            (STRCAT TMP "Support")
            )
           ((SETQ TMP (FINDFILE "ACAD.pat"))
            (SETQ TMP (VL-FILENAME-DIRECTORY TMP))
            (AND (EQ "\\" (SUBSTR TMP (STRLEN TMP)))
                 (SETQ TMP (SUBSTR TMP (1- (STRLEN TMP))))
                 )
            TMP
            )
           )
     )
   (IF DCLNAME
     (SETQ TMPDCL
	    (STRCAT (_GETSAVEPATH)
		    "\\"
		    (if	(and
			  (> (strlen DCLNAME) 4)
			  (= ".dcl"
			     (substr (setq DCLNAME (STRCASE DCLNAME T))
				     (- (strlen DCLNAME) 3)
				     4
			     )
			  )
			)
		      (substr DCLNAME 1 (- (strlen DCLNAME) 4))
		      DCLNAME
		    )
		    ".dcl"
	    )
     )
     (SETQ TMPDCL (VL-FILENAME-MKTEMP "tmp" "" ".dcl"))
     )
   (if (not (findfile tmpdcl))
     (progn
       (setq f (open tmpdcl "w"))
       (foreach str '("xl2cad:dialog {"
                      "    label = \"Excel 转CAD表格 \" ;"
                      "    :boxed_radio_column {"
                      "        key = \"Range\" ;"
                      "        label = \"Excel数据范围\" ;"
                      "        :radio_button {"
                      "            key = \"Used\" ;"
                      "            label = \"所有使用的单元格\" ;"
                      "        }"
                      "        :radio_button {"
                      "            key = \"User\" ;"
                      "            label = \"用户选定的单元格\" ;"
                      "        }"
                      "        :radio_button {"
                      "            key = \"PrintArea\" ;"
                      "            label = \"页面可打印区域\" ;"
                      "        }"
                      "    }"
                      ":button {"
                      "    alignment = left ;"
                      "    fixed_height = true ;"
                      "    fixed_width = true ;"
                      "    key = \"getfile\" ;"
                      "    label = \"选择Excel文件->\" ;"
                      "    width = 20 ;"
                      "}"
                      "    :boxed_column {"
                      "        label = \"生成设定\" ;"
                      "        :row {"
                      "            :toggle {"
                      "                key = \"CellColor\" ;"
                      "                label = \"单元格背景颜色\" ;"
                      "            }"
                      "            :toggle {"
                      "                key = \"AnnoColor\" ;"
                      "                label = \"文 本 颜 色\" ;"
                      "            }"
                      "        }"
                      "        :row {"
                      "        :toggle {"
                      "            key = \"PageSetup\" ;"
                      "            label = \"按页面设置输出\" ;"
                      "        }"
                      "        :toggle {"
                      "            key = \"Merge\" ;"
                      "            label = \"合并表格线\" ;"
                      "        }"
                      "        }"
                      "        :row {"
                      "            :toggle {"
                      "                key = \"KeepTHeight\" ;"
                      "                label = \"缺省文本高度\" ;"
                      "            }"
                      "            :edit_box {"
"                key = \"THeight\" ;"
"                label = \"\" ;"
"            }"
"        }"
"        :row {"
"            :edit_box {"
"                key = \"RowHeight\" ;"
"                label = \"表格行高度:\" ;"
"                edit_width = 6 ;"
"            }"
"            :edit_box {"
"                key = \"TableWidth\" ;"
"                label = \"表格总宽度:\" ;"
"                edit_width = 6 ;"
"            }"
"        }"
"        :row {"
"            :popup_list {"
"                key = \"BasePoint\" ;"
"                label = \"插入基点:\" ;"
"                edit_width = 12 ;"
"            }"
"        }"
"        :boxed_radio_row {"
                      "            key = \"Gather\" ;"
                      "            label = \"实体集合\" ;"
                      "            :radio_button {"
                      "                key = \"None\" ;"
                      "                label = \"无操作\" ;"
                      "            }"
                      "            :radio_button {"
                      "                key = \"Group\" ;"
                      "                label = \"无名组\" ;"
                      "            }"
                      "            :radio_button {"
                      "                key = \"UBlock\" ;"
                      "                label = \"无名块\" ;"
                      "            }"
                      "        }"
                      "        :boxed_column {"
                      "            label = \"实体图层\" ;"
                      "            :popup_list {"
                      "                edit_width = 15 ;"
                      "                key = \"BLayer\" ;"
                      "                label = \"单元格线:\" ;"
                      "            }"
                      "            :popup_list {"
                      "                edit_width = 15 ;"
                      "                key = \"TLayer\" ;"
                      "                label = \"表格内容:\" ;"
                      "            }"
		      "         :row {"
                      "        :radio_button {"
                      "            key = \"ByLayer\" ;"
                      "            label = \"颜色随层\" ;"
                      "        }"
                      "        :radio_button {"
                      "            key = \"ByBlock\" ;"
                      "            label = \"颜色随块\" ;"
                      "        }"
		      "            }"
                      "        }"
                      "    }"
                      "    ok_cancel_help;"
                      "    errtile;"
                      "}"
                      )
         (write-line str f)
         )
       (close f)
       )
     )
   tmpdcl
   )
  ;;
 (defun start_xl2x ()
   (setq *DrawRange* (getenv "Excel2CAD\\DrawRange"))
   (if (null *DrawRange*)
     (progn
     (setq *DrawRange* "Used")
     (setEnv "Excel2CAD\\DrawRange" *DrawRange*)
     )
     )
   (set_tile *DrawRange* "1")
   (GXL-DCL-ADDLIST "BLayer" Layers (VL-POSITION "0" Layers))
   (setq BLayer (nth 0 layers))
   (GXL-DCL-ADDLIST "TLayer" Layers (VL-POSITION "0" Layers))
   (setq TLayer (nth 0 layers))
   (setq *CellColor* (= "1" (getenv "Excel2CAD\\CellColor")))
   (if *CellColor*
       (set_tile "CellColor" "1") ;_ 背景颜色
     (progn
       (set_tile "CellColor" "0") ;_ 背景颜色
       (setEnv "Excel2CAD\\CellColor" "0")
     )
   )
   (setq *AnnoColor* (= "1" (getenv "Excel2CAD\\AnnoColor")))
   (if *AnnoColor*
       (set_tile "AnnoColor" "1") ;_ 文本颜色
     (progn
       (set_tile "AnnoColor" "0") ;_ 文本颜色
       (setEnv "Excel2CAD\\AnnoColor" "0")
     )
   )
   (setq *Oprate* (getenv "Excel2CAD\\Oprate"))
   (if *Oprate*
     (setq *Oprate* (atoi *Oprate*))
     (setq *Oprate* 0)
     )
   (setenv "Excel2CAD\\Oprate" (itoa *Oprate*))
   (cond
     ((or (null *Oprate*) (= 0 *Oprate*))
      (setq *Oprate* 0)
      (set_tile "None" "1")
      )
     ((= 1 *Oprate*)
      (set_tile "Group" "1")
      )
     ((= 2 *Oprate*)
      (set_tile "UBlock" "1")
      )
     ) 
   (setq *Merge* (= "1" (getenv "Excel2CAD\\Merge")))
   (if *Merge*
     (set_tile "Merge" "1")
     (progn
     (set_tile "Merge" "0")
     (Setenv "Excel2CAD\\Merge" "0")
     )
     )
   (setq *THeight* (getenv "Excel2CAD\\THeight"))
   (if (null *THeight*)
     (progn
       (setq *THeight* 300)
       (Setenv "Excel2CAD\\THeight" "300")
     )
     (setq *THeight* (atof *THeight*))
   )
  (set_tile "THeight" (rtos *THeight* 2 1))
  (setq *RowHeight* (getenv "Excel2CAD\\RowHeight"))
  (if (null *RowHeight*)
    (progn
      (setq *RowHeight* 0.0)
      (Setenv "Excel2CAD\\RowHeight" "0.0")
    )
    (setq *RowHeight* (atof *RowHeight*))
  )
  (set_tile "RowHeight" (rtos *RowHeight* 2 1))
  (setq *TableWidth* (getenv "Excel2CAD\\TableWidth"))
  (if (null *TableWidth*)
    (progn
      (setq *TableWidth* 0.0)
      (Setenv "Excel2CAD\\TableWidth" "0.0")
    )
    (setq *TableWidth* (atof *TableWidth*))
  )
  (set_tile "TableWidth" (rtos *TableWidth* 2 1))
  ;;基点位置设置
  (setq *BasePointPos* (getenv "Excel2CAD\\BasePoint"))
  (if (null *BasePointPos*)
    (progn
      (setq *BasePointPos* 0)
      (setenv "Excel2CAD\\BasePoint" "0")
    )
    (setq *BasePointPos* (atoi *BasePointPos*))
  )
  (GXL-DCL-ADDLIST "BasePoint" '("左上" "左下" "右上" "右下") *BasePointPos*)
  (setq *KeepTHeight* (= "1" (getenv "Excel2CAD\\KeepTHeight")))
   (if *KeepTHeight*
     (progn
     (mode_tile "THeight" 0)
     (set_tile "KeepTHeight" "1")
     )
     (progn
     (mode_tile "THeight" 1)
     (set_tile "KeepTHeight" "0")
     (Setenv "Excel2CAD\\KeepTHeight" "0")
     )
     )
   (setq *pageSetUp* (= "1" (getenv "Excel2CAD\\pageSetUp")))
   (if *pageSetUp*
     (set_tile "PageSetup" "1")
     (progn
     (set_tile "PageSetup" "0")
     (Setenv "Excel2CAD\\PageSetup" "0")
     )
     )
   (setq *defaultColor* (getenv "Excel2CAD\\defaultColor"))
   (if (null *defaultColor*)
     (progn
     (setq *defaultColor* 0)
     (Setenv "Excel2CAD\\defaultColor" "0")
     )
     (setq *defaultColor* (atoi *defaultColor*))
     )
   (cond
     ((= 0 *defaultColor*) (set_tile "ByBlock" "1"))
     (t (set_tile "ByLayer" "1")
      (setq *defaultColor* 256)
      )
     )
 ;;控件控制动作
   (action_tile "getfile" "(act_getfile)")
   (action_tile "Used" "(act_Used $key $value $reason)")
   (action_tile "PrintArea" "(act_PrintArea $key $value)")
   (action_tile "User" "(act_User $key $value $reason)")
   (action_tile "CellColor" "(act_CellColor $key $value $reason)")
   (action_tile "AnnoColor" "(act_AnnoColor $key $value $reason)")
   (action_tile "PageSetup" "(act_PageSetup $key $value)")
   (action_tile "Merge" "(act_Merge $key $value $reason)")
   (action_tile "KeepTHeight" "(act_KeepTHeight $value)")
   (action_tile "THeight" "(setq *THeight* (gxl-chkrealp $value $key 6)) (if *THeight* (Setenv \"Excel2CAD\\\\THeight\" (rtos *THeight* 2)))")
  (action_tile "RowHeight" "(setq *RowHeight* (gxl-chkrealp $value $key 6)) (if *RowHeight* (Setenv \"Excel2CAD\\\\RowHeight\" (rtos *RowHeight* 2)))")
  (action_tile "TableWidth" "(setq *TableWidth* (gxl-chkrealp $value $key 6)) (if *TableWidth* (Setenv \"Excel2CAD\\\\TableWidth\" (rtos *TableWidth* 2)))")
   (action_tile "Gather" "(act_Gather $key $value $reason)")
   (action_tile "None" "(act_None $key $value $reason)")
   (action_tile "Group" "(act_Group $key $value $reason)")
   (action_tile "UBlock" "(act_UBlock $key $value $reason)")
   (action_tile "BLayer" "(act_BLayer $key $value $reason)")
   (action_tile "TLayer" "(act_TLayer $key $value $reason)")
   (action_tile "ByBlock" "(setq *defaultColor* 0) (Setenv \"Excel2CAD\\\\defaultColor\" \"0\")")
   (action_tile "ByLayer" "(setq *defaultColor* 256) (Setenv \"Excel2CAD\\\\defaultColor\" \"256\")")
   (action_tile "BasePoint" "(act_BasePoint $key $value $reason)")
   (action_tile "help" "(alert \"***Excel To AutoCAD*** \n\n版权所有:    \n\n联系方式:          \n\n\")")
 )
  ;;act_getfile动作
  (defun act_getfile (/ filename)
    (setq filename (getfiled "" "" "xls;xlsx" 4))
    (if filename (setq *xlapp* (vlxls-app-open filename t)))
    )

  ;;控件 Used 动作
    (defun act_Used (key val reason)
      (setq *DrawRange* key)
      (setEnv "Excel2CAD\\DrawRange" key)
     )

  ;;控件 User 动作
    (defun act_User (key val reason)
      (setq *DrawRange* key)
      (setEnv "Excel2CAD\\DrawRange" key)
     )
   (defun act_PrintArea (key val)
     (setq *DrawRange* key)
     (setEnv "Excel2CAD\\DrawRange" key)
     )
  ;;控件 CellColor 动作
    (defun act_CellColor (key val reason)
      (setq *CellColor* (= "1" val))
      (setEnv "Excel2CAD\\CellColor" val)
     )

  ;;控件 AnnoColor 动作
    (defun act_AnnoColor (key val reason)
      (setq *AnnoColor* (= "1" val))
      (setEnv "Excel2CAD\\AnnoColor" val)
     )
 ;;按页面设置输出
  (defun act_PageSetup (key val)
    (setq *PageSetUp* (= "1" val))
    (setEnv "Excel2CAD\\PageSetup" val)
    )
  ;;控件 Merge 动作
    (defun act_Merge (key val reason)
      (setq *Merge* (= "1" val))
      (setEnv "Excel2CAD\\Merge" val)
     )


  ;;控件 None 动作
    (defun act_None (key val reason)
      (setq *Oprate* 0)
      (set_tile "None" "1")
      (setEnv "Excel2CAD\\Oprate" "0")
     )

  ;;控件 Group 动作
    (defun act_Group (key val reason)
      (setq *Oprate* 1)
      (set_tile "Group" "1")
      (setEnv "Excel2CAD\\Oprate" "1")
     )

  ;;控件 UBlock 动作
    (defun act_UBlock (key val reason)
      (setq *Oprate* 2)
      (set_tile "UBlock" "1")
      (setEnv "Excel2CAD\\Oprate" "2")
     )
  ;;缺省文本高度
  (defun act_KeepTHeight (val)
    (setq *KeepTHeight* (= "1" val))
    (setEnv "Excel2CAD\\KeepTHeight" val)
    (if *KeepTHeight*
      (mode_tile "THeight" 0)
      (mode_tile "THeight" 1)
      )
    )
  ;;控件 BLayer 动作
    (defun act_BLayer (key val reason)
      (setq BLayer (nth (read val) layers))
     )

  ;;控件 TLayer 动作
    (defun act_TLayer (key val reason)
      (setq TLayer (nth (read val) layers))
     )
  ;;控件 BasePoint 动作
    (defun act_BasePoint (key val reason)
      (setq *BasePointPos* (atoi val))
      (setEnv "Excel2CAD\\BasePoint" val)
     )
  ;;绘制顶端标题
  (defun PrintTitleRows	(Range / R		PRINTAREA    
			   CELLS	COL	     ROW
			   MERGEP	WIDTH	     HEIGHT
			   TEXT		FONT	     HORIZONTALALIGNMENT
			   VERTICALALIGNMENT	     DXF71
			   DXF62	DXF420	     RANGEFONT
			   DXF7		TEXTFONT     DXF40
			   TEXTVERFLAG	TMP	     OLDROW
			   	P0	     
			   RIGHTTOPPT	OLDHEIGHT    MERGEID
			   WIDTH1	HEIGHT1	     P1
			   P2		P3	     HORLINE
			   VERLINES	INTERIORCOLOR
			   INTERIORTRUECOLOR	     TEXTPT
                         Columns
			  )
    (setq r (VLXLS-GET-PROPERTY
	      *XLAPP*
	      "ActiveSheet.PageSetup.PrintTitleRows"
	    )
    )
    (if	(/= "" r)
      (progn
	(progn
	    (setq r	    (GXL-STRPARSE r ":"))
          (vlax-for a (VLXLS-GET-PROPERTY range "Columns")
            (setq
              Columns (cons (VLXLS-GET-PROPERTY a "Column") Columns)
              )
            )
          (setq Columns (reverse Columns))
          (setq r (strcat (chr (+ 64 (car Columns))) (car r) ":" (chr (+ 64 (last Columns))) (last r)))
          (setq range	    (vlax-get-property *XLAPP* 'range r)
                cells	    (vlax-get-property range 'cells)
                )
	    ;;逐个绘制表头，未完成
	    (vlax-for cell cells
	      (gxl-Sys-Progress to -1)
	      (setq col	   (vlax-get-property cell 'column)
		    row	   (vlax-get-property cell 'row)
		    range  (msxlp-get-range
			     *xlApp*
			     (VLXLS-RANGEID (list col row))
			   )
		    Mergep (equal :vlax-true
				  (vlax-variant-value
				    (vlax-get-property cell 'MergeCells)
				  )
			   )
		    width  (* defaultHeight
			      GridScale
			      (vlax-variant-value
				(vlax-get-property cell 'width)
			      )
			   )
		    height (* defaultHeight
			      GridScale
			      (vlax-variant-value
				(vlax-get-property cell 'height)
			      )
			   )
		    text   (vlax-variant-value (vlax-get-property cell 'text))
	      )
	      (if (and *RowHeight* (> *RowHeight* 0)) (setq height *RowHeight*))
	      (if (and (/= text "")
		       (not (equal width 0 0.01))
		  )
		(progn
		  (setq
		    font (vlax-get-property range 'font)
		    HorizontalAlignment
		     (vlax-variant-value
		       (vlax-get-property
			 Cell
			 'HorizontalAlignment
		       )
		     )
		    HorizontalAlignment
		     (cond ((= HorizontalAlignment -4152) 2) ;_ 右
			   ((= HorizontalAlignment -4108) 1) ;_ 中
			   (t 0) ;_ 左
		     )
		    VerticalAlignment
		     (vlax-variant-value
		       (vlax-get-property
			 Cell
			 'VerticalAlignment
		       )
		     )
		    VerticalAlignment
		     (cond ((= VerticalAlignment -4160) 0) ;_ 上
			   ((= VerticalAlignment -4108) 1) ;_ 中
			   (t 2) ;_ 下
		     )
		    DXF71 (nth VerticalAlignment
			       (nth HorizontalAlignment dxf71data)
			  )
		    DXF62 (vlxls-color-eci->aci
			    (vlax-variant-value
			      (vlax-get-property Font 'colorIndex)
			    )
			  )
		    DXF420 (vlxls-color-eci->truecolor
			     (vlax-variant-value
			       (vlax-get-property Font 'colorIndex)
			     )
			   )
		  )
		  ;;计算Range的字体 RangeFont i ii char cfont charFont caption f TextVerFlag
		  (setq	RangeFont
			 (mapcar
			   '(lambda (x) (cons x (VLXLS-GET-PROPERTY font x)))
			   '("NAME"	    "SIZE"
			     "COLORINDEX"   "BOLD"
			     "ITALIC"	    "SUBSCRIPT"
			     "SUPERSCRIPT"  "UNDERLINE"
			    )
			 )
		  )
		  (setq DXF7 (cdr (assoc "NAME" RangeFont)))
		  (if (null dxf7)
		    (setq DXF7 StandardFont)
		  )
		  ;;字体
		  (setq textFont (strcat "{\\f" DXF7 "|b0|i0|c134|p0;"))
		  (setq Dxf40 (cdr (assoc "SIZE" RangeFont)))
		  (if (null DXF40)
		    (setq DXF40 StandardFontSize)
		  )
		  ;;字大小
		  (setq	textFont (strcat textFont
					 "\\H"
					 (rtos DXF40 2 1)
					 "x;"
				 )
		  )
		  ;;加粗
		  (if (equal :vlax-true (cdr (assoc "BOLD" RangeFont)))
		    (setq textfont (strcat textFont "\\W1.2;"))
		  )
		  ;;倾斜
		  (if
		    (equal :vlax-true (cdr (assoc "ITALIC" RangeFont)))
		     (setq textfont (strcat textFont "\\Q18;"))
		  )
		  ;;下划线
		  (if (= 2 (cdr (assoc "UNDERLINE" RangeFont)))
		    (setq textfont (strcat textFont "\\L"))
		  )
		  ;;上标 "SUPERSCRIPT"
		  ;;下标 "SUBSCRIPT"
		  ;;文字是否竖向
		  (setq	TextVerFlag
			 (= (GXL-CATCHAPPLY
			      VLXLS-GET-PROPERTY
			      (list range "Orientation")
			    )
			    -4166
			 )
		  )
		  (if TextVerFlag
		    (progn
		      (setq text (gxl-str->singleonly text))
		      (setq tmp	 (car text)
			    text (cdr text)
		      )
		      (foreach a text (setq tmp (strcat tmp "\\P" a)))
		      (setq text tmp)
		    )
		  )
		  ;;逐字取样式
		  ;;(setq textFont (strcat textFont (GetRangeTextStyle RANGE RANGEFONT text) "}"))

		  (setq text (strcat textFont text "}"))

		)
	      )
	      (cond ((null OldRow) (setq OldRow Row))
		    ((/= OldRow Row) ;_ 换行
		     (if *pageSetUp*
		       (progn
			 (if nil	;(member row HPageBreaks) ;_ 换页
			   (progn
			     (setq OldRow Row
				   StartPoint
				    (polar StartPoint
					   (* 1.5 pi)
					   oldheight
				    )
			     )
			     (if *Merge*
			       (progn
				 (entmake
				   (list
				     '(0 . "line")
				     '(100 . "AcDbEntity")
				     '(67 . 0)
				     (cons 8 Blayer)
				     (cons 62 *defaultColor*)
				     '(100 . "AcDbLine")
				     (cons 10
					   StartPoint

				     )
				     (cons
				       11
				       (setq p0
					      (polar
						StartPoint
						0
						(* defaultHeight
						   GridScale
						   Totalwide
						)
					      )
				       )
				     )
				     '(210 0.0 0.0 1.0)
				   )
				 )
				 (entmake
				   (list
				     '(0 . "line")
				     '(100 . "AcDbEntity")
				     '(67 . 0)
				     (cons 8 Blayer)
				     (cons 62 *defaultColor*)
				     '(100 . "AcDbLine")
				     (cons 10 RightTopPt)
				     (cons 11 p0)
				     '(210 0.0 0.0 1.0)
				   )
				 )

			       )
			     )
			     (setq StartPoint (polar StartPoint
						     (* 1.5 pi)
						     PageMargin
					      )
				   Curpt      StartPoint
				   RightTopPt (polar StartPoint
						     0
						     (*	defaultHeight
							GridScale
							Totalwide
						     )
					      )
			     ) ;_ 移动页间距
			   )
			   (setq OldRow	    Row
				 StartPoint (polar StartPoint
						   (* 1.5 pi)
						   oldheight
					    )
				 Curpt	    StartPoint
			   )
			 )
		       )
		       (setq OldRow	Row
			     StartPoint	(polar StartPoint (* 1.5 pi) oldheight)
			     Curpt	StartPoint
		       )
		     )


		    )
	      )

	      (setq oldheight height)
	      (if (not (equal width 0 0.01))
		(progn
		  (if Mergep
		    (progn
		      (setq mergeId (mapcar 'vlxls-rangeid
					    (vlxls-cellid
					      (vlxls-range-getid range)
					    )
				    )
			    width1  (* defaultHeight
				       GridScale
				       (VLXLS-GET-PROPERTY
					 range
					 "MergeArea.width"
				       )
				    )
			    height1 (* defaultHeight
				       GridScale
				       (VLXLS-GET-PROPERTY
					 range
					 "MergeArea.height"
				       )
				    )
		      )
		    )
		    (setq width1  width
			  height1 height
		    )
		  )
		  (if (and *RowHeight* (> *RowHeight* 0) Mergep (> (vlax-variant-value (vlax-get-property cell 'height)) 0))
		    (setq height1 (* *RowHeight* (/ (VLXLS-GET-PROPERTY range "MergeArea.height") (vlax-variant-value (vlax-get-property cell 'height)))))
		  )
		  (if
		    (or	(not Mergep)
			(and Mergep (equal (car mergeId) (list col row)))
		    )
		     (progn
		       (setq p0	(polar Curpt (* 1.5 pi) height1)
			     p1	Curpt
			     p2	(polar Curpt 0 width1)
			     p3	(polar p2 (* 1.5 pi) height1)
		       ) ;_ 框的四个角点 左下、左上、右上、右下
		       (if *Merge*
			 (progn
			   (if Horline
			     (progn
			       (if (equal p1 (gxl-dxf HorLine 11) 1e-3)
				 (gxl-ch_ent HorLine 11 p2) ;_ 更新水平直线末端点
				 (progn
				   (entmake
				     (list
				       '(0 . "line")
				       '(100 . "AcDbEntity")
				       '(67 . 0)
				       (cons 8 Blayer)
				       (cons 62 *defaultColor*)
				       '(100 . "AcDbLine")
				       (cons 10 p1)
				       (cons 11 p2)
				       '(210 0.0 0.0 1.0)
				     )
				   )
				   (setq Horline (entlast))
				 )
			       )
			     )
			     (progn
			       (entmake
				 (list
				   '(0 . "line")
				   '(100 . "AcDbEntity")
				   '(67 . 0)
				   (cons 8 Blayer)
				   (cons 62 *defaultColor*)
				   '(100 . "AcDbLine")
				   (cons 10 p1)
				   (cons 11 p2)
				   '(210 0.0 0.0 1.0)
				 )
			       )
			       (setq Horline (entlast))
			     )
			   )
			   (if VerLines
			     (progn
			       (if (not
				     (vl-some
				       (Function
					 (lambda (Line)
					   (if (equal p1
						      (gxl-dxf Line 11)
						      1e-3
					       )
					     (gxl-ch_ent Line 11 p0) ;_ 更新垂直直线末端点
					   )
					 )
				       )
				       VerLines
				     )
				   )
				 (progn
				   (entmake
				     (list
				       '(0 . "line")
				       '(100 . "AcDbEntity")
				       '(67 . 0)
				       (cons 8 Blayer)
				       (cons 62 *defaultColor*)
				       '(100 . "AcDbLine")
				       (cons 10 p1)
				       (cons 11 p0)
				       '(210 0.0 0.0 1.0)
				     )
				   )
				   (setq
				     VerLines (cons (entlast) VerLines)
				   )
				 )
			       )
			     )
			     (progn
			       (entmake
				 (list
				   '(0 . "line")
				   '(100 . "AcDbEntity")
				   '(67 . 0)
				   (cons 8 Blayer)
				   (cons 62 *defaultColor*)
				   '(100 . "AcDbLine")
				   (cons 10 p1)
				   (cons 11 p0)
				   '(210 0.0 0.0 1.0)
				 )
			       )
			       (setq VerLines (cons (entlast) VerLines))
			     )
			   )
			 )
			 (entmake
			   (list
			     '(0 . "LWPOLYLINE")
			     '(100 . "AcDbEntity")
			     '(67 . 0)
			     (cons 8 BLayer)
			     (cons 62 *defaultColor*)
			     '(100 . "AcDbPolyline")
			     '(90 . 4)
			     '(70 . 1)
			     '(43 . 0.0)
			     '(38 . 0.0)
			     '(39 . 0.0)
			     (cons 10 p0)
			     (cons 10 p1)
			     (cons 10 p2)
			     (cons 10 p3)
			     '(210 0.0 0.0 1.0)
			   )
			 )
		       )
		       (if *CellColor* ;_ 绘制背景颜色
			 (progn
			   (if (/= -4142
				   (setq Interiorcolor
					  (VLXLS-GET-PROPERTY
					    range
					    "Interior.ColorIndex"
					  )
				   )
			       )
			     (progn
			       (setq Interiorcolor     (VLXLS-COLOR-ECI->ACI
							 Interiorcolor
						       )
				     Interiortruecolor (VLXLS-COLOR-ECI->TRUECOLOR
							 Interiorcolor
						       )
			       )
			       (entmake
				 (vl-remove
				   nil
				   (list
				     '(0 . "SOLID")
				     '(100 . "AcDbEntity")
				     '(67 . 0)
				     (cons 8 BLayer)
				     (cons 62 Interiorcolor)
				     ;|(if (not (or (= 256 Interiorcolor)
				   (= 0 Interiortruecolor)
			       )
			  )
			(cons 420 Interiortruecolor)
		      )|;
				     '(100 . "AcDbTrace")
				     (cons 10 p0)
				     (cons 11 p1)
				     (cons 12 p3)
				     (cons 13 p2)
				     '(210 0.0 0.0 1.0)
				   )
				 )
			       )
                               ;(setq SSSolid (cons (entlast) SSSolid))
			     )
			   )
                           
			 )
		       )
		       (if (/= "" text)
			 (progn
			   (setq textpt
				  (nth (1- DXF71) (Get9JustPts p0 p2))
			   )
			   (cond ((= 0 HorizontalAlignment) ;_ 左对齐
				  (setq
				    textpt (polar textpt 0 (* height 0.1))
				  )
				 )
				 ((= 2 HorizontalAlignment) ;_ 右对齐
				  (setq
				    textpt (polar textpt pi (* height 0.1))
				  )
				 )
			   )
			   (cond
			     ((= 0 VerticalAlignment) ;_ 上对齐
			      (setq textpt (polar textpt
						  (* 1.5 pi)
						  (* height 0.1)
					   )
			      )
			     )
			     ((= 2 VerticalAlignment) ;_ 下对齐
			      (setq textpt (polar textpt
						  (* 0.5 pi)
						  (* height 0.1)
					   )
			      )
			     )
			   )
			   (entmake
			     (vl-remove
			       nil
			       (list
				 (cons 0 "MTEXT")
				 '(100 . "AcDbEntity")
				 '(67 . 0)
				 (cons 8 TLayer)
				 (if *AnnoColor*
				   (cons 62 dxf62)
				   (cons 62 *defaultColor*)
				 )
				 '(100 . "AcDbMText")
				 (cons 10 textpt)
				 (cons 40 textHeight)
				 (cons 41 width1)
					;(cons 50 0)
				 ;;'(46 . 0.0)
				 (cons 71 DXF71)
				 (cons 72 5)
				 (cons 1 text)
				 (cons 7 "Standard")
				 '(210 0.0 0.0 1.0)
				 '(11 1.0 0.0 0.0)
				 '(50 . 0.0)
				 '(73 . 1)
			       )
			     )
			   )
			 )
		       )
		     )
		  )
		  (setq Curpt (polar Curpt 0 width))
		)
	      )
	    ) ;_ vlax-for
	    
	  )
	(setq startpoint (polar startpoint (* 1.5 pi) oldheight) curpt startpoint)
      )
    )
   
  )
  ;;Range的text逐字取样式
  (defun GetRangeTextStyle (RANGE RANGEFONT text	    /	     I
				  II	   CHAR	    CFONT    CAPTION
				  CHARFONT F	    TEXTFONT
				 )
   (if (equal :vlax-false (vlxls-get-property range "HasFormula"))
	  (progn
	    (setq i  0
		  ii (GXL-CATCHAPPLY
		       vlax-get-property
		       (list (vlax-get-property range 'characters) 'count)
		     )
	    )
	    (if	ii
	      (repeat ii
		(setq char  (vlax-get-property
			      range
			      'characters
			      (setq i (1+ i))
			      1
			    )
		      cfont (vlax-get-property char 'font)
		      caption (VLXLS-GET-PROPERTY char "caption")
		)
		(setq charFont
		       (mapcar
			 '(lambda (x) (cons x (VLXLS-GET-PROPERTY cfont x)))
			 '("NAME"	  "SIZE"	 "COLORINDEX"
			   "BOLD"	  "ITALIC"	 "SUBSCRIPT"
			   "SUPERSCRIPT"  "UNDERLINE"
			  )
		       )
		)
		(if (and (setq f (cdr (assoc "NAME" charFont)))
			 (/= f (cdr (assoc "NAME" RangeFont)))
			 )
		  (setq textfont (strcat "\\f" f  "|b0|i0|c134|p0;"))
		  ) ;_ 字体
		(if (and (setq f (cdr (assoc "SIZE" charFont)))
			 (equal f (cdr (assoc "SIZE" RangeFont)) 0.01)
			 )
		  (setq textfont (strcat textFont "\\H" (rtos f 2 1) "x;"))
		  ) ;_ 大小
		(if (and (setq f (cdr (assoc "COLORINDEX" charFont)))
			 (equal f (cdr (assoc "COLORINDEX" RangeFont)) 0.01)
			 )
		  (setq textfont (strcat textFont "\\C" (itoa (vlxls-color-eci->aci f)) ";"))
		  ) ;_ 颜色
		;;加粗
		(if (not (equal	(setq f (cdr (assoc "BOLD" charFont)))
				(cdr (assoc "BOLD" RangeFont))
			 )
		    )
		  (if (equal :vlax-true f)
		    (setq textfont (strcat textFont "\\W1.2;"))
		    (setq textfont (strcat textFont "\\W0.83;"))
		  )
		)
		;;倾斜
		(if
		  (not (equal (setq f (cdr (assoc "ITALIC" charFont)))
			      (cdr (assoc "ITALIC" RangeFont))
		       )
		  )
		   (if (equal :vlax-true f)
		     (setq textfont (strcat textFont "\\Q18;"))
		     (setq textfont (strcat textFont "\\Q0;"))
		   )
		)
		;;上标
		(if (equal :vlax-true (cdr (assoc "SUPERSCRIPT" RangeFont)))
		    (setq textFont (strcat textFont "\\H0.33x;\\A2;"))
		    )
		;;下标
		(if (equal :vlax-true (cdr (assoc "SUPERSCRIPT" RangeFont)))
		    (setq textFont (strcat textFont "\\H0.33x;\\A0;"))
		    )
		;;下划线
		(if
		  (not (equal (setq f (cdr (assoc "UNDERLINE" charFont)))
			      (cdr (assoc "UNDERLINE" RangeFont))
		       )
		  )
		   (if (= 2 f)
		     (setq textfont (strcat textFont "\\L"))
		     (setq textfont (strcat textFont "\\l"))
		   )
		)
               (setq textFont (strcat textFont caption))
	       (if (and TextVerFlag (/= i ii)) (setq textFont (strcat textFont "\\P")))
	      )
	      (setq textfont (strcat textfont text))
	    )
	  )
	  (setq textfont (strcat textfont text))
	)
    )
  ;;绘制页眉页脚 PageSetUp vla对象 pt 表格基点 Flag = t 页眉 = nil 页脚
  (defun DrawPageSetUp (PAGESETUP PT	      FLAG	  /
				  GETFONTSTR  LEFTHEADER  CENTERHEADER
				  RIGHTHEADER D		  TEXTPT
			 LeftFooter CenterFooter RightFooter
				 )
; PageSetup:特性值:
;   AlignMarginsHeaderFooter = 0
;   Application (RO) = #<VLA-OBJECT _Application 0cdd3e9c>
;   BlackAndWhite = 0
;   BottomMargin = 70.8661
;   CenterFooter = "&\"幼圆,加粗\"&16页脚中&N第&P页"
;   CenterFooterPicture (RO) = #<VLA-OBJECT Graphic 1821ca84>
;   CenterHeader = "页眉中"
;   CenterHeaderPicture (RO) = #<VLA-OBJECT Graphic 1821d5c4>
;   CenterHorizontally = 0
;   CenterVertically = 0
;   Creator (RO) = 1480803660
;   DifferentFirstPageHeaderFooter = 0
;   Draft = 0
;   EvenPage (RO) = #<VLA-OBJECT Page 1821c454>
;   FirstPage (RO) = #<VLA-OBJECT Page 1821ddec>
;   FirstPageNumber = -4105
;   FitToPagesTall = 1
;   FitToPagesWide = 1
;   FooterMargin = 36.8504
;   HeaderMargin = 36.8504
;   LeftFooter = "&\"楷体,常规\"&14页&\"楷体,加粗 倾斜\"脚&\"楷体,常规\"左"
;   LeftFooterPicture (RO) = #<VLA-OBJECT Graphic 1821df54>
;   LeftHeader = "页眉左"
;   LeftHeaderPicture (RO) = #<VLA-OBJECT Graphic 1821c724>
;   LeftMargin = 53.8583
;   OddAndEvenPagesHeaderFooter = 0
;   Order = 1.0
;   Orientation = 1.0
;   Pages (RO) = #<VLA-OBJECT Pages 1821c0ac>
;   PaperSize = 9.0
;   Parent (RO) = #<VLA-OBJECT _Worksheet 1821dad4>
;   PrintArea = "$A$1:$N$105"
;   PrintComments = -4142
;   PrintErrors = 0
;   PrintGridlines = 0
;   PrintHeadings = 0
;   PrintNotes = 0
;   PrintQuality = ...不显示带索引的内容...
;   PrintTitleColumns = ""
;   PrintTitleRows = "$1:$3"
;   RightFooter = "&\"楷体,加粗\"&KFF0000页脚右"
;   RightFooterPicture (RO) = #<VLA-OBJECT Graphic 1821dccc>
;   RightHeader = "页眉右"
;   RightHeaderPicture (RO) = #<VLA-OBJECT Graphic 1821c8d4>
;   RightMargin = 53.8583
;   ScaleWithDocHeaderFooter = -1
;   TopMargin = 70.8661
;   Zoom = 100
    (defun GetFontstr (str / size fontname fontstr color)
      ;;用正则表达式删除格式文字
      ;;"&\"幼圆,加粗\"&16页脚&\"楷体,加粗倾斜\"&12&KFFFF00中共&\"幼圆,加粗\"&16&K000000&N页 第&P页"
      (setq fontname
	     (gxl-RegExSearch
	       str
	       "\&\\\".+?\""
	       "im"
	       )
	    )
      (if fontname
	(progn
	  (setq fontname (caddar fontname))
	  (setq fontname
		(gxl-RegExRePlace
		  fontname
		  ""
		  "&\\\"|\\\""
		  "mg"
		  )
		)
	  (setq fontname (GXL-STRPARSE fontname ","))
	  (setq fontstr (strcat "{\\f" (car fontname) "|b0|i0|c134|p0;"))
	  (if (cadr fontname)
	    (progn
	      (if (WCMATCH (cadr fontname) "*加粗*") 
		(setq fontstr (strcat fontstr "\\W1.2;"))
		)
	      (if (WCMATCH (cadr fontname) "*倾斜*") 
		(setq fontstr (strcat fontstr "\\Q18;"))
		)
	      
	      )
	    )
	  )
	(setq fontstr (strcat "{\\f" standardFont "|b0|i0|c134|p0;" "\\H" (rtos StandardFontSize 2 1) "x;"))
	)
      (setq size
	     (gxl-RegExSearch
	       str
	       "&\\d{1,2}"
	       "im"
	       )
	    )
      (if size
	(progn
	  (setq size (caddar size))
	  (setq size
		(gxl-RegExRePlace
		  size
		  ""
		  "&"
		  "mg"
		  )
		)
	  (setq fontstr (strcat fontstr "\\H" size "x;"))
	  )
	)
      (if *AnnoColor*
	(progn
	  (setq	color
		 (gxl-RegExSearch
		   str
		   "\&K[A-Za-z0-9]{6}"
		   "im"
		 )
	  )
	  (if color
	    (progn
	      (setq color (strcat "#" (substr (caddar color) 3)))
	      (setq color (gxl-Hex->ACI color))
	      (setq fontstr (strcat fontstr "\\C" (itoa color) ";"))
	    )
	  )
	)
	(setq fontstr (strcat fontstr "\\C" (itoa *defaultColor*) ";"))
      )

	  
      (setq str
	     (gxl-RegExRePlace
	       str
	       ""
	       "&\\d{1,2}|\&\\\".+?\"|\&K[A-Za-z0-9]{6}"
	       "mg"
	     )
      )
      (setq str
	     (gxl-RegExRePlace
	       str
	       (itoa TotalPage)
	       "&N"
	       "mg"
	     )
      )
      (setq str
	     (gxl-RegExRePlace
	       str
	       (itoa Page)
	       "&P"
	       "mg"
	     )
      )
      ;(strcat "{\\f" standardFont "|b0|i0|c134|p0;" "\\H" (rtos StandardFontSize 2 1) "x;" str"}")
      (strcat fontstr str "}")
      )
    (cond
      (flag ;_ 页眉
       (setq LeftHeader (vlax-get-property PageSetUp 'LeftHeader)
	     CenterHeader (vlax-get-property PageSetUp 'CenterHeader)
	     RightHeader (vlax-get-property PageSetUp 'RightHeader)
	     )
       (if (/= "" LeftHeader)
	 (progn
	   (setq d (* defaultHeight GridScale (vlax-get-property PageSetUp 'HeaderMargin)))
	   (setq textpt (polar pt (* pi 0.5) d))
	   (setq LeftHeader (GetFontstr LeftHeader))
	   (entmake
	      (vl-remove
		nil
		(list
		  (cons 0 "MTEXT")
		  '(100 . "AcDbEntity")
		  '(67 . 0)
		  (cons 8 TLayer)
		  (cons 62 *defaultColor*)
		  '(100 . "AcDbMText")
		  (cons 10 textpt)
		  (cons 40 textHeight)
		  (cons 41 (* 0.333 totalwide GridScale defaultHeight))
		  (cons 71 4)
		  (cons 72 5)
		  (cons 1 LeftHeader)
		  (cons 7 "Standard")
		  '(210 0.0 0.0 1.0)
		  '(11 1.0 0.0 0.0)
		  '(50 . 0.0)
		  '(73 . 1)
		)
	      )
	    )
	   )
	 )
       (if (/= "" CenterHeader)
	 (progn
	   (setq d (* defaultHeight GridScale (vlax-get-property PageSetUp 'HeaderMargin)))
	   (setq textpt (polar (polar pt (* pi 0.5) d) 0 (* 0.5 totalwide GridScale defaultHeight)))
	   (setq CenterHeader (GetFontstr CenterHeader))
	   (entmake
	      (vl-remove
		nil
		(list
		  (cons 0 "MTEXT")
		  '(100 . "AcDbEntity")
		  '(67 . 0)
		  (cons 8 TLayer)
		  (cons 62 *defaultColor*)
		  '(100 . "AcDbMText")
		  (cons 10 textpt)
		  (cons 40 textHeight)
		  (cons 41 (* 0.333 totalwide GridScale defaultHeight))
		  (cons 71 5)
		  (cons 72 5)
		  (cons 1 CenterHeader)
		  (cons 7 "Standard")
		  '(210 0.0 0.0 1.0)
		  '(11 1.0 0.0 0.0)
		  '(50 . 0.0)
		  '(73 . 1)
		)
	      )
	    )
	   )
	 )
       (if (/= "" RightHeader)
	 (progn
	   (setq d (* defaultHeight GridScale (vlax-get-property PageSetUp 'HeaderMargin)))
	   (setq textpt (polar (polar pt (* pi 0.5) d) 0 (* totalwide GridScale defaultHeight)))
	   (setq RightHeader (GetFontstr RightHeader))
	   (entmake
	      (vl-remove
		nil
		(list
		  (cons 0 "MTEXT")
		  '(100 . "AcDbEntity")
		  '(67 . 0)
		  (cons 8 TLayer)
		  (cons 62 *defaultColor*)
		  '(100 . "AcDbMText")
		  (cons 10 textpt)
		  (cons 40 textHeight)
		  (cons 41 (* 0.333 totalwide GridScale defaultHeight))
		  (cons 71 6)
		  (cons 72 5)
		  (cons 1 RightHeader)
		  (cons 7 "Standard")
		  '(210 0.0 0.0 1.0)
		  '(11 1.0 0.0 0.0)
		  '(50 . 0.0)
		  '(73 . 1)
		)
	      )
	    )
	   )
	 )
       )
      (t ;_ 页脚
       (setq LeftFooter (vlax-get-property PageSetUp 'LeftFooter)
	     CenterFooter (vlax-get-property PageSetUp 'CenterFooter)
	     RightFooter (vlax-get-property PageSetUp 'RightFooter)
	     d (* defaultHeight GridScale (vlax-get-property PageSetUp 'FooterMargin))
	     )
       (if (/= "" LeftFooter)
	 (progn
	   (setq textpt (polar pt (* pi 1.5) d))
	   (setq LeftFooter (GetFontstr LeftFooter))
	   (entmake
	      (vl-remove
		nil
		(list
		  (cons 0 "MTEXT")
		  '(100 . "AcDbEntity")
		  '(67 . 0)
		  (cons 8 TLayer)
		  (cons 62 *defaultColor*)
		  '(100 . "AcDbMText")
		  (cons 10 textpt)
		  (cons 40 textHeight)
		  (cons 41 (* 0.333 totalwide GridScale defaultHeight))
		  (cons 71 4)
		  (cons 72 5)
		  (cons 1 LeftFooter)
		  (cons 7 "Standard")
		  '(210 0.0 0.0 1.0)
		  '(11 1.0 0.0 0.0)
		  '(50 . 0.0)
		  '(73 . 1)
		)
	      )
	    )
	   )
	 )
       (if (/= "" CenterFooter)
	 (progn
	   (setq textpt (polar (polar pt (* pi 1.5) d) 0 (* 0.5 totalwide GridScale defaultHeight)))
	   (setq CenterFooter (GetFontstr CenterFooter))
	   (entmake
	      (vl-remove
		nil
		(list
		  (cons 0 "MTEXT")
		  '(100 . "AcDbEntity")
		  '(67 . 0)
		  (cons 8 TLayer)
		  (cons 62 *defaultColor*)
		  '(100 . "AcDbMText")
		  (cons 10 textpt)
		  (cons 40 textHeight)
		  (cons 41 (* 0.333 totalwide GridScale defaultHeight))
		  (cons 71 5)
		  (cons 72 5)
		  (cons 1 CenterFooter)
		  (cons 7 "Standard")
		  '(210 0.0 0.0 1.0)
		  '(11 1.0 0.0 0.0)
		  '(50 . 0.0)
		  '(73 . 1)
		)
	      )
	    )
	   )
	 )
       (if (/= "" RightFooter)
	 (progn
	   (setq textpt (polar (polar pt (* pi 1.5) d) 0 (* totalwide GridScale defaultHeight)))
	   (setq RightFooter (GetFontstr RightFooter))
	   (entmake
	      (vl-remove
		nil
		(list
		  (cons 0 "MTEXT")
		  '(100 . "AcDbEntity")
		  '(67 . 0)
		  (cons 8 TLayer)
		  (cons 62 *defaultColor*)
		  '(100 . "AcDbMText")
		  (cons 10 textpt)
		  (cons 40 textHeight)
		  (cons 41 (* 0.333 totalwide GridScale defaultHeight))
		  (cons 71 6)
		  (cons 72 5)
		  (cons 1 RightFooter)
		  (cons 7 "Standard")
		  '(210 0.0 0.0 1.0)
		  '(11 1.0 0.0 0.0)
		  '(50 . 0.0)
		  '(73 . 1)
		)
	      )
	    )
	   )
	 )
       
       )
      )
    )
;;主程序开始
  (setierr)
(setq Layers (gxl-table "layer"))
  (if *EVE-SKIP-DIALOG*
    (progn
      ;;跳过对话框, 直接从环境变量读取设置
      (setq *DrawRange* (getenv "Excel2CAD\\DrawRange"))
      (if (null *DrawRange*) (setq *DrawRange* "Used"))
      (setq BLayer (nth 0 layers)
            TLayer (nth 0 layers))
      (setq *CellColor* (= "1" (getenv "Excel2CAD\\CellColor")))
      (setq *AnnoColor* (= "1" (getenv "Excel2CAD\\AnnoColor")))
      (setq *Oprate* (getenv "Excel2CAD\\Oprate"))
      (if *Oprate* (setq *Oprate* (atoi *Oprate*)) (setq *Oprate* 0))
      (setq *Merge* (= "1" (getenv "Excel2CAD\\Merge")))
      (setq *THeight* (getenv "Excel2CAD\\THeight"))
      (if (null *THeight*) (setq *THeight* 300.0) (setq *THeight* (atof *THeight*)))
      (setq *RowHeight* (getenv "Excel2CAD\\RowHeight"))
      (if (null *RowHeight*) (setq *RowHeight* 0.0) (setq *RowHeight* (atof *RowHeight*)))
      (setq *TableWidth* (getenv "Excel2CAD\\TableWidth"))
      (if (null *TableWidth*) (setq *TableWidth* 0.0) (setq *TableWidth* (atof *TableWidth*)))
      (setq *BasePointPos* (getenv "Excel2CAD\\BasePoint"))
      (if (null *BasePointPos*) (setq *BasePointPos* 0) (setq *BasePointPos* (atoi *BasePointPos*)))
      (setq *KeepTHeight* (= "1" (getenv "Excel2CAD\\KeepTHeight")))
      (setq *pageSetUp* (= "1" (getenv "Excel2CAD\\pageSetUp")))
      (setq *defaultColor* (getenv "Excel2CAD\\defaultColor"))
      (if (null *defaultColor*) (setq *defaultColor* 0) (setq *defaultColor* (atoi *defaultColor*)))
      (if (/= 0 *defaultColor*) (setq *defaultColor* 256))
      (setq ecode 1)
      (setq *EVE-SKIP-DIALOG* nil)
    )
    (progn
      ;;对话框开始
      (setq dclcode (load_dialog (mkTmpDcl "xl2cad")))
      (new_dialog "xl2cad" dclcode)
      (start_xl2x)
      (setq ecode (start_dialog))
    )
  )
  (cond
    ((= 1 ecode)
  (if *CellColor* (setvar "REGENMODE" 0))
 (vlxls-app-init)
 (or *xlapp*
     (if (VL-CATCH-ALL-ERROR-P
	   (setq *xlApp* (VL-CATCH-ALL-APPLY
			   'vlax-get-or-create-object
			   '("Excel.Application")
			 )
	   )
	 )
       (exit)
     )
 )
 (if (equal :vlax-false (vlax-get-property *XLAPP* 'visible))
    (vla-put-visible *xlApp* 1)
  )
 (if (= "User" *DrawRange*)
     (vlax-put-property *XLAPP* 'Visible 1)
 )
  (setq workbooks (vlax-get-property *xlApp* 'workbooks))
  (if (= 0 (vla-get-Count workbooks))
    (setq workbook (vlax-invoke workbooks 'add))
    (setq workbook (vlax-get-property *xlApp* 'activeworkbook))
  )
  (setq sheet (vlax-get-property *xlApp* 'activesheet))
  (setq	UsedRange (vlax-get-property sheet 'UsedRange)
	col	  (vlax-get-property
		    (vlax-get-property UsedRange 'columns)
		    'count
		  )
	row	  (vlax-get-property
		    (vlax-get-property UsedRange 'rows)
		    'count
		  )
  )
  (cond
    ((= "Used" *DrawRange*)
     (setq Cells (vlax-get-property UsedRange 'Cells))
     ;(setq PrintArea (VLXLS-GET-PROPERTY *xlApp* "Activesheet.PageSetup.PrintArea"))
    )
    ((= "User" *DrawRange*)
     (setq Selection (vlax-get-property *xlApp* 'Selection)
	   Cells     (vlax-get-property Selection 'Cells)
     )
     (if (and Selection Cells)
       (progn
	 (setq sel-rows (vlax-get-property (vlax-get-property Selection 'Rows) 'Count)
	       sel-cols (vlax-get-property (vlax-get-property Selection 'Columns) 'Count)
	       sheet-rows (vlax-get-property (vlax-get-property sheet 'Rows) 'Count)
	       sheet-cols (vlax-get-property (vlax-get-property sheet 'Columns) 'Count)
	 )
	 (if (or (>= sel-rows sheet-rows) (>= sel-cols sheet-cols))
	   (progn
	     (alert "检测到您选择了整行、整列或整张表格！\n\n这将导致生成大量空单元格，耗时长且无意义。\n\n请仅选择有数据的单元格区域后重试。")
	     (exit)
	   )
	 )
       )
     )
     ;(setq PrintArea (VLXLS-GET-PROPERTY *xlApp* "Activesheet.PageSetup.PrintArea"))
    )
    ((= "PrintArea" *DrawRange*)
     (setq PrintArea (VLXLS-GET-PROPERTY *xlApp* "Activesheet.PageSetup.PrintArea"))
     (if (/= "" PrintArea)
       (setq Cells (vlax-get-property
		     (vlax-get-property *xlApp* 'range PrintArea)
		     'Cells
		   )
       )
       (progn
	 (alert "当前活动表格没有可打印的页面！\n\n请重新设置页面！程序将退出！")
	 (exit)
	 )
     )
    )
  )

  (if *pageSetUp*
    (progn
      (setq HPageBreaks nil)
      (GXL-CATCHAPPLY
	(lambda ()
	   (vlax-for a (VLXLS-GET-PROPERTY
			 *xlapp*
			 "activesheet.HPageBreaks"
		       )
	     (setq HPageBreaks
		    (cons (VLXLS-GET-PROPERTY a "Location.row")
			  HPageBreaks
		    )
	     )
	   )
	 )
	nil
      )
      (setq HPageBreaks (reverse HPageBreaks)) ;_ 储存分页的Row位置
    )
  )
  (if *EVE-PRESET-PT*
    (setq StartPoint *EVE-PRESET-PT*
          BasePoint StartPoint
          *EVE-PRESET-PT* nil)
    (progn
      (initget 7)
      (setq StartPoint (getpoint "\n放置位置:"))
      (setq StartPoint (trans StartPoint 1 0)
            BasePoint StartPoint)))
  (setq	curpt  StartPoint
	OldRow nil
	to (vlax-get-property cells 'count)
  )
    ;|71 
	 附着点：
	1 = 左上；2 = 中上；3 = 右上
	4 = 左中；5 = 正中；6 = 右中
	7 = 左下；8 = 中下；9 = 右下 
   |;
  (setq dxf71data '((1 4 7) (2 5 8) (3 6 9)))
  (GXL-SYS-PROGRESS-INIT "" to)
  (setq StandardFont (vlax-get-property *xlApp* 'StandardFont))
  (setq StandardFontSize (vlax-get-property *XLAPP* 'StandardFontSize))
  (setq defaultHeight (/ *THeight* StandardFontSize)) ;_ 默认高度
  (setq textHeight defaultHeight)
  (setq GridScale 1.941747572815534)
  (setq totalwide (vlax-variant-value (vlax-get-property cells 'Width))
            totalheight (vlax-variant-value (vlax-get-property cells 'height))
            )
  (setq pagesetup (VLXLS-GET-PROPERTY *xlapp* "activesheet.pagesetup"))
     (if (and *TableWidth* (> *TableWidth* 0))
    (progn
      (setq scale (/ *TableWidth* (* totalwide GridScale defaultHeight)))
      (setq defaultHeight (* defaultHeight scale))
    )
    (if (not *KeepTHeight*)
    (progn      
      (entmake
          (list
            '(0 . "LWPOLYLINE")
            '(100 . "AcDbEntity")
            '(67 . 0)
            '(100 . "AcDbPolyline")
            '(90 . 4)
            '(70 . 1)
            '(43 . 0.0)
            '(38 . 0.0)
            '(39 . 0.0)
            (cons 10 StartPoint)
            (cons 10
                  (setq p0 (polar StartPoint
                                  0
                                  (* totalwide GridScale defaultHeight)
                                  )
                        )
                  )
            (cons 10
                  (polar p0
                         (* 1.5 pi)
                         (* totalheight GridScale defaultHeight)
                         )
                  )
            (cons 10
                  (polar StartPoint
                         (* 1.5 pi)
                         (* totalheight GridScale defaultHeight)
                         )
                  )
            '(210 0.0 0.0 1.0)
            )
          )
      (setq endent (entlast))
      (setvar 'ORTHOMODE 1)
      (initget 6)
      (setq p0 (getdist (trans StartPoint 0 1) (strcat "\n输入表格宽度<" (rtos (* totalwide GridScale defaultHeight) 2 2) ">:")))
      (if (null p0) (setq p0 (* totalwide GridScale defaultHeight)))
      (entdel endent)
      (setq scale (/ p0 (* totalwide GridScale defaultHeight)))
      (setq defaultHeight (* defaultHeight scale))  
     )
    )
    )
     ;;根据基点位置偏移StartPoint
     ;;计算实际表格高度（考虑*RowHeight*覆盖）
     (if (and *RowHeight* (> *RowHeight* 0))
       (setq tblHeight (* *RowHeight* (vlax-get-property (vlax-get-property cells 'Rows) 'Count)))
       (setq tblHeight (* totalheight GridScale defaultHeight))
     )
     (cond
       ((= *BasePointPos* 1) ;_ 左下
        (setq StartPoint (polar StartPoint (* 0.5 pi) tblHeight))
        )
       ((= *BasePointPos* 2) ;_ 右上
        (setq StartPoint (polar StartPoint pi (* totalwide GridScale defaultHeight)))
        )
       ((= *BasePointPos* 3) ;_ 右下
        (setq StartPoint (polar StartPoint pi (* totalwide GridScale defaultHeight)))
        (setq StartPoint (polar StartPoint (* 0.5 pi) tblHeight))
        )
     )
     (setq curpt StartPoint
           BasePoint StartPoint)
     (setq endent (entlast) )
     (setq page 1 TotalPage (1+ (length HPageBreaks)))
     (if *pageSetUp*
       (progn
	 (setq PageMargin  ;_ 计算页间距
		(* defaultHeight
		   GridScale
		   (+
		   (VLXLS-GET-PROPERTY
		     *xlapp*
		     "activesheet.pagesetup.BottomMargin"
		   )
		   (VLXLS-GET-PROPERTY
		     *xlapp*
		     "activesheet.pagesetup.FooterMargin"
		   )
		   (VLXLS-GET-PROPERTY
		     *xlapp*
		     "activesheet.pagesetup.TopMargin"
		   )
		   (VLXLS-GET-PROPERTY
		     *xlapp*
		     "activesheet.pagesetup.HeaderMargin"
		   )
		   )
		)
	 )
	 (setq RightTopPt  ;_ 每页右上角点
		(polar StartPoint
		       0
		       (* defaultHeight GridScale Totalwide)
		)
	 )
	 ;;输出页眉
	 (if *pageSetUp* (DrawPageSetUp pagesetup StartPoint t))
       )
     )
  ;;输出表头
  (if *pageSetUp* (PrintTitleRows cells))
  (if (setq TitleRows ;_ 存储表头的行数
	     (VLXLS-GET-PROPERTY
	       *XLAPP*
	       "ActiveSheet.PageSetup.PrintTitleRows"
	     )
      )
    (progn
      (setq TitleRows
	     (mapcar
	       'atoi
	       (vl-remove ""
			  (GXL-STRPARSEBYLST TitleRows '(":" "$"))
	       )
	     )
	    tmp (car TitleRows)
	    tmp1 (cadr TitleRows)
	    TitleRows nil
      )
      (if tmp1
	(while (<= tmp tmp1)
	  (setq TitleRows (cons tmp TitleRows)
		tmp (1+ tmp)
		)
	  )
	(setq TitleRows (list tmp))
	)
      (setq TitleRows (reverse TitleRows))
    )
  )

    
  
   ;;逐行逐列绘制表格
  (vlax-for cell cells
    (gxl-Sys-Progress to -1)
    (setq col	 (vlax-get-property cell 'column)
	  row	 (vlax-get-property cell 'row)
          range (msxlp-get-range *xlApp* (VLXLS-RANGEID (list col row)))
          Mergep (equal :vlax-true (vlax-variant-value (vlax-get-property cell 'MergeCells)))
	  width	 (* defaultHeight GridScale (vlax-variant-value (vlax-get-property cell 'width)))
	  height (* defaultHeight GridScale (vlax-variant-value (vlax-get-property cell 'height)))
	  text	 (vlax-variant-value (vlax-get-property cell 'text))
          )
    (if (and *RowHeight* (> *RowHeight* 0)) (setq height *RowHeight*))
    (cond
      ((and *pageSetUp* (member row TitleRows))) ;_ 忽略打印表头位置的表格
      (t
    (if	(and (/= text "")
	     (not (equal width 0 0.01))
	)
      (progn
	(setq
	  font (vlax-get-property range 'font)
	  HorizontalAlignment
	   (vlax-variant-value
	     (vlax-get-property
	       Cell
	       'HorizontalAlignment
	     )
	   )
	  HorizontalAlignment
	   (cond ((= HorizontalAlignment -4152) 2) ;_ 右
		 ((= HorizontalAlignment -4108) 1) ;_ 中
		 (t 0) ;_ 左
	   )
	  VerticalAlignment
	   (vlax-variant-value
	     (vlax-get-property
	       Cell
	       'VerticalAlignment
	     )
	   )
	  VerticalAlignment
	   (cond ((= VerticalAlignment -4160) 0) ;_ 上
		 ((= VerticalAlignment -4108) 1) ;_ 中
		 (t 2) ;_ 下
	   )
	  DXF71	(nth VerticalAlignment
		     (nth HorizontalAlignment dxf71data)
		)
	  DXF62	(vlxls-color-eci->aci
		  (vlax-variant-value
		    (vlax-get-property Font 'colorIndex)
		  )
		)
	  DXF420 (vlxls-color-eci->truecolor
		   (vlax-variant-value
		     (vlax-get-property Font 'colorIndex)
		   )
		 )
	)
	;;计算Range的字体 RangeFont i ii char cfont charFont caption f TextVerFlag
	(setq RangeFont
	       (mapcar
		 '(lambda (x) (cons x (VLXLS-GET-PROPERTY font x)))
		 '("NAME"	  "SIZE"	 "COLORINDEX"
		   "BOLD"	  "ITALIC"	 "SUBSCRIPT"
		   "SUPERSCRIPT"  "UNDERLINE"
		  )
	       )
	)
	(setq DXF7 (cdr (assoc "NAME" RangeFont)))
	(if (null dxf7) (setq DXF7 StandardFont))
	;;字体
	(setq textFont (strcat "{\\f" DXF7 "|b0|i0|c134|p0;"))
	(setq Dxf40 (cdr (assoc "SIZE" RangeFont)))
	(if (null DXF40) (setq DXF40 StandardFontSize))
	;;字大小
	(setq textFont (strcat textFont "\\H" (rtos DXF40 2 1) "x;"))
	;;加粗
	(if (equal :vlax-true (cdr (assoc "BOLD" RangeFont)))
	  (setq textfont (strcat textFont "\\W1.2;"))
	  )
	;;倾斜
	(if (equal :vlax-true (cdr (assoc "ITALIC" RangeFont)))
	  (setq textfont (strcat textFont "\\Q18;"))
	  )
	;;下划线
	(if (= 2 (cdr (assoc "UNDERLINE" RangeFont)))
	  (setq textfont (strcat textFont "\\L"))
	  )
	;;上标 "SUPERSCRIPT"
	;;下标 "SUBSCRIPT"
	;;文字是否竖向
	(setq TextVerFlag
	       (= (GXL-CATCHAPPLY
		    VLXLS-GET-PROPERTY
		    (list range "Orientation")
		  )
		  -4166
	       )
	)
	(if TextVerFlag
	  (progn
	    (setq text (gxl-str->singleonly text))
	    (setq tmp  (car text)
		  text (cdr text)
	    )
	    (foreach a text (setq tmp (strcat tmp "\\P" a)))
	    (setq text tmp)
	  )
	)
	;;逐字取样式
	;;(setq textFont (strcat textFont (GetRangeTextStyle RANGE RANGEFONT text) "}"))
	
	(setq text (strcat textFont text "}"))

      )
    )
    (cond ((null OldRow) (setq OldRow Row))
	  ((/= OldRow Row) ;_ 换行
	   (if *pageSetUp*
	     (progn
	       (if (member row HPageBreaks) ;_ 换页
		 (progn
		   (setq OldRow	    Row
			 StartPoint (polar StartPoint
					   (* 1.5 pi)
					   oldheight
				    )
		   )
		   (if *Merge*
		     (progn
		       (entmake
			 (list
			   '(0 . "line")
			   '(100 . "AcDbEntity")
			   '(67 . 0)
			   (cons 8 Blayer)
			   (cons 62 *defaultColor*)
			   '(100 . "AcDbLine")
			   (cons 10
				 StartPoint

			   )
			   (cons
			     11
			     (setq p0
				    (polar
				      StartPoint
				      0
				      (* defaultHeight GridScale Totalwide)
				    )
			     )
			   )
			   '(210 0.0 0.0 1.0)
			 )
		       )
		       (entmake
			 (list
			   '(0 . "line")
			   '(100 . "AcDbEntity")
			   '(67 . 0)
			   (cons 8 Blayer)
			   (cons 62 *defaultColor*)
			   '(100 . "AcDbLine")
			   (cons 10 RightTopPt)
			   (cons 11 p0)
			   '(210 0.0 0.0 1.0)
			 )
		       )
		       
		     )
		   )
		   ;;输出页脚代吗
		   (if *pageSetUp* (DrawPageSetUp pagesetup StartPoint nil))
		   ;;分组或分块
		   (cond
		     ((= 1 *Oprate*)
		      (setq ss (GXL-SEL-ENTNEXTALL endent))
		      (if *CellColor*
			(progn
			  (command "_select" ss "")
			  (setq s1 (ssget "_p" '((0 . "solid"))))
			  (if s1
			    (gxl-MovetoBottom s1)
			  )
			)
		      )
		      (gxl-AX:AddUnNameGroup ss)
		      (setq endent (entlast))
		     )
		     ((= 2 *Oprate*)
		      (setq ss (GXL-SEL-ENTNEXTALL endent))
		      (if *CellColor*
			(progn
			  (command "_select" ss "")
			  (setq s1 (ssget "_p" '((0 . "solid"))))
			  (if s1
			    (gxl-MovetoBottom s1)
			  )
			)
		      )
		      (gxl-BLK-UnBlockBase ss (nth *BasePointPos* '(4 1 3 2)))
		      (setq endent (entlast))
		     )
		   )

		     
		   (setq StartPoint (polar StartPoint (* 1.5 pi) PageMargin)
			 Curpt	    StartPoint
			 RightTopPt (polar StartPoint 0 (* defaultHeight GridScale Totalwide))
			 ) ;_ 移动页间距
		   (setq page (1+ page))
		   ;;输出页眉代吗
		   (if *pageSetUp* (DrawPageSetUp pagesetup StartPoint t))
		   ;;输出表头
                   (if *pageSetUp* (PrintTitleRows cells))
		   
		 )
		 (setq OldRow	  Row
		       StartPoint (polar StartPoint (* 1.5 pi) oldheight)
		       Curpt	  StartPoint
		 )
	       )
	     )
	     (setq OldRow     Row
		   StartPoint (polar StartPoint (* 1.5 pi) oldheight)
		   Curpt      StartPoint
	     )
	   )

	   
	  )
    )
    
    (setq oldheight height)
    (if (not (equal width 0 0.01))
      (progn
    (if Mergep
      (progn
        (setq mergeId (mapcar 'vlxls-rangeid
                              (vlxls-cellid (vlxls-range-getid range))
                              )
              width1 (* defaultHeight GridScale (VLXLS-GET-PROPERTY range "MergeArea.width"))
              height1 (* defaultHeight GridScale (VLXLS-GET-PROPERTY range "MergeArea.height"))
              )
        )
      (setq width1 width height1 height)
      )
    (if (and *RowHeight* (> *RowHeight* 0) Mergep (> (vlax-variant-value (vlax-get-property cell 'height)) 0))
      (setq height1 (* *RowHeight* (/ (VLXLS-GET-PROPERTY range "MergeArea.height") (vlax-variant-value (vlax-get-property cell 'height)))))
    )
    (if	(or (not Mergep)
	    (and Mergep (equal (car mergeId) (list col row)))
	)
      (progn
        (setq p0 (polar Curpt (* 1.5 pi) height1)
              p1 Curpt
              p2 (polar Curpt 0 width1)
              p3 (polar p2 (* 1.5 pi) height1)
              ) ;_ 框的四个角点 左下、左上、右上、右下
        (if *Merge*
          (progn
            (if Horline
              (progn
                (if (equal p1 (gxl-dxf HorLine 11) 1e-3)
                  (gxl-ch_ent HorLine 11 p2) ;_ 更新水平直线末端点
                  (progn
                (entmake
                  (list
                    '(0 . "line")
                    '(100 . "AcDbEntity")
                    '(67 . 0)
                    (cons 8 Blayer)
		    (cons 62 *defaultColor*)
                    '(100 . "AcDbLine")
                    (cons 10 p1)
                    (cons 11 p2)
                    '(210 0.0 0.0 1.0)
                    )
                  )
                (setq Horline (entlast))
                )
                  )
               )
              (progn
                (entmake
                  (list
                    '(0 . "line")
                    '(100 . "AcDbEntity")
                    '(67 . 0)
                    (cons 8 Blayer)
		    (cons 62 *defaultColor*)
                    '(100 . "AcDbLine")
                    (cons 10 p1)
                    (cons 11 p2)
                    '(210 0.0 0.0 1.0)
                    )
                  )
                (setq Horline (entlast))
                )
              )
            (if VerLines
              (progn
                (if (not
                      (vl-some
                      (Function
                      (lambda (Line)
                         (if (equal p1 (gxl-dxf Line 11) 1e-3)
                           (gxl-ch_ent Line 11 p0) ;_ 更新垂直直线末端点
                           )
                         )
                      )
                      VerLines
                      )
                    )
                  (progn
                    (entmake
                      (list
                        '(0 . "line")
                        '(100 . "AcDbEntity")
                        '(67 . 0)
                        (cons 8 Blayer)
			(cons 62 *defaultColor*)
                        '(100 . "AcDbLine")
                        (cons 10 p1)
                        (cons 11 p0)
                        '(210 0.0 0.0 1.0)
                        )
                      )
                    (setq VerLines (cons (entlast) VerLines))
                    )
                  )
                )
              (progn
                (entmake
                  (list
                    '(0 . "line")
                    '(100 . "AcDbEntity")
                    '(67 . 0)
                    (cons 8 Blayer)
		    (cons 62 *defaultColor*)
                    '(100 . "AcDbLine")
                    (cons 10 p1)
                    (cons 11 p0)
                    '(210 0.0 0.0 1.0)
                    )
                  )
                (setq VerLines (cons (entlast) VerLines))
                )
              )
            )
          (entmake
            (list
              '(0 . "LWPOLYLINE")
              '(100 . "AcDbEntity")
              '(67 . 0)
              (cons 8 BLayer)
	      (cons 62 *defaultColor*)
              '(100 . "AcDbPolyline")
              '(90 . 4)
              '(70 . 1)
              '(43 . 0.0)
              '(38 . 0.0)
              '(39 . 0.0)
              (cons 10 p0)
              (cons 10 p1)
              (cons 10 p2)
              (cons 10 p3)
              '(210 0.0 0.0 1.0)
              )
            )
          )
	(if *CellColor* ;_ 绘制背景颜色
	  (progn
	    (if	(/= -4142
		    (setq Interiorcolor
			   (VLXLS-GET-PROPERTY
			     range
			     "Interior.ColorIndex"
			   )
		    )
		)
	      (progn
		(setq Interiorcolor	(VLXLS-COLOR-ECI->ACI Interiorcolor)
		      Interiortruecolor	(VLXLS-COLOR-ECI->TRUECOLOR
					  Interiorcolor
					)
		)
		(entmake
		  (vl-remove
		    nil
		    (list
		      '(0 . "SOLID")
		      '(100 . "AcDbEntity")
		      '(67 . 0)
		      (cons 8 BLayer)
		      (cons 62 Interiorcolor)
		      ;|(if (not (or (= 256 Interiorcolor)
				   (= 0 Interiortruecolor)
			       )
			  )
			(cons 420 Interiortruecolor)
		      )|;
		      '(100 . "AcDbTrace")
		      (cons 10 p0)
		      (cons 11 p1)
		      (cons 12 p3)
		      (cons 13 p2)
		      '(210 0.0 0.0 1.0)
		    )
		  )
		)
	      )
	    )
	  )
	)
	(if (/= "" text)
	  (progn
	    (setq textpt (nth (1- DXF71) (Get9JustPts p0 p2)))
	    (cond ((= 0 HorizontalAlignment) ;_ 左对齐
		   (setq textpt (polar textpt 0 (* height 0.1)))
		  )
		  ((= 2 HorizontalAlignment) ;_ 右对齐
		   (setq textpt (polar textpt pi (* height 0.1)))
		  )
	    )
	    (cond
	      ((= 0 VerticalAlignment) ;_ 上对齐
	       (setq textpt (polar textpt (* 1.5 pi) (* height 0.1)))
	      )
	      ((= 2 VerticalAlignment) ;_ 下对齐
	       (setq textpt (polar textpt (* 0.5 pi) (* height 0.1)))
	      )
	    )
	    (entmake
	      (vl-remove
		nil
		(list
		  (cons 0 "MTEXT")
		  '(100 . "AcDbEntity")
		  '(67 . 0)
		  (cons 8 TLayer)
		  (if *AnnoColor*
		    (cons 62 dxf62)
		    (cons 62 *defaultColor*)
		  )
		  ;|(if (and *AnnoColor*
			   (not (or (= 256 dxf62) (= 0 dxf420)))
		      )
		    (cons 420 dxf420)
		  )|;
		  '(100 . "AcDbMText")
		  (cons 10 textpt)
		  (cons 40 textHeight)
		  (cons 41 width1)
		  ;(cons 50 0)
		  ;;'(46 . 0.0)
		  (cons 71 DXF71)
		  (cons 72 5)
		  (cons 1 text)
		  (cons 7 "Standard")
		  '(210 0.0 0.0 1.0)
		  '(11 1.0 0.0 0.0)
		  '(50 . 0.0)
		  '(73 . 1)
		)
	      )
	    )
	  )
	)
      )
    )
    (setq Curpt (polar Curpt 0 width))
    )
      )
    ) ;_ t
     ) ;_ cond
  )
 (GXL-SYS-PROGRESS-DONE)
 (if *Merge*
   (progn
     (if *pageSetUp*
       (setq dd (* PageMargin (* (length HPAGEBREAKS))))
       (setq dd 0)
       )
            (entmake
              (list
                '(0 . "line")
                '(100 . "AcDbEntity")
                '(67 . 0)
                (cons 8 Blayer)
		(cons 62 *defaultColor*)
                '(100 . "AcDbLine")
		(if *pageSetUp*
		  (cons 10 (setq p0 RightTopPt))
		  (cons	10
			(setq
			  p0 (polar BasePoint
				    0
				    (* defaultHeight GridScale Totalwide)
			     )
			)
		  )
		)
		(cons 11 p3)
                ;(cons 11 (setq p1 (polar p0 (* 1.5 pi) (+ dd (* defaultHeight GridScale Totalheight)))))
                '(210 0.0 0.0 1.0)
                )
              )
            (entmake
              (list
                '(0 . "line")
                '(100 . "AcDbEntity")
                '(67 . 0)
                (cons 8 Blayer)
		(cons 62 *defaultColor*)
                '(100 . "AcDbLine")
                (cons 10 (polar p3 pi (* defaultHeight GridScale Totalwide)))
                (cons 11 p3)
                '(210 0.0 0.0 1.0)
                )
              )
            (setq p3 (polar p2 (* 1.5 pi) height1))
            )
   )
   ;;输出页脚代吗
    (if *pageSetUp* (DrawPageSetUp pagesetup StartPoint nil))
 (setq ss (GXL-SEL-ENTNEXTALL endent))
 (if *CellColor*
   (progn
     (command "_select" ss "")
     (setq s1 (ssget "_p" '((0 . "solid"))))
     (if s1
       (gxl-MovetoBottom s1)
       )
     )
   )
 (cond
     ((= 1 *Oprate*)
      (gxl-AX:AddUnNameGroup ss)
      )
     ((= 2 *Oprate*)
      (gxl-BLK-UnBlockBase ss (nth *BasePointPos* '(4 1 3 2)))
      )
     )
     (vlax-release-object *xlapp*)
     )
    )
  (reerr)
(princ)
)
(princ)
;;;==================================================================
;;; EVE - 依据块插入点执行EV功能 (无对话框版)
;;;------------------------------------------------------------------
;;; 功能: 选择一个块(或使用预选块), 读取其插入点, 删除原块,
;;;       然后跳过对话框, 直接复用EV上次设置, 以该插入点作为
;;;       放置位置执行EV(Excel转CAD)功能
;;;------------------------------------------------------------------
;;; 与EV的区别:
;;;   EV  - 弹出对话框设置参数, 需手动点击放置位置
;;;   EVE - 跳过对话框直接复用上次设置, 自动使用块插入点,
;;;         支持预选块直接执行
;;;------------------------------------------------------------------
;;; 用法:
;;;   1. 先选中一个块, 再输入EVE -> 直接执行, 无任何交互
;;;   2. 未选中块, 输入EVE -> 提示选择块, 然后直接执行
;;;==================================================================
(defun c:eve (/ blkEnt ss insPt)
  (setq *EVE-PRESET-PT* nil
        *EVE-SKIP-DIALOG* nil)
  ;; 检查是否有预选块
  (if (setq ss (ssget "i" '((0 . "INSERT"))))
    (setq blkEnt (ssname ss 0))
    ;; 没有预选块, 提示用户选择
    (while (not blkEnt)
      (setq blkEnt (car (entsel "\n选择要替换的块: ")))
      (cond
        ((not blkEnt)
         (princ "\n未选择对象, 请重试。")
        )
        ((/= "INSERT" (cdr (assoc 0 (entget blkEnt))))
         (princ "\n所选对象不是块, 请重试。")
         (setq blkEnt nil)
        )
      )
    )
  )
  ;; 读取插入点并删除原块
  (setq insPt (cdr (assoc 10 (entget blkEnt))))
  (entdel blkEnt)
  ;; 设置预设点和跳过对话框标志
  (setq *EVE-PRESET-PT* insPt
        *EVE-SKIP-DIALOG* T)
  (c:ev)
  ;; 清理标志
  (setq *EVE-SKIP-DIALOG* nil)
  (princ)
)
(princ)
;;***************
(defun c:fff (/ myerr olderr ss osm ort ped rad ss1 dis n en lst enl)

  (setvar "cmdecho" 0)
;;;****************************************************
  (princ "\n请保证所选对象都在当前视口内...")
  (defun myerr (msg)
    ;;************************************************
    ;;在这里写入错误处理函数
    (setq *error* olderr)
    (princ msg)
    (if	osm
      (setvar "osmode" osm)
    )
    (if	ort
      (setvar "orthomode" ort)
    )
    (command "undo" "E")
    ;;**********************

    (princ)
  )
  (setq olderr *error*)
  (setq *error* myerr)
  ;;初始化
  (setq ss (ssgetfirst))
  (command "undo" "BE")
  (setq osm (getvar "osmode"))
  (setq ort (getvar "orthomode"))
  (setq ped (getvar "peditaccept"))

  (setvar "osmode" 0)
  (setvar "orthomode" 0)
;;;****************************************************
  ;;在这里写入正常工作的函数
  (setq rad -1)
  (while (and
	   (/= nil rad)
	   (> 0 rad)
	 )
    (princ "\n请输入倒角半径<")
    (princ (getvar "filletrad"))
    (setq rad (getreal ">:"))
  )
  (if rad
    (setvar "filletrad" rad)
  )

  (setq ss1 (ssget '((0 . "line,arc,LWPOLYLINE"))))
  (if ss1
    (setq dis (getdist "\n请输入一个合适的间距:"))
  )
  (if (and ss1 dis)
    (progn
      (setq n -1)
      (setq ss (ssadd))
      (repeat (sslength ss1)
	(setq en (ssname ss1 (setq n (1+ n))))
	(setq lst (entget en))
	(cond
	  ((= "LINE" (cdr (assoc 0 lst)))
	   (ssadd en ss)
	  )
	  ((= "ARC" (cdr (assoc 0 lst)))
	   (entdel en)
	  )
	  ((= "LWPOLYLINE" (cdr (assoc 0 lst)))
	   (setq enl (entlast))
	   (command "explode" en)
	   (while (setq enl (entnext enl))
	     (setq lst (entget enl))
	     (cond
	       ((= "LINE" (cdr (assoc 0 lst)))
		(ssadd enl ss)
	       )
	       (t
		(entdel enl)
	       )
	     )
	   )


	  )
	)
      )

      (if (and ss (> (sslength ss) 0))
	(progn
	  (setvar "peditaccept" 1)
	  (setq enl (entlast))
	  (command "pedit" "m" ss "" "j" "j" "e" dis "")
	  (setq ss (ssadd))
	  (while (setq enl (entnext enl))
	    (ssadd enl ss)
	  )
	  (if (> (sslength ss) 0)
	    (progn
	      (setq n -1)
	      (repeat (sslength ss)
		(hero_fillet (ssname ss (setq n (1+ n))))
	      )
	    )
	  )
	)
      )
    )
  )
;;;****************************************************
  ;;结束
  (setvar "osmode" osm)
  (setvar "orthomode" ort)
  (command "undo" "E")
  (setvar "peditaccept" ped)
  (setq *error* olderr)
  (princ)
)
(defun getmidp (firp senp)
;;;取得两点的中点
  (list	(/ (+ (car firp) (car senp)) 2.0)
	(/ (+ (cadr firp) (cadr senp)) 2.0)
	0
  )
)
(defun hero_fillet (en / cl lst n p1 p2 p3 mp1 mp2 filrad)
  (setq filrad (getvar "filletrad"))
  (if (> filrad 0)
    (progn
      (setq lst (entget en))
      (setq cl (cdr (assoc 70 lst)))
      (setq lst (vl-remove-if '(lambda (x) (/= 10 (car x))) lst))
      (if (= 1 cl)
	(setq lst (append lst (list (car lst))))
      )
      (setq n 0)
      (repeat (- (length lst) 2)
	(setq p1 (cdr (nth n lst)))
	(setq p2 (cdr (nth (+ n 1) lst)))
	(setq p3 (cdr (nth (+ n 2) lst)))
	(setq mp1 (getmidp p1 p2)
	      mp2 (getmidp p2 p3)
	)
	(if  (hero_canfillet p1 p2 p3  filrad)
		  

	  (command "fillet" mp1 mp2)
	)
	(setq n (1+ n))
      )
      (if (vlax-curve-isclosed (vlax-ename->vla-object en))
	(progn
	  (setq p1 (cdr (nth n lst)))
	  (setq p2 (cdr (nth (+ n 1) lst)))
	  (setq p3 (cdr (nth 1 lst)))
	  (setq	mp1 (getmidp p1 p2)
		mp2 (getmidp p2 p3)
	  )
	  (if  (hero_canfillet p1 p2 p3 filrad)
		    

	    (command "fillet" mp1 mp2)
	  )
	)
      )
    )
  )
)
(defun hero_canfillet(p1 p2 p3 filrad)
  (setq ang1 (angle p1 p2))
  (setq ang2 (angle p2 p3))
  (setq ang (+ (- pi ang1) ang2))
  (if (> ang pi) (setq ang (- (*  2 pi) ang )))
  (setq ang (/ ang 2.0))
  (setq fl (abs (* (* filrad (cos ang)) 2)))
  (if (and
	(> (distance p1 p2) fl)
	(> (distance p2 p3) fl)
	(not (equal (abs (sin ang1)) (abs (sin ang2)) 0.001))
	)
    t
    nil
    )
  )
;;***************
(defun ce ( /  hangdau)
(defun sosanh (e1 e2 / p1 p2)
(setq p1 (car e1)
p2 (car e2)
)
(if (equal (cadr p1) (cadr p2) fuzz)
(< (car p1) (car p2))
(< (cadr p2) (cadr p1))
)
)
(setq
ss (ssget '((0 . "TEXT,MTEXT")))
lst (ss2ent ss)
lst (mapcar '(lambda (e / txt obj pos1 pos2 newstr) 
               (setq obj (vlax-ename->vla-object e))
               (setq txt (vla-get-TextString obj))
               ;; 如果是MTEXT，提取格式代码后的实际内容
               (if (= "AcDbMText" (vla-get-ObjectName obj))
                 (progn
                   ;; 查找最后一个分号位置
                   (setq pos1 0)
                   (while (setq pos2 (vl-string-search ";" txt pos1))
                     (setq pos1 (1+ pos2)))
                   ;; 提取分号后到}之前的内容
                   (if (> pos1 0)
                     (progn
                       (setq txt (substr txt (1+ pos1)))
                       (if (setq pos2 (vl-string-search "}" txt))
                         (setq txt (substr txt 1 pos2))
                       )
                     )
                   )
                   ;; 清理 \P 换行符
                   (setq newstr "")
                   (setq pos1 1)
                   (while (<= pos1 (strlen txt))
                     (if (and (= (substr txt pos1 1) "\\")
                              (<= (1+ pos1) (strlen txt))
                              (= (substr txt (1+ pos1) 1) "P"))
                       (progn
                         (setq newstr (strcat newstr " "))
                         (setq pos1 (+ pos1 2)))
                       (progn
                         (setq newstr (strcat newstr (substr txt pos1 1)))
                         (setq pos1 (1+ pos1))))
                   )
                   (setq txt newstr)
                 )
               )
               (cons (cdr (assoc 10 (entget e))) txt)) lst)





lst (mapcar '(lambda (e) (if (= (cdr e) "*") (cons (car e) "") e)) lst)

caotext (cdr (assoc 40 (entget (ssname ss 0))))
fuzz (* caotext 1.0)
lst (vl-sort lst 'sosanh)
index 1
oldy nil
)
;; 学习EV命令的Excel连接方法
(if (setq *xlapp* (vlax-get-or-create-object "Excel.Application"))
  (progn
    ;; 获取当前活动的工作簿和工作表（完全学习EV命令）
    (setq workbook (vlax-get-property *xlapp* 'activeworkbook))
    (setq sheet (vlax-get-property *xlapp* 'activesheet))
    
    ;; 确保Excel可见
    (if (equal :vlax-false (vlax-get-property *xlapp* 'visible))
      (vla-put-visible *xlapp* 1)
    )
    
    ;; 获取当前选中的单元格位置
    (setq ActiveCell (vlax-get-property *xlapp* 'ActiveCell))
    (setq row (vlax-get-property ActiveCell 'Row))
    (setq col (vlax-get-property ActiveCell 'Column))
    (setq base-col col)  ; 记录基准列位置
    
(foreach e lst
  (if (equal oldy (cadr (car e)) fuzz)
    (setq col (1+ col))
    (progn
      (if hangdau
        (progn
          (setq row (1+ row))
          (setq col base-col)  ; 换行时回到基准列位置
        )
        (setq hangdau t)
      )
    )
  )
  ;; 写入Excel单元格
  (vlxls-cell-put-value *xlapp* 
    (strcat (chr (+ 64 col)) (itoa row)) 
    (cdr e))
  (setq oldy (cadr (car e)))
)
;; 将选中单元格向下移动一行
(setq NextRow (1+ row))
(setq NextCellAddr (strcat (chr (+ 64 base-col)) (itoa NextRow)))
(setq NextCell (vlax-get-property *xlapp* 'Range NextCellAddr))
(vlax-invoke-method NextCell 'Select)


(princ "\n数据已写入当前Excel文档的选中位置")

  )
  (progn
    (alert "未找到运行的Excel应用程序！\n请先打开Excel文档后再运行此命令。")
    (exit)
  )
)
)
(defun ss2ent (ss / sodt index lstent)
(setq
sodt (if ss
(sslength ss)
0
)
index 0
)
(repeat sodt
(setq ent (ssname ss index)
index (1+ index)
lstent (cons ent lstent)
)
)
(reverse lstent)
)
;;; The C: function definition
;;;
(defun c:ce () 
  (if (not msxlp-get-range)
    (vlxls-app-init)
  )
  (if (not *xlapp*)
    (setq *xlapp* (vlax-get-or-create-object "Excel.Application"))
  )
  (ce)
)
(princ)
(princ)
;;; CDS Command - 往右移动版本的CE
;;; 功能：将选中的TEXT/MTEXT文本按位置排序后逐个写入Excel然后自动往右移动
(defun cds ( /  hangdau)
(defun sosanh (e1 e2 / p1 p2)
(setq p1 (car e1)
p2 (car e2)
)
(if (equal (cadr p1) (cadr p2) fuzz)
(< (car p1) (car p2))
(< (cadr p2) (cadr p1))
)
)
(setq
ss (ssget '((0 . "TEXT,MTEXT")))
lst (ss2ent ss)
lst (mapcar '(lambda (e / txt obj pos1 pos2 newstr) 
               (setq obj (vlax-ename->vla-object e))
               (setq txt (vla-get-TextString obj))
               ;; 对于MTEXT提取格式代码后的实际文本
               (if (= "AcDbMText" (vla-get-ObjectName obj))
                 (progn
                   ;; 查找最后一个分号的位置
                   (setq pos1 0)
                   (while (setq pos2 (vl-string-search ";" txt pos1))
                     (setq pos1 (1+ pos2)))
                   ;; 提取分号后}之前的内容
                   (if (> pos1 0)
                     (progn
                       (setq txt (substr txt (1+ pos1)))
                       (if (setq pos2 (vl-string-search "}" txt))
                         (setq txt (substr txt 1 pos2))
                       )
                     )
                   )
                   ;; 替换 \P 为空格
                   (setq newstr "")
                   (setq pos1 1)
                   (while (<= pos1 (strlen txt))
                     (if (and (= (substr txt pos1 1) "\\")
                              (<= (1+ pos1) (strlen txt))
                              (= (substr txt (1+ pos1) 1) "P"))
                       (progn
                         (setq newstr (strcat newstr " "))
                         (setq pos1 (+ pos1 2)))
                       (progn
                         (setq newstr (strcat newstr (substr txt pos1 1)))
                         (setq pos1 (1+ pos1))))
                   )
                   (setq txt newstr)
                 )
               )
               (cons (cdr (assoc 10 (entget e))) txt)) lst)

lst (mapcar '(lambda (e) (if (= (cdr e) "*") (cons (car e) "") e)) lst)

caotext (cdr (assoc 40 (entget (ssname ss 0))))
fuzz (* caotext 1.0)
lst (vl-sort lst 'sosanh)
index 1
oldy nil
)
;; 学习EV函数的Excel操作方法
(if (setq *xlapp* (vlax-get-or-create-object "Excel.Application"))
  (progn
    ;; 获取当前的工作簿和工作表并完全学习EV函数
    (setq workbook (vlax-get-property *xlapp* 'activeworkbook))
    (setq sheet (vlax-get-property *xlapp* 'activesheet))
    
    ;; 确保Excel可见
    (if (equal :vlax-false (vlax-get-property *xlapp* 'visible))
      (vla-put-visible *xlapp* 1)
    )
    
    ;; 获取当前选中的单元格位置
    (setq ActiveCell (vlax-get-property *xlapp* 'ActiveCell))
    (setq row (vlax-get-property ActiveCell 'Row))
    (setq col (vlax-get-property ActiveCell 'Column))
    (setq base-row row)  ; 记录基准行位置
    
(foreach e lst
  (if (equal oldy (cadr (car e)) fuzz)
    (setq row (1+ row))
    (progn
      (if hangdau
        (progn
          (setq col (1+ col))
          (setq row base-row)  ; 换列时回到基准行位置
        )
        (setq hangdau t)
      )
    )
  )
  ;; 写入Excel单元格
  (vlxls-cell-put-value *xlapp* 
    (strcat (chr (+ 64 col)) (itoa row)) 
    (cdr e))
  (setq oldy (cadr (car e)))
)
;; 将选中的单元格向右移动一列
(setq NextCol (1+ col))
(setq NextCellAddr (strcat (chr (+ 64 NextCol)) (itoa base-row)))
(setq NextCell (vlax-get-property *xlapp* 'Range NextCellAddr))
(vlax-invoke-method NextCell 'Select)

(princ "\n数据已写入当前Excel的选中位置")

  )
  (progn
    (alert "未找到运行中的Excel应用程序\n请先打开Excel的文档再执行命令")
    (exit)
  )
)
)

;;; The C: function definition for CDS
;;;
(defun c:cds () 
  (if (not msxlp-get-range)
    (vlxls-app-init)
  )
  (if (not *xlapp*)
    (setq *xlapp* (vlax-get-or-create-object "Excel.Application"))
  )
  (cds)
)
(princ)
(princ)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;************************************************************************
;;***************
;; CEE功能：将选中的所有TEXT/MTEXT文本合并输送到一个单元格，然后往下移动
(defun cee ( / )
(defun sosanh (e1 e2 / p1 p2)
(setq p1 (car e1)
p2 (car e2)
)
(if (equal (cadr p1) (cadr p2) fuzz)
(< (car p1) (car p2))
(< (cadr p2) (cadr p1))
)
)
(setq
ss (ssget '((0 . "TEXT,MTEXT")))
lst (ss2ent ss)
lst (mapcar '(lambda (e / txt obj pos1 pos2 newstr) 
               (setq obj (vlax-ename->vla-object e))
               (setq txt (vla-get-TextString obj))
               ;; 如果是MTEXT，提取格式代码后实际文本
               (if (= "AcDbMText" (vla-get-ObjectName obj))
                 (progn
                   ;; 查找最后一个分号位置
                   (setq pos1 0)
                   (while (setq pos2 (vl-string-search ";" txt pos1))
                     (setq pos1 (1+ pos2)))
                   ;; 获取分号和}之前的内容
                   (if (> pos1 0)
                     (progn
                       (setq txt (substr txt (1+ pos1)))
                       (if (setq pos2 (vl-string-search "}" txt))
                         (setq txt (substr txt 1 pos2))
                       )
                     )
                   )
                   ;; 替换 \P 换行符
                   (setq newstr "")
                   (setq pos1 1)
                   (while (<= pos1 (strlen txt))
                     (if (and (= (substr txt pos1 1) "\\")
                              (<= (1+ pos1) (strlen txt))
                              (= (substr txt (1+ pos1) 1) "P"))
                       (progn
                         (setq newstr (strcat newstr " "))
                         (setq pos1 (+ pos1 2)))
                       (progn
                         (setq newstr (strcat newstr (substr txt pos1 1)))
                         (setq pos1 (1+ pos1))))
                   )
                   (setq txt newstr)
                 )
               )
               (cons (cdr (assoc 10 (entget e))) txt)) lst)

lst (mapcar '(lambda (e) (if (= (cdr e) "*") (cons (car e) "") e)) lst)

caotext (cdr (assoc 40 (entget (ssname ss 0))))
fuzz (* caotext 1.0)
lst (vl-sort lst 'sosanh)
)

;; 学习EV命令的Excel操作方法
(if (setq *xlapp* (vlax-get-or-create-object "Excel.Application"))
  (progn
    ;; 获取当前的工作簿和工作表，完全学习EV命令
    (setq workbook (vlax-get-property *xlapp* 'activeworkbook))
    (setq sheet (vlax-get-property *xlapp* 'activesheet))
    
    ;; 确保Excel可见
    (if (equal :vlax-false (vlax-get-property *xlapp* 'visible))
      (vla-put-visible *xlapp* 1)
    )
    
    ;; 获取当前选中的单元格位置
    (setq ActiveCell (vlax-get-property *xlapp* 'ActiveCell))
    (setq row (vlax-get-property ActiveCell 'Row))
    (setq col (vlax-get-property ActiveCell 'Column))
    
    ;; 关键差异：将所有的文本合并为一个字符串
    (setq merged-text "")
    (foreach e lst
      (if (> (strlen merged-text) 0)
        (setq merged-text (strcat merged-text " " (cdr e)))  ; 用空格连接，也可以使用"\n"换行
        (setq merged-text (cdr e))
      )
    )
    
    ;; 将合并后的文本写入当前单元格
    (vlxls-cell-put-value *xlapp* 
      (strcat (chr (+ 64 col)) (itoa row)) 
      merged-text)
    
    ;; 将选中单元格向下移动一行
    (setq NextRow (1+ row))
    (setq NextCellAddr (strcat (chr (+ 64 col)) (itoa NextRow)))
    (setq NextCell (vlax-get-property *xlapp* 'Range NextCellAddr))
    (vlax-invoke-method NextCell 'Select)

    (princ "\n合并后文本已写入当前Excel单元格")
  )
  (progn
    (alert "未找到运行的Excel应用程序\n请先打开Excel文档，并进行操作")
    (exit)
  )
)
)

;;; The C: function definition for CEE
;;;
(defun c:cee () 
  (if (not msxlp-get-range)
    (vlxls-app-init)
  )
  (if (not *xlapp*)
    (setq *xlapp* (vlax-get-or-create-object "Excel.Application"))
  )
  (cee)
)

(princ)

;;; EVS Command - 简化版Excel到CAD内容传输
;;; 功能：将Excel选定单元格内容传输到CAD，传输后自动下移选择区域
;;; 只传输内容，不创建表格框架，独立的字号和宽度控制系统
(defun evs ( / selection startpoint curpt defaultHeight textWidth tlayer text-list 
            start-row start-col row-count col-count new-start-row new-end-row 
            new-end-col new-range-address new-range cells cell text col-letter
            end-col-letter kwd)
  
  ;; 辅助函数：将列号转换为Excel列字母
  (defun col-to-letter (col / result remainder)
    (setq result "")
    (while (> col 0)
      (setq remainder (rem (1- col) 26))
      (setq result (strcat (chr (+ 65 remainder)) result))
      (setq col (/ (1- col) 26))
    )
    result
  )
  
  ;; 辅助函数：智能转换数值为字符串
  (defun smart-number-to-string (value)
    (cond
      ((= (type value) 'STR) value)  ; 已经是字符串
      ((= (type value) 'INT) (itoa value))  ; 整数
      ((= (type value) 'REAL)  ; 实数
       (if (= value (fix value))  ; 检查是否为整数值的实数
         (itoa (fix value))  ; 转换为整数字符串
         (rtos value 2 2)  ; 保留2位小数的实数字符串
       )
      )
      (t (vl-princ-to-string value))  ; 其他类型
    )
  )
  
  ;; 检查Excel应用程序
  (if (setq *xlapp* (vlax-get-or-create-object "Excel.Application"))
    (progn
      ;; 确保Excel可见
      (if (equal :vlax-false (vlax-get-property *xlapp* 'visible))
        (vla-put-visible *xlapp* 1)
      )
      
      ;; 获取当前选择的单元格范围
      (setq selection (vlax-get-property *xlapp* 'Selection))
      
      ;; 检查是否有选择的单元格
      (if selection
        (progn
          ;; 第一步：获取选择范围的详细信息
          (setq start-row (vlax-get-property selection 'Row))
          (setq start-col (vlax-get-property selection 'Column))
          (setq row-count (vlax-get-property (vlax-get-property selection 'Rows) 'Count))
          (setq col-count (vlax-get-property (vlax-get-property selection 'Columns) 'Count))
          
          ;; 第二步：获取所有单元格的数据并存储到列表中
          (setq cells (vlax-get-property selection 'Cells))
          (setq text-list '())
          
          ;; 遍历选中的单元格，收集所有文本数据
          (vlax-for cell cells
            ;; 直接获取单元格的文本值
            (setq text (vlax-variant-value (vlax-get-property cell 'Value)))
            
            ;; 如果单元格有内容，添加到列表中
            (if (and text (/= text "") (/= text nil))
              (progn
                ;; 使用智能转换函数
                (setq text (smart-number-to-string text))
                (setq text-list (append text-list (list text)))
              )
            )
          )
          
          ;; 第三步：立即执行Excel单元格下移操作（趁Selection还有效）
          (setq new-start-row (+ start-row row-count))
          (setq new-end-row (+ new-start-row row-count -1))
          (setq new-end-col (+ start-col col-count -1))
          
          ;; 转换列号为字母
          (setq col-letter (col-to-letter start-col))
          (setq end-col-letter (col-to-letter new-end-col))
          
          ;; 构建新的范围地址字符串（使用A1格式）
          (setq new-range-address 
            (if (and (= row-count 1) (= col-count 1))
              ;; 单个单元格
              (strcat col-letter (itoa new-start-row))
              ;; 多个单元格
              (strcat col-letter (itoa new-start-row) 
                      ":" 
                      end-col-letter (itoa new-end-row))
            )
          )
          
          ;; 选择新范围（在Excel还是活动窗口时完成）
          (setq new-range (vlax-get-property *xlapp* 'Range new-range-address))
          (vlax-invoke-method new-range 'Select)
          
          ;; 第四步：获取EVS专用的字号和宽度设置
          ;; 从环境变量读取EVS专用字号
          (if (setq defaultHeight (getenv "EVS\\TextHeight"))
            (setq defaultHeight (atof defaultHeight))
            (setq defaultHeight 3.0)
          )
          
          ;; 从环境变量读取EVS专用文字宽度
          (if (setq textWidth (getenv "EVS\\TextWidth"))
            (setq textWidth (atof textWidth))
            (setq textWidth 1.0)  ; 默认宽度系数1.0
          )
          
          ;; 确保参数是有效数值
          (if (or (not (numberp defaultHeight)) (<= defaultHeight 0))
            (setq defaultHeight 3.0)
          )
          (if (or (not (numberp textWidth)) (<= textWidth 0))
            (setq textWidth 1.0)
          )
          
          ;; 第五步：获取插入点，支持S键设置字号和宽度
          (initget "S")
          (setq startpoint (getpoint (strcat "\n指定插入点 [设置文字参数(S)] <当前字号:" (rtos defaultHeight 2 1) " 宽度:" (rtos textWidth 2 1) ">:")))
          
          ;; 检查用户是否选择设置文字参数
          (while (= startpoint "S")
            ;; 设置文字高度
            (setq defaultHeight (getreal (strcat "\n输入文字高度 <" (rtos defaultHeight 2 1) ">:")))
            (if (not defaultHeight)
              (setq defaultHeight 3.0)
            )
            
            ;; 设置文字宽度系数
            (setq textWidth (getreal (strcat "\n输入文字宽度系数 <" (rtos textWidth 2 1) ">:")))
            (if (not textWidth)
              (setq textWidth 1.0)
            )
            
            ;; 保存新的参数到环境变量
            (setenv "EVS\\TextHeight" (rtos defaultHeight 2 6))
            (setenv "EVS\\TextWidth" (rtos textWidth 2 6))
            
            (initget "S")
            (setq startpoint (getpoint (strcat "\n指定插入点 [设置文字参数(S)] <当前字号:" (rtos defaultHeight 2 1) " 宽度:" (rtos textWidth 2 1) ">:")))
          )
          
          ;; 如果用户取消了插入点选择
          (if (not startpoint)
            (progn
              (princ "\n*取消*")
              (exit)
            )
          )
          
          (setq startpoint (trans startpoint 1 0))
          (setq curpt startpoint)
          
          ;; 获取文字图层
          (setq tlayer (if (and (boundp 'TLayer) TLayer) TLayer "0"))
          
          ;; 第六步：在CAD中创建文字对象
          (foreach text text-list
            (entmake
              (list
                (cons 0 "TEXT")
                '(100 . "AcDbEntity")
                '(67 . 0)
                (cons 8 tlayer)
                (if (and (boundp '*defaultColor*) *defaultColor*)
                  (cons 62 *defaultColor*)
                  '(62 . 256)
                )
                '(100 . "AcDbText")
                (cons 10 curpt)
                (cons 40 defaultHeight)  ; 文字高度
                (cons 41 textWidth)      ; 文字宽度系数
                (cons 1 text)
                (cons 7 "Standard")
                '(210 0.0 0.0 1.0)
              )
            )
            ;; 移动到下一个位置（向下）
            (setq curpt (polar curpt (* 1.5 pi) (* defaultHeight 1.5)))
          )
          
          (princ (strcat "\n已传输 " (itoa (length text-list)) " 个文字到CAD，字号: " (rtos defaultHeight 2 1) " 宽度: " (rtos textWidth 2 1) "，Excel选择区域已下移到: " new-range-address))
        )
        (progn
          (alert "请先在Excel中选择要传输的单元格范围！")
        )
      )
    )
    (progn
      (alert "未找到运行中的Excel应用程序\n请先打开Excel文档后再执行此命令")
    )
  )
)

;;; The C: function definition for EVS
;;;
(defun c:evs () 
  (evs)
)

;;; EVSG Command - 带智能居中的Excel到CAD内容传输
;;; 功能：将Excel选定单元格内容传输到CAD，检测封闭矩形并自动居中放置
;;; 如果插入点被直线封闭，文字自动调整到矩形几何中心
(defun evsg ( / selection startpoint curpt defaultHeight textWidth tlayer text-list 
             start-row start-col row-count col-count new-start-row new-end-row 
             new-end-col new-range-address new-range cells cell text col-letter
             end-col-letter kwd center-point)
  
  ;; 辅助函数：将列号转换为Excel列字母
  (defun col-to-letter (col / result remainder)
    (setq result "")
    (while (> col 0)
      (setq remainder (rem (1- col) 26))
      (setq result (strcat (chr (+ 65 remainder)) result))
      (setq col (/ (1- col) 26))
    )
    result
  )
  
  ;; 辅助函数：智能转换数值为字符串
  (defun smart-number-to-string (value)
    (cond
      ((= (type value) 'STR) value)  ; 已经是字符串
      ((= (type value) 'INT) (itoa value))  ; 整数
      ((= (type value) 'REAL)  ; 实数
       (if (= value (fix value))  ; 检查是否为整数值的实数
         (itoa (fix value))  ; 转换为整数字符串
         (rtos value 2 2)  ; 保留2位小数的实数字符串
       )
      )
      (t (vl-princ-to-string value))  ; 其他类型
    )
  )
  
  ;; 辅助函数：检测点是否在封闭矩形内并返回矩形中心
  (defun find-enclosing-rectangle (pt / ss ent entdata p1 p2 left-line right-line 
                                      top-line bottom-line left-x right-x top-y bottom-y
                                      tolerance center-x center-y)
    (setq tolerance 0.01)  ; 容差值
    (setq left-line nil right-line nil top-line nil bottom-line nil)
    
    ;; 获取所有直线
    (setq ss (ssget "X" '((0 . "LINE"))))
    
    (if ss
      (progn
        ;; 遍历所有直线，寻找封闭矩形的边界
        (setq i 0)
        (repeat (sslength ss)
          (setq ent (ssname ss i))
          (setq entdata (entget ent))
          (setq p1 (cdr (assoc 10 entdata)))
          (setq p2 (cdr (assoc 11 entdata)))
          
          ;; 检查是否为水平线
          (if (< (abs (- (cadr p1) (cadr p2))) tolerance)
            (progn
              ;; 水平线 - 检查点是否在线段的X范围内
              (if (and (>= (car pt) (min (car p1) (car p2)))
                       (<= (car pt) (max (car p1) (car p2))))
                (progn
                  ;; 检查是否为上边界（Y坐标大于点的Y坐标）
                  (if (and (> (cadr p1) (cadr pt))
                           (or (not top-line) (< (cadr p1) top-y)))
                    (progn
                      (setq top-line ent)
                      (setq top-y (cadr p1))
                    )
                  )
                  ;; 检查是否为下边界（Y坐标小于点的Y坐标）
                  (if (and (< (cadr p1) (cadr pt))
                           (or (not bottom-line) (> (cadr p1) bottom-y)))
                    (progn
                      (setq bottom-line ent)
                      (setq bottom-y (cadr p1))
                    )
                  )
                )
              )
            )
          )
          
          ;; 检查是否为垂直线
          (if (< (abs (- (car p1) (car p2))) tolerance)
            (progn
              ;; 垂直线 - 检查点是否在线段的Y范围内
              (if (and (>= (cadr pt) (min (cadr p1) (cadr p2)))
                       (<= (cadr pt) (max (cadr p1) (cadr p2))))
                (progn
                  ;; 检查是否为左边界（X坐标小于点的X坐标）
                  (if (and (< (car p1) (car pt))
                           (or (not left-line) (> (car p1) left-x)))
                    (progn
                      (setq left-line ent)
                      (setq left-x (car p1))
                    )
                  )
                  ;; 检查是否为右边界（X坐标大于点的X坐标）
                  (if (and (> (car p1) (car pt))
                           (or (not right-line) (< (car p1) right-x)))
                    (progn
                      (setq right-line ent)
                      (setq right-x (car p1))
                    )
                  )
                )
              )
            )
          )
          
          (setq i (1+ i))
        )
        
        ;; 检查是否找到了四条边界线
        (if (and left-line right-line top-line bottom-line)
          (progn
            ;; 计算矩形中心点
            (setq center-x (/ (+ left-x right-x) 2.0))
            (setq center-y (/ (+ bottom-y top-y) 2.0))
            (list center-x center-y 0.0)
          )
          nil  ; 未找到完整的封闭矩形
        )
      )
      nil  ; 没有找到直线
    )
  )
  
  ;; 检查Excel应用程序
  (if (setq *xlapp* (vlax-get-or-create-object "Excel.Application"))
    (progn
      ;; 确保Excel可见
      (if (equal :vlax-false (vlax-get-property *xlapp* 'visible))
        (vla-put-visible *xlapp* 1)
      )
      
      ;; 获取当前选择的单元格范围
      (setq selection (vlax-get-property *xlapp* 'Selection))
      
      ;; 检查是否有选择的单元格
      (if selection
        (progn
          ;; 第一步：获取选择范围的详细信息
          (setq start-row (vlax-get-property selection 'Row))
          (setq start-col (vlax-get-property selection 'Column))
          (setq row-count (vlax-get-property (vlax-get-property selection 'Rows) 'Count))
          (setq col-count (vlax-get-property (vlax-get-property selection 'Columns) 'Count))
          
          ;; 第二步：获取所有单元格的数据并存储到列表中
          (setq cells (vlax-get-property selection 'Cells))
          (setq text-list '())
          
          ;; 遍历选中的单元格，收集所有文本数据
          (vlax-for cell cells
            ;; 直接获取单元格的文本值
            (setq text (vlax-variant-value (vlax-get-property cell 'Value)))
            
            ;; 如果单元格有内容，添加到列表中
            (if (and text (/= text "") (/= text nil))
              (progn
                ;; 使用智能转换函数
                (setq text (smart-number-to-string text))
                (setq text-list (append text-list (list text)))
              )
            )
          )
          
          ;; 第三步：立即执行Excel单元格下移操作（趁Selection还有效）
          (setq new-start-row (+ start-row row-count))
          (setq new-end-row (+ new-start-row row-count -1))
          (setq new-end-col (+ start-col col-count -1))
          
          ;; 转换列号为字母
          (setq col-letter (col-to-letter start-col))
          (setq end-col-letter (col-to-letter new-end-col))
          
          ;; 构建新的范围地址字符串（使用A1格式）
          (setq new-range-address 
            (if (and (= row-count 1) (= col-count 1))
              ;; 单个单元格
              (strcat col-letter (itoa new-start-row))
              ;; 多个单元格
              (strcat col-letter (itoa new-start-row) 
                      ":" 
                      end-col-letter (itoa new-end-row))
            )
          )
          
          ;; 选择新范围（在Excel还是活动窗口时完成）
          (setq new-range (vlax-get-property *xlapp* 'Range new-range-address))
          (vlax-invoke-method new-range 'Select)
          
          ;; 第四步：获取EVSG专用的字号和宽度设置
          ;; 从环境变量读取EVSG专用字号
          (if (setq defaultHeight (getenv "EVSG\\TextHeight"))
            (setq defaultHeight (atof defaultHeight))
            (setq defaultHeight 3.0)
          )
          
          ;; 从环境变量读取EVSG专用文字宽度
          (if (setq textWidth (getenv "EVSG\\TextWidth"))
            (setq textWidth (atof textWidth))
            (setq textWidth 1.0)  ; 默认宽度系数1.0
          )
          
          ;; 确保参数是有效数值
          (if (or (not (numberp defaultHeight)) (<= defaultHeight 0))
            (setq defaultHeight 3.0)
          )
          (if (or (not (numberp textWidth)) (<= textWidth 0))
            (setq textWidth 1.0)
          )
          
          ;; 第五步：获取插入点，支持S键设置字号和宽度
          (initget "S")
          (setq startpoint (getpoint (strcat "\n指定插入点 [设置文字参数(S)] <当前字号:" (rtos defaultHeight 2 1) " 宽度:" (rtos textWidth 2 1) ">:")))
          
          ;; 检查用户是否选择设置文字参数
          (while (= startpoint "S")
            ;; 设置文字高度
            (setq defaultHeight (getreal (strcat "\n输入文字高度 <" (rtos defaultHeight 2 1) ">:")))
            (if (not defaultHeight)
              (setq defaultHeight 3.0)
            )
            
            ;; 设置文字宽度系数
            (setq textWidth (getreal (strcat "\n输入文字宽度系数 <" (rtos textWidth 2 1) ">:")))
            (if (not textWidth)
              (setq textWidth 1.0)
            )
            
            ;; 保存新的参数到环境变量
            (setenv "EVSG\\TextHeight" (rtos defaultHeight 2 6))
            (setenv "EVSG\\TextWidth" (rtos textWidth 2 6))
            
            (initget "S")
            (setq startpoint (getpoint (strcat "\n指定插入点 [设置文字参数(S)] <当前字号:" (rtos defaultHeight 2 1) " 宽度:" (rtos textWidth 2 1) ">:")))
          )
          
          ;; 如果用户取消了插入点选择
          (if (not startpoint)
            (progn
              (princ "\n*取消*")
              (exit)
            )
          )
          
          (setq startpoint (trans startpoint 1 0))
          
          ;; 第六步：检测封闭矩形并调整插入点
          (setq center-point (find-enclosing-rectangle startpoint))
          (if center-point
            (progn
              (setq curpt center-point)
              (princ "\n检测到封闭矩形，文字将居中放置")
            )
            (progn
              (setq curpt startpoint)
              (princ "\n未检测到封闭矩形，使用指定位置")
            )
          )
          
          ;; 获取文字图层
          (setq tlayer (if (and (boundp 'TLayer) TLayer) TLayer "0"))
          
          ;; 第七步：在CAD中创建文字对象
          (foreach text text-list
            (entmake
              (list
                (cons 0 "TEXT")
                '(100 . "AcDbEntity")
                '(67 . 0)
                (cons 8 tlayer)
                (if (and (boundp '*defaultColor*) *defaultColor*)
                  (cons 62 *defaultColor*)
                  '(62 . 256)
                )
                '(100 . "AcDbText")
                (cons 10 curpt)
                (cons 40 defaultHeight)  ; 文字高度
                (cons 41 textWidth)      ; 文字宽度系数
                (cons 72 1)              ; 水平对齐：居中
                (cons 73 2)              ; 垂直对齐：中间
                (cons 11 curpt)          ; 对齐点
                (cons 1 text)
                (cons 7 "Standard")
                '(210 0.0 0.0 1.0)
              )
            )
            ;; 移动到下一个位置（向下）
            (setq curpt (polar curpt (* 1.5 pi) (* defaultHeight 1.5)))
          )
          
          (princ (strcat "\n已传输 " (itoa (length text-list)) " 个文字到CAD，字号: " (rtos defaultHeight 2 1) " 宽度: " (rtos textWidth 2 1) "，Excel选择区域已下移到: " new-range-address))
        )
        (progn
          (alert "请先在Excel中选择要传输的单元格范围！")
        )
      )
    )
    (progn
      (alert "未找到运行中的Excel应用程序\n请先打开Excel文档后再执行此命令")
    )
  )
)

;;; The C: function definition for EVSG
;;;
(defun c:evsg () 
  (evsg)
)

(princ)
;;************************************************************************

