;; Text Stretch v1.0
;; Auto-stretch single line text to fit selected rectangular area
;; Text height dynamically adjusts to match area height, rounded to integer

;; Get rectangle bounds from two corner points
(defun get-rectangle-bounds-from-points (p1 p2)
  (list
    (min (car p1) (car p2))      ;; min X
    (max (car p1) (car p2))      ;; max X
    (min (cadr p1) (cadr p2))    ;; min Y
    (max (cadr p1) (cadr p2))    ;; max Y
  )
)

;; Calculate text height from rectangle based on direction
;; For horizontal rectangle: use width as text height
;; For vertical rectangle: use height as text height
(defun calculate-text-height (rect-bounds direction)
  (setq min-x (car rect-bounds))
  (setq max-x (cadr rect-bounds))
  (setq min-y (caddr rect-bounds))
  (setq max-y (cadddr rect-bounds))
  (setq width (- max-x min-x))
  (setq height (- max-y min-y))
  
  ;; Choose dimension based on direction
  (if (= direction "HORIZONTAL")
    (setq dimension width)
    (setq dimension height)
  )
  
  ;; Round to nearest integer
  (setq text-height (fix (+ dimension 0.5)))
  
  ;; Ensure minimum height of 1
  (if (< text-height 1)
    (setq text-height 1)
  )
  
  text-height
)

;; Get rectangle center point (not used anymore, kept for reference)
(defun get-rectangle-center (rect-bounds)
  (setq min-x (car rect-bounds))
  (setq max-x (cadr rect-bounds))
  (setq min-y (caddr rect-bounds))
  (setq max-y (cadddr rect-bounds))
  
  (list
    (/ (+ min-x max-x) 2.0)
    (/ (+ min-y max-y) 2.0)
    0
  )
)

;; Convert 2D point to 3D point
(defun to-3d-point (pt)
  (if (= (length pt) 2)
    (list (car pt) (cadr pt) 0)
    pt
  )
)

;; Determine rectangle direction (horizontal or vertical)
;; Returns "HORIZONTAL" if width > height, "VERTICAL" if height > width
(defun get-rectangle-direction (rect-bounds)
  (setq width (- (cadr rect-bounds) (car rect-bounds)))
  (setq height (- (cadddr rect-bounds) (caddr rect-bounds)))
  
  (if (> width height)
    "HORIZONTAL"
    "VERTICAL"
  )
)

;; Get text rotation angle based on direction
;; HORIZONTAL rectangle -> vertical text (270 degrees = 4.71239 radians, top to bottom)
;; VERTICAL rectangle -> horizontal text (0 degrees = 0 radians, left to right)
(defun get-text-rotation (direction)
  (if (= direction "HORIZONTAL")
    (* 270.0 (/ pi 180.0))  ;; Convert 270 degrees to radians
    0.0
  )
)

;; Create or get text style with specified font
(defun ensure-text-style (style-name font-file)
  ;; Check if style exists
  (if (not (tblsearch "STYLE" style-name))
    (progn
      ;; Create new text style
      (entmake (list
        (cons 0 "STYLE")
        (cons 100 "AcDbSymbolTableRecord")
        (cons 100 "AcDbTextStyleTableRecord")
        (cons 2 style-name)
        (cons 70 0)
        (cons 40 0.0)
        (cons 41 1.0)
        (cons 50 0.0)
        (cons 71 0)
        (cons 42 1.0)
        (cons 3 font-file)  ;; Font file name
        (cons 4 "")
      ))
      (princ (strcat "\nCreated text style: " style-name " with font: " font-file))
    )
  )
  style-name
)

;; Create text using native DTEXT command with pre-filled parameters
(defun create-text-with-command (base-pt text-height rotation style-name)
  ;; Set current text style
  (setvar "TEXTSTYLE" style-name)
  
  ;; Convert rotation from radians to degrees for TEXT command
  (setq rotation-deg (* rotation (/ 180.0 pi)))
  
  ;; Enable CMDECHO to show TEXT command execution
  (setvar "CMDECHO" 1)
  
  ;; Use native DTEXT command with all parameters pre-filled
  ;; DTEXT command sequence: basepoint -> height -> rotation -> text content
  ;; We provide: basepoint, height, rotation
  ;; Then DTEXT command takes over for user input
  (command "._DTEXT" base-pt text-height rotation-deg)
  
  ;; Program ends here, TEXT command continues for user input
)

;@name 文字拉伸
;@group 文本编辑
;@desc 框选一个矩形区域，输入单行文字，文字高度自动调整为框选区域的高度（整数），文字居中显示在该区�?
;@order 21
;@require DrawingOpen
(defun c:TTD ()
  ;; Local variables
  (setq rect-p1 nil)
  (setq rect-p2 nil)
  (setq rect-bounds nil)
  (setq text-height nil)
  (setq center-pt nil)
  (setq text-string nil)
  (setq text-entity nil)
  (setq direction nil)
  (setq rotation nil)
  (setq original-ortho nil)
  
  (setvar "CMDECHO" 0)
  
  ;; Save original ORTHO setting
  (setq original-ortho (getvar "ORTHOMODE"))
  
  (princ "\n=== Text Stretch v1.0 ===")
  
  ;; Step 1: Enable ORTHO mode temporarily
  (setvar "ORTHOMODE" 1)
  (princ "\nOrtho mode enabled temporarily")
  
  ;; Step 2: Get first corner of rectangle
  (princ "\nSelect first corner of rectangle: ")
  (setq rect-p1 (getpoint))
  
  (if (not rect-p1)
    (progn
      (princ "\nError: No point selected!")
      (setvar "ORTHOMODE" original-ortho)
      (princ)
      (exit)
    )
  )
  
  ;; Step 3: Get second corner of rectangle
  (princ "Select second corner of rectangle: ")
  (setq rect-p2 (getpoint rect-p1))
  
  ;; Restore original ORTHO setting immediately after getpoint
  (setvar "ORTHOMODE" original-ortho)
  
  (if (not rect-p2)
    (progn
      (princ "\nError: No second point selected!")
      (princ)
      (exit)
    )
  )
  
  ;; Step 4: Calculate rectangle bounds
  (setq rect-bounds (get-rectangle-bounds-from-points rect-p1 rect-p2))
  
  (princ (strcat "\nRectangle bounds: X(" 
                 (rtos (car rect-bounds) 2 2) "-" (rtos (cadr rect-bounds) 2 2) ") "
                 "Y(" (rtos (caddr rect-bounds) 2 2) "-" (rtos (cadddr rect-bounds) 2 2) ")"))
  
  ;; Step 5: Determine rectangle direction
  (setq direction (get-rectangle-direction rect-bounds))
  
  (if (= direction "HORIZONTAL")
    (princ "\nDetected: Horizontal rectangle - text will be vertical (top to bottom)")
    (princ "\nDetected: Vertical rectangle - text will be horizontal (left to right)")
  )
  
  ;; Step 6: Calculate text height based on direction
  (setq text-height (calculate-text-height rect-bounds direction))
  
  (princ (strcat "\nCalculated text height: " (itoa text-height)))
  
  ;; Step 7: Get text rotation angle based on direction
  (setq rotation (get-text-rotation direction))
  
  (princ (strcat "\nText rotation: " (rtos (* rotation (/ 180.0 pi)) 2 1) " degrees"))
  
  ;; Step 8: Ensure text style exists
  (setq style-name (ensure-text-style "TTD_SONGTI" "simsun.ttf"))
  
  ;; Step 9: Use first point as text base point
  (setq center-pt (to-3d-point rect-p1))
  
  (princ (strcat "\nText base point: (" 
                 (rtos (car center-pt) 2 2) "," (rtos (cadr center-pt) 2 2) ")"))
  
  ;; Step 10: Create text using native DTEXT command
  (princ "\nStarting text input (use native DTEXT command)...")
  (create-text-with-command center-pt text-height rotation style-name)
  
  (if text-entity
    (princ "\nSuccess: Text created and stretched to fit rectangle.")
    (princ "\nText input completed.")
  )
  
  (princ "\n=== Operation complete ===")
  (princ)
)

(princ "\nText Stretch v1.0 loaded. Type TTD to start.")
