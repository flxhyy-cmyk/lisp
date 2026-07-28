;--------------------------------------------------------- `
(defun c:`(/ ent)
(setq ent (ssget))
(command "change" ent "" "p" "c" "bylayer" "")
)
;--------------------------------------------------------- 0
(defun c:00(/ ent)
(setq ent (ssget))
(command "change" ent "" "p" "c" "byblock" "")
)
;--------------------------------------------------------- 1
(defun c:11(/ ent)
(setq ent (ssget))
(command "change" ent "" "p" "c" "1" "")
)
;--------------------------------------------------------- 2
(defun c:22(/ ent)
(setq ent (ssget))
(command "change" ent "" "p" "c" "2" "")
)
;--------------------------------------------------------- 3
(defun c:33(/ ent)
(setq ent (ssget))
(command "change" ent "" "p" "c" "3" "")
)
;--------------------------------------------------------- 4
(defun c:44(/ ent)
(setq ent (ssget))
(command "change" ent "" "p" "c" "4" "")
)
;--------------------------------------------------------- 5
(defun c:55(/ ent)
(setq ent (ssget))
(command "change" ent "" "p" "c" "5" "")
)
;--------------------------------------------------------- 6
(defun c:66(/ ent)
(setq ent (ssget))
(command "change" ent "" "p" "c" "6" "")
)
;--------------------------------------------------------- 7
(defun c:77(/ ent)
(setq ent (ssget))
(command "change" ent "" "p" "c" "7" "")
)
;--------------------------------------------------------- 8
(defun c:88(/ ent)
(setq ent (ssget))
(command "change" ent "" "p" "c" "8" "")
)