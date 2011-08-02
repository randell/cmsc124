; Joseph Randell L. Benavidez
; 2001-49967
; August 11, 2004
; CMSC 124 V-3L

; This program takes is a list an returns the last
; element of the list; handicap: should not use the
; built-in functions 'last' or 'reverse'

(defun my_last(l)
	(cond
		((atom l) l)
		((= (length l) 1) l)
		((listp l) (my_last (cdr l)))
	)
)