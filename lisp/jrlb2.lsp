; Joseph Randell L. Benavidez
; 2001-49967
; August 18, 2004
;
; CMSC 137 U-2L Lisp Seatwork #2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; 5.) my-remove (x y) - use iteration or recursion,
;       don't use remove

(defun my-remove (L n)
  (setq newList nil)
  (dolist (x L newList)
    (if (not (equal x n))
      (setq newList (cons x newList))
    )
  )
  (reverse newList)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; 4.) common (L1 L2) - returns list containing 
;       elements common to L1 & L2, use iteration
;       or recursion

(defun common (L1 L2)
  (setq newList nil)
  (dolist (x L1)
    (dolist  (y L2)
      (if (equal x y)
        (setq newList (cons x newList))
      )
    )
  )
 (print newList)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; 3.) my-reverse (L) - don't use reverse, dolist
;       user iteration or recursion

(defun my-reverse (L)
  (cons (car L) myList)
)