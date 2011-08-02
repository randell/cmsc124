; Joseph Randell L. Benavidez
; 2001-49967
; August 11, 2004
;
; CMSC 137 U-2L Lisp Seatwork #1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; 1.) use car/cdr to extract 'bill'
;
; a.)

(setq L1 '(((tim)) (bill) tom))
(car (car (cdr L1)))

;
; b.) 

(setq L2 '(tim (jim) ((bill)) (tom) john))
(car (car (car (cdr (cdr L2)))))

;
; c.) 

(setq L3 '(((tim) (jim (bill))) tom))
(car (car (cdr (car (cdr (car L3))))))

;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; 2.) define the following functions:
;
; a.) f1 (x y z)
;     = ((x + y) (y + z)) / ((x - y - z) (x - y - z))

(defun f1 (x y z)
  (/ (* (+ x y) (+ y z)) (* (- (- x y) z) (- (- x y) z)))
)

; b.) volCone (r h)
;     -computes for the volume of a cone

(defun volCone (r h)
  (* (* (/ 1.0 3.0) 3.14) (* r r) h)
)

; c.) f2 ()
;     -asks for diameter 'd' and height 'h'
;     -compute and print volume of cone; use
;      function defined in b.

(defun f2 ()
  (print "Let's compute the volume of a cone...")
  (princ "Enter value for Diameter: ")
  (setq d (read))
  (setq r (/ d 2.0))
  (princ "Enter value for Height:   ")
  (setq h (read))
  (princ "Volume of the cone = ")
  (print (volCone(r h)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; 3.) use recursion; choose one:
;
; a.) count-even(L)
;     -given L, return number of even numbers

(defun count-even (L)
  (cond
    ((null L) 0)    
    ((listp L)
      (cond 
        ((evenp (car L))
          (+ 1 (count-even (cdr L)))
        )
	((oddp (car L))
	  (count-even (cdr L))
	)
      )
    )
    ((atom L)
      (cond 
        ((evenp L) 1)
	((oddp L) 0)
      )
    )
  )
)

; b.) sum-even(L)
;     -given L, return sum of even numbers in L

(defun sum-even (L)
  (cond
    ((null L) 0)
    ((listp L)
      (cond 
        ((evenp (car L))
          (+ (car L) (sum-even (cdr L)))
        )
	((oddp (car L))
	  (sum-even (cdr L))
	)
      )
    )
    ((atom L)
      (cond 
        ((evenp L) L)
	((oddp L) 0)
      )
    )
  )
)

; c.) is-here(L n)
;     -given list L, atom n, return T if n is in L.
;      nil otherwise

(defun is-here (L n)
  (cond
    ((null L) nil)
    ((listp L)
      (cond
        ((equal (car L) n) T)
	((not (equal (car L) n)) 
          (is-here (cdr L) n)
	)
      )
    )
  )
)

; d.) divide (x y)
;     -use repeated subtraction
;     -integer division

(defun divide (x y)
  (cond
    ((= y 0) nil)
    ((< x y) 0)
    ((>= x y)
      (+ 1 (divide (- x y) y))
    )
  )
)