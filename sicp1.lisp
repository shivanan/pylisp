;; Some code from Structure and Interpretation of Computer Programs
;; yes, that's scheme and this is, um, lisp, but whatever
(defun sum (term a next b)
  (if (> a b)
    0
    ( + (term a)
	(sum term (next a) next b))))
(defun sum-cubes (a b)
  (defun cube (a) (* a a a))
  (defun inc (n) (+ n 1))
  (sum cube a inc b))

(defun pi-sum (a b)
  (defun pi-term (x)
    (/ 1.0 (* x (+ x 2))))
  (defun pi-next (x)
    (+ x 4))
  (sum pi-term a pi-next b))

(defun compute-pi (degree)
  (* 8 (pi-sum 1 degree)))
(print "PI:" (compute-pi 2000)) ; need to set sys.setrecursionlimit() to make this work
(setq cube-sum-10 3025)
(setq actual-cube-sum (sum-cubes 1 10))
(cond 
  ((eq cube-sum-10 actual-cube-sum) (print "Cube Sum Valid:" cube-sum-10))
  (t (print "***Cube Sum Failed***") (print "Expected:" cube-sum-10) (print "Got:" actual-cube-sum))
  )


;; Reversal
(defun append (x _list)
  (add  x _list)
  )
(defun reverse (lst)
  (cond
    ((null lst) (list ) )
    (t 
      (append (reverse (cdr lst))(list (car lst))))
    )
  )
(setq initial (list 1 2 3 4 5))
(setq final (list 5 4 3 2 1))
(setq actual (reverse initial))
(print "Reversal Test")
(print "Initial:" initial)
(print "Final:" actual)
(cond ((eq final actual) (print "Test Passed"))
      (t (print "**Test Failed**")))


;; Test for lexically-scoped closures
;;Paul Graham's accumulator - http://www.paulgraham.com/accgen.html
(print "Paul Graham's Accumulator Test")
(defun foo (n) (lambda (i) (incf n i)))
(setq add-two (foo 2))
(print (add-two 0))
(print (add-two 2))
(print (add-two 4))
(print (add-two 2))
(setq add-nine (foo 9))
(print (add-nine 4))
(print (add-nine 0))
(print (eq add-nine add-two))
(print (eval '(+ 2 2)))
(print + '+)
