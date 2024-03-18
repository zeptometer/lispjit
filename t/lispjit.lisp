(in-package :cl-user)
(defpackage lispjit-test
  (:use :cl :fiveam :lispjit))
(in-package lispjit-test)

(def-suite :lispjit)
(in-suite :lispjit)

(defmacro evals-to (expected expr)
  `(is (eql ,expected (interpret ',expr))))

(test simple-expr
  (evals-to 2 2)
  (evals-to 2 (+ 1 1))
  (evals-to 10 (* 2 5))
  (evals-to t t)
  (evals-to nil nil)
  (evals-to t (>= 10 9))
  (evals-to nil (>= -10 9))
  (evals-to nil (not t))
  (evals-to t (not nil))
  (evals-to 1 (if t 1 0))
  (evals-to 0 (if nil 1 0)))

(test combined-expr
  (evals-to 19 (+ (+ -10 11) (* 3 6)))
  (evals-to 10 (+ 2 (if (>= 6 (+ 2 3)) (* 2 4) -199))))
