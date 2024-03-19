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

(test function
  (evals-to 1 ((fn (x) 1) 2))
  (evals-to 2 ((fn (x) x) 2))
  (evals-to 30 ((fn (x y) (+ x y)) 10 20))
  (evals-to 40 ((fn (f x) (f x)) (fn (x) (* x 2)) 20))
  (evals-to 40 (((fn (x) (fn (y) (+ x y))) 10) 30)))

(test recursive-function
  (evals-to 1 ((recfn (self x) 1) 2))
  ;; fibonacci
  (evals-to 13 ((recfn (self x) (if (not (>= x 2)) 1 (+ (self (+ x -1)) (self (+ x -2))))) 6)))

(defmacro compiles-to (expected expr env)
  `(is (equal ',expected (compile-body ',expr ',env))))

(test compile-body
  (compiles-to 1 1 env)
  (compiles-to t t env)
  (compiles-to nil nil env)
  (compiles-to (lispjit::lookup-var env 'x) x env)
  (compiles-to (+ 1 (lispjit::lookup-var env 'x)) (+ 1 x) env)
  (compiles-to (- 1 (lispjit::lookup-var env 'x)) (- 1 x) env)
  (compiles-to (>= 1 (lispjit::lookup-var env 'x)) (>= 1 x) env)
  (compiles-to (not (lispjit::lookup-var env 'x)) (not x) env)
  (compiles-to (if (lispjit::lookup-var env 'x)
                   (lispjit::lookup-var env 'y)
                   (lispjit::lookup-var env 'z))
               (if x y z) env)
  (compiles-to (lispjit::make-closure
                :env env
                :args '(x y)
                :body '(+ x y)
                :ncall 0
                :bin nil)
               (fn (x y) (+ x y)) env)
  (compiles-to (lispjit::make-recursive-closure
                :env env
                :args '(x y)
                :body '(+ x y)
                :ncall 0
                :bin nil)
               (recfn (x y) (+ x y)) env)
  (compiles-to (lispjit::invoke-closure
                (lispjit::lookup-var env 'x)
                (list (lispjit::lookup-var env 'y)))
               (x y)
               env))

(test compie-closure-does-not-explode
  (is (lispjit::compile-closure (interpret '(fn (x) (+ x 1))))))

