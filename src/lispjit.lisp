(defpackage :lispjit
  (:use :common-lisp)
  (:export :interpret))

(in-package :lispjit)

(defun interpret% (code)
  (cond
    ((integerp code) code)
    ((or (eq code t) (eq code nil)) code)
    ((consp code)
     (destructuring-bind (op . args) code
       (case op
         (+ (destructuring-bind (a b) args
               (let* ((ea (interpret% a))
                      (eb (interpret% b)))
                 (+ ea eb))))
         (* (destructuring-bind (a b) args
               (let* ((ea (interpret% a))
                      (eb (interpret% b)))
                 (* ea eb))))
         (>= (destructuring-bind (a b) args
                (let* ((ea (interpret% a))
                       (eb (interpret% b)))
                  (>= ea eb))))
         (not (destructuring-bind (a) args
                (not (interpret% a))))
         (if (destructuring-bind (condition then else) args
               (let ((ec (interpret% condition)))
                 (if ec
                     (interpret% then)
                     (interpret% else))))))))))

(defun interpret (code) (interpret% code))
