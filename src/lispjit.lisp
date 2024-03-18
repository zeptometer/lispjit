(defpackage :lispjit
  (:use :common-lisp)
  (:export :interpret :fn))

(in-package :lispjit)

(defun lookup-var (env var)
  (if (null env)
      nil
      (let ((res (assoc var (car env))))
        (if res
            (cdr res)
            (lookup-var (cdr env) var)))))

(defun extend-env (vars args env)
  (cons (mapcar #'cons vars args) env))

(defstruct closure env args body)

(defun interpret% (code env)
  (cond
    ((integerp code) code)
    ((or (eq code t) (eq code nil)) code)
    ((symbolp code) (lookup-var env code))
    ((consp code)
     (destructuring-bind (head . tail) code
       (case head
         (fn (destructuring-bind (args body) tail
               (make-closure :env env :args args :body body)))
         ;; primitive functions
         (+ (destructuring-bind (a b) tail
              (let* ((ea (interpret% a env))
                     (eb (interpret% b env)))
                (+ ea eb))))
         (* (destructuring-bind (a b) tail
              (let* ((ea (interpret% a env))
                     (eb (interpret% b env)))
                (* ea eb))))
         (>= (destructuring-bind (a b) tail
               (let* ((ea (interpret% a env))
                      (eb (interpret% b env)))
                 (>= ea eb))))
         (not (destructuring-bind (a) tail
                (not (interpret% a env))))
         ;; primitive syntaxc
         (if (destructuring-bind (condition then else) tail
               (let ((ec (interpret% condition env)))
                 (if ec
                     (interpret% then env)
                     (interpret% else env)))))
         (t (let* ((ehead (interpret% head env))
                   (eargs (interpret%* tail env)))
              (cond
                ((closure-p ehead)
                 (let* ((newenv (extend-env (closure-args ehead) eargs (closure-env ehead))))
                   (interpret% (closure-body ehead) newenv)))))))))))

(defun interpret%* (code-list env)
  (mapcar #'(lambda (code) (interpret% code env)) code-list))

(defun interpret (code) (interpret% code nil))
