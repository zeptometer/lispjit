(defpackage :lispjit
  (:use :common-lisp)
  (:export :interpret :fn :recfn :*time-to-compile*
           ;; visible for testing purpose
   :compile-body))

(in-package :lispjit)

(defparameter *time-to-compile* 10)

(defun lookup-var (env var)
  (if (null env)
      nil
      (let ((res (assoc var (car env))))
        (if res
            (cdr res)
            (lookup-var (cdr env) var)))))

(defun extend-env (vars args env)
  (cons (mapcar #'cons vars args) env))

(defstruct closure env args body ncall bin)
(defstruct recursive-closure env args body ncall bin)

(defun invoke-closure (clos args)
  (cond
    ((closure-p clos)
     (incf (closure-ncall clos))
     (when (and (not (closure-bin clos)) (>= (closure-ncall clos) *time-to-compile*))
       (setf (closure-bin clos) (compile-closure clos)))
     (let ((maybebin (closure-bin clos)))
       (if maybebin
           (funcall maybebin args)
           (let* ((newenv (extend-env (closure-args clos) args (closure-env clos))))
             (interpret% (closure-body clos) newenv)))))
    ((recursive-closure-p clos)
     (incf (recursive-closure-ncall clos))
     (when (and (not (recursive-closure-bin clos)) (>= (recursive-closure-ncall clos) *time-to-compile*))
       (format t "compile!~%")
       (setf (recursive-closure-bin clos) (compile-recursive-closure clos)))
     (let ((maybebin (recursive-closure-bin clos)))
       (if maybebin
           (funcall maybebin args)
           (let* ((newenv (extend-env (recursive-closure-args clos)
                                      (cons clos args)
                                      (recursive-closure-env clos))))
             (interpret% (recursive-closure-body clos) newenv)))))))

(defun interpret% (code env)
  (cond
    ((integerp code) code)
    ((or (eq code t) (eq code nil)) code)
    ((symbolp code) (lookup-var env code))
    ((consp code)
     (destructuring-bind (head . tail) code
       (case head
         ;; function
         (fn (destructuring-bind (args body) tail
               (make-closure :env env :args args :body body :ncall 0 :bin nil)))
         ;; recursive function
         ;; first argument is the function itself
         (recfn (destructuring-bind (args body) tail
                  (make-recursive-closure :env env :args args :body body :ncall 0 :bin nil)))
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
              (invoke-closure ehead eargs))))))))

(defun interpret%* (code-list env)
  (mapcar #'(lambda (code) (interpret% code env)) code-list))

;;; let's compile
;;; (+ (* 1 x) ((fn (x) x) 1))
(defun compile-body (body env)
  (cond
    ((integerp body) body)
    ((or (eq body t) (eq body nil)) body)
    ((symbolp body) `(lookup-var ,env ',body))
    ((consp body)
     (destructuring-bind (head . tail) body
       (case head
         (fn (destructuring-bind (args body) tail
               `(make-closure :env ,env :args ',args :body ',body :ncall 0 :bin nil)))
         (recfn (destructuring-bind (args body) tail
                  `(make-recursive-closure :env ,env :args ',args :body ',body :ncall 0 :bin nil)))
         (+ (destructuring-bind (a b) tail
              (let* ((ca (compile-body a env))
                     (cb (compile-body b env)))
                `(+ ,ca ,cb))))
         (- (destructuring-bind (a b) tail
              (let* ((ca (compile-body a env))
                     (cb (compile-body b env)))
                `(- ,ca ,cb))))
         (>= (destructuring-bind (a b) tail
               (let* ((ca (compile-body a env))
                      (cb (compile-body b env)))
                 `(>= ,ca ,cb))))
         (not (destructuring-bind (a) tail
                `(not ,(compile-body a env))))
         (if (destructuring-bind (condition then else) tail
               (let* ((ccond (compile-body condition env))
                      (cthen (compile-body then env))
                      (cbody (compile-body else env)))
                 `(if ,ccond ,cthen ,cbody))))
         (t (let* ((chead (compile-body head env))
                   (cargs (mapcar #'(lambda (x) (compile-body x env)) tail)))
              `(invoke-closure ,chead (list ,@cargs)))))))))

(defun compile-rec-body (body env self self-compiled)
  (cond
    ((integerp body) body)
    ((or (eq body t) (eq body nil)) body)
    ((symbolp body) `(lookup-var ,env ',body))
    ((consp body)
     (destructuring-bind (head . tail) body
       (case head
         (fn (destructuring-bind (args body) tail
               `(make-closure :env ,env :args ',args :body ',body :ncall 0 :bin nil)))
         (recfn (destructuring-bind (args body) tail
                  `(make-recursive-closure :env ,env :args ',args :body ',body :ncall 0 :bin nil)))
         (+ (destructuring-bind (a b) tail
              (let* ((ca (compile-rec-body a env self self-compiled))
                     (cb (compile-rec-body b env self self-compiled)))
                `(+ ,ca ,cb))))
         (- (destructuring-bind (a b) tail
              (let* ((ca (compile-rec-body a env self self-compiled))
                     (cb (compile-rec-body b env self self-compiled)))
                `(- ,ca ,cb))))
         (>= (destructuring-bind (a b) tail
               (let* ((ca (compile-rec-body a env self self-compiled))
                      (cb (compile-rec-body b env self self-compiled)))
                 `(>= ,ca ,cb))))
         (not (destructuring-bind (a) tail
                `(not ,(compile-rec-body a env self self-compiled))))
         (if (destructuring-bind (condition then else) tail
               (let* ((ccond (compile-rec-body condition env self self-compiled))
                      (cthen (compile-rec-body then env self self-compiled))
                      (cbody (compile-rec-body else env self self-compiled)))
                 `(if ,ccond ,cthen ,cbody))))
         (t
          (let* ((cargs (mapcar #'(lambda (x) (compile-rec-body x env  self self-compiled)) tail)))
            (if (eq self head)
                `(,self-compiled (list ,@cargs))
                (let ((chead (compile-rec-body head env self self-compiled)))
                  `(invoke-closure ,chead (list ,@cargs)))))))))))

(defun compile-closure (clos)
  (funcall (eval 
            `(lambda (env)
               (lambda (eargs)
                 (let ((newenv (extend-env ',(closure-args clos) eargs env)))
                   ,(compile-body (closure-body clos) 'newenv)))))
   (closure-env clos)))

(defun compile-recursive-closure (clos)
  (let ((fname (gensym "fname"))
        (env (gensym "env"))
        (args (gensym "args"))
        (newenv (gensym "newenv"))
        (selfsym (first (recursive-closure-args clos))))
    (let ((code `(lambda (,env)
                   (labels ((,fname (,args)
                              (let ((,newenv (extend-env ',(cdr (recursive-closure-args clos)) ,args ,env)))
                                ,(compile-rec-body (recursive-closure-body clos) newenv selfsym fname))))
           #',fname))))
;      (format t "compiles ~a~%to~%~a~%" (recursive-closure-body clos) code)
      (funcall (eval code) env))))

(defun interpret (code) (interpret% code nil))
