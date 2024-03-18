(in-package :cl-user)
(defpackage lispjit-test-asd
  (:use :common-lisp :asdf :lispjit-asd :uiop))
(in-package :lispjit-test-asd)

(defsystem lispjit-test
  :version "0.0.1"
  :author "Yuito MURASE"
  :license "MIT"
  :depends-on (:lispjit :fiveam)
  :components ((:module "t" :components ((:file "lispjit"))))
  :perform (test-op (o s)
                    (symbol-call :fiveam :run! :lispjit)))
