(in-package :cl-user)
(defpackage lispjit-asd
  (:use :common-lisp :asdf))
(in-package :lispjit-asd)

(defsystem lispjit
  :version "0.0.1"
  :author "Yuito Murase"
  :license "MIT"
  :description "A poc lisp interpreter with just-in-time code generation"
  :components ((:file "lispjit"))
  :in-order-to ((test-op (test-op lispjit-test))))
