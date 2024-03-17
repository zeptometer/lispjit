(in-package :cl-user)
(defpackage lispjit-test
  (:use :cl :fiveam))
(in-package lispjit-test)

(def-suite :lispjit)
(in-suite :lispjit)

(test lispjittest
      (is (= 2 (+ 1 1))))
