(in-package #:cl-user)
(defpackage #:mapgen
  (:use #:cl+qt)
  ;; main.lisp
  (:export #:main))

(in-package #:mapgen)
(defparameter *debug* T)
(setf (v:repl-level) (if *debug* :debug :warn))
