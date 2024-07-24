(defpackage reformer
  (:use
    :cl
    :clack
    :cl-who
    :parenscript)
  (:export
    #:main))

(in-package :reformer)

(defun main ()
  (format t "Reformer is up and running!~%"))
