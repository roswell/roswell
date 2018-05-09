(defpackage :roswell.dist.disable
  (:use :cl))
(in-package :roswell.dist.disable)

(defun disable (&rest r)
  (dolist (elm (rest r))
    (let ((dist (ql-dist:find-dist elm)))
      (if dist
          (ql-dist:disable dist)
          (format t "~A not found.~%" elm)))))
