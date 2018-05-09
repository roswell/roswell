(defpackage :roswell.dist.enable
  (:use :cl))
(in-package :roswell.dist.enable)

(defun enable (&rest r)
  (dolist (elm (rest r))
    (let ((dist (ql-dist:find-dist elm)))
      (if dist
          (ql-dist:enable dist)
          (format t "~A not found.~%" elm)))))
