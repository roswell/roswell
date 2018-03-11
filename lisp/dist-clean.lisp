(defpackage :roswell.dist.clean
  (:use :cl)
  (:shadow :list))
(in-package :roswell.dist.clean)

(defun clean (&rest r)
  (setf r (cdr r))
  (let* ((name (first r))
         (dist (ql-dist:find-dist name)))
    (if dist
        (ql-dist:clean dist)
        (format t "dist:~A not found.~%" name))))
