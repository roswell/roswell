(defpackage :roswell.dist.delete
  (:use :cl)
  (:shadow :delete))
(in-package :roswell.dist.delete)

(defun delete (&rest r)
  (dolist (elm (rest r))
    (ql-dist:uninstall (ql-dist:find-dist elm))))

