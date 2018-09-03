(defpackage :roswell.dist.add
  (:use :cl))
(in-package :roswell.dist.add)

(defun add (&rest r)
  (dolist (elm (rest r))
    (ql-dist:install-dist elm :prompt nil)))