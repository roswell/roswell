(defpackage :roswell.dist.default
  (:use :cl))
(in-package :roswell.dist.default)

(defun default (&rest r)
  (format t "~S" r))
