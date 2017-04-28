(defpackage :roswell.list.git
  (:use :cl :roswell.util))
(in-package :roswell.list.git)

(defun git (&rest r)
  (declare (ignore r))
  (let* ((* (directory (merge-pathnames "**/.git/" (first ql:*local-project-directories*))))
         (* (mapcar (lambda (x) (directory (merge-pathnames "../*.asd" x))) *))
         (* (apply #'append *)))
    (format t "~{~A~%~}" (mapcar #'pathname-name *))))
