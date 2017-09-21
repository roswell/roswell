(defpackage :roswell.list.dump
  (:use :cl :roswell.util))
(in-package :roswell.list.dump)

(defun dump (&rest args)
  (setf args (cdr args))
  (loop
    with impl = (impl (or (first args)
                          (ros:opt "*lisp")
                          (ros:opt "default.lisp")))
      initially (format *error-output* "List of dumped images for ~A:~%" impl)
    with path = (merge-pathnames
                 (format nil "impls/~A/~A/~A/dump/" (uname-m) (uname) impl)
                 (homedir))
    for i in (directory (make-pathname :name :wild
                                       :type (core-extention impl)
                                       :defaults path))
    do (format t "~A~%" (pathname-name i))))
