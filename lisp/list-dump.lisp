(defpackage :ros.list.dump
  (:use :cl :ros.util))
(in-package :ros.list.dump)

(defun dump (&rest params)
  (let ((impl (impl (first params))))
    (format *error-output* "List of dumped images for ~A:~%" impl)
    (format t "~{~A~%~}"
            (mapcar #'pathname-name
                    (directory (make-pathname :name :wild
                                              :type (ros.util:core-extention impl)
                                              :defaults
                                              (merge-pathnames (format nil "impls/~A/~A/~A/dump/"
                                                                       (uname-m) (uname) impl)
                                                               (homedir))))))))
