(defpackage :roswell.delete.dump
  (:use :cl :ros.util))
(in-package :roswell.delete.dump)

(defun dump (params)
  (declare (ignorable params))
  (print (list params (config "default.lisp")))
  (let* ((impl (and (second params) (parse-version-spec (second params))))
         (name (first params))
         path)
    (let ((lisp (or (first impl) (config "default.lisp"))))
      (setf impl (format nil "~A/~A" lisp
                         (or (second impl) (config (format nil "~A.version" lisp))))))
    (setf path (make-pathname :name name
                              :type (core-extention impl)
                              :defaults
                              (merge-pathnames (format nil "impls/~A/~A/~A/dump/"
                                                       (uname-m) (uname) impl)
                                               (homedir))))
    (if (probe-file path)
        (delete-file path)
        (format *error-output* "The specified image doesn't exist \"~A ~A\"~%" name impl))))
