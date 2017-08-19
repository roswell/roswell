(defpackage :roswell.delete.dump
  (:use :cl :roswell.util))
(in-package :roswell.delete.dump)

(defun dump (_ &rest params)
  (declare (ignore _))
  (let* ((lisp (parse-version-spec
                 (or (ros:opt "*lisp")
                     (ros:opt "default.lisp"))))
         (version (or (second lisp) (ros:opt (format nil "~A.version" (first lisp)))))
         (impl (first lisp))
         (path (make-pathname :name (first params)
                              :type (core-extention impl)
                              :defaults
                              (merge-pathnames (format nil "impls/~A/~A/~A/~A/dump/"
                                                       (uname-m) (uname) impl version)
                                               (homedir)))))
    (if (probe-file path)
        (delete-file path)
        (error "The specified image doesn't exist \"~A/~A\"~%" impl version))))
