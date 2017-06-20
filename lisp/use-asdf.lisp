(defpackage :roswell.use.asdf
  (:use :cl :roswell.util))
(in-package :roswell.use.asdf)

(defun asdf (impl version &rest r)
  (declare (ignore r impl))
  (cond
    ((null version)
     (let ((s *error-output*)
           (asdf (config "asdf.version")))
       (format s (if (zerop (length asdf))
                     "ASDF version is not specified.~%"
                     "choosen version is ~S~%") asdf))
     t)
    ((member version '("no" "-") :test 'equal)
     (setf (config "asdf.version") nil)
     t)
    ((probe-file (merge-pathnames (format nil "lisp/asdf/~A/asdf.lisp" version) (homedir)))
     (setf (config "asdf.version") version))
    (t (error "~A not installed" version))))
