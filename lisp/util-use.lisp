(roswell:include "util" "util-use")
(defpackage :roswell.util.use
  (:use :cl :roswell.util)
  (:export :use))
(in-package :roswell.util.use)

(defun use (impl &rest args)
  "Parse the lisp version string (such as ccl-bin/1.11) and set it to the correct config slot(s)"
  (destructuring-bind (lisp version) (parse-version-spec impl)
    (cond
      ((let ((func (module "use" lisp)))
         (when func
           (apply func lisp version args))
         func))
      ((and impl
            (ignore-errors
             (roswell:roswell `("-L" ,impl "version=t" "run"))))
       (cond ((and lisp version)
              (setf (config "default.lisp") lisp
                    (config (format nil "~A.version" lisp)) version))
             (lisp
              (setf (config "default.lisp")
                    lisp)))
       t))))
