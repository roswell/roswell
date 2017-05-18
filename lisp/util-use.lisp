(roswell:include "util" "util-use")
(defpackage :roswell.util.use
  (:use :cl :roswell.util)
  (:export :use))
(in-package :roswell.util.use)

(defun use (arg)
  "Parse the lisp version string (such as ccl-bin/1.11) and set it to the correct config slot(s)"
  (destructuring-bind (lisp version) (parse-version-spec arg)
    (cond
      ((and arg
            (ignore-errors
             (roswell:roswell `("-L" ,arg "version=t" "run"))))
       (cond ((and lisp version)
              (setf (config "default.lisp") lisp
                    (config (format nil "~A.version" lisp)) version))
             (lisp
              (setf (config "default.lisp")
                    lisp)))
       t)
      (t (let ((func (module "use" lisp)))
           (funcall (or func 'identity) version))))))
