(ros:include "util")
(ros:include "util-use" :load nil)

(defpackage :ros.use
  (:use :cl :ros.util)
  (:export :use))

(in-package :ros.use)

(defun use (arg)
  "Parse the lisp version string (such as ccl-bin/1.11) and set it to the correct config slot(s)"
  (destructuring-bind (lisp version) (parse-version-spec arg)
    (cond
      ((and arg
            (ignore-errors
             (ros:roswell `("-L" ,arg "version=t" "run"))))
       (cond ((and lisp version)
              (setf (config "default.lisp") lisp
                    (config (format nil "~A.version" lisp)) version))
             (lisp
              (setf (config "default.lisp")
                    lisp)))
       t)
      (t (let ((func (ros.util:module "use" lisp)))
           (funcall func version))))))
