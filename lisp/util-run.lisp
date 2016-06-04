(defpackage :ros.run
  (:use :cl)
  (:export :*run-assoc* :probe-run-script))
(in-package :ros.run)

(defvar *run-assoc* nil)

(defun probe-run-script (impl)
  (ignore-errors
   (quicklisp-client:register-local-projects)
   (let ((imp (format nil "roswell.run.~A" impl)))
     (and (or (ql-dist:find-system imp)
              (ql:where-is-system imp))
          (ql:quickload imp :silent t)))))
