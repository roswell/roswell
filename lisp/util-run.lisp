(defpackage :ros.run
  (:use :cl)
  (:export :*run-assoc* :probe-run-script))
(in-package :ros.run)

(defvar *run-assoc* nil)

(defun probe-run-script (impl)
  (ignore-errors
   (quicklisp-client:register-local-projects)
   (module "run" impl)))
