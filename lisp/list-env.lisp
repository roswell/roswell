(roswell:include "util-config")

(defpackage :roswell.list.env
  (:use :cl)
  (:export :env :env-list))
(in-package :roswell.list.env)

(defun env-list ()
  (mapcar (lambda (i) (first (last (pathname-directory i))))
          (directory (merge-pathnames "env/*/config" (roswell:opt "homedir")))))

(defun env (&rest r)
  (let ((name (third (assoc "roswellenv" (roswell.util.config:load-config ".roswellenv") :test 'equal))))
    (cond ((null r)
	   (dolist (i (env-list))
	     (format t "~A~A~%" (if (equal name i) :* " ") i))))))
