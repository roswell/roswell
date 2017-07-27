(defpackage :roswell.list.env
  (:use :cl)
  (:export :env :env-list))
(in-package :roswell.list.env)

(defun env-list ()
  (mapcar (lambda (i) (first (last (pathname-directory i))))
          (directory (merge-pathnames "env/*/config" (roswell:opt "homedir")))))

(defun env (&rest r)
  (cond ((null r)
         (dolist (i (env-list))
           (format t "~A~A~%" (if (equal (roswell:opt "roswellenv") i) :* " ") i)))))
