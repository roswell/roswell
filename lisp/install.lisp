#!/bin/sh
#|-*- mode:lisp -*-|#
#|install roswell
exec ros -Q +R -L sbcl-bin -- $0 "$@"
|#

(ros:util "install%")

(in-package :ros.install)

(defun main (subcmd impl/version params &rest argv)
  (let* (imp
         (pos (position #\/ impl/version))
         (*ros-path* (make-pathname :defaults (ros:opt "argv0")))
         version sub)
    (let (*read-eval*)
      (ros::ros-opts (read-from-string params)))
    (if pos
        (setq version (subseq impl/version (1+ pos))
              imp (subseq impl/version 0 pos))
        (setq imp impl/version))
    (cond ((probe-impl-script imp)
           (install-impl imp version subcmd argv))
          ((probe-file (setf sub (make-pathname :defaults impl/version :type "ros")))
           (install-ros sub))
          ((or (ql-dist:find-system imp)
               (ql:where-is-system imp))
           (install-system-script imp))
          (t (error "'~A' is not a valid target for 'install' -- It should be a name of either:
+ a quicklisp-installable system
+ a common lisp installation ~%" imp)))))
