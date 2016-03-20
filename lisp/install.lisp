#!/bin/sh
#|-*- mode:lisp -*-|#
#|install roswell
exec ros -Q +R -L sbcl-bin -- $0 "$@"
|#

(ros:util "install%")

(in-package :ros.install)

(defun main (subcmd impl/version &rest argv)
  (let* (imp
         (seq impl/version)
         (pos (position #\/ impl/version))
         (*ros-path* (make-pathname :defaults (ros:opt "argv0")))
         version sub)
    (if pos
        (setq imp (subseq seq 0 pos)
              version (subseq seq (1+ pos)))
        (setq imp seq))
    (cond ((probe-impl-script imp)
           (let (*read-eval*)
             (ros::ros-opts (read-from-string (first argv))))
           (install-impl imp version subcmd (rest argv)))
          ((probe-file (setf sub (make-pathname :defaults impl/version :type "ros")))
           (install-ros sub))
          ((or (ql-dist:find-system imp)
               (ql:where-is-system imp))
           (install-system-script imp))
          (t (error "'~A' is not a valid target for 'install' -- It should be a name of either:
+ a quicklisp-installable system
+ a common lisp installation ~%" imp)))))
