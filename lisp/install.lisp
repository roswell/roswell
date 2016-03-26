#!/bin/sh
#|-*- mode:lisp -*-|#
#|install roswell
exec ros -Q +R -L sbcl-bin -- $0 "$@"
|#

#-ros.util
(ros:util)

(defpackage :ros.install
  (:use :cl :ros.util)
  (:export :*build-hook*))

(in-package :ros.install)

(when (probe-file (merge-pathnames "impls/ALL/ALL/quicklisp/setup.lisp" (homedir)))
  (ros:util "install%"))

(defvar *ros-path* nil)
(defvar *help-cmds* nil)
(defvar *install-cmds* nil)
(defvar *list-cmd* nil)

(defun set-opt (item val)
  (let ((found (assoc item (ros::ros-opts) :test 'equal)))
    (if found
        (setf (second found) val)
        (push (list item val) ros::*ros-opts*))))

(defun probe-impl-script (impl)
  (let ((sub (make-pathname :name nil :type nil :defaults *load-pathname*)))
    (and
     (setf sub (or (probe-file (merge-pathnames (format nil "install-~A.lisp" impl) sub))
                   (probe-file (merge-pathnames (format nil "install+~A.lisp" impl) sub))))
     (load sub))))

(defun install-impl (impl version subcmd argv)
  (let ((cmds (cond
                ((equal subcmd "install") (cdr (assoc impl *install-cmds* :test #'equal)))
                ((equal subcmd "help") (cdr (assoc impl *help-cmds* :test #'equal))))))
    (when cmds
      (let ((param `(t :target ,impl :version ,version :argv ,argv)))
        (handler-case
            (loop for call in cmds
               do (setq param (funcall call (rest param)))
               while (first param))
          #+sbcl
          (sb-sys:interactive-interrupt (condition)
            (declare (ignore condition))
            (format t "SIGINT detected, cleaning up the partially installed files~%")
            (ros:roswell `(,(format nil "deleteing ~A/~A" (getf (cdr param) :target) (getf (cdr param) :version))) :string t)))))))

(defun read-call (func &rest params)
  (ignore-errors (apply (let (*read-eval*) (read-from-string func)) params)))

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
           (read-call "install-ros" sub))
          ((or (read-call "ql-dist:find-system" imp)
               (read-call "ql:where-is-system" imp))
           (read-call "install-system-script" imp))
          (t (error "'~A' is not a valid target for 'install' -- It should be a name of either:
+ a quicklisp-installable system
+ a common lisp installation ~%" imp)))))
