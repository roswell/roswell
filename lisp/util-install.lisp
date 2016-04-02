(cl:in-package :cl-user)
(when (cl:find-package :ros.install.util)
  (pushnew :ros.install.util *features*))

(defpackage :ros.install
  (:use :cl :ros.util)
  (:export :*build-hook*))

(in-package :ros.install)

(defvar *ros-path* nil)
(defvar *help-cmds* nil)
(defvar *install-cmds* nil)
(defvar *list-cmd* nil)

(defun set-opt (item val)
  (let ((found (assoc item (ros::ros-opts) :test 'equal)))
    (if found
        (setf (second found) val)
        (push (list item val) ros::*ros-opts*))))

(defun read-call (func &rest params)
  (ignore-errors (apply (let (*read-eval*) (read-from-string func)) params)))

(defun probe-impl-script (impl)
  (or (and
       (equal "quicklisp" impl)
       (load (make-pathname :name "install+quicklisp" :type "lisp" :defaults *load-pathname*)))
      (let ((imp (format nil "roswell.install.~A" impl)))
        (and (or (read-call "ql-dist:find-system" imp)
                 (read-call "ql:where-is-system" imp))
             (read-call "ql:quickload" imp)))))

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
