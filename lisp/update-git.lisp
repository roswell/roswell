(roswell:include "util")
(defpackage :roswell.update.git
  (:use :cl :roswell.util))
(in-package :roswell.update.git)

(defun git (path)
  (unless (pathnamep path)
    (setf path (make-pathname
                :type nil :name nil
                :defaults (asdf:system-source-file (asdf:find-system path)))))
  (when (probe-file (merge-pathnames ".git/" path))
    (format *error-output* "git pull on ~A~%" path)
    (unless (roswell.util:which "git")
      (format *error-output* "git command not found~%")
      (ros:quit 1))
    (multiple-value-bind (o e exit-code)
        (uiop/run-program:run-program
         `(,(sh) "-lc" ,(format nil "cd ~S;git pull" (uiop:native-namestring path)))
         :output t :error-output t :ignore-error-status t)
      (declare (ignore o e))
      (unless (zerop exit-code)
        (ros:quit exit-code)))
    t))
