(roswell:include "util")
(defpackage :roswell.update.hg
  (:use :cl :roswell.util))
(in-package :roswell.update.hg)

(defun hg (&rest args)
  (if (car args)
      (let ((path (car args)))
	(unless (pathnamep path)
	  (setf path (make-pathname
		      :type nil :name nil
		      :defaults (asdf:system-source-file (asdf:find-system path)))))
	(when (probe-file (merge-pathnames ".hg/" path))
	  (format *error-output* "hg pull and update on ~A~%" path)
	  (unless (roswell.util:which "hg")
	    (format *error-output* "hg command not found~%")
	    (ros:quit 1))
	  (multiple-value-bind (o e exit-code)
	      (uiop/run-program:run-program
	       `(,(sh) "-lc" ,(format nil "cd ~S;hg pull && hg update" (uiop:native-namestring path)))
	       :output t :error-output t :ignore-error-status t)
	    (declare (ignore o e))
	    (unless (zerop exit-code)
	      (ros:quit exit-code)))
	  t))
      (format *error-output* "No path supplied")))
