(roswell:include "util")
(defpackage :roswell.update.default
  (:use :cl :roswell.util))
(in-package :roswell.update.default)

(defun default (param &rest args)
  (declare (ignore args))
  (let* ((subpath (uiop:subpathp param (first ql:*local-project-directories*)))
         (target (when subpath
                   (remove-if-not #'stringp (pathname-directory subpath)))))
    (format *error-output* "Target project would be ~A~%" target)
    (clone-github (first target) (second target)
                  :git nil
                  :path "local-projects"
                  :force-update t)))
