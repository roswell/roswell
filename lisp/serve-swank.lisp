(roswell:include "util")
(defpackage :roswell.serve.swank
  (:use :cl :roswell.util))
(in-package :roswell.serve.swank)

(defun swank (&rest r)
  (ql:quickload :swank :silent t)
  (let ((port 4005))
    (loop for (var val) on (cdr r) by #'cddr
          do (cond ((equal var "--port")
                    (setf port (parse-integer val)))))
    (roswell.util:read-call "swank:create-server" :port port :dont-close t))
  (loop (sleep 1)))
