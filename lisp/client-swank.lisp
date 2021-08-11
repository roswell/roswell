(roswell:include "util")
(defpackage :roswell.client.swank
  (:use :cl :roswell.util))
(in-package :roswell.client.swank)
(ql:quickload "swank-protocol" :silent t)

(defun swank-wait (connection)
  (loop for msg = (swank-protocol:read-message connection)
        do (cond ((find (first msg) '(:indentation-update :return :presentation-end)) :ignore)
                 ((eql (first msg) :new-package)
                  (setf (swank-protocol:connection-package connection) (second msg)))
                 ((eql (first msg) :presentation-start)
                  (format *error-output* "=> ")
                  (force-output *error-output*))
                 ((eql (first msg) :debug-activate)
                  (roswell:quit 1))
                 ((eql (first msg) :write-string)
                  (format t "~A" (second msg))
                  (force-output))
                 (t (print msg *error-output*)))
        until (find (first msg) '(:return :debug-activate))))

(defun swank (&rest r)
  (let ((port 4005)
        (interface "localhost")
        (eval))
    (loop for (var val) on (cdr r) by #'cddr
          do (cond ((equal var "--port")
                    (setf port (parse-integer val)))
                   ((equal var "--interface")
                    (setf interface val))
                   ((or (equal var "--eval")
                        (equal var "-e"))
                    (push (cons :eval val) eval))))
    (let* ((connection (swank-protocol:make-connection
                        interface
                        port)))
      (swank-protocol:connect connection)
      (swank-protocol:request-swank-require 
       connection
       '(:swank-presentations :swank-repl))
      (swank-wait connection)
      (swank-protocol:request-init-presentations connection)
      (swank-wait connection)
      (swank-protocol:request-create-repl connection)
      (swank-wait connection)
      (loop for i in (nreverse eval)
            do (swank-protocol:request-listener-eval 
                connection 
                (case (first i)
                  (:eval (rest i))))
               (swank-wait connection)))))
