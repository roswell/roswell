(defpackage :roswell.init.system
  (:use :cl :roswell.util)
  (:shadow :system))
(in-package :roswell.init.system)

(defun system (name &rest params)
  (unless (first params)
    (format *error-output* "~&System name not specified")
    (roswell:quit 1))
  (setf name (first params)
        name (namestring (make-pathname :defaults name :type nil)))
  (map () (lambda (i)
            (setf name (remove i name)))
       "./\\")
  (let ((path (make-pathname :defaults name :type "asd")))
    (handler-case
        (unless
            (with-open-file (out path
                                 :direction :output
                                 :if-exists nil
                                 :if-does-not-exist :create)
              (when out
                (format out "(defsystem ~S)~%" name)
                (format t "~&Successfully generated: ~A~%" path)
                t))
          (format *error-output* "~&File already exists: ~A~%" path)
          (roswell:quit 1))
      (error (e)
        (format *error-output* "~&~A~%" e)
        (roswell:quit 1)))))
