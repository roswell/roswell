(defpackage :roswell.init.dist
  (:use :cl :roswell.util))
(in-package :roswell.init.dist)

(defun dist (name &rest params)
  (setf name (first params))
  (let ((path (make-pathname :defaults "distinfo" :type "txt")))
    (handler-case
        (unless
            (prog1
                (with-open-file (out path
                                     :direction :output
                                     :if-exists nil
                                     :if-does-not-exist :create)
                  (when out
                    (format out "name: ~A~%" name)
                    (format out "preference: ~A~%" (get-universal-time))
                    (format t "~&Successfully generated: ~A~%" path)
                    t)))
          (format *error-output* "~&File already exists: ~A~%" path)
          (roswell:quit 1))
      (error (e)
        (format *error-output* "~&~A~%" e)
        (roswell:quit 1)))))
