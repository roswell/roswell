(defpackage :roswell.init.default
  (:use :cl :roswell.util))
(in-package :roswell.init.default)

(defun default (name &rest params)
  (setf params (loop for (i j) on params by #'cddr
                     collect (intern i :keyword)
                     collect j))
  (setf name (namestring (make-pathname :defaults name :type nil)))
  (map () (lambda (i)
            (setf name (remove i name)))
       "./\\")
  (let* ((date (get-universal-time))
         (path (make-pathname :defaults name :type "ros")))
    (handler-case
        (unless
            (prog1
                (with-open-file (out path
                                     :direction :output
                                     :if-exists nil
                                     :if-does-not-exist :create)
                  (when out
                    (format out "~@{~A~%~}"
                            "#!/bin/sh"
                            "#|-*- mode:lisp -*-|#"
                            "#|"
                            "exec ros -Q -- $0 \"$@\"" "|#"
                            "(progn ;;init forms"
                            "  (ros:ensure-asdf)"
                            (let ((lib (getf params :|lib|)))
                              (format nil "  ~A#+quicklisp(ql:quickload '(~A) :silent t)"
                                      (if lib "" ";;")
                                      (or lib "")))
                            "  )"
                            ""
                            (format nil "(defpackage :ros.script.~A.~A" name date)
                            "  (:use :cl))"
                            (format nil "(in-package :ros.script.~A.~A)" name date)
                            ""
                            "(defun main (&rest argv)"
                            "  (declare (ignorable argv)))"
                            ";;; vim: set ft=lisp lisp:")
                    (format t "~&Successfully generated: ~A~%" path)
                    t))
              #+sbcl (sb-posix:chmod path #o700))
          (format *error-output* "~&File already exists: ~A~%" path)
          (roswell:quit 1))
      (error (e)
        (format *error-output* "~&~A~%" e)
        (roswell:quit 1)))))
