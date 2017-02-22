(defpackage :roswell.init.default
  (:use :cl :roswell.util))
(in-package :roswell.init.default)

(defun default (name &rest params)
  (declare (ignore name))
  (let* ((date (get-universal-time))
         (name (first params))
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
                            "#| <Put a one-line description here>"
                            "exec ros -Q -- $0 \"$@\"" "|#"
                            "(progn ;;init forms"
                            "  (ros:ensure-asdf)"
                            "  ;;#+quicklisp (ql:quickload '() :silent t)"
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
