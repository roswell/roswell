(roswell:include "util")
(defpackage :roswell.dist.default
  (:use :cl :roswell.util))
(in-package :roswell.dist.default)

(defun default (&rest r)
  (if (null r)
      (progn
        (format *error-output* "Possible subcommands:~%")
        (finish-output *error-output*)
        (format t "~{~A~%~}"
                (sort (loop for x in (asdf:registered-systems)
                         when (ignore-errors (and (string-equal "roswell.dist." x :end2 13)
                                                  (not (string-equal "roswell.dist.default" x))))
                         collect (subseq x 13))
                      #'string<)))
      (format *error-output* "not suppported type for dist:~A~%" (first r))))

(defun options (stream command-name)
  (format stream "~{   ~A~%~}~%"
          (sort (loop for x in (directory (merge-pathnames (format nil "~A-*.lisp" command-name) (opt "lispdir")))
                   for name = (subseq (pathname-name x) (1+ (length command-name)))
                   collect name)
                #'string<)))
