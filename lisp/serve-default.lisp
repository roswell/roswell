(roswell:include "util")
(defpackage :roswell.serve.default
  (:use :cl :roswell.util))
(in-package :roswell.serve.default)

(defun default (&rest r)
  (if (null r)
      (progn
        (format *error-output* "Possible subcommands:~%")
        (finish-output *error-output*)
        (format t "~{~A~%~}"
                (sort (loop for x in (asdf:registered-systems)
                         when (ignore-errors (and (string-equal "roswell.serve." x :end2 14)
                                                  (not (string-equal "roswell.serve.default" x))))
                         collect (subseq x 14))
                      #'string<)))
      (format *error-output* "not suppported type for serve:~A~%" (first r))))
