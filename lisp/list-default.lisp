(defpackage :roswell.list.default
  (:use :cl))
(in-package :roswell.list.default)

(defun default (&rest r)
  (if (null r)
      (progn
        (format *error-output* "Possible subcommands:~%")
        (finish-output *error-output*)
        (format t "~{~A~%~}"
                (sort (loop for x in (asdf:registered-systems)
                         when (ignore-errors (and (string-equal "roswell.list." x :end2 13)
                                                  (not (string-equal "roswell.list.default" x))))
                         collect (subseq x 13))
                      #'string<)))
      (format *error-output* "not suppported type for list:~A~%" (first r))))
