(roswell:include "util-install")
(defpackage :roswell.list.asdf
  (:use :cl :roswell.install :roswell.util :roswell.locations)
  (:export :asdf-get-version))
(in-package :roswell.list.asdf)

(defun asdf-get-version ()
  (cddr (github-version
   (asdf-git-version-uri) "asdf" (lambda (href) (subseq href (1+ (position #\/ href :from-end t)))))))

(defun asdf (&rest r)
  (declare (ignore r))
  (dolist (i (asdf-get-version))
    (format t "~A~%" i)))
