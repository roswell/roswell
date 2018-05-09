(defpackage :roswell.dist.versions
  (:use :cl))
(in-package :roswell.dist.versions)

(defun versions (&rest r)
  (if (rest r)
      (dolist (v (ql-dist:available-versions (ql-dist:find-dist (second r))))
        (format t "~A~%" (first v)))
      (dolist (i (ql-dist:all-dists))
        (format t "~A~%" i))))
