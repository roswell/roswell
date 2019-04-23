(defpackage :roswell.dist.versions
  (:use :cl))
(in-package :roswell.dist.versions)

(defun versions (&rest r)
  (if (rest r)
      (let ((versions (ql-dist:available-versions (ql-dist:find-dist (second r))))
            len1 len2)
        (loop for v in versions
              maximize (length (first v)) into name
              maximize (length (rest v)) into uri
              finally (setf len1 name
                            len2 uri))
        (dolist (v versions)
          (format t "~vA ~vA~%"
                  len1 (first v)
                  len2 (rest v))))
      (dolist (i (ql-dist:all-dists))
        (format t "~A~%" i))))
