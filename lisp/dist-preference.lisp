(defpackage :roswell.dist.preference
  (:use :cl))
(in-package :roswell.dist.preference)

(defun preference (&rest r)
  (let ((dist (ql-dist:find-dist (second r))))
    (if dist
        (progn
          (when (third r)
            (let ((score (if (equal (third r) "touch")
                             (get-universal-time)
                             (parse-integer (third r)))))
              (setf (ql-dist:preference dist) score)))
          (format t "~15A~A~%"
                  (ql-dist:name dist)
                  (ql-dist:preference dist)))
        (dolist (dist (ql-dist:all-dists))
          (when (ql-dist:enabledp dist)
            (format t "~15A~A~%"
                    (ql-dist:name dist)
                    (ql-dist:preference dist)))))))
