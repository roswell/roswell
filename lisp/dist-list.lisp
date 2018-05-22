(defpackage :roswell.dist.list
  (:use :cl)
  (:shadow :list))
(in-package :roswell.dist.list)

(defun list (&rest r)
  (if (second r)
      (dolist (system (ql:provided-systems
                       (ql-dist:find-dist (second r))))
        (format t "~A~%" (ql-dist:short-description system)))
      (dolist (dist (sort (ql-dist:all-dists) #'> :key #'ql-dist:preference))
        (format t "~A~15A~11A~A~%"
                (if (ql-dist:enabledp dist) " " "#")
                (ql-dist:name dist)
                (subseq (ql-dist:version dist)
                        0 (min (length (ql-dist:version dist)) 10))
                (ql-dist::distinfo-subscription-url dist)))))
