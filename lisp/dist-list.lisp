(defpackage :roswell.dist.list
  (:use :cl)
  (:shadow :list))
(in-package :roswell.dist.list)

(defun list (&rest r)
  (if (second r)
      (dolist (system (ql:provided-systems
                       (ql-dist:find-dist (second r))))
        (format t "~A~%" (ql-dist:short-description system)))
      (let* ((dists (sort (ql-dist:all-dists) #'> :key #'ql-dist:preference))
             (len (loop for dist in dists
                        maximize (length (ql-dist:name dist)))))
        (dolist (dist dists)
          (format t "~A~vA ~11A~A~%"
                  (if (ql-dist:enabledp dist) " " "#")
                  len
                  (ql-dist:name dist)
                  (subseq (ql-dist:version dist)
                          0 (min (length (ql-dist:version dist)) 10))
                  (or (ql-dist::distinfo-subscription-url dist) ""))))))
