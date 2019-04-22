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
             len1 len2)
        (loop for dist in dists
              maximize (length (ql-dist:name dist)) into name
              maximize (length (ql-dist:version dist)) into version
              finally (setf len1 name
                            len2 version))
        (dolist (dist dists)
          (format t "~A~vA ~vA ~A~%"
                  (if (ql-dist:enabledp dist) " " "#")
                  len1
                  (ql-dist:name dist)
                  len2
                  (ql-dist:version dist)
                  (or (ql-dist::distinfo-subscription-url dist) ""))))))
