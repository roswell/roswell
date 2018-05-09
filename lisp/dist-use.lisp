(defpackage :roswell.dist.use
  (:use :cl))
(in-package :roswell.dist.use)

(defun use (&rest r)
  (setf r (cdr r))
  (let* ((name (first r))
         (version (second r))
         (alist (ql-dist:available-versions (ql-dist:dist name)))
         (found (cdr (assoc version alist :test 'equal))))
    (if found 
        (ql-dist:install-dist found :replace t)
        (format t "~A not found.~%" version))))
