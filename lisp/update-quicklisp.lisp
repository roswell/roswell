(roswell:include "util")
(defpackage :roswell.update.quicklisp
  (:use :cl :roswell.util))
(in-package :roswell.update.quicklisp)

(defun quicklisp (&rest r)
  (ql:update-client :prompt nil)
  (cond
    ((or (second r)
         (and (first r)
              (zerop (count-if-not
                      (lambda (x) (or (digit-char-p x)
                                      (char= x #\-)))
                      (first r)))
              (setf r (list "quicklisp" (first r)))))
     (ql-dist:install-dist
      (cdr (assoc (second r) (ql-dist:available-versions
                              (ql-dist:dist (first r)))
                  :test 'equal))
      :replace t))
    ((first r)
     (ql:update-dist (first r)))
    ((not r)
     (ql:update-all-dists)))
  t)
