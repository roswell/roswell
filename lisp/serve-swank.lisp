(roswell:include "util")
(defpackage :roswell.serve.swank
  (:use :cl :roswell.util))
(in-package :roswell.serve.swank)

(defun swank (&rest r)
  (print r))


