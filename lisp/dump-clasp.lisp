(roswell:include "util-dump")
(defpackage :roswell.dump.clasp
  (:use :cl :roswell.util :roswell.util.dump))
(in-package :roswell.dump.clasp)

(defun clasp (type &rest args)
  (case type
    (:query (remove "EXECUTABLE" (first args) :test 'string=))
    (:output
     (ext:save-lisp-and-die (namestring (first args))))))
