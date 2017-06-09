(roswell:include "util-dump")
(defpackage :roswell.dump.ccl
  (:use :cl :roswell.util :roswell.util.dump))
(in-package :roswell.dump.ccl)

(defun dump-executable (cmds out script)
  (declare (ignore script))
  (map nil #'funcall (nreverse *queue*))
  (ccl:gc)
  (ccl:save-application
   out
   :impurify *impurify*
   :purify   *purify*
   :toplevel-function
   #'(lambda ()
       (setf *load-pathname* (pathname (first (ccl::command-line-arguments))))
       (setf roswell:*argv* (rest (ccl::command-line-arguments)))
       (roswell:run cmds))
   :prepend-kernel t))

(defun ccl (type &rest args)
  (case type
    (:query (first args))
    (:executable
     (apply 'dump-executable args))
    (:output
     (ccl:save-application (first args)))))
