(roswell:include "util")
(defpackage :roswell.dump.clisp
  (:use :cl :roswell.util))
(in-package :roswell.dump.clisp)

(defun dump-executable (cmds out script)
  (declare (ignore script))
  (ext:saveinitmem
   out
   :quiet t
   :executable 0
   :norc t
   :script nil
   :init-function
   #'(lambda ()
       (setf *load-pathname* (pathname (first ext:*args*)))
       (setf roswell:*argv* (rest ext:*args*))
       (roswell:run cmds))))

(defun clisp (type &rest args)
  (case type
    (:executable
     (apply 'dump-executable args))))
