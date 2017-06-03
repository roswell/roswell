(roswell:include "util")
(defpackage :roswell.dump.sbcl
  (:use :cl :roswell.util))
(in-package :roswell.dump.sbcl)

(defun dump-executable (cmds out script)
  (declare (ignore script))
  (map nil #'funcall (nreverse ros.script.dump:*queue*))
  (sb-ext:gc :full t)
  (sb-ext:save-lisp-and-die
   out
   :purify   ros.script.dump:*purify*
   ; we all want our programs to be small, right?
   #+sb-core-compression :compression
   #+sb-core-compression ros.script.dump:*compression*
   :toplevel
   #'(lambda ()
       (setf *load-pathname* (pathname (first sb-ext:*posix-argv*)))
       (setf roswell:*argv* (rest sb-ext:*posix-argv*))
       (roswell:run cmds))
   :executable t
   :save-runtime-options t))

(defun sbcl (type &rest args)
  (case type
    (:query (first args))
    (:executable
     (apply 'dump-executable args))
    (:output
     (sb-ext:save-lisp-and-die (first args)))))
