(ros:include "util")
(defpackage :roswell.dump.sbcl
  (:use :cl :roswell.util))
(in-package :roswell.dump.sbcl)

(defun dump-executable (cmds out script)
  (declare (ignore script))
  (sb-ext:save-lisp-and-die
   out
   ;; no need to do GC because of :purify t by default
   ;; however, this only affects old cheyneyGC
   ;; http://www.sbcl.org/manual/#Efficiency-Hacks
   :purify t ; just here to make it explicit
   :toplevel
   #'(lambda ()
       (setf *load-pathname* (pathname (first sb-ext:*posix-argv*)))
       (setf ros:*argv* (rest sb-ext:*posix-argv*))
       (ros:run cmds))
   :executable t
   :save-runtime-options t))

(defun sbcl (type &rest args)
  (case type
    (:executable
     (apply 'dump-executable args))))
