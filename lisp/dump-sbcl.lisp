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

(in-package :ros.script.dump)

(defun delete-compiler-information-sbcl ()
  "This removes the entire compiler information about the functions.
This includes macro/compiler-macro definitions, inline expansions, 
IR1 (deftransform), IR2 (VOP) information in the infodb."
  ;; see src/compiler/globaldb.lisp
  #-sbcl
  (warn "delete-compiler is available only in SBCL")
  #+sbcl
  (do-all-symbols (s)
    (when (fboundp s)
      (setf (sb-int:info :function :inlinep s) :notinline)
      (sb-int:clear-info :function :inline-expansion-designator s)
      ;; Does this have the same effect as proclaiming notinline?
      ;; --- seems like so. src/compiler/proclaim.lisp
      ;; --- SB-C::PROCESS-INLINE-DECLARATION
      (sb-int:clear-info :function :source-transform s)
      (sb-int:clear-info :function :info s)
      (sb-int:clear-info :function :ir1-convert s)
      (sb-int:clear-info :function :predicate-truth-constraint s)
      (sb-int:clear-info :function :macro-function s)
      (sb-int:clear-info :function :compiler-macro-function s))
    (let ((s `(setf ,s)))
      (when (fboundp s)
        (setf (sb-int:info :function :inlinep s) :notinline)
        (sb-int:clear-info :function :inline-expansion-designator s)
        (sb-int:clear-info :function :source-transform s)
        (sb-int:clear-info :function :info s)
        (sb-int:clear-info :function :ir1-convert s)
        (sb-int:clear-info :function :predicate-truth-constraint s)
        (sb-int:clear-info :function :macro-function s)
        (sb-int:clear-info :function :compiler-macro-function s)))))
