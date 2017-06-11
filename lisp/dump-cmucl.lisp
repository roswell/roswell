(roswell:include "util-dump")
(defpackage :roswell.dump.cmucl
  (:use :cl :roswell.util :roswell.util.dump))
(in-package :roswell.dump.cmucl)

;;; cmucl

;; cf. https://common-lisp.net/project/cmucl/doc/cmu-user/extensions.html#toc47

;; "This feature is only available on some platforms, as indicated by having the feature
;; :executable. Currently only x86 ports and the solaris/sparc port have this feature."
(defun dump-executable (cmds out)
  (setf ext:*batch-mode* nil)
  (setf ext::*gc-run-time* 0)
  (preprocess-before-dump)
  (ext:gc :full t)
  (ext:save-lisp
   out
   :purify *purify*
   :executable t
   :print-herald nil ; suppress verbose startup message
   :init-function
   #'(lambda ()
       (setf *load-pathname* (pathname (first extensions:*command-line-strings*)))
       (setf roswell:*argv* (rest extensions:*command-line-strings*))
       (roswell:run cmds))
   :process-command-line nil))

(defun cmucl (type &rest args)
  (case type
    (:query (first args))
    (:executable
     (apply 'dump-executable args))))
