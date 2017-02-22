(roswell:include "util")
(defpackage :roswell.util.swank
  (:use :cl :roswell.util)
  (:export :swank))
(in-package :roswell.util.swank)

(defun swank (&key port style (version (config "slime.version")) delete load-contribs dont-close)
  (when delete
    (ignore-errors (delete-package (find-package :swank-loader))))
  (let ((path (merge-pathnames (format nil "lisp/swank/~A/swank-loader.lisp" version) (homedir))))
    (when (probe-file path)
      (load path)))
  (read-call "swank-loader:init"
             :delete delete
             :load-contribs load-contribs)
  (when port
    (apply (read-from-string "swank:create-server")
           `(:port
             ,port
             :style ,(or style (symbol-value (read-from-string "swank:*communication-style*")))
             :dont-close ,(or dont-close (read-from-string "swank:*dont-close*"))
             ,@(when (boundp (read-from-string "swank::*coding-system*"))
                 (list :coding))))))
