(ros:include "util")
(defpackage :roswell.util.swank
  (:use :cl :roswell.util)
  (:export :swank))
(in-package :roswell.util.swank)

(defun swank (&key port style (version (config "slime.version")) delete load-contribs dont-close)
  (when delete
    (ignore-errors (delete-package (find-package :swank-loader))))
  (load (merge-pathnames (format nil "lisp/slime/~A/swank-loader.lisp" version) (homedir)))
  (read-call "swank-loader:init"
             :delete delete
             :load-contribs load-contribs)
  (when port
    (if (boundp (read-from-string "swank::*coding-system*"))
        (read-call "swank:create-server"
                   :port port
                   :style (or style (symbol-value (read-from-string "swank:*communication-style*")))
                   :dont-close (or dont-close)
                   :coding)
        (read-call "swank:create-server"
                   :port port
                   :style (or style (symbol-value (read-from-string "swank:*communication-style*")))
                   :dont-close (or dont-close)))))
