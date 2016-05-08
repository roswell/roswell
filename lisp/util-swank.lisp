(cl:in-package :cl-user)
(when (cl:find-package :ros.swank.util)
  (pushnew :ros.swank.util *features*))

#-ros.util
(ros:include "util")

(defpackage :ros.swank.util
  (:use :cl :ros.util)
  (:export :swank :swank-write-version))

(in-package :ros.swank.util)
(defvar *version-file* (merge-pathnames "lisp/swank/version.txt" (homedir)))

(defun swank-write-version (name)
  (setf (config "asdf.version") name))

(defun swank-read-version ()
  (config "asdf.version"))

(defun swank (&key port style (version (swank-read-version)) delete load-contribs dont-close)
  (when delete
    (ignore-errors (delete-package (find-package :swank-loader))))
  (load (merge-pathnames (format nil "lisp/swank/~A/swank-loader.lisp" version) (homedir)))
  (funcall (read-from-string "swank-loader:init")
           :delete delete
           :load-contribs load-contribs)
  (funcall (read-from-string "swank:create-server")
           :port (or port (symbol-value (read-from-string "swank::default-server-port")))
           :style (or style (symbol-value (read-from-string "swank:*communication-style*")))
           :dont-close (or dont-close)))
