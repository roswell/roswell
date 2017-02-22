(roswell:include "help-options")
(defpackage :roswell.help.run
  (:use :cl :roswell.util :roswell.help.options))
(in-package :roswell.help.run)

(defun run (argv)
  (declare (ignorable argv))
  (let ((s *error-output*))
    (format s "Usage: ~A [OPTIONS] run [OPTIONS] [-- implementation-native-options...]~%~%" (pathname-name (opt "wargv0")))
    (let ((*options* (nthcdr 4 *options*))) ;; fix me
      (options nil))))
