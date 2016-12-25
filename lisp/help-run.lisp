(ros:include "util")
(defpackage :roswell.help.run
  (:use :cl :roswell.util :ros.script.help.3668211011))
(in-package :roswell.help.run)

(defun run (argv)
  (declare (ignorable argv))
  (let ((s *error-output*))
    (format s "Usage: ~A [OPTIONS] run [OPTIONS] [-- implementation-native-options...]~%~%" (opt "wargv0"))
    (option s (nthcdr 4 *options*)))) ;; fix me
