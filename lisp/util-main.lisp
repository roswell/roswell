(roswell:include "util" "util-main")
(defpackage :roswell.util.main
  (:use :cl :roswell.util)
  (:export :module-main))
(in-package :roswell.util.main)

(defun module-main (args &key default usage
                           (mod-name #'identity)
                           (error-output *error-output*))
  (let* ((name (first args))
         (packagename (package-name *package*))
         (pos (loop repeat 3
                 with pos = 0
                 for p = (position #\. packagename :start pos)
                 while p
                 collect p
                 do (setf pos (1+ p))))
         (module-name (string-downcase
                       (subseq packagename (1+ (second pos)) (third pos)))))
    (cond
      ((and name
            (let ((*error-output* error-output))
              (module module-name (funcall mod-name name))))
       (apply (module module-name (funcall mod-name name)) args))
      ((and name
            default
            (module module-name default))
       (apply (module module-name default) args))
      ((and usage
            (module module-name usage))
       (apply (module module-name usage) args)))))
