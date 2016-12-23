(ros:include "util-install-quicklisp")
(defpackage :roswell.run.msys
  (:use :cl :ros.util))
(in-package :roswell.run.msys)

(defvar *msys2-arch*)
(defvar *msys2-bits*)
(defun msys (system version program restart script verbose)
  (declare (ignore system version program restart script verbose))
  (let* ((*msys2-bits* (or ;;(and (position "--32" (getf argv :argv) :test 'equal) 32)
                        ;;(and (position "--64" (getf argv :argv) :test 'equal) 64)
                        #+x86-64 64
                        #-x86-64 32))
         (*msys2-arch* (if (= 32 *msys2-bits*)
                           "i686" "x86_64"))
         (msys (merge-pathnames (format nil "impls/~A/~A/msys~A/" (uname-m) (uname) *msys2-bits*) (homedir))))
    (config-env)
    (uiop/run-program:run-program
     `(,(sb-ext:native-namestring (merge-pathnames "msys2_shell.cmd" msys))))))
