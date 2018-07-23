(roswell:include "util-install-quicklisp")
(defpackage :roswell.install.msys2+
  (:use :cl :roswell.install :roswell.util :roswell.locations))
(in-package :roswell.install.msys2+)
#-win32
(progn
  (warn "msys2 is only required on windows"))

(defvar *msys2-arch*)
(defvar *msys2-bits*)

(defun msys2-get-version ()
  '("20180531"))
;;sha1 "309f604a165179d50fbe4131cf87bd160769f974"
;;(ironclad:byte-array-to-hex-string (ironclad:digest-file :sha1 path))

(defun msys2-setup (argv)
  (let* ((uname-m (roswell.util:uname-m))
         (*msys2-bits* (or (and (ros:opt "32") 32)
                           (and (ros:opt "64") 64)
                           (cond
                             ((equal uname-m "x86-64") 64)
                             ((equal uname-m "x86") 32))))
         (*msys2-arch* (if (= 32 *msys2-bits*)
                           "i686" "x86_64"))
         (path (merge-pathnames (format nil "archives/msys2-~A.tar.xz" (getf argv :version)) (homedir)))
         (msys (merge-pathnames (format nil "impls/~A/~A/msys2/~A/" (uname-m) (uname) (getf argv :version)) (homedir))))
    (if (probe-file (merge-pathnames (format nil "mingw~A/bin/gcc.exe" *msys2-bits*) msys))
        (format t "msys2 have been setup~%")
        (progn
          (format *error-output* "Download ~a~%" (file-namestring path))
          (force-output *error-output*)
          (when (or (not (probe-file path))
                    (opt "download.force"))
            (download
             (format nil "~Amsys2/Base/~A/msys2-base-~A-~A.tar.xz"
                     (msys2-uri)
                     *msys2-arch* *msys2-arch* (getf argv :version))
             path))
          (format t " done.~%")
          (expand path
                  (ensure-directories-exist
                   (merge-pathnames (format nil "impls/~A/~A/msys2/" (uname-m) (uname))
                                    (homedir))))
          (format t "extraction done.~%")
          (ql-impl-util:rename-directory
           (merge-pathnames (format nil "impls/~A/~A/msys2/msys~A"
                                    (uname-m) (uname) *msys2-bits*)
                            (homedir))
           (merge-pathnames (format nil "impls/~A/~A/msys2/~A"
                                    (uname-m) (uname) (getf argv :version))
                            (homedir)))
          (uiop/run-program:run-program
           `(,(uiop:native-namestring (merge-pathnames "usr/bin/bash" msys))
             "-lc" " ")
           :output t)
          (uiop/run-program:run-program
           `(,(uiop:native-namestring (merge-pathnames "usr/bin/bash" msys))
             "-lc"
             ,(format nil "~@{~A~}"
                      "for i in {1..3}; do pacman --noconfirm "
                      "-Suy autoconf automake pkg-config "
                      "mingw-w64-" *msys2-arch* "-gcc "
                      "make zlib-devel && break || sleep 15; done")))
          (uiop/run-program:run-program
           `(,(uiop:native-namestring (merge-pathnames "autorebase.bat" msys))))
          (setf (config "msys2.version") (getf argv :version)))))
  (cons t argv))

(defun msys2-help (argv)
  (format t "~%")
  (cons t argv))

(defun msys2+ (type)
  (case type
    (:help '(msys2-help))
    (:install `(,(decide-version 'msys2-get-version)
                msys2-setup))
    (:list 'msys2-get-version)))
