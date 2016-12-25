(ros:include "util-install-quicklisp")
(defpackage :roswell.install.msys2+
  (:use :cl :roswell.install :roswell.util :roswell.locations))
(in-package :roswell.install.msys2+)
#-win32
(progn
  (warn "msys2 is only required on windows"))

(defvar *msys2-basever* "20150916")
;;(defvar *msys2-sha1* "88fa66ac2a18715a542e0768b1af9b2f6e3680b2")
;;(ironclad:byte-array-to-hex-string (ironclad:digest-file :sha1 path))
(defvar *msys2-arch*)
(defvar *msys2-bits*)
(defun msys2-setup (argv)
  (let* ((*msys2-bits* (or (and (position "--32" (getf argv :argv) :test 'equal) 32)
                           (and (position "--64" (getf argv :argv) :test 'equal) 64)
                           #+x86-64 64
                           #-x86-64 32))
         (*msys2-arch* (if (= 32 *msys2-bits*)
                           "i686" "x86_64"))
         (path (merge-pathnames (format nil "archives/msys2-~A.tar.xz" *msys2-basever*) (homedir)))
         (msys (merge-pathnames (format nil "impls/~A/~A/msys~A/" (uname-m) (uname) *msys2-bits*) (homedir))))
    (if  (probe-file (merge-pathnames (format nil "mingw~A/bin/gcc.exe" *msys2-bits*) msys))
         (format t "msys2 have been setup~%")
         (progn
           (format *error-output* "Download ~a~%" (file-namestring path))
           (force-output *error-output*)
           (when (or (not (probe-file path))
                     (opt "download.force"))
             (download
              (format nil "~Amsys2/Base/~A/msys2-base-~A-~A.tar.xz"
                      (msys2-uri)
                      *msys2-arch* *msys2-arch* *msys2-basever*) path))
           (format t " done.~%")
           (expand path
                   (ensure-directories-exist
                    (merge-pathnames (format nil "impls/~A/~A/" (uname-m) (uname))
                                     (homedir))))
           (format t "extraction done.~%")
           (uiop/run-program:run-program
            `(,(sb-ext:native-namestring (merge-pathnames "usr/bin/bash" msys))
              "-lc" " ")
            :output t)
           (uiop/run-program:run-program
            `(,(sb-ext:native-namestring (merge-pathnames "usr/bin/bash" msys))
              "-lc"
              ,(format nil "~@{~A~}"
                       "for i in {1..3}; do pacman --noconfirm "
                       "-Suy autoconf automake pkg-config "
                       "mingw-w64-" *msys2-arch* "-gcc "
                       "make zlib-devel && break || sleep 15; done")))
           (uiop/run-program:run-program
            `(,(sb-ext:native-namestring (merge-pathnames "autorebase.bat" msys)))))))
  (cons t argv))

(defun msys2-help (argv)
  (format t "~%")
  (cons t argv))

(defun msys2+ (type)
  (case type
    (:help '(msys2-help))
    (:install `(msys2-setup))
    #+nil(:list 'msys2-get-version)))
