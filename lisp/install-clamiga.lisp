(roswell:include "util-install-quicklisp")
(defpackage :roswell.install.clamiga
  (:use :cl :roswell.install :roswell.util :roswell.locations))
(in-package :roswell.install.clamiga)

(defun clamiga-get-version ()
  (format *error-output* "Checking version to install....~%")
  (github-version (clamiga-git-version-uri) "clamiga"
                  (lambda (href)
                    (let ((a (subseq href (1+ (position #\/ href :from-end t)))))
                      (and (plusp (length a))
                           (char= (aref a 0) #\v)
                           (subseq a 1))))))

(defun clamiga-src (argv)
  (merge-pathnames (format nil "src/cl-amiga-~A/" (getf argv :version)) (homedir)))

(defun clamiga-make ()
  (or #+win32 (and (which "mingw32-make") "mingw32-make") (make)))

(defun clamiga-run (argv name cmd)
  (with-open-file (out (ensure-directories-exist
                        (merge-pathnames (format nil "impls/log/~A-~A/~A.log"
                                                 (getf argv :target) (opt "as") name)
                                         (homedir)))
                       :direction :output :if-exists :append :if-does-not-exist :create)
    (format out "~&--~&~A~%" (date))
    (let ((*standard-output* (make-broadcast-stream out #+sbcl(make-instance 'count-line-stream))))
      (uiop/run-program:run-program
       (list (sh) "-lc" (format nil "cd ~S;~A ~A" (namestring (clamiga-src argv)) (clamiga-make) cmd))
       :output t))))

(defun clamiga-argv-parse (argv)
  (let ((pos (position "--as" (getf argv :argv) :test 'equal)))
    (set-opt "as" (or (and pos (ignore-errors (nth (1+ pos) (getf argv :argv)))
                           (format nil "~A-~A"
                                   (getf argv :version)
                                   (nth (1+ pos) (getf argv :argv))))
                      (getf argv :version))))
  (set-opt "prefix" (merge-pathnames (format nil "impls/~A/~A/~A/~A/" (uname-m) (uname) (getf argv :target) (opt "as")) (homedir)))
  (cons t argv))

(defun clamiga-download (argv)
  (set-opt "download.uri" (format nil "~Av~A.tar.gz" (clamiga-uri) (getf argv :version)))
  (set-opt "download.archive" (merge-pathnames (format nil "archives/cl-amiga-~A.tar.gz" (getf argv :version)) (homedir)))
  `((,(opt "download.archive") ,(opt "download.uri"))))

(defun clamiga-expand (argv)
  (format t "~%Extracting archive:~A~%" (opt "download.archive"))
  (expand (opt "download.archive")
          (merge-pathnames "src/" (homedir)))
  (cons t argv))

(defun clamiga-build (argv)
  (format t "~&make~%")
  (clamiga-run argv "make" "host CC_HOST=cc")
  (cons t argv))

(defun clamiga-install (argv)
  (format t "~&Installing ~A/~A..." (getf argv :target) (opt "as"))
  (format t "~&prefix: ~s~%" (opt "prefix"))
  (ensure-directories-exist (opt "prefix"))
  (clamiga-run argv "install" (format nil "install CC_HOST=cc 'PREFIX=~A'"
                                      (string-right-trim "/" (namestring (opt "prefix")))))
  (format *error-output* "done.~%")
  (cons t argv))

(defun clamiga-clean (argv)
  (format t "~&Cleaning~%")
  (clamiga-run argv "clean" "clean")
  (format t "done.~%")
  (cons t argv))

(defun clamiga (type)
  (case type
    (:help '())
    (:install `(,(decide-version 'clamiga-get-version)
                clamiga-argv-parse
                start
                ,(decide-download 'clamiga-download)
                clamiga-expand
                clamiga-build
                clamiga-install
                clamiga-clean
                setup))
    (:list 'clamiga-get-version)))
