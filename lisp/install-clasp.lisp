(roswell:include "util-install-quicklisp")
(defpackage :roswell.install.clasp
  (:use :cl :roswell.install :roswell.util :roswell.locations))
(in-package :roswell.install.clasp)

(defparameter *clasp-version*
  ;; alias commit external-clasp-version
  '(("2017-06-13" "7a6472a72ff62b8ae4b010bd772f5f674b6af544" "3.9.1")
    ("2017-06-10" "71bf379fced077b49b479c41a28a124a5c01177d" "3.9.1")
    ("2017-06-03" "35f15d43e89c848d53b8cc4745120f3a07f60cb2" "3.9.1")
    ("2017-05-20" "699847a1af369670bbc4fcef7421e4bfb45b3208" "3.9.1")
    ("2017-05-04" "3771079ff22fdfbb5e2f8f8d4e13b66801da62d1" "3.9.1")
    ("2017-04-15" "c4ca73300a421ccef78cad4dd4f1b237d7b46982" "3.9.1")
    ("2017-03-20" "fa20fc6c7ef9b9b615cbb07070ec2de7130a1e03" "3.9.1")
    ("2017-03-12" "aa7edca94666024ecfbf82cf1f59e1f75074ec3d" "3.9.1")
    ("2017-02-25" "1802d376053d45925bd654e070c09c7b37d2eb20" "3.9.1")
    ("2017-02-16" "3f6ffecc31f8d0cc835fe0f87cb88402c95c9519" "3.9.1")))

(defun clasp-get-version ()
  (mapcar #'first *clasp-version*))

(defun clasp-argv-parse (argv)
  (let ((pos (position "--as" (getf argv :argv) :test 'equal)))
    (set-opt "as" (or (and pos (ignore-errors (nth (1+ pos) (getf argv :argv)))
                           (format nil "~A-~A"
                                   (getf argv :version)
                                   (nth (1+ pos) (getf argv :argv))))
                      (getf argv :version))))
  (set-opt "src" (merge-pathnames (format nil "src/clasp/~A/" (getf argv :version)) (homedir)))
  (cons t argv))

(defun clasp-lib (argv)
  (roswell:roswell '("install externals-clasp+") :interactive nil)
  (cons t argv))

(defun clasp-expand (argv)
  (let ((output (namestring (opt "src"))))
    (unless (probe-file output)
      (format t "git clone clasp~%")
      (uiop/run-program:run-program
       (list "git" "clone" "https://github.com/drmeister/clasp.git" output)
       :output t :ignore-error-status nil))
    (chdir (opt "src"))
    (format t "git fetch~%")
    (uiop/run-program:run-program
     (list "git" "fetch" "origin")
     :output t :ignore-error-status nil)
    (format t "git checkout ~A~%" (getf argv :version))
    (uiop/run-program:run-program
     (list "git" "checkout" (or (second (assoc (getf argv :version) *clasp-version* :test 'equal)) (getf argv :version)))
     :output t :ignore-error-status nil))
  (cons (not (opt "until-extract")) argv))

(defun clasp-make (argv)
  (let ((h (homedir))
        (v (getf argv :version))
        (sbcl-base (merge-pathnames (format nil "impls/~A/~A/sbcl-bin/~A/"
                                            (uname-m) (uname)
                                            (config "sbcl-bin.version"))
                                    (homedir))))
    (with-open-file (out (merge-pathnames "wscript.config" (opt "src"))
                         :direction :output :if-exists :supersede)
      (format out "EXTERNALS_CLASP_DIR = '~A'~%" (namestring (merge-pathnames (format nil "lib/~A/~A/externals-clasp/~A"
                                                                                      (uname-m) (uname)
                                                                                      (config "externals.clasp.version"))
                                                                              (homedir))))
      (format out "SBCL                = '~A'~%" (namestring (merge-pathnames "bin/sbcl" sbcl-base))))
    (setenv "SBCL_HOME" (namestring (merge-pathnames "lib/sbcl" sbcl-base)))
    (format t "with sbcl-bin(~A) build clasp" sbcl-base))
  (with-open-file (out (ensure-directories-exist
                        (merge-pathnames (format nil "log/clasp/~A/make.log"
                                                 (getf argv :version))
                                         (homedir)))
                       :direction :output :if-exists :append :if-does-not-exist :create)
    (format out "~&--~&~A~%" (date))
    (let* ((src (opt "src"))
           (cmd "./waf configure update_submodules build_cboehm")
           (*standard-output* (make-broadcast-stream out #+sbcl(make-instance 'count-line-stream))))
      (chdir src)
      (format t "~&~S~%" cmd)
      (uiop/run-program:run-program
       (list (sh) "-lc" (format nil "cd ~S;~A"
                                (#+win32 mingw-namestring #-win32 princ-to-string src)
                                cmd))
       :output t :ignore-error-status nil)
      (format *error-output* "done.~%")))
  (cons t argv))

(defun clasp-install (argv)
  (unsetenv "SBCL_HOME")
  (let ((src (opt "src"))
        (impl-path (merge-pathnames
                    (format nil "impls/~A/~A/~A/" (uname-m) (uname) (getf argv :target)) (homedir))))
    (ensure-directories-exist impl-path)
    (uiop/run-program:run-program
     (list (sh) "-lc" (format nil "cd ~S;cp -r build ~S"
                              (#+win32 mingw-namestring #-win32 princ-to-string src)
                              (#+win32 mingw-namestring #-win32 princ-to-string impl-path)))
     :output t :ignore-error-status nil)
    (ql-impl-util:rename-directory
     (merge-pathnames "build" impl-path)
     (merge-pathnames (opt "as") impl-path)))
  (cons t argv))

(defun clasp-clean (argv)
  (format t "~&Cleaning~%")
  (let ((src (opt "src")))
    (chdir src)
    (let ((*standard-output* *standard-output*))
      (uiop/run-program:run-program
       (list (sh) "-lc" (format nil "cd ~S;rm -rf build" src)) :output t))
    (format t "done.~%"))
  (cons t argv))

(defun clasp-help (argv)
  (format t "~%")
  (cons t argv))

(defun clasp (type)
  (case type
    (:help '(clasp-help))
    (:install `(,(decide-version 'clasp-get-version)
                clasp-argv-parse
                start
                clasp-lib
                clasp-expand
                clasp-make
                clasp-install
                clasp-clean
                setup))))
