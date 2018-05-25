(roswell:include "util-install-quicklisp")
(defpackage :roswell.install.clasp
  (:use :cl :roswell.install :roswell.util :roswell.locations))
(in-package :roswell.install.clasp)

(defparameter *clasp-version*
  ;; alias commit external-clasp-version
  '(("2018-05-17" "b561e8fa7300ae774e7d75eb6d6514926657c557" "5.0-20171109")
    ("2018-04-01" "d0de68494af19e8b52fb56d83399b0c27b22a528" "5.0")
    ("2018-03-11" "ee3e6c7f94e1e9a692541caa9f71ba73cfdbc292" "5.0")
    ("2018-02-05" "948467383606819bedafd2998c2139e190bd3391" "5.0")
    ("2017-12-20" "46634ed2f4dd927059e9d3ab00a00aee1f8991e3" "5.0")
    ("2017-12-12" "141be6a01d806efc64725e516dce8a58d3d1f732" "5.0")
    ("2017-11-20" "97f9f147a35d9b18d9581bcd7c816aa45aecd894" "5.0")
    ("2017-11-05" "fb57b58cb27527469a0798ae5645e9e87bad7de4" "5.0")
    ("2017-10-27" "c4c0e740dba84dc3cdf4fd3dfa88404cb05f1932" "5.0")
    ("2017-10-12" "d9730c986661441db19064ccbd6961def8a7f793" "5.0")
    ("2017-09-17" "04a94c3824892c7c5f6eec142f45868d5496571b" "5.0")
    ("2017-08-24" "96f3376fe238510ba1f97553601f500f4fe41196" "3.9.1")
    ("2017-08-15" "c0af3c5839ab5d5ef918f9732abbf6b6e7ef4f85" "3.9.1")
    ("2017-06-13" "7a6472a72ff62b8ae4b010bd772f5f674b6af544" "3.9.1")
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

(defun clasp-get-externals-clasp-version (argv)
  (or (third (assoc (getf argv :version) *clasp-version* :test 'equal))
      (config "externals.clasp.version")))

(defun clasp-lib (argv)
  (roswell:roswell (list (format nil "install externals-clasp+/~a"
                                 (clasp-get-externals-clasp-version argv)))
                   :interactive nil)
  (cons t argv))

(defun clasp-expand (argv)
  (let ((output (namestring (opt "src"))))
    (unless (probe-file output)
      (format t "git clone clasp~%")
      (uiop/run-program:run-program
       (list "git" "clone" "https://github.com/clasp-developers/clasp.git" output)
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
                                                                                      (clasp-get-externals-clasp-version argv))
                                                                              (homedir))))

      (let* ((path (format nil "lib/~A/~A/externals-clasp/~A/build/release/bin/llvm-config"
                          (uname-m) (uname)
                          (clasp-get-externals-clasp-version argv))))
        (format out "LLVM_CONFIG_BINARY  = '~A'~%" (namestring (merge-pathnames path (homedir)))))
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
           (cmd "./waf update_dependencies configure build_cboehm")
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
