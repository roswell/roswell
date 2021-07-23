(roswell:include "util-install-quicklisp")
(defpackage :roswell.install.sbcl-head
  (:use :cl :roswell.install :roswell.util :roswell.locations))
(in-package :roswell.install.sbcl-head)

(defun sbcl-head-variant ()
  (or (roswell:opt "variant")
      (roswell:roswell `("roswell-internal-use" "version" "sbcl-bin-variant") :string t)))

(defun sbcl-head-get-version ()
  (format *error-output* "Checking version to install....~%")
  (let ((elts
          (let ((file (merge-pathnames "tmp/sbcl_head.tsv" (homedir))))
            (download (sbcl-head-version-uri) file :interval 3600)
            (mapcar (lambda (x) (uiop:split-string x :separator '(#\tab)))
                    (uiop:read-file-lines file))))
        (uname (uname))
        (uname-m (uname-m))
        (variant (sbcl-head-variant)))
    (mapcar 'third (remove-if-not (lambda (x)
                                    (and (equal (first x) uname)
                                         (equal (second x) uname-m)
                                         (equal (fourth x) variant)
                                         ))
                                  elts))))

(defun sbcl-head-argv-parse (argv)
  (let ((pos (position "--as" (getf argv :argv) :test 'equal))
        (uname (uname))
        (uname-m (uname-m)))
    (set-opt "as" (or (and pos (ignore-errors (nth (1+ pos) (getf argv :argv)))
                           (format nil "~A-~A"
                                   (getf argv :version)
                                   (nth (1+ pos) (getf argv :argv))))
                      (getf argv :version)))
    (set-opt "prefix" (merge-pathnames (format nil "impls/~A/~A/~A/~A/" (uname-m) (uname) (getf argv :target) (opt "as")) (homedir)))
    (set-opt "src" (merge-pathnames (format nil "src/sbcl-~A-~A-~A~A/"
                                            (getf argv :version)
                                            uname-m
                                            uname
                                            (let ((var (sbcl-head-variant)))
                                              (if (zerop (length var))
                                                  ""
                                                  (format nil "-~A" var))))
                                    (homedir))))
  (cons t argv))

(defun sbcl-head-download (argv)
  (set-opt "download.uri" (format nil "~@{~A~}" (sbcl-head-uri) (getf argv :version)
                                  "/sbcl-"
                                  (getf argv :version) "-"
                                  (uname-m) "-"
                                  (uname)
                                  (let ((var (sbcl-head-variant)))
                                    (if (zerop (length var))
                                        ""
                                        (format nil "-~A" var)))
                                  "-binary.tar.bz2"))
  (set-opt "download.archive" (let ((pos (position #\/ (opt "download.uri") :from-end t)))
                                (when pos
                                  (merge-pathnames (format nil "archives/~A" (subseq (opt "download.uri") (1+ pos))) (homedir)))))
  `((,(opt "download.archive") ,(opt "download.uri"))))

(defun sbcl-head-expand (argv)
  (let* ((h (homedir))
         (v (getf argv :version)))
    (cond
      (t
       (format t "~%Extracting archive:~A~%" (opt "download.archive"))
       (expand (opt "download.archive")
               (ensure-directories-exist (merge-pathnames "src/" h)))
       )))
  (cons t argv))

(defun sbcl-head-install (argv)
  (let* ((impl-path (opt "prefix"))
         (src (opt "src"))
         (install-root impl-path)
         (log-path (merge-pathnames (format nil "impls/log/~A-~A/install.log" (getf argv :target) (opt "as")) (homedir))))
    #-delete-later(format t "~S~%" (list :sbcl-head-install :debug impl-path src log-path))
    (unless (opt "archive")
      (format t "~&Installing ~A/~A" (getf argv :target) (opt "as"))
      (format t "~&prefix: ~s~%" impl-path)
      (ensure-directories-exist impl-path)
      (ensure-directories-exist log-path)
      (chdir src)
      (unsetenv "SBCL_HOME")
      (setenv "INSTALL_ROOT" (format nil "~A" install-root))
      (with-open-file (out log-path :direction :output :if-exists :append :if-does-not-exist :create)
        (format out "~&--~&~A~%" (date))
        (let ((*standard-output* (make-broadcast-stream
                                  out #+sbcl(make-instance 'count-line-stream))))
          (uiop/run-program:run-program
           (list (sh) "-lc" (format nil "cd ~S;~A ~A"
                                    (#+win32 mingw-namestring #-win32 princ-to-string src)
                                    (or #-win32 (sh) "")
                                    "./install.sh")) :output t)))
      (format *error-output* "done.~%")))
  (cons t argv))

(defun sbcl-head (type)
  (case type
    (:help '(sbcl-head-help))
    (:install `(,(decide-version 'sbcl-head-get-version)
                sbcl-head-argv-parse
                #+win32 sbcl-msys
                start
                ,(decide-download 'sbcl-head-download)
                sbcl-head-expand
                sbcl-head-install
                setup))
    (:list 'sbcl-head-get-version)))
