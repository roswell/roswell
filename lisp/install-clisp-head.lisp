(roswell:include "util-install-quicklisp")
(defpackage :roswell.install.clisp-head
  (:use :cl :roswell.install :roswell.util :roswell.locations))
(in-package :roswell.install.clisp-head)

(defun clisp-head-variant ()
  (let ((uname (uname)))
    (if (and (equal uname "linux"))
        "glibc2.19"
        ""))) ;; TBD

(defun clisp-head-get-version ()
  (format *error-output* "Checking version to install....~%")
  (let ((elts
          (let ((file (merge-pathnames "tmp/clisp_head.tsv" (homedir))))
            (download (clisp-head-version-uri) file :interval 3600)
            (mapcar (lambda (x) (uiop:split-string x :separator '(#\tab)))
                    (uiop:read-file-lines file))))
        (uname (uname))
        (uname-m (uname-m))
        (variant (clisp-head-variant)))
    (mapcar 'third (remove-if-not (lambda (x)
                                    (and (equal (first x) uname)
                                         (equal (second x) uname-m)
                                         (equal (fourth x) variant)
                                         ))
                                  elts))))

(defun clisp-head-argv-parse (argv)
  (let ((uname (uname))
        (uname-m (uname-m)))
    (set-opt "as" (getf argv :version))
    (set-opt "prefix" (merge-pathnames (format nil "impls/~A/~A/~A/~A/" (uname-m) (uname) (getf argv :target) (opt "as")) (homedir)))
    (set-opt "src" (merge-pathnames (format nil "src/clisp-~A-~A-~A~A/"
                                            (getf argv :version)
                                            uname-m
                                            uname
                                            (let ((var (clisp-head-variant)))
                                              (if (zerop (length var))
                                                  ""
                                                  (format nil "-~A" var))))
                                    (homedir))))
  (cons t argv))

(defun clisp-head-download (argv)
  (set-opt "download.uri" (format nil "~@{~A~}" (clisp-head-uri) (getf argv :version)
                                  "/clisp-"
                                  (getf argv :version) "-"
                                  (uname-m) "-"
                                  (uname)
                                  (let ((var (clisp-head-variant)))
                                    (if (zerop (length var))
                                        ""
                                        (format nil "-~A" var)))
                                  "-binary"
                                  #-win32 ".tar.bz2"
                                  #+win32 ".msi"))
  (set-opt "download.archive" (let ((pos (position #\/ (opt "download.uri") :from-end t)))
                                (when pos
                                  (merge-pathnames (format nil "archives/~A" (subseq (opt "download.uri") (1+ pos))) (homedir)))))
  `((,(opt "download.archive") ,(opt "download.uri"))))

#-win32
(defun clisp-head-expand (argv)
  (let ((impls (merge-pathnames (format nil "impls/~A/~A/clisp-head/" (uname-m) (uname)) (homedir))))
    (cond
      (t
       (format t "~%Extracting archive:~A~%" (opt "download.archive"))
       (expand (opt "download.archive")
               (ensure-directories-exist impls))))
    (ql-impl-util:rename-directory
     (merge-pathnames (format nil "clisp-~A-~A-~A~A/"
                              (opt "as")
                              (uname-m)
                              (uname)
                              (let ((var (clisp-head-variant)))
                                (if (zerop (length var))
                                    ""
                                    (format nil "-~A" var))))
                      impls)
     (merge-pathnames (format nil "~A/" (opt "as")) impls)))
  (cons t argv))

(defun clisp-head-help (argv)
  (format t "no options for clisp-head~%")
  (cons t argv))

(defun clisp-head (type)
  (case type
    (:help '(clisp-head-help))
    (:install `(,(decide-version 'clisp-head-get-version)
                clisp-head-argv-parse
                start
                ,(decide-download 'clisp-head-download)
                clisp-head-expand
                setup))
    (:list 'clisp-head-get-version)))
