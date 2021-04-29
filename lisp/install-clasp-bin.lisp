(roswell:include "util-install-quicklisp")
(defpackage :roswell.install.clasp-bin
  (:use :cl :roswell.install :roswell.util :roswell.locations))
(in-package :roswell.install.clasp-bin)

(defun clasp-bin-get-version ()
  (format *error-output* "Checking version to install...~%")
  (cddr (github-version (clasp-git-version-uri)
                        "clasp"
                        (lambda (href) (subseq href (1+ (position #\/ href :from-end t)))))))

(defun clasp-bin-argv-parse (argv)
  (format *error-output* "~&Installing clasp-bin/~A...~%" (getf argv :version))
  (when (position "--without-install" (getf argv :argv) :test 'equal)
    (set-opt "without-install" t))
  (cons t argv))

(defun clasp-bin-download (argv)
  (let* ((uname (uname))
         (version (getf argv :version))
         (name (format nil "~@{~A~}" "clasp-" version "-x86-64-" uname)))
    (set-opt "as" version)
    (set-opt "download.uri" (or (opt "download.uri")
                                (format nil "~@{~A~}" (clasp-bin-uri)
                                        version "/" name "-binary" ".tar.gz")))
    (set-opt "download.archive" (let ((pos (position #\/ (opt "download.uri") :from-end t)))
                                  (when pos
                                    (merge-pathnames (format nil "archives/~A" (subseq (opt "download.uri") (1+ pos))) (homedir)))))
    `((,(opt "download.archive") ,(opt "download.uri")))))

(defun clasp-bin-expand (argv)
  (format t "~%Extracting archive:~A~%" (opt "download.archive"))
  (let* ((impls (merge-pathnames (format nil "impls/~A/~A/clasp-bin/" (uname-m) (uname)) (homedir)))
         (path (merge-pathnames (format nil "~A/" (opt "as")) impls))
         (version (getf argv :version))
         (uname (uname)))
    (expand (opt "download.archive") (ensure-directories-exist impls))
    (and (probe-file path)
         (uiop/filesystem:delete-directory-tree
          path :validate t))
    (ql-impl-util:rename-directory
     (merge-pathnames (format nil "~@{~A~}" "clasp-" version "-x86-64-" uname "/") impls)
     (merge-pathnames (opt "as") impls)))
  (cons t argv))

(defun clasp-bin-help (argv)
  (format t "clasp-bin install options~%")
  (flet ((fmt (param default more)
           (format t "--~A~A ~A~%~5T~A~%"
                   (cond ((eql default t) "without-")
                         ((null default) "with-")
                         (t ""))
                   param
                   (or (and (not (null default))
                            (not (eql default t))
                            default)
                       "")
                   more)))
    (fmt "install" t "Download archive"))
  (cons t argv))

(defun clasp-bin (type)
  (case type
    (:help '(clasp-bin-help))
    (:install `(,(decide-version 'clasp-bin-get-version)
                clasp-bin-argv-parse
                ,(decide-download 'clasp-bin-download)
                clasp-bin-expand
                setup))
    (:list 'clasp-bin-get-version)))
