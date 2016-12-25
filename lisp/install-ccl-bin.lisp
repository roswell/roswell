(ros:include "util-install-quicklisp")
(defpackage :roswell.install.ccl-bin
  (:use :cl :roswell.install :ros.util :ros.locations))
(in-package :roswell.install.ccl-bin)

(defun ccl-bin-get-version ()
  (let ((file (merge-pathnames "tmp/ccl-bin.html" (homedir))))
    (format *error-output* "Checking version to install...~%")
    (unless (and (probe-file file)
                 (< (get-universal-time) (+ (* 60 60) (file-write-date file))))
      (download (ccl-bin-uri) file))
    (loop for link in (plump:get-elements-by-tag-name (plump:parse file) "a")
          for href = (plump:get-attribute link "href")
          for len = (length href)
          when (and (digit-char-p (aref href 0))
                    (char= (aref href (1- len)) #\/))
            collect (string-right-trim "/" href))))

(defvar *ccl-uname-m-alist*
  '(("x86-64" . "x86")
    ("armhf" . "arm")))

(defun ccl-uname-m ()
  (or (cdr (assoc (uname-m) *ccl-uname-m-alist* :test 'equal))
      (uname-m)))

(defun ccl-bin-argv-parse (argv)
  (format *error-output* "~&Installing ccl-bin/~A...~%" (getf argv :version))
  (when (position "--without-install" (getf argv :argv) :test 'equal)
    (set-opt "without-install" t))
  (cons t argv))

(defun ccl-bin-download (argv)
  (let ((uname (uname))
        (ccl-uname-m (ccl-uname-m)))
    (set-opt "as" (getf argv :version))
    (set-opt "download.uri" (format nil "~@{~A~}" (ccl-bin-uri)
                                    (getf argv :version) "/ccl-" (getf argv :version) "-" uname ccl-uname-m (if (equal uname "windows")
                                                                                                                ".zip"".tar.gz")))
    (set-opt "download.archive" (let ((pos (position #\/ (opt "download.uri") :from-end t)))
                                  (when pos
                                    (merge-pathnames (format nil "archives/~A" (subseq (opt "download.uri") (1+ pos))) (homedir)))))
    `((,(opt "download.archive") ,(opt "download.uri")))))

(defun ccl-bin-expand (argv)
  (format t "~%Extracting archive:~A~%" (opt "download.archive"))
  (let* ((impls (merge-pathnames (format nil "impls/~A/~A/ccl-bin/" (uname-m) (uname)) (homedir)))
         (path (merge-pathnames (format nil "~A/" (opt "as")) impls)))
    (#-win32 expand #+win32 zip:unzip (opt "download.archive") (ensure-directories-exist impls))
    (and (probe-file path)
         (uiop/filesystem:delete-directory-tree
          path :validate t))
    (ql-impl-util:rename-directory
     (merge-pathnames (format nil "ccl/") impls)
     (merge-pathnames (format nil "~A/" (opt "as")) impls)))
  (cons t argv))

(defun ccl-bin-help (argv)
  (format t "ccl-bin install options~%")
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

(defun ccl-bin (type)
  (case type
    (:help '(ccl-bin-help))
    (:install `(,(decide-version 'ccl-bin-get-version)
                ccl-bin-argv-parse
                ,(decide-download 'ccl-bin-download)
                ccl-bin-expand
                setup))
    (:list 'ccl-bin-get-version)))
