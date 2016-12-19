(ros:include "util-install-quicklisp")
(defpackage :roswell.install.allegro
  (:use :cl :ros.install :ros.util :ros.locations))
(in-package :roswell.install.allegro)
(ros:quicklisp :environment nil)

(defun allegro-get-version ()
  (list "100express"))

(defvar *allegro-uname-m-alist*
  '(("x86-64" . "x86")))

(defvar *allegro-uname-alist*
  '(("linux" . "linu")
    ("darwin" . "macos")))

(defun allegro-uname-m ()
  (or (cdr (assoc (uname-m) *allegro-uname-m-alist* :test 'equal))
      (uname-m)))

(defun allegro-uname ()
  (or (cdr (assoc (uname) *allegro-uname-alist* :test 'equal))
      (uname)))

(defun allegro-argv-parse (argv)
  (format *error-output* "~&Installing allegro/~A...~%" (getf argv :version))
  (cons t argv))

(defun allegro-download (argv)
  (let ((uname (allegro-uname))
        (uname-m (allegro-uname-m)))
    (set-opt "as" (getf argv :version))
    (set-opt "download.uri" (format nil "~@{~A~}" (allegro-uri)
                                    "/acl" (getf argv :version) "/" uname uname-m "/acl" (getf argv :version)
                                    "-" uname "x-" uname-m (cond ((equal uname "macos") ".dmg")
                                                                 ((equal uname "linu") ".bz2"))))
    (set-opt "download.archive" (let ((pos (position #\/ (opt "download.uri") :from-end t)))
                                  (when pos
                                    (merge-pathnames (format nil "archives/~A" (subseq (opt "download.uri") (1+ pos))) (homedir)))))
    `((,(opt "download.archive") ,(opt "download.uri")))))

(defun allegro-expand (argv)
  (format t "~%Extracting archive:~A~%" (opt "download.archive"))
  (let* ((impls (merge-pathnames (format nil "impls/~A/~A/allegro/" (uname-m) (uname)) (homedir)))
         (path (merge-pathnames (format nil "~A/" (opt "as")) impls))
         (uname (allegro-uname)))
    (cond
      ((equal uname "macos")
       (let ((mount-dir (string-right-trim
                         (format nil "~%")
                         (uiop:run-program
                          (format nil "hdiutil attach ~A | awk -F '\t' 'END{print $NF}'" (opt "download.archive"))
                          :output :string))))
         (uiop:run-program (format nil "cp -r ~A/AllegroCLexpress.app/Contents/Resources/ ~A"
                                   mount-dir
                                   (ensure-directories-exist (merge-pathnames (format nil "~A/" (opt "as")) impls))))
         (uiop:run-program (format nil "hdiutil detach \"~A\"" mount-dir))))
      (t
       (expand (opt "download.archive") (ensure-directories-exist impls))
       (and (probe-file path)
            (uiop/filesystem:delete-directory-tree
             path :validate t))
       (ql-impl-util:rename-directory
        (merge-pathnames (format nil "acl~A/" (getf argv :version)) impls)
        (merge-pathnames (format nil "~A/" (opt "as")) impls))))
    (cons t argv)))

(defun allegro-help (argv)
  (format t "allegro install options~%")
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

(defun allegro (type)
  (case type
    (:help '(allegro-help))
    (:install `(,(decide-version 'allegro-get-version)
                allegro-argv-parse
                ,(decide-download 'allegro-download)
                allegro-expand
                setup))
    (:list 'allegro-get-version)))
