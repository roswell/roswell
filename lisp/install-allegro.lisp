(roswell:include "util-install-quicklisp")
(defpackage :roswell.install.allegro
  (:use :cl :roswell.install :roswell.util :roswell.locations))
(in-package :roswell.install.allegro)
(roswell:quicklisp :environment nil)

(defun allegro-get-version ()
  (mapcar #'car *allegro-agreement-uri*))

(defun make-allegro-uri (argv &key
                              (uname (uname))
                              (uname-m (uname-m))
                              (version (getf argv :version)))
  (let ((os (intern uname :keyword))
        (machine (intern uname-m :keyword)))
    (cond ((find version '("11.0express"
                           "10.1express")
                 :test 'equal)
           (format nil "~@{~A~}"
                   (allegro-uri) "ftp/pub/acl" version "/"
                   (ecase os
                     (:|linux| (ecase machine
                                 (:|x86-64| "linuxamd64.64")
                                 (:|arm64| "linuxarm64")
                                 (:|x86| "linux86")))
                     (:|darwin| (case machine
                                  (:|arm64| "macarm64.64")
                                  (:|x86-64| "macosx86-64.64")))
                     (:|windows| os)  ;; 32bit only
                     (:|freebsd| os)) ;; 32bit only
                   "/acl" version
                   (ecase os
                     (:|linux| (format nil "~@{~A~}"
                                       "-linux-"
                                       (ecase machine
                                         (:|x86-64| "x64")
                                         (:|arm64| "aarch64")
                                         (:|x86| "x86"))
                                       (cond
                                         ((equal "100express" version) ".bz2")
                                         (t ".tbz2"))))
                     (:|darwin| (format nil "~@{~A~}"
                                        (if (equal version "10.1express")
                                            "-macosx-"
                                            "-macos-")
                                        (ecase machine
                                          (:|x86-64| "x64.dmg")
                                          (:|arm64| "arm64.dmg"))))
                     (:|windows| "-x86.exe")
                     (:|freebsd| (format nil "~@{~A~}"
                                         "-freebsd-"
                                         (ecase machine
                                           (:|x86-64| "x86.tbz2")
                                           (:|x86| "x86.tbz2"))))))))))

(defun allegro-argv-parse (argv)
  (format *error-output* "~&Installing allegro/~A...~%" (getf argv :version))
  (cons t argv))

(defun allegro-download (argv)
  (let* ((version (getf argv :version)))
    (set-opt "as" version)
    (set-opt "download.uri" (make-allegro-uri argv))
    (set-opt "download.archive" (let ((pos (position #\/ (opt "download.uri") :from-end t)))
                                  (when pos
                                    (merge-pathnames (format nil "archives/~A" (subseq (opt "download.uri") (1+ pos))) (homedir)))))
    `((,(opt "download.archive") ,(opt "download.uri")))))

(defun allegro-expand (argv)
  (format t "~%Extracting archive:~A~%" (opt "download.archive"))
  (let* ((uname (uname))
         (impls (merge-pathnames (format nil "impls/~A/~A/allegro/" (uname-m) uname) (homedir)))
         (path (merge-pathnames (format nil "~A/" (opt "as")) impls)))
    (cond
      ((equal uname "darwin")
       (let ((mount-dir (string-right-trim
                         (format nil "~%")
                         (uiop:run-program
                          (format nil "hdiutil attach ~A | awk -F '\t' 'END{print $NF}'" (opt "download.archive"))
                          :output :string))))
         (uiop:run-program (format nil "cp -r \"~A/AllegroCL64express.app/Contents/Resources/\" \"~A\""
                                   mount-dir
                                   (ensure-directories-exist (merge-pathnames (format nil "~A/" (opt "as")) impls))))
         (uiop:run-program (format nil "hdiutil detach \"~A\"" mount-dir))))
      (t
       (expand (opt "download.archive") (ensure-directories-exist impls))
       (and (probe-file path)
            (uiop/filesystem:delete-directory-tree
             path :validate t))
       (ql-impl-util:rename-directory
        (first (loop for file in  (directory (merge-pathnames (format nil "acl~A*/" (getf argv :version)) impls))
                     when (probe-file file)
                     collect it))
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
