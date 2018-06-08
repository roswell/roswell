(roswell:include "util-install-quicklisp")
(defpackage :roswell.install.mkcl
  (:use :cl :roswell.install :roswell.util :roswell.locations))
(in-package :roswell.install.mkcl)

(defvar *mkcl-options*
  '())

(defun mkcl-get-version ()
  (format *error-output* "Checking version to install....~%")
  (github-version "https://github.com/jcbeaudoin/MKCL/releases.atom"
                  "mkcl"
                  (lambda (href)
                    (let ((a (ignore-errors
                               (subseq href (1+ (position #\/ href :from-end t :end (position #\/ href :from-end t)))))))
                      (ignore-errors (subseq a (position-if #'digit-char-p a)))))))

(defun mkcl-version-filename (version)
  (or
   (find-if (lambda (x) (ignore-errors (equal (subseq x (- (length x) (length version))) version)))
           (github-version "https://github.com/jcbeaudoin/MKCL/releases.atom" "mkcl"
                           (lambda (href) (subseq href (1+ (position #\/ href :from-end t))))))
   version))

(defun mkcl-chdir (argv)
  (let ((version (getf argv :version)))
    (or (ignore-errors
          (chdir (merge-pathnames (format nil "src/MKCL-~A/" version) (homedir)))))))

(defun mkcl-argv-parse (argv)
  (let ((pos (position "--as" (getf argv :argv) :test 'equal)))
    (set-opt "as" (or (and pos (ignore-errors (nth (1+ pos) (getf argv :argv)))
                           (format nil "~A-~A"
                                   (getf argv :version)
                                   (nth (1+ pos) (getf argv :argv))))
                      (getf argv :version))))
  (when (position "--archive" (getf argv :argv) :test 'equal)
    (set-opt "install.force" "t")
    (set-opt "archive" "t"))
  (when (position "--without-install" (getf argv :argv) :test 'equal)
    (set-opt "without-install" t))
  (set-opt "prefix" (merge-pathnames (format nil "impls/~A/~A/~A/~A/" (uname-m) (uname) (getf argv :target) (opt "as")) (homedir)))
  (labels ((with (opt default)
             (set-opt opt
                      (cond ((position (format nil "--with-~A" opt) (getf argv :argv) :test 'equal) t)
                            ((position (format nil "--without-~A" opt) (getf argv :argv) :test 'equal) nil)
                            (t default)))))
    (loop for (opt default . nil) in *mkcl-options*
       do (with opt default)))
  (cons t argv))

(defun mkcl-download (argv)
  (set-opt "download.uri" (format nil "~A~A.tar.gz" "https://github.com/jcbeaudoin/MKCL/archive/" (mkcl-version-filename (getf argv :version))))
  (set-opt "download.archive" (let ((pos (position #\/ (opt "download.uri") :from-end t)))
                                (when pos
                                  (merge-pathnames (format nil "archives/~A" (subseq (opt "download.uri") (1+ pos))) (homedir)))))
  `((,(opt "download.archive") ,(opt "download.uri"))))

(defun mkcl-expand (argv)
  (format t "~%Extracting archive:~A~%" (opt "download.archive"))
  (expand (opt "download.archive")
          (merge-pathnames "src/" (homedir)))
  (cons t argv))

(defun mkcl-config (argv)
  (format t "~&configure~%")
  (with-open-file (out (ensure-directories-exist
                        (merge-pathnames (format nil "impls/log/~A-~A/config.log"
                                                 (getf argv :target) (opt "as"))
                                         (homedir)))
                       :direction :output :if-exists :append :if-does-not-exist :create)
    (format out "~&--~&~A~%" (date))
    (let* ((cmd (format nil "./configure '--prefix=~A'" (opt "prefix")))
           (*standard-output* (make-broadcast-stream out #+sbcl(make-instance 'count-line-stream))))
      (mkcl-chdir argv)
      (uiop/run-program:run-program cmd :output t :ignore-error-status t)))
  (cons t argv))

(defun mkcl-make (argv)
  (format t "~&make~%")
  (with-open-file (out (ensure-directories-exist
                        (merge-pathnames (format nil "impls/log/~A-~A/make.log"
                                                 (getf argv :target) (opt "as"))
                                         (homedir)))
                       :direction :output :if-exists :append :if-does-not-exist :create)
    (format out "~&--~&~A~%" (date))
    (let* ((cmd (format nil "make"))
           (*standard-output* (make-broadcast-stream out #+sbcl(make-instance 'count-line-stream))))
      (mkcl-chdir argv)
      (uiop/run-program:run-program cmd :output t :ignore-error-status t)))
  (cons t argv))

(defun mkcl-install (argv)
  (let* ((impl-path (opt "prefix"))
         (log-path (merge-pathnames (format nil "impls/log/~A-~A/install.log" (getf argv :target) (opt "as")) (homedir))))
    (format t "~&Installing ~A/~A..." (getf argv :target) (opt "as"))
    (format t "~&prefix: ~s~%" impl-path)
    (ensure-directories-exist impl-path)
    (ensure-directories-exist log-path)
    (mkcl-chdir argv)
    (with-open-file (out log-path :direction :output :if-exists :append :if-does-not-exist :create)
      (format out "~&--~&~A~%" (date))
      (let ((*standard-output* (make-broadcast-stream
                                out #+sbcl(make-instance 'count-line-stream))))
        (uiop/run-program:run-program "make install" :output t)))
    (format *error-output* "done.~%"))
  (cons t argv))

(defun mkcl-clean (argv)
  (format t "~&Cleaning~%")
  (mkcl-chdir argv)
  (let* ((out (make-broadcast-stream))
         (*standard-output* (make-broadcast-stream
                             out #+sbcl(make-instance 'count-line-stream))))
    (uiop/run-program:run-program
     (list (sh) "-lc" (format nil "cd ~S;make clean" (namestring (uiop:getcwd))))
     :output t))
  (format t "done.~%")
  (cons t argv))

(defun mkcl (type)
  (case type
    (:help '())
    (:install `(,(decide-version 'mkcl-get-version)
                mkcl-argv-parse
                start
                ,(decide-download 'mkcl-download)
                mkcl-expand
                mkcl-config
                mkcl-make
                mkcl-install
                mkcl-clean
                setup))
    #+nil(:list 'mkcl-get-version)))
