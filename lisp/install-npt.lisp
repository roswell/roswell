(roswell:include "util-install-quicklisp")
(defpackage :roswell.install.npt
  (:use :cl :roswell.install :roswell.util :roswell.locations))
(in-package :roswell.install.npt)

(defvar *npt-options*
  '())

(defun npt-get-version ()
  (format *error-output* "Checking version to install....~%")
  (cons "git" (cddr (github-version
                     (npt-git-version-uri) "npt"
                     (lambda (href) (subseq href (+ 2 (position #\/ href :from-end t))))))))

(defun npt-argv-parse (argv)
  (let ((pos (position "--as" (getf argv :argv) :test 'equal)))
    (set-opt "as" (or (and pos (ignore-errors (nth (1+ pos) (getf argv :argv)))
                           (format nil "~A-~A"
                                   (getf argv :version)
                                   (nth (1+ pos) (getf argv :argv))))
                      (getf argv :version))))
  (set-opt "prefix" (merge-pathnames (format nil "impls/~A/~A/~A/~A/" (uname-m) (uname) (getf argv :target) (opt "as")) (homedir)))
  (set-opt "src" (merge-pathnames (format nil "src/npt-~A/" (getf argv :version)) (homedir)))
  (labels ((with (opt)
             (cond ((position (format nil "--with-~A" opt) (getf argv :argv) :test 'equal) (set-opt opt t))
                   ((position (format nil "--without-~A" opt) (getf argv :argv) :test 'equal) (set-opt opt :false)))))
    (loop for (name default description sb-prefix) in *npt-options*
          do
             (when default
               (set-opt name (eql default t)))
             (with name)))
  (cons t argv))

(defun npt-download (argv)
  (cond
    ((or (equal "git" (getf argv :version))
         (probe-file (merge-pathnames "src/npt-git/" (homedir))))
     (set-opt "src" (merge-pathnames "src/npt-git/" (homedir)))
     ()) ;; skip downloading if version is 'git'
    (t
     (set-opt "download.uri" (format nil "~@{~A~}" (npt-uri) "npt-"
                                     (getf argv :version) ".tar.gz"))
     (set-opt "download.archive" (let ((pos (position #\/ (opt "download.uri") :from-end t)))
                                   (when pos
                                     (merge-pathnames (format nil "archives/v~A" (subseq (opt "download.uri") (1+ pos))) (homedir)))))
     `((,(opt "download.archive") ,(opt "download.uri"))))))

(defun npt-expand (argv)
  (let* ((h (homedir))
         (v (getf argv :version))
         (gitsrc (merge-pathnames "src/npt-git/" h)))
    (cond
      ((equal "git" v)
       (unless (probe-file gitsrc)
         (clone-github "nptcl" "npt" :path (merge-pathnames "src/" h))
         (ql-impl-util:rename-directory
          (merge-pathnames "src/nptcl/npt/" h)
          gitsrc)
         (uiop/filesystem:delete-directory-tree (merge-pathnames "src/nptcl/" h) :validate t)))
      ((probe-file gitsrc)
       (chdir gitsrc)
       (uiop/run-program:run-program "git checkout master")
       (uiop/run-program:run-program "git pull")
       (or (uiop/run-program:run-program
            (format nil "git checkout ~A" v) :ignore-error-status t)
           (uiop/run-program:run-program
            (format nil "git checkout nptcl-~A" v))))
      #+nil ;; fix later
      (t
       (format t "~%Extracting archive:~A~%" (opt "download.archive"))
       (expand (opt "download.archive")
               (merge-pathnames "src/" h))
       (ignore-errors
        (ql-impl-util:rename-directory
         (merge-pathnames (format nil "src/sbcl-sbcl-~A" v) h)
         (merge-pathnames (format nil "src/sbcl-~A" v) h))
        (with-open-file (o (merge-pathnames (format nil "src/sbcl-~A/version.lisp-expr" v) h)
                           :direction :output
                           :if-does-not-exist :create
                           :if-exists nil)
          (format o "~S~%" v))))))
  (cons (not (opt "until-extract")) argv))

(defun npt-bootstrap (argv)
  (format t "~&bootstrap~%")
  (with-open-file (out (ensure-directories-exist
                        (merge-pathnames (format nil "impls/log/~A-~A/bootstrap.log"
                                                 (getf argv :target) (opt "as"))
                                         (homedir)))
                       :direction :output :if-exists :append :if-does-not-exist :create)
    (format out "~&--~&~A~%" (date))
    (let* ((cmd "./bootstrap.sh")
           (*standard-output* (make-broadcast-stream out #+sbcl(make-instance 'count-line-stream)))
           (src (opt "src")))
      (chdir src)
      (uiop/run-program:run-program cmd :output t :ignore-error-status t)))
  (cons t argv))

(defun npt-config (argv)
  (format t "~&configure~%")
  (with-open-file (out (ensure-directories-exist
                        (merge-pathnames (format nil "impls/log/~A-~A/config.log"
                                                 (getf argv :target) (opt "as"))
                                         (homedir)))
                       :direction :output :if-exists :append :if-does-not-exist :create)
    (format out "~&--~&~A~%" (date))
    (let* ((cmd (format nil "./configure '--prefix=~A'" (opt "prefix")))
           (*standard-output* (make-broadcast-stream out #+sbcl(make-instance 'count-line-stream)))
           (src (opt "src")))
      (chdir src)
      (uiop/run-program:run-program cmd :output t :ignore-error-status t)))
  (cons t argv))

(defun npt-make (argv)
  (format t "~&make~%")
  (with-open-file (out (ensure-directories-exist
                        (merge-pathnames (format nil "impls/log/~A-~A/make.log"
                                                 (getf argv :target) (opt "as"))
                                         (homedir)))
                       :direction :output :if-exists :append :if-does-not-exist :create)
    (format out "~&--~&~A~%" (date))
    (let* ((cmd (format nil "make"))
           (*standard-output* (make-broadcast-stream out #+sbcl(make-instance 'count-line-stream))))
      (chdir (opt "src"))
      (uiop/run-program:run-program cmd :output t :ignore-error-status t)))
  (cons t argv))

(defun npt-install (argv)
  (let* ((impl-path (opt "prefix"))
         (log-path (merge-pathnames (format nil "impls/log/~A-~A/install.log" (getf argv :target) (opt "as")) (homedir))))
    (format t "~&Installing ~A/~A..." (getf argv :target) (opt "as"))
    (format t "~&prefix: ~s~%" impl-path)
    (ensure-directories-exist impl-path)
    (ensure-directories-exist log-path)
    (chdir (opt "src"))
    (with-open-file (out log-path :direction :output :if-exists :append :if-does-not-exist :create)
      (format out "~&--~&~A~%" (date))
      (let ((*standard-output* (make-broadcast-stream
                                out #+sbcl(make-instance 'count-line-stream))))
        (uiop/run-program:run-program "make install" :output t)))
    (format *error-output* "done.~%"))
  (cons t argv))

(defun npt-clean (argv)
  (format t "~&Cleaning~%")
  (chdir (opt "src"))
  (let* ((out (make-broadcast-stream))
         (*standard-output* (make-broadcast-stream
                             out #+sbcl(make-instance 'count-line-stream))))
    (uiop/run-program:run-program
     (list (sh) "-lc" (format nil "cd ~S;make clean" (namestring (uiop:getcwd))))
     :output t))
  (format t "done.~%")
  (cons t argv))

(defun npt (type)
  (case type
    (:help '())
    (:install `(,(decide-version 'npt-get-version)
                npt-argv-parse
                start
                ,(decide-download 'npt-download)
                npt-expand
                npt-bootstrap
                npt-config
                npt-make
                npt-install
                npt-clean
                setup
                ))
    (:list 'npt-get-version)))