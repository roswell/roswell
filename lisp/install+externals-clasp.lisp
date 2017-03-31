(roswell:include "util-install-quicklisp")
(defpackage :roswell.install.externals-clasp+
  (:use :cl :roswell.install :roswell.util :roswell.locations))
(in-package :roswell.install.externals-clasp+)

(defun externals-clasp-get-version ()
  '("cb00dd0edc7e698162aa176a7d51b9d704bd2596"))

(defun externals-clasp-download (argv)
  (set-opt "download.uri" (format nil "~@{~A~}" "https://github.com/drmeister/externals-clasp/archive/"
                                  (getf argv :version) ".tar.gz"))
  (set-opt "download.archive" (let ((pos (position #\/ (opt "download.uri") :from-end t)))
                                (when pos
                                  (merge-pathnames (format nil "archives/externals-clasp-~A" (subseq (opt "download.uri") (1+ pos))) (homedir)))))
  `((,(opt "download.archive") ,(opt "download.uri"))))

(defun externals-clasp-expand (argv)
  (format t "~%Extracting archive:~A~%" (opt "download.archive"))
  (when (and (probe-file (merge-pathnames (format nil "lib/~A/~A/externals-clasp/~A/success" (uname-m) (uname) (getf argv :version))
                                          (homedir)))
             (not (opt "install.force")))
    (format t "~A/~A is already installed. add 'install.force=t' option for the forced re-installation.~%"
            (getf argv :target)  (getf argv :version))
    (return-from externals-clasp-expand (cons nil argv)))
  (expand (opt "download.archive")
          (ensure-directories-exist
           (merge-pathnames (format nil "lib/~A/~A/externals-clasp/" (uname-m) (uname)) (homedir))))
  (ignore-errors
   (let ((h (homedir))
         (v (getf argv :version)))
     (set-opt "src" (merge-pathnames (format nil "lib/~A/~A/externals-clasp/~A/" (uname-m) (uname) v) h))
     (ql-impl-util:rename-directory
      (merge-pathnames (format nil "lib/~A/~A/externals-clasp/externals-clasp-~A" (uname-m) (uname) v) h)
      (merge-pathnames (format nil "lib/~A/~A/externals-clasp/~A" (uname-m) (uname) v) h))))
  (cons (not (opt "until-extract")) argv))

(defun externals-clasp-make (argv)
  (with-open-file (out (ensure-directories-exist
                        (merge-pathnames (format nil "log/externals-clasp/~A/make.log"
                                                 (getf argv :version))
                                         (homedir)))
                       :direction :output :if-exists :append :if-does-not-exist :create)
    (format out "~&--~&~A~%" (date))
    (let* ((src (opt "src"))
           (cmd "make")
           (*standard-output* (make-broadcast-stream out #+sbcl(make-instance 'count-line-stream))))
      (chdir src)
      (format t "~&~S~%" cmd)
      (uiop/run-program:run-program cmd :output t :ignore-error-status nil)))
  (setf (config "externals.clasp.version") (getf argv :version))
  (cons t argv))

(defun externals-clasp-sentinel (argv)
  (with-open-file (i (merge-pathnames (format nil "lib/~A/~A/externals-clasp/~A/success" (uname-m) (uname) (getf argv :version))
                                      (homedir))
                     :direction :probe
                     :if-does-not-exist :create))
  (cons t argv))

(defun externals-clasp-help (argv)
  (format t "~%")
  (cons t argv))

(defun externals-clasp+ (type)
  (case type
    (:help '(externals-clasp-help))
    (:install `(,(decide-version 'externals-clasp-get-version)
                start
                ,(decide-download 'externals-clasp-download)
                externals-clasp-expand
                externals-clasp-make
                externals-clasp-sentinel))))
