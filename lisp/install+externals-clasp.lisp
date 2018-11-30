(roswell:include "util-install-quicklisp")
(defpackage :roswell.install.externals-clasp+
  (:use :cl :roswell.install :roswell.util :roswell.locations))
(in-package :roswell.install.externals-clasp+)

(defparameter *external-clasp-version*
  ;; alias commit
  '(("6.0.1" "8ad13ba592230f13d11c13b2b0498844ca7083ef")
    ("5.0-20171109" "f8c4535b474dfec5fd567a161c0a6c94a61743f2")
    ("5.0"     "bffc94f06a87b88eecc8fef0731929a97ba3dad8")
    ("4.0"     "489501b0fce58d63d00efbb01d0c3b39a07a57ce")
    ("3.9.1"   "cb00dd0edc7e698162aa176a7d51b9d704bd2596")))

(defun externals-clasp-get-version ()
  (mapcar #'first *external-clasp-version*))

(defun externals-clasp-download (argv)
  (set-opt "download.uri" (format nil "~@{~A~}" "https://github.com/gos-k/externals-clasp/archive/"
                                  (or (second (assoc (getf argv :version) *external-clasp-version* :test 'equal))
                                      (getf argv :version))
                                  ".tar.gz"))
  (set-opt "download.archive" (merge-pathnames (format nil "archives/externals-clasp-~A.tar.gz" (getf argv :version)) (homedir)))
  `((,(opt "download.archive") ,(opt "download.uri"))))

(defun externals-clasp-expand (argv)
  (format t "~%Extracting archive:~A~%" (opt "download.archive"))
  (let* ((h (homedir))
         (v (getf argv :version))
         ;; replace externals-clasp to externals_clasp
         ;; skip version check of clasp's wscript
         (clasp (merge-pathnames (format nil "lib/~A/~A/externals_clasp/" (uname-m) (uname) v) h)))
    (set-opt "src" (merge-pathnames (format nil "~A/" v) clasp))
    (when (and (probe-file (merge-pathnames "success" (opt "src")))
               (not (opt "install.force")))
      (format t "~A/~A is already installed. add 'install.force=t' option for the forced re-installation.~%"
              (getf argv :target) v)
      (return-from externals-clasp-expand (cons nil argv)))
    (expand (opt "download.archive") (ensure-directories-exist clasp))
    (ignore-errors
     (ql-impl-util:rename-directory
      (merge-pathnames (format nil "externals-clasp-~A" (or (second (assoc v *external-clasp-version* :test 'equal)) v)) clasp)
      (merge-pathnames (format nil "~A" v) clasp))))
  (cons (not (opt "until-extract")) argv))

(defun count-cpu ()
  #+linux
  (handler-case
      (with-open-file (stream #p"/proc/cpuinfo" :if-does-not-exist :error)
        (loop
          for line = (read-line stream nil nil)
          while line
          count (and (< 9 (length line))
                     (string= "processor" (subseq line 0 9)))))
    (file-error () nil))
  #+darwin
  (ignore-errors
   (let ((res (uiop:run-program "sysctl hw.logicalcpu" :output :string)))
     (parse-integer (subseq str(1+ (position #\: res))) :junk-allowed t))))

(defun total-memory ()
  "Return system installed memory in Bytes"
  #+linux
  (ignore-errors
    (with-open-file (stream #p"/proc/meminfo")
      (loop
           with len = (length "MemTotal:")
          for line = (read-line stream nil nil)
           while line
           when (equal (subseq line 0 len)
                       "MemTotal:")
           do (return-from total-memory
                (values (* 1024 (read-from-string line t nil :start len)))))))
  #+darwin
  :tbd
  nil)

(defvar *memory/cpu* 1000000000 "when building,memory which should be assignd to a cpu more than this in byte.")

(defun externals-clasp-make (argv)
  (with-open-file (out (ensure-directories-exist
                        (merge-pathnames (format nil "log/externals-clasp/~A/make.log"
                                                 (getf argv :version))
                                         (homedir)))
                       :direction :output :if-exists :append :if-does-not-exist :create)
    (format out "~&--~&~A~%" (date))
    (let* ((pjobs (max (min (or (count-cpu) 1)
                            (floor (or (total-memory)
                                       most-positive-fixnum)
                                   *memory/cpu*))
                       1))
           (src (opt "src"))
           (cmd "make")
           (*standard-output* (make-broadcast-stream out #+sbcl(make-instance 'count-line-stream))))
      (chdir src)
      (format t "~&~S~%" cmd)
      (with-open-file (stream (merge-pathnames "local.config" (opt "src")) :direction :output :if-exists :supersede)
        (format stream "export PJOBS=~A~%" pjobs))
      (uiop/run-program:run-program cmd :output t :ignore-error-status nil)))
  (setf (config "externals.clasp.version") (getf argv :version))
  (cons t argv))

(defun externals-clasp-sentinel (argv)
  (with-open-file (i (merge-pathnames "success" (opt "src"))
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
