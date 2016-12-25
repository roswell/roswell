(ros:include "util-install")
(defpackage :roswell.install.quicklisp
  (:use :cl :roswell.install :ros.util :ros.locations))
(in-package :roswell.install.quicklisp)

(defun quicklisp-help (argv)
  (format *error-output* "no options for quicklisp~%")
  (cons t argv))

(defun quicklisp-patch (path)
  (ros:ensure-asdf)
  (read-call "uiop:copy-file"
   (make-pathname
    :defaults #.*load-pathname*
    :name "patch-quicklisp" :type "lisp")
   (ensure-directories-exist (merge-pathnames "local-init/ros-download.lisp" path))))

(defun quicklisp-argv-parse (argv)
  (set-opt "download.uri" (format nil "~A~A" (quicklisp-uri) "quicklisp.lisp"))
  (set-opt "download.archive" (merge-pathnames (format nil "archives/~A" "quicklisp.lisp") (homedir)))
  (cons t argv))

(defun quicklisp-download (argv)
  (if (or (not (probe-file (opt "download.archive")))
          (opt "download.force"))
      (progn
        (format t "~&Downloading archive: ~A~%" (opt "download.uri"))
        (download (opt "download.uri") (opt "download.archive")))
      (format t "~&Skip downloading ~A.~%Specify 'download.force=t' to download again.~%"
              (opt "download.uri")))
  (cons (not (opt "without-install")) argv))

(defun quicklisp-install (argv)
  #-win32
  (ignore-errors
   (require :sb-posix)
   (let ((gid (sb-posix:getenv "SUDO_GID")))
     (sb-posix:setgid (parse-integer gid)))
   (let ((uid (sb-posix:getenv "SUDO_UID")))
     (sb-posix:setuid (parse-integer uid))))
  (let ((archive (opt "download.archive"))
        (path (opt "quicklisp")))
    (quicklisp-patch path)
    (cond
      ((probe-file (merge-pathnames "setup.lisp" path))
       (format *error-output* "Quicklisp is already setup.~%"))
      (t
       (let ((*standard-output* (make-broadcast-stream)))
         (load archive))
       ;; use roswell to download everithing.
       (setf (fdefinition (find-symbol (string :fetch) :qlqs-http))
             (lambda (url file &key (follow-redirects t) quietly
                                 (maximum-redirects 10))
               "Request URL and write the body of the response to FILE."
               (declare (ignorable url file follow-redirects quietly
                                   maximum-redirects))
               (ros:roswell `("roswell-internal-use" "download" ,url ,file) :interactive nil)))
       (let ((*standard-output* (make-broadcast-stream)))
         (funcall (intern (string :install) (find-package :quicklisp-quickstart))
                  :path path
                  :client-url (opt "quicklisp.client")
                  :dist-url (opt "quicklisp.dist"))))))
  (cons t argv))

(defun quicklisp (type)
  (case type
    (:help '(quicklisp-help))
    (:install '(quicklisp-argv-parse
                quicklisp-download
                quicklisp-install))))
