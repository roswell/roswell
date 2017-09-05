(roswell:include "util-install")
(defpackage :roswell.install.quicklisp
  (:use :cl :roswell.install :roswell.util :roswell.locations))
(in-package :roswell.install.quicklisp)

(defun quicklisp-help (argv)
  (format *error-output* "no options for quicklisp~%")
  (cons t argv))

(defun quicklisp-patch (path)
  (with-open-file (out (ensure-directories-exist (merge-pathnames "local-init/roswell.lisp" path))
                       :direction :output :if-exists :supersede)
    (format out "(roswell:include \"patch-quicklisp\")~%")))

(defun quicklisp-argv-parse (argv)
  (set-opt "download.uri" (format nil "~A~A" (quicklisp-uri) "quicklisp.lisp"))
  (set-opt "download.archive" (merge-pathnames "archives/quicklisp.lisp" (homedir)))
  (let ((pos (position "--path" (getf argv :argv) :test 'equal)))
    (and pos (ignore-errors (nth (1+ pos) (getf argv :argv)))
         (set-opt "quicklisp" (nth (1+ pos) (getf argv :argv)))))
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
  #+sbcl
  (ignore-errors
   (require :sb-posix)
   (let ((gid (sb-posix:getenv "SUDO_GID")))
     (sb-posix:setgid (parse-integer gid)))
   (let ((uid (sb-posix:getenv "SUDO_UID")))
     (sb-posix:setuid (parse-integer uid))))
  (let*((archive (opt "download.archive"))
        (env (opt "roswellenv"))
        (path (if env
                  (merge-pathnames (format nil "env/~A/lisp/quicklisp/" env) (homedir))
                  (opt "quicklisp"))))
    (when env
      (setf (config "quicklisp" :where env) env))
    (quicklisp-patch path)
    (cond
      ((probe-file (merge-pathnames "setup.lisp" path))
       (format *error-output* "Quicklisp is already setup.~%"))
      (t
       (when (find-package :ql)
         (delete-package :ql)
         (defpackage #:quicklisp
           (:export #:setup))
         (setf (fdefinition (intern (string :setup) (find-package :quicklisp)))
               (lambda())))
       (let ((*standard-output* (make-broadcast-stream)))
         (load archive))
       ;; use roswell to download everithing.
       (setf (fdefinition (find-symbol (string :fetch) :qlqs-http))
             (lambda (url file &key (follow-redirects t) quietly
                                 (maximum-redirects 10))
               "Request URL and write the body of the response to FILE."
               (declare (ignorable url file follow-redirects quietly
                                   maximum-redirects))
               (download url file)))
       (let ((*standard-output* (make-broadcast-stream)))
         (funcall (intern (string :install) (find-package :quicklisp-quickstart))
                  :path path
                  :proxy (opt "quicklisp.proxy")
                  :client-url (opt "quicklisp.client")
                  :dist-url (opt "quicklisp.dist"))))))
  (cons t argv))

(defun quicklisp (type)
  (case type
    (:help '(quicklisp-help))
    (:install '(quicklisp-argv-parse
                quicklisp-download
                quicklisp-install))))
