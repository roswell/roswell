(in-package :ros.install)
(ros:quicklisp :environment nil)

#+win32
(ql:quickload :zip :silent t)

(defun ccl-bin-get-version ()
  (let (result
        (file (merge-pathnames "tmp/ccl-bin.html" (homedir))))
    (format t "Checking version to install...~%")
    (unless (and (probe-file file)
                 (< (get-universal-time) (+ (* 60 60) (file-write-date file))))
      (download "http://ccl.clozure.com/ftp/pub/release/" file))
    (with-open-file (in file #+sbcl :external-format #+sbcl :utf-8)
      (ros:quicklisp :environment nil)
      (with-output-to-string (*standard-output*)
        (funcall (intern (string :quickload) :ql)
                 :cl-html-parse))
      (funcall (read-from-string "net.html.parser:parse-html")
               in
               :callbacks
               (list (cons :a (lambda (arg)
                                (let* ((href (getf (cdr (car arg)) :href))
                                       (len (length href)))
                                  (when (and (digit-char-p (aref href 0))
                                             (char= (aref href (1- len)) #\/))
                                    (push (string-right-trim "/" href) result))))))
               :callback-only t))
    (format t "~&Installing ccl-bin/~A...~%" (first result))
    result))

(defun ccl-bin-version (argv)
  (let ((version (getf argv :version)))
    (when (or (null version) (equal version "latest"))
      (setf (getf argv :version) (first (ccl-bin-get-version)))))
  (cons t argv))

(defun uname% ()
  (let ((uname (uname)))
    uname))

(defun uname-m% ()
  (let ((uname-m (uname-m)))
    (when (equal uname-m "x86-64")
      (setq uname-m "x86"))
    uname-m))

(defun ccl-bin-argv-parse (argv)
  (let ((uname (uname%))
        (uname-m (uname-m%)))
    (set-opt "as" (getf argv :version))
    (when (position "--without-install" (getf argv :argv) :test 'equal)
      (set-opt "without-install" t))
    (set-opt "download.uri" (format nil "~@{~A~}" "http://ccl.clozure.com/ftp/pub/release/"
                                    (getf argv :version) "/ccl-" (getf argv :version) "-" uname uname-m (if (equal uname "windows")
                                                                                                            ".zip"".tar.gz")))
    (set-opt "download.archive" (let ((pos (position #\/ (get-opt "download.uri") :from-end t)))
                                  (when pos 
                                    (merge-pathnames (format nil "archives/~A" (subseq (get-opt "download.uri") (1+ pos))) (homedir)))))
    (set-opt "prefix" (merge-pathnames (format nil "impls/~A/~A/~A/~A/" uname-m uname (getf argv :target) (get-opt "as")) (homedir)))
    (set-opt "src" (merge-pathnames (format nil "src/~A-~A/" (getf argv :target) (getf argv :version)) (homedir)))
    (cons t argv)))

(defun ccl-bin-download (argv)
  (if (or (not (probe-file (get-opt "download.archive")))
          (get-opt "download.force"))
      (progn
        (format t "~&Downloading archive:~A~%" (get-opt "download.uri"))
        ;;TBD proxy support... and other params progress bar?
        (download (get-opt "download.uri") (get-opt "download.archive")))
      (format t "~&Skip downloading ~A~%specify download.force=t to download it again.~%"
              (get-opt "download.uri")))
  (cons (not (get-opt "without-install")) argv))

(defun ccl-bin-expand (argv)
  (format t "~%Extracting archive:~A~%" (get-opt "download.archive"))
  (#-win32 expand #+win32 zip:unzip
           (get-opt "download.archive")
           (ensure-directories-exist (merge-pathnames (format nil "impls/~A/~A/ccl-bin/" (uname-m) (uname%)) (homedir))))
  (let ((path (merge-pathnames (format nil "impls/~A/~A/ccl-bin/~A/" (uname-m) (uname%) (get-opt "as")) (homedir))))
    (and (probe-file path)
         (uiop/filesystem:delete-directory-tree 
          path :validate t)))
  (ql-impl-util:rename-directory
   (merge-pathnames (format nil "impls/~A/~A/ccl-bin/ccl/" (uname-m) (uname%)) (homedir))
   (merge-pathnames (format nil "impls/~A/~A/ccl-bin/~A/" (uname-m) (uname%) (get-opt "as")) (homedir)))
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

(push `("ccl-bin" . (ccl-bin-version
                     ccl-bin-argv-parse
                     ccl-bin-download
                     ccl-bin-expand
                     setup))
      *install-cmds*)

(push `("ccl-bin" . ,(list 'ccl-bin-help)) *help-cmds*)
