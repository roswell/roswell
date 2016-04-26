(in-package :ros.install)
(ros:quicklisp :environment nil)

#+win32
(ql:quickload :zip :silent t)

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

(defun ccl-bin-version (argv)
  (let ((version (getf argv :version)))
    (when (or (null version) (equal version "latest"))
      (setf (getf argv :version) (first (ccl-bin-get-version)))))
  (cons t argv))

(defvar *ccl-uname-m-alist*
  '(("x86-64" . "x86")
    ("armhf" . "arm")))

(defun ccl-uname-m ()
  (or (cdr (assoc (uname-m) *ccl-uname-m-alist* :test 'equal))
      (uname-m)))

(defun ccl-bin-argv-parse (argv)
  (let ((uname (uname))
        (ccl-uname-m (ccl-uname-m)))
    (format *error-output* "~&Installing ccl-bin/~A...~%" (getf argv :version))
    (set-opt "as" (getf argv :version))
    (when (position "--without-install" (getf argv :argv) :test 'equal)
      (set-opt "without-install" t))
    (set-opt "download.uri" (format nil "~@{~A~}" (ccl-bin-uri)
                                    (getf argv :version) "/ccl-" (getf argv :version) "-" uname ccl-uname-m (if (equal uname "windows")
                                                                                                                ".zip"".tar.gz")))
    (set-opt "download.archive" (let ((pos (position #\/ (get-opt "download.uri") :from-end t)))
                                  (when pos 
                                    (merge-pathnames (format nil "archives/~A" (subseq (get-opt "download.uri") (1+ pos))) (homedir)))))
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
  (let* ((impls (merge-pathnames (format nil "impls/~A/~A/ccl-bin/" (uname-m) (uname)) (homedir)))
         (path (merge-pathnames (format nil "~A/" (get-opt "as")) impls)))
    (#-win32 expand #+win32 zip:unzip (get-opt "download.archive") (ensure-directories-exist impls))
    (and (probe-file path)
         (uiop/filesystem:delete-directory-tree 
          path :validate t))
    (ql-impl-util:rename-directory
     (merge-pathnames (format nil "ccl/") impls)
     (merge-pathnames (format nil "~A/" (get-opt "as")) impls)))
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
(push `("ccl-bin" . ccl-bin-get-version) *list-cmd*)
