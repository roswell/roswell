(roswell:include "util-install-quicklisp")
(defpackage :roswell.install.abcl-bin
  (:use :cl :roswell.install :roswell.util :roswell.locations))
(in-package :roswell.install.abcl-bin)

(defvar *abcl-bin-get-version-cache* nil)

(defun abcl-bin-get-version ()
  (or *abcl-bin-get-version-cache*
      (setf *abcl-bin-get-version-cache*
            (let ((file (merge-pathnames "tmp/abcl-bin.html" (homedir))))
              (format *error-output* "Checking version to install....~%")
              (download (abcl-bin-uri) file)
              (loop for a in (plump:get-elements-by-tag-name
                              (plump:parse file) "a")
                    for x = (string-right-trim "/" (plump:get-attribute a "href"))
                    when (digit-char-p (aref x 0))
                    collect x)))))

(defun abcl-bin-impl ()
  (merge-pathnames (format nil "impls/~A/~A/abcl-bin/" (uname-m) (uname)) (homedir)))

(defun abcl-bin-argv-parse (argv)
  (when (position "--without-install" (getf argv :argv) :test 'equal)
    (set-opt "without-install" t))
  (set-opt "prefix" (abcl-bin-impl))
  (cons t argv))

(defun abcl-bin-download (argv)
  (set-opt "as" (version argv))
  (set-opt "download.uri" (format nil "~@{~A~}" (abcl-bin-uri)
                                  (version argv) "/abcl-bin-" (version argv)".tar.gz"))
  (set-opt "download.archive"
           (let ((pos (position #\/ (opt "download.uri") :from-end t)))
             (when pos
               (merge-pathnames (format nil "archives/~A" (subseq (opt "download.uri") (1+ pos))) (homedir)))))
  `((,(opt "download.archive") ,(opt "download.uri"))))

(defun abcl-bin-expand (argv)
  (format t "~%Extracting archive:~A~%" (opt "download.archive"))
  (expand
   (opt "download.archive")
   (ensure-directories-exist (abcl-bin-impl)))
  (let ((path (merge-pathnames (format nil "~A/" (opt "as")) (abcl-bin-impl))))
    (and (probe-file path)
         (uiop/filesystem:delete-directory-tree
          path :validate t)))
  (ql-impl-util:rename-directory
   (merge-pathnames (format nil "abcl-bin-~A/" (version argv)) (abcl-bin-impl))
   (merge-pathnames (format nil "~A/" (opt "as")) (abcl-bin-impl)))
  (cons t argv))

(defun abcl-bin-script (argv)
  (let ((java (which "java"))
        (dir (merge-pathnames (format nil "~A/" (opt "as")) (abcl-bin-impl))))
    (unless java
      (format *error-output* "Error: JAVA wasn't found in the path. 'ros use abcl' will fail.~%")
      (format *error-output* "Installation incomplete.")
      (roswell:quit 1))
    (install-script
     (merge-pathnames "abcl" dir)
     (format
      nil
      "exec ~A -Xmx4g -cp \"~Aabcl-contrib.jar\" -jar \"~:*~Aabcl.jar\" \"\$@\""
      java dir))
    (cons t argv)))

(defun abcl-bin (type)
  (case type
    #+nil(:help '(abcl-bin-help))
    (:install `(,(decide-version 'abcl-bin-get-version)
                abcl-bin-argv-parse
                ,(decide-download 'abcl-bin-download)
                abcl-bin-expand
                abcl-bin-script
                setup))
    #+nil(:list 'abcl-bin-get-version)))
