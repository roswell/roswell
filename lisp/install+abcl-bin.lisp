
(in-package :ros.install)
(ql:quickload '(:plump :simple-date-time :split-sequence) :silent t)

(defun abcl-bin-get-version ()
  (let ((file (merge-pathnames "tmp/abcl-bin.html" (homedir))))
    (format *error-output* "Checking version to install....~%")
    (download "https://common-lisp.net/project/armedbear/releases/" file)
    (loop for a in (plump:get-elements-by-tag-name
                    (plump:parse file) "a")
       for x = (string-right-trim "/" (plump:get-attribute a "href"))
       when (digit-char-p (aref x 0))
       collect x)))

(defun abcl-bin-version (argv)
  (let ((version (getf argv :version)))
    (when (or (null version) (equal version "latest"))
      (setf (getf argv :version) (first (abcl-bin-get-version)))))
  (cons t argv))

#+nil("1.3.3" "1.3.2" "1.3.1" "1.3.0" "1.2.1" "1.2.0" "1.1.1" "1.1.0" "1.1.0.2"
         "1.1.0.1" "1.0.1" "1.0.0" "0.27.0" "0.26.2" "0.26.1" "0.25.0" "0.24.0"
         "0.23.1")
;;"https://common-lisp.net/project/armedbear/releases/1.3.3/abcl-bin-1.3.3.tar.gz"

(defun abcl-bin-argv-parse (argv)
  (set-opt "as" (getf argv :version))
  (when (position "--without-install" (getf argv :argv) :test 'equal)
    (set-opt "without-install" t))
  (set-opt "download.uri" (format nil "~@{~A~}" "https://common-lisp.net/project/armedbear/releases/"
                                  (getf argv :version) "/abcl-bin-" (getf argv :version)".tar.gz"))
  (set-opt "download.archive" (let ((pos (position #\/ (get-opt "download.uri") :from-end t)))
                                (when pos 
                                  (merge-pathnames (format nil "archives/~A" (subseq (get-opt "download.uri") (1+ pos))) (homedir)))))
  (set-opt "prefix" (merge-pathnames (format nil "impls/~A/~A/~A/~A/" (uname-m) (uname) (getf argv :target) (get-opt "as")) (homedir)))
  (set-opt "src" (merge-pathnames (format nil "src/~A-~A/" (getf argv :target) (getf argv :version)) (homedir)))
  (cons t argv))

(defun abcl-bin-download (argv)
  (if (or (not (probe-file (get-opt "download.archive")))
          (get-opt "download.force"))
      (progn
        (format t "~&Downloading archive:~A~%" (get-opt "download.uri"))
        ;;TBD proxy support... and other params progress bar?
        (download (get-opt "download.uri") (get-opt "download.archive")))
      (format t "~&Skip downloading ~A~%specify download.force=t to download it again.~%"
              (get-opt "download.uri")))
  (cons (not (get-opt "without-install")) argv))

(defun abcl-bin-expand (argv)
  (format t "~%Extracting archive:~A~%" (get-opt "download.archive"))
  (expand 
   (get-opt "download.archive")
   (ensure-directories-exist (merge-pathnames (format nil "impls/~A/~A/abcl-bin/" (uname-m) (uname)) (homedir))))
  (let ((path (merge-pathnames (format nil "impls/~A/~A/abcl-bin/~A/" (uname-m) (uname) (get-opt "as")) (homedir))))
    (and (probe-file path)
         (uiop/filesystem:delete-directory-tree 
          path :validate t)))
  (ql-impl-util:rename-directory
   (merge-pathnames (format nil "impls/~A/~A/abcl-bin/abcl-bin-~A/" (uname-m) (uname) (getf argv :version)) (homedir))
   (merge-pathnames (format nil "impls/~A/~A/abcl-bin/~A/" (uname-m) (uname) (get-opt "as")) (homedir)))
  (cons t argv))

(push `("abcl-bin" . (abcl-bin-version
                      abcl-bin-argv-parse
                      abcl-bin-download
                      abcl-bin-expand
                      setup))
      *install-cmds*)
