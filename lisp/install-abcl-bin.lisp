
(in-package :ros.install)
(ql:quickload '(:plump :simple-date-time :split-sequence :cl-ppcre) :silent t)

(defun abcl-bin-get-version ()
  (let ((file (merge-pathnames "tmp/abcl-bin.html" (homedir))))
    (format *error-output* "Checking version to install....~%")
    (download (abcl-bin-uri) file)
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

(defun abcl-bin-impl ()
  (merge-pathnames (format nil "impls/~A/~A/abcl-bin/" (uname-m) (uname)) (homedir)))

(defun abcl-bin-argv-parse (argv)
  (set-opt "as" (getf argv :version))
  (when (position "--without-install" (getf argv :argv) :test 'equal)
    (set-opt "without-install" t))
  (set-opt "download.uri" (format nil "~@{~A~}" (abcl-bin-uri)
                                  (getf argv :version) "/abcl-bin-" (getf argv :version)".tar.gz"))
  (set-opt "download.archive" (let ((pos (position #\/ (get-opt "download.uri") :from-end t)))
                                (when pos 
                                  (merge-pathnames (format nil "archives/~A" (subseq (get-opt "download.uri") (1+ pos))) (homedir)))))
  (set-opt "prefix" (abcl-bin-impl))
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
   (ensure-directories-exist (abcl-bin-impl)))
  (let ((path (merge-pathnames (format nil "~A/" (get-opt "as")) (abcl-bin-impl))))
    (and (probe-file path)
         (uiop/filesystem:delete-directory-tree 
          path :validate t)))
  (ql-impl-util:rename-directory
   (merge-pathnames (format nil "abcl-bin-~A/" (getf argv :version)) (abcl-bin-impl))
   (merge-pathnames (format nil "~A/" (get-opt "as")) (abcl-bin-impl)))
  (cons t argv))

(defun abcl-bin-script (argv)
  (cons
   (let ((java (ros.util:which "java"))
         (dir (merge-pathnames (format nil "~A/" (get-opt "as")) (abcl-bin-impl))))
     (when (and java)
       (install-script
        (merge-pathnames "abcl" dir)
        (format
         nil
         (if (not (zerop (length
                          (remove-if-not
                           (lambda ($)
                             (and (cl-ppcre:scan "version" $)
                                  (cl-ppcre:scan "1\\.8" $)))
                           (split-sequence:split-sequence
                            #\Newline
                            (nth-value 1(uiop:run-program "java -version"  :error-output :string)))))))
             "exec ~A -Xmx4g -cp \"~Aabcl-contrib.jar\" -jar \"~:*~Aabcl.jar\" \"\$@\""
             "exec ~A -Xmx4g -XX:MaxPermSize=1g -cp \"~Aabcl-contrib.jar\" -jar \"~:*~Aabcl.jar\" \"\$@\"")
         java dir)))
     t)
   argv))

(push `("abcl-bin" . (abcl-bin-version
                      abcl-bin-argv-parse
                      abcl-bin-download
                      abcl-bin-expand
                      abcl-bin-script
                      setup))
      *install-cmds*)
