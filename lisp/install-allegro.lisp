(in-package :ros.install)
(ros:quicklisp :environment nil)

(defun allegro-get-version ()
  (list "100express"))

(defun allegro-version (argv)
  (let ((version (getf argv :version)))
    (when (or (null version) (equal version "latest"))
      (setf (getf argv :version) (first (allegro-get-version))
            (getf argv :version-not-specified) 0)))
  (cons t argv))

(defvar *allegro-uname-m-alist*
  '(("x86-64" . "x86")))

(defvar *allegro-uname-alist*
  '(("linux" . "linu")
    ("darwin" . "macos")))

(defun allegro-uname-m ()
  (or (cdr (assoc (uname-m) *allegro-uname-m-alist* :test 'equal))
      (uname-m)))

(defun allegro-uname ()
  (or (cdr (assoc (uname) *allegro-uname-alist* :test 'equal))
      (uname)))

(defun allegro-argv-parse (argv)
  (format *error-output* "~&Installing allegro/~A...~%" (getf argv :version))
  (set-opt "as" (getf argv :version))
  (cons t argv))

(defun allegro-download (argv)
  (let ((uname (allegro-uname))
        (uname-m (allegro-uname-m)))
    (set-opt "download.uri" (format nil "~@{~A~}" (allegro-uri)
                                    "/acl" (getf argv :version) "/" uname uname-m "/acl" (getf argv :version)
                                    "-" uname "x-" uname-m (cond ((equal uname "macos") ".dmg")
                                                                 ((equal uname "linu") ".bz2"))))
    (set-opt "download.archive" (let ((pos (position #\/ (get-opt "download.uri") :from-end t)))
                                  (when pos 
                                    (merge-pathnames (format nil "archives/~A" (subseq (get-opt "download.uri") (1+ pos))) (homedir))))))
  (if (or (not (probe-file (get-opt "download.archive")))
          (get-opt "download.force"))
      (progn
        (format t "~&Downloading archive:~A~%" (get-opt "download.uri"))
        (download (get-opt "download.uri") (get-opt "download.archive")))
      (format t "~&Skip downloading ~A~%specify download.force=t to download it again.~%"
              (get-opt "download.uri")))
  (cons t argv))

(defun allegro-expand (argv)
  (format t "~%Extracting archive:~A~%" (get-opt "download.archive"))
  (let* ((impls (merge-pathnames (format nil "impls/~A/~A/allegro/" (uname-m) (uname)) (homedir)))
         (path (merge-pathnames (format nil "~A/" (get-opt "as")) impls))
         (uname (allegro-uname)))
    (cond
      ((equal uname "macos")
       (let ((mount-dir (string-right-trim
                         (format nil "~%")
                         (uiop:run-program
                          (format nil "hdiutil attach ~A | awk -F '\t' 'END{print $NF}'" (get-opt "download.archive"))
                          :output :string))))
         (uiop:run-program (format nil "cp -r ~A/AllegroCLexpress.app/Contents/Resources/ ~A"
                                   mount-dir
                                   (ensure-directories-exist (merge-pathnames (format nil "~A/" (get-opt "as")) impls))))
         (uiop:run-program (format nil "hdiutil detach \"~A\"" mount-dir))))
      (t
       (expand (get-opt "download.archive") (ensure-directories-exist impls))
       (and (probe-file path)
            (uiop/filesystem:delete-directory-tree 
             path :validate t))
       (ql-impl-util:rename-directory
        (merge-pathnames (format nil "acl~A/" (getf argv :version)) impls)
        (merge-pathnames (format nil "~A/" (get-opt "as")) impls))))
    (cons t argv)))

(defun allegro-help (argv)
  (format t "allegro install options~%")
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

(push `("allegro" . (allegro-version
                     allegro-argv-parse
                     allegro-download
                     allegro-expand
                     setup))
      *install-cmds*)

(push `("allegro" . ,(list 'allegro-help)) *help-cmds*)
(push `("allegro" . allegro-get-version) *list-cmd*)
