(in-package :ros.install)
(ql:quickload '(:plump :simple-date-time :split-sequence) :silent t)

(defvar *ecl-options*
  '())

(defun ecl-get-version ()
  (format *error-output* "Checking version to install....~%")
  (github-version *ecl-git-version-uri* "ecl"
                  (lambda (href)
                    (let ((a (subseq href (1+ (position #\/ href :from-end t)))))
                      (subseq a (position-if #'digit-char-p a))))))

(defun ecl-version-filename (version)
  (find-if (lambda (x) (ignore-errors (equal (subseq x (- (length x) (length version))) version)))
           (github-version *ecl-git-version-uri* "ecl"
                           (lambda (href) (subseq href (1+ (position #\/ href :from-end t)))))))

(defun ecl-msys (argv)
  (unless (ros:getenv "MSYSCON")
    (ros:roswell '("install msys2+") :interactive nil))
  (cons t argv))

(defun ecl-version (argv)
  (let ((version (getf argv :version)))
    (when (or (null version) (equal version "latest"))
      (setf (getf argv :version) (first (ecl-get-version)))))
  (cons t argv))

(defun ecl-argv-parse (argv)
  (let ((pos (position "--as" (getf argv :argv) :test 'equal)))
    (set-opt "as" (or (and pos (ignore-errors (nth (1+ pos) (getf argv :argv)))
                           (format nil "~A-~A"
                                   (getf argv :version)
                                   (nth (1+ pos) (getf argv :argv))))
                      (getf argv :version))))
  (when (position "--archive" (getf argv :argv) :test 'equal)
    (set-opt "install.force" "t")
    (set-opt "archive" "t"))
  (when (position "--without-install" (getf argv :argv) :test 'equal)
    (set-opt "without-install" t))
  (set-opt "download.uri" (format nil "~A~A.tar.gz" *ecl-uri* (ecl-version-filename (getf argv :version))))
  (set-opt "download.archive" (let ((pos (position #\/ (get-opt "download.uri") :from-end t)))
                                (when pos
                                  (merge-pathnames (format nil "archives/~A" (subseq (get-opt "download.uri") (1+ pos))) (homedir)))))
  (set-opt "prefix" (merge-pathnames (format nil "impls/~A/~A/~A/~A/" (uname-m) (uname) (getf argv :target) (get-opt "as")) (homedir)))
  (print (format nil "src/mirror-ecl-~A/" (ecl-version-filename (getf argv :version))))
  (set-opt "src" (merge-pathnames (format nil "src/mirror-ecl-~A/" (ecl-version-filename (getf argv :version))) (homedir)))
  (labels ((with (opt default)
             (set-opt opt
                      (cond ((position (format nil "--with-~A" opt) (getf argv :argv) :test 'equal) t)
                            ((position (format nil "--without-~A" opt) (getf argv :argv) :test 'equal) nil)
                            (t default)))))
    (loop for (opt default . nil) in *ecl-options*
       do (with opt default)))
  (cons t argv))

(defun ecl-download (argv)
  (if (or (not (probe-file (get-opt "download.archive")))
          (get-opt "download.force"))
      (progn
        (format t "~&Downloading archive: ~A~%" (get-opt "download.uri"))
        (download (get-opt "download.uri") (get-opt "download.archive")))
      (format t "~&Skip downloading ~A.~%Specify 'download.force=t' to download again.~%"
              (get-opt "download.uri")))
  (cons (not (get-opt "without-install")) argv))

(defun ecl-expand (argv)
  (format t "~%Extracting archive:~A~%" (get-opt "download.archive"))
  (expand (get-opt "download.archive")
          (merge-pathnames "src/" (homedir)))
  (cons t argv))

(defun ecl-config (argv)
  (format t "~&configure~%")
  (with-open-file (out (ensure-directories-exist
                        (merge-pathnames (format nil "impls/log/~A-~A/config.log"
                                                 (getf argv :target) (get-opt "as"))
                                         (homedir)))
                       :direction :output :if-exists :append :if-does-not-exist :create)
    (format out "~&--~&~A~%" (date))
    (let* ((src (get-opt "src"))
           (cmd (format nil "./configure '--prefix=~A'" (get-opt "prefix")))
           (*standard-output* (make-broadcast-stream out #+sbcl(make-instance 'count-line-stream))))
      (ros.util:chdir src)
      (uiop/run-program:run-program cmd :output t :ignore-error-status t)))
  (cons t argv))

(defun ecl-make (argv)
  (format t "~&make~%")
  (with-open-file (out (ensure-directories-exist
                        (merge-pathnames (format nil "impls/log/~A-~A/make.log"
                                                 (getf argv :target) (get-opt "as"))
                                         (homedir)))
                       :direction :output :if-exists :append :if-does-not-exist :create)
    (format out "~&--~&~A~%" (date))
    (let* ((src (namestring (get-opt "src")))
           (cmd (format nil "make"))
           (*standard-output* (make-broadcast-stream out #+sbcl(make-instance 'count-line-stream))))
      (ros.util:chdir src)
      (uiop/run-program:run-program cmd :output t :ignore-error-status t)))
  (cons t argv))

(defun ecl-install (argv)
  (let* ((impl-path (get-opt "prefix"))
         (src (namestring (namestring (get-opt "src"))))
         (log-path (merge-pathnames (format nil "impls/log/~A-~A/install.log" (getf argv :target) (get-opt "as")) (homedir))))
    (format t "~&Installing ~A/~A..." (getf argv :target) (get-opt "as"))
    (format t "~&prefix: ~s~%" impl-path)
    (ensure-directories-exist impl-path)
    (ensure-directories-exist log-path)
    (ros.util:chdir src)
    (with-open-file (out log-path :direction :output :if-exists :append :if-does-not-exist :create)
      (format out "~&--~&~A~%" (date))
      (let ((*standard-output* (make-broadcast-stream
                                out #+sbcl(make-instance 'count-line-stream))))
        (uiop/run-program:run-program "make install" :output t)))
    (format *error-output* "done.~%"))
  (cons t argv))

(defun ecl-clean (argv)
  (format t "~&Cleaning~%")
  (let ((src (get-opt "src")))
    (ros.util:chdir src nil)
    (let* ((out (make-broadcast-stream))
           (*standard-output* (make-broadcast-stream
                               out #+sbcl(make-instance 'count-line-stream))))
      (uiop/run-program:run-program
       (list (sh) "-lc" (format nil "cd ~S;make clean" src)) :output t))
    (format t "done.~%"))
  (cons t argv))

(push `("ecl" . ,(list #+win32 ecl-msys
                       'ecl-version
                       'ecl-argv-parse
                       'start
                       'ecl-download
                       'ecl-expand
                       'ecl-config
                       'ecl-make
                       'ecl-install
                       'ecl-clean
                       'setup
                       ))
      *install-cmds*)
