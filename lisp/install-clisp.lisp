(in-package :ros.install)

(defun clisp-get-version ()
  (let ((file (merge-pathnames "tmp/clisp.html" (homedir))))
    (format *error-output* "Checking version to install....~%")
    (unless (and (probe-file file)
                 (< (get-universal-time) (+ (* 60 60) (file-write-date file))))
      (download (clisp-version-uri) file))
    (loop for link in (plump:get-elements-by-tag-name (plump:parse file) "a")
          for href = (plump:get-attribute link "href") for len = (1- (length href))
          when (and (eql (aref href len) #\/)
                    (not (eql (aref href 0) #\/))
                    (not (find #\: href))
                    (not (equal "latest/" href)))
            collect (subseq href 0 len))))

(defun clisp-version (argv)
  (let ((version (getf argv :version)))
    (when (or (null version) (equal version "latest"))
      (setf (getf argv :version) (first (clisp-get-version)))))
  (cons t argv))

(defun clisp-argv-parse (argv)
  (let ((pos (position "--as" (getf argv :argv) :test 'equal)))
    (set-opt "as" (or (and pos (ignore-errors (nth (1+ pos) (getf argv :argv)))
                           (format nil "~A-~A"
                                   (getf argv :version)
                                   (nth (1+ pos) (getf argv :argv))))
                      (getf argv :version))))
  (set-opt "download.uri" (format nil "~@{~A~}" (clisp-uri)
                                  (getf argv :version) "/clisp-"  (getf argv :version) ".tar.bz2"))
  (set-opt "download.archive" (let ((pos (position #\/ (get-opt "download.uri") :from-end t)))
                                (when pos
                                  (merge-pathnames (format nil "archives/~A" (subseq (get-opt "download.uri") (1+ pos))) (homedir)))))
  (set-opt "prefix" (merge-pathnames (format nil "impls/~A/~A/~A/~A/" (uname-m) (uname) (getf argv :target) (get-opt "as")) (homedir)))
  (set-opt "src" (merge-pathnames (format nil "src/~A-~A/" (getf argv :target) (getf argv :version)) (homedir)))
  (cons t argv))

(defun clisp-download (argv)
  (if (or (not (probe-file (get-opt "download.archive")))
          (get-opt "download.force"))
      (progn
        (format t "~&Downloading archive: ~A~%" (get-opt "download.uri"))
        (download (get-opt "download.uri") (get-opt "download.archive")))
      (format t "~&Skip downloading ~A.~%Specify 'download.force=t' to download it again.~%"
              (get-opt "download.uri")))
  (cons t argv))

(defun clisp-lib (argv)
  (when (and (find :linux *features*)
             (not (or (find :arm *features*))))
    (ros:roswell '("install ffcall+") :interactive nil))
  (ros:roswell '("install sigsegv+") :interactive nil)
  (cons t argv))

(defun clisp-expand (argv)
  (format t "~%Extracting archive: ~A~%" (get-opt "download.archive"))
  (expand (get-opt "download.archive")
          (merge-pathnames "src/" (homedir)))
  (cons t argv))

(defun clisp-patch (argv)
  #+darwin
  (let ((file (merge-pathnames "tmp/clisp.patch" (homedir)))
        (uri (clisp-patch1-uri)))
    (format t "~&Downloading patch: ~A~%" uri)
    (download uri file)
    (ros.util:chdir (get-opt "src"))
    (format t "~%Applying patch:~%")
    (uiop/run-program:run-program (format nil "git apply ~A" file) :output t))
  (cons t argv))

(defun clisp-config (argv)
  (format t "~&configure~%")
  (with-open-file (out (ensure-directories-exist
                        (merge-pathnames (format nil "impls/log/~A-~A/config.log"
                                                 (getf argv :target) (get-opt "as"))
                                         (homedir)))
                       :direction :output :if-exists :append :if-does-not-exist :create)
    (format out "~&--~&~A~%" (date))
    (let* ((src (get-opt "src"))
           (cmd (format nil "./configure --with-libsigsegv-prefix=~A ~A '--prefix=~A'"
                        (merge-pathnames (format nil "lib/~A/~A/~A/~A" (uname-m) (uname) "sigsegv" "2.10") (homedir))
                        (or #+linux(format nil "--with-libffcall-prefix=~A"
                                           (merge-pathnames (format nil "lib/~A/~A/~A/~A" (uname-m) (uname) "ffcall" "1.10") (homedir)))
                            "")
                        (get-opt "prefix")))
           (*standard-output* (make-broadcast-stream out #+sbcl(make-instance 'count-line-stream))))
      (ros.util:chdir src)
      (uiop/run-program:run-program cmd :output t :ignore-error-status t)))
  (cons t argv))

(defun clisp-make (argv)
  (format t "~&make~%")
  (with-open-file (out (ensure-directories-exist
                        (merge-pathnames (format nil "impls/log/~A-~A/make.log"
                                                 (getf argv :target) (get-opt "as"))
                                         (homedir)))
                       :direction :output :if-exists :append :if-does-not-exist :create)
    (format out "~&--~&~A~%" (date))
    (let* ((src (namestring (namestring (merge-pathnames "src/" (get-opt "src")))))
           (cmd (format nil "ulimit -s 16384 && make"))
           (*standard-output* (make-broadcast-stream out #+sbcl(make-instance 'count-line-stream))))
      (ros.util:chdir src)
      (uiop/run-program:run-program cmd :output t :ignore-error-status t)))
  (cons t argv))

(defun clisp-install (argv)
  (let* ((impl-path (get-opt "prefix"))
         (src (namestring (merge-pathnames "src/" (get-opt "src"))))
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

(defun clisp-clean (argv)
  (format t "~&Cleaning~%")
  (let ((src (namestring (merge-pathnames "src/" (get-opt "src")))))
    (ros.util:chdir src)
    (let* ((out (make-broadcast-stream))
           (*standard-output* (make-broadcast-stream
                               out #+sbcl(make-instance 'count-line-stream))))
      (uiop/run-program:run-program
       (list (sh) "-lc" (format nil "cd ~S;make clean" src)) :output t))
    (format t "done.~%"))
  (cons t argv))

(push `("clisp" . ,(list 'clisp-version
                         'clisp-argv-parse
                         'start
                         'clisp-download
                         'clisp-lib
                         'clisp-expand
                         'clisp-patch
                         'clisp-config
                         'clisp-make
                         'clisp-install
                         'clisp-clean
                         'setup))
      *install-cmds*)

(defun clisp-help (argv)
  (format t "no options for clisp~%")
  (cons t argv))

(push `("clisp" . ,(list 'clisp-help)) *help-cmds*)
(push `("clisp" . clisp-get-version) *list-cmd*)
