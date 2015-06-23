(in-package :ros.install)

(defun clisp-get-version ()
  (let (result (file (merge-pathnames "tmp/clisp.html" (homedir))))
    (format t "Checking version to install....~%")
    (unless (and (probe-file file)
                 (< (get-universal-time) (+ (* 60 60) (file-write-date file))))
      (download "http://ftp.gnu.org/pub/gnu/clisp/release/" file))
    (with-open-file (in file #+sbcl :external-format #+sbcl :utf-8)
      (with-output-to-string (*standard-output*)
        (funcall (intern (string :quickload) :ql)
                 :cl-html-parse))
      (funcall (read-from-string "net.html.parser:parse-html")
               in
               :callbacks
               (list (cons :a (lambda (arg)
                                (let* ((href (getf (cdr (car arg)) :href))
                                       (len (1- (length href))))
                                  (when (and (eql (aref href len) #\/)
                                             (not (eql (aref href 0) #\/))
                                             (not (find #\: href))
                                             (not (equal "latest/" href)))
                                    (push (subseq href 0 len) result))))))
               :callback-only t))
    result))

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
  (set-opt "download.uri" (format nil "~@{~A~}" "http://sourceforge.net/projects/clisp/files/clisp/"
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

(defun clisp-ffcall (argv)
  (let* ((uri "http://www.haible.de/bruno/gnu/ffcall-1.10.tar.gz")
         (pos (position #\/ uri :from-end t))
         (archive (merge-pathnames (format nil "archives/~A" (subseq uri (1+ pos))) (homedir))))
    (if (or (not (probe-file archive))
            (get-opt "download.force"))
        (progn
          (format t "~&Downloading archive: ~A~%" uri)
          (download uri archive))
        (format t "~&Skip downloading ~A.~%Specify 'download.force=t' to download it again.~%"
                uri)))
  (cons t argv))

(defun clisp-expand (argv)
  (format t "~%Extracting archive: ~A~%" (get-opt "download.archive"))
  (expand (get-opt "download.archive")
          (merge-pathnames "src/" (homedir)))
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
           (cmd (format nil "./configure --ignore-absence-of-libsigsegv '--prefix=~A'" (get-opt "prefix")))
           (*standard-output* (make-broadcast-stream out #+sbcl(make-instance 'count-line-stream))))
      (uiop/os:chdir src)
      (format t "~&chdir ~A~%" src)
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
           (cmd (format nil "make"))
           (*standard-output* (make-broadcast-stream out #+sbcl(make-instance 'count-line-stream))))
      (uiop/os:chdir src)
      (format t "~&chdir ~A~%" src)
      (uiop/run-program:run-program cmd :output t :ignore-error-status t)))
  (cons t argv))

(defun clisp-install (argv)
  (let* ((impl-path (get-opt "prefix"))
         (src (namestring (namestring (merge-pathnames "src/" (get-opt "src")))))
         (log-path (merge-pathnames (format nil "impls/log/~A-~A/install.log" (getf argv :target) (get-opt "as")) (homedir))))
    (format t "~&Installing ~A/~A..." (getf argv :target) (get-opt "as"))
    (format t "~&prefix: ~s~%" impl-path)
    (ensure-directories-exist impl-path)
    (ensure-directories-exist log-path)
    (uiop/os:chdir src)
    (format t "~&chdir ~A~%" src)
    (with-open-file (out log-path :direction :output :if-exists :append :if-does-not-exist :create)
      (format out "~&--~&~A~%" (date))
      (let ((*standard-output* (make-broadcast-stream
                                out #+sbcl(make-instance 'count-line-stream))))
        (uiop/run-program:run-program "make install" :output t)))
    (format *error-output* "done.~%"))
  (cons t argv))

(setq *install-cmds*
      (list 'clisp-version
            'clisp-argv-parse
            'start
            'clisp-download
            'clisp-ffcall
            'clisp-expand
            'clisp-config
            'clisp-make
            'clisp-install))

(setq *help-cmds*
      (list 'clisp-help))
