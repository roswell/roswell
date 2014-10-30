(in-package :ros.install)

(defclass count-line-stream (sb-gray:fundamental-character-output-stream)
  ((base :initarg :base 
         :initform *standard-output*
         :reader count-line-stream-base)
   (print-char :initarg :print-char
               :initform `((900 . line-number)(10 . #\.))
               :accessor count-line-stream-print-char)
   (count-char :initarg :count-char
                :initform #\NewLine
                :reader count-line-stream-count-char)
   (count :initform -1
          :accessor count-line-stream-count)))

(defmethod sb-gray:stream-write-char ((stream count-line-stream) character)
  (when (char= character (count-line-stream-count-char stream))
    (loop
       :with count := (incf (count-line-stream-count stream))
       :with stream- := (count-line-stream-base stream)
       :for (mod . char) :in (count-line-stream-print-char stream)
       :when (zerop (mod count mod))
       :do (if (characterp char)
               (write-char char stream-)
               (funcall char stream))
       (force-output stream-))))

(defun line-number (stream)
  (format (count-line-stream-base stream) "~&~8d " (count-line-stream-count stream)))

(defun sbcl-version (argv)
  (let ((version (getf argv :version)))
    (when (or (null version) (equal version "latest"))
      ;;TBD
      (setf (getf argv :version) "1.2.4")))
  (set-opt "download.uri" (format nil "~@{~A~}" "http://sourceforge.net/projects/sbcl/files/sbcl/" 
                                  (getf argv :version) "/sbcl-" (getf argv :version) "-source.tar.bz2"))
  (set-opt "download.archive" (let ((pos (position #\/ (get-opt "download.uri") :from-end t)))
                                (when pos 
                                  (merge-pathnames (format nil "archives/~A" (subseq (get-opt "download.uri") (1+ pos))) (homedir)))))
  (set-opt "prefix" (merge-pathnames (format nil "impls/~A/~A/~A/~A/" (uname-m) (uname) (getf argv :target) (getf argv :version)) (homedir)))
  (set-opt "src" (merge-pathnames (format nil "src/~A-~A/" (getf argv :target) (getf argv :version)) (homedir)))
  (cons t argv))

(defun sbcl-start (argv)
  (when (and (equal (getf argv :target) "sbcl")
             (not (get-opt "sbcl.compiler")))
    (format t "compiler variable 'sbcl.compiler'.assume it as 'sbcl-bin'~%")
    (set-opt "sbcl.compiler" "sbcl-bin"))
  (cons t argv))

(defun sbcl-download (argv)
  (when (get-opt "download.skip")
    (format t "~&Skip downloading ~A~%" (get-opt "download.uri"))
    (return-from sbcl-download
      (cons t argv)))
  (format t "~&Downloading archive.:~A~%" (get-opt "download.uri"))
  ;;TBD proxy support... and other params progress bar?
  (download (get-opt "download.uri") (get-opt "download.archive"))
  (cons t argv))

(defun sbcl-expand (argv)
  (format t "~&Extracting archive.:~A~%" (get-opt "download.archive"))
  (expand (get-opt "download.archive")
          (merge-pathnames "src/" (homedir)))
  (cons t argv))

(defun sbcl-make (argv)
  (with-open-file (out (ensure-directories-exist
                        (merge-pathnames (format nil "impls/log/~A-~A/make.log"
                                                 (getf argv :target) (getf argv :version))
                                         (homedir)))
                       :direction :output :if-exists :append :if-does-not-exist :create)
    (format out "~A~%" (date))
    (let* ((src (get-opt "src"))
           (compiler (format nil "~A lisp=~A --no-rc run --" *ros-path* (get-opt "sbcl.compiler")))
           (cmd (format nil "sh make.sh \"--xc-host=~A\" \"--prefix=~A\"" compiler (get-opt "prefix")))
           (*standard-output* (make-broadcast-stream out (make-instance 'count-line-stream))))
      (uiop/os:chdir src)
      (uiop/run-program:run-program cmd :output t)))
  (cons t argv))

(defun sbcl-install (argv)
  (format t "~&Installing~%")
  (let* ((impl-path (get-opt "prefix"))
         (src (get-opt "src"))
         (install-root impl-path)
         (log-path (merge-pathnames (format nil "impls/log/~A-~A/install.log" (getf argv :target) (getf argv :version)) (homedir))))
    (format t "~&prefix :~s~%" impl-path)
    (format t "~&installing ~A/~A" (getf argv :target) (getf argv :version))
    (ensure-directories-exist impl-path)
    (ensure-directories-exist log-path)
    (uiop/os:chdir src)
    (format t "chdir ~A" src)
    (unsetenv "SBCL_HOME")
    (setenv "INSTALL_ROOT" (format nil "~A" install-root))
    (with-open-file (out log-path :direction :output :if-exists :append :if-does-not-exist :create)
      (format out "~A~%" (date))
      (let ((*standard-output* (make-broadcast-stream
                                out
                                (make-instance 'count-line-stream))))
        (uiop/run-program:run-program "sh install.sh" :output t)))
    (format t"done.~%"))
  (cons t argv))

(setq *install-cmds*
      (list 'sbcl-version
            'sbcl-start
            'start
            'sbcl-download
            'sbcl-expand
            'sbcl-make
            'sbcl-install
            'setup))
