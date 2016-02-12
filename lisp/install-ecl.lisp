(in-package :ros.install)
(ql:quickload '(:plump :simple-date-time :split-sequence) :silent t)

(defvar *ecl-options*
  '())

(defun parse-date (str)
  (setq str (string-trim " " str)
        str (substitute #\: #\- str)
        str (substitute #\: #\space str))
  (setq str (split-sequence:split-sequence #\: str))
  (make-instance 'simple-date-time:date-time
                 :day (parse-integer (first str))
                 :month (simple-date-time::from-short-month-name (second str))
                 :year (parse-integer (third str))
                 :hour (parse-integer (fourth str))
                 :minute (parse-integer (fifth str))
                 :second 0))
;;(parse-date "22-Aug-2015 18:19  ")

(defun ecl-get-version ()
  (let ((file (merge-pathnames "tmp/ecl.html" (homedir))))
    (format *error-output* "Checking version to install....~%")
    (download "https://common-lisp.net/project/ecl/files/" file)
    (with-output-to-string (*standard-output*)
      (funcall (intern (string :quickload) :ql)
               :plump))
    (let* ((the-newest "current-release.tgz")
           (tgz-list
            (sort
             (delete nil
                     (mapcar (lambda (x)
                               (let* ((str (plump:get-attribute x "href"))
                                      (len (length str)))
                                 (when (equal (subseq str (if (plusp (- len 3))
                                                              (- len 3)
                                                              0))
                                              "tgz")
                                   (cons str
                                         (parse-date (plump:text
                                                      (plump:next-element
                                                       (plump:parent x))))))))
                             (plump:get-elements-by-tag-name
                              (plump:parse (merge-pathnames "tmp/ecl.html" (homedir))) "a")))
             'simple-date-time:date-time> :key 'cdr))
           timestamp)
      (setq tgz-list (delete-if (lambda (x)
                                  (when (equal (first x) the-newest)
                                    (setq timestamp (cdr x)))
                                  (not (eql (mismatch "ecl" (first x)) 3))) tgz-list))
      (when timestamp
        (let (find)
          (setq tgz-list (delete-if (lambda (x)
                                      (when (simple-date-time:date-time= (cdr x) timestamp)
                                        (setq find x)))
                                    tgz-list))
          (when find
            (push find tgz-list))))
      (mapcar (lambda (x)
                (let ((str (car x)))
                  (subseq str 4 (- (length str) 4))))
              tgz-list))))

(defun ecl-msys (argv)
  (unless (ros:getenv "MSYSCON")
    (ros:roswell '("install msys2") :interactive nil))
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
  (set-opt "download.uri" (format nil "~@{~A~}" "https://common-lisp.net/project/ecl/files/ecl-"
                                  (getf argv :version) ".tgz"))
  (set-opt "download.archive" (let ((pos (position #\/ (get-opt "download.uri") :from-end t)))
                                (when pos
                                  (merge-pathnames (format nil "archives/~A" (subseq (get-opt "download.uri") (1+ pos))) (homedir)))))
  (set-opt "prefix" (merge-pathnames (format nil "impls/~A/~A/~A/~A/" (uname-m) (uname) (getf argv :target) (get-opt "as")) (homedir)))
  (set-opt "src" (merge-pathnames (format nil "src/~A-~A/" (getf argv :target) (getf argv :version)) (homedir)))
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
