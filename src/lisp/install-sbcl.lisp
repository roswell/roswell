(in-package :ros.install)

(defvar *sbcl-options*
  '(("thread" t "Build SBCL without support for native threads")
    ("core-compression" t "Build SBCL without support for compressed cores and without a dependency on zlib")
    ("ldb" nil "Include low-level debugger in the build")
    ("xref-for-internals" nil "Include XREF information for SBCL internals (increases core size by 5-6MB)")
    ("simd-pack" nil "Enable SIMD intrinsics")))

(defun sbcl-get-version ()
  (let (result
        (file (merge-pathnames "tmp/sbcl.html" (homedir))))
    (format *error-output* "Checking version to install....~%")
    (unless (and (probe-file file)
                 (< (get-universal-time) (+ (* 60 60) (file-write-date file))))
      (download "https://github.com/sbcl/sbcl/releases.atom" file))
    (with-open-file (in file #+sbcl :external-format #+sbcl :utf-8)
      (net.html.parser:parse-html
       in
       :callbacks
       (list (cons :link (lambda (arg)
                           (let* ((href (getf (cdr (car arg)) :href))
                                  (pos (position #\- href)))
                             (when pos
                               (push (subseq href (1+ pos)) result))))))
       :callback-only t))
    (setq result (nreverse result))
    result))

(defun sbcl-msys (argv)
  (ros:roswell '("install msys2") :interactive nil)
  (cons t argv))

(defun sbcl-version (argv)
  (let ((version (getf argv :version)))
    (when (or (null version) (equal version "latest"))
      (setf (getf argv :version) (first (sbcl-get-version)))))
  (cons t argv))

(defun sbcl-argv-parse (argv)
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
  (set-opt "download.uri" (format nil "~@{~A~}" "https://github.com/sbcl/sbcl/archive/sbcl-"
                                  (getf argv :version) ".tar.gz"))
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
    (loop for (opt default . nil) in *sbcl-options*
       do (with opt default)))
  (cons t argv))

(defun sbcl-start (argv)
  (when (and (equal (getf argv :target) "sbcl")
             (not (get-opt "sbcl.compiler")))
    (format t "Using 'sbcl-bin' to compile SBCL. (default)~%")
    (set-opt "sbcl.compiler" "sbcl-bin"))
  (cons t argv))

(defun sbcl-download (argv)
  (if (or (not (probe-file (get-opt "download.archive")))
          (get-opt "download.force"))
      (progn
        (format t "~&Downloading archive: ~A~%" (get-opt "download.uri"))
        (download (get-opt "download.uri") (get-opt "download.archive")))
      (format t "~&Skip downloading ~A.~%Specify 'download.force=t' to download again.~%"
              (get-opt "download.uri")))
  (cons (not (get-opt "without-install")) argv))

(defun sbcl-expand (argv)
  (format t "~%Extracting archive:~A~%" (get-opt "download.archive"))
  (expand (get-opt "download.archive")
          (merge-pathnames "src/" (homedir)))
  (ignore-errors
    (let ((h (homedir))
          (v (getf argv :version)))
      (ql-impl-util:rename-directory
       (merge-pathnames (format nil "src/sbcl-sbcl-~A" v) h)
       (merge-pathnames (format nil "src/sbcl-~A" v) h))
      (with-open-file (o (merge-pathnames (format nil "src/sbcl-~A/version.lisp-expr" v) h)
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists nil)
        (format o "~S~%" v))))
  (cons t argv))

(defun sbcl-patch (argv)
  #+darwin
  (let ((file (merge-pathnames "tmp/sbcl.patch" (homedir)))
        (uri "https://gist.githubusercontent.com/snmsts/e8e4fd4bd5e458ac45e8/raw/bb7f1cd2e8e9a914f4e9b1b5acf889ecf75dfe0c/posix-tests.patch"))
    (format t "~&Downloading patch: ~A~%" uri)
    (download uri file)
    (uiop/os:chdir (get-opt "src"))
    (format t "~&chdir ~A~%" (get-opt "src"))
    (format t "~%Applying patch:~%")
    (uiop/run-program:run-program "patch -p0 -N" :output t :input file :ignore-error-status t))
  (cons t argv))

(defun sbcl-config (argv)
  (with-open-file (out (ensure-directories-exist
                        (merge-pathnames
                         (format nil "src/sbcl-~A/customize-target-features.lisp"
                                 (getf argv :version)) (homedir)))
                       :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format out "~s"
            `(lambda (list)
               (dolist (i ',(loop for (name . nil) in *sbcl-options*
                               collect (list (read-from-string (format nil ":sb-~A" name)) (get-opt name))))
                 (if (second i)
                     (pushnew (first i) list)
                     (setf list (remove (first i) list))))
               list)))
  (cons t argv))

(defun sbcl-make (argv)
  (with-open-file (out (ensure-directories-exist
                        (merge-pathnames (format nil "impls/log/~A-~A/make.log"
                                                 (getf argv :target) (get-opt "as"))
                                         (homedir)))
                       :direction :output :if-exists :append :if-does-not-exist :create)
    (format out "~&--~&~A~%" (date))
    (let* ((src (get-opt "src"))
           (compiler (format nil "~A lisp=~A --no-rc run -- --disable-debugger" *ros-path* (get-opt "sbcl.compiler")))
           (cmd (list (sh) "-lc" (format nil "cd ~S;~A ~A ~A ~A"
                                         (#+win32 mingw-namestring #-win32 princ-to-string src)
                                         (or #-win32 (sh) "")
                                         "./make.sh" (format nil "'--xc-host=~A'"  compiler)
                                         (format nil "'--prefix=~A'"
                                                 (funcall #+win32 (lambda (x)
                                                                    (mingw-namestring (ensure-directories-exist x)))
                                                          #-win32 'identity
                                                          (get-opt "prefix"))))))
           (*standard-output* (make-broadcast-stream out #+sbcl(make-instance 'count-line-stream))))
      (uiop/os:chdir src)
      (format t "~&chdir ~A~%" src)
      (format t "~&~S~%" cmd)
      (uiop/run-program:run-program cmd :output t :ignore-error-status nil)))
  (cons t argv))

(defun sbcl-install (argv)
  (let* ((impl-path (get-opt "prefix"))
         (src (get-opt "src"))
         (install-root impl-path)
         (log-path (merge-pathnames (format nil "impls/log/~A-~A/install.log" (getf argv :target) (get-opt "as")) (homedir))))
    (unless (get-opt "archive")
      (format t "~&Installing ~A/~A" (getf argv :target) (get-opt "as"))
      (format t "~&prefix: ~s~%" impl-path)
      (ensure-directories-exist impl-path)
      (ensure-directories-exist log-path)
      (uiop/os:chdir src)
      (format t "~&chdir ~A~%" src)
      (ros:unsetenv "SBCL_HOME")
      (ros:setenv "INSTALL_ROOT" (format nil "~A" install-root))
      (with-open-file (out log-path :direction :output :if-exists :append :if-does-not-exist :create)
        (format out "~&--~&~A~%" (date))
        (let ((*standard-output* (make-broadcast-stream
                                  out #+sbcl(make-instance 'count-line-stream))))
          (uiop/run-program:run-program
           (list (sh) "-lc" (format nil "cd ~S;~A"
                                    (#+win32 mingw-namestring #-win32 princ-to-string src)
                                    "./install.sh")) :output t)))
      (format *error-output* "done.~%")))
  (cons t argv))

(defun sbcl-backup-features (argv)
  (let ((src (get-opt "src")) origin opts)
    (with-open-file (out (merge-pathnames "share/features.lisp-expr" (get-opt "prefix"))
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (flet ((read-from-file (f)
               (with-open-file (in (merge-pathnames f src))
                 (read in))))
        (setq origin (funcall (compile nil (read-from-file "local-target-features.lisp-expr"))
                              (read-from-file "base-target-features.lisp-expr"))
              opts (funcall (if (probe-file (merge-pathnames #1="customize-target-features.lisp" src))
                                (compile nil (read-from-file #1#))
                                #'identity) (copy-list origin)))
        (format out "(:+ ~s)~%(:- ~s)~%"
                (set-difference opts origin)
                (set-difference origin opts)))))
  (cons t argv))

(defvar *sbcl-copy-files*
  `((:copy
     "BUGS"
     "COPYING"
     "CREDITS"
     "INSTALL"
     "NEWS"
     "README"
     ("find-gnumake.sh" #o755)
     ("install.sh" #o755)
     "pubring.pgp"
     ("run-sbcl.sh" #o755)
     ("sbcl-pwd.sh" #o755)
     "contrib/asdf-module.mk"
     "contrib/vanilla-module.mk"
     "doc/sbcl.1"
     "output/sbcl.core"
     ("src/runtime/sbcl" #o755)
     "contrib/*/Makefile"
     "output/prefix.def"
     "obj/sbcl-home/contrib/*.*")
    (:touch
     ,(lambda (from to method)
              (loop for e in (directory (merge-pathnames "obj/asdf-cache/*" from))
                 do (funcall method (let ((x (merge-pathnames e "test-passed.test-report")))
                                      (make-pathname :defaults x
                                                     :directory (append (pathname-directory to)
                                                                        (nthcdr (length (pathname-directory from))
                                                                                (pathname-directory x)))))))))))

(defun sbcl-make-archive (argv)
  (when (get-opt "archive")
    (let ((from (truename (get-opt "src")))
          (to (truename (ensure-directories-exist (merge-pathnames (format nil "tmp/sbcl-~A-~A-~A/" (getf argv :version) (uname-m) (uname)) (homedir))))))
      (flet ((copy (from to)
               (ensure-directories-exist to)
               (uiop:copy-file from to))
             (touch (file)
               (ensure-directories-exist file)
               (with-open-file (i file
                                  :direction :probe
                                  :if-does-not-exist :create))))
        (loop :for (method . elts) :in *sbcl-copy-files*
           :do (case method
                 (:copy (loop for elt in elts
                           do (if (and (stringp elt) (wild-pathname-p elt))
                                  (mapc (lambda (x)
                                          (copy x (make-pathname :defaults x
                                                                 :directory (append (pathname-directory to)
                                                                                    (nthcdr (length (pathname-directory from))
                                                                                            (pathname-directory x))))))
                                        (reverse (directory (merge-pathnames elt from))))
                                  (if (consp elt)
                                      (progn
                                        (copy (merge-pathnames (first elt) from)
                                              (merge-pathnames (first elt) to))
                                        (sb-posix:chmod (merge-pathnames (first elt) to) (second elt)))
                                      (copy (merge-pathnames elt from)
                                            (merge-pathnames elt to))))))
                 (:touch (loop for elt in elts
                            do (if (functionp elt)
                                   (funcall elt from to #'touch)))))))))
  (cons t argv))

(defun sbcl-clean (argv)
  (format t "~&Cleaning~%")
  (let ((src (get-opt "src")))
    (uiop/os:chdir src)
    (format t "~&chdir ~A~%" src)
    (let* ((out (make-broadcast-stream))
           (*standard-output* (make-broadcast-stream
                               out #+sbcl(make-instance 'count-line-stream))))
      (uiop/run-program:run-program
       (list (sh) "-lc" (format nil "cd ~S;./clean.sh" src)) :output t))
    (format t "done.~%"))
  (cons t argv))

(defun sbcl-help (argv)
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
    (format t "sbcl install options~%")
    (fmt "as" "nickname" "install non-default optioned version of SBCL")
    (fmt "install" t "Download archive")
    (dolist (e *sbcl-options*)
      (apply #'fmt e)))
  (cons t argv))

(push `("sbcl" . ,(list #+win32 'sbcl-msys
                        'sbcl-version
                        'sbcl-argv-parse
                        'sbcl-start
                        'start
                        'sbcl-download
                        'sbcl-expand
                        'sbcl-patch
                        'sbcl-config
                        'sbcl-make
                        'sbcl-install
                        'sbcl-backup-features
                        'sbcl-make-archive
                        'sbcl-clean
                        'setup))
      *install-cmds*)

(push `("sbcl" . ,(list 'sbcl-help)) *help-cmds*)
(push `("sbcl" . sbcl-get-version) *list-cmd*)
