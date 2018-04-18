(roswell:include "util-install-quicklisp")
(defpackage :roswell.install.sbcl
  (:use :cl :roswell.install :roswell.util :roswell.locations))
(in-package :roswell.install.sbcl)

#|
:sb-source-locations
:sb-unicode
:sb-test
:sb-doc
:inline-constants
:package-local-nicknames
:ieee-floating-point
|#

(defvar *sbcl-options*
  ;;(name default description sb-prefix)
  `(("thread" ,(or #+(or x86 x86-64 arm64) t) "Build SBCL without support for native threads." t)
    ("core-compression" t "Build SBCL without support for compressed cores and without a dependency on zlib." t)
    ("ldb" nil "Include low-level debugger in the build." t)
    ("xref-for-internals" nil "Include XREF information for SBCL internals (increases core size by 5-6MB)." t)
    ("simd-pack" ,(or #+x86-64 t) "Enable SIMD intrinsics." t)
    ("show" nil "Use the extra debugging information." t)
    ("after-xc-core" nil "controls whether the build process produces an after-xc.core file." t)
    ("show-assem" nil "Enable extra debugging output." t)
    ("qshow" nil "Compile the C runtime with support for low-level debugging output." t)
    ("eval" nil "Support for a full evaluator." t)
    ("dynamic-core" ,(or #+win32 t) "The runtime to be rebuilt without requiring changes to the core file." t)
    ("linkable-runtime" nil "Allowing linking with extra object files" t)
    ("fasteval" nil "Support for a different evaluator (interpreter)." t)
    ("fluid" nil "Setting this makes SBCL more \"fluid\"." t)
    ("dyncount" nil "Enable code for collecting statistics This code is probably pretty stale." t)
    ("hash-table-debug" nil "Enable detecting concurrent accesses to the same hash-table. somewhat too eager." t)
    ("safepoint" nil "Synchronization between threads using safepoints instead of signals." t)
    ("thruption" nil "Compiling with safepoints, the INTERRUPT-THREAD mechanism can also use safepoints." t)
    ("wtimer" nil "Compiling with safepoints and thruptions,replace setitimer with a background thread." t)
    ("ud2-breakpoints" nil "use the UD2 instruction which generates SIGILL instead." nil)
    ("32x16-divide" nil "affects the definition of a lot of things in bignum.lisp.not needed for X86." nil)))

(defun sbcl-get-version ()
  (format *error-output* "Checking version to install....~%")
  (github-version (sbcl-git-version-uri) "sbcl" (lambda (href) (ignore-errors (subseq href (1+ (position #\- href :from-end t)))))))

(defun sbcl-msys (argv)
  (unless (or (roswell:getenv "MSYSCON")
              (opt "until-extract"))
    (roswell:roswell '("install msys2+") :interactive nil))
  (cons t argv))

(defun sbcl-argv-parse (argv)
  (let ((pos (position "--as" (getf argv :argv) :test 'equal)))
    (set-opt "as" (or (and pos (ignore-errors (nth (1+ pos) (getf argv :argv)))
                           (format nil "~A-~A"
                                   (getf argv :version)
                                   (nth (1+ pos) (getf argv :argv))))
                      (getf argv :version))))
  (setf (getf argv :target)
        (if (find "--sbcl-bin" (getf argv :argv) :test 'equal)
            "sbcl-bin"
            "sbcl"))
  (when (position "--archive" (getf argv :argv) :test 'equal)
    (set-opt "install.force" "t")
    (set-opt "archive" "t"))
  (when (position "--without-install" (getf argv :argv) :test 'equal)
    (set-opt "until-extract" t))
  (set-opt "prefix" (merge-pathnames (format nil "impls/~A/~A/~A/~A/" (uname-m) (uname) (getf argv :target) (opt "as")) (homedir)))
  (set-opt "src" (merge-pathnames (format nil "src/sbcl-~A/" (getf argv :version)) (homedir)))
  (labels ((with (opt)
             (cond ((position (format nil "--with-~A" opt) (getf argv :argv) :test 'equal) (set-opt opt t))
                   ((position (format nil "--without-~A" opt) (getf argv :argv) :test 'equal) (set-opt opt :false)))))
    (loop for (name default description sb-prefix) in *sbcl-options*
          do
             (when default
               (set-opt name (eql default t)))
             (with name)))
  (cons (if (opt "core-compression")
            (require-system-package "zlib")
            t)
        argv))

(defun sbcl-start (argv)
  (when (and (find (getf argv :target) '("sbcl-bin" "sbcl") :test 'equal)
             (not (opt "sbcl.compiler")))
    (set-opt "sbcl.compiler" "sbcl-bin"))
  (cons t argv))

(defun sbcl-download (argv)
  (cond
    ((or (equal "git" (getf argv :version))
         (probe-file (merge-pathnames "src/sbcl-git/" (homedir))))
     (set-opt "src" (merge-pathnames "src/sbcl-git/" (homedir)))
     ()) ;; skip downloading if version is 'git'
    (t
     (set-opt "download.uri" (format nil "~@{~A~}" (sbcl-uri) "sbcl-"
                                     (getf argv :version) ".tar.gz"))
     (set-opt "download.archive" (let ((pos (position #\/ (opt "download.uri") :from-end t)))
                                   (when pos
                                     (merge-pathnames (format nil "archives/~A" (subseq (opt "download.uri") (1+ pos))) (homedir)))))
     `((,(opt "download.archive") ,(opt "download.uri"))))))

(defun sbcl-expand (argv)
  (let* ((h (homedir))
         (v (getf argv :version))
         (gitsrc (merge-pathnames "src/sbcl-git/" h)))
    (cond
      ((equal "git" (getf argv :version))
       (unless (probe-file gitsrc)
         (clone-github "sbcl" "sbcl" :path (merge-pathnames "src/" h))
         (ql-impl-util:rename-directory
          (merge-pathnames "src/sbcl/sbcl" h)
          (merge-pathnames "src/sbcl-git" h))
         (uiop/filesystem:delete-directory-tree (merge-pathnames "src/sbcl/" h) :validate t)))
      ((probe-file gitsrc)
       (funcall 'sbcl-patch argv :revert t) ;; revert patch.
       (chdir gitsrc)
       (uiop/run-program:run-program "git checkout master")
       (uiop/run-program:run-program "git pull")
       (or (uiop/run-program:run-program
            (format nil "git checkout ~A" (getf argv :version)) :ignore-error-status t)
           (uiop/run-program:run-program
            (format nil "git checkout sbcl-~A" (getf argv :version)))))
      (t
       (format t "~%Extracting archive:~A~%" (opt "download.archive"))
       (expand (opt "download.archive")
               (merge-pathnames "src/" h))
       (ignore-errors
        (ql-impl-util:rename-directory
         (merge-pathnames (format nil "src/sbcl-sbcl-~A" v) h)
         (merge-pathnames (format nil "src/sbcl-~A" v) h))
        (with-open-file (o (merge-pathnames (format nil "src/sbcl-~A/version.lisp-expr" v) h)
                           :direction :output
                           :if-does-not-exist :create
                           :if-exists nil)
          (format o "~S~%" v))))))
  (cons (not (opt "until-extract")) argv))

(defvar *sbcl-patch-list-cache* nil)
(defun sbcl-patch-list ()
  (or *sbcl-patch-list-cache*
      (setf *sbcl-patch-list-cache*
            (loop for i in (list
                            #+darwin "sbcl-posix-tests.patch"
                            #+linux  "sbcl-1.3.11.patch")
               collect (probe-file (merge-pathnames i (opt "patchdir")))))))

(defun sbcl-patch (argv &key revert (src (opt "src")))
  (unless (opt "sbcl.patchless")
    (dolist (patch (sbcl-patch-list))
      (format t "~%Applying patch:~A~%" (file-namestring patch))
      (chdir src)
      (uiop/run-program:run-program
       (if revert
           "patch -p0 -R -r -"
           "patch -p0 -N -r -")
       :output t :input patch :ignore-error-status t)))
  (cons t argv))

(defun sbcl-config (argv)
  (with-open-file (out (ensure-directories-exist
                        (merge-pathnames
                         (format nil "src/sbcl-~A/customize-target-features.lisp"
                                 (getf argv :version)) (homedir)))
                       :direction :output :if-exists :supersede :if-does-not-exist :create)
    (let ((*package* (find-package :roswell.install.sbcl)))
      (format out "~s"
              `(lambda (list)
                 (dolist (i ',(loop for (name default description sb-prefix) in *sbcl-options*
                                    when (opt name)
                                    collect (list (read-from-string (format nil ":~A~A" (if sb-prefix "sb-" "") name))
                                                  (eql t (opt name)))))
                   (if (second i)
                       (pushnew (first i) list)
                       (setf list (remove (first i) list))))
                 list))))
  (cons t argv))

(defun sbcl-make (argv)
  (with-open-file (out (ensure-directories-exist
                        (merge-pathnames (format nil "impls/log/~A-~A/make.log"
                                                 (getf argv :target) (opt "as"))
                                         (homedir)))
                       :direction :output :if-exists :append :if-does-not-exist :create)
    (format out "~&--~&~A~%" (date))
    (let* ((src (opt "src"))
           (compiler (format nil "~A -L ~A without-roswell=t run" *ros-path* (opt "sbcl.compiler")))
           (cmd (list (sh) "-lc" (format nil "cd ~S;~A ~A ~A ~A"
                                         (#+win32 mingw-namestring #-win32 princ-to-string src)
                                         (or #-win32 (sh) "")
                                         "./make.sh" (format nil "'--xc-host=~A'"  compiler)
                                         (format nil "'--prefix=~A'"
                                                 (funcall #+win32 (lambda (x)
                                                                    (mingw-namestring (ensure-directories-exist x)))
                                                          #-win32 'identity
                                                          (opt "prefix"))))))
           (*standard-output* (make-broadcast-stream out #+sbcl(make-instance 'count-line-stream))))
      (chdir src)
      (format t "~&~S~%" cmd)
      (uiop/run-program:run-program cmd :output t :ignore-error-status nil)))
  (cons t argv))

(defun sbcl-install (argv)
  (let* ((impl-path (opt "prefix"))
         (src (opt "src"))
         (install-root impl-path)
         (log-path (merge-pathnames (format nil "impls/log/~A-~A/install.log" (getf argv :target) (opt "as")) (homedir))))
    (unless (opt "archive")
      (format t "~&Installing ~A/~A" (getf argv :target) (opt "as"))
      (format t "~&prefix: ~s~%" impl-path)
      (ensure-directories-exist impl-path)
      (ensure-directories-exist log-path)
      (chdir src)
      (unsetenv "SBCL_HOME")
      (setenv "INSTALL_ROOT" (format nil "~A" install-root))
      (with-open-file (out log-path :direction :output :if-exists :append :if-does-not-exist :create)
        (format out "~&--~&~A~%" (date))
        (let ((*standard-output* (make-broadcast-stream
                                  out #+sbcl(make-instance 'count-line-stream))))
          (uiop/run-program:run-program
           (list (sh) "-lc" (format nil "cd ~S;~A ~A"
                                    (#+win32 mingw-namestring #-win32 princ-to-string src)
                                    (or #-win32 (sh) "")
                                    "./install.sh")) :output t)))
      (format *error-output* "done.~%")))
  (cons t argv))

(defun sbcl-install-win32 (argv)
  (uiop/run-program:run-program
   (list (sh) "-lc" (format nil "cp `which zlib1.dll` ~S"
                            (#+win32 mingw-namestring #-win32 princ-to-string (merge-pathnames "bin/" (opt "prefix")))))
   :output t)
  (cons t argv))

(defun sbcl-backup-features (argv)
  (let ((src (opt "src")) origin opts)
    (ignore-errors ;; TBD found error on sbcl/1.1.14. Not so important so far to save features.
     (with-open-file (out (merge-pathnames "share/features.lisp-expr" (opt "prefix"))
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
                 (set-difference origin opts))))))
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
  (when (opt "archive")
    (let ((from (truename (opt "src")))
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
                                              #+sbcl(sb-posix:chmod (merge-pathnames (first elt) to) (second elt)))
                                            (copy (merge-pathnames elt from)
                                                  (merge-pathnames elt to))))))
                    (:touch (loop for elt in elts
                                  do (if (functionp elt)
                                         (funcall elt from to #'touch)))))))))
  (cons t argv))

(defun sbcl-clean (argv)
  (format t "~&Cleaning~%")
  (let ((src (opt "src")))
    (chdir src)
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
    (loop for (name default description sb-prefix) in *sbcl-options*
          do (fmt name default description)))
  (cons t argv))

(defun sbcl (type)
  (case type
    (:help '(sbcl-help))
    (:install `(,(decide-version 'sbcl-get-version)
                sbcl-argv-parse
                #+win32 sbcl-msys
                sbcl-start
                start
                ,(decide-download 'sbcl-download)
                sbcl-expand
                sbcl-patch
                sbcl-config
                sbcl-make
                sbcl-install
                #+win32 sbcl-install-win32
                sbcl-backup-features
                sbcl-make-archive
                sbcl-clean
                setup))
    (:list 'sbcl-get-version)))
