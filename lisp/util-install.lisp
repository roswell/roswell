(roswell:include '("locations" "util") "util-install")
(defpackage :roswell.install
  (:use :cl :roswell.util :roswell.locations)
  (:export :*build-hook* :install-impl :read-call :*ros-path*
           :install-system-script :install-impl-if-probed :install-script-if-probed
           :install-system-if-probed :mingw-namestring :install-github :*checkout-default*
           :install :decide-version :decide-download :require-system-package
           :start :setup :date :github-version :version :install-script :count-line-stream))
(in-package :roswell.install)

(defvar *ros-path* nil)
(defvar *env* "ROSINSTALL")
(defvar *checkout-default* 'checkout-github)

(defun install-impl (impl version argv cmds)
  "See install-impl-if-probed."
  (when cmds
    (let ((param `(t :target ,impl :version ,version :version-not-specified nil :argv ,argv)))
      (handler-case
          (loop for call in cmds
                when (roswell:verbose)
                  do (format *error-output* "~&:<install ~A~%~S~%:>" call (rest param))
                do (setq param (funcall call (rest param)))
                while (first param))
        #+sbcl
        (sb-sys:interactive-interrupt (condition)
          (declare (ignore condition))
          (format t "SIGINT detected, cleaning up the partially installed files~%")
          (roswell:roswell `(,(format nil "deleteing ~A/~A" (getf (cdr param) :target) (getf (cdr param) :version))) :string t))))))

(defun install-impl-if-probed (imp version argv)
  "Install the implementation when there is a file named install-XXXX (e.g. install-sbcl) in the roswell directory.
These files contain the `recipes` for download/build the binaries/sources of these implementations."
  (values (let ((fun (module "install" imp)))
            (when fun
              (install-impl imp version argv (funcall fun :install))
              (setf argv nil)
              t))
          argv))

(defun install-script-if-probed (impl/version)
  "Install a .ros script (ros install XXX.ros or just ros install XXX if it is in the current directory)"
  (let* (sub
         (result (and (pathname-name impl/version)
                      (probe-file (setf sub (make-pathname :defaults impl/version :type "ros"))))))
    (when result
      (funcall 'install-ros sub)
      result)))

(defun install-system-if-probed (imp)
  "Install the quicklisp system."
  (let ((result (or (read-call "ql-dist:find-system" imp)
                    (read-call "ql:where-is-system" imp))))
    (when (and result
               (= 1 (count #\, (ros:getenv *env*))))
      (funcall 'install-system-script imp)
      result)))

(defun install-localpath-if-probed (namestring)
  "To install a system by local path. NAMESTRING should be a path to an asdf file.
To differentiate it from the system with the same name in quicklisp, the path should start with '.', contain at least one '/'."
  (when (and (eql #\. (aref namestring 0))
             (find #\/ namestring))
    (let* ((path (truename namestring))
           (system (or (pathname-name path)
                       (first (last (pathname-directory path)))
                       (error "system not specified!")))
           (dir (make-pathname :defaults path :name nil :type nil))
           (dest (ensure-directories-exist (merge-pathnames (format nil "local-projects/local/~A/" system) (homedir)))))
      (format *error-output* "installing ~A~%" (truename namestring))
      (copy-dir dir dest)
      (install-system-if-probed system))))

(defun github-version (uri project filter)
  (let ((elts
          (let ((file (merge-pathnames (format nil "tmp/~A.html" project) (homedir))))
            (unless (and (probe-file file)
                         (< (get-universal-time) (+ (* 60 60) (file-write-date file))))
              (download uri file))
            (read-call "plump:parse" file))))
    (nreverse
     (loop for link in (read-call "plump:get-elements-by-tag-name" elts "link")
           for href = (read-call "plump:get-attribute" link "href")
           when (funcall filter href)
           collect it))))

(defun checkout-github (impl version tag)
  "Install a system from github."
  (clone-github impl version :path "local-projects" :branch tag))

(defun install (argv)
  "Install an implementation or a system.

   How this function parses it's arguments.

   It takes each argument and makes following:

   1. Splits the argument by first '/' occurence.
   2. Everything before / becomes the name of an implementation or a system.
   3. Everything after / (if any), again splitted by '/'.
   4. If step 3 was successful, first part of the string becomes a 'version'
      and second part (if any) becomes a 'tag'.

   After the parsing, roswell sequentially tries:

   1. To install 'implementation' of the given 'version'.
   2. To install a something.ros script, if argument is a path to a file.
   3. To install an asdf system with same name as exact argument's value.
   4. To install a system by local path.
      Path should start with '.', contain at least one '/' and point
      to an asd file.
      System is installed into \"${roswell-homedir}/local-projects/local/${system-name}/\".
   5. If nothing of above matched, it tries to clone system from the github,
      like:
      git clone -b 'tag' 'system'/'version'

      For example, if you did: ros install some/repo/the-feature, it will do

      git clone -b the-feature \\
          https://github.com/some/repo \\
          ${roswell-home}/local-projects/some/repo
      
 "
  (read-call "quicklisp-client:register-local-projects")
  (loop
    with *ros-path* = (make-pathname :defaults (opt "argv0"))
    with changed
    with envold = (ros:getenv *env*)
    for impl/version/tag = (first argv)
    for _ = (remove "" (split-sequence #\/ impl/version/tag) :test 'equal)
    for impl = (first _)
    for version = (second _)
    for tag = (and (cddr _) (format nil "~{~A~^/~}" (cddr _)))
    for version/tag = (if tag (format nil "~A/~A" version tag)
                          version)
    do (setf argv (rest argv))
       (setenv *env* (format nil "~A,~A" impl/version/tag envold))
       (cond
         ;;registerd implementations like sbcl ccl-bin abcl etc
         ((setf (values _ argv) (install-impl-if-probed impl version/tag argv)))
         ;;local ros file like tool.ros
         ((install-script-if-probed impl/version/tag))
         ;;asd/quicklisp registered systems which contain "roswell" directory
         ((install-system-if-probed impl/version/tag))
         ;;local relative pathname
         ((install-localpath-if-probed impl/version/tag))
         ;;github registerd system like "fukamachi/sblint" checkout
         (version
          (funcall *checkout-default* impl version tag)
          (read-call "quicklisp-client:register-local-projects")
          (or (and (install-impl-if-probed version nil argv)
                   (or (setf argv nil) t))
              (install-system-if-probed version)))
         (t (format *error-output* "'~A' is not a valid target for 'install' -- It should be a name of either:
+ a quicklisp-installable system
+ a common lisp installation ~%" impl)
            (roswell:quit 1)))
       (setf changed t)
    while argv
    finally (when changed (roswell:exec `(,(opt "argv0") "setup")))))

#+win32
(defun mingw-namestring (path)
  (string-right-trim (format nil "~%")
                     (uiop:run-program `(,(sh) "-lc" ,(format nil "cd ~S;pwd" (uiop:native-namestring path)))
                                       :output :string)))

(defvar *package-assoc*
  '(("dpkg" . (("zlib" . "zlib1g-dev")))))

(defun require-system-package (&rest packages)
  "check installation of system library like 'zlib' and show how to install."
  (declare (ignorable packages))
  (loop with result
        with (mgr . assoc) = (find-if #'roswell.util:which *package-assoc* :key #'first)
        for package in packages
        for to-install = (cdr (assoc package assoc :test 'equal))
        do (cond
             ((null to-install))
             ((equal mgr "dpkg")
              (or (ignore-errors
                   (uiop:run-program `(,mgr "-l" ,to-install))
                   t)
                  (push to-install result))))
        finally
           (return
             (if result
                 (cond
                   ((equal mgr "dpkg")
                    (format *error-output*
                            "might cause error 'apt-get install ~{~A~^ ~}' would help~%" result)
                    t)) ;; Don't have confidence.
                 t))))

(defun setup (argv)
  (unless (equal (getf argv :target) "sbcl-bin")
    (setf (config "default.lisp") (getf argv :target)
	  (config (format nil "~A.version" (getf argv :target))) (opt "as")))
  (setf (config "setup.time") (format nil "~A" (get-universal-time)))
  (cons t argv))
