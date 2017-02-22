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
(defvar *checkout-default* 'checkout-github)

(defun install-impl (impl version argv cmds)
  (when cmds
    (let ((param `(t :target ,impl :version ,version :version-not-specified nil :argv ,argv)))
      (handler-case
          (loop for call in cmds
                do (setq param (funcall call (rest param)))
                while (first param))
        #+sbcl
        (sb-sys:interactive-interrupt (condition)
          (declare (ignore condition))
          (format t "SIGINT detected, cleaning up the partially installed files~%")
          (roswell:roswell `(,(format nil "deleteing ~A/~A" (getf (cdr param) :target) (getf (cdr param) :version))) :string t))))))

(defun install-impl-if-probed (imp version argv)
  (values (let ((fun (module "install" imp)))
            (when fun
              (install-impl imp version argv (funcall fun :install))
              (setf argv nil)
              t))
          argv))

(defun install-script-if-probed (impl/version)
  (let* (sub
         (result (and (pathname-name impl/version)
                      (probe-file (setf sub (make-pathname :defaults impl/version :type "ros"))))))
    (when result
      (funcall 'install-ros sub)
      result)))

(defun install-system-if-probed (imp)
  (let ((result (or (read-call "ql-dist:find-system" imp)
                    (read-call "ql:where-is-system" imp))))
    (when result
      (funcall 'install-system-script imp)
      result)))

(defun install-localpath-if-probed (namestring)
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
           when (eql (aref href 0) #\/)
             collect (funcall filter href)))))

(defun checkout-github (impl version tag)
  (clone-github impl version :path "local-projects" :branch tag))

(defun install (argv)
  (read-call "quicklisp-client:register-local-projects")
  (loop
    with *ros-path* = (make-pathname :defaults (opt "argv0"))
    with _
    with changed
    for impl/version/tag = (first argv)
    for pos = (position #\/ impl/version/tag)
    for impl = (if pos (subseq impl/version/tag 0 pos) impl/version/tag)
    for version/tag = (when pos (subseq impl/version/tag (1+ pos)))
    for pos2 = (position #\/ version/tag)
    for version = (if pos2 (subseq version/tag 0 pos2) version/tag)
    for tag = (when pos2 (subseq version/tag (1+ pos2)))
    do (setf argv (rest argv))
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

(defun require-system-package (&rest packages)
  "check installation of system library like 'zlib' and show how to install."
  (declare (ignorable packages))
  t)
