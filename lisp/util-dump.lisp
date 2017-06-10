#+ros.init
(roswell:include "util" "util-dump")
(defpackage :roswell.util.dump
  (:use :cl :roswell.util)
  (:export :*compression* :*queue* :*purify* :*impurify* :remove-docstrings
   :*package-blacklist* :*additional-blacklist-for-destroy-packages*
   :makunbound-symbols-and-delete-package :delete-all-packages
   :delete-macro-definitions :delete-compiler-macro-definitions))
(in-package :roswell.util.dump)

(defvar *compression* t "
  A flag enabling the core compression. Effective on
  sbcl only, and only effective when sbcl is compiled with
  sb-core-compression.")

(defvar *queue* nil "list of functions to be performed before dumping")
(defvar *purify* t "Whether running a purifying GC (moves objects to non-GC'd static space) before dump")
(defvar *impurify* t "CCL only. Impurify all static space objects to dynamic space. Precedes the purifying GC.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ignore-errors
   (when (find-symbol "MAKE-PACKAGE-HASHTABLE" :sb-impl)
     (push :roswell-dump-newer-sbcl *features*))))

(defvar *package-blacklist* `("KEYWORD" "ROSWELL" "ROS.SCRIPT.DUMP" "ROSWELL.UTIL.DUMP"
                                        ;; add impl-specific customization
                                        #+sbcl "ROSWELL.DUMP.SBCL"
                                        #+(or) ,@'())
  "A list of package-designators which is not deleted by delete-all-packages.
The default value contains the minimal blacklist.")

(defvar *additional-blacklist-for-destroy-packages*
  '("ROS")
  "An additional list of package-designators that needs to be protected from destroy-packages-sbcl.
These are appended to the blacklist before destroying the package system.
Notably, it must include all nicknames.")

(defun remove-docstrings ()
  "Docstrings are unnecessary when the resulting binary is expected to be a batch program.
With this feature, applications that use docstrings may not work properly."
  (do-all-symbols (s)
    (dolist (doc-type '(function compiler-macro setf
                        method-combination type structure
                        variable))
      (when (documentation s doc-type)
        (setf (documentation s doc-type) nil)))))

(defun makunbound-symbols-and-delete-package (pkg-designator)
  (format t "Deleting ~a~%" pkg-designator)
  (force-output *standard-output*)
  (handler-case
      (progn
        #-ccl
        (do-symbols (symbol pkg-designator)
          (ignore-errors (makunbound symbol))
          (ignore-errors (fmakunbound symbol))
          (ignore-errors (unintern symbol pkg-designator)))
        #+ccl
        (do-symbols (symbol pkg-designator)
          ;; f/makunbound causes segv
          (ignore-errors (unintern symbol pkg-designator))))
    (package-error ()))
  (handler-case
    (delete-package pkg-designator)
    (package-error ()))
  ;; 
  ;; alternative: more restrictive error handling, handle the name conflict caused during deleting a package
  ;; cf. http://clhs.lisp.se/Body/f_del_pk.htm
  #+(or)
  (handler-bind ((package-error #'continue))
    (delete-package pkg-designator)))

(defun delete-all-packages ()
  ;; push the package name of the main function (== package of the given script)
  (when roswell:*main*
    (pushnew (package-name (symbol-package roswell:*main*))
             *package-blacklist* :test #'string=))
  (map nil #'makunbound-symbols-and-delete-package
       (set-difference (list-all-packages)
                       (mapcar #'find-package *package-blacklist*))))

(defun delete-macro-definitions ()
  "Delete the macro functions assuming no run-time compilation would occur.
This is a portable implementation."
  (#+sbcl sb-ext:without-package-locks
   #-sbcl progn
   (do-all-symbols (s)
     (when (macro-function s)
       (fmakunbound s)))))

(defun delete-compiler-macro-definitions ()
  "Delete the compiler-macros assuming no run-time compilation would occur.
This is a portable implementation."
  (#+sbcl sb-ext:without-package-locks
   #-sbcl progn
   (do-all-symbols (s)
     (when (compiler-macro-function s)
       (setf (compiler-macro-function s) nil)))))
