(eval-when (:compile-toplevel :load-toplevel :execute)
  (roswell:include "install-sbcl"))
(defpackage :roswell.install.sbcl-source
  (:use :cl :roswell.install :roswell.util :roswell.locations))
(in-package :roswell.install.sbcl-source)

(defun sbcl-source-argv-parse (argv)
  (let ((pos (position "--as" (getf argv :argv) :test 'equal)))
    (set-opt "as" (or (and pos (ignore-errors (nth (1+ pos) (getf argv :argv)))
                           (format nil "~A-~A"
                                   (getf argv :version)
                                   (nth (1+ pos) (getf argv :argv))))
                      (getf argv :version))))
  (setf (getf argv :target) "sbcl")
  (set-opt "non-git" t)
  (set-opt "until-extract" t)
  (set-opt "src" (merge-pathnames (format nil "src/sbcl-~A/" (getf argv :version)) (homedir)))
  (cons t argv))

(defun sbcl-source-help (argv)
  (format t "no options for sbcl-source~%")
  (cons t argv))

(defun sbcl-source (type)
  (case type
    (:help '(sbcl-source-help))
    (:install `(,(decide-version 'roswell.install.sbcl::sbcl-get-version)
                sbcl-source-argv-parse
                ,(decide-download 'roswell.install.sbcl::sbcl-download)
                roswell.install.sbcl::sbcl-expand))
    (:list 'roswell.install.sbcl::sbcl-get-version)))
