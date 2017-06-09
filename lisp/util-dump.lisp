#+ros.init
(roswell:include "util" "util-dump")
#+quicklisp
(ql:quickload '(:trivia) :silent t)
(defpackage :roswell.util.dump
  (:use :cl :roswell.util :trivia)
  (:export :*compression* :*queue* :*purify* :*impurify*))
(in-package :roswell.util.dump)

(defvar *compression* t "
  A flag enabling the core compression. Effective on
  sbcl only, and only effective when sbcl is compiled with
  sb-core-compression.")

(defvar *queue* nil "list of functions to be performed before dumping")
(defvar *purify* t "Whether running a purifying GC (moves objects to non-GC'd static space) before dump")
(defvar *impurify* t "CCL only. Impurify all static space objects to dynamic space. Precedes the purifying GC.")
