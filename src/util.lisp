(cl:in-package :cl-user)
(when (cl:find-package :ros.util)
  (push :ros.util *features*))

(defpackage :ros.util
  (:use :cl)
  (:export :uname :uname-m :homedir :config))

(in-package :ros.util)

(defun uname ()
    (ros:roswell '("roswell-internal-use uname") :string t))

(defun uname-m ()
  (ros:roswell '("roswell-internal-use uname -m") :string t))

(defun homedir ()
  (ros:opt "homedir"))

(defun config (c)
  (ros:roswell (list "config show" c) :string t))

(defun (setf config) (a b)
  (ros:roswell (list "config" b a) :string t)
  a)
