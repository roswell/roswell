(defpackage :roswell.dist.add
  (:use :cl))
(in-package :roswell.dist.add)

(defun add (&rest r)
  (let ((gh-support (asdf:find-system "gh-dist")))
    (dolist (elm (rest r))
      (if (and gh-support
               (= 1 (count #\/ elm)))
          (uiop:symbol-call :gh-dist :install elm :prompt nil)
          (ql-dist:install-dist elm :prompt nil)))))
