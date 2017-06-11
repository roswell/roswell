(roswell:include "util-dump")
(defpackage :roswell.dump.ecl
  (:use :cl :roswell.util :roswell.util.dump))
(in-package :roswell.dump.ecl)

;;; ecl [WIP]

;; In ecl, we have to explicitly specify ALL object(fasl) files
;; in order to build a standalone executable.
;; uiop/image:create-image / dump-image does the similar things.


;; cf.https://common-lisp.net/project/ecl/manual/ch34s03.html
;; c:build-program
;; {image-name &key lisp-files ld-flags prologue-code epilogue-code}

(defun parse-options (file)
  (with-open-file (i file)
    (read-line i)
    (list
     (loop for i on (read i)
           when (keywordp (first i))
             append (prog1
                        (list (first i) (second i))
                      (setf i (cdr i))))
     (second (read i)))))

(defun dump-executable (cmds out ros-file)
  (declare (ignore out))
  (let ((options (parse-options ros-file))
        (path (make-pathname :defaults (truename ros-file))))
    (eval `(asdf:defsystem ,(pathname-name path)
               :build-operation asdf:program-op
             :depends-on (:uiop ,@(getf (first options) :system))
             ;; need to apply uiop:*command-line-arguments* for the function.
             :entry-point ,(format nil "~A::~A" (second options) :main)
             :components ((:file ,(pathname-name path)
                           :type ,(pathname-type path)))
             :class asdf:program-system))
    (preprocess-before-dump)
    (asdf:operate
     'asdf:program-op
     (pathname-name path)))) 

(defun ecl (type &rest args)
  (case type
    (:query (first args))
    (:executable
     (apply 'dump-executable args))))
