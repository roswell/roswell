(cl:in-package :cl-user)
#-asdf
(require :asdf)

(defpackage :ros
  (:use :cl)
  (:shadow :load :eval :package))

(in-package :ros)
(push :ros-launched *features*)
(defvar *verbose* nil)
(export (defvar *argv* nil))

(defun load (cmd arg &rest rest)
  (declare (ignorable cmd rest))
  (unless (cl:load arg :if-does-not-exist nil)
    (when *verbose* (format t "~s not exists~%" arg))))

(defun source-registry (cmd arg &rest rest)
  (declare (ignorable cmd rest))
  (asdf:initialize-source-registry arg))

(defun system (cmd arg &rest rest)
  (declare (ignorable cmd rest))
  #-quicklisp(asdf:operate 'asdf:load-op arg)
  #+quicklisp(ql:quickload arg))

(setf (fdefinition 'load-systm)
      #'system)

(defun package (cmd arg &rest rest)
  (declare (ignorable cmd rest))
  (setq *package* (find-package (read-from-string (format nil "#:~A" arg)))))

(defun system-package (cmd arg &rest rest)
  (declare (ignorable cmd rest))
  (apply #'system cmd arg rest)
  (apply #'package cmd arg rest))

(defun eval (cmd arg &rest rest)
  (declare (ignorable cmd rest))
  (cl:eval (read-from-string arg)))

(defun quit (cmd &rest rest)
  (declare (ignorable cmd rest))
  #+sbcl
  (sb-ext:exit))

(export
 (defun script (cmd arg &rest rest)
   (declare (ignorable cmd arg))
   (setf *argv* rest)
   (with-open-file (in arg)
     (let ((line(read-line in)))
       (cl:load (make-concatenated-stream
                 (make-string-input-stream
                  (if (equal (subseq line 0 (min (length line) 2)) "#!")
                      "" line))
                 in
                 (make-string-input-stream
                  "(cl:apply 'main ros:*argv*)")))))))

(export 
 (defun run (list)
   (loop :for elt :in list
      :do (apply (intern (string (first elt)) (find-package :ros)) elt))))
