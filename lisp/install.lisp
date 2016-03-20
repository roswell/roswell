#!/bin/sh
#|-*- mode:lisp -*-|#
#|install roswell
exec ros -Q +R -L sbcl-bin -- $0 "$@"
|#

(ros:util "install%")

(in-package :ros.install)

(defun main (subcmd impl/version &rest argv)
  (let* (imp
         version verbose
         (seq impl/version)
         (pos (position #\/ impl/version))
         (*ros-path* (make-pathname :defaults (ros:opt "argv0")))
         sub cmds)
    (if pos
        (setq imp (subseq seq 0 pos)
              version (subseq seq (1+ pos)))
        (setq imp seq))
    (cond ((and
            (setf sub (make-pathname :name nil :type nil :defaults *load-pathname*)
                  sub (or (probe-file (merge-pathnames (format nil "install-~A.lisp" imp) sub))
                          (probe-file (merge-pathnames (format nil "install+~A.lisp" imp) sub))))
            (load sub))
           (setf *opts* (let (*read-eval*)
                          (append (read-from-string (first argv))
                                  (read-from-string (second argv))))
                 verbose (third argv)
                 argv (nthcdr 3 argv)
                 cmds (cond
                        ((equal subcmd "install") (cdr (assoc imp *install-cmds* :test #'equal)))
                        ((equal subcmd "help") (cdr (assoc imp *help-cmds* :test #'equal)))))
           (when cmds
             (let ((param `(t :target ,imp :version ,version :argv ,argv)))
               (handler-case
                   (loop for call in cmds
                      do (setq param (funcall call (rest param)))
                    while (first param))
                 (sb-sys:interactive-interrupt (condition)
                   (declare (ignore condition))
                   (format t "SIGINT detected, cleaning up the partially installed files~%")
                   (ros:roswell `(,(format nil "deleteing ~A/~A" (getf (cdr param) :target) (getf (cdr param) :version))) :string t))))))
          ((probe-file (setf sub (make-pathname :defaults impl/version :type "ros")))
           (install-ros sub))
          ((or (ql-dist:find-system imp)
               (ql:where-is-system imp))
           (let ((step 0))
             (handler-bind ((error (lambda (c)
                                     (declare (ignore c))
                                     ;; handle errors, but do not unwind -- Errors should automatically return the error code
                                     (format *error-output* "Aborted during step [~a/3]." step))))
               (format *error-output* "~&[~a/3] System '~A' found. Loading the system.." (incf step) imp)
               (let ((*features* (cons :ros.installing *features*))
                     (*standard-output* (make-broadcast-stream))
                     (*error-output*    (make-broadcast-stream))
                     (*trace-output*    (make-broadcast-stream)))
                 (if (ql:where-is-system imp)
                     (progn (ql:quickload imp)
                            (asdf:oos 'asdf:load-op imp :force t))
                     (ql:quickload imp)))
               (when *build-hook*
                 (format *error-output* "~&[~a/3] Processing build-hook.." (incf step))
                 (funcall *build-hook*))
               (format *error-output* "~&[~a/3] Attempting to install the scripts in ~
                                         roswell/ subdirectory of the system..." (incf step))
               (let ((scripts (directory (merge-pathnames "roswell/*.*" (ql:where-is-system imp)))))
                 (if scripts
                     (format t "~&Found ~a scripts:~{ ~a~}~%"
                             (length scripts) (mapcar #'pathname-name scripts))
                     (format t "~&No roswell scripts found.~%"))
                 (dolist (from scripts)
                   (install-ros from))))))
          (t (error "'~A' is not a valid target for 'install' -- It should be a name of either:
+ a quicklisp-installable system
+ a common lisp installation ~%" imp)))))
