(ros:include "util")
(defpackage :roswell.dump.ecl
  (:use :cl :roswell.util))
(in-package :roswell.dump.ecl)

;;; ecl [WIP]

;; In ecl, we have to explicitly specify ALL object(fasl) files
;; in order to build a standalone executable.
;; uiop/image:create-image / dump-image does the similar things.


;; cf.https://common-lisp.net/project/ecl/manual/ch34s03.html
;; c:build-program
;; {image-name &key lisp-files ld-flags prologue-code epilogue-code}

(defun print-%-readable-or-lose (sym-name &optional (s *standard-output*))
  (let ((*print-readably* t))
    (handler-case
        (format s
                "(% ~s '~s)~%"
                sym-name
                (ignore-errors
                  (symbol-value
                   (read-from-string sym-name))))
      (print-not-readable ()))))


(defun dump-executable (cmds out ros-file)
  (format *error-output* "~&; ECL is actually not suppported. Gotcha! ~%")
  (ros:quit 1)
  #+nil
  (let* ((tmp (uiop:run-program "mktemp -d" :output '(:string :stripped t)))
         (ros-opts-file (format nil "~a/ros-opts.lisp" tmp))
         objfiles
         (*compile-verbose* t)
         (*compile-print* t))
    (format *error-output* "~&; In directory ~a~%" tmp)
    (unwind-protect
        (progn
          (with-open-file (*standard-output*
                           ros-opts-file
                           :direction :output
                           :if-does-not-exist :create)
            #+nil
            (prin1
             `(setf *load-verbose* t
                    *load-print* t))
            (terpri)
            ;; fixme: duplicated, but necessary
            (prin1
             `(defpackage :ros
                 (:use :cl)
                 (:shadow :load :eval :package :restart :print :write)
                 (:export :run :*argv* :*main* :quit :script :quicklisp :getenv :opt
                          :ignore-shebang :ensure-using-downloaded-asdf :include :ensure-asdf
                          :roswell :exec :setenv :unsetenv :version :swank :verbose)
                 (:documentation "Roswell backend.")))
            (terpri)
            (progn
              (prin1
               `(cl:load ,(make-pathname
                           :name "init"
                           :type "lisp"
                           :defaults #.*load-pathname*)))
              (terpri))
            #+nil
            (progn
              (prin1
               `(in-package :ros))
              (terpri)
              (princ
               "(defmacro eval-with-printing (&body body)
                (list* 'progn
                       (loop for form in body
                             collect (list 'cl:print (list 'quote form))
                             collect (list 'cl:terpri)
                             collect (list 'cl:finish-output)
                             collect form)))")
              (terpri)
              (format *standard-output* "~&(eval-with-printing~&")
              (terpri)
              (princ `(defun % (string value)
                        (ignore-errors
                          (setf (symbol-value
                                 (read-from-string string))
                                value))))
              (terpri)
              (princ
               `(trace %))
              (terpri)
              (dolist (sym-name '("QUICKLISP-CLIENT::*LOCAL-PROJECT-DIRECTORIES*"
                                  "ROS::*ROS-OPTS*"))
                (print-%-readable-or-lose sym-name))
              (terpri)
              (prin1
               `(format t "~&loading init.ros...~&"))
              (with-open-file (s (make-pathname
                                  :name "init"
                                  :type "lisp"
                                  :defaults #.*load-pathname*))
                (ignore-errors
                  ;; copy and paste
                  (loop (write-char (read-char s) *standard-output*))))
              (format *standard-output* "~&)~&")))
          (format *error-output* "~&finished dumping all special variables.")
          (proclaim '(optimize (debug 3) (speed 0)))
          (push (compile-file ros-opts-file
                              :system-p t
                              :output-file
                              (format nil "~a/ros-opts.o" tmp)) objfiles)
          (push (compile-file ros-file
                              :system-p t
                              :output-file
                              (format nil "~a/script.o" tmp)) objfiles)
          (c:build-program
           out
           :lisp-files ;#+nil
           ;; objfiles
           ;; #+nil
           (nreverse objfiles)
           :epilogue-code
           (print
            `(progn
               (setf *load-pathname* (pathname (ext:argv 0)))
               (setf ros:*argv*
                     (eval
                      (read-from-string
                       "(loop for i from 0 below (ext:argc)
                             collect (ext:argv i))")))
               (print ros:*argv*)
               (ros:run ',cmds)))))
      ;; (uiop:run-program (format nil "rm -r ~a" tmp))
      )))

(defun ecl (type &rest args)
  (case type
    (:executable
     (apply 'dump-executable args))))
