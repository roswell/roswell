(defpackage :roswell.init.env
  (:use :cl :roswell.util))
(in-package :roswell.init.env)

(defun install-quicklisp (&optional path
                            (from (merge-pathnames "lisp/quicklisp/" (homedir))))
  (dolist (relative '("" "quicklisp/" "local-init/"))
    (dolist (file (uiop:directory-files (merge-pathnames "*.*" (merge-pathnames relative from))))
      (uiop:copy-file file (make-pathname
                            :defaults (ensure-directories-exist (merge-pathnames relative path))
                            :name (pathname-name file)
                            :type (pathname-type file))))))

(defun env (name &rest params)
  (setf name (first params))
  (let* ((path (if name
                   (merge-pathnames (format nil "env/~A/" name)
                                    (opt "homedir"))
                   (error "name is not specified")))
         (lisp (ros:opt "default.lisp"))
         (ver (ros:opt (format nil "~A.version" lisp))))
    (ensure-directories-exist
     (merge-pathnames "local-projects/" path))
    (unless
        (with-open-file (out (setf path (merge-pathnames "config" path))
                             :direction :output
                             :if-exists nil
                             :if-does-not-exist :create)
          (when out
            (format out "default.lisp	0	~A~%" lisp)
            (format out "~A.version	0	~A~%" lisp ver)
            (format t "~&Successfully generated: ~A~%" path)
            t))
      (unless (equal name "roswell")
        (format *error-output* "~&File already exists: ~A~%" path))
      (roswell:quit 1))))
