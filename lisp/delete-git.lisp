(defpackage :roswell.delete.git
  (:use :cl :roswell.util))
(in-package :roswell.delete.git)

(defun git (&rest argv)
  (if (rest argv)
      (dolist (x (rest argv) (roswell:quit 0))
        (let* ((* (directory (merge-pathnames "**/.git/" (first ql:*local-project-directories*))))
               (* (mapcar (lambda (x) (directory (merge-pathnames "../*.asd" x))) *))
               (* (apply #'append *))
               (* (remove x * :test (complement #'equal) :key #'pathname-name))
               (* (mapcar (lambda (x) (truename (make-pathname :defaults x :type nil :name nil))) *)))
          (loop for x in *
                with a = (namestring (truename (homedir)))
                for b = (namestring x)
                do (and (string= a b :end2 (min (length a) (length b)))
                        (probe-file b)
                        (progn
                          (format t "delete ~S~%" x)
                          (uiop/filesystem:delete-directory-tree x :validate t))))))
      `(,(roswell:opt "wargv0") "help" ,(format nil "delete-git"))))
