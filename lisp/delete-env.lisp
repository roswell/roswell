(defpackage :roswell.delete.env
  (:use :cl :roswell.util))
(in-package :roswell.delete.env)

(defun env (&rest argv)
  (if (rest argv)
      (dolist (x (rest argv) (roswell:quit 0))
        (let ((* (probe-file (merge-pathnames (format nil "env/~A/config" x)
                                              (roswell:opt "homedir")))))
          (when *
            (format t "delete ~S~%" x)
            (uiop/filesystem:delete-directory-tree
             (make-pathname :defaults * :name nil) :validate t))))
      `(,(roswell:opt "wargv0") "help" ,(format nil "delete-env"))))

