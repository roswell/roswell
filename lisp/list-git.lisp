(defpackage :roswell.list.git
  (:use :cl :roswell.util))
(in-package :roswell.list.git)

(defun git (&rest r)
  (declare (ignore r))
  (let* ((* (loop for v being the hash-values in (roswell.util::local-project-build-hash)
                  for .git/ = (merge-pathnames ".git/"
                                               (make-pathname :defaults v :name nil :type nil))
                  when (and (equal (pathname-name v)
                                   (first (last (pathname-directory v))))
                            (uiop:directory-exists-p .git/))
                  collect v))
         (* (sort * #'string< :key #'pathname-name)))
    (format t "~{~A~%~}" (mapcar #'pathname-name *))))
