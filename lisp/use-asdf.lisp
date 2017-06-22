(defpackage :roswell.use.asdf
  (:use :cl :roswell.util))
(in-package :roswell.use.asdf)

(defun asdf (impl version &rest r)
  (declare (ignore r impl))
  (cond
    ((null version)
     (let ((s *error-output*)
           (asdf (config "asdf.version")))
       (format s (if (zerop (length asdf))
                     "ASDF version is not specified.~%"
                     "choosen version is ~S~%") asdf)
       (format s "Version Candidates:~%no~%~{~A~%~}"
               (sort (mapcar (lambda (x) (car (last (pathname-directory x))) )
                             (directory (merge-pathnames (format nil "lisp/asdf/*/") (homedir))))
                     (lambda (a b)
                       (loop for a- = a then (cdr a-)
                             for b- = b then (cdr b-)
                             for x = (car a-) for y = (car b-)
                             do (cond ((null x) (return nil))
                                      ((null y) (return t))
                                      ((> x y) (return t))
                                      ((< x y) (return nil)))))
                     :key (lambda (x)
                            (loop
                              for p = (position #\. x)
                              collect (parse-integer (subseq x 0 p))
                              while p
                              do (setq x (subseq x (1+ p))))))))
     t)
    ((member version '("no" "-") :test 'equal)
     (setf (config "asdf.version") nil)
     t)
    ((probe-file (merge-pathnames (format nil "lisp/asdf/~A/asdf.lisp" version) (homedir)))
     (setf (config "asdf.version") version))
    (t (error "~A not installed" version))))
