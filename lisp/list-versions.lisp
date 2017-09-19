(defpackage :roswell.list.versions
  (:use :cl :roswell.util))
(in-package :roswell.list.versions)

(defun versions (&rest args)
  ;; Experimental?
  (setf args (cdr args))
  (if args
      (destructuring-bind (impl version) (parse-version-spec (impl (first args)))
        (declare (ignore version))
        (format *error-output* "Installable versions for ~A:~%" impl)
        (let ((fun (module "install" impl)))
          (and (setq fun (funcall fun :list))
               (format t "~{~A~%~}" (funcall fun)))))
      (format t "candidates for ros list versions [impl] are:~2%~{~A~%~}"
              (mapcar (lambda (x)
                        (subseq x 8))
                      (remove-if-not
                       (lambda (x)
                         (string-equal "install-" x :end2 (min 8 (length x))))
                       (mapcar #'pathname-name
                               (directory (make-pathname
                                           :defaults *load-pathname*
                                           :type "lisp"
                                           :name :wild))))))))
