(defpackage :ros.list.versions
  (:use :cl :ros.util))
(in-package :ros.list.versions)

(defun versions (&rest params)
  ;; Experimental?
  (if params
      (destructuring-bind (impl version) (parse-version-spec (impl (first params)))
        (declare (ignore version))
        (format *error-output* "Installable versions for ~A:~%" impl)
        (let ((path (or (probe-file (make-pathname
                                     :defaults *load-pathname*
                                     :type "lisp"
                                     :name (format nil "install-~A" impl)))
                        (probe-file (make-pathname
                                     :defaults *load-pathname*
                                     :type "lisp"
                                     :name (format nil "install+~A" impl))))))
          (when path
            (dolist (x '("util-install" "util-install-quicklisp"))
              (ros:script nil (make-pathname :defaults *load-pathname* :name x :type "lisp")))
            (load path)
            (let ((cmd (read-from-string "ros.install::*list-cmd*")))
              (and (boundp cmd)
                   (symbol-value cmd)
                   (setq cmd (cdr (assoc (first params) (symbol-value cmd) :test 'equal)))
                   (dolist (v (funcall cmd))
                     (format t "~A~%" v)))))))
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
