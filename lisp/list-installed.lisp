(defpackage :roswell.list.installed
  (:use :cl :roswell.util)
  (:export :installed))
(in-package :roswell.list.installed)

(defun installed (&rest r)
  (setf r (cdr r))
  (cond
    ((null r)
     (format *error-output* "Installed implementations:~%")
     (finish-output *error-output*)
     (dolist (d (directory
                 (make-pathname
                  :defaults (merge-pathnames
                             (format nil "impls/~A/~A/" (uname-m) (uname))
                             (homedir))
                  :name :wild
                  :type :wild)))
       (installed nil (first (last (pathname-directory d))))))
    (t
     (dolist (impl/version r)
       (destructuring-bind (impl version) (parse-version-spec impl/version)
         (if version
             (if (probe-file (merge-pathnames
                              (format nil "impls/~A/~A/~A/~A" (uname-m) (uname) impl version)
                              (homedir)))
                 (format t "~a~%" impl/version)
                 (format *error-output* "~a is not installed~%" impl/version))
             (progn
               (format *error-output* "~%Installed versions of ~a:~%" impl)
               (finish-output *error-output*)
               (dolist (d (directory
                           (make-pathname
                            :defaults (merge-pathnames
                                       (format nil "impls/~A/~A/~A/" (uname-m) (uname) impl)
                                       (homedir))
                            :name :wild
                            :type :wild)))
                 (format t "~{~A~^/~}~%" (last (pathname-directory d) 2))
                 (finish-output)))))))))
