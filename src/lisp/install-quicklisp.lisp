#+(#.(cl:if (cl:find-package :ros.install) :and :or))
(progn
  (in-package :ros.install)
  (defun quicklisp-help (argv)
    (format *error-output* "no options for quicklisp~%")
    (cons t argv))
  (setq ros.install::*help-cmds*
        (list 'quicklisp-help)))

(in-package #:cl-user)

(defun main (exec archive path &rest r)
  (declare (ignorable exec r))
  (cond
    ((probe-file path)
     (format *error-output* "Quicklisp is already setup.~%"))
    (t
     (load archive)
     ;; use roswell to download everithing.
     (progn
       (setf (fdefinition (find-symbol (string :fetch) :qlqs-http))
             (lambda (url file &key (follow-redirects t) quietly
                                 (maximum-redirects 10))
               "Request URL and write the body of the response to FILE."
               (declare (ignorable url file follow-redirects quietly
                                   maximum-redirects))
               (ros:roswell `("roswell-internal-use download" ,url ,file) :interactive nil)))
       (with-open-file (out (ensure-directories-exist (merge-pathnames "local-init/ros-download.lisp" path))
                            :direction :output :if-exists :supersede)
         (format out "~s"
                 '(setf (fdefinition (find-symbol (string :fetch) :ql-http))
                   (lambda (url file &key (follow-redirects t) quietly
                                       (maximum-redirects 10))
                     "Request URL and write the body of the response to FILE."
                     (declare (ignorable url follow-redirects quietly maximum-redirects))
                     (ros:roswell `("roswell-internal-use download"
                                    ,(funcall (find-symbol (string :urlstring) :ql-http)
                                              (funcall (find-symbol (string :url) :ql-http) url)) ,file) :interactive nil)
                     (values (make-instance (find-symbol (string :header) :ql-http) :status 200)
                             (probe-file file)))))))
     (funcall (intern (string :install) (find-package :quicklisp-quickstart)) :path path))))
