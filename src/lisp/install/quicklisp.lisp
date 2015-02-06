#+(#.(cl:if (cl:find-package :qlqs-http) :and :or))
(progn
  (in-package #:qlqs-http)
  ;; use roswell to download everithing.
  '(defun fetch (url file &key (follow-redirects t) quietly
                 (maximum-redirects *maximum-redirects*))
    "Request URL and write the body of the response to FILE."
    (declare (ignorable url file follow-redirects quietly
              maximum-redirects))))
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
     (funcall (intern (string :install) (find-package :quicklisp-quickstart)) :path path))))
