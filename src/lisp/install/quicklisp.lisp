#+(#.(cl:if (cl:find-package :qlqs-http) :and :or))
(progn
  (in-package #:qlqs-http)
  ;; use roswell to download everithing.
  '(defun fetch (url file &key (follow-redirects t) quietly
                 (maximum-redirects *maximum-redirects*))
    "Request URL and write the body of the response to FILE."
    (declare (ignorable url file follow-redirects quietly
              maximum-redirects))))

(in-package #:cl-user)

(defun main (exec path &rest r)
  (declare (ignorable exec r))
  (if (find-package :quicklisp-quickstart)
      (funcall (intern (string :install) (find-package :quicklisp-quickstart)) :path path)
      (error "something wrong")))

(in-package :ros.install)

(setq *help-cmds*
      (list 'quicklisp-help))
