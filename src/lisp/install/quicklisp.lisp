(in-package #:qlqs-http)

;; use roswell to download everithing.
'(defun fetch (url file &key (follow-redirects t) quietly
              (maximum-redirects *maximum-redirects*))
  "Request URL and write the body of the response to FILE."
  (declare (ignorable url file follow-redirects quietly
                   maximum-redirects)))

(in-package #:cl-user)

(defun main (exec path &rest r)
  (declare (ignorable exec r))
  (unless (find-package :quicklisp-quickstart)
    (error "something wrong"))
  (quicklisp-quickstart:install :path path))
