#+(#.(cl:if (cl:find-package :ros.install) :and :or))
(progn
  (in-package :ros.install)
  (defun quicklisp-help (argv)
    (format *error-output* "no options for quicklisp~%")
    (cons t argv))
  (push  '("quicklisp" quicklisp-help) *help-cmds*))

(in-package #:cl-user)

(defun main (exec archive path &rest r)
  (declare (ignorable exec r))
  (cond
    ((probe-file path)
     (format *error-output* "Quicklisp is already setup.~%"))
    (t
     #-win32
     (ignore-errors
       (require :sb-posix)
       (let ((gid (sb-posix:getenv "SUDO_GID")))
         (sb-posix:setgid (parse-integer gid)))
       (let ((uid (sb-posix:getenv "SUDO_UID")))
         (sb-posix:setuid (parse-integer uid))))
     (let ((*standard-output* (make-broadcast-stream)))
       (load archive))
     ;; use roswell to download everithing.
     (progn
       (setf (fdefinition (find-symbol (string :fetch) :qlqs-http))
             (lambda (url file &key (follow-redirects t) quietly
                                 (maximum-redirects 10))
               "Request URL and write the body of the response to FILE."
               (declare (ignorable url file follow-redirects quietly
                                   maximum-redirects))
               (ros:roswell `("roswell-internal-use" "download" ,url ,file) :interactive nil)))
       (with-open-file (out (ensure-directories-exist (merge-pathnames "local-init/ros-download.lisp" path))
                            :direction :output :if-exists :supersede)
         (format out "~s"
                 '(setf (fdefinition (find-symbol (string :fetch) :ql-http))
                   (lambda (url file &key (follow-redirects t) quietly
                                       (maximum-redirects 10))
                     "Request URL and write the body of the response to FILE."
                     (declare (ignorable url follow-redirects quietly maximum-redirects))
                     (ros:roswell `("roswell-internal-use" "download"
                                    ,(funcall (find-symbol (string :urlstring) :ql-http)
                                              (funcall (find-symbol (string :url) :ql-http) url)) ,file "2")
                                  #+abcl :interactive ;; intend to fix problem on abcl from commit 4a60ea0
                                  #-abcl *standard-output*)
                     (values (make-instance (find-symbol (string :header) :ql-http) :status 200)
                             (probe-file file)))))))
     (let ((*standard-output* (make-broadcast-stream)))
       (funcall (intern (string :install) (find-package :quicklisp-quickstart)) :path path)))))
