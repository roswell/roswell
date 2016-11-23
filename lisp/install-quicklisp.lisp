(in-package :ros.install)

(defun quicklisp-help (argv)
  (format *error-output* "no options for quicklisp~%")
  (cons t argv))

(defun quicklisp-patch (path)
  (with-open-file (out (ensure-directories-exist (merge-pathnames "local-init/ros-download.lisp" path))
                       :direction :output :if-exists :supersede)
    (let ((*package* (find-package :ros.install)))
      (format
       out "~@{~s~^~%~}"
       '(let ((*error-output* (make-broadcast-stream)))
         (setf (fdefinition (find-symbol (string :fetch) :ql-http))
          (lambda (url file &key (follow-redirects t) quietly
                              (maximum-redirects 10))
            "Request URL and write the body of the response to FILE."
            (declare (ignorable url follow-redirects quietly maximum-redirects))
            (ros:roswell `("roswell-internal-use" "download"
                                                  ,(funcall (find-symbol (string :urlstring) :ql-http)
                                                            (funcall (find-symbol (string :url) :ql-http) url)) ,file "2")
                         (if (find :abcl *features*)
                             :interactive *standard-output*))
            (values (make-instance (find-symbol (string :header) :ql-http) :status 200)
                    (probe-file file)))))
       '(pushnew :quicklisp-support-https *features*)
       '(in-package #:ql-dist)
       '(let ((*error-output* (make-broadcast-stream)))
         (when (or (loop for k in '(:win32 :windows :mswindows)
                      never (find k *features*))
                   (probe-file (merge-pathnames (format nil "impls/~A/windows/7za/9.20/7za.exe"
                                                        (ros:roswell '("roswell-internal-use""uname""-m") :string t))
                                                (ros:opt "homedir"))))
           (defmethod install ((release release))
             (let ((archive (ensure-local-archive-file release))
                   (output (relative-to (dist release)
                                        (make-pathname :directory
                                                       (list :relative "software"))))
                   (tracking (install-metadata-file release)))
               (ensure-directories-exist output)
               (ensure-directories-exist tracking)
               (ros:roswell `("roswell-internal-use" "tar" "-xf" ,archive "-C" ,output))
               (ensure-directories-exist tracking)
               (with-open-file (stream tracking
                                       :direction :output
                                       :if-exists :supersede)
                 (write-line (qenough (base-directory release)) stream))
               (let ((provided (provided-systems release))
                     (dist (dist release)))
                 (dolist (file (system-files release))
                   (let ((system (find-system-in-dist (pathname-name file) dist)))
                     (unless (member system provided)
                       (error "FIND-SYSTEM-IN-DIST returned ~A but I expected one of ~A"
                              system provided))
                     (let ((system-tracking (install-metadata-file system))
                           (system-file (merge-pathnames file
                                                         (base-directory release))))
                       (ensure-directories-exist system-tracking)
                       (unless (probe-file system-file)
                         (error "Release claims to have ~A, but I can't find it"
                                system-file))
                       (with-open-file (stream system-tracking
                                               :direction :output
                                               :if-exists :supersede)
                         (write-line (qenough system-file)
                                     stream))))))
               release))))))))

(defun quicklisp-argv-parse (argv)
  (set-opt "download.uri" (format nil "~A~A" (quicklisp-uri) "quicklisp.lisp"))
  (set-opt "download.archive" (merge-pathnames (format nil "archives/~A" "quicklisp.lisp") (homedir)))
  (cons t argv))

(defun quicklisp-download (argv)
  (if (or (not (probe-file (ros:opt "download.archive")))
          (ros:opt "download.force"))
      (progn
        (format t "~&Downloading archive: ~A~%" (ros:opt "download.uri"))
        (download (ros:opt "download.uri") (ros:opt "download.archive")))
      (format t "~&Skip downloading ~A.~%Specify 'download.force=t' to download again.~%"
              (ros:opt "download.uri")))
  (cons (not (ros:opt "without-install")) argv))

(defun quicklisp-install (argv)
  #-win32
  (ignore-errors
   (require :sb-posix)
   (let ((gid (sb-posix:getenv "SUDO_GID")))
     (sb-posix:setgid (parse-integer gid)))
   (let ((uid (sb-posix:getenv "SUDO_UID")))
     (sb-posix:setuid (parse-integer uid))))
  (let ((archive (ros:opt "download.archive"))
        (path (ros:opt "quicklisp")))
    (quicklisp-patch path)
    (cond
      ((probe-file (merge-pathnames "setup.lisp" path))
       (format *error-output* "Quicklisp is already setup.~%"))
      (t
       (let ((*standard-output* (make-broadcast-stream)))
         (load archive))
       ;; use roswell to download everithing.
       (setf (fdefinition (find-symbol (string :fetch) :qlqs-http))
             (lambda (url file &key (follow-redirects t) quietly
                                 (maximum-redirects 10))
               "Request URL and write the body of the response to FILE."
               (declare (ignorable url file follow-redirects quietly
                                   maximum-redirects))
               (ros:roswell `("roswell-internal-use" "download" ,url ,file) :interactive nil)))
       (let ((*standard-output* (make-broadcast-stream)))
         (funcall (intern (string :install) (find-package :quicklisp-quickstart))
                  :path path
                  :client-url (ros:opt "quicklisp.client")
                  :dist-url (ros:opt "quicklisp.dist"))))))
  (cons t argv))

(push '("quicklisp" quicklisp-help) *help-cmds*)
(push `("quicklisp" . (quicklisp-argv-parse
                       quicklisp-download
                       quicklisp-install)) *install-cmds*)
