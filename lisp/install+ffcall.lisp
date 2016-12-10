(in-package :ros.install)

(defvar *ffcall-version* "1.10")
(defun ffcall-setup (argv)
  (let* ((uri (format nil "~Affcall-~A.tar.gz" (ffcall-uri) *ffcall-version*))
         (pos (position #\/ uri :from-end t))
         (path (merge-pathnames (format nil "archives/~A" (subseq uri (1+ pos))) (homedir)))
         (expand-dir (merge-pathnames (format nil "src/ffcall-~A/" *ffcall-version*) (homedir))))
    (if (or (not (probe-file path))
            (opt "download.force"))
        (progn
          (format *error-output* "~&Downloading archive: ~A~%" uri)
          (force-output *error-output*)
          (download uri path)
          (format *error-output* " done.~%"))
        (format *error-output* "~&Skip downloading ~A.~%Specify 'download.force=t' to download it again.~%"
                uri))
    (expand path (ensure-directories-exist (merge-pathnames "src/" (homedir))))
    (format t "~&configure~%")
    (with-open-file (out (ensure-directories-exist
                          (merge-pathnames "impls/log/ffcall/config.log" (homedir)))
                         :direction :output :if-exists :append :if-does-not-exist :create)
      (format out "~&--~&~A~%" (date))
      (let* ((cmd (format nil "./configure '--prefix=~A'"
                          (ensure-directories-exist(merge-pathnames (format nil "lib/~A/~A/~A/~A/" (uname-m) (uname) "ffcall" *ffcall-version*) (homedir)))))
             (*standard-output* (make-broadcast-stream out #+sbcl(make-instance 'count-line-stream))))
        (ros.util:chdir expand-dir)
        (uiop/run-program:run-program cmd :output t :ignore-error-status t)))
    (format t "~&make~%")
    (with-open-file (out (ensure-directories-exist
                          (merge-pathnames "impls/log/ffcall/make.log" (homedir)))
                         :direction :output :if-exists :append :if-does-not-exist :create)
      (format out "~&--~&~A~%" (date))
      (let* ((cmd (format nil "make"))
             (*standard-output* (make-broadcast-stream out #+sbcl(make-instance 'count-line-stream))))
        (uiop/run-program:run-program cmd :output t :ignore-error-status t)))
    (format t "~&install~%")
    (with-open-file (out (merge-pathnames "impls/log/ffcall/install.log" (homedir))
                         :direction :output :if-exists :append :if-does-not-exist :create)
      (format out "~&--~&~A~%" (date))
      (let ((*standard-output* (make-broadcast-stream
                                out #+sbcl(make-instance 'count-line-stream))))
        (uiop/run-program:run-program "make install" :output t))
      (format *error-output* "done.~%")))
  (cons t argv))

(push `("ffcall+" . ,(list
                     'ffcall-setup))
      *install-cmds*)

(defun ffcall-help (argv)
  (format t "~%")
  (cons t argv))

(push `("ffcall+" . ,(list 'ffcall-help)) *help-cmds*)
