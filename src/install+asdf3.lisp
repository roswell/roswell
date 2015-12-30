(in-package :ros.install)
(ros:quicklisp :environment nil)

(defvar *asdf-uri* '("https://common-lisp.net/project/asdf/asdf.lisp"
                     "https://raw.githubusercontent.com/luismbo/cl-travis/master/deps/asdf.lisp"))

(defun setup-asdf3 (argv)
  (format t "setting up ASDF3...~%")
  (let ((path (merge-pathnames "lisp/asdf3.lisp" (homedir)))
        (success t))
    (if (probe-file path)
        (format t "asdf3 already setup~%")
        (progn
          (loop for uri in *asdf-uri*
             until (or (ignore-errors(ros.install::download uri (ensure-directories-exist path))) t))
          (unless (probe-file path)
            (setq success nil))))
    (cons success argv)))

(push `("asdf3" . ,'(setup-asdf3)) *install-cmds*)

(defun asdf3-help (argv)
  (format t "~%")
  (cons t argv))

(push `("asdf3" . ,(list 'asdf3-help)) *help-cmds*)
