(in-package :ros.install)

(defun sbcl-bin-help (argv)
  (format t "no options for sbcl-bin~%")
  (cons t argv))

(push `("sbcl-bin" . ,(list 'sbcl-bin-help)) *help-cmds*)
