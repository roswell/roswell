(in-package :ros.install)

(defun sbcl-bin-help (argv)
  (format t "no options for sbcl-bin~%")
  (cons t argv))

(setq *help-cmds*
      (list 'sbcl-bin-help))
