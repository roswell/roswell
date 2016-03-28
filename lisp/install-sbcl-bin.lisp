(in-package :ros.install)

;; sbcl-bin is specially treated. Installation are mainly done by src/cmd-install-sbcl-bin.c.

(defun sbcl-bin-help (argv)
  (format t "no options for sbcl-bin~%")
  (cons t argv))

(defun sbcl-bin-argv-parse (argv)
  (format *error-output* "~&Install Script for sbcl-bin...~%")
  (cons t argv))

(push `("sbcl-bin" . ,(list 'sbcl-bin-help)) *help-cmds*)

(push `("sbcl-bin" . (sbcl-bin-argv-parse))
      *install-cmds*)
