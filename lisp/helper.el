(defun roswell-slime-directory ()
  (concat 
   (substring (shell-command-to-string "ros roswell-internal-use version confdir") 0 -1)
   "lisp/swank/"
   (substring (shell-command-to-string "ros config show swank.version") 0 -1)
   "/"))

(defvar roswell-slime-contribs '(slime-fancy))

(let* ((slime-directory (roswell-slime-directory)))
  (add-to-list 'load-path slime-directory)
  (require 'slime-autoloads)
  (setq slime-backend (expand-file-name "swank-loader.lisp"
                                        slime-directory))
  (setq slime-path slime-directory)
  (slime-setup roswell-slime-contribs))

(setq inferior-lisp-program "ros run")
