(defun roswell-configdir ()
  (substring (shell-command-to-string "ros roswell-internal-use version confdir") 0 -1))

(defun roswell-load (system)
  (let ((result (substring (shell-command-to-string
                            (concat "ros -L sbcl-bin -e \"(format t \\\"~A~%\\\" (uiop:native-namestring (ql:where-is-system \\\""
                                    system
                                    "\\\")))\"")) 0 -1)))
    (unless (equal "NIL" result)
      (load (concat result "roswell/elisp/init.el")))))

(defun roswell-opt (var)
  (with-temp-buffer
    (insert-file-contents (concat (roswell-configdir) "config"))
    (goto-char (point-min))
    (re-search-forward (concat "^" var "\t[^\t]+\t\\(.*\\)$"))
    (match-string 1)))

(defun roswell-directory (type)
  (concat
   (roswell-configdir)
   "lisp/"
   type
   "/"
   (roswell-opt (concat type ".version"))
   "/"))

(defvar roswell-slime-contribs '(slime-fancy))

(let ((type (or (ignore-errors (roswell-opt "emacs.type")) "slime")))
  (cond ((equal type "slime")
         (let ((slime-directory (roswell-directory type)))
           (add-to-list 'load-path slime-directory)
           (require 'slime-autoloads)
           (setq slime-backend (expand-file-name "swank-loader.lisp"
                                                 slime-directory))
           (setq slime-path slime-directory)
           (slime-setup roswell-slime-contribs)))
        ((equal type "sly")
         (add-to-list 'load-path (roswell-directory type))
         (require 'sly-autoloads))))

(setq inferior-lisp-program "ros run")
