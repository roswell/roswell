(roswell:include '("install-slime"))
(defpackage :roswell.install.sly
  (:use :cl :roswell.install :roswell.locations :roswell.util :roswell.install.slime))
(in-package :roswell.install.sly)

(defun sly-help (argv)
  (let ((s *error-output*))
    (format s "[WIP]Usage: ~A slime install/use/list/delete version ~%" (opt "wargv0")))
  (cons t argv))

(defun sly-get-version ()
  (format *error-output* "Checking version to install....~%")
  (cons "git" (cddr (github-version (sly-git-version-uri) "sly"
                  (lambda (href) (subseq href (+ 1 (position #\/ href :from-end t))))))))

(defun sly-from-git (name)
  (if (probe-file (merge-pathnames "lisp/sly/git/" (homedir)))
      () ;; tbd... checkout 
      (let* ((str (sly-git-version-uri))
             (end2 (position #\/ str :from-end t))
             (end (position #\/ str :from-end t :end end2))
             (start (position #\/ str :from-end t :end  end)))
        (clone-github
         (subseq str (1+ start) end)
         (subseq str (1+ end) end2)
         :path "lisp/sly"
         :branch (unless (equal name "git")
                   name)
         :alias "git"))))

(defun sly-install (argv)
  (let ((name (or (getf argv :version) "git")))
    (sly-from-git name)
    (setf (config "sly.version") name)
    (setf (config "emacs.type") "sly")
    (slime-write-helper))
  (cons t argv))

(defun sly (type)
  (case type
    (:help '(sly-help))
    (:install `(sly-install))
    (:list 'sly-list)))
