(roswell:include "list-asdf")
(defpackage :roswell.install.asdf
  (:use :cl :roswell.install :roswell.util :roswell.list.asdf :roswell.locations))
(in-package :roswell.install.asdf)

(defun help (argv)
  (format *error-output* "no options for asdf~%")
  (cons t argv))

(defun asdf-install (argv)
  (let* ((version (getf argv :version))
         (url (format nil "~A~A.tar.gz" (asdf-uri) version))
         (file (merge-pathnames (format nil "archives/asdf-~A.tar.gz" version) (homedir)))
         (src (merge-pathnames "src/" (homedir))))
    (if (= (length version) 40)
        (let ((alias (format nil "asdf-~A" version)))
          (unless (probe-file (merge-pathnames alias src))
            (clone-github "roswell" "asdf"
                          :alias alias
                          :path src :force-git t))
          (format t "git checkout ~A~%" version)
          (uiop/run-program:run-program
           (list (sh) "-lc" (format nil "cd ~S;git checkout ~A"
                                    (#+win32 mingw-namestring #-win32 princ-to-string
                                     (merge-pathnames (format nil "asdf-~A/" version) src))
                                    version))
           :output t :ignore-error-status nil))
        (progn
          (unless (probe-file file)
            (download url file))
          (expand file src)))
    (let* ((src (merge-pathnames (format nil "asdf-~A/" version) src))
           (cmd (list (sh) "-lc" (format nil "cd ~S;~A"
                                         (#+win32 mingw-namestring #-win32 princ-to-string src)
                                         "make"))))
      (uiop/os:chdir src)
      (format t "~&chdir ~A~%" src)
      (uiop/run-program:run-program cmd :output t :ignore-error-status nil)
      (let ((built (merge-pathnames "build/asdf.lisp" src)))
        (if (probe-file built)
            (progn
              (uiop/stream:copy-file
               built
               (ensure-directories-exist
                (merge-pathnames (format nil "lisp/asdf/~A/asdf.lisp" version) (homedir))))
              (setf (config "asdf.version") version))
            (error "build fail asdf ~A~%" 'version)))))
  (cons t argv))

(defun asdf (type)
  (case type
    (:help '(help))
    (:install `(,(decide-version 'asdf-get-version)
                asdf-install))))
