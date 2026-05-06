(roswell:include "util-install")
(defpackage :roswell.install.dotcl-bin
  (:use :cl :roswell.install :roswell.util :roswell.locations))
(in-package :roswell.install.dotcl-bin)

(defun dotcl-bin-impl ()
  (merge-pathnames (format nil "impls/~A/~A/dotcl-bin/" (uname-m) (uname)) (homedir)))

(defun dotcl-bin-rid ()
  (let* ((arch (uname-m))
         (os (uname))
         (arch-key (cond ((equal arch "x86-64") "x64")
                         ((equal arch "arm64") "arm64")
                         (t (error "dotcl-bin: unsupported arch ~A" arch))))
         (os-key (cond ((equal os "linux") "linux")
                       ((equal os "darwin") "osx")
                       ((equal os "windows") "win")
                       (t (error "dotcl-bin: unsupported os ~A" os)))))
    (format nil "~A-~A" os-key arch-key)))

(defvar *dotcl-bin-version-cache* nil)

(defun dotcl-bin-get-version ()
  (or *dotcl-bin-version-cache*
      (setf *dotcl-bin-version-cache*
            (let ((file (merge-pathnames "tmp/dotcl-bin.json" (homedir))))
              (format *error-output* "Checking version to install....~%")
              (download "https://api.github.com/repos/dotcl/dotcl/releases" file)
              (with-open-file (in file)
                (loop for line = (read-line in nil nil) while line
                      for pos = (search "\"tag_name\"" line)
                      when pos
                      collect (let* ((q1 (position #\" line :start (+ pos 11)))
                                     (q2 (and q1 (position #\" line :start (1+ q1)))))
                                (when (and q1 q2)
                                  (let ((tag (subseq line (1+ q1) q2)))
                                    (if (and (plusp (length tag)) (char= (aref tag 0) #\v))
                                        (subseq tag 1)
                                        tag))))))))))

(defun dotcl-bin-argv-parse (argv)
  (set-opt "prefix" (dotcl-bin-impl))
  (cons t argv))

(defun dotcl-bin-download (argv)
  (let ((rid (dotcl-bin-rid))
        (ver (version argv)))
    (set-opt "as" ver)
    (set-opt "download.uri"
             (format nil "https://github.com/dotcl/dotcl/releases/download/v~A/dotcl-~A-~A.tar.bz2"
                     ver rid ver))
    (set-opt "download.archive"
             (let ((pos (position #\/ (opt "download.uri") :from-end t)))
               (when pos
                 (merge-pathnames (format nil "archives/~A"
                                          (subseq (opt "download.uri") (1+ pos)))
                                  (homedir)))))
    `((,(opt "download.archive") ,(opt "download.uri")))))

(defun dotcl-bin-expand (argv)
  (format t "~%Extracting archive:~A~%" (opt "download.archive"))
  (let ((dest (merge-pathnames (format nil "~A/" (opt "as")) (dotcl-bin-impl))))
    (when (probe-file dest)
      (uiop/filesystem:delete-directory-tree dest :validate t))
    (ensure-directories-exist dest)
    (expand (opt "download.archive") dest)
    #-os-windows
    (let ((apphost (merge-pathnames "runtime" dest)))
      (when (probe-file apphost)
        (uiop:run-program (list "chmod" "+x" (namestring apphost))
                          :ignore-error-status t))))
  (cons t argv))

(defun dotcl-bin-script (argv)
  (let* ((dotnet (which "dotnet"))
         (dir (merge-pathnames (format nil "~A/" (opt "as")) (dotcl-bin-impl))))
    (unless dotnet
      (format *error-output* "Error: 'dotnet' (.NET SDK 10+) was not found in PATH.~%")
      (format *error-output* "       Install .NET SDK 10 from https://dotnet.microsoft.com/download~%")
      (format *error-output* "       'ros use dotcl-bin' will fail without dotnet.~%")
      (roswell:quit 1))
    (install-script
     (merge-pathnames "dotcl" dir)
     (format nil "exec \"~Aruntime\" \"$@\"" dir))
    (cons t argv)))

(defun dotcl-bin (type)
  (case type
    (:install `(,(decide-version 'dotcl-bin-get-version)
                dotcl-bin-argv-parse
                ,(decide-download 'dotcl-bin-download)
                dotcl-bin-expand
                dotcl-bin-script
                setup))
    (:list 'dotcl-bin-get-version)))
