;;;/*
(roswell:include "util-install-quicklisp")
(defpackage :roswell.install.7zip+
  (:use :cl :roswell.install :roswell.util :roswell.locations))
(in-package :roswell.install.7zip+)
#-win32
(progn
  (warn "7zip is only required on windows"))

(defun 7za ()
  (let* ((uri (7za-uri))
         (pos (position #\/ uri :from-end t))
         (pos2 (when pos
                 (position #\/ uri :from-end t :end pos)))
         (pos3 (if pos2
                   (position #\/ uri :from-end t :end pos2)
                   0))
         (version (when pos2 (subseq uri (1+ pos3) pos2)))
         (prefix (merge-pathnames (format nil "impls/~A/~A/~A/~A/" (uname-m) (uname) "7za" version) (homedir))))
    (values (merge-pathnames "7za.exe" prefix) version)))

(defun unzip (path output-path)
  #+win32(zip:unzip path output-path))

(defun setup-7za (argv)
  (format t "setting up 7zip...~%")
  (multiple-value-bind (exec version) (7za)
    (let* ((prefix (make-pathname :defaults exec
                                  :name nil :type nil))
           (archive (merge-pathnames (format nil "archives/~A-~A.zip" "7za" version) (homedir))))
      (if (probe-file (merge-pathnames "7za.exe" prefix))
          (format t "7zip already setup~%")
          (progn
            (format t "archive=~A extract ~A~%" archive (7za-uri))
            (download (7za-uri) (ensure-directories-exist archive))
            (unzip archive (ensure-directories-exist prefix)))))
    (cons t argv)))

(defun 7z-help (argv)
  (format t "~%")
  (cons t argv))

(defun 7zip+ (type)
  (case type
    (:help '(7z-help))
    (:install '(setup-7za))))
#|*/
var f=WSH.Arguments(0),d=WSH.Arguments(1),
o=new ActiveXObject('Scripting.FileSystemObject'),
s=new ActiveXObject('Shell.Application');
if(!o.FolderExists(d)){o.CreateFolder(d);}
if(o.FileExists(f)){s.NameSpace(o.getFolder(d).Path).
CopyHere(s.NameSpace(o.getFile(f).Path).Items(),20);}
//|#
