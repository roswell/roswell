(in-package :ros.install)
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
  (zip:unzip
   path
   output-path))

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

(push `("7zip" . ,'(setup-7za)) *install-cmds*)

(defun 7z-help (argv)
  (format t "~%")
  (cons t argv))

(push `("7zip" . ,(list '7z-help)) *help-cmds*)
