(in-package :ros.install)
#-win32
(error "7zip is only required on windows")

(ros:quicklisp :environment nil)
(ql:quickload :zip :silent t)

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
          (format t "archive=~A extract ~A~%" archive *7za-archive*)
          (ros.install::download *7za-archive* (ensure-directories-exist archive))
          (unzip archive (ensure-directories-exist prefix)))))
  (cons t argv)))

(setq *install-cmds*
      '(setup-7za))

(defun 7z-help (argv)
  (format t "~%")
  (cons t argv))

(setq *help-cmds*
      (list '7z-help))
