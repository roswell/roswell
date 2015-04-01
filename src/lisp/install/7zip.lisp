(in-package :ros.install)
#-win32
(progn
  (warn "7zip is only required on windows")
  (ros:quit 0))

(ros:quicklisp :environment nil)
(ql:quickload :zip :silent t)

(defvar *7za-archive* "http://sourceforge.net/projects/sevenzip/files/7-Zip/9.20/7za920.zip/download#")
(defun 7za ()
    (let* ((pos (position #\/ *7za-archive* :from-end t))
           (pos2 (when pos
                   (position #\/ *7za-archive* :from-end t :end pos)))
           (pos3 (if pos2
                     (position #\/ *7za-archive* :from-end t :end pos2)
                   0))
           (version (when pos2 (subseq *7za-archive* (1+ pos3) pos2)))
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
