(in-package :ros.install)
(ql:quickload :zip)

;; experiments... on repl
;;(setq *ros-path* "C:/MinGW/msys/1.0/local/bin/ros")
;;(setq *home-path* #p"c:/Users/Vagrant/.roswell/")


;; strategy to obtain toolchain for windows
;; sbcl-bin -> quicklisp(until here should be finished when this file is loaded)
;; -> zip:unzip -> 7za.exe -> mingw-get
;; reference for mingw-get : https://chocolatey.org/packages/mingw-get ?

(defvar *7za-archive* "http://jaist.dl.sourceforge.net/project/sevenzip/7-Zip/9.20/7za920.zip")

(defun unzip (path output-path)
  (zip:unzip
   path
   output-path))

(defun msys-setup-7za (argv)
  ;;[todo] stop download if file are already exists...
  (let ((archive (let ((pos (position #\/ *7za-archive* :from-end t)))
                   (when pos
                     (merge-pathnames (format nil "archives/~A" (subseq *7za-archive* (1+ pos))) (homedir)))))
        (prefix (merge-pathnames (format nil "impls/~A/~A/~A/" (uname-m) (uname) "7za") (homedir))))
    (ros.install::download *7za-archive*
                           archive)
    (unzip archive (ensure-directories-exist prefix)))
  (cons t argv))

(setq *install-cmds*
      (list
       'msys-setup-7za))

(defun msys-help (argv)
  (format t "hoge~%")
  (cons t argv))

(setq *help-cmds*
      (list 'msys-help))
