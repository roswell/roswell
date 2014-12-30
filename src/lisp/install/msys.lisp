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
(defvar *mingw-get-files* '("mingw-get-0.6.2-mingw32-beta-20131004-1-bin.tar.xz"
                            "mingw-get-0.6.2-mingw32-beta-20131004-1-lic.tar.xz"
                            "mingw-get-setup-0.6.2-mingw32-beta-20131004-1-dll.tar.xz"
                            "mingw-get-setup-0.6.2-mingw32-beta-20131004-1-xml.tar.xz"))

(defun unzip (path output-path)
  (zip:unzip
   path
   output-path))

(defun msys-setup-7za (argv)
  (format t "setting up 7zip...~%")
  (let* ((pos (position #\/ *7za-archive* :from-end t))
         (archive (when pos
                    (merge-pathnames (format nil "archives/~A" (subseq *7za-archive* (1+ pos))) (homedir))))
         (pos2 (when pos
                 (position #\/ *7za-archive* :from-end t :end pos)))
         (version (when pos2 (subseq *7za-archive* (1+ pos2) pos)))
         (prefix (merge-pathnames (format nil "impls/~A/~A/~A/~A/" (uname-m) (uname) "7za" version) (homedir))))
    (if (probe-file (merge-pathnames "7za.exe" prefix))
        (format t "7zip already setup~%")
        (progn
          (ros.install::download *7za-archive* archive)
          (unzip archive (ensure-directories-exist prefix)))))
  (cons t argv))

(defun msys-setup-msys (argv)
  (loop :for file :in *mingw-get-files*
     :for path := (merge-pathnames (format nil "archives/~A" file) (homedir))
     :do
     (unless (probe-file path)
       (format t "Download ~a" file)
       (ros.install::download (format nil "~@{~A~}" "http://prdownloads.sourceforge.net/mingw/" file "?download")
                              path)
       (format t " done.~%")))
  (cons t argv))

(setq *install-cmds*
      (list
       'msys-setup-7za
       'msys-setup-msys))

(defun msys-help (argv)
  (format t "hoge~%")
  (cons t argv))

(setq *help-cmds*
      (list 'msys-help))
