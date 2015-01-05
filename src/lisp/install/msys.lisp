(in-package :ros.install)
(ql:quickload :zip)

;; experiments... on repl
;;(setq *ros-path* "C:/MinGW/msys/1.0/local/bin/ros")
;;(setq *home-path* #p"c:/Users/Vagrant/.roswell/")

;; strategy to obtain toolchain for windows
;; sbcl-bin -> quicklisp(until here should be finished when this file is loaded)
;; -> zip:unzip -> 7za.exe -> mingw-get
;; memo
;; bin/mingw-get.exe install msys-base
;; bin/mingw-get.exe install mingw32-gcc
;; bin/mingw-get.exe install mingw32-autoconf (is needed?)

;; write /etc/fstab C:/Users/Vagrant/.roswell/impls/x86/windows/msys/-/     /mingw
;; ensure ros's path in  env PATH
;; instead of "sh" "sh -l" to use for WIN32.

(defvar *7za-archive* "http://sourceforge.net/projects/sevenzip/files/7-Zip/9.20/7za920.zip/download#")
(defvar *mingw-get-files* '("mingw-get-0.6.2-mingw32-beta-20131004-1-bin.tar.xz"
                            "mingw-get-0.6.2-mingw32-beta-20131004-1-lic.tar.xz"
                            "mingw-get-setup-0.6.2-mingw32-beta-20131004-1-dll.tar.xz"
                            "mingw-get-setup-0.6.2-mingw32-beta-20131004-1-xml.tar.xz"))

(defun unzip (path output-path)
  (zip:unzip
   path
   output-path))

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

(defun un7za (path output-path)
  (uiop/run-program:run-program
   (format nil "~A x ~A -so |~A x -ttar -si -y -o~A"
           (uiop/filesystem:native-namestring (7za))
           (uiop/filesystem:native-namestring path)
           (uiop/filesystem:native-namestring (7za))
           (uiop/filesystem:native-namestring output-path))))

(defun msys-setup-7za (argv)
  (format t "setting up 7zip...~%")
  (let* ((archive (7za))
         (prefix (make-pathname :defaults archive
                                :name nil :type nil)))
    (if (probe-file (merge-pathnames "7za.exe" prefix))
        (format t "7zip already setup~%")
        (progn
          (ros.install::download *7za-archive* archive)
          (unzip archive (ensure-directories-exist prefix)))))
  (cons t argv))

(defun msys-setup-fstab (argv)
  (let ((path (merge-pathnames (format nil "impls/~A/~A/~A/~A/" (uname-m) (uname) "msys" "-") (homedir))))
    (with-open-file (o (merge-pathnames "msys/1.0/etc/fstab" path)
                       :direction :output
                       :if-exists :overwrite
                       :if-does-not-exist :create)
      (format o "~A    /mingw~%" path)))
  (cons t argv))
;;(msys-setup-fstab t)

(defun msys-setup-msys (argv)
  (loop :for file :in *mingw-get-files*
     :for path := (merge-pathnames (format nil "archives/~A" file) (homedir))
     :do
     (format t "Download ~a" file)
     (if (probe-file path)
         (format t " skip.~%")
         (progn
           (ros.install::download (format nil "~@{~A~}" "http://prdownloads.sourceforge.net/mingw/" file "?download")
                                  path)
           (format t " done.~%")))
     (format t "extract")
     (un7za path (ensure-directories-exist
                  (merge-pathnames (format nil "impls/~A/~A/~A/~A/" (uname-m) (uname) "msys" "-") (homedir))))
     (format t " done ~%"))
  (cons t argv))

(setq *install-cmds*
      (list
       'msys-setup-7za
       'msys-setup-msys
       'msys-setup-fstab))

(defun msys-help (argv)
  (format t "~%")
  (cons t argv))

(setq *help-cmds*
      (list 'msys-help))
