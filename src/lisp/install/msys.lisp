(in-package :ros.install)
#-win32
(progn
  (warn "msys is only required on windows")
  (ros:quit 0))

(ros:quicklisp :environment nil)

;; experiments... on repl
;;(setq *ros-path* "C:/MinGW/msys/1.0/local/bin/ros")
;;(setq *home-path* #p"c:/Users/Vagrant/.roswell/")

;; ensure ros's path in  env PATH
;; instead of "sh" "sh -l" to use for WIN32.

(defvar *mingw-get-files* '("mingw-get-0.6.2-mingw32-beta-20131004-1-bin.tar.xz"
                            "mingw-get-0.6.2-mingw32-beta-20131004-1-lic.tar.xz"
                            "mingw-get-setup-0.6.2-mingw32-beta-20131004-1-dll.tar.xz"
                            "mingw-get-setup-0.6.2-mingw32-beta-20131004-1-xml.tar.xz"))

(defvar *mingw-w64-files* '("http://sourceforge.net/projects/mingw-w64/files/Toolchains%20targetting%20Win32/Personal%20Builds/mingw-builds/4.9.2/threads-win32/dwarf/i686-4.9.2-release-win32-dwarf-rt_v3-rev1.7z/download"))

(defun un7za (path output-path)
  (uiop/run-program:run-program
   (format nil "~A x ~A -so |~A x -ttar -si -y -o~A"
           (uiop/filesystem:native-namestring (7za))
           (uiop/filesystem:native-namestring path)
           (uiop/filesystem:native-namestring (7za))
           (uiop/filesystem:native-namestring output-path))))

(defun msys-setup-fstab (argv)
  (let ((path (merge-pathnames (format nil "impls/~A/~A/~A/~A/~A/" (uname-m) (uname) "msys" "mingw32" "i686-w64-mingw32") (homedir))))
    (with-open-file (o (ensure-directories-exist (merge-pathnames "msys/1.0/etc/fstab" path))
                       :direction :output
                       :if-exists :overwrite
                       :if-does-not-exist :create)
      (format o "~A    /mingw~%" path)))
  (cons t argv))

(defun msys-setup-profile (argv)
  ;;"sed -i -e 's/^cd/#cd/g' /etc/profile"
  (uiop/run-program:run-program
   (format nil "~A -i -e 's/^cd/#cd/g' ~A"
           (merge-pathnames (format nil "impls/~A/~A/~A/~A/msys/1.0/bin/sed" (uname-m) (uname) "msys" "mingw32") (homedir))
           (merge-pathnames (format nil "impls/~A/~A/~A/~A/msys/1.0/etc/profile" (uname-m) (uname) "msys" "mingw32") (homedir))))
  (cons t argv))

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
                  (merge-pathnames (format nil "impls/~A/~A/~A/~A/" (uname-m) (uname) "msys" "mingw32") (homedir))))
     (format t " done ~%"))
  (let ((mingw-get (merge-pathnames (format nil "impls/~A/~A/msys/mingw32/bin/mingw-get" (uname-m) (uname))
                                    (homedir))))
    (flet ((install (package &key upgrade)
             (let* ((method (if upgrade "upgrade" "install"))
                    (cmd (format nil "~A ~A ~A" (namestring mingw-get)
                                 method
                                 package)))
               (format t "~A ~A" method package)
               (force-output)
               (let ()
                 (uiop/run-program:run-program cmd :output t :error-output t))
               (format t " done.~%"))))
      (install "msys-base")
      (install "msys-bash")))
  (cons t argv))

(defun msys-setup-mingw (argv)
  (loop for url in *mingw-w64-files*
     for pos2 = (position #\/ url :from-end t)
     for pos1 = (position #\/ url :from-end t :end pos2)
     for file = (subseq url (1+ pos1) pos2)
     for path = (merge-pathnames (format nil "archives/~A" file) (homedir))
     do (format *error-output* "Download ~a" file)
       (if (probe-file path)
           (format *error-output* " skip.~%")
           (ros.install::download url path))
       (uiop/run-program:run-program
        (format nil "~A x ~A -y -o~A"
               (uiop/filesystem:native-namestring (7za))
               (uiop/filesystem:native-namestring path)
               (uiop/filesystem:native-namestring
                (ensure-directories-exist
                 (merge-pathnames (format nil "impls/~A/~A/~A/" (uname-m) (uname) "msys") (homedir)))))))
  (cons t argv))

(setq *install-cmds*
      (list
       'msys-setup-msys
       'msys-setup-mingw
       'msys-setup-fstab
       'msys-setup-profile))

(defun msys-help (argv)
  (format t "~%")
  (cons t argv))

(setq *help-cmds*
      (list 'msys-help))
