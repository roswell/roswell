(in-package :ros.install)
#-win32
(progn
  (warn "msys is only required on windows"))

(ros:quicklisp :environment nil)
(ql:quickload '(:cxml) :silent t)

;; experiments... on repl
;;(setq *ros-path* "C:/MinGW/msys/1.0/local/bin/ros")
;;(setq *home-path* #p"c:/Users/Vagrant/.roswell/")

;; ensure ros's path in  env PATH
;; instead of "sh" "sh -l" to use for WIN32.

(defvar *mingw-get-files* '(;;"mingw-get-0.6.2-mingw32-beta-20131004-1-bin.tar.xz"
                            ;;"mingw-get-0.6.2-mingw32-beta-20131004-1-lic.tar.xz"
                            ;;"mingw-get-setup-0.6.2-mingw32-beta-20131004-1-dll.tar.xz"
                            "mingw-get-setup-0.6.2-mingw32-beta-20131004-1-xml.tar.xz"))

(defvar *mingw* #-x86-64 "mingw32" #+x86-64 "mingw64")

(defvar *mingw-w64-files*
  #-x86-64 '("http://sourceforge.net/projects/mingw-w64/files/Toolchains%20targetting%20Win32/Personal%20Builds/mingw-builds/4.9.2/threads-win32/dwarf/i686-4.9.2-release-win32-dwarf-rt_v3-rev1.7z/download")
  #+x86-64 '("http://sourceforge.net/projects/mingw-w64/files/Toolchains%20targetting%20Win64/Personal%20Builds/mingw-builds/4.9.2/threads-win32/seh/x86_64-4.9.2-release-win32-seh-rt_v3-rev1.7z/download"))

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
     (expand path (ensure-directories-exist
                   (merge-pathnames (format nil "impls/~A/~A/~A/~A/" (uname-m) (uname) "msys" *mingw*) (homedir))))
     (format t " done ~%"))
  (let ((mingw-get (merge-pathnames (format nil "impls/~A/~A/msys/~A/bin/mingw-get" (uname-m) (uname) *mingw*)
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

(defvar *msys-meta* nil)
(defun msys-meta% ()
  (or *msys-meta*
      (setf *msys-meta*
            (loop :for file :in (directory (merge-pathnames (format nil "impls/~A/~A/msys/~A/var/lib/mingw-get/data/*.xml" (uname-m) (uname) *mingw*)
                                                            (homedir)))
               :append
               (coerce (dom:get-elements-by-tag-name(dom:first-child (cxml:parse-file (truename file) (cxml-dom:make-dom-builder))) "package") 'list)))))

(defun msys-meta (name)
  (loop :for i := -1 :then (position #\- name :start (1+ i))
     :while i
     :for c :from 0 :to 1
     :finally (when (> c 1) (setf name (subseq name 0 i))))
  (let* ((package (find name (msys-meta%)
                        :key (lambda (i) (dom:get-attribute i "name"))
                        :test 'equal))
         (uri (dom:get-attribute (aref (dom:get-elements-by-tag-name
                                        (dom:parent-node
                                         package)
                                        "download-host") 0)
                                 "uri")))
    (list :uri uri
          :tar (loop :for i :across (dom:get-elements-by-tag-name package "release")
                  :when (dom:get-attribute i "tarname")
                  :collect it)
          :requires (loop :for i :across (dom:get-elements-by-tag-name package "requires")
                       :when (dom:get-attribute i "eq")
                       :collect it))))

(defun msys-download (name &key meta path recursive (done nil))
  (let* ((meta (or meta (msys-meta name)))
         (files (or
                 (ignore-errors
                   (loop :with regex := (cl-ppcre:regex-replace-all
                                         "\\\\\\*"
                                         (cl-ppcre:quote-meta-chars
                                          (cl-ppcre:scan-to-strings
                                           "[^-]*\\-[^-]*-(.*)$"
                                           (aref (nth-value 1 (cl-ppcre:scan-to-strings "[^-]*\\-[^-]*-(.*)$" name)) 0)))
                                         ".*")
                      :for i :in (getf meta :tar)
                      :when (cl-ppcre:scan-to-strings regex i)
                      :collect i))
                 (list (first (getf meta :tar))))))
    (append
     (loop :for i :in files
        :collect (let ((path (merge-pathnames i path)))
                   (if (probe-file path)
                       (format t " ~A skip.~%" i)
                       (progn
                         (ros.install::download (cl-ppcre:regex-replace "%F" (getf meta :uri) i) path)
                         (format t " done.~%")))
                   path)))
    (when recursive
      (mapcar (lambda (x)
                (msys-download name :path path :recursive t))
              (getf meta :requires)))))


(defun msys-setup-fstab (argv)
  (let ((path (merge-pathnames (format nil "impls/~A/~A/~A/~A/~A" (uname-m) (uname) "msys" *mingw* (format nil "~A-w64-mingw32"
                                                                                                            #-x86-64 "i686"
                                                                                                            #+x86-64"x86_64")) (homedir))))
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
           (merge-pathnames (format nil "impls/~A/~A/~A/~A/msys/1.0/bin/sed" (uname-m) (uname) "msys" *mingw*) (homedir))
           (merge-pathnames (format nil "impls/~A/~A/~A/~A/msys/1.0/etc/profile" (uname-m) (uname) "msys" *mingw*) (homedir))))
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
       (expand
        (uiop/filesystem:native-namestring path)
        (uiop/filesystem:native-namestring
         (ensure-directories-exist
          (merge-pathnames (format nil "impls/~A/~A/~A/" (uname-m) (uname) "mingw") (homedir))))))
  (cons t argv))

(push `("msys" . ,(list
                   'msys-setup-msys
                   'msys-setup-mingw
                   'msys-setup-fstab
                   'msys-setup-profile))
      *install-cmds*)

(defun msys-help (argv)
  (format t "~%")
  (cons t argv))

(push `("msys" . ,(list 'msys-help)) *help-cmds*)
