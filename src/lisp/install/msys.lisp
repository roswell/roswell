(in-package :ros.install)

(defun msys-extract (argv)
  (format t "extracting msys archive.~%")
  (expand (merge-pathnames "archives/msys2-base-i686-20141113.tar.xz" (homedir))
          (merge-pathnames "impls/ho/" (homedir)))
  (format t "extracted~%")
  (cons t argv))

(defun msys-and-exit (cmd)
  (with-open-file (out (ensure-directories-exist
                        (merge-pathnames "impls/ho/msys32/etc/post-install/10msys.post"
                                         (homedir)))
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (format out "~A~%exit~%" cmd))
  (uiop/run-program:run-program
   (namestring (merge-pathnames "impls/ho/msys32/mingw32_shell.bat" (homedir)))
   :output t))

(defun msys-setup (argv)
  (format t "first launch~%")
  (msys-and-exit "")
  (format t "first done~%")
  (format t "second launch~%")
  (msys-and-exit "pacman -Syu --noconfirm")
  (format t "second done~%")
  (format t "third launch~%")
  (msys-and-exit "pacman -S mingw-w64-i686-gcc --noconfirm")
  (format t "third done~%")
  (cons t argv))

(setq *install-cmds*
      (list ;;'msys-extract
            'msys-setup))

(defun msys-help (argv)
  (format t "hoge~%")
  (cons t argv))

(setq *help-cmds*
      (list 'msys-help))
