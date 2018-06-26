(roswell:include () "util")
(defpackage :roswell.util
  (:use :cl)
  (:import-from :ros :opt :ensure-asdf)
  (:export
   :uname :uname-m :homedir :config :impl :which :config-env :checkoutdir
   :parse-version-spec :download :expand :sh :chdir :system :module
   :core-extention :clone-github :opt :read-call :set-opt :copy-dir
   :roswell-installable-searcher :setenv :unsetenv :ensure-asdf :split-sequence))
(in-package :roswell.util)

(defun split-sequence (del seq &key end start (test #'eql) &allow-other-keys)
  (loop with from = (or start 0)
     with length = (or end (length seq))
     for to from from
     when (or (= length to)
              (funcall test (elt seq to) del))
     collect (prog1
                 (subseq seq from to)
               (setq from (1+ to)))
     until (= length to)))

(defun setenv (name value)
  (declare (ignorable name value))
  #+sbcl(funcall (read-from-string "sb-posix:setenv") name value 1)
  #+ccl(ccl:setenv name value t)
  #+clasp(ext:setenv name value t)
  #+clisp(system::setenv name value)
  #+cmucl(let ((f (ignore-errors (symbol-function (read-from-string "unix:unix-setenv")))))
           (when f (funcall f name value 1)))
  #+ecl(ext:setenv name value)
  #+lispworks (hcl:setenv name name value)
  value)

(defun unsetenv (name)
  (declare (ignorable name))
  #+sbcl(funcall (read-from-string "sb-posix:unsetenv") name)
  #+ccl(ccl:unsetenv name)
  #+clisp(system::setenv name nil)
  #+cmucl(let ((f (ignore-errors (symbol-function (read-from-string "unix:unix-unsetenv")))))
           (when f (funcall f name)))
  #+lispworks(hcl:setenv name nil)
  nil)

(defun read-call (func &rest params)
  "FUNC is a string containing a symbol name of a function.
Returns NIL when the package in the symbol prefix is not available."
  (ignore-errors (apply (let (*read-eval*) (read-from-string func)) params)))

(defun copy-dir (from to)
  (when (wild-pathname-p from)
    (error "wild card not supported"))
  (loop with path = (truename from)
        for l in (delete-if (lambda (x) (eql :absolute (first (pathname-directory (make-pathname :defaults x)))))
                            (mapcar (lambda(x) (enough-namestring (namestring x) path))
                                    (directory (merge-pathnames "**/*.*" path))))
        do (if (or (pathname-name l)
                   (pathname-type l))
               (ignore-errors
                (read-call "uiop:copy-file"
                           (merge-pathnames l path)
                           (ensure-directories-exist (merge-pathnames l to)))))))

(defun module (prefix name)
  "Load external system"
  (and (loop for c across "/\\"
             never (find c name))
       (let ((imp (format nil "roswell.~A.~A" prefix name)))
         (or #1=(ignore-errors
                 (let (*read-eval*)
                   (read-from-string (format nil "~A::~A" imp name))))
             (progn
               (read-call "ql:register-local-projects")
               (or
                (and (or (read-call "ql-dist:find-system" imp)
                         (read-call "ql:where-is-system" imp))
                     (read-call "ql:quickload" imp :silent t))
                (roswell:include (format nil "~A-~A" prefix name)))
               #1#)))))

(defun set-opt (item val)
  (let ((found (assoc item (roswell::ros-opts) :test 'equal)))
    (if found
        (setf (second found) val)
        (push (list item val) roswell::*ros-opts*))))

(defun uname ()
  "Returns uname. Example: linux , darwin"
  (roswell:opt "uname"))

(defun uname-m ()
  "Returns the machine type as a string. Example: x86-64"
  (roswell:opt "uname-m"))

(defun homedir ()
  "Returns the user-level installation directory of roswell. Example: /home/user/.roswell"
  (or (ignore-errors (probe-file (merge-pathnames ".roswell/" *default-pathname-defaults*)))
      (opt "homedir")))

(defun impl (imp)
  "Returns a full name/version of an implementation. Default: current system.
Example:
  (impl) -> sbcl/1.3.2 (current system)
  (impl \"ccl\") -> ccl/system (currently installed system) "
  (roswell:roswell `("roswell-internal-use" "impl" ,(or imp "")) :string t))

(defun which (cmd)
  "equivalent to \"which\" command on shell"
  (let ((result (roswell:roswell `("roswell-internal-use" "which" ,cmd) :string t)))
    (unless (zerop (length result))
      result)))

(defvar *backslash-encode-assoc*
  '((#\Space . #\_)
    (#\\ . #\\)
    (#\Newline . #\n)
    (#\Return . #\r)))

(defun backslash-encode (string)
  (loop
    for i across string
    for c = (cdr (assoc i *backslash-encode-assoc*))
    when c collect #\\ into r
    when c collect it into r
    unless c collect i into r
    finally (return (coerce r 'string))))

(defun download (uri file &key proxy (verbose nil) (output :interactive))
  "Interface to curl4 in the roswell C binary"
  (declare (ignorable proxy))
  (ensure-directories-exist file)
  (roswell:roswell `("roswell-internal-use" "download" ,(backslash-encode uri)
                     ,file ,@(when verbose (list verbose)))
                   output nil))

(defun expand (archive dest &key verbose)
  "Interface to the roswell C binary"
  #+win32
  (progn
    (roswell:include "install+7zip")
    (unless (probe-file (read-call "roswell.install.7zip+::7za"))
      (roswell:roswell '("install 7zip+"))))
  (roswell:roswell `(,(if verbose "-v" "")"roswell-internal-use tar" "-xf" ,archive "-C" ,dest)
                   (or #-win32 :interactive nil) nil))

(defun core-extention (&optional (impl (opt "impl")))
  "Interface to the roswell C binary. Returns \"core\" on sbcl."
  (roswell:roswell `("roswell-internal-use" "core-extention" ,impl) :string t))

(defun config (c)
  "Interface to roswell C binary."
  (let ((result (roswell:roswell `("config" "show" ,c) :string t)))
    (unless (zerop (length result)) result)))

(defun (setf config) (val item &key where)
  "Interface to roswell C binary."
  (roswell:roswell
   `(,@(when (stringp where)
             `("-N" ,where))
       "config" ,@(cond
                   ((stringp where) `("--env"))
                   (where `(,(format nil "--~(~A~)" where))))
       ,(if val "set" "unset")
       ,item
       ,@(when val `(,val)))
       :string t)
  val)

(defun chdir (dir &optional (verbose t))
  (when verbose
    (format t "~&chdir ~A~%" dir))
  (funcall (intern (string :chdir) :uiop/os) dir))

(defun sh ()
  (or #+win32
      (unless (roswell:getenv "MSYSCON")
        (format nil "~A" (#+sbcl sb-ext:native-namestring #-sbcl namestring
                          (merge-pathnames (format nil "impls/~A/~A/msys2/~A/usr/bin/bash" (uname-m) (uname)
                                                   (config "msys2.version")) (homedir)))))
      (which "bash")
      "sh"))

(defvar *version*
  `(:roswell ,(or (ignore-errors (roswell:version))
                  (progn
                    (pushnew :ros.without-bin *features*)
                    (read-call "asdf:component-version"
                               (read-call "asdf:find-system" :roswell))))
    :lisp ,(lisp-implementation-type)
    :version ,(lisp-implementation-version))
  "Stores the version information for roswell binary and the current implementation.")

(defun parse-version-spec (string)
  "Parse the given version specification string and returns a list of strings (LISP VERSION).
If it does not contain a version substring, VERSION becomes a null.
If it is a version string only (detected when it starts from digit-char), LISP becomes NIL.
Examples:
ccl-bin/1.11 -> (\"ccl-bin\" \"1.11\")
ccl-bin      -> (\"ccl-bin\" nil)
1.11         -> (nil \"1.11\")
"
  (let ((pos (position #\/ string)))
    (if pos
        `(,(subseq string 0 pos) ,(subseq string (1+ pos)))
        (if (digit-char-p (aref string 0))
            `(nil ,string)
            `(,string nil)))))

(defun checkoutdir ()
  "Returns the parent directory of the first local project directory in ql:*local-project-directories*."
  ;; see roswell:quicklisp for why.
  (roswell:quicklisp)
  (let* ((* (read-from-string "ql:*local-project-directories*"))
         (* (first (symbol-value *)))
         (* (merge-pathnames "../" *)))
    (truename *)))

(defun clone-github (owner name &key
                                (alias (format nil "~A/~A" owner name))
                                branch force-git
                                (path "templates")
                                (home (checkoutdir)))
  (format *error-output* "Installing from github ~A/~A~%" owner name)
  (if (or force-git (which "git"))
      (let ((dir (merge-pathnames (format nil "~A/~A/" path alias) home)))
        (setq branch (if branch (format nil "-b ~A" branch) ""))
        (if (funcall (intern (string :probe-file*) :uiop) dir)
            ()
            (funcall (intern (string :run-program) :uiop)
                     (format nil "git clone ~A https://github.com/~A/~A.git ~A"
                             branch
                             owner name
                             (namestring (ensure-directories-exist dir))))))
      (let* ((path/ (merge-pathnames (format nil "~A/~A.tgz" path alias) home))
             (dir (merge-pathnames ".expand/" (make-pathname :defaults path/ :name nil :type nil))))
        (funcall (intern (string :delete-directory-tree) :uiop) dir :if-does-not-exist :ignore :validate t)
        (setq branch (or branch "master"))
        (download (format nil "https://github.com/~A/archive/~A.tar.gz" alias branch) path/)
        (expand path/ (ensure-directories-exist dir))
        (rename-file (first (directory (merge-pathnames "*/" dir)))
                     (merge-pathnames (format nil "~A/~A/" path alias) home))
        (delete-file path/)
        (funcall (intern (string :delete-directory-tree) :uiop) dir :if-does-not-exist :ignore :validate t)
        t)))

(defun config-env ()
  "Set up several environment variables for Windows. No effect on linux and osx."
  #+win32
  (let* ((w (opt "wargv0"))
         (a (opt "argv0"))
         (path (read-call "uiop:native-namestring"
                          (make-pathname :type nil :name nil :defaults (if (zerop (length w)) a w))))
         (uname-m (uname-m))
         (bits (or
                (and (roswell:opt "32") 32)
                (and (roswell:opt "64") 64)
                (cond
                  ((equal uname-m "x86-64") 64)
                  ((equal uname-m "x86") 32)))))
    (unless (roswell:getenv "MSYSTEM")
      (setenv "PATH" (format nil "~{~A;~}~A"
                             (mapcar (lambda (x)
                                       (read-call "uiop:native-namestring"
                                                  (merge-pathnames
                                                   (format nil "impls/~A/~A/msys2/~A/~A"
                                                           (uname-m) (uname) (config "msys2.version") x)
                                                   (homedir))))
                                     `(,(format nil "mingw~A/bin" bits)
                                       "usr/local/bin"
                                       "usr/bin"))
                             (roswell:getenv "PATH"))))
    (setenv "MSYSTEM" (if (= bits 32) "MINGW32" "MINGW64"))
    (setenv "MSYS2_PATH_TYPE" "inherit")
    (setenv "PATH" (format nil "~A;~A\\.roswell\\bin;~A"
                           (subseq path 0 (1- (length path)))
                           (roswell:getenv "USERPROFILE")
                           (roswell:getenv "PATH")))))
