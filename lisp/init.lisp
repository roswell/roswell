
#|

The entry of all roswell commands.
The true internal entry invoked by the C binary is roswell:run.
(to see how this function is invoked, consult ros.c, cmd-run-sbcl.c etc.)
All roswell commands are dispatched from this function via the symbol lookup.

For example, -sp SP or --system-package SP will cause the roswell C binary
to invoke #'system-package.

This file also contains many functions which can be otherwise delegated to
UIOP, such as run-program, quit, getenv. This is because UIOP may not be
available in older implementations. Similarly, older implementations do not
have the latest asdf, and this file has a workaround for this.

|#


(cl:in-package :cl-user)

(let ((*standard-output* (make-broadcast-stream)))
  #+sbcl (require :sb-posix))

(defpackage :roswell
  (:use :cl)
  (:nicknames :ros)
  (:shadow :load :eval :package :restart :print :write)
  (:export :run :*argv* :*main* :*load* :*cmd* :quit :script :quicklisp :getenv :opt
           :ignore-shebang :asdf :include :ensure-asdf :revert-extension
           :roswell :exec :setenv :unsetenv :version :swank :verbose :*init-hook*)
  (:documentation "Roswell backend."))

(in-package :roswell)
(defvar *init-hook* nil)
(defparameter *argv* nil)
(defparameter *ros-opts* nil)
(defparameter *main* nil)
(defvar *cmd* nil)
(defparameter *load* `((identity . cl:load)))

;; small tools
(defun getenv (x)
  #+abcl(extensions:getenv x)
  #+ccl(ccl:getenv x)
  #+clasp(ext:getenv x)
  #+clisp(ext:getenv x)
  #+cmucl
  (let ((f (ignore-errors (symbol-function (read-from-string "unix:unix-getenv")))))
    (if f
        (funcall f x)
        (cdr (assoc x ext:*environment-list* :test #'string=))))
  #+ecl(ext:getenv x)
  #+mkcl(mkcl:getenv x)
  #+sbcl(sb-posix:getenv x)
  #+allegro(sys:getenv x)
  #+lispworks(hcl:getenv x)
  #-(or abcl ecl ccl clisp sbcl cmucl allegro clasp lispworks mkcl)
  (when (find :asdf *features*)
    (funcall (read-from-string "asdf::getenv") x)))

(defun ros-opts (&optional append)
  (when append
    (setf *ros-opts* (append append (ros-opts))))
  (or *ros-opts*
      (ignore-errors
       (setf *ros-opts*
             (let((*read-eval*))
               (or (read-from-string (getenv "ROS_OPTS"))
                   '()))))))

(defun opt (param &key from-end)
  (second (assoc param
                 (funcall (if from-end #'reverse #'identity)
                          (ros-opts))
                 :test 'equal)))

(defun verbose ()
  (and (opt "verbose")
       (let ((ret (parse-integer (opt "verbose"))))
         (if (zerop ret)
             nil ret))))

(let (sentinel)
  (defun ensure-asdf (&key (version (opt "asdf.version")))
    (let ((*error-output* (if (verbose)
                              *error-output*
                              (make-broadcast-stream))))
      (setf sentinel
            (or sentinel
                (when (and version
                           (and (find :asdf *features*)
                                (not (or (equal version
                                                (funcall (read-from-string "asdf:asdf-version")))
                                         (= (length version) 40)))))
                  (funcall 'asdf :no-download t))
                (find :asdf *features*)
                (ignore-errors (require "asdf")))
            sentinel (not (not sentinel))))))

#+(and unix sbcl) ;; from swank
(progn
  (sb-alien:define-alien-routine ("execvp" %execvp) sb-alien:int
    (program sb-alien:c-string)
    (argv (* sb-alien:c-string)))
  (defun execvp (program args)
    "Replace current executable with another one."
    (let ((a-args (sb-alien:make-alien sb-alien:c-string
                                       (+ 1 (length args)))))
      (unwind-protect
           (progn
             (loop for index from 0 by 1
                   and item in (append args '(nil))
                   do (setf (sb-alien:deref a-args index)
                            item))
             (when (minusp
                    (%execvp program a-args))
               (let ((errno (sb-impl::get-errno)))
                 (case errno
                   (2 (error "No such file or directory: ~S" program))
                   (otherwise
                    (error "execvp(3) failed. (Code=~D)" errno))))))
        (sb-alien:free-alien a-args)))))

(defun quit (&optional (return-code 0) &rest rest)
  (let ((ret (or (and (numberp return-code) return-code) (first rest) 0)))
    #+sbcl (ignore-errors (funcall (read-from-string "cl-user::exit") :code ret))
    #+sbcl (ignore-errors (funcall (read-from-string "cl-user::quit") :unix-status ret))
    #+clasp (core:quit ret)
    #+clisp (ext:exit ret)
    #+ccl (ccl:quit ret)
    #+mkcl (mkcl:quit :exit-code ret)
    #+cmucl (progn (finish-output) (finish-output *error-output*) (unix:unix-exit ret))
    #+lispworks (lw:quit :status ret)
    (ignore-errors
     (progn (ensure-asdf)
            (funcall (read-from-string "asdf::quit") ret)))
    t))

(defun run-program (args &key output)
  (if (ignore-errors #1=(read-from-string "uiop/run-program:run-program"))
      (funcall #1# (format nil "~{~A~^ ~}" args)
               :output output
               #+(and sbcl win32) :force-shell #+(and sbcl win32) nil
               :error-output :interactive)
      (with-output-to-string (out)
        #+sbcl(funcall (read-from-string "sb-ext:run-program")
                       (first args) (mapcar #'princ-to-string (rest args))
                       :output out)
        #+clisp(if (eql output :string)
                   (format nil "~{~A~%~}"
                           (loop with i = (ext:run-shell-command (format nil "~{~A~^ ~}" args) :output :stream)
                                 for line = (read-line i nil nil)
                                 while line
                                 collect line))
                   (ext:run-shell-command (format nil "~{~A~^ ~}" args))))))

(defun exec (args)
  #+(and unix sbcl)
  (execvp (first args) args)
  #+(and unix ccl)
  (ignore-errors
   (ccl:with-string-vector (argv args) (ccl::%execvp argv)))
  (quit (run-program args :output :interactive)))

(defun quicklisp (&key path (environment "QUICKLISP_HOME"))
  "Corresponds to -Q command. Finds and loads setup.lisp for quicklisp, and adds appropriate local-project paths."
  (unless (find :quicklisp *features*)
    (let ((path (make-pathname
                 :name "setup"
                 :type "lisp"
                 :defaults (or path
                               (and environment (getenv environment))
                               (opt "quicklisp"))))
          (local (ignore-errors
                  (truename
                   (merge-pathnames
                    ".roswell/local-projects/"
                    *default-pathname-defaults*)))))
      (when (probe-file path)
        (cl:load path :verbose (verbose))
        (unless (getenv environment)
          (loop with symbol = (read-from-string "ql:*local-project-directories*")
                ;; ql:*local-project-directories* defaults to a list of a single pathname,
                ;; which is <directory containing setup.lisp>/local-projects/ .
                for path in `(;; Searches local-project/ in the current directory
                              ,local
                              ;; This is WHAAAAAT????
                              ,(ignore-errors
                                (truename (merge-pathnames "../../../local-projects/" (first (symbol-value symbol)))))
                              ;; Searches local-project/ in e.g. ~/.roswell/
                              ,(ensure-directories-exist (merge-pathnames "local-projects/" (opt "homedir"))))
                for probe = (and path (or (ignore-errors (probe-file path))
                                          #+clisp(ext:probe-directory path)))
                when probe
                do (set symbol (cons path (symbol-value symbol)))
                until probe))
        t))))

(defvar *included-names* '("init"))
(defparameter *include-path* (or #.*compile-file-pathname* *load-pathname*))

(defun include (names &optional provide)
  "CL:LOAD the files in the same directory as this init.lisp.
The same file never loaded twice.
NAMES is a list of lisp file name strings without extension.
PROVIDE is a string used for grouping/naming a set of included files.
PROVIDE is just a marker and does not correspond to a file.
As a hacky side effect, files with the same name as PROVIDE is not loaded.
"
  (loop
    for name in `(,provide
                  ,@(if (listp names)
                        names
                        (list names)))
    for path = (make-pathname
                :defaults *include-path*
                :name name :type "lisp")
    do (when (and name (not (member name *included-names* :test 'string=)))
         (push name *included-names*)
         (when (and (probe-file path)
                    (not (equal provide name)))
           (funcall 'load path)))))

(defun swank (&rest params)
  (include "util-swank")
  (apply (read-from-string "roswell.util.swank:swank") params))

(defun shebang-reader (stream sub-character infix-parameter)
  (declare (ignore sub-character infix-parameter))
  (loop for x = (read-char stream nil nil)
        until (or (not x) (eq x #\newline)))
  (values))

(defun ignore-shebang ()
  (set-dispatch-macro-character #\# #\! #'shebang-reader))

(defun roswell (args &optional (output :string) trim)
  (let* ((a0 (funcall (or #+win32(lambda (x) (substitute #\\ #\/ x)) #'identity)
                      (if (zerop (length (opt "wargv0")))
                          (opt "argv0")
                          (opt "wargv0"))))
         (ret (run-program (cons a0 args) :output output)))
    (if trim
        (remove #\Newline (remove #\Return ret))
        ret)))

(defvar *downloaded-asdf-loaded* nil)
(defun asdf (&key no-download)
  (setf *downloaded-asdf-loaded*
        (or *downloaded-asdf-loaded*
            (let* ((version-installed (opt "asdf.version"))
                   (version (or version-installed
                                (unless no-download
                                  (roswell '("install" "asdf"))
                                  (roswell '("config" "show" "asdf.version") :string t))))
                   (path (merge-pathnames (format nil "lisp/asdf/~A/asdf.lisp" version) (opt "homedir"))))
              (when (equal version "NIL")
                (error "asdf download error?"))
              (let ((fasl (make-pathname :defaults path :type (substitute #\- #\/ (substitute #\_ #\. (opt "impl"))))))
                (when (verbose)
                  (format *error-output* "asdf fasl:~A~%" fasl))
                (unless (probe-file fasl)
                  (roswell `(,@(when (verbose) '("-v")) "-L" ,(opt "impl") "compile-file" "-asdf" ,version) :string t))
                (setf path (or (probe-file fasl) path)))
              (when (probe-file path)
                (ignore-errors
                 (locally
                     (declare #+sbcl(sb-ext:muffle-conditions sb-kernel:redefinition-warning))
                   (handler-bind
                       (#+sbcl(sb-kernel:redefinition-warning #'muffle-warning))
                     (funcall 'load path)))))))))

(let ((symbol (ignore-errors (read-from-string "asdf::*user-cache*")))
      (impl (substitute #\- #\/ (opt "impl"))))
  (when (opt "asdf.version")
    (setq impl (format nil "~A-~A" impl (opt "asdf.version"))))
  (when (and symbol (boundp symbol))
    (cond ((listp (symbol-value symbol))
           (set symbol (append (symbol-value symbol) (list impl))))
          ((pathnamep (symbol-value symbol))
           (set symbol (merge-pathnames (format nil "~A/" impl) (symbol-value symbol))))
          (t (cl:print "tbd.....")))))

(defparameter *version-cache* nil)
(defun version (&optional opt)
  (if opt
      (roswell `("roswell-internal-use" "version" ,(string-downcase (princ-to-string opt))) :string t)
      (or *version-cache*
          (setf *version-cache* (roswell '("roswell-internal-use" "version") :string t)))))

#+sbcl
(when (ignore-errors (string-equal (opt "impl") "sbcl-bin" :end1 8))
  (flet ((source (version)
           (let ((path (merge-pathnames (format nil "src/sbcl-~A/" version) (opt "homedir"))))
             (when (probe-file path)
               (sb-ext:set-sbcl-source-location path)))))
    (or (source (lisp-implementation-version))
        (source "git"))))

(defun source-registry (arg &rest rest)
  (declare (ignorable rest))
  (let ((dir (format nil "~{~A~^:~}"
                     (loop for i = arg then (subseq i (1+ pos))
                           for pos = (position #\: i)
                           for part = (if pos (subseq i 0 pos) i)
                           when (and (not (zerop (length part)))
                                     (probe-file part))
                           collect (namestring (probe-file part))
                           while pos))))
    (if (zerop (length dir))
        (warn "Source-registry ~S is invalid. Ignored." arg)
        (funcall (read-from-string "asdf:initialize-source-registry") dir))))

(defun system (args &rest rest)
  (declare (ignorable rest))
  (ensure-asdf)
  (unless (find :asdf *features*)
    (error "Can't find asdf to load system"))
  (loop for ar = args then (subseq ar (1+ p))
        for p = (position #\, ar)
        for arg = (if p (subseq ar 0 p) ar)
        do (if (find :quicklisp *features*)
               (funcall (read-from-string "ql:quickload") arg :silent (not (verbose)))
               (funcall (read-from-string "asdf:operate") (read-from-string "asdf:load-op") arg))
        while p))

(setf (fdefinition 'load-system)
      #'system)

(defun package (arg &rest rest)
  (declare (ignorable rest))
  (setq *package* (find-package (read-from-string (format nil "#:~A" arg)))))

(defun system-package (arg &rest rest)
  (declare (ignorable rest))
  (apply #'system arg rest)
  (apply #'package arg rest))

(defun eval (arg &rest rest)
  (declare (ignorable rest))
  (loop with start = 0
        with end = (gensym)
        with exp
        do (multiple-value-setq (exp start)
             (read-from-string arg nil end :start start))
        until (eql exp end)
        do (cl:eval exp)))

(defun restart (arg &rest rest)
  (declare (ignorable rest))
  (funcall (read-from-string arg)))

(defun entry (arg &rest rest)
  (declare (ignorable rest))
  (let* ((sym (read-from-string arg))
         (*package* (symbol-package sym)))
    (quit (apply sym *argv*))))

(setf (fdefinition 'init) #'eval)

(defun print (arg &rest rest)
  (declare (ignorable rest))
  (cl:print (cl:eval (read-from-string arg))))

(defun write (arg &rest rest)
  (declare (ignorable rest))
  (cl:write (cl:eval (read-from-string arg))))

(defun script (arg &rest rest)
  "load and evaluate the script"
  (setf *argv* rest)
  (flet ((body (in)
           (let ((line(read-line in))
                 *ros-opts*)
             (push :ros.script *features*)
             (locally
                 (declare #+sbcl(sb-ext:muffle-conditions sb-kernel:redefinition-warning))
               (handler-bind
                   (#+sbcl(sb-kernel:redefinition-warning #'muffle-warning))
                 (funcall #+(or sbcl clisp) 'cl:load
                          #-(or sbcl clisp)
                          (progn
                            (ensure-asdf)
                            (read-from-string "asdf::eval-input"))
                          (make-concatenated-stream
                           (make-string-input-stream
                            (format nil "(cl:setf cl:*load-pathname* ~S cl:*load-truename* (ignore-errors (truename cl:*load-pathname*)))~A"
                                    (ignore-errors (merge-pathnames (make-pathname :defaults arg)))
                                    (if (equal (subseq line 0 (min (length line) 2)) "#!")
                                        "" line)))
                           in
                           (make-string-input-stream
                            (if (eql *cmd* :script)
                                "(roswell:quit (cl:apply 'main roswell:*argv*))"
                                "(setf roswell:*main* 'main)"))))))
             (setf *features* (remove :ros.script *features*)))))
    (if (streamp arg)
        (body arg)
        (if (probe-file arg)
            (with-open-file (in arg)
              (body in))
            (format t "script ~S does not exist~%" arg)))))

(defun stdin (arg &rest rest)
  (declare (ignorable arg rest))
  (apply #'script :script *standard-input* rest))

(defun load (file &rest rest)
  (let ((function (rest (find-if (lambda (x) (funcall (first x) file)) *load*))))
    (when (verbose)
      (format *error-output* "~A ~A~%" function file)
      (finish-output))
    (apply function file rest)))

(defun run (list)
  "The true internal entry invoked by the C binary. All roswell commands are dispatched from this function"
  (loop for elt in list
        for *cmd* = (first elt)
        do (apply (intern (string (first elt)) (find-package :ros)) (rest elt)))
  (loop for f in (reverse *init-hook*)
        do (ignore-errors (funcall f))))

(when (opt "roswellenv")
  (pushnew :roswellenv *features*)
  (pushnew (read-from-string (format nil ":roswell.env.~A" (opt "roswellenv"))) *features*))

#+clisp
(unless (find :ros.init *features*)
  (push :ros.init *features*)
  (loop
    with *package* = (find-package :cl-user)
    for i in ext:*args*
    do (cl:eval (cl:read-from-string i))))

(unless (find :ros.init *features*)
  (push :ros.init *features*))
