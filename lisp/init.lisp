
#|

The entry of all roswell commands.
The true internal entry invoked by the C binary is ros:run.
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

(defpackage :ros
  (:use :cl)
  (:shadow :load :eval :package :restart :print :write)
  (:export :run :*argv* :*main* :quit :script :quicklisp :getenv :opt
           :ignore-shebang :ensure-using-downloaded-asdf :include :ensure-asdf
           :roswell :exec :setenv :unsetenv :version :swank :verbose)
  (:documentation "Roswell backend."))

(in-package :ros)
(defvar *argv* nil)
(defvar *ros-opts* nil)
(defvar *main* nil)

;; small tools
(defun getenv (x)
  #+abcl(extensions:getenv x)
  #+ccl(ccl:getenv x)
  #+clisp(ext:getenv x)
  #+cmucl
  (let ((f (ignore-errors (symbol-function (read-from-string "unix:unix-getenv")))))
    (if f
        (funcall f x)
        (cdr (assoc x ext:*environment-list* :test #'string=))))
  #+ecl(ext:getenv x)
  #+sbcl(sb-posix:getenv x)
  #+allegro(sys:getenv x)
  #-(or abcl ecl ccl clisp sbcl cmucl allegro)
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

(defun opt (param)
  (second (assoc param (ros-opts) :test 'equal)))

(defun verbose ()
  (let ((ret (parse-integer (ros:opt "verbose"))))
    (if (zerop ret)
        nil ret)))

(let (sentinel)
  (defun ensure-asdf ()
    (or
     sentinel
     (not (setf sentinel t))
     (when (and (find :asdf *features*)
                (ros:opt "asdf")
                (not (equal (ros:opt "asdf")
                            (funcall (read-from-string "asdf:asdf-version")))))
       (ignore-errors
         (locally
             (declare #+sbcl(sb-ext:muffle-conditions sb-kernel:redefinition-warning))
           (handler-bind
               (#+sbcl(sb-kernel:redefinition-warning #'muffle-warning))
             (cl:load (merge-pathnames (format nil "lisp/asdf/~A/asdf.lisp" (opt "asdf")) (opt "homedir")))))))
     (find :asdf *features*)
     (ignore-errors (require :asdf)))))

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

(defun setenv (name value)
  (declare (ignorable name value))
  #+sbcl(funcall (read-from-string "sb-posix:setenv") name value 1)
  #+ccl(ccl:setenv name value t)
  #+clisp(system::setenv name value)
  #+cmucl(let ((f (ignore-errors (symbol-function (read-from-string "unix:unix-setenv")))))
           (when f (funcall f name value 1)))
  #+ecl(ext:setenv name value)
  value)

(defun unsetenv (name)
  (declare (ignorable name))
  #+sbcl(funcall (read-from-string "sb-posix:unsetenv") name)
  #+ccl(ccl:unsetenv name)
  #+clisp(system::setenv name nil)
  #+cmucl(let ((f (ignore-errors (symbol-function (read-from-string "unix:unix-unsetenv")))))
           (when f (funcall f name)))
  nil)

(defun quit (&optional (return-code 0) &rest rest)
  (let ((ret (or (and (numberp return-code) return-code) (first rest) 0)))
    #+sbcl (ignore-errors (funcall (read-from-string "cl-user::exit") :code ret))
    #+sbcl (ignore-errors (funcall (read-from-string "cl-user::quit") :unix-status ret))
    #+clisp (ext:exit ret)
    #+ccl (ccl:quit ret)
    #+cmucl (progn (finish-output) (finish-output *error-output*) (unix:unix-exit ret))
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
        #+clisp(ext:run-shell-command (format nil "~{~A~^ ~}" args :output output)))))

(defun exec (args)
  #+(and unix sbcl)
  (execvp (first args) args)
  #+(and unix ccl)
  (ignore-errors
    (ccl:with-string-vector (argv args) (ccl::%execvp argv)))
  (quit (run-program args)))

(defun quicklisp (&key path (environment "QUICKLISP_HOME"))
  (unless (find :quicklisp *features*)
    (let ((path (make-pathname
                 :name "setup"
                 :type "lisp"
                 :defaults (or path
                               (and environment (getenv environment))
                               (opt "quicklisp")))))
      (when (probe-file path)
        (cl:load path)
        (let ((symbol (read-from-string "ql:*local-project-directories*")))
          (when (or (ignore-errors (probe-file path))
                    #+clisp(ext:probe-directory path))
            (set symbol (cons (merge-pathnames "local-projects/" (opt "homedir"))
                              (symbol-value symbol)))))
        t))))

(defvar *included-names* '())
(defvar *include-path* #.*load-pathname*)
(defun include (name)
  (unless (find name *included-names* :test 'equal)
    (cl:load (make-pathname
              :defaults *include-path*
              :name name :type "lisp"))
    (push name *included-names*)))

(defun swank (&rest params)
  (unless (cl:find-package :ros.swank.util)
    (include "util-swank"))
  (apply (read-from-string "ros.swank.util:swank") params))

#+quicklisp
(let ((path (merge-pathnames "local-projects/" (opt "homedir"))))
  (when (or (ignore-errors (probe-file path))
            #+clisp(ext:probe-directory path))
    (push path ql:*local-project-directories*)))

(defun shebang-reader (stream sub-character infix-parameter)
  (declare (ignore sub-character infix-parameter))
  (loop for x = (read-char stream nil nil)
     until (or (not x) (eq x #\newline))))

(compile 'shebang-reader)
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
(defun ensure-using-downloaded-asdf ()
  (unless *downloaded-asdf-loaded*
    (roswell '("ros" "asdf" "install"))
    (cl:load
     (merge-pathnames
      (format nil "lisp/~A/asdf.lisp"
              (roswell '("config" "show" "asdf.version") :string t)) (opt "homedir")))
    (setf *downloaded-asdf-loaded* t)))

(let ((symbol (ignore-errors(read-from-string "asdf::*user-cache*")))
      (impl (substitute #\- #\/ (second (assoc "impl" (ros-opts) :test 'equal)))))
  (when (and symbol (boundp symbol))
    (cond ((listp (symbol-value symbol))
           (set symbol (append (symbol-value symbol) (list impl))))
          ((pathnamep (symbol-value symbol))
           (set symbol (merge-pathnames (format nil "~A/" impl) (symbol-value symbol))))
          (t (cl:print "tbd.....")))))

#+sbcl
(when (ignore-errors (string-equal (ros:opt "impl") "sbcl-bin" :end1 8))
  (let ((path (merge-pathnames (format nil "src/sbcl-~A/" (lisp-implementation-version)) (opt "homedir"))))
    (when (probe-file path)
      (sb-ext:set-sbcl-source-location path))))

(defun source-registry (cmd arg &rest rest)
  (declare (ignorable cmd rest))
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

(defun system (cmd args &rest rest)
  (declare (ignorable cmd rest))
  (ensure-asdf)
  (unless (find :asdf *features*)
    (error "Can't find asdf to load system"))
  (loop for ar = args then (subseq ar (1+ p))
     for p = (position #\, ar)
     for arg = (if p (subseq ar 0 p) ar)
     do (if (find :quicklisp *features*)
            (funcall (read-from-string "ql:quickload") arg :silent t)
            (funcall (read-from-string "asdf:operate") (read-from-string "asdf:load-op") arg)) 
     while p))

(setf (fdefinition 'load-system)
      #'system)

(defvar *version-cache* nil)
(defun version (&optional opt) 
  (if opt
      (roswell `("roswell-internal-use" "version" ,(string-downcase (princ-to-string opt))) :string t)
      (or *version-cache*
          (setf *version-cache* (roswell '("roswell-internal-use" "version") :string t)))))

(defun package (cmd arg &rest rest)
  (declare (ignorable cmd rest))
  (setq *package* (find-package (read-from-string (format nil "#:~A" arg)))))

(defun system-package (cmd arg &rest rest)
  (declare (ignorable cmd rest))
  (apply #'system cmd arg rest)
  (apply #'package cmd arg rest))

(defun eval (cmd arg &rest rest)
  (declare (ignorable cmd rest))
  (loop with start = 0
        with end = (gensym)
        with exp
        do (multiple-value-setq (exp start)
             (read-from-string arg nil end :start start))
        until (eql exp end)
        do (cl:eval exp)))

(defun restart (cmd arg &rest rest)
  (declare (ignorable cmd rest))
  (funcall (read-from-string arg)))

(defun entry (cmd arg &rest rest)
  (declare (ignorable cmd rest))
  (let ((ret (apply (read-from-string arg) *argv*)))
    (quit (cond ((numberp ret) ret)
                (ret 0)
                (t 1)))))

(setf (fdefinition 'init) #'eval)

(defun print (cmd arg &rest rest)
  (declare (ignorable cmd rest))
  (cl:print (cl:eval (read-from-string arg))))

(defun write (cmd arg &rest rest)
  (declare (ignorable cmd rest))
  (cl:write (cl:eval (read-from-string arg))))

(defun script (cmd arg &rest rest)
  "load and evaluate the script"
  (setf *argv* rest)
  (flet ((body (in)
           (let ((line(read-line in)))
             (push :ros.script *features*)
             (locally
                 (declare #+sbcl(sb-ext:muffle-conditions sb-kernel:redefinition-warning))
               (handler-bind
                   (#+sbcl(sb-kernel:redefinition-warning #'muffle-warning))
                 (funcall #+(or sbcl clisp) 'cl:load
                          #-(or sbcl clisp) (read-from-string "asdf::eval-input")
                          (make-concatenated-stream
                           (make-string-input-stream
                            (format nil "(cl:setf cl:*load-pathname* ~S cl:*load-truename* (ignore-errors (truename cl:*load-pathname*)))~A"
                                    (ignore-errors (merge-pathnames (make-pathname :defaults arg)))
                                    (if (equal (subseq line 0 (min (length line) 2)) "#!")
                                        "" line)))
                           in
                           (make-string-input-stream
                            (if (eql cmd :script)
                                "(ros:quit (cl:apply 'main ros:*argv*))"
                                "(setf ros:*main* 'main)"))))))
             (setf *features* (remove :ros.script *features*)))))
    (if (streamp arg)
        (body arg)
        (if (probe-file arg)
            (with-open-file (in arg)
              (body in))
            (format t "script ~S does not exist~%" arg)))))

(defun stdin (cmd arg &rest rest)
  (declare (ignorable arg cmd rest))
  (apply #'script :script *standard-input* rest))

(defun load (x file)
  (declare (ignore x))
  (cl:load file))

(defun run (list)
  "The true internal entry invoked by the C binary. All roswell commands are dispatched from this function"
  (loop :for elt :in list
     :do (apply (intern (string (first elt)) (find-package :ros)) elt)))

#+clisp
(unless (find :ros.init *features*)
  (push :ros.init *features*)
  (loop
     with *package* = (find-package :cl-user)
     for i in ext:*args*
     do (cl:eval (cl:read-from-string i))))

(unless (find :ros.init *features*)
  (push :ros.init *features*))
