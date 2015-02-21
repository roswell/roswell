(cl:in-package :cl-user)
#-asdf
(require :asdf)
#+sbcl
(require :sb-posix)

(defpackage :ros
  (:use :cl)
  (:shadow :load :eval :package :restart :print :write)
  (:export :run :*argv* :quit :script :quicklisp :getenv :opt :ignore-shebang))

(in-package :ros)
(defvar *verbose* 0)
(defvar *argv* nil)
(defvar *ros-opts* nil)

;; small tools
(defun getenv (x)
  (asdf::getenv x))

(defun ros-opts ()
  (or *ros-opts*
      (setf *ros-opts*
            (let((*read-eval*))
              (read-from-string (getenv "ROS_OPTS"))))))

(defun opt (param)
  (second (assoc param (ros-opts) :test 'equal)))

(defun quicklisp (&key path (environment "QUICKLISP_HOME"))
  (unless (find :quicklisp *features*)
    (cl:load
     (make-pathname
      :name "setup"
      :type "lisp"
      :defaults (or path
                    (and environment (getenv environment))
                    (second (assoc "quicklisp" (ros-opts)
                                   :test 'equal)))))))

(defun shebang-reader (stream sub-character infix-parameter)
  (declare (ignore sub-character infix-parameter))
  (loop for x = (read-char stream nil nil)
     until (or (not x) (eq x #\newline))))

(compile 'shebang-reader)
(defun ignore-shebang ()
  (set-dispatch-macro-character #\# #\! #'shebang-reader))

(defun impl ()
  (let ((s (second (assoc "impl" (ros-opts) :test 'equal))))
    (subseq s (1+ (position #\/ s)))))

(unless (equal (first (last asdf::*user-cache*)) (impl))
  (setf asdf::*user-cache* (append asdf::*user-cache* (list (impl)))))

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
        (warn "Source-registry ~S isn't valid. Ignored." arg)
        (asdf:initialize-source-registry dir))))

(defun system (cmd arg &rest rest)
  (declare (ignorable cmd rest))
  #-quicklisp(asdf:operate 'asdf:load-op arg)
  #+quicklisp(ql:quickload arg :silent t))

(setf (fdefinition 'load-systm)
      #'system)

(defun package (cmd arg &rest rest)
  (declare (ignorable cmd rest))
  (setq *package* (find-package (read-from-string (format nil "#:~A" arg)))))

(defun system-package (cmd arg &rest rest)
  (declare (ignorable cmd rest))
  (apply #'system cmd arg rest)
  (apply #'package cmd arg rest))

(defun eval (cmd arg &rest rest)
  (declare (ignorable cmd rest))
  (cl:eval (read-from-string arg)))

(defun quit (return-code &rest rest)
  (let ((ret (or (and (numberp return-code) return-code) (first rest) 0)))
    (ignore-errors(funcall 'asdf::quit ret))
    #+sbcl(ignore-errors(cl-user::exit :code ret))
    #+sbcl(ignore-errors(funcall (read-from-string "cl-user::quit") :unix-status ret))))

(defun restart (cmd arg &rest rest)
  (declare (ignorable cmd rest))
  (funcall (read-from-string arg)))

(defun entry (cmd arg &rest rest)
  (declare (ignorable cmd rest))
  (apply (read-from-string arg) *argv*))

(setf (fdefinition 'init) #'eval)

(defun print (cmd arg &rest rest)
  (declare (ignorable cmd rest))
  (cl:print (cl:eval (read-from-string arg))))

(defun write (cmd arg &rest rest)
  (declare (ignorable cmd rest))
  (cl:write (cl:eval (read-from-string arg))))

(defun script (cmd arg &rest rest)
  (declare (ignorable cmd))
  (setf *argv* rest)
  (if (probe-file arg)
      (with-open-file (in arg)
        (let ((line(read-line in)))
          (push :ros.script *features*)
          (cl:load (make-concatenated-stream
                    (make-string-input-stream
                     (format nil "(cl:setf cl:*load-pathname* ~S cl:*load-truename* (truename cl:*load-pathname*))~A" (merge-pathnames (make-pathname :defaults arg))
                             (if (equal (subseq line 0 (min (length line) 2)) "#!")
                                 "" line)))
                    in
                    (make-string-input-stream
                     (if (eql cmd :script)
                         "(cl:apply 'main ros:*argv*)"
                         ""))))
          (setf *features* (remove :ros.script *features*))))
      (format t "script ~S does not exist~%" arg)))

(defun load (x file)
  (declare (ignore x))
  (cl:load file))

(defun run (list)
  (loop :for elt :in list
     :do (apply (intern (string (first elt)) (find-package :ros)) elt)))

(push :ros.init *features*)
