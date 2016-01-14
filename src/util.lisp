(cl:in-package :cl-user)
(when (cl:find-package :ros.util)
  (push :ros.util *features*))

(defpackage :ros.util
  (:use :cl)
  (:export :uname :uname-m :homedir :config :use :impl))

(in-package :ros.util)

(defun uname ()
  (ros:roswell '("roswell-internal-use" "uname") :string t))

(defun uname-m ()
  (ros:roswell '("roswell-internal-use" "uname" "-m") :string t))

(defun homedir ()
  (ros:opt "homedir"))

(defun impl (imp)
  (ros:roswell `("roswell-internal-use" "impl" ,(or imp "")) :string t))

(defun config (c)
  (ros:roswell (list "config show" c) :string t))

(defun (setf config) (a b)
  (ros:roswell (list "config" b a) :string t)
  a)

(defun use (arg)
  (when (and arg
             (ignore-errors
               (ros:roswell `("-L" ,arg "version=t" "run"))))
    (let* ((pos (position #\/ arg))
           (lisp (if pos
                     (subseq arg 0 pos)
                     arg)))
      (if pos
          (progn
            (config "default.lisp" lisp)
            (config (format nil "~A.version" lisp)
                    (subseq arg (1+ pos))))
          (config (if (digit-char-p (aref lisp 0))
                      (format nil "~A.version" (config "default.lisp"))
                      "default.lisp")
                  lisp)))
    t))
