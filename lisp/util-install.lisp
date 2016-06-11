(cl:in-package :cl-user)

(ros:include "locations")

(defpackage :ros.install
  (:use :cl :ros.util :ros.locations)
  (:export :*build-hook* :install-impl :probe-impl :read-call :*ros-path*
   :install-system-script :install-impl-if-probed :install-script-if-probed
           :install-system-if-probed))

(in-package :ros.install)

(defvar *ros-path* nil)
(defvar *help-cmds* nil)
(defvar *install-cmds* nil)
(defvar *list-cmd* nil)

(defun set-opt (item val)
  (let ((found (assoc item (ros::ros-opts) :test 'equal)))
    (if found
        (setf (second found) val)
        (push (list item val) ros::*ros-opts*))))

(defun read-call (func &rest params)
  (ignore-errors (apply (let (*read-eval*) (read-from-string func)) params)))

(defun probe-impl (impl)
  (or (ignore-errors
       (let ((imp (format nil "roswell.install.~A" impl)))
         (and (or (read-call "ql-dist:find-system" imp)
                  (read-call "ql:where-is-system" imp))
              (read-call "ql:quickload" imp :silent t))))
      (and ;; before setup quicklisp
       (find impl '("sbcl-bin" "quicklisp") :test 'equal)
       (load (make-pathname :name (format nil "install-~A" impl) :type "lisp" :defaults *load-pathname*)))))

(defun install-impl (impl version argv)
  (let ((cmds (cdr (assoc impl *install-cmds* :test #'equal))))
    (when cmds
      (let ((param `(t :target ,impl :version ,version :argv ,argv)))
        (handler-case
            (loop for call in cmds
               do (setq param (funcall call (rest param)))
               while (first param))
          #+sbcl
          (sb-sys:interactive-interrupt (condition)
            (declare (ignore condition))
            (format t "SIGINT detected, cleaning up the partially installed files~%")
            (ros:roswell `(,(format nil "deleteing ~A/~A" (getf (cdr param) :target) (getf (cdr param) :version))) :string t)))))))

(defun install-impl-if-probed (imp version argv)
  (let ((result (probe-impl imp)))
    (when result
      (install-impl imp version argv)
      result)))

(defun install-script-if-probed (impl/version)
  (let* (sub
         (result (probe-file (setf sub (make-pathname :defaults impl/version :type "ros")))))
    (when result
      (read-call "install-ros" sub)
      result)))

(defun install-system-if-probed (imp)
  (let ((result (or (read-call "ql-dist:find-system" imp)
                    (read-call "ql:where-is-system" imp))))
    (when result
      (read-call "install-system-script" imp)
      result)))

(defun github-version (uri project filter)
  (let ((file (merge-pathnames (format nil "tmp/~A.html" project) (homedir))))
    (unless (and (probe-file file)
                 (< (get-universal-time) (+ (* 60 60) (file-write-date file))))
      (download uri file))
    (nreverse
     (loop for link in (read-call "plump:get-elements-by-tag-name" (read-call "plump:parse" file) "link")
           for href = (read-call "plump:get-attribute" link "href")
           when (eql (aref href 0) #\/)
             collect (funcall filter href)))))

(pushnew :ros.install.util *features*)
