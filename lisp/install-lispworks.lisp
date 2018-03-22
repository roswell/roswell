(roswell:include "util-install-quicklisp")
(defpackage :roswell.install.lispworks
  (:use :cl :roswell.install :roswell.util))
(in-package :roswell.install.lispworks)

(defparameter *code*
"(in-package \"CL-USER\")
(load-all-patches)

(defun main ()
  (let ((args (rest sys:*line-arguments-list*)))
    (loop :until (null args)
          :for arg := (pop args)
          :do (cond ((string= arg \"--eval\")
                     (eval (read-from-string (pop args)))))))
  (lw:start-tty-listener))

(pushnew '(\"Main\" (:priority 60000000 :restart-action :continue) main)
         mp:*initial-processes*)

(save-image ~S
            :console t
            :multiprocessing t
            :environment nil)")

(defun get-option (argv option)
  (second (member option argv :test #'string=)))

(defun touch (file)
  (open file :direction :probe :if-does-not-exist :create))

(defun guess-version (lw-tar)
  (let ((list (uiop:split-string (pathname-name lw-tar) :separator "-.")))
    (let ((major (digit-char-p (char (first list) 2)))
          (minor (digit-char-p (char (first list) 3)))
          (arch (second list))
          (os (third list)))
      (values major minor arch os))))

(defun build-lw-console (prefix lw-program)
  (let ((lw-console (merge-pathnames "lw-console" prefix)))
    (uiop:with-temporary-file (:stream stream :pathname lw-file)
      (write-string (format nil *code* lw-console) stream)
      :close-stream
      (uiop:run-program (list lw-program "-build" (princ-to-string lw-file))))))

(defun lw-runtime-name (lw-tar)
  (multiple-value-bind (major minor arch os)
      (guess-version lw-tar)
    (format nil "lispworks-~D-~D-0-~A-~A"
            major minor arch os)))

(defun install-1 (prefix lw-tar lwdoc-tar)
  (let ((target (ensure-directories-exist (make-pathname :defaults prefix :name nil :type nil))))
    (if target
        (progn
          (expand lw-tar target)
          (when lwdoc-tar
            (expand lwdoc-tar target))
          (let ((lwlicfile
                  (multiple-value-bind (major minor)
                      (guess-version lw-tar)
                    (merge-pathnames (format nil "lib/~D-~D-0-0/config/lwlicense" major minor)
                                     target))))
            (touch lwlicfile)
            (sb-posix:chmod (probe-file lwlicfile) #o666)))
        (error "~S not exists" target))))

(defun lw-install (argv)
  (let* ((argv (getf argv :argv))
         (lw-tar (get-option argv "--lw-tar"))
         (lwdoc-tar (get-option argv "--lwdoc-tar"))
         (serial-number (get-option argv "--lwlicenseserial"))
         (key (get-option argv "--lwlicensekey"))
         (prefix (merge-pathnames (make-pathname
                                   :directory (list :relative "impls" (uname-m) (uname) "LispWorks"))
                                  (homedir))))
    (install-1 prefix lw-tar lwdoc-tar)
    (let ((lw-program (merge-pathnames (lw-runtime-name lw-tar)
                                       prefix)))
      (uiop:run-program (list lw-program
                              "--lwlicenseserial" serial-number
                              "--lwlicensekey" key))
      (build-lw-console prefix lw-program)))
  (cons t argv))

(defun help (argv)
  (format t "lispworks install options~%")
  (flet ((fmt (param default more)
           (format t "--~A ~A~%~5T~A~%"
                   param
                   (or (and (not (null default))
                            (not (eql default t))
                            default)
                       "")
                   more)))
    (fmt "lw-tar" nil "")
    (fmt "lwdoc-tar" nil "")
    (fmt "--lwlicenseserial" nil "")
    (fmt "--lwlicensekey" nil ""))
  (cons t argv))

(defun lispworks (type)
  (case type
    (:help '(help))
    (:install '(lw-install
                setup))
    (:list)))
