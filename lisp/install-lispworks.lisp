(defpackage :roswell.install.lispworks
  (:use :cl :roswell.util :roswell.install))
(in-package :roswell.install.lispworks)

(defparameter *code*
"(in-package \"CL-USER\")
(load-all-patches)

(defun main ()
  (let ((args (rest sys:*line-arguments-list*)))
    (loop :until (null args)
          :for arg := (pop args)
          :do (cond ((string= arg \"--eval\")
                     (eval (read-from-string (pop args))))))))

(pushnew '(\"Main\" (:priority 60000000 :restart-action :continue) main)
         mp:*initial-processes*)

(save-image ~S
            :console t
            :multiprocessing t
            :environment nil)")

(defun get-option (argv option)
  (second (member option argv :test #'string=)))

(defun touch (file)
  (with-open-file (stream file :direction :output :if-exists :supersede :if-does-not-exist :create))
  file)

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

(defun install-1 (prefix lw-tar lwdoc-tar)
  (ensure-directories-exist prefix)
  (setf prefix (probe-file prefix))
  (uiop:run-program (format nil "tar -zxf '~A' -C '~A'" lw-tar prefix))
  (when lwdoc-tar (uiop:run-program (format nil "tar -zxf '~A' -C '~A'" lwdoc-tar prefix)))
  (let ((lwlicfile (merge-pathnames "lib/7-1-0-0/config/lwlicense" prefix)))
    (touch lwlicfile)
    (sb-posix:chmod (probe-file lwlicfile) #o666)))

(defun install (argv)
  (let* ((argv (getf argv :argv))
         (lw-tar (get-option argv "--lw-tar"))
         (lwdoc-tar (get-option argv "--lwdoc-tar"))
         (serial-number (get-option argv "--lwlicenseserial"))
         (key (get-option argv "--lwlicensekey"))
         (prefix (merge-pathnames (make-pathname
                                   :directory (list :relative "impls" (uname-m) (uname) "LispWorks"))
                                  (homedir))))
    (multiple-value-bind (major minor arch os)
        (guess-version lw-tar)
      (install-1 prefix lw-tar lwdoc-tar)
      (let ((lw-program (merge-pathnames (format nil "lispworks-~D-~D-0-~A-~A"
                                                 major minor arch os)
                                         prefix)))
        (uiop:run-program (list lw-program
                                "--lwlicenseserial" serial-number
                                "--lwlicensekey" key))
        (build-lw-console prefix lw-program)))))

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
    (:install '(install setup))
    (:list)))
