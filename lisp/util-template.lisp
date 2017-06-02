(roswell:include "util" "util-template")
(defpackage :roswell.util.template
  (:use :cl :roswell.util)
  (:export
   :template-init
   :template-list
   :template-set-default
   :template-rm
   :template-add
   :template-apply))

(in-package :roswell.util.template)

(defun sanitize (name)
  (remove-if (lambda (x) (find x "./\\")) name))

(defun template-apply (template-name args params)
  (declare (ignorable template-name args params))
  (print (list template-name args)))

(defun template-path (name)
  (merge-pathnames (format nil "templates/~A/" name)
                   (first ql:*local-project-directories*)))

(defun enc-string (str)
  (format nil "~{%~36r~}" (loop for i across str collect (char-code i))))

(defun dec-string (str)
  (with-input-from-string (s (substitute #\Space #\% str))
    (coerce
     (loop with *read-base* = 36
           with *read-eval*
           for i = (read s nil nil)
           while i
           collect (code-char i)) 'string)))

(defun write-template (name &key
                              (path (template-path name))
                              list)
  (with-open-file (o (merge-pathnames (format nil "roswell.init.~A.asd" name) path)
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (let ((package (read-from-string (format nil ":roswell.init.~A" name)))
          (*package* (find-package :roswell.util.template)))
      (format o "~{~S~%~}"
              `((defpackage ,package
                  (:use :cl))
                (in-package ,package)
                (defvar *params* ,(cons 'list list))
                (defun ,(read-from-string name) (_ &rest r)
                  (roswell:include "util-template")
                  (funcall (read-from-string "roswell.util.template:template-apply") _ r *params*)))))))

(defun read-template (name &key (path (template-path name)))
  (let ((package)
        read/)
    (with-open-file (o (merge-pathnames (format nil "roswell.init.~A.asd" name) path)
                       :direction :input
                       :if-does-not-exist :error)
      (setq read/ (read o))
      (unless (and
               (eql 'defpackage (first read/))
               (string-equal "ROSWELL.INIT." (symbol-name (setq package (second read/))) :end2 13))
        (error "not init template ~A~%" read/))
      (setq read/ (read o))
      (unless (and
               (eql 'in-package (first read/))
               (eql (second read/) package))
        (error "not init template ~A~%" read/))
      (setq read/ (read o))
      (unless (and (eql 'defvar (first read/))
                   (string-equal  '*params* (second read/))) 
        (error "not init template ~S~%" (list read/)))
      (rest (third read/)))))

(defun template-init (names)
  (let* ((name (sanitize (first names)))
         (path (template-path name)))
    (when (probe-file (merge-pathnames ".git/" path))
      (format *error-output* "already exist ~A~%" path)
      (ros:quit 0))
    (ensure-directories-exist path)
    (uiop:chdir path)
    (uiop:run-program "git init" :output :interactive :error-output :interactive)
    (write-template name :path path)))

(defun list-templates (&key filter name)
  (let* ((* (directory (merge-pathnames "**/.git/" (first ql:*local-project-directories*))))
         (* (mapcar (lambda (x) (directory (merge-pathnames "../*.asd" x))) *))
         (* (apply #'append *))
         (* (remove-if-not (lambda (x) (ignore-errors (string-equal "roswell.init." (pathname-name x) :end2 13))) *))
         (* (cons (merge-pathnames "init-default.lisp" (ros:opt "lispdir")) *))
         (* (if filter
                (remove-if-not (lambda (x)
                                 (find (pathname-name x)
                                       (list (format nil "roswell.init.~A" filter)
                                             (format nil "init-~A" filter))
                                       :test 'equal))
                               *)
                *)))
    (cond
      (name
       (mapcar (lambda (x)
                 (subseq (pathname-name x)
                         (1+ (position-if
                              (lambda (x) (find x ".-"))
                              (pathname-name x) :from-end t))))
               *))
      (t *))))

(defun template-dir (name)
  (merge-pathnames (format nil "~A/" name) (make-pathname :defaults (first (list-templates :filter name)) :type nil :name nil)))

(defun list-in-template (name)
  (mapcar (lambda (x) (dec-string (file-namestring x)))
          (directory (merge-pathnames "*" (template-dir name)))))

(defun template-list (_)
  "List the installed templates"
  (cond
    ((not _)
     (format t "~{~A~%~}" (list-templates :name t)))
    ((first _)
     (let* ((name (sanitize (first _)))
            (path (first (list-templates :filter name))))
       (when (and path (equal (pathname-type path) "asd"))
         (print (mapcar (lambda (x) (dec-string (file-namestring x))) (directory (merge-pathnames (format nil "~A/*" name) (make-pathname :defaults path :type nil :name nil))))))))))

(defun template-set-default (_)
  (let ((name (sanitize (first _))))
    (when name
      (let ((path (list-templates :filter name)))
        (if path
            (setf (config "init.default") name)
            (format *error-output* "template: ~S not found.~%" name))))))

(defun template-add-file (template-name file-name path-copy-from)
  (let ((info (read-template template-name)))
    (uiop:copy-file path-copy-from
                    (merge-pathnames (enc-string file-name) (template-dir template-name)))
    (unless (find file-name (getf info :files) :key (lambda (x) (getf x :name)))
      (push (list :name file-name) (getf info :files)))
    (write-template template-name :list info)))

(defun template-add (_)
  ;; care windows someday
  (let ((name (config "init.default")))
    (unless (and name
                 (not (equal name "default")))
      (setf name (sanitize (first _))
            _ (rest _)))
    (if (and (list-templates :filter name)
             (not (equal name "default")))
        (loop for i in _
              do (template-add-file name i i))
        (format *error-output* "template ~S is not editable.~%" name))))

(defun template-rm (names)
  "Remove (delete) a template."
  (declare (ignorable names))
  )
