#+ros.init
(roswell:include '("util" "system") "util-template")
#+quicklisp
(ql:quickload '("uiop" "djula") :silent t)
(defpackage :roswell.util.template
  (:use :cl)
  (:export
   :*template-function-plist*
   :*template-base-directories*

   :template-path
   :template-file-path
   :template-asd-path

   :templates-list
   :template-create
   :template-remove
   :template-directory
   :template-parameter
   :template-remove-file
   :template-add-file
   :template-attr-file
   :template-attr-common
   :template-export-files
   :template-import-files

   :template-apply

   #+ros.init :template-default))

(in-package :roswell.util.template)

(defvar *template-base-directories* '())
(defvar *template-function-plist*
  '(:djula djula
    :copy copy-file))

(defun run-n-get-first-result (cmds)
  (loop for cmd in cmds
        for result = (ignore-errors (uiop:run-program cmd :output :string))
        when result
          return (remove #\Newline result)))

(defvar *author* nil)
(defun author ()
  (or *author*
      (setf *author* (run-n-get-first-result '("git config --global --get user.name"
                                               "whoami")))))

(defvar *email* nil)
(defun email ()
  (or *email*
      (setf *email* (run-n-get-first-result '("git config --global --get user.email"
                                              "echo $(whoami)@$(hostname)")))))

(defun sanitize (name)
  (remove-if (lambda (x) (find x "./\\")) name))

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

(defun templates-list (&key filter name)
  (let* ((* (loop for x in (append *template-base-directories*
                                   (mapcar (lambda (path)
                                             (merge-pathnames "templates/" path))
                                           ql:*local-project-directories*))
                  append (directory (merge-pathnames "**/*.asd" x))))
         (* (remove-if-not (lambda (x) (ignore-errors (string-equal "roswell.init." (pathname-name x) :end2 13))) *))
         (* (cons (merge-pathnames "init-default.lisp" (ros:opt "lispdir")) *))
         (* (if filter
                (let ((filter (sanitize filter)))
                  (remove-if-not (lambda (x)
                                   (find (pathname-name x)
                                         (list (format nil "roswell.init.~A" filter)
                                               (format nil "init-~A" filter))
                                         :test 'equal))
                                 *))
                *)))
    (cond
      (name
       (mapcar (lambda (x)
                 (cond
                   ((equal (pathname-name x) "init-default") "default")
                   (t (subseq (pathname-name x) 13))))
               *))
      (t *))))

(defun template-path (name)
  (let ((found (templates-list :filter name)))
    (if found
        (make-pathname :type nil :name nil
                       :defaults (first found))
        (merge-pathnames (format nil "templates/~A/" (sanitize name)) (first ql:*local-project-directories*)))))

(defun template-file-path (template-name path)
  (merge-pathnames (enc-string path)
                   (merge-pathnames (format nil "~A-template/" template-name)
                                    (template-path template-name))))

(defun template-asd-path (template-name)
  (make-pathname :defaults (template-path template-name)
                 :type "asd"
                 :name (format nil "roswell.init.~A" template-name)))

(defun apply-djula (template-string stream params)
  (apply 'djula::render-template* (djula::compile-string template-string) stream
         `(,@params
           :author ,(author)
           :email ,(email)
           :universal_time ,(get-universal-time))))

(defun copy-file (src dest params)
  (declare (ignore params))
  (when (probe-file dest)
    (error "file exits ~A" dest))
  (uiop:copy-file src dest))

(defun djula (src dest params)
  (with-open-file (o dest :direction :output)
    (apply-djula (uiop:read-file-string src) o params)))

(defun template-apply (template-name args info &key (path *default-pathname-defaults*))
  (declare (ignorable template-name args info))
  ;; tbd after-hook?
  (flet ((octal (string)
           (parse-integer string :radix 8))
         (key (string)
           (when string
             (let (*read-eval*)
               (read-from-string (format nil ":~A" string))))))
    (setf args `(:name ,(first args)
                       ,@(loop for i on (rest args) by #'cddr
                               while (string-equal (first i) "--" :end1 2)
                               collect (key (subseq (first i) 2))
                               collect (second i))))
    (loop for i in (getf info :files)
          for from = (template-file-path template-name (getf i :name))
          for to = (ensure-directories-exist (merge-pathnames (if (getf i :rewrite)
                                                                  (apply-djula (getf i :rewrite) nil args)
                                                                  (getf i :name))
                                                              path))
          do (unless (equal (file-namestring to) "")
               (funcall (getf *template-function-plist* (key (getf i :method))) from to args))
             (when (ignore-errors (octal (getf i :chmod)))
               #+sbcl(sb-posix:chmod to (octal (getf i :chmod)))))))

(defun template-write (name list)
  (with-open-file (o (ensure-directories-exist (template-asd-path name))
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (let ((package (read-from-string (format nil ":roswell.init.~A" name)))
          (*package* (find-package :roswell.util.template)))
      (format o "~{~S~%~}"
              `((defpackage ,package
                  (:use :cl))
                (in-package ,package)
                (defvar *params* '(,@list))
                (defun ,(read-from-string name) (_ &rest r)
                  (asdf:load-system :roswell.util.template :verbose nil)
                  (funcall (read-from-string "roswell.util.template:template-apply") _ r *params*)))))))

(defun template-read-asd (asd-path)
  (let (package
        read/)
    (with-open-file (o asd-path
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
      (second (third read/)))))

(defun template-read (name)
  (template-read-asd (template-asd-path name)))

(defun template-create (name)
  (template-write (sanitize name) nil))

(defun template-remove (name)
  (uiop:delete-directory-tree
   (template-path name)
   :validate (lambda (path)
               (equal (car (last (pathname-directory path)))
                      name))))

(defun template-directory (name)
  (getf (template-read (sanitize name)) :files))

(defun template-parameter (name)
  (getf (template-read (sanitize name)) :common))

#+ros.init
(defun (setf template-default) (template-name)
  (setf (roswell.util:config "init.default") (sanitize template-name)))

#+ros.init
(defun template-default (&optional (default "default"))
  (or (roswell.util:config "init.default") (sanitize default)))

(defun template-add-file (template-name file-name path-copy-from)
  (let ((template-name (sanitize template-name))
        (info (template-read template-name)))
    (uiop:copy-file path-copy-from
                    (ensure-directories-exist (template-file-path template-name file-name)))
    (unless (find file-name (getf info :files) :key (lambda (x) (getf x :name)) :test 'equal)
      (let ((method (getf (getf info :common) :default-method)))
        (push (list :name file-name :method (if method method "copy"))
              (getf info :files))))
    (template-write template-name info)))

(defun template-remove-file (template-name file-name)
  (let* ((template-name (sanitize template-name))
         (info (template-read template-name)))
    (uiop:delete-file-if-exists
     (template-file-path template-name file-name))
    (when (find file-name (getf info :files) :key (lambda (x) (getf x :name)) :test 'equal)
      (setf (getf info :files)
            (remove file-name (getf info :files)
                    :key (lambda (x) (getf x :name)) :test 'equal))
      (template-write template-name info))))

(defun template-attr-file (template-name file-name key value)
  (let* ((template-name (sanitize template-name))
         (info (template-read template-name))
         (found (find file-name (getf info :files) :key (lambda (x) (getf x :name)) :test 'equal)))
    (when found
      (if (eql (getf found key #1='#:a) #1#)
          (setf (cdr (last found)) (list key value))
          (setf (getf found key ) value))
      (template-write template-name info))))

(defun template-attr-common (template-name key value)
  (let* ((template-name (sanitize template-name))
         (info (template-read template-name)))
    (setf (getf (getf info :common) key) value)
    (template-write template-name info)))

#+ros.init
(roswell.util:system "util-template")

(defun template-export-files (template-name dst)
  (let ((path (first (templates-list :filter template-name))))
    (when (and path (equal (pathname-type path) "asd"))
      (mapc (lambda (x)
              (let* ((file-name (getf x :name))
                     (dst-file-path (merge-pathnames file-name dst)))
                (ensure-directories-exist dst-file-path)
                (uiop:copy-file (template-file-path template-name file-name)
                                dst-file-path)))
            (template-directory template-name))
      (uiop:copy-file path
                      (merge-pathnames (format nil "~A.asd" (pathname-name path)) dst)))))

(defun template-import-files (src)
  (let ((asd-path (first (directory (merge-pathnames "roswell.init.*.asd" src)))))
    (unless asd-path
      (error "\"roswell.init.*.asd\" is not exist"))
    (let ((name (subseq (pathname-name asd-path) 13)))
      (when (templates-list :filter name)
        (template-remove name))
      (template-create name)
      (uiop:with-current-directory (src)
        (mapc (lambda (x)
                (let ((file-name (getf x :name))
                      (chmod (getf x :chmod))
                      (rewrite (getf x :rewrite)))
                  (template-add-file name file-name file-name)
                  (template-attr-file name file-name :method (getf x :method))
                  (when rewrite
                    (template-attr-file name file-name :rewrite rewrite))
                  (when chmod
                    (template-attr-file name file-name :chmod chmod))))
              (reverse (getf (template-read-asd asd-path) :files)))))))
