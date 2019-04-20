(in-package :roswell)
(defun asd-p (file)
  (and
   (ignore-errors (read-from-string "asdf:load-asd"))
   (equal (pathname-type file) "asd")))

(defun load-asd (file)
  (roswell.util:read-call "asdf:load-asd" file))
(setf *load* (acons 'asd-p 'load-asd (remove 'asd-p *load* :key 'first)))
(defun ros-p (file)
  (or(equal (pathname-type file) "ros")
     (null (pathname-type file))))
(defun load-ros (file)
  (let (*cmd* *main*)
    (script file)))
(setf *load* (acons 'ros-p 'load-ros (remove 'ros-p *load* :key 'first)))
(in-package :roswell.util)
(defun fetch-via-roswell (url file &key (follow-redirects t) quietly (maximum-redirects 10))
  "Request URL and write the body of the response to FILE."
  (declare (ignorable follow-redirects maximum-redirects))
  (download (ql-http::urlstring (ql-http:url url)) file
            :verbose (if quietly nil "2")
            :output (if (find :abcl *features*)
                        :interactive
                        *standard-output*))
  (values (make-instance 'ql-http::header :status 200)
          (probe-file file)))
(dolist (x '("https" "http"))
  (setf ql-http:*fetch-scheme-functions*
        (acons x 'fetch-via-roswell
               (remove x ql-http:*fetch-scheme-functions* :key 'first :test 'equal))))
(pushnew :quicklisp-support-https *features*)

(defun roswell-installed-system-name (system-name)
  ;;  should return repo part of system-name.
  ;; "user//repo" "user//repo/branch" "git://bra/bra/bra/repo.git" "github://user/repo"
  (when (find "" (split-sequence #\/ system-name) :test 'equal)
    (if (find #\: system-name)
        (values nil "not implemented yet") ;; TBD
        (second (remove "" (split-sequence #\/ system-name) :test 'equal)))))

(defun roswell-installable-searcher (system-name)
  (let ((name (roswell-installed-system-name system-name))
        pname)
    (and
     name
     (not (when (setf pname (read-call "asdf/find-system:primary-system-name" system-name))
            (asdf:find-system pname nil)))
     (prog1
         (or (quicklisp-client:local-projects-searcher name)
             (unless (find system-name (loop for ar = (ros:getenv "ROSINSTALL") then (subseq ar (1+ p))
                                             for p = (position #\, ar)
                                             for arg = (if p (subseq ar 0 p) ar)
                                             collect arg
                                             while p)
                           :test 'equal)
               (roswell:roswell `("install" ,system-name))
               (quicklisp-client:register-local-projects)
               (quicklisp-client:local-projects-searcher name))
             (return-from roswell-installable-searcher)) ;;can't find.
       (eval `(asdf:defsystem ,system-name :depends-on (,name))))
     (asdf:find-system system-name))))

(unless (find 'roswell-installable-searcher (symbol-value (read-from-string "asdf:*system-definition-search-functions*")))
  (set (read-from-string "asdf:*system-definition-search-functions*")
       (append (list 'roswell-installable-searcher)
               (symbol-value (read-from-string "asdf:*system-definition-search-functions*")))))

(pushnew 'roswell-dist-enumeration-function ql-dist:*dist-enumeration-functions*)

(defclass roswell-dist (ql-dist:dist)
  ())

(defun roswell-dist-enumeration-function ()
  (loop for file in (directory (merge-pathnames "dists/*/distinfo.txt" (roswell.util:homedir)))
     collect (ql-dist::make-dist-from-file file 'roswell-dist)))

(in-package #:ql-dist)
(defvar *install-use-roswell* t)
(let ((*error-output* (make-broadcast-stream)))
  (when (or
         (loop for k in '(:win32 :windows :mswindows)
               never (find k *features*))
         (probe-file
          (merge-pathnames
           (format nil "impls/~A/windows/7za/9.20/7za.exe"
                   (roswell:roswell '("roswell-internal-use" "uname" "-m") :string
                                    T))
           (roswell.util:homedir))))
    (defmethod install :around ((release release))
      (if *install-use-roswell*
          (let ((archive (ensure-local-archive-file release))
                (output
                  (relative-to (dist release)
                               (make-pathname :directory (list :relative "software"))))
                (tracking (install-metadata-file release)))
            (ensure-directories-exist output)
            (ensure-directories-exist tracking)
            (roswell:roswell
             `("roswell-internal-use" "tar" "-xf" ,archive "-C" ,output))
            (ensure-directories-exist tracking)
            (with-open-file
                (stream tracking :direction :output :if-exists :supersede)
              (write-line (qenough (base-directory release)) stream))
            (let ((provided (provided-systems release)) (dist (dist release)))
              (dolist (file (system-files release))
                (let ((system (find-system-in-dist (pathname-name file) dist)))
                  (unless (member system provided)
                    (error
                     "FIND-SYSTEM-IN-DIST returned ~A but I expected one of ~A"
                     system provided))
                  (let ((system-tracking (install-metadata-file system))
                        (system-file
                          (merge-pathnames file (base-directory release))))
                    (ensure-directories-exist system-tracking)
                    (unless (probe-file system-file)
                      (error "release claims to have ~A, but I can't find it"
                             system-file))
                    (with-open-file
                        (stream system-tracking :direction :output :if-exists
                                :supersede)
                      (write-line (qenough system-file) stream))))))
            release)
          (call-next-method)))))

(in-package :roswell)
(defun revert-extension ()
  (setf ql-http:*fetch-scheme-functions*
        (acons "http" 'ql-http:http-fetch
               (remove "http" ql-http:*fetch-scheme-functions* :key 'first :test 'equal))
        ql-http:*fetch-scheme-functions*
        (remove "https" ql-http:*fetch-scheme-functions* :key 'first :test 'equal)
        ql-dist:*dist-enumeration-functions*
        (remove 'roswell-dist-enumeration-function ql-dist:*dist-enumeration-functions*)
        ql-dist::*install-use-roswell* nil)
  (ignore-errors
   (set (read-from-string "asdf:*system-definition-search-functions*")
        (remove 'roswell.util:roswell-installable-searcher (symbol-value (read-from-string "asdf:*system-definition-search-functions*")))))
  t)
