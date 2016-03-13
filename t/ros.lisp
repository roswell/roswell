(in-package :cl-user)
(defpackage roswell-test
  (:use :cl
        :ros
        :prove)
  (:shadowing-import-from :ros :run))
(in-package :roswell-test)

(ql:quickload :uiop)

(defun ! (r &optional expected)
  (if expected
      (is (string-trim #.(format nil " ~%")
                       (uiop:run-program r :output :string))
          expected r)
      (ok (ignore-errors
            (or (uiop:run-program r) t))
          r)))

(defun !-tree (tree)
  (labels ((rec (current stack)
             (mapcar (lambda (child)
                       (if (consp child)
                           (rec child (cons (car current) stack))
                           (! (format nil "~{~a~^ ~}" (reverse (list* child (car current) stack)) ))))
                     (cdr current))))
    (rec tree nil)))

(plan nil)

(ok (getenv "USER") "(getenv \"USER\")")
(ok (not (getenv "NON_EXITS_ENV")) "(getenv \"NON_EXITS_ENV\") return nil if key not exist")
#-(or alisp abcl);;pass when sbcl ccl clisp
(is (progn
      (setenv "NON_EXITS_ENV" "hoge")
      (getenv "NON_EXITS_ENV"))
    "hoge"
    "(setenv \"NON_EXITS_ENV\")")

#-(or ecl alisp abcl)
(ok (and
     (getenv "NON_EXITS_ENV")
     (progn
       (unsetenv "NON_EXITS_ENV")
       (not (getenv "NON_EXITS_ENV"))))
    "(unsetenv \"NON_EXITS_ENV\")")

(ok (not (zerop (length (roswell '("roswell-internal-use" "uname") :string :trim))))
    "ros:roswell uname return something")

(ok (every (lambda (x) (or (digit-char-p x)
                           (eql x #\.)))  (version))
    "ros:version returns [0-9.]*")

(ok (opt "impl") "(ros:opt \"impl\") return something")

#+(and test are required someday)
(ros:ensure-using-downloaded-asdf
 ros:exec
 ros:ignore-shebang
 ros:quicklisp
 ros:quit
 ros:script
 ros:util
 ros:with-lock-held)

(! "ros version")
(! "ros -v run -- --version")
(! "ros config")
(! "ros -e \"(defvar *a* 1)\" -e \"(setq *a* (* *a* 2))\" -e \"(print *a*)\"" "2")
(! "ros list")
(! "ros list installed")
(! "ros list dump")
#+broken (! "ros list versions")

(!-tree
 '("ros"
   ("list" "" "installed" "dump" #+broken "versions" "")
   ("config" "" "show" "set")
   "version"
   "help"
   ("dump" "" "executable" "output")
   "use"
   "asdf"
   "fmt"
   ;;"build"
   ("init" "" "testinit" "testinit2.ros")))

(ok (probe-file "testinit.ros"))
(when (probe-file "testinit.ros") (delete-file "testinit.ros"))
(ok (probe-file "testinit2.ros"))
(when (probe-file "testinit2.ros") (delete-file "testinit2.ros"))

(ros:quit (if (finalize) 0 1))
