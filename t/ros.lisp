(in-package :cl-user)
(defpackage roswell-test
  (:use :cl
        :ros
        :prove)
  (:shadowing-import-from :ros :run))
(in-package :roswell-test)

(ql:quickload :uiop :silent t)

(defun ! (r &optional expected)
  (if expected
      (is (print (string-trim #(#\Space #\Tab #\Newline #\Return #\linefeed)
                              (with-output-to-string (s)
                                (uiop:run-program r
                                                  :output (make-broadcast-stream s *standard-output*)
                                                  :error-output *error-output*))))
          expected r)
      (ok (ignore-errors
            (or (uiop:run-program r :error-output *error-output*
                                  :output *standard-output*)
                t))
          r)))

(defun !e (r)
  (is-error (uiop:run-program r :output *standard-output* :error-output *error-output*)
            'error (format nil "~a expected to fail" r)))

(defun !-tree (tree)
  (labels ((rec (current stack)
             (mapcar (lambda (child)
                       (if (consp child)
                           (rec child (cons (car current) stack))
                           (! (format nil "~{~a~^ ~}" (reverse (list* child (car current) stack)) ))))
                     (cdr current))))
    (rec tree nil)))

(defun ensure-delete-file (file)
  (when (probe-file file) (delete-file file)))

(plan nil)


(ok (getenv "USER") "(getenv \"USER\")")
(ok (not (getenv "NON_EXITS_ENV")) "(getenv \"NON_EXITS_ENV\") return nil if key not exist")
#-(or allegro abcl);;pass when sbcl ccl clisp
(is (progn
      (setenv "NON_EXITS_ENV" "hoge")
      (getenv "NON_EXITS_ENV"))
    "hoge"
    "(setenv \"NON_EXITS_ENV\")")

#-(or ecl allegro abcl)
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

#+sbcl
(when (equal (ros:opt "default.lisp") "sbcl-bin")
  (! "ros install ccl-bin/1.11")
  (! "ros use sbcl-bin")
  (! "ros delete ccl-bin/1.11"))
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
   ("init" "testinit" "testinit2.ros")))
(ok (probe-file "testinit.ros"))
(ensure-delete-file "testinit.ros")
(ok (probe-file "testinit2.ros"))
(ensure-delete-file "testinit2.ros")

(!e "ros init")

(ok (probe-file "t/test-template"))
(! "ros template rm test-template")
(! "ros template list" "")

(! "ros template add t/test-template AUTHOR")
(!e "ros template add t/test-template AUTHOR") ;; overwrite error
(! "ros template add -f t/test-template AUTHOR") ;; force overwrite
(! "ros template list"
   "test-template")
(! "ros template show test-template")
(! "ros init - test-template BOB" "my name is BOB")
(! "ros template rm test-template")
(! "ros template list" "")

(! "ros template add t/test-template --optional AUTHOR '\"Alice\"'")
(! "ros template list" "test-template")
(! "ros template show test-template")
(! "ros init - test-template BOB" "my name is BOB")
(! "ros init - test-template" "my name is Alice")
(! "ros template rm test-template")
(! "ros template list" "")

(ros:quit (if (finalize) 0 1))
