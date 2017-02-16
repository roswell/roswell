(in-package :cl-user)
(defpackage roswell-test
  (:use :cl
        :roswell
        :roswell.util
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
(! "ros swank install")
(! (format nil "ros help install ~A" (ros:opt "default.lisp")))

#+sbcl
(when (equal (ros:opt "default.lisp") "sbcl-bin")
  (! "ros install ccl-bin/1.11")
  (! "ros use sbcl-bin")
  (! "ros delete ccl-bin/1.11")
  (progn
    (setenv "MAKEFLAGS" "-j 2")
    (prog1 (! "ros install clisp")
      (unsetenv "MAKEFLAGS")))
  (! "ros use sbcl-bin"))
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
   ))


(ros:quit (if (finalize) 0 1))
