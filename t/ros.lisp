(in-package :cl-user)
(defpackage roswell-test
  (:use :cl
        :ros
        :prove)
  (:shadowing-import-from :ros :run))
(in-package :roswell-test)

(plan nil)

(ok (getenv "USER") "(getenv \"USER\")")
(ok (not (getenv "NON_EXITS_ENV")) "(getenv \"NON_EXITS_ENV\") return nil if key not exist")
(is (progn
      (setenv "NON_EXITS_ENV" "hoge")
      (getenv "NON_EXITS_ENV"))
    "hoge"
    "(setenv \"NON_EXITS_ENV\")")

(ok (progn
      (unsetenv "NON_EXITS_ENV")
      (not (getenv "NON_EXITS_ENV")))
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

(finalize)
