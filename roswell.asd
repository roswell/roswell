#-asdf
(require :asdf)

#|

This file is a stub.
Roswell will be asdf-loadable in the future

|#

(in-package :cl-user)

(defpackage roswell-asd
  (:use :cl :asdf))
(in-package :roswell-asd)

(defsystem roswell
  :version "0.0.4.57"
  :author "SANO Masatoshi"
  :license "MIT"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "install%" :depends-on ("init"))
                 (:file "init"))))
  :description "a command line tool to install and manage Common Lisp implementations damn easily."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op roswell-test))))
