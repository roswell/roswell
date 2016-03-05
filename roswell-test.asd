#-asdf
(require :asdf)

(in-package :cl-user)
(defpackage roswell-test-asd
  (:use :cl :asdf))
(in-package :roswell-test-asd)

(defsystem roswell-test
  :author "SANO Masatoshi"
  :license "MIT"
  :depends-on (:prove)
  :components ((:module "t"
                :components
                ((:file "ros"))))
  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
