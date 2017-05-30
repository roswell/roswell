#+(and ros.init clisp)
(ros:asdf)

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
