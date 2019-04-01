(defsystem "roswell"
  :version "19.4.10.98"
  :author "SANO Masatoshi"
  :license "MIT"
  :depends-on ("simple-date-time" "split-sequence" "plump" "zip")
  :components ((:module "lisp"
                :components
                ((:file "install-allegro" :depends-on ("util-install-quicklisp"))
                 (:file "install-abcl-bin" :depends-on ("util-install-quicklisp"))
                 (:file "install-cmu-bin" :depends-on ("util-install-quicklisp"))
                 (:file "install-ccl-bin" :depends-on ("util-install-quicklisp"))
                 (:file "install-clisp" :depends-on ("install+ffcall" "install+sigsegv"))
                 (:file "install-ecl" :depends-on ("util-install-quicklisp"))
                 (:file "install-sbcl" :depends-on ("util-install-quicklisp"))
                 (:file "install-sbcl-bin" :depends-on ("util-install-quicklisp"))
                 (:file "install-quicklisp" :depends-on ("util-install-quicklisp"))
                 (:file "install+ffcall" :depends-on ("util-install-quicklisp"))
                 (:file "install+7zip" :depends-on ("util-install-quicklisp"))
                 (:file "install+msys2" :depends-on ("util-install-quicklisp"))
                 (:file "install+sigsegv" :depends-on ("util-install-quicklisp"))
                 (:file "util-install-quicklisp" :depends-on ("system"))
                 (:file "system" :depends-on ("util-install"))
                 (:file "util-install" :depends-on ("locations"))
                 (:file "locations" :depends-on ("util"))
                 (:file "util-swank" :depends-on ("util"))
                 (:file "util" :depends-on ("init"))
                 (:file "init"))))
  :description "a command line tool to install and manage Common Lisp implementations damn easily."
  :long-description #.(uiop:read-file-string (merge-pathnames
                                              #p"README.md" (or *load-pathname* *compile-file-pathname*)))
  :in-order-to ((test-op (test-op :roswell/test))))

(defsystem "roswell/test"
  :depends-on (:prove)
  :components ((:module "t"
                        :components
                        ((:file "ros"))))
  :perform (test-op :after (op c)
                    #+quickisp(ql:quickload :prove-asdf)
                    #-asdf(asdf:load-system :prove-asdf)
                    (uiop:symbol-call :prove-asdf :run-test-system c)
                    (asdf:clear-system c)))
