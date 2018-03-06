#|

This file is a stub.
roswell does not function without help of C codes.

|#

(defsystem "roswell"
  :version "18.3.10.89"
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
