;; load from quicklisp
(roswell:include "util")
(asdf:defsystem roswell.extend.quicklisp
  :components ((:file "extend-quicklisp")))
(ql:quickload :roswell.extend.quicklisp :silent t)
