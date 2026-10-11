;; load from quicklisp
(roswell:include '("util" "extend-quicklisp-system"))
(ql:quickload "roswell.extend.quicklisp" :silent t)
#+dotcl
(pushnew 'roswell.util::dotcl-dist-enumeration-function ql-dist:*dist-enumeration-functions*)
