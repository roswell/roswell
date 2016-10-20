(cl:in-package :cl-user)

#-asdf
(require :asdf)

(ros:include "util")
(in-package :ros.util)

(defmacro system (file &key depends-on (prefix ":roswell.install"))
  `(asdf:defsystem
       ,(read-from-string
         (format nil "~A.~A~A" prefix
                 (subseq file (1+ (or (position #\- file)
                                      (position #\+ file))))
                 (if (find #\+ file) "+""")))
     :components ((:file ,file))
     ,@(when depends-on `(:depends-on ,depends-on))))
