(in-package :roswell.util)

(ros:ensure-asdf)
(ros:include "util" "system")

(defmacro system (file &key depends-on (prefix ":roswell"))
  (let ((pos (or (position #\- file)
                 (position #\+ file))))
    `(asdf:defsystem
         ,(read-from-string
           (format nil "~A.~A.~A~A" prefix
                   (subseq file 0 pos)
                   (subseq file (1+ pos))
                   (if (find #\+ file) "+""")))
       :components ((:file ,file))
       ,@(when depends-on `(:depends-on ,depends-on)))))
