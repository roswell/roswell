(in-package :roswell.util)

(roswell:ensure-asdf)
(roswell:include "util" "system")

(defmacro system (file &body rest)
  (destructuring-bind (&key depends-on (prefix ":roswell")) (first rest)
    (setf rest (rest rest))
    (let* ((pos (or (position #\+ file)
                    (position #\- file)))
           (symbol-name (read-from-string
                         (format nil ":~A~A"
                                 (subseq file (1+ pos))
                                 (if (find #\+ file) "+"""))))
           (name (read-from-string
                  (format nil "~A.~A.~A" prefix
                          (subseq file 0 pos)
                          symbol-name))))
      `(progn
         (asdf:defsystem ,name
           ,@(unless rest `(:components ((:file ,file))))
           ,@(when depends-on `(:depends-on ,depends-on)))
         ,@(when rest
             `((defpackage ,name)
               (setf (fdefinition (intern ,(string symbol-name) ,name))
                     (lambda ,@rest))
               (intern ,(string symbol-name) ,name)))))))
