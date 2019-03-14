(defpackage :roswell.build.ros
  (:use :cl))
(in-package :roswell.build.ros)

(defun parse-argv (argv)
  (let* ((header "exec ros")
         (hl (length header)))
    (and (equal header (subseq argv 0 (length header)))
         (loop for last = nil then c
               for i from 0
               for c across argv
               when (and (equal #\- last)
                         (equal #\- c))
                 do (return (subseq argv hl (1- i)))))))

(defun ros (&rest argv)
  (let (opts
        impl-argv
        dump-argv)
    (dolist (l (rest argv))
      (if (find #\= l)
          (push l impl-argv)
          (push l dump-argv)))
    (setf impl-argv (nreverse impl-argv)
          dump-argv (nreverse dump-argv))
    (with-open-file (in (first argv))
      (loop repeat 4
            for line = (read-line in)
            when (ignore-errors (string= line "exec" :end1 4))
              do (setf opts (parse-argv line))))
    (roswell:roswell `(,@(when (roswell:verbose) '("-v"))
                       ,@impl-argv
                         ,opts "-L" ,(or (ros:opt "*lisp")
                                         (ros:opt "default.lisp"))
                         "dump" ,@dump-argv "executable" ,(first argv))
                     :interactive nil)))
