(roswell:include "util" "util-config")
(defpackage :roswell.util.config
  (:use :cl)
  (:export :load-config :save-config :unconfig :config :env))
(in-package :roswell.util.config)

(defun load-config (path)
  (and (probe-file path)
       (with-open-file (in path)
         (loop for line = (read-line in nil nil)
            while line
            collect
              (loop for last = -1 then pos
                 for pos = (position #\Tab line :start (1+ last))
                 collect (subseq line (1+ last) pos)
                 while pos)))))

(defun save-config (path config)
  (with-open-file (out path :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
    (loop for line in config
          do (format out #.(format nil "~{~A~}" '("~{~A~^" #\Tab "~}~%")) line))))

(defun unconfig (config var)
  (remove var config :test 'equal :key #'first))

(defun config (config var val)
  (let ((found (find var config :test 'equal :key #'first)))
    (if found
        (progn
          (setf (third found) val)
          config)
        (cons (list var "0" val)
              config))))

(defun env ()
  (or (roswell:opt "*roswellenv")
      (third (assoc "roswellenv"
                    (load-config ".roswellenv")
                    :test 'equal))))
