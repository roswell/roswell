(defpackage :roswell.build.asd
  (:use :cl))
(in-package :roswell.build.asd)

(defun asd (system-path &optional (method) &rest argv)
  (declare (ignorable argv))
  (let (r)
    (do-symbols (i (find-package :asdf))
      (let ((name (symbol-name i)))
        (and (equalp "po-" (subseq (reverse name) 0 (min 3 (length name))))
             (string-equal (string-downcase name) method :end1 (- (length name) 3))
             (setf r i))))
    (when r
      (roswell:roswell `("-L" ,(or (ros:opt "*lisp")
                                   (ros:opt "default.lisp"))
                              "-l" ,system-path
                              "-e" ,(format nil "\"(asdf:operate '~S :~A)\"" r (pathname-name system-path)))
                       :interactive nil))))
