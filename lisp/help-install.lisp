(roswell:include "util-install-quicklisp")
(roswell:quicklisp :environment nil)
(defpackage :roswell.help.install
  (:use :cl :roswell.util :roswell.install))
(in-package :roswell.help.install)

(defun install (argv)
  (if (not (second argv))
      (let ((s *error-output*)
            (cmd (pathname-name (opt "wargv0"))))
        (flet ((usage (msg)
                 (if (search "~A" msg)
                     (format s msg cmd)
                     (format s msg))
                 (terpri s)))
          
          (mapcar #'usage
                  '("Usage:"
                    ""
                    "To install a new Lisp implementaion:"
                    "   ~A install impl [options]"
                    "or a system from the GitHub:"
                    "   ~A install fukamachi/prove/v2.0.0 [repository... ]"
                    "or an asdf system from quicklisp:"
                    "   ~A install quicklisp-system [system... ]"
                    "or a local script:"
                    "   ~A install ./some/path/to/script.ros [path... ]"
                    "or a local system:"
                    "   ~A install ./some/path/to/system.asd [path... ]"
                    ""
                    "For more details on impl specific options, type:"
                    "   ~A help install impl"
                    ""
                    ""
                    "Candidates impls for installation are:")))
        
        (loop for i in (asdf:registered-systems)
              with len = #.(length "roswell.install.")
              when (and (string-equal "roswell.install." i :end2 (min (length i) len))
                        (not (eql (aref i (1- (length i))) #\+)))
                do (format s "~A~%" (subseq i len))))
      (let* ((impl (second argv))
             (fun (module "install" impl)))
        (install-impl impl nil nil (funcall fun :help)))))
