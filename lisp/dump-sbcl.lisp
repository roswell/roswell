(roswell:include "util-dump")
(defpackage :roswell.dump.sbcl
  (:use :cl :roswell.util :roswell.util.dump)
  (:export :delete-compiler-information-sbcl :destroy-packages-sbcl :delete-debug-info))
(in-package :roswell.dump.sbcl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ignore-errors
   (when (find-symbol "MAKE-PACKAGE-HASHTABLE" :sb-impl)
     (pushnew :roswell-dump-newer-sbcl *features*)
     (when (and (find-symbol "INFO-HASHTABLE" :sb-impl)
                (find-symbol "INFO-MAPHASH" :sb-int))
       (pushnew :roswell-dump-sbcl-use-info-hashtable *features*)))))

(defun dump-executable (cmds out script)
  (declare (ignore script))
  (preprocess-before-dump)
  (sb-ext:gc :full t)
  (when *bundle-shared*
    (setf sb-sys:*shared-objects*
          (loop for d in sb-sys:*shared-objects*
             for p = (sb-alien::shared-object-pathname d)
             collect (if (find ".cache" (pathname-directory p) :test 'equal)
                         (progn
                           (uiop:copy-file p (merge-pathnames (file-namestring p)
                                                              (make-pathname :defaults out :type nil :name nil)))
                           (sb-alien::make-shared-object
                            :pathname (make-pathname :defaults (format nil "./~A" (file-namestring p)))
                            :namestring (format nil "./~A" (file-namestring p))
                            :handle (sb-alien::shared-object-handle d)
                            :dont-save (sb-alien::shared-object-dont-save d)))
                         d))))
  (sb-ext:save-lisp-and-die
   out
   :purify *purify*
   ; we all want our programs to be small, right?
   #+sb-core-compression :compression
   #+sb-core-compression *compression*
   :toplevel
   #'(lambda ()
       (setf *load-pathname* (pathname (first sb-ext:*posix-argv*)))
       (setf roswell:*argv* (rest sb-ext:*posix-argv*))
       (roswell:run cmds))
   :executable t
   :save-runtime-options t))

(defun sbcl (type &rest args)
  (case type
    (:query (first args))
    (:executable
     (apply 'dump-executable args))
    (:output
     (sb-ext:save-lisp-and-die (first args)))))

(defun safe-clear-info (category kind name)
  (when (cond
          (;; 1.3.x -
           (find-symbol "META-INFO" :sb-int)
           (funcall (find-symbol "META-INFO" :sb-int) category kind nil))
          (;; 1.2.x - 1.3.x
           (find-symbol "META-INFO" :sb-c)
           (funcall (find-symbol "META-INFO" :sb-c) category kind nil))
          (;; - 1.1.18
           (find-symbol "TYPE-INFO-OR-LOSE" :sb-c)
           (ignore-errors
             (funcall (find-symbol "TYPE-INFO-OR-LOSE" :sb-c) category kind))))
    (sb-int:clear-info category kind name)))

(defun delete-compiler-information-sbcl ()
  "This removes the entire compiler information about the functions.
This includes macro/compiler-macro definitions, inline expansions, 
IR1 (deftransform), IR2 (VOP) information in the infodb."
  ;; see src/compiler/globaldb.lisp
  #-sbcl
  (warn "delete-compiler is available only in SBCL")
  #+sbcl
  (declare (sb-ext:muffle-conditions style-warning))
  #+sbcl
  (do-all-symbols (s)
    (when (fboundp s)
      (setf (sb-int:info :function :inlinep s) :notinline)
      (safe-clear-info :function :inline-expansion-designator s)
      ;; Does this have the same effect as proclaiming notinline?
      ;; --- seems like so. src/compiler/proclaim.lisp
      ;; --- SB-C::PROCESS-INLINE-DECLARATION
      (safe-clear-info :function :source-transform s)
      (safe-clear-info :function :info s)
      (safe-clear-info :function :ir1-convert s)
      (safe-clear-info :function :predicate-truth-constraint s)
      (safe-clear-info :function :macro-function s)
      (safe-clear-info :function :compiler-macro-function s))
    (let ((s `(setf ,s)))
      (when (fboundp s)
        (setf (sb-int:info :function :inlinep s) :notinline)
        (safe-clear-info :function :inline-expansion-designator s)
        (safe-clear-info :function :source-transform s)
        (safe-clear-info :function :info s)
        (safe-clear-info :function :ir1-convert s)
        (safe-clear-info :function :predicate-truth-constraint s)
        (safe-clear-info :function :macro-function s)
        (safe-clear-info :function :compiler-macro-function s)))))

(defun destroy-packages-sbcl ()
  (when roswell:*main*
    (pushnew (package-name (symbol-package roswell:*main*))
             *package-blacklist* :test #'string=))
  (setf *package-blacklist*
        (union *package-blacklist*
               *additional-blacklist-for-destroy-packages*
               :test #'string=))
  #+(and sbcl (not roswell-dump-newer-sbcl))
  (warn "destroying packages is not supported on older versions of sbcl")
  #+(not sbcl)
  (warn "destroying packages is supported only on sbcl")
  #+roswell-dump-newer-sbcl
  (let (packages)
    (setf *features* (delete :roswell-dump-newer-sbcl *features*))
    (flet ((destroyer (package-name package)
             (unless (member package-name *package-blacklist* :test #'string=)
               (format t "~&Deleting ~s ~a " package-name package)
               (dolist (package (if (atom package) (list package) package)) ; in >1.5.0, the second arg could be a list (esp. for nicknamed pkgs)
                 (setf (sb-impl::package-%use-list package) nil)
                 (format t ".")
                 (setf (sb-impl::package-%used-by-list package) nil)
                 (format t ".")
                 (setf (sb-impl::package-%shadowing-symbols package) nil)
                 (format t ".")
                 (setf (sb-impl::package-internal-symbols package)
                       (sb-impl::make-package-hashtable 0))
                 (format t ".")
                 (setf (sb-impl::package-external-symbols package)
                       (sb-impl::make-package-hashtable 0))
                 (format t ".")
                 (setf (sb-impl::package-tables package) #())
                 (format t ".")
                 (setf (sb-impl::package-%implementation-packages package) nil)
                 (format t ".")
                 (setf (sb-impl::package-%local-nicknames package) nil)
                 (format t "."))
               ;;(setf (sb-impl::package-%locally-nicknamed-by package) nil)
               (push package-name packages)
               (format t ".")
               (do-symbols (symbol package-name)
                 (sb-impl::%set-symbol-package symbol nil)
                 (format t ".")
                 (unintern symbol)
                 (format t ".")))))
      (etypecase sb-impl::*package-names*
        (hash-table
         (maphash #'destroyer sb-impl::*package-names*)
         (dolist (package packages)
           (remhash package sb-impl::*package-names*)))
        
        #+roswell-dump-sbcl-use-info-hashtable
        (sb-impl::info-hashtable
         ;; sb-int:info-maphash is a macro ... weird
         (sb-int:info-maphash #'destroyer sb-impl::*package-names*)
         ;; info-hashtable does not have remhash
         )))))

;; TODO: why not just use delete-package? document it

(defun delete-fun-debug-info (fun)
  ;; cf. src/code/describe.lisp
  ;; function-lambda-expression
  (etypecase fun
    #+sb-eval
    (sb-eval::interpreted-function
     )
    #+sb-fasteval
    (sb-interpreter:interpreted-function
     ;; src/interpreter/function.lisp
     ;; fun-lambda-expression
     )
    (function
     (let* ((fun (sb-impl::%fun-fun fun)) ; obtain the true function from a funcallable-instance
            (code (sb-di::fun-code-header fun)))
       (setf (sb-kernel:%code-debug-info code) nil)))))

(defun delete-debug-info ()
  #-sbcl
  (warn "delete-debug-info is available only in SBCL")
  #+sbcl
  (declare (sb-ext:muffle-conditions style-warning))
  #+sbcl
  (do-all-symbols (s)
    (when (fboundp s)
      (delete-fun-debug-info (symbol-function s)))
    (safe-clear-info :source-location :declaration s)
    (safe-clear-info :type :source-location s)
    (safe-clear-info :source-location :variable s)
    (safe-clear-info :source-location :constant s)
    (safe-clear-info :source-location :typed-structure s)
    (safe-clear-info :source-location :symbol-macro s)
    (safe-clear-info :source-location :vop s)
    (safe-clear-info :source-location :declaration s)
    (safe-clear-info :source-location :alien-type s)
    (safe-clear-info :function :deprecated s)
    (safe-clear-info :variable :deprecated s)
    (safe-clear-info :type :deprecated s)
    (safe-clear-info :function :deprecated s)
    (safe-clear-info :function :deprecated s)
    (safe-clear-info :function :deprecated s)))
