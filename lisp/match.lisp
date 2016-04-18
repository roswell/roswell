
(ql:quickload :alexandria :silent t)

(defpackage :ros.match
  (:use :cl :alexandria)
  (:shadow :once-only)
  (:export #:match
           #:ematch
           #:lambda-match
           #:lambda-ematch))

(in-package :ros.match)

;; from alexandria, but adds `ignorable'
(defmacro once-only (specs &body forms)
  (let ((gensyms (make-gensym-list (length specs) "ONCE-ONLY"))
        (names-and-forms (mapcar (lambda (spec)
                                   (etypecase spec
                                     (list
                                      (destructuring-bind (name form) spec
                                        (cons name form)))
                                     (symbol
                                      (cons spec spec))))
                                 specs)))
    ;; bind in user-macro
    `(let ,(mapcar (lambda (g n) (list g `(gensym ,(string (car n)))))
                   gensyms names-and-forms)
       ;; bind in final expansion
       `(let (,,@(mapcar (lambda (g n)
                           ``(,,g ,,(cdr n)))
                   gensyms names-and-forms))
          (declare (ignorable ,,@gensyms))
          ;; bind in user-macro
          ,(let ,(mapcar (lambda (n g) (list (car n) g))
                         names-and-forms gensyms)
             ,@forms)))))

(defvar *what*)
(defvar *bindings*)
(defvar *env*)
(defmacro match (*what* &body clauses &environment *env*)
  (once-only (*what*)
    (parse-patterns clauses)))

(defmacro ematch (what &body clauses)
  (once-only (what)
    `(match ,what
       ,@clauses
       (_ (error "level0 match error! ~S does not match ~S" ,what ',clauses)))))

(defmacro lambda-match (&body clauses)
  (with-gensyms (arg)
    `(lambda (,arg)
       (match ,arg
         ,@clauses))))

(defmacro lambda-ematch (&body clauses)
  (with-gensyms (arg)
    `(lambda (,arg)
       (ematch ,arg
         ,@clauses))))

(defun parse-patterns (clauses)
  (if (null clauses)
      nil
      (destructuring-bind ((pattern &rest body) . rest) clauses
        (multiple-value-bind (condition bindings)
            (let ((*bindings* nil))
              (values (make-pattern-predicate pattern)
                      *bindings*))
          `(if ,condition
               (let* ,(reverse bindings)
                 (declare (ignorable ,@(mapcar #'first bindings)))
                 ,@body)
               ,(parse-patterns rest))))))



(defun make-pattern-predicate (pattern)
  (if (atom pattern)
      (cond
        ((constantp pattern *env*) `(equal ,*what* ,pattern))
        ((symbolp pattern)
         (unless (string= "_" (symbol-name pattern))
           (push `(,pattern ,*what*) *bindings*))
         t)
        (t (error "what is this? ~a" pattern)))
      (destructuring-bind (name . args) pattern
        (ecase name
          (quote `(equal ,*what* ',@args))
          (cons
           (destructuring-bind (car cdr) args
             `(and (consp ,*what*)
                   ,(let* ((what `(car ,*what*))
                           (*what* what))
                      (once-only (*what*)
                        (push `(,*what* ,what) *bindings*)
                        (make-pattern-predicate car)))
                   ,(let* ((what `(cdr ,*what*))
                           (*what* what))
                      (once-only (*what*)
                        (push `(,*what* ,what) *bindings*)
                        (make-pattern-predicate cdr))))))
          (list
           (if args
               (destructuring-bind (car . cdr) args
                 (make-pattern-predicate
                  (if cdr
                      `(cons ,car (list ,@cdr))
                      `(cons ,car nil))))
               `(null ,*what*)))
          (list*
           (destructuring-bind (car . cdr) args
             (make-pattern-predicate
              (if cdr
                  `(cons ,car (list* ,@cdr))
                  car))))))))

(push :ros.match *features*)
