(in-package :ros.install)
(ql:quickload '(:plump :simple-date-time :split-sequence) :silent t)

(defun parse-date (str)
  (setq str (string-trim " " str)
        str (substitute #\: #\- str)
        str (substitute #\: #\space str))
  (setq str (split-sequence:split-sequence #\: str))
  (make-instance 'simple-date-time:date-time
                 :day (parse-integer (first str))
                 :month (simple-date-time::from-short-month-name (second str))
                 :year (parse-integer (third str))
                 :hour (parse-integer (fourth str))
                 :minute (parse-integer (fifth str))
                 :second 0))
;;(parse-date "22-Aug-2015 18:19  ")

(defun ecl-get-version ()
  (let ((file (merge-pathnames "tmp/ecl.html" (homedir))))
    (format *error-output* "Checking version to install....~%")
    (download "https://common-lisp.net/project/ecl/files/" file)
    (with-output-to-string (*standard-output*)
      (funcall (intern (string :quickload) :ql)
	       :plump))
    (let* ((tgz-list
            (remove nil
                    (mapcar (lambda (x)
                              (let* ((str (plump:get-attribute x "href"))
                                     (len (length str)))
                                (when (equal (subseq str (if (plusp (- len 3))
                                                             (- len 3)
                                                             0))
                                             "tgz")
                                  (cons str
                                        (parse-date (plump:text
                                                     (plump:next-element
                                                      (plump:parent x))))))))
                            (plump:get-elements-by-tag-name
                             (plump:parse (merge-pathnames "tmp/ecl.html" (homedir))) "a"))))
           #+nil(target "current-release.tgz"))
      tgz-list)))


