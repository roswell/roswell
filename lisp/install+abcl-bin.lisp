
(defun abcl-bin-get-version ()
  (let ((file (merge-pathnames "tmp/abcl-bin.html" (homedir))))
    (format *error-output* "Checking version to install....~%")
    (download "https://common-lisp.net/project/armedbear/releases/" file)
    (with-output-to-string (*standard-output*)
      (funcall (intern (string :quickload) :ql)
               :plump))
    (let* ((the-newest "current-release.tgz")
           (tgz-list
            (sort
             (delete nil
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
                              (plump:parse (merge-pathnames "tmp/abcl-bin.html" (homedir))) "a")))
             'simple-date-time:date-time> :key 'cdr))
           timestamp)
      (setq tgz-list (delete-if (lambda (x)
                                  (when (equal (first x) the-newest)
                                    (setq timestamp (cdr x)))
                                  (not (eql (mismatch "ecl" (first x)) 3))) tgz-list))
      (when timestamp
        (let (find)
          (setq tgz-list (delete-if (lambda (x)
                                      (when (simple-date-time:date-time= (cdr x) timestamp)
                                        (setq find x)))
                                    tgz-list))
          (when find
            (push find tgz-list))))
      (mapcar (lambda (x)
                (let ((str (car x)))
                  (subseq str 4 (- (length str) 4))))
              tgz-list))))
