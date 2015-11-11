(in-package :ros.install)

(defun ecl-get-version ()
  (let ((file (merge-pathnames "tmp/ecl.html" (homedir))))
    (format *error-output* "Checking version to install....~%")
    (download "https://common-lisp.net/project/ecl/files/" file)
    (with-output-to-string (*standard-output*)
      (funcall (intern (string :quickload) :ql)
	       :plump))
    (flet ((! (f &rest r) (apply (read-from-string f) r)))
      (let* ((tgz-list
	      (remove nil
		      (mapcar (lambda (x)
				(let* ((str (!"plump:get-attribute" x "href"))
				       (len (length str)))
				  (when (equal (subseq str (if (plusp (- len 3))
							       (- len 3)
							       0)) "tgz")
				    (cons str
					  (!"plump:text"
					    (!"plump:next-element" (!"plump:parent" x)))))))
			      (!"plump:get-elements-by-tag-name"
				(!"plump:parse" (merge-pathnames "tmp/ecl.html" (homedir))) "a"))))
	     (target "current-release.tgz"))
	tgz-list))))
