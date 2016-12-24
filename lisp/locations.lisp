(cl:in-package :cl-user)
(when (cl:find-package :ros.locations)
  (pushnew :ros.locations *features*))

(defpackage :ros.locations
  (:use :cl))
(in-package :ros.locations)

(defvar *locations* '())

(defmacro defuri (name &optional uri)
  (let ((downcased (format nil "~(~A~)" name)))
    `(progn
       (export
        (defun ,name ()
          (or (ros:opt ,downcased)
              ,uri
              (ros:roswell '("roswell-internal-use" "version" ,downcased) :string t))))
       (push ',name *locations*))))

(defuri clisp-version-uri "http://ftp.gnu.org/pub/gnu/clisp/release/")
(defuri 7za-uri       "http://sourceforge.net/projects/sevenzip/files/7-Zip/9.20/7za920.zip/download#")
(defuri allegro-uri   "http://www.franz.com/ftp/pub/")
(defuri abcl-bin-uri  "https://common-lisp.net/project/armedbear/releases/")
(defuri ccl-bin-uri   "http://ccl.clozure.com/ftp/pub/release/")
(defuri clisp-uri     "http://sourceforge.net/projects/clisp/files/clisp/")
(defuri cmu-bin-uri   "https://common-lisp.net/project/cmucl/downloads/")
(defuri quicklisp-uri "http://beta.quicklisp.org/")

(defuri ffcall-uri    "http://www.haible.de/bruno/gnu/")
(defuri sigsegv-uri   "http://ftpmirror.gnu.org/libsigsegv/")
(defuri msys2-uri     "http://kent.dl.sourceforge.net/project/")

(defuri sbcl-patch1-uri  "https://gist.githubusercontent.com/snmsts/e8e4fd4bd5e458ac45e8/raw/bb7f1cd2e8e9a914f4e9b1b5acf889ecf75dfe0c/posix-tests.patch")
(defuri sbcl-patch2-uri  "https://gist.githubusercontent.com/snmsts/14ca6f57b1730a4c30312e394eb7e914/raw/c55b6176f92038c7573d6d9e4e222861b84158ee/sbcl-1.3.11.patch")
(defuri clisp-patch1-uri "https://raw.githubusercontent.com/Homebrew/homebrew/2fb8cb1a2279f80dc89900b3ebaca9e5afc90494/Library/Formula/clisp.rb")

(defuri asdf-git-version-uri "https://github.com/roswell/asdf/releases.atom")
(defuri ecl-git-version-uri  "https://github.com/roswell/ecl/releases.atom")
(defuri sbcl-git-version-uri "https://github.com/sbcl/sbcl/releases.atom")
(defuri slime-git-version-uri"https://github.com/slime/slime/releases.atom")

(defuri asdf-uri "https://github.com/roswell/asdf/archive/")
(defuri ecl-uri  "https://github.com/roswell/ecl/archive/")
(defuri sbcl-uri "https://github.com/sbcl/sbcl/archive/")

(defuri sbcl-bin-version-uri)
(defuri sbcl-bin-uri)

(defuri slime-uri "https://github.com/slime/slime.git")

(push :ros.locations *features*)
