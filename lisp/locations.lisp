(defpackage :roswell.locations
  (:use :cl))
(in-package :roswell.locations)

(defvar *locations* '())

(defmacro defuri (name &optional uri)
  (let ((downcased (format nil "~(~A~)" name)))
    `(progn
       (export
        (defun ,name ()
          (or (roswell:opt ,downcased)
              ,uri
              (roswell:roswell '("roswell-internal-use" "version" ,downcased) :string t))))
       (push ',name *locations*))))

(defuri clisp-version-uri "http://ftp.gnu.org/pub/gnu/clisp/release/")
(defuri 7za-uri       "http://sourceforge.net/projects/sevenzip/files/7-Zip/9.20/7za920.zip/download#")
(defuri allegro-uri   "http://www.franz.com/")
(defuri abcl-bin-uri  "https://common-lisp.net/project/armedbear/releases/")
(defuri ccl-bin-uri   "https://github.com/roswell/ccl_bin/releases/download/")
(defuri clisp-uri     "http://sourceforge.net/projects/clisp/files/clisp/")
(defuri cmu-bin-uri   "https://common-lisp.net/project/cmucl/downloads/")
(defuri quicklisp-uri "http://beta.quicklisp.org/")

(defuri ffcall-uri    "https://github.com/roswell/libffcall/archive/")

(defuri sigsegv-uri   "http://ftpmirror.gnu.org/libsigsegv/")
(defuri msys2-uri     "http://kent.dl.sourceforge.net/project/")

(defuri clisp-patch1-uri "https://raw.githubusercontent.com/Homebrew/homebrew/2fb8cb1a2279f80dc89900b3ebaca9e5afc90494/Library/Formula/clisp.rb")

(defuri asdf-git-version-uri "https://github.com/roswell/asdf/releases.atom")
(defuri ecl-git-version-uri  "https://github.com/roswell/ecl/releases.atom")
(defuri sbcl-git-version-uri "https://github.com/sbcl/sbcl/releases.atom")
(defuri ccl-git-version-uri  "https://github.com/roswell/ccl_bin/releases.atom")
(defuri slime-git-version-uri"https://github.com/slime/slime/releases.atom")
(defuri sly-git-version-uri  "https://github.com/joaotavora/sly/releases.atom")
(defuri npt-git-version-uri "https://github.com/nptcl/npt/releases.atom")

(defuri asdf-uri "https://github.com/roswell/asdf/archive/")
(defuri ecl-uri  "https://github.com/roswell/ecl/archive/")
(defuri sbcl-uri "https://github.com/sbcl/sbcl/archive/")
(defuri npt-uri  "https://github.com/nptcl/npt/archive/")

(defuri sbcl-bin-version-uri)
(defuri sbcl-bin-uri)

(export
 (defvar *allegro-agreement-uri*
   '(("10.1express" . #1="http://franz.com/ftp/pub/legal/ACL-Express-20150812.pdf")
     ("100express"  . #1#)
     ("101b"        . "http://franz.com/products/licensing/FSLA10.1.beta.pdf"))))
