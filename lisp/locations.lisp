(cl:in-package :cl-user)
(when (cl:find-package :ros.locations)
  (pushnew :ros.locations *features*))

(defpackage :ros.locations
  (:use :cl))
(in-package :ros.locations)

(defmacro defuri (name uri)
  `(export (defvar ,(intern (format nil "*~A-~A*" name :uri)) ,uri)))

(defuri :clisp-version "http://ftp.gnu.org/pub/gnu/clisp/release/")
(defuri :7za       "http://sourceforge.net/projects/sevenzip/files/7-Zip/9.20/7za920.zip/download#")
(defuri :abcl-bin  "https://common-lisp.net/project/armedbear/releases/")
(defuri :ccl-bin   "http://ccl.clozure.com/ftp/pub/release/")
(defuri :clisp     "http://sourceforge.net/projects/clisp/files/clisp/")
(defuri :ecl       "https://github.com/roswell/mirror-ecl/archive/")
(defuri :sbcl      "https://github.com/sbcl/sbcl/archive/")
(defuri :quicklisp "http://beta.quicklisp.org/")

(defuri :ffcall    "http://www.haible.de/bruno/gnu/")
(defuri :sigsegv   "http://ftpmirror.gnu.org/libsigsegv/")
(defuri :msys2     "http://kent.dl.sourceforge.net/project/")

(defuri :sbcl-patch1  "https://gist.githubusercontent.com/snmsts/e8e4fd4bd5e458ac45e8/raw/bb7f1cd2e8e9a914f4e9b1b5acf889ecf75dfe0c/posix-tests.patch")
(defuri :clisp-patch1 "https://raw.githubusercontent.com/Homebrew/homebrew/2fb8cb1a2279f80dc89900b3ebaca9e5afc90494/Library/Formula/clisp.rb")

(defuri :ecl-git-version "https://github.com/roswell/mirror-ecl/releases.atom")
(defuri :sbcl-git-version "https://github.com/sbcl/sbcl/releases.atom")
(defuri :asdf-git-version "https://github.com/fare/asdf/releases.atom")
