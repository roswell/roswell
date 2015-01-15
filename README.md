# Roswell - Common Lisp environment setup Utility.

[![Build Status](https://travis-ci.org/snmsts/roswell.svg?branch=master)](https://travis-ci.org/snmsts/roswell)

This software is now beta.Basic interfaces are fixed.I beleive it works well on some platform,
but some part are not implemented,not stable on other os yet.

## Description
Installing Common Lisp and setting up everything just work is still difficult and you have to learn some tips.

## Why we named roswell?
From &lsquo;[made with secret alien technology](http://www.lisperati.com/logo.html)&rsquo;.

I felt making it easier to use Lisp for people is a kind of making opportunity for humanbeings to mingle with alien technology. I recall &lsquo;roswell incident&rsquo; by the concept. I'm not sure what you feel.

## Installation

    $ git clone -b release https://github.com/snmsts/roswell.git
    $ sh bootstrap
    $ ./configure
    $ make
    $ sudo make install

## How to use
### setting up
First of all, you have to install Lisp environment. You can install a binary version of SBCL and Quicklisp from internet by these commands:

    $ ros setup

### command help
After the installation type ros and you'll get some information by

    $ ros

### install lisp
If you want to install the newest released version of sbcl you can do it by

    $ ros install sbcl

and if you have specific version like "1.2.0" to install then

    $ ros install sbcl/1.2.0

will install that version.

### set default implementation used by roswell.

    $ ros list installed sbcl

will return which version of sbcl are installed.

then

    $ ros run -- --version

will show you which version are used.You can change implementation by

    $ ros use sbcl/1.2.3

## Using roswell in Emacs and SLIME
to setup slime with roswell,type like this

```
ros -Q -e '(ql:quickload :quicklisp-slime-helper)' -q
```

Add lines like below to init.el
```lisp
(load (expand-file-name "~/.roswell/impls/ALL/ALL/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "ros -L sbcl -Q run")
```
to load with `.sbclrc`

```lisp
(setq inferior-lisp-program "ros -L sbcl -Q -l ~/.sbclrc run")
```

## Chef recipe for roswell

[@Rudolph-Miller](https://github.com/Rudolph-Miller) is providing a Chef recipe for setting roswell up.

- [Rudolph-Miller/chef-roswell](https://github.com/Rudolph-Miller/chef-roswell)

## See Also
+ [keens/cim](https://github.com/keens/cim) : similar project that is implemented in shellscript.I motivated to implement roswell by seeing this project.

## Author
SANO Masatoshi (snmsts@gmail.com)

## Contributors
 * Eitaro Fukamachi (e.arrows@gmail.com)
 * Tomoya KAWANISHI (chopsticks.tk.ppfm@gmail.com)

## Special Thanks to
 * Eitaro Fukamachi (e.arrows@gmail.com) for naming this project.

## Project
 * https://github.com/snmsts/roswell

## License
MIT
