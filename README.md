# Roswell - Common Lisp environment setup Utility.

[![Build Status](https://travis-ci.org/snmsts/roswell.svg?branch=master)](https://travis-ci.org/snmsts/roswell)
[![Circle CI](https://circleci.com/gh/snmsts/roswell/tree/master.svg?style=svg)](https://circleci.com/gh/snmsts/roswell/tree/master)
[![Build status](https://ci.appveyor.com/api/projects/status/ubs9la7881yarjjg?svg=true)](https://ci.appveyor.com/project/snmsts/roswell)
[![Quicklisp](http://quickdocs.org/badge/roswell.svg)](http://quickdocs.org/roswell/)

This software is still beta. Basic interfaces are fixed, we believe it works well on Unix-like platforms like Linux, Mac OS X and FreeBSD, but some parts are not implemented, not stable on other OSes yet.

## Description
Roswell is a command line tool to install and manage Common Lisp implementations damn easily.

## Limitations
There's a lot of works should be done.

checkout [issue lists](https://github.com/snmsts/roswell/issues) if you have interest in what's lacking.

## Installation

### Building

#### For Mac OS X users

If you're on Mac OS X and wanna use Homebrew, use the custom tap:

    $ brew tap snmsts/roswell
    $ brew install roswell

#### For Arch linux users

You can install Roswell via AUR:

    $ yaourt -S roswell

#### others can build from source

    $ git clone -b release https://github.com/snmsts/roswell.git
    $ cd roswell
    $ sh bootstrap
    $ ./configure
    $ make
    $ sudo make install

## How to use

First of all, you can get sub-commands and command line options by this command:

    $ ros

you'll get like below

```
Usage: ros [OPTIONS] [Command arguments...]
Usage: ros [OPTIONS] [[--] script-path arguments...]

Commands:
    install  Install archive and build it for roswell environment
    config   Get and set options
    setup    Initial setup
    version  Show the roswell version information
    help     Show Command help
    run      Run repl
    use      change default implementation
    init     create new ros script
    dump     Dump image for faster startup or Make Executable
    delete   Delete installed implementations
    list     List Information
    emacs    launch emacs with slime
    build

Options:
    -w CODE     --wrap CODE          shell wrapper CODE to run in roswell
    -m IMAGE    --image IMAGE        build from Lisp image IMAGE
    -L NAME     --lisp NAME          try use these LISP implementation
    -l FILE     --load FILE          load lisp FILE while building
    -S X        --source-registry X  override source registry of asdf systems
    -s SYSTEM   --system SYSTEM      load asdf SYSTEM while building
                --load-system SYSTEM same as above (buildapp compatibility)
    -p PACKAGE  --package PACKAGE    change current package to PACKAGE
    -sp SP      --system-package SP  combination of -s SP and -p SP
    -e FORM     --eval FORM          evaluate FORM while building
                --require MODULE     require MODULE while building
    -q          --quit               quit lisp here
    -r FUNC     --restart FUNC       restart from build by calling (FUNC)
    -E FUNC     --entry FUNC         restart from build by calling (FUNC argv)
    -i FORM     --init FORM          evaluate FORM after restart
    -ip FORM    --print FORM         evaluate and princ FORM after restart
    -iw FORM    --write FORM         evaluate and write FORM after restart
    -F FORM     --final FORM         evaluate FORM before dumping IMAGE
    -R          --rc                 try read /etc/rosrc, ~/.roswell/init.lisp
    +R          --no-rc              skip /etc/rosrc, ~/.roswell/init.lisp
    -Q          --quicklisp          use quicklisp (default)
    +Q          --no-quicklisp       do not use quicklisp
    -v          --verbose            be quite noisy while building
                --quiet              be quite quiet while building (default)
                --test               for test purpose
```

### Installing Lisps

If you want to install the newest released version of sbcl you can do it by:

    $ ros install sbcl

and if you have specific version like "1.2.0" to install then

    $ ros install sbcl/1.2.0

will install that version.

You can get which version of SBCL are installed by this command:

    $ ros list installed sbcl

#### Supported Implementations and compile options.

So far, `sbcl`,`sbcl-bin`,`ccl-bin` can be parameter for `ros install`.

You can see compile option by

    $ ros help install sbcl

### Setting the default implementation used by ros

    $ ros run -- --version

will show you which version are used. You can change implementation by

    $ ros use sbcl/1.2.3

## Using roswell with Emacs and SLIME

To setup slime with roswell, type like this

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

Or, Just simply `ros emacs` would launch emacs with slime setup.

## Chef recipe for roswell

[@Rudolph-Miller](https://github.com/Rudolph-Miller) is providing a Chef recipe for setting roswell up.

- [Rudolph-Miller/chef-roswell](https://github.com/Rudolph-Miller/chef-roswell)

## Roswell with CircleCI

[@fukamachi](https://github.com/fukamachi) use Roswell with CircleCI.

- [fukamachi/legion/circle.yml](https://github.com/fukamachi/legion/blob/master/circle.yml)

## Where to put my local project?
You can put at ~/.roswell/local-projects/ (From version 0.0.3.34).

## Why we named it 'roswell'?
From &lsquo;[made with secret alien technology](http://www.lisperati.com/logo.html)&rsquo;.

I felt making it easier to use Lisp for people is a kind of making opportunity for humanbeings to mingle with alien technology. I recall &lsquo;roswell incident&rsquo; by the concept. I'm not sure what you feel.

## See Also
+ [keens/cim](https://github.com/keens/cim) : similar project that is implemented in shellscript.I motivated to implement roswell by seeing this project.
+ [cl-launch](http://cliki.net/cl-launch) : influenced by the project of command line parameters design.

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
Licensed under the MIT License.
