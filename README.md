# Roswell - Common Lisp environment setup Utility.

[![Build Status](https://travis-ci.org/roswell/roswell.svg?branch=master)](https://travis-ci.org/roswell/roswell)
[![Circle CI](https://circleci.com/gh/roswell/roswell/tree/master.svg?style=svg)](https://circleci.com/gh/roswell/roswell/tree/master)
[![Build status](https://ci.appveyor.com/api/projects/status/7jq0392ffqb4i3yn?svg=true)](https://ci.appveyor.com/project/snmsts/roswell-en89n)
[![Quicklisp](http://quickdocs.org/badge/roswell.svg)](http://quickdocs.org/roswell/)

Roswell is a Lisp implementation installer/manager, launcher, and much more!

Roswell started out as a command-line tool with the aim to make installing and managing Common Lisp implementations really simple and easy.

Roswell has now evolved into a full-stack environment for Common Lisp development, and has many features that makes it easy to test, share, and distribute your Lisp applications.
With Roswell, we aim to push the Common Lisp community to a whole new level of productivity.

Roswell is still in beta. Despite this, the basic interfaces are stable and not likely to change. Roswell currently works well on Unix-like platforms such as Linux, Mac OS X and FreeBSD.
Roswell also works on other operating systems, but currently some parts or features might be missing or unstable.

Checkout [issues list](https://github.com/roswell/roswell/issues) if you are interested in what's lacking.

## Installation, Dependency & Usage

See our [github wiki](https://github.com/roswell/roswell/wiki).
We provide prebuilt binaries for homebrew on OSX, AUR on Arch and **also on Windows**.
* [Linux (arch)](https://github.com/roswell/roswell/wiki/1.-Installation#linux)
* [Mac OS X (brew)](https://github.com/roswell/roswell/wiki/1.-Installation#mac-os-x--homebrew-----use-the-custom-tap)
* [Windows](https://github.com/roswell/roswell/wiki/1.-Installation#windows)
* [Building from Source](https://github.com/roswell/roswell/wiki/1.-Installation#building-from-source)

## Features

[Feature comparison is available!](https://github.com/roswell/roswell/wiki/0.-What's-the-Difference%3F)

* Implementation Manager (similar to CIM)
* Scripting environment (similar to cl-launch)
* Building utility (similar to buildapp)
* **Novel** : Easier setup for initializing a script
* **Novel** : Better integration to the command-line interface (Bash completion, etc)
* **Novel** : Infrastructure for bundling/installing the scripts to/from a quicklisp system
* **Novel** : Better support for Windows environment (tested exhaustively)
* **Novel** : Better integration to CI environment (e.g. Travis-CI, CircleCI, Coverall)

## Usage

Roswell has git-like subcommands which resemble that of cl-launch, buildapp and CIM.

```
$ ros
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

### Managing/Installing Several Lisp Installations

    $ ros install               # displays a list of all installable implementations
    
    $ ros install sbcl-bin      # default sbcl
    $ ros install sbcl          # The newest released version of sbcl
    $ ros install ccl-bin       # default prebuilt binary of ccl
    $ ros install sbcl/1.2.0    # A specific version of sbcl
    
    $ ros list installed sbcl   # Listing the installed implementations
    $ ros run -- --version      # check which implementation is used
    SBCL 1.2.15
    $ ros use sbcl/1.2.3        # change the default implementation

To use an implementation that was not installed by roswell, use i.e. `ros use sbcl/system`.

The list of supported implementations continues to grow!


### Scripting with Roswell

```diff
$ ros init
Usage: ros init [template] name [options...]

$ ros init fact
Successfully generated: fact.ros

$ emacs fact.ros
## editing the fact.ros ...

$ cat fact.ros
#!/bin/sh
#|-*- mode:lisp -*-|#
#|
exec ros -Q -- $0 "$@"
|#

(defun fact (n)
  (if (zerop n)
      1
      (* n (fact (1- n)))))

(defun main (n &rest argv)
  (declare (ignore argv))
  (format t "~&Factorial ~D = ~D~%" n (fact (parse-integer n))))

$ ./fact.ros 10
Factorial 10 = 3628800
```

## Chef recipe for roswell

[@Rudolph-Miller](https://github.com/Rudolph-Miller) is providing a Chef recipe for setting roswell up.

- [Rudolph-Miller/chef-roswell](https://github.com/Rudolph-Miller/chef-roswell)

## Roswell with CircleCI

[@fukamachi](https://github.com/fukamachi) use Roswell with CircleCI.

- [fukamachi/legion/circle.yml](https://github.com/fukamachi/legion/blob/master/circle.yml)

## Naming Origin 'roswell'
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
 * Masataro Asai (guicho2.71828@gmail.com)

## Special Thanks to
 * Eitaro Fukamachi (e.arrows@gmail.com) for naming this project.

## Project
 * https://github.com/roswell/roswell

## License
Licensed under the MIT License.
