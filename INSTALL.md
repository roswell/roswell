## Dependencies

+ libcurl (both v3 and v4 should work) : used for downloading the lisp implimentation binaries etc.
+ automake (required when building from the source)
+ developmental headers of libcurl (required when building from the source)

## Building from the Source

Instruction for the system-wide installation follows:

    $ git clone -b release https://github.com/snmsts/roswell.git
    $ cd roswell
    $ sh bootstrap
    $ ./configure
    $ make
    $ sudo make install

**The installation directory is configurable** by specifying `--prefix` option to `./configure`. The default location is `/usr/local`. Actual binary and supporting files are finally installed into `$PREFIX/share/common-lisp/source/roswell/`.

This is useful when using roswell as a backend of CI services (e.g. travis): When you specify a directory accessible without root permission, then you can switch to the new [container-based infrastructure](http://docs.travis-ci.com/user/workers/container-based-infrastructure/) for faster startup (of the tests). 

This option can be used to write an installation script for CI services, but [we already provide such a script for Travis, CircleCI and Coverall](4.-Roswell-as-a-Testing-Environment-(Travis-CI-and-Coverall)) using this feature. 

Example:

    $ git clone -b release https://github.com/snmsts/roswell.git
    $ cd roswell
    $ sh bootstrap
    $ ./configure --prefix ~/.local/
    $ make && make install              ### in this case, no sudo is required!
    $ ~/.local/bin/ros

For the further information, see https://github.com/snmsts/roswell/wiki

