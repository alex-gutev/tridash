Tridash
=======

Tridash is a programming language which aims to make application state
management easier and less error-prone.

For tutorials, documentation and a philosophy behind the language
visit the project's
[wiki](https://github.com/alex-gutev/tridash/wiki).


Installation
============

Binary Distribution
-------------------

_These instructions are for distributions containing a prebuilt binary_

Run the `install.sh` script. This will install the Tridash compiler
and related files to the `/usr/local` prefix. If this requires root
privileges, the script will have to be run with the `sudo` command:

`sudo ./install.sh`

Building from source
--------------------

### Prerequisites:

1. [SBCL](http://www.sbcl.org) Common Lisp compiler. Other common lisp
   implementations may also work however are not officially supported.
2. [Quicklisp](https://www.quicklisp.org/beta/) to download the
   project's dependencies.

### Building:

These build instructions are for Linux and UNIX like systems. To build
on Windows using the following instructions,
[Cygwin](https://www.cygwin.com) or [MinGW](http://www.mingw.org) has
to be installed.

1. Run `make` in the current directory.

   To install at a different prefix, other than `/usr/local` run:

   `make PREFIX=<prefix>`

   The prefix has to be specified at build time as the core module and
   runtime library paths are embedded in the executable.

   _To compile with a different Common Lisp compiler, or if `sbcl` is
   not in your `PATH`, set the `LISP` environment variable to the
   Common Lisp compiler._

2. Run `sudo make install`.

   The prefix has to be specified, `sudo make PREFIX=<prefix>
   install`, in this step as well, if the compiler is to be installed
   at a prefix other than `/use/local`.

   _The `DESTDIR` variable is supported._
