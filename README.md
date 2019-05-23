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

To uninstall run the `uninstall.sh` script.

_These scripts are simply convenience wrappers over `make install` and
`make uninstall`._

Building from source
--------------------

### Prerequisites:

1. [SBCL](http://www.sbcl.org) Common Lisp compiler. Other common lisp
   implementations may also work however are not officially supported.
2. [Quicklisp](https://www.quicklisp.org/beta/) to download the
   project's dependencies.

### Building:

These build instructions are for Linux and UNIX like systems. To build
on Windows, using the following instructions,
[Cygwin](https://www.cygwin.com) or [MinGW](http://www.mingw.org) is
required.

1. Run `./configure` in the source directory.

   _For an out-of-source build, e.g. in a `build` directory, navigate
   to the build directory and run `../configure`._

   This command configures the build for installation at the
   `/usr/local` prefix. You can specify a different prefix with the
   prefix option, e.g:

   `./configure --prefix=/usr`

   The common lisp compiler can be set with the `LISP=<compiler>`
   option. By default the script searches for `sbcl`.

   Run `./configure --help` for a listing of all configuration
   options.

1. Run `make` in the current directory.

2. Run `sudo make install`.

   _If root privileges are not required for installation `sudo` can be omitted._

   _The `DESTDIR` variable is supported for staged installs._

To uninstall run `sudo make uninstall` from the same directory.
