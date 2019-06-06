dnl Initialization

AC_INIT([Tridash], [0.4], [alex.gutev@gmail.com])
AC_CONFIG_SRCDIR([src/main.lisp])

dnl Common Lisp Compiler

AC_PATH_PROG(LISP, sbcl)
AC_ARG_VAR(LISP, [Common Lisp compiler])

dnl Other Tools

AC_PATH_PROGS(ASCIIDOC, [asciidoctor asciidoc])
AC_ARG_VAR(ASCIIDOC, [Path to AsciiDoc/Asciidoctor processor])

AC_PROG_SED

dnl Output

AC_SUBST([PACKAGE], AC_PACKAGE_TARNAME)

AS_MKDIR_P([doc])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
