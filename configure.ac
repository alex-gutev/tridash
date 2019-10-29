dnl Initialization

AC_INIT([Tridash], [0.6], [alex.gutev@gmail.com])
AC_CONFIG_SRCDIR([src/main.lisp])

dnl Common Lisp Compiler

AC_PATH_PROG(LISP, sbcl)
AC_ARG_VAR(LISP, [Common Lisp compiler])

dnl Other Tools

AC_PATH_PROG(UGLIFYJS, [uglifyjs])
AC_ARG_VAR(UGLIFYJS, [Path to uglifyjs (JavaScript minifier) program])

AC_PROG_SED


dnl Documentation Tools

AC_PATH_PROG(A2X, [a2x])
AC_ARG_VAR(A2X, [Path to a2x (AsciiDoc converter) program])

AC_PATH_PROG(DB2X_XSLTPROC, [db2x_xsltproc])
AC_ARG_VAR(DB2X_XSLTPROC, [Path to db2x_xsltproc program])

AC_PATH_PROG(DB2X_TEXIXML, [db2x_texixml])
AC_ARG_VAR(DB2X_TEXIXML, [Path to db2x_texixml program])

AC_PATH_PROG(MAKEINFO, [makeinfo])
AC_ARG_VAR(MAKEINFO, [Path to makeinfo program])

AC_PATH_PROG(INSTALL_INFO, [install-info])
AC_ARG_VAR(INSTALL_INFO, [Path to install-info program])


dnl Variables

AC_ARG_VAR(LISP_FLAGS, [Additional flags to pass to the lisp compiler])
AC_ARG_VAR(LISP_TEST_FLAGS, [Flags to pass to the lisp environment when testing])


dnl Output

AC_SUBST([PACKAGE], AC_PACKAGE_TARNAME)

AS_MKDIR_P([doc])
AS_MKDIR_P([src/backends/javascript/runtime])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
