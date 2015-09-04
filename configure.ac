#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.65])
AC_INIT([roswell],[0.0.3.46],snmsts@gmail.com)

AM_CONFIG_HEADER(config.h)
AC_CONFIG_FILES([Makefile src/Makefile])
AC_CONFIG_SRCDIR([src/lsp.c])
AM_INIT_AUTOMAKE([foreign])
AM_MAINTAINER_MODE([enable])

# Checks for programs.
AC_PROG_CC
AM_PROG_CC_C_O
#AC_PROG_INSTALL

# Checks for libraries.
wwwlib=no

AC_CHECK_LIB([wininet], [main],[LIBS="-lwininet $LIBS";wwwlib=yes],[])
if test "x$wwwlib" = xno; then
  AC_CHECK_LIB([curl], [curl_global_init],[LIBS="-lcurl $LIBS";wwwlib=yes],[])
  AC_CHECK_HEADERS(curl/curl.h)
fi

if test "x$wwwlib" = xno; then
  AC_MSG_ERROR([libcurl development files required]);
fi

AC_CHECK_HEADERS(windows.h)

# Change master uri for sbcl-bin
AC_ARG_WITH([sbcl_bin_base],
  [AC_HELP_STRING([--with-sbcl-bin-base=URI],[set URI for sbcl-bin [default=http://prdownloads.sourceforge.net/sbcl/]])],
  [SBCL_BIN_BASE_URI=$withval],[SBCL_BIN_BASE_URI="http://prdownloads.sourceforge.net/sbcl/"])
AC_DEFINE_UNQUOTED([SBCL_BIN_BASE_URI],["${SBCL_BIN_BASE_URI}"],[uri for sbcl master])

# man file (ros.1) generation switch
AC_ARG_ENABLE([manual_generation],
  [AC_HELP_STRING([--enable-manual-generation],[generate manual if enabled])],
  [case "${enableval}" in
    yes) manual_generate=true ;;
    no)  manual_generate=false ;;
    *) AC_MSG_ERROR([bad value ${enableval} for --enable-manual-generate]) ;;
   esac],[])
AM_CONDITIONAL(MANUAL_GENERATE,[test x$manual_generate = x"true"])

AC_OUTPUT
