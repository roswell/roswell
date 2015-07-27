#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.65])
AC_INIT([roswell],[0.0.3.39],snmsts@gmail.com)

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

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_OUTPUT
