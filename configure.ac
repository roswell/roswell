#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.65])
AC_INIT([roswell],[0.0.2.22],snmsts@gmail.com)

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

inc_windows=no
case "$host_os" in
  *mingw* ) inc_windows=yes ;;
esac

wwwlib=no
AM_CONDITIONAL([COND_WINDOWS], [test "$inc_windows" = yes])
#AC_CHECK_LIB([wininet], [main],[LIBS="-lwininet $LIBS";wwwlib=yes],[])
AC_CHECK_LIB([curl], [curl_global_init],[LIBS="-lcurl $LIBS";wwwlib=yes],[])

if test "x$wwwlib" = xno; then
  AC_MSG_ERROR([libcurl development files required]);
fi

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS(stdlib.h stdio.h sys/stat.h curl/curl.h)

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_OUTPUT
