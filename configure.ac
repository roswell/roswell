#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.65])
AC_INIT([roswell],[0.0.1.9],snmsts@gmail.com)

AM_CONFIG_HEADER(config.h)
AC_CONFIG_FILES([Makefile src/Makefile])
AC_CONFIG_SRCDIR([src/lsp.c])
AM_INIT_AUTOMAKE
AM_MAINTAINER_MODE([enable])

# Checks for programs.
AC_PROG_CC
AM_PROG_CC_C_O
#AC_PROG_INSTALL

# Checks for libraries.

AC_CHECK_LIB([curl], [curl_global_init],[],[AC_MSG_ERROR([libcurl development files required])])
AC_CHECK_LIB([archive], [archive_read_open_filename],[],[AC_MSG_ERROR([libarchive is needed to compile lsp])])

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS(stdlib.h stdio.h sys/stat.h archive.h)

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_OUTPUT
