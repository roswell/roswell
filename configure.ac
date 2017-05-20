#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.59])
AC_INIT([roswell],[17.5.8.77],snmsts@gmail.com)

AM_CONFIG_HEADER(config.h)
AC_CONFIG_FILES([Makefile src/Makefile documents/Makefile lisp/Makefile scripts/Makefile])
AC_CONFIG_SRCDIR([src/Makefile.am])
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

AC_CHECK_TOOL([WINDRES], [windres], [])
AM_CONDITIONAL([WITH_WIN_ICON], [test "$WINDRES" != ""])

# Change master uri for sbcl-bin
AC_ARG_WITH([sbcl_bin_base],
  [AC_HELP_STRING([--with-sbcl-bin-base=URI],[set URI for sbcl-bin [default=https://github.com/roswell/sbcl_bin/releases/download/]])],
  [SBCL_BIN_URI=$withval],[SBCL_BIN_URI="https://github.com/roswell/sbcl_bin/releases/download/"])
AC_DEFINE_UNQUOTED([SBCL_BIN_URI],["${SBCL_BIN_URI}"],[uri for sbcl master])

AC_ARG_WITH([platformhtml_base],
  [AC_HELP_STRING([--with-platformhtml-base=URI],[set URI for platform.html [default=https://github.com/roswell/sbcl_bin/releases/download/files/build.html]])],
  [PLATFORM_HTML_URI=$withval],[PLATFORM_HTML_URI=https://github.com/roswell/sbcl_bin/releases/download/files/build.html])
AC_DEFINE_UNQUOTED([PLATFORM_HTML_URI],["${PLATFORM_HTML_URI}"],[uri for platform.html])

AC_ARG_WITH([platformhtml_backup_base],
  [AC_HELP_STRING([--with-platformhtml-backup-base=URI],[set backup URI for platform.html [default=http://www.sbcl.org/]])],
  [PLATFORM_HTML_BACKUP_URI=$withval],[PLATFORM_HTML_BACKUP_URI="http://www.sbcl.org/platform-table.html"])
AC_DEFINE_UNQUOTED([PLATFORM_HTML_BACKUP_URI],["${PLATFORM_HTML_BACKUP_URI}"],[backup uri for platform.html])

# man file (ros.1) generation switch
AC_ARG_ENABLE([manual_generation],
  [AC_HELP_STRING([--enable-manual-generation],[generate manual if enabled])],
  [case "${enableval}" in
    yes) manual_generate=true ;;
    no)  manual_generate=false ;;
    *) AC_MSG_ERROR([bad value ${enableval} for --enable-manual-generate]) ;;
   esac],[])
AM_CONDITIONAL(MANUAL_GENERATE,[test x$manual_generate = x"true"])

AC_ARG_ENABLE([manual_install],
  [AC_HELP_STRING([--enable-manual-installation],[install manual if enabled])],
  [case "${enableval}" in
    yes) manual_install=true ;;
    no)  manual_install=false ;;
    *) AC_MSG_ERROR([bad value ${enableval} for --enable-manual-install]) ;;
   esac],[])
AM_CONDITIONAL(MANUAL_INSTALL,[test x$manual_install != x"false"])

AC_ARG_ENABLE([html_generation],
  [AC_HELP_STRING([--enable-html-generation],[generate html if enabled])],
  [case "${enableval}" in
    yes) html_generate=true ;;
    no)  html_generate=false ;;
    *) AC_MSG_ERROR([bad value ${enableval} for --enable-html-generate]) ;;
   esac],[])
AM_CONDITIONAL(HTML_GENERATE,[test x$html_generate = x"true"])

dnl FIX ME. PKG_CHECK_MODULES fails with pkg-config.
with_bash_completion=no
dnl AC_ARG_WITH([bash-completion],
dnl     AS_HELP_STRING([--with-bash-completion@<:@=INSTALL_DIR@:>@],
dnl         [Install the bash auto-completion script in this directory.]),
dnl     [BASH_COMPLETION_DIR=$withval],
dnl     [with_bash_completion=no])

dnl AS_IF([test "x$with_bash_completion" = "xyes"],
dnl       [PKG_CHECK_MODULES([BASH_COMPLETION], [bash-completion >= 2.0],
dnl          [BASH_COMPLETION_DIR="`pkg-config --variable=completionsdir bash-completion`"],
dnl          [with_bash_completion=no])])
    
AC_SUBST([BASH_COMPLETION_DIR])
AM_CONDITIONAL([ENABLE_BASH_COMPLETION],[test "x$with_bash_completion" != "xno"])

dnl FIX ME.
with_zsh_completion=no
dnl AC_ARG_WITH([zsh-completion],
dnl     AS_HELP_STRING([--with-zsh-completion@<:@=INSTALL_DIR@:>@],
dnl         [Install the zsh auto-completion script in this directory.]),
dnl     [ZSH_COMPLETION_DIR=$withval],
dnl     [with_zsh_completion=no])

dnl AS_IF([test "x$with_zsh_completion" = "xyes"],
dnl       [PKG_CHECK_MODULES([ZSH_COMPLETION], [zsh-completion >= 2.0],
dnl          [ZSH_COMPLETION_DIR="`pkg-config --variable=completionsdir zsh-completion`"],
dnl          [with_zsh_completion=no])])
    
AC_SUBST([ZSH_COMPLETION_DIR])
AM_CONDITIONAL([ENABLE_ZSH_COMPLETION],[test "x$with_zsh_completion" != "xno"])
    
AC_OUTPUT
