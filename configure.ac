#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.65])
AC_INIT([roswell],[0.0.5.59],snmsts@gmail.com)

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

# Change master uri for sbcl-bin
AC_ARG_WITH([sbcl_bin_base],
  [AC_HELP_STRING([--with-sbcl-bin-base=URI],[set URI for sbcl-bin [default=http://prdownloads.sourceforge.net/sbcl/]])],
  [SBCL_BIN_BASE_URI=$withval],[SBCL_BIN_BASE_URI="http://prdownloads.sourceforge.net/sbcl/"])
AC_DEFINE_UNQUOTED([SBCL_BIN_BASE_URI],["${SBCL_BIN_BASE_URI}"],[uri for sbcl master])

AC_ARG_WITH([quicklisp_base],
  [AC_HELP_STRING([--with-quicklisp-base=URI],[set URI for quicklisp [default=http://beta.quicklisp.org/]])],
  [QUICKLISP_BASE_URI=$withval],[QUICKLISP_BASE_URI="http://beta.quicklisp.org/"])
AC_DEFINE_UNQUOTED([QUICKLISP_BASE_URI],["${QUICKLISP_BASE_URI}"],[uri for quicklisp])

AC_ARG_WITH([platformhtml_base],
  [AC_HELP_STRING([--with-platformhtml-base=URI],[set URI for platform.html [default=http://www.sbcl.org/]])],
  [PLATFORM_HTML_BASE_URI=$withval],[PLATFORM_HTML_BASE_URI="http://www.sbcl.org/"])
AC_DEFINE_UNQUOTED([PLATFORM_HTML_BASE_URI],["${PLATFORM_HTML_BASE_URI}"],[uri for platform.html])

AC_ARG_WITH([platformhtml_backup_base],
  [AC_HELP_STRING([--with-platformhtml-backup-base=URI],[set backup URI for platform.html [default is gist]])],
  [PLATFORM_HTML_BACKUP_BASE_URI=$withval],[PLATFORM_HTML_BACKUP_BASE_URI="https://gist.githubusercontent.com/snmsts/12f0cf8567b8e26ae15f/raw/0b9134f9820c195e1ca84d382176777ec989183e/"])
AC_DEFINE_UNQUOTED([PLATFORM_HTML_BACKUP_BASE_URI],["${PLATFORM_HTML_BACKUP_BASE_URI}"],[backup uri for platform.html])

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

# html file generation switch
AC_ARG_ENABLE([html_generation],
  [AC_HELP_STRING([--enable-html-generation],[generate html if enabled])],
  [case "${enableval}" in
    yes) html_generate=true ;;
    no)  html_generate=false ;;
    *) AC_MSG_ERROR([bad value ${enableval} for --enable-html-generate]) ;;
   esac],[])
AM_CONDITIONAL(HTML_GENERATE,[test x$html_generate = x"true"])



AC_ARG_WITH([bash-completion],
    AS_HELP_STRING([--with-bash-completion@<:@=INSTALL_DIR@:>@],
        [Install the bash auto-completion script in this directory. default: yes,
         and INSTALL_DIR will be the correct pathname given by pkg-config.]),
    [],
    [with_bash_completion=yes])

AS_IF([test "x$with_bash_completion" = "xyes"],
      [PKG_CHECK_MODULES([BASH_COMPLETION], [bash-completion >= 2.0],
         [BASH_COMPLETION_DIR="`pkg-config --variable=completionsdir bash-completion`"],
         [with_bash_completion=no])])
    
AC_SUBST([BASH_COMPLETION_DIR])
AM_CONDITIONAL([ENABLE_BASH_COMPLETION],[test "x$with_bash_completion" != "xno"])


AC_ARG_WITH([zsh-completion],
    AS_HELP_STRING([--with-zsh-completion@<:@=INSTALL_DIR@:>@],
        [Install the zsh auto-completion script in this directory. default: yes,
         and INSTALL_DIR will be the correct pathname given by pkg-config.]),
    [],
    [with_zsh_completion=yes])

AS_IF([test "x$with_zsh_completion" = "xyes"],
      [PKG_CHECK_MODULES([ZSH_COMPLETION], [zsh-completion >= 2.0],
         [ZSH_COMPLETION_DIR="`pkg-config --variable=completionsdir zsh-completion`"],
         [with_zsh_completion=no])])
    
AC_SUBST([ZSH_COMPLETION_DIR])
AM_CONDITIONAL([ENABLE_ZSH_COMPLETION],[test "x$with_zsh_completion" != "xno"])




    
AC_OUTPUT
