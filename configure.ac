#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.59])
AC_INIT([roswell],[19.09.12.102],snmsts@gmail.com)



AM_CONFIG_HEADER(config.h)
AC_CONFIG_FILES([Makefile src/Makefile])
AC_CONFIG_SRCDIR([Makefile.am])
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

AC_CHECK_HEADERS(windows.h,[with_windows=true])
AM_CONDITIONAL([WITH_WINDOWS], [test "$with_windows" != ""])
AC_CHECK_TOOL([WINDRES], [windres], [])
AM_CONDITIONAL([WITH_WIN_ICON], [test "$WINDRES" != ""])

AC_CHECK_MEMBERS([struct dirent.d_type],,,[#include <dirent.h>])

# Git is optional, so fail gracefully, build from tar may not have .git/
AC_CHECK_PROG(GIT_CHECK,git,yes)
AS_IF([test "x$GIT_CHECK" = xyes],
      [AC_SUBST([git_version], `set -o pipefail ; { git describe  | sed -n -e 's/^.*-g//p' ; } 2>/dev/null || echo 'NO-GIT-REVISION'
`,[git SHA1 for HEAD.])],
      [AC_SUBST([git_version],[NO-GIT-REVISION])])
      
AS_IF([test "x$GIT_CHECK" = xyes],      
      [AC_SUBST([git_version_long], `set -e | git describe --abbrev=7 --dirty 2>/dev/null || echo 'NO-GIT-REVISION'`, [long version for git source])],
      [AC_SUBST([git_version_long], [NO-GIT-REVISION])])


# Define rosdir and rospatchdir
# TODO: Make these ./configure line options.
# See https://www.gnu.org/prep/standards/html_node/Directory-Variables.html#Directory-Variables
# Especially 'lispdir' example for emacs.
roslispdir='${sysconfdir}/roswell'
AC_SUBST(roslispdir)
roslisppatchdir='${roslispdir}/patch'
AC_SUBST(roslisppatchdir)

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
  [AC_HELP_STRING([--disable-manual-install],[install manual if enabled])],
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

AM_COND_IF([MANUAL_INSTALL],
           [AC_CONFIG_FILES([documents/Makefile])])

AC_OUTPUT
