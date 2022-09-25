dnl Process this file with autoconf to produce a configure script.
AC_INIT([mmm-mode], [0.5.8])
AC_CONFIG_SRCDIR([mmm-mode.el])
AM_INIT_AUTOMAKE([info-in-builddir])

dnl
dnl Apparently, if you run a shell window in Emacs, it sets the EMACS
dnl environment variable to 't'.  Lets undo the damage.
dnl
if test "${EMACS}" = "t"; then
   EMACS=""
fi

AC_ARG_WITH(xemacs,           --with-xemacs             Use XEmacs to build, [ if test "${withval}" = "yes"; then EMACS=xemacs; else EMACS=${withval}; fi ])
AC_ARG_WITH(emacs,            --with-emacs              Use Emacs to build, [ if test "${withval}" = "yes"; then EMACS=emacs; else EMACS=${withval}; fi ])

AC_CHECK_PROG(EMACS, xemacs, xemacs, emacs)

AM_PATH_LISPDIR

AC_EMACS_VERSION


dnl Checks for programs.

dnl Checks for libraries.

dnl Checks for header files.

dnl Checks for typedefs, structures, and compiler characteristics.

dnl Checks for library functions.

AC_SUBST(EMACS)

AC_OUTPUT(Makefile)
