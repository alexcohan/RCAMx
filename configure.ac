AC_INIT(RCAMx, version-0.1)

LDFLAGS = `"${R_HOME}/bin/R" CMD config LDFLAGS`
LIBnn = `"${R_HOME}/bin/R" CMD config LIBnn`

AC_SUBST(LDFLAGS)
AC_SUBST(LIBnn)

AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
