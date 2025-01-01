
**GRGENV -- get value of PGPLOT environment parameter (HPUX)
************************************************************************
      SUBROUTINE GRGENV(NAME,VALUE,L)
      CHARACTER*(*) NAME,VALUE
************************************************************************
* Return the value of a PGPLOT environment parameter. In this context,
* environment parameters are UNIX environment variables; e.g. parameter
* ENVOPT is environment variable PGPLOT_ENVOPT. Translation is not
* recursive and is case-sensitive.
*
* Arguments:
*  NAME   : (input) the name of the parameter to evaluate.
*  VALUE  : receives the value of the parameter, truncated or extended
*           with blanks as necessary. If the parameter is undefined,
*           a blank string is returned.
*  L      : receives the number of characters in VALUE, excluding
*           trailing blanks. If the parameter is undefined, zero is
*           returned.
*
* 31-July-1992
*-----------------------------------------------------------------------
      CHARACTER*32 PGNAME
*-----------------------------------------------------------------------
      PGNAME = 'PGPLOT_' // NAME
      LN = INDEX(PGNAME, ' ')-1
      CALL GETENV(PGNAME(:LN),VALUE)
      IF (VALUE.EQ.' ') THEN
        L = 0
      ELSE
	L = LEN(VALUE)+1
   10   L = L-1
	IF (VALUE(L:L).EQ.' ') GO TO 10
      ENDIF
      RETURN
      END
