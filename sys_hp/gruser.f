**GRUSER -- get user name (HPUX)
*******************************************************************
      SUBROUTINE GRUSER(STRING, L)
      CHARACTER*(*) STRING
*******************************************************************
* Return the name of the user running the program.
*
* Arguments:
*  STRING : receives user name, truncated or extended with
*           blanks as necessary.
*  L      : receives the number of characters in VALUE, excluding
*           trailing blanks.
*
* 31-Jul-1992
*------------------------------------------------------------------
      CALL GETENV('LOGNAME',STRING)
      IF (STRING.EQ.' ') THEN
        L = 0
      ELSE
	L = LEN(STRING)+1
   10   L = L-1
	IF (STRING(L:L).EQ.' ') GO TO 10
      ENDIF
      RETURN
      END
