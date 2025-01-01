**GRGCOM -- read with prompt from user's terminal (HPUX)
*+
      INTEGER FUNCTION GRGCOM(STRING, PROMPT, L)
      CHARACTER*(*) STRING, PROMPT
      INTEGER L
*
* Issue prompt and read a line from the user's terminal; in VMS,
* this is equivalent to LIB$GET_COMMAND.
*
* Arguments:
*  STRING : (output) receives the string read from the terminal.
*  PROMPT : (input) prompt string.
*  L      : (output) length of STRING.
*
* Returns:
*  GRGCOM : 1 if successful, 0 if an error occurs (e.g., end of file).
*--
* 9-Feb-1988
* 10-Feb-1990 revised to always read from stdin (unit 5), but issue a
*             prompt only when device is a terminal.
* 5-Aug-1992  write to unit 6 and assume pgplot is being run from a
*             terminal.
*-----------------------------------------------------------------------
      INTEGER IER
*
      GRGCOM = 0
      L = 0
      IER = 0
      WRITE (6,'(A,$)',IOSTAT=IER) PROMPT
      IF (IER.EQ.0) READ (5,'(A)',IOSTAT=IER) STRING
      IF (IER.EQ.0) GRGCOM = 1
      L = LEN(STRING)+1
   10 L = L-1
      IF (STRING(L:L).NE.' ') GO TO 10
      RETURN
      END
