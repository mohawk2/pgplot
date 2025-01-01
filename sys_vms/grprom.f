
C*GRPROM -- prompt user before clearing screen (VMS)
C+
      SUBROUTINE GRPROM
C
C If the program is running under control of a terminal (in VMS,
C if SYS$COMMAND is a terminal), display "Type <RETURN> for next
C page: " and wait for the user to type <CR> before proceeding.
C
C Arguments:
C  none
C--
C 15-Feb-1988
C-----------------------------------------------------------------------
      INTEGER   DC$_TERM, DVI$_DEVCLASS
      PARAMETER (DC$_TERM=66)
      PARAMETER (DVI$_DEVCLASS=4)
      INTEGER  DEVCLASS, LIB$GETDVI
      INTEGER LIB$GET_COMMAND, IER, L
      CHARACTER*4 MESS
C
      IER = LIB$GETDVI(DVI$_DEVCLASS, , 'SYS$COMMAND', DEVCLASS)
      IF (IER.EQ.1 .AND. DEVCLASS.EQ.DC$_TERM) THEN
          IER = LIB$GET_COMMAND(MESS,
     1            CHAR(7)//'Type <RETURN> for next page: ',
     2            L)
      END IF
      END
