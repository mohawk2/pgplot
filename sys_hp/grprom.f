C*GRPROM -- prompt user before clearing screen (Sun/Convex-UNIX)
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
C 15-Dec-1988
C  5-Aug-1992 assume program run from a terminal
C-----------------------------------------------------------------------
      INTEGER IER
C
      WRITE (6, '(A,A,$)', IOSTAT=IER)
     :CHAR(7), 'Type <RETURN> for next page: '
      IF (IER.EQ.0) READ (5, '(1X)', IOSTAT=IER)
      RETURN
      END
