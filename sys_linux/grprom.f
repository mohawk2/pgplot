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
C  2-Feb-1998. KS/AAO Removed use of ISATTY as a temporary fix.
C-----------------------------------------------------------------------
      INTEGER IER
C
      WRITE (0, '(A,A,$)', IOSTAT=IER)
     1        CHAR(7), 'Type <RETURN> for next page: '
      IF (IER.EQ.0) READ (5, '(1X)', IOSTAT=IER)
      END
