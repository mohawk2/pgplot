C*GRPROM -- prompt user before clearing screen (NeXT version)
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
C 1991-Jul-02 - From SUN version [AFT]
C-----------------------------------------------------------------------
      INTEGER IER
C
C      IF (ISATTY(0) .AND. ISATTY(5)) THEN
          WRITE (*, '(A,A,$)', IOSTAT=IER)
     1        CHAR(7), 'Type <RETURN> for next page: '
          IF (IER.EQ.0) READ (*, '(1X)', IOSTAT=IER)
C      END IF
      RETURN
      END
