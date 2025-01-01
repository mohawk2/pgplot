C*GRQUIT -- report a fatal error and abort execution (VMS)
C+
      SUBROUTINE GRQUIT (TEXT)
      CHARACTER*(*) TEXT
C
C Report a fatal error (via GRWARN) and exit with fatal status; a
C traceback is generated unless the program is linked /NOTRACE.
C
C Argument:
C  TEXT (input): text of message to be sent to GRWARN.
C--
C  9-Feb-1988
C-----------------------------------------------------------------------
C
      CALL GRWARN(TEXT)
      CALL LIB$STOP(%VAL('15820C'X))
C     ! "fatal error in library"
      END
