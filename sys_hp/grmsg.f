C*GRMSG -- issue message to user (HPUX)
C+
      SUBROUTINE GRMSG (TEXT)
      CHARACTER*(*) TEXT
C
C Display a message on standard error.
C It is assumed that Fortran unit 7 is attached to stderr.
C
C Argument:
C  TEXT (input): text of message to be printed (the string
C      may not be blank).
C--
C 31-Jul-1992 (MJC)
C-----------------------------------------------------------------------
      INTEGER   I
C
      IF (TEXT.NE.' ') THEN
        I = LEN(TEXT)+1
   10   I = I-1
        IF (TEXT(I:I).EQ.' ') GO TO 10
        WRITE (7,'(A)') TEXT(:I)
      ENDIF
      RETURN
      END
