C*GRWARN -- issue warning message to user (HPUX)
C+
      SUBROUTINE GRWARN (TEXT)
      CHARACTER*(*) TEXT
C
C Report a warning message on standard error, with prefix "%PGPLOT, ".
C It is assumed that Fortran unit 7 is attached to stderr.
C
C Argument:
C  TEXT (input): text of message to be printed (the string
C      may not be blank).
C--
C 31-Jul-1992
C-----------------------------------------------------------------------
      INTEGER   I
C
      IF (TEXT.NE.' ') THEN
          I = LEN(TEXT)
   10     IF (TEXT(I:I).EQ.' ') THEN
              I = I-1
          GOTO 10
          END IF
          WRITE (7, '(2A)') '%PGPLOT, ', TEXT(1:I)
      ENDIF
      RETURN
      END
