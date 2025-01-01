
C*GRWARN -- issue warning message to user (NeXT version)
C+
      SUBROUTINE GRWARN (TEXT)
      CHARACTER*(*) TEXT
C
C Report a warning message on standard error, with prefix "%PGPLOT, ".
C It is assumed that Fortran unit 0 is attached to stderr.
C
C Argument:
C  TEXT (input): text of message to be printed (the string
C      may not be blank).
C--
C 1991-Jul-03 - From SUN version [AFT]
C-----------------------------------------------------------------------
      INTEGER   I
C
      IF (TEXT.NE.' ') THEN
          I = LEN(TEXT)
   10     IF (TEXT(I:I).EQ.' ') THEN
              I = I-1
          GOTO 10
          END IF
          WRITE (*, '(2A)') '%PGPLOT, ', TEXT(1:I)
      END IF
      END
