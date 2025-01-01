
C*GRWARN -- issue warning message to user (Sun/Convex-UNIX)
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
C 18-Feb-1988
C-----------------------------------------------------------------------
      INTEGER   I
C
      IF (TEXT.NE.' ') THEN
          I = LEN(TEXT)
   10     IF (TEXT(I:I).EQ.' ') THEN
              I = I-1
          GOTO 10
          END IF
          WRITE (0, '(2A)') '%PGPLOT, ', TEXT(1:I)
      END IF
      END
