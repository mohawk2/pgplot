C*GRMSG -- issue message to user (Sun/Convex-UNIX)
C+
      SUBROUTINE GRMSG (TEXT)
      CHARACTER*(*) TEXT
C
C Display a message on standard error.
C It is assumed that Fortran unit 0 is attached to stderr.
C
C Argument:
C  TEXT (input): text of message to be printed (the string
C      may not be blank).
C--
C 13-Dec-1990 [TJP].
C-----------------------------------------------------------------------
      INTEGER   I
C
      IF (TEXT.NE.' ') THEN
          I = LEN(TEXT)
   10     IF (TEXT(I:I).EQ.' ') THEN
              I = I-1
          GOTO 10
          END IF
          WRITE (0, '(A)') TEXT(1:I)
      END IF
      END
