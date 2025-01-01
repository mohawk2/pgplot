
C*GRMSG -- issue message to user (VMS)
C+
      SUBROUTINE GRMSG (TEXT)
      CHARACTER*(*) TEXT
C
C Display a message on standard output.
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
          CALL LIB$PUT_OUTPUT(TEXT(1:I))
      END IF
      END
