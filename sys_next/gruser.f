C*GRUSER -- get user name (NeXT version)
C+
      SUBROUTINE GRUSER(STRING, L)
      CHARACTER*(*) STRING
      INTEGER L
C
C Return the name of the user running the program.
C
C Arguments:
C  STRING : receives user name, truncated or extended with
C           blanks as necessary.
C  L      : receives the number of characters in VALUE, excluding
C           trailing blanks.
C--
C 1991-Jul-02 - From SUN version [AFT]
C-----------------------------------------------------------------------
      INTEGER I
C
      CALL CGTENV('USER'//CHAR(0), STRING)
      IF (STRING.EQ.' ') THEN
          L = 0
      ELSE
          DO 10 I=LEN(STRING),1,-1
              L = I
              IF (STRING(I:I).NE.' ') GOTO 20
   10     CONTINUE
          L = 0
   20     CONTINUE
      END IF
      END
