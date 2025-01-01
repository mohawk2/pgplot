
C*GRWARN -- issue warning message to user (VMS)
C+
      SUBROUTINE GRWARN (TEXT)
      CHARACTER*(*) TEXT
C
C Report a warning message on standard error, with prefix "%PGPLOT, ".
C In VMS, I/O is done with routine LIB$PUT_OUTPUT.
C
C Argument:
C  TEXT (input): text of message to be printed (the string
C      may not be blank).
C--
C  1-Feb-1983
C 23-Sep-1985 [TJP] - change prefix from GRPCKG to PGPLOT.
C-----------------------------------------------------------------------
      INTEGER   I
C
      IF (TEXT.NE.' ') THEN
          I = LEN(TEXT)
          DO WHILE (TEXT(I:I).EQ.' ')
              I = I-1
          END DO
          CALL LIB$PUT_OUTPUT('%PGPLOT, '//TEXT(1:I) )
      END IF
      END
