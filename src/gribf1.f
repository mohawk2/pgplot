
C*GRIBF1 -- fill buffer with a specified character
C+
      SUBROUTINE GRIBF1 (BUFSIZ,BUFFER,FILL)
C
C GRPCKG (internal routine): fill a buffer with a given character.
C
C Arguments:
C
C BUFFER (byte array, input): (address of) the buffer.
C BUFSIZ (integer, input): number of bytes in BUFFER.
C FILL (integer, input): the fill character. BUFSIZ bytes starting at
C       address BUFFER are set to contents of FILL.
C--
C (1-Feb-1983)
C-----------------------------------------------------------------------
      INTEGER  BUFSIZ, I, FILL
      BYTE     BUFFER(BUFSIZ)
C
      DO 10 I=1,BUFSIZ
          BUFFER(I) = FILL
   10 CONTINUE
      END
