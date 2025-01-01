C*GRRE02 -- PGPLOT Retrographics driver, transfer data to buffer
C+
      SUBROUTINE GRRE02 (CADD, LADD, CBUF, LBUF, UNIT)
      INTEGER   LADD, LBUF, UNIT
      CHARACTER CADD*(*), CBUF*(*)
C
C Arguments:
C  CADD   (input)  : text to add to buffer.
C  LADD   (input)  : number of characters to transfer.
C  CBUF   (input)  : output buffer.
C  LBUF   (in/out) : number of valid characters in CBUF.
C  UNIT   (input)  : channel number for output (when buffer is full).
C
C Subroutines called:
C   GRWTER
C-----------------------------------------------------------------------
      INTEGER  I
C-----------------------------------------------------------------------
      IF (LBUF+LADD.GE.LEN(CBUF)) CALL GRWTER(UNIT, CBUF, LBUF)
      DO 10 I=1,LADD
          LBUF = LBUF + 1
          CBUF(LBUF:LBUF) = CADD(I:I)
   10 CONTINUE
C-----------------------------------------------------------------------
      END
