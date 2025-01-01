
C*GRZS03 -- PGPLOT ZSTEM driver, encode coordinate pair, optimize coding.
C+
      SUBROUTINE GRZS03(LASTI, LASTJ, I0, J0, CBUF, LBUF)
      INTEGER   LASTI, LASTJ, I0, J0, LBUF
      CHARACTER CBUF*(*)
C
C Assume cursor is at position LASTI, LASTJ and that the light or
C dark vector condition has been correctly set.  Add up to 5 characters
C to CBUF to draw a vector to I0, J0.  The minimum number of characters
C are encoded to obtain the motion.
C-----------------------------------------------------------------------
      INTEGER IEX, ILOX, IHIX, ILOY, IHIY
      INTEGER LEX, LLOX, LHIX, LLOY, LHIY
C
      LHIY = LASTJ/128
      LLOY = MOD(LASTJ/4, 32)
      LHIX = LASTI/128
      LLOX = MOD(LASTI/4, 32)
      LEX = 4*MOD(LASTJ, 4) + MOD(LASTI, 4)
C
      IHIY = J0/128
      ILOY = MOD(J0/4, 32)
      IHIX = I0/128
      ILOX = MOD(I0/4, 32)
      IEX = 4*MOD(J0, 4) + MOD(I0, 4)
C
      IF(IHIY.NE.LHIY) THEN
          LBUF=LBUF+1
          CBUF(LBUF:LBUF) = CHAR(32+IHIY)
      END IF
      IF(IEX.NE.LEX) THEN
          LBUF=LBUF+1
          CBUF(LBUF:LBUF) = CHAR(96+IEX)
      END IF
      IF(IEX.NE.LEX .OR. ILOY.NE.LLOY .OR. IHIX.NE.LHIX) THEN
          LBUF=LBUF+1
          CBUF(LBUF:LBUF) = CHAR(96+ILOY)
      END IF
      IF(IHIX.NE.LHIX) THEN
          LBUF=LBUF+1
          CBUF(LBUF:LBUF) = CHAR(32+IHIX)
      END IF
      LBUF=LBUF+1
      CBUF(LBUF:LBUF) = CHAR(64+ILOX)
      RETURN
      END
