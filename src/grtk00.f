C*GRTK00 -- PGPLOT Tektronix 4100 driver, encode coordinate pair
C+
      SUBROUTINE GRTK00(IX, IY, C)
      INTEGER IX, IY
      CHARACTER*(*) C
C
C Encode XY coordinate pair in host syntax. Input integers IX, IY; 
C output string C, containing 5 characters. The valid range of
C XY coordinates is (0,4095). The output string contains characters
C with ASCII values in the range 32-127. NB the 'delete' character
C (127) can occur in LOY or EXTRA byte; it can be replaced by
C escape-? if desired.
C-----------------------------------------------------------------------
      INTEGER EX1, EX2, LOX, HIX, LOY, HIY
C
      EX2 = MOD(IX, 4)
      LOX = MOD(IX/4, 32)
      HIX = IX/128
      EX1 = MOD(IY, 4)
      LOY = MOD(IY/4, 32)
      HIY = IY/128
      C(1:1) = CHAR(32+HIY)
      C(2:2) = CHAR(96+4*EX1+EX2)
      C(3:3) = CHAR(96+LOY)
      C(4:4) = CHAR(32+HIX)
      C(5:5) = CHAR(64+LOX)
C
      END
