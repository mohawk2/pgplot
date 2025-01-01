
C*GRZS02 -- PGPLOT ZSTEM driver, encode coordinate pair
C+
      SUBROUTINE GRZS02(I0, J0, CBUF, LBUF)
      INTEGER   I0, J0, LBUF
      CHARACTER CBUF*(*)
C
C Encode XY coordinate pair in host syntax. Input integers IX, IY; 
C output 5 characters in CBUF. The valid range of XY coordinates is
C (0,4095). The output string contains characters with ASCII values
C in the range 32-127. NB the 'delete' character (127) can occur in
C LOY or EXTRA byte; it can be replaced by escape-? if desired.
C-----------------------------------------------------------------------
      INTEGER IEX, ILOX, IHIX, ILOY, IHIY
C
      IHIY = J0/128
      ILOY = MOD(J0/4, 32)
      IHIX = I0/128
      ILOX = MOD(I0/4, 32)
      IEX  = 4*MOD(J0, 4) + MOD(I0, 4)
      CBUF(LBUF+1:LBUF+5)=CHAR(32+IHIY)//
     :                    CHAR(96+IEX)//
     :                    CHAR(96+ILOY)//
     :                    CHAR(32+IHIX)//
     :                    CHAR(64+ILOX)
      LBUF=LBUF+5
      RETURN
C
      END
