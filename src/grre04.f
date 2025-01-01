C*GRRE04 -- PGPLOT Retrographics driver, cursor routine
C+
      SUBROUTINE GRRE04 (UNIT, IX, IY, IC, IER)
      INTEGER   UNIT, IX, IY, IC, IER
C
C Arguments:
C   UNIT   (input)  : channel for output to device.
C   IX, IY (in/out) : initial/final coordinates of cursor (device 
C                     coordinates).
C   IC     (output) : character code.
C   IER    (output) : error status (0 is OK).
C
C-----------------------------------------------------------------------
      CHARACTER CBUF*8, CPROM*10
      INTEGER   I1, I2, LBUF
C
C Position cursor (by drawing a dark vector).
      CPROM(1:10)=CHAR(29)//
     :            CHAR(32+(IY/32))//
     :            CHAR(96+MOD(IY,32))//
     :            CHAR(32+(IX/32))//
     :            CHAR(64+MOD(IX,32))//
     :            CHAR(27)//CHAR(47)//CHAR(102)//
     :            CHAR(27)//CHAR(26)
C
C Do a prompt with read.
      LBUF = 6
      CALL GRPTER(UNIT, CPROM, 10, CBUF, LBUF)
C Must read at least 5 characters.
      IF(LBUF.LT.5) THEN
         IER=1
      ELSE
C Decode coordinates.
         IC = ICHAR( CBUF(1:1) )
         I1 = MOD( ICHAR(CBUF(2:2)), 32 )
         I2 = MOD( ICHAR(CBUF(3:3)), 32 )
         IX = I1*32 + I2
         I1 = MOD( ICHAR(CBUF(4:4)), 32 )
         I2 = MOD( ICHAR(CBUF(5:5)), 32 )
         IY = I1*32 + I2
         IER = 0
      END IF
      RETURN
C-----------------------------------------------------------------------
      END
