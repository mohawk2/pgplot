C*GRRE04 -- PGPLOT Retrographics driver, cursor routine
C+
      SUBROUTINE GRRE04 (IX, IY, IC, IER, UNIT)
      INTEGER IX, IY, IC, IER, UNIT
C
C Arguments:
C   IX, IY (in/out) : initial/final coordinates of cursor (device 
C                     coordinates).
C   IC     (output) : character code.
C   IER    (output) : error status (1 => OK).
C   UNIT   (input)  : channel for output to device.
C
C-----------------------------------------------------------------------
      INTEGER BUFFER(7), PROMPT(10)
      DATA PROMPT/29, 4*0, 27, 47, 102, 27, 26/
      INTEGER I1, I2, N
C
      PROMPT(2) = ((IY/32).AND.31).OR.32
      PROMPT(3) = MOD(IY,32).OR.96
      PROMPT(4) = ((IX/32).AND.31).OR.32
      PROMPT(5) = MOD(IX,32).OR.64
      N = 10
      CALL GRWTERM(UNIT, PROMPT, N)
      BUFFER(1) = 0
      BUFFER(2) = PROMPT(2)
      BUFFER(3) = PROMPT(3)
      BUFFER(4) = PROMPT(4)
      BUFFER(5) = PROMPT(5)
      N = 6
      CALL GRRTERM(UNIT, BUFFER, N)
      IC = BUFFER(1)
      I1 = BUFFER(2).AND.31
      I2 = BUFFER(3).AND.31
      IX = I1*32 + I2
      I1 = BUFFER(4).AND.31
      I2 = BUFFER(5).AND.31
      IY = I1*32 + I2
      IER = 1
C-----------------------------------------------------------------------
      END
