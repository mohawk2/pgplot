
C*GRVT04 -- PGPLOT Regis (VT125) driver, cursor routine
C+
      SUBROUTINE GRVT04 (IX, IY, IC, IER, UNIT)
      INTEGER IX, IY, IC, IER, UNIT
C
C Arguments:
C   IX, IY (in/out) : initial/final coordinates of cursor (device 
C                     coordinates).
C   IC     (output) : character code.
C   IER    (output) : error status (1 => OK).
C   UNIT   (input)  : channel for output to device.
C
C The cursor is moved by using the arrow keys on the
C terminal; the cursor "speed" (step size) is controlled by the
C PF1 (smallest step) to PF4 (largest step) keys.
C
C The user indicates that the cursor has been positioned by
C typing any character on his keyboard (SYS$COMMAND), with the
C following exceptions: control characters (^C, ^O, ^Q, ^R, ^S,
C ^T, ^U, ^X, ^Y, DEL) are intercepted by the operating system
C and cannot be used; NUL, ESC (^[) and escape sequences (e.g., 
C arrow keys) are ignored by GRCURS.
C-----------------------------------------------------------------------
      INTEGER        GRGETC,SYS$ASSIGN, SYS$DASSGN
      INTEGER        STEP, ICHAN, NBYTES
      DATA           STEP/4/      ! initial step size
C-----------------------------------------------------------------------
      IER = SYS$ASSIGN('SYS$COMMAND',ICHAN,,)
      IF (IER.NE.1) RETURN
C     -- position graphics cursor
   10 WRITE (UNIT,111) CHAR(27),IX,IY
  111 FORMAT(A,'PpP[', I4 ,',', I4 ,']')
      IC = GRGETC(ICHAN)
      IF (IC.EQ.-1) THEN
          IY = MIN(479,IY+STEP)
      ELSE IF (IC.EQ.-2) THEN
          IY = MAX(0,IY-STEP)
      ELSE IF (IC.EQ.-3) THEN
          IX = MIN(767,IX+STEP)
      ELSE IF (IC.EQ.-4) THEN
          IX = MAX(0,IX-STEP)
      ELSE IF (IC.EQ.-11) THEN
          STEP = 1
      ELSE IF (IC.EQ.-12) THEN
          STEP = 4
      ELSE IF (IC.EQ.-13) THEN
          STEP = 16
      ELSE IF (IC.EQ.-14) THEN
          STEP = 64
      END IF
C     -- back to text mode
      WRITE (UNIT,112) CHAR(27)
  112 FORMAT(A,'\')
      IF (IC.LE.0 .OR. IC.GT.255) GOTO 10
      IER = SYS$DASSGN(%VAL(ICHAN))
C-----------------------------------------------------------------------
      END
