C*GRVT02 -- PGPLOT Regis (VT125) driver, transfer data to buffer
C+
      SUBROUTINE GRVT02 (INSTR, BUFFER, HWM, UNIT)
      INTEGER   HWM, UNIT
      CHARACTER*(*) INSTR, BUFFER
C
C Arguments:
C  INSTR  (input)  : text of instruction (bytes).
C  BUFFER (in/out) : output buffer.
C  HWM    (in/out) : number of bytes used in BUFFER.
C  UNIT   (input)  : channel number for output (when buffer is full).
C
C Subroutines called:
C   GRVT03
C-----------------------------------------------------------------------
      INTEGER BUFSIZ, N
C-----------------------------------------------------------------------
      BUFSIZ = LEN(BUFFER)
      N = LEN(INSTR)
      IF (HWM+N.GE.BUFSIZ) CALL GRVT03(BUFFER, UNIT, HWM)
      BUFFER(HWM+1:HWM+N) = INSTR(1:N)
      HWM = HWM+N
C-----------------------------------------------------------------------
      END
