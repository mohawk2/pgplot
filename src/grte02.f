C*GRTE02 -- PGPLOT Tektronix-4010 driver, transfer data to buffer
C+
      SUBROUTINE GRTE02 (INSTR, N, BUFFER, HWM, UNIT)
      INTEGER   N, HWM, UNIT
      BYTE      INSTR(*), BUFFER(*)
C
C Arguments:
C  INSTR  (input)  : text of instruction (bytes).
C  N      (input)  : number of bytes to transfer.
C  BUFFER (input)  : output buffer.
C  HWM    (in/out) : number of bytes used in BUFFER.
C  UNIT   (input)  : channel number for output (when buffer is full).
C
C Subroutines called:
C   GRWTRM
C-----------------------------------------------------------------------
      INTEGER BUFSIZ
      PARAMETER (BUFSIZ=1024)
      INTEGER  I
C-----------------------------------------------------------------------
      IF (HWM+N.GE.BUFSIZ) CALL GRWTRM(UNIT, BUFFER, HWM)
      DO 10 I=1,N
          HWM = HWM + 1
          BUFFER(HWM) = INSTR(I)
   10 CONTINUE
C-----------------------------------------------------------------------
      END
