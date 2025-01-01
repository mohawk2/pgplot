
C*GRIM03 -- PGPLOT Impress driver, copy buffer to file
C+
      SUBROUTINE GRIM03 (BUFFER, UNIT, N)
      BYTE BUFFER(*)
      INTEGER UNIT, N
C
C Arguments:
C   BUFFER (input) address of buffer to be output
C   UNIT   (input) unit number for output
C   N      (input) number of bytes to transfer
C          (output) set to zero
C-----------------------------------------------------------------------
      INTEGER J
C-----------------------------------------------------------------------
      IF (N.GT.0) WRITE (UNIT) (BUFFER(J),J=1,N)
      N = 0
C-----------------------------------------------------------------------
      END
