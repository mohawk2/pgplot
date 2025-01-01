C*GRQM00 -- PGPLOT QMS/QUIC driver, flush buffer
C+
      SUBROUTINE GRQM00 (LUN, BUF, SIZ)
      CHARACTER*(*) BUF
      INTEGER LUN, SIZ
C--
      WRITE (LUN, '(A)') BUF(1:SIZ)
      BUF(1:LEN(BUF)) = ' '
      SIZ = 0
      END
