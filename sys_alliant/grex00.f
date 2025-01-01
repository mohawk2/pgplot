C*GREX00 -- PGPLOT Talaris/EXCL driver, flush buffer
C+
      SUBROUTINE GREX00 (LUN, BUF, SIZ)
      CHARACTER*(*) BUF
      INTEGER LUN, SIZ
C--
      WRITE (LUN, '(A)') BUF(1:SIZ)
      END
