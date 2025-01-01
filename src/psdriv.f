C*PSDRIV -- PGPLOT PostScript driver (landscape mode)
C+
      SUBROUTINE PSDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C--
      CALL GRPS01(IFUNC, RBUF, NBUF, CHR, LCHR, 0)
      END
