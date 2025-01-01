C*VPDRIV -- PGPLOT PostScript driver (portrait mode)
C+
      SUBROUTINE VPDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C--
      CALL GRPS01(IFUNC, RBUF, NBUF, CHR, LCHR, 1)
      END
