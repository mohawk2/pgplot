C*GREXEC -- PGPLOT device handler dispatch routine
C+
      SUBROUTINE GREXEC(IDEV,IFUNC,RBUF,NBUF,CHR,LCHR)
      INTEGER IDEV, IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C---
      INTEGER NDEV
      PARAMETER (NDEV=15)
      CHARACTER*10 MSG
C---
      GOTO(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15) IDEV
      IF (IDEV.EQ.0) THEN
          RBUF(1) = NDEV
          NBUF = 1
      ELSE
          WRITE (MSG,'(I10)') IDEV
          CALL GRWARN('Unknown device code in GREXEC: '//MSG)
      END IF
      RETURN
C---
    1 CALL GFDRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
      RETURN
    2 CALL IMDRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
      RETURN
    3 CALL IVDRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
      RETURN
    4 CALL NUDRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
      RETURN
    5 CALL PSDRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
      RETURN
    6 CALL PXDRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
      RETURN
    7 CALL QMDRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
      RETURN
    8 CALL QPDRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
      RETURN
    9 CALL REDRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
      RETURN
   10 CALL TEDRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
      RETURN
   11 CALL TFDRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
      RETURN
   12 CALL TKDRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
      RETURN
   13 CALL TVDRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
      RETURN
   14 CALL VPDRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
      RETURN
   15 CALL VTDRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
      RETURN
C
      END
