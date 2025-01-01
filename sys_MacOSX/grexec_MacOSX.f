C*GREXEC -- PGPLOT device handler dispatch routine
C*
C*    KS/AAO 14th Aug 2002. Very minimal driver for MacOSX.
C+
      SUBROUTINE GREXEC(IDEV,IFUNC,RBUF,NBUF,CHR,LCHR)
      INTEGER IDEV, IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C---
      INTEGER NDEV
      PARAMETER (NDEV=4)
      CHARACTER*10 MSG
C---
      GOTO(1,2,3,4) IDEV
      IF (IDEV.EQ.0) THEN
          RBUF(1) = NDEV
          NBUF = 1
      ELSE
          WRITE (MSG,'(I10)') IDEV
          CALL GRWARN('Unknown device code in GREXEC: '//MSG)
      END IF
      RETURN
C---
    1 CALL NUDRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
      RETURN
    2 CALL PSDRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
      RETURN
    3 CALL XWDRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
      RETURN
    4 CALL X2DRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
      RETURN
C
      END
