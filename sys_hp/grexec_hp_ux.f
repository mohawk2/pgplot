**GREXEC -- PGPLOT device handler dispatch routine
***************************************************************
      SUBROUTINE GREXEC(IDEV,IFUNC,RBUF,NBUF,CHR,LCHR)
***************************************************************
      INTEGER IDEV,NDEV,IFUNC,NBUF,LCHR
      REAL RBUF(*)
      CHARACTER*10 MSG
      CHARACTER*(*) CHR
      PARAMETER (NDEV=7)
*--------------------------------------------------------------
      IF (IDEV.EQ.0) THEN
        RBUF(1) = NDEV
        NBUF = 1
	RETURN
      ELSE IF (IDEV.GT.7) THEN
        WRITE (MSG,'(I10)') IDEV
        CALL GRWARN('Unknown device code in GREXEC: '//MSG)
        RETURN
      END IF
      GO TO (1,2,3,4,5,6,7) IDEV
    1 CALL NUDRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
      RETURN
    2 CALL PSDRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
      RETURN
    3 CALL VPDRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
      RETURN
    4 CALL CPDRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
      RETURN
    5 CALL VCDRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
      RETURN
    6 CALL XWDRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
      RETURN
    7 CALL X2DRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
      RETURN
      END
