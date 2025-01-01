C*GRLDEV -- list supported device types
C+
      SUBROUTINE GRLDEV
C
C Support routine for PGLDEV.  Makes use of information in grpckg1.inc
C file.
C
C Arguments: none
C--
C  5-Aug-1986 [AFT]
C 13-Dec-1990 Change warnings to messages [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER I,K,NDEV,NUM,NBUF,LCHR
      REAL    RBUF(6)
      CHARACTER*18 CNAM(4),CHR
      CHARACTER*72 TEXT
C
  111 FORMAT(4A18)
C---
      CALL GRMSG('Legal PGPLOT device types are:')
      NUM=0
C
C--- First obtain number of devices.
      CALL GREXEC(0,0,RBUF,NBUF,CHR,LCHR)
      NDEV=NINT(RBUF(1))
C
      DO 10 I=1,NDEV
          NUM=NUM+1
          CALL GREXEC(I, 1,RBUF,NBUF,CHR,LCHR)
          CNAM(NUM)='/'//CHR
          IF(NUM.GE.4) THEN
              NUM=0
              WRITE (TEXT,111) CNAM(1),CNAM(2),CNAM(3),CNAM(4)
              CALL GRMSG(TEXT)
          END IF
   10 CONTINUE
C
C Lastly list partial line, if needed.
C
      IF (NUM.GT.0) THEN
          WRITE (TEXT,111) (CNAM(K),K=1,NUM)
          CALL GRMSG(TEXT)
      END IF
      END
