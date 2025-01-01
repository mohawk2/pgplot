C*GRRE01 -- PGPLOT Retrographics driver, line segment
C+
      SUBROUTINE GRRE01 (I0, J0, I1, J1, CONT, CTMP, LTMP)
      INTEGER  I0, J0, I1, J1
      LOGICAL CONT
      INTEGER LTMP
      CHARACTER CTMP*(*)
C
C Arguments:
C   I0, J0 (input) : device coordinates of the starting point.
C   I1, J1 (input) : device coordinates of the end point.
C   CTMP  (output) : buffer for instruction.
C   LTMP  (output) : Number of valid characters in CTMP.
C
C Partially optimized.
C-----------------------------------------------------------------------
      INTEGER    GS
      PARAMETER (GS = 29)
      INTEGER   LXMASK, HXMASK, LYMASK, HYMASK
      PARAMETER (LXMASK = 64)
      PARAMETER (HXMASK = 32)
      PARAMETER (LYMASK = 96)
      PARAMETER (HYMASK = 32)
      INTEGER   HIY, LOY, HIX, LOX
C
      IF (CONT) THEN
          CTMP(1:1) = CHAR( (J1/32)    + HYMASK )
          CTMP(2:2) = CHAR( MOD(J1,32) + LYMASK )
          CTMP(3:3) = CHAR( (I1/32)    + HXMASK )
          CTMP(4:4) = CHAR( MOD(I1,32) + LXMASK )
          LTMP = 4
      ELSE
          CTMP(1:1) = CHAR( GS )
          CTMP(2:2) = CHAR( (J0/32)    + HYMASK )
          CTMP(3:3) = CHAR( MOD(J0,32) + LYMASK )
          CTMP(4:4) = CHAR( (I0/32)    + HXMASK )
          CTMP(5:5) = CHAR( MOD(I0,32) + LXMASK )
          LTMP = 5
          LTMP = LTMP+1
          CTMP(LTMP:LTMP) = CHAR( 0 )
          HIY = (J1/32)    + HYMASK
          LOY = MOD(J1,32) + LYMASK
          HIX = (I1/32)    + HXMASK
          LOX = MOD(I1,32) + LXMASK
          IF (CHAR(HIY).NE.CTMP(2:2)) THEN
              LTMP = LTMP +1
              CTMP(LTMP:LTMP) = CHAR( HIY )
          END IF
          IF (CHAR(LOY).NE.CTMP(3:3) .OR. CHAR(HIX).NE.CTMP(4:4)) THEN
              LTMP = LTMP +1
              CTMP(LTMP:LTMP) = CHAR( LOY )
          END IF
          IF (CHAR(HIX).NE.CTMP(4:4)) THEN
              LTMP = LTMP +1
              CTMP(LTMP:LTMP) = CHAR( HIX )
          END IF
          LTMP = LTMP +1
          CTMP(LTMP:LTMP) = CHAR( LOX )
      END IF
C-----------------------------------------------------------------------
      END
