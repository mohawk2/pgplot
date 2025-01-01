C*GRTE01 -- PGPLOT Tektronix-4010 driver, line segment
C+
      SUBROUTINE GRTE01 (I0, J0, I1, J1, CONT, TKBUF, NW)
      INTEGER  I0, J0, I1, J1
      LOGICAL CONT
      INTEGER NW
      INTEGER TKBUF(*)
C
C Arguments:
C   I0, J0 (input) : device coordinates of the starting point.
C   I1, J1 (input) : device coordinates of the end point.
C   TKBUF (output) : buffer for instruction.
C
C Partially optimized.
C-----------------------------------------------------------------------
      INTEGER GS
      PARAMETER (GS = 29)
      INTEGER LXMASK, HXMASK, LYMASK, HYMASK
      PARAMETER (LXMASK = 64)
      PARAMETER (HXMASK = 32)
      PARAMETER (LYMASK = 96)
      PARAMETER (HYMASK = 32)
      INTEGER HIY, LOY, HIX, LOX
C
      IF (CONT) THEN
          TKBUF(1) = (J1/32)    + HYMASK
          TKBUF(2) = MOD(J1,32) + LYMASK
          TKBUF(3) = (I1/32)    + HXMASK
          TKBUF(4) = MOD(I1,32) + LXMASK
          NW = 4
      ELSE
          TKBUF(1) = GS
          TKBUF(2) = (J0/32)    + HYMASK
          TKBUF(3) = MOD(J0,32) + LYMASK
          TKBUF(4) = (I0/32)    + HXMASK
          TKBUF(5) = MOD(I0,32) + LXMASK
          NW = 5
          NW = NW+1
          TKBUF(NW) = 0
          HIY = (J1/32)    + HYMASK
          LOY = MOD(J1,32) + LYMASK
          HIX = (I1/32)    + HXMASK
          LOX = MOD(I1,32) + LXMASK
          IF (HIY.NE.TKBUF(2)) THEN
              NW = NW +1
              TKBUF(NW) = HIY
          END IF
          IF (LOY.NE.TKBUF(3) .OR. HIX.NE.TKBUF(4)) THEN
              NW = NW +1
              TKBUF(NW) = LOY
          END IF
          IF (HIX.NE.TKBUF(4)) THEN
              NW = NW +1
              TKBUF(NW) = HIX
          END IF
          NW = NW +1
          TKBUF(NW) = LOX
      END IF
C     -- extra null byte required for real Tektronixes
      NW = NW+1
      TKBUF(NW) = 0
C-----------------------------------------------------------------------
      END
