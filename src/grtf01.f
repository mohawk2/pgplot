!
C*GRTF01 -- PGPLOT Tektronix file driver, line segment
C+
      SUBROUTINE GRTF01 (I0, J0, I1, J1, TKBUF, NW)
      INTEGER  I0, J0, I1, J1
      INTEGER NW
      BYTE TKBUF(*)
C
C Arguments:
C   I0, J0 (input) : device coordinates of the starting point.
C   I1, J1 (input) : device coordinates of the end point.
C   TKBUF (output) : buffer for instruction.
C
C Not optimized.
C-----------------------------------------------------------------------
      BYTE GS
      PARAMETER (GS = '1D'X)
      BYTE LXMASK, HXMASK, LYMASK, HYMASK, EXMASK
      PARAMETER (LXMASK = '40'X)
      PARAMETER (HXMASK = '20'X)
      PARAMETER (LYMASK = '60'X)
      PARAMETER (EXMASK = '60'X)
      PARAMETER (HYMASK = '20'X)
      INTEGER X, Y
C
      X = I0/4
      Y = J0/4
      TKBUF(1) = GS
      TKBUF(2) = (Y/32)    + HYMASK
      TKBUF(3) = (MOD(J0,4)*4) + MOD(I0,4) + EXMASK
      TKBUF(4) = MOD(Y,32) + LYMASK
      TKBUF(5) = (X/32)    + HXMASK
      TKBUF(6) = MOD(X,32) + LXMASK
      X = I1/4
      Y = J1/4
      TKBUF(7) = (Y/32)    + HYMASK
      TKBUF(8) = (MOD(J1,4)*4) + MOD(I1,4) + EXMASK
      TKBUF(9) = MOD(Y,32) + LYMASK
      TKBUF(10) = (X/32)    + HXMASK
      TKBUF(11) = MOD(X,32) + LXMASK
      NW = 11
C-----------------------------------------------------------------------
      END
