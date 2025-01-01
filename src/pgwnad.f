C*PGWNAD -- set window and adjust viewport to same aspect ratio
C+
      SUBROUTINE PGWNAD (X1, X2, Y1, Y2)
      REAL X1, X2, Y1, Y2
C
C Change the window in world coordinate space that is to be mapped on
C to the viewport, and simultaneously adjust the viewport so that the
C world-coordinate scales are equal in x and y. The new viewport is
C the largest one that can fit within the previously set viewport
C while retaining the required aspect ratio.
C
C Arguments:
C  X1     (input)  : the x-coordinate of the bottom left corner
C                    of the viewport.
C  X2     (input)  : the x-coordinate of the top right corner
C                    of the viewport (note X2 may be less than X1).
C  Y1     (input)  : the y-coordinate of the bottom left corner
C                    of the viewport.
C  Y2     (input)  : the y-coordinate of the top right corner of the
C                    viewport (note Y2 may be less than Y1).
C--
C 25-Sep-1985 - new routine (TJP).
C 31-May-1989 - correct error: XVP and YVP not set (TJP).
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      REAL SCALE,OXLEN,OYLEN
C
      IF (PGOPEN.EQ.0) RETURN
C
      XSCALE = XLEN/ABS(X2-X1)
      YSCALE = YLEN/ABS(Y2-Y1)
      SCALE = MIN(XSCALE/XPERIN,YSCALE/YPERIN)
      XSCALE = SCALE*XPERIN
      YSCALE = SCALE*YPERIN
      OXLEN = XLEN
      OYLEN = YLEN
      XLEN = XSCALE*ABS(X2-X1)
      YLEN = YSCALE*ABS(Y2-Y1)
      XVP  = XVP + 0.5*(OXLEN-XLEN)
      YVP  = YVP + 0.5*(OYLEN-YLEN)
      XOFF = XVP + (NXC-1)*XSZ
      YOFF = YVP + (NY-NYC)*YSZ
      XBLC = X1
      XTRC = X2
      YBLC = Y1
      YTRC = Y2
      CALL PGVW
      END
