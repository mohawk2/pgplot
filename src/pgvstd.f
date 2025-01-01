C*PGVSTD -- set standard (default) viewport
C+
      SUBROUTINE PGVSTD
C
C Define the viewport to be the standard viewport.  The standard
C viewport is the full area of the view surface (or subpage),
C less a margin of 4 character heights all round for labelling.
C It thus depends on the current character size, set by PGSCH.
C
C Arguments: none.
C--
C (22-Apr-1983)
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      REAL     XLEFT, XRIGHT, YBOT, YTOP
C
      IF (PGOPEN.EQ.0) RETURN
C
      XLEFT  = 4.0*YSP/XPERIN
      XRIGHT = XLEFT + (XSZ-8.0*YSP)/XPERIN
      YBOT   = 4.0*YSP/YPERIN
      YTOP   = YBOT + (YSZ-8.0*YSP)/YPERIN
      CALL PGVSIZ(XLEFT, XRIGHT, YBOT, YTOP)
      END
