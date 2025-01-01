C*PGVSIZ -- set viewport (inches)
C+
      SUBROUTINE PGVSIZ (XLEFT, XRIGHT, YBOT, YTOP)
      REAL XLEFT, XRIGHT, YBOT, YTOP
C
C Change the size and position of the viewport, specifying
C the viewport in physical device coordinates (inches).  The
C viewport is the rectangle on the view surface "through"
C which one views the graph.  All the PG routines which plot lines
C etc. plot them within the viewport, and lines are truncated at
C the edge of the viewport (except for axes, labels etc drawn with
C PGBOX or PGLAB).  The region of world space (the coordinate
C space of the graph) which is visible through the viewport is
C specified by a call to PGSWIN.  It is legal to request a
C viewport larger than the view surface; only the part which
C appears on the view surface will be plotted.
C
C Arguments:
C  XLEFT  (input)  : x-coordinate of left hand edge of viewport, in
C                    inches from left edge of view surface.
C  XRIGHT (input)  : x-coordinate of right hand edge of viewport, in
C                    inches from left edge of view surface.
C  YBOT   (input)  : y-coordinate of bottom edge of viewport, in
C                    inches from bottom of view surface.
C  YTOP   (input)  : y-coordinate of top  edge of viewport, in inches
C                    from bottom of view surface.
C--
C 13-Dec-1990  Make errors non-fatal [TJP].
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
C
      IF (PGOPEN.EQ.0)  RETURN
      IF (XLEFT.GE.XRIGHT .OR. YBOT.GE.YTOP) THEN
          CALL GRWARN('PGVSIZ ignored: invalid arguments')
          RETURN
      END IF
C
      XLEN = (XRIGHT-XLEFT)*XPERIN
      YLEN = (YTOP-YBOT)*YPERIN
      XVP  = XLEFT*XPERIN
      YVP  = YBOT*YPERIN
      XOFF = XVP + (NXC-1)*XSZ
      YOFF = YVP + (NY-NYC)*YSZ
      CALL PGVW
      END
