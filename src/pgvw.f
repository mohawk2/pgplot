C
      SUBROUTINE PGVW
C
C PGPLOT (internal routine): set the GRPCKG scaling transformation
C and window appropriate for the current window and viewport. This
C routine is called whenever the viewport or window is changed.
C
C Arguments: none
C
C (11-Feb-1983)
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
C
C Scale plotter in world coordinates.
C
      XSCALE = XLEN/ABS(XTRC-XBLC)
      YSCALE = YLEN/ABS(YTRC-YBLC)
      IF (XBLC.GT.XTRC) THEN
          XSCALE = -XSCALE
      END IF
      IF (YBLC.GT.YTRC) THEN
          YSCALE = -YSCALE
      END IF
      XORG = XOFF-XBLC*XSCALE
      YORG = YOFF-YBLC*YSCALE
      CALL GRTRN0(XORG,YORG,XSCALE,YSCALE)
C
C Window plotter in viewport.
C
      CALL GRAREA(IDENT,XOFF,YOFF,XLEN,YLEN)
      END
