
C*GRTRN0 -- define scaling transformation
C+
      SUBROUTINE GRTRN0 (XORG,YORG,XSCALE,YSCALE)
C
C GRPCKG (internal routine): Define scaling transformation for current
C device (equivalent to GRTRAN without device selection).
C
C Arguments:
C
C XORG, YORG, XSCALE, YSCALE (input, real): parameters of the scaling
C       transformation. This is defined by:
C               XABS = XORG + XWORLD * XSCALE,
C               YABS = YORG + YWORLD * YSCALE,
C       where (XABS, YABS) are the absolute device coordinates
C       corresponding to world coordinates (XWORLD, YWORLD).
C--
C (1-Feb-1983)
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      REAL     XORG, YORG, XSCALE, YSCALE
C
      GRXORG(GRCIDE) = XORG
      GRXSCL(GRCIDE) = XSCALE
      GRYORG(GRCIDE) = YORG
      GRYSCL(GRCIDE) = YSCALE
C
      END
