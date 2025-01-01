C*PGSHLS -- set color representation using HLS system
C+
      SUBROUTINE PGSHLS (CI, CH, CL, CS)
      INTEGER CI
      REAL    CH, CL, CS
C
C Set color representation: i.e., define the color to be
C associated with a color index.  This routine is equivalent to
C PGSCR, but the color is defined in the Hue-Lightness-Saturation
C model instead of the Red-Green-Blue model.
C
C Reference: SIGGRAPH Status Report of the Graphic Standards Planning
C Committee, Computer Graphics, Vol.13, No.3, Association for
C Computing Machinery, New York, NY, 1979.
C
C Argument:
C  CI     (input)  : the color index to be defined, in the range 0-max.
C                    If the color index greater than the device
C                    maximum is specified, the call is ignored. Color
C                    index 0 applies to the background color.
C  CH     (input)  : hue, in range 0.0 to 360.0.
C  CL     (input)  : lightness, in range 0.0 to 1.0.
C  CS     (input)  : saturation, in range 0.0 to 1.0.
C--
C 9-May-1988 - new routine [TJP].
C-----------------------------------------------------------------------
      REAL CR, CG, CB
      CALL GRXRGB (CH,CL,CS,CR,CG,CB)
      CALL GRSCR(CI,CR,CG,CB)
      END
