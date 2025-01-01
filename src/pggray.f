C*PGGRAY -- gray-scale map of a 2D data array
C+
      SUBROUTINE PGGRAY (A, IDIM, JDIM, I1, I2, J1, J2,
     1                   FG, BG, TR)
      INTEGER IDIM, JDIM, I1, I2, J1, J2
      REAL    A(IDIM,JDIM)
      REAL    FG, BG
      REAL TR(6)
C
C Draw gray-scale map of an array in current window. The subsection
C of the array A defined by indices (I1:I2, J1:J2) is mapped onto
C the view surface world-coordinate system by the transformation
C matrix TR. The resulting quadrilateral region is clipped at the edge
C of the window and shaded with the shade at each point determined
C by the corresponding array value.  The shade is a number in the
C range 0 to 1 obtained by linear interpolation between the background
C level (BG) and the foreground level (FG), i.e.,
C
C   shade = [A(i,j) - BG] / [FG - BG]
C
C The background level BG can be either less than or greater than the
C foreground level FG.  Points in the array that are outside the range
C BG to FG are assigned shade 0 or 1 as appropriate.
C
C The algorithm used by PGGRAY is device-dependent.  On devices
C that have only two color indices (0 and 1), the background color
C is the color assigned to color index 0, the foreground color
C is the color assigned to color index 1, and PGGRAY uses a
C "dithering" algorithm to fill in pixels in the two colors, with
C the shade (computed as above) determining the faction of pixels
C that are assigned color index 1.
C
C On devices that have more than 16 color indices, PGGRAY may use
C color indices outside the range 0-15 to provide more than two
C gray shades.  Note that PGGRAY may change the color representation
C of these color indices, but it will not change the representation
C of indices 0-15.
C
C On most devices, the shaded region is "opaque", i.e., it obscures
C all graphical elements previously drawn in the region. But on
C devices that do not have erase capability, the background shade
C is "transparent" and allows previously-drawn graphics to show
C through.
C
C The transformation matrix TR is used to calculate the world
C coordinates of the center of the "cell" that represents each
C array element. The world coordinates of the center of the cell
C corresponding to array element A(I,J) are given by:
C
C          X = TR(1) + TR(2)*I + TR(3)*J
C          Y = TR(4) + TR(5)*I + TR(6)*J
C
C Usually TR(3) and TR(5) are zero -- unless the coordinate
C transformation involves a rotation or shear.  The corners of the
C quadrilateral region that is shaded by PGGRAY are given by
C applying this transformation to (I1-0.5,J1-0.5), (I2+0.5, J2+0.5).
C
C Arguments:
C  A      (input)  : the array to be plotted.
C  IDIM   (input)  : the first dimension of array A.
C  JDIM   (input)  : the second dimension of array A.
C  I1, I2 (input)  : the inclusive range of the first index
C                    (I) to be plotted.
C  J1, J2 (input)  : the inclusive range of the second
C                    index (J) to be plotted.
C  FG     (input)  : the array value which is to appear with shade
C                    1 ("foreground").
C  BG     (input)  : the array value which is to appear with shade
C                    0 ("background").
C  TR     (input)  : transformation matrix between array grid and
C                    world coordinates.
C--
C  2-Sep-1987: remove device-dependent code to routine GRGRAY (TJP).
C  7-Jun-1988: change documentation and argument names (TJP).
C 31-May-1989: allow 1-pixel wide arrays to be plotted (TJP).
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
C
C Check inputs.
C
      IF (I1.LT.1 .OR. I2.GT.IDIM .OR. I1.GT.I2 .OR.
     1    J1.LT.1 .OR. J2.GT.JDIM .OR. J1.GT.J2) THEN
          CALL GRWARN('PGGRAY: invalid range I1:I2, J1:J2')
      ELSE IF (FG.EQ.BG) THEN
          CALL GRWARN('PGGRAY: foreground level = background level')
      ELSE IF (PGOPEN.NE.0) THEN
C
C Call lower-level routine to do the work.
C
          CALL PGBBUF
          CALL GRGRAY(A, IDIM, JDIM, I1, I2, J1, J2, FG, BG, TR)
          CALL PGEBUF
      END IF
C-----------------------------------------------------------------------
      END
