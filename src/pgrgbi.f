C*PGRGBI -- true-color image from a 3D data array
C%void cpgrgbi(const float *a, int idim, int jdim, int i1, int i2, \
C% int j1, int j2, float a1, float a2, const float *tr);
C+
      SUBROUTINE PGRGBI (A, IDIM, JDIM, I1, I2, J1, J2,
     1                   A1, A2, TR)
      INTEGER IDIM, JDIM, I1, I2, J1, J2
      REAL    A(IDIM,JDIM,3), A1, A2, TR(6)
C
C Draw a true-color image of an array in current window. The subsection
C of the array A defined by indices (I1:I2, J1:J2) is mapped onto
C the view surface world-coordinate system by the transformation
C matrix TR. The resulting quadrilateral region is clipped at the edge
C of the window. Each element of the array is represented in the image
C by a small quadrilateral, which is filled with a color specified by
C the corresponding array value.
C
C The subroutine works only on devices that use true-color rendering
C mapped through PGPLOT's color map interface:  it works by changing the
C color table to match particular RGB values for each pixel as it is
C plotted.  This is does not work with devices that do post-facto color
C changing; 8-bit PseudoColor X windows are one example.  At the end of
C the plot operation the color table is restored.  The X Windows device
C is currently rather slow due to the color selection overhead (an RGB
C color is selected for each pixel).  This will be improved in a future
C release of the X Windows driver.
C
C Array values in the range A1 to A2 are mapped on to the interval
C 0 to 1 and piped directly into RGB values on the output device.
C
C On devices which have no available color indices (C1 > C2),
C PGRGBI will return without doing anything.  The more color indices
C are available, the quicker PGRGBI runs; it clusters a whole color-table's
C worth of pixels into each plot call.
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
C quadrilateral region that is shaded by PGIMAG are given by
C applying this transformation to (I1-0.5,J1-0.5), (I2+0.5, J2+0.5).
C
C Arguments:
C  A      (input)  : the array to be plotted. (NX, NY, 3)
C  IDIM   (input)  : the first  (X) dimension of array A.
C  JDIM   (input)  : the second (Y) dimension of array A.
C  I1, I2 (input)  : the inclusive range of the X index
C                    (I) to be plotted.
C  J1, J2 (input)  : the inclusive range of the Y index
C                    (J) to be plotted.
C  A1     (input)  : the array value for a dark (R,G,B) component
C  A2     (input)  : the array value for a bright (R,G,B) component
C  TR     (input)  : transformation matrix between array grid and
C                    world coordinates.
C--
C 17-Jan-2003 New routine ported from pgimag.f [Craig DeForest]
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      REAL PA(6)
      LOGICAL PGNOTO
C
C Check inputs.
C
      IF (PGNOTO('PGRGBI')) RETURN
      IF (I1.LT.1 .OR. I2.GT.IDIM .OR. I1.GT.I2 .OR.
     1    J1.LT.1 .OR. J2.GT.JDIM .OR. J1.GT.J2) THEN
          CALL GRWARN('PGRGBI: invalid range I1:I2, J1:J2')
      ELSE IF (A1.EQ.A2) THEN
          CALL GRWARN('PGRGBI: foreground level = background level')
      ELSE IF (PGMNCI(PGID).GT.PGMXCI(PGID)) THEN
          CALL GRWARN('PGIMAG: not enough colors available')
      ELSE
C
C Call lower-level routine to do the work.
C
          CALL PGBBUF
          PA(1) = TR(1)*PGXSCL(PGID) + PGXORG(PGID)
          PA(2) = TR(2)*PGXSCL(PGID)
          PA(3) = TR(3)*PGXSCL(PGID)
          PA(4) = TR(4)*PGYSCL(PGID) + PGYORG(PGID)
          PA(5) = TR(5)*PGYSCL(PGID)
          PA(6) = TR(6)*PGYSCL(PGID)
          CALL GRRGB0(A, IDIM, JDIM, I1, I2, J1, J2, A1, A2, PA,
     :                PGMNCI(PGID), PGMXCI(PGID))
          CALL PGEBUF
      END IF
C-----------------------------------------------------------------------
      END
