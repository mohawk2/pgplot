C*GRRGB00 -- RGB color image of a 3D data array
C The array dimensions are (color,X,Y); their sizes are (3, NX, NY).
C
C Based on GRIMG0
C+
      SUBROUTINE GRRGB0 (A, IDIM, JDIM, I1, I2, J1, J2,
     1                   A1, A2, PA, MININD, MAXIND)
      INTEGER IDIM, JDIM, I1, I2, J1, J2, MININD, MAXIND
      REAL    A(IDIM,JDIM,3), A1, A2, PA(6)
C
C This is a support routine for PGRGBIM.
C
C Arguments:
C  A      (input)  : the array to be plotted.
C  IDIM   (input)  : the X dimension of array A.
C  JDIM   (input)  : the Y dimension of array A.
C  I1, I2 (input)  : the inclusive range of the first index
C                    (I) to be plotted.
C  J1, J2 (input)  : the inclusive range of the second
C                    index (J) to be plotted.
C  A1     (input)  : the array value that corresponds to pixels fully OFF
C  A2     (input)  : the array value that corresponds to pixels fully ON
C  PA     (input)  : transformation matrix between array grid and
C                    device coordinates.
C  MININD (input)  : minimum color index to use.
C  MAXIND (input)  : maximum color index to use.
C
C Logarithmic and square-root scaling aren't allowed since maintaining
C the RGB ratio requires some care in those cases.
C--
C  17-Jan-2003 - new routine [Craig DeForest]
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      CHARACTER C
C-----------------------------------------------------------------------
C
C Switch on type of device support.
C
      C = GRGCAP(GRCIDE)(7:7)
      IF (C.EQ.'Q') THEN
C         -- Image-primitive devices
          CALL GRRGB1(A, IDIM, JDIM, I1, I2, J1, J2,
     :                A1, A2, PA, MININD, MAXIND)
      ELSE IF (C.EQ.'P') THEN
C         -- Pixel-primitive devices
          CALL GRRGB2(A, IDIM, JDIM, I1, I2, J1, J2,
     :                A1, A2, PA, MININD, MAXIND)
      ELSE IF (C.EQ.'N') THEN
C         -- Other devices
          CALL GRWARN(
     :     'images cannot be displayed on the selected device')
      ELSE
C         -- Unknown device code
          CALL GRWARN('unexpected error in routine GRIMG0')
      END IF
C-----------------------------------------------------------------------
      END
