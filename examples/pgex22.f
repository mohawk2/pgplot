      PROGRAM PGEX22
C-----------------------------------------------------------------------
C Test program for PGPLOT: test of imaging routine PGGRAY (with 
C PGCONT). This program serves to both demonstrate and test PGGRAY.
C It computes an (arbitrary) function on a 2D array, and uses both
C PGGRAY and PGCONT to display it. An irregular transformation (TR 
C matrix) is used, to test (a) that the routine works when the array
C pixels are not aligned with the device pixels, and (b) that the
C image is clipped correctly at the edge of the viewport. The program
C also draws the bounding quadrilateral of the contour map. The contours
C should end on this quadrilateral, but note that the grayscale image 
C extends one half pixel beyond this quadrilateral (if the subarray to 
C be displayed contains N pixels in one dimension, the width of the
C image is N units, while the width of the contour map is N-1 pixels).
C-----------------------------------------------------------------------
      INTEGER I,J
      REAL F(40,40),FMIN,FMAX,ALEV,TR(6),BLACK,WHITE
C
C Open device for graphics.
C
      CALL PGBEG(0,'?',2,1)
C
C Compute a suitable function.
C
      FMIN = F(1,1)
      FMAX = F(1,1)
      DO 20 I=1,40
          DO 10 J=1,40
              F(I,J) = COS(0.6*SQRT(I*2.)-0.4*J/3.)*COS(0.4*I/3)+
     1                     (I-J)/40.0
              FMIN = MIN(F(I,J),FMIN)
              FMAX = MAX(F(I,J),FMAX)
   10     CONTINUE
   20 CONTINUE
C
C (1) Test PGGRAY
C Clear the screen. Set up window and viewport.
C
      CALL PGPAGE
      CALL PGSVP(0.05,0.95,0.05,0.95)
      CALL PGWNAD(-38.0,40.0,-38.0,40.0)
      CALL PGSCI(5)
      CALL PGMTXT('t',1.0,0.0,0.0,'Test of PGGRAY')
      CALL PGSCI(1)
C
C Draw the map.  
C
      BLACK = FMIN
      WHITE = FMAX
      TR(1) = 0.
      TR(2) = -1.414
      TR(3) = 1.414
      TR(4) = 56.56
      TR(5) = -1.414
      TR(6) = -1.414
      CALL PGGRAY(F,40,40,1,40,1,40,BLACK,WHITE,TR)
      CALL PGSCI(2)
      CALL OUTLIN(1,40,1,40,TR)
      CALL PGSCI(5)
      CALL PGBOX('bcnts',0.0,0,'bcnts',0.0,0)
C
C (2) Test PGCONT
C Clear the screen. Set up window and viewport.
C
      CALL PGPAGE
      CALL PGSVP(0.05,0.95,0.05,0.95)
      CALL PGWNAD(-38.0,40.0,-38.0,40.0)
      CALL PGSCI(5)
      CALL PGMTXT('t',1.0,0.0,0.0,'Test of PGCONT')
      CALL PGSCI(1)
C
C Draw the map.  
C
      DO 30 I=1,21
          ALEV = FMIN + (I-1)*(FMAX-FMIN)/20.0
          IF (MOD(I,5).EQ.0) THEN
              CALL PGSLW(3)
          ELSE
              CALL PGSLW(1)
          END IF
          IF (I.LT.10) THEN
              CALL PGSLS(2)
          ELSE
              CALL PGSLS(1)
          END IF
          CALL PGCONT(F,40,40,1,40,1,40,ALEV,-1,TR)
   30 CONTINUE
      CALL PGSLS(1)
      CALL PGSLW(1)
      CALL PGSCI(2)
      CALL OUTLIN(1,40,1,40,TR)
      CALL PGSCI(5)
      CALL PGBOX('bcnts',0.0,0,'bcnts',0.0,0)
C
C Close the device and exit.
C
      CALL PGEND
      END

      SUBROUTINE OUTLIN(I1,I2,J1,J2,TR)
      INTEGER I1,I2,J1,J2
      REAL TR(6)
C
C Draw the enclosing rectangle of the subarray to be contoured,
C applying the transformation TR.
C
C For a contour map, the corners are (I1,J1) and (I2,J2); for
C a gray-scale map, they are (I1-0.5,J1-0.5), (I2+0.5, J2+0.5).
C
      INTEGER K
      REAL XW(5), YW(5), T
C
      XW(1) = I1
      YW(1) = J1
      XW(2) = I1
      YW(2) = J2
      XW(3) = I2
      YW(3) = J2
      XW(4) = I2
      YW(4) = J1
      XW(5) = I1
      YW(5) = J1
      DO 10 K=1,5
          T = XW(K)
          XW(K) = TR(1) + TR(2)*T + TR(3)*YW(K)
          YW(K) = TR(4) + TR(5)*T + TR(6)*YW(K)
   10 CONTINUE
      CALL PGLINE(5,XW,YW)
      END
