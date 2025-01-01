      PROGRAM PGEX17
C-----------------------------------------------------------------------
C Test program for PGPLOT. 
C Author: T. J. Pearson.
C Last modified: 9-Feb-1988
C-----------------------------------------------------------------------
      CHARACTER*128 DEVICE
      CHARACTER*80 GTYPE, GVER
      INTEGER I, J, L, L1, L2, PGBEG
      REAL X, X1, X2, Y, Y1, Y2, R, XI, XP, YP
      REAL PX(43), PY(43)
      DATA PX / 0.0,2.0,4.0,6.0,8.0,10.0,12.0,14.0,16.4,17.0,17.3,
     1          17.8, 18.5, 20.0, 22.0, 24.0, 26.0, 28.0, 29.0,
     2          28.8,27.2,25.0,23.0,21.5,21.1,21.5,22.8, 24.1, 25.1,
     3          25.2, 24.2, 22.1, 20.0, 18.0, 16.0, 14.0, 12.0,
     4          10.0,  8.0,  6.1,  4.2,  3.0,  1.3 /
      DATA PY / 8.8, 7.6, 7.1, 7.4, 8.0, 8.9, 9.6, 9.9, 9.4,
     1          9.7, 12.0, 14.0, 16.1, 17.0, 17.0, 16.0, 13.9,
     2          13.1, 13.2, 12.3, 11.5, 11.5, 11.5, 11.2, 10.5,
     3          9.0, 8.0, 7.0, 5.1, 3.6, 1.9, 1.1, 0.9, 0.7,
     4          0.8, 1.0, 1.0, 1.2, 1.8, 2.1, 2.9, 4.1, 6.0 /
C
C Open device for graphics
C
Cvms  CALL LIB$INIT_TIMER
      IF (PGBEG(0,'?',1,1).NE.1) STOP
      CALL PGQINF('DEV/TYPE', DEVICE, L)
      CALL PGQINF('VERSION', GVER, L1)
      CALL PGQINF('TYPE', GTYPE, L2)
      CALL PGBBUF
C
C Clear the screen; set background color.
C
      CALL PGPAGE
      CALL PGSCR(0,0.0,0.0,0.35)
C
C Draw a frame at the physical extremities of the plot.
C Dimensions are X by Y (inches).
C
      CALL PGSVP(0.0, 1.0, 0.0, 1.0)
      CALL PGQVP(1, X1, X2, Y1, Y2)
      X = X2-X1
      Y = Y2-Y1
      CALL PGSWIN(0.0, X, 0.0, Y)
      CALL PGSFS(2)
      CALL PGRECT(0.0, X, 0.0, Y)
      CALL PGMOVE(0.5*X, 0.0)
      CALL PGDRAW(0.5*X, Y)
      CALL PGMOVE(0.0, 0.5*Y)
      CALL PGDRAW(X, 0.5*Y)
C
C Draw a circle of diameter 0.5 x min(x,y)
C
      R = 0.25*MIN(X,Y)
      CALL PGCIRC(X*0.5, Y*0.5, R)
C
C Draw some more circles with different line-styles; this tests
C the dashing algorithm on curved lines.
C
      CALL PGSLS(2)
      CALL PGCIRC(X*0.5, Y*0.5, R*1.1)
      CALL PGSLS(3)
      CALL PGCIRC(X*0.5, Y*0.5, R*1.2)
      CALL PGSLS(2)
      CALL PGSLW(3)
      CALL PGCIRC(X*0.5, Y*0.5, R*1.3)
      CALL PGSLS(1)
      CALL PGSLW(1)      
C
C Demonstrate different line-styles
C
      DO 10 I=1,5
          CALL PGSLS(I)
          CALL PGMOVE(I*(X/20.0),0.0)
          CALL PGDRAW(I*(X/20.0),Y)
   10 CONTINUE
      CALL PGSLS(1)
C
C Demonstrate different line-widths
C
      DO 20 I=1,5
          CALL PGSLW(I)
          CALL PGMOVE(0.0, I*(Y/20.0))
          CALL PGDRAW(X, I*(Y/20.0))
   20 CONTINUE
      CALL PGSLW(1)
C
C Draw dots in different thicknesses.
C
      DO 30 I=1,21
          XP = 6*Y/20.0
          YP = I*Y/22.0
          CALL PGSLW(I)
          CALL PGPT(1,XP,YP,-1)
   30 CONTINUE
C
C Demonstrate different line-colors
C
      CALL PGSLW(4)
      DO 40 I=0,15
          CALL PGSCI(I)
          XI = (I+20)*(X/40.0)
          CALL PGMOVE(XI,0.0)
          CALL PGDRAW(XI,Y)
   40 CONTINUE
      CALL PGSCI(1)
      CALL PGSLW(1)
C
C Demonstrate fill area
C
      DO 50 J=1,43
         PX(J) = (PX(J)+50.0)/100.0*X
         PY(J) = (PY(J)+75.0)/100.0*Y
   50 CONTINUE
      DO 70 I=0,3
          CALL PGSCI(I)
          CALL PGSFS(1)
          CALL PGPOLY(43,PX,PY)
          CALL PGSCI(1)
          CALL PGSFS(2)
          CALL PGPOLY(43,PX,PY)
          DO 60 J=1,43
             PY(J) = PY(J)-0.25*Y
   60     CONTINUE
   70 CONTINUE
C
C Write the device type on the plot.
C
      CALL PGSWIN(0.0, 1.0, 0.0, 1.0)
      CALL PGSFS(1)
      CALL PGSCI(0)
      CALL PGRECT(0.31, 1.0-0.31, 0.85, 0.97)
      CALL PGSCI(1)
      CALL PGSFS(2)
      CALL PGRECT(0.31, 1.0-0.31, 0.85, 0.97)
      CALL PGPTXT(0.5, 0.91, 0.0, 0.5, 'PGPLOT '//GVER(1:L1))
      CALL PGPTXT(0.5, 0.87, 0.0, 0.5, 'Device '//GTYPE(1:L2))
C
C Close the device.
C
      CALL PGEBUF
      CALL PGEND
C
C Report dimensions.
C
      WRITE (6,100) DEVICE(1:L), X, Y, R
  100 FORMAT (' Device: ',A/
     1        ' Plot dimensions (x,y; inches): ',F9.2,', ',F9.2/
     2        ' Radius of circle (inches):     ',F9.2)
Cvms  CALL LIB$SHOW_TIMER
C
C and exit.
C
      END

      SUBROUTINE PGCIRC(X0, Y0, R)
      REAL X0, Y0, R
C
C Draw a circle of radius R, center (X0, Y0) [world coordinates].
C
      REAL A, XI, YI

      CALL PGMOVE(X0+R,Y0)
      DO 10 A=0.,360.,1.0
         XI = X0 + R*COS(A/57.29577951)
         YI = Y0 + R*SIN(A/57.29577951)
         CALL PGDRAW(XI,YI)
   10 CONTINUE
      END
