      PROGRAM PGDEMO
C-----------------------------------------------------------------------
C Demonstration program for PGPLOT. The main program opens the output
C device and calls a series of subroutines, one for each sample plot.
C-----------------------------------------------------------------------
      INTEGER PGBEG
C
C Call PGBEG to initiate PGPLOT and open the output device; PGBEG
C will prompt the user to supply the device name and type. Always
C check the return code from PGBEG.
C
      IF (PGBEG(0,'?',1,1) .NE. 1) STOP
C
C Call the demonstration subroutines.
C
      CALL PGEX1
      CALL PGEX2
      CALL PGEX3
      CALL PGEX4
      CALL PGEX5
      CALL PGEX6
      CALL PGEX7
      CALL PGEX8
      CALL PGEX9
      CALL PGEX10
      CALL PGEX11
C
C Finally, call PGEND to terminate things properly.
C
      CALL PGEND
C-----------------------------------------------------------------------
      END

      SUBROUTINE PGEX1
C-----------------------------------------------------------------------
C This example illustrates the use of PGENV, PGLAB, PGPT, PGLINE.
C-----------------------------------------------------------------------
      INTEGER I
      REAL XS(5),YS(5), XR(100), YR(100)
      DATA XS/1.,2.,3.,4.,5./
      DATA YS/1.,4.,9.,16.,25./
C
C Call PGENV to specify the range of the axes and to draw a box, and
C PGLAB to label it. The x-axis runs from 0 to 10, and y from 0 to 20.
C
      CALL PGENV(0.,10.,0.,20.,0,1)
      CALL PGLAB('(x)', '(y)', 'PGPLOT Example 1 - y = x\u2')
C
C Mark five points (coordinates in arrays XS and YS), using symbol
C number 9.
C
      CALL PGPT(5,XS,YS,9)
C
C Compute the function at 60 points, and use PGLINE to draw it.
C
      DO 10 I=1,60
          XR(I) = 0.1*I
          YR(I) = XR(I)**2
   10 CONTINUE
      CALL PGLINE(60,XR,YR)
C-----------------------------------------------------------------------
      END

      SUBROUTINE PGEX2
C-----------------------------------------------------------------------
C Repeat the process for another graph. This one is a graph of the
C sinc (sin x over x) function.
C-----------------------------------------------------------------------
      INTEGER I
      REAL XR(100), YR(100)
C
      CALL PGENV(-2.,10.,-0.4,1.2,0,1)
      CALL PGLAB('(x)', 'sin(x)/x', 
     $             'PGPLOT Example 2 - Sinc Function')
      DO 20 I=1,100
          XR(I) = (I-20)/6.
          YR(I) = 1.0
          IF (XR(I).NE.0.0) YR(I) = SIN(XR(I))/XR(I)
   20 CONTINUE
      CALL PGLINE(100,XR,YR)
C-----------------------------------------------------------------------
      END

      SUBROUTINE PGEX3
C----------------------------------------------------------------------
C This example illustrates the use of PGBOX and attribute routines to
C mix colors and line-styles.
C----------------------------------------------------------------------
      REAL PI
      PARAMETER (PI=3.14159265)
      INTEGER I
      REAL XR(360), YR(360)
      REAL ARG
C
C Call PGENV to initialize the viewport and window; the
C AXIS argument is -2, so no frame or labels will be drawn.
C
      CALL PGENV(0.,720.,-2.0,2.0,0,-2)
C
C Set the color index for the axes and grid (index 5 = cyan).
C Call PGBOX to draw first a grid at low brightness, and then a
C frame and axes at full brightness. Note that as the x-axis is
C to represent an angle in degrees, we request an explicit tick 
C interval of 90 deg with subdivisions at 30 deg, as multiples of
C 3 are a more natural division than the default.
C
      CALL PGSCI(14)
      CALL PGBOX('G',30.0,0,'G',0.2,0)
      CALL PGSCI(5)
      CALL PGBOX('ABCTSN',90.0,3,'ABCTSNV',0.0,0)
C
C Call PGLAB to label the graph in a different color (3=green).
C
      CALL PGSCI(3)
      CALL PGLAB('x (degrees)','f(x)','PGPLOT Example 3')
C
C Compute the function to be plotted: a trig function of an
C angle in degrees, computed every 2 degrees.
C
      DO 20 I=1,360
          XR(I) = 2.0*I
          ARG = XR(I)/180.0*PI
          YR(I) = SIN(ARG) + 0.5*COS(2.0*ARG) + 
     1                0.5*SIN(1.5*ARG+PI/3.0)
   20 CONTINUE
C
C Change the color (6=magenta), line-style (2=dashed), and line
C width and draw the function.
C
      CALL PGSCI(6)
      CALL PGSLS(2)
      CALL PGSLW(3)
      CALL PGLINE(360,XR,YR)
C
C Restore attributes to defaults.
C
      CALL PGSLS(1)
      CALL PGSCI(1)
      CALL PGSLW(1)
C-----------------------------------------------------------------------
      END

      SUBROUTINE PGEX4
C-----------------------------------------------------------------------
C Demonstration program for PGPLOT: draw a histogram.
C-----------------------------------------------------------------------
      INTEGER  I, ISEED
      REAL     DATA(1000), X(620), Y(620)
      REAL     RNGAUS, RAN5, JUNK
C
C Call RNGAUS to obtain 1000 samples from a normal distribution.
C
      ISEED = -5678921
      JUNK = RAN5(ISEED)
      DO 10 I=1,1000
          DATA(I) = RNGAUS(ISEED)
   10 CONTINUE
C
C Draw a histogram of these values.
C
      CALL PGHIST(1000,DATA,-3.1,3.1,31,0)
      CALL PGLAB('Variate', ' ',
     $             'PGPLOT Example 4 - Histogram (Gaussian)')
C
C Superimpose the theoretical distribution.
C
      DO 20 I=1,620
          X(I) = -3.1 + 0.01*(I-1)
          Y(I) = 0.2*1000./SQRT(2.*3.14159265)*EXP(-0.5*X(I)*X(I))
   20 CONTINUE
      CALL PGLINE(620,X,Y)
C-----------------------------------------------------------------------
      END

      SUBROUTINE PGEX5
C----------------------------------------------------------------------
C Demonstration program for the PGPLOT plotting package.  This example
C illustrates how to draw a log-log plot.
C PGPLOT subroutines demonstrated:
C    PGENV, PGERRY, PGLAB, PGLINE, PGPT, PGSCI.
C----------------------------------------------------------------------
      INTEGER   RED, GREEN, CYAN
      PARAMETER (RED=2)
      PARAMETER (GREEN=3)
      PARAMETER (CYAN=5)
      INTEGER   I
      REAL      X, YLO, YHI
      REAL      FREQ(15), FLUX(15), XP(100), YP(100), ERR(15)
      DATA FREQ / 26., 38., 80., 160., 178., 318.,
     1            365., 408., 750., 1400., 2695., 2700.,
     2            5000., 10695., 14900. /
      DATA FLUX / 38.0, 66.4, 89.0, 69.8, 55.9, 37.4,
     1            46.8, 42.4, 27.0, 15.8, 9.09, 9.17,
     2            5.35, 2.56, 1.73 /
      DATA ERR  / 6.0, 6.0, 13.0, 9.1, 2.9, 1.4,
     1            2.7, 3.0, 0.34, 0.8, 0.2, 0.46,
     2            0.15, 0.08, 0.01 /
C
C Call PGENV to initialize the viewport and window; the AXIS argument 
C is 30 so both axes will be logarithmic. The X-axis (frequency) runs 
C from 0.01 to 100 GHz, the Y-axis (flux density) runs from 0.3 to 300
C Jy. Note that it is necessary to specify the logarithms of these
C quantities in the call to PGENV. We request equal scales in x and y
C so that slopes will be correct.  Use PGLAB to label the graph.
C
      CALL PGSCI(CYAN)
      CALL PGENV(-2.0,2.0,-0.5,2.5,1,30)
      CALL PGLAB('Frequency, \gn (GHz)',
     1             'Flux Density, S\d\gn\u (Jy)',
     2             'PGPLOT Example 5 - Log-Log plot')
C
C Draw a fit to the spectrum (don't ask how this was chosen). This 
C curve is drawn before the data points, so that the data will write 
C over the curve, rather than vice versa.
C
      DO 10 I=1,100
          X = 1.3 + I*0.03
          XP(I) = X-3.0
          YP(I) = 5.18 - 1.15*X -7.72*EXP(-X)
   10 CONTINUE
      CALL PGSCI(RED)
      CALL PGLINE(100,XP,YP)
C
C Plot the measured flux densities: here the data are installed with a
C DATA statement; in a more general program, they might be read from a
C file. We first have to take logarithms (the -3.0 converts MHz to GHz).
C
      DO 20 I=1,15
          XP(I) = ALOG10(FREQ(I))-3.0
          YP(I) = ALOG10(FLUX(I))
   20 CONTINUE
      CALL PGSCI(GREEN)
      CALL PGPT(15, XP, YP, 17)
C
C Draw +/- 2 sigma error bars: take logs of both limits.
C
      DO 30 I=1,15
          YHI = ALOG10(FLUX(I)+2.*ERR(I))
          YLO = ALOG10(FLUX(I)-2.*ERR(I))
          CALL PGERRY(1,XP(I),YLO,YHI,1.0)
   30 CONTINUE
C-----------------------------------------------------------------------
      END

      SUBROUTINE PGEX6
C----------------------------------------------------------------------
C Demonstration program for the PGPLOT plotting package.  This example
C illustrates the use of PGPOLY using SOLID and HOLLOW fill-area
C attributes.
C----------------------------------------------------------------------
      REAL TWOPI
      PARAMETER (TWOPI=2.0*3.14159265)
      INTEGER I, J
      REAL X(10), Y(10)
C
C Call PGENV to initialize the viewport and window; the
C AXIS argument is -2, so no frame or labels will be drawn.
C
      CALL PGENV(0.,8.,0.,8.0,1,-2)
C
C Call PGLAB to label the graph.
C
      CALL PGSCI(3)
      CALL PGLAB(' ',' ','PGPLOT Example 6 - PGPOLY')
C
C Draw assorted regular convex polygons (solid).
C
      CALL PGSFS(1)
      DO 20 I=3,9
          CALL PGSCI(I-2)
          DO 10 J=1,I
            X(J) = I-2 + 0.5*COS(TWOPI*(J-1)/I)
            Y(J) = 6 + 0.5*SIN(TWOPI*(J-1)/I)
   10     CONTINUE
          CALL PGPOLY (I,X,Y)
   20 CONTINUE
C
C Draw assorted regular convex polygons (hollow).
C
      CALL PGSFS(2)
      DO 40 I=3,9
          CALL PGSCI(I-2)
          DO 30 J=1,I
            X(J) = I-2 + 0.5*COS(TWOPI*(J-1)/I)
            Y(J) = 3 + 0.5*SIN(TWOPI*(J-1)/I)
   30     CONTINUE
          CALL PGPOLY (I,X,Y)
   40 CONTINUE
      CALL PGSFS(1)
C-----------------------------------------------------------------------
      END

      SUBROUTINE PGEX7
C-----------------------------------------------------------------------
C Example program for PGPLOT. This program generates an Aitoff equal-
C area projection of the whole sky, centered on (lat=0, long=180).
C-----------------------------------------------------------------------
      REAL  RPDEG
      PARAMETER (RPDEG=3.1415926/180.0)
      INTEGER I, J
      REAL B, L, XC(361), YC(361)
C
      CALL PGBBUF
C
C Call PGENV to create a rectangular window of 4 x 2 units. This is 
C the bounding rectangle of the Aitoff plot. The JUST argument is 1
C to get equal scales in x and y.  Setting the character height to
C zero eliminates the margin that PGENV normally leaves for labels.
C
      CALL PGSCH(0.0)
      CALL PGENV(-2.0, 2.0, -1.0, 1.0, 1, -2)
C
C Draw 7 lines of constant longitude at longitude 0, 60, 120, ..., 
C 360 degrees. Each line is made up of 180 straight-line segments.
C
      DO 20 J=1,7
          L = (-180.+(J-1)*60.)*RPDEG
          DO 10 I=1,181
              B = (I-91)*RPDEG
              CALL AITOFF(B,L,XC(I),YC(I))
   10     CONTINUE
          CALL PGLINE(181,XC,YC)
   20 CONTINUE
C
C Draw 5 lines of constant latitude at latitudes -60, -30, 0, 30, 
C 60 degrees. Each line is made up of 360 straight-line segments.
C
      DO 40 J=1,5
          B = (-60.+(J-1)*30.)*RPDEG
          DO 30 I=1,361
              L = FLOAT(I-181)*RPDEG
              CALL AITOFF(B,L,XC(I),YC(I))
   30     CONTINUE
          CALL PGLINE(361,XC,YC)
   40 CONTINUE
C
C Having drawn the lines of latitude and longitude, one can now mark 
C points, etc.  To do this, call subroutine AITOFF to convert 
C latitude and longitude to (x,y) coordinates.  This is outside the 
C scope of this example program.  
C
      CALL PGEBUF
      CALL PGSCH(1.0)
      END

      SUBROUTINE AITOFF(B,L,X,Y)
C-----------------------------------------------------------------------
C Aitoff projection .
C
C       Input: latitude and longitude (B,L) in radians
C       Output: cartesian (X,Y) in range +/-2, +/-1
C-----------------------------------------------------------------------
      REAL L,B,X,Y,L2,DEN
C
      L2 = L/2.0
      DEN = SQRT(1.0+COS(B)*COS(L2))
      X = 2.0*COS(B)*SIN(L2)/DEN
      Y = SIN(B)/DEN
      END

      SUBROUTINE PGEX8
C-----------------------------------------------------------------------
C Demonstration program for PGPLOT. This program shows some of the
C possibilities for overlapping windows and viewports.
C T. J. Pearson  1986 Nov 28
C-----------------------------------------------------------------------
      INTEGER I
      REAL XR(720), YR(720)
C-----------------------------------------------------------------------
C Color index:
      INTEGER BLACK, WHITE, RED, GREEN, BLUE, CYAN, MAGENT, YELLOW
      PARAMETER (BLACK=0)
      PARAMETER (WHITE=1)
      PARAMETER (RED=2)
      PARAMETER (GREEN=3)
      PARAMETER (BLUE=4)
      PARAMETER (CYAN=5)
      PARAMETER (MAGENT=6)
      PARAMETER (YELLOW=7)
C Line style:
      INTEGER FULL, DASHED, DOTDSH, DOTTED, FANCY
      PARAMETER (FULL=1)
      PARAMETER (DASHED=2)
      PARAMETER (DOTDSH=3)
      PARAMETER (DOTTED=4)
      PARAMETER (FANCY=5)
C Character font:
      INTEGER NORMAL, ROMAN, ITALIC, SCRIPT
      PARAMETER (NORMAL=1)
      PARAMETER (ROMAN=2)
      PARAMETER (ITALIC=3)
      PARAMETER (SCRIPT=4)
C Fill-area style:
      INTEGER SOLID, HOLLOW
      PARAMETER (SOLID=1)
      PARAMETER (HOLLOW=2)
C-----------------------------------------------------------------------
C
      CALL PGPAGE
C
C Define the Viewport
C
      CALL PGSVP(0.1,0.6,0.1,0.6)
C
C Define the Window
C
      CALL PGSWIN(0.0, 630.0, -2.0, 2.0)
C
C Draw a box
C
      CALL PGSCI(CYAN)
      CALL PGBOX ('ABCTS', 90.0, 3, 'ABCTSV', 0.0, 0)
C
C Draw labels
C
      CALL PGSCI (RED)
      CALL PGBOX ('N',90.0, 3, 'VN', 0.0, 0)
C
C Draw SIN line
C
      DO 10 I=1,360
          XR(I) = 2.0*I
          YR(I) = SIN(XR(I)/57.29577951)
   10 CONTINUE
      CALL PGSCI (MAGENT)
      CALL PGSLS (DASHED)
      CALL PGLINE (360,XR,YR)
C
C Draw COS line by redefining the window
C
      CALL PGSWIN (90.0, 720.0, -2.0, 2.0)
      CALL PGSCI (YELLOW)
      CALL PGSLS (DOTTED)
      CALL PGLINE (360,XR,YR)
      CALL PGSLS (FULL)
C
C Re-Define the Viewport
C
      CALL PGSVP(0.45,0.85,0.45,0.85)
C
C Define the Window, and erase it
C
      CALL PGSWIN(0.0, 180.0, -2.0, 2.0)
      CALL PGSCI(0)
      CALL PGRECT(0.0, 180., -2.0, 2.0)
C
C Draw a box
C
      CALL PGSCI(BLUE)
      CALL PGBOX ('ABCTSM', 60.0, 3, 'VABCTSM', 1.0, 2)
C
C Draw SIN line
C
      CALL PGSCI (WHITE)
      CALL PGSLS (DASHED)
      CALL PGLINE (360,XR,YR)
      CALL PGSLS (SOLID)
C-----------------------------------------------------------------------
      END

      SUBROUTINE PGEX9
C----------------------------------------------------------------------
C Demonstration program for the PGPLOT plotting package.  This example
C illustrates curve drawing with PGFUNT; the parametric curve drawn is
C a simple Lissajous figure.
C                              T. J. Pearson  1983 Oct 5
C----------------------------------------------------------------------
      REAL     FX, FY
      EXTERNAL FX, FY
C
C Call PGFUNT to draw the function (autoscaling).
C
      CALL PGSCI(5)
      CALL PGFUNT(FX,FY,360,0.0,2.0*3.14159265,0)
C
C Call PGLAB to label the graph in a different color.
C
      CALL PGSCI(3)
      CALL PGLAB('x','y','PGPLOT Example 9 - routine PGFUNT')
      CALL PGSCI(1)
C
      END

      REAL FUNCTION FX(T)
      REAL T
      FX = SIN(T*5.0)
      RETURN
      END

      REAL FUNCTION FY(T)
      REAL T
      FY = SIN(T*4.0)
      RETURN
      END

      SUBROUTINE PGEX10
C----------------------------------------------------------------------
C Demonstration program for the PGPLOT plotting package.  This example
C illustrates curve drawing with PGFUNX.
C                              T. J. Pearson  1983 Oct 5
C----------------------------------------------------------------------
C The following define mnemonic names for the color indices and
C linestyle codes.
      INTEGER   BLACK, WHITE, RED, GREEN, BLUE, CYAN, MAGENT, YELLOW
      PARAMETER (BLACK=0)
      PARAMETER (WHITE=1)
      PARAMETER (RED=2)
      PARAMETER (GREEN=3)
      PARAMETER (BLUE=4)
      PARAMETER (CYAN=5)
      PARAMETER (MAGENT=6)
      PARAMETER (YELLOW=7)
      INTEGER   FULL, DASH, DOTD
      PARAMETER (FULL=1)
      PARAMETER (DASH=2)
      PARAMETER (DOTD=3)
C
C The Fortran functions to be plotted must be declared EXTERNAL.
C
      REAL     BESJ0, BESJ1
      EXTERNAL BESJ0, BESJ1
C
C Call PGFUNX twice to draw two functions (autoscaling the first time).
C
      CALL PGSCI(YELLOW)
      CALL PGFUNX(BESJ0,500,0.0,10.0*3.14159265,0)
      CALL PGSCI(RED)
      CALL PGSLS(DASH)
      CALL PGFUNX(BESJ1,500,0.0,10.0*3.14159265,1)
C
C Call PGLAB to label the graph in a different color. Note the
C use of "\f" to change font.  Use PGMTXT to write an additional
C legend inside the viewport.
C
      CALL PGSCI(GREEN)
      CALL PGLAB('\fix', 
     1             '\fiy',
     2             '\frPGPLOT Example 10')
      CALL PGMTXT('T', -4.0, 0.5, 0.5,
     1     '\fiy = J\d0\u(x)\fr (solid), '//
     2     '\fiy = J\d1\u(x)\fr (dashed)')
      CALL PGSCI(1)
C-----------------------------------------------------------------------
      END

      SUBROUTINE PGEX11
C-----------------------------------------------------------------------
C Test routine for PGPLOT: draws a skeletal dodecahedron.
C-----------------------------------------------------------------------
      INTEGER NVERT
      REAL T, T1, T2, T3
      PARAMETER (NVERT=20)
      PARAMETER (T=1.618)
      PARAMETER (T1=1.0+T)
      PARAMETER (T2=-1.0*T)
      PARAMETER (T3=-1.0*T1)
      INTEGER I, J, K
      REAL VERT(3,NVERT), R, ZZ
      REAL X(2),Y(2)
C
C Cartesian coordinates of the 20 vertices.
C
      DATA VERT/ T, T, T,       T, T,T2,
     3           T,T2, T,       T,T2,T2,
     5          T2, T, T,      T2, T,T2,
     7          T2,T2, T,      T2,T2,T2,
     9          T1,1.0,0.0,    T1,-1.0,0.0,
     B          T3,1.0,0.0,    T3,-1.0,0.0,
     D          0.0,T1,1.0,    0.0,T1,-1.0,
     F          0.0,T3,1.0,    0.0,T3,-1.0,
     H          1.0,0.0,T1,    -1.0,0.0,T1,
     J          1.0,0.0,T3,   -1.0,0.0,T3 /
C
C Initialize the plot (no labels).
C
      CALL PGENV(-4.,4.,-4.,4.,1,-2)
      CALL PGSCI(2)
      CALL PGSLS(1)
      CALL PGSLW(1)
C
C Write a heading.
C
      CALL PGLAB(' ',' ','PGPLOT Example 11 - Dodecahedron')
C
C Mark the vertices.
C
      DO 2 I=1,NVERT
          ZZ = VERT(3,I)
          CALL PGPT(1,VERT(1,I)+0.2*ZZ,VERT(2,I)+0.3*ZZ,9)
    2 CONTINUE
C
C Draw the edges - test all vertex pairs to find the edges of the 
C correct length.
C
      CALL PGSLW(3)
      DO 20 I=2,NVERT
          DO 10 J=1,I-1
              R = 0.
              DO 5 K=1,3
                  R = R + (VERT(K,I)-VERT(K,J))**2
    5         CONTINUE
              R = SQRT(R)
              IF(ABS(R-2.0).GT.0.1) GOTO 10
              ZZ = VERT(3,I)
              X(1) = VERT(1,I)+0.2*ZZ
              Y(1) = VERT(2,I)+0.3*ZZ
              ZZ = VERT(3,J)
              X(2) = VERT(1,J)+0.2*ZZ
              Y(2) = VERT(2,J)+0.3*ZZ
              CALL PGLINE(2,X,Y)
   10     CONTINUE
   20 CONTINUE
      CALL PGSLW(1)
      CALL PGSCI(1)
C-----------------------------------------------------------------------
      END

      REAL FUNCTION BESJ0(XX)
      REAL XX
C     BESSEL FUNCTION J0(XX)
C     REVISED SEPT, 1971  -  TRANSFERED TO VAX JULY 1979.
C     J0(-XX) = J0(XX)
C-----------------------------------------------------------------------
      REAL X, XO3, T, F0, THETA0
C
      X = ABS(XX)
      IF (X .LE. 3.0) THEN
          XO3 = X/3.
          T   = XO3*XO3
          BESJ0 = 1.+T*(-2.2499997+T*(1.2656208+T*(-.3163866+T*(.0444479
     1            +T*(-.003944+T*.0002100)))))
      ELSE
          T = 3./X
          F0 = .79788456+T*(-.00000077+T*(-.00552740+T*(-.00009512
     1         +T*(.00137237+T*(-.00072805+T*.00014476)))))
          THETA0 = X-.78539816+T*(-.04166397+T*(-.00003954
     1      +T*(.00262573+T*(-.00054125+T*(-.00029333+T*.00013558)))))
          BESJ0 = F0*COS(THETA0)/SQRT(X)
      END IF
      RETURN
      END

      REAL FUNCTION BESJ1(XX)
      REAL XX
C     BESSEL FUNCTION J1(XX)
C     REVISED SEPT,1971
C     J1(-XX)=-J1(XX)
C     TRANSFERED TO VAX    JULY 1979
C-----------------------------------------------------------------------
      REAL X, XO3, T, F1, THETA1
C
      X = ABS(XX)
      IF (X .LE. 3.0) THEN
          XO3 = X/3.
          T = XO3*XO3
          BESJ1 = .5+T*(-.56249985+T*(.21093573+T*
     1                 (-.03954289+T*(.00443319+T*
     2                 (-.00031761+T*.00001109)))))
          BESJ1 = BESJ1*XX
      ELSE
          T = 3./X
          F1 = .79788456+T*(.00000156+T*(.01659667+T*(.00017105
     1         +T*(-.00249511+T*(.00113653-T*.00020033)))))
          THETA1 = X-2.35619449+T*(.12499612+T*(.00005650+T*(-.00637879
     1             +T*(.00074348+T*(.00079824-T*.00029166)))))
          BESJ1 = F1*COS(THETA1)/SQRT(X)
      END IF
      IF (XX .LT. 0.0) BESJ1 = -BESJ1
      RETURN
      END

C*RNGAUS -- random number from Gaussian (normal) distribution
C+
      REAL FUNCTION RNGAUS (ISEED)
      INTEGER ISEED
C
C Returns a normally distributed deviate with zero mean and unit 
C variance. The routine uses the Box-Muller transformation of uniform
C deviates. Reference: Press et al., Numerical Recipes, Sec. 7.2.
C
C Arguments:
C  ISEED  (in/out) : seed used for RAN5 random-number generator.
C
C Subroutines required:
C  RAN5 -- return a uniform random deviate between 0 and 1.
C
C History:
C  1987 Nov 13 - TJP.
C-----------------------------------------------------------------------
      INTEGER ISET
      REAL R, V1, V2, FAC, GSET
      REAL RAN5
      SAVE ISET, GSET
      DATA ISET/0/
C
      IF (ISET.EQ.0) THEN
   10     V1 = 2.*RAN5(ISEED)-1.
          V2 = 2.*RAN5(ISEED)-1.
          R = V1**2+V2**2
          IF (R.GE.1.) GOTO 10
          FAC = SQRT(-2.*LOG(R)/R)
          GSET = V1*FAC
          RNGAUS = V2*FAC
          ISET = 1
      ELSE
          RNGAUS = GSET
          ISET = 0
      END IF
C
      END

C*RAN5 -- random number from uniform distribution
C+
      REAL FUNCTION RAN5(IDUM)
      INTEGER IDUM
C
C Returns a uniform random deviate between 0.0 and 1.0.
C
C Park and Miller's "Minimal Standard" random number generator (Comm.
C ACM, 31, 1192, 1988) with Bays-Durham shuffle. Call with IDUM negative
C to initialize. Be sure to preserve IDUM between calls.
C Reference: Press and Farrar, Computers in Physics, Vol.4, No. 2, 
C p.190, 1990.
C
C Arguments:
C  IDUM  (in/out) : seed.
C
C History:
C  1990 Apr 6 - TJP.
C-----------------------------------------------------------------------
      INTEGER IA, IM, IQ, IR, NTAB
      REAL    AM, ATAB
      PARAMETER (IA=16807, IM=2147483647, AM=1.0/IM)
      PARAMETER (IQ=127773, IR=2836, NTAB=32, ATAB=NTAB-1)
C-
      INTEGER J, K
      REAL V(NTAB), Y
      SAVE V, Y
      DATA V/NTAB*0.0/, Y/0.5/
C-
      IF (IDUM.LE.0) THEN
          IDUM = MAX(-IDUM,1)
          DO 12 J=NTAB,1,-1
              K = IDUM/IQ
              IDUM = IA*(IDUM-K*IQ) - IR*K
              IF (IDUM.LT.0) IDUM = IDUM+IM
              V(J) = AM*IDUM
   12     CONTINUE
          Y = V(1)
      END IF
    1 CONTINUE
          K = IDUM/IQ
          IDUM = IA*(IDUM-K*IQ) - IR*K
          IF (IDUM.LT.0) IDUM = IDUM+IM
          J = 1 + INT(ATAB*Y)
          Y = V(J)
          RAN5 = Y
          V(J) = AM*IDUM
      IF (RAN5.EQ.0.0 .OR. RAN5.EQ.1.0) GOTO 1
      RETURN
      END
