      PROGRAM PGDEM3
C-----------------------------------------------------------------------
C Demonstration program for PGPLOT contouring routines.
C-----------------------------------------------------------------------
      WRITE (*,'(A)') ' Demonstration of PGPLOT contouring routines'
C
C Call PGBEG to initiate PGPLOT and open the output device; PGBEG
C will prompt the user to supply the device name and type.
C
      CALL PGBEG(0,'?',1,1)
C
C Call the demonstration subroutines.
C
      WRITE (*,'(A)') ' Routine PGCONT'
      CALL PGEX31
      WRITE (*,'(A)') ' Routine PGCONS'
      CALL PGEX32
      WRITE (*,'(A)') ' Routine PGCONB'
      CALL PGEX33
      WRITE (*,'(A)') ' Routine PGCONX'
      CALL PGEX34
C
C Finally, call PGEND to terminate things properly.
C
      CALL PGEND
C-----------------------------------------------------------------------
      END

      SUBROUTINE PGEX31
C-----------------------------------------------------------------------
C Demonstration of contouring routine PGCONT.
C-----------------------------------------------------------------------
      REAL BLANK
      PARAMETER (BLANK=-1.2E20)
      INTEGER I,J
      REAL F(40,40),FMIN,FMAX,ALEV,TR(6)
      DATA TR/0.,1.,0.,0.,0.,1./
C
C Compute a suitable function.
C
      FMIN = F(1,1)
      FMAX = F(1,1)
      DO 20 I=1,40
          DO 10 J=1,40
              F(I,J) = COS(0.3*SQRT(I*2.)-0.4*J/3.)*COS(0.4*I/3)+
     1                     (I-J)/40.0
              FMIN = MIN(F(I,J),FMIN)
              FMAX = MAX(F(I,J),FMAX)
   10     CONTINUE
   20 CONTINUE
C
C Clear the screen. Set up window and viewport.
C
      CALL PGPAGE
      CALL PGSVP(0.05,0.95,0.05,0.95)
      CALL PGSWIN(1.0,40.0,1.0,40.0)
      CALL PGBOX('bcts',0.0,0,'bcts',0.0,0)
      CALL PGMTXT('t',1.0,0.0,0.0,'Contouring using PGCONT')
C
C Draw the map.  PGCONT is called once for each contour, using
C different line attributes to distinguish contour levels.
C
      DO 30 I=1,21
          ALEV = FMIN + (I-1)*(FMAX-FMIN)/20.0
          IF (MOD(I,5).EQ.0) THEN
              CALL PGSLW(3)
          ELSE
              CALL PGSLW(1)
          END IF
          IF (I.LT.10) THEN
              CALL PGSCI(2)
              CALL PGSLS(2)
          ELSE
              CALL PGSCI(3)
              CALL PGSLS(1)
          END IF
          CALL PGCONT(F,40,40,1,40,1,40,ALEV,-1,TR)
   30 CONTINUE
      CALL PGSLW(1)
      CALL PGSLS(1)
      CALL PGSCI(1)
      END

      SUBROUTINE PGEX32
C-----------------------------------------------------------------------
C Demonstration of contouring routine PGCONS.
C-----------------------------------------------------------------------
      REAL BLANK
      PARAMETER (BLANK=-1.2E20)
      INTEGER I,J
      REAL F(40,40),FMIN,FMAX,ALEV,TR(6)
      DATA TR/0.,1.,0.,0.,0.,1./
C
C Compute a suitable function.
C
      FMIN = F(1,1)
      FMAX = F(1,1)
      DO 20 I=1,40
          DO 10 J=1,40
              F(I,J) = COS(0.3*SQRT(I*2.)-0.4*J/3.)*COS(0.4*I/3)+
     1                     (I-J)/40.0
              FMIN = MIN(F(I,J),FMIN)
              FMAX = MAX(F(I,J),FMAX)
   10     CONTINUE
   20 CONTINUE
C
C Clear the screen. Set up window and viewport.
C
      CALL PGPAGE
      CALL PGBOX('bcts',0.0,0,'bcts',0.0,0)
      CALL PGMTXT('t',1.0,0.0,0.0,'Contouring using PGCONS')
C
C Draw the map.  PGCONS is called once for each contour, using
C different line attributes to distinguish contour levels.
C
      DO 40 I=1,21
          ALEV = FMIN + (I-1)*(FMAX-FMIN)/20.0
          IF (MOD(I,5).EQ.0) THEN
              CALL PGSLW(3)
          ELSE
              CALL PGSLW(1)
          END IF
          IF (I.LT.10) THEN
              CALL PGSCI(2)
              CALL PGSLS(2)
          ELSE
              CALL PGSCI(3)
              CALL PGSLS(1)
          END IF
          CALL PGCONS(F,40,40,1,40,1,40,ALEV,-1,TR)
   40 CONTINUE
      CALL PGSLW(1)
      CALL PGSLS(1)
      CALL PGSCI(1)
      END

      SUBROUTINE PGEX33
C-----------------------------------------------------------------------
C Demonstration of contouring routine PGCONB.
C-----------------------------------------------------------------------
      REAL BLANK
      PARAMETER (BLANK=-1.2E20)
      INTEGER I,J
      REAL F(40,40),FMIN,FMAX,ALEV,TR(6),X,Y,R
      DATA TR/0.,1.,0.,0.,0.,1./
C
C Compute a suitable function.
C
      FMIN = F(1,1)
      FMAX = F(1,1)
      DO 20 I=1,40
          DO 10 J=1,40
              F(I,J) = COS(0.3*SQRT(I*2.)-0.4*J/3.)*COS(0.4*I/3)+
     1                     (I-J)/40.0
              FMIN = MIN(F(I,J),FMIN)
              FMAX = MAX(F(I,J),FMAX)
   10     CONTINUE
   20 CONTINUE
C
C "Blank" the data outside an annulus.
C
      DO 60 I=1,40
          DO 50 J=1,40
              R = SQRT((I-20.5)**2 + (J-20.5)**2)
              IF (R.GT.20.0 .OR. R.LT.3.0) F(I,J) = BLANK
   50     CONTINUE
   60 CONTINUE
C
      CALL PGPAGE
      CALL PGBOX('bcts',0.0,0,'bcts',0.0,0)
      CALL PGMTXT('t',1.0,0.0,0.0,'Contouring using PGCONB')
      DO 80 I=1,21
          ALEV = FMIN + (I-1)*(FMAX-FMIN)/20.0
          IF (MOD(I,5).EQ.0) THEN
              CALL PGSLW(3)
          ELSE
              CALL PGSLW(1)
          END IF
          IF (I.LT.10) THEN
              CALL PGSCI(2)
              CALL PGSLS(2)
          ELSE
              CALL PGSCI(3)
              CALL PGSLS(1)
          END IF
          CALL PGCONB(F,40,40,1,40,1,40,ALEV,-1,TR,BLANK)
   80 CONTINUE
C
C Mark the blanked points for easy identification.
C
      CALL PGSCI(1)
      DO 100 I=1,40
          DO 90 J=1,40
              IF (F(I,J).EQ.BLANK) THEN
                  X = TR(1) + REAL(I)*TR(2) + REAL(J)*TR(3)
                  Y = TR(4) + REAL(I)*TR(5) + REAL(J)*TR(6)
                  CALL PGPT(1, X, Y, -1)
              END IF
   90     CONTINUE
  100 CONTINUE
      END

      SUBROUTINE PGEX34
C-----------------------------------------------------------------------
C This program is intended to demonstrate the use of the PGPLOT routine
C PGCONX. As an example, we take data defined on a sphere. We want to
C draw a contour map of the data on an equal-area projection of the 
C surface of the sphere; we choose the Aitoff projection centered on
C Declination (latitude) 0, Right Ascension (longitude) 0. The data
C are defined at 1-degree intervals in both coordinates. We thus need
C a data array dimensioned 361 by 181; the array index runs from -90
C to +90 in declination (181 elements) and from -180 to +180 in right
C ascension (361 elements). The data at -180 and +180 must be
C identical, of course, but they need to be duplicated in the array as
C these two longitudes appear on opposite sides of the map. 
C-----------------------------------------------------------------------
      REAL  RPDEG
      PARAMETER (RPDEG=3.1415926/180.0)
      INTEGER I, J
      REAL RA, DEC, B, L, XC(361), YC(361)
      REAL Q(361,181), C(9)
      EXTERNAL PLOT
C
C Call PGENV to create a rectangular window of 4 x 2 units. This is 
C the bounding rectangle of the Aitoff plot. The JUST argument is 1
C to get equal scales in x and y.  
C
      CALL PGBBUF
      CALL PGENV(-2.0, 2.0, -1.0, 1.0, 1, -2)
      CALL PGLAB('Right Ascension', 'Declination', 
     1             'Aitoff Equal-Area Projection of the Sphere')
C
C Draw 7 lines of constant longitude at longitude 0, 60, 120, ..., 
C 360 degrees. Each line is made up of 180 straight-line segments.
C
      DO 20 J=1,7
          RA = (-180.+(J-1)*60.)*RPDEG
          DO 10 I=1,181
              DEC = (I-91)*RPDEG
              CALL AITOFF(DEC,RA,XC(I),YC(I))
   10     CONTINUE
          CALL PGLINE(181,XC,YC)
   20 CONTINUE
C
C Draw 5 lines of constant latitude at latitudes -60, -30, 0, 30, 
C 60 degrees. Each line is made up of 360 straight-line segments.
C
      DO 40 J=1,5
          DEC = (-60.+(J-1)*30.)*RPDEG
          DO 30 I=1,361
              RA = (I-181)*RPDEG
              CALL AITOFF(DEC,RA,XC(I),YC(I))
   30     CONTINUE
          CALL PGLINE(361,XC,YC)
   40 CONTINUE
      CALL PGEBUF
C
C Compute the data to be contoured. In practice the data might be read
C in from an external file. In this example the data are computed: they
C are the galactic latitudes of the points on the sphere. Thus the 
C contours will be lines of constant galactic latitude.
C
      DO 60 J=1,181
          DEC = (J-91)*RPDEG
          DO 50 I=1,361
              RA = (I-181)*RPDEG
              CALL GALACT(RA, DEC, B,L)
              Q(I,J) = B
   50     CONTINUE
   60 CONTINUE
C
C Draw the contour map using PGCONX. Contours at 0, 20, 40, 60, 80.
C
      DO 70 I=1,9
          C(I) = -100.0 +I*20.0
   70 CONTINUE
      CALL PGBBUF
      CALL PGSCI(2)
      CALL PGCONX(Q, 361, 181, 1, 361, 1, 181, C, 9, PLOT)
      CALL PGEBUF
      END

      SUBROUTINE PLOT(VISBLE, X, Y, Z)
      INTEGER VISBLE
      REAL X,Y,Z
C-----------------------------------------------------------------------
C Plotting subroutine for PGCONX. This routine converts the input
C coordinates (latitude and longitude) into the projected coordinates
C (x and y), and moves or draws as requested by VISBLE.
C-----------------------------------------------------------------------
      REAL  RPDEG
      PARAMETER (RPDEG=3.1415926/180.0)
      REAL B, L, XWORLD, YWORLD
      B = (Y-91.0)*RPDEG
      L = (X-181.0)*RPDEG
      CALL AITOFF(B, L, XWORLD, YWORLD)
      IF (VISBLE.EQ.0) THEN
          CALL PGMOVE(XWORLD, YWORLD)
      ELSE
          CALL PGDRAW(XWORLD, YWORLD)
      END IF
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

      SUBROUTINE GALACT(RA,DEC,GLAT,GLONG)
C-----------------------------------------------------------------------
C Convert 1950.0 equatorial coordinates (RA, DEC) to galactic
C latitude and longitude (GLAT, GLONG).
C
C Arguments:
C  RA, DEC (input): 1950.0 RA and Dec (radians).
C  GLAT, GLONG (output): galactic latitude and longitude 
C      (degrees).
C
C Reference: e.g., D. R. H. Johnson and D. R. Soderblom, A. J. v93, 
C  p864 (1987).
C-----------------------------------------------------------------------
      REAL RA, RRA, DEC, RDEC, CDEC, R(3,3), E(3), G(3)
      REAL RADDEG, RADHR, GLAT, GLONG
      INTEGER I, J
      DATA R/-.066988740D0, .492728466D0,-.867600811D0,-.872755766D0,
     $       -.450346958D0,-.188374601D0,-.483538915D0, .744584633D0,
     $        .460199785D0/
      DATA RADDEG/57.29577951D0/,RADHR/3.8197186335D0/
C-----------------------------------------------------------------------
      RRA = RA
      RDEC = DEC
      CDEC = COS(RDEC)
      E(1) = CDEC*COS(RRA)
      E(2) = CDEC*SIN(RRA)
      E(3) = SIN(RDEC)
      DO 20 I=1,3
          G(I) = 0.0
          DO 10 J=1,3
              G(I) = G(I) + E(J)*R(I,J)
   10     CONTINUE
   20 CONTINUE
      GLAT  = ASIN(G(3))*RADDEG
      GLONG = ATAN2(G(2),G(1))*RADDEG
      IF (GLONG.LT.0.0) GLONG = GLONG+360.0
      RETURN
C-----------------------------------------------------------------------
      END
