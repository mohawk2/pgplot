      PROGRAM FIG42
C----------------------------------------------------------------------
C Illustrating the use of PGPTEXT.
C (Figure 4.2 of PGPLOT manual).
C T. J. Pearson  1988 Mar 17.
C----------------------------------------------------------------------
      INTEGER PGBEGIN, I
      REAL DX, DY, XOFF, YOFF
      CHARACTER*80 SAMPLE(7)
      DATA SAMPLE(1)
     1 /'Normal:  \fnABCDQ efgh 1234 \ga\gb\gg\gd \gL\gH\gD\gW'/
      DATA SAMPLE(2)
     1 /'Roman:  \frABCDQ efgh 1234 \ga\gb\gg\gd \gL\gH\gD\gW'/
      DATA SAMPLE(3)
     1 /'Italic:  \fiABCDQ efgh 1234 \ga\gb\gg\gd \gL\gH\gD\gW'/
      DATA SAMPLE(4)
     1 /'Script:  \fsABCDQ efgh 1234 \ga\gb\gg\gd \gL\gH\gD\gW'/
      DATA SAMPLE(5)
     1 /'\fif\fr(\fix\fr) = \fix\fr\u2\dcos(2\gp\fix\fr)e\u\fix\fr\u2'/
      DATA SAMPLE(6)
     1 /'\fiH\d0\u \fr= 75 \(2233) 25 km s\u-1\d Mpc\u-1\d'/
      DATA SAMPLE(7)
     1 /'\fsL/L\d\(2281)\u\fr = 5.6 (\gl1216\A)'/
C
C Open device and determine size of view surface.
C Lower left corner is (X1,Y1), upper right (X2, Y2) [inches].
C
      IF (PGBEGIN(0,'?',1,1).NE.1) CALL EXIT
      CALL PGBBUF
      CALL PGVPORT(0.0, 1.0, 0.0, 1.0)
      CALL PGQVP(1, X1, X2, Y1, Y2)
      X = X2-X1
      Y = Y2-Y1
C
C Initialize.
C
      DX = 4.5
      DY = 7.5
      XOFF = (X-DX)*0.5
      YOFF = (Y-DY)*0.5
      IF (XOFF.LT.0.0 .OR. YOFF.LT.0.0)
     1  WRITE (*,*) 'WARNING: device view surface too small'
      CALL PGVSIZE(XOFF, XOFF+DX,
     1             YOFF, YOFF+DY)
      CALL PGWINDOW(0.,1.,16.,0.)
      CALL PGSLW(1)
      CALL PGBOX('BC',10.0,0,'BC',10.0,0)
      CALL PGSCH(1.0)
      DO 10 I=1,7
          CALL PGTEXT(0.05,FLOAT(I),SAMPLE(I))
   10 CONTINUE
      CALL PGSCH(1.5)
      CALL PGSLW(3)
      CALL PGTEXT(0.05,10.0,'Bigger (1.5)')
      CALL PGSCH(0.5)
      CALL PGSLW(1)
      CALL PGTEXT(0.5,10.0,'Smaller (0.5)')
      CALL PGSCH(1.0)
      CALL PGPTEXT(0.5, 12.0, 0.0, 0.0, 'Left justified (0.0)')
      CALL PGPOINT(1,0.5,12.0,3)
      CALL PGPTEXT(0.5, 13.0, 0.0, 0.5, 'Centered (0.5)')
      CALL PGPOINT(1,0.5,13.0,3)
      CALL PGPTEXT(0.5, 14.0, 0.0, 1.0, 'Right justified (1.0)')
      CALL PGPOINT(1,0.5,14.0,3)
      CALL PGPTEXT(0.5, 15.0, 45.0, 0.0, 'Angle = 45\(2218)')
      CALL PGPOINT(1,0.5,15.0,3)
      CALL PGEBUF
      CALL PGEND
      END
