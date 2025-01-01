      PROGRAM FIG41
C----------------------------------------------------------------------
C Plot a table of the standard PGPLOT graph marker symbols.
C (Figure 4.1 of PGPLOT manual).
C T. J. Pearson  1988 Mar 17.
C----------------------------------------------------------------------
      INTEGER PGBEGIN
      CHARACTER*2 LABEL
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
      NX = 4
      NY = 8
      IX = NX
      JY = 1
      DX = 4.5/NX
      DY = 7.5/NY
      XOFF = (X-DX*NX)*0.5
      YOFF = (Y-DY*NY)*0.5
      IF (XOFF.LT.0.0 .OR. YOFF.LT.0.0)
     1  WRITE (*,*) 'WARNING: device view surface too small'
C
C Loop through all known symbols (N=0-31)
C
      DO 10 N=0,31
          WRITE (LABEL,'(I2)') N
C
C Define window and viewport.
C
          IX = IX+1
          IF (IX.GT.NX) THEN
            IX = 1
            JY = JY-1
          END IF
          IF (JY.LT.1) THEN
            JY = NY
            CALL PGADVANCE
          END IF
          CALL PGVSIZE(XOFF+(IX-1)*DX, XOFF+IX*DX,
     1                 YOFF+(JY-1)*DY, YOFF+JY*DY)
          CALL PGWINDOW(-1.,1.,-1.,1.)
C
C Call PGBOX to draw a box and PGMTEXT to label it.
C
          CALL PGSLW(1)
          CALL PGBOX('BC',10.0,0,'BC',10.0,0)
          CALL PGSCH(0.5)
          CALL PGMTEXT('T',-1.5,0.05,0.0,LABEL)
C
C Call PGPOINT to draw the symbol.
C
          CALL PGSLW(2)
          CALL PGSCH(1.5)
          CALL PGPOINT(1,0.0,0.0,N)
   10 CONTINUE
C
C Don't forget to call PGEND!
C
      CALL PGEBUF
      CALL PGEND
      END
