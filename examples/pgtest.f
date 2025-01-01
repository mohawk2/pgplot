      PROGRAM PGTEST
C-----------------------------------------------------------------------
C Demonstration program for PGPLOT. The main program opens the output
C device and calls a series of subroutines, one for each sample plot.
C-----------------------------------------------------------------------
      INTEGER PGBEG
C
C Call PGBEG to initiate PGPLOT and open the output device; PGBEG
C will prompt the user to supply the device name and type.
C
      IF (PGBEG(0,'?',1,1) .NE. 1) STOP
C
C Call the demonstration subroutines.
C
      CALL PGEX22
C
C Finally, call PGEND to terminate things properly.
C
      CALL PGEND
C-----------------------------------------------------------------------
      END

      SUBROUTINE PGEX22
C-----------------------------------------------------------------------
C Demonstration program for the PGPLOT plotting package.
C Plot a table of the standard PGPLOT graph marker symbols. This
C program also illustrates how windows and viewports may be manipulated.
C-----------------------------------------------------------------------
      CHARACTER*2 LABEL
      INTEGER NX, NY, N, IX, JY, LW
      REAL X, X1, X2, XOFF, Y, Y1, Y2, YOFF, DX, DY
      REAL XPIX1, XPIX2, YPIX1, YPIX2, RES
C
C Determine size of view surface.
C Lower left corner is (X1,Y1), upper right (X2, Y2) [inches].
C
      CALL PGSVP(0.0, 1.0, 0.0, 1.0)
      CALL PGQVP(1, X1, X2, Y1, Y2)
      X = X2-X1
      Y = Y2-Y1
C
C Determine device resolution (pixels/inch), and use it to choose
C line width.
C
      CALL PGQVP(3, XPIX1, XPIX2, YPIX1, YPIX2)
      RES = ABS(XPIX2-XPIX1)/ABS(X)
      LW = 1
      IF (RES.GT.166.0) LW = 2
C
C Choose horizontal or vertical arrangement depending on
C device aspect ratio.
C
      IF (X.GT.Y) THEN
          NX = 8
          NY = 4
      ELSE
          NX = 4
          NY = 8
      END IF
      DX = MIN(X/NX, 0.95*Y/NY)
      DY = DX
      IX = NX
      JY = 1
      XOFF = X1 + (X-NX*DX)*0.5
      YOFF = Y1 + (0.95*Y-NY*DY)*0.5
      CALL PGBBUF
C
C Each symbol will be drawn in a standard window; the window is moved
C by manipulating the viewport.
C
      CALL PGSWIN(-1.,1.,-1.,1.)
C
C Loop through all known symbols (N=0-31). 
C
      DO 10 N=0,31
          WRITE (LABEL,'(I2)') N
C
C Define window and viewport. The loop allows the plot to extend over
C more than one page if necessary; each page is labelled at the top.
C
          IX = IX+1
          IF (IX.GT.NX) THEN
            IX = 1
            JY = JY-1
          END IF
          IF (JY.LT.1) THEN
            JY = NY
            CALL PGPAGE
            CALL PGSCH(1.2)
            CALL PGVSIZ(XOFF, XOFF+NX*DX, YOFF, YOFF+NY*DY)
            CALL PGSLW(LW)
            CALL PGMTXT('T', 1.0, 0.5, 0.5,
     1                   '\fiPGPLOT \frMarker Symbols')
          END IF
          CALL PGVSIZ(XOFF+(IX-1)*DX, XOFF+IX*DX,
     1                 YOFF+(JY-1)*DY, YOFF+JY*DY)
C
C Call PGBOX to draw a box and PGMTXT to label it.
C
          CALL PGSLW(1)
          CALL PGBOX('BC',10.0,0,'BC',10.0,0)
          CALL PGSCH(0.5)
          CALL PGMTXT('T',-1.5,0.05,0.0,LABEL)
C
C Call PGPT to draw the symbol.
C
          CALL PGSLW(LW)
          CALL PGSCH(1.5)
          CALL PGPT(1,0.0,0.0,-N)
   10 CONTINUE
C
      CALL PGEBUF
C-----------------------------------------------------------------------
      END
