      PROGRAM PGEX5
C----------------------------------------------------------------------
C Demonstration program for the PGPLOT plotting package. This example
C illustrates the different line widths.
C                              T. J. Pearson  1982 Dec 28
C----------------------------------------------------------------------
      INTEGER IW
      REAL X(2), Y(2)
C
C Call PGBEG to initiate PGPLOT and open the output device; PGBEG
C will prompt the user to supply the device name and type.
C
      CALL PGBEG(0,'?',1,1)
C
C Call PGENV to initialize the viewport and window.
C
      CALL PGENV(0.,15.,0.,15.,0,0)
C
C Call PGLAB to label the graph.
C
      CALL PGLAB('Line Width',' ','PGPLOT Example 5')
C
C Draw 14 oblique lines in different thicknesses.
C
      DO 10 IW=1,14
          X(1) = IW
          Y(1) = 0.0
          X(2) = 0.0
          Y(2) = IW
          CALL PGSLW(IW)
          CALL PGLINE(2,X,Y)
   10 CONTINUE
C
C Draw another set of lines, dashed instead of solid.
C
      CALL PGSLS(2)
      DO 20 IW=1,14
          X(1) = IW
          Y(1) = 15.0
          X(2) = 15.0
          Y(2) = IW
          CALL PGSLW(IW)
          CALL PGLINE(2,X,Y)
   20 CONTINUE
C
C Don't forget to call PGEND!
C
      CALL PGEND
      END
