C*PGHIST -- histogram of unbinned data
C+
      SUBROUTINE PGHIST (N, DATA, DATMIN, DATMAX, NBIN, PGFLAG)
      INTEGER N
      REAL    DATA(*)
      REAL    DATMIN, DATMAX
      INTEGER NBIN, PGFLAG
C
C Draw a histogram of N values of a variable in array
C DATA(1...N) in the range DATMIN to DATMAX using NBIN bins.  Note
C that array elements which fall exactly on the boundary between
C two bins will be counted in the higher bin rather than the
C lower one; and array elements whose value is less than DATMIN or
C greater than or equal to DATMAX will not be counted at all.
C
C Arguments:
C  N      (input)  : the number of data values.
C  DATA   (input)  : the data values. Note: the dimension of array
C                    DATA must be greater than or equal to N. The
C                    first N elements of the array are used.
C  DATMIN (input)  : the minimum data value for the histogram.
C  DATMAX (input)  : the maximum data value for the histogram.
C  NBIN   (input)  : the number of bins to use: the range DATMIN to
C                    DATMAX is divided into NBIN equal bins and
C                    the number of DATA values in each bin is
C                    determined by PGHIST.  NBIN may not exceed 200.
C  PGFLAG (input)  : if PGFLAG = 1, the histogram is plotted in the
C                    current window and viewport; if PGFLAG = 0,
C                    PGENV is called automatically by PGHIST to start
C                    a new plot (the x-limits of the window will be
C                    DATMIN and DATMAX; the y-limits will be chosen
C                    automatically.
C--
C Side effects:
C
C The pen position is changed to (DATMAX,0.0) in world coordinates.
C--
C  6-Sep-1983
C 13-Dec-1990  Make errors non-fatal [TJP].
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      INTEGER  MAXBIN
      PARAMETER (MAXBIN=200)
      INTEGER  I, IBIN, JUNK, NUM(MAXBIN), NUMMAX
      REAL     BINSIZ, PGRND
      REAL     CUR, PREV, XLO, XHI, YLO, YHI
C
      IF (N.LT.1 .OR. DATMAX.LE.DATMIN .OR. NBIN.LT.1 .OR.
     1    NBIN.GT.MAXBIN) THEN
          CALL GRWARN('PGHIST ignored: invalid arguments')
          RETURN
      END IF
      CALL PGBBUF
C
C How many values in each bin?
C
      DO 10 IBIN=1,NBIN
          NUM(IBIN) = 0
   10 CONTINUE
      DO 20 I=1,N
          IBIN = (DATA(I)-DATMIN)/(DATMAX-DATMIN)*NBIN+1
          IF (IBIN.GE.1 .AND. IBIN.LE.NBIN) NUM(IBIN) = NUM(IBIN)+1
   20 CONTINUE
      NUMMAX = 0
      DO 30 IBIN=1,NBIN
          NUMMAX = MAX(NUMMAX,NUM(IBIN))
   30 CONTINUE
      BINSIZ = (DATMAX-DATMIN)/NBIN
C
C Boundaries of plot.
C
      XLO = DATMIN
      XHI = DATMAX
      YLO = 0.0
      YHI = PGRND(1.01*NUMMAX,JUNK)
C
C Define environment if necessary.
C
      IF (PGFLAG.EQ.0) CALL PGENV(XLO,XHI,YLO,YHI,0,0)
C
C Draw Histogram.
C
      PREV = 0.0
      CALL GRMOVA(DATMIN,0.0)
      DO 40 IBIN=1,NBIN
          CUR = NUM(IBIN)
          XHI = DATMIN + IBIN*BINSIZ
          XLO = XHI - BINSIZ
          IF (CUR.EQ.0.0) THEN
              CONTINUE
          ELSE IF (CUR.LE.PREV) THEN
              CALL GRMOVA(XLO,CUR)
              CALL GRLINA(XHI,CUR)
          ELSE
              CALL GRMOVA(XLO,PREV)
              CALL GRLINA(XLO,CUR)
              CALL GRLINA(XHI,CUR)
          END IF
          CALL GRLINA(XHI,0.0)
          PREV = CUR
   40 CONTINUE
C
      CALL PGEBUF
      END
