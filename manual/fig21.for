      PROGRAM FIG21
C----------------------------------------------------------------------
C Simple Example of use of PGPLOT.
C (Figure 2.1 of PGPLOT manual).
C T. J. Pearson  1986 April 23.
C----------------------------------------------------------------------
C     PROGRAM SIMPLE
      REAL XS(5),YS(5), XR(100),YR(100)
      DATA XS/1.,2.,3.,4.,5./
      DATA YS/1.,4.,9.,16.,25./
      CALL PGBEGIN(0,'?',1,1)
      CALL PGENV(0.,10.,0.,20.,0,1)
      CALL PGLABEL('(x)', '(y)', 'A Simple Graph')
      CALL PGPOINT(5,XS,YS,9)
      DO 10 I=1,60
          XR(I) = 0.1*I
          YR(I) = XR(I)**2
   10 CONTINUE
      CALL PGLINE(60,XR,YR)
      CALL PGEND
      END
