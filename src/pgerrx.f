C*PGERRX -- horizontal error bar
C+
      SUBROUTINE PGERRX (N, X1, X2, Y, T)
      INTEGER N
      REAL X1(*), X2(*), Y(*)
      REAL T
C
C Plot horizontal error bars.
C This routine draws an error bar only; to mark the data point in
C the middle of the error bar, an additional call to PGPT or
C PGERRY is required.
C
C Arguments:
C  N      (input)  : number of error bars to plot.
C  X1     (input)  : world x-coordinates of lower end of the
C                    error bars.
C  X2     (input)  : world x-coordinates of upper end of the
C                    error bars.
C  Y      (input)  : world y-coordinates of the data.
C  T      (input)  : length of terminals to be drawn at the ends
C                    of the error bar, as a multiple of the default
C                    length; if T = 0.0, no terminals will be drawn.
C
C Note: the dimension of arrays X1, X2, and Y must be greater
C than or equal to N. If N is 1, X1, X2, and Y may be scalar
C variables, or expressions, eg:
C       CALL PGERRX(1,X-SIGMA,X+SIGMA,Y)
C--
C (6-Oct-1983)
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      INTEGER  I
      REAL     TIK
C
      IF (PGOPEN.EQ.0) RETURN
      IF (N.LT.1) RETURN
      CALL PGBBUF
C
      TIK = T*XSP*0.15/YSCALE
      DO 10 I=1,N
          IF (TIK.NE.0.0) THEN
              CALL GRMOVA(X1(I),Y(I)-TIK)
              CALL GRLINA(X1(I),Y(I)+TIK)
          END IF
          CALL GRMOVA(X1(I),Y(I))
          CALL GRLINA(X2(I),Y(I))
          IF (TIK.NE.0.0) THEN
              CALL GRMOVA(X2(I),Y(I)-TIK)
              CALL GRLINA(X2(I),Y(I)+TIK)
          END IF
   10 CONTINUE
      CALL PGEBUF
      END
