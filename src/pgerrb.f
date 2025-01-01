C*PGERRB -- horizontal error bar
C+
      SUBROUTINE PGERRB (DIR, N, X, Y, E, T)
      INTEGER DIR, N
      REAL X(*), Y(*), E(*)
      REAL T
C
C Plot error bars in the direction specified by DIR.
C This routine draws an error bar only; to mark the data point at
C the start of the error bar, an additional call to PGPT is required.
C
C Arguments:
C  DIR    (input)  : direction to plot the error bar relative to
C                    the data point.  DIR is 1 for +X; 2 for +Y;
C                    3 for -X; and 4 for -Y;
C  N      (input)  : number of error bars to plot.
C  X      (input)  : world x-coordinates of the data.
C  Y      (input)  : world y-coordinates of the data.
C  E      (input)  : value of error bar distance to be added to the
C                    data position in world coordinates.
C  T      (input)  : length of terminals to be drawn at the ends
C                    of the error bar, as a multiple of the default
C                    length; if T = 0.0, no terminals will be drawn.
C
C Note: the dimension of arrays X, Y, and E must be greater
C than or equal to N. If N is 1, X, Y, and E may be scalar
C variables, or expressions.
C--
C  1-Mar-1991 - new routine [JM].
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      INTEGER  I,IDIR
      REAL     TIK,XX,YY
C
      IF (N.LT.1) RETURN
      CALL PGBBUF
C
      IDIR = MOD(DIR-1,4)+1
      TIK = T*XSP*0.15/YSCALE
      DO 10 I=1,N
          CALL GRMOVA(X(I),Y(I))
          IF (IDIR.EQ.1) THEN
            XX = X(I)+E(I)
            YY = Y(I)
          ELSE IF (IDIR.EQ.2) THEN
            XX = X(I)
            YY = Y(I)+E(I)
          ELSE IF (IDIR.EQ.3) THEN
            XX = X(I)-E(I)
            YY = Y(I)
          ELSE IF (IDIR.EQ.4) THEN
            XX = X(I)
            YY = Y(I)-E(I)
          END IF
          CALL GRLINA(XX,YY)
          IF (TIK.NE.0.0) THEN
            IF ((IDIR.EQ.1) .OR. (IDIR.EQ.3)) THEN
              CALL GRMOVA(XX-TIK,YY)
              CALL GRLINA(XX+TIK,YY)
            ELSE IF ((IDIR.EQ.2) .OR. (IDIR.EQ.4)) THEN
              CALL GRMOVA(XX,YY-TIK)
              CALL GRLINA(XX,YY+TIK)
            END IF
          END IF
   10 CONTINUE
      CALL PGEBUF
      END
