C*PGRND -- find the smallest "round" number greater than x
C+
      REAL FUNCTION PGRND (X, NSUB)
      REAL X
      INTEGER NSUB
C
C Rooutine to find the smallest "round" number larger than x, a
C "round" number being 1, 2 or 5 times a power of 10. If X is negative,
C PGRND(X) = -PGRND(ABS(X)). eg PGRND(8.7) = 10.0,
C PGRND(-0.4) = -0.5.  If X is zero, the value returned is zero.
CThis routine is used by PGBOX for choosing  tick intervals.
C
C Returns:
C  PGRND         : the "round" number.
C Arguments:
C  X      (input)  : the number to be rounded.
C  NSUB   (output) : a suitable number of subdivisions for
C                    subdividing the "nice" number: 2 or 5.
C--
C  6-Sep-1989 - Changes for standard Fortran-77 [TJP].
C-----------------------------------------------------------------------
      INTEGER  I,ILOG
      REAL     FRAC,NICE(3),PWR,XLOG,XX
      INTRINSIC ABS, LOG10, SIGN
      DATA     NICE/2.0,5.0,10.0/
C
      IF (X.EQ.0.0) THEN
          PGRND = 0.0
          NSUB = 2
          RETURN
      END IF
      XX   = ABS(X)
      XLOG = LOG10(XX)
      ILOG = XLOG
      IF (XLOG.LT.0) ILOG=ILOG-1
      PWR  = 10.0**ILOG
      FRAC = XX/PWR
      DO 10 I=1,3
          IF (FRAC.LE.NICE(I)) GOTO 20
   10 CONTINUE
   20 PGRND = SIGN(PWR*NICE(I),X)
      NSUB = 5
      IF (NICE(I).EQ.2.0) NSUB = 2
      END
