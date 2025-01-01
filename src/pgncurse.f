C*PGNCURSE -- non-standard alias for PGNCUR
C+
      SUBROUTINE PGNCURSE (MAXPT, NPT, X, Y, SYMBOL)
      INTEGER MAXPT, NPT
      REAL    X(*), Y(*)
      INTEGER SYMBOL
C--
      CALL PGNCUR (MAXPT, NPT, X, Y, SYMBOL)
      END
