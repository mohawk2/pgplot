C*PGCURSE -- non-standard alias for PGCURS
C+
      INTEGER FUNCTION PGCURSE (X, Y, CH)
      REAL X, Y
      CHARACTER*1 CH
C--
      INTEGER PGCURS
      PGCURSE = PGCURS (X, Y, CH)
      END
