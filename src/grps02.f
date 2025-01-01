
C*GRPS02 -- PGPLOT PostScript driver, copy buffer to file
C+
      SUBROUTINE GRPS02 (UNIT, S)
C
C Support routine for PSdriver: write character string S on
C specified Fortran unit.
C-----------------------------------------------------------------------
      INTEGER UNIT
      CHARACTER*(*) S
C
      WRITE (UNIT, '(A)') S
C-----------------------------------------------------------------------
      END
