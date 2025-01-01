C*GRPX02 -- PGPLOT Printronix driver, copy bitmap to output file
C+
      SUBROUTINE GRPX02 (UNIT, BX, BY, BITMAP)
      INTEGER UNIT, BX, BY, BUFSIZ
      BYTE BITMAP(BX,BY)
C
C Arguments:
C  UNIT   (input)  Fortran unit number for output
C  BX, BY (input)  dimensions of BITMAP
C  BITMAP (input)  the bitmap array
C-----------------------------------------------------------------------
      BYTE SUFFIX(3)
      DATA SUFFIX/ 5, 13, 10/
      INTEGER I, J
C
C Write bitmap.
C
      DO J=1,BY
C
C Until CONVEX line printer driver is modified, throw away the 132nd column
C
          DO I=1,MIN(BX,131)
              CALL FPUTC(UNIT, CHAR(BITMAP(I,J)))
          END DO
          CALL FPUTC(UNIT, CHAR(5))
          CALL FPUTC(UNIT, CHAR(10))
      END DO
C
C Write blank plot lines to fill up page
C
      END
