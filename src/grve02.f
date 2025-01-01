!
C*GRVE02 -- PGPLOT Versatec driver, copy bitmap to output file
C+
      SUBROUTINE GRVE02 (UNIT, BX, BY, BITMAP)
      INTEGER UNIT, BX, BY, BUFSIZ
      BYTE BITMAP(BX,BY)
C
C Arguments:
C  UNIT   (input)  Fortran unit number for output
C  BX, BY (input)  dimensions of BITMAP
C  BITMAP (input)  the bitmap array
C-----------------------------------------------------------------------
      BYTE PREFIX
      DATA PREFIX/ 4/
      INTEGER I, J, K
C
C Write bitmap.
C
      DO J=1,BY
          DO K=BX,2,-1
              IF (BITMAP(K,J).NE.0) GOTO 10
          END DO
   10     WRITE (UNIT=UNIT) PREFIX,(BITMAP(I,J),I=1,K)
      END DO
C
      END
