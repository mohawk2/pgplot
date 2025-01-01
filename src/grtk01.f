C*GRTK01 -- PGPLOT Tektronix 4100 driver, encode integer
C+
      SUBROUTINE GRTK01(I, C, NC)
      INTEGER I
      CHARACTER*(*) C
      INTEGER NC
C
C Encode integer in host syntax. Input integer I; output encoded string
C C, containing NC characters (1, 2, or 3).  This version encodes
C integers up to 1023, which fit in two characters.
C-----------------------------------------------------------------------
      INTEGER J
C
      J = IABS(I)
      IF (J.LT.16) THEN
          IF (I.LT.0) THEN
              C(1:1) = CHAR(J+32)
          ELSE
              C(1:1) = CHAR(J+48)
          END IF
          NC = 1
      ELSE
          C(1:1) = CHAR(J/16+64)
          IF (I.LT.0) THEN
              C(2:2) = CHAR(MOD(J,16)+32)
          ELSE
              C(2:2) = CHAR(MOD(J,16)+48)
          END IF
          NC = 2
      END IF
C
      END
