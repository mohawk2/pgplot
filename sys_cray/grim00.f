
C*GRIM00 -- PGPLOT Impress driver, write word
C+
      SUBROUTINE GRIM00(BUF,WORD)
      INTEGER BUF(2), WORD(2)
C-- UNIX
      BUF(1) = WORD(1)
      BUF(2) = WORD(2)
C-- VMS
C      BUF(1) = WORD(2)
C      BUF(2) = WORD(1)
      END
