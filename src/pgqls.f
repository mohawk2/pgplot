C*PGQLS -- inquire line style
C+
      SUBROUTINE PGQLS (LS)
      INTEGER  LS
C
C Query the current Line Style attribute (set by routine PGSLS).
C
C Argument:
C  LS     (output) : the current line-style attribute (in range 1-5).
C--
C  5-Nov-1985 - new routine [TJP].
C-----------------------------------------------------------------------
      CALL GRQLS(LS)
      END
