C*PGQLW -- inquire line width
C+
      SUBROUTINE PGQLW (LW)
      INTEGER  LW
C
C Query the current Line-Width attribute (set by routine PGSLW).
C
C Argument:
C  LW     (output)  : the line-width (in range 1-21).
C--
C  5-Nov-1985 - new routine [TJP].
C-----------------------------------------------------------------------
      CALL GRQLW(LW)
      END
