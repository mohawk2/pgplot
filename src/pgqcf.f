C*PGQCF -- inquire character font
C+
      SUBROUTINE PGQCF (IF)
      INTEGER  IF
C
C Query the current Character Font (set by routine PGSCF).
C
C Argument:
C  IF     (output)   : the current font number (in range 1-4).
C--
C  5-Nov-1985 - new routine [TJP].
C-----------------------------------------------------------------------
      CALL GRQFNT(IF)
      END
