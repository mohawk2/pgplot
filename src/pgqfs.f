C*PGQFS -- inquire fill-area style
C+
      SUBROUTINE PGQFS (FS)
      INTEGER  FS
C
C Query the current Fill-Area Style attribute (set by routine
C PGSFS).
C
C Argument:
C  FS     (output) : the current fill-area style:
C                      FS = 1 => solid (default)
C                      FS = 2 => hollow
C--
C  5-Nov-1985 - new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
C
      IF (PGOPEN.EQ.0) THEN
          FS = 1
      ELSE
          FS = PGFAS
      END IF
      END
