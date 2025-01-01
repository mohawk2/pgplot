C*PGEBUF -- end batch of output (buffer)
C+
      SUBROUTINE PGEBUF
C
C A call to PGEBUF marks the end of a batch of graphical output begun
C with the last call of PGBBUF.  PGBBUF and PGEBUF calls should always
C be paired. Each call to PGBBUF increments a counter, while each call
C to PGEBUF decrements the counter. When the counter reaches 0, the
C batch of output is written on the output device.
C
C Arguments: none
C--
C 21-Nov-1985 - new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
C
      IF (PGOPEN.NE.0) THEN
          PGBLEV = MAX(0, PGBLEV - 1)
          IF (PGBLEV.EQ.0) CALL GRTERM
      END IF
      END
