C*PGQCH -- inquire character height
C+
      SUBROUTINE PGQCH (SIZE)
      REAL SIZE
C
C Query the Character Size attribute (set by routine PGSCH).
C
C Argument:
C  SIZE   (output) : current character size (dimensionless multiple of
C                    the default size).
C--
C  5-Nov-1985 - new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
C
      IF (PGOPEN.EQ.0) THEN
          SIZE = 1.0
      ELSE
          SIZE = PGCHSZ
      END IF
      END
