C*PGSFS -- set fill-area style
C+
      SUBROUTINE PGSFS (FS)
      INTEGER  FS
C
C Set the Fill-Area Style attribute for subsequent area-fill by
C PGPOLY.  At present only two styles are available: solid (fill
C polygon with solid color of the current color-index), and hollow
C (draw outline of polygon only, using current line attributes).
C
C Argument:
C  FS     (input)  : the fill-area style to be used for subsequent
C                    plotting:
C                      FS = 1 => solid (default)
C                      FS = 2 => hollow
C                    Other values give an error message and are
C                    treated as 2.
C--
C 21-Oct-1985 - new routine [TJP].
C 17-Dec-1990 - pass to GR level [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
C
      IF (PGOPEN.EQ.0) RETURN
      IF (FS.LT.1 .OR. FS.GT.2) THEN
          CALL GRWARN('illegal fill-area style requested')
          PGFAS = 2
      ELSE
          PGFAS = FS
      END IF
      CALL GRSFS(PGFAS)
      END
