C*PGSWIN -- set window
C+
      SUBROUTINE PGSWIN (X1, X2, Y1, Y2)
      REAL X1, X2, Y1, Y2
C
C Change the window in world coordinate space that is to be mapped on
C to the viewport.  Usually PGSWIN is called automatically by PGENV,
C but it may be called directly by the user.
C
C Arguments:
C  X1     (input)  : the x-coordinate of the bottom left corner
C                    of the viewport.
C  X2     (input)  : the x-coordinate of the top right corner
C                    of the viewport (note X2 may be less than X1).
C  Y1     (input)  : the y-coordinate of the bottom left corner
C                    of the viewport.
C  Y2     (input)  : the y-coordinate of the top right corner
C                    of the viewport (note Y2 may be less than Y1).
C--
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
C
      IF (PGOPEN.EQ.0) RETURN
C
      XBLC = X1
      XTRC = X2
      YBLC = Y1
      YTRC = Y2
      CALL PGVW
      END
