C*PGRECT -- draw a rectangle, using fill-area attributes
C+
      SUBROUTINE PGRECT (X1, X2, Y1, Y2)
      REAL X1, X2, Y1, Y2
C
C This routine can be used instead of PGPOLY for the special case of
C drawing a rectangle aligned with the coordinate axes; only two
C vertices need be specified instead of four.  On most devices, it is
C faster to use PGRECT than PGPOLY for drawing rectangles.  The
C rectangle has vertices at (X1,Y1), (X1,Y2), (X2,Y2), and (X1,Y2).
C
C Arguments:
C  X1, X2 (input) : the horizontal range of the rectangle.
C  Y1, Y2 (input) : the vertical range of the rectangle.
C--
C 21-Nov-1986 - [TJP].
C 22-Mar-1988 - use GRRECT for fill [TJP].
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
C
      CALL PGBBUF
C
C Outline only.
C
      IF (PGFAS.EQ.2) THEN
          CALL GRMOVA(X1,Y1)
          CALL GRLINA(X1,Y2)
          CALL GRLINA(X2,Y2)
          CALL GRLINA(X2,Y1)
          CALL GRLINA(X1,Y1)
C
C Solid fill.
C
      ELSE
          CALL GRRECT(X1,Y1,X2,Y2)
          CALL GRMOVA(X1,Y1)
      END IF
      CALL PGEBUF
      END
