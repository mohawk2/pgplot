C*PGPOLY -- fill a polygonal area with shading
C+
      SUBROUTINE PGPOLY (N, XPTS, YPTS)
      INTEGER N
      REAL XPTS(*), YPTS(*)
C
C Fill-area primitive routine: shade the interior of a closed
C polygon in the current window.  The action of this routine depends
C on the setting of the Fill-Area Style attribute. If Fill-Area Style
C is SOLID (the default), the interior of the polygon is solid-filled
C using the current Color Index. If Fill-Area Style is HOLLOW, the
C outline of the polygon is drwan using the current line attributes
C (color index, line-style, and line-width). Other values of the Fill-
C Area attribute may be allowed in future, e.g., for shading with
C patterns or hatching. The polygon is clipped at the edge of the
C window. The pen position is changed to (XPTS(1),YPTS(1)) in world
C coordinates (if N > 1).  If the polygon is not convex, a point is
C assumed to lie inside the polygon if a straight line drawn to
C infinity intersects and odd number of the polygon's edges.
C
C Arguments:
C  N      (input)  : number of points defining the polygon; the
C                    line consists of N straight-line segments,
C                    joining points 1 to 2, 2 to 3,... N-1 to N, N to 1.
C                    N should be greater than 2 (if it is 2 or less,
C                    nothing will be drawn).
C  XPTS   (input)  : world x-coordinates of the vertices.
C  YPTS   (input)  : world y-coordinates of the vertices.
C                    Note: the dimension of arrays XPTS and YPTS must be
C                    greater than or equal to N.
C--
C 21-Nov-1983 - [TJP].
C 16-Jul-1984 - revised to shade polygon with GRFA [TJP].
C 21-Oct-1985 - test PGFAS [TJP].
C 27-Nov-1986
C-----------------------------------------------------------------------
      INTEGER  I
      INCLUDE  'pgplot.inc'
C
      IF (N.LT.3) RETURN
      CALL PGBBUF
C
      IF (PGFAS.EQ.2) THEN
          CALL GRMOVA(XPTS(N),YPTS(N))
          DO 10 I=1,N
              CALL GRLINA(XPTS(I),YPTS(I))
   10     CONTINUE
      ELSE
          CALL GRFA(N,XPTS,YPTS)
          CALL GRMOVA(XPTS(1),YPTS(1))
      END IF
      CALL PGEBUF
      END
