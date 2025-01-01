C*GRFA -- fill area (polygon)
C+
      SUBROUTINE GRFA (N,PX,PY)
C
C GRPCKG: FILL AREA: fill a polygon with solid color.  The polygon
C is defined by the (x,y) world coordinates of its N vertices.  If
C this is not a function supported by the device, shading is
C accomplished by drawing horizontal lines spaced by 1 pixel.  By
C selecting color index 0, the interior of the polygon can be erased
C on devices which permit it.  The polygon need not be convex, but if
C it is re-entrant (i.e., edges intersect other than at the vertices),
C it may not be obvious which regions are "inside" the polygon.  The
C following rule is applied: for a given point, create a straight line
C starting at the point and going to infinity. If the number of
C intersections between the straight line and the polygon is odd, the
C point is within the polygon; otherwise it is outside. If the
C straight line passes a polygon vertex tangentially, the
C intersection  count is not affected. [There is a bug in this version
C of GRFA which fails to apply this rule correctly for some scan lines
C across a re-entrant polygon.] The only attribute which applies to
C FILL AREA is color index: line-width and line-style are ignored.
C There is a limitation on the complexity of the polygon: GFA will
C fail if any horizontal line intersects more than 32 edges of the
C polygon.
C
C Arguments:
C
C N (input, integer): the number of vertices of the polygon (at least
C       3).
C PX, PY (input, real arrays, dimension at least N): world coordinates
C       of the N vertices of the polygon.
C--
C 16-Jul-1984 - [TJP].
C  5-Aug-1986 - add GREXEC support [AFT].
C 21-Feb-1987 - If needed, calls begin picture [AFT].
C  8-Nov-2010 - Modified loop with real variables to conform to gfortran. [KS]
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER MAXSEC
      PARAMETER (MAXSEC=32)
      INTEGER  I, J, N, NSECT, LW, LS
      INTEGER INDEX, ICOUNT
      REAL    RBUF(6)
      INTEGER NBUF,LCHR,IHAREA
      CHARACTER*32 CHR
      REAL     X(MAXSEC), PX(*), PY(*), Y, YMIN, YMAX, DY
      REAL     TEMP, S1,S2, T1,T2
C
      IF (GRCIDE.LT.1) RETURN
      IF (N.LT.3) THEN
          CALL GRWARN('GRFA - polygon has < 3 vertices.')
          RETURN
      END IF
C
C Inquire if hardware polygon fill exists.
C
      IHAREA=0
      CALL GREXEC(GRGTYP, 4,RBUF,NBUF,CHR,LCHR)
      IHAREA=0
      IF(CHR(4:4).EQ.'A') IHAREA=1
C
      IF(IHAREA.NE.0) THEN
          IF (.NOT.GRPLTD(GRCIDE)) CALL GRBPIC
          RBUF(1) = N
          CALL GREXEC(GRGTYP,20,RBUF,NBUF,CHR,LCHR)
          DO 10 I=1,N
              RBUF(1) = PX(I)*GRXSCL(GRCIDE) + GRXORG(GRCIDE)
              RBUF(2) = PY(I)*GRYSCL(GRCIDE) + GRYORG(GRCIDE)
              CALL GREXEC(GRGTYP,20,RBUF,NBUF,CHR,LCHR)
   10     CONTINUE
          RETURN
      END IF
C
C For other devices fill area is simulated.
C
C Save attributes.
C
      CALL GRQLS(LS)
      CALL GRQLW(LW)
      CALL GRSLS(1)
      CALL GRSLW(1)
C
C Find range of raster-lines to be shaded.
C
      YMIN = PY(1)
      YMAX = YMIN
      DO 20 I=2,N
          TEMP = PY(I)
          YMIN = MIN(YMIN,TEMP)
          YMAX = MAX(YMAX,TEMP)
   20 CONTINUE
      YMIN = YMIN*GRYSCL(GRCIDE) + GRYORG(GRCIDE)
      YMAX = YMAX*GRYSCL(GRCIDE) + GRYORG(GRCIDE)
      CALL GREXEC(GRGTYP, 3,RBUF,NBUF,CHR,LCHR)
      DY=RBUF(3)
C
C Find intersections of edges with current raster line. (This code used to be
C quite simply DO 40 Y=YMIN,YMAX,DY, but GFORTRAN complains about this now -
C loop limits and index must all be integer, so I've replaced it with the
C following, which ought to be equivalent.)
C
      ICOUNT = NINT((YMAX - YMIN)/DY) + 1
      DO 40 INDEX = 1,ICOUNT
          Y = YMIN + (INDEX - 1)*DY
          NSECT = 0
          DO 30 I=2,N+1
              IF (I.GT.N) THEN
                    S1 = PX(N)*GRXSCL(GRCIDE) + GRXORG(GRCIDE)
                    S2 = PX(1)*GRXSCL(GRCIDE) + GRXORG(GRCIDE)
                    T1 = PY(N)*GRYSCL(GRCIDE) + GRYORG(GRCIDE)
                    T2 = PY(1)*GRYSCL(GRCIDE) + GRYORG(GRCIDE)
              ELSE
                    S1 = PX(I-1)*GRXSCL(GRCIDE) + GRXORG(GRCIDE)
                    S2 = PX(I)*GRXSCL(GRCIDE) + GRXORG(GRCIDE)
                    T1 = PY(I-1)*GRYSCL(GRCIDE) + GRYORG(GRCIDE)
                    T2 = PY(I)*GRYSCL(GRCIDE) + GRYORG(GRCIDE)
              END IF
              IF ((T1.LT.Y .AND. Y.LE.T2).OR.
     1            (T1.GT.Y .AND. Y.GE.T2)) THEN
                    NSECT = NSECT+1
                    IF (NSECT.GT.MAXSEC) THEN
                        CALL GRWARN('GRFA - polygon is too complex.')
                        RETURN
                    END IF
                    X(NSECT) = S1 + (S2-S1)*(Y-T1)/(T2-T1)
              END IF
   30     CONTINUE
C
C Sort the intersections into increasing x order.
C
          DO 34 I=2,NSECT
              DO 32 J=1,I
                IF (X(J).GT.X(I)) THEN
                    TEMP = X(J)
                    X(J) = X(I)
                    X(I) = TEMP
                END IF
   32         CONTINUE
   34     CONTINUE
C
C Draw the horizontal line-segments.
C
          DO 36 I=1,NSECT-1,2
              GRXPRE(GRCIDE) = X(I)
              GRYPRE(GRCIDE) = Y
              CALL GRLIN0(X(I+1),Y)
   36     CONTINUE
   40 CONTINUE
C
C Restore attributes.
C
      CALL GRSLS(LS)
      CALL GRSLW(LW)
      END
