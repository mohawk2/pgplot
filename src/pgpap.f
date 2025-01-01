C*PGPAP -- change the size of the view surface ("paper size")
C+
      SUBROUTINE PGPAP (WIDTH, ASPECT)
      REAL WIDTH, ASPECT
C
C This routine changes the size of the view surface to a specified
C width and aspect ratio (height/width), in so far as this is possible
C on the specific device. It is always possible to obtain a view
C surface smaller than the default size; on some devices (e.g.,
C printers that print on roll or fan-feed paper) it is possible
C to obtain a view surface larger than the default. If this routine is
C used, it must be called immediately after PGBEGIN.
C
C Arguments:
C  WIDTH  (input)  : the requested width of the view surface in inches;
C                    if WIDTH=0.0, PGPAP will obtain the largest view
C                    surface available consistent with argument ASPECT.
C  ASPECT (input)  : the aspect ratio (height/width) of the view
C                    surface; e.g., ASPECT=1.0 gives a square view
C                    surface, ASPECT=0.618 gives a horizontal
C                    rectangle, ASPECT=1.618 gives a vertical rectangle.
C--
C (22-Apr-1983; bug fixed 7-Jun-1988)
C  6-Oct-1990 Modified to work correctly on interactive devices.
C 13-Dec-1990 Make errors non-fatal [TJP].
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      REAL     HDEF, HMAX, HREQ, WDEF, WMAX, WREQ
      REAL     XSMAX, YSMAX
C
      IF (PGOPEN.EQ.0)  RETURN
      IF (WIDTH.LT.0.0 .OR. ASPECT.LE.0.0) THEN
          CALL GRWARN('PGPAP ignored: invalid arguments')
          RETURN
      END IF
C     -- Find default size WDEF, HDEF and maximum size WMAX, HMAX
C        of view surface (inches)
      CALL GRSIZE(IDENT,XSZ,YSZ,XSMAX,YSMAX,XPERIN,YPERIN)
      WDEF = XSZ/XPERIN
      HDEF = YSZ/YPERIN
      WMAX = XSMAX/XPERIN
      HMAX = YSMAX/YPERIN
C     -- Find desired size WREQ, HREQ of view surface (inches)
      IF (WIDTH.NE.0.0) THEN
          WREQ = WIDTH
          HREQ = WIDTH*ASPECT
      ELSE
          WREQ = WDEF
          HREQ = WDEF*ASPECT
          IF (HREQ.GT.HDEF) THEN
              WREQ = HDEF/ASPECT
              HREQ = HDEF
          END IF
      END IF
C     -- Scale the requested view surface to fit the maximum
C        dimensions
      IF (WMAX.GT.0.0 .AND. WREQ.GT.WMAX) THEN
          WREQ = WMAX
          HREQ = WMAX*ASPECT
      END IF
      IF (HMAX.GT.0.0 .AND. HREQ.GT.HMAX) THEN
          WREQ = HMAX/ASPECT
          HREQ = HMAX
      END IF
C     -- Establish the new view surface dimensions
      XSZ = WREQ*XPERIN
      YSZ = HREQ*YPERIN
      CALL GRSETS(IDENT,XSZ,YSZ)
      XSZ = XSZ/NX
      YSZ = YSZ/NY
      NXC = NX
      NYC = NY
      CALL PGSCH(1.0)
      CALL PGVSTD
      END
