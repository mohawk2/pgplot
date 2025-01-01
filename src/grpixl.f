
C*GRPIXL -- solid-fill multiple rectangular areas
C+
      SUBROUTINE GRPIXL (IA, IDIM, JDIM, I1, I2, J1, J2, 
     1                   X1, X2, Y1, Y2)
      INTEGER IDIM, JDIM, I1, I2, J1, J2
      INTEGER IA(IDIM,JDIM)
      REAL    X1, X2, Y1, Y2
C
C Determine the size of each rectangular element. If it is equal
C to the device pen width and the device supports pixel primitives,
C use pixel primitives. Otherwise, if the size is smaller than the
C device pen width emulate pixel output by plotting points. If the
C size is larger than the device pen width, emulate by outputting
C solid-filled rectangles.
C
C Arguments:
C  IA     (input)  : the array to be plotted.
C  IDIM   (input)  : the first dimension of array A.
C  JDIM   (input)  : the second dimension of array A.
C  I1, I2 (input)  : the inclusive range of the first index
C                    (I) to be plotted.
C  J1, J2 (input)  : the inclusive range of the second
C                    index (J) to be plotted.
C  X1, Y1 (input)  : world coordinates of one corner of the output
C                    region
C  X2, Y2 (input)  : world coordinates of the opposite corner of the
C                    output region
C--
C 18-Jan-1991 - [GvG]
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      REAL    RBUF(3)
      INTEGER NBUF, LCHR
      CHARACTER*32 CHR
      REAL    XLL, YLL, XUR, YUR
      REAL    XMIN, YMIN, XMAX, YMAX
      REAL    WIDTH, XSIZE, YSIZE
      INTEGER IL, IR, JB, JT
C
      IF (GRCIDE.LT.1) RETURN
C
C Convert to device coordinates
C
      CALL GRTXY0(.FALSE., X1, Y1, XLL, YLL)
      CALL GRTXY0(.FALSE., X2, Y2, XUR, YUR)
      XMIN = MIN(XLL,XUR)
      XMAX = MAX(XLL,XUR)
      YMIN = MIN(YLL,YUR)
      YMAX = MAX(YLL,YUR)
C
C Check if completely outside clipping region
C
      IF (XMAX .LT. GRXMIN(GRCIDE) .OR. GRXMAX(GRCIDE) .LT. XMIN .OR.
     1    YMAX .LT. GRYMIN(GRCIDE) .OR. GRYMAX(GRCIDE) .LT. YMIN)
     2   RETURN
C
C Don't paint "pixels" completely before left clipping boundary
C
      IF (XMIN .LT. GRXMIN(GRCIDE)) THEN
         IL = I1 + (GRXMIN(GRCIDE) - XMIN) * (I2 - I1 + 1) /
     1             (XMAX - XMIN)
         XMIN = XMIN + ((XMAX - XMIN) * (IL - I1)) / (I2 - I1 + 1)
      ELSE
         IL = I1
      ENDIF
C
C Don't paint "pixels" completely after right clipping boundary
C
      IF (GRXMAX(GRCIDE) .LT. XMAX) THEN
         IR = I2 - (XMAX - GRXMAX(GRCIDE)) * (I2 - I1 + 1) /
     1             (XMAX - XMIN) + 1
         XMAX = XMIN + ((XMAX - XMIN) * (IR - I1 + 1)) /
     1                 (I2 - I1 + 1)
      ELSE
         IR = I2
      ENDIF
C
C Don't paint "pixels" completely under bottop clipping boundary
C
      IF (YMIN .LT. GRYMIN(GRCIDE)) THEN
         JB = J1 + (GRYMIN(GRCIDE) - YMIN) * (J2 - J1 + 1) /
     1             (YMAX - YMIN)
         YMIN = YMIN + ((YMAX - YMIN) * (JB - J1)) / (J2 - J1 + 1)
      ELSE
         JB = J1
      ENDIF
C
C Don't paint "pixels" completely above top clipping boundary
C
      IF (GRYMAX(GRCIDE) .LT. YMAX) THEN
         JT = J2 - (YMAX - GRYMAX(GRCIDE)) * (J2 - J1 + 1) /
     1             (YMAX - YMIN) + 1
         YMAX = YMIN + ((YMAX - YMIN) * (JT - J1 + 1)) /
     1                 (J2 - J1 + 1)
      ELSE
         JT = J2
      ENDIF
C
C Check against pen width
C
      CALL GREXEC(GRGTYP, 3, RBUF, NBUF, CHR, LCHR)
      WIDTH = RBUF(3)
      XSIZE = (I2 - I1 + 1) * WIDTH
      YSIZE = (J2 - J1 + 1) * WIDTH
C
C Use rectangles if "pixel" is too large
C
      IF (XMAX - XMIN .GT. XSIZE + 0.5 * WIDTH .OR.
     1    YMAX - YMIN .GT. YSIZE + 0.5 * WIDTH) THEN
         CALL GRPXRE(IA, IDIM, JDIM, IL, IR, JB, JT, XMIN, XMAX,
     1               YMIN, YMAX)
C
C Use either pixel primitives or points
C
      ELSE
C
C Clip pixels lying more than 50% outside clipping boundaries
C
         IF (XMIN .LT. GRXMIN(GRCIDE) - 0.5 * WIDTH) THEN
            XMIN = XMIN + (XMAX - XMIN) / (IR - IL + 1)
            IL = IL + 1
         ENDIF
         IF (GRXMAX(GRCIDE) + 0.5 * WIDTH .LT. XMAX) THEN
            XMAX = XMAX - (XMAX - XMIN) / (IR - IL + 1)
            IR = IR - 1
         ENDIF
         IF (YMIN .LT. GRYMIN(GRCIDE) - 0.5 * WIDTH) THEN
            YMIN = YMIN + (YMAX - YMIN) / (JT - JB + 1)
            JB = JB + 1
         ENDIF
         IF (GRYMAX(GRCIDE) + 0.5 * WIDTH .LT. YMAX) THEN
            YMAX = YMAX - (YMAX - YMIN) / (JT - JB + 1)
            JT = JT - 1
         ENDIF
C
C Recalculate size
C
         XSIZE = (IR - IL + 1) * WIDTH
         YSIZE = (JT - JB + 1) * WIDTH
C
C Determine device capabilities.
C
         CALL GREXEC(GRGTYP, 4, RBUF, NBUF, CHR, LCHR)
C
C Use pixel primitives if available and possible
C
         IF (CHR(7:7) .EQ. 'P' .AND. 
     1       XSIZE - 0.5 * WIDTH .LE. XMAX - XMIN .AND.
     2       YSIZE - 0.5 * WIDTH .LE. YMAX - YMIN) THEN
            CALL GRPXPX(IA, IDIM, JDIM, IL, IR, JB, JT, XMIN, YMIN)
C
C Otherwise, use points
C
         ELSE
            CALL GRPXPO(IA, IDIM, JDIM, IL, IR, JB, JT, XMIN, XMAX,
     1                  YMIN, YMAX)
         ENDIF
      ENDIF
      END
