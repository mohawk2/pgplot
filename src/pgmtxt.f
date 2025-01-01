C*PGMTXT -- write text at position relative to viewport
C+
      SUBROUTINE PGMTXT (SIDE, DISP, COORD, FJUST, TEXT)
      CHARACTER*(*) SIDE, TEXT
      REAL DISP, COORD, FJUST
C
C Write text at a position specified relative to the viewport (outside
C or inside).  This routine is useful for annotating graphs. It is used
C by routine PGLAB.  The text is written using the current values of
C attributes color-index, line-width, character-height, and
C character-font.
C
C Arguments:
C  SIDE   (input)  : must include one of the characters 'B', 'L', 'T',
C                    or 'R' signifying the Bottom, Left, Top, or Right
C                    margin of the viewport. If it includes 'LV' or
C                    'RV', the string is written perpendicular to the
C                    frame rather than parallel to it.
C  DISP   (input)  : the displacement of the character string from the
C                    specified edge of the viewport, measured outwards
C                    from the viewport in units of the character
C                    height. Use a negative value to write inside the
C                    viewport, a positive value to write outside.
C  COORD  (input)  : the location of the character string along the
C                    specified edge of the viewport, as a fraction of
C                    the length of the edge.
C  FJUST  (input)  : controls justification of the string parallel to
C                    the specified edge of the viewport. If
C                    FJUST = 0.0, the left-hand end of the string will
C                    be placed at COORD; if JUST = 0.5, the center of
C                    the string will be placed at COORD; if JUST = 1.0,
C                    the right-hand end of the string will be placed at
C                    at COORD. Other values between 0 and 1 give inter-
C                    mediate placing, but they are not very useful.
C  TEXT   (input) :  the text string to be plotted. Trailing spaces are
C                    ignored when justifying the string, but leading
C                    spaces are significant.
C
C--
C 18-Apr-1983
C 15-Aug-1987 - fix BBUF/EBUF error.
C 27-Aug-1987 - fix justification error if XPERIN.ne.YPERIN.
C 05-Sep-1989 - change so that DISP has some effect for 'RV' and 
C               'LV' options [nebk]
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      REAL ANGLE, D, X, Y
      INTEGER L
      CHARACTER*20 TEST
C
      IF (PGOPEN.EQ.0) RETURN
C
      L = LEN(TEXT)
   10 IF (TEXT(L:L).NE.' ') GOTO 20
          L = L-1
          IF (L.EQ.0) RETURN
          GOTO 10
   20 CONTINUE
      D = 0.0
      IF (FJUST.NE.0.0) CALL GRLEN(TEXT(1:L),D)
      D = D*FJUST
      CALL GRTOUP(TEST,SIDE)
      IF (INDEX(TEST,'B').NE.0) THEN
          ANGLE = 0.0
          X = XOFF + COORD*XLEN - D
          Y = YOFF - YSP*DISP
      ELSE IF (INDEX(TEST,'LV').NE.0) THEN
          ANGLE = 0.0
          X = XOFF - YSP*DISP - D
          Y = YOFF + COORD*YLEN - 0.3*YSP
      ELSE IF (INDEX(TEST,'L').NE.0) THEN
          ANGLE = 90.0
          X = XOFF - YSP*DISP
          Y = YOFF + COORD*YLEN - D*(YPERIN/XPERIN)
      ELSE IF (INDEX(TEST,'T').NE.0) THEN
          ANGLE = 0.0
          X = XOFF + COORD*XLEN - D
          Y = YOFF + YLEN + YSP*DISP
      ELSE IF (INDEX(TEST,'RV').NE.0) THEN
          ANGLE = 0.0
          X = XOFF + XLEN + YSP*DISP - D
          Y = YOFF + COORD*YLEN - 0.3*YSP
      ELSE IF (INDEX(TEST,'R').NE.0) THEN
          ANGLE = 90.0
          X = XOFF + XLEN + YSP*DISP
          Y = YOFF + COORD*YLEN - D*(YPERIN/XPERIN)
      ELSE
          CALL GRWARN('Invalid "SIDE" argument in PGMTXT.')
          RETURN
      END IF
      CALL PGBBUF
      CALL GRTEXT(.FALSE.,ANGLE,.TRUE., X, Y, TEXT(1:L))
      CALL PGEBUF
      END
