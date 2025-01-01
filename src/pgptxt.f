C*PGPTXT -- write text at arbitrary position and angle
C+
      SUBROUTINE PGPTXT (X, Y, ANGLE, FJUST, TEXT)
      REAL X, Y, ANGLE, FJUST
      CHARACTER*(*) TEXT
C
C Primitive routine for drawing text. The text may be drawn at any
C angle with the horizontal, and may be centered or left- or right-
C justified at a specified position.  Routine PGTEXT provides a
C simple interface to PGPTXT for horizontal strings. Text is drawn
C using the current values of attributes color-index, line-width,
C character-height, and character-font.  Text is NOT subject to
C clipping at the edge of the window.
C
C Arguments:
C  X      (input)  : world x-coordinate.
C  Y      (input)  : world y-coordinate. The string is drawn with the
C                    baseline of all the characters passing through
C                    point (X,Y); the positioning of the string along
C                    this line is controlled by argument FJUST.
C  ANGLE  (input)  : angle, in degrees, that the baseline is to make
C                    with the horizontal, increasing counter-clockwise
C                    (0.0 is horizontal).
C  FJUST  (input)  : controls horizontal justification of the string.
C                    If FJUST = 0.0, the string will be left-justified
C                    at the point (X,Y); if FJUST = 0.5, it will be
C                    centered, and if FJUST = 1.0, it will be right
C                    justified. [Other values of FJUST give other
C                    justifications.]
C  TEXT   (input)  : the character string to be plotted.
C--
C (2-May-1983)
C 31-Jan-1985 - convert to Fortran-77 standard...
C 13-Feb-1988 - correct a PGBBUF/PGEBUF mismatch if string is blank.
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      INTEGER L
      REAL D, XP, YP
C
      IF (PGOPEN.EQ.0) RETURN
      CALL PGBBUF
C
      L = LEN(TEXT)
   10 IF (TEXT(L:L).NE.' ') GOTO 20
          L = L-1
          IF (L.LE.0) GOTO 30
          GOTO 10
   20 CONTINUE
      D = 0.0
      IF (FJUST.NE.0.0) CALL GRLEN(TEXT(1:L),D)
      XP = XORG + X*XSCALE - D*FJUST*COS(ANGLE/57.29578)
      YP = YORG + Y*YSCALE - D*FJUST*SIN(ANGLE/57.29578)
      CALL GRTEXT(.TRUE. ,ANGLE, .TRUE., XP, YP, TEXT(1:L))
   30 CALL PGEBUF
      END
