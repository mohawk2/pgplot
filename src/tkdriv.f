C*TKDRIV -- PGPLOT Tektronix 4100 driver
C+
      SUBROUTINE TKDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C
C PGPLOT driver for Tektronix 4100 series
C
C Version 1.0 - 1988 Dec 17 - T. J. Pearson.
C Version 1.1 - 1991 Jun 28 - standardization (TJP).
C
C Commands sent to device:
C ESC, '%!0'
C       Select code: syntax (integer) [TEK]
C ESC, '%!1'
C       Select code: syntax (integer) [ANSI]
C ESC, 'RU1;6'
C       Begin pixel operations: surface 1; replace mode; 6 bit/pixel
C ESC, 'LL2'
C       Set dialog area lines = 2.
C ESC, 'RS', C1, C2
C       Set pixel viewport: lower left, upper right.
C ESC, 'TM111'
C       Set color mode: RGB, opaque, normal color
C ESC, 'RR', C1, C2, '0'
C       Rectangle fill: lower left (xycoord), upper right (xy coord),
C       fill index (integer)
C ESC, 'ML', C3(:NC3)
C       Set line index: line-index (integer)
C ESC, 'LF', C1
C       Move: position (xy coord)
C ESC, 'LG', C1
C       Draw: position (xy coord)
C ESC, 'TG14', C1(:NC1), C2(:NC2),
C       Set surface color map: surface number, color mixtures
C-----------------------------------------------------------------------
      CHARACTER*(*) TYPE
      PARAMETER (TYPE='TK4100')
      INTEGER NX, NY, NCOL
      PARAMETER (NX=639, NY=479, NCOL=15)
C
      INTEGER  UNIT, IER, I
      LOGICAL  APPEND
      CHARACTER*5 C1, C2, C3, C4
      INTEGER  NC1, NC2, NC3, NC4
      INTEGER  CI, I0, J0, I1, J1, IR, IG, IB, LASTI, LASTJ
      INTEGER CTABLE(3,0:15)
C
      SAVE CI, LASTI, LASTJ
C
      DATA CTABLE /000,000,000, 100,100,100, 100,000,000, 000,100,000,
     1             000,000,100, 000,100,100, 100,000,100, 100,100,000,
     2             100, 50,000,  50,100,000, 000,100, 50, 000, 50,100,
     3              50,000,100, 100,000, 50,  33, 33, 33,  67, 67, 67/
C-----------------------------------------------------------------------
C
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,110,120,
     &     130,140,150,160,170,180,190,200,210,220,230,240), IFUNC
  900 CALL GRWARN('Unimplemented function in TK device driver.')
      NBUF = -1
      RETURN
C
C--- IFUNC = 1, Return device name.-------------------------------------
C
   10 CHR = TYPE
      LCHR = LEN(TYPE)
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices.---------------------------------------
C
   20 RBUF(1) = 0
      RBUF(2) = NX
      RBUF(3) = 0
      RBUF(4) = NY
      RBUF(5) = 0
      RBUF(6) = NCOL
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution. ------------------------------
C
   30 RBUF(1) = 100.0
      RBUF(2) = 100.0
      RBUF(3) = 1
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info. -------------------------------
C    (This device is Interactive, No cursor, No dashed lines, No area
C    fill, No thick lines, Rectangle fill)
C
   40 CHR = 'INNNNRNNNN'
      LCHR = 10
      RETURN
C
C--- IFUNC = 5, Return default file name. ------------------------------
C
   50 CALL GRTRML(CHR, LCHR)
      RETURN
C
C--- IFUNC = 6, Return default physical size of plot. ------------------
C
   60 RBUF(1) = 0
      RBUF(2) = NX
      RBUF(3) = 0
      RBUF(4) = NY
      NBUF = 4
      RETURN
C
C--- IFUNC = 7, Return misc defaults. ----------------------------------
C
   70 RBUF(1) = 1
      NBUF = 1
      RETURN
C
C--- IFUNC = 8, Select plot. -------------------------------------------
C
   80 CONTINUE
      RETURN
C
C--- IFUNC = 9, Open workstation. --------------------------------------
C
   90 CONTINUE
      APPEND = RBUF(3).NE.0.0
      CALL GRGLUN(UNIT)
      RBUF(1) = UNIT
      OPEN (UNIT=UNIT, STATUS='UNKNOWN', FILE=CHR(1:LCHR), IOSTAT=IER)
      IF (IER.NE.0) THEN
          CALL GRWARN('Cannot open output file for TK plot')
          RBUF(2) = 0
          CALL GRFLUN(UNIT)
      ELSE
          INQUIRE (UNIT=UNIT, NAME=CHR)
          LCHR = LEN(CHR)
   91     IF (CHR(LCHR:LCHR).EQ.' ') THEN
              LCHR = LCHR-1
              GOTO 91
          END IF
          RBUF(2) = 1
      END IF
      RETURN
C
C--- IFUNC=10, Close workstation. --------------------------------------
C
  100 CONTINUE
C     -- close file
      CLOSE (UNIT)
      CALL GRFLUN(UNIT)
      RETURN
C
C--- IFUNC=11, Begin picture. ------------------------------------------
C
  110 CONTINUE
C     -- put device in graphics mode
      WRITE (UNIT, '(2A)') CHAR(27), '%!0'
      WRITE (UNIT, '(2A)') CHAR(27), 'RU1;6'
      WRITE (UNIT, '(2A)') CHAR(27), 'LL2'
C     -- set up viewport size
      CALL GRTK00(0, 0, C1)
      CALL GRTK00(639, 479, C2)
      WRITE (UNIT, '(4A)') CHAR(27), 'RS', C1, C2
C     -- set color mode to RGB
      WRITE (UNIT, '(2A)') CHAR(27), 'TM111'
C     -- set default color representation
      DO 111 I=0,15
          CALL GRTK01(I,           C1, NC1)
          CALL GRTK01(CTABLE(1,I), C2, NC2)
          CALL GRTK01(CTABLE(2,I), C3, NC3)
          CALL GRTK01(CTABLE(3,I), C4, NC4)
          WRITE (UNIT, '(6A)') CHAR(27), 'TG14', C1(:NC1), C2(:NC2),
     1                     C3(:NC3), C4(:NC4)
  111 CONTINUE
C     -- erase screen
      CALL GRTK00(0, 0, C1)
      CALL GRTK00(639, 479, C2)
      WRITE (UNIT, '(5A)') CHAR(27), 'RR', C1, C2, '0'
C     -- set color index 1
      WRITE (UNIT, '(3A)') CHAR(27), 'ML', '1'
      RETURN
C
C--- IFUNC=12, Draw line. ----------------------------------------------
C
  120 CONTINUE
      I0 = IFIX(RBUF(1)*4095.0/639.0 + 0.5)
      J0 = IFIX(RBUF(2)*4095.0/639.0 + 0.5)
      I1 = IFIX(RBUF(3)*4095.0/639.0 + 0.5)
      J1 = IFIX(RBUF(4)*4095.0/639.0 + 0.5)
      IF (I0.NE.LASTI .OR. J0.NE.LASTJ) THEN
C         -- move to (I0,J0)
          CALL GRTK00(I0, J0, C1)
          WRITE (UNIT, '(3A)') CHAR(27), 'LF', C1
      END IF
C     -- draw to (I1,J1)
      CALL GRTK00(I1, J1, C1)
      WRITE (UNIT, '(3A)') CHAR(27), 'LG', C1
C     -- save current point
      LASTI = I1
      LASTJ = J1
      RETURN
C
C--- IFUNC=13, Draw dot. -----------------------------------------------
C
  130 CONTINUE
      I0 = IFIX(RBUF(1)*4095.0/639.0 + 0.5)
      J0 = IFIX(RBUF(2)*4095.0/639.0 + 0.5)
      I1 = I0
      J1 = J0
      IF (I0.NE.LASTI .OR. J0.NE.LASTJ) THEN
C         -- move to (I0,J0)
          CALL GRTK00(I0, J0, C1)
          WRITE (UNIT, '(3A)') CHAR(27), 'LF', C1
      END IF
C     -- draw to (I1,J1)
      CALL GRTK00(I1, J1, C1)
      WRITE (UNIT, '(3A)') CHAR(27), 'LG', C1
C     -- save current point
      LASTI = I1
      LASTJ = J1
      RETURN
C
C--- IFUNC=14, End picture. --------------------------------------------
C
  140 CONTINUE
C     -- return to text mode
      WRITE (UNIT, '(2A)') CHAR(27), '%!1'
      RETURN
C
C--- IFUNC=15, Select color index. -------------------------------------
C
  150 CONTINUE
      CI = NINT(RBUF(1))
      IF (CI.GT.NCOL .OR. CI.LT.0) CI = 1
      CALL GRTK01(CI, C1, NC1)
      WRITE (UNIT, '(3A)') CHAR(27), 'ML', C1(:NC1)
      RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C
  160 CONTINUE
      RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
C           RBUF(1)   in/out : cursor x coordinate.
C           RBUF(2)   in/out : cursor y coordinate.
C           CHR(1:1)  output : keystroke.
C
  170 CONTINUE
      RETURN
C
C--- IFUNC=18, Erase alpha screen. -------------------------------------
C
  180 CONTINUE
      RETURN
C
C--- IFUNC=19, Set line style. -----------------------------------------
C    (Not implemented: should not be called.)
C
  190 GOTO 900
C
C--- IFUNC=20, Polygon fill. -------------------------------------------
C    (Not implemented: should not be called.)
C
  200 GOTO 900
C
C--- IFUNC=21, Set color representation. -------------------------------
C
  210 CONTINUE
      CI = RBUF(1)
      IR = IFIX(RBUF(2)*100.0)
      IG = IFIX(RBUF(3)*100.0)
      IB = IFIX(RBUF(4)*100.0)
      CALL GRTK01(CI, C1, NC1)
      CALL GRTK01(IR, C2, NC2)
      CALL GRTK01(IG, C3, NC3)
      CALL GRTK01(IB, C4, NC4)
      WRITE (UNIT, '(6A)') CHAR(27), 'TG14', C1(:NC1), C2(:NC2),
     1                     C3(:NC3), C4(:NC4)
      RETURN
C
C--- IFUNC=22, Set line width. -----------------------------------------
C    (Not implemented: should not be called.)
C
  220 GOTO 900
C
C--- IFUNC=23, Escape. -------------------------------------------------
C
  230 CONTINUE
      RETURN
C
C--- IFUNC=24, Rectangle fill. -----------------------------------------
C
  240 CONTINUE
      I0 = NINT(RBUF(1))
      J0 = NINT(RBUF(2))
      I1 = NINT(RBUF(3))
      J1 = NINT(RBUF(4))
      CALL GRTK00(I0, J0, C1)
      CALL GRTK00(I1, J1, C2)
      CALL GRTK01(CI, C3, NC3)
      WRITE (UNIT, '(5A)') CHAR(27), 'RR', C1, C2, C3(:NC3)
      RETURN
C-----------------------------------------------------------------------
      END
