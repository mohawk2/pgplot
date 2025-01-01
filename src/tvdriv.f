C*TVDRIV -- PGPLOT Imagraph Driver
C+
      SUBROUTINE TVDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C
C PGPLOT driver for Imagraph image display: AGC SERIES VME-1280-10 board
C
C IMAgraph Corporation
C 800 West Cummings Park
C Woburn, Massachusetts 01801
C
C Utilizes Hitachi HD63484 Advanced CRT Controller (ACRTC) as graphics 
C engine. Ref: Hitachi publication #U75 "HD63484 ACRTC Advanced CRT 
C Controller User's Manual".
C
C Configured with 1024 x 1024 8-bit pixels (image frame memory)
C plus 2-bit pixel overlay.
C
C Look up tables: BT458 RAMDAC supports 256 color/grey shades from a 
C palette of 16.8 million; overlay 4 colors from same palette.
C-----------------------------------------------------------------------
C
C Device type code: /TV
C
C Default device name: /dev/im0
C
C Default view surface dimensions: Depends on monitor.
C
C Resolution: The display address space is 1024 pixels (horizontal) by
C 1024 pixels (vertical).
C
C Color capability: 256 color indices (0-255).
C
C Input capability: None.
C-----------------------------------------------------------------------
C
      INTEGER UNIT, IER, ICX, ICY, ICH, L
      LOGICAL APPEND, MONO
      CHARACTER*10 MSG
      CHARACTER*3  TEST
      INTEGER*2 X, Y, C, R, G, B, LASTX, LASTY
      SAVE APPEND, MONO, LASTX, LASTY
C-----------------------------------------------------------------------
C
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230,240,250,260), IFUNC
  900 WRITE (MSG,'(I10)') IFUNC
      CALL GRWARN('Unimplemented function in Imagraph device driver:'
     1    //MSG)
      NBUF = -1
      RETURN
C
C--- IFUNC = 1, Return device name -------------------------------------
C
   10 CHR = 'IMAGRAPH'
      LCHR = 8
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices ---------------------------------------
C
   20 RBUF(1) = 0
      RBUF(2) = 1023
      RBUF(3) = 0
      RBUF(4) = 1023
      RBUF(5) = 0
      RBUF(6) = 255
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution -------------------------------
C
   30 RBUF(1) = 100.0
      RBUF(2) = 100.0
      RBUF(3) = 1
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info --------------------------------
C    (This device is Interactive, Cursor, No dashed lines, No area
C    fill, no thick lines, rectangle fill, pixel)
C
   40 CHR = 'ICNNNRPNNN'
      LCHR = 10
      RETURN
C
C--- IFUNC = 5, Return default file name -------------------------------
C
   50 CHR = '/dev/im0'
      LCHR = 8
      RETURN
C
C--- IFUNC = 6, Return default physical size of plot -------------------
C
   60 RBUF(1) = 0
      RBUF(2) = 1023
      RBUF(3) = 0
      RBUF(4) = 1023
      NBUF = 4
      RETURN
C
C--- IFUNC = 7, Return misc defaults -----------------------------------
C
   70 RBUF(1) = 2.0
      NBUF = 1
      RETURN
C
C--- IFUNC = 8, Select plot --------------------------------------------
C
   80 CONTINUE
      RETURN
C
C--- IFUNC = 9, Open workstation ---------------------------------------
C
   90 CONTINUE
C     -- APPEND flag?
      APPEND = (RBUF(3).NE.0.0)
C     -- monochrome monitor?
      CALL GRGENV('MONITOR', MSG, L)
      MONO = (MSG(1:1).EQ.'M' .OR. MSG(1:1).EQ.'m') 
C     -- special abbreviated device name?
      IF (LCHR.EQ.3) THEN
          CALL GRTOUP(TEST, CHR(:LCHR))
          IF (TEST.EQ.'IM0') THEN
              CHR = '/dev/im0'
              LCHR = 8
          ELSE IF (TEST.EQ.'IM1') THEN
              CHR = '/dev/im1'
              LCHR = 8
          ELSE IF (TEST.EQ.'IM2') THEN
              CHR = '/dev/im2'
              LCHR = 8
          END IF
      END IF
C     -- open device
      CALL GRTV00(CHR,LCHR,UNIT,IER)
      RBUF(1) = UNIT
      RBUF(2) = IER
      NBUF = 2
      IF (IER.NE.1) THEN
          CALL GRWARN('Cannot open Imagraph device: '//CHR(:LCHR))
          RETURN
      END IF
      C = 0
      IF (MONO) C = 1
      IF (.NOT.APPEND) CALL GRTV12(C)
      LASTX = -1
      RETURN
C
C--- IFUNC=10, Close workstation ---------------------------------------
C
  100 CONTINUE
      CALL GRTV01
      RETURN
C
C--- IFUNC=11, Begin picture -------------------------------------------
C
  110 CONTINUE
      IF (.NOT.APPEND) CALL GRTV02
      LASTX = -1
      RETURN
C
C--- IFUNC=12, Draw line -----------------------------------------------
C
  120 CONTINUE
      X = NINT(RBUF(1))
      Y = NINT(RBUF(2))
      IF (X.NE.LASTX .OR. Y.NE.LASTY) CALL GRTV03(X, Y)
      X = NINT(RBUF(3))
      Y = NINT(RBUF(4))
      CALL GRTV04(X, Y)
      LASTX = X
      LASTY = Y
      RETURN
C
C--- IFUNC=13, Draw dot ------------------------------------------------
C
  130 CONTINUE
      X = NINT(RBUF(1))
      Y = NINT(RBUF(2))
      IF (X.NE.LASTX .OR. Y.NE.LASTY) CALL GRTV03(X, Y)
      CALL GRTV05
      RETURN
C
C--- IFUNC=14, End picture ---------------------------------------------
C
  140 CONTINUE
      CALL GRTV06
      LASTX = -1
      RETURN
C
C--- IFUNC=15, Select color index --------------------------------------
C
  150 CONTINUE
      C = RBUF(1)
      IF (C.GT.255) C = 1
      CALL GRTV09(C)
      LASTX = -1
      RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C
  160 CONTINUE
      CALL GRTV06
      LASTX = -1
      RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
C           RBUF(1)   in/out : cursor x coordinate.
C           RBUF(2)   in/out : cursor y coordinate.
C           CHR(1:1)  output : keystroke.
C
  170 CONTINUE
      ICX = NINT(RBUF(1))
      ICY = NINT(RBUF(2))
  171 ICX = MAX(0,MIN(1023,ICX))
      ICY = MAX(0,MIN(1023,ICY))
      CALL GRTV07(ICX,ICY)
      CALL GRGETC(ICH)
      IF (ICH.LT.0) THEN
          CALL GRMCUR(ICH, ICX, ICY)
          GOTO 171
      END IF
      CALL GRTV08(ICX,ICY)
      RBUF(1) = ICX
      RBUF(2) = ICY
      CHR = CHAR(ICH)
      LASTX = -1
      RETURN
C
C--- IFUNC=18, Erase alpha screen. -------------------------------------
C    (Not implemented: no alpha screen)
C
  180 CONTINUE
      RETURN
C
C--- IFUNC=19, Set line style. -----------------------------------------
C    (Not implemented: should not be called)
C
  190 CONTINUE
      GOTO 900
C
C--- IFUNC=20, Polygon fill. -------------------------------------------
C    (Not implemented: should not be called)
C
  200 CONTINUE
      GOTO 900
C
C--- IFUNC=21, Set color representation. -------------------------------
C
  210 CONTINUE
      IF (MONO) THEN
         RBUF(2) = 0.30*RBUF(2) + 0.59*RBUF(3) + 0.11*RBUF(4)
         RBUF(3) = RBUF(2)
         RBUF(4) = RBUF(2)
      END IF
      C = RBUF(1)
      R = NINT(255*RBUF(2))
      G = NINT(255*RBUF(3))
      B = NINT(255*RBUF(4))
      CALL GRTV10(C, R, G, B)
      LASTX = -1
      RETURN
C
C--- IFUNC=22, Set line width. -----------------------------------------
C    (Not implemented: should not be called)
C
  220 CONTINUE
      GOTO 900
C
C--- IFUNC=23, Escape --------------------------------------------------
C    (Not implemented: ignored)
C
  230 CONTINUE
      RETURN
C
C--- IFUNC=24, Rectangle fill ------------------------------------------
C
  240 CONTINUE
      X = NINT(RBUF(1))
      Y = NINT(RBUF(2))
      CALL GRTV03(X, Y)
      X = NINT(RBUF(3))
      Y = NINT(RBUF(4))
      CALL GRTV11(X, Y)
      LASTX = -1
      RETURN
C
C--- IFUNC=25, Not implemented -----------------------------------------
C
  250 CONTINUE
      RETURN
C
C--- IFUNC=26, Line of pixels ------------------------------------------
C
  260 CONTINUE
      ICX = NINT(RBUF(1))
      ICY = NINT(RBUF(2))
      CALL GRTV13(ICX, ICY, NBUF-2, RBUF(3))
      RETURN
C-----------------------------------------------------------------------
      END
