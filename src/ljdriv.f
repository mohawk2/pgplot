C*LJDRIV -- PGPLOT HP-LaserJet driver
C+
      SUBROUTINE LJDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C-----------------------------------------------------------------------
C PGPLOT driver for HP-LaserJet device.
C-----------------------------------------------------------------------
C Version 0.0  - 1988 Dec 15 - C. J. Lonsdale.
C-----------------------------------------------------------------------
C
C Supported device: HP Laserjet, Laserjet+ or Laserjet series II printer
C
C Device type code: /LJHIGHRES
C
C Default device name: PGPLOT.LJPLOT.
C
C Default view surface dimensions: 7.84in (horizontal) by 10.0in
C (vertical).
C
C Resolution: 300 (x) x 300 (y) pixels/inch.
C
C Color capability: Color indices 0 (erase, white) and 1 (black) are
C supported. It is not possible to change color representation.
C
C Input capability: None.
C
C File format: UNIX byte string -- nothing special. A full-page plot occupies
C 1864 512-byte blocks before compression.
C
C Obtaining hardcopy: Use the command PRINT/PASSALL.
C
C Modified:
C   26th Feb 2004. G77 doesn't like CHAR() in PARAMETER statements.
C                  Parameters made into ordinary variable. KS/AAO.
C-----------------------------------------------------------------------
      CHARACTER*(*) TYPE, DEFNAM
      PARAMETER (TYPE='LJHIGHRES')
      PARAMETER (DEFNAM='PGPLOT.LJPLOT')
      CHARACTER*1 FF
      CHARACTER*50 DUMMYC
C
      INTEGER UNIT, IER, IC, BX, BY, NPICT
      SAVE UNIT, IC, BX, BY, NPICT
      CHARACTER*10 MSG
C-----------------------------------------------------------------------
C
      FF=CHAR(12)
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230), IFUNC
  900 WRITE (MSG,'(I10)') IFUNC
      CALL GRWARN('Unimplemented function in '//TYPE//' device driver:'
     1    //MSG)
      NBUF = -1
      RETURN
C
C--- IFUNC = 1, Return device name -------------------------------------
C
   10 CHR = TYPE
      LCHR = LEN(TYPE)
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices ---------------------------------------
C
   20 RBUF(1) = 0
      RBUF(2) = 2352
      RBUF(3) = 0
      RBUF(4) = 3000
      RBUF(5) = 0
      RBUF(6) = 1
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution -------------------------------
C
   30 RBUF(1) = 300.0
      RBUF(2) = 300.0
      RBUF(3) = 1
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info --------------------------------
C    (This device is Hardcopy, No cursor, No dashed lines, No area fill,
C    no thick lines)
C
   40 CHR = 'HNNNNNNNNN'
      LCHR = 10
      RETURN
C
C--- IFUNC = 5, Return default file name -------------------------------
C
   50 CHR = DEFNAM
      LCHR = LEN(DEFNAM)
      RETURN
C
C--- IFUNC = 6, Return default physical size of plot -------------------
C
   60 RBUF(1) = 0
      RBUF(2) = 2352
      RBUF(3) = 0
      RBUF(4) = 3000
      NBUF = 4
      RETURN
C
C--- IFUNC = 7, Return misc defaults -----------------------------------
C
   70 RBUF(1) = 1
      NBUF=1
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
C     -- dimensions of plot buffer
      BX = 294 
C             (2352/8, 6 bytes less than 8 full inches)
      BY = 3000
      NPICT = 0
C     -- open output file in C subroutine
      CALL GRCOPN(CHR(:LCHR),UNIT,IER)
      RBUF(1) = UNIT
      IF (IER.NE.0) THEN
          DUMMYC = CHR(:LCHR)
          CALL GRWARN('Cannot open output file for '//TYPE//' plot: '//
     1                DUMMYC(:LCHR))
          RBUF(2) = 0
      ELSE
          RBUF(2) = 1
      END IF
C     -- allocate memory for bitmap in C subroutine
      CALL GRGETM((BX+1)*(BY+5),IER)
      IF (IER.NE.0) THEN
          CALL GRGMSG(IER)
          CALL GRWARN('Failed to allocate plot buffer.')
          RBUF(2) = IER
          CALL GRCCLS(UNIT)
      END IF
      RETURN
C
C--- IFUNC=10, Close workstation ---------------------------------------
C
  100 CONTINUE
      CALL GRCCLS(UNIT)
      CALL GRFREM
      RETURN
C
C--- IFUNC=11, Begin picture -------------------------------------------
C
  110 CONTINUE
      NPICT = NPICT+1
      IF (NPICT.GT.1) THEN
          CALL GRPUTC(UNIT, FF)
          CALL GRCLRM((BX+1)*(BY+5))
      ENDIF
      RETURN
C
C--- IFUNC=12, Draw line -----------------------------------------------
C
  120 CONTINUE
      CALL GRLJ01(1, RBUF, IC, BX, BY)
      RETURN
C
C--- IFUNC=13, Draw dot ------------------------------------------------
C
  130 CONTINUE
      CALL GRLJ01(0, RBUF, IC, BX, BY)
      RETURN
C
C--- IFUNC=14, End picture ---------------------------------------------
C
  140 CONTINUE
C     -- This is the clever bit that compresses the bitmap
      CALL GRLJ02(UNIT, BX, BY)
      RETURN
C
C--- IFUNC=15, Select color index --------------------------------------
C
  150 CONTINUE
      IC = RBUF(1)
      IF (IC.LT.0 .OR. IC.GT.1) THEN
          IC = 1
          RBUF(1) = IC
      END IF
      RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C    (Not used.)
C
  160 CONTINUE
      RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
C    (Not implemented: should not be called)
C
  170 CONTINUE
      GOTO 900
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
C    (Not implemented: ignored)
C
  210 CONTINUE
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
C-----------------------------------------------------------------------
      END
