C*NUDRIV -- PGPLOT Null device driver
C+
      SUBROUTINE NUDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C
C PGPLOT driver for Null device (no graphical output)
C
C Version 1.0  - 1987 May 26 - T. J. Pearson.
C Version 1.1  - 1988 Mar 23 - add rectangle fill.
C
C Supported device: The ``null'' device can be used to suppress
C all graphic output from a program.  
C
C Device type code: /NULL.
C
C Default device name: None (the device name, if specified, is 
C ignored).
C
C Default view surface dimensions: Undefined (The device pretends to
C be a hardcopy device with 1000 pixels/inch and a view surface 8in 
C high by 10.5in wide.)
C
C Resolution: Undefined.
C
C Color capability: Color indices 0--255 are accepted.
C
C Input capability: None.
C
C File format: None.
C
C Obtaining hardcopy: Not possible.
C-----------------------------------------------------------------------
      CHARACTER*10 MSG
C
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230,240), IFUNC
  900 WRITE (MSG, '(I10)') IFUNC
      CALL GRWARN('Unimplemented function in NULL device driver: '//MSG)
      NBUF = -1
      RETURN
C
C--- IFUNC = 1, Return device name.-------------------------------------
C
   10 CHR = 'NULL'
      LCHR = 4
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices.---------------------------------------
C
   20 RBUF(1) = 0
      RBUF(2) = 10499
      RBUF(3) = 0
      RBUF(4) = 7999
      RBUF(5) = 0
      RBUF(6) = 255
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution. ------------------------------
C
   30 RBUF(1) = 1000.0
      RBUF(2) = 1000.0
      RBUF(3) = 1
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info. -------------------------------
C    (This device is Hardcopy, No cursor, Dashed lines, Area fill, Thick
C    lines, Rectangle fill)
C
   40 CHR = 'HNDATRNNNN'
      LCHR = 10
      RETURN
C
C--- IFUNC = 5, Return default file name. ------------------------------
C
   50 CHR = 'NL:'
      LCHR = 3
      RETURN
C
C--- IFUNC = 6, Return default physical size of plot. ------------------
C
   60 RBUF(1) = 0
      RBUF(2) = 10499
      RBUF(3) = 0
      RBUF(4) = 7999
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
      RBUF(1) = 0
      RBUF(2) = 1
      NBUF = 2
      RETURN
C
C--- IFUNC=10, Close workstation. --------------------------------------
C
  100 CONTINUE
      RETURN
C
C--- IFUNC=11, Begin picture. ------------------------------------------
C
  110 CONTINUE
      RETURN
C
C--- IFUNC=12, Draw line. ----------------------------------------------
C
  120 CONTINUE
      RETURN
C
C--- IFUNC=13, Draw dot. -----------------------------------------------
C
  130 CONTINUE
      RETURN
C
C--- IFUNC=14, End picture. --------------------------------------------
C
  140 CONTINUE
      RETURN
C
C--- IFUNC=15, Select color index. -------------------------------------
C
  150 CONTINUE
      RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C
  160 CONTINUE
      RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
C    (Not implemented: should not be called.)
C
  170 GOTO 900
C
C--- IFUNC=18, Erase alpha screen. -------------------------------------
C
  180 CONTINUE
      RETURN
C
C--- IFUNC=19, Set line style. -----------------------------------------
C
  190 CONTINUE
      RETURN
C
C--- IFUNC=20, Polygon fill. -------------------------------------------
C
  200 CONTINUE
      RETURN
C
C--- IFUNC=21, Set color representation. -------------------------------
C
  210 CONTINUE
      RETURN
C
C--- IFUNC=22, Set line width. -----------------------------------------
C
  220 CONTINUE
      RETURN
C
C--- IFUNC=23, Escape. -------------------------------------------------
C
  230 CONTINUE
      RETURN
C
C--- IFUNC=24, Rectangle fill. -----------------------------------------
C
  240 CONTINUE
      RETURN
C-----------------------------------------------------------------------
      END
