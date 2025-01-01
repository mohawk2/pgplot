C*IVDRIV -- PGPLOT I2S IVAS Driver
C+
      SUBROUTINE IVDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C
C PGPLOT driver for I2S IVAS image display
C
C-----------------------------------------------------------------------
C
C Supported device:  I2S IVAS via DR11W based GPIT
C
C Device type code: /IVAS
C
C Default device name: /dev/ga0
C
C Default view surface dimensions: Depends on monitor; nominally
C ???in (horizontal) by ???in (vertical).
C
C Resolution: The display address space is 1024 pixels (horizontal) by
C 1024 pixels (vertical).
C
C Color capability: 16 color indeces
C
C Input capability: *** to be determined ***
C 
C File format: It is not possible to send IVAS plots to a disk
C file. 
C 
C Obtaining hardcopy: Turn brightness up to full and leave CRT on for
C several days.  Image will become permanent.
C-----------------------------------------------------------------------
C
      INTEGER UNIT, IER, ICX, ICY, ICH
      LOGICAL APPEND
      CHARACTER*10 MSG
      SAVE APPEND
C
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230,240), IFUNC
  900 WRITE (MSG,'(I10)') IFUNC
      CALL GRWARN('Unimplemented function in IVAS device driver:'
     1    //MSG)
      NBUF = -1
      RETURN
C
C--- IFUNC = 1, Return device name -------------------------------------
C
   10 CHR = 'IVAS'
      LCHR = 4
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
      RBUF(6) = 15
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution -------------------------------
C
C *** this will depend on the CRT ***
   30 RBUF(1) = 100.0
      RBUF(2) = 100.0
      RBUF(3) = 1
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info --------------------------------
C    (This device is Interactive, Cursor, No dashed lines, No area
C    fill, no thick lines, rectangle fill)
C
   40 CHR = 'ICNNNRNNNN'
      LCHR = 10
      RETURN
C
C--- IFUNC = 5, Return default file name -------------------------------
C
   50 CHR = '/dev/ga0'
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
      APPEND = (RBUF(3).NE.0.0)
      CALL GRIV00(UNIT,IER)
      RBUF(1) = UNIT
      RBUF(2) = IER
      NBUF = 2
      IF (.NOT.APPEND) CALL GRIV12
      RETURN
C
C--- IFUNC=10, Close workstation ---------------------------------------
C
  100 CONTINUE
      CALL GRIV01
      RETURN
C
C--- IFUNC=11, Begin picture -------------------------------------------
C
  110 CONTINUE
      IF (.NOT.APPEND) CALL GRIV02
      RETURN
C
C--- IFUNC=12, Draw line -----------------------------------------------
C
  120 CONTINUE
      CALL GRIV03(RBUF(1),RBUF(2))
      CALL GRIV04(RBUF(3),RBUF(4))
C     -- additional dot needed at end point
      CALL GRIV05
      RETURN
C
C--- IFUNC=13, Draw dot ------------------------------------------------
C
  130 CONTINUE
      CALL GRIV03(RBUF(1),RBUF(2))
      CALL GRIV05
      RETURN
C
C--- IFUNC=14, End picture ---------------------------------------------
C
  140 CONTINUE
      CALL GRIV06
      RETURN
C
C--- IFUNC=15, Select color index --------------------------------------
C
  150 CONTINUE
      CALL GRIV09(RBUF(1))
      RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C
  160 CONTINUE
      CALL GRIV06
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
      CALL GRIV07(ICX,ICY)
      CALL GRGETC(ICH)
      IF (ICH.LT.0) THEN
          CALL GRMCUR(ICH, ICX, ICY)
          GOTO 171
      END IF
      CALL GRIV08(ICX,ICY)
      RBUF(1) = ICX
      RBUF(2) = ICY
      CHR = CHAR(ICH)
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
      CALL GRIV10(RBUF(1),RBUF(2),RBUF(3),RBUF(4))
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
      CALL GRIV03(RBUF(1),RBUF(2))
      CALL GRIV11(RBUF(3),RBUF(4))
      RETURN
C-----------------------------------------------------------------------
      END
