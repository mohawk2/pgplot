C*REDRIV -- PGPLOT Retrographics driver
C+
      SUBROUTINE REDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C
C PGPLOT driver for Retrographics VT640 terminal.
C
C Version 1.0 - 1987 Jun 26 - T. J. Pearson.
C Version 1.1 - 1987 Aug 21 - remove unnecessary US codes, and
C                             add special code for continued
C                             lines.
C Version 1.2 - 1988 Jul  1 - adapted for Unix.
C
C Supported device:  Digital Engineering, Inc., Retrographics
C modified VT100 terminal (VT640). The driver can also be used
C for the Visual-603 terminal (Visual Technology Incorporated, 
C 1703 Middlesex Street, Lowell, Mass 01851). The Visual-603 is a 
C VT200-compatible terminal with a Tektronix 4010/4014
C emulation mode and graphics extensions.
C
C Device type code: /RETRO. 
C
C Default device name: (the logged-in terminal). 
C
C Default view surface dimensions: Depends on monitor; nominally
C 8in (horizontal) by 6in (vertical).
C
C Resolution: The display consists of 640 pixels (horizontal) by
C 480 pixels (vertical), nominally at 80 pixels/inch. The coordinate
C system used for Tektronix emulation is 1024 x 780 pixels.
C
C Color capability: Color indices 0 (erase, black) and 1
C (bright: usually green) are supported. It is not possible to change
C color representation. 
C
C Input capability: The graphics cursor is a crosshair across
C the entire screen. The user positions the cursor using the four arrow
C keys on the keyboard of the Retrographics terminal. ``By striking the
C desired directional arrow key, the crosshair will move across the
C display screen at the rate of one dot per keystroke. Applying a cons-
C tant pressure on an arrow key will cause the crosshair to move at a
C continuous rapid rate; releasing the key will stop the crosshair's
C movement.'' The user indicates that the cursor has been positioned by
C typing any printable ASCII character on the keyboard of the
C Retrographics terminal. Most control characters (eg, ^C) are
C intercepted by the operating system and cannot be used. 
C 
C File format: It is not possible to send Retro plots to a disk
C file. For Tektronix-style disk files, use device type /TFILE.
C 
C Obtaining hardcopy: Not possible. 
C-----------------------------------------------------------------------
      CHARACTER CTMP*32
      CHARACTER CBUF*72, CTKTER*8
      SAVE      CBUF,    CTKTER
      INTEGER IER, I0, I1, J0, J1, ICIND, IX, IY, ICH, LTMP
      INTEGER LASTI, LASTJ, LBUF, UNIT
      SAVE    LASTI, LASTJ, LBUF, UNIT
      INTEGER GROTER
      LOGICAL APPEND, CONT
      SAVE    APPEND
      CHARACTER*10 MSG
C-----------------------------------------------------------------------
C
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230), IFUNC
  900 WRITE (MSG,'(I10)') IFUNC
      CALL GRWARN('Unimplemented function in RETRO device driver:'
     1    //MSG)
      NBUF = -1
      RETURN
C
C--- IFUNC = 1, Return device name -------------------------------------
C
   10 CHR = 'RETRO'
      LCHR = 5
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices ---------------------------------------
C
   20 RBUF(1) = 0
      RBUF(2) = 1023
      RBUF(3) = 0
      RBUF(4) = 779
      RBUF(5) = 0
      RBUF(6) = 1
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution -------------------------------
C
   30 RBUF(1) = 80.0*(8.0/5.0)
      RBUF(2) = 80.0*(13.0/8.0)
      RBUF(3) = 1
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info --------------------------------
C    (This device is Interactive, Cursor, No dashed lines, No area fill,
C    no thick lines)
C
   40 CHR = 'ICNNNNNNNN'
      LCHR = 10
      RETURN
C
C--- IFUNC = 5, Return default file name -------------------------------
C
   50 CONTINUE
      CALL GRTRML(CHR, LCHR)
      RETURN
C
C--- IFUNC = 6, Return default physical size of plot -------------------
C
   60 RBUF(1) = 0
      RBUF(2) = 1023
      RBUF(3) = 0
      RBUF(4) = 779
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
      APPEND = RBUF(3).NE.0.0
C     -- open device
      IER = GROTER(CHR, LCHR)
      IF (IER .EQ. -1) THEN
          CALL GRWARN('Unable to access graphics device.')
          UNIT = -1
          RBUF(1) = -1
          RBUF(2) = -1
          NBUF = 2
          RETURN
      ELSE
          UNIT = IER
          RBUF(1) = IER
          RBUF(2) = 1
          NBUF = 2
      END IF
      CTKTER(1:8)=CHAR(29)//CHAR(55)//CHAR(127)//CHAR(32)//
     :            CHAR(64)//CHAR( 3)//CHAR( 24)//CHAR(31)
      LASTI = -1
      LASTJ = -1
C     -- no device initialization required
      RETURN
C
C--- IFUNC=10, Close workstation ---------------------------------------
C
  100 CONTINUE
      CALL GRCTER(UNIT)
      RETURN
C
C--- IFUNC=11, Begin picture -------------------------------------------
C
  110 CONTINUE
C     -- erase screen; no wait required
      IF (.NOT.APPEND) THEN
          CTMP(1:4)=CHAR(29)//CHAR(27)//CHAR(12)//CHAR(24)
          LTMP = 4
          LASTI = -1
          GOTO 1000
      ELSE
          RETURN
      END IF
C
C--- IFUNC=12, Draw line -----------------------------------------------
C
  120 CONTINUE
      I0 = NINT(RBUF(1))
      J0 = NINT(RBUF(2))
      I1 = NINT(RBUF(3))
      J1 = NINT(RBUF(4))
      CONT = (LASTI.EQ.I0) .AND. (LASTJ.EQ.J0)
      CALL GRRE01(I0, J0, I1, J1, CONT, CTMP, LTMP)
      LASTI = I1
      LASTJ = J1
      GOTO 1000
C
C--- IFUNC=13, Draw dot ------------------------------------------------
C
  130 CONTINUE
      I0 = NINT(RBUF(1))
      J0 = NINT(RBUF(2))
      CALL GRRE01(I0, J0, I0, J0, .FALSE., CTMP, LTMP)
      LASTI = I0
      LASTJ = J0
      GOTO 1000
C
C--- IFUNC=14, End picture ---------------------------------------------
C
  140 CONTINUE
      RETURN
C
C--- IFUNC=15, Select color index --------------------------------------
C
  150 CONTINUE
      ICIND = RBUF(1)
      IF (ICIND.LT.0 .OR. ICIND.GT.1) THEN
          ICIND = 1
          RBUF(1) = ICIND
      END IF
      CTMP(1:5)=CHAR(29)//CHAR(27)//CHAR(47)//CHAR(49-ICIND)//CHAR(100)
      LTMP = 5
      LASTI = -1
      GOTO 1000
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C
  160 CONTINUE
      CALL GRRE02(CTKTER,8,CBUF,LBUF,UNIT)
      CALL GRWTER(UNIT, CBUF, LBUF)
      LASTI = -1
      RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
C           RBUF(1)   in/out : cursor x coordinate.
C           RBUF(2)   in/out : cursor y coordinate.
C           CHR(1:1)  output : keystroke.
C
  170 CONTINUE
C     -- flush buffer
      CALL GRWTER(UNIT, CBUF, LBUF)
C     -- 
      IX = NINT(RBUF(1))
      IY = NINT(RBUF(2))
      CALL GRRE04(UNIT, IX, IY, ICH, IER)
      IF (IER.EQ.0) THEN
          RBUF(1) = IX
          RBUF(2) = IY
          CHR(1:1) = CHAR(ICH)
      ELSE
          CHR(1:1) = CHAR(0)
      END IF
      NBUF = 2
      LCHR = 1
      LASTI = -1
      RETURN
C
C--- IFUNC=18, Erase alpha screen. -------------------------------------
C
  180 CONTINUE
      CALL GRRE02(CTKTER,8,CBUF,LBUF,UNIT)
      CTMP=CHAR(27)//'[2J'//CHAR(27)//'[H'
      CALL GRRE02(CTMP,7,CBUF,LBUF,UNIT)
      LASTI = -1
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
C
C--- Send the command. -------------------------------------------------
C
 1000 CALL GRRE02(CTMP,LTMP,CBUF,LBUF,UNIT)
C-----------------------------------------------------------------------
      END
