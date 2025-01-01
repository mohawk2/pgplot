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
C Version 1.3 - 1989 Jun 28 - Cray cf77 FORTRAN under UNICOS.
C                             Added 'SAVE' statements.
C Supported device:  Digital Engineering, Inc., Retrographics
C modified VT100 terminal (VT640). The driver can also be used
C for the Visual-603 terminal (Visual Technology Incorporated, 
C 1703 Middlesex Street, Lowell, Mass 01851). The Visual-603 is a 
C VT200-compatible terminal with a Tektronix 4010/4014
C emulation mode and graphics extensions.
C
C Device type code: /RETRO. 
C
C Default device name: TT: (the logged-in terminal). 
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
      INTEGER BUFSIZ
      PARAMETER (BUFSIZ=1024)
      INTEGER BUFLEV, UNIT, IER, I0, I1, J0, J1, IC, NW
      SAVE UNIT, BUFLEV, IC, NW
      INTEGER IX, IY, ICH, LASTI, LASTJ
      SAVE LASTI, LASTJ
C      INTEGER LIB$GET_VM, LIB$FREE_VM
      INTEGER GROTERM
      LOGICAL APPEND, CONT
      SAVE APPEND
      CHARACTER*10 MSG, LINE*72
      INTEGER TKBUF(100),TKTERM(7),TKERAS(7), BUFFER(BUFSIZ)
      SAVE TKTERM, TKERAS, BUFFER
C     BYTE    TKBUF(100)
C
      DATA    TKERAS/27, '[', '2', 'J', 27, '[', 'H'/
      DATA    TKTERM/29, 55, 127, 32, 64, 3, 24/
      DATA    BUFLEV/0/
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
   50 CHR = '/dev/tty'
      LCHR = 8
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
C     -- allocate buffer
C      IER = LIB$GET_VM(BUFSIZ, BUFFER)
C      IF (IER.NE.1) THEN
C          CALL GRGMSG(IER)
C          CALL GRWARN('Failed to allocate plot buffer.')
C          RBUF(2) = IER
C          RETURN
C      END IF
C     -- open device
      IER = GROTERM(CHR, LCHR)
      IF (IER .EQ. -1) THEN
          LENGTH = MIN(LEN(LINE), 34+LCHR)
          LINE(1:LENGTH) = 'Unable to access graphics device: '//
     1                     CHR(:LCHR)
          CALL GRWARN(LINE)
          UNIT = -1
          RBUF(1) = -1
          RBUF(2) = -1
C          IER = LIB$FREE_VM(BUFSIZ, BUFFER)
          NBUF = 2
          RETURN
      ELSE
          UNIT = IER
          RBUF(1) = IER
          RBUF(2) = 1
          NBUF = 2
      END IF
      IC = 1
      LASTI = -1
      LASTJ = -1
C     -- no device initialization required
      RETURN
C
C--- IFUNC=10, Close workstation ---------------------------------------
C
  100 CONTINUE
      CALL GRCTERM(UNIT)
C     IER = LIB$FREE_VM(BUFSIZ, BUFFER)
C     IF (IER.NE.1) THEN
C         CALL GRWARN('Error deallocating plot buffer.')
C         CALL GRGMSG(IER)
C     END IF
      RETURN
C
C--- IFUNC=11, Begin picture -------------------------------------------
C
  110 CONTINUE
C     -- erase screen; no wait required
      IF (.NOT.APPEND) THEN
          TKBUF(1) = 29
          TKBUF(2) = 27
          TKBUF(3) = 12
          TKBUF(4) = 24
          NW = 4
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
      CALL GRRE01(I0, J0, I1, J1, CONT, TKBUF, NW)
      LASTI = I1
      LASTJ = J1
      GOTO 1000
C
C--- IFUNC=13, Draw dot ------------------------------------------------
C
  130 CONTINUE
      I0 = NINT(RBUF(1))
      J0 = NINT(RBUF(2))
      CALL GRRE01(I0, J0, I0, J0, .FALSE., TKBUF, NW)
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
      IC = RBUF(1)
      IF (IC.LT.0 .OR. IC.GT.1) THEN
          IC = 1
          RBUF(1) = IC
      END IF
      TKBUF(1) = 29
      TKBUF(2) = 27
      TKBUF(3) = 47
      TKBUF(4) = 49 - IC
      TKBUF(5) = 100
      NW = 5
      LASTI = -1
      GOTO 1000
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C
  160 CONTINUE
      CALL GRTE02(TKTERM,7,BUFFER,BUFLEV,UNIT)
      CALL GRWTERM(UNIT, BUFFER, BUFLEV)
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
      CALL GRWTERM(UNIT, BUFFER, BUFLEV)
C     -- 
      IX = NINT(RBUF(1))
      IY = NINT(RBUF(2))
      CALL GRRE04(IX, IY, ICH, IER, UNIT)
      IF (IER.EQ.1) THEN
          RBUF(1) = IX
          RBUF(2) = IY
          CHR = CHAR(ICH)
      ELSE
          CHR = CHAR(0)
      END IF
      NBUF = 2
      LCHR = 1
      LASTI = -1
      RETURN
C
C--- IFUNC=18, Erase alpha screen. -------------------------------------
C
  180 CONTINUE
      CALL GRTE02(TKTERM,7,BUFFER,BUFLEV,UNIT)
      CALL GRTE02(TKERAS,7,BUFFER,BUFLEV,UNIT)
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
 1000 CALL GRTE02(TKBUF,NW,BUFFER,BUFLEV,UNIT)
C-----------------------------------------------------------------------
      END
