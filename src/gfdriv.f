C*GFDRIV -- PGPLOT GraphOn 200-series driver [UNIX]
C+
      SUBROUTINE GFDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C
C PGPLOT driver for GraphOn 200-series terminals (tested on GO-230).
C [This device handler uses subroutines from the Retrographics driver,
C so REDRIVER must also be installed.]
C
C Version 1.0 - 1987 Oct 31 - T. J. Pearson [adapted from REDRIVER 
C                               v1.1].
C Version 1.1 - 1988 Mar 23 - add rectangle fill.
C Version 1.2 - 1988 Jul  1 - adapted for Unix.
C Version 1.3 - 1990 Mar 15 - correct error in rectangle fill.
C
C Supported device:  GraphOn Corporation 200-series terminals.
C These emulate a Tektronix-4010 with enhancements.
C
C Device type code: /GF. 
C
C Default device name: the logged-in terminal (TT: in VMS, /dev/tty in 
C Unix). 
C
C Default view surface dimensions: Depends on monitor; nominally 8in
C (horizontal) by 6in (vertical). 
C 
C Resolution: The coordinate system used for Tektronix emulation is
C 1024 x 780 pixels. 
C 
C Color capability: Color indices 0 (erase, black) and 1 (normal) are
C supported. It is not possible to change color representation. 
C 
C Input capability: The graphics cursor is a crosshair across the
C entire screen. The user positions the cursor using the four arrow
C keys on the keyboard of the GraphOn 200-series terminal; the shift
C key speeds up the cursor motion. The user indicates that the cursor
C has been positioned by typing any printable ASCII character on the
C keyboard of the GraphOn 200-series terminal. Most control characters
C (eg, ^C) are intercepted by the operating system and cannot be used.
C 
C File format: It is not possible to send plots to a disk file. For
C Tektronix-style disk files, use device type /TFILE. 
C 
C User Notes
C ----------
C Use a device type of "/GF".
C The default device is "TT", the user's terminal; a different
C terminal can be specified if appropriate, e.g., "TXA3:/GRA". (The
C PGPLOT GRAPHON device handler is an adaptation of the TEK4010 device
C handler, with extensions for switching between text and graphics,
C erase-mode writing [color index 0], rectangle fill, and improved
C speed.) 
C 
C The GO-230 emulates both a VT220 terminal and a Tektronix-4010
C terminal. Usually, it is configured with two separate screen
C memories, one for each "terminal". These memories ("windows") may be
C displayed simultaneously or one at a time on the terminal screen.
C PGPLOT displays all Tektronix commands in the Tektronix window, and
C it ensures that all text written to the terminal without going
C through PGPLOT is displayed in the VT220 window. 
C 
C The user may choose whether to display both windows simultaneously
C or only the active one; this is a parameter in the SETUP DISPLAY
C menu. If only the active window is displayed, the user can switch
C from the active to the inactive window and back by pressing the
C WINDOW key (number F4 in the row of function keys). Use SHIFT-WINDOW
C to turn on or turn off the status line displayed at the bottom of
C the screen; the status line should be off in order to see the full
C Tektronix window. Other useful keys are RESET (SHIFT-HOME), which
C erases the text if the VT220 screen is selected, and PAGE (NEXT
C SCREEN) which erases the graphics if the Tek screen is selected. 
C 
C If both windows are displayed simultaneously, the GraphOn terminal
C behaves like a Retrographics (VT640) terminal. If only one window is
C displayed, the graph will disappear as soon as the program sends
C text to the terminal; you have to use the WINDOW key to view the
C graphics. Users may prefer one mode or the other, depending on the
C application program.
C 
C To move the graphics cursor (a cross-hair) use the arrow keys on
C the keyboard; use the shift key with the arrow keys to make the
C cursor move faster. 
C 
C For PGPLOT to function correctly, a number of SETUP parameters must
C be chosen correctly (this is probably not a complete list). 
C 
C In the SETUP DIRECTORY menu: set the terminal to "VT220 and 4014".
C In the SETUP GENERAL menu:   "full resolution for Tek emulation".
C In the SETUP DISPLAY menu:   "both planes" or "only plane with
C                              active window" (user preference).
C In the SETUP GRAPHICS menu:
C       standard functions:    "4010" mode, GIN terminator "CR/EOT".  
C       enhanced functions:    "full enhancement".
C 
C Bugs: the cursor routine in PGPLOT (PGCURSE) attempts to position 
C the cross-hair cursor at a specified point on the screen when it 
C enables it; the GraphOn terminal ignores this request, and leaves 
C the cursor where the user left it. In this behavior, the GraphOn
C is probably emulating a real Tektronix terminal all too well.
C 
C-----------------------------------------------------------------------
      INTEGER BUFSIZ
      PARAMETER (BUFSIZ=1024)
      INTEGER BUFFER
      INTEGER BUFLEV
      INTEGER UNIT, IER, I0, I1, J0, J1, IX, IY, ICH
      SAVE    UNIT
      INTEGER LASTI, LASTJ
      INTEGER LIB$GET_VM, LIB$FREE_VM
      INTEGER GROTRM
      LOGICAL APPEND, CONT
      CHARACTER*10 MSG
      INTEGER IC
      BYTE    TKBUF(100)
      INTEGER NW
      BYTE    TKERAS(7), TKTERM(7), BRECT(3), ERECT(2)
      DATA    TKERAS/27, '[', '2', 'J', 27, '[', 'H'/
      DATA    TKTERM/29, 55, 127, 32, 64, 3, 24/
      DATA    BRECT/29, 27, 2/
      DATA    ERECT/27, 3/
C-----------------------------------------------------------------------
C
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230,240), IFUNC
  900 WRITE (MSG,'(I10)') IFUNC
      CALL GRWARN('Unimplemented function in GRAPHON device driver:'
     1    //MSG)
      NBUF = -1
      RETURN
C
C--- IFUNC = 1, Return device name -------------------------------------
C
   10 CHR = 'GF (GraphOn)'
      LCHR = 16
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
C    no thick lines, Rectangle fill)
C
   40 CHR = 'ICNNNRNNNN'
      LCHR = 10
      RETURN
C
C--- IFUNC = 5, Return default file name -------------------------------
C
   50 CALL GRTRML(CHR, LCHR)
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
      IER = LIB$GET_VM(BUFSIZ, BUFFER)
      IF (IER.NE.1) THEN
          CALL GRGMSG(IER)
          CALL GRWARN('Failed to allocate plot buffer.')
          RBUF(2) = IER
          RETURN
      END IF
C     -- open device
      IER = GROTRM(CHR, LCHR)
      IF (IER .EQ. -1) THEN
          CALL GRWARN('Unable to access graphics device.')
          UNIT = -1
          RBUF(1) = -1
          RBUF(2) = -1
          IER = LIB$FREE_VM(BUFSIZ, BUFFER)
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
      CALL GRCTRM(UNIT)
      IER = LIB$FREE_VM(BUFSIZ, BUFFER)
      IF (IER.NE.1) THEN
          CALL GRWARN('Error deallocating plot buffer.')
          CALL GRGMSG(IER)
      END IF
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
      TKBUF(3) = 1
      IF (IC.EQ.0) TKBUF(3) = 16
      NW = 3
      LASTI = -1
      GOTO 1000
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C
  160 CONTINUE
      CALL GRTE02(TKTERM,7,%val(BUFFER),BUFLEV,UNIT)
      CALL GRWTRM(UNIT, %val(BUFFER), BUFLEV)
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
      CALL GRWTRM(UNIT, %val(BUFFER), BUFLEV)
C     -- 
      IX = NINT(RBUF(1))
      IY = NINT(RBUF(2))
      CALL GRGF04(IX, IY, ICH, IER, UNIT)
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
      CALL GRTE02(TKTERM,7,%val(BUFFER),BUFLEV,UNIT)
      CALL GRTE02(TKERAS,7,%val(BUFFER),BUFLEV,UNIT)
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
C--- IFUNC=24, Rectangle fill ------------------------------------------
C
  240 CONTINUE
      I0 = NINT(RBUF(1))
      J0 = NINT(RBUF(2))
      I1 = NINT(RBUF(3))
      J1 = NINT(RBUF(4))
      CONT = .FALSE.
      CALL GRTE02(BRECT,3,%val(BUFFER),BUFLEV,UNIT)
      CALL GRRE01(I0, J0, I1, J1, CONT, TKBUF, NW)
      CALL GRTE02(TKBUF,NW,%val(BUFFER),BUFLEV,UNIT)
      CALL GRTE02(ERECT,2,%val(BUFFER),BUFLEV,UNIT)
      LASTI = -1
      RETURN
C
C--- Send the command. -------------------------------------------------
C
 1000 CALL GRTE02(TKBUF,NW,%val(BUFFER),BUFLEV,UNIT)
C-----------------------------------------------------------------------
      END
