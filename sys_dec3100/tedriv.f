C*TEDRIV -- PGPLOT Tektronix-4010 driver
C+
      SUBROUTINE TEDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C
C PGPLOT driver for Tektronix-4010 VT640 terminal.
C
C Version 1.0 - 1987 Jun 26 - T. J. Pearson.
C Version 1.1 - 1987 Aug 15 - correct bug (missing RETURN).
C Version 1.2 - 1987 Aug 27 - handle continued lines better.
C Version 1.3 - 1988 Jul  6 - Unix.
C-----------------------------------------------------------------------
C
C Supported device:  Tektronix 4006/4010 storage-tube terminal
C (and, maybe, emulators).
C
C Device type code: /TEK4010
C
C Default device name: /dev/tty (the logged-in terminal). 
C
C Default view surface dimensions: Depends on monitor; nominally
C 7.9in (horizontal) by 6.0in (vertical).
C
C Resolution: The display address space is 1024 pixels (horizontal) by
C 780 pixels (vertical); actual resolution is usually worse than this.
C
C Color capability: Only color index 1 (bright: usually green) is
C supported. Requests to draw in color-index zero (normally erase) are
C ignored, as selective erase is not possibe. It is not possible to 
C change color representation. 
C
C Input capability: the PGPLOT cursor routine enables the Tektronix 
C cross-hair cursor, and waits for the operator to move the cross-hair.
C When the operator strikes a keyboard character, the routine returns
C with the character and the cross-hair location. N.B. The `graphic
C input terminators' sent by the terminal to the computer can be
C changed by jumpers (Tektronix terminals) or software (most emulators).
C The terminal or emulator should be configured to send two characters
C (usually CR, EOT).
C 
C File format: It is not possible to send Tektonix-4010 plots to a disk
C file. 
C 
C Obtaining hardcopy: Use the hardcopy switch on the terminal, if there
C is one, and if a suitable hard copy unit is connected.
C-----------------------------------------------------------------------
      INTEGER BUFSIZ
      PARAMETER (BUFSIZ=1024)
C
      INTEGER BUFFER, BUFLEV, UNIT, IER, I0, I1, J0, J1, IC, NW
      INTEGER IX, IY, ICH, LASTI, LASTJ
      INTEGER LIB$GET_VM, LIB$FREE_VM
      INTEGER GROTRM
      LOGICAL APPEND, CONT
      CHARACTER*10 MSG
      BYTE    TKBUF(100), TKTERM(6)
      DATA    TKTERM/29, 55, 127, 32, 64, 31/
C-----------------------------------------------------------------------
C
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230), IFUNC
  900 WRITE (MSG,'(I10)') IFUNC
      CALL GRWARN('Unimplemented function in TEK4010 device driver:'
     1    //MSG)
      NBUF = -1
      RETURN
C
C--- IFUNC = 1, Return device name -------------------------------------
C
   10 CHR = 'TEK4010'
      LCHR = 7
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
   30 RBUF(1) = 130.0
      RBUF(2) = 130.0
      RBUF(3) = 1
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info --------------------------------
C    (This device is Interactive, Cursor, No dashed lines, No area
C    fill, no thick lines)
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
          CALL GRWARN('Unable to access graphics device: ')
C          CALL GRWARN('Unable to access graphics device: '//
C     :                CHR(:LCHR))
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
C     -- erase screen; wait one second (required for real Tektronixes)
      IF (.NOT.APPEND) THEN
          TKBUF(1) = 29
          TKBUF(2) = 27
          TKBUF(3) = 12
          NW = 3
          CALL GRTE02(TKBUF, NW, %val(BUFFER), BUFLEV, UNIT)
          CALL GRWTRM(UNIT, %val(BUFFER), BUFLEV)
          CALL SLEEP(2)
      END IF
      RETURN
C
C--- IFUNC=12, Draw line -----------------------------------------------
C
  120 CONTINUE
      IF (IC.EQ.0) RETURN
      I0 = NINT(RBUF(1))
      J0 = NINT(RBUF(2))
      I1 = NINT(RBUF(3))
      J1 = NINT(RBUF(4))
      CONT = (LASTI.EQ.I0) .AND. (LASTJ.EQ.J0)
      CALL GRTE01(I0, J0, I1, J1, CONT, TKBUF, NW)
      LASTI = I1
      LASTJ = J1
      GOTO 1000
C
C--- IFUNC=13, Draw dot ------------------------------------------------
C
  130 CONTINUE
      IF (IC.EQ.0) RETURN
      I0 = NINT(RBUF(1))
      J0 = NINT(RBUF(2))
      CALL GRTE01(I0, J0, I0, J0, .FALSE., TKBUF, NW)
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
      LASTI = -1
      RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C
  160 CONTINUE
C     -- home cursor (by drawing a dark vector) and return to alpha mode
C        (code US)
      CALL GRTE02(TKTERM,6,%val(BUFFER),BUFLEV,UNIT)
C     -- flush buffer
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
      CALL GRTE04(IX, IY, ICH, IER, UNIT)
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
C
C--- Send the command. -------------------------------------------------
C
 1000 CALL GRTE02(TKBUF,NW,%val(BUFFER),BUFLEV,UNIT)
C-----------------------------------------------------------------------
      END

