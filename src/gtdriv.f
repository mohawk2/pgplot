C*GTDRIV -- PGPLOT driver for GTERM terminal emulator
C+
      SUBROUTINE GTDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C
C PGPLOT driver for GTERM terminal emulator.
C
C Version 1.1 - 1989 Oct 26 - T. J. Pearson.
C Version 1.2 - 1990 Jan 31 - improved color setup commands [A.Tennant]
C-----------------------------------------------------------------------
C
C Supported device:  IRAF GTERM Tektronix terminal emulator, with
C color extensions.
C
C Device type code: /GTERM
C
C Default device name: the logged-in terminal. 
C
C Default view surface dimensions: Depends on monitor; nominally
C 7.9in (horizontal) by 6.0in (vertical).
C
C Resolution: The display address space is 1024 pixels (horizontal) by
C 780 pixels (vertical); actual resolution is usually worse than this.
C
C Color capability: Color indices 0-15 are supported.
C
C Input capability: the PGPLOT cursor routine enables the Tektronix 
C cross-hair cursor, and waits for the operator to move the cross-hair.
C When the operator strikes a keyboard character, the routine returns
C with the character and the cross-hair location. N.B. The `graphic
C input terminators' sent by the terminal to the computer can be
C changed by jumpers (Tektronix terminals) or software (most emulators).
C The terminal or emulator should be configured to send two characters
C (usually CR, EOT).
C-----------------------------------------------------------------------
C Extensions to basic Tektronix protocol: 
C 
C 1. A CAN code (^X) is sent at the end of each PGPLOT buffer to
C switch GTERM back from its graphics window to its text window. 
C 
C 2. Color extensions. The following Tek-4115 commands are sent in
C graphics mode (i.e., after a GS code) 
C 
C SET-LINE-INDEX          <esc>ML<int>
C SET-SURFACE-COLOR-MAP   <esc>TG<int><int-array>
C
C where <esc> is the escape character.  <int> is an integer in
C Tektronix "host" format. 
C
C The parameter for SET-LINE-INDEX is the index into the color map
C that should be used for drawing. Color index 0 is the background
C color. 
C
C The parameters for SET-SURFACE-COLOR-MAP are as follows. The first
C <int> is the surface number to set.  It is ignored by GTERM. The
C integer array is a sequence of integers.  The first entry is the
C number of elements in the array. The following entries are
C quadruples, one for each index.  The quadruples consist of the color
C map index, then the RGB values (0-255). 
C-----------------------------------------------------------------------
      INTEGER BUFSIZ
      PARAMETER (BUFSIZ=1024)
      INTEGER CAN, ESC, GS, US
      PARAMETER (CAN=24, ESC=27, GS=29, US=31)
C
      BYTE BUFFER(BUFSIZ)
      INTEGER BUFLEV, UNIT, IER, I0, I1, J0, J1, CI, NW, I
      INTEGER IX, IY, ICH, LASTI, LASTJ, IR, IG, IB, LTEXT
      INTEGER GROTRM
      LOGICAL APPEND, CONT
      CHARACTER*10 MSG
      BYTE    TKBUF(100), TKTERM(7)
      DATA    TKTERM/GS, 55, 127, 32, 64, US, CAN/
      CHARACTER*5 C1, C2, C3, C4
      CHARACTER*32 TEXT
      INTEGER  NC1, NC2, NC3, NC4
      INTEGER CTABLE(3,0:15)
      DATA CTABLE /000,000,000, 255,255,255, 255,000,000, 000,255,000,
     1             000,000,255, 000,255,255, 255,000,255, 255,255,000,
     2             255,128,000, 128,255,000, 000,255,128, 000,128,255,
     3             128,000,255, 255,000,128, 085,085,085, 170,170,170/
C
      SAVE LASTI, LASTJ, BUFLEV, APPEND
C-----------------------------------------------------------------------
C
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,900,900,
     2     210,900,230), IFUNC
  900 WRITE (MSG,'(I10)') IFUNC
      CALL GRWARN('Unimplemented function in GTERM device driver:'
     1    //MSG)
      NBUF = -1
      RETURN
C
C--- IFUNC = 1, Return device name -------------------------------------
C
   10 CHR = 'GTERM'
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
      RBUF(6) = 15
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
   50 CALL GRTRML(CHR,LCHR)
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
   70 RBUF(1) = 1.0
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
      IER = GROTRM(CHR, LCHR)
      IF (IER .EQ. -1) THEN
          CALL GRWARN('Unable to access graphics device: '//
     :                CHR(:LCHR))
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
      LASTI = -1
      LASTJ = -1
      BUFLEV = 0
C     -- set default color representation
      IF (.NOT.APPEND) THEN
          DO 111 I=0,15
              CALL GRTK01(I,           C1, NC1)
              CALL GRTK01(CTABLE(1,I), C2, NC2)
              CALL GRTK01(CTABLE(2,I), C3, NC3)
              CALL GRTK01(CTABLE(3,I), C4, NC4)
              TEXT = CHAR(GS)//CHAR(ESC)//'TG14'//
     1               C1(:NC1)//C2(:NC2)//C3(:NC3)//C4(:NC4)//CHAR(US)
              LTEXT = 7 + NC1 + NC2 + NC3 + NC4
              CALL GRWTRM(UNIT, TEXT, LTEXT)
  111     CONTINUE
          CALL GRWTRM(UNIT, CHAR(CAN), 1)
      END IF
      RETURN
C
C--- IFUNC=10, Close workstation ---------------------------------------
C
  100 CONTINUE
      CALL GRCTRM(UNIT)
      RETURN
C
C--- IFUNC=11, Begin picture -------------------------------------------
C
  110 CONTINUE
C     -- erase screen; 
      IF (.NOT.APPEND) THEN
          TKBUF(1) = GS
          TKBUF(2) = ESC
          TKBUF(3) = 12
          TKBUF(4) = CAN
          NW = 4
          CALL GRTE02(TKBUF, NW, BUFFER, BUFLEV, UNIT)
          CALL GRWTRM(UNIT, BUFFER, BUFLEV)
      END IF
      RETURN
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
C     -- flush buffer
      CALL GRWTRM(UNIT, BUFFER, BUFLEV)
      CI = NINT(RBUF(1))
      CALL GRTK01(CI, C1, NC1)
      TEXT = CHAR(GS)//CHAR(ESC)//'ML'//C1(:NC1)//CHAR(US)//CHAR(CAN)
      LTEXT = 6 + NC1
      CALL GRWTRM(UNIT, TEXT, LTEXT)
      LASTI = -1
      RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C
  160 CONTINUE
C     -- home cursor (by drawing a dark vector) and return to alpha mode
C        (code US) and text screen (code CAN)
      CALL GRTE02(TKTERM,7,BUFFER,BUFLEV,UNIT)
C     -- flush buffer
      CALL GRWTRM(UNIT, BUFFER, BUFLEV)
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
      CALL GRWTRM(UNIT, BUFFER, BUFLEV)
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
C--- IFUNC=21, Set color representation. -------------------------------
C
  210 CONTINUE
C     -- flush buffer
      CALL GRWTRM(UNIT, BUFFER, BUFLEV)
      CI = RBUF(1)
      IR = RBUF(2)*255.0
      IG = RBUF(3)*255.0
      IB = RBUF(4)*255.0
      CALL GRTK01(CI, C1, NC1)
      CALL GRTK01(IR, C2, NC2)
      CALL GRTK01(IG, C3, NC3)
      CALL GRTK01(IB, C4, NC4)
      TEXT = CHAR(GS)//CHAR(ESC)//'TG14'//C1(:NC1)//
     1       C2(:NC2)//C3(:NC3)//C4(:NC4)//CHAR(US)//CHAR(CAN)
      LTEXT = 8 + NC1 + NC2 + NC3 + NC4
      CALL GRWTRM(UNIT, TEXT, LTEXT)
      RETURN
C
C--- IFUNC=23, Escape --------------------------------------------------
C    (Ignored)
C
  230 CONTINUE
      RETURN
C
C--- Send the command. -------------------------------------------------
C
 1000 CALL GRTE02(TKBUF,NW,BUFFER,BUFLEV,UNIT)
C-----------------------------------------------------------------------
      END
