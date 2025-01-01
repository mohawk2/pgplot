C*TFDRIV -- PGPLOT Tektronix file driver
C+
      SUBROUTINE TFDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C
C PGPLOT driver for Tektronix disk file. This version uses 
C the extended (12-bit) addressing of the Tektronix-4014.
C
C Version 1.0 - 1987 Aug 18 - T. J. Pearson.
C
C
C Supported device:  disk file which may be printable on a
C Tektronix-compatible device.
C
C Device type code: /TFILE. 
C
C Default device name: PGPLOT.TFPLOT.
C
C Default view surface dimension: Depends on printer; nominally
C 400 pixels/inch giving 10.24 in (horizontal) x 7.80 in (vertical).
C
C Resolution: The coordinate system used for Tektronix emulation is
C 4096 x 3120 pixels.
C
C Color capability: Only color index 1 is supported. Primitives drawn 
C in 'erase' mode (color index 0) are ignored (not erased). It is not 
C possible to change color representation. 
C
C Input capability: None.
C 
C File format: Binary, variable length records (maximum 1024 bytes);
C no carriage-control attribute.
C 
C Obtaining hardcopy: depends on the available printer. e.g., for an
C Imagen printer with IMPRINT software,
C    $ IMPRINT/STYLE=TEKTRONIX file.type
C
C Note: the file cannot easily be displayed on a Tektronix-compatible
C *terminal* because it contains control characters which will be
C interpreted by the operating system. The terminal must be set to
C 'Passall' mode before the file can be displayed.
C-----------------------------------------------------------------------
      CHARACTER*(*) TYPE, DEFNAM
      PARAMETER (DEFNAM='PGPLOT.TFPLOT')
      PARAMETER (TYPE='TFILE')
      INTEGER BUFSIZ
      PARAMETER (BUFSIZ=1024)
      INTEGER BUFFER
      INTEGER BUFLEV
      INTEGER UNIT, IER, I0, I1, J0, J1
      INTEGER LIB$GET_VM, LIB$FREE_VM
      CHARACTER*10 MSG
      INTEGER IC
      BYTE    TKBUF(100)
      INTEGER NW, IW
      BYTE    TKTERM(6)
      DATA    TKTERM/29, 55, 127, 32, 64, 31/
C-----------------------------------------------------------------------
C
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230), IFUNC
  900 WRITE (MSG,'(I10)') IFUNC
      CALL GRWARN('Unimplemented function in TFILE device driver:'
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
      RBUF(2) = 4095
      RBUF(3) = 0
      RBUF(4) = 3119
      RBUF(5) = 0
      RBUF(6) = 1
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution -------------------------------
C    (Nominal values)
C
   30 RBUF(1) = 400.0
      RBUF(2) = 400.0
C      (multiple strokes are spaced by 2 pixels, or 1/200 inch; ideally
C       the printer `pen' width should be 1/200 inch)
      RBUF(3) = 2
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info --------------------------------
C    (Non-interactive, No cursor, No dashed lines, No area fill,
C    no thick lines)
C
   40 CHR = 'NNNNNNNNNN'
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
      RBUF(2) = 4095
      RBUF(3) = 0
      RBUF(4) = 3119
      NBUF = 4
      RETURN
C
C--- IFUNC = 7, Return misc defaults -----------------------------------
C
   70 RBUF(1) = 8.0
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
C     -- allocate buffer
      IER = LIB$GET_VM(BUFSIZ, BUFFER)
      IF (IER.NE.1) THEN
          CALL GRGMSG(IER)
          CALL GRWARN('Failed to allocate plot buffer.')
          RBUF(2) = IER
          RETURN
      END IF
C     -- open device
      CALL GRGLUN(UNIT)
      NBUF = 2
      RBUF(1) = UNIT
      OPEN (UNIT=UNIT, FILE=CHR(:LCHR), CARRIAGECONTROL='NONE',
     1      DISPOSE='DELETE', STATUS='UNKNOWN',
     2      FORM='UNFORMATTED', RECORDTYPE='VARIABLE', IOSTAT=IER,
     3      RECL=256)
      IF (IER.NE.0) THEN
          CALL GRWARN('Cannot open output file for '//TYPE//' plot: '//
     1                CHR(:LCHR))
          RBUF(2) = 0
          CALL GRFLUN(UNIT)
          IER = LIB$FREE_VM(BUFSIZ, BUFFER)
          RETURN
      ELSE
          INQUIRE (UNIT=UNIT, NAME=CHR)
          LCHR = LEN(CHR)
   91     IF (CHR(LCHR:LCHR).EQ.' ') THEN
              LCHR = LCHR-1
              GOTO 91
          END IF
          RBUF(2) = 1
      END IF
      IC = 1
C     -- no device initialization required
      RETURN
C
C--- IFUNC=10, Close workstation ---------------------------------------
C
  100 CONTINUE
      CLOSE (UNIT, DISPOSE='KEEP')
      CALL GRFLUN(UNIT)
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
      TKBUF(1) = 29
      TKBUF(2) = 27
      TKBUF(3) = 12
      TKBUF(4) = 24
      NW = 4
      GOTO 1000
C
C--- IFUNC=12, Draw line -----------------------------------------------
C
  120 CONTINUE
      IF (IC.EQ.0) RETURN
      I0 = NINT(RBUF(1))
      J0 = NINT(RBUF(2))
      I1 = NINT(RBUF(3))
      J1 = NINT(RBUF(4))
      CALL GRTF01(I0, J0, I1, J1, TKBUF, NW)
      GOTO 1000
C
C--- IFUNC=13, Draw dot ------------------------------------------------
C
  130 CONTINUE
      IF (IC.EQ.0) RETURN
      I0 = NINT(RBUF(1))
      J0 = NINT(RBUF(2))
      CALL GRTF01(I0, J0, I0, J0, TKBUF, NW)
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
      RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C
  160 CONTINUE
      CALL GRTF02(TKTERM,6,%val(BUFFER),BUFLEV,UNIT)
      CALL GRTF03(%val(BUFFER), UNIT, BUFLEV)
      RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
C           Not implemented.
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
C
C--- Send the command. -------------------------------------------------
C
 1000 CALL GRTF02(TKBUF,NW,%val(BUFFER),BUFLEV,UNIT)
C-----------------------------------------------------------------------
      END
