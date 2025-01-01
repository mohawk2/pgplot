C*VVDRIV -- PGPLOT Versatec driver (portrait mode)
C+
      SUBROUTINE VVDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C-----------------------------------------------------------------------
C PGPLOT driver for Versatec device (portrait mode).
C-----------------------------------------------------------------------
C Version 1.0 - 1987 Aug 6 - T. J. Pearson.
C Version 1.1 - 1987 Aug 19 - add RECL= (TJP).
C-----------------------------------------------------------------------
C
C Supported device: Versatec V80 dot-matrix printer.
C
C Device type code: /VVERSATEC.
C
C Default device name: PGPLOT.VVPLOT.
C
C Default view surface dimensions: 8.0in (horizontal) by 10.5in 
C (vertical).
C
C Resolution: 200 (x) x 200 (y) pixels/inch.
C
C Color capability: Color indices 0 (erase, white) and 1 (black) are
C supported. It is not possible to change color representation.
C
C Input capability: None.
C
C File format: Variable-length records, maximum 264 bytes, with
C embedded carriage-control characters. A full-page plot occupies
C 832 512-byte blocks.
C
C Obtaining hardcopy: Use the command PRINT/PASSALL.
C-----------------------------------------------------------------------
      CHARACTER*(*) TYPE, DEFNAM
      PARAMETER (TYPE='VVERSATEC')
      PARAMETER (DEFNAM='PGPLOT.VVPLOT')
      CHARACTER FF
      PARAMETER (FF=CHAR(12))
C
      INTEGER UNIT, IER, IC, I, BX, BY, NPICT
      INTEGER LIB$GET_VM, LIB$FREE_VM
      CHARACTER*10 MSG
      INTEGER BITMAP
      REAL XBUF(4)
C-----------------------------------------------------------------------
C
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
      RBUF(2) = -1
      RBUF(3) = 0
      RBUF(4) = 2099
      RBUF(5) = 0
      RBUF(6) = 1
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution -------------------------------
C
   30 RBUF(1) = 200.0
      RBUF(2) = 200.0
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
      RBUF(2) = 1599
      RBUF(3) = 0
      RBUF(4) = 2099
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
      BX = 263 ! 2100/8
      BY = 1600
      CALL GRGLUN(UNIT)
      RBUF(1) = UNIT
      NPICT = 0
      OPEN (UNIT=UNIT, FILE=CHR(:LCHR), CARRIAGECONTROL='NONE',
     1                          DISPOSE='DELETE', STATUS='NEW',
     2      FORM='UNFORMATTED', RECORDTYPE='VARIABLE', IOSTAT=IER,
     3      RECL=128)
      IF (IER.NE.0) THEN
          CALL GRWARN('Cannot open output file for '//TYPE//' plot: '//
     1                CHR(:LCHR))
          RBUF(2) = 0
          CALL GRFLUN(UNIT)
      ELSE
          INQUIRE (UNIT=UNIT, NAME=CHR)
          LCHR = LEN(CHR)
   91     IF (CHR(LCHR:LCHR).EQ.' ') THEN
              LCHR = LCHR-1
              GOTO 91
          END IF
          RBUF(2) = 1
      END IF
      IER = LIB$GET_VM(BX*BY, BITMAP)
      IF (IER.NE.1) THEN
          CALL GRGMSG(IER)
          CALL GRWARN('Failed to allocate plot buffer.')
          RBUF(2) = IER
          CLOSE (UNIT=UNIT, DISPOSE='DELETE')
          CALL GRFLUN(UNIT)
      END IF
      RETURN
C
C--- IFUNC=10, Close workstation ---------------------------------------
C
  100 CONTINUE
      CLOSE (UNIT=UNIT, DISPOSE='KEEP')
      CALL GRFLUN(UNIT)
      IER = LIB$FREE_VM(BX*BY, BITMAP)
      IF (IER.NE.1) THEN
          CALL GRGMSG(IER)
          CALL GRWARN('Failed to deallocate plot buffer.')
      END IF
      RETURN
C
C--- IFUNC=11, Begin picture -------------------------------------------
C
  110 CONTINUE
      NPICT = NPICT+1
C%    type *,'Begin picture',NPICT
      IF (NPICT.GT.1) WRITE (UNIT=UNIT) FF
      CALL GRIBF1(BX*BY, %val(BITMAP), '00'X)
      RETURN
C
C--- IFUNC=12, Draw line -----------------------------------------------
C
  120 CONTINUE
      XBUF(1) = RBUF(2)
      XBUF(2) = 1599 - RBUF(1)
      XBUF(3) = RBUF(4)
      XBUF(4) = 1599 - RBUF(3)
      CALL GRVE01(1, XBUF, IC, BX, BY, %val(BITMAP))
      RETURN
C
C--- IFUNC=13, Draw dot ------------------------------------------------
C
  130 CONTINUE
      XBUF(1) = RBUF(2)
      XBUF(2) = 1599 - RBUF(1)
      CALL GRVE01(0, XBUF, IC, BX, BY, %val(BITMAP))
      RETURN
C
C--- IFUNC=14, End picture ---------------------------------------------
C
  140 CONTINUE
C%    type *,'End picture  ',NPICT
      CALL GRVE02(UNIT, BX, BY, %val(BITMAP))
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
