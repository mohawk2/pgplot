C*GRPS01 -- PGPLOT PostScript driver
C+
      SUBROUTINE GRPS01 (IFUNC, RBUF, NBUF, CHR, LCHR, MODE)
      INTEGER IFUNC, NBUF, LCHR, MODE
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C
C PGPLOT driver for PostScript devices.
C
C Version 1.2  - 1987 Aug  5 - T. J. Pearson.
C Version 1.3  - 1987 Nov 16 - add "bind" commands to prolog - TJP.
C Version 1.4  - 1988 Jan 28 - change dimensions so whole field can be
C                              plotted - TJP.
C Version 1.5  - 1988 Oct 27 - make EOF characters optional - TJP.
C Version 1.6  - 1988 Dec 15 - standard Fortran - TJP.
C Version 1.7  - 1989 Jul  5 - change color indices so most colors
C                              are black - TJP.
C Version 2.0  - 1990 Sep 10 - parameterize dimensions; correct
C                              bounding box; add color support (from
C                              D. Meier's CPdriver) - TJP.
C
C Supported device: 
C   Any printer that accepts the PostScript page description language, 
C   eg, the LaserWriter (Apple Computer, Inc.).
C   PostScript is a trademark of Adobe Systems Incorporated.
C
C Device type code: 
C   /PS (landscape mode, long edge of paper horizontal).
C   /VPS (portrait mode, short edge of paper horizontal).
C
C Default file name:
C   pgplot.ps
C
C Default view surface dimensions:
C   10.5 inches horizontal x  7.8 inches vertical (landscape mode),
C    7.8 inches horizontal x 10.5 inches vertical (portrait mode).
C   These dimensions can be changed with environment variables.
C
C Resolution:
C   The driver uses coordinate increments of 0.001 inch, giving an
C   ``apparent'' resolution of 1000 pixels/inch. The true resolution is
C   device-dependent; eg, on an Apple LaserWriter it is 300 pixels/inch
C   (in both dimensions). 
C
C Color capability (monochrome mode): 
C   Monochrome mode is the default.
C   Color indices 0-15 are supported. Color index 0 is white (erase
C   or background color), indices 1-13 are black, 14 is light grey,
C   and 15 is dark grey. It is not possible to change the color
C   representation.
C
C Color capability (color mode): 
C   Color mode is requested by defining environment variable
C   PGPLOT_PS_COLOR.
C   Color indices 0-15 are supported. Color index 0 is white (erase
C   or background color), index 1 is black, and indices 2-15 have the
C   standard PGPLOT color assignments.
C   It is possible to change the color representation using PGSCR.
C
C Input capability: none.
C
C File format: the file contains variable length records (maximum 132
C characters) containing PostScript commands. The commands use only
C printable ASCII characters, and the file can be examined or modified 
C with a text editor. 
C
C Obtaining hardcopy: use the VMS COPY command to send the file to a
C suitable device or use the VMS PRINT command if a printer queue has 
C been established.
C
C Environment variables:
C
C  PGPLOT_PS_COLOR
C If this variable is defined, the PostScript device driver operates
C in color mode. The file can be printed on a monochrome printer, but
C colors will be converted to shades of grey. 
C
C  PGPLOT_PS_WIDTH      default  7800
C  PGPLOT_PS_HEIGHT     default 10500
C  PGPLOT_PS_HOFFSET    default   350
C  PGPLOT_PS_VOFFSET    default   250
C These variables tell PGPLOT how big an image to produce. The defaults
C are appropriate for 8.5 x 11-inch paper. The maximum dimensions of
C a PGPLOT image are WIDTH by HEIGHT, with the lower left corner offset
C by HOFFSET horizontally and VOFFSET vertically from the lower left
C corner of the paper. The units are milli-inches. The "top" of the
C paper is the edge that comes out of the printer first.
C
C  PGPLOT_PS_EOF
C Normally the output file does not contain special end-of-file
C characters. But if environment variable PGPLOT_PS_EOF is defined
C (with any value) PGPLOT writes a control-D job-separator character at 
C the beginning and at the end of the file. This is appropriate for
C Apple LaserWriters using the serial interface, but it may not be 
C appropriate for other PostScript devices.
C
C References:
C (1) Adobe Systems, Inc.: PostScript Language Reference Manual.
C Addison-Wesley, Reading, Massachusetts, 1985.
C (2) Adobe Systems, Inc.: PostScript Language Tutorial and Cookbook.
C Addison-Wesley, Reading, Massachusetts, 1985.
C-----------------------------------------------------------------------
      INTEGER DWD, DHT, DOFFW, DOFFH
      CHARACTER*(*) PTYPE, LTYPE, DEFNAM
      PARAMETER (PTYPE='VPS', LTYPE='PS')
      PARAMETER (DEFNAM='pgplot.ps')
C -- printable paper area: in milli-inches; (WIDTH, HEIGHT) are
C    the dimensions of the printable area; OFFW, OFFH the offset from
C    the lower left corner of the paper
      PARAMETER (DWD=7800, DHT=10500, DOFFW=350, DOFFH=250)
C
      INTEGER WIDTH, HEIGHT, OFFW, OFFH
      SAVE    WIDTH, HEIGHT, OFFW, OFFH
      INTEGER  IER, I0, J0, I1, J1, L, LASTI, LASTJ, UNIT, LOBUF
      SAVE                             LASTI, LASTJ, UNIT, LOBUF
      INTEGER  CI, LW, NPTS, NPAGE, JUNK
      SAVE             NPTS, NPAGE
      INTEGER  GROPTX
      LOGICAL  START, LANDSC, COLOR
      SAVE     START,         COLOR
      REAL RVALUE(0:15), GVALUE(0:15), BVALUE(0:15)
      SAVE RVALUE,       GVALUE,       BVALUE
      CHARACTER*80  INSTR, MSG
      CHARACTER*132 OBUF
      SAVE          OBUF
      CHARACTER*4   SHADE(0:15)
      SAVE          SHADE
      DATA SHADE /'1.00', 13*'0.00', '0.67', '0.33'/
      DATA RVALUE 
     1     / 1.00, 0.00, 1.00, 0.00, 0.00, 0.00, 1.00, 1.00,
     2       1.00, 0.50, 0.00, 0.00, 0.50, 1.00, 0.33, 0.67/
      DATA GVALUE
     1     / 1.00, 0.00, 0.00, 1.00, 0.00, 1.00, 0.00, 1.00,
     2       0.50, 1.00, 1.00, 0.50, 0.00, 0.00, 0.33, 0.67/
      DATA BVALUE
     1     / 1.00, 0.00, 0.00, 0.00, 1.00, 1.00, 1.00, 0.00,
     2       0.00, 0.00, 0.50, 1.00, 1.00, 0.50, 0.33, 0.67/
C-----------------------------------------------------------------------
C
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230), IFUNC
      GOTO 900
C
C--- IFUNC = 1, Return device name.-------------------------------------
C
   10 LANDSC = MODE.EQ.0
      IF (LANDSC) THEN
          CHR = LTYPE
          LCHR = LEN(LTYPE)
      ELSE
          CHR = PTYPE
          LCHR = LEN(PTYPE)
      END IF
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices.---------------------------------------
C
   20 RBUF(1) = 0
      RBUF(3) = 0
      RBUF(5) = 0
      LANDSC = MODE.EQ.0
      IF (LANDSC) THEN
          RBUF(2) = HEIGHT-1
          RBUF(4) = WIDTH-1
      ELSE
          RBUF(2) = WIDTH-1
          RBUF(4) = HEIGHT-1
      END IF
      RBUF(6) = 15
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution. ------------------------------
C
   30 RBUF(1) = 1000.0
      RBUF(2) = 1000.0
      RBUF(3) = 5
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info. -------------------------------
C    (This device is Hardcopy, No cursor, No dashed lines, Area fill, 
C    Thick lines)
C
   40 CHR = 'HNNATNNNNN'
      LCHR = 10
      RETURN
C
C--- IFUNC = 5, Return default file name. ------------------------------
C
   50 CHR = DEFNAM
      LCHR = LEN(DEFNAM)
      RETURN
C
C--- IFUNC = 6, Return default physical size of plot. ------------------
C
   60 RBUF(1) = 0
      RBUF(3) = 0
      LANDSC = MODE.EQ.0
      IF (LANDSC) THEN
          RBUF(2) = HEIGHT-1
          RBUF(4) = WIDTH-1
      ELSE
          RBUF(2) = WIDTH-1
          RBUF(4) = HEIGHT-1
      END IF
      NBUF = 4
      RETURN
C
C--- IFUNC = 7, Return misc defaults. ----------------------------------
C
   70 RBUF(1) = 8
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
C     -- Color mode?
      CALL GRGENV('PS_COLOR', INSTR, L)
      COLOR = L.GT.0
C     -- Device dimensions
      WIDTH = DWD
      HEIGHT = DHT
      OFFW = DOFFW
      OFFH = DOFFH
      CALL GRGENV('PS_WIDTH', INSTR, L)
      IF (L.GT.0) READ(INSTR(:L),'(BN,I10)',IOSTAT=JUNK) WIDTH
      CALL GRGENV('PS_HEIGHT', INSTR, L)
      IF (L.GT.0) READ(INSTR(:L),'(BN,I10)',IOSTAT=JUNK) HEIGHT
      CALL GRGENV('PS_HOFFSET', INSTR, L)
      IF (L.GT.0) READ(INSTR(:L),'(BN,I10)',IOSTAT=JUNK) OFFW
      CALL GRGENV('PS_VOFFSET', INSTR, L)
      IF (L.GT.0) READ(INSTR(:L),'(BN,I10)',IOSTAT=JUNK) OFFH
      CALL GRGLUN(UNIT)
      NBUF = 2
      RBUF(1) = UNIT
      IER = GROPTX(UNIT, CHR(1:LCHR), DEFNAM)
      IF (IER.NE.0) THEN
          MSG = 'Cannot open output file for PostScript plot: '//
     1                CHR(:LCHR)
          CALL GRWARN(MSG)
          RBUF(2) = 0
          CALL GRFLUN(UNIT)
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
      LOBUF = 0
      LASTI = -1
      LASTJ = - 1
      NPTS = 0
      CALL GRGENV('PS_EOF', INSTR, L)
      IF (L.GT.0) CALL GRPS02(UNIT, CHAR(4))
      CALL GRPS02(UNIT, '%!PS-Adobe-1.0')
      CALL GRUSER(INSTR, L)
      IF (L.GT.0) CALL GRPS02(UNIT, '%%Creator: '//INSTR(1:L))
      CALL GRPS02(UNIT, '%%Title: PGPLOT PostScript plot')
      CALL GRDATE(INSTR, L)
      IF (L.GT.0) CALL GRPS02(UNIT, '%%CreationDate: '//INSTR(1:L))
C -- bounding box is based on maximum plot dimensions, not
C    actual dimensions
      CALL GRFAO('%%BoundingBox: # # # #', L, INSTR,
     1   NINT(OFFW*0.072), NINT(OFFH*0.072), NINT((WIDTH+OFFW)*0.072), 
     2   NINT((HEIGHT+OFFH)*0.072))
      CALL GRPS02(UNIT, INSTR(:L))
      CALL GRPS02(UNIT, '%%DocumentFonts: (atend)')
      CALL GRPS02(UNIT, '%%Pages: (atend)')
      CALL GRPS02(UNIT, '%%EndComments')
      CALL GRPS02(UNIT, 
     1  '/l {moveto rlineto currentpoint stroke moveto} bind def')
      CALL GRPS02(UNIT, 
     1  '/c {rlineto currentpoint stroke moveto} bind def')
      CALL GRPS02(UNIT, 
     1  '/d {moveto 0 0 rlineto currentpoint stroke moveto} bind def')
      CALL GRPS02(UNIT, '/SLW {5 mul setlinewidth} bind def')
      CALL GRPS02(UNIT, '/SCF /pop load def')
      CALL GRPS02(UNIT, '/BP {newpath moveto} bind def')
      CALL GRPS02(UNIT, '/LP /rlineto load def')
      CALL GRPS02(UNIT, '/EP {rlineto closepath eofill} bind def')
      CALL GRPS02(UNIT, '%%EndProlog')
      NPAGE = 0
      RETURN
C
C--- IFUNC=10, Close workstation. --------------------------------------
C
  100 CONTINUE
      CALL GRPS02(UNIT, ' ')
      CALL GRPS02(UNIT, '%%Trailer')
      CALL GRPS02(UNIT, '%%DocumentFonts: ')
      CALL GRFAO('%%Pages: #', L, INSTR, NPAGE, 0, 0, 0)
      CALL GRPS02(UNIT, INSTR(:L))
      CALL GRGENV('PS_EOF', INSTR, L)
      IF (L.GT.0) CALL GRPS02(UNIT, CHAR(4))
      CLOSE (UNIT)
      CALL GRFLUN(UNIT)
      RETURN
C
C--- IFUNC=11, Begin picture. ------------------------------------------
C
  110 CONTINUE
      NPAGE = NPAGE+1
      CALL GRPS02(UNIT, ' ')
      CALL GRFAO('%%Page: # #', L, INSTR, NPAGE, NPAGE, 0, 0)
      CALL GRPS02(UNIT, INSTR(:L))
      CALL GRPS02(UNIT, '/PGPLOT save def')
      CALL GRPS02(UNIT, '0.072 0.072 scale')
      LANDSC = MODE.EQ.0
      IF (LANDSC) THEN
          CALL GRFAO('# # translate 90 rotate', L, INSTR, WIDTH+OFFW, 
     1               OFFH, 0, 0)
      ELSE
          CALL GRFAO('# # translate', L, INSTR, OFFW, OFFH, 0, 0)
      END IF
      CALL GRPS02(UNIT, INSTR(:L))
      CALL GRPS02(UNIT, '1 setlinejoin 1 setlinecap 1 SLW 1 SCF')
      RETURN
C
C--- IFUNC=12, Draw line. ----------------------------------------------
C
  120 CONTINUE
      I0 = NINT(RBUF(1))
      J0 = NINT(RBUF(2))
      I1 = NINT(RBUF(3))
      J1 = NINT(RBUF(4))
      IF (I0.EQ.LASTI .AND. J0.EQ.LASTJ) THEN
          CALL GRFAO('# # c', L, INSTR, (I1-I0), (J1-J0), 0, 0)
      ELSE
          CALL GRFAO('# # # # l', L, INSTR, (I1-I0), (J1-J0), I0, J0)
      END IF
      LASTI = I1
      LASTJ = J1
      GOTO 800
C
C--- IFUNC=13, Draw dot. -----------------------------------------------
C
  130 CONTINUE
      I1 = NINT(RBUF(1))
      J1 = NINT(RBUF(2))
      CALL GRFAO('# # d', L, INSTR, I1, J1, 0, 0)
      LASTI = I1
      LASTJ = J1
      GOTO 800
C
C--- IFUNC=14, End picture. --------------------------------------------
C
  140 CONTINUE
      IF (LOBUF.NE.0) THEN
          CALL GRPS02(UNIT, OBUF(1:LOBUF))
          LOBUF = 0
      END IF
      CALL GRPS02(UNIT, 'showpage PGPLOT restore')
      RETURN
C
C--- IFUNC=15, Select color index. -------------------------------------
C
  150 CONTINUE
      CI = NINT(RBUF(1))
      IF (COLOR) THEN
          WRITE(INSTR,'(3(F5.3,1X),''setrgbcolor'')')
     1          RVALUE(CI), GVALUE(CI), BVALUE(CI)
          L = 29
      ELSE
          INSTR = SHADE(CI)//' setgray'
          L = LEN(SHADE(CI))+8
      END IF
      LASTI = -1
      GOTO 800
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C
  160 CONTINUE
      IF (LOBUF.NE.0) THEN
          CALL GRPS02(UNIT, OBUF(1:LOBUF))
          LOBUF = 0
      END IF
      RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
C    (Not implemented: should not be called.)
C
  170 GOTO 900
C
C--- IFUNC=18, Erase alpha screen. -------------------------------------
C    (Null operation: there is no alpha screen.)
C
  180 CONTINUE
      RETURN
C
C--- IFUNC=19, Set line style. -----------------------------------------
C    (Not implemented: should not be called.)
C
  190 GOTO 900
C
C--- IFUNC=20, Polygon fill. -------------------------------------------
C
  200 CONTINUE
      IF (NPTS.EQ.0) THEN
          NPTS = RBUF(1)
          START = .TRUE.
          RETURN
      ELSE
          NPTS = NPTS-1
          I0 = NINT(RBUF(1))
          J0 = NINT(RBUF(2))
          IF (START) THEN
              CALL GRFAO('# # BP', L, INSTR, I0, J0, 0, 0)
              START = .FALSE.
              LASTI = I0
              LASTJ = J0
          ELSE IF (NPTS.EQ.0) THEN
              CALL GRFAO('# # EP', L, INSTR, (I0-LASTI), 
     1                     (J0-LASTJ), 0, 0)
              LASTI = -1
              LASTJ = -1
          ELSE
              CALL GRFAO('# # LP', L, INSTR, (I0-LASTI), 
     1                     (J0-LASTJ), 0, 0)
              LASTI = I0
              LASTJ = J0
          END IF
          GOTO 800
      END IF
C
C--- IFUNC=21, Set color representation. -------------------------------
C
  210 CONTINUE
      IF (COLOR) THEN
          CI = RBUF(1)
          RVALUE(CI) = RBUF(2)
          GVALUE(CI) = RBUF(3)
          BVALUE(CI) = RBUF(4)
      END IF
      RETURN
C
C--- IFUNC=22, Set line width. -----------------------------------------
C
  220 CONTINUE
      LW = NINT(RBUF(1))
      CALL GRFAO('# SLW', L, INSTR, LW, 0, 0, 0)
      LASTI = -1
      GOTO 800
C
C--- IFUNC=23, Escape. -------------------------------------------------
C
  230 CONTINUE
      CALL GRPS02(UNIT, CHR(:LCHR))
      LASTI = -1
      RETURN
C-----------------------------------------------------------------------
C Buffer output if possible.
C
  800 IF ( (LOBUF+L+1). GT. 132) THEN
          CALL GRPS02(UNIT, OBUF(1:LOBUF))
          OBUF(1:L) = INSTR(1:L)
          LOBUF = L
      ELSE
          IF (LOBUF.GT.1) THEN
              LOBUF = LOBUF+1
              OBUF(LOBUF:LOBUF) = ' '
          END IF
          OBUF(LOBUF+1:LOBUF+L) = INSTR(1:L)
          LOBUF = LOBUF+L
      END IF
      RETURN
C-----------------------------------------------------------------------
C Error: unimplemented function.
C
  900 WRITE (MSG,
     1  '(''Unimplemented function in PS device driver: '',I10)') IFUNC
      CALL GRWARN(MSG)
      NBUF = -1
      RETURN
C-----------------------------------------------------------------------
      END
