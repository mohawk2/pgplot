C*CGDRIV -- PGPLOT CGI graphics for Sun
C+
      SUBROUTINE CGDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER   IFUNC, NBUF, LCHR
      REAL      RBUF(*)
      CHARACTER CHR*(*)
C
C PGPLOT driver for Sun CGI graphics.  CGI is a low level graphics
C package that is common to all Sun systems (and can easily be called
C from Fortran).  If you include this driver in your version of PGPLOT
C then you will need to include the following line when linking any
C program that uses PGPLOT:
C   -lcgi77 -lcgi -lsunwindow -lpixrect
C
C 1989-May- 5 - Revised version, Fixed to work under OS4.0 [AFT]
C 1989-Jan-28 - Merged Diab Jerius changes into version with cursor,
C               polygon fill, and rectangle fill.  [AFT]
C-----------------------------------------------------------------------
C
C Supported device:  Sun workstation running CGI software.  This driver
C     will only plot onto the Sun Console.
C
C Device type code: /CGI
C
C Default device name: /dev/console (cannot be changed)
C
C Default view surface dimensions:  If you are not running suntools,
C     the full screen is used.  If you are running suntools, then
C     CGI will cause a new window to be opened.  The environment
C     variables PGPLOT_NLNT and PGPLOT_NWNH can be used to override
C     the default position and size of the CGI viewsurface.  The
C     contents of PGPLOT_NLNT should be of the form "left,top", and
C     those of PGPLOT_NWNH should have the form "width,height".
C     For example, the C-shell command
C
C     % setenv PGPLOT_NWNH 1000,1000
C
C     would create a viewsurface 1000 pixels square.
C
C Resolution: Depends on window.  Default window size is 700x700 pixels.
C
C Color capability: On color monitors, indices 0 to 15 are supported.
C     The driver will correctly plot onto black and white screens,
C     however, a (harmless) error message will be displayed when the
C     driver tries to load the color table.
C
C Input capability: Can display a cursor and position it with the mouse.
C     To terminate the cursor read, Press the left mouse button.  PGPLOT
C     expects to read a character, however, this has not been implemented.
C
C File format: It is not possible to send CGI plots to a disk file.
C
C Obtaining hardcopy: Not possible.
C-----------------------------------------------------------------------
      INTEGER    MXCOL, MXPOLY
      PARAMETER (MXCOL=16, MXPOLY=512)
      INTEGER    IC_LOCATOR
      PARAMETER (IC_LOCATOR=0)
C
      INTEGER   CFOPENVWS, CFCOTABLE
C
      CHARACTER CSCREEN*64, CWINDOW*64, CPTR*64, CTMP*64
      CHARACTER MSG*10
      REAL      VAL
      INTEGER   IXPOS(MXPOLY), IYPOS(MXPOLY)
      INTEGER   IER, IRETAIN, IDD, IFLAGS, NAME, IWFD, IC, ISTART
      INTEGER   IDCLASS, IDNUM, ICHOIC, IPICK, ISEG, ITIME
      INTEGER   ICNT, ITRIG, IVALID, IX, IY, MXX, MXY, N, NPTS
      INTEGER   ITMPR, ITMPG, ITMPB
      LOGICAL   APPEND, QFIRST
      INTEGER   IRED(MXCOL), IGREEN(MXCOL), IBLUE(MXCOL)
      SAVE APPEND, CPTR, IDD, MXX, MXY, NPTS, QFIRST
      DATA IRED  /  0,255,255,  0,  0,  0,255,255,
     :              255,127,  0,  0,127,255, 85,170/
      DATA IGREEN/  0,255,  0,255,  0,255,  0,255,
     :              127,255,255,127,  0,  0, 85,170/
      DATA IBLUE /  0,255,  0,  0,255,255,255,  0,
     :                0,  0,127,255,255,127, 85,170/
      DATA NPTS/0/
      DATA QFIRST/.TRUE./
C-----------------------------------------------------------------------
C
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230,240) IFUNC
  900 WRITE (MSG,'(I10)') IFUNC
      CALL GRWARN('Unimplemented function in CGI device driver:'
     1    //MSG)
      NBUF = -1
      RETURN
C
C--- IFUNC = 1, Return device name -------------------------------------
C
   10 CHR = 'CGI'
      LCHR = 3
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices ---------------------------------------
C
   20 IF(QFIRST) CALL GRCG00(CPTR, MXX, MXY, QFIRST)
      RBUF(1) = 0
      RBUF(2) = MXX
      RBUF(3) = 0
      RBUF(4) = MXY
      RBUF(5) = 0
      RBUF(6) = 255
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution -------------------------------
C
   30 RBUF(1) = 100.0
      RBUF(2) = 100.0
      RBUF(3) = 1
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info --------------------------------
C    (This device is Interactive, Cursor, No dashed lines, Area
C    fill, No thick lines, Rectangle).
C
   40 CHR = 'ICNANRNNNN'
      LCHR = 10
      RETURN
C
C--- IFUNC = 5, Return default file name -------------------------------
C
   50 CHR = ' '
      LCHR = 0
      RETURN
C
C--- IFUNC = 6, Return default physical size of plot -------------------
C
   60 IF(QFIRST) CALL GRCG00(CPTR, MXX, MXY, QFIRST)
      RBUF(1) = 0
      RBUF(2) = MXX
      RBUF(3) = 0
      RBUF(4) = MXY
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
      RBUF(1) = 0
      RBUF(2) = 1
      NBUF = 2
C Open CGI package
      CALL CFOPENCGI()
C The graphics window should be redrawn, if uncovered by another window
      IRETAIN=1
C Get environment info.
      CALL GETENV('WINDOW_PARENT', CTMP)
      IF(CTMP.EQ.' ') THEN
C Not a window use CG1DD, a full screen, color frame buffer.
         IDD=3
      ELSE
C In a window, PIXWINDD, a color window.
         IDD=9
      END IF
C Create a new window at specified location
      IFLAGS=1
      IF(QFIRST) CALL GRCG00(CPTR, MXX, MXY, QFIRST)
C Open view surface
      IER=CFOPENVWS(NAME, CSCREEN, CWINDOW, IWFD,
     :   IRETAIN, IDD, MXCOL, 'Color', IFLAGS, CPTR)
C And do a hard reset
      CALL CFHARDRST()
      IF(IER.NE.0) THEN
         WRITE (MSG,'(I10)') IER
         CALL GRWARN('Unable to open VWS, error='//MSG)
         RBUF(2) = 0
         CALL CFCLOSECGI()
         RETURN
      END IF
C Load default color table (on B&W displays this generates an error
C message.
      IER = CFCOTABLE(0,IRED,IGREEN,IBLUE,MXCOL)
C Define extent of viewport coordinates
      CALL CFVDCEXT(0,0,MXX,MXY)
C Disable clipping
      CALL CFCLIPIND(0)
C Area fill should be solid and perimeter invisible.
      CALL CFINTSTYLE(1, 0)
      RETURN
C
C--- IFUNC=10, Close workstation ---------------------------------------
C
  100 CONTINUE
      CALL CFDEACTVWS(NAME)
C      CALL CFCLOSEVWS(NAME)
      IF(IDD.EQ.9) THEN
C- When running in a window, Closing CGI will cause the window to go
C- away.  Therefore prompt user.
         CALL GRPROM
      END IF
      CALL CFCLOSECGI()
      RETURN
C
C--- IFUNC=11, Begin picture -------------------------------------------
C
  110 CONTINUE
      IF (.NOT.APPEND) THEN
         CALL CFCLRVWS(NAME,0,0)
      END IF
      APPEND=.FALSE.
      RETURN
C
C--- IFUNC=12, Draw line -----------------------------------------------
C
  120 CONTINUE
      IXPOS(1) = NINT(RBUF(1))
      IYPOS(1) = NINT(RBUF(2))
      IXPOS(2) = NINT(RBUF(3))
      IYPOS(2) = NINT(RBUF(4))
      CALL CFPOLYLINE(IXPOS,IYPOS,2)
      GOTO 1000
C
C--- IFUNC=13, Draw dot ------------------------------------------------
C Oh dear, this is the tedious way to do it!
C
  130 CONTINUE
      IXPOS(1) = NINT(RBUF(1))
      IYPOS(1) = NINT(RBUF(2))
      IXPOS(2) = IXPOS(1)
      IYPOS(2) = IYPOS(1)
      CALL CFPOLYLINE(IXPOS,IYPOS,2)
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
C- Line color
      CALL CFLNCOLOR(IC)
C- Fill color
      CALL CFFLCOLOR(IC)
      RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C
  160 CONTINUE
      RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
C           RBUF(1)   in/out : cursor x coordinate.
C           RBUF(2)   in/out : cursor y coordinate.
C           CHR(1:1)  output : keystroke.
C
  170 CONTINUE
C- Request locator
      IDCLASS=IC_LOCATOR
      IDNUM=1
      CALL CFINITLID(IDCLASS, IDNUM,
     1   IX, IY, IXPOS, IYPOS, N, VAL, ICHOIC, CTMP, ISEG, IPICK)
C- Look for left button press
      CALL CFASSOC(2, IDCLASS, IDNUM)
C- or right button press
      CALL CFASSOC(4, IDCLASS, IDNUM)
C- Give the user as long as he needs.
  175 ITIME=-1
      CTMP=' '
      CALL CFREQINP(IDCLASS, IDNUM, ITIME, IVALID, ITRIG,
     1   IX, IY, IXPOS, IYPOS, N, VAL, ICHOIC, CTMP, ISEG, IPICK)
      IF(ITRIG.EQ.4) THEN
         WRITE(*,176) CHAR(7)
  176    FORMAT(1X,A1,'Do you really want to exit? ',$)
         READ(*,*) CHR
         IF(CHR(1:1).NE.'Y' .AND. CHR(1:1).NE.'y') GOTO 175
         CALL CFDEACTVWS(NAME)
         CALL CFCLOSECGI()
         STOP
      END IF
      LCHR = 0
      CHR=' '
      CALL CFDISSOC(2, IDCLASS, IDNUM)
      CALL CFDISSOC(4, IDCLASS, IDNUM)
      CALL CFRELIDEV(IDCLASS, IDNUM)
      RBUF(1)=IX
      RBUF(2)=IY
      NBUF = 2
      LCHR = 0
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
C Must store the coordinate pairs until complete polygon has been sent.
C Some care is used so that polygons with more than MXPOLY sides do not
C cause the IxPOS arrays to be overwritten.
C
  200 CONTINUE
      IF (NPTS.EQ.0) THEN
          NPTS = RBUF(1)
          ICNT = 0
          RETURN
      ELSE
          ICNT = ICNT+1
          IF(ICNT.LE.MXPOLY) THEN
              IXPOS(ICNT) = NINT(RBUF(1))
              IYPOS(ICNT) = NINT(RBUF(2))
          END IF
          IF(ICNT.EQ.NPTS) THEN
              CALL CFPOLYGON(IXPOS, IYPOS, MIN(NPTS,MXPOLY))
              NPTS=0
          END IF
      END IF
      RETURN
C
C--- IFUNC=21, Set color representation. -------------------------------
C
  210 CONTINUE
      ISTART=RBUF(1)
      ITMPR=RBUF(2)*255.0
      ITMPG=RBUF(3)*255.0
      ITMPB=RBUF(4)*255.0
      IER = CFCOTABLE(ISTART,ITMPR,ITMPG,ITMPB,1)
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
C--- IFUNC=24, Draw a rectangle ----------------------------------------
C
  240 CONTINUE
      IXPOS(1) = NINT(RBUF(1))
      IYPOS(1) = NINT(RBUF(2))
      IXPOS(2) = NINT(RBUF(3))
      IYPOS(2) = NINT(RBUF(4))
      CALL CFRECTANGLE(IXPOS(1),IYPOS(1),IXPOS(2),IYPOS(2))
      RETURN
C
C--- Send the command. -------------------------------------------------
C
 1000 CONTINUE
C-----------------------------------------------------------------------
      END

      SUBROUTINE GRCG00(CPTR, MXX, MXY, QFIRST)
      CHARACTER CPTR*(*)
      INTEGER   MXX, MXY
      LOGICAL   QFIRST
C
C Routine to generate the initializing string for the CGI window.
C
C Arguments:
C  CPTR   : (output) The character array needed by the CGI software
C  MXX    : (output) The width of the window in pixels
C  MXY    : (output) The height of the window in pixels
C  QFIRST : (output) Always set to .FALSE.
C--
C 1990-Jan-28 - [AFT]
C-----------------------------------------------------------------------
      CHARACTER CTMP*64
      INTEGER   LPTR, LTMP
C---
      QFIRST=.FALSE.
      CALL GRGENV('NLNT', CTMP, LTMP)
      IF(LTMP.GT.0) THEN
         CPTR(:LTMP)=CTMP(:LTMP)//','
         LPTR=LTMP+1
      ELSE
         CPTR(:7)='50,100,'
         LPTR=7
      END IF
C---
      CALL GRGENV('NWNH', CTMP, LTMP)
      IF(LTMP.EQ.0) THEN
         CTMP='700,700'
         LTMP=7
      END IF
      CPTR(LPTR+1:LPTR+LTMP)=CTMP(:LTMP)
      LPTR=LPTR+LTMP
      CPTR(LPTR+1:)=',100,100,64,64,0'
C
      READ(CTMP,*,ERR=900, END=900) MXX, MXY
C---
      RETURN
C---
C Try to cope with improperly set environment variables.
  900 MXX=1000
      MXY=1000
      RETURN
      END
