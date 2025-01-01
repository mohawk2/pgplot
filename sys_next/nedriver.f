C*NEDRIV -- PGPLOT Nextstep driver (use with pgview display program)
C+
      SUBROUTINE NEDRIV(IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER   IFUNC, NBUF, LCHR
      REAL      RBUF(*)
      CHARACTER CHR*(*)
C
C PGPLOT driver for
C
C 1991-Jul-02 - New version - [AFT]
C
C Supported device:
C
C Device type code: /next
C
C Default device name:
C
C Default view surface dimensions:
C
C Resolution:
C
C Color capability:
C
C Input capability:
C
C File format:
C
C Obtaining hardcopy:
C
C Supported systems:
C
C Required routines:
C-----------------------------------------------------------------------
      IMPLICIT NONE
C (XLEN,YLEN) is the size of the window including the frame in pixels.
      REAL      XLEN, YLEN
      PARAMETER (XLEN=720., YLEN=540.)
C
      CHARACTER CMSG*10
      REAL      SHADE(0:15)
      SAVE      SHADE
      REAL      RECT(4), XLAS, YLAS
      SAVE      RECT,    XLAS, YLAS
      INTEGER   ICNT, NPTS
      SAVE      ICNT, NPTS
      LOGICAL   APPEND, QSTART
      SAVE      APPEND, QSTART
      DATA XLAS,YLAS/-1.,-1./
      DATA SHADE /1.00, 13*0.00, 0.67, 0.33/
C-----------------------------------------------------------------------
C
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230), IFUNC
  900 WRITE (CMSG,'(I10)') IFUNC
      CALL GRWARN('Unimplemented function in NeXT device driver:'
     1    //CMSG)
      NBUF = -1
      RETURN
C
C--- IFUNC = 1, Return device name -------------------------------------
C
   10 CONTINUE
      CHR = 'NEXT'
      LCHR = 4
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices ---------------------------------------
C
   20 CONTINUE
      RBUF(1) = 0.
      RBUF(3) = 0.
      RBUF(5) = 0.
      RBUF(2) = XLEN-2.
      RBUF(4) = YLEN-2.
      RBUF(6) = 15
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution -------------------------------
C
   30 CONTINUE
      RBUF(1) = 92.0
      RBUF(2) = 92.0
      RBUF(3) = 1
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info --------------------------------
C    (This device is Hardcopy, No cursor, No dashed lines, Area fill, 
C    Thick lines)
C
   40 CONTINUE
      CHR = 'INNATNNNNN'
      LCHR = 10
      RETURN
C
C--- IFUNC = 5, Return default file name -------------------------------
C
   50 CONTINUE
      RETURN
C
C--- IFUNC = 6, Return default physical size of plot -------------------
C
   60 CONTINUE
      RBUF(1) = 0.
      RBUF(3) = 0.
      RBUF(2) = XLEN-2.
      RBUF(4) = YLEN-2.
      NBUF = 4
      RETURN
C
C--- IFUNC = 7, Return misc defaults -----------------------------------
C
   70 CONTINUE
      RBUF(1) = 10.0
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
C Set up the plotting Window.  This driver can support up to two windows.
      CALL nexsup(0,0.0,0.0)
C Move bottom left corner in one pixel in order to avoid erasing the
C frame.  This is formally in error, since PGPLOT can write to that
C first pixel.  There must be a better way to erase the rectangle.
      RECT(1)=1.0
      RECT(2)=1.0
      RBUF(2) = 1
      RETURN
C
C--- IFUNC=10, Close workstation ---------------------------------------
C
  100 CONTINUE
      RETURN
C
C--- IFUNC=11, Begin picture -------------------------------------------
C
  110 CONTINUE
      IF(.NOT.APPEND) THEN
         CALL nexsup(1,0.0,0.0)
      END IF
      APPEND=.FALSE.
      XLAS=-1.
      ICNT=0
      RETURN
C
C--- IFUNC=12, Draw line -----------------------------------------------
C NOTE: Display Postscript does not round numbers but truncates.
C
  120 CONTINUE
      RBUF(1)=RBUF(1)
      RBUF(2)=RBUF(2)
      RBUF(3)=RBUF(3)
      RBUF(4)=RBUF(4)
      IF(RBUF(1).EQ.XLAS .AND. RBUF(2).EQ.YLAS .AND. ICNT.LT.1000) THEN
         CALL nexsup(3,RBUF(3),RBUF(4))
         ICNT=ICNT+1
      ELSE
         CALL nexsup(2,RBUF(1),RBUF(2))
         CALL nexsup(3,RBUF(3),RBUF(4))
         ICNT=0
      END IF
      XLAS = RBUF(3)
      YLAS = RBUF(4)
      RETURN
C
C--- IFUNC=13, Draw dot ------------------------------------------------
C
  130 CONTINUE
      RBUF(1)=RBUF(1)
      RBUF(2)=RBUF(2)
      CALL nexsup(2,RBUF(1),RBUF(2))
      CALL nexsup(4,0.0,0.0)
      XLAS = RBUF(1)
      YLAS = RBUF(2)
      RETURN
C
C--- IFUNC=14, End picture ---------------------------------------------
C
  140 CONTINUE
      CALL nexsup(5,0.0,0.0)
      XLAS=-1.
      RETURN
C
C--- IFUNC=15, Select color index --------------------------------------
C
  150 CONTINUE
      CALL nexsup(6,SHADE(NINT(RBUF(1))),0.0)
      XLAS=-1.
      RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C
  160 CONTINUE
      CALL nexsup(5,0.0,0.0)
      XLAS=-1.
      RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
C           RBUF(1)   in/out : cursor x coordinate.
C           RBUF(2)   in/out : cursor y coordinate.
C           CHR(1:1)  output : keystroke.
C
  170 CONTINUE
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
      IF (NPTS.EQ.0) THEN
         NPTS = RBUF(1)
         QSTART = .TRUE.
         RETURN
      ELSE
         NPTS = NPTS-1
         RBUF(1)=RBUF(1)
         RBUF(2)=RBUF(2)
         IF (QSTART) THEN
            QSTART = .FALSE.
            CALL nexsup(5,0.0,0.0)
            CALL nexsup(2,RBUF(1),RBUF(2))
            XLAS = RBUF(1)
            YLAS = RBUF(2)
         ELSE IF(NPTS.GT.0) THEN
            CALL nexsup(3,RBUF(1),RBUF(2))
            XLAS = RBUF(1)
            YLAS = RBUF(2)
         ELSE
            CALL nexsup(3,RBUF(1),RBUF(2))
            CALL nexsup(7,0.0,0.0)
            XLAS = -1.
            YLAS = -1.
            ICNT=0
         END IF
      END IF
      RETURN
C
C--- IFUNC=21, Set color representation. -------------------------------
C    (Not implemented: ignored)
C
  210 CONTINUE
      RETURN
C
C--- IFUNC=22, Set line width. -----------------------------------------
C
  220 CONTINUE
      XLAS = -1.
      CALL nexsup(8,RBUF(1),0.0)
      RETURN
C
C--- IFUNC=23, Escape --------------------------------------------------
C    (Not implemented: ignored)
C
  230 CONTINUE
      RETURN
C-----------------------------------------------------------------------
      END
