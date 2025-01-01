C*GRGRAY -- gray-scale map of a 2D data array
C+
      SUBROUTINE GRGRAY (A, IDIM, JDIM, I1, I2, J1, J2,
     1                   BLACK, WHITE, TR)
      INTEGER IDIM, JDIM, I1, I2, J1, J2
      REAL    A(IDIM,JDIM)
      REAL    BLACK, WHITE
      REAL TR(6)
C
C This is a device-dependent support routine for PGGRAY.
C WARNING: this routine and its support routines depend on the PGPLOT
C common block. Such dependencies should be eliminated.
C
C Draw gray-scale map of an array in current window. Array
C values between BLACK and WHITE are shaded in gray levels determined
C by linear interpolation. BLACK may be either less than or greater
C than WHITE.  Array values outside the range BLACK to WHITE are
C shaded black or white as appropriate.
C
C For most devices, GRGRAY calculates a random number for each pixel of
C the picture, and can therefore be very slow. The result is rather
C more successful on some devices than others.
C
C For PostScript devices (/PS, /VPS, /CPS, /VCPS), GRGRAY uses PostScript 
C gray-scale generation, which is quite fast.
C
C For TV-type devices with sufficient bits per pixel, GRGRAY assigns
C 128 gray levels to color-indices 128-255. Note that GRGRAY changes
C the color representation of these color indices.
C
C
C Arguments:
C  A      (input)  : the array to be plotted.
C  IDIM   (input)  : the first dimension of array A.
C  JDIM   (input)  : the second dimension of array A.
C  I1, I2 (input)  : the inclusive range of the first index
C                    (I) to be plotted.
C  J1, J2 (input)  : the inclusive range of the second
C                    index (J) to be plotted.
C  BLACK  (input)  : the array value which is to appear black
C                    (all pixels filled in).
C  WHITE  (input)  : the array value which is to appear white
C                    (no pixels filled in).
C  TR     (input)  : transformation matrix between array grid and
C                    world coordinates (see GRCONT).
C--
C 12-Dec-1986 - Speed up plotting [J. Biretta].
C  3-Apr-1987 - Add special code for /PS, /VPS, /GR.
C  2-Sep-1987 - Adapted from PGGRAY [TJP].
C  1-Dec-1988 - Put random-number generator inline [TJP].
C  3-Apr-1989 - Use "line of pixels" primitive where available [TJP].
C  6-Sep-1989 - Changes for standard Fortran-77 [TJP].
C 19-Jan-1990 - Add special code for /CPS, /VCPS [DLM]
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      INCLUDE 'grpckg1.inc'
      INTEGER  I,IX,IX1,IX2,IY,IY1,IY2,J, NBUF, LCHR
      REAL     DEN,VALUE,BW,RBUF(12)
      REAL     XXAA,XXBB,YYAA,YYBB,XYAA,XYBB,YXAA,YXBB,XYAAIY,YXAAIY
      CHARACTER*16 TYPE, CHR
      LOGICAL  JUNK
      INTEGER  M, IAA, ICC, JRAN, MININD, MAXIND
      REAL     RAND, RM
      PARAMETER (M=714025, IAA=1366, ICC=150889, RM=1.0/M)
      INTRINSIC LEN, MAX, MOD, NINT, REAL
C-----------------------------------------------------------------------
C
C N.B. Arguments are assumed to be valid (checked by PGGRAY).
C
C Test for special devices.
C
      CALL GRQCOL(MININD,MAXIND)
      MININD = MAX(17,MAXIND-127)
      NBUF = 0
      LCHR = LEN(CHR)
      CALL GREXEC(GRGTYP, 4,RBUF,NBUF,CHR,LCHR)
      IF (CHR(7:7).EQ.'P' .AND. MAXIND-MININD .GT. 15) THEN
          CALL GRGRGR(A, IDIM, JDIM, I1, I2, J1, J2,
     1                   BLACK, WHITE, TR, MININD, MAXIND)
          RETURN
      END IF
      CALL GRQTYP(TYPE, JUNK)
      IF (TYPE.EQ.'PS' .OR. TYPE.EQ.'VPS' .OR. TYPE.EQ.'CPS' 
     1    .OR. TYPE.EQ.'VCPS' ) THEN
          CALL GRGRPS(A, IDIM, JDIM, I1, I2, J1, J2,
     1                   BLACK, WHITE, TR)
          RETURN
      END IF
C
C Initialize random-number generator (based on RAN2 of Press et al.,
C Numerical Recipes)
C
      JRAN = 76773
C
      IX1 = XOFF
      IX2 = XOFF+XLEN
      IY1 = YOFF
      IY2 = YOFF+YLEN
      DEN = TR(2)*TR(6)-TR(3)*TR(5)
C
C Calculate constants.
C
C       OLD CALCULATION METHOD:
C              YREL = (IY-YORG)/YSCALE-TR(4)
C              XREL = (IX-XORG)/XSCALE-TR(1)
C              I = NINT((TR(6)*XREL-TR(3)*YREL)/DEN)
C              J = NINT((TR(2)*YREL-TR(5)*XREL)/DEN)
C
      BW = BLACK-WHITE
      XXAA = -TR(6)*(XORG/XSCALE+TR(1))/DEN
      XXBB = TR(6)/DEN/XSCALE
      XYAA = -TR(3)*(YORG/YSCALE+TR(4))/DEN
      XYBB = TR(3)/DEN/YSCALE
      YYAA = -TR(2)*(YORG/YSCALE+TR(4))/DEN
      YYBB = TR(2)/DEN/YSCALE
      YXAA = -TR(5)*(XORG/XSCALE+TR(1))/DEN
      YXBB = TR(5)/DEN/XSCALE
C
      IF(TR(3).EQ.0..AND.TR(5).EQ.0.) THEN
C
C         Vector plotting, without rotation.
C
          DO 20 IY=IY1,IY2
          J = NINT(YYAA+YYBB*IY)
          IF (J.LT.J1.OR.J.GT.J2) GOTO 20
              DO 10 IX=IX1,IX2
                  I = NINT(XXAA+XXBB*IX)
                  IF (I.LT.I1.OR.I.GT.I2) GOTO 10
                  VALUE = (A(I,J)-WHITE)/(BW)
                  JRAN = MOD(JRAN*IAA+ICC, M)
                  RAND = JRAN*RM
                  IF (VALUE.GT.RAND) THEN
                      CALL GRVCT0(3,.TRUE.,1,REAL(IX),REAL(IY))
                  END IF
   10         CONTINUE
   20     CONTINUE
C
      ELSE
C
C         Vector plotting, with rotation.
C
          DO 120 IY=IY1,IY2
          XYAAIY = XXAA-XYAA-XYBB*IY
          YXAAIY = YYAA+YYBB*IY-YXAA
              DO 110 IX=IX1,IX2
                  I = NINT(XYAAIY+XXBB*IX)
                  IF (I.LT.I1.OR.I.GT.I2) GOTO 110
                  J = NINT(YXAAIY-YXBB*IX)
                  IF (J.LT.J1.OR.J.GT.J2) GOTO 110
                  VALUE = (A(I,J)-WHITE)/(BW)
                  JRAN = MOD(JRAN*IAA+ICC, M)
                  RAND = JRAN*RM
                  IF (VALUE.GT.RAND) THEN
                      CALL GRVCT0(3,.TRUE.,1,REAL(IX),REAL(IY))
                  END IF
  110          CONTINUE
  120      CONTINUE
       END IF
C-----------------------------------------------------------------------
       END
