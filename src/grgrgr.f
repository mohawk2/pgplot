C*GRGRGR -- gray-scale map of a 2D data array (pixel devices)
C+
      SUBROUTINE GRGRGR (A, IDIM, JDIM, I1, I2, J1, J2,
     1                   BLACK, WHITE, TR, MININD, MAXIND)
      INTEGER IDIM, JDIM, I1, I2, J1, J2, MININD, MAXIND
      REAL    A(IDIM,JDIM)
      REAL    BLACK, WHITE
      REAL TR(6)
C
C (This routine is called by GRGRAY.)
C  6-Sep-1989 - Changes for standard Fortran-77 [TJP].
C 26-Nov-1990 - [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      INCLUDE 'grpckg1.inc'
      INTEGER  I,IV,IX,IX1,IX2,IY,IY1,IY2,J, NPIX, LCHR
      REAL     DEN,VALUE,BW
      REAL     XXAA,XXBB,YYAA,YYBB,XYAA,XYBB,YXAA,YXBB,XYAAIY,YXAAIY
      REAL     BUFFER(1026)
      CHARACTER*1 CHR
      INTRINSIC NINT, REAL
C-----------------------------------------------------------------------
C
C Initialize color map: linear gray scale in color indices 
C MININD-MAXIND.
C
      DO 5 I=MININD,MAXIND
          VALUE = REAL(I-MININD)/REAL(MAXIND-MININD)
          CALL GRSCR(I, VALUE, VALUE, VALUE)
    5 CONTINUE
C
      IX1 = XOFF
      IX2 = XOFF+XLEN
      IY1 = YOFF
      IY2 = YOFF+YLEN
      DEN = TR(2)*TR(6)-TR(3)*TR(5)
      BW = (BLACK-WHITE)/REAL(MAXIND-MININD)
      XXAA = -TR(6)*(XORG/XSCALE+TR(1))/DEN
      XXBB = TR(6)/DEN/XSCALE
      XYAA = -TR(3)*(YORG/YSCALE+TR(4))/DEN
      XYBB = TR(3)/DEN/YSCALE
      YYAA = -TR(2)*(YORG/YSCALE+TR(4))/DEN
      YYBB = TR(2)/DEN/YSCALE
      YXAA = -TR(5)*(XORG/XSCALE+TR(1))/DEN
      YXBB = TR(5)/DEN/XSCALE
C
      DO 120 IY=IY1,IY2
          XYAAIY = XXAA-XYAA-XYBB*IY
          YXAAIY = YYAA+YYBB*IY-YXAA
          NPIX = 0
          BUFFER(2) = IY
          DO 110 IX=IX1,IX2
              I = NINT(XYAAIY+XXBB*IX)
              IF (I.LT.I1.OR.I.GT.I2) GOTO 110
              J = NINT(YXAAIY-YXBB*IX)
              IF (J.LT.J1.OR.J.GT.J2) GOTO 110
              IV = MININD + NINT((A(I,J)-WHITE)/BW)
              IF (IV.LT.MININD) IV = MININD
              IF (IV.GT.MAXIND) IV = MAXIND
              NPIX = NPIX+1
              IF (NPIX.EQ.1) BUFFER(1) = IX
              BUFFER(NPIX+2) = IV
  110     CONTINUE
          NPIX = NPIX+2
          CALL GREXEC(GRGTYP, 26, BUFFER, NPIX, CHR, LCHR)
  120 CONTINUE
C-----------------------------------------------------------------------
      END
