C*GRGRPS -- gray-scale map of a 2D data array (POstScript)
C+
      SUBROUTINE GRGRPS (A, IDIM, JDIM, I1, I2, J1, J2,
     1                   BLACK, WHITE, TR)
      INTEGER IDIM, JDIM, I1, I2, J1, J2
      REAL    A(IDIM,JDIM)
      REAL    BLACK, WHITE
      REAL TR(6)
C
C (This routine is called by GRGRAY when the output device is of type
C PostScript).
C--
C (27-Feb-1987) Special code for /PS by Dale E. Gary.
C (25-Jan-1989) The special code for >512 pixels doesn't work: remove
C               it [TJP].
C (27-Nov-1990) Simplified; standard Fortran [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      INTEGER  I,J,VALUE,NXP,NYP,NXSTEP,NYSTEP
      INTEGER  II, HIGH, LOW
      REAL     AT,B,C,D,TX,TY,T1,T2,T3,T4,T5,T6,FAC
      CHARACTER*80 INLINE
      CHARACTER*1 HEXDIG(0:15)
      DATA HEXDIG/'0','1','2','3','4','5','6','7',
     1            '8','9','A','B','C','D','E','F'/
C-----------------------------------------------------------------------
      NXSTEP = 1
      NYSTEP = 1
      NXP = I2 - I1 + 1
      NYP = J2 - J1 + 1
C
C Set clipping rectangle in device.
C
      CALL GRTERM
      CALL GRESC('gsave')
      CALL GRESC('newpath ')
      WRITE (INLINE, 900) NINT(XOFF), NINT(YOFF), NINT(XLEN),NINT(YLEN),
     *          -NINT(XLEN)
900   FORMAT(I6,I6,' moveto ',I6, ' 0 rlineto  0 ',I6,' rlineto ',
     *I6,' 0 rlineto')
      CALL GRESC(INLINE)
      CALL GRESC('closepath clip ')
C
C Build an image transformation matrix.
C
      T1 = TR(1)*XSCALE + XORG
      T2 = TR(2)*XSCALE
      T3 = TR(3)*XSCALE
      T4 = TR(4)*YSCALE + YORG
      T5 = TR(5)*YSCALE
      T6 = TR(6)*YSCALE
      FAC = T2*T6 - T3*T5
      AT = T6/FAC
      B = -T5/FAC
      C = -T3/FAC
      D = T2/FAC
      TX = (T3*T4 - T1*T6)/FAC - (I1-0.5)
      TY = (T5*T1 - T4*T2)/FAC - (J1-0.5)
C
C Use a PostScript "image" operator.
C
      WRITE (INLINE, '(A,I5,A)') '/picstr ',NXP,' string def'
      CALL GRESC(INLINE)
      WRITE (INLINE, 901)   NXP, NYP, AT, B, C, D, TX, TY
  901 FORMAT(2I4,' 8 [',6(1PE10.3,' '),']')
      CALL GRESC(INLINE)
      CALL GRESC('{ currentfile picstr readhexstring pop} image')
C
C Write out the image array in hexadecimal.
C
      II = 0
      DO 20 J = J1,J2,NYSTEP
          DO 10 I = I1,I2,NXSTEP
              II = II + 1
              VALUE = NINT((A(I,J) - BLACK)*255./(WHITE - BLACK))
              VALUE = MIN(MAX(VALUE,0),255)
              HIGH = VALUE/16
              LOW  = VALUE-16*HIGH
              INLINE(2*II-1:2*II-1) = HEXDIG(HIGH)
              INLINE(2*II:2*II) = HEXDIG(LOW)
              IF (II .EQ. 32) THEN
                  CALL GRESC(INLINE(1:2*II))
                  II = 0
              END IF
   10     CONTINUE
   20 CONTINUE
      IF (II.GT.0) CALL GRESC(INLINE(1:2*II))
      CALL GRESC('grestore')
      CALL GRTERM
      END
