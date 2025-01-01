C*PGSCH -- set character height
C+
      SUBROUTINE PGSCH (SIZE)
      REAL SIZE
C
C Set the character size attribute. The size affects all text and graph
C markers drawn later in the program. The default character size is
C 1.0, corresponding to a character height about 1/40 the height of
C the view surface.  Changing the character size also scales the length
C of tick marks drawn by PGBOX and terminals drawn by PGERRX and PGERRY.
C
C Argument:
C  SIZE   (input)  : new character size (dimensionless multiple of
C                    the default size).
C--
C (1-Mar-1983)
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      REAL     XC, XCNEW, YC, XS, YS
C
      IF (PGOPEN.EQ.0) RETURN
C
      CALL GRCHSZ(IDENT, XC, YC, XS, YS)
      IF (XSZ/XPERIN .GT.YSZ/YPERIN) THEN
          XCNEW = SIZE*XC*YSZ/YS/40.0
      ELSE
          XCNEW = SIZE*XC*(XSZ*YPERIN/XPERIN)/YS/40.0
      END IF
      CALL GRSETC(IDENT,XCNEW)
      XSP = XS*XCNEW/XC
      YSP = YS*XCNEW/XC
      PGCHSZ = SIZE
      END
