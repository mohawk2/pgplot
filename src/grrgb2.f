C*GRIMG2 -- image of a 2D data array (pixel-primitive devices)
C
C The routine works by changing the color table of the device every
C few pixels.  It's moderately slow but at least it works in the 
C existing device framework.  At the end, the original color table is
C restored. 
C
C To avoid aliasing on image reduction, the original image pixels are
C averaged over the `footprint' of each device pixel in the array index
C plane.
C
C+
      SUBROUTINE GRRGB2 (A, IDIM, JDIM, I1, I2, J1, J2,
     $                   A1, A2, PA, MININD, MAXIND)
      INTEGER IDIM, JDIM, I1, I2, J1, J2, MININD, MAXIND
      REAL    A(IDIM,JDIM,3)
      REAL    A1, A2
      REAL    PA(6)
C
C (This routine is called by GRRGB0.)
C--
C 17-Jan-2003 New routine [CED] based on GRIMG2
C 14-Aug-2003 Switched to anti-aliasing algorithm [CED]
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER  I,IX,IX1,IX2,IY,IY1,IY2,J, NPIX, LCHR,II,III,JJ,JJJ
      REAL    PINV(6)
      REAL    PPA(6)
      INTEGER ISV
      REAL    DI,DJ, XX0, XY0

      REAL     BUFFER(260)
      REAL     OCT(3,256)
      CHARACTER*1 CHR
      INTRINSIC NINT, LOG, SQRT
      INTEGER  OCI, CI
      REAL     CBUF(4)
      REAL    ATMP,BTMP

C-----------------------------------------------------------------------
C
C Color range
C
      ARANGE = A2 - A1
      if(ARANGE.LE.0) THEN
         CALL GRWARN('Invalid RGB value range')
         RETURN
      END IF

      if(MININD.le.16) MININD = 16
      if(MAXIND-MININD.ge.240) MAXIND=MININD+239

C 
C Old plotting color
C     
      CALL PGQCI(OCI)

C
C Store old color map
C
      DO 50 I=MININD,MAXIND
         CBUF(1) = I
         CALL GREXEC(GRGTYP,29,CBUF,J,CHR,LCHR)
         OCT(1,I) = CBUF(2)
         OCT(2,I) = CBUF(3)
         OCT(3,I) = CBUF(4)
 50   CONTINUE

C
C Location of current window in device coordinates.
C
      IX1 = NINT(GRXMIN(GRCIDE))+1
      IX2 = NINT(GRXMAX(GRCIDE))-1
      IY1 = NINT(GRYMIN(GRCIDE))+1
      IY2 = NINT(GRYMAX(GRCIDE))-1

C
C Prepare the PPA and PINV matrices, and ISV.
C 
      CALL GRSVD( PA, PINV, PPA, ISV)
      
C
C Start a new page if necessary.
C
      IF (.NOT.GRPLTD(GRCIDE)) CALL GRBPIC
C
C Run through every device pixel (IX, IY) in the current window and
C determine which array pixel (I,J) it falls in.  Use that pixel as the 
C basis for the range of pixels that need to be considered.
C
      DO 120 IY=IY1,IY2
          NPIX = 0
          CI = MININD
          BUFFER(2) = IY

          XY0 = PINV(1) + PINV(3)*IY
          YY0 = PINV(4) + PINV(6)*IY

          DO 110 IX=IX1,IX2
             CBUF(1) = CI
             
             DI = XY0 + PINV(2)*IX
             I = NINT(DI)
             DI = DI - I
             
             IF (I.LT.I1.OR.I.GT.I2) GOTO 110
             DJ = YY0 + PINV(5)*IX
             J = NINT(DJ)
             DJ = DJ - J
             
             IF (J.LT.J1.OR.J.GT.J2) GOTO 110
             
             
C Weight is set to be nonzero but negligible.
             WGT = 1E-9
             CBUF(2) = 0
             CBUF(3) = 0
             CBUF(4) = 0
             
C
C Calculate the weighted average of the surrounding array elements
C

             DO 104 II = -ISV,ISV
                III = I + II
                IF (III .LT. I1 .OR. III .GT. I2) GOTO 104
                
                DO 102 JJ = -ISV,ISV
                   JJJ = J + JJ
                   IF (JJJ .LT. J1 .OR. JJJ .GT. J2) GOTO 102
                   
C Find the distance in device space using the padded Taylor expansion
                   DX = ABS ( (II-DI) * PPA(2) + (JJ-DJ) * PPA(3) )
                   DY = ABS ( (II-DI) * PPA(5) + (JJ-DJ) * PPA(6) )
                   
                   
C     Apply a cosine rolloff (factor-of-2 comes out in the wash)
C     cosine is too slow - use linear.
                   IF ((DX.LT.1) .AND. (DY.LT.1)) THEN
C                      ATMP =  (1 + cos(3.14159*DX)) *
C     $                     (1 + cos(3.14159*DY))
                      ATMP = (1-DX) * (1-DY)
                      WGT = WGT + ATMP
                      CBUF(2) = CBUF(2) +
     $                     ATMP * (A(III,JJJ,1) - A1)/ARANGE
                      CBUF(3) = CBUF(3) +
     $                     ATMP * (A(III,JJJ,2) - A1)/ARANGE
                      CBUF(4) = CBUF(4) +
     $                     ATMP * (A(III,JJJ,3) - A1)/ARANGE
                   ENDIF
                   
C     
C     -- Set color table entry
C     
 102            CONTINUE
 104         CONTINUE
             
C     Now send the pixel to its destination   
             IF(WGT.EQ.0) WGT = 1
             CBUF(2) = CBUF(2) / WGT
             CBUF(3) = CBUF(3) / WGT
             CBUF(4) = CBUF(4) / WGT
             
             CALL GREXEC(GRGTYP,21,CBUF,4,CHR,LCHR)
             
             NPIX = NPIX+1
             IF (NPIX.EQ.1) BUFFER(1) = IX
             BUFFER(NPIX+2) = CI
             
             CI = CI + 1
             
             IF (CI.gt.MAXIND) THEN
                IF(NPIX.GT.0) THEN
                   IF(NPIX.GT.0) CALL
     :               GREXEC(GRGTYP, 26, BUFFER, NPIX+2, CHR, LCHR)
                end if
                
                CI = MININD
                NPIX = 0
             END IF
             
 110      CONTINUE
          IF (NPIX.GT.0) CALL 
     :         GREXEC(GRGTYP, 26, BUFFER, NPIX+2, CHR, LCHR)
 120   CONTINUE
C-----------------------------------------------------------------------
       
C     
C     Restore old color map
C     
       DO 150 I=MININD,MAXIND
          CBUF(1) = I
          CBUF(2) = OCT(1,I)
          CBUF(3) = OCT(2,I)
          CBUF(4) = OCT(3,I)
          CALL GREXEC(GRGTYP,21,CBUF,4,CHR,LCHR)
 150   CONTINUE
       
C     
C     Restore old olotting color
C     
       CALL PGSCI(OCI)
       
       END
      
