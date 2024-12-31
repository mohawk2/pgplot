C*GRIMG2 -- image of a 2D data array (pixel-primitive devices)
C+
      SUBROUTINE GRIMG2 (A, IDIM, JDIM, I1, I2, J1, J2,
     1                   A1, A2, PA, MININD, MAXIND, MODE)
      INTEGER IDIM, JDIM, I1, I2, J1, J2, MININD, MAXIND, MODE
      REAL    A(IDIM,JDIM)
      REAL    A1, A2
      REAL    PA(6)
C
C (This routine is called by GRIMG0.)
C--
C 7-Sep-1994  New routine [TJP].
C 19-Aug-2003 Modified to use optimized resampling [CED]
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER  I,IV,IX,IX1,IX2,IY,IY1,IY2,J, NPIX, LCHR
      REAL     AV, SFAC, SFACL
      REAL     BUFFER(1026)
      CHARACTER*1 CHR
      INTRINSIC NINT, LOG
      PARAMETER (SFAC=65000.0)

      REAL     PPA(6),PINV(6), WGT, XY0, YY0, DI, DJ
      INTEGER  II,III,JJ,JJJ
      INTEGER  ISV

C-----------------------------------------------------------------------
C
C Location of current window in device coordinates.
C
      IX1 = NINT(GRXMIN(GRCIDE))+1
      IX2 = NINT(GRXMAX(GRCIDE))-1
      IY1 = NINT(GRYMIN(GRCIDE))+1
      IY2 = NINT(GRYMAX(GRCIDE))-1

C
C Prepare the PPA and PINV matrices, and ISV
C
      CALL GRSVD( PA, PINV, PPA, ISV )

C
C Start a new page if necessary.
C
      IF (.NOT.GRPLTD(GRCIDE)) CALL GRBPIC
C
C Run through every device pixel (IX, IY) in the current window and
C determine which array pixel (I,J) it falls in.
C
      SFACL = LOG(1.0+SFAC)
      DO 120 IY=IY1,IY2
          NPIX = 0
          BUFFER(2) = IY

          XY0 = PINV(1) + PINV(3)*IY
          YY0 = PINV(4) + PINV(6)*IY

          DO 110 IX=IX1,IX2

             DI = XY0 + PINV(2)*IX 
             I = NINT(DI)
             DI = DI - I
             
             IF (I.LT.I1.OR.I.GT.I2) GOTO 110
             DJ = YY0 + PINV(5)*IX
             J = NINT(DJ)
             DJ = DJ - J

C Weight is set to be nonzero but negligible.
             WGT = 1E-9
             AV = 0

C
C Calculate the weighted average of the surrounding array elements
C
             DO 104 II=-ISV,ISV
                III = I + II
                IF(III.LT.I1 .OR. III.GT.I2) GOTO 104
                
                DO 102 JJ = -ISV,SV
                   JJJ = J + JJ
                   IF(JJJ.LT.J1 .OR. JJJ.GT.J2) GOTO 102
                   
C     Find the distance in device space using the padded Taylor expansion
                   DX = ABS ( (II-DI) * PPA(2) + (JJ-DJ) * PPA(3) )
                   DY = ABS ( (II-DI) * PPA(5) + (JJ-DJ) * PPA(6) )
                   
C     Apply a cosine rolloff (factor-of-2 comes out in the wash)
C     Cosine is too slow -- use a linear rolloff instead...
                   IF ((DX.LT.1) .AND. (DY.LT.1)) THEN
C                      ATMP =  (1 + cos(3.14159*DX)) *
C     $                     (1 + cos(3.14159*DY))
                       ATMP = (1 - DX) * (1-DY)
                      WGT = WGT + ATMP
                      AV = AV + ATMP * A(III,JJJ)
                   ENDIF
                   
 102            CONTINUE
 104         CONTINUE

             AV = AV / WGT
             
             IF (A2.GT.A1) THEN
                AV = MIN(A2, MAX(A1,AV))
             ELSE
                AV = MIN(A1, MAX(A2,AV))
             END IF
             IF (MODE.EQ.0) THEN
                IV = NINT((MININD*(A2-AV) + MAXIND*(AV-A1))/(A2-A1))
             ELSE IF (MODE.EQ.1) THEN
                IV = MININD + NINT((MAXIND-MININD)*
     :               LOG(1.0+SFAC*ABS((AV-A1)/(A2-A1)))/SFACL)
             ELSE IF (MODE.EQ.2) THEN
                IV = MININD + NINT((MAXIND-MININD)*
     :               SQRT(ABS((AV-A1)/(A2-A1))))
             ELSE
                IV = MININD
             END IF
C     
             IF (NPIX.LE.1024) THEN
C     -- drop pixels if buffer too small (to be fixed!)
                NPIX = NPIX+1
                IF (NPIX.EQ.1) BUFFER(1) = IX
                BUFFER(NPIX+2) = IV
             END IF
 110      CONTINUE
          IF (NPIX.GT.0) CALL 
     :         GREXEC(GRGTYP, 26, BUFFER, NPIX+2, CHR, LCHR)
 120   CONTINUE
C-----------------------------------------------------------------------
      END
