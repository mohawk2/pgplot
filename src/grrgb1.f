C*GRIMG1 -- RGB image of a 3D data array (image-primitive devices)
C The routine works by changing the color table of the device every
C few pixels.  It's moderately slow but at least it works in the existing
C device framework.  At the end, the original color table is restored.
C
C Only color table elements between MININD and MAXIND are used.
C
C+
      SUBROUTINE GRRGB1 (A, IDIM, JDIM, I1, I2, J1, J2,
     1                   A1, A2, PA, MININD, MAXIND)
      INTEGER IDIM, JDIM, I1, I2, J1, J2, MININD, MAXIND
      REAL    A(IDIM,JDIM,3), A1, A2, PA(6)
C
C (This routine is called by GRRGB0.)
C--
C 17-Jan-2003 New routine [Craig DeForest].
C
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER NBUF, LCHR
      REAL    RBUF(21), FAC,  SFAC, SFACL, RV, GV, BV, ARANGE
      CHARACTER*1 CHR
      REAL ORVAL(1024), OGVAL(1024), OBVAL(1024)
      INTEGER  I, J, II, NXP, NYP
      INTRINSIC NINT, LOG
      PARAMETER (SFAC=65000.0)
      INTEGER NCOLS
C-----------------------------------------------------------------------
C Number of color indices available
      NCOLS = MAXIND - MININD + 1
      IF (NCOLS.LT.1) THEN
         CALL GRWARN('Even RGB images need a few color table entries')
         RETURN
      END IF
      IF (NCOLS.GT.20) THEN
         NCOLS = 20
      END IF

      ARANGE = A2 - A1
      if(ARANGE.LE.0) THEN
         CALL GRWARN('Invalid RGB value range')
         RETURN
      END IF

C Store old color table
      DO 5 I=0,NCOLS
         CALL PGQCR(I+MININD,RV,GV,BV)
         ORVAL(i) = RV
         OGVAL(i) = GV
         OBVAL(i) = BV
 5    CONTINUE

C Size of image.
C
      NXP = I2 - I1 + 1
      NYP = J2 - J1 + 1
      RBUF(1) = 0.0
      RBUF(2) = NXP
      RBUF(3) = NYP
C
C Clipping rectangle.
C
      RBUF(4) = GRXMIN(GRCIDE)
      RBUF(5) = GRXMAX(GRCIDE)
      RBUF(6) = GRYMIN(GRCIDE)
      RBUF(7) = GRYMAX(GRCIDE)
C
C Image transformation matrix.
C
      FAC = PA(2)*PA(6) - PA(3)*PA(5)
      RBUF(8)  =  PA(6)/FAC
      RBUF(9)  = (-PA(5))/FAC
      RBUF(10) = (-PA(3))/FAC
      RBUF(11) =  PA(2)/FAC
      RBUF(12) = (PA(3)*PA(4) - PA(1)*PA(6))/FAC - (I1-0.5)
      RBUF(13) = (PA(5)*PA(1) - PA(4)*PA(2))/FAC - (J1-0.5)
C
C Send setup info to driver.
C
      IF (.NOT.GRPLTD(GRCIDE)) CALL GRBPIC
      CALL GRTERM
      NBUF = 13
      LCHR = 0
      CALL GREXEC(GRGTYP, 26, RBUF, NBUF, CHR, LCHR)
C
C Convert image array to color indices and send to driver.
C
      SFACL = LOG(1.0+SFAC)
      II = 0

      DO 20 J = J1,J2
          DO 10 I = I1,I2

C Scale RGB values to between 0 and 1
              RV = ( A(I,J,1) - A1 ) / ARANGE
              GV = ( A(I,J,2) - A1 ) / ARANGE
              BV = ( A(I,J,3) - A1 ) / ARANGE
C Increment index and stuff current index into the RBUF
              II = II + 1
              CALL PGSCR(II+MININD,RV,GV,BV)
              RBUF(II+1) = II+MININD

              IF (II.EQ.NCOLS) THEN
                  NBUF = II + 1
                  RBUF(1) = II
                  CALL GREXEC(GRGTYP, 26, RBUF, NBUF, CHR, LCHR)
                  II = 0
              END IF
   10     CONTINUE
   20 CONTINUE
      IF (II.GT.0) THEN
          NBUF = II + 1
          RBUF(1) = II
          CALL GREXEC(GRGTYP, 26, RBUF, NBUF, CHR, LCHR)
          II = 0
      END IF
C
C Send termination code to driver.
C
      NBUF = 1
      RBUF(1) = -1
      CALL GREXEC(GRGTYP, 26, RBUF, NBUF, CHR, LCHR)

C
C Restore color table to previous values
C
      DO 30 I=0,NCOLS
         RV = ORVAL(i)
         GV = OGVAL(i)
         BV = OBVAL(i)
         CALL PGSCR(I+MININD,RV,GV,BV)
 30   CONTINUE

C-----------------------------------------------------------------------
      END
