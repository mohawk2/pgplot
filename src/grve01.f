!
C*GRVE01 -- PGPLOT Versatec driver, draw line
C+
      SUBROUTINE GRVE01 (LINE,RBUF,ICOL, BX, BY, BITMAP)
      INTEGER LINE
      REAL RBUF(4)
      INTEGER ICOL, BX, BY
      BYTE BITMAP(BX,BY)
C
C Draw a straight-line segment from absolute pixel coordinates
C (RBUF(1),RBUF(2)) to (RBUF(3),RBUF(4)).  The line either overwrites
C (sets to black) or erases (sets to white) the previous contents
C of the bitmap, depending on the current color index. Setting bits
C is accomplished with a VMS BISB2 instruction, expressed in
C Fortran as .OR.; clearing bits is accomplished with a VMS BICB2
C instruction, expressed in Fortran as .AND..NOT.. The line is
C generated with a Simple Digital Differential Analyser (ref:
C Newman & Sproull). 
C
C Arguments:
C
C LINE            I I      =0 for dot, =1 for line.
C RBUF(1),RBUF(2) I R      Starting point of line.
C RBUF(3),RBUF(4) I R      End point of line.
C ICOL            I I      =0 for erase, =1 for write.
C BITMAP        I/O B      (address of) the frame buffer.
C
C-----------------------------------------------------------------------
      BYTE    QMASK(0:7)
      INTEGER IXDIM, LENGTH, KX, KY, K
      REAL    D, XINC, YINC, XP, YP
      DATA    QMASK /'80'x,'40'x,'20'x,'10'x,'08'x,'04'x,'02'x,'01'x/
C
      IF (LINE.GT.0) THEN
          D = MAX(ABS(RBUF(3)-RBUF(1)), ABS(RBUF(4)-RBUF(2)))
          LENGTH = D
          IF (LENGTH.EQ.0) THEN
              XINC = 0.
              YINC = 0.
          ELSE
              XINC = (RBUF(3)-RBUF(1))/D
              YINC = (RBUF(4)-RBUF(2))/D
          END IF
      ELSE
          LENGTH = 0
          XINC = 0.
          YINC = 0.
      END IF
      XP = RBUF(1)+0.5
      YP = RBUF(2)+0.5
      IF (ICOL.NE.0) THEN
          DO K=0,LENGTH
              KX = XP
              KY = (BY-1)-INT(YP)
              BITMAP(KX/8+1,KY+1) = BITMAP(KX/8+1,KY+1) .OR. 
     1                              QMASK(MOD(KX,8))
              XP = XP + XINC
              YP = YP + YINC
          END DO
      ELSE
          DO K=0,LENGTH
              KX = XP
              KY = (BY-1)-INT(YP)
              BITMAP(KX/8+1,KY+1) = BITMAP(KX/8+1,KY+1) .AND.
     1                              (.NOT.QMASK(MOD(KX,8)))
              XP = XP + XINC
              YP = YP + YINC
          END DO
      END IF
      END
