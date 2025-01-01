      PROGRAM HERSHEY
C----------------------------------------------------------------------
C Generate table of all known Hershey characters.
C (For Appendix B of the PGPLOT Manual.)
C As this program uses the GRPCKG internal routine GRSYXD, it cannot
C be linked with the shareable library.
C The program produces 7 separate files, one for each page of the
C table. The device type (/VPS) needs to be changed if you don't
C have a PostScript printer.
C                              T. J. Pearson  1989 Jul 5
C----------------------------------------------------------------------
      CHARACTER*80 DEVICE
      INTEGER PGBEGIN
      INTEGER        XYGRID(300),PAGE
      REAL           XC,YC
      LOGICAL        UNUSED,MOVE
      REAL           X(5)
      CHARACTER*8    LABEL
C
C Open device and initialize
C
      DEVICE = 'figb1.ps/vps'
      IF (PGBEGIN(0,DEVICE,1,1).NE.1) CALL EXIT
      WRITE (*,*) DEVICE
      CALL PGBBUF
      CALL PGSCH(0.25)
      PAGE = 1
      NX = 10
      NY = 15
      IX = NX
      JY = 1
      DX = 0.5
      DY = 0.5
      N = 0
C
C Loop through all known symbols
C
   20 N = N+1
      IF (N.GT.4000) GOTO 30
      CALL GRSYXD(N,XYGRID,UNUSED)
      IF (UNUSED) GOTO 20
      WRITE (LABEL,'(I4.4)') N
C
      IX = IX+1
      IF (IX.GT.NX) THEN
          IX = 1
          JY = JY-1
      END IF
      IF (JY.LT.1) THEN
          JY = NY
          IF (PAGE.NE.1) THEN
              CALL PGEBUF
              CALL PGEND
              WRITE (DEVICE(5:5), '(I1)') PAGE
              IF (PGBEGIN(0,DEVICE,1,1).NE.1) CALL EXIT
              WRITE (*,*) DEVICE
              CALL PGBBUF
              CALL PGSCH(0.25)
          END IF
          PAGE = PAGE+1
          CALL PGADVANCE
      END IF
      CALL PGVSIZE(1.75+(IX-1)*DX,1.75+IX*DX,1.0+(JY-1)*DY,1.0+JY*DY)
      CALL PGWINDOW(-50.,50.,-50.,50.0)
C
C Call PGBOX to draw a BOX.
C
      CALL PGSLW(1)
      CALL PGBOX('BC',10.0,0,'BC',10.0,0)
      CALL PGMTEXT('T',-1.5,0.05,0.0,LABEL)
C
      CALL PGSLW(2)
      I = 6
      MOVE = .TRUE.
   26 XC = XYGRID(I)
      I = I+1
      IF (XYGRID(I).EQ.-64) GOTO 20
      YC = XYGRID(I)
      I = I+1
      IF (XYGRID(I-2).EQ.-64) THEN
          MOVE = .TRUE.
          GOTO 26
      END IF
      IF (MOVE) THEN
          CALL PGMOVE(XC,YC)
          MOVE = .FALSE.
      ELSE
          CALL PGDRAW(XC,YC)
      END IF
      GOTO 26
C
C Don't forget to call PGEND!
C
   30 CALL PGEBUF
      CALL PGEND
      END
