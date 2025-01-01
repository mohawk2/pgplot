      PROGRAM PGEX14
C----------------------------------------------------------------------
C Demonstration program for the PGPLOT plotting package.  
C This is a line-drawing test; it draws a regular n-gon joining
C each vertex to every other vertex. It is not optimized for pen
C plotters.
C                              T. J. Pearson  1984 Dec 26
C----------------------------------------------------------------------
      INTEGER I, J, NV
      REAL A, D, X(100), Y(100)
C
C Call PGBEG to select the output device. PGPLOT is initialized so 
C that it buffers output, does not prompt when starting a new page, 
C and uses zero character size (so that there is no margin around the
C viewport). The color indices are changed to something more
C interesting than black on white.
C
      CALL PGBEG(0,'?',1,1)
      CALL PGSCH(0.0)
      CALL PGASK(.FALSE.)
      CALL PGSCR(0,0.2,0.3,0.3)
      CALL PGSCR(1,1.0,0.5,0.2)
      CALL PGSCR(2,0.2,0.5,1.0)
C
C Ask the user to specify the number of vertices, and compute the 
C coordinates for unit circumradius.
C
   10 WRITE (6,'(A)') ' Number of vertices (2-100, 0 to exit): '
      READ (5,*,ERR=10,END=50) NV
      IF (NV.GT.100.OR. NV.LT.2) GOTO 50
      D = 360.0/NV
      A = -D
      DO 20 I=1,NV
          A = A+D
          X(I) = COS(A/57.29577951)
          Y(I) = SIN(A/57.29577951)
   20 CONTINUE
C
C Select a square viewport.
C
      CALL PGBBUF
      CALL PGSCI(2)
      CALL PGENV(-1.05,1.05,-1.05,1.05,1,-1)
      CALL PGSCI(1)
C
C Draw the polygon.
C
      DO 40 I=1,NV-1
          DO 30 J=I+1,NV
            CALL PGMOVE(X(I),Y(I))
            CALL PGDRAW(X(J),Y(J))
   30     CONTINUE
   40 CONTINUE
C
C Flush the buffer and request a new number of vertices.
C
      CALL PGEBUF
      CALL PGUPDT
      GOTO 10
C
C End of program.
C
   50 CALL PGEND
      END
