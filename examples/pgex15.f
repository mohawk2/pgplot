      PROGRAM PGEX15
C-----------------------------------------------------------------------
C Test program for PGPLOT: test of Cursor
C-----------------------------------------------------------------------
      CHARACTER*1 CH
      REAL X,Y
C
C Open device for graphics.
C
      CALL PGBEG(0,'?',1,1)
C
C Clear the screen. Draw a frame at the physical extremities of the
C plot, using full-screen viewport and standard window.
C
      CALL PGPAGE
      CALL PGSVP(0.0,1.0,0.0,1.0)
      CALL PGSWIN(0.0,1.0,0.0,1.0)
      CALL PGBOX('bcts',0.1,5,'bcts',0.1,5)
C
C Loop to read and display cursor position. Initial position for cursor
C is center of viewport. 
C
      X = 0.5
      Y = 0.5
   10 CONTINUE
          CALL PGCURS(X,Y,CH)
          WRITE (*, '(2F8.3,I4)') X,Y,ICHAR(CH)
      IF (CH.NE.'/'.AND. CH.NE.CHAR(0)) GOTO 10
C
C Close the device and exit.
C
      CALL PGEND
      END
