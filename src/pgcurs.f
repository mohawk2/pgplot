C*PGCURS -- read cursor position
C+
      INTEGER FUNCTION PGCURS (X, Y, CH)
      REAL X, Y
      CHARACTER*1 CH
C
C Read the cursor position and a character typed by the user.
C The position is returned in world coordinates.  PGCURS positions
C the cursor at the position specified, allows the user to move the
C cursor using the joystick or arrow keys or whatever is available on
C the device. When he has positioned the cursor, the user types a
C single character on the keyboard; PGCURS then returns this
C character and the new cursor position (in world coordinates).
C
C Returns:
C  PGCURS         : 1 if the call was successful; 0 if the device
C                    has no cursor or some other error occurs.
C Arguments:
C  X      (in/out) : the world x-coordinate of the cursor.
C  Y      (in/out) : the world y-coordinate of the cursor.
C  CH     (output) : the character typed by the user; if the device has
C                    no cursor or if some other error occurs, the value
C                    CHAR(0) [ASCII NUL character] is returned.
C
C Note: The cursor coordinates (X,Y) may be changed by PGCURS even if
C the device has no cursor or if the user does not move the cursor.
C Under these circumstances, the position returned in (X,Y) is that of
C the pixel nearest to the requested position.
C--
C  1-Aug-1984 - extensively revised [TJP].
C 11-Jun-1991 - missing return value [TJP].
C-----------------------------------------------------------------------
      INCLUDE      'pgplot.inc'
      INTEGER      GRCURS, I, J
C
      IF (PGOPEN.EQ.0) THEN
          CH = CHAR(0)
          PGCURS = 0
          RETURN
      END IF
C
      I = NINT(XORG + X*XSCALE)
      J = NINT(YORG + Y*YSCALE)
      PGCURS = GRCURS(IDENT,I,J,CH)
      X = (I - XORG)/XSCALE
      Y = (J - YORG)/YSCALE
      CALL GRTERM
      END
