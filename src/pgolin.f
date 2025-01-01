C*PGOLIN -- mark a set of points using the cursor
C+
      SUBROUTINE PGOLIN (MAXPT, NPT, X, Y, SYMBOL)
      INTEGER MAXPT, NPT
      REAL    X(*), Y(*)
      INTEGER SYMBOL
C
C Interactive routine for user to enter data points by use of
C the cursor.  Routine allows user to Add and Delete points.  The
C points are returned in the order that they were entered (unlike
C PGNCUR).
C
C Arguments:
C  MAXPT  (input)  : maximum number of points that may be accepted.
C  NPT    (in/out) : number of points entered; should be zero on
C                    first call.
C  X      (in/out) : array of x-coordinates.
C  Y      (in/out) : array of y-coordinates.
C  SYMBOL (input)  : code number of symbol to use for marking
C                    entered points (see PGPT).
C
C Note (1): The dimension of arrays X and Y must be greater than or
C equal to MAXPT.
C
C Note (2): On return from the program, cursor points are returned in
C the order they were entered. Routine may be (re-)called with points
C already defined in X,Y (number in NPT), and they will be plotted
C first, before editing.
C
C Note (3): User commands: the user types single-character commands
C after positioning the cursor: the following are accepted:
C A (Add)    - add point at current cursor location.
C D (Delete) - delete the last point entered.
C X (eXit)   - leave subroutine.
C--
C  4-Nov-1985 - new routine (adapted from PGNCUR) - TJP.
C 13-Dec-1990 - change warnings to messages [TJP].
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      CHARACTER*1 LETTER
      INTEGER  PGCURS, SAVCOL
      REAL     XP, YP
C
C Check that PGPLOT is in the correct state.
C
      IF (PGOPEN.EQ.0) RETURN
C
C Save current color.
C
      CALL GRQCI(SAVCOL)
C
C Put current points on screen.  Position cursor on last point,
C or in middle viewport if there are no current points.
C
      IF (NPT.NE.0) THEN
          CALL PGPT(NPT,X,Y,SYMBOL)
          XP = X(NPT)
          YP = Y(NPT)
      ELSE
          XP = 0.5*(XBLC+XTRC)
          YP = 0.5*(YBLC+YTRC)
      END IF
C
C Loop over cursor inputs.
C
  100 IF (PGCURS(XP,YP,LETTER).NE.1) RETURN
      IF (LETTER.EQ.CHAR(0)) RETURN
      CALL GRTOUP(LETTER,LETTER)
C
C A (ADD) command:
C
      IF (LETTER .EQ. 'A') THEN
          IF (NPT.GE.MAXPT) THEN
              CALL GRMSG('ADD ignored (too many points).')
          ELSE
              NPT = NPT + 1
              X(NPT) = XP
              Y(NPT) = YP
              CALL PGPT(1,X(NPT),Y(NPT),SYMBOL)
              CALL GRTERM
          END IF
C
C D (DELETE) command:
C
      ELSE IF (LETTER.EQ.'D') THEN
          IF (NPT.LE.0) THEN
              CALL GRMSG('DELETE ignored (there are no points left).')
          ELSE
              CALL GRSCI(0)
              CALL PGPT(1,X(NPT),Y(NPT),SYMBOL)
              XP = X(NPT)
              YP = Y(NPT)
              CALL GRSCI(SAVCOL)
              CALL GRTERM
              NPT = NPT-1
          END IF
C
C X (EXIT) command:
C
      ELSE IF (LETTER.EQ.'X') THEN
          CALL GRETXT
          RETURN
C
C Illegal command:
C
      ELSE
          CALL GRMSG('Commands are A (add), D (delete), X (exit).')
      END IF
C
      GOTO 100
      END
