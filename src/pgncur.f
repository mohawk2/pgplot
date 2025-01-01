C*PGNCUR -- mark a set of points using the cursor
C+
      SUBROUTINE PGNCUR (MAXPT, NPT, X, Y, SYMBOL)
      INTEGER MAXPT, NPT
      REAL    X(*), Y(*)
      INTEGER SYMBOL
C
C Interactive routine for user to enter data points by use of
C the cursor.  Routine allows user to Add and Delete points.  The
C points are returned in order of increasing x-ccordinate, not in the
C order they were entered.
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
C increasing order of X. Routine may be (re-)called with points
C already defined in X,Y (number in NPT), and they will be plotted
C first, before editing.
C
C Note (3): User commands: the user types single-character commands
C after positioning the cursor: the following are accepted:
C A (Add)    - add point at current cursor location.
C D (Delete) - delete nearest point to cursor.
C X (eXit)   - leave subroutine.
C--
C 27-Nov-1983
C  9-Jul-1983 - modified to use GRSCI instead of GRSETLI [TJP].
C 13-Dec-1990 - changed warnings to messages [TJP].
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      CHARACTER*1 LETTER
      INTEGER  PGCURS, I, J, SAVCOL
      REAL     DELTA, XP, YP, XPHYS, YPHYS
      REAL     XMIN, XIP, YIP
C
C Check that PGPLOT is in the correct state.
C
      IF (PGOPEN.EQ.0) RETURN
C
C Save current color.
C
      CALL GRQCI(SAVCOL)
C
C Put current points on screen.
C
      IF (NPT.NE.0) CALL PGPT(NPT,X,Y,SYMBOL)
C
C Start with the cursor in the middle of the viewport.
C
      XP = 0.5*(XBLC+XTRC)
      YP = 0.5*(YBLC+YTRC)
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
              GOTO 100
          END IF
C         ! Find what current points new point is between.
          DO 120 J=1,NPT
              IF (XP.LT.X(J)) GOTO 122
  120     CONTINUE
          J = NPT + 1
C         ! New point is beyond last current
  122     CONTINUE
C         ! J is vector location where new point should be included.
          DO 140 I=NPT,J,-1
              X(I+1) = X(I)
              Y(I+1) = Y(I)
  140     CONTINUE
          NPT = NPT + 1
C         ! Add new point to point array.
          X(J) = XP
          Y(J) = YP
          CALL PGPT(1,X(J),Y(J),SYMBOL)
          CALL GRTERM
C
C D (DELETE) command:
C
      ELSE IF (LETTER.EQ.'D') THEN
          IF (NPT.LE.0) THEN
              CALL GRMSG('DELETE ignored (there are no points left).')
              GOTO 100
          END IF
          XMIN = 1.E+08
C         ! Look for point closest in radius.
C         ! Convert cursor points to physical.
          XPHYS = XORG + XP*XSCALE
          YPHYS = YORG + YP*YSCALE
          DO 220 I=1,NPT
C             ! Convert array points to physical.
              XIP = XORG + X(I)*XSCALE
              YIP = YORG + Y(I)*YSCALE
              DELTA = SQRT( (XIP-XPHYS)**2 + (YIP-YPHYS)**2 )
              IF (DELTA.LT.XMIN) THEN
                 XMIN = DELTA
                 J = I
              END IF
  220     CONTINUE
C         ! Remove point from screen by writing in background color.
          CALL GRSCI(0)
          CALL PGPT(1,X(J),Y(J),SYMBOL)
          CALL GRSCI(SAVCOL)
          CALL GRTERM
C         ! Remove point from cursor array.
          NPT = NPT-1
          DO 240 I=J,NPT
              X(I) = X(I+1)
              Y(I) = Y(I+1)
  240     CONTINUE
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
