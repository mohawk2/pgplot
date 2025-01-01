
C*GRZS01 -- PGPLOT ZSTEM driver, draw a line on screen
C+
      SUBROUTINE GRZS01(IGRAPH,LASTI,LASTJ,I0,J0,I1,J1,CBUF,LBUF)
      INTEGER   IGRAPH, LASTI, LASTJ, I0, J0, I1, J1, LBUF
      CHARACTER CBUF*(*)
C
C If LASTI<0 then the cursor is moved to I0,J0 and then a vector
C drawn to I1,J1.  If LASTI>=0 then this routine assumes the cursor
C is currently at position LASTI,LASTJ and moves to the nearest
C point (either I0,J0 or I1,J1) and draws a line to the more distant
C point.  Upon return, LASTI,LASTJ will have to the current cursor
C position.  Full 12 bit Tektronix coordinates are generated in
C optimized format.  Nothing is transimitted to terminal, but rather
C the info is stored in the CBUF buffer array.  Note:
C 1) This routine can require up to 10 characters of storage in CBUF,
C therefore, before calling you should ensure that LEN(CBUF)-LBUF>9.
C 2) The 'delete' character (127) can occur in LOY or EXTRA byte;
C it can be replaced by escape-? if desired.
C
C 1989-Jan-18 - Ensure that terminal is in Graph Mode - [AFT]
C 1989-Jan-13 - New routine - [AFT]
C-----------------------------------------------------------------------
      INTEGER ID0, ID1, ITMP
C
      IF(LASTI.LT.0) THEN
C Last position is invalid, therefore do a dark vector move with all
C coordinates specified.
          LBUF=LBUF+1
          CBUF(LBUF:LBUF)=CHAR(29)
          CALL GRZS02(I0,J0,CBUF,LBUF)
          IGRAPH=1
      ELSE
C Last position is valid, do an optimized move operation.
          ID0=ABS(LASTI-I0)+ABS(LASTJ-J0)
          ID1=ABS(LASTI-I1)+ABS(LASTJ-J1)
          IF(ID1.LT.ID0) THEN
C Swap coordinates to minimize 'pen motion'.  For optimized coordinates
C this can reduce the amount of IO to the the terminal.
              ITMP=I0
              I0=I1
              I1=ITMP
              ITMP=J0
              J0=J1
              J1=ITMP
              ITMP=ID0
              ID0=ID1
              ID1=ITMP
          END IF
          IF(ID0.NE.0 .OR. ID1.NE.0) THEN
C Position has change, so do a move operation.
              LBUF=LBUF+1
              CBUF(LBUF:LBUF)=CHAR(29)
              CALL GRZS03(LASTI,LASTJ,I0,J0,CBUF,LBUF)
              IGRAPH=1
          END IF
      END IF
C
C If terminal is not in Graph Mode, then put it into Graph Mode and
C execute a zero length dark move.
      IF(IGRAPH.EQ.0) THEN
          ITMP = MOD(I0/4, 32)
          CBUF(LBUF+1:LBUF+2)=CHAR(29)//CHAR(64+ITMP)
          LBUF=LBUF+2
          IGRAPH=1
      END IF
C
C 'Pen' is now correctly positioned, do an optimized draw.
      CALL GRZS03(I0,J0,I1,J1,CBUF,LBUF)
C
C Remember current position.
      LASTI=I1
      LASTJ=J1
      RETURN
C
      END
