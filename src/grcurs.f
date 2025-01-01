C*GRCURS -- read cursor position
C+
      INTEGER FUNCTION GRCURS (IDENT,IX,IY,CH)
      INTEGER IDENT, IX, IY
      CHARACTER*(*) CH
C
C GRPCKG: Read the cursor position and a character typed by the user.
C The position is returned in absolute device coordinates (pixels).
C GRCURS positions the cursor at the position specified, and
C allows the user to move the cursor using the joystick or
C arrow keys or whatever is available on the device. When he has
C positioned the cursor, the user types a single character on his
C keyboard; GRCURS then returns this character and the new cursor
C position.
C
C Returns:
C
C GRCURS (integer): 1 if the call was successful; 0 if the device
C      has no cursor or some other error occurs. 
C
C Arguments:
C
C IDENT (integer, input): GRPCKG plot identifier (from GROPEN).
C IX (integer, input/output): the device x-coordinate of the cursor.
C IY (integer, input/output): the device y-coordinate of the cursor.
C CH (character, output): the character typed by the user; if the device
C      has no cursor or if some other error occurs, the value CHAR(0)
C      [ASCII NUL character] is returned.
C--
C  1-Aug-1984 - extensively revised [TJP].
C 29-Jan-1985 - add ARGS and HP2648 devices (?) [KS/TJP].
C  5-Aug-1986 - add GREXEC support [AFT].
C 11-Jun-1987 - remove built-ins [TJP].
C 15-Feb-1988 - remove test for batch jobs; leave this to the device
C               handler [TJP].
C 13-Dec-1990 - remove code to abort after 10 cursor errors (I may 
C               regret this!) [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      REAL           RBUF(6)
      INTEGER        NBUF,LCHR,IXMX,IYMX,ICURS
      CHARACTER*16   CHR
      INTEGER        ERRCNT
      SAVE           ERRCNT
      DATA           ERRCNT/0/
C
C Validate identifier, and select device.
C
      CALL GRSLCT(IDENT)
      CALL GRTERM
C
C Make sure cursor is on view surface. (It does not
C have to be in the viewport.)
C
      CALL GREXEC(GRGTYP, 6,RBUF,NBUF,CHR,LCHR)
      IXMX = RBUF(2)
      IYMX = RBUF(4)
      CALL GREXEC(GRGTYP, 4,RBUF,NBUF,CHR,LCHR)
      ICURS = 0
      IF (CHR(2:2).EQ.'C') ICURS=1
      IX = MAX(0,MIN(IXMX,IX))
      IY = MAX(0,MIN(IYMX,IY))
C
C External devices.
C
      IF (ICURS.GT.0) THEN
          RBUF(1)=IX
          RBUF(2)=IY
          CALL GREXEC(GRGTYP,17,RBUF,NBUF,CHR,LCHR)
          IX = RBUF(1)
          IY = RBUF(2)
          CH = CHR
          GRCURS = 1
C
C Other devices are illegal.
C
      ELSE
          CALL GREXEC(GRGTYP, 1,RBUF,NBUF,CHR,LCHR)
          IF (ERRCNT.LE.10) CALL 
     1        GRWARN('output device has no cursor: '//CHR(:LCHR))
          CH = CHAR(0)
          GRCURS = 0
          ERRCNT = ERRCNT+1
      END IF
C
      END
