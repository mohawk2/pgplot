C*GRVT03 -- PGPLOT Regis (VT125) driver, copy buffer to device
C+
      SUBROUTINE GRVT03 (BUFFER, UNIT, N)
      CHARACTER*(*) BUFFER
      INTEGER IBUF(1030)
      INTEGER UNIT, N
C
C Arguments:
C   BUFFER (input) address of buffer to be output
C   UNIT   (input) channel number for output
C   N      (input) number of bytes to transfer
C          (output) set to zero
C-----------------------------------------------------------------------
C Note: CHAR(27) = escape, CHAR(92) = backslash.
C     CHARACTER*(*) PREFIX, SUFFIX
C     PARAMETER (PREFIX=CHAR(27)//'Pp')
C     PARAMETER (SUFFIX=CHAR(27)//CHAR(92))
C-----------------------------------------------------------------------
      IF (N.GE.1)  THEN
	  IBUF(1) = 27
	  IBUF(2) = ICHAR('P')
	  IBUF(3) = ICHAR('p')
	  DO 10 I=1,N
	    IBUF(I+3) = ICHAR(BUFFER(I:I))
   10     CONTINUE
	  IBUF(N+4) = 27
	  IBUF(N+5) = 92
	  CALL GRWTERM(UNIT, IBUF, N+5)
C         WRITE (UNIT, '(3A)') PREFIX, BUFFER(1:N), SUFFIX
      END IF
      N = 0
C-----------------------------------------------------------------------
      END
