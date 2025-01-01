C*GRVT03 -- PGPLOT Regis (VT125) driver, copy buffer to device
C+
      SUBROUTINE GRVT03 (BUFFER, UNIT, N)
      CHARACTER*(*) BUFFER
      INTEGER UNIT, N
C
C Arguments:
C   BUFFER (input) address of buffer to be output
C   UNIT   (input) channel number for output
C   N      (input) number of bytes to transfer
C          (output) set to zero
C
C Modified:
C   26th Feb 2004. G77 doesn't like CHAR() in PARAMETER statements.
C                  SUFFIX and PREFIX made into ordinary variables. KS/AAO.
C-----------------------------------------------------------------------
C Note: CHAR(27) = escape, CHAR(92) = backslash.
      CHARACTER PREFIX*3, SUFFIX*2
      PREFIX=CHAR(27)//'Pp'
      SUFFIX=CHAR(27)//CHAR(92)
C-----------------------------------------------------------------------
      IF (N.GE.1) 
     1    WRITE (UNIT, '(3A)') PREFIX, BUFFER(1:N), SUFFIX
      N = 0
C-----------------------------------------------------------------------
      END
