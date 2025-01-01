C*GRSFS -- set fill area style
C+
      SUBROUTINE GRSFS (IFS)
      INTEGER IFS
C
C GRPCKG: Set the fill style for subsequent area fills
C The default fill style is 1 filled the other available is 2
C for Hollow (currently hollow means just draw outline)
C
C Argument:
C  IFS (input): the fill style number to be used for subsequent
C       area filling on the current device (in range 1-2).
C--
C 13-Dec-1990 - [PAH] new routine
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
C
      IF (GRCIDE.LT.1) THEN
          CALL GRWARN('GRSFS - no graphics device is active.')
          RETURN
      END IF
C
C AT THIS POINT IT WOULD BE NICE TO ADD GREXEC SUPPORT.
C
C Ignore request if no change is to be made.
C
      IF (IFS.EQ.GRFASL(GRCIDE)) RETURN
C
C Save fill style setting.
C
      GRFASL(GRCIDE) = IFS
C
      END

