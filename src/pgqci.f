C*PGQCI -- inquire color index
C+
      SUBROUTINE PGQCI (CI)
      INTEGER  CI
C
C Query the Color Index attribute (set by routine PGSCI).
C
C Argument:
C  CI     (output) : the current color index (in range 0-max). This is
C                    the color index actually in use, and may differ
C                    from the color index last requested by PGSCI if
C                    that index is not available on the output device.
C--
C  5-Nov-1985 - new routine [TJP].
C-----------------------------------------------------------------------
      CALL GRQCI(CI)
      END
