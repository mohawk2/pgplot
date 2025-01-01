C*PGEND -- terminate PGPLOT
C+
      SUBROUTINE PGEND
C
C Terminate PGPLOT, close the plot file, release the graphics
C device.  If the call to PGEND is omitted, some or all of the plot
C may be lost. If the environment parameter PGPLOT_IDENT is defined 
C (with any value), and the device is a hardcopy device, an
C identifying label is written on the plot (by calling PGIDEN: q.v.).
C
C Arguments: none
C--
C 10-Sep-1990 - adjust position of identification label.
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL INTER
      CHARACTER*16 DEFSTR
      INTEGER L
C
      IF (PGOPEN.NE.0) THEN
          CALL GRQTYP(DEFSTR,INTER)
          CALL GRGENV('IDENT', DEFSTR, L)
          IF (L.NE.0 .AND. (.NOT.INTER)) CALL PGIDEN
          CALL GRCLOS
          PGOPEN = 0
      END IF
      END
