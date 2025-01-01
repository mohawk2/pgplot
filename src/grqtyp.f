
C*GRQTYP -- inquire current device type
C+
      SUBROUTINE GRQTYP (TYPE,INTER)
      CHARACTER*(*) TYPE
      LOGICAL INTER
C
C GRPCKG: obtain the device type of the currently selected graphics
C device, and determine whether or not it is an interactive device.
C
C Arguments:
C
C TYPE (output, CHARACTER*(*)): receives the device type, as a
C       character string, eg 'PRINTRONIX', 'TRILOG', 'VERSATEC',
C       'TEK4010', 'TEK4014', 'GRINNELL', or 'VT125'.  The character
C       string should have a length of at least 8 to ensure that the
C       type is unique.
C INTER (output, LOGICAL*4): receives the value .TRUE. if the device is
C       interactive, .FALSE. otherwise.  Interactive devices are
C       terminals and Grinnells, which are capable of graphical input;
C       non-interactive devices include the printers, metafiles, and
C       the null device.
C--
C (23-May-1983)
C  5-Aug-1986 - add GREXEC support [AFT].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      REAL    RBUF(6)
      INTEGER NBUF,LCHR
      CHARACTER*10 CHR
C
      IF (GRCIDE.LT.1) THEN
          CALL GRWARN('GRQTYP - no graphics device is active.')
          TYPE = 'NULL'
          INTER = .FALSE.
      ELSE
          CALL GREXEC(GRGTYP, 1,RBUF,NBUF,TYPE,LCHR)
          CALL GREXEC(GRGTYP, 4,RBUF,NBUF,CHR,LCHR)
          INTER = (CHR(1:1).EQ.'I')
      END IF
C
      END
