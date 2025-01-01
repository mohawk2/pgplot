C*GROPTX -- open output text file [Unix]
C+
      INTEGER FUNCTION GROPTX (UNIT, NAME, DEFNAM)
      INTEGER UNIT
      CHARACTER*(*) NAME, DEFNAM
C
C Input:
C  UNIT : Fortran unit number to use
C  NAME : name of file to create
C  DEFNAM : default file name (used to fill in missing fields for VMS)
C
C Returns:
C  0 => success; any other value => error.
C-----------------------------------------------------------------------
      INTEGER IER
      OPEN (UNIT=UNIT, FILE=NAME,
     2      STATUS='UNKNOWN',
     2      IOSTAT=IER)
      GROPTX = IER
C-----------------------------------------------------------------------
      END
