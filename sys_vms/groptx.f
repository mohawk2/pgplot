C*GROPTX -- open output text file (VMS)
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
      OPEN (UNIT=UNIT,
     1      FILE=NAME,
     2      STATUS='NEW',
     3      CARRIAGECONTROL='LIST',
     4      DEFAULTFILE=DEFNAM,
     5      RECL=2048,
     6      IOSTAT=IER)
      GROPTX = IER
C-----------------------------------------------------------------------
      END
