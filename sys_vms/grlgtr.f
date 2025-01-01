
C*GRLGTR -- translate logical name (VMS)
C+
      SUBROUTINE GRLGTR (NAME)
      CHARACTER*(*) NAME
C
C Recursive translation of a logical name.
C Up to 20 levels of equivalencing can be handled.
C This is used in the parsing of device specifications in the
C VMS implementation of PGPLOT. In other implementations, it may
C be replaced by a null routine.
C
C Argument:
C  NAME (input/output): initially contains the name to be
C       inspected.  If an equivalence is found it will be replaced
C       with the new name. If not, the old name will be left there. The
C       escape sequence at the beginning of process-permanent file
C       names is deleted and the '_' character at the beginning of
C       device names is left in place.
C--
C (1-Feb-1983)
C-----------------------------------------------------------------------
      INCLUDE '($SSDEF)'
      INTEGER I, LENGTH, RET, SYS$TRNLOG
C
      DO I=1,20
          IF (NAME.EQ.' ') RETURN
C=        CALL GRTOUP(NAME,NAME)
          IF (NAME(1:1) .EQ. '_') RETURN
          LENGTH = LEN(NAME)
          DO WHILE (NAME(LENGTH:LENGTH).EQ.' ')
              LENGTH = LENGTH-1
          END DO
          RET = SYS$TRNLOG( NAME(1:LENGTH), LENGTH, NAME, , ,)
          NAME(LENGTH+1:) = ' '
          IF (RET .EQ. SS$_NOTRAN) RETURN
          IF (.NOT.RET) RETURN
          IF (ICHAR(NAME(1:1)) .EQ. 27) NAME = NAME(5:) !Delete Esc
      END DO
      RETURN
      END
