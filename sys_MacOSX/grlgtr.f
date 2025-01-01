
C*GRLGTR -- translate logical name (Sun/Convex-UNIX)
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
C 18-Feb-1988
C-----------------------------------------------------------------------
      END
