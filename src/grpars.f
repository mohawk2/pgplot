
C*GRPARS -- parse device specification string
C+
      INTEGER FUNCTION GRPARS (DESCR,DEV,TYPE,APPEND)
      CHARACTER*(*) DESCR, DEV
      INTEGER  TYPE
      LOGICAL  APPEND
C
C GRPCKG: decode a device-specification; called by GROPEN.
C
C Returns:
C  GRPARS (output): 1 if the device-specification is
C       acceptable; any other value indicates an error.
C
C Arguments:
C  DESCR (input): the device specification.
C  DEV (output):  device name or file spec.
C  TYPE (output): device type (integer code); 0 if no device
C       type is specified.
C  APPEND (output): .TRUE. if /APPEND specified, .FALSE. otherwise.
C--
C 23-Jul-1984 - [TJP].
C 19-Feb-1988 - allow device part to be quoted [TJP].
C 30-Mar-1989 - remove logical translation of device and type [TJP].
C 17-Jun-1991 - ignore comments after ' (' [TJP].
C-----------------------------------------------------------------------
      INTEGER       MAXW
      PARAMETER     (MAXW=3)
      CHARACTER*128 UPPER
      CHARACTER*32  CTYPE
      INTEGER       GRDTYP, I, L, NW, P1(10), P2(10), B, E
      LOGICAL       QUOTE
C
C Default results.
C
      DEV = ' '
      TYPE = 0
      APPEND = .FALSE.
      GRPARS = 1
C
C Null string is acceptable.
C
      IF (LEN(DESCR).LT.1) RETURN
      IF (DESCR.EQ.' ') RETURN
C
C Discount trailing blanks and comments.
C
      L = INDEX(DESCR, ' (')
      IF (L.EQ.0) L = LEN(DESCR)
   10 IF (DESCR(L:L).EQ.' ') THEN
          L = L-1
      GOTO 10
      END IF
C
C Perform logical name translation (system dependent), and
C discount trailing blanks again.
C
      UPPER = DESCR(1:L)
      CALL GRLGTR(UPPER)
      L = LEN(UPPER)
   20 IF (UPPER(L:L).EQ.' ') THEN
          L = L-1
      GOTO 20
      END IF
C
C Divide string into substrings delimited by slashes. Ignore
C slashes within quotes.
C
      NW = 1
      I = 1
      P1(1) = 0
      QUOTE = .FALSE.
   30 IF (I.LE.L) THEN
          IF ((UPPER(I:I).EQ.'/').AND.(.NOT.QUOTE)) THEN
              P2(NW) = I-1
              NW = NW+1
              IF (NW.GT.MAXW) THEN
                  CALL GRWARN(
     1               'Too many qualifiers in device specification')
                  GRPARS = 64
                  RETURN
              END IF
              P1(NW) = I
          ELSE IF (UPPER(I:I).EQ.'"') THEN
              QUOTE = .NOT.QUOTE
          END IF
          I = I+1
      GOTO 30
      END IF
      P2(NW) = L
C
C Substring 1 is the device name. Remove enclosing quotes if necessary.
C
      B = P1(1)+1
      E = P2(1)
      IF (UPPER(B:B).EQ.'"' .AND. UPPER(E:E).EQ.'"' .AND. B.NE.E) THEN
          B = B+1
          E = E-1
      END IF
      IF (E.GE.B) THEN
          DEV = UPPER(B:E)
C;        CALL GRLGTR(DEV)
      END IF
C
C Substring 2, if present, is the device type. 
C
      IF (NW.LT.2) THEN
          TYPE = 0
      ELSE IF (P2(2).LE.P1(2)) THEN
          CALL GRWARN('No device type specified')
          GRPARS = GRPARS+16
          TYPE = 0
      ELSE
          CTYPE = UPPER(P1(2)+1:P2(2))
C;        CALL GRLGTR(CTYPE)
          CALL GRTOUP(CTYPE,CTYPE)
          TYPE = GRDTYP(CTYPE)
          IF (TYPE.EQ.0) THEN
              CALL GRWARN('Unrecognized device type')
              GRPARS = GRPARS+2
          END IF
      END IF
C
C Substring 3, if present, is a qualifier.
C
      IF (NW.LT.3) THEN
          APPEND = .FALSE.
      ELSE IF (P2(3).LE.P1(3)) THEN
          CALL GRWARN('Invalid qualifier in device specification')
          GRPARS = GRPARS+4
          APPEND = .FALSE.
      ELSE
          CTYPE = UPPER(P1(3)+1:P2(3))
          CALL GRTOUP(CTYPE,CTYPE)
          L = P2(3) - P1(3)
          IF (CTYPE(1:L) .EQ. 'NOAPPEND') THEN
              APPEND = .FALSE.
          ELSE  IF (CTYPE(1:L).EQ.'APPEND') THEN
              APPEND = .TRUE.
          ELSE
              CALL GRWARN('Invalid qualifier in device specification')
              GRPARS = GRPARS+8
          END IF
      END IF
C
      END
