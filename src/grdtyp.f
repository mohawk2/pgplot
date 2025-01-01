
C*GRDTYP -- decode graphics device type string
C+
      INTEGER FUNCTION GRDTYP (TEXT)
C
C GRPCKG (internal routine): determine graphics device type code from
C type name. It compares the argument with the table of known device
C types in common.
C
C Argument:
C
C TEXT (input, character): device type name, eg 'PRINTRONIX'; the name
C       may be abbreviated to uniqueness.
C
C Returns:
C
C GRDTYP (integer): the device type code, in the range 1 to
C       GRTMAX, or zero if the type name is not recognised or
C       ambiguous.
C--
C 27-Dec-1984 - rewrite so that is doesn't have to be modified for
C               new devices [TJP].
C  5-Aug-1986 - add GREXEC support [AFT].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      CHARACTER*(*) TEXT
      INTEGER  CODE, I, L, MATCH
      REAL     RBUF(6)
      INTEGER NDEV,NBUF,LCHR
      CHARACTER*32 CHR
C
      GRDTYP = 0
      L = LEN(TEXT)
      IF (TEXT(1:L).EQ.' ') RETURN
   10 IF (TEXT(L:L).EQ.' ') THEN
          L = L-1
      GOTO 10
      END IF
      MATCH = 0
      CODE = 0
      CALL GREXEC(0,0,RBUF,NBUF,CHR,LCHR)
      NDEV=NINT(RBUF(1))
      DO 30 I=1,NDEV
          CALL GREXEC(I, 1,RBUF,NBUF,CHR,LCHR)
          IF (TEXT(1:L).EQ.CHR(1:L)) THEN
              MATCH = MATCH+1
              CODE = I
          END IF
   30 CONTINUE
      IF (MATCH.EQ.1) THEN
          GRDTYP = CODE
          GRGTYP = GRDTYP
      END IF
C
      END
