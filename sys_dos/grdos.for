C*GRDATE -- get date and time as character string (MS-DOS)
C+
      SUBROUTINE GRDATE(CDATE, LDATE)
      CHARACTER CDATE*(17)
      INTEGER   LDATE
C
C Return the current date and time, in format 'dd-Mmm-yyyy hh:mm'.
C To receive the whole string, the CDATE should be declared
C CHARACTER*17.
C
C Arguments:
C  CDATE : receives date and time, truncated or extended with
C           blanks as necessary.
C  L      : receives the number of characters in STRING, excluding
C           trailing blanks. This will always be 17, unless the length
C           of the string supplied is shorter.
C--
C 1989-Mar-17 - [AFT]
C-----------------------------------------------------------------------
      CHARACTER CMON(12)*3
      INTEGER*2 IHR, IMIN, ISEC, I100TH
      INTEGER*2 IYR, IMON, IDAY
      DATA CMON/'Jan','Feb','Mar','Apr','May','Jun',
     :          'Jul','Aug','Sep','Oct','Nov','Dec'/
C---
      CALL GETTIM(IHR, IMIN, ISEC, I100TH)
      CALL GETDAT(IYR, IMON, IDAY)
      WRITE(CDATE,111) IDAY,CMON(IMON),IYR,IHR,IMIN
  111 FORMAT(I2,'-',A3,'-',I4,' ',I2,':',I2)
      LDATE=17
      RETURN
      END

C*GRFLUN -- free a Fortran logical unit number (MS-DOS)
C+
      SUBROUTINE GRFLUN(LUN)
      INTEGER LUN
C
C Free a Fortran logical unit number allocated by GRGLUN. [This version
C is pretty stupid; GRGLUN allocates units starting at 81, and GRFLUN
C does not free units.]
C
C Arguments:
C  LUN    : the logical unit number to free.
C--
C 25-Nov-1988
C-----------------------------------------------------------------------
      RETURN
      END

C*GRGCOM -- read with prompt from user's terminal (MS-DOS)
C+
      INTEGER FUNCTION GRGCOM(CREAD, CPROM, LREAD)
      CHARACTER CREAD*(*), CPROM*(*)
      INTEGER   LREAD
C
C Issue prompt and read a line from the user's terminal; in VMS,
C this is equivalent to LIB$GET_COMMAND.
C
C Arguments:
C  CREAD : (output) receives the string read from the terminal.
C  CPROM : (input) prompt string.
C  LREAD : (output) length of CREAD.
C
C Returns:
C  GRGCOM : 1 if successful, 0 if an error occurs (e.g., end of file).
C--
C 1989-Mar-29
C-----------------------------------------------------------------------
      INTEGER IER
C---
   11 FORMAT(A)
C---
      GRGCOM = 0
      LREAD = 0
      WRITE (*, 101, IOSTAT=IER) CPROM
  101 FORMAT(1X,A,$)
      IF (IER.EQ.0) READ (*, 11, IOSTAT=IER) CREAD
      IF (IER.EQ.0) GRGCOM = 1
      LREAD = LEN(CREAD)
   10 IF (CREAD(LREAD:LREAD).NE.' ') GOTO 20
      LREAD = LREAD-1
      GOTO 10
   20 CONTINUE
      END
C*********
C*GRMSG -- issue message to user (DOS version)
C+
      SUBROUTINE GRMSG (TEXT)
      CHARACTER*(*) TEXT
C
C Display a message on standard error.
C
C Argument:
C  TEXT (input): text of message to be printed (the string
C      may not be blank).
C--
C 1991-Jul-27 - From SUN version [AFT]
C-----------------------------------------------------------------------
      INTEGER   I
C
      IF (TEXT.NE.' ') THEN
          I = LEN(TEXT)
   10     IF (TEXT(I:I).EQ.' ') THEN
              I = I-1
          GOTO 10
          END IF
          WRITE (*, '(1X,A)') TEXT(1:I)
      END IF
      END
C*********
      INTERFACE TO CHARACTER FUNCTION GETENV [C]
     +   (CBUF[REFERENCE])
C---
C Allow MS-Fortran to call the GETENV function built into the
C Fortran 5.0 library.
C---
      CHARACTER*1 CBUF
      END
C*********

C*GRGENV -- get value of PGPLOT environment parameter (MS-DOS)
C+
      SUBROUTINE GRGENV(CNAME, CVALUE, LVALUE)
      CHARACTER CNAME*(*), CVALUE*(*)
      INTEGER   LVALUE
C
C Return the value of a PGPLOT environment parameter.
C
C Arguments:
C CNAME   : (input) the name of the parameter to evaluate.
C CVALUE  : receives the value of the parameter, truncated or extended
C           with blanks as necessary. If the parameter is undefined,
C           a blank string is returned.
C LVALUE  : receives the number of characters in CVALUE, excluding
C           trailing blanks. If the parameter is undefined, zero is
C           returned.
C--
C 1990-Mar-19 - [AFT]
C-----------------------------------------------------------------------
      CHARACTER GETENV*64
C
      CHARACTER CTMP*64
      INTEGER   I, LTMP
C
      CTMP = 'PGPLOT_'//CNAME
      LTMP = INDEX(CTMP,' ')
      IF(LTMP.EQ.0) LTMP=LEN(CTMP)-1
      CTMP(LTMP:LTMP)=CHAR(0)
      CTMP=GETENV(CTMP(:LTMP))
      CVALUE = ' '
      LVALUE = 0
C---
C MS-Fortran Kludge, if the environment variable is undefined, then
C GETENV points to NULL (memory location zero).  I see no easy way to
C detect this condition in Fortran, therefore, I compare with an
C environment variable that noone would ever define and hence should
C always point at NULL.
      IF(GETENV('#$%^'//CHAR(0)).EQ.CTMP) GOTO 140
      DO 130 I=1,LEN(CTMP)
         IF(CTMP(I:I).EQ.CHAR(0)) GOTO 140
         LVALUE=LVALUE+1
         CVALUE(LVALUE:LVALUE)=CTMP(I:I)
  130 CONTINUE
  140 CONTINUE
      RETURN
      END

C*GRGLUN -- get a Fortran logical unit number (MS-DOS)
C+
      SUBROUTINE GRGLUN(LUN)
      INTEGER LUN
C
C Get an unused Fortran logical unit number.
C Returns a Logical Unit Number that is not currently opened.
C After GRGLUN is called, the unit should be opened to reserve
C the unit number for future calls.  Once a unit is closed, it
C becomes free and another call to GRGLUN could return the same
C number.  Also, GRGLUN will not return a number in the range 1-9
C as older software will often use these units without warning.
C
C Arguments:
C  LUN    : receives the logical unit number, or -1 on error.
C--
C 12-Feb-1989 [AFT/TJP].
C-----------------------------------------------------------------------
      INTEGER I
      LOGICAL QOPEN
C---
      DO 10 I=99,10,-1
          INQUIRE (UNIT=I,  OPENED=QOPEN)
          IF (.NOT.QOPEN) THEN
              LUN = I
              RETURN
          END IF
   10 CONTINUE
      CALL GRWARN('GRGLUN: out of units.')
      LUN = -1
      RETURN
      END

C*GRGMSG -- print system message (MS-DOS)
C+
      SUBROUTINE GRGMSG (ISTAT)
      INTEGER   ISTAT
C
C This routine obtains the text of the VMS system message corresponding
C to code ISTAT, and displays it using routine GRWARN. On non-VMS
C systems, it just displays the error number.
C
C Argument:
C  ISTAT (input): 32-bit system message code.
C--
C 1989-Mar-29
C-----------------------------------------------------------------------
      CHARACTER CBUF*10
C
      WRITE (CBUF, 101) ISTAT
  101 FORMAT(I10)
      CALL GRWARN('system message number: '//CBUF)
      END

C*GRLGTR -- translate logical name (MS-DOS)
C+
      SUBROUTINE GRLGTR (CNAME)
      CHARACTER CNAME*(*)
C
C Recursive translation of a logical name.
C Up to 20 levels of equivalencing can be handled.
C This is used in the parsing of device specifications in the
C VMS implementation of PGPLOT. In other implementations, it may
C be replaced by a null routine.
C
C Argument:
C  CNAME (input/output): initially contains the name to be
C       inspected.  If an equivalence is found it will be replaced
C       with the new name. If not, the old name will be left there. The
C       escape sequence at the beginning of process-permanent file
C       names is deleted and the '_' character at the beginning of
C       device names is left in place.
C--
C 18-Feb-1988
C-----------------------------------------------------------------------
      END

C*GROPTX -- open output text file [MS-DOS]
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

C*GRPROM -- prompt user before clearing screen (MS-DOS)
C+
      SUBROUTINE GRPROM
C
C Display "Type <RETURN> for next page: " and wait for the user to
C type <CR> before proceeding.
C
C Arguments:
C  none
C--
C 1989-Mar-29
C-----------------------------------------------------------------------
      INTEGER   IER
      CHARACTER CMESS*14
C---
   11 FORMAT(A)
C---
      WRITE(*,101,IOSTAT=IER) CHAR(7)//'Type <RETURN> for next page: '
  101 FORMAT(1X,A,$)
      IF (IER.EQ.0) READ (*, 11, IOSTAT=IER) CMESS
      RETURN
      END

C*GRQUIT -- report a fatal error and abort execution (MS-DOS)
C+
      SUBROUTINE GRQUIT (CTEXT)
      CHARACTER CTEXT*(*)
C
C Report a fatal error (via GRWARN) and exit with fatal status; a
C traceback is generated unless the program is linked /NOTRACE.
C
C Argument:
C  CTEXT (input): text of message to be sent to GRWARN.
C--
C 18-Feb-1988
C-----------------------------------------------------------------------
      CALL GRWARN(CTEXT)
      STOP 'Fatal error in PGPLOT library'
      END

C*GRTRML -- get name of user's terminal (MS-DOS)
C+
      SUBROUTINE GRTRML(CTERM, LTERM)
      CHARACTER CTERM*(*)
      INTEGER   LTERM
C
C Return the device name of the user's terminal, if any.
C
C Arguments:
C  CTERM : receives the terminal name, truncated or extended with
C           blanks as necessary.
C  LTERM : receives the number of characters in CTERM, excluding
C           trailing blanks. If there is not attached terminal,
C           zero is returned.
C--
C 1989-Nov-08
C-----------------------------------------------------------------------
      CTERM = 'CON'
      LTERM = 3
      RETURN
      END

C*GRTTER -- test whether device is user's terminal (MS-DOS)
C+
      SUBROUTINE GRTTER(CDEV, QSAME)
      CHARACTER CDEV*(*)
      LOGICAL   QSAME
C
C Return a logical flag indicating whether the supplied device
C name is a name for the user's controlling terminal or not.
C (Some PGPLOT programs wish to take special action if they are
C plotting on the user's terminal.)
C
C Arguments:
C  CDEV : (input) the device name to be tested.
C  QSAME   : (output) .TRUE. is CDEV contains a valid name for the
C           user's terminal; .FALSE. otherwise.
C--
C 18-Feb-1988
C-----------------------------------------------------------------------
      CHARACTER CTERM*64
      INTEGER   LTERM
C
      CALL GRTRML(CTERM, LTERM)
      QSAME = (CDEV.EQ.CTERM(:LTERM))
      END

C*GRUSER -- get user name (MS-DOS)
C+
      SUBROUTINE GRUSER(CUSER, LUSER)
      CHARACTER CUSER*(*)
      INTEGER   LUSER
C
C Return the name of the user running the program.
C
C Arguments:
C  CUSER  : receives user name, truncated or extended with
C           blanks as necessary.
C  LUSER  : receives the number of characters in VALUE, excluding
C           trailing blanks.
C--
C 1989-Mar-19 - [AFT]
C-----------------------------------------------------------------------
C
      CALL GRGENV('USER', CUSER, LUSER)
      RETURN
      END

C*GRWARN -- issue warning message to user (MS-DOS)
C+
      SUBROUTINE GRWARN (CTEXT)
      CHARACTER CTEXT*(*)
C
C Report a warning message on standard error, with prefix "%PGPLOT, ".
C It is assumed that Fortran unit 0 is attached to stderr.
C
C Argument:
C  CTEXT (input): text of message to be printed (the string
C      may not be blank).
C--
C 18-Feb-1988
C-----------------------------------------------------------------------
      INTEGER   I
C
      IF (CTEXT.NE.' ') THEN
          I = LEN(CTEXT)
   10     IF (CTEXT(I:I).EQ.' ') THEN
              I = I-1
          GOTO 10
          END IF
          WRITE (*, 101) CTEXT(1:I)
  101     FORMAT(' %PGPLOT, ',A)
      END IF
      END
