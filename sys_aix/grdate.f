C*GRDATE -- get date and time as character string (AIX version)
C+
      SUBROUTINE GRDATE(STRING, L)
      CHARACTER*(*) STRING
      INTEGER L
C
C Return the current date and time, in format 'dd-Mmm-yyyy hh:mm'.
C To receive the whole string, the STRING should be declared
C CHARACTER*17.
C
C Arguments:
C  STRING : receives date and time, truncated or extended with
C           blanks as necessary.
C  L      : receives the number of characters in STRING, excluding
C           trailing blanks. This will always be 17, unless the length
C           of the string supplied is shorter.
C--
C 1990-Oct-19 - [AFT]
C-----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER CTIME*18
      CHARACTER CMON(12)*3
      SAVE      CMON
      INTEGER   ITZONE(11), ISECS
      DATA CMON/'Jan','Feb','Mar','Apr','May','Jun',
     :          'Jul','Aug','Sep','Oct','Nov','Dec'/
C
C Call the C function time to get UNIX time in seconds.
      CALL gettimeofday(ISECS, %val(0))
C CONTIM uses the UNIX function localtime to convert UNIX time to an
C array
      CALL contim(ISECS, ITZONE)
C
      WRITE(CTIME(1:11),101) ITZONE(4),CMON(ITZONE(5)+1),1900+ITZONE(6)
  101 FORMAT(I2,'-',A3,'-',I4)
      CTIME(12:12)=' '
      WRITE(CTIME(13:17),121) ITZONE(3), ITZONE(2)
  121 FORMAT(I2,':',I2)
C
      L=MIN(17,LEN(STRING))
      STRING(1:L) = CTIME(1:L)
      RETURN
      END
