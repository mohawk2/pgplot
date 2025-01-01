
**GRDATE -- get date and time as character string (HPUX)
************************************************************************
      SUBROUTINE GRDATE(STRING, L)
      CHARACTER*(*) STRING
************************************************************************
* Return the current date and time, in format 'dd-Mmm-yyyy hh:mm'.
* To receive the whole string, the STRING should be declared
* CHARACTER*17 in calling routine.
*
* Arguments:
*  STRING : receives date and time, truncated or extended with
*           blanks as necessary.
*  L      : receives the number of characters in STRING, excluding
*           trailing blanks. This will always be 17, unless the length
*           of the string supplied is shorter.
*
* 31-Jul-1992
*-----------------------------------------------------------------------
      CHARACTER*8 STIME
      CHARACTER*9 SDATE
*-----------------------------------------------------------------------
      CALL DATE(SDATE)
      CALL TIME(STIME)
      STRING(:7) = SDATE(:7)
      IF (SDATE(8:9).GT.'91') THEN
	STRING(8:11) = '19' // SDATE(8:9)
      ELSE
	STRING(8:11) = '20' // SDATE(8:9)
      ENDIF
      STRING(12:17) = ' ' // STIME(:5)
      L = 17
      RETURN
      END
