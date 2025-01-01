
C*GRZS05 -- PGPLOT ZSTEM driver, dump buffer (if needed)
C+
      SUBROUTINE GRZS05( ICHAN, NEW, IGRAPH, CBUF, LBUF )
      INTEGER   ICHAN, NEW, IGRAPH, LBUF
      CHARACTER CBUF*(*)
C
C Ensure that there is enough room in CBUF to write NEW characters.
C If there is not enough room in CBUF, then CBUF(:LBUF) will be
C written to logical unit ICHAN and LBUF set equal to zero.  If there
C is room in CBUF then no action is taken.  In order to ensure
C that all info in flushed to the terminal, set NEW=LEN(CBUF).
C The sequence ESC//'[?38h' tells VT240 terminals to go to Tektronix
C mode.  
C
C 1989-Jan-13 - New routine [AFT]
C-----------------------------------------------------------------------
      INTEGER ITMP
C---
      IF(LBUF+NEW.GT.LEN(CBUF)) THEN
          ITMP=6
          CALL GRWTER(ICHAN,CHAR(27)//'[?38h',ITMP)
          CALL GRWTER(ICHAN,CBUF(:LBUF),LBUF)
          IGRAPH=0
      END IF
      RETURN
      END
