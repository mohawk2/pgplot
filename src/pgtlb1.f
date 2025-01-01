
C.PGTLB1 -- Prepare a label for (DD) HH MM SS.S labeling
C.
      SUBROUTINE PGTLB1 (SUPTYP, SIGNF, ASIGN, DD, HH, MM, SS, DOD,
     *                   DOH, DOM, DOS, SPREC, TEXT, TLEN)
C
      REAL SS
      INTEGER DD, HH, MM, TLEN, SPREC
      CHARACTER ASIGN*1, TEXT*(*), SIGNF*1, SUPTYP*4
      LOGICAL DOD, DOH, DOM, DOS
C
C Write DD HH MM SS.S time labels.  Can also be used for HH MM SS.S 
C where HH > 24.   This is a support routine for PGTBOX and should 
C not be called by the user
C
C Inputs
C  SUPTYP :  'NONE', 'DHMS', or ' DMS' for no superscript labeling,
C            d,h,m, & s   or   o, ' & '' superscripting
C  SIGNF  :  Tells which field the sign is associated with.  
C            One of 'D', 'H', 'M', or 'S'    
C  ASIGN  :  + or - for positive or negative times
C  DD     :  Day
C  HH     :  Hours
C  MM     :  Minutes
C  SS     :  Seconds
C  DOD    :  If .true. then write DD into label
C  DOH    :  If .true. then write HH into label.  
C  DOM    :  If .true. then write MM into label
C  DO2    :  If .true. then write SS.S into label
C  SPREC  :  Number of places after the decimal to write seconds 
C            string to.  Must be in the range 0-3
C Outputs
C  TEXT   :  Label
C  TLEN   :  Length of label
C
C-
C
C  05-Sep-1989 -- New routine (Neil Killeen)
C  20-Apr-1991 -- Complete rewrite; support for new DD (day) field and 
C                 superscripted labels [nebk]
C  14-May-1991 -- Removed BSL as a parameter (Char(92)) and made it
C                 a variable to appease Cray compiler [mjs/nebk]
C-------------------------------------------------------------------------
      INTEGER FLEN, FST, FMAX, TRLEN(3), SUPPNT, TMPNT
      CHARACTER FIELD*6, FRMAT1*4, FRMAT2(3)*6, TRAIL(4,3)*11, 
     *TEMP*100, BSL*1
C
      SAVE FRMAT1, FRMAT2
      SAVE TRLEN
C
C   The FRMAT assignments must match the length of FIELD 
C
      DATA FRMAT1 /'(I6)'/
      DATA FRMAT2 / '(F6.1)', '(F6.2)', '(F6.3)'/
      DATA TRLEN /5, 11, 1/
C-------------------------------------------------------------------------
C
C   Initialize
C
      FMAX = LEN(FIELD)
      BSL = CHAR(92)
      TLEN = 0
      TEXT = ' '
C
C   Assign trailing strings.  Use CHAR(92) for backslash as the latter
C   must be escaped on SUNs thus requiring preprocessing.  The
C   concatenator operator precludes the use of a data statement
C
      TRAIL(1,1) = BSL//'ud'//BSL//'d'
      TRAIL(2,1) = BSL//'uh'//BSL//'d'
      TRAIL(3,1) = BSL//'um'//BSL//'d'
      TRAIL(4,1) = BSL//'us'//BSL//'d'
C
      TRAIL(1,2) = BSL//'u'//BSL//'(2199)'//BSL//'d'
      TRAIL(2,2) = BSL//'u'//BSL//'(2729)'//BSL//'d'
      TRAIL(3,2) = BSL//'u'//BSL//'(2727)'//BSL//'d'
      TRAIL(4,2) = BSL//'u'//BSL//'(2728)'//BSL//'d'
C      
      TRAIL(1,3) = ' '
      TRAIL(2,3) = ' '
      TRAIL(3,3) = ' '
      TRAIL(4,3) = ' '
C
C   Point at correct superscript strings
C
      IF (SUPTYP.EQ.'DHMS') THEN
        SUPPNT = 1
      ELSE IF (SUPTYP.EQ.' DMS') THEN
        SUPPNT = 2
      ELSE
        SUPPNT = 3
      END IF
C
CCCC
C   Days field
CCCC
C
      IF (DOD) THEN
C
C   Write into temporary field
C
        FIELD = ' '
        CALL PGNPL (FMAX, DD, FLEN)
        WRITE (FIELD, FRMAT1) DD
        FST = FMAX - FLEN + 1
C
C   Write output text string with desired superscripting
C
        TMPNT = 2
        IF (SIGNF.EQ.'D' .AND. ASIGN.NE.' ') TMPNT = 1
C
        TEMP = ASIGN//FIELD(FST:FMAX)//TRAIL(1,SUPPNT)
        TEXT(TLEN+1:) = TEMP(TMPNT:)
        TLEN = TLEN + (2 - TMPNT) + FLEN + TRLEN(SUPPNT)
      END IF
C
CCCC 
C   Hours field
CCCC
C
      IF (DOH) THEN
C
C   Write into temporary field
C
        FIELD = ' '
        CALL PGNPL (FMAX, HH, FLEN)
        WRITE (FIELD, FRMAT1) HH
        FST = FMAX - FLEN + 1
C
C   Write output text string with desired superscripting
C
        TMPNT = 2
        IF (SIGNF.EQ.'H' .AND. ASIGN.NE.' ') TMPNT = 1
C
        TEMP = ASIGN//FIELD(FST:FMAX)//TRAIL(2,SUPPNT)
        TEXT(TLEN+1:) = TEMP(TMPNT:)
        TLEN = TLEN + (2 - TMPNT) + FLEN + TRLEN(SUPPNT)
      END IF
C
CCCC
C   Minutes field
CCCC
C
      IF (DOM) THEN
C
C   Write into temporary field
C
        FIELD = ' '
        CALL PGNPL (FMAX, MM, FLEN)
        WRITE (FIELD, FRMAT1) MM
        FST = FMAX - FLEN + 1
C
C   Write output text string with desired superscripting
C
        TMPNT = 2
        IF (SIGNF.EQ.'M' .AND. ASIGN.NE.' ') TMPNT = 1
C
        TEMP = ASIGN//FIELD(FST:FMAX)//TRAIL(3,SUPPNT)
        TEXT(TLEN+1:) = TEMP(TMPNT:)
        TLEN = TLEN + (2 - TMPNT) + FLEN + TRLEN(SUPPNT)
      END IF
C
CCCC
C   Seconds field
CCCC
C
      IF (DOS) THEN
C
C   Write into temporary field
C 
        FIELD = ' '
        IF (SPREC.GE.1) THEN
C
C   Fractional label.  Upto 3 places after the decimal point allowed
C
          WRITE (FIELD, FRMAT2(SPREC)) SS
C
          FST = FMAX - SPREC - 3 + 1
          IF (FIELD(FST:FST).EQ.' ') FST = FST + 1
          FLEN = FMAX - FST + 1
        ELSE
C
C   Integer label
C
          CALL PGNPL (FMAX, NINT(SS), FLEN)
          WRITE (FIELD, FRMAT1) NINT(SS)
          FST = FMAX - FLEN + 1
        END IF
C
C   Write output text string with desired superscripting.
C
        TMPNT = 2
        IF (SIGNF.EQ.'S' .AND. ASIGN.NE.' ') TMPNT = 1
        TEMP = ASIGN//FIELD(FST:FMAX)//TRAIL(4,SUPPNT)
        TEXT(TLEN+1:) = TEMP(TMPNT:)
        TLEN = TLEN + (2 - TMPNT) + FLEN + TRLEN(SUPPNT)
      END IF
C  
C   A trailing blank will occur if no superscripting wanted
C
      IF (TEXT(TLEN:TLEN).EQ.' ' .AND. TLEN.GT.1) TLEN = TLEN - 1
C      
      RETURN
      END
