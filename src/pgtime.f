
C.PGTIME -- Work out offset time & units for time labeling
C.
      SUBROUTINE PGTIME (AXIS, DODAY, TMIN, TMAX, ITS, TSCALE, TICK,
     *                   NSUB, TMINS, TMAXS, TINTS)
C
      REAL TMIN, TMAX, TICK, TMINS, TMAXS, TINTS
      INTEGER NSUB, ITS(3), TSCALE
      LOGICAL DODAY
      CHARACTER AXIS*1
C
C Work out direction signs, offset time, tick increments, DD HH MM  of
C start time, units for DD HH MM SS style labeling and window in 
C scaled units.  This is a support routine for PGTBOX and should not 
C be called by the user.
C
C Input/output:
C  DODAY  :  Write labels as DD HH MM SS.S else HH MM SS.S with
C            hours ranging above 24.  Useful for declination labels
C Input:
C  AXIS   :  'X' or 'Y' for use in determining if labels overwrite
C  TMIN   :  Start time in seconds 
C  TMAX   :  End   time in seconds
C Outputs:
C  ITS(3) :  DD, HH, and MM of absolute value of start time (seconds)
C            If DODAY is false, DD is zero and HH compensates
C  TSCALE :  Determines units of axis (1 => ss, 60 => mm, 3600 => hh)
C  TICK   :  Major tick interval in units given by TSCALE
C  NSUB   :  Number of intervals between major ticks
C  TMINS  :  Start time in units given by TSCALE and offset from the
C            split time.  Always positve
C  TMAXS  :  End time in units given by TSCALE and offset from the
C            split time.  Either positive or negative
C  TINTS  :  Time interval in units given by TSCALE.  Always positive
C--
C 05-Sep-1988 - new routine (Neil Killeen)
C 08-Apr-1991 - correctly work out HH MM SS when the time is
C               greater than 60 hours [nebk]
C 20-Apr-1991 - revise to add support for new DD (day) field and
C               do lots of work on tick algorithm [nebk]
C-----------------------------------------------------------------------
      INTEGER NLIST1, NLIST2, NLIST3, NLIST4, NTICMX
      PARAMETER (NLIST1 = 19, NLIST2 = 10, NLIST3 = 6, NLIST4 = 8,
     *           NTICMX = 8)
C
      DOUBLE PRECISION REM
      REAL TICKS1(NLIST1), TICKS2(NLIST2), TICKS3(NLIST3), 
     *TICKS4(NLIST4), DIFF, TICMIN, TOCK, TOCK2, TSPLIT, TINT, 
     *LENX, LENY
      INTEGER NSUBS1(NLIST1), NSUBS2(NLIST2), NSUBS3(NLIST3), 
     *NSUBS4(NLIST4), I, SDIR, NPL, NTICK, ITICK, STRLEN
      CHARACTER STR*15, SUP*5
C
      SAVE TICKS1, TICKS2, TICKS3, TICKS4
      SAVE NSUBS1, NSUBS2, NSUBS3, NSUBS4
C
      DATA TICKS1 /0.001,  0.002,                 0.005,
     *             0.01,   0.02,                  0.05,  
     *             0.1,    0.2,                   0.5,  
     *             1.0,    2.0,   3.0,    4.0,    5.0,
     *             6.0,   10.0,  15.0,   20.0,   30.0/
      DATA NSUBS1 / 4,      4,                     2,    
     *              4,      4,                     2,    
     *              4,      4,                     2,    
     *              4,      4,     3,      4,      5,
     *              3,      2,     3,      2,      3/
C
      DATA TICKS2 /1.0,    2.0,   3.0,    4.0,    5.0,
     *             6.0,   10.0,  15.0,   20.0,   30.0/
      DATA NSUBS2 / 4,      4,     3,      4,      5,
     *              3,      2,     3,      2,      3/
C
      DATA TICKS3 /1.0,    2.0,   3.0,    4.0,    6.0,   12.0/
      DATA NSUBS3 / 4,      4,     3,      4,      3,      2/
C
      DATA TICKS4 /1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 8.0, 9.0/
      DATA NSUBS4 / 4,   4,   3,   4,   5,   3,   4,   3 /
C----------------------------------------------------------------------
C
C   Turn off DD (day) field if it has been unnecessarily asked for
C
      IF ((ABS(TMIN).LT.24.0*3600.0) .AND. (ABS(TMAX).LT.24.0*3600.0))
     *   DODAY = .FALSE.
C
C   ITS(1:3) = nearest DD,HH,MM of absolute minimum time
C
      REM = ABS(TMIN) / 3600.0
      IF (DODAY) THEN 
        ITS(1) = INT(REM / 24.0)
        REM = REM - ITS(1)*24.0
      ELSE
        ITS(1) = 0
      END IF
C
      ITS(2) = INT(REM)
      REM = (REM - ITS(2)) * 60.0
C
      ITS(3) = INT(REM)
C
C   Work out units and split time depending on time interval.
C
      TINT = ABS(TMAX - TMIN)
      IF (TINT.LE.5*60) THEN
C
C   Time interval less than 5 minutes, time in seconds, split at 
C   nearest earlier minute
C
        TSCALE = 1
        TSPLIT = ITS(1)*3600*24 + ITS(2)*3600 + ITS(3)*60
      ELSE IF (TINT.LE.5*3600) THEN
C
C   Time interval less than 5 hours, time in minutes, split at 
C   nearest earlier hour
C
        TSCALE = 60
        TSPLIT = ITS(1)*3600*24 + ITS(2)*3600
      ELSE 
        IF (.NOT.DODAY) THEN
C
C   Time in hours, split at nearest earlier day
C
          TSCALE = 3600
          TSPLIT = 0.0
        ELSE
          IF (TINT.LE.5*24*3600) THEN
C
C   Time interval less than 5 days, time in hours split at 
C   nearest earlier day 
C
            TSCALE = 3600
            TSPLIT = ITS(1) * 3600 * 24
          ELSE
C
C   Time in days
C
            TSCALE = 3600*24
            TSPLIT = 0.0
          END IF
        END IF
      END IF
C
C Direction sign, if start time negative, reverse direction
C
      SDIR = 1
      IF (TMAX.LT.TMIN) SDIR = -1
      IF (TMIN.LT.0.0) SDIR = -SDIR
C
C   Work out start time in units dictated by TSCALE and offset from
C   the split time.   Start time and interval always positive.
C   End time may be negative.
C
      TMINS = (ABS(TMIN) - TSPLIT) / TSCALE
      TINTS = TINT / TSCALE
      TMAXS = TMINS + SDIR * TINTS
C
CCCCC
C   Divide interval into NTICK major ticks and NSUB minor intervals
C   The tick choosing algorithm is not very robust, so watch out
C   if you fiddle anything. 
CCCCC
C
      TICMIN = 1000000.0
      IF (TSCALE.EQ.1) THEN
C
C   Time in seconds.  If the time interval is very small, may need to 
C   label with up to 3 decimal places.  Have less ticks to help prevent
C   X-label overwrite. STR is a dummy tick label to assess label 
C   overwrite potential
C
        IF (AXIS.EQ.'X') THEN
          SUP = CHAR(92)//'us'//CHAR(92)//'d'
          IF (TINTS.LE.0.01) THEN
            NTICK = 4
C
            STR = '60.423'//SUP
            STRLEN = 11
          ELSE IF (TINTS.LE.0.1) THEN
            NTICK = 5
C
            STR = '60.42'//SUP
            STRLEN = 10
          ELSE
            NTICK = 6
C
            STR = '60.4'//SUP
            STRLEN = 9
          END IF
        ELSE
          NTICK = 6
        END IF
        TOCK = TINTS / NTICK
C
C   Select nearest tick from list.
C
        DO 100 I = 1, NLIST1
          DIFF = ABS(TOCK - TICKS1(I))
          IF (DIFF.LT.TICMIN) THEN
            TICK = TICKS1(I)
            NSUB = NSUBS1(I)
            TICMIN = DIFF
            ITICK = I
          END IF
100     CONTINUE
C
C   The algorithm has guessed at a tick.  Make sure that the X-labels 
C   do not overwrite and that there aren't too many ticks. Otherwise,
C   try the next tick up in the list.   If no good, give up.
C
        IF (AXIS.EQ.'X') CALL PGLEN (4, STR(1:STRLEN), LENX, LENY)
C
        NTICK = INT(TINTS / TICK)
        IF ( (ITICK.LT.NLIST1)  .AND. 
     *     ( (AXIS.EQ.'X' .AND. (LENX/TSCALE).GT.0.9*TICK) .OR. 
     *       (NTICK.GT.NTICMX) ) ) THEN
          IF (TICKS1(ITICK+1).LT.TINTS) THEN
            NSUB = NSUBS1(ITICK+1)
            TICK = TICKS1(ITICK+1)
          END IF
        END IF
      ELSE IF (TSCALE.EQ.60) THEN
C
C   Time in minutes (integer labels only as anything smaller would 
C   be seconds rather than fractional minutes).
C
        NTICK = 6
        TOCK = TINTS / NTICK
C
C   Select nearest tick from list
C
        DO 200 I = 1, NLIST2
          DIFF = ABS(TOCK - TICKS2(I))
          IF (DIFF.LT.TICMIN) THEN
            TICK = TICKS2(I)
            NSUB = NSUBS2(I)
            TICMIN = DIFF
            ITICK = I
          END IF
200     CONTINUE
C
C   Check label overwrite and/or too many ticks.
C
        IF (AXIS.EQ.'X') THEN
          SUP = CHAR(92)//'um'//CHAR(92)//'d'
          STR = '42'//SUP
          STRLEN = 7
          CALL PGLEN (4, STR(1:STRLEN), LENX, LENY)
        END IF
C
        NTICK = INT(TINTS / TICK)
        IF ( (ITICK.LT.NLIST2)  .AND. 
     *     ( (AXIS.EQ.'X' .AND. (LENX/TSCALE).GT.0.9*TICK) .OR. 
     *       (NTICK.GT.NTICMX) ) ) THEN
          IF (TICKS2(ITICK+1).LT.TINTS) THEN
            NSUB = NSUBS2(ITICK+1)
            TICK = TICKS2(ITICK+1)
          END IF
        END IF
      ELSE 
        IF (TSCALE.EQ.3600 .AND. DODAY) THEN
C
C   Time in hours with the day field (integer labels only)
C
          NTICK = 6
          TOCK = TINTS / NTICK
C
C   Select nearest tick from list
C
          DO 300 I = 1, NLIST3
             DIFF = ABS(TOCK - TICKS3(I))
             IF (DIFF.LT.TICMIN) THEN
               TICK = TICKS3(I)
               NSUB = NSUBS3(I)
               TICMIN = DIFF
               ITICK = I
             END IF
300       CONTINUE
C
C   Check label overwrite and/or too many ticks.
C
          IF (AXIS.EQ.'X') THEN
            SUP = CHAR(92)//'uh'//CHAR(92)//'d'
            STR = '42'//SUP
            STRLEN = 7
            CALL PGLEN (4, STR(1:STRLEN), LENX, LENY)
          END IF
C
          NTICK = INT(TINTS / TICK)
          IF ( (ITICK.LT.NLIST3)  .AND. 
     *       ( (AXIS.EQ.'X' .AND. (LENX/TSCALE).GT.0.9*TICK) .OR. 
     *         (NTICK.GT.NTICMX) ) ) THEN
            IF (TICKS3(ITICK+1).LT.TINTS) THEN
              NSUB = NSUBS3(ITICK+1)
              TICK = TICKS3(ITICK+1)
            END IF
          END IF
        ELSE
C
C   Time in hours with no day field or time in days (integer labels).
C   Have less ticks for big numbers or the X-labels will overwrite.
C
          IF (AXIS.EQ.'X') THEN
            CALL PGNPL (-1, NINT(MAX(TINTS,TMINS,ABS(TMAXS))), NPL)
            IF (NPL.LE.3) THEN
              NTICK = 6
            ELSE IF (NPL.EQ.4) THEN
              NTICK = 5
            ELSE
              NTICK = 4
            END IF
            STR = '3456787912'
            STRLEN = NPL           
          ELSE
            NTICK = 6
          END IF
          TOCK = TINTS / NTICK
C
C   Select nearest tick from list; 1 choose nearest nice integer 
C   scaled by the appropriate power of 10
C
          CALL PGNPL (-1, NINT(TOCK), NPL)
          TOCK2 = TOCK / 10**(NPL-1)
C
          DO 400 I = 1, NLIST4
            DIFF = ABS(TOCK2 - TICKS4(I))
            IF (DIFF.LT.TICMIN) THEN
              TICK = TICKS4(I)
              NSUB = NSUBS4(I)
              TICMIN = DIFF
              ITICK = I
            END IF
400       CONTINUE
          TICK = TICK * 10**(NPL-1)
C
C   Check label overwrite and/or too many ticks.
C
          IF (AXIS.EQ.'X') THEN
            STR(STRLEN+1:) = CHAR(92)//'ud'//CHAR(92)//'d'
            STRLEN = STRLEN + 5
            CALL PGLEN (4, STR(1:STRLEN), LENX, LENY)
          END IF
C
          NTICK = INT(TINTS / TICK)
          IF ( (AXIS.EQ.'X' .AND. (LENX/TSCALE).GT.0.9*TICK) .OR. 
     *         (NTICK.GT.NTICMX) ) THEN
            IF (ITICK.LT.NLIST4) THEN
              IF (TICKS4(ITICK+1)*10**(NPL-1).LT.TINTS) THEN
                NSUB = NSUBS4(ITICK+1)
                TICK = TICKS4(ITICK+1) * 10**(NPL-1)
              END IF
            ELSE
              IF (TICKS4(1)*10**NPL.LT.TINTS) THEN
                NSUB = NSUBS4(1)
                TICK = TICKS4(1) * 10**NPL
              END IF
            END IF
          END IF
        END IF
      END IF
C
      RETURN
      END
