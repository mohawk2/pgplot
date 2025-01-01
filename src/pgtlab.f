
C.PGTLAB -- label box with (DD) HH MM SS style labeling
C.
      SUBROUTINE PGTLAB (DODAY, SUPTYP, AXIS, CONVTL, FIRST, TMIN, TMAX,
     *                   ITS, TSCALE, TMINS, TICK, TINTS, DISP)
C
      REAL TMIN, TMAX, TMINS, TICK, TINTS, DISP
      INTEGER ITS(3), TSCALE
      CHARACTER AXIS*(*), SUPTYP*(*)
      LOGICAL FIRST, DODAY, CONVTL
C
C Label an axis in (DD) HH MM SS style.  This is a support subroutine for
C PGTBOX and should not be called by the user. 
C
C Inputs:
C  DODAY  :  Write labels as DD HH MM SS.S else HH MM SS.S with
C            hours ranging above 24.  Useful for declination labels
C  SUPTYP :  If 'DHMS' then superscript the fields with d, h, m, & s
C            If ' DMS' then superscript the fields with    o, '  & '' 
C              Good for declination plots.  You should obviously not 
C              ask for the day field for this to do anything sensible. 
C            If 'NONE' then no superscripting is done.
C  AXIS   :  'X' for x-axis, 'Y' for y-axis
C  CONVTL :  If .true., write the labels in the conventional axis 
C            locations (bottom and left for 'X' and 'Y').  Otherwise
C            write them on the top and right axes ('X' and 'Y')
C  FIRST  :  If .true. then write full label for first tick mark 
C            (DD HH MM SS.S)   Otherwise, then only write the last 
C            part of the label. e.g., If the full label is 17 42 22.2 
C            then just write 22.2
C  TMIN   :  Start time (seconds)
C  TMAX   :  End time (seconds)
C  ITS(3) :  DD, HH and MM of absolute value of start time (seconds)
C            If DODAY is FALSE, then ITS(1)=0 and ITS(2) compensates
C  TSCALE :  Determines units of axis (1 => ss, 60 => mm, 3600 => hh)
C  TMINS  :  Value of (offset) start time in units determined 
C            by TSCALE.  Always positive.
C  TICK   :  Major tick interval
C  TINTS  :  Absolute time window [ABS(TMAX - TMIN)] in scaled units
C  DISP   :  Displacement of label from axis in character heights
C            For x-axis, label is bottom justified to this location
C            For y-axis, label is right justified to this location
C--
C 05-Sep-1988 - new routine (Neil Killeen)
C 20-Apr-1991 - rewrite and add support for new DD (day) field [nebk]
C-----------------------------------------------------------------------
      DOUBLE PRECISION TFRAC, DELTFR
      REAL NEXTIC, COORD, FJUST, XLEN, YLEN, TSEC, TSCINC
      INTEGER ICOUNT, TLEN, DD, HH, MM, SDIR, IS, SPREC, SSPREC,
     *TLEN2, ITICK, ITMINS, NT, IREM
      CHARACTER TEXT*80, ASIGN(-1:1)*1, SIGNF*1, AXLOC*2, TEXT2*30
      LOGICAL WRITDD, WRITHH, WRITMM, WRITSS, SWITCH
C
      SAVE ASIGN
C
      DATA ASIGN / '-', ' ', ' '/
C-----------------------------------------------------------------------
C
C   Work out the precision with which to write fractional seconds
C   labels into the SS.S field.   All other fields have integer
C   labels only and TICK should always be greater than 1.0
C
      IF (TSCALE.EQ.1) THEN
        IF (TICK.LT.0.01) THEN
          SPREC = 3
        ELSE IF (TICK.LT.0.1) THEN
          SPREC = 2
        ELSE IF (TICK.LT.1.0) THEN
          SPREC = 1
        ELSE
          SPREC = 0
        END IF
      ELSE
        IF (TICK.LT.1.0) CALL GRWARN 
     *  ('PGTLAB: logic error for time resolution poorer than seconds')
        SPREC = 0
      END IF
C
C   Designate which field out of DD or HH will carry the sign, 
C   depending on whether you want the day field or not. 
C
      SIGNF = 'H'
      IF (DODAY) SIGNF = 'D'
C
C   IS points to the correct time sign in the array ASIGN and 
C   toggles between +1 and -1
C
      IS = 1
      IF (TMIN.LT.0.0) IS = -1
C
C   Direction sign
C
      IF (TMAX.GT.TMIN) THEN
        SDIR = 1
      ELSE
        SDIR = -1
      END IF
C
C   Keep track of whether positive or negative time with the absolute
C   time in seconds. 
C
      TSEC = TMIN
      TSCINC = SDIR * TSCALE * TICK
C
C If the start time is negative, reverse direction, make the start
C time positive, and count down toward zero.  When and if zero reached,
C count back up again by flipping the sign of SDIR again.
C
      IF (TMIN.LT.0.0) SDIR = -SDIR
C
C   Find the location of the first tick in scaled units.   Use integers
C   to try and avoid rounding problems
C
      ITICK = NINT(TICK * 1000)
      ITMINS = NINT(TMINS * 1000)
      NT = INT(ITMINS/ ITICK)
      IREM = ITMINS - NT*ITICK
      IF (SDIR.EQ.1 .AND. IREM.NE.0) NT = NT + 1
      NEXTIC = NT * TICK
C
C   Find the tick location and tick increment as a fraction of the 
C   window size.  Used for the label locations and when to stop
C
      TFRAC = SDIR * (NEXTIC - TMINS) / TINTS
      DELTFR = TICK / TINTS
C
C   Begin loop over major ticks with horrid F77 DO WHILE.  Remember 
C   NEXTIC is ALWAYS positive and when SDIR = -1 we are counting 
C   backwards in time.   SDIR flips sign at zero.
C
      SWITCH = .FALSE.
      DD = ITS(1)
      HH = ITS(2)
      MM = ITS(3)
      ICOUNT = 1
C
200   CONTINUE
        IF (NINT(TFRAC*1000).GT.1000) GOTO 300
C
        WRITDD = .FALSE.
        WRITHH = .FALSE.
        WRITMM = .FALSE.
        WRITSS = .FALSE.
C
CCCC
C   Time in seconds
CCCC
C
        IF (TSCALE.EQ.1) THEN
          WRITSS = .TRUE.
          IF (NEXTIC.GE.60.0 .OR. NEXTIC.LT.0.0) THEN
C
C   Increment minutes
C
            IF (SDIR.EQ.1) WRITMM = .TRUE.
            NEXTIC = NEXTIC - SDIR*60.0
            MM = MM + SDIR
C
            IF (MM.GE.60 .OR. MM.LT.0) THEN
C
C   Increment hours 
C
              MM = MM - SDIR*60
              IF (SDIR.EQ.1) WRITHH = .TRUE.
              HH = HH + SDIR
C
              IF (DODAY .AND. (HH.GE.24 .OR. HH.LT.0)) THEN
C
C   Increment days
C
                HH = HH - SDIR*24.0
                IF (SDIR.EQ.1) WRITDD = .TRUE.
                DD = DD + SDIR
              END IF
            END IF
          END IF
C
C   Write labels at appopriate times for decreasing time
C
          IF (SDIR.EQ.-1 .AND. NEXTIC.EQ.0.0) WRITMM = .TRUE.
          IF (SDIR.EQ.-1 .AND. MM.EQ.0 .AND. WRITMM) WRITHH = .TRUE.
          IF (DODAY .AND. SDIR.EQ.-1 .AND. HH.EQ.0 .AND.
     *        WRITHH .AND. WRITMM) WRITDD = .TRUE.
C
C   When 0 00 00 00.000 reached, change sign on label to make the change
C   clear.  Also, for the X-axis, write the seconds field with no decimal
C   places, to avoid possible label overwriting.
C   
          SSPREC = SPREC
          IF (DD.EQ.0 .AND. HH.EQ.0 .AND. MM.EQ.0 .AND. NEXTIC.EQ.0.0 
     *     .AND. ((ICOUNT.EQ.1 .AND. SDIR.EQ.-1) .OR. ICOUNT.NE.1)) THEN
            IS = -IS
            SWITCH = .TRUE.
            IF (AXIS.EQ.'X') SSPREC = 0
          END IF
C
C   Prepare label.  
C
          CALL PGTLB1 (SUPTYP, SIGNF, ASIGN(IS), DD, HH, MM, NEXTIC, 
     *                 WRITDD, WRITHH, WRITMM, WRITSS, SSPREC, 
     *                 TEXT, TLEN)
C
CCCC
C   Time in minutes
CCCC
C
        ELSE IF (TSCALE.EQ.60) THEN
          WRITMM = .TRUE.
          IF (NEXTIC.GE.60.0 .OR. NEXTIC.LT.0.0) THEN
C
C   Increment hours 
C
            NEXTIC = NEXTIC - SDIR*60.0
            IF (SDIR.EQ.1) WRITHH = .TRUE.
            HH = HH + SDIR
C
            IF (DODAY .AND. (HH.GE.24 .OR. HH.LT.0)) THEN
C
C   Increment days
C
              HH = HH - SDIR*24.0
              IF (SDIR.EQ.1) WRITDD = .TRUE.
              DD = DD + SDIR
            END IF
          END IF
C
C   Write labels at appropriate times for decreasing time
C
          IF (SDIR.EQ.-1 .AND. NEXTIC.EQ.0.0) WRITHH = .TRUE.
          IF (DODAY .AND. SDIR.EQ.-1 .AND. HH.EQ.0 .AND. 
     *        WRITHH) WRITDD = .TRUE.
C
C   When 0 00 00 reached, change sign on label to make 
C   change over clear.
C
          IF (DD.EQ.0 .AND. HH.EQ.0 .AND. NEXTIC.EQ.0.0 .AND.
     *      ((ICOUNT.EQ.1 .AND. SDIR.EQ.-1) .OR. ICOUNT.NE.1)) THEN
            IS = -IS
            SWITCH = .TRUE.
          END IF
C
C   Prepare label.  
C 
          CALL PGTLB1 (SUPTYP, SIGNF, ASIGN(IS), DD, HH, NINT(NEXTIC),
     *                 0.0, WRITDD, WRITHH, WRITMM, .FALSE., SPREC,
     *                 TEXT, TLEN)
C
CCCC
C   Time in hours
CCCC
C
        ELSE IF (TSCALE.EQ.3600) THEN
          WRITHH = .TRUE.
          IF (DODAY) THEN
            IF (NEXTIC.GE.24.0 .OR. NEXTIC.LT.0.0) THEN
C
C   Increment days
C
              NEXTIC = NEXTIC - SDIR*24.0
              IF (SDIR.EQ.1) WRITDD = .TRUE.
              DD = DD + SDIR
            END IF
C
C   Write labels at appropriate times for decreasing time
C
            IF (SDIR.EQ.-1 .AND. NEXTIC.EQ.0.0) WRITDD = .TRUE.
          END IF
C
C   When 0 00 reached, change sign on label to make 
C   change over clear.  DD will be zero if DODAY is .FALSE.
C
          IF (DD.EQ.0 .AND. NEXTIC.EQ.0.0 .AND.
     *      ((ICOUNT.EQ.1 .AND. SDIR.EQ.-1) .OR. ICOUNT.NE.1)) THEN
            IS = -IS
            SWITCH = .TRUE.
          END IF
C
C   Prepare label.  
C 
          CALL PGTLB1 (SUPTYP, SIGNF, ASIGN(IS), DD, NINT(NEXTIC), 0, 
     *                 0.0, WRITDD, WRITHH, .FALSE., .FALSE., SPREC,
     *                 TEXT, TLEN)
C
CCCC
C   Time in days
CCCC
C
        ELSE IF (TSCALE.EQ.3600*24) THEN
C
C   When 0 reached, change sign on label to make change over clear
C
          WRITDD = .TRUE.
          IF (NEXTIC.EQ.0.0 .AND. 
     *      ((ICOUNT.EQ.1 .AND. SDIR.EQ.-1) .OR. ICOUNT.NE.1)) THEN
            IS = -IS
            SWITCH = .TRUE.
          END IF
C
C   Time in days.  No wrap checks.  Prepare label and write DD
C   DD masquerades as SS.S in call to PGTLB1
C
          CALL PGTLB1 (SUPTYP, SIGNF, ASIGN(IS), NINT(NEXTIC), 0, 0, 
     *                 0.0, WRITDD, .FALSE., .FALSE., .FALSE., SPREC,
     *                 TEXT, TLEN)

        END IF
C
C   Special case of first tick, write full label
C
        IF (ICOUNT.EQ.1 .AND. FIRST) THEN
C
C   Prepare label. 
C
          WRITDD = .FALSE.
          IF (DODAY) WRITDD = .TRUE.
          IF (TSCALE.EQ.1) THEN
            CALL PGTLB1 (SUPTYP, SIGNF, ASIGN(IS), DD, HH, MM, NEXTIC,
     *                   WRITDD, .TRUE., .TRUE., .TRUE., SPREC,
     *                   TEXT, TLEN)
          ELSE IF (TSCALE.EQ.60) THEN
            CALL PGTLB1 (SUPTYP, SIGNF, ASIGN(IS), DD, HH, NINT(NEXTIC),
     *                   0.0, WRITDD, .TRUE., .TRUE., .FALSE., SPREC,
     *                   TEXT, TLEN)
          ELSE IF (TSCALE.EQ.3600) THEN
            CALL PGTLB1 (SUPTYP, SIGNF, ASIGN(IS), DD, NINT(NEXTIC), 0,
     *                   0.0, WRITDD, .TRUE., .FALSE., .FALSE., SPREC,
     *                   TEXT, TLEN)
          ELSE
            CALL PGTLB1 (SUPTYP, SIGNF, ASIGN(IS), NINT(NEXTIC), 0, 0,
     *                   0.0, .TRUE., .FALSE., .FALSE., .FALSE., SPREC,
     *                   TEXT, TLEN)
          END IF
C
C   Work out X-label location so that last field of label is centred
C   on its tick 
C
          IF (AXIS.EQ.'X') THEN
            IF (TSCALE.EQ.1) THEN
              CALL PGTLB1 (SUPTYP, SIGNF, ASIGN(IS), 0, 0, 0, NEXTIC,
     *                     .FALSE., .FALSE., .FALSE., .TRUE., SPREC,
     *                     TEXT2, TLEN2)
            ELSE
              CALL PGTLB1 (SUPTYP, SIGNF, ASIGN(IS), 0, 0, NINT(NEXTIC),
     *                     0.0, .FALSE., .FALSE., .TRUE., .FALSE., 0,
     *                     TEXT2, TLEN2)
            END IF
            CALL PGLEN (5, TEXT2(1:TLEN2), XLEN, YLEN)
            COORD = TFRAC + XLEN / 2.0
            FJUST = 1.0
          END IF
        ELSE
          IF (AXIS.EQ.'X') THEN
            COORD = TFRAC
            FJUST = 0.5
          END IF
        END IF
C
C   Write label
C
        IF (AXIS.EQ.'X') THEN
          IF (CONVTL) THEN
            AXLOC = 'B'
          ELSE
            AXLOC = 'T'
          END IF
C
          CALL PGMTXT (AXLOC, DISP, COORD, FJUST, TEXT(1:TLEN))
        ELSE IF (AXIS.EQ.'Y') THEN
          IF (CONVTL) THEN
            AXLOC = 'LV'
            FJUST = 1.0
          ELSE
            AXLOC = 'RV'
            FJUST = 0.0
          END IF
          COORD = TFRAC
C
          CALL PGMTXT (AXLOC, DISP, COORD, FJUST, TEXT(1:TLEN))
        END IF
C
C   Undo previous fooling around with ASIGN when 0 00 00 00 reached.
C   Increment absolute time and catch change of sign.
C
        IF (SWITCH) IS = -IS
        TSEC = TSEC + TSCINC
        IF (ASIGN(IS).EQ.'-' .AND. TSEC.GT.0.0) THEN
          SDIR = -SDIR
          IS = 1
        ELSE IF (ASIGN(IS).EQ.' ' .AND. TSEC.LT.0.0) THEN
          SDIR = -SDIR
          IS = -1
        END IF
C
C   Find next tick
C
        NEXTIC = NEXTIC + SDIR*TICK
        TFRAC = TFRAC + DELTFR 
        ICOUNT = 0
        SWITCH = .FALSE.
      GOTO 200
300   CONTINUE
C
      RETURN
      END
