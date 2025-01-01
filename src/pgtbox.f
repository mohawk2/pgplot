C*PGTBOX -- Draw a box & optionally write (DD) HH MM SS.S style labeling.
C+
      SUBROUTINE PGTBOX (XOPT, XTICKD, NXSUBD, YOPT, YTICKD, NYSUBD)
C
      REAL XTICKD, YTICKD
      INTEGER NXSUBD, NYSUBD
      CHARACTER XOPT*(*), YOPT*(*)
C
C Draw a box and optionally label one or both axes with (DD) HH MM SS style 
C numeric labels (useful for time or RA - DEC plots). Should deal with 
C axes that increase or decrease, and are positive or negative, or both.
C If the (DD) HH MM SS labeling is desired, then PGSWIN should have been
C previously called with the extrema in SECONDS.   
C
C In the seconds field, you can have at most 3 places after the decimal
C point, so that 1 ms is the smallest time interval you can time label.
C
C Large numbers are coped with by fields of 6 characters long.  Thus 
C you could have times with days or hours as big as 999999.  However, 
C in practice, you might have trouble with labels overwriting  themselves
C with such large numbers unless you a) use a small time INTERVAL, 
C b) use a small character size or c) choose your own sparse ticks in 
C the call to PGTBOX
C
C PGTBOX can be used in place of PGBOX as it calls PGBOX and only
C invokes time labeling if so directed.  Other options are passed 
C intact to PGBOX.
C
C
C Inputs:
C  XOPT   :  X-options for PGTBOX.  Same as for PGBOX plus 
C
C             'Z' for time labeling
C             'Y' means don't include the day field so that labels
C                 are HH MM SS.S rather than DD HH MM SS.S   The hours
C                 will accumulate beyond 24 if necessary in this case.
C             'H' means superscript numbers with d, h, m, & s  symbols
C             'D' means superscript numbers with    o, ', & '' symbols 
C                 Obviously you should not ask for the DD (day) field
C                 (no 'Y' above) with option 'D' .
C             'F' means write only the last part of the label for the 
C                 first time tick on the axis.  E.g., if the full
C                 first label is 17 42 34.4 then write only 34.4
C                 Useful for sub-panels that abut each other.
C
C             Note that PGBOX option 'L' (log labels) is ignored
C             with option 'Z'  Everything else functions as advertised.
C
C  YOPT   :  Y-options for PGTBOX.  See above.
C
C  XTICKD :  X-axis major tick increment.  0.0 for default. 
C  YTICKD :  Y-axis major tick increment.  0.0 for default. 
C            If the 'Z' option is used then XTICKD and/or YTICKD must
C            be in seconds.
C  NXSUB  :  Number of intervals for minor ticks on X-axis. 0 for default
C  NYSUB  :  Number of intervals for minor ticks on Y-axis. 0 for default
C
C
C  The regular XOPT and YOPT axis options for PGBOX are
C
C  A : draw Axis (X axis is horizontal line Y=0, Y axis is vertical
C      line X=0).
C  B : draw bottom (X) or left (Y) edge of frame.
C  C : draw top (X) or right (Y) edge of frame.
C  G : draw Grid of vertical (X) or horizontal (Y) lines.
C  I : Invert the tick marks; ie draw them outside the viewport
C      instead of inside.
C  L : label axis Logarithmically.
C  N : write Numeric labels in the conventional location below the
C      viewport (X) or to the left of the viewport (Y).
C  P : extend ("Project") major tick marks outside the box (ignored if
C      option I is specified).
C  M : write numeric labels in the unconventional location above the
C      viewport (X) or to the right of the viewport (Y).
C  T : draw major Tick marks at the major coordinate interval.
C  S : draw minor tick marks (Subticks).
C  V : orient numeric labels Vertically. This is only applicable to Y.
C      The default is to write Y-labels parallel to the axis
C
C--
C 05-Sep-1988 - new routine (Neil Killeen)
C 20-Apr-1991 - add support for new DD (day) field and implement
C               labeling on any axis (bottom,top,left,right) [nebk]
C-----------------------------------------------------------------------
      REAL XTICK, YTICK, DISP, XMIN, XMAX, YMIN, YMAX, TXINTS, TYINTS,
     *TXMIN, TXMAX, TYMIN, TYMAX
      INTEGER ITSX(3), ITSY(3), IPT, TSCALX, TSCALY, NXSUB, NYSUB
      CHARACTER XXOPT*15, YYOPT*15, SUPTYP*4
      LOGICAL XTIME, YTIME, FIRST, DODAYX, DODAYY, CONVTL
C------------------------------------------------------------------------
C
C   Get window in world coordinates
C 
      CALL PGQWIN (TXMIN, TXMAX, TYMIN, TYMAX)
C
C   X-axis first
C
      CALL GRTOUP (XXOPT, XOPT)
      XTIME = .FALSE.
      IF (INDEX(XXOPT,'Z').NE.0) THEN
C
C   Work out units for labeling, tick increments, and time window in 
C   scaled units (days, hours, mins or secs). Windows that start at 
C   negative times are made positive and reversed in direction
C
        IF (ABS(TXMAX-TXMIN).LT.0.001) THEN
          CALL GRWARN ('PGTBOX: X-axis time interval too small '//
     *                 '(< 1 ms) for time labels')
        ELSE
          XTIME = .TRUE.
          IF (INDEX(XXOPT,'Y').NE.0) THEN
            DODAYX = .FALSE.
          ELSE
            DODAYX = .TRUE.
          END IF
C
          CALL PGTIME ('X', DODAYX, TXMIN, TXMAX, ITSX, TSCALX, XTICK,
     *                 NXSUB, XMIN, XMAX, TXINTS)
        END IF
      END IF
C
C   No time labeling required, just pass the current window and the
C   user's values for the ticks to PGBOX
C
      IF (.NOT.XTIME) THEN
        XMIN = TXMIN
        XMAX = TXMAX
        XTICK = 0.0
        NXSUB = 0
      END IF
C
C   Same again for Y-axis
C
      CALL GRTOUP (YYOPT, YOPT)
      YTIME = .FALSE.
      IF (INDEX(YYOPT,'Z').NE.0) THEN
        IF (ABS(TYMAX-TYMIN).LT.0.001) THEN
          CALL GRWARN ('PGTBOX: Y-axis time interval too small '//
     *                 '(< 1ms) for time labels')
        ELSE
          YTIME = .TRUE.
          IF (INDEX(YYOPT,'Y').NE.0) THEN
            DODAYY = .FALSE.
          ELSE
            DODAYY = .TRUE.
          END IF
C
          CALL PGTIME ('Y', DODAYY, TYMIN, TYMAX, ITSY, TSCALY, YTICK,
     *                 NYSUB, YMIN, YMAX, TYINTS)
        END IF
      END IF
C
      IF (.NOT.YTIME) THEN
        YMIN = TYMIN
        YMAX = TYMAX
        YTICK = 0.0
        NYSUB = 0
      END IF
C
C   Set window in scaled units for time labelling
C
      IF (XTIME .OR. YTIME) CALL PGSWIN (XMIN, XMAX, YMIN, YMAX)
C
C   Set user supplied ticks if specified.  Scale for time axes.
C
      IF (XTICKD.NE.0.0) THEN
        XTICK = XTICKD
        IF (XTIME) XTICK = XTICK / TSCALX
      END IF
C
      IF (YTICKD.NE.0.0) THEN
        YTICK = YTICKD
        IF (YTIME) YTICK = YTICK / TSCALY
      END IF
C
      IF (NXSUBD.NE.0) NXSUB = NXSUBD
      IF (NYSUBD.NE.0) NYSUB = NYSUBD
C
C   Parse options list.  For call to PGBOX when doing time labeling, we 
C   don't want L (log), N or M (write numeric labels).  Pass the rest as 
C   they are. 
C
      IF (XTIME) THEN
        IPT = INDEX(XXOPT,'L')
        IF (IPT.NE.0) XXOPT(IPT:IPT) = ' '
        IPT = INDEX(XXOPT,'N')
        IF (IPT.NE.0) XXOPT(IPT:IPT) = ' '
        IPT = INDEX(XXOPT,'M')
        IF (IPT.NE.0) XXOPT(IPT:IPT) = ' '
      END IF
C
      IF (YTIME) THEN
        IPT = INDEX(YYOPT,'L')
        IF (IPT.NE.0) YYOPT(IPT:IPT) = ' '
        IPT = INDEX(YYOPT,'N')
        IF (IPT.NE.0) YYOPT(IPT:IPT) = ' '
        IPT = INDEX(YYOPT,'M')
        IF (IPT.NE.0) YYOPT(IPT:IPT) = ' '
      END IF
C
C   Draw box and ticks
C
      CALL PGBOX (XXOPT, XTICK, NXSUB, YYOPT, YTICK, NYSUB)
C
C   Add (DD) HH MM SS labels if desired.  Go back to the original user
C   specified options list.
C
      XXOPT = ' '
      CALL GRTOUP (XXOPT, XOPT)
      IF (XTIME .AND. (INDEX(XXOPT,'N').NE.0 .OR.
     *                 INDEX(XXOPT,'M').NE.0)) THEN
        FIRST = .TRUE.
        IF (INDEX(XXOPT,'F').NE.0) FIRST = .FALSE.
C
        CONVTL = .TRUE.
        IF (INDEX(XXOPT,'M').NE.0) CONVTL = .FALSE.
C
        SUPTYP = 'NONE'
        IF (INDEX(XXOPT,'D').NE.0) SUPTYP = ' DMS'
        IF (INDEX(XXOPT,'H').NE.0) SUPTYP = 'DHMS'
C
        DISP = 1.6
        IF (.NOT.CONVTL) DISP = 0.5
C
        CALL PGTLAB (DODAYX, SUPTYP, 'X', CONVTL, FIRST, TXMIN, TXMAX, 
     *               ITSX, TSCALX, XMIN, XTICK, TXINTS, DISP)
      END IF
C
      YYOPT = ' '
      CALL GRTOUP (YYOPT, YOPT)
      IF (YTIME .AND. (INDEX(YYOPT,'N').NE.0 .OR.
     *                 INDEX(YYOPT,'M').NE.0)) THEN
        FIRST = .TRUE.
        IF (INDEX(YYOPT,'F').NE.0) FIRST = .FALSE.
C
        CONVTL = .TRUE.
        IF (INDEX(YYOPT,'M').NE.0) CONVTL = .FALSE.
C
        SUPTYP = 'NONE'
        IF (INDEX(YYOPT,'D').NE.0) SUPTYP = ' DMS'
        IF (INDEX(YYOPT,'H').NE.0) SUPTYP = 'DHMS'
C
        DISP = 0.8
        IF (.NOT.CONVTL) DISP = 0.3
C
        CALL PGTLAB (DODAYY, SUPTYP, 'Y', CONVTL, FIRST, TYMIN, TYMAX,
     *               ITSY, TSCALY, YMIN, YTICK, TYINTS, DISP)
      END IF
C
C   Restore unscaled window
C
      IF (XTIME .OR. YTIME) CALL PGSWIN (TXMIN, TXMAX, TYMIN, TYMAX)
C
      RETURN
      END
