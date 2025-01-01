C*PGBEG -- begin PGPLOT, open output device
C+
      INTEGER FUNCTION PGBEG (UNIT, FILE, NXSUB, NYSUB)
      INTEGER       UNIT
      CHARACTER*(*) FILE
      INTEGER       NXSUB, NYSUB
C
C Begin PGPLOT, open the plot file.  A call to PGBEG is
C required before any other calls to PGPLOT subroutines.  If a plot
C file is already open for PGPLOT output, it is closed before the new
C file is opened.
C
C Returns:
C  PGBEG         : a status return value. A value of 1 indicates
C                    successful completion, any other value indicates
C                    an error. In the event of error a message is
C                    written on the standard error unit.  
C                    To test the return value, call
C                    PGBEG as a function, eg IER=PGBEG(...); note
C                    that PGBEG must be declared INTEGER in the
C                    calling program.
C Arguments:
C  UNIT  (input)   : this argument is ignored by PGBEG (use zero).
C  FILE  (input)   : the "device specification" for the plot device.
C                    Device specifications are installation dependent,
C                    but usually have the form "device/type" or
C                    "file/type". If this argument is a
C                    question mark ('?'), PGBEG will prompt the user
C                    to supply a string.
C  NXSUB  (input)  : the number of subdivisions of the view surface in
C                    X.
C  NYSUB  (input)  : the number of subdivisions of the view surface in
C                    Y. PGPLOT puts NXSUB x NYSUB graphs on each plot
C                    page or screen; when the view surface is sub-
C                    divided in this way, PGPAGE moves to the next
C                    sub-page, not the  next physical page.
C--
C  1-Jan-1984 [TJP]
C  8-Aug-1985 [TJP] - add '?' prompting.
C 31-Dec-1985 [TJP] - fix '?' prompting in batch jobs.
C 11-Sep-1986 [TJP] - add PGLDEV call.
C  9-Feb-1988 [TJP] - replace VMS-specific code with GRGCOM.
C 13-Dec-1990 [TJP] - make error reading input non-fatal.
C-----------------------------------------------------------------------
      INCLUDE       'pgplot.inc'
      INTEGER       DEFTYP,GRDTYP,GROPEN,L,LR
      INTEGER       GRGCOM, IER
      REAL          DUMMY,DUMMY2,XCSZ
      CHARACTER*20  DEFSTR
      CHARACTER*128 REQ
      LOGICAL JUNK
C
C Close the plot-file if it is already open. It is assumed that PGOPEN
C is initially zero; on some systems, this may require BLOCKDATA.
C
      IF (PGOPEN.NE.0) CALL PGEND
C
C Open the plot file; default type is given by environment variable
C PGPLOT_TYPE.
C
      CALL GRGENV('TYPE', DEFSTR, L)
      IF (L.EQ.0) THEN
          DEFTYP = 0
      ELSE
          CALL GRTOUP(DEFSTR, DEFSTR)
          DEFTYP = GRDTYP(DEFSTR(1:L))
      END IF
      IF (FILE(1:1).EQ.'?') THEN
   10     IER = GRGCOM(REQ,'Graphics device/type (? to see list): ',LR)
          IF (IER.NE.1) THEN
              CALL GRWARN('Error reading device specification')
              PGBEG = IER
              RETURN
          END IF
          IF (LR.LT.1) GOTO 10
          IF (REQ(1:1).EQ.'?') THEN
              CALL PGLDEV
              GOTO 10
          END IF
          PGBEG = GROPEN(DEFTYP,UNIT,REQ,IDENT)
          IF (PGBEG.NE.1) GOTO 10
      ELSE
          PGBEG = GROPEN(DEFTYP,UNIT,FILE,IDENT)
      END IF
C
C Failed to open plot file?
C
      IF (PGBEG.NE.1) RETURN
C
C Success: determine device characteristics.
C
      PGOPEN = 1
      ADVSET = 0
      CALL GRSIZE(IDENT,XSZ,YSZ,DUMMY,DUMMY2,XPERIN,YPERIN)
      CALL GRCHSZ(IDENT,XCSZ,DUMMY,XSP,YSP)
      NX = MAX(NXSUB,1)
      NY = MAX(NYSUB,1)
      XSZ = XSZ/NX
      YSZ = YSZ/NY
      NXC = NX
      NYC = NY
      CALL GRQTYP(DEFSTR,JUNK)
C
C Set the prompt state to ON, so that terminal devices pause between
C pages; this can be changed with PGASK.
C
      CALL PGASK(.TRUE.)
C
C If environment variable PGPLOT_BUFFER is defined (any value),
C start buffering output.
C
      PGBLEV = 0
      CALL GRGENV('BUFFER', DEFSTR, L)
      IF (L.GT.0) CALL PGBBUF
C
C Set default attributes.
C
      CALL PGSCI(1)
      CALL PGSLS(1)
      CALL PGSLW(1)
      CALL PGSCH(1.0)
      CALL PGSCF(1)
      CALL PGSFS(1)
C
C Set the default window (unit square).
C
      XBLC = 0.0
      XTRC = 1.0
      YBLC = 0.0
      YTRC = 1.0
C
C Set the default viewport.
C
      CALL PGVSTD
      END
