C*ZSDRIV -- PGPLOT ZSTEM driver
C+
      SUBROUTINE ZSDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER   IFUNC, NBUF, LCHR
      REAL      RBUF(*)
      CHARACTER CHR*(*)
C
C PGPLOT driver for ZSTEM terminal emulator.
C
C 1989-Mar- 2 - New version - [AFT]
C 1990-Jul- 3 - Increased resolution to work on VGA - [AFT]
C 1990-Nov-18 - Proper use of SAVE statement [AFT]
C 1990-Nov-21 - Junk no longer (sometimes) echoed when using cursor [AFT]
C
C Supported device:  ZSTEM 240 and ZSTEM 4014 terminal emulators for
C the IBM PC and clones.  ZSTEM is the best VT240 terminal emulator
C that I've seen for the IBM thing.  ZSTEM supports Tektronix 4014
C emulation and the 4105 color index escape sequences.  If your VT240
C or emulator does not support this escape sequence, then you will need
C to modify the code for: IFUNC=15 to only allow color indices 0,1 and
C not to send the 4105 sequence; IFUNC=12 and 13 to not draw anything if
C color index=0; IFUNC=2 to reflect the reduction in supported indices.
C ZSTEM can be obtained from:
C    KEA Systems LTD.
C    2150 West Broadway, Suite 412
C    Vancouver, British Columbia
C    Canada, V6K 4L9
C
C Device type code: /ZSTEM
C
C Default device name: (the logged-in terminal).
C
C Default view surface dimensions: Depends on monitor; nominally
C 7.9in (horizontal) by 6.0in (vertical).
C
C Resolution: The display address space is 4092 pixels (horizontal) by
C 3120 pixels (vertical); actual resolution is much worse than this.
C NB I believe that there is a bug in ZSTEM that prevents plotting when
C the x coordinate is 4095.  To avoid this I set MAX X to 4092 (1023*4).
C
C Color capability: Only color indices 0 to 7 are supported.  This
C driver does not allow you to change the color representation.
C
C Input capability: the PGPLOT cursor routine enables the Tektronix 
C cross-hair cursor, and waits for the operator to move the cross-hair.
C When the operator strikes a keyboard character, the routine returns
C with the character and the cross-hair location. N.B. The `graphic
C input terminators' sent by the terminal to the computer can be
C changed by the emulator to be either, CR/EOT, CR or none.  To
C work with this driver the emulator MUST be configured to send a CR.
C 
C File format: It is not possible to send ZSTEM plots to a disk
C file. 
C 
C Obtaining hardcopy: Use the hardcopy switch on the terminal, if there
C is one, and if a suitable hard copy unit is connected.
C
C Supported systems: This version will run on both VAX VMS and Sun Unix
C systems.
C
C Required routines: This code requires the routines GROTER, GRCTER,
C GRWTER, and GRRTER.
C-----------------------------------------------------------------------
      INTEGER   NX, NY
      PARAMETER (NX=1023, NY=779)
C
      CHARACTER CBUF*72
      SAVE      CBUF
      CHARACTER CMSG*10
      INTEGER   GROTER
      INTEGER   IER, I0, I1, J0, J1, IX, IY, ICH, ICIND
      INTEGER   ICHAN, IGRAPH, LASTI, LASTJ, LBUF
      SAVE      ICHAN, IGRAPH, LASTI, LASTJ, LBUF
      LOGICAL   APPEND
      SAVE      APPEND
C-----------------------------------------------------------------------
11    FORMAT(A)
C
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230), IFUNC
  900 WRITE (CMSG,'(I10)') IFUNC
      CALL GRWARN('Unimplemented function in ZSTEM device driver:'
     1    //CMSG)
      NBUF = -1
      RETURN
C
C--- IFUNC = 1, Return device name -------------------------------------
C
   10 CONTINUE
      CHR = 'ZSTEM'
      LCHR = 7
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices ---------------------------------------
C
   20 CONTINUE
      RBUF(1) = 0
      RBUF(2) = NX
      RBUF(3) = 0
      RBUF(4) = NY
      RBUF(5) = 0
      RBUF(6) = 7
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution -------------------------------
C Assume the screen size is 10 inches by 7.5 inches (vert).
C Note: My monitor is actually 10x6.5 and using those numbers results
C in a true circle in PGEX17.  However, text drawn with that resolution
C will have a different relative length on the screen then the same
C text generated with the QMS or Postscript driver.  Since text position
C is more important than true circles, I use 10.x7.5 which gives better
C text position, but a flattened circle in PGEX17.
   30 CONTINUE
      RBUF(1) = NX/10.0
      RBUF(2) = NY/7.5
      RBUF(3) = 1
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info --------------------------------
C    (This device is Interactive, Cursor, No dashed lines, No area
C    fill, no thick lines)
C
   40 CONTINUE
      CHR = 'ICNNNNNNNN'
      LCHR = 10
      RETURN
C
C--- IFUNC = 5, Return default file name -------------------------------
C
   50 CONTINUE
      CALL GRTRML(CHR, LCHR)
      RETURN
C
C--- IFUNC = 6, Return default physical size of plot -------------------
C
   60 CONTINUE
      RBUF(1) = 0
      RBUF(2) = NX
      RBUF(3) = 0
      RBUF(4) = NY
      NBUF = 4
      RETURN
C
C--- IFUNC = 7, Return misc defaults -----------------------------------
C
   70 CONTINUE
      RBUF(1) = 10.0
      NBUF=1
      RETURN
C
C--- IFUNC = 8, Select plot --------------------------------------------
C
   80 CONTINUE
      RETURN
C
C--- IFUNC = 9, Open workstation ---------------------------------------
C
   90 CONTINUE
      APPEND = RBUF(3).NE.0.0
      ICHAN=GROTER(CHR, LCHR)
      IF (ICHAN.LT.0) THEN
          CALL GRWARN('Cannot open output device for ZSTEM plot.')
          RBUF(2) = 0
      ELSE
          RBUF(2) = 1
      END IF
      LASTI = -1
      LASTJ = -1
      LBUF = 0
      IGRAPH=0
C     -- no device initialization required
      RETURN
C
C--- IFUNC=10, Close workstation ---------------------------------------
C
  100 CONTINUE
      CALL GRZS05(ICHAN, 1, IGRAPH, CBUF, LBUF)
      LBUF=LBUF+1
      CBUF(LBUF:LBUF)=CHAR(24)
      CALL GRZS05(ICHAN, LEN(CBUF), IGRAPH, CBUF, LBUF)
      CALL GRCTER(ICHAN)
      RETURN
C
C--- IFUNC=11, Begin picture -------------------------------------------
C
  110 CONTINUE
      IF (.NOT.APPEND) THEN
C Erase screen
          CALL GRZS05(ICHAN, 3, IGRAPH, CBUF, LBUF)
          CBUF(LBUF+1:LBUF+3) = CHAR(29)//CHAR(27)//CHAR(12)
          LBUF=LBUF+3
      END IF
      RETURN
C
C--- IFUNC=12, Draw line -----------------------------------------------
C
  120 CONTINUE
      CALL GRZS05(ICHAN, 11, IGRAPH, CBUF, LBUF)
      I0 = MIN(MAX(0,NINT(RBUF(1)*4091./NX)),4095)
      J0 = MIN(MAX(0,NINT(RBUF(2)*3119./NY)),4095)
      I1 = MIN(MAX(0,NINT(RBUF(3)*4091./NX)),4095)
      J1 = MIN(MAX(0,NINT(RBUF(4)*3119./NY)),4095)
      CALL GRZS01(IGRAPH, LASTI, LASTJ, I0, J0, I1, J1, CBUF, LBUF)
      RETURN
C
C--- IFUNC=13, Draw dot ------------------------------------------------
C
  130 CONTINUE
      CALL GRZS05(ICHAN, 11, IGRAPH, CBUF, LBUF)
      I0 = MIN(MAX(0,NINT(RBUF(1)*4091./NX)),4095)
      J0 = MIN(MAX(0,NINT(RBUF(2)*3119./NY)),4095)
      CALL GRZS01(IGRAPH, LASTI, LASTJ, I0, J0, I0, J0, CBUF, LBUF)
      RETURN
C
C--- IFUNC=14, End picture ---------------------------------------------
C
  140 CONTINUE
      RETURN
C
C--- IFUNC=15, Select color index --------------------------------------
C
  150 CONTINUE
      ICIND = RBUF(1)
      IF (ICIND.LT.0 .OR. ICIND.GT.7) THEN
          ICIND = 1
          RBUF(1) = ICIND
      END IF
C The following 3 lines send the Tek 4105 escape seqence to set the
C color index.
      CALL GRZS05(ICHAN, 4, IGRAPH, CBUF, LBUF)
      CBUF(LBUF+1:LBUF+4)=CHAR(27)//'ML'//CHAR(48+ICIND)
      LBUF=LBUF+4
C Make sure that a GS character is sent in this buffer.
      LASTI=-1
      RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C
  160 CONTINUE
C Home cursor (by drawing a dark vector) and return to alpha mode
C (code US)
      CALL GRZS05(ICHAN, 7, IGRAPH, CBUF, LBUF)
      LBUF=LBUF+1
      CBUF(LBUF:LBUF)=CHAR(29)
      CALL GRZS02(0,3119,CBUF,LBUF)
      LBUF=LBUF+1
      CBUF(LBUF:LBUF)=CHAR(31)
C Flush buffer
      CALL GRZS05(ICHAN, LEN(CBUF), IGRAPH, CBUF, LBUF)
      LASTI=-1
      RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
C           RBUF(1)   in/out : cursor x coordinate.
C           RBUF(2)   in/out : cursor y coordinate.
C           CHR(1:1)  output : keystroke.
C
  170 CONTINUE
C Flush buffer
      CALL GRZS05(ICHAN, LEN(CBUF), IGRAPH, CBUF, LBUF)
      IX = MIN(MAX(0,NINT(RBUF(1)*4091./NX)),4095)
      IY = MIN(MAX(0,NINT(RBUF(2)*3119./NY)),4095)
      CALL GRZS04(ICHAN, IX, IY, ICH, IER)
      IF (IER.EQ.0) THEN
          RBUF(1) = IX*NX/1023.
          RBUF(2) = IY*NY/779.
          CHR = CHAR(ICH)
      ELSE
          CHR = CHAR(0)
      END IF
      NBUF = 2
      LCHR = 1
      LASTI = -1
      RETURN
C
C--- IFUNC=18, Erase alpha screen. -------------------------------------
C    (Not implemented)
C
  180 CONTINUE
      RETURN
C
C--- IFUNC=19, Set line style. -----------------------------------------
C    (Not implemented: should not be called)
C
  190 CONTINUE
      GOTO 900
C
C--- IFUNC=20, Polygon fill. -------------------------------------------
C    (Not implemented: should not be called)
C
  200 CONTINUE
      GOTO 900
C
C--- IFUNC=21, Set color representation. -------------------------------
C    (Not implemented: ignored)
C
  210 CONTINUE
      RETURN
C
C--- IFUNC=22, Set line width. -----------------------------------------
C    (Not implemented: should not be called)
C
  220 CONTINUE
      GOTO 900
C
C--- IFUNC=23, Escape --------------------------------------------------
C    (Not implemented: ignored)
C
  230 CONTINUE
      RETURN
C-----------------------------------------------------------------------
      END
