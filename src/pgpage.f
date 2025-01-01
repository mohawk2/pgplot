C*PGPAGE -- advance to new page
C+
      SUBROUTINE PGPAGE
C
C Advance plotter to a new (sub-)page, clearing the screen if
C necessary. If the "prompt state" is ON (see PGASK), confirmation is
C requested from the user before clearing the screen.  For an
C explanation of sub-pages, see PGBEG.  PGPAGE does not change the
C window or the position of the viewport relative to the (sub-)page.
C
C Arguments: none
C--
C  7-Feb-1983
C 23-Sep-1984 - correct bug: call GRTERM at end (if flush mode set).
C 31-Jan-1985 - make closer to Fortran-77.
C 19-Nov-1987 - explicitly clear the screen if device is interactive;
C               this restores the behavior obtained with older versions
C               of GRPCKG.
C  9-Feb-1988 - move prompting into routine GRPROM.
C 11-Apr-1989 - change name to PGPAGE.
C 10-Sep-1990 - add identification labelling.
C-----------------------------------------------------------------------
      INCLUDE      'pgplot.inc'
      CHARACTER*16 STR
      INTEGER      L
      LOGICAL      INTER
C
      IF (PGOPEN.EQ.0) RETURN
C
      NXC = NXC + 1
      IF (NXC.GT.NX) THEN
          NXC = 1
          NYC = NYC + 1
          IF (NYC.GT.NY) THEN
              NYC = 1
              IF (ADVSET.EQ.1) THEN
                  CALL GRQTYP(STR,INTER)
                  CALL GRGENV('IDENT', STR, L)
                  IF (L.NE.0 .AND. (.NOT.INTER)) CALL PGIDEN
              END IF
              IF (ADVSET.EQ.1 .AND. PROMPT) THEN
                  CALL GRTERM
                  CALL GRPROM
              END IF
              CALL GRPAGE
C
C If the device is interactive, call GRBPIC to clear the page.
C (If the device is not interactive, GRBPIC will be called
C automatically before the first output; omitting the call here
C ensures that a blank page is not output.)
C
              CALL GRQTYP(STR,INTER)
              IF (INTER) CALL GRBPIC
          END IF
      END IF
      XOFF = XVP + (NXC-1)*XSZ
      YOFF = YVP + (NY-NYC)*YSZ
C
C Window the plot in the new viewport.
C
      CALL PGVW
      ADVSET = 1
      CALL GRTERM
      END
