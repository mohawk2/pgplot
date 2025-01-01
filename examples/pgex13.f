      PROGRAM PGEX13
C-----------------------------------------------------------------------
C      Test program for PGPLOT plotting package
C      T. J. Pearson  1982 July 1
C-----------------------------------------------------------------------
      INTEGER I
      REAL XR(720),YR(720)
C-----------------------------------------------------------------------
C Color index:
      INTEGER BLACK, WHITE, RED, GREEN, BLUE, CYAN, MAGENT, YELLOW
      PARAMETER (BLACK=0)
      PARAMETER (WHITE=1)
      PARAMETER (RED=2)
      PARAMETER (GREEN=3)
      PARAMETER (BLUE=4)
      PARAMETER (CYAN=5)
      PARAMETER (MAGENT=6)
      PARAMETER (YELLOW=7)
C Line style:
      INTEGER FULL, DASHED, DOTDSH, DOTTED, FANCY
      PARAMETER (FULL=1)
      PARAMETER (DASHED=2)
      PARAMETER (DOTDSH=3)
      PARAMETER (DOTTED=4)
      PARAMETER (FANCY=5)
C Character font:
      INTEGER NORMAL, ROMAN, ITALIC, SCRIPT
      PARAMETER (NORMAL=1)
      PARAMETER (ROMAN=2)
      PARAMETER (ITALIC=3)
      PARAMETER (SCRIPT=4)
C Fill-area style:
      INTEGER SOLID, HOLLOW
      PARAMETER (SOLID=1)
      PARAMETER (HOLLOW=2)
C-----------------------------------------------------------------------
      CALL PGBEG(0,'?',1,1)
      CALL PGSCI(CYAN)
      CALL PGENV(0.,720.,-2.0,2.0,0,1)
      CALL PGSCI(GREEN)
      CALL PGLAB('x (degrees)',' ','TRIGONOMETRIC FUNCTIONS')
      DO 10 I=1,360
          XR(I) = 2.0*I
          YR(I) = SIN(XR(I)/57.29577951)
   10 CONTINUE
      CALL PGSCI(MAGENT)
      CALL PGSLS(DASHED)
      CALL PGLAB(' ','- sin(x)         -',' ')
      CALL PGLINE(360,XR,YR)
      DO 20 I=1,360
          XR(I) = 2.0*I
          YR(I) = COS(XR(I)/57.29577951)
   20 CONTINUE
      CALL PGSCI(YELLOW)
      CALL PGLAB(' ','-         cos(x) -',' ')
      CALL PGSLS(DOTTED)
      CALL PGLINE(360,XR,YR)
      CALL PGEND
      END
