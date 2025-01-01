      PROGRAM PGEX24
C
      INTEGER I, J, N, M
      REAL PI, THINC, R, G, B, THETA
      REAL XI(100),YI(100),XO(100),YO(100),XT(3),YT(3)
C
      PI = ACOS(-1.0)
      N = 33
      M = 8
      THINC=2.0*PI/N
      DO 10 I=1,N
        XI(I) = 0.0
        YI(I) = 0.0
   10 CONTINUE
      CALL PGBEG(0,'?',1,1)
      CALL PGENV(-1.,1.,-1.,1.,1,-2)
      DO 50 J=1,M
        R = 1.0
        G = 1.0 - FLOAT(J)/FLOAT(M)
        B = G
        CALL PGSCR(J, R, G, B)
        THETA = -FLOAT(J)*PI/FLOAT(N)
        R = FLOAT(J)/FLOAT(M)
        DO 20 I=1,N
          THETA = THETA+THINC
          XO(I) = R*COS(THETA)
          YO(I) = R*SIN(THETA)
   20   CONTINUE
        DO 30 I=1,N
          XT(1) = XO(I)
          YT(1) = YO(I)
          XT(2) = XO(MOD(I,N)+1)
          YT(2) = YO(MOD(I,N)+1)
          XT(3) = XI(I)
          YT(3) = YI(I)
          CALL PGSCI(J)
          CALL PGPOLY(3,XT,YT)
          CALL PGSCI(1)
          CALL PGMOVE(XT(1),YT(1))
          CALL PGDRAW(XT(2),YT(2))
          CALL PGDRAW(XT(3),YT(3))
          CALL PGDRAW(XT(1),YT(1))
   30   CONTINUE
        DO 40 I=1,N
          XI(I) = XO(I)
          YI(I) = YO(I)
   40   CONTINUE
   50 CONTINUE
      CALL PGEND
      END
