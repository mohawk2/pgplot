C*GRSVD -- SVD portion of GRRGB2 and GRIMG2
C
C This is a helper routine for GRRGB2 and GRIMG2 -- you feed in 
C a transformation matrix and get back (A) its inverse, (B) a padded
C version of the original matrix, and (C) an integer that is guaranteed
C to be larger than the largest padded singular value of the original matrix.
C
C The matrices are all six-element heterogeneous matrices.
C
C PA is the only input parameter -- the others are all for output.
C+

      SUBROUTINE GRSVD(PA, PINV, PPA, ISV)
      REAL PA(6),PINV(6),PPA(6)
      INTEGER ISV
      
      REAL   ATMP,BTMP
      REAL   THETA,SVX,SVY,SVECX,SVECY,SV1,SV2,R2X,R2Y
      REAL   XY0,YY0
      
C Transformation from array coordinates to device coordinates.
C
C Transformation is:
C
C   X = PA(1) + PA(2)*I + PA(3)*J
C   Y = PA(4) + PA(5)*I + PA(6)*J
C
C Invert the matrix, and calculate offset in array coordinates.
C The XXINV array converts from device coordinates back to array coordinates.

      ATMP = PA(2)*PA(6)-PA(3)*PA(5)
      PINV(2) =  PA(6)/ATMP
      PINV(3) = -PA(3)/ATMP
      PINV(5) = -PA(5)/ATMP
      PINV(6) =  PA(2)/ATMP
      PINV(1) =  - (PINV(2) * PA(1)  +  PINV(3) * PA(4))
      PINV(4) =  - (PINV(5) * PA(1)  +  PINV(6) * PA(4))
      
C
C Singular value decompose the 2x2 inverse matrix to find the range of 
C pixels we have to sample on each side.  (Resolve the matrix TR as
C R2 x S x R1, where R2 and R1 are rotation matrices and S is the
C singular value diagonal matrix.)
C
C The ATMP/BTMP stuff finds the rotation angle of the SVD basis up 
C to a +/- ambiguity.
C
      
      ATMP = PINV(3)**2 + PINV(6)**2 -PINV(2)**2 - PINV(5)**2
      BTMP = PINV(2)*PINV(3) + PINV(5)*PINV(6)
      
      IF (ATMP .EQ. 0) THEN
         THETA = 0 
      ELSE 
         THETA = 0.5 * ASIN( BTMP / SQRT( ATMP**2 / 4 + BTMP**2 ))
      ENDIF
      
C Now try the four possibilities to find the largest singular value

      SVX = SIN( THETA )
      SVY = COS( THETA )

      SV = ( PINV(2) * SVX + PINV(3) * SVY) ** 2 +
     $     ( PINV(5) * SVX + PINV(6) * SVY) ** 2
      SVECX = SVX
      SVECY = SVY

      SV1 = (PINV(2) * SVY + PINV(3) * SVX) ** 2 +
     $      (PINV(5) * SVY + PINV(6) * SVX) ** 2
      IF( SV1 .GT. SV ) THEN BEGIN
        SVECX = SVY
        SVECY = SVX
      ENDIF

      SV1 = ( - PINV(2) * SVX + PINV(3) * SVY ) ** 2 +
     $      ( - PINV(5) * SVX + PINV(6) * SVY ) ** 2
      IF( SV1 .GT. SV ) THEN BEGIN
        SVECX = -SVX
        SVECY = SVY
      ENDIF

      SV1 = ( PINV(2) * SVY - PINV(3) * SVX ) ** 2 +
     $      ( PINV(5) * SVY - PINV(6) * SVY ) ** 2
      IF( SV1 .GT. SV ) THEN BEGIN
        SVECX = SVY
        SVECY = -SVX
      ENDIF

      SV1 = SQRT(SV1)

      ATMP = SQRT(SVECX*SVECX + SVECY*SVECY)
      SVECX = SVECX/ATMP
      SVECY = SVECY/ATMP

C SVECX now has the vector associated with the largest singular value,
C and SV1 has the value itself.  SV2 gets the smallest singular value.
      SV2 = ( PINV(2) * SVECY - PINV(3) * SVECX ) ** 2 +
     $      ( PINV(5) * SVECY - PINV(6) * SVECX ) ** 2
      SV2 = SQRT(SV2)

C Now find the second rotation matrix.  
C Only need two components to know the whole thing.
      R2X = ( PINV(2) * SVECX + PINV(3) * SVECY ) / SV1
      R2Y = ( PINV(5) * SVECX + PINV(6) * SVECY ) / SV1
      R2 = sqrt(R2X*R2X + R2Y*R2Y)
      R2X = R2X/R2
      R2Y = R2Y/R2

C Finally, pad the singular values and reconstruct a padded PA by 
C multiplying the rotation matrices and the padded singular values.
      IF(SV1 .LT. 1) THEN
         SV1 = 1.
      ENDIF

      IF(SV2 .LT. 1) THEN 
         SV2 = 1.
      ENDIF

      PPA(1) = PA(1)
      PPA(2) =  SVECX * R2X / SV1  -  SVECY * R2Y / SV2
      PPA(3) = -SVECX * R2Y / SV1  -  SVECY * R2X / SV2
      PPA(4) = PA(4)
      PPA(5) =  SVECY * R2X / SV1  +  SVECX * R2Y / SV2
      PPA(6) = -SVECY * R2Y / SV1  +  SVECX * R2X / SV2

C ISV is the range to average over in array space.
      ISV = INT(SV2+SV1 + 1)

      END
