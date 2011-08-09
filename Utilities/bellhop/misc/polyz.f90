FUNCTION PZ( X0, X, F, N )

  ! Polynomial approximant of order N at X0

  IMPLICIT NONE
  INTEGER N, I, J
  COMPLEX ( KIND=8 ) PZ, X0, X( N ), F( N ), FT( N ), H( N )

  ! Initialize arrays
  H  = X - X0
  FT = F

  ! Recursion for solution
  IF ( N >= 2 ) THEN
     DO I = 1, N-1
        DO J = 1, N-I
           FT( J ) = ( H(J+I) * FT(J) - H(J) * FT(J+1) ) / ( H( J+I ) - H( J ) )
        END DO
     END DO
  ENDIF
  PZ = FT(1)

  RETURN
END FUNCTION PZ
