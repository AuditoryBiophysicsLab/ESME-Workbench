SUBROUTINE SUBTAB( X, NX )

  ! If X(3) = -999.9 then subtabulation is performed
  ! i.e., a vector is generated with NX points in [ X(1), X(2) ]
  ! If x(2) = -999.9 then X(1) is repeated

  REAL X( NX )

  IF ( NX >= 3 ) THEN
     IF ( X( 3 ) == -999.9 ) THEN   ! testing for equality here is dangerous
        IF ( X( 2 ) == -999.9 ) X( 2 ) = X( 1 )
        deltaX = ( X( 2 ) - X( 1 ) ) / ( NX - 1 )
        X( 1:NX ) = X( 1 ) + (/ ( I, I=0, NX-1 ) /) * deltaX
     END IF
  END IF

  RETURN
END SUBROUTINE SUBTAB
