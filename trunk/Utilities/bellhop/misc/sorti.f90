SUBROUTINE SORT( X, N )

  ! Does an insertion sort on a vector of real numbers

  ! At the Ith step, the first I-1 positions contain a sorted
  ! vector.  We shall insert the Ith value into its place in that
  ! vector shifting up to produce a new vector of length I.

  REAL X( * )

  IF ( N == 1 ) RETURN

  DO I = 2, N

     T = X( I )

     IF ( T < X( 1 ) ) THEN          ! Goes in the first position
        CALL SHIFTIN( X, T, 1, I - 1 )
     ELSE IF ( T < X( I - 1 ) ) THEN ! Binary search for its place

        IRIGHT = I - 1
        ILEFT  = 1

        DO WHILE ( IRIGHT > ILEFT + 1 )
           IMID = ( ILEFT + IRIGHT ) / 2
           IF ( T < X(IMID) ) THEN
              IRIGHT = IMID
           ELSE
              ILEFT  = IMID
           ENDIF
        END DO

        ! Shift and insert
        CALL SHIFTIN( X, T, IRIGHT, I - 1 )

     ENDIF

  END DO

  RETURN
END SUBROUTINE SORT

SUBROUTINE SHIFTIN( X, T, I1, I2 )

  ! Shift values (I1, I2) one position to the right
  !  and insert T at position I1

  REAL X( * )

  DO  K = I2, I1, -1
     X( K + 1 ) = X( K )
  END DO

  X( I1 ) = T

  RETURN
END SUBROUTINE SHIFTIN
