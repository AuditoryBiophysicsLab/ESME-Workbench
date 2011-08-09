MODULE SdRdRMod
  ! Reads in source depths, receiver depths, and receiver ranges

  IMPLICIT NONE
  SAVE
  INTEGER Nsd, Nrd, NR, Ntheta
  INTEGER, ALLOCATABLE :: Isd( : ), Ird( : )
  REAL,    ALLOCATABLE ::  sd( : ),  rd( : ), WS( : ), WR( : ), R( : ), theta( : )

CONTAINS

  SUBROUTINE SDRD( ENVFIL, PRTFIL, ZMIN, ZMAX )

    IMPLICIT NONE
    INTEGER ENVFIL, PRTFIL, IS, IR, IAllocStat
    REAL ZMIN, ZMAX

    ! *** Read source depths ***

    READ( ENVFIL, * ) Nsd

    IF ( Nsd <= 0 ) THEN
       WRITE( PRTFIL, * ) ' Nsd = ', Nsd
       CALL ERROUT( PRTFIL, 'F', 'SDRD', 'Number of sources must be positive'  )
    ENDIF

    IF ( ALLOCATED( sd ) ) DEALLOCATE( sd, WS, Isd )
    ALLOCATE( sd( MAX( 3, Nsd ) ), WS( Nsd ), Isd( Nsd ), Stat = IAllocStat )
    IF ( IAllocStat /= 0 ) THEN
       WRITE( PRTFIL, * ) 'Nsd = ', Nsd
       CALL ERROUT( PRTFIL, 'F', 'SDRD', 'Too many sources'  )
    END IF

    sd( 3 ) = -999.9
    READ( ENVFIL, * ) sd( 1 : Nsd )

    CALL SUBTAB( sd, Nsd )
    !CALL SORT(   sd, Nsd )

    WRITE( PRTFIL, * )
    WRITE( PRTFIL, * ) 'Number of sources   = ', Nsd
    IF ( Nsd >= 1  ) WRITE( PRTFIL, "( 5G14.6 )" ) ( sd( IS ), IS = 1, MIN( Nsd, 51 ) )
    IF ( Nsd > 51 ) WRITE( PRTFIL, * ) ' ... ', sd( Nsd )

    ! *** Read receiver depths ***

    READ( ENVFIL, * ) Nrd

    IF ( Nrd <= 0 ) THEN
       WRITE( PRTFIL, * ) ' Nrd = ', Nrd
       CALL ERROUT( PRTFIL, 'F', 'SDRD', 'Number of receivers must be positive'  )
    ENDIF

    IF ( ALLOCATED( rd ) ) DEALLOCATE( rd, WR, Ird )
    ALLOCATE( rd( MAX( 3, Nrd ) ), WR( Nrd ), Ird( Nrd ), Stat = IAllocStat  )
    IF ( IAllocStat /= 0 ) THEN
       WRITE( PRTFIL, * ) 'Nrd = ', Nrd
       CALL ERROUT( PRTFIL, 'F', 'SDRD', 'Too many receivers'  )
    END IF

    rd( 3 ) = -999.9
    READ( ENVFIL, * ) rd( 1 : Nrd )

    CALL SUBTAB( rd, Nrd )
    !CALL SORT(   rd, Nrd )

    WRITE( PRTFIL, * )
    WRITE( PRTFIL, * ) 'Number of receivers = ', Nrd
    IF ( Nrd >= 1 ) WRITE( PRTFIL, "( 5G14.6 )" ) ( rd( IR ), IR = 1, MIN( Nrd, 51 ) )
    IF ( Nrd > 51 ) WRITE( PRTFIL, * ) ' ... ', rd( Nrd )

    ! *** Check for sd/rd in upper or lower halfspace ***

    DO IS = 1, Nsd
       IF      ( sd( IS ) < ZMIN ) THEN
          sd( IS ) = ZMIN
          CALL ERROUT( PRTFil, 'W', 'SdRdRMod', 'Source above the top bdry has been moved down' ) 
       ELSE IF ( sd( IS ) > ZMAX ) THEN
          sd( IS ) = ZMAX
          CALL ERROUT( PRTFil, 'W', 'SdRdRMod', 'Source below the bottom bdry has been moved up' ) 
       ENDIF
    END DO

    DO IR = 1, Nrd
       IF      ( rd( IR ) < ZMIN ) THEN
          rd( IR ) = ZMIN
       ELSE IF ( rd( IR ) > ZMAX ) THEN
          rd( IR ) = ZMAX
       ENDIF
    END DO

    RETURN
  END SUBROUTINE SDRD

  !********************************************************************

  SUBROUTINE RANGES( ENVFIL, PRTFIL )

    ! Read receiver ranges

    IMPLICIT NONE
    INTEGER ENVFIL, PRTFIL, IR, IAllocStat

    READ(  ENVFIL, * ) NR
    WRITE( PRTFIL, * )
    WRITE( PRTFIL, * ) 'Number of ranges   = ', NR

    IF ( ALLOCATED( R ) ) DEALLOCATE( R )
    ALLOCATE( R( MAX( 3, NR ) ), Stat = IAllocStat )
    IF ( IAllocStat /= 0 ) CALL ERROUT( PRTFIL, 'F', 'RANGES', 'Too many range points' )

    R( 3 ) = -999.9
    READ( ENVFIL, * ) R( 1 : NR )

    CALL SUBTAB( R, NR )
    CALL SORT(   R, NR )

    WRITE( PRTFIL, "( 5G14.6 )" ) ( R( IR ), IR = 1, MIN( NR, 51 ) )
    IF ( NR > 51 ) WRITE( PRTFIL, * ) ' ... ', R( NR )

    R( 1:NR ) = 1000.0 * R( 1:NR )   ! Convert ranges to meters

    ! For a point source can't have receiver at origin
    ! IF ( OPT(1:1) == 'R' .AND. R( 1 ) <= 0.0 ) 
    !IF ( R( 1 ) <= 0.0 ) R( 1 ) = MIN( 1.0, R( 2 ) )

    RETURN
  END SUBROUTINE ranges

END MODULE SdRdRMod
