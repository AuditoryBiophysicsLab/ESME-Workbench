MODULE bdrymod

  ! Loads
  ! altimetry (top bdry) and bathymetry (bottom bdry) data
  !
  ! x = coordinate of boundary
  ! t = tangent
  ! n = normal
  ! Rlen = length of tangent (temporary variable to normalize tangent)

  SAVE
  INTEGER, PARAMETER :: ATIFIL = 40, BTYFIL = 41
  INTEGER NATIPTS, NBTYPTS
  REAL (KIND=8), ALLOCATABLE :: xBot( :, : ), tBot( :, : ), nBot( :, : ), RLenBot( : ), &
       xTop( :, : ), tTop( :, : ), nTop( :, : ), RLenTop( : )

CONTAINS

  SUBROUTINE READATI( TopATI, DepthT, rBox, PRTFil )

    INTEGER     PRTFil
    CHARACTER*1 TopATI

    IF ( TopATI == '*' ) THEN
       WRITE( PRTFIL, * ) '*********************************'
       WRITE( PRTFIL, * ) 'Using top-altimetry file'

       OPEN ( FILE = 'ATIFIL', UNIT = ATIFIL, STATUS = 'OLD' )

       READ(  ATIFIL, * ) NatiPts
       WRITE( PRTFIL, * ) 'Number of altimetry points', NatiPts

       ALLOCATE( xTop( 2, NatiPts ), tTop( 2, NatiPts - 1 ), nTop( 2, NatiPts - 1 ), &
            RLenTop( NatiPts - 1 ), Stat = IAllocStat )
       IF ( IAllocStat /= 0 ) &
            CALL ERROUT( PRTFIL, 'F', 'BELLHOP', 'Insufficient memory for altimetry data: reduce # ati points' )

       WRITE( PRTFIL, * )
       WRITE( PRTFIL, * ) ' Range (km)  Depth (m)'

       DO I = 1, NatiPts
          READ(  ATIFIL, * ) xTop( :, I )
          WRITE( PRTFIL, FMT = "(2G11.3)" ) xTop( :, I )
       END DO

       xTop( 1, : ) = 1000.0 * xTop( 1, : )   ! Convert ranges in km to m

       ! compute tangent and outward-pointing normal to top
       tToP( 1, : ) = xTop( 1, 2:NatiPts ) - xTop( 1, 1:NatiPts - 1 )
       tTop( 2, : ) = xTop( 2, 2:NatiPts ) - xTop( 2, 1:NatiPts - 1 )
       RLenTop = SQRT( tTop( 1, : ) ** 2 + tTop( 2, : ) ** 2 )

       tTop( 1, : ) = tTop( 1, : ) / RLenTop
       tTop( 2, : ) = tTop( 2, : ) / RLenTop
       DEALLOCATE( RLenTop )

       nTop( 1, : ) =  tTop( 2, : )
       nTop( 2, : ) = -tTop( 1, : )

    ELSE   ! no bathymetry given, use SSP depth for flat top
       NatiPts = 2
       ALLOCATE( xTop( 2, 2 ), tTop( 2, 1 ), nTop( 2, 1 ), Stat = IAllocStat )
       IF ( IAllocStat /= 0 ) &
            CALL ERROUT( PRTFIL, 'F', 'BELLHOP', 'Insufficient memory'  )
       xTop( :, 1 ) = (/ -rBox, DepthT /)
       xTop( :, 2 ) = (/  rBox, DepthT /)

       tTop( :, 1 ) = (/ 1.0,  0.0 /)   ! tangent to top
       nTop( :, 1 ) = (/ 0.0, -1.0 /)   ! outward-pointing normal
    ENDIF

  END SUBROUTINE READATI

  ! **********************************************************************!

  SUBROUTINE READBTY( BotBTY, DepthB, rBox, PRTFil )

    ! Reads in the bottom bathymetry

    INTEGER     PRTFil
    CHARACTER*1 BotBTY

    IF ( BotBTY == '*' ) THEN
       WRITE( PRTFIL, * ) '*********************************'
       WRITE( PRTFIL, * ) 'Using bottom-bathymetry file'

       OPEN ( FILE = 'BTYFIL', UNIT = BTYFIL, STATUS = 'OLD' )

       READ(  BTYFIL, * ) NbtyPts
       WRITE( PRTFIL, * ) 'Number of bathymetry points', NbtyPts

       ALLOCATE( xBot( 2, NbtyPts ), tBot( 2, NbtyPts - 1 ), nBot( 2, NbtyPts - 1 ), &
            RlenBot( NbtyPts - 1 ), Stat = IAllocStat )
       IF ( IAllocStat /= 0 ) &
            CALL ERROUT( PRTFIL, 'F', 'BELLHOP', 'Insufficient memory for bathymetry data: reduce # bty points' )

       WRITE( PRTFIL, * )
       WRITE( PRTFIL, * ) ' Range (km)  Depth (m)'

       DO I = 1, NbtyPts
          READ(  BTYFIL, * ) xBot( :, I )
          WRITE( PRTFIL, FMT = "(2G11.3)" ) xBot( :, I )
       END DO

       xBot( 1, : ) = 1000.0 * xBot( 1, : )   ! Convert ranges in km to m

       ! compute tangent and outward-pointing normal to bottom
       tBot( 1, : ) = xBot( 1, 2:NbtyPts ) - xBot( 1, 1:NbtyPts - 1 )
       tBot( 2, : ) = xBot( 2, 2:NbtyPts ) - xBot( 2, 1:NbtyPts - 1 )
       RLenBot = SQRT( tBot( 1, : ) ** 2 + tBot( 2, : ) ** 2 )

       tBot( 1, : ) = tBot( 1, : ) / RLenBot
       tBot( 2, : ) = tBot( 2, : ) / RLenBot

       nBot( 1, : ) = -tBot( 2, : )
       nBot( 2, : ) =  tBot( 1, : )
       DEALLOCATE( RLenBot )

    ELSE   ! no bathymetry given, use SSP depth for flat bottom
       NbtyPts = 2
       ALLOCATE( xBot( 2, 2 ), tBot( 2, 1 ), nBot( 2, 1 ), Stat = IAllocStat )
       IF ( IAllocStat /= 0 ) &
            CALL ERROUT( PRTFIL, 'F', 'BELLHOP', 'Insufficient memory'  )
       xBot( :, 1 ) = (/ -rBox, DEPTHB /)
       xBot( :, 2 ) = (/  rBox, DEPTHB /)

       tBot( :, 1 ) = (/ 1.0, 0.0 /)   ! tangent to bottom
       nBot( :, 1 ) = (/ 0.0, 1.0 /)   ! outward-pointing normal
    ENDIF

  END SUBROUTINE READBTY

END MODULE bdrymod
