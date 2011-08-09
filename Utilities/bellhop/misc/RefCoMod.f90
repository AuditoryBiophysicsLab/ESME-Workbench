MODULE RefCoMod

  ! reflection coefficient data

  SAVE
  INTEGER, PARAMETER :: BRCFil = 31, TRCFil = 32, IRCFil = 12
  INTEGER NBotPts, NTopPts
  REAL (KIND=8), ALLOCATABLE :: thetaBot( : ), RBot( : ), phiBot( : ), thetaTop( : ), RTop( : ), phiTop( : )
  INTEGER    NkTab
  INTEGER,          ALLOCATABLE :: ITab( : )
  REAL    (KIND=8), ALLOCATABLE :: XTab( : )
  COMPLEX (KIND=8), ALLOCATABLE :: FTab( : ), GTab( : )

CONTAINS

  SUBROUTINE READRC( BotRC, TopRC, PRTFil )

    ! Optionally read in bottom reflection coefficient

    PARAMETER (  PI = 3.14159265, DegRad = PI / 180.0 )
    INTEGER PRTFil
    CHARACTER*1 BotRC, TopRC
    CHARACTER*80 Title2

    IF ( BotRC == 'F' ) THEN
       WRITE( PRTFil, * ) 'Using tabulated bottom reflection coef.'
       OPEN( FilE = 'BRCFIL', UNIT = BRCFil, STATUS = 'OLD', IOSTAT = IOStat )
       IF ( IOsTAT /= 0 ) CALL ERROUT( PRTFil, 'F', 'GETPAR', 'Unable to open Bottom Reflection Coefficient file' )

       READ(  BRCFil, * ) NBotPTS
       WRITE( PRTFil, * ) 'Number of points in bottom reflection coefficient = ', NBotPTS

       IF ( ALLOCATED( thetaBot ) ) DEALLOCATE( thetaBot, RBot, phiBot )
       ALLOCATE( thetaBot( NBotPTS ), RBot( NBotPTS ), phiBot( NBotPTS ), Stat = IAllocStat )
       IF ( IAllocStat /= 0 ) &
            CALL ERROUT( PRTFil, 'F', 'READRD', 'Insufficient memory for bot. refl. coef.: reduce # points'  )

       READ( BRCFil, * ) ( thetaBot( I ), RBot( I ), phiBot( I ), I = 1, NBotPTS )
       phiBot = DegRad * phiBot   ! convert to radians

    ELSE   ! should allocate something anyway, since variable is passed
       ALLOCATE( thetaBot( 1 ), RBot( 1 ), phiBot( 1 ), Stat = IAllocStat )
    ENDIF

    ! Optionally read in top reflection coefficient

    IF ( TopRC == 'F' ) THEN
       WRITE( PRTFil, * ) 'Using tabulated top    reflection coef.'
       OPEN( FILE = 'TRCFIL', UNIT = TRCFil, STATUS = 'OLD', IOSTAT = IOStat )
       IF ( IOsTAT /= 0 ) CALL ERROUT( PRTFil, 'F', 'READRC', 'Unable to open Top Reflection Coefficient file' )

       READ(  TRCFil, * ) NTopPTS
       WRITE( PRTFil, * ) 'Number of points in top reflection coefficient = ', NTopPts

       ALLOCATE( thetaTop( NTopPTS ), RTop( NTopPTS ), phiTop( NTopPTS ), Stat = IAllocStat )
       IF ( IAllocStat /= 0 ) CALL ERROUT( PRTFil, 'F', 'READRC', 'Insufficient memory for top refl. coef.: reduce # points'  )

       READ( TRCFil, * ) ( thetaTop( I ), RTop( I ), phiTop( I ), I = 1, NTopPTS )
       phiTop = DegRad * phiTop   ! convert to radians
    ELSE   ! should allocate something anyway, since variable is passed
       ALLOCATE( thetaTop( 1 ), RTop( 1 ), phiTop( 1 ), Stat = IAllocStat )
    ENDIF

    ! Optionally read in internal reflection coefficient data

    IF ( BotRC == 'P' ) THEN
       WRITE( PRTFIL, * ) 'Reading precalculated refl. coeff. table'
       OPEN( FILE = 'IRCFIL', UNIT = IRCFil, STATUS = 'OLD', IOSTAT = IOStat )
       IF ( IOsTAT /= 0 ) CALL ERROUT( PRTFil, 'F', 'READRC', 'Unable to open Internal Reflection Coefficient file' )

       READ(  IRCFil, * ) Title2, Freq
       READ(  IRCFil, * ) NkTab
       WRITE( PRTFil, * )
       WRITE( PRTFil, * ) 'Number of points in internal reflection coefficient = ', NkTab

       IF ( ALLOCATED( XTab ) )  DEALLOCATE( XTab, FTab, GTab, ITab )
       ALLOCATE( XTab( NkTab ), FTab( NkTab ), GTab( NkTab ), ITab( NkTab ), Stat = IAllocStat )
       IF ( IAllocStat /= 0 ) CALL ERROUT( PRTFil, 'F', 'GETPAR', 'Too many points in reflection coefficient' )

       READ( IRCFil, FMT = "(5G15.7, I5 )" ) ( XTab( IK ), FTab( IK ), GTab( IK ), ITab( IK ), IK = 1, NkTab )
       CLOSE( IRCFil )
    ENDIF

  END SUBROUTINE READRC

  SUBROUTINE REFCO( ThetaInt, RInt, PhiInt, Theta, R, Phi, NPts, PRTFil )

    ! Given an angle ThetaInt, REFCO returns the magnitude and
    ! phase of the reflection coefficient (RInt, PhiInt).

    ! Uses linear interpolation using the two nearest abscissas
    ! Assumes Phi has been unwrapped so that it varies smoothly.
    ! I tried modifying it to allow a complex angle of incidence but
    ! stopped when I realized I needed to fuss with a complex atan2 routine

    IMPLICIT REAL (KIND=8) (A-H, O-Z)
    INTEGER PRTFil
    REAL (KIND=8) :: Theta( * ), R( * ), Phi( * )

    ILeft  = 1
    IRight = NPts
    ThetaIntr = REAL( ThetaInt )

    ! Three cases: ThetaInt left, in, or right of tabulated interval

    IF     ( ThetaIntr < Theta( ILeft  ) ) THEN
       !IRight = 2
       RInt   = 0.0     !R(   ILeft  )
       PhiInt = 0.0     !Phi( ILeft  )
       CALL ERROUT( PRTFIL, 'W', 'REFCO', 'Refl. Coef. being set to 0 outside tabulated domain' )
       WRITE( PRTFil, * ) 'angle = ', Thetaintr, 'lower limit = ', Theta( ILeft)
    ELSE IF( ThetaIntr > Theta( IRight ) ) THEN
       !ILeft = NPts - 1
       RInt   = 0.0     !R(   IRight )
       PhiInt = 0.0     !Phi( IRight )
       CALL ERROUT( PRTFIL, 'W', 'REFCO', 'Refl. Coef. being set to 0 outside tabulated domain' )
       WRITE( PRTFil, * ) 'angle = ', ThetaIntr, 'upper limit = ', Theta( IRight )
    ELSE

       ! Search for bracketting abscissas: Log2(NPts) stabs required for a bracket

       DO WHILE ( ILeft /= IRight - 1 )
          IMid = ( ILeft + IRight ) / 2
          IF ( Theta( IMid ) > ThetaIntr ) THEN
             IRight = IMid
          ELSE
             ILeft  = IMid
          ENDIF
       END DO
       ! Linear interpolation for reflection coef

       Alpha  = ( ThetaInt - Theta( ILeft ) ) / ( Theta( IRight ) - Theta( ILeft ) )
       RInt   = ( 1 - Alpha ) * R(   ILeft ) + Alpha * R(   IRight )
       PhiInt = ( 1 - Alpha ) * Phi( ILeft ) + Alpha * Phi( IRight )
       !WRITE( *, * ) thetaint, theta( ILeft ), theta( IRight ), alpha, R( ILeft ), r( IRight ), rint
       !RCmplx = ( 1 - Alpha ) * R( ILeft ) * EXP( CI * Phi( ILeft ) ) + Alpha * R( IRight ) * EXP( CI * Phi( IRight ) )

    ENDIF
    RETURN
  END SUBROUTINE REFCO

  SUBROUTINE IRCINT( X, F, G, IPow, XTab, FTab, GTab, ITab, NkTab )

    ! Internal reflection coefficient interpolator.
    ! Returns F, G, IPow for given X using tabulated values.
    ! Uses polynomial interpolation using the two nearest abscissas.
    ! The order of the polynomial is set by N below

    PARAMETER ( N = 3 )
    IMPLICIT REAL ( KIND=8 ) (A-H, O-Z)
    INTEGER    ITab(*)
    REAL    ( KIND=8 ) :: XTab(*)
    COMPLEX ( KIND=8 ) :: FTab(*), GTab(*), XT( N ), FT( N ), GT( N ), PZ, X, F, G

    Ten    = 10.0
    Xr     = DBLE( X )        ! taking the real part
    ILeft  = 1
    IRight = NkTab

    ! Three cases: X left, in, or right of tabulated interval

    IF (     Xr < XTab( ILeft  ) ) THEN
       F    = FTab( ILeft  )
       G    = GTab( ILeft  )
       IPow = ITab( ILeft  )
    ELSE IF( Xr > XTab( IRight ) ) THEN
       F    = FTab( IRight )
       G    = GTab( IRight )
       IPow = ITab( IRight )
    ELSE

       ! Search for bracketting abscissas:
       ! Log base 2 (NPTS) stabs required for a bracket

       DO WHILE ( ILeft /= IRight-1 )
          IMid = ( ILeft + IRight )/2
          IF ( XTab( IMid ) > Xr ) THEN
             IRight = IMid
          ELSE
             ILeft = IMid
          ENDIF
       END DO

       ! Extract the subset for interpolation and scale

       ILeft  = MAX( ILeft  - (N-2)/2, 1 )
       IRight = MIN( IRight + (N-1)/2, NkTab )

       NAct = IRight - ILeft + 1
       DO I = 1, NAct
          J       = I + ILeft - 1
          IDel    = ITab( J ) - ITab( ILeft )
          XT( I ) = XTab( J )
          FT( I ) = FTab( J ) * Ten ** IDel
          GT( I ) = GTab( J ) * Ten ** IDel
       END DO

       ! Construct the interpolate

       F    = PZ( X, XT, FT, NAct )
       G    = PZ( X, XT, GT, NAct )
       IPow = ITab( ILeft )
    ENDIF

    RETURN
  END SUBROUTINE IRCINT
END MODULE RefComod
