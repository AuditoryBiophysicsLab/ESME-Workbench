PROGRAM BELLHOP

  ! Beam tracing in cylindrical coordinates
  ! Michael B. Porter and Homer P. Bucker

  USE bellMod
  USE RefCoMod
  USE bdryMod
  USE angleMod
  USE SdRdRMod
  USE ArrMod
  USE BeamPatternMod

  INTEGER, PARAMETER    :: SHDFIL = 25, RAYFIL = 21, ArrivalsStorage = 20000000
  REAL,    PARAMETER    :: DegRad = pi / 180.0
  INTEGER   IBPvec( 1 )
  REAL      xs( 2 ), gradc( 2 )
  COMPLEX,  ALLOCATABLE ::   U( :, : )
  COMPLEX   eps, PICKeps
  CHARACTER Title*80, BotOpt*3, RunType*5, BeamType*3, Component*1

  CALL CPU_TIME( Tstart )

  ! Read in control data

  CALL READIN( Title, freq, iSingle, NImage, iBeamWindow, deltas, MaxN, zBox, rBox, epsMultiplier, rLoop,  &
       TopOpt, DepthT, cPT, rhoT, BotOpt, DepthB, cPB, rhoB, RunType, BeamType, Component )
  CALL READATI(  TopOpt(5:5), DepthT, rBox, PRTFil )    ! READ AlTImetry
  CALL READBTY(  BotOpt(2:2), DepthB, rBox, PRTFil )    ! READ BaThYmetrY
  CALL READRC(   BotOpt(1:1), TopOpt(2:2),  PRTFil )    ! READ Reflection Coefficients (top and bottom)
  CALL READPAT( RunType(3:3),               PRTFil )    ! Read Source Beam Pattern

  IF ( RunType( 5:5 ) == 'I' ) THEN
     Nrd_per_range = 1     ! irregular grid
  ELSE
     Nrd_per_range = Nrd   ! rectilinear grid
  ENDIF

  ! for a TL calculation, allocate space for the pressure matrix
  IF ( SCAN( 'CSI', RunType(1:1) ) /= 0 ) THEN
     ALLOCATE ( U( Nrd_per_range, Nr ), Stat = IAllocStat )
     IF ( IAllocStat /= 0 ) &
          CALL ERROUT( PRTFIL, 'F', 'BELLHOP', 'Insufficient memory for TL matrix: reduce Nr * Nrd'  )
  ELSE
     ALLOCATE ( U( 1, 1 ), Stat = IAllocStat )
  ENDIF

  ! for an arrivals run, allocate space for arrivals matrices
  IF ( SCAN( 'Aa', RunType(1:1) ) /= 0 ) THEN
     MaxNArr = MAX( ArrivalsStorage / ( Nrd_per_range * Nr ), 10 )   ! allow space for at least 10 arrivals
     WRITE( PRTFIL, * )
     WRITE( PRTFIL, * ) '( Maximum # of arrivals = ', MaxNArr, ')'

     ALLOCATE ( AArr( Nrd_per_range, Nr, MaxNArr ), PhaseArr( Nrd_per_range, Nr, MaxNArr ), DelArr( Nrd_per_range, Nr, MaxNArr ), &
          SrcAngArr( Nrd_per_range, Nr, MaxNArr ), RcvrAngArr( Nrd_per_range, Nr, MaxNArr ), &
          NArr( Nrd_per_range, Nr ), NTopBncArr( Nrd_per_range, Nr, MaxNArr ), NBotBncArr( Nrd_per_range, Nr, MaxNArr ), &
          Stat = IAllocStat )
     IF ( IAllocStat /= 0 ) &
          CALL ERROUT( PRTFIL, 'F', 'BELLHOP', 'Insufficient memory to allocate arrivals matrix; reduce parameter ArrivalsStorage' )
  ELSE
     MaxNArr = 1
     ALLOCATE ( AArr( Nrd_per_range, Nr, 1 ), PhaseArr( Nrd_per_range, Nr, 1 ), DelArr( Nrd_per_range, Nr, 1 ), &
          SrcAngArr( Nrd_per_range, Nr, 1 ), RcvrAngArr( Nrd_per_range, Nr, 1 ), &
          NArr( Nrd_per_range, Nr ), NTopBncArr( Nrd_per_range, Nr, 1 ), NBotBncArr( Nrd_per_range, Nr, 1 ), Stat = IAllocStat )
  END IF

  IF ( Nr > 1 ) THEN
     DeltaR = r( Nr ) - r( Nr - 1 )
  ELSE
     DeltaR = 0.0
  ENDIF

  omega  = 2.0 * pi * freq
  alpha  = DegRad * alpha   ! convert to radians
  Dalpha = 0.0
  IF ( NBeams /= 1 ) Dalpha = ( alpha( NBeams ) - alpha( 1 ) ) / ( NBeams - 1 )  ! angular spacing between beams

  WRITE( *, * )
  DO IS = 1, Nsd   ! Loop over source depth
     xs = (/ 0.0, sd( IS ) /)   ! source coordinate

     IF ( SCAN( 'CSI', RunType(1:1) ) /= 0 ) U    = 0.0 ! For a TL run, zero out pressure matrix
     IF ( SCAN( 'Aa',  RunType(1:1) ) /= 0 ) NArr = 0   ! For an arrivals run, zero out arrival matrix

     CALL SSP( xs, c, gradc, crr, crz, czz, TopOpt, 'TAB' )
     RadMax = 5 * c / freq  ! 5 wavelength max radius

     ! Are there enough beams?
     DalphaOpt = SQRT( c / ( 6.0 * freq * r( Nr ) ) )
     NBeamsOpt = 2 + ( alpha( NBeams ) - alpha( 1 ) ) / DalphaOpt

     IF ( RunType(1:1) == 'C' .AND. NBeams < NBeamsOpt ) THEN
        CALL ERROUT( PRTFIL, 'W', 'BELLHOP', 'Too few beams' )
        WRITE( PRTFIL, * ) 'NBeams should be at least = ', NBeamsOpt
     ENDIF

     ! Trace successive beams

     DO ibeam = 1, NBeams

        IF ( iSingle == 0 .OR. ibeam == iSingle ) THEN    ! Single beam run?

           alpha0 = alpha( ibeam ) * 180.0 / pi   ! take-off angle in degrees
           IBPvec = maxloc( SrcBmPat( :, 1 ), mask = SrcBmPat( :, 1 ) < alpha0 )       ! index of ray angle in beam pattern
           IBP    = IBPvec( 1 )
           IBP    = MAX( IBP, 1 )               ! don't go before beginning of table
           IBP    = MIN( IBP, NSBPPts - 1 )     ! don't go past end of table

           ! linear interpolation to get amplitudeIsegBot( CrossBot ) )'
           s    = ( alpha0 - SrcBmPat( IBP, 1 ) ) / ( SrcBmPat( IBP + 1, 1 ) - SrcBmPat( IBP, 1 ) )
           Amp0 = ( 1 - s ) * SrcBmPat( IBP, 2 ) + s * SrcBmPat( IBP + 1, 2 )

           WRITE( *, * ) 'Tracing beam ', ibeam, alpha0
           CALL TRACE( deltas, xs, alpha( ibeam ), Amp0, BeamType, zBox, rBox, BotOpt, RunType )   ! Trace a ray

           IF ( RunType(1:1) == 'R' ) THEN     ! Write the ray trajectory to RAYFIL
              CALL WRTRAY( alpha0, xv, Trayv, Nsteps, NumTopBnc( Nsteps ), NumBotBnc( Nsteps ), DepthT, DepthB )
           ELSE                                ! Compute the contribution to the field

              Eps = PICKEPS( BeamType(1:1), omega, c, cZ, alpha( ibeam ), Dalpha, rLoop, epsMultiplier ) ! 'optimal' beam constant

              SELECT CASE ( RunType(2:2) )
              CASE ( 'R' )
                 iBeamWindow2 = iBeamWindow **2
                 CALL INFLUR(   U, DeltaR, Eps, alpha( IBeam ), NImage, IBeamWindow2, RunType, RadMax, BeamType, Component )
              CASE ( 'C' )
                 iBeamWindow2 = iBeamWindow **2
                 CALL INFLUC(   U, DeltaR, Eps, alpha( IBeam ), NImage, IBeamWindow2, RunType, RadMax, BeamType )
              CASE ( 'S' )
                 CALL INFLUSGB( U,  sd( IS ), alpha( IBeam ), RunType, Dalpha, deltas )
              CASE ( 'B' )
                 CALL INFLUGRB( U,  sd( IS ), alpha( IBeam ), RunType, Dalpha, deltas )
              CASE DEFAULT
                 CALL INFLUG(   U,  sd( IS ), alpha( IBeam ), RunType, Dalpha )
              END SELECT

           END IF
        END IF
     END DO ! Next beam

     ! write results to disk

     IF ( SCAN( 'CSI', RunType(1:1) ) /= 0 ) THEN   ! TL calculation
        CALL SCALEP( Dalpha, cV( 1 ), R, U, Nrd_per_range, Nr, RunType, TopOpt, freq )
        IRec  = 7 + Nrd_per_range * ( IS - 1 )
        DO Ird1 = 1, Nrd_per_range
           IRec = IRec + 1
           WRITE( SHDFil, REC = IRec ) U( Ird1, 1 : Nr )
        END DO

     ELSE IF ( RunType(1:1) == 'A' ) THEN   ! arrivals calculation, ascii
        CALL WRTARRASC( R, Nrd_per_range, Nr, TopOpt, freq, RunType(4:4) )
     ELSE IF ( RunType(1:1) == 'a' ) THEN   ! arrivals calculation, binary
        CALL WRTARRBIN( R, Nrd_per_range, Nr, TopOpt, freq, RunType(4:4) )
     END IF

  END DO    ! Next source depth

  ! close all files
  IF ( SCAN( 'CSI', RunType(1:1) ) /= 0 ) THEN   ! TL calculation
     CLOSE( SHDFIL )
  ELSE IF ( RunType(1:1) == 'A' ) THEN   ! arrivals calculation, ascii
     CLOSE( ARRFIL )
  ELSE IF ( RunType(1:1) == 'a' ) THEN   ! arrivals calculation, binary
     CLOSE( ARRFIL )
  ELSE IF ( RunType(1:1) == 'R' ) THEN
     CLOSE( RAYFIL )
  END IF

  ! Display run time
  CALL CPU_TIME( Tstop )
  WRITE( PRTFIL, "( /, ' CPU Time = ', G15.3 )" ) Tstop - Tstart

  STOP
END PROGRAM BELLHOP

! **********************************************************************!

FUNCTION PICKEPS( BeamType, omega, c, cZ, alpha, Dalpha, rLoop, EpsMult )

  ! Picks the optimum value for epsilon

  INTEGER, PARAMETER :: PRTFIL = 6
  COMPLEX, PARAMETER :: i = ( 0.0, 1.0 )
  LOGICAL, SAVE      :: INIFLG = .TRUE.
  COMPLEX               PICKEPS, EpsOpt
  CHARACTER             BeamType*1, TAG*40

  SELECT CASE ( BeamType )

  CASE ( 'F' )
     TAG    = 'Space filling beams'
     halfwidth = 2.0 / ( ( omega / c ) * Dalpha )
     EpsOpt = i * 0.5 * omega * halfwidth ** 2
  CASE ( 'M' )
     TAG    = 'Minimum width beams'
     halfwidth = SQRT( 2.0 * c * 1000.0 * rLoop / omega )
     EpsOpt = i * 0.5 * omega * halfwidth ** 2
  CASE ( 'W' )
     TAG    = 'WKB beams'
     IF ( cZ == 0.0 ) THEN
        EpsOpt = 1.0E10
     ELSE
        EpsOpt = ( -SIN( alpha ) / COS( alpha ** 2 ) ) * c * c / cZ
     ENDIF
  CASE ( 'C' )
     TAG    = 'Cerveny style beam'
  END SELECT

  PICKEPS = EpsMult * EpsOpt

  ! On first call write info to prt file
  IF ( INIFLG ) THEN
     WRITE( PRTFIL, * )
     WRITE( PRTFIL, * ) TAG
     WRITE( PRTFIL, * ) 'halfwidth  = ', halfwidth
     WRITE( PRTFIL, * ) 'EpsOpt     = ', EpsOpt
     WRITE( PRTFIL, * ) 'EpsMult    = ', EpsMult
     WRITE( PRTFIL, * )
     INIFLG = .FALSE.
  END IF

  RETURN
END FUNCTION PICKEPS

! **********************************************************************!

SUBROUTINE TRACE( deltas, xs, alpha, Amp0, BeamType, zBox, rBox, BotOpt, RunType )

  ! Traces the beam corresponding to a particular take-off angle

  USE bellMod
  USE bdryMod
  USE RefCoMod

  INTEGER          IsegTopT( 1 ), IsegBotT( 1 )
  REAL             xs( 2 ), gradc( 2 )
  CHARACTER        BotOpt*3, BeamType*3, BC*1, RunType*5
  REAL (KIND=8) :: DbegTop( 2 ), DendTop( 2 ), DbegBot( 2 ), DendBot( 2 )

  ! Initial conditions

  CALL SSP( xs, c, gradc, crr, crz, czz, TopOpt, 'TAB' )

  NumTopBnc( 1 ) = 0
  NumBotBnc( 1 ) = 0
  cV(        1 ) = c
  xv(     :, 1 ) = xs
  TrayV(  :, 1 ) = (/ COS( alpha ), SIN( alpha ) /) / c
  pV(     :, 1 ) = (/ 1.0, 0.0 /)
  qV(     :, 1 ) = (/ 0.0, 1.0 /)
  tauV(      1 ) = 0.0
  AmpV(      1 ) = Amp0
  PhaseV(    1 ) = 0.0

  ! second component of qv is not used in geometric beam tracing
  ! set I.C. to 0 in hopes of saving run time
  IF (RunType(2:2) == 'G' ) qV( :,  1 ) = (/ 0.0, 0.0 /)

  ! identify the top segment above the source

  IsegTopT = MAXLOC( xTop( 1, 1:NatiPts), xTop( 1, 1:NatiPts) <= xs( 1 ) )

  IF ( IsegTopT( 1 ) > 0 .AND. IsegTopT( 1 ) < NatiPts ) THEN
     IsegTop  = IsegTopT( 1 )   ! IsegTop MUST LIE IN [ 1, NatiPts-1 ]
  ELSE
     CALL ERROUT( PRTFIL, 'F', 'TRACE', 'Top altimetry undefined above the source' )
  ENDIF

  ! identify the bottom segment below the source

  IsegBotT = MAXLOC( xbot( 1, 1:NbtyPts), xbot( 1, 1:NbtyPts) <= xs( 1 ) )

  IF ( IsegBotT( 1 ) > 0 .AND. IsegBotT( 1 ) < NbtyPts ) THEN
     IsegBot  = IsegBotT( 1 )   ! IsegBot MUST LIE IN [ 1, NbtyPts-1 ]
  ELSE
     CALL ERROUT( PRTFIL, 'F', 'TRACE', 'Bottom bathymetry undefined below the source' )
  ENDIF

  ! Trace the beam
  ! (note that REFLECT alters the step index is)

  is = 0
  DbegTop    = xv( :, 1 ) - xTop( :, IsegTop )  ! vector pointing from top    to ray
  DbegBot    = xv( :, 1 ) - xbot( :, IsegBot )  ! vector pointing from bottom to ray
  DistBegTop = DOT_PRODUCT( nTop( :, IsegTop ), DbegTop )
  DistBegBot = DOT_PRODUCT( nBot( :, IsegBot ), DbegBot )

  ! !!! note above distance is really a negative distance (throughout the code)

  IF ( DistBegTop >= 0 .OR. DistBegBot >= 0 ) THEN
     Nsteps = 1
     RETURN       ! source must be within the medium
  END IF

  STEPPING: DO ISTEP = 1, MaxN - 1
     is = is + 1

     CALL STEP( &
          xv( :, is   ), Trayv( :, is   ), pV(  :, is   ), qV( :, is   ), tauV( is   ), AmpV( is   ), PhaseV( is  ), cV( is   ),  &
          xv( :, is+1 ), Trayv( :, is+1 ), pV(  :, is+1 ), qV( :, is+1 ), tauV( is+1 ), AmpV( is+1 ), PhaseV( is+1), cV( is+1 ),  &
          xTop( :, IsegTop ), nTop( :, IsegTop ), &
          xBot( :, IsegBot ), nBot( :, IsegBot ), deltas, TopOpt )
     NumTopBnc( is + 1 ) = NumTopBnc( is )
     NumBotBnc( is + 1 ) = NumBotBnc( is )

     ! New altimetry segment?
     IF ( xv( 1, is + 1 ) < xTop( 1, IsegTop ) .OR. xv( 1, is + 1 ) > xTop( 1, IsegTop + 1 ) ) THEN
        IsegTopT = MAXLOC( xTop( 1, : ), xTop( 1, : ) < xv( 1, is + 1 ) )
        IF( IsegTopT( 1 ) > 0 .AND. IsegTopT( 1 ) < NatiPts ) IsegTop  = IsegTopT( 1 )  ! IsegTop MUST LIE IN [ 1, NatiPts-1 ]
     END IF

     ! New bathymetry segment?
     IF ( xv( 1, is + 1 ) < xbot( 1, IsegBot ) .OR. xv( 1, is + 1 ) > xbot( 1, IsegBot + 1 ) ) THEN
        IsegBotT = MAXLOC( xbot( 1, : ), xbot( 1, : ) < xv( 1, is + 1 ) )
        IF( IsegBotT( 1 ) > 0 .AND. IsegBotT( 1 ) < NbtyPts ) IsegBot  = IsegBotT( 1 )  ! IsegBot MUST LIE IN [ 1, NbtyPts-1 ]
     END IF

     ! Reflections?
     ! Tests that ray at step i is inside, and ray at step i+1 is outside
     ! to detect only a crossing from inside to outside

     DendTop    = xv( :, is + 1 ) - xTop( :, IsegTop )  ! vector pointing from top    to ray
     DendBot    = xv( :, is + 1 ) - xbot( :, IsegBot )  ! vector pointing from bottom to ray
     DistEndTop = DOT_PRODUCT( nTop( :, IsegTop ), DendTop )
     DistEndBot = DOT_PRODUCT( nBot( :, IsegBot ), DendBot )

     IF      ( DistBegTop < 0.0 .AND. DistEndTop >= 0.0 ) THEN
        BC = TopOpt(2:2)
        CALL REFLECT( is, BeamType, BC, cPT, rhoT, 'TOP', tTop( :, IsegTop  ), nTop( :, IsegTop ), &
             thetaTop, RTop, phiTop, NTopPTS )
        NumTopBnc( is + 1 ) = NumTopBnc( is ) + 1
        DendTop    = xv( :, is + 1 ) - xTop( :, IsegTop )  ! vector pointing from top    to ray
        DendBot    = xv( :, is + 1 ) - xbot( :, IsegBot )  ! vector pointing from bottom to ray
        DistEndTop = DOT_PRODUCT( nTop( :, IsegTop ), DendTop )
        DistEndBot = DOT_PRODUCT( nBot( :, IsegBot ), DendBot )
     ELSE IF ( DistBegBot < 0.0 .AND. DistEndBot >= 0.0 ) THEN  ! test bottom reflection
        BC = BotOpt(1:1)
        CALL REFLECT( is, BeamType, BC, cPB, rhoB, 'BOT', tBot( :, IsegBot  ), nBot( :, IsegBot ), &
             thetaBot, RBot, phiBot, NBotPTS )
        NumBotBnc( is + 1 ) = NumBotBnc( is ) + 1
        DendTop    = xv( :, is + 1 ) - xTop( :, IsegTop )  ! vector pointing from top    to ray
        DendBot    = xv( :, is + 1 ) - xbot( :, IsegBot )  ! vector pointing from bottom to ray
        DistEndTop = DOT_PRODUCT( nTop( :, IsegTop ), DendTop )
        DistEndBot = DOT_PRODUCT( nBot( :, IsegBot ), DendBot )
     END IF

     DBegTop    = DEndTop
     DbegBot    = DEndBot
     DistBegTop = DistEndTop
     DistBegBot = DistEndBot

     ! Has the ray left the box, lost its energy, or exceeded storage limit?
     IF ( ABS( xv( 1, is + 1 ) ) > rBox .OR. xv( 2, is + 1 ) > zBox .OR. AmpV( is + 1 ) < 0.005 ) THEN
        Nsteps = is + 1
        EXIT STEPPING
     ELSE IF ( is >= MaxN - 3 ) THEN
        CALL ERROUT( PRTFIL, 'W', 'TRACE', 'Insufficient storage for ray trajectory' )
        Nsteps = is
        EXIT STEPPING
     END IF
  END DO STEPPING   ! Next step

  RETURN
END SUBROUTINE TRACE

! **********************************************************************!

SUBROUTINE STEP( x0, Tray0, P0, q0, tau0, Amp0, Phase0, c0, &
                 x2, Tray2, P2, q2, tau2, Amp2, Phase2, c2, &
                 xTop, nTop, xBot, nBot, deltas, TopOpt )

  ! Does a single step along the ray
  ! x denotes the ray coordinate, (r,z)
  ! Tray denotes the scaled tangent to the ray (previously (rho, zeta))

  USE sspMod

  REAL x(2), gradc0( 2 ), gradc1( 2 ), gradc2( 2 ), Amp0, Phase0, Amp2, Phase2
  REAL (KIND=8) :: &
       x0(2), TRay0(2), p0(2), q0(2), tau0, &
       x1(2), Tray1(2), p1(2), q1(2),       &
       x2(2), Tray2(2), p2(2), q2(2), tau2, &
       xTop(2), nTop(2), xBot(2), nBot(2), d( 2 ), e( 2 )
  CHARACTER TopOpt*4, Crossing*8

  ! Phase 1 of modified polygon method (an Euler step)

  CALL SSP( SNGL( x0 ), c0, gradc0, crr0, crz0, czz0, TopOpt, 'TAB' )

  Layer0    = Layer     ! make note of current layer
  csq0      = c0 * c0
  cnn0_csq0 = crr0 * Tray0(2)**2 - 2.0 * crz0 * Tray0(1) * Tray0(2) + czz0 * Tray0(1)**2

  h = deltas            ! initially set the step h, to the basic one, deltas
  x = x0 + H * c0 * Tray0 ! make a trial step

  ! Detect interface or boundary crossing
  Crossing = 'none'
  IF ( Tray0(2) /= 0.0 ) THEN
     IF      ( x(2) < zSSPV( Layer     ) .AND. Layer /=1       ) THEN
        Crossing = 'intabove'
     ELSE IF ( x(2) > zSSPV( Layer + 1 ) .AND. Layer /= NSSP-1 ) THEN
        Crossing = 'intbelow'
     END IF
  END IF

  d = x - xTop              ! vector from top to ray
  IF ( DOT_PRODUCT( nTop, d ) > 0.0 ) Crossing = 'Top'

  d = x - xBot              ! vector from bottom to ray
  IF ( DOT_PRODUCT( NBot, d ) > 0.0 ) Crossing = 'Bottom'

  ! Adjust step to land on an interface or boundary
  SELECT CASE ( Crossing )
  CASE ( 'intabove' )
     h = ( zSSPV( Layer     ) - x0(2) ) / ( Tray0(2) * c0 )
  CASE ( 'intbelow' )
     h = ( zSSPV( Layer + 1 ) - x0(2) ) / ( Tray0(2) * c0 )
  CASE ( 'Top' )
     e =  x0 - xTop         ! vector from top to ray origin
     h = -DOT_PRODUCT( e, nTop ) / ( c0 * DOT_PRODUCT( Tray0, nTop ) )
  CASE ( 'Bottom' )
     e =  x0 - xBot         ! vector bottom to ray
     h = -DOT_PRODUCT( e, nBot ) / ( c0 * DOT_PRODUCT( Tray0, nBot ) )
  CASE DEFAULT
  END SELECT

  h     = MAX( h, 1e4 * EPSILON( deltas ) * deltas )   ! make sure we make some motion
  halfh = 0.5 * h   ! first step of the modified polygon method is a half step

  x1    = x0    + halfh * c0 * Tray0
  Tray1 = Tray0 - halfh * gradc0 / csq0
  p1    = p0    - halfh * cnn0_csq0 * q0
  q1    = q0    + halfh * c0        * p0

  ! Phase 2 of modified polygon method

  CALL SSP( SNGL( x1 ), c1, gradc1, crr1, crz1, czz1, TopOpt, 'TAB' )

  csq1      = c1 * c1
  cnn1_csq1 = crr1 * Tray1(2)**2 - 2.0 * crz1 * Tray1(1) * Tray1(2) + czz1 * Tray1(1)**2

  SELECT CASE ( Crossing )
  CASE ( 'intabove' )
     h = ( zSSPV( Layer0     ) - x0(2) ) / ( Tray1(2) * c1 )
  CASE ( 'intbelow' )
     h = ( zSSPV( Layer0 + 1 ) - x0(2) ) / ( Tray1(2) * c1 )
  CASE ( 'Top' )
     h = -DOT_PRODUCT( e, nTop ) / ( c1 * DOT_PRODUCT( Tray1, nTop ) )
  CASE ( 'Bottom' )
     h = -DOT_PRODUCT( e, nBot ) / ( c1 * DOT_PRODUCT( Tray1, nBot ) )
  CASE DEFAULT
  END SELECT

  h = MAX( h, 1e4 * EPSILON( deltas ) * deltas )   ! make sure we make some motion

  x2     = x0    + h * c1 * Tray1
  Tray2  = Tray0 - h * gradc1 / csq1
  p2     = p0    - h * cnn1_csq1 * q1
  q2     = q0    + h * c1        * p1
  tau2   = tau0  + h / c1
  Amp2   = Amp0
  Phase2 = Phase0

  ! If we crossed an interface, apply jump condition

  CALL SSP( SNGL( x2 ), c2, gradc2, crr2, crz2, czz2, TopOpt, 'TAB' )

  IF ( Layer /= Layer0 ) THEN
     RN = -Tray2( 1 ) ** 2 / Tray2( 2 ) * ( gradc2( 2 ) - gradc0( 2 ) ) / c0   ! needs updating for c(r,z) problem
     p2 = p2 + q2 * RN
  END IF

  RETURN
END SUBROUTINE STEP

! **********************************************************************!

SUBROUTINE REFLECT( is, BeamType, BC, cHS, rhoHS, BOTTOP, tbdry, nbdry, theta, RefC, phi, Npts )

  USE bellMod
  USE sspMod
  USE RefCoMod

  REAL                gradc( 2 )
  REAL    (KIND=8) :: rhoHS, theInt, rInt, phiInt, tbdry( 2 ), nbdry( 2 ), cN, cS, RM, RN, Tg, Th
  REAL    (KIND=8) :: theta( Npts ), RefC( Npts ), phi( Npts )
  COMPLEX             gamma1, gamma2, gamma1Sq, gamma2Sq, GK, Refl
  COMPLEX (KIND=8) :: cHS, ch, a, b, d, sb, delta, ddelta
  CHARACTER           BeamType*3, BC*1, BotTop*3

  is = is + 1
  NumTopBnc( is + 1 ) = NumTopBnc( is )
  NumBotBnc( is + 1 ) = NumBotBnc( is )

  ! here's the geometric part, changing the ray direction
  xv( :, is+1 ) = xv( :, is )

  Tg = DOT_PRODUCT( trayv( :, is ), TBdry )  ! component of ray tangent, along boundary
  Th = DOT_PRODUCT( trayv( :, is ), NBdry )  ! component of ray tangent, normal to boundary

  Trayv( :, is+1 ) =  Trayv( :, is ) - 2.0 * Th * NBdry

  ! Calculate the change in curvature
  ! Based on formulas given by Muller, Geoph. J. R.A.S., 79 (1984).

  CALL SSP( SNGL( xv( :, is + 1 ) ), c, gradc, crr, crz, czz, TopOpt, 'TAB' )

  cV( is ) = c
  cn       = gradc( 2 ) * Trayv( 1, is )
  cs       = gradc( 2 ) * Trayv( 2, is )   ! assumes gradc( 2 ) = cr = 0

  IF ( BOTTOP == 'TOP' ) cn = -cn    ! flip sign for top reflection

  RM = Tg / Th
  RN = RM * ( 4 * cn - 2 * RM * cs ) / c

  SELECT CASE ( BeamType(2:2) )
  CASE ( 'D' )
     RN = 2.0 * RN
  CASE ( 'Z' )
     RN = 0.0
  END SELECT

  pV( :, is + 1 ) = pV( :, is ) + qV( :, is ) * RN
  qV( :, is + 1 ) = qV( :, is )

  ! account for phase change

  SELECT CASE ( BC )
  CASE ( 'R' )                 ! rigid
     tauV(   is + 1 ) = tauV(   is )
     AmpV(   is + 1 ) = AmpV(   is )
     PhaseV( is + 1 ) = PhaseV( is )
  CASE ( 'V' )                 ! vacuum
     tauV(   is + 1 ) = tauV(   is )
     AmpV(   is + 1 ) = AmpV(   is )
     PhaseV( is + 1 ) = PhaseV( is ) + pi
  CASE ( 'F' )                 ! file
     theInt = RadDeg * ABS( ATAN2( Th, Tg ) )   ! angle of incidence (relative to normal to bathymetry)
     IF ( theInt > 90 ) theInt = 180. - theInt  ! reflection coefficient is symmetric about 90 degrees
     CALL RefCO( theInt, rInt, phiInt, theta, RefC, phi, Npts, PRTFil )
     tauV(   is + 1 ) = tauV(   is )
     AmpV(   is + 1 ) = AmpV(   is ) * rInt
     PhaseV( is + 1 ) = PhaseV( is ) + phiInt
  CASE ( 'A' )                 ! half-space
     GK     = omega * Tg   ! wavenumber in direction parallel to bathymetry
     gamma1Sq = ( omega / c   ) ** 2 - GK ** 2 - i * tiny( omega )! tiny is because g95 Fortran can get -zero, leading to wrong branch cut
     gamma2Sq = ( omega / cHS ) ** 2 - GK ** 2 - i * tiny( omega )
     gamma1   = SQRT( -gamma1Sq )
     gamma2   = SQRT( -gamma2Sq )

     Refl = ( rhoHS * gamma1 - gamma2 ) / ( rhoHS * gamma1 + gamma2 )

     IF ( ABS( Refl ) < 1.0E-5 ) THEN   ! kill a ray that has lost its energy in reflection
        tauV(   is + 1 ) = tauV( is )
        AmpV(   is + 1 ) = 0.0
        PhaseV( is + 1 ) = PhaseV( is )
     ELSE
        tauV(   is + 1 ) = tauV( is )
        AmpV(   is + 1 ) = ABS( Refl ) * AmpV(  is )
        PhaseV( is + 1 ) = PhaseV( is ) + ATAN2( AIMAG( Refl ), REAL( Refl ) )

        ! compute beam-displacement Tindle, Eq. (14)
        ! needs a correction to beam-width as well ...
        !  IF ( REAL( gamma2Sq ) < 0.0 ) THEN
        !     rhoW   = 1.0   ! density of water
        !     rhoWSq  = rhoW  * rhoW
        !     rhoHSSq = rhoHS * rhoHS
        !     DELTA = 2 * GK * rhoW * rhoHS * ( gamma1Sq - gamma2Sq ) /
        ! &( gamma1 * i * gamma2 *
        ! &( -rhoWSq * gamma2Sq + rhoHSSq * gamma1Sq ) )
        !     RV( is + 1 ) = RV( is + 1 ) + DELTA
        !  END IF

        if ( BeamType(3:3) == 'S' ) then   ! beam displacement & width change (Seongil's version)

           ch = cV( is ) / conjg( chs )
           co = Trayv( 1, is ) * cV( is )
           si = Trayv( 2, is ) * cV( is )
           ck = omega / cV( is )

           a   = 2 * rhoHS * (1 - ch * ch )
           b   = co * co - ch * ch
           d   = rhoHS * rhoHS * si * si + b
           sb  = sqrt( b )
           cco = co * co
           ssi = si * si

           delta  = a * co / si / ( ck * sb * d )    
           pdelta = real( delta ) / ( cV( is ) / co)

           ddelta  = -a / ( ck*sb*d ) - a*cco / ssi / (ck*sb*d) + a*cco / (ck*b*sb*d) &
                -a*co / si / (ck*sb*d*d) * (2*rhoHS*rhoHS*si*co-2*co*si)
           rddelta = -real( ddelta )
           sddelta = rddelta / abs( rddelta )        

           xv(   1, is+1 ) = xv( 1,  is+1 ) + real( delta )   ! displacement
           tauV(    is+1 ) = tauV( is+1 ) + pdelta            ! phase change
           qV( :,   is+1 ) = qV( :, is+1 ) + sddelta * rddelta * si * c * pV( :, is ) ! beam width change
        endif

     ENDIF
  END SELECT

  RETURN
END SUBROUTINE REFLECT

!*******************************************************************!

SUBROUTINE INFLUR( U, deltar, eps, alpha, NImage, iBeamWindow2, RunType, RadMax, BeamType, Component )

  ! Computes the beam influence, i.e. the contribution of a single beam to the complex pressure
  ! This routine is for the Cerveny-style beams

  USE bellMod
  USE SdRdRMod

  INTEGER   KMAHV( MaxN )
  REAL      n, nA, nB, nSq
  COMPLEX   pVB( MaxN ), qVB( MaxN ), q, eps, epsV( MaxN ), contri, U( Nrd_per_range, Nr ), gammaV( MaxN ), gamma, P_n, P_s
  CHARACTER RunType*5, BeamType*3, Component*1

  ! Note that during reflection imag(q) is constant and
  ! adjacent normals cannot bracket a segment of the TL
  ! line, so no special treatment is necessary

  DS = 2 * ( SIN( omega * xv( 2, 1 ) * Trayv( 2, 1 ) ) ) ** 2   ! Lloyd mirror pattern
  IF ( BeamType(1:1) == 'C' ) THEN
     epsV = i * ABS( qV(1, :) / qV(2, :) )
  ELSE
     epsV( 1 : Nsteps ) = eps
  ENDIF

  pVB(    : ) = pV( 1, : ) + epsV * pV( 2, : )
  qVB(    : ) = qV( 1, : ) + epsV * qV( 2, : )
  gammaV( : ) = pVB( : ) / qVB( : )

  ! compute KMAH index
  ! Following is incorrect for 'Cerveny'-style beamwidth (narrow as possible)
  KMAHV(  1 ) = 1
  DO is = 2, Nsteps
     KMAHV(  is ) = KMAHV( is - 1 )
     CALL BRCUT( qVB( is - 1 ), qVB( is ), BeamType, KMAHV( is ) )
  END DO

  DO id = 1, Nrd ! Loop over receivers
     ZR = RD( id )

     DO Image = 1, NImage ! Loop over images
        ir1 = 9999

        DO is = 2, Nsteps  ! Loop over steps
           IF ( ABS( xv( 1, is ) - xv( 1, is - 1 ) ) < TINY( xv( 1, is ) ) ) CYCLE   ! don't process duplicate points

           ! Compute ray-centered coordinates, (ZNV, RNV)
           ZNV = -Trayv( 1, is ) * cV( is )
           RNV =  Trayv( 2, is ) * cV( is )
           IF ( ABS( ZNV ) < tiny( ZNV ) ) THEN   ! Check for normal parallel to TL-line
              CYCLE   ! skip to next step on ray
           ENDIF

           SELECT CASE ( Image )     ! Images of beams
           CASE ( 1 )                ! True beam
              nB  = ( ZR -                  xv( 2, is )   ) / ZNV
           CASE ( 2 )                ! Surface reflected beam
              RNV = -RNV
              nB  = ( ZR - ( 2.0 * DepthT - xv( 2, is ) ) ) / ZNV
           CASE ( 3 )                ! Bottom reflected beam
              RNV = -RNV
              nB  = ( ZR - ( 2.0 * DepthB - xv( 2, is ) ) ) / ZNV
           END SELECT

           rB  = xv( 1, is ) + nB * RNV
           ir2 = MAX( MIN( INT( ( rB - r(1) ) / deltar ) + 1, Nr ), 1 ) ! index of receiver

           DO ir = ir1 + 1, ir2    ! Compute influence for each rcvr
              W     = ( r( ir ) - rA ) / ( rB - rA )
              q     =    qVB( is-1 ) + W * (    qVB( is ) -    qVB( is-1 ) )
              gamma = gammaV( is-1 ) + W * ( gammaV( is ) - gammaV( is-1 ) )
              n     = nA + W * ( nB - nA )
              nSq   = n * n

              IF ( AIMAG( gamma ) > 0 ) THEN
                 WRITE( PRTFIL, * ) 'Unbounded beam'
                 CYCLE   ! next receiver depth
              ENDIF

              IF ( -0.5 * omega * AIMAG( gamma ) * nSq < iBeamWindow2 ) THEN   ! Within beam window?
                 c      =   cV( is-1 ) + W * (   cV( is ) -   cV( is-1 ) )
                 tau    = tauV( is-1 ) + W * ( tauV( is ) - tauV( is-1 ) )
                 contri = AmpV( is ) * SQRT( c * ABS( epsV( is ) ) / q * COS( alpha ) )* &
                          EXP( -i * omega * ( tau - phaseV( is ) +  0.5 * gamma * nSq ) )
 
                 SELECT CASE ( Component )
                 CASE ( 'P' )   ! pressure
                 CASE ( 'V' )   ! vertical component
                    P_n = -i * omega * gamma * n * contri
                    P_s = -i * omega / c         * contri
                    contri = c * DOT_PRODUCT( (/ P_n, P_s /), TrayV( :, is ) ) 
                 CASE ( 'H' )   ! horizontal component
                    P_n = -i * omega * gamma * n * contri
                    P_s = -i * omega / c         * contri
                    contri = c * ( -P_n * TrayV( 2, is ) + P_s * TrayV( 1, is ) ) 
                 END SELECT

                 KMAH = KMAHV( is - 1 )
                 CALL BRCUT( qVB( is - 1 ), q, BeamType, KMAH ) ! Get correct branch of SQRT

                 IF ( KMAH  < 0  ) contri = -contri
                 IF ( Image == 2 ) contri = -contri

                 SELECT CASE ( RunType(1:1) )
                 CASE ( 'I' )    ! Incoherent TL
                    contri =      ABS(contri)
                 CASE ( 'S' )    ! Semi-coherent TL
                    contri = DS * ABS(contri)
                 END SELECT

                 U( id, ir ) = U( id, ir ) + HERMITE( n, RadMax, 2 * RadMax ) * contri
              ENDIF
           END DO   ! next ir
           rA  = rB
           nA  = nB
           ir1 = ir2
        END DO   ! Next step, is
     END DO   ! Next image
  END DO   ! Next receiver depth

  RETURN
END SUBROUTINE INFLUR
! **********************************************************************!
SUBROUTINE INFLUC( U, deltar, eps, alpha, NImage, iBeamWindow2, RunType, RadMax, BeamType )

  ! Computes the beam influence, i.e. 
  ! the contribution of a single beam to the complex pressure
  ! This version uses a beam representation in Cartesian coordinates

  USE bellMod
  USE SdRdRMod

  INTEGER   KMAHV( MaxN )
  REAL      x(2), Tray(2), Nray( 2 ), gradc(2)
  COMPLEX   pVB( MaxN ), qVB( MaxN ), q, eps, contri, U( Nrd, Nr ), gammaV( MaxN ), gamma, const
  CHARACTER RunType*5, BeamType*3

  ! Note that during reflection imag(q) is constant and
  ! adjacent normals cannot bracket a segment of the TL
  ! line, so no special treatment is necessary

  DS = 2.0 * ( SIN( omega * xv(2,1) * Trayv( 2, 1 ) ) )**2   ! Lloyd mirror pattern

  ! Begin by forming (p, q) and KMAH index
  ! Note treatment of KMAH index is incorrect for 'Cerveny' style beam width BeamType

  DO is = 1, Nsteps
     IF ( BeamType(1:1) == 'C' ) eps = i * ABS( qV(1,is) / qV(2,is) )

     pVB( is ) = pV( 1, is ) + eps * pV( 2, is )
     qVB( is ) = qV( 1, is ) + eps * qV( 2, is )
     ! WRITE( *, * ) is, p1v( is ), q1v( is ), p2v( is ), q2v( is )
     ! RLTEMP = SQRT( -2.0 / ( omega * AIMAG( pV( is ) / qV( is ) ) ) )
     ! RKTEMP = -cV( is ) * REAL( pV( is ) / qV( is ) )
     ! WRITE( PRTFIL, * ) RLTEMP, RKTEMP

     ! IF ( BeamType(2:2) == 'D' ) THEN
     !  pV( is ) = REAL( pV( is ) )
     !  P = pV( is )
     !  L = 30.0 * 1500.0 / omega
     !  qV( is ) = ( i * omega * L**2 / 2.0) * P
     ! ENDIF

     TRay = cV( is ) * Trayv( :, is )
     NRay = (/ Tray( 2 ), -Tray( 1 ) /)
     CALL SSP( SNGL( xv( :, is ) ), c, gradc, crr, crz, czz, TopOpt, 'TAB')
     Tr  = Tray(  1 )
     Tz  = Tray(  2 )
     csq = c * c
     cS  = DOT_PRODUCT( gradc, Tray )
     cN  = DOT_PRODUCT( gradc, Nray )

     gammaV( is ) = 0.0
     IF ( qVB( is ) /= 0.0 ) gammaV( is ) = 0.5 * ( pVB(is) / qVB(is) * TR**2 + &
          2.0 * cN / csq * TZ * TR - cS / csq * TZ**2 )

     IF ( is == 1 ) THEN
        KMAHV( 1 ) = 1
     ELSE
        KMAHV( is ) = KMAHV( is - 1 )
        CALL BRCUT( qVB( is - 1 ), qVB( is ), BeamType, KMAHV( is ) )
     ENDIF

  END DO

  ! Loop over steps

  DO is = 3, Nsteps
     IF ( xv( 1, is ) > r( Nr ) ) RETURN
     rA = xv( 1, is-1 )
     rB = xv( 1, is )
     IF ( ABS( rB - rA ) < TINY( rB ) ) CYCLE   ! don't process duplicate points

     ! Compute upper index on rcvr line
     ir1 = MAX( MIN( INT( ( rA - r(1) ) / deltar ) + 1, Nr ), 1 ) ! should be ", 0 )" ?
     ir2 = MAX( MIN( INT( ( rB - r(1) ) / deltar ) + 1, Nr ), 1 )

     ! Following for projecting back reflected beams
     !  IF ( RV( is-2 ) == RV( is-1 ) ) ir1 = 1
     !  IF ( RV( is )   == RV( is+1 ) ) ir2 = Nr

     DO ir = ir1 + 1, ir2
        W = ( r( ir ) - rA ) / ( rB - rA )

        x     =    xv( :, is-1 ) + W * (    xv( :, is ) -      xv( :, is-1 ) )
        Tray  = TrayV( :, is-1 ) + W * ( TrayV( :, is ) -   TrayV( :, is-1 ) )
        c     =       cV( is-1 ) + W * (       cV( is ) -         cV( is-1 ) )
        q     =     qV(1, is-1 ) + W * (    qV( 1, is ) -       qV( 1,is-1 ) )
        tau   =     tauV( is-1 ) + W * (     tauV( is ) -       tauV( is-1 ) )
        gamma =   gammaV( is-1 ) + W * (   gammaV( is ) -     gammaV( is-1 ) )

        IF ( AIMAG( gamma ) > 0 ) THEN
           WRITE( PRTFIL, * ) 'Unbounded beam'
           WRITE( PRTFIL, * ) gammaV(is-1), gammaV(is), gamma
           CYCLE   ! next receiver
        ENDIF

        IF ( BeamType(1:1) == 'C' ) eps = i * ABS( qV( 1, is ) / qV( 2, is ) )
        const = SQRT( c * ABS( eps ) / q * COS( alpha ) )

        ! Get correct branch of SQRT
        KMAH = KMAHV( is - 1 )
        CALL BRCUT( qVB( is - 1 ), q, BeamType, KMAH )
        IF ( KMAH < 0 ) const = -const

        DO id = 1, Nrd  ! Loop over receivers
           ZR = RD( id )

           ! True beam
           deltaz = ZR - x( 2 )
           !  IF ( omega * AIMAG( gamma ) * deltaz**2 < iBeamWindow2 )
           contri = HERMITE( deltaz, 0.0, RadMax ) * &
                EXP( -i * omega * ( tau + Tray( 2 ) * deltaz + gamma * deltaz**2) )

           IF ( NImage >= 2 ) THEN ! Surface reflected beam
              deltaz = -ZR + 2.0 * DepthT - x( 2 )
              IF ( omega * AIMAG( gamma ) * deltaz**2 < iBeamWindow2 )    &
                   contri =  contri - HERMITE( deltaz, 0.0, RadMax ) *  &
                   EXP( -i * omega * ( tau + Tray( 2 ) * deltaz + gamma * deltaz**2) )
           ENDIF

           IF ( NImage >= 3 ) THEN ! Bottom reflected beam
              deltaz = -ZR + 2.0 * DepthB - x( 2 )
              IF ( omega * AIMAG( gamma ) * deltaz**2 < iBeamWindow2 ) &
                   contri =  contri + HERMITE( deltaz, 0.0, RadMax ) * &
                   EXP( -i * omega * ( tau + Tray( 2 ) * deltaz + gamma * deltaz**2) )
           ENDIF

           ! contribution to field
           SELECT CASE( RunType(1:1) )
           CASE ( 'C' )   ! coherent
              contri = const * contri
           CASE ( 'I' )   ! incoherent
              contri =ABS( const * contri )
           CASE ( 'S' )   ! semi-coherent
              contri = DS * ABS( const * contri )
           END SELECT

           U( id, ir ) = U( id, ir ) + contri

        END DO   ! Next receiver depth
     END DO   ! Next receiver range
  END DO   ! Next step along the ray

  RETURN
END SUBROUTINE INFLUC

! **********************************************************************!

SUBROUTINE INFLUG( U, zs, alpha, RunType, Dalpha )

  ! Computes the beam influence, i.e.
  ! the contribution of a single beam to the complex pressure
  ! This version uses a beam representation in Cartesian coordinates

  USE bellMod
  USE SdRdRMod
  USE ArrMod

  REAL      Amp, delay, xray( 2 ), tray(2), nray( 2 ), xrcvr( 2 ), s, n
  COMPLEX   U(Nrd_per_range, * )
  CHARACTER RunType*5, RunTypeE*1

  ! some ugly code to have RunType 'a' treated like 'A'
  RunTypeE = RunType( 1 : 1 )
  IF ( RunTypeE == 'a' ) RunTypeE = 'A'

  DS       = SQRT( 2.0 ) * SIN( omega * zs * Trayv( 2, 1 ) )   ! Lloyd mirror pattern
  q0       = cV( 1 ) / Dalpha   ! Reference for J = q0 / q
  SrcAngle = RadDeg * alpha     ! take-off angle in degrees
  phase    = 0.0
  qOld     = qv( 1, 1 )
  rA       = xv( 1, 1 )
  ir       = 1
  IF ( RunType( 4:4 ) == 'R' ) THEN  ! point source
     Ratio1 = SQRT( ABS( COS( alpha ) ) )
  ELSE
     Ratio1 = 1
  END IF

  DO is = 2, Nsteps  ! Loop over steps
     rB   = xv( 1, is )
     xray = xv( :, is - 1 )

     ! compute normalized tangent (compute it because we need to measure the step length)
     tray = xV( :, is ) - xV( :, is - 1 )
     rlen = sqrt( tray( 1 )**2 + tray( 2 ) **2 )
     IF ( rlen < TINY( xV( 1, is ) ) ) CYCLE   ! if duplicate point in ray, skip to next step along the ray
     tray = tray / rlen                      ! unit tangent to ray
     nray = (/ -tray( 2 ), tray( 1 ) /)      ! unit normal  to ray

     ! phase shifts at caustics
     q  = qV( 1, is-1 )
     IF ( q <= 0.0 .AND. qOld > 0.0 .OR. q >= 0.0 .AND. qOld < 0.0 ) phase = phase + pi / 2.  ! phase shifts at caustics
     qold = q

     DO WHILE ( ABS( rB - rA ) > TINY( rA ) .AND. rB > r( ir ) )  ! Loop over bracketted receiver ranges
        DO id = 1, Nrd_per_range  ! Loop over receiver depths
           IF ( RunType( 5:5 ) == 'I' ) THEN
              xrcvr = (/ r( ir ), rd( ir ) /)   ! rectilinear grid
           ELSE
              xrcvr = (/ r( ir ), rd( id ) /)   ! irregular   grid
           ENDIF

           s      = dot_product( xrcvr - xray, tray ) / rlen  ! proportional distance along ray
           n      = dot_product( xrcvr - xray, nray )         ! normal distance to ray
           n      = abs( n )
           q      = qV( 1, is-1 ) + s * ( qV( 1, is ) -  qV( 1, is-1 ) )       ! amplitude
           RadMax = ABS( q / q0 )               ! beam radius

           IF ( n < RadMax ) THEN
              A        = 1 / RadMax
              c        =    cV( is-1 ) + s * (   cV( is ) -   cV( is-1 ) )       ! sound speed
              delay    =  tauV( is-1 ) + s * ( tauV( is ) - tauV( is-1 ) )       ! delay
              const    = Ratio1 * SQRT( c / ABS( q ) ) * A * AmpV( is )
              phaseInt = phase
              IF ( q <= 0.0 .AND. qOld > 0.0 .OR. q >= 0.0 .AND. qOld < 0.0 ) phaseInt = phase + pi / 2.  ! phase shifts at caustics
              IF ( RunTypeE == 'S' ) const = DS * const ! semi-coherent TL

              Amp   = const * ( RadMax - n )
              SELECT CASE( RunTypeE )
              CASE ( 'E' )      ! eigenrays
                 CALL WRTRAY( SrcAngle, xv, Trayv, Nsteps, NumTopBnc( is ), NumBotBnc( is ), DepthT, DepthB )
              CASE ( 'A' )      ! arrivals
                 RcvrAngle  = RadDeg * ATAN2( tray(2), tray(1) )
                 CALL AddArr( omega, id, ir, Amp, PhaseV( is - 1 ) + phaseInt, delay, &
                      SrcAngle, RcvrAngle, NumTopBnc( is ), NumBotBnc( is ) )
              CASE ( 'C'  )     ! coherent TL
                 U( id, ir ) = U( id, ir ) + Amp * EXP( -i * ( omega * delay - PhaseV( is - 1 ) - phaseInt ) )
              CASE DEFAULT      ! incoherent/semi-coherent TL
                 W = ( RadMax - n ) / RadMax   ! hat function: 1 on center, 0 on edge
                 U( id, ir ) = U( id, ir ) + ( Amp / W ) ** 2 * W
              END SELECT
           ENDIF
        END DO   ! Next receiver depth

        ir = ir + 1
        IF ( ir > Nr ) RETURN
     END DO   ! Next receiver range

     rA = rB
  END DO   ! Next step along the ray

  RETURN
END SUBROUTINE INFLUG


! **********************************************************************!

SUBROUTINE INFLUGRB( U, zs, alpha, RunType, Dalpha, deltas )

  ! Computes the beam influence, i.e.
  ! the contribution of a single beam to the complex pressure
  ! This version uses the 'GRAB' style of beam

  USE bellMod
  USE SdRdRMod
  USE ArrMod

  INTEGER, PARAMETER :: BeamWindow = 4   ! beam window: kills beams outside e**(-0.5 * ibwin**2 )
  REAL      lambda, xray( 2 ), tray(2), nray( 2 ), xrcvr( 2 ), s, n
  COMPLEX   U(Nrd_per_range, * )
  CHARACTER RunType*5, RunTypeE*1

  ! some ugly code to have RunType 'a' treated like 'A'
  RunTypeE = RunType( 1 : 1 )
  IF ( RunTypeE == 'a' ) RunTypeE = 'A'

  DS       = SQRT( 2.0 ) * SIN( omega * zs * Trayv( 2, 1 ) )   ! Lloyd mirror pattern
  q0       = cV( 1 ) / Dalpha   ! Reference for J = q0 / q
  SrcAngle = RadDeg * alpha     ! take-off angle in degrees
  phase    = 0
  qOld     = qv( 1, 1 )
  rA       = xv( 1, 1 )
  ir       = 1

  IF ( RunType( 4:4) == 'R' ) THEN   ! point source
     Ratio1 = SQRT( ABS( COS( alpha ) ) ) / 1.2535
  ELSE
     Ratio1 = 1 / 1.2535 ! factor representing sum of Gaussians in free space
  END IF

  DO is = 2, Nsteps   ! Loop over steps

     rB   = xv( 1, is )
     xray = xv( :, is - 1 )

     ! compute normalized tangent (compute it because we need to measure the step length)
     tray = xV( :, is ) - xV( :, is - 1 )
     rlen = sqrt( tray( 1 )**2 + tray( 2 ) **2 )
     tray = tray / rlen
     nray = (/ -tray( 2 ), tray( 1 ) /)      ! unit normal  to ray

     ! phase shifts at caustics
     q  = qV( 1, is-1 )
     IF ( q <= 0.0 .AND. qOld > 0.0 .OR. q >= 0.0 .AND. qOld < 0.0 ) phase = phase + pi / 2.  ! phase shifts at caustics
     qold = q

     DO WHILE ( ABS( rB - rA ) > TINY( rA ) .AND. rB > r( ir ) )   ! Loop over bracketted receiver ranges

        DO id = 1, Nrd_per_range  ! Loop over receiver depths
           IF ( RunType( 5:5 ) == 'I' ) THEN
              xrcvr = (/ r( ir ), rd( ir ) /)   ! rectilinear grid
           ELSE
              xrcvr = (/ r( ir ), rd( id ) /)   ! irregular   grid
           ENDIF

           s      = dot_product( xrcvr - xray, tray ) / rlen  ! proportional distance along ray
           n      = dot_product( xrcvr - xray, nray )         ! normal distance to ray
           n      = ABS( n )
           q      = qV( 1, is-1 ) + s * ( qV( 1, is ) -  qV( 1, is-1 ) )       ! amplitude
           sigma  = ABS( q / q0 )                             ! beam radius

           ! calculate the beamwidth
           ! must be at least pi * lambda, except in the nearfield
           lambda = cV( is - 1 ) / ( omega / ( 2 * pi ) )
           sigma  = MAX( sigma, MIN( 0.2 * is * deltas / lambda, pi * lambda ) )

           IF ( n < BeamWindow * sigma ) THEN   ! Within beam window?
              A        = ABS( q0 / q )
              c        =    cV( is-1 ) + s * (   cV( is ) -   cV( is-1 ) )       ! sound speed
              delay    =  tauV( is-1 ) + s * ( tauV( is ) - tauV( is-1 ) )       ! delay
              const    = Ratio1 * SQRT( c / abs( q ) ) * AmpV( is )
              phaseInt = phase
              IF ( q <= 0.0 .AND. qOld > 0.0 .OR. q >= 0.0 .AND. qOld < 0.0 ) phaseInt = phase + pi / 2.  ! phase shifts at caustics
              IF ( RunTypeE == 'S' ) const = DS * const ! semi-coherent TL
              Amp    = const * EXP( -0.5 * ( n / sigma )**2 ) / ( 2. * sigma * A )

              SELECT CASE( RunTypeE )
              CASE ( 'E' )                ! eigenrays
                 CALL WRTRAY( SrcAngle, xv, Trayv, Nsteps, NumTopBnc( is ), NumBotBnc( is ), DepthT, DepthB )
              CASE ( 'A' )                ! arrivals
                 RcvrAngle  = RadDeg * ATAN2( tray(2), tray(1) )
                 CALL AddArr( omega, id, ir, Amp, PhaseV( is ) + phaseInt, delay, &
                      SrcAngle, RcvrAngle, NumTopBnc( is ), NumBotBnc( is ) )
              CASE( 'C' )                 ! coherent TL
                 U( id, ir ) = U( id, ir ) + Amp * EXP( -i * ( omega * delay - PhaseV( is ) - phaseInt ) )
              CASE DEFAULT                ! incoherent/semicoherent TL
                 W =  EXP( -0.5 * ( n / sigma )**2 ) / ( 2. * sigma * A )   ! Gaussian decay
                 U( id, ir ) = U( id, ir ) + ( Amp / W ) ** 2 * W
              END SELECT
           END IF
        END DO   ! Next receiver depth

        ir = ir + 1
        IF ( ir > Nr ) RETURN
     END DO   ! Next receiver range

     rA = rB
  END DO   ! Next step along the ray

  RETURN
END SUBROUTINE INFLUGRB


! **********************************************************************!

SUBROUTINE INFLUSGB( U, zs, alpha, RunType, Dalpha, deltas )

  ! Computes the beam influence, i.e. 
  ! the contribution of a single beam to the complex pressure
  ! This version uses a beam representation in Cartesian coordinates

  USE bellMod
  USE SdRdRMod
  USE ArrMod

  REAL      x( 2 ), Tray( 2 )
  COMPLEX   U( Nrd_per_range, Nr ), contri
  CHARACTER RunType*5

  Ratio1  = SQRT(  COS( alpha ) )
  phase = 0
  qOld  = 1.0
  BETA  = 0.98  ! Beam Factor
  A     = -4.0 * LOG( BETA ) / Dalpha**2
  CN    = Dalpha * SQRT( A / pi )
  rA    = xv( 1, 1 )
  ir    = 1

  DO is = 2, Nsteps    ! Loop over steps

     rB = xv( 1, is )
     ! phase shifts at caustics
     q  = qV( 1, is-1 )
     IF ( q < 0.0 .AND. qOld >= 0.0 .OR. q > 0.0 .AND. qOld <= 0.0 ) phase = phase + pi / 2.  ! phase shifts at caustics
     qold = q

     DO WHILE ( ABS( rB - rA ) > TINY( rA ) .AND. rB > r( ir ) )   ! Loop over bracketted receiver ranges

        W = ( r( ir ) - rA ) / ( rB - rA )
        x     =    xv( :, is-1 ) + W * (    xv( :, is ) -      xv( :, is-1 ) )
        Tray  = TrayV( :, is-1 ) + W * ( TrayV( :, is ) -   TrayV( :, is-1 ) )
        q     =     qV(1, is-1 ) + W * (    qV( 1, is ) -       qV( 1,is-1 ) )
        tau   =     tauV( is-1 ) + W * (     tauV( is ) -       tauV( is-1 ) )

        return
        ! folloing is incorrect because ray doesn't always use a step of deltas
        SINT  =  (is-1) * deltas + W * deltas

        IF ( q < 0.0 .AND. qOld >= 0.0 .OR. q > 0.0 .AND. qOld <= 0.0 ) phase = phase + pi / 2. ! phase shifts at caustics

        DO id = 1, Nrd_per_range   ! Loop over receiver depths
           deltaz =  RD( id ) - x( 2 )   ! ray to rcvr distance
           ! Adeltaz    = ABS( deltaz )
           ! IF ( Adeltaz < RadMax ) THEN
           SELECT CASE( RunType(1:1) )
           CASE ( 'E' )         ! eigenrays
              SrcAngle = RadDeg * alpha   ! take-off angle in degrees
              CALL WRTRAY( SrcAngle, xv, Trayv, Nsteps, NumTopBnc( is ), NumBotBnc( is ), DepthT, DepthB )

           CASE DEFAULT         ! coherent TL
              CPA    = ABS( deltaz * ( rB - rA ) ) / SQRT( ( rB - rA )**2 + ( xv(2,is) - xv(2,is-1))**2  )
              DS     = SQRT( deltaz**2 - CPA**2 )
              SX     = SINT + DS
              thet   = ATAN( CPA / SX )
              delay  = tau + Tray(2) * deltaz
              contri = Ratio1 * CN * AmpV( is ) * EXP(-A * thet ** 2 - i * ( omega * delay - PhaseV( is ) - phase ) ) / SQRT( SX )
              !             write( *, * ) i, id, cpa, ds, sx, theta, delay, a, cn, contri
              U( id, ir ) = U( id, ir ) + contri

           END SELECT
           ! ENDIF
        END DO   ! Next receiver depth

        qOld = q
        ir = ir + 1
        IF ( ir > Nr ) RETURN
     END DO   ! Next receiver range

     rA = rB
  END DO   ! Next step along the ray

  RETURN
END SUBROUTINE INFLUSGB

! **********************************************************************!

SUBROUTINE BRCUT( q1C, q2C, BeamType, KMAH )

  ! Checks for a branch cut crossing

  COMPLEX   q1C, q2C
  CHARACTER BeamType*3

  SELECT CASE ( BeamType(1:1) )
  CASE ( 'W' )   ! WKBeams
     q1 = REAL( q1C )
     q2 = REAL( q2C )
     IF    ( ( q1 < 0.0 .AND. q2 >= 0.0 ) .OR. ( q1 > 0.0 .AND. q2 <= 0.0 ) ) KMAH = -KMAH
  CASE DEFAULT
     IF ( REAL( q2C ) < 0.0 ) THEN
        q1 = AIMAG( q1C )
        q2 = AIMAG( q2C )
        IF ( ( q1 < 0.0 .AND. q2 >= 0.0 ) .OR. ( q1 > 0.0 .AND. q2 <= 0.0 ) ) KMAH = -KMAH
     ENDIF
  END SELECT

  RETURN
END SUBROUTINE BRCUT

! **********************************************************************!

FUNCTION HERMITE( x, x1, x2 )

  ! Calculates a smoothing function based on the h0 hermite cubic

  Ax  = ABS( x  )

  IF ( Ax <= x1 ) THEN
     HERMITE = 1.0
  ELSE IF ( Ax >= x2 ) THEN
     HERMITE = 0.0
  ELSE
     u       = ( Ax - x1 ) / ( x2 - x1 )
     HERMITE = ( 1 + 2 * u ) * ( 1 - u ) ** 2
  ENDIF

  !hermit = hermit / ( 0.5 * ( x1 + x2 ) )

  RETURN
END FUNCTION HERMITE

! **********************************************************************!

SUBROUTINE SCALEP( Dalpha, c, r, U, Nrd, Nr, RunType, TopOpt, freq )

  ! Scale the pressure field

  REAL, PARAMETER :: pi = 3.14159265
  REAL               r( Nr )
  COMPLEX            U( Nrd, Nr )
  CHARACTER          TopOpt*4, RunType*5

  ! Compute scale factor for field
  SELECT CASE ( RunType(2:2) )
  CASE ( 'C' )
     const = -Dalpha * SQRT( freq ) / c
  CASE ( 'R' )
     const = -Dalpha * SQRT( freq ) / c
  CASE DEFAULT
     const = -1.0
  END SELECT

  IF ( TopOpt(4:4) == 'T' ) THEN ! Thorpe attenuation?
     f2    = ( freq / 1000.0 ) ** 2
     alpha = 40.0 * f2 / ( 4100.0 + f2 ) + 0.1 * f2 / ( 1.0 + f2 )
     alpha = alpha / 914.4     ! dB / m
     alpha = alpha / 8.6858896 ! Nepers / m
  ELSE
     alpha = 0.0
  ENDIF

  IF ( RunType(1:1) /= 'C' ) U = SQRT( REAL( U ) ) ! For incoherent run, convert intensity to pressure

  ! add in attenuation
  DO ir = 1, Nr
     IF ( RunType(4:4) == 'X' ) THEN   ! line source
        factor = -4.0 * SQRT( pi ) * const
     ELSE                              ! point source
        IF ( r ( ir ) == 0 ) THEN
           factor = 1e5       ! avoid /0 at origin
        ELSE
           factor = const * EXP( -alpha * r( ir ) ) / SQRT( r( ir ) )
        END IF
     END IF
     U( :, ir ) = factor * U( :, ir )
  END DO

  RETURN
END SUBROUTINE SCALEP

! **********************************************************************!

SUBROUTINE WRTRAY( alpha0, xv, Trayv, Nsteps, NumTopBnc, NumBotBnc, DepthT, DepthB )

  ! Compress the ray data keeping every ISKIP point, points near surface or bottom, and last point.
  ! Write to RAYFIL.

  INTEGER, PARAMETER :: RAYFIL = 21

  INTEGER ( KIND = 2 ) :: NumTopBnc, NumBotBnc
  REAL (KIND=8) :: xv( 2, * ), Trayv( 2, * )

  ! compression

  N2 = 1
  ISKIP = MAX( Nsteps / 5000, 1 )   ! max #pts written is about 5000

  DO is = 2, Nsteps
     ! following ensures that we always write ray pts. near bdry reflections
     IF ( MIN( DepthB - xv( 2, is ),  xv( 2, is ) - DepthT ) < 0.2 .OR. &
          MOD( is, ISKIP ) == 0 .OR. is == Nsteps ) THEN
        N2 = N2 + 1
        xv( :, N2 ) = xv( :, is )
     END IF
  END DO

  ! write to ray file

  WRITE( RAYFIL, * ) alpha0
  WRITE( RAYFIL, * ) N2, NumTopBnc, NumBotBnc
  DO is = 1, N2
     WRITE( RAYFIL, * ) SNGL( xv( :, is ) ) ! , SNGL( Trayv( 2, is ) )
  END DO

  RETURN
END SUBROUTINE WRTRAY

