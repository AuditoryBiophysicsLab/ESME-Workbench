SUBROUTINE READIN( Title, freq, iSingle, &
     Nimage, Ibwin, deltas, MaxN, zBox, rBox, epmult, rLoop, &
     TopOpt, DepthT, cpT, rhoT, BotOpt, DepthB, cpB, rhoB, RunType, BeamType, Component )

  ! Routine to read in and echo all the input data

  ! Note that default values of SSP, DENSITY, Attenuation will not work

  USE anglemod
  USE sspmod
  USE SdRdRMod

  IMPLICIT REAL (KIND=4) ( A-H, O-Z )
  INTEGER, PARAMETER ::  ENVFil = 5, PRTFil = 6, RAYFil = 21, ARRFil = 36
  REAL,    PARAMETER ::  C0 = 1500.0

  INTEGER AllocateStatus
  REAL                  lambdax10, lambdax66, x( 2 ), gradC( 2 )
  REAL    (KIND=8) ::   freqD, rhoT, rhoB, BumDen, eta, xi
  COMPLEX (KIND=8) ::   cpT, cpB, csT, csB
  CHARACTER             Title*80, TopOpt*5, BotOpt*3, RunType*5, BeamType*3, SSPType*1, BCType*1, AtUnit*1, &
                        FileName*6, PlotType*10, Component*1

  DOUBLE PRECISION :: alphaR, betaR, alphaI, betaI, rhoR
  COMMON /CPREV/ alphaR, betaR, rhoR, alphaI, betaI

  alphaR  = 1500.0;  betaR  = 0.0;  rhoR   = 1.0
  alphaI  = 0.0;     betaI  = 0.0
  iSingle = 0

  Title(1:9) = 'BELLHOP- '
  READ(  ENVFil, * ) Title(10:80)
  WRITE( PRTFil, * ) Title

  READ(  ENVFil, *    ) freq
  WRITE( PRTFil, '('' frequency = '', G11.4, / )' ) freq

  freqD = freq

  READ(  ENVFil, * ) NMedia
  WRITE( PRTFil, * ) 'Dummy parameter NMedia = ', NMedia
  IF ( NMedia /= 1 ) CALL ERROUT( PRTFil, 'F', 'READIN', &
       'Only one medium or layer is allowed in BELLHOP; sediment layers must be handled using a reflection coefficient' )

  ! ****** Read in SSP data ******

  READ(  ENVFil, * ) TopOpt
  WRITE( PRTFil, * )

  SSPType(1:1) = TopOpt(1:1)
  BCType(1:1)  = TopOpt(2:2)
  AtUnit(1:1)  = TopOpt(3:3)

  ! SSP approximation options

  SELECT CASE ( SSPType )
  CASE ( 'N' )
     WRITE( PRTFil, * ) '    N2-LINEAR approximation to SSP'
  CASE ( 'C' )
     WRITE( PRTFil, * ) '    C-LINEAR approximation to SSP'
  CASE ( 'S' )
     WRITE( PRTFil, * ) '    SPLINE approximation to SSP'
  CASE ( 'A' )
     WRITE( PRTFil, * ) '    ANALYTIC SSP option'
  CASE DEFAULT
     CALL ERROUT( PRTFil, 'F', 'READIN', 'Unknown option for SSP approximation' )
  END SELECT

  ! Attenuation options

  SELECT CASE ( AtUnit )
  CASE ( 'N' )
     WRITE( PRTFil, * ) '    Attenuation units: nepers/m'
  CASE ( 'F' )
     WRITE( PRTFil, * ) '    Attenuation units: dB/mkHz'
  CASE ( 'M' )
     WRITE( PRTFil, * ) '    Attenuation units: dB/m'
  CASE ( 'W' )
     WRITE( PRTFil, * ) '    Attenuation units: dB/wavelength'
  CASE ( 'Q' )
     WRITE( PRTFil, * ) '    Attenuation units: Q'
  CASE ( 'L' )
     WRITE( PRTFil, * ) '    Attenuation units: Loss parameter'
  CASE DEFAULT
     CALL ERROUT( PRTFil, 'F', 'READIN', 'Unknown attenuation units' )
  END SELECT

  ! optional addition of volume attenuation using standard formulas

  SELECT CASE ( TopOpt(4:4) )
  CASE ( 'T' )
     WRITE( PRTFil, * ) '    THORP attenuation added'
  END SELECT

  SELECT CASE ( TopOpt(5:5) )
  CASE ( '*' )
     WRITE( PRTFil, * ) '    Development options enabled'
  END SELECT

  ! Top BC

  IF ( BCType(1:1) == 'A' ) WRITE( PRTFil, FMT = "(//, 'Z    alphaR     betaR      rho     alphaI     betaI', /)" )

  CALL TOPBOT( ENVFil, PRTFil, freqD, BCType, AtUnit, cpT, csT, rhoT, BumDen, eta, xi )

  ! Ocean SSP

  READ(  ENVFil, * ) NPts, Sigma, DepthB
  WRITE( PRTFil, * )
  WRITE( PRTFil, * ) 'Depth = ', DepthB

  IF ( TopOpt(1:1) == 'A' ) THEN
     WRITE( PRTFil, * ) 'Analytic SSP option'
  ELSE
     x = (/ 0.0, DepthB /)   ! tells SSP Depth to read to
     CALL SSP( x, C, gradC, CRR, CRZ, CZZ, SSPType, 'INI' )
  ENDIF

  DepthT = MINVAL( zSSPV )   ! Depth of top boundary is taken from first SSP point

  ! Bottom BC

  READ( ENVFil, * ) BotOpt, Sigma

  WRITE( PRTFil, * )
  WRITE( PRTFil, FMT = "(33X, '( RMS roughness = ', G10.3, ' )' )" ) Sigma

  CALL TOPBOT( ENVFil, PRTFil, freqD, BotOpt, AtUnit, cpB, csB, rhoB, BumDen, eta, xi )
  CALL SDRD(   ENVFil, PRTFil, -HUGE( DepthT ), HUGE( DepthB ) )   ! *** Read source/receiver Depths ***
  CALL RANGES( ENVFil, PRTFil )                   ! *** Read receiver ranges ***

  ! Run type

  READ(  ENVFil, * ) RunType(1:5)
  WRITE( PRTFil, * )

  SELECT CASE ( RunType(1:1) )
  CASE ( 'R' )
     WRITE( PRTFil, * ) 'Ray trace run'
  CASE ( 'E' )
     WRITE( PRTFil, * ) 'Eigenray trace run'
  CASE ( 'I' )
     WRITE( PRTFil, * ) 'Incoherent TL calculation'
  CASE ( 'S' )
     WRITE( PRTFil, * ) 'Semi-coherent TL calculation'
  CASE ( 'C' )
     WRITE( PRTFil, * ) 'Coherent TL calculation'
  CASE ( 'A' )
     WRITE( PRTFil, * ) 'Arrivals calculation, ASCII  file output'
  CASE ( 'a' )
     WRITE( PRTFil, * ) 'Arrivals calculation, binary file output'
  CASE DEFAULT
     CALL ERROUT( PRTFil, 'F', 'READIN', 'Unknown RunType selected' )
  END SELECT

  SELECT CASE ( RunType(2:2) )
  CASE ( 'C' )
     WRITE( PRTFil, * ) 'Cartesian beams'
  CASE ( 'R' )
     WRITE( PRTFil, * ) 'Ray centered beams'
  CASE ( 'S' )
     WRITE( PRTFil, * ) 'Simple gaussian beams'
  CASE ( 'B' )
     WRITE( PRTFil, * ) 'gaussian beam Bundles (grab)'
  CASE DEFAULT
     RunType(2:2) = 'G'
     WRITE( PRTFil, * ) 'Geometric beams'
  END SELECT

  SELECT CASE ( RunType(4:4) )
  CASE ( 'R' )
     WRITE( PRTFil, * ) 'Point source (cylindrical coordinates)'
  CASE ( 'X' )
     WRITE( PRTFil, * ) 'Line source (Cartesian coordinates)'
  CASE DEFAULT
     RunType(4:4) = 'R'
     WRITE( PRTFil, * ) 'Point source (cylindrical coordinates)'
  END SELECT

  SELECT CASE ( RunType(5:5 ) )
  CASE ( 'R' )
     WRITE( PRTFil, * ) 'Rectilinear receiver grid: Receivers at ( rr( ir ), rd( ir ) ) )'
  CASE ( 'I' )
     WRITE( PRTFil, * ) 'Irregular grid: Receivers at rr( : ) x rd( : )'
  CASE DEFAULT
     WRITE( PRTFil, * ) 'Rectilinear receiver grid: Receivers at rr( : ) x rd( : )'
     RunType(5:5) = 'R'
  END SELECT

  ! Beam angles

  NBeams     = 0

  IF ( TopOpt( 5:5 ) == 'I' ) THEN
     READ( ENVFil, * ) NBeams, iSingle     ! hidden option to trace a single beam
  ELSE
     READ( ENVFil, * ) NBeams
  END IF

  IF ( NBEAMS == 0 ) THEN
     NBeams = MAX( INT( 0.3 * R( NR ) * Freq / c0 ), 300 )   ! automatically estimate NBeams to use
  END IF

  ALLOCATE( alpha( NBeams ), STAT = AllocateStatus )
  IF ( AllocateStatus /= 0 ) THEN
     CALL ERROUT( PRTFil, 'F', 'READIN', 'Insufficient memory to store beam angles'  )
  ENDIF

  IF ( NBeams > 2 ) alpha( 3 ) = -999.9
  READ( ENVFil, * ) alpha
  CALL SUBTAB( alpha, NBeams )
  CALL SORT(   alpha, NBeams )

  WRITE( PRTFil, * )
  WRITE( PRTFil, * ) 'Number of beams   = ', NBeams
  IF ( iSingle > 0 ) WRITE( PRTFil, * ) 'Trace only beam number ', iSingle

  IF ( NBeams >= 1 ) WRITE( PRTFil, "( 5G14.6 )" ) ( alpha( IBEAM ), IBEAM = 1, MIN( NBeams, 51 ) )

  ! Limits for tracing beams
  READ(  ENVFil, * ) deltas, zBox, rBox

  ! Automatic step size selection < 10 wavelengths

  IF ( deltas == 0.0 ) THEN

     lambdax10 = 10 * C0 / freq   ! 10 wavelengths
     lambdax66 = 66 * C0 / freq   ! 66 wavelengths
     Dby20  = ( DepthB - DepthT ) / 20.0

     SELECT CASE ( RunType(1:1) )
     CASE ( 'R' )
        deltas = MIN( lambdax66, Dby20 )
     CASE ( 'E' )
        deltas = MIN( lambdax66, Dby20 )
     CASE ( 'I' )
        deltas = MIN( lambdax66, Dby20 )
     CASE ( 'S' )
        deltas = MIN( lambdax66, Dby20 )
     CASE ( 'C' )
        deltas = MIN( lambdax10, Dby20 )
     CASE ( 'A' )
        deltas = MIN( lambdax10, Dby20 )
     CASE ( 'a' )
        deltas = MIN( lambdax10, Dby20 )
     END SELECT

     deltas = MAX( deltas, rBox / MaxN )		! at most do MaxN steps
     deltas = MIN( deltas, DepthB - DepthT )	! deltas < channel height
  ENDIF

  WRITE( PRTFil, * )
  WRITE( PRTFil, * ) 'Step length,     deltas = ', deltas
  WRITE( PRTFil, * )
  WRITE( PRTFil, * ) 'Maximum ray Depth, zBox = ', zBox
  WRITE( PRTFil, * ) 'Maximum ray range, rBox = ', rBox

  rBox = 1000.0 * rBox

  ! *** Beam characteristics ***

  ! note: curvature change can cause overflow in grazing case
  ! suppress by setting BeamType(2:2) = 'Z'

  IF ( SCAN( RunType(2:2), 'GBS' ) /= 0 ) THEN
     BeamType(1:2) = 'MS'; rLoop = 1.0; epmult = 1.0
     BeamType(3:3) = RunType(3:3)
     SELECT CASE ( BeamType(3:3) )
     CASE ( 'S' )
        WRITE( PRTFil, * ) 'Beam shift in effect'
     CASE DEFAULT
        WRITE( PRTFil, * ) 'No beam shift in effect'
     END SELECT
  ELSE
     READ(  ENVFil, * ) BeamType(1:2), epmult, rLoop
     WRITE( PRTFil, * )
     WRITE( PRTFil, * )
     WRITE( PRTFil, * ) 'Type of beam = ', BeamType(1:1)

     SELECT CASE ( BeamType(2:2) )
     CASE ( 'D' )
        WRITE( PRTFil, * ) 'Curvature doubling invoked'
     CASE ( 'Z' )
        WRITE( PRTFil, * ) 'Curvature zeroing invoked'
     CASE ( 'S' )
        WRITE( PRTFil, * ) 'Standard curvature condition'
     CASE DEFAULT
        CALL ERROUT( PRTFil, 'F', 'READIN', 'Unknown curvature condition' )
     END SELECT

     WRITE( PRTFil, * ) 'Epsilon multiplier', epmult
     WRITE( PRTFil, * ) 'Range for choosing beam width', rLoop

     ! Images, windows
     READ(  ENVFil, * ) Nimage, Ibwin, Component
     WRITE( PRTFil, * )
     WRITE( PRTFil, * ) 'Number of images, Nimage  = ', Nimage
     WRITE( PRTFil, * ) 'Beam windowing parameter  = ', Ibwin
     WRITE( PRTFil, * ) 'Component                 = ', Component

  ENDIF

  WRITE( PRTFil, * )
  CLOSE( ENVFil )

  ! Write appropriate header information

  IF ( RunType(1:1) == 'R' .OR. RunType(1:1) == 'E' ) THEN
     OPEN ( FILE = 'RAYFIL', UNIT = RAYFil, FORM = 'FORMATTED' )
     WRITE( RAYFil, * ) '''', Title(1:50), ''''
     WRITE( RAYFil, * ) freq
     WRITE( RAYFil, * ) NBeams
     WRITE( RAYFil, * ) DepthT
     WRITE( RAYFil, * ) DepthB
  ELSE IF ( RunType(1:1) == 'A' ) THEN
     OPEN ( FILE = 'ARRFIL', UNIT = ARRFil, FORM = 'FORMATTED' )
     WRITE( ARRFil, * ) freq, NSD, NRD, NR
     WRITE( ARRFil, * ) SD( 1 : NSD )
     WRITE( ARRFil, * ) RD( 1 : NRD )
     WRITE( ARRFil, * ) R(  1 : NR  )
  ELSE IF ( RunType(1:1) == 'a' ) THEN   ! arrival file in binary format
     OPEN ( FILE = 'ARRFIL', UNIT = ARRFil, FORM = 'UNFORMATTED' )
     WRITE( ARRFil ) freq, NSD, NRD, NR
     WRITE( ARRFil ) SD( 1 : NSD )
     WRITE( ARRFil ) RD( 1 : NRD )
     WRITE( ARRFil ) R(  1 : NR  )
  ELSE
     FileName = 'SHDFIL'
     SELECT CASE ( RunType( 5: 5 ) )
     CASE ( 'R' )
        PlotType = 'rectilin  '
     CASE ( 'I' )
        PlotType = 'irregular '
        IF ( Nrd .NE. Nr ) CALL ERROUT( PRTFil, 'F', 'READIN', 'Irregular grid option selected with Nrd not equal to Nr' )
     END SELECT

     ALLOCATE( theta( 1 ) )
     theta( 1 ) = 0   ! dummy bearing angle
     ntheta     = 1
     atten      = 0.0
     CALL WriteHeader( FileName, Title, theta, Ntheta, SD, NSD, RD, NRD, R, NR, freq, atten, PlotType, 0.0, 0.0 )
  END IF

  RETURN
END SUBROUTINE READIN

!**********************************************************************!

SUBROUTINE SSP( x, C, gradC, CRR, CRZ, CZZ, OPT, Task )

  ! Call the particular profil routine based specified by Task

  ! SSP is expected to perform two tasks:
  !   Task = 'TAB'  then tabulate cp, cs, rhoT 
  !   Task = 'INI' then initialize
  !   X( 2 ) is the (r,z) coordinate

  INTEGER, PARAMETER :: PRTFil = 6
  REAL                  x( 2 ), gradC( 2 )
  CHARACTER             OPT*1, Task*3

  SELECT CASE ( OPT )
  CASE ( 'N' )
     CALL N2LIN(  x, C, gradC, CRR, CRZ, CZZ, Task )
  CASE ( 'C' )
     CALL CLIN(   x, C, gradC, CRR, CRZ, CZZ, Task )
  CASE ( 'S' )
     CALL CCUBIC( x, C, gradC, CRR, CRZ, CZZ, Task )
  CASE ( 'A' )
     CALL ANALYT( x, C, gradC, CRR, CRZ, CZZ, Task )
  CASE DEFAULT
     WRITE( PRTFil, * ) 'Non-existent profile option: ', OPT
     STOP
  END SELECT

  RETURN
END SUBROUTINE SSP

!**********************************************************************!

SUBROUTINE N2LIN( x, C, gradC, CRR, CRZ, CZZ, Task )

  ! N2-linear interpolation of SSP data

  USE sspmod
  INTEGER, PARAMETER :: ENVFil = 5, PRTFil = 6
  REAL,    PARAMETER :: PREC = 1.0E-6
  CHARACTER Task*3
  SAVE ztmp

  REAL x( 2 ), gradC( 2 )
  REAL (KIND=8) ::   alphaR, betaR, rhoR, alphaI, betaI
  COMMON /CPREV/ alphaR, betaR, rhoR, alphaI, betaI

  IF ( Task == 'INI' ) THEN

     ! *** Section to read in SSP data ***

     WRITE( PRTFil, * )
     WRITE( PRTFil, * ) 'N2-Linear SSP option'
     WRITE( PRTFil, * )
     WRITE( PRTFil, * ) 'Sound speed profile:'

     Depth = x( 2 )
     NSSP  = 1

     DO IZ = 1, MaxSSP

        READ(  ENVFil, * ) ztmp, alphaR
        zSSPV( IZ ) = ztmp
        cSSPV( IZ ) = alphaR
        WRITE( PRTFil, * ) zSSPV( IZ ), cSSPV( IZ )
        N2V( IZ ) = 1.0 / cSSPV( IZ ) ** 2

        IF ( IZ > 1 ) N2ZV( IZ - 1 ) = ( N2V(   IZ ) - N2V(   IZ - 1 ) ) / &
             ( zSSPV( IZ ) - zSSPV( IZ - 1 ) )

        ! Have we read the last point?
        IF ( ABS( zSSPV( IZ ) - Depth ) < PREC ) THEN
           Layer = 1
           RETURN
        ENDIF

        NSSP = NSSP + 1

     END DO

     ! Fall through means too many points in the profile

     WRITE( PRTFil, * ) 'Max. #SSP points: ', MaxSSP
     CALL ERROUT( PRTFil, 'F', 'N2LIN', 'Number of SSP points exceeds limit' )

  ELSE

     !  *** Section to return SSP info ***

     IF ( x(2) < zSSPV( Layer ) .OR. x(2) > zSSPV( Layer + 1 ) ) THEN
        DO IZ = 2, NSSP   ! Search for bracketting Depths
           IF ( x( 2 ) < zSSPV( IZ ) ) THEN
              Layer = IZ - 1
              EXIT
           ENDIF
        END DO
     ENDIF

     W = ( x( 2 ) - zSSPV( Layer ) ) / ( zSSPV( Layer + 1 ) - zSSPV( Layer ) )

     C   = 1.0 / SQRT( ( 1.0 - W ) * N2V( Layer ) + W * N2V( Layer + 1 ) )
     gradC  = (/ 0.0, -0.5 * C * C * C * N2ZV( Layer ) /)
     CRR = 0.0; CRZ = 0.0; CRR = 0.0
     CZZ = 3.0 * gradc( 2 ) * gradc( 2 ) / C
  ENDIF

  RETURN
END SUBROUTINE N2LIN

!**********************************************************************!

SUBROUTINE CLIN( x, C, gradC, CRR, CRZ, CZZ, Task )

  ! C-linear interpolation of SSP data

  USE sspmod

  INTEGER, PARAMETER :: ENVFil = 5, PRTFil = 6
  REAL,    PARAMETER :: PREC = 1.0E-6
  CHARACTER Task*3
  SAVE ztmp

  REAL X( 2 ), gradC( 2 )
  REAL (KIND=8) ::   alphaR, betaR, rhoR, alphaI, betaI
  COMMON /CPREV/ alphaR, betaR, rhoR, alphaI, betaI

  IF ( Task == 'INI' ) THEN

     !  *** Section to read in SSP data ***

     WRITE( PRTFil, * )
     WRITE( PRTFil, * ) 'C-Linear SSP option'
     WRITE( PRTFil, * )
     WRITE( PRTFil, * ) 'Sound speed profile:'

     Depth = x( 2 )
     NSSP = 1

     DO IZ = 1, MaxSSP

        READ(  ENVFil, * ) ztmp, alphaR
        zSSPV( IZ ) = ztmp
        cSSPV( IZ ) = alphaR
        WRITE( PRTFil, * ) zSSPV( IZ ), cSSPV( IZ )

        IF ( IZ > 1 ) CZV( IZ - 1 )  = ( cSSPV( IZ ) - cSSPV( IZ - 1 ) ) / &
             ( zSSPV( IZ ) - zSSPV( IZ - 1 ) )

        ! Have we read the last point?
        IF ( ABS( zSSPV( IZ ) - Depth ) < PREC ) THEN
           Layer = 1
           RETURN
        ENDIF

        NSSP = NSSP + 1

     END DO

     ! Fall through means too many points in the profile

     WRITE( PRTFil, * ) 'Max. #SSP points: ', MaxSSP
     CALL ERROUT( PRTFil, 'F', 'N2LIN', 'Number of SSP points exceeds limit' )

  ELSE

     ! *** Section to return SSP info ***

     IF ( x( 2 ) < zSSPV( Layer ) .OR. x( 2 ) > zSSPV( Layer + 1 ) ) THEN
        DO IZ = 2, NSSP   ! Search for bracketting Depths
           IF ( x( 2 ) < zSSPV( IZ ) ) THEN
              Layer = IZ - 1
              EXIT
           ENDIF
        END DO
     ENDIF

     C   = cSSPV( Layer ) + ( x(2) - zSSPV( Layer ) ) * CZV( Layer )
     gradC  = (/ 0.0, CZV( Layer ) /)
     CRR = 0.0; CRZ = 0.0; CRR = 0.0; CZZ = 0.0

  ENDIF

  RETURN
END SUBROUTINE CLIN

!**********************************************************************!

SUBROUTINE CCUBIC( x, C, gradC, CRR, CRZ, CZZ, Task )

  ! Cubic spline interpolation

  USE sspmod
  INTEGER, PARAMETER :: ENVFil = 5, PRTFil = 6
  REAL,    PARAMETER :: PREC = 1.E-6
  CHARACTER Task*3
  SAVE ztmp
  REAL X( 2 ), gradC( 2 )
  REAL (KIND=8) ::   alphaR, betaR, rhoR, alphaI, betaI
  COMMON /CPREV/ alphaR, betaR, rhoR, alphaI, betaI

  IF ( Task == 'INI' ) THEN

     ! *** Task 'INIT' for initialization ***

     WRITE( PRTFil, * )
     WRITE( PRTFil, * ) 'Spline SSP option'
     WRITE( PRTFil, * )
     WRITE( PRTFil, * ) 'Sound speed profile:'

     Depth = x( 2 )
     NSSP  = 1

     DO IZ = 1, MaxSSP

        READ(  ENVFil, * ) ztmp, alphaR
        zSSPV( IZ ) = ztmp
        cSSPV( IZ ) = alphaR
        WRITE( PRTFil, * ) zSSPV( IZ ), cSSPV( IZ )

        CVS( 1, IZ ) = cSSPV( IZ )

        IF ( ABS( zSSPV( IZ ) - Depth ) < PREC ) THEN
           Layer = 1

           ! Compute spline coefs
           IBCBEG = 0;      IBCEND = 0
           CALL CSPLINE( zSSPV, CVS(1, 1), NSSP, IBCBEG, IBCEND, NSSP )
           RETURN
        ENDIF
        NSSP = NSSP + 1
     END DO

     ! Fall through means too many points in the profile
     WRITE( PRTFil, * ) 'Max. #SSP points: ', MaxSSP
     CALL ERROUT( PRTFil, 'F', 'N2LIN', 'Number of SSP points exceeds limit' )
  ELSE

     ! *** Section to return SSP info ***

     IF ( x( 2 ) < zSSPV( Layer ) .OR. x( 2 ) > zSSPV( Layer+1 ) ) THEN
        DO IZ = 2, NSSP   ! Search for bracketting Depths
           IF ( x( 2 ) < zSSPV( IZ ) ) THEN
              Layer = IZ - 1
              EXIT
           ENDIF
        END DO

     ENDIF

     HSPLNE = x( 2 ) - zSSPV( Layer )

     ! C   = SPLINE(   CVS( 1, Layer ), HSPLNE )
     ! CZ  = SPLINEX(  CVS( 1, Layer ), HSPLNE )
     ! CZZ = SPLINEXX( CVS( 1, Layer ), HSPLNE )

     CALL SPLINEALL( CVS( 1, Layer ), HSPLNE, C, CZ, CZZ )

     gradC  = (/ 0.0, CZ /)
     CRR = 0.0;    CRZ = 0.0;    CRR = 0.0
  ENDIF

  RETURN
END SUBROUTINE CCUBIC

!**********************************************************************!

SUBROUTINE TOPBOT( ENVFil, PRTFil, freq, BCType, AtUnit, cpHS, csHS, rhoHS, BumDen, eta, xi )

  ! Handles top and bottom boundary conditions

  ! Input:
  !   ENVFil: Environmental file
  !   PRTFil: Print file
  !   freq:   frequency
  !   BCType: Boundary condition type

  ! Output:
  !  cpHS:    P-wave speed in halfspace
  !  csHS:    S-wave speed in halfspace
  !  rhoHS:   density in halfspace

  !  BumDenN:  Bump density
  !  eta:     Principal radius 1
  !  xi:      Principal radius 2

  IMPLICIT NONE

  INTEGER             ENVFil, PRTFil
  REAL (KIND=8)    :: alphaR, betaR, alphaI, betaI, rhoR, freq, rhoHS, BumDen, eta, xi, ztmp
  COMPLEX (KIND=8) :: cpHS, csHS, CRCI
  CHARACTER           BCType*1, AtUnit*1
  COMMON /CPREV/ alphaR, betaR, rhoR, alphaI, betaI

  ! Echo to PRTFil user's choice of boundary condition

  SELECT CASE ( BCType )
  CASE ( 'S' )
     WRITE( PRTFil, * ) '    Twersky SOFT BOSS scatter model'
  CASE ( 'H' )
     WRITE( PRTFil, * ) '    Twersky HARD BOSS scatter model'
  CASE ( 'T' )
     WRITE( PRTFil, * ) '    Twersky (amplitude only) SOFT BOSS scatter model'
  CASE ( 'I' )
     WRITE( PRTFil, * ) '    Twersky (amplitude only) HARD BOSS scatter model'
  CASE ( 'V' )
     WRITE( PRTFil, * ) '    VACUUM'
  CASE ( 'R' )
     WRITE( PRTFil, * ) '    Perfectly RIGID'
  CASE ( 'A' )
     WRITE( PRTFil, * ) '    ACOUSTO-ELASTIC half-space'
  CASE ( 'F' )
     WRITE( PRTFil, * ) '    FILE used for reflection loss'
  CASE ( 'W' )
     WRITE( PRTFil, * ) '    Writing an IRC file'
  CASE ( 'P' )
     WRITE( PRTFil, * ) '    reading PRECALCULATED IRC'
  CASE DEFAULT
     CALL ERROUT( PRTFil, 'F', 'TOPBOT', 'Unknown boundary condition type' )
  END SELECT

  ! ****** Read in BC parameters depending on particular choice ******

  cpHS  = 0.0
  csHS  = 0.0
  rhoHS = 0.0

  ! *** Twersky ice model parameters ***

  IF ( BCType == 'S' .OR. BCType == 'H' .OR. &
       BCType == 'T' .OR. BCType == 'I' ) THEN

     READ(  ENVFil, *    ) BumDen, eta, xi
     WRITE( PRTFil, FMT = "( /, ' Twersky ice model parameters:' )" )
     WRITE( PRTFil, FMT = "(' Bumden = ', G15.6, '  Eta = ', G11.3, '  Xi = ', G11.3, /)" ) BumDen, eta, xi
  ENDIF

  ! *** Half-space properties ***

  IF ( BCType == 'A' ) THEN
     READ(  ENVFil, *    ) ztmp, alphaR, betaR, rhoR, alphaI, betaI
     WRITE( PRTFil, FMT = "( F10.2, 3X, 2F10.2, 3X, F6.2, 3X, 2F10.4 )" ) &
          ztmp, alphaR, betaR, rhoR, alphaI, betaI

     cpHS = CRCI( alphaR, alphaI, freq, AtUnit )
     csHS = CRCI( betaR,  betaI,  freq, AtUnit )
     rhoHS = rhoR
  ENDIF

  RETURN
END SUBROUTINE TOPBOT

!**********************************************************************!

FUNCTION CRCI( c, alpha, freq, AtUnit )

  ! Converts real wave speed and attenuation to a single complex wave speed
  ! 6 CASES:
  !     N for Nepers/meter
  !     M for dB/meter      (M for Meters)
  !     F for dB/m-kHZ      (F for frequency dependent)
  !     W for dB/wavelength (W for Wavelength)
  !     Q for Q
  !     L for Loss parameter
  !     T for Thorpe

  IMPLICIT REAL (KIND=8) (A-H, O-Z)
  COMPLEX (KIND=8) :: CRCI
  CHARACTER AtUnit*1

  omega = 2.0 * 3.1415926535897932 * freq

  ! Convert to Nepers/m

  alphaT = 0.0

  SELECT CASE ( AtUnit )
  CASE ( 'N' )   ! Nepers/m
     alphaT = alpha
  CASE ( 'M' )   ! dB/meter
     alphaT = alpha / 8.6858896
  CASE ( 'F' )   ! dB/m-kHZ
     alphaT = alpha * freq / 8685.8896
  CASE ( 'W' )   ! dB/wavelength
     IF ( c /= 0.0 ) alphaT = alpha * freq / ( 8.6858896 * c )
  CASE ( 'Q' )
     IF( c * alpha /= 0.0 ) alphaT = omega / ( 2.0 * c * alpha )
  CASE ( 'L' )   ! loss parameter
     IF ( C /= 0.0 ) ALPHAT = ALPHA * OMEGA / C
  CASE ( 'T' )   ! Thorpe
     f2     = ( freq / 1000.0 ) **2
     alphaT = 40.0 * f2 / ( 4100.0 + f2 ) + 0.1*f2 / ( 1.0 + f2 )
     alphaT = alphaT / 914.4     ! dB / m
     alphaT = alphaT / 8.6858896 ! Nepers / m
  END SELECT

  ! Convert Nepers/m to equivalent imaginary sound speed

  alphaT = alphaT * c**2 / omega
  CRCI   = CMPLX( c, alphaT, KIND=4 )

  RETURN
END FUNCTION CRCI
