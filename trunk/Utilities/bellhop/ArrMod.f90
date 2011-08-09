MODULE ArrMod

  ! Variables for arrival information

  INTEGER, PARAMETER :: ARRFIL = 36

  INTEGER   MaxNArr
  INTEGER ( KIND = 2 ),  ALLOCATABLE :: NArr( :, : ), NTopBncArr( :, :, : ), NBotBncArr( :, :, : )
  REAL,     ALLOCATABLE :: DelArr( :, :, : ), SrcAngArr( :, :, : ), RcvrAngArr( :, :, : ), &
       AArr( :, :, : ), PhaseArr( :, :, : )
CONTAINS

  SUBROUTINE AddArr( omega, id, ir, Amp, Phase, delay, SrcAngle, RcvrAngle, NumTopBnc, NumBotBnc )

    ! ADDs the amplitude and delay for an ARRival into a matrix of same
    ! extra logic to keep only the strongest arrivals

    REAL, PARAMETER :: PhaseTol = 0.5  ! arrivals with essentially the same phase are grouped into one
    LOGICAL   NewRay
    INTEGER   IArr( 1 )
    INTEGER ( KIND = 2 ) :: NumTopBnc, NumBotBnc

    Nt     = NArr( id, ir )    ! # of arrivals
    NewRay = .TRUE.

    ! Is this the second bracketting ray of a pair?
    ! If so, we want to combine the arrivals to conserve space.
    ! (test this by seeing if the arrival time is close to the previous one)
    ! (also need that the phase is about the same to make sure surface and direct paths are not joined)

    IF ( Nt >= 1 ) THEN
       IF( omega * ABS( delay - DelArr( id, ir, Nt ) ) < PhaseTol .AND. &
           ABS( PhaseArr( id, ir, Nt ) - Phase )       < PhaseTol ) NewRay = .FALSE.
    END IF

    IF ( NewRay ) THEN
       IF ( Nt >= MaxNArr ) THEN       ! space available to add an arrival?
          IARR = MINLOC( AArr( id, ir, : ) )   ! no: replace weakest arrival
          IF ( Amp > AArr( id, ir, IArr( 1 ) ) ) THEN
             AArr(       id, ir, IArr( 1 ) ) = Amp		! amplitude
             PhaseArr(   id, ir, IArr( 1 ) ) = Phase  	! phase
             DelArr(     id, ir, IArr( 1 ) ) = delay		! delay time
             SrcAngArr(  id, ir, IArr( 1 ) ) = SrcAngle	! angle
             RcvrAngArr( id, ir, IArr( 1 ) ) = RcvrAngle	! angle
             NTopBncArr( id, ir, IArr( 1 ) ) = NumTopBnc	! Number of top     bounces
             NBotBncArr( id, ir, IArr( 1 ) ) = NumBotBnc 	!   "       bottom
          ENDIF
       ELSE
          NArr(       id, ir         ) = Nt + 1       	! # of arrivals
          AArr(       id, ir, Nt + 1 ) = Amp			! amplitude
          PhaseArr(   id, ir, Nt + 1 ) = Phase                ! phase
          DelArr(     id, ir, Nt + 1 ) = delay	   	! delay time
          SrcAngArr(  id, ir, Nt + 1 ) = SrcAngle		! angle
          RcvrAngArr( id, ir, Nt + 1 ) = RcvrAngle		! angle
          NTopBncArr( id, ir, Nt + 1 ) = NumTopBnc 		! Number of top     bounces
          NBotBncArr( id, ir, Nt + 1 ) = NumBotBnc 		!   "       bottom
       ENDIF
    ELSE      ! not a new ray
       !PhaseArr(   id, ir, Nt ) = PhaseArr( id, ir, Nt )
       AmpTot = AArr( id, ir, Nt ) + Amp
       DelArr(     id, ir, Nt ) = ( AArr( id, ir, Nt ) * DelArr( id, ir, Nt ) + Amp * delay ) / AmpTot ! weighted sum
       AArr(       id, ir, Nt ) = AmpTot
       SrcAngArr(  id, ir, Nt ) = SrcAngle
       RcvrAngArr( id, ir, Nt ) = RcvrAngle
    ENDIF

    RETURN
  END SUBROUTINE AddArr

  ! **********************************************************************!

  SUBROUTINE WRTARRASC( R, Nrd, Nr, TopOpt, freq, SourceType )

    ! Writes the arrival data (Amplitude, delay for each eigenray)
    ! ASCII output file

    REAL,    PARAMETER :: PI = 3.14159265, RadDeg = 180 / PI
    REAL    r( Nr )
    CHARACTER TopOpt*4, SourceType*1

    ! *** Thorpe attenuation? ***

    IF ( TopOpt(4:4) == 'T' ) THEN
       f2 = ( freq / 1000.0 ) ** 2
       alpha = 40.0 * f2 / ( 4100.0 + f2 ) + 0.1 * f2 / ( 1.0 + f2 )
       alpha = alpha / 914.4     ! dB / m
       alpha = alpha / 8.6858896 ! Nepers / m
    ELSE
       alpha = 0.0
    ENDIF


    WRITE( ARRFIL, * ) MAXVAL( NArr( 1:Nrd, 1:Nr ) )

    DO id = 1, Nrd
       DO ir = 1, Nr
          IF ( SourceType == 'X' ) THEN   ! line source
             factor =  4.0 * SQRT( PI ) * EXP( -alpha * r( ir ) )
          ELSE                            ! point source
             IF ( r ( ir ) == 0 ) THEN
                factor = 1e5       ! avoid /0 at origin
             ELSE
                factor = EXP( -alpha * r( ir ) ) / SQRT( r( ir ) )  ! vol. atten. and cyl. spreading
             END IF
          END IF

          WRITE( ARRFIL, * ) NArr( id, ir )
          DO IARR = 1, NArr( id, ir )
             AArrT = factor * AArr( id, ir, IARR )
             ! you can compress the output file a lot by putting in an explicit format statement here ...
             ! However, you'll need to make sure you keep adequate precision
             WRITE( ARRFIL, * ) AArrT, RadDeg * PhaseArr( id, ir, IARR ), DelArr( id, ir, IARR ), &
                  SrcAngArr( id, ir, IARR), RcvrAngArr( id, ir, IARR ), &
                  NTopBncArr( id, ir, IARR ), NBotBncArr( id, ir, IARR )
          END DO  ! next arrival
       END DO  ! next receiver depth
    END DO  ! next range

    RETURN
  END SUBROUTINE WRTARRASC

  ! **********************************************************************!

  SUBROUTINE WRTARRBIN( R, Nrd, Nr, TopOpt, freq, SourceType )

    ! Writes the arrival data (amplitude, delay for each eigenray)
    ! Binary output file

    REAL,    PARAMETER :: PI = 3.14159265, RadDeg = 180 / PI
    REAL    r( Nr )
    CHARACTER TopOpt*4, SourceType*1

    ! *** Thorpe attenuation? ***

    IF ( TopOpt(4:4) == 'T' ) THEN
       f2 = ( freq / 1000.0 ) ** 2
       alpha = 40.0 * f2 / ( 4100.0 + f2 ) + 0.1 * f2 / ( 1.0 + f2 )
       alpha = alpha / 914.4     ! dB / m
       alpha = alpha / 8.6858896 ! Nepers / m
    ELSE
       alpha = 0.0
    ENDIF

    WRITE( ARRFIL ) MAXVAL( NArr( 1:Nrd, 1:Nr ) )

    DO id = 1, Nrd
       DO ir = 1, Nr
          IF ( SourceType == 'X' ) THEN   ! line source
             factor =  4.0 * SQRT( PI ) * EXP( -alpha * r( ir ) )
          ELSE                            ! point source
             IF ( r ( ir ) == 0 ) THEN
                factor = 1e5       ! avoid /0 at origin
             ELSE
                factor = EXP( -alpha * r( ir ) ) / SQRT( r( ir ) )  ! vol. atten. and cyl. spreading
             END IF
          END IF

          WRITE( ARRFIL ) NArr( id, ir )

          DO IARR = 1, NArr( id, ir )
             AArrT = factor * AArr( id, ir, IARR )
             ! integers written out as reals below for fast reading in Matlab
             WRITE( ARRFIL ) AArrT, RadDeg * PhaseArr( id, ir, IARR ), DelArr( id, ir, IARR ), &
                  SrcAngArr( id, ir, IARR), RcvrAngArr( id, ir, IARR ),  &
                  REAL( NTopBncArr( id, ir, IARR ) ), REAL( NBotBncArr( id, ir, IARR ) )
          END DO   ! next arrival
       END DO   ! next receiver depth
    END DO   ! next range

    RETURN
  END SUBROUTINE WRTARRBIN

END MODULE ArrMod
