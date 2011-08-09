MODULE beampatternmod

  ! Loads a source beam pattern

  SAVE
  INTEGER, PARAMETER :: SBPFil = 50
  INTEGER NSBPPts
  REAL (KIND=8), ALLOCATABLE :: SrcBmPat( :, : )

CONTAINS

  SUBROUTINE READPAT( SBP, PrtFil )

    INTEGER     PrtFil
    CHARACTER*1 SBP

    IF ( SBP == '*' ) THEN
       WRITE( PrtFil, * ) '*********************************'
       WRITE( PrtFil, * ) 'Using source beam pattern file'

       OPEN ( FILE = 'SBPFIL', UNIT = SBPFil, STATUS = 'OLD' )

       READ(  SBPFil, * ) NSBPPts
       WRITE( PrtFil, * ) 'Number of source beam pattern points', NSBPPts

       ALLOCATE( SrcBmPat( NSBPPts, 2 ), Stat = IAllocStat )
       IF ( IAllocStat /= 0 ) &
            CALL ERROUT( PrtFil, 'F', 'BELLHOP', 'Insufficient memory for source beam pattern data: reduce # SBP points' )

       WRITE( PrtFil, * )
       WRITE( PrtFil, * ) ' Angle (degrees)  Power (dB)'

       DO I = 1, NSBPPts
          READ(  SBPFil, * ) SrcBmPat( I, : )
          WRITE( PrtFil, FMT = "(2G11.3)" ) SrcBmPat( I, : )
       END DO


    ELSE   ! no pattern given, use omni source pattern
       NSBPPts = 2
       ALLOCATE( SrcBmPat( 2, 2 ), Stat = IAllocStat )
       IF ( IAllocStat /= 0 ) &
            CALL ERROUT( PrtFil, 'F', 'BELLHOP', 'Insufficient memory'  )
       SrcBmPat( 1, : ) = (/ -180.0, 0.0 /)
       SrcBmPat( 2, : ) = (/  180.0, 0.0 /)
    ENDIF

    SrcBmPat( :, 2 ) = 10 ** ( SrcBmPat( :, 2 ) / 20 )  ! convert dB to linear scale

  END SUBROUTINE READPAT

END MODULE beampatternmod
