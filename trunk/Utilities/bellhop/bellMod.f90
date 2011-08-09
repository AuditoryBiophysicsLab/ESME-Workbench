MODULE bellmod

   INTEGER, PARAMETER :: PRTFIL = 6, MaxN = 300000
   REAL,    PARAMETER :: PI     = 3.14159265, RadDeg = 180 / PI
   COMPLEX, PARAMETER :: i      = ( 0.0, 1.0 )

   ! Reduce MaxN (= max # of steps along a ray) to reduce storage
   ! Note space is wasted in NumTopBnc, NumBotBnc ...

   INTEGER ( KIND=2 ) :: NumTopBnc( MaxN), NumBotBnc( MaxN )
   INTEGER            :: NSteps, Nrd_per_range
   REAL                  cV(   MaxN ), Ampv( MaxN ), Phasev( MaxN ), omega, depthT, depthB
   REAL    (KIND=8)   :: xv( 2,  MaxN ), TrayV( 2, MaxN ), pV(  2, MaxN ), qV( 2, MaxN ), tauV( MaxN ), rhoB, rhoT
   COMPLEX (KIND=8)   :: cpT, cpB
   CHARACTER          :: TopOpt*5

END MODULE bellmod
