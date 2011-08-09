MODULE sspmod

   ! holds SSP input by user and associated variables

   IMPLICIT NONE
   SAVE
   INTEGER, PARAMETER :: MaxSSP = 2001
   INTEGER NSSP, LAYER
   REAL ( KIND = 4 ) :: ZSSPV( MaxSSP ), CSSPV( MaxSSP ), CZV( MaxSSP ), N2V( MaxSSP ), N2ZV( MaxSSP ), CVS( 4, MaxSSP )
END MODULE sspmod
