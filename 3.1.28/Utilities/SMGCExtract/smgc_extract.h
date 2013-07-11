/*
 *
 * extract.h  Include file for gmca_extract.c
 *
 * extract.h has some variables and prototypes used 
 * by gmca_extract.
 *
 * 12/19/98 by Mike Duncan, Planning Systems Inc.
 *
 */


#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#define MSTAT 182    /*  Number of elements per stat record )*/
#define PSTAT   7    /*  Number of stats per parameter */
#define NPARAM 18    /*  Number of parameters per month */
#define PARSIZ 4     /*  Number of bytes per element */

short int MISSVAL = -9999;  /* the infamous missing value */

FILE *GetSMGCFile(int lat, int lon);


/* changed wave and swell period scale factors to
 * match what's in smgc_stat.h. also changed wind, swell
 * and wave directions  --md-- 9/2/02 */
/* scale factors for all the variables */
double Scale[NPARAM] = { 0.10,		/* year */
                     	0.10,		/* air/sea temp diff */
					   	0.10,		/* air temp */
					   	1.00,		/* ceiling height */
					   	0.10,		/* relative hum. */
                     	0.10,		/* sea level pressure */
					   	0.10,		/* sea surf/ temp. */
					   	10.0,		/* swell direction */
					   	0.10,		/* swell height */
					   	0.10,		/* swell period */
                    	1.00,		/* total cloud cover */
						1.00,		/* visibility  */
						10.0,		/* wave direction */
						0.10,		/* wave height */
						0.10,		/* wave period */
                    	1.00,		/* weather */
						1.0,		/* wind direction */
						0.10};		/* wind speed */


/* the nominal range of databank values in real form.
These are adjusted in some cases to "include" the
min or max value.  For the true databank range see
gmca.readme */

double Range[NPARAM][2] = {	1854.00, 1997.00,	/* year */
			 -82.00,   50.50,   	/* air/sea temp diff */
			 -50.00,   50.00,   	/* air temp.  */
			   0.00,    9.00,   	/* ceiling height */
			   0.00,  104.40,  	/* relative humidity */
			 920.00, 1060.00, 	/* sea level pressure */
			  -4.00,   40.00,   	/* sea surface temp */
			   0.00,   36.00,  	/* swell direction */
			   0.00,   50.00,   	/* swell height */
			   0.00,   30.00,   	/* swell period */
 			   0.00,    8.00,   	/* total cloud cover */
			   1.00,   10.00,   	/* visiblilty */
			   0.00,   36.00,  	/* wave direction */
			   0.00,   35.00,   	/* wave height */
			   0.00,   30.00,   	/* wave period */
			   0.00,   99.00,   	/* weather */
			   0.00,   36.00,  	/* wind direction */
			   0.00,  102.00  };	/* wind speed */


/* BinSizes for the various SMGC parameters */

double BinSize[NPARAM] = {1.00,         /* year */
                           1.00,        /* air/sea temp diff */
                           1.00,        /* air temp.  */
                           1.00,        /* ceiling height */
                           1.00,        /* relative humidity */
                           1.00,        /* sea level pressure */
                           1.00,        /* sea surface temp */
                           1.00,        /* swell direction */
                           1.00,        /* swell height */
                           1.00,        /* swell period */
                           1.00,        /* total cloud cover */
                           1.00,        /* visiblilty */
                           1.00,        /* wave direction */
                           1.00,        /* wave height */
                           1.00,        /* wave period */
                           1.00,        /* weather */
                           1.00,        /* wind direction */
                           1.00};       /* wind speed */

/* END extract.h */
