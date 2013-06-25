#ifndef PARAMS_H
#define PARAMS_H

// The number of attempts to temporarily and artificially change the animat travel rate
// while it is shore following and is stuck in a crevasse 
#define SHOREFOLLOW_ARTIFICIAL_CHANGE_ATTEMPS 30

// The number of attempts actually change the animat heading or heading and travel rate
// while it is shore following and is stuck in a crevasse 
#define SHOREFOLLOW_MODELBASED_CHANGE_ATTEMPTS 125

// The value in degrees to artificially increment the animat heading while it is shore
// following and is stuck in a crevasse.
#define SHOREFOLLOW_ARTIFICIAL_DEG_INCR 0.5

#ifndef FALSE
#define FALSE 0
#define TRUE !FALSE
#endif

#ifdef _DEBUG
const BOOL MULTISOUNDSOURCEALLOWED = TRUE;
#else
const BOOL MULTISOUNDSOURCEALLOWED = FALSE;
#endif

// Acoustic Aversion Testing
#define ACSTC_SOURCE_LEVEL_DB 235 // as per Dorian 5/21/09. 
								  // In code, most this can be set to is 255 due to way it is stored
								  // in the binary output file. (for 8 bytes)
// If clock starts at zero, (00:00:00), first iteration is transition from 0 to 1, so one is the result 
// of the first iteration.  12 is the result of the 12th iteration.
#define ACSTC_SOURCE_BEGINS_ITERATION 1
#define ACSTC_SOURCE_DUTY_PERIOD 10	


// Number of species groups.
#define NUM_SPEGROUPS_ALLOC SIZE_10 // This is the number of species groups reserved an must be greater than
										// or equal to NUM_SPEGROUPS_INUSE below.
#define NUM_SPEGROUPS_INUSE SIZE_10 // This must match the number of enumerations in SPECIESGROUP below.


// Behavior transition as a function of depth
#define DEFAULT_DEPTH_SPAN_SHALLOW 0
#define DEFAULT_DEPTH_SPAN_DEEP -3500

#endif