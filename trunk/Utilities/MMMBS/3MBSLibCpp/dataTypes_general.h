#ifndef MMMBSDATATYPES_GENERAL
#define MMMBSDATATYPES_GENERAL

#include <windows.h>
#include <string.h>
#include <stdio.h>
#include <tchar.h>
#include <crtdbg.h>
#include "LinkedList.h"


// super.sub (2 decimal places) 
// starting version 5.4 bottom following has horizontal travel rates
#define MMBSLIB_VERSION_SUPER  8
#define MMBSLIB_VERSION_SUB     6
#define MMMBLIB_SPECIES_VERSION_SUPER 8
#define MMMBLIB_SPECIES_VERSION_SUB 5
#define MMMBLIB_BINOUTPUT_VERSION_SUPER 1 // String will have 3 decimal places.
#define MMMBLIB_BINOUTPUT_VERSION_SUB 0 // String will have 3 decimal places.
#define SZ_OUTPUTFILEID "3MBBinaryOutput"

// 3MB library version 7.01 was sent to Dorian on February 28 2010 as a bug fix so the library was incremented to version 7.02

// Version 7.0 of both 3mb and the species builder add the ability to have behaviors terminate as a function of
// cumulative distribution function (mean and standard deviation).

// Version 6.0 (both 3mb and species builder) introduces the behavior transition as a function of depth.  It also includes 
// ability for variable descent and ascent rates in dive reversals, cumulative vs simple probability of behavior transition
// doisplay, and beginings of slope environmental attractor.
// Previous versions were 3MB 5.10 and Species Builder 5.0


// super.sub (2 decimal places) 
// 07/04/09: Advanced to version 5.0 for hoizontal travel rates associated with bottom following.


#define NUMSECSPERDAY 24*60*60
// previous species version: 2.? and 3.0

/*
Version Information
A minor change not affecting results increments 100ths value
A change that affects results increments 10th's value
A release increments 1's value

1.00: Through Dorian Meeting December 10, 2006
1.10: Through Dorian Meeting January 08, 2007
	Note that 1.10 and 1.00 binary outputs are not compatable.
1.20: Started on January 08, 2007 through Dorian Meeting Jaunuary 22, 2007
	  Corrects problem with animat
1.30: Started on January 23, 2007
1.31: Fixes problem with dive depth going beyond bathydepth
1.32: Modifications to Dive routine (clean up).
1.40: Modifications to Dive routine (clean up).  Data generated and brought to meeting
      with Dorian on 2/26/2007
1.41  - Begins 2/26/2007, post Dorian meeting.
      - Placed Reversals as top priority, bottom following second, ascending default.
	    previously (1.40) bottom following had priority.
	  - Release to BU ESME.
1.42  - Begins May 8, 2007
	  - (05/8/07) Added ability to set/override acoustic exposure bathymetry, temperature,
	    and salinity from calling application. Only bathymetry and acoustic exposure are
		currently allowed through the ESME-3mbs wrapper class pending future discoveries.
	  - (05/8/07) Corrected bug that had the simulation run 1 iteration during
	    initialization.  Not supposed to do that.
	  - (05/9/07) Added bathymetry depth to the text (.trk) output file.
	  - (05/9/07) Corrected the problem of the default bathy depth overrding user set
	             bathy depth at initialization.
	  - (05/14/07) Added ability to set output directory.

1.43  - Colin special release, compiled (06/19/2007)
	    - has all of the work done so far for version 1.5, none of which is functional or
		  is called.

1.50	  - Begins (06/11/07)
	  - (06/11/07)
		* Packed Data modified
			-	modified the packed data structure: reduced Dive activity bits from four
				down to three, allowing for 8 dive activities (only 7 used, highly unlikely
				there will ever be more)
			-	reduced behavior bits from 8 down to 7, allowing/limiting number of user
				defined matrix model behaviors for/to 128.
			-	allocated 5 bites for overriding behaviors
		* Added NRMLBEH structure as an initial step for coast-following behavior
		* Corrected crash bug that happens if 3mbs hangs, abort is hit (and doesn't respond)
		  then user clicks the [x] in upper right hand corner.  Bug is due to animat function
		  Direction() when the animat gets stuck (the Colin-Eryn problem) in bathy
		  cracks so it becomes unable to change its direction.  When user hits abort the
		  application doesn't repond because it was stuck in that loop.  A Static variable
		  s_abort for the animat class was added so if the user hits abort the animat can
		  exit that loop and everything shuts down properly.
	    * A solution to the Direction() problem with the animat getting stuck is 
		  to slow the animat down then let it try to find a direction to head.  A strategy
		  tested is to reduce its speed by 1% per 1000's attempts (every 1000 attempts to
		  find a new direction the animat's speed is reduced to 99%).  Need Dorian's approval.
		  Even with coast-following implemented this problem won't go away, so this seems a 
		  good strategy.  How to handle the behavior modeling if the speed is reduced??? Need
		  to check with Dorian.  
*/

const double PI = 3.14159265358979;

#define SIZE_2		  2
#define SIZE_3		  3
#define SIZE_4		  4
#define SIZE_8		  8
#define SIZE_9		  9
#define SIZE_10		 10
#define SIZE_12		 12
#define SIZE_16		 16
#define SIZE_20		 20
#define SIZE_24		 24
#define SIZE_28		 28
#define SIZE_32		 32
#define SIZE_36		 36
#define SIZE_40		 40
#define SIZE_43		 43
#define SIZE_44		 44
#define SIZE_48		 48
#define SIZE_52		 52
#define SIZE_54		 54
#define SIZE_56		 56
#define SIZE_58		 58
#define SIZE_60		 60
#define SIZE_64		 64
#define SIZE_68		 68
#define SIZE_72		 72
#define SIZE_76		 76
#define SIZE_96		 96
#define SIZE_120	120
#define SIZE_124	124
#define SIZE_128	128
#define SIZE_200	200
#define SIZE_254	254
#define SIZE_256	256
#define SIZE_384	384
#define SIZE_512	512
#define SIZE_1024  1024
#define SIZE_2048  2048
#define SIZE_4096  4096
#define SIZE_8192  8192


#define SPECIES_DESCRPTN_MAX_LEN SIZE_96
#define SPECIES_COMMENT_MAX_LEN SIZE_4096


// All vector models are ALWAYS either type MATRIX, ARRAY or ELEMENT
// ELEMENT's memory in the vector model is statically allocated and will always have one
// row and one column, even if not used in the species being modeled.
union EIGHTBYTEPTRS
{
	// both **ppa and *pa are 4 bytes on a 32 bit machine, 8 bytes on a 64 bit machine.
	double **ppa; // used when pointing to a matrix
	double *pa; // used when pointing to a vector
	TCHAR __space[SIZE_8];
	//EIGHTBYTES bytes;
};

typedef struct Matrix
{
	INT32 rowCnt;
	INT32 colCnt;
	EIGHTBYTEPTRS p;
	//double **a; // elements in the matrix 
	//TCHAR _16byteAlign[4]; // 16 byte alignment
}MATRIX; // 16 bytes


typedef struct Array
{
	INT32 rowCnt;
	INT32 colCnt;
	EIGHTBYTEPTRS p; // elements in the array 
}ARRAY; // 16 bytes

//typedef struct ARRAY03pt0 ARRAY;

typedef struct ELEMENT
{
	INT32 rowCnt;
	INT32 colCnt;
	double a; // elements in the array 
}ELEMENT; // 16 bytes

typedef struct CoordinateWithDepth
{	
	double lon;
	double lat;
	double depth;
	char _res[8];
}COORD_DEPTH;

typedef struct AcousticSrceInf
{
	// Information about the animat if it is actually a scound source
	DWORD beginIteration;
	DWORD dutyPeriod;
	float outputValue; // in dB 
	BOOL isASoundSource;
	BYTE _RESERVED[SIZE_16];
}ACOUSTICSRCEINF;

typedef struct InhabitantInf
{
	COORD_DEPTH coord; // Common to all inhabitants
	ACOUSTICSRCEINF acstcSrc; // Acoustic Source Specific
}INHABITINF;

typedef struct InhabitantBufferInf
{
	int popBuffLen; // population buffer length
	int highlightBuffLen; // highlighted buffer length.
	int numHighlightedAnimats;
	INHABITINF *popBuff; // population buffer
	INHABITINF *highlightBuff; // highlighted population buffer
}INHABITBUFFERINF;

typedef struct COORDVALUE
{
	double lon;
	double lat;
	double value;
	TCHAR _res[8];
}COORDVALUE;

typedef struct CountBin
{
	int trans; // behavior transitioned to.
	int sec; // time in behavior before transitioning.
}CNTBIN;

typedef struct SingleBehaviorTransitionTestStruct
{
	CNTBIN **cnt; //[Period][trail]
}SNGLBEHTRANSTRUCT;

typedef struct BehaviorTransitionTestStruct
{
	int duration; // In seconds;
	int numBehaviors; 
	int *secs; // dynamically allocated based upon the number of seconds desired.
	MATRIX *m; // array of transition matrices, one for each behavior.
}BEHTRANSTESTSTRUCT;


#endif // MMMBSDATATYPES_GENERAL