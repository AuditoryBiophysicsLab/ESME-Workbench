#ifndef MMMBSDATATYPES_SPECIESMODELING
#define MMMBSDATATYPES_SPECIESMODELING
#include "dataTypes_general.h"


// Sub model ACOUSTIC_POD_RESPONSE_goesAway models how individuals of a species respond when, in a pod, individuals' acoustic
// exposures exceed a maximum threshold (maximum threshold is modeled in struct ACOUSTIC_POD_RESPONSE_goesAway.  The individual
// will either remain with the pod, or break away, presumably to escape the acoustic source.enum ACOUSTIC_POD_RESPONSE_goesAway.
enum DIRECTIONAL_MODEL_TYPE
{
	RANDOM_WALK = 0,
	VECTOR_MODEL_DIRECTIONAL_NO_BIASING = 1,
	VECTOR_MODEL_DIRECTIONAL_WITH_VECTOR_MODEL_BIASING = 2, 
	CORRELATED_RANDOM_WALK = 3,
	CORRELATED_RANDOM_WALK_WITH_DIR_BIASING = 4,
};

enum BTTMFLLW_MDL_TYPE
{
	NO_BTTMFLLWNG = 0, // No bottom following
	BOTTOMFOLLOWS_NORML_MDL = 1, // Bottom following follows normal behavior travel rate
	BOTTOMFOLLOWS_REPLACEMENT_MDL = 2, // Bottom following with bottom following travel rate
};

enum STANDARD_MODEL_TYPE
{
	GAUSSIAN, // Gaussian distribution
	UNIFORM,  // Uniform random distribution
	VECTOR,	  // Vector model
};


typedef struct GAUSS
{
	double mean;
	double std;
	double coeff;
	BYTE _RESERVED[SIZE_8];
}GAUSS;

typedef struct RANDOM
{
	double max;
	double min;
	double coeff;
	BYTE _RESERVED[SIZE_8];
}RANDOM;

//------------------//
// Behavior Modeling
//------------------//
//--------------//
// Rate Modeling
//--------------//
typedef struct RATE_VECTORMDL
{
	MATRIX vector;
	ELEMENT step;
	ARRAY  terminate;
}RATE_VECTORMDL;

typedef struct RATE_MODEL
{
	STANDARD_MODEL_TYPE modelType;
	BYTE _RESERVED1[SIZE_4];

	RANDOM rnd;
	GAUSS gauss;
	RATE_VECTORMDL vm;

	BYTE  _RESERVED2[SIZE_32];
	GAUSS ae;
}RATE_MODEL; // used in rate of travel and acent/decent rates. 112 + 48 = 160

//----------------//
// Direction Model
//----------------//
typedef struct RANDOMWALK
{
	double termCoeff;
	BYTE _RESERVED[SIZE_24];
}RANDOMWALK;

typedef struct CORRANDWALK
{
	double perturbation;
	double termCoeff;
	BYTE _RESERVED[SIZE_16];
}CORRANDWALK;

typedef struct CORRANDWALKDB
{
	double perturbation;
	double directionOfBias;
	double bias;
	double arcStep;
	double termCoeff;
	BYTE _RESERVED[SIZE_24];
}CORRANDWALKDB;

typedef struct DIRVECTORMODEL
{
	MATRIX direction;
	MATRIX directionalBias;
	ARRAY  terminate;
}DIRVECTORMODEL;


//------------------//
// Reversal Modeling
//------------------//
typedef struct ReversalIntegerRandom
{
	int min;
	int max;
}REV_INT_RND;
typedef struct ReversalRandomModel
{
	double probOfReversal;
	REV_INT_RND count;
	GAUSS time;
}REVERSAL_RND;

typedef struct ReversalGaussianModel
{
	double probOfReversal;
	GAUSS count;
	GAUSS time;
	BYTE _RESERVED[SIZE_8];
}REVERSAL_GAUSS;

//--------------------------------------------------------------------------------------//
//                    version 2.0
//--------------------------------------------------------------------------------------//
typedef struct VectorModel
{
	ARRAY vector;
	ELEMENT step;
	BYTE _RESERVED[SIZE_32];
}VCTRMDLPARAM;

////
//--------------//
// Rate Modeling
//--------------//
typedef struct RateVectorModel
{
	ARRAY vector;
	ELEMENT step;
	ELEMENT terminate;
	BYTE _RESERVED[SIZE_16];
}RATEVCTRMDLPARAM;

typedef struct RateSpeciesSubmodel
{
	RANDOM rnd;
	GAUSS gauss;
	RATEVCTRMDLPARAM vm;
	STANDARD_MODEL_TYPE modelType; // 4 bytes
	BYTE  _RESERVED[SIZE_28];
}RATEMDL; // used in rate of travel and acent/decent rates. 112 + 48 = 160


typedef struct DEPTHPARAM
{
	RANDOM rnd;
	GAUSS gauss;
	VCTRMDLPARAM vm;
	STANDARD_MODEL_TYPE modelType;
	BYTE _RESERVED[SIZE_28];
}DEPTHPARAM; // Used in rate of travel and acent/decent rates.

//-----------------//
// Surface Interval
//-----------------//
typedef struct SurfaceIntervalModel
{
	GAUSS gauss;
	VCTRMDLPARAM vm;
	STANDARD_MODEL_TYPE modelType;
	BYTE _RESERVED1[SIZE_28];
}SURFINTRVLPARAM;


typedef struct DirectionVectorModel
{
	ARRAY direction;
	MATRIX directionalBias;
	ELEMENT  terminate;
}DIRVCTRMDLPARAM;

typedef struct DirectionModel
{
	RANDOMWALK rndWalk;
	CORRANDWALK crRndWalk;
	CORRANDWALKDB crRndWalkDb;
	DIRVCTRMDLPARAM vm;
	DIRECTIONAL_MODEL_TYPE modelType;
	BYTE  _RESERVED1[SIZE_28];
}DIRCTNMDL; // 128 + 48 = 176


//-------------------//
// Reversals Modeling
//-------------------//
typedef struct ReversalVectorModel
{
	ELEMENT  probOfReversal;
	ARRAY count;
	ARRAY time;
	ELEMENT timeStep;
}REVVCTRMDLPARAM;


// Added 12/21/09
enum REVERSAL_DIVE_RATE_TYPE
{
	NO_INDEPENDENT = 0,
	INDEPENDENT = 1, // a single rate for diving specific for reversal
	INDEPENDENT_DIVE_AND_ASCENT = 2, // a dive rate and ascent rate specific for reverasl.
};

typedef struct ReversalSpeciesDefinition
{
	REVERSAL_RND rnd;
	REVERSAL_GAUSS gauss;
	REVVCTRMDLPARAM vm;

	//---------------------------------------//
	// modified 12/21/09
//	BOOL hasaIndependentDiveRate;
	REVERSAL_DIVE_RATE_TYPE diveRateType; // 4
	//BYTE _RESERVED1[SIZE_12];
	//---------------------------------------//

	GAUSS diveRate; // models descent and ascent rates if diveRateType is INDEPENDENT and
					// descent if diveRateType is INDEPENDENT_DIVE_AND_ASCENT

	BOOL reverses; // no=0, yes=1 move to species model. //4
	STANDARD_MODEL_TYPE modelType; // 4

	// 16 byte alignment before other structs
	BYTE _RESERVED2[SIZE_8];

	//---------------------------------------//
	// modified 12/21/09
	//BYTE _RESERVED2[SIZE_32];
	GAUSS ascentRate; // added 12/21/09
	//---------------------------------------//
}REVERSAL_DEF;
#pragma message("Make REVERSAL_DEF 16-byte aligned on next species upgrade")



enum DECAYFUNCTION
{
	FNCaXtimesfive,
	FNCbbeexeequalmc2,
	dogface,
};

enum UNITS
{
	unitsPhysXFX8,
	unitsBehZZZEE,
	WOWHOTDOG,
	IMASPECIALGROUP,
};

//-----------------------------//
// Behavior Transition Modeling
//-----------------------------//
typedef struct BehaviorTransitionModel
{
	double meanTimeInBeh; //T50
	double slopeCoefficient; // k
	BYTE  _reserved[SIZE_16];
	ARRAY behavior;
	ELEMENT terminate; // no longer used, but preserved for backwards compatability.
}BEHTRANSMDL; // this should go away entirely and be put into the upgrader application.

typedef struct DepthEnvAttractorModel
{
	double shelfDepth;
	double basinDepth;
	double slopeDepth;
	// 24 bytes

	BOOL shelfIsEnabled;
	BOOL basinIsEnabled;
	BOOL slopeIsEnabled;
	// 12 bytes: 12 + 24 = 36

	BYTE _reserved[SIZE_4]; //36 + 4 = 40

	double shelfSlope;
	double basinSlope;
	double slopeSlope;
	// 24 bytes: 40 + 24 = 64

}DEPTH_ENV_ATTRACTOR_MDL;


typedef struct PlaceHolderAttractorModel
{
	double max;
	double min;
	double delta;
	// 24 bytes

	BOOL maxIsEnabled;
	BOOL minIsEnabled;
	BOOL deltaIsEnabled;
	// 12 bytes: 12+24 = 36

	BYTE _reserved[SIZE_28];
	//28 bytes: 36+ 28 = 64
}ENVATTRACTORMDL;

//---------------------------//
// Acoustic Exposure Modeling
//---------------------------//
// Need to set initial values.
typedef struct AcousticAversionSpeciesModel
{
	BOOL travelDirectionAffected;
	BOOL travelRateAffected;
	BOOL depthAffected;
	BOOL surfaceIntervalAffected;

	BOOL ascentRateAffected;
	BOOL descentRateAffected;
	BOOL reversalAffected;
	BOOL flatBottomDiveAffected; // Always active to prevent flat bottom diving.

	double beachingDepth;
	BOOL podBreaksUp;
	BOOL beaches;

	BOOL flatBottomDives;
	BYTE _RESERVED[SIZE_28];

	GAUSS travelRate;
	CORRANDWALKDB travel;
	REVERSAL_GAUSS reversal;
	GAUSS surfaceInterval;
	GAUSS depth;
	GAUSS descentRate;
	GAUSS ascentRate;
}ACSTCAVRSNMDL;


//--------------------------------------------//
// Bottom Following with Dive Rate
// (added on 07/04/09 for species version 5.0)
//--------------------------------------------//
typedef struct BottomFollowing
{
	BTTMFLLW_MDL_TYPE type;
	BYTE _RESERVED1[SIZE_28];

	RATEMDL rateMdl;
	BYTE _RESERVED2[SIZE_32];
}BTTMFLLW;


typedef struct DivingSpeciesModel
{
	RATEMDL descentRate;
	RATEMDL ascentRate;
	SURFINTRVLPARAM srfInv;
	REVERSAL_DEF reversal;
	DEPTHPARAM depth;

	BTTMFLLW bttmFollow;
	//BOOL bttmFollows;
	BYTE _RESERVED[SIZE_32];
}DIVEMDL;


 // delete this
#define SZ_BEHAVIOR_LEN SIZE_64
typedef struct BEHAVIOR_NAME
{
	TCHAR sz[SZ_BEHAVIOR_LEN];
}BEHAVIOR_NAME;

typedef struct DepthSpan
{
	int shallow;
	int deep;
	BYTE _RES[SIZE_8];
}DEPTHSPAN;

enum BEHTRANS_TERM_MODEL
{
	T50_K_TERM,
	GAUSSIAN_TERM, // Gaussian distribution
};

typedef struct BehaviorTransitionStruct // added in version 6.0 3MB version 6.0 species builder;
{
	BYTE _RES1[SIZE_64];
	DEPTHSPAN depthSpan;
	MATRIX m;
}BEHTRAN;

union BEHTRAN8BYTEPTR
{
	BEHTRAN *arr; // 4 bytes compiled as 32 bit addressing, 8 bytes as 64 bit.
	BYTE __space[SIZE_8];
};// 8 bytes regardless of 32- or 64-bit compilation.


// This structure is written to file.
typedef struct BehaviorModel
{
	TCHAR szName[SZ_BEHAVIOR_LEN];

	// Behavior Transitions
	int nrmlBehTransCnt;
	BEHTRANS_TERM_MODEL nrmlBehTermType; // Added for behavior model version 7.
	BOOL depthHasPriorityOverTemp;
	BYTE _RES1[SIZE_20];

	BEHTRAN8BYTEPTR nrmlBehTrans;
	BYTE _RES2[SIZE_24];

	BEHTRANSMDL depthEnvAttBehTrans; // while in depth environmental attractor (not used at this time)
	BEHTRANSMDL tempEnvAttBehTrans; // while in temperature environmental attractor (not used at this time)

	// Environmental Attractors
	DEPTH_ENV_ATTRACTOR_MDL depthEnvAtt;
	//ENVATTRACTORMDL  depthEnvAtt;
	ENVATTRACTORMDL tempEnvAtt;

	// Travel
	RATEMDL travelRate;
	DIRCTNMDL travelDirection;

	// Dive;
	DIVEMDL dive;

	// More reserved space.
	BYTE _RES3[SIZE_64];
}NRMLBEHMDL;

union NRMLBEHMDL8BYTEPTR
{
	NRMLBEHMDL *behavior; // 4 bytes compiled as 32 bit addressing, 8 bytes as 64 bit.
	TCHAR __space[SIZE_8];
};// 8 bytes regardless of 32- or 64-bit compilation.

// Used in both SpeciesBinaryOutInformation (SPECIESBINOUTINF) struct and 
// SpeciesModel (SPECIES_MDL) struct

// SPECIESSPECIFICATION is it's own struct because it is used in two other structs:SPECIESBINOUTINF and SPECIES_MDL
#define SZ_SPECIESSPECIFICATION_HEADER "SpeciesSpecific"
typedef struct SpeciesSpecification
{
	TCHAR szHeader[SIZE_16]; // "SpeciesSpecific"

	UINT32 mbsVerSuper;
	UINT32 mbsVerSub;
	UINT32 speVerSuper;
	UINT32 speVerSub;

	UINT32  year; // Save Year
	UINT32  month;// Save Month
	UINT32  day;// Save Day
	UINT32  hour;// Save Hour

	UINT32  min;// Save Min
	UINT32  sec;// Save Sec
	UINT32  id; // ID generated using save date, time, and millisecond information.
	UINT32 numBehaviors;

	UINT32 group;  // Species Group enumeration, see enum SPECIESGROUP
	UINT32 name; // Species name enumeration, see enum SPECIESNAME

	BOOL deepWaterSeedingDepthEnabled;
	BYTE _reserved1[SIZE_36];

	double shoreFollowDepth;
	double minSeedingDepth;

	double deepWaterSeedingDepth;

	BYTE _reserved2[SIZE_56];

	//----------------------------------------------------------------------------------//
	// Room here for stuff needed for acoustic source.  Take away from _reserved2 above.
	//----------------------------------------------------------------------------------//
	TCHAR speciesShrtDscrptn[SPECIES_DESCRPTN_MAX_LEN];
	TCHAR speciesComment[SPECIES_COMMENT_MAX_LEN];
}SPECIESSPECIFICATION; // Remains the same in version 4


// Species Information Saved To binary file.
#define SPECIES_TITLE_BUFF_LEN SIZE_256
typedef struct SpeciesBinaryOutInformation
{
	// File title comes strictly from the name of the file loaded in and must be/is set by
	// the application loading the species in.
	TCHAR  fileTitle[SPECIES_TITLE_BUFF_LEN];
	SPECIESSPECIFICATION description;
	BYTE _RESERVED[SIZE_16];
}SPECIESBINOUTINF;

//-----------------//
// Species Modeling
//-----------------//
typedef struct SpeciesModel
{
	SPECIESSPECIFICATION description;
	NRMLBEHMDL8BYTEPTR p;
	BYTE _reserved1[SIZE_72]; // from 68 to 72

	ACSTCAVRSNMDL acousticAversion;

	int initBehSpanCnt;
	BEHTRAN8BYTEPTR initialBehavior;
	BYTE _reserved2[SIZE_20]; // from 68 to 72

}SPECIES_MDL;

#endif // MMMBSDATATYPES_SPECIESMODELING
