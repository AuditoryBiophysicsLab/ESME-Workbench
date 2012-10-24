#ifndef MMMBSDATATYPES_H
#define MMMBSDATATYPES_H

#include "dataTypes_scenarioExecution.h"
#include "dataTypes_speciesModeling.h"
#include "ListManager.h"
#include "dataTypes_ScenarioToFile.h"
#include "dataTypes_ScenarioSetup.h"

//#ifndef KILO
const int KILO = 1024;
//#endif

//#ifndef MEGA
const int MEGA = 1024*1024;
//#endif

//#ifndef GIGA
const int GIGA = 1024*1024*1024;
//#endif

//------------------------- Defaults ---------------------------------------------------//
const int MAX_NUM_ANIMATS = 262144; // 2^18.
const double BATHY_MIN_SEED_DEPTH = -2.00000000000001;

//#define BATHY_INITIAL_MAX_DISPLAY_DEPTH -2000.0
const int BATHY_DEFAULT_DEPTH = -5000;

const double SHORE_FOLLOW_DEPTH_DEFAULT = -10.00000001;
const double ANIMAT_BEACHES_DEPTH_DEFAULT = -2.0000000000001;

const double AVAIL_PHYSICAL_MEM_SCALE_FACTOR = 1.0/3.0; // reduces by scale factor, so
const int MAXIMUM_PHYSICAL_MEMORY_ALLOC = GIGA; // 1 gig
//--------------------------------------------------------------------------------------//

//--------------------------------------------------------------------------------------//
// Animat State Ouput Bit Format For Packed Data
//----------------------------------------------//
// These are bit shifts (I think they are really masks, not shifts... need to investigate)
#define ACOUSTIC_AVERSION_SHIFT 1	//00001
#define POD_FOLLOW_SHIFT 2			//00010
#define SHORE_FOLLOW_SHIFT 4		//00100
#define ENV_ATTR_DEPTH_SHIFT 8		//01000
#define ENV_ATTR_TEMP_SHIFT 16		//10000
#define ENV_OFFSCREEN_RESET_OVERRIDE 18
/*3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0  Mask Value   Shift Value
1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0  (num bits) (first bit loc)
----------------------------------------------------------------------------------------------------------------
0 0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 = 127 =  1h  <<  20   = Next Behavior
0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 =   1 =  1h  <<  19   = Next Behavior Selected
0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 =   1 =  1h  <<  18   = Off Screen Animat Reset
0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 =   1 =  1h  <<  17   = AE Threshold Flag Beh B
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 =   1 =  1h  <<  16   = AE Threshold Flag Phys B
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 =  31 = 1fh  <<  11   = Override Behaviors
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 =   7 =  7h  <<   8   = Dive Activity
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 =   1 =  1h  <<   7   = AE Threshold Flag Phys A
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 = 127 = 7fh  <<   0   = Current Behavior
//-------------------------------------------------------------------------------------------------------*/



//--------------------------------------------------------------------------------------//
// Constant Declarations
//----------------------//
// note that MAX_PATH is defined in WinDef.h to be 260
const int BUFFERED_MAX_PATH = MAX_PATH + 252; // equals a total of 256 + 256 = 512
const TCHAR TXTHEADERPre[]  = TEXT("3mbVer(%d.%02d)   speVer(%d.%02d)   speUniqueID(%05d)   Group(%s)   ShoreFollow(%.1f) .               .                     .                      . . . . . . . . . . . . . . .\n");
const TCHAR TXTHEADERPre2[] = TEXT("3mbVer(%d.%02d)   speVer(--)   Group(--)   ShoreFollow(--) AcstcSrcLvl(%s) AcstcSrcStartTime(%s) AcstcSrcDutyPeriod(%s) . . . . . . . . . . . . . . .\n");
const TCHAR TXTHEADER0[]    = TEXT(".      .       .     .               .              .          .        .        .         .        .        .                   .                   .           .           ThrshA Pys  ThrshB Psy  ThrshB_Beh .      .\n");
const TCHAR TXTHEADER1[]    = TEXT("Unique .       . ___Behavior____         Active        Dive      Dive    Current   Bathy   .  TravelModel    .                   .                   Cumulative   Instant     %5.1f dB    %5.1f dB     (RSK)     Rel    Secs \n");
const TCHAR TXTHEADER1x[]   = TEXT("Unique .       . ___Behavior____         Active        Dive      Dive    Current   Bathy   .  TravelModel    .                   .                   Cumulative   Instant     ----- dB    ----- dB     (RSK)     Rel    Secs \n");
const TCHAR TXTHEADER2[]    = TEXT("  ID   Clock   State       Name         Modifiers    Activity    Rate     Depth    Depth    Rate    Bearing       Longtude            Latitude        Exposure    Exposure   Flag Count  Flag Count    Value    Angle   Avert\n");
const TCHAR TXTHEADER3[]    = TEXT("------ ------- ----- --------------- -------------- ---------- -------- -------- --------- -------- -------- ------------------- ------------------- ----------- ----------- ---- -----  ---- -----  ---------- ------ ------\n");
//                            1      2       3     4               5              6          7        8        9         10       11       12                  13                  14          15          16   17     18   19     20             21     22                        

const TCHAR TXTHEADER0_SPECONS[]   = TEXT(".      .       .     .               .              .          .        .        .         .        .        .                   .                   .           .           ThrshA Pys  ThrshB Psy  ThrshB Beh  .     .\n");
const TCHAR TXTHEADER1_SPECONS[]   = TEXT("Unique .       . ___Behavior____         Active        Dive      Dive    Current   Bathy   .  TravelModel    .                   .                   Cumulative   Instant    %5.1f dB    %5.1f dB    %5.1f dB     Rel   Secs\n");
const TCHAR TXTHEADER2_SPECONS[]   = TEXT("  ID   Clock   State       Name         Modifiers    Activity    Rate     Depth    Depth    Rate    Bearing       Longtude            Latitude        Exposure    Exposure   Flag Count  Flag Count  Flag Count  Angle  Avert\n");
const TCHAR TXTHEADER3_SPECONS[]   = TEXT("------ ------- ----- --------------- -------------- ---------- -------- -------- --------- -------- -------- ------------------- ------------------- ----------- ----------- ---- -----  ---- -----  ---- -----  ----- ------\n");
//                                   1      2       3     4               5              6          7        8        9         10       11       12                  13                  14          15          16   17     18   19     20   21     22    23

const TCHAR PINWHEEL[4][2] = {TEXT("-"), TEXT("\\"), TEXT("|"), TEXT("/")};

const TCHAR CRLF[3] = {13, 10, 0}; 
const TCHAR SZ_BEH[]			= TEXT("behavior");
const TCHAR SZ_BEH_INIT[]		= TEXT("initial_behavior");
const TCHAR SZ_BEH_TERM[]		= TEXT("terminate_behavior");

const TCHAR SZ_ROT[]			= TEXT("rate_of_travel");
const TCHAR SZ_ROT_STEP[]		= TEXT("rate_of_travel_step_value");
const TCHAR SZ_ROT_TERM[]		= TEXT("terminate_rate_of_travel");

const TCHAR SZ_DIR[]			= TEXT("direction");
const TCHAR SZ_DIR_BIAS[]		= TEXT("direction_bias");
const TCHAR SZ_DIR_TERM[]		= TEXT("terminate_direction");

const TCHAR SZ_DEPTH[]			= TEXT("dive");
const TCHAR SZ_DEPTH_STEP[]		= TEXT("depth_step_value");

const TCHAR SZ_ROA[]			= TEXT("rate_of_ascent");
const TCHAR SZ_ROA_STEP[]		= TEXT("rate_of_ascent_step_value");
const TCHAR SZ_ROA_TERM[]		= TEXT("terminate_rate_of_ascent");

const TCHAR SZ_ROD[]			= TEXT("rate_of_dive");
const TCHAR SZ_ROD_STEP[]		= TEXT("rate_of_dive_step_value");
const TCHAR SZ_ROD_TERM[]		= TEXT("terminate_rate_of_dive");

const TCHAR SZ_SI[]				= TEXT("surface_interval");
const TCHAR SZ_SI_STEP[]		= TEXT("surface_interval_step_value");

const TCHAR SZ_REV[]			= TEXT("reversals");
const TCHAR SZ_REV_PROB[]		= TEXT("prob_reversals");
const TCHAR SZ_REV_TIME[]		= TEXT("time_reversed");
const TCHAR SZ_REV_TIME_STEP[]	= TEXT("time_reversed_step_value");



//--------------------------------------------------------------------------------------//
// Preprocessor Definitions
//-------------------------//
#define SCENARIO_EXT	TEXT(".sce")
#define BATHYMETRY_EXT   TEXT(".bth")
#define SALINITY_EXT     TEXT(".slt")
#define TEMPERATURE_EXT  TEXT(".tem")
#define MMMBS_OUTPUT_EXT TEXT(".3mb")
#define SPECIES_EXT      TEXT(".spe")

const double SHELF_ENVATTR_BATHY_SHALLOWER_THAN = -550.0;
const double SHELF_ENVATTR_DEG_LESS_THAN = 1.0;

const double BASIN_ENVATTR_BATHY_DEEPER_THAN = -550.0;
const double BASIN_ENVATTR_DEG_LESS_THAN = 1.0;

const double SLOPE_ENVATTR_BATHY_DEEPER_THAN = -150;
const double SLOPE_ENVATTR_DEG_GREATER_THAN = 1.0;


// States, messages, status
typedef struct DISTANGL
{
	double distance;
	double angle;
}DISTANGL;


typedef struct HHMMSS
{
	DWORD hour;
	DWORD min;
	DWORD sec;
}HHMMSS;

typedef struct DayHourMinSec
{
	DWORD day;
	DWORD hour;
	DWORD min;
	DWORD sec;
}DDHHMMSS;


enum RESLT
{
	// Add new error then update 3mbsWrapperDataTypes.h
	// Non-errors.
	OK = 0,
	OK_EOFREACHED = 1,
	OK_FILESNOTMATCH = 2,

	// Errors.
	ALREADYRUNNING_ERROR = 3,
	MEMALLOC_ERROR = 4,
	MEM_ALREADY_ALLOC_ERROR = 5,
	FILEFORMAT_ERROR = 6, 	// Error with the file... as in not formatted properly.
	FILENAME_ERROR = 7,
	OPENFILEREAD_ERROR = 8,
	OPENFILEWRITE_ERROR = 9,
	OPENTEXTOUTPUTFILE_ERROR = 10,
	CREATEBINARYOUTPUT_ERROR = 11,
	OPEN_ESME_EXCHANGE_ERROR = 12,

	SETFILEPOINTER_ERROR = 13,

	FILEREAD_ERROR = 14,	// can be either file read error or buffer read error.
	FILEWRITE_ERROR = 15,
	WRONGFILETYPE_ERROR = 16,
	WRONGENVDATATYPE_ERROR = 17,
	INVALIDHANDLE_ERROR = 18,
	USERMODELLINELENGTHEXCEEDED_ERROR = 19,
	UNRECOGNIZED_SPECIES_MATRIX_PARAM_ERROR = 20,

	// Setup Errors
	NOSPECIESLOADED_ERROR = 21,
	NOANIMATPOPULATION_ERROR = 22,
	UNPOPULATEDSPECIES_ERROR = 23,
	POPLIMITEXCEEDED_ERROR = 24,

	BUFFERLENGTH_INADEQUATE_ERROR = 25,

	INVALID_GENERAL_INDEX_ERROR = 26,
	INVALID_SPECIES_INDEX_ERROR = 27,
	INVALID_POD_INDEX_ERROR = 28,
	INVALID_INDIVIDUAL_INDEX_ERROR = 29,
	INVALID_ANIMAT_INDEX_ERROR = 30,

	PARAM_HAD_NULLREF_ERROR = 31,
	PARAM_INVALID_ERROR = 32,

	UNPOPULATED_POD_ERROR = 33,
	FILE_NOT_OPEN_ERROR = 34,
	INVALID_ITERATION_INDEX = 35,

	// Directory Errors
	INVALID_DIRECTORY = 36,
	SET_DIRECTORY_ERROR = 37,
	DELETE_FILE_ERROR = 38,
	DIRECTORY_NOT_EMPTY_TO_DELETE = 39,
	DELETE_DIRECTORY_ERROR = 40,

	OBSOLETE_SPECIES_VERSION = 41,
	OBSOLETE_3MBS_VERSION = 42,

	FILEMANAGER_SPECIESLIST_UNSET = 43,

	SCEPARAMS_NOT_MATCH_FILEMANAGER_PARAMS = 44,
	FILE_NOT_OPEN = 45, // File isn't open.


	// Open/Save file dialog procedures.
	FILE_EXTENSION_TOO_LARGE = 46,
	FILE_FILTER_TOO_LARGE = 47,
	FILE_PATH_TOO_LONG = 48,

	FILESIZE_TOO_LARGE = 49,
	MAX_NUM_PLAYBACK_STATES_EXCEEDED = 50,

	INVALID_SEEEDING_DEPTH = 51,
	INVALID_SPECIES_SEEEDING_DEPTH = 52,

	COORDINATE_OFF_MAP = 53,
	NULL_POINTER_RETUNRED_ERROR = 54,

};

// Move this later.
#define MAX_NUM_PLAYBACK_STATES 2520000

typedef TCHAR NAMES[SZ_BEHAVIOR_LEN];



//--------------------------------------------------------------------------------------//
// Binary Ouput Configuration Bit Settings
//---------------------------------------//
// 00000000 00000000 00000000 00000000
#define OUTCONFIG_BATHYMAP					0x00000001 // 00000000 00000000 00000000 00000001 (bit 0)
#define OUTCONFIG_SALINITYMAP				0x00000002 // 00000000 00000000 00000000 00000010 (bit 1)
#define OUTCONFIG_TEMPERATUREMAP			0x00000004 // 00000000 00000000 00000000 00000100 (bit 2)
#define OUTCONFIG_POSTRUNANALYSIS			0x00000008 // 00000000 00000000 00000000 00001000 (bit 3)
#define OUTCONFIG_SPECSINFANIMATASSION		0x00000010 // 00000000 00000000 00000000 00010000 (bit 4)
	// Animat States Begin
#define OUTCONFIG_ANIMATID					0x00000020 // 00000000 00000000 00000000 00100000 (bit 5)
#define OUTCONFIG_TIMEOFDAY					0x00000040 // 00000000 00000000 00000000 01000000 (bit 6)
#define OUTCONFIG_COORDINATE				0x00000080 // 00000000 00000000 00000000 10000000 (bit 7)
#define OUTCONFIG_DEPTH						0x00000100 // 00000000 00000000 00000001 00000000 (bit 8)
#define OUTCONFIG_BEARING					0x00000200 // 00000000 00000000 00000010 00000000 (bit 9)
#define OUTCONFIG_DIVERATE					0x00000400 // 00000000 00000000 00000100 00000000 (bit 10)
#define OUTCONFIG_TRAVELRATE				0x00000800 // 00000000 00000000 00001000 00000000 (bit 11)
#define OUTCONFIG_AECUMULATIVE				0x00001000 // 00000000 00000000 00010000 00000000 (bit 12)
#define OUTCONFIG_aeMoment					0x00002000 // 00000000 00000000 00100000 00000000 (bit 13)
#define OUTCONFIG_AERELATIVEANGLE			0x00004000 // 00000000 00000000 01000000 00000000 (bit 14)
#define OUTCONFIG_AETIMEAVERTING			0x00008000 // 00000000 00000000 10000000 00000000 (bit 15)
#define OUTCONFIG_BATHYDEPTH				0x00010000 // 00000000 00000001 00000000 00000000 (bit 16)
#define OUTCONFIG_SALINITY					0x00020000 // 00000000 00000010 00000000 00000000 (bit 17)
#define OUTCONFIG_TEMPERATURE				0x00040000 // 00000000 00000100 00000000 00000000 (bit 18)
#define OUTCONFIG_TARGETDEPTH				0x00080000 // 00000000 00001000 00000000 00000000 (bit 19)
#define OUTCONFIG_PACKEDDATA				0x00100000 // 00000000 00010000 00000000 00000000 (bit 20)
#define OUTCONFIG_CALCULATEDDEPTH			0x00200000 // 00000000 00100000 00000000 00000000 (bit 21)
#define OUTCONFIG_AECOORDINATE				0x00400000 // 00000000 01000000 00000000 00000000 (bit 22)
#define OUTCONFIG_ENABLED					0x00800000 // 00000000 10000000 00000000 00000000 (bit 23)
#define OUTCONFIG_XYDISTANCE				0x01000000 // 00000001 00000000 00000000 00000000 (bit 24)
#define OUTCONFIG_RISK						0x02000000 // 00000010 00000000 00000000 00000000 (bit 25)
#define OUTCONFIG_x26						0x04000000 // 00000100 00000000 00000000 00000000 (bit 26)
#define OUTCONFIG_x27						0x08000000 // 00001000 00000000 00000000 00000000 (bit 27)
#define OUTCONFIG_x28						0x10000000 // 00010000 00000000 00000000 00000000 (bit 28)
#define OUTCONFIG_x29						0x20000000 // 00100000 00000000 00000000 00000000 (bit 29)
#define OUTCONFIG_x30						0x40000000 // 01000000 00000000 00000000 00000000 (bit 30)
#define OUTCONFIG_OUTPUTBYTIME				0x80000000 // 10000000 00000000 00000000 00000000 (bit 31)
//--------------------------------------------------------------------------------------*/

// Scenario Boolean Parameters (16 bits)
#define SCEBOOL_ENABLEESMETRACKING			0x00000001 // 00000000 00000000 00000000 00000001 (bit 1)
#define SCEBOOL_DURATIONLESS				0x00000002 // 00000000 00000000 00000000 00000010 (bit 2)
#define SCEBOOL_ACOUSTICANIMATACTIVE		0x00000004 // 00000000 00000000 00000000 00000100 (bit 3)
#define SCEBOOL_DISTSTANCECALCMETHOD		0x00000008 // 00000000 00000000 00000000 00001000 (bit 4)
#define SCEBOOL_USECURRENTTICK				0x00000010 // 00000000 00000000 00000000 00010000 (bit 5)
#define SCEBOOL_INDEPENDENT_ANIMAT_RND_GEN	0x00000020 // 00000000 00000000 00000000 00100000 (bit 6)
#define SCEBOOL_xBIT06						0x00000040 // 00000000 00000000 00000000 01000000 (bit 7)
#define SCEBOOL_xBIT07						0x00000080 // 00000000 00000000 00000000 10000000 (bit 8)
#define SCEBOOL_xBIT08						0x00000100 // 00000000 00000000 00000001 00000000 (bit 9)
#define SCEBOOL_xBIT09						0x00000200 // 00000000 00000000 00000010 00000000 (bit 10)
#define SCEBOOL_xBIT10						0x00000400 // 00000000 00000000 00000100 00000000 (bit 11)
#define SCEBOOL_xBIT11						0x00000800 // 00000000 00000000 00001000 00000000 (bit 12)
#define SCEBOOL_xBIT12						0x00001000 // 00000000 00000000 00010000 00000000 (bit 13)
#define SCEBOOL_xBIT13						0x00002000 // 00000000 00000000 00100000 00000000 (bit 14)
#define SCEBOOL_xBIT14						0x00004000 // 00000000 00000000 01000000 00000000 (bit 15)
#define SCEBOOL_xBIT15						0x00008000 // 00000000 00000000 10000000 00000000 (bit 16)

enum SUBMODEL_VECTOR_STATUS
{
	VALID,

	// BehaviorTransition Model
	BEHAVIOR_UNDEFINED, // BehaviorTransition Matrix Model not defined.
	BEHAVIOR_BADFORMAT,
	INITBEHAVIOR_BEHAVIOR_MISMATCH, // Number of behaviors mismatch
	TERMBEHAVIOR_UNDEFINED, // Terminate BehaviorTransition Vector Model not defined.
	TERMBEHAVIOR_BEHAVIOR_MISMATCH, // Number of behaviors mismatch

	BEHAVIOR_MDL, // General BehaviorTransition Matrix Model error used by other models.

	// Rate Models
	VECTOR_LENGTH_MISMATCH, // Number of behaviors mismatch
	VECTOR_UNDEFINED,
	TERMINATE_UNDEFINED,
	TERMINATE_MISMATCH,

	UNDEFINED,
	BADFORMAT,
};


enum EXTRACTORACTIVITY
{
	EXTRACTOR_IDLE_PRERUN, // no actiivty
	EXTRACTOR_INIT, // initializing
	EXTRACTOR_EXTRACTING, // extracting binary data from file to text file.
	EXTRACTOR_IDLE_POSTRUN, // no actiivty
};

typedef struct EnvironmentalMinMax
{
	double xMin; // Lat
	double xMax; // Lat
	double yMin; // Lon
	double yMax; // Lon
	double zMin; // Depth
	double zMax; // Depth
	double v1Min;
	double v1Max;
	double v2Min;
	double v2Max;
}ENVMINMAX;

typedef struct BathyExtremes
{
	double xMin;
	double xMax;
	double yMin;
	double yMax;
	double depthMin;
	double depthMax;
	double slopeMin;
	double slopeMax;
	double slopeHeadingMin;
	double slopeHeadingMax;
}BATHYEXTREMES;


// File Extractor
typedef struct _FileExtractorState
{
	EXTRACTORACTIVITY activity;
	// Both
	DWORD currentAnimat; // this is the current animat... not the current animat index.

	// Dynamically changing by animat

	// Dynamic, extract by iteration
	DWORD currentIteration;
	DWORD lastAnimat;

	// Potentially changes.
	RESLT status;
}FESTATE;
// internal
typedef struct FileExtractorSetup
{
	// Remain Static during an extraction.
	_fSCENARIOPARAMS sceParams;
	BINARYOUTPUT bin;
	BOOL specificAnimatSet;
	DWORD specificAnimat; // specific animat index;
	DWORD rangeGroup; // as in 1st 500, 2nd 500, 3rd 500.  If 0, not set.  Otherwise it is.
	BOOL dumpFileDirectly; // a future implementation option, but code mostly written already to handle this.
}FESETUP;



//--------------------------------- Scenario Modeling ----------------------------------//

enum LEVLBTYPE{ THRESHOLD, DOSE,};

typedef struct
{
	DWORDLONG scenarioParams; // 8 bytes
	DWORDLONG bathyMap;
	DWORDLONG salinityMap;
	DWORDLONG temperatureMap;
	DWORDLONG postAnalysis;
	DWORDLONG speciesDesc; // species description
	DWORDLONG animatAssoc; // animat association to the species description
	DWORDLONG animatState; // single animat state
	DWORDLONG aeState; // species description
	BYTE _unused[8];
}BINOUT_INF;

typedef struct DiskUsage
{
	// not maintained continuously... generated upon request and at run time only.
	//BINOUT_FILEPOINTER fp;  // file pointer
	//BINOUT_SIZE store;// storage bytes
	DWORDLONG totalDiskSpace; // total stored bytes to hard drive
	DWORDLONG totalAnimatStateBytes; // total bytes taken by all animat states
	DWORDLONG totalAEStateBytes;
}DISKUSAGE;


#define ANIMAT_RUNTIME_SPECIFCN_PODMEMSHIP		0x000001 // 00000000 00000001
#define ANIMAT_RUNTIME_SPECIFCN_PODLEADERTYPE	0x000002 // 00000000 00000010


// Individual animat associations to species index, pod index into associated species,
// animat's index into associated pod in associated species, and compacted information
// made up of animat membership (pod or individual), and pod leader type (animat or
// calculated centroid.
typedef struct AnimatAssociation
{
	// This information saved to file.

	UINT16 speciesNumber; // animat's associated species index
	UINT16 pod_id; // animat's pod index into its associated species
	UINT16 id; // animat's id in its associated pod index in its associated species index.

/* bit/Boolean values are made up of the following:
     bit 0: Pod Membership.
              0 if animat is in a pod
	   		  1 if animat is an individual
     bit 1: Pod Leader Type (see enumeration PODLEADERTYPE)
              0 if pod leader is a focal animat
			  1 if pod leader is a calculated centroid.*/
	UINT16 compactInf;
	BYTE _RESERVED1[SIZE_8];
	ACOUSTICSRCEINF acstcSrc; // 16 bytes
	BYTE _RESERVED2[SIZE_16];

#pragma message("TODO: Add focal distance to this struct")

}ANIMATASSCN;

typedef struct AnimatAssociationCompactInformation
{
	// Not saved to file.
	BOOL individual; // true = individual, false = pod
	PODLEADERTYPE podLeaderType; // 0 = focal animat, 1 = calculated centroid
}ANIMATASSCN_COMPACTINF;

//--------------------------------------------------------------------------------------//
typedef struct PackagedStateDataStruct
{
	int behavior;
	BOOL threshFlagBBeh;
	BOOL threshFlagBPhy;
	BOOL threshFlagAPhy;
	ACTIVITY diveActivity;
	int overrideBehavior;
	BOOL offScreenAnimatReset; // reset (make new animat) if animat goes off screen.
	int nextBehavior; // added 1/26/10
	BOOL nextBehaviorSelected; // added 1/26/10
} PACKAGED_STATE_DATA;

typedef struct AcousticSourceState
{
	float lat;
	float lon;
	BOOL active; // 1 byte
}ACST_SRC_STATE;


typedef struct AnimatStateFileOuput
{	
	// See also struct ANIMATSTATE.
	// This struct exists so that information from struct ANIMATSTATE can be converted 
	// into formats that are outputted to file.  For example, converting doubles to
	// floats.  Doubles are used for calculating values but floats are used for
	// outputting to file.

	// This stuct is not writtent to file.  Instead the user configures output
	// so individual componets from this struct are output to file.  Struct
	// ANIMATSTATE_FILEOUTPUT_CONFIG holds the Booleans that indicate which
	// states are to be outputted.
	UINT32 animatID;
	UINT32 clock;
	float lat;
	float lon;
	float depth;
	float bearing;
	float diveRate;
	float travelRate;
	float aeCmltve; // This become cumulative.
	float aeMoment;
	float aeRelAngle; // change this to acoustic source bearing or something
	UINT32 aeTimeAvrt;			// aversion cycle (~ 1/s) tally
	float bathyDepth;
	float salinity;
	float temperature;
	DWORD packedData;
	float targetDepth;
	float calcDepth; // 72 + 56 = 128


	float xDistance; //
	float yDistance;

	float risk;
}ANIMATSTATE_FILEOUT;

#endif