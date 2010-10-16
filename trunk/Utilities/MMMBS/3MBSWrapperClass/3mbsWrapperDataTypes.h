#ifndef _MBSDATATYPES
#define _MBSDATATYPES

#ifndef UINT
#define UINT unsigned int
#endif

using namespace System;

namespace mbs
{
	public enum class mbsDISTCALC
	{
		PLANAR_GEOMETRY,
		LAT_LON,
	};

	public value struct mbsCONFIG
	{
		bool maintainFirstAnimatState; // (C), tells 3mb to hold the first animat's state for the entire run.

		bool durationLess; //36
		bool seedWithCurrentTick; // Random number seeding. TRUE:seeded w/current tick count. FALSE: seeded w/seedValue (Change name to currentTickSeeding to match ESME script.)
		mbsDISTCALC distCalcMethod;
		//bool outputText;			 // Post-simulation: TRUE: output text.  FALSE:don't output text.
		//bool splitTextOutput;	 // Specifies splitting text output file into multiple file (TRUE) or one (FALSE) (change name to splitTextFiles to match ESME script).
		//int	iterationsPerFile;	 // If 'splitTextOutput' is TRUE, specifies num iterations per file to split into. (change name to iterationsPerSplitTextFile to match esme script).
		UINT seedValue;			 // Random number seeding. If 'autoDetermine' is FALSE, uses 'seedValue' to seed with. (change name to randomSeedValue to match ESME script).

		BOOL outputByTime;	// Options are to output by time or output by animat.


		//*********************//
		// Output Configuration
		//*********************//
		bool enabled; //8 // Any output at all?  Overrides all following ouput configuration params.

		// Include in binary output file
		bool bathyMap;
		bool salinityMap;
		bool temperatureMap;
		bool postRunAnalysis;
		bool speInfAndAnimatAsscn;

		// Animat state output 
		//bool ID; // always saved if anything is saved, so commented out here.
		//bool timeOfDay; // always saved if anything is saved, so commented out here.
		//bool coordinate; // always saved if anything is saved, so commented out here.
		//bool depth; // always saved if anything is saved, so commented out here.
		bool bearing; //*
		bool diveRate; //*
		bool travelRate; //*
		bool aeCmltve;  //*
		bool aeMoment; //*
		bool aeRelAngle; //*
		bool aeTimeAvrt; //*
		bool bathyDepth; //*
		bool salinity; //*
		bool temperature; //*
		bool packedData; // always saved if anything is saved, so commented out here.
		bool targetDepth; //*
		bool calcDepth; // Saved only if everything is saved.
		bool xyDistance; // 41
		bool risk; //42

		// Acoustic Source (coordinate) state output
		bool AECoordinate;
	};

	public value struct mbsHHMMSS
	{
		UINT hour;
		UINT min;
		UINT sec;
	};


	public value struct mbsPosition
	{
		double latitude;
		double longitude;
		double depth;
	};

	public value struct mbsANIMAT_STATE
	{	
		// Starting version 1.3
		unsigned int behavior;
		int nextBehavior;
		BOOL nextBehaviorSelected;
		double lat;
		double lon;
		double depth;
		double bearing;
		double diveRate;
		double travelRate;
		double aeCmltve;
		double aeMoment;
		double aeRelAngle; // change this to acoustic source bearing or something
		int	  aeTimeAvrt;			// aversion cycle (~ 1/s) tally
		double bathyDepth;
		double salinity;
		double temperature;
		DWORD packedData;
		double targetDepth;
		double calcDepth;
		double xDistance;
		double yDistance;

	};

	public value struct mbsBATHYVALUE
	{
		double depth;
		double slope;
		double slopeHeading;
	};


	public value struct mbsBATHYEXTREMES
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
	};

	public value struct mbsSCENARIO_PARAMS
	{
		unsigned int libVerSuper;
		unsigned int libVerSub;
		DWORD	numSpecies;
		int		totalNumAnimats;
		int		numberOfIterations;
		int		startTime;
		//int		numberOfStates;
	};

	public enum class mbsPODLEADERTYPE
	{
		ANIMAT,
		CENTROID,
	};

	public enum class mbsRUNSTATE
	{
		FINISHED,			// Initial state.
		INITIALIZING,
		RUNNING,
		RUNPAUSED,	
		DATAEXTRACTING,
		UNKNOWN,
	};

	public enum class mbsBUILDTYPE
	{
		MBRELEASE,
		MBDEBUG,
	};

	public value struct mbsBUILDINF
	{
		mbsBUILDTYPE buildType;
		int bitSize;
		String ^szBuildDate;
		String ^szBuildTime;
	};

	public enum class mbsSCEACTIVITY
	{
		RUN_FINISHED, // no actiivty
		ALLOCOUTPUTBUFF, // allocating memory for animat output to file buffer

		SCE_INIT, // initializing scenaro
		SCE_INITANIMATS, // looping through each animat to initialize them
		SCE_RUNITERATING, // Iterating
		SCE_RUNBUFFERFLUSH, // flushing output buffer to file
		SCE_PAUSED, // running, but paused waiting on calling application input
	};

	public value struct mbsFM_BUFFERSTATE
	{
		size_t numBytes; // Actual size in bytes allocated for the state buffer.
		DWORD bufferCycleCount; // the number of times the buffer has been flushed
		DWORD bufferIterationLevel; // states iterations in terms of per animat count currently stored in the buffer.
		DWORD fileIterationRWCount; // iterations written to file so far
		size_t animatStateSize; // size, in bytes, of each animat state saved to file as a result of how the scenario was set up.
		size_t acousticSrcStateSize; // size, in bytes, of each acoustic state saved to file a a result of how the scenario was set up
		DWORD currentAnimatFlush;
		DWORD bufferIterationCapacity; // the number of iterations the buffer can hold
	};

	public enum class mbsRESULT
	{
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

		INVALID_GENERAL_INDEX_ERROR = 26, // new
		INVALID_SPECIES_INDEX_ERROR = 27,
		INVALID_POD_INDEX_ERROR = 28,
		INVALID_INDIVIDUAL_INDEX_ERROR = 29,
		INVALID_ANIMAT_INDEX_ERROR = 30,

		PARAM_HAD_NULLREF_ERROR = 31,
		PARAM_INVALID_ERROR = 32,

		UNPOPULATED_POD_ERROR = 33,
		FILE_NOT_OPEN_ERROR = 34,
		INVALID_ITERATION_INDEX = 35,

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

		FILE_EXTENSION_TOO_LARGE = 46,
		FILE_FILTER_TOO_LARGE = 47,
		FILE_PATH_TOO_LONG = 48,

		FILESIZE_TOO_LARGE = 49,
		MAX_NUM_PLAYBACK_STATES_EXCEEDED = 50,

		INVALID_SEEEDING_DEPTH = 51,
		INVALID_SPECIES_SEEEDING_DEPTH = 52,

		COORDINATE_OFF_MAP = 53,


	};

	public value struct mbsACST_SRC_STATE
	{
		float lat;
		float lon;
		BOOL active;
	};

	public value struct mbsSCESTATE
	{
		// Static when scenario is running
		mbsSCEACTIVITY activity;

		// Dynamic changing or set during scenario execution
		DWORDLONG runClock; 
		DWORD currentAnimat;
		DWORD currentIteration;// animat state number underway

		DWORD runNumber;
		mbsRESULT errorStatus;
		mbsACST_SRC_STATE acousticSrc;

		mbsFM_BUFFERSTATE bufferState;
	};


	//-------------------------------------------------------------------------------------//
	// Version 2.0 species modeling
	//-------------------------------------------------------------------------------------//
	public enum class mbsACOUSTIC_POD_RESPONSE
	{
		REMAINS_INTACT_goesAway,
		BREAKS_APART_goesAway,
	};

	public enum class mbsDIRECTIONAL_MODEL_TYPE
	{
		RANDOM_WALK = 0,
		VECTOR_MODEL_DIRECTIONAL_NO_BIASING = 1,
		VECTOR_MODEL_DIRECTIONAL_WITH_VECTOR_MODEL_BIASING = 2, 
		CORRELATED_RANDOM_WALK = 3,
		CORRELATED_RANDOM_WALK_WITH_DIR_BIASING = 4,
	};


	public enum class mbsSTANDARD_MODEL_TYPE
	{
		GAUSSIAN, // Gaussian distribution
		UNIFORM,  // Uniform random distribution
		VECTOR,	  // Vector model
	};


	public value struct mbsGAUSS
	{
		double mean;
		double std;
		double coeff; // not necissarily used by all models
	};

	public value struct mbsRANDOM
	{
		double max;
		double min;
		double coeff;
	};


	public value struct mbsELEMENT
	{
		int rowCnt;
		int colCnt;
		double a; // elements in the array 
	}; // 16 bytes

	public value struct mbsARRAY
	{
		int rowCnt;
		int colCnt;
		array <mbsELEMENT> ^a;
		//double *a; // elements in the array 
	}; // 16 bytes

	public value struct mbsMATRIX
	{
		int rowCnt;
		int colCnt;
		array <mbsARRAY> ^a;
		//double **a; // elements in the matrix 
	}; // 16 bytes

	public value struct mbsVECTORMODEL
	{
		mbsARRAY vector;
		mbsELEMENT step;
	};

//------------------//
// Behavior Modeling
//------------------//
#if 0
	public value struct mbsBEHAVIOR_MODEL_GETSREPLACEd /* Retire?) */
	{
		// USER VECTOR MODELS.
		mbsMATRIX behavior;
		mbsMATRIX initial;
		mbsARRAY  terminate;
	}mbsBehaviorModel; // 96 bytes
#endif

//----------------//
// Direction Model
//----------------//
	public value struct mbsRANDOMWALK
	{
		double termCoeff;
	};

	public value struct mbsCORRANDWALK
	{
		double perturbation;
		double termCoeff;
	};

	public value struct mbsCORRANDWALKDB
	{
		double perturbation;
		double directionOfBias;
		double bias;
		double arcStep;
		double termCoeff;
	};

	public value struct mbsDIRVECTORMODEL
	{
		mbsMATRIX direction;
		mbsMATRIX directionalBias;
		mbsARRAY  terminate;
	};

	//------------------//
	// Reversal Modeling
	//------------------//
	public value struct mbsREV_INT_RND
	{
		int min;
		int max;
	};

	public value struct mbsREVERSAL_RND
	{
		double probOfReversal;
		mbsREV_INT_RND count;
		mbsGAUSS time;
	};

	public value struct mbsREVERSAL_GAUSS
	{
		double probOfReversal;
		mbsGAUSS count;
		mbsGAUSS time;
	};


//--------------------------------------------------------------------------------------//
//                    version 2.0
//--------------------------------------------------------------------------------------//
	//--------------//
	// Rate Modeling
	//--------------//
	public value struct mbsRATEVECTORMODEL
	{
		mbsARRAY vector;
		mbsELEMENT step;
		mbsELEMENT terminate;
	};

	public value struct mbsRATEMODEL
	{
		mbsSTANDARD_MODEL_TYPE modelType; // 4 bytes
		mbsRANDOM rnd;
		mbsGAUSS gauss;
		mbsRATEVECTORMODEL vm;
	}; // used in rate of travel and acent/decent rates. 112 + 48 = 160

	public value struct mbsDEPTH_MODEL
	{
		mbsSTANDARD_MODEL_TYPE modelType;
		BOOL bottomFollows;
		mbsRANDOM rnd;
		mbsGAUSS gauss;
		mbsVECTORMODEL vm; // correct
	}; // Used in rate of travel and acent/decent rates.

	//-----------------//
	// Surface Interval
	//-----------------//
	public value struct mbsSURFINTRVLMODEL
	{
		mbsSTANDARD_MODEL_TYPE modelType;
		mbsGAUSS gauss;
		mbsVECTORMODEL vm; // correct
	};


	public value struct mbsDIRVECTRMODEL
	{
		mbsARRAY direction;
		mbsMATRIX directionalBias;
		mbsELEMENT  terminate;
	};

	public value struct mbsDIRCTNMDL
	{
		mbsRANDOMWALK rndWalk;
		mbsCORRANDWALK crRndWalk;
		mbsCORRANDWALKDB crRndWalkDb;
		mbsDIRVECTRMODEL vm;
		mbsDIRECTIONAL_MODEL_TYPE modelType;
	};

	//-------------------//
	// Reversals Modeling
	//-------------------//
	public value struct mbsREVERSlVCTRMDL
	{
		mbsELEMENT  probOfReversal;
		mbsARRAY count;
		mbsARRAY time;
		mbsELEMENT timeStep;
	};


	public enum class mbsREVERSAL_DIVE_RATE_TYPE
	{
		NO_INDEPENDENT = 0,
		INDEPENDENT = 1, // a single rate for diving specific for reversal
		INDEPENDENT_DIVE_AND_ASCENT = 2, // a dive rate and ascent rate specific for reverasl.
	};

	public value struct mbsREVERSAL_MODEL
	{
		mbsREVERSAL_RND rnd;
		mbsREVERSAL_GAUSS gauss;
		mbsREVERSlVCTRMDL vm;

		//BOOL hasaIndependentDiveRate;
		mbsREVERSAL_DIVE_RATE_TYPE diveRateType;
		mbsGAUSS diveRate;
		mbsGAUSS ascentRate;

		BOOL reverses; // no=0, yes=1 move to species model.
		mbsSTANDARD_MODEL_TYPE modelType;
	};

	public enum class mbsDECAYFUNCTION
	{
		FNCa,
		FNCb,
	};

	public enum class mbsUNITS
	{
		unitsPhys,
		unitsBeh,
	};

	public enum class mbsBehTermFormula
	{
		T50_K,
		GAUSSIAN,
	};

	//-----------------------------//
	// Behavior Transition Modeling
	//-----------------------------//
	public value struct mbsBEHTRANSMDL
	{
		mbsARRAY behavior;
		mbsELEMENT terminate;
		double meanTimeInBeh; //T50
		double slopeCoefficient; // k
	}; 

	public value struct mbsDEPTH_ENV_ATTRACTOR_MDL
	{
		BOOL shelfIsEnabled;
		double shelfDepth;
		double shelfSlope;

		BOOL basinIsEnabled;
		double basinDepth;
		double basinSlope;

		BOOL slopeIsEnabled;
		double slopeDepth;
		double slopeSlope;
	};

	public value struct mbsENVATTRACTORMDL
	{
		BOOL maxIsEnabled;
		BOOL minIsEnabled;
		BOOL deltaIsEnabled;
		double max;
		double min;
		double delta;
	};

	//---------------------------//
	// Acoustic Exposure Modeling
	//---------------------------//
	// Need to set initial values.
	public value struct mbsACSTCAVRSNMDL
	{
//		double actThreshA;
//		double actThreshB;
//		double deactThreshA;
//		double deactThreshB;
//		mbsUNITS unitsPhys;
//		mbsUNITS unitsBeh;
//		mbsDECAYFUNCTION decayfncA;
//		mbsDECAYFUNCTION decayfncB;

		BOOL travelDirectionAffected;
		BOOL travelRateAffected;

		BOOL depthAffected;
		BOOL surfaceIntervalAffected;
		BOOL ascentRateAffected;
		BOOL descentRateAffected;
		BOOL reversalAffected;
		BOOL flatBottomDiveAffected; // Always active to prevent flat bottom diving.

		BOOL podBreaksUp;
		BOOL beaches;
		double beachingDepth;

		mbsGAUSS travelRate;
		mbsCORRANDWALKDB travel;

		mbsREVERSAL_GAUSS reversal;
		mbsGAUSS surfaceInterval;
		mbsGAUSS depth;
		mbsGAUSS descentRate;
		mbsGAUSS ascentRate;
		BOOL flatBottomDives;
	};

	//-----------------------------------------//
	// Added 7-7-09 for species version 5.0
	public enum class mbsBTTMFLLW_MDL_TYPE
	{
		mbsNO_BTTMFLLWNG = 0, // Bottom following follows normal behavior travel rate
		mbsBOTTOMFOLLOWS_CURRENT_VELOCITY = 1,
		mbsBOTTOMFOLLOWS_GAUSSIAN_VELOCITY, // Gaussian distribution
		mbsBOTTOMFOLLOWS_UNIFORM_VELOCITY,  // Uniform random distribution
	};

	public value struct mbsBTTMFLLW
	{
		mbsBTTMFLLW_MDL_TYPE bttmFollowType;
		mbsRANDOM rnd;
		mbsGAUSS gauss;
	};
	//-----------------------------------------//



	public value struct mbsDIVEMDL
	{
		//-----------------------------------------//
		// Changed 7-7-09 for species version 5.0
		//BOOL bottomFollows; // this goes away.
		mbsBTTMFLLW bttmFollow; // this replaces it.
		//-----------------------------------------//

		mbsRATEMODEL descentRate;
		mbsRATEMODEL ascentRate;
		mbsSURFINTRVLMODEL srfInv;
		mbsREVERSAL_MODEL reversal;
		mbsDEPTH_MODEL depth;
	};

	public value struct mbsDEPTHSPAN
	{
		double shallow;
		double deep;
	};

	public value struct mbsBEHTRAN // added in version 6.0 3MB version 6.0 species builder;
	{
		mbsDEPTHSPAN depthSpan;
		mbsMATRIX m;
	};


	public value struct mbsNRMLBEHMDL
	{
		String ^szName;

		// Behavior Transitions
		BOOL depthHasPriorityOverTemp;
		int nrmlBehTransCnt;
		mbsBehTermFormula behTransTermFormula;

		//mbsMATRIX nrmlBehTransMatrix;// normal over time.
		//mbsBEHTRANSMDL nrmlBehTrans; // normal over time.
		array<mbsBEHTRAN> ^nrmlBehTrans;
		mbsBEHTRANSMDL depthEnvAttBehTrans; // while in depth environmental attractor
		mbsBEHTRANSMDL tempEnvAttBehTrans; // while in temperature environmental attractor

		// Environmental Attractors
		mbsDEPTH_ENV_ATTRACTOR_MDL depthEnvAtt;
		mbsENVATTRACTORMDL tempEnvAtt;

		// Travel
		mbsRATEMODEL travelRate;
		mbsDIRCTNMDL travelDirection;

		// Dive;
		mbsDIVEMDL dive;
	};

	//-----------------//
	// Species Modeling
	//-----------------//
	public ref struct mbsSPECIESMODEL
	{
		////////////////////
		// Newly Added
		unsigned int mbsLibVerSuper;
		unsigned int mbsLibVerSub;
		unsigned int speVerSuper;
		unsigned int speVerSub;
		String ^szSpeciesShrtDscrptn ;
		String ^szSpeciesLongComment;
		int speciesGroup;

		int speciesName;
		int year;
		int month;
		int day;
		int hour;
		int min;

		int sec;
		int id;
		//////////////////////////


		//mbsMATRIX initialBehavior;
		int initBehSpanCnt;
		array<mbsBEHTRAN> ^initialBehavior;
		mbsACSTCAVRSNMDL acousticAversion;
		double shoreFollowDepth;
		double minSeedDepth;
		double deepWaterSeedingDepth;
		BOOL deepWaterSeedingDepthEnabled;
		int behaviorCount;
		array<mbsNRMLBEHMDL> ^behavior;
	};


	public value struct mbsCNTBIN
	{
		int trans; // behavior transitioned to.
		int sec; // time in behavior before transitioning.
	};

	public value struct mbsCNTBINARRAY
	{
        double start; // in hours
        double end; // in hours
		array<mbsCNTBIN> ^trialArray;
	};


	public value struct mbsSNGLBEHTRANSTRUCT
	{
		array<mbsCNTBINARRAY> ^timePeriodArray;
	};


	public value struct mbsTestStruct
	{
		int er;
		double de;
	};

	public value struct mbsTEST
	{
		array<mbsTestStruct> ^cow;
	};

}
#endif _MBSDATATYPES
