#ifndef DATA_TYPES_SCENARIOSETUP_H
#define DATA_TYPES_SCENARIOSETUP_H
#include "dataTypes_General.h"

// Initialization of instances of this struct occur in Scenaro.cpp in the iniitalizer for CScenario.
typedef struct ScenarioParamsSpeciesGroup
{
	TCHAR szGroup[SZGROUPTNAMEBUFFERLENGTH];

	double lvlAphys; // Level A Physiological threshold
	double lvlBphys; // Level B Physiological threshold
	double lvlBBeh_RiskA; // Risk transition sharpness parameter
	// The following have meaning but not implemented.
	double deactThreshB;

	UINT8 unitsPhys; // this is an enumeration.  see enum UNITS.  1 Bytes
	UINT8 unitsBeh;  // this is an enumeration.  see enum UNITS.  1 Bytes
	UINT8 decayfncB; // this is an enumeration.  see enum DECAYFUNCTION.  1 Bytes
}SCEPARMSSPECIESGROUP;

typedef struct AnimatStateFileOutputConfiguration
{
	// This gets moved to Class CFileManager

	BOOL ID; // always saved if anything is saved // 14
	BOOL timeOfDay; // always saved if anything is saved // 15
	BOOL coordinate; // always saved if anything is saved // 16
	BOOL depth; // always saved if anything is saved // 17
	BOOL bearing; //18
	BOOL diveRate; // 19
	BOOL travelRate; // 20
	BOOL aeCmltve;  // 21
	BOOL aeMoment; // 22
	BOOL aeRelAngle; // 23
	BOOL aeTimeAvrt; //24
	BOOL bathyDepth; // 25
	BOOL salinity; // 26
	BOOL temperature; // 27
	//--------------------------------------------------------//
	// Packed data
	BOOL packedData; // always saved if anything is saved. // 28
	//BOOL behavior;
	//BOOL threshFlagBBeh;
	//BOOL threshFlagBPhy;
	//BOOL threshFlagAPhy;
	//BOOL diveActivity;
	//BOOL overridingBehavior;
	//BOOL offScreenAnimatReset; // include flag when animat goes off screen.
	//--------------------------------------------------------//
	BOOL targetDepth; // 29
	BOOL calcDepth; // Saved only if everything is saved.//30
	BOOL xyDistance; // 41

	BOOL risk; // 42
}ANIMATSTATE_FILEOUTPUT_CONFIG;

typedef struct StateFileOuputConfiguration
{	// change name to HEADERINF
	// This gets moved to Class CFileManager
	BOOL bathyMap; // 9
	BOOL salinityMap; //10
	BOOL temperatureMap; // 11
	BOOL postRunAnalysis; //12
	BOOL speInfAndAnimatAsscn; // 13
}STATE_FILEOUTPUT_CONFIG;


typedef struct OutputConfig
{
	BOOL enabled;  //8 // FALSE means no binary data is outputted.  Must be FALSE for scenarios w/o specified duration.
	STATE_FILEOUTPUT_CONFIG headerInf; // 9 - 13 // various header parameter options 
	ANIMATSTATE_FILEOUTPUT_CONFIG animat; // 14-30 // various animat parameter options  
	BOOL AECoordinate; // 31// output the acoustic coordinate (also referred to as acoustic source state)
	BOOL outputByTime; // 32 // Time-based layout if TRUE, animat-based layout if FALSE
}BINARYOUTPUT;

typedef struct Seeding
{
	BOOL useCurrentTick; // 39 // If TRUE, seeds random-based functions with the current system tick.
	DWORD value; // 40 // If 'useCurrentTick' is FALSE, seeds random-based functions with the value stored.
}SEEDING;

enum DISTCALC
{
	PLANAR_GEOMETRY,
	LAT_LON,
};


typedef struct UserParams
{
	BINARYOUTPUT output; // 8-32, 41-42.  Flags that correspond to animat states and other
	// items that are to be saved to file (and therefore output buffer) during scenario
	// execution

	BOOL durationless;	// 36.  Flag for no specified duration as happends when 3mb is
	// being run by an outside application such as ESME.  If durationless, no binary
	// output is allowed or even possible because the binary output buffer in certain
	// configurations depend on knowing the number of iterations to run for properly
	// placing animats states in the output file.

	//-----------------//
	// This goes away
	//------------------
	BOOL acousticAnimatActive; // 37.  A flag that indicates if the acoustic source is
							   // active in this scenario , may not need this anymore

	DISTCALC distCalcMethod; //38
	SEEDING seed; //39 - 40

	// SCENARIO SETUP ONLY (not saved to file)
	TCHAR szOutputDir[SIZE_256];     //(A)
	TCHAR szScenarioTitle[SIZE_128];	// (B)// title of this scenario.
	BOOL maintainFirstAnimatState;  // (C), tells 3mb to hold the first animat's state for
								    // the entire run.
}USERPARAMS;

typedef struct ScenarioParameters
{

	// See also _fSCENARIOPARAMS, the version of SCENARIOPARAMS outputted to file.

	// Not directly accessed (and therefore settable by) user.
	DWORD libVerSuper; // 1
	DWORD libVerSub; // 2

	DWORD speciesVerSuper; // alpha 1
	DWORD speciesVerSub; // alpha 2

	DWORD outputVerSuper; // beta 1
	DWORD outputVerSub; // beta 2

	DWORD numSpecies; // 3 // Includes the acoustic source types since acoustic sources are updated with the species.
	DWORD totalNumAnimats; // 4 /Includes the acoustic sources present.

	DWORD numAcstSrcTypes; // 43 Number of acoustic source types.  Needed mainly for the binary output file
	DWORD totalNumAcstcSrcs; // 44 Number of acoustic source populants.  Needed mainly for the binary output file

	DWORD duration; // 5 // put here because there are limits on how long a scenario may run.
	DWORD numSaveIterations; // 6
	DWORD startTime;	// 7 // put here instead of USERPARAMS because must be set withing 24 hour format.

	UINT8 speciesGroupCnt; //33
	SCEPARMSSPECIESGROUP speciesGroup[NUM_SPEGROUPS_INUSE]; //34

	BOOL enableAcstSrcTracking; // 35

	// Not yet numbered
	BOOL acousticPingCycleOutputLimit;   // (D), a flag that tells the scenario to limit output
								    // state data to acoustic ping cycles

	//------------------------------------//
	// Added 7/20/2010 per NUWC requrest
	//------------------------------------//
	OUTPTINTRVLLMT intrvlOutptLim;
	//BOOL intrvlOutputLimitEnabled;
	//DWORD intrvlOutputLimitStart;
	//DWORD intrvlOutputLimitValue;
	//------------------------------------//


	USERPARAMS user; //8-32, 36-42, + // SCENARIO SETUP ONLY (A - B)
	// Set by user.
}SCENARIOPARAMS; // working version of scenario params

// Provided for feedback to calling applications
// remove the ___ from each when previous enumerations are removed.
enum SCEACTIVITY
{

	__RUN_FINISHED, // no actiivty at the end of a run.
	___ALLOCOUTPUTBUFF, // allocating memory for animat output to file buffer

	___SCE_INIT, // initializing scenaro
	___SCE_INITANIMATS, // looping through each animat to initialize them
	___SCE_RUNITERATING, // Iterating
	___SCE_RUNBUFFERFLUSH, // flushing output buffer to file
	___SCE_PAUSED, // running, but paused waiting on calling application input
};

enum BUILDTYPE
{
	MBRELEASE,
	MBDEBUG,
};


typedef struct MbBuildInformation
{
	BUILDTYPE buildType;
	int bitSize;
	char szBuildDate[SIZE_128];
	char szBuildTime[SIZE_128];
}BUILDINF;


#endif