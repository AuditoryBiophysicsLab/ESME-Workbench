// Scenario.h: interface for the CScenario class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_SCENARIO_H__6CB0F29F_B295_4E16_9739_07589E780F32__INCLUDED_)
#define AFX_SCENARIO_H__6CB0F29F_B295_4E16_9739_07589E780F32__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "datatypes.h"	// Added by ClassView
#include "datatypes_statsAnalysis.h"	// Added by ClassView
#include "Species.h"
#include "LinkedList.h"
#include "IRunnable.h"
#include "Mutex.h"
//#include "OutputManager.h"
#include "FileManager.h"
#include "Bathymetry.h"
#include "ScenarioStatic.h"

typedef struct GUI_INTERFACE
{
	HWND hWndParent;
	HWND hWndMessageLB; // window to a message list box.
}GUI_INTERFACE;



enum RUNSTATE
{
	// This is maintained to support the original interface with ESME.
	// Only state where mbs isn't running
	FINISHED,	// Initial state.
	// Mbs is running
	INITIALIZING,
	RUNNING,
	RUNPAUSED,
	DATAEXTRACTING,
	UNKNOWN,
};



typedef struct _ScenarioState
{
	// Static when scenario is running
	SCEACTIVITY activity;

	// Dynamic changing or set during scenario execution
	DWORDLONG runClock;
	DWORD currentAnimat;
	DWORD currentIteration; // animat state number underway

	// One at a time
	//DWORD bufferIterationLength;
	DWORD runNumber; //x
	RESLT errorStatus;
	ACST_SRC_STATE acousticSrc;
}_SCESTATE;

typedef struct ScenarioState
{
	// Static when scenario is running
	SCEACTIVITY activity;

	// Dynamic changing or set during scenario execution
	DWORDLONG runClock; 
	DWORD currentAnimat;
	DWORD currentIteration;// animat state number underway

	DWORD runNumber;
	RESLT errorStatus;
	ACST_SRC_STATE acousticSrc;

	FM_BUFFERSTATE bufferState;
}SCESTATE;


// A value for filling in statistical data.
#define MAXITERATIONSREAD 10*60*60


/*--------------------------------------------------------------------------------------//
OUTPUTBUFFERSIZE
Used as the return from a call to member function DetermineBufferSize() and is mean soley
to make returning values clean as opposed to having to pass in pointers to the information
returned by this call.  The function DetermineBufferSize() makes a function call to
system function GlobalMemoryStatusEx() to determine how much physical memory is available.
Based upon the amount of physical memory available plus the passed in parameters:
	number of animats,
	the number of iterations to be saved,
	size of animat states,
	and size of acoustic exposure states (0 if not being used in the scenario)
	DetermineBufferSize() determines the number of bytes to allocate 

size_t numBytes: The number of bytes to allocate.
				 The data type size_t was chosen because this is the type function
				 malloc() takes for input.  Since size_t is defined as an unsigned int
				 there potential for the number of bytes it can hold (and therefore the
				 maximum value size) to change among platforms.

DWORD numIterations:  The number of iterations to be saved to file containing each
					  animat's state plus the acoustic exposure state if it is also being
					  saved the buffer can hold based upon the size of the buffer.  Data
					  type DWORD chosen because the number of iterations will always be
					  greater than or equal to 0 and the number of iterations cannot
					  consevably ever be more than the value a 32 bit integer can hold
					  (over 49 thousand days worth...).
//--------------------------------------------------------------------------------------*/
typedef struct OutputBufferSize
{
	MEMORYSTATUSEX memuse;
	DWORD memscaleFactor;
	size_t numBytes;
	DWORD numIterations;
}OUTPUTBUFFERSIZE; // see about getting rid of this.

typedef struct SpeciesAnimatStateBufferPointer
{
	ANIMATSTATE *animatState; // A pointer to this species' animat population location
}SPECIESANIMATSTATEBUFFER;


typedef struct OffscreenTakenAnimatCounts
{
	int lvlAPhysTakesTotal;
	int lvlBPhysTakesTotal;
	double lvlBBehTakesTotal; // level b behavioral (risk function)
	DWORD offScreenCount;
	int numInitialAnimats;
}ANIMATRESETTAKES;

typedef struct SpeciesResetTakes
{
	SPECIESGROUP speGrp; // Ths species group this species belongs to
	//ANIMATSTATE *animatState; // A pointer to this species' animat population location
		// in the animat state buffer.  No memory allocated for this.
	ANIMATRESETTAKES animat; // Incremented each time an animat of this species
		// goes off the map if it was taken (verify this operates correctly.)
}SPERESETTAKES;

typedef struct ScenarioResetTakes
{
	SPERESETTAKES *speBuff; // Dynamically allocated.  Maintains counts of animats
		// that went off the map/screen for each species.
	ANIMATRESETTAKES speGroup[NUM_SPEGROUPS_ALLOC]; // incremented each time
		// an animat of this group goes off the map/screen if it was taken
	ANIMATRESETTAKES animat; // Entire animat population.  Incremented each time a taken animat goes
		// off the map if it was taken
	UINT16 numSpeciesGroupsUsed;
}SCERESETTAKES;

typedef struct ProgressMonitoring
{
	DWORD startTick;
	DWORD pcntDone;
	DWORD nextUpdate;
	DWORD lastTick;
	DWORD nextWindowUpdate;

	DWORD currentTick;
	double fSecsRem;
	double totalWritten; // all animats, all iterations
	int totalStates;
}PRGRSSMONITORING;

typedef struct AnimatFileNaming	
{
	TCHAR	fileName[BUFFERED_MAX_PATH];
	TCHAR	fileTitle[SIZE_256]; // this should be changed to BUFFERED_MAX_PATH
}ANIMATFILENAMING;

//--------------------------------------------------------------------------------------//
// For commenting out the bodies of this class's methods for debugging.
//#define NOCSENARIOFUNCTIONS
//--------------------------------------------------------------------------------------//

class CScenario: public IRunnable
{
public:
	//-----------------------//
	// Constructor/Destructor
	//-----------------------//
	CScenario();
	virtual ~CScenario();


private: // member variables

	CFileManagerStatic m_fileManagerStatic;

	C3mbStaticsLib m_staticLib;
	CStaticScenario m_staticScenario;

#ifndef NOCSENARIOFUNCTIONS
	CListManager <CSpecies> m_speciesList; // the population of animats by species

	CFileManager m_fileMgr; // file manager handles scenario execution file IO

	CMutex m_mbsActiveMutex; // a mutex for thread safety.
	CMutex m_mbsPausedMutex; // a mutex for thread safety.
	CirThread m_runThread; 	// A thread class for all threads run by class CScenario.
	ENVDATA m_envData; // a struct holding bathymetry, salinity, and temperature classes.


	TAKE m_SceTakesCopy;
	TAKESTATS *m_SpeTakesCopy;
	SCENARIOPARAMS m_mbSce; // MMMB scenario configuration..
	_SCESTATE m_state; // scenario state
	ANIMATSTATE *m_firstAnimatStateHoldArray; // holds first animat's states for duration of
	DWORD m_animatStateHoldArrayLength;
		// entire run when scenario is configured to do so.
	BOOL m_abort; // Flag to abort current run and/or data extraction
	BOOL m_exit;	// Flag to abort current run and/or data extraction

	int	m_throttleIterations; // Input from a routine that calls 3mb also makes a function
		// call to set this value.  m_throttleIterations is the number of iterations to
		// set through before pausing and permits the calling application to retrieve
		// current animat state (and other information) as needed.  The scenario continues
		// to execute as long as m_throttleIterations hold a value less than or greater
		// than zero and decrements each iteration.  If zero, stops until calling routine
		// sceResets it.
	//----------------------------------------------------------------------//
#endif//_REMOVEME2
	BOOL AcousticSourcePings(DWORD CurrentIteration, ACOUSTICSRCEINF AcstcSrc);
	BOOL Throttle(int ThrottleIteration, int CurrentIteration, int ScenarioDuration, BOOL Durationless);

public: // Member Functions
	// Sound Source Functions
	BOOL SoundSourceSpeciesPresent();
	int GetSoundSourceTypeCount(); // returns the number of sound source types present.
	int GetTotalSoundSourceCount(); // returns the number of sound sources animats
	int GetNthSoundSourceModelCount(int Nth); // IS ZERO basaed... returns the number of instances
	BOOL SpeciesIsASoundSourceModel(int Nth); // IS ZERO basaed... returns true if species is a sound source type.
	// of a sound source for the Nth sound source model loaded in

	RESLT ExtractBinaryDataToText(TCHAR *szFileName);

	CWorkingList *GetWorkingList(int StartTime, int Duration);



	ANIMATSTATE RetrieveFirstAnimatStateAtIndex(DWORD Index);
	RUNSTATE GetRunState_old();
	void ResetRunCount();
	DWORD GetRunCount();
	RESLT ScenarioToText(TCHAR *FileName);
	RESLT ScenarioToText(FILE *fd);
	RESLT SpeciesToText(int SpeciesIndex, TCHAR *FileName);
	RESLT SpeciesToText(int SpeciesIndex, FILE *fd);

	RESLT CompareFileOutput(HANDLE Hd1, HANDLE Hd2);
	RESLT CompareFileOutput(TCHAR *FileName1, TCHAR *FileName2);
	//void SetAnimatAcousticExposure(double SourceLat, double SourceLon, double *Array); // for esme to call.
	void SetAnimatBathymetry(double *Array);
	BOOL IsActive();
	SCESTATE GetState(); // Retreives the calling process/thread's version of the Scenario State

	void EnableEsmeAcousticExposureSetTracking();

	COORD_DEPTH *GetAnimatPopulationCurrentCoordinates(COORD_DEPTH *CoordBuffer);
	INHABITINF *GetAnimatPopulationCurrentCoordinates(INHABITINF *CoordBuffer);
	INHABITINF *GetAnimatPopulationInitialCoordinates(INHABITINF *IC = NULL);
	INHABITINF *GetAnimatCurrentCoordinates(int SpeciesIndex, int BufferLength, INHABITINF *CoordBuffer = NULL);
	INHABITINF *GetAnimatInitialCoordinates(int SpeciesIndex, int BufferLength, INHABITINF *IC = NULL);
	INHABITINF GetPodMemberCoordinate(int SpeciesIndex, int PodIndex, int PodMemberIndex);
	INHABITINF *GetPodMemberCoordinates(int SpeciesIndex, int PodIndex, INHABITINF *CoordBuffer=NULL);
	INHABITINF GetIndividualCoordinate(int SpeciesIndex, int IndividualIndex);
	INHABITINF *GetIndividualCoordinates(int SpeciesIndex, INHABITINF *CoordBuffer=NULL);
	INHABITINF	GetPodMemberInitialCoordinate(int SpeciesIndex, int PodIndex, int PodMemberIndex);
	INHABITINF *GetPodInitialCoordinates(int SpeciesIndex, int PodIndex, INHABITINF *IC=NULL); /*x*/
	INHABITINF	GetIndividualInitialCoordinate(int SpeciesIndex, int IndividualIndex);
	INHABITINF *GetIndividualInitialCoordinates(int SpeciesIndex, INHABITINF *IC=NULL); /*x*/


	//--------//
	// Adds
	//--------//
	RESLT ResetAcousticSourceInf();
	RESLT AddSpecies(TCHAR *FileName, int *Index = NULL);
	RESLT AddSpecies(OPENFILENAME *ofn, int *Index = NULL);
	RESLT AddPod(int SpeciesIndex, PODLEADERTYPE LeaderType=ANIMAT, double FocalDistance=0, int BufferLength=0, INHABITINF *InitialCond=NULL);
	RESLT AddPodMember(int SpeciesIndex, int PodIndex, INHABITINF InitialCond);
	RESLT AddPodMembers(int SpeciesIndex, int PodIndex, INHABITINF *InitialCond, int AnimatCount);
	RESLT AddIndividual(int SpeciesIndex, INHABITINF InitialCond);
	RESLT AddIndividuals(int SpeciesIndex, INHABITINF *InitialCond, int AnimatCount);

	//----------//
	// Deletions
	//----------//
	void DeleteSpecies(); // Deletes all species from scenario.
	void DeleteSpecies(int Index); // Deletes specific species from scenario.
	void DeletePods(int SpeciesIndex); // Deletes all pods from specific species
	void DeletePod(int SpeciesIndex, int PodIndex); // Deletes specific pod from specific species.
	void DeletePodMember(int SpeciesIndex, int PodIndex, int AnimatIndex); // Deletes specific animat from specific pod in specific species.
	void DeleteIndividuals(int SpeciesIndex); // Deletes all individuals from specific species.
	void DeleteIndividual(int SpeciesIndex, int IndividualIndex);// Deletes specific individual from specific species.

	//-------------//
	// Counts
	//-------------//
	int GetSpeciesCount();
	int GetAnimatCount();// Count of all animats in the scenario.
	int GetAnimatCount(int SpeciesIndex);// Count of all animats in a specific species
	int GetPodCount(); // Count of all pods in the scenario.
	int GetPodCount(int SpeciesIndex);// Count of all pods in a specific species
	int GetPodMemberCount(int SpeciesIndex, int PodIndex);
	int GetIndivdualCount();// Count of all individuals in the scenario.
	int GetIndivdualCount(int SpeciesIndex);// Count of all individuals in a specific species

	//-------//
	// Access
	//-------//
	void SetPodLeaderType(int SpeciesIndex, int PodIndex, PODLEADERTYPE Type);
	PODLEADERTYPE GetPodLeaderType(int SpeciesIndex, int PodIndex);
	void SetPodLeaderFocalDistance(int SpeciesIndex, int PodIndex, double FocalDistance);
	double GetPodLeaderFocalDistance(int SpeciesIndex, int PodIndex);
	COORD_DEPTH GetPodFocalCoordinate(int SpeciesIndex, int PodIndex);
	void GetCopyOfBehaviorNamesModeledInSpecies(int SpeciesIndex, BEHAVIOR_NAME *CopyBuffer, DWORD BufferSize);
	DWORD GetNumberOfBehaviorsModeledInSpecies(int SpeciesIndex);
	DWORD GetNumberOfBehaviorsModeledInScenario();

	// Added
	double GetShoreFollowingDepth(int SpeciesIndex);
	double GetMinimumSeededingDepth(int SpeciesIndex);

	//-------------------//
	// Species Titles
	//-------------------//
	void SetSpeciesDisplayTitle(int Index, TCHAR *Title);
	void GetSpeciesDisplayTitle(int Index, TCHAR *Buffer, DWORD BufferSize);


	//----------------------------------//
	// Simulation Routines and Variables
	//----------------------------------//
	RESLT RunScenario(int NumIterations=-1); // this goes away
	RESLT InitializeRun();
	RESLT StepRun(int NumIterations);
	RESLT FinishRun();
	void ClearScenario();
	void AbortRun();
	void Exit();

	//--------------------------------------------------------------//
	// Scenario Setup And User Interface (GUI or Console)
	//--------------------------------------------------------------//
	void SetScenarioTitle(TCHAR *Title);/**/ //code//
	void SetConfiguration(USERPARAMS Configuration);/**/ //code//
	USERPARAMS GetConfiguration();
	void SetDuration(HHMMSS Duration);/**/ //code//

	// Output-limiting functions.
	int SetAcousticSrceLimitOutput(BOOL Limit); // returns the number of iterations resulting from setting or clearing this.
	BOOL GetAcousticSrceLimitOutput();
	RESLT ReadCSVListFromFile(TCHAR *szFileName);
	BOOL CSVFileLoaded(); // returns FALSE if no CSV file is loaded, TRUE otherwise.
	void ClearCSVList();
	int GetSaveStatesCount();
	void SetItervalLimitOutput(BOOL Enable, DWORD Start, DWORD Interval);
	void GetItervalLimitOutput(BOOL *pEnable, DWORD *pStart, DWORD *pInterval);
	BOOL IntervalLimitOutputEnabled();
	void SetDuration(int Seconds);/**/ //code//
	HHMMSS GetDuration();/**/ //code//
	int GetDurationSeconds();/**/ //code//
	void SetStartTime(HHMMSS StartTime);/**/ //code//
	HHMMSS GetStartTime();/**/ //code//
	void CalculateRequiredDiskSpace(DWORDLONG *BinStorage, DWORDLONG *TextStorage);

	//-------------------//
	// Environmental Data
	//-------------------//
	double SetBaythyConstantDepth(double Depth);
	BATHYVALUE GetBathymetryDepth(double lat, double lon);/**/ //code//
	CBathymetry *GetBathymetryClassRef();
	

	RESLT SeedingCoordinateIsValid(UINT SpeciesIndex, double Lat, double Lon);

	RESLT LoadBathymetryFromTextFile(OPENFILENAME *Ofn);/**/ //code//
	RESLT LoadBathymetryFromTextFile(TCHAR *FileName);/**/ //code//
	void ClearBathymetry();/**/ //code//
	BOOL BathymetryLoaded();/**/ //code//
	void GetBathymetryFileName(TCHAR *FileNameBuffer, int BufferLength);/**/ //code//
	//void BathymetryToTextFile(TCHAR *FileName);/**/ //code//
	BATHYEXTREMES GetBathymetryExtremes();/**/ //code//
	ENVDATAPOINTCOUNT GetBathymetryDataPointCounts();
	double GetTotalBathySufaceAreaMeters();
	double GetTotalBathySufaceAreaMeters(CEnvironmentData *Env);
	double GetBathymetryWaterSufaceAreaMeters();
	double GetBathymetryLandSufaceAreaMeters();
	RESLT LoadTemperatureFromTextFile(OPENFILENAME *Ofn);/**/ //code//
	RESLT LoadTemperatureFromTextFile(TCHAR *FileName);/**/ //code//
	void ClearTemperature();/**/ //code//
	BOOL TemperatureLoaded();/**/ //code//
	void GetTemperatureFileName(TCHAR *FileNameBuffer, int BufferLength);/**/ //code//
	RESLT LoadSalinityFromTextFile(OPENFILENAME *Ofn);/**/ //code//
	RESLT LoadSalinityFromTextFile(TCHAR *FileName);/**/ //code//
	void ClearSalinity();/**/ //code//
	BOOL SalinityLoaded();/**/ //code//
	void GetSalinityFileName(TCHAR *FileNameBuffer, int BufferLength);/**/ //code//


	//------------------------------------------------------------//
	// File Functions, Functions Related to File IO, and Variables
	//------------------------------------------------------------//
	RESLT LoadFromBinFile(TCHAR *FileName);/**/ //code//
	RESLT SaveToBinFile(TCHAR *FileName);/**/ //code//

	//------------------//
	// Accesor Functions
	//------------------//

	//---------------------//
	// Scenario Description
	//---------------------//
	RESLT GenerateTitleAndFileName(ANIMATFILENAMING **BinaryFile, ANIMATFILENAMING **AcstExpTracking);
	RESLT RunParamsToTextFile(FILE *fd);
	RESLT GetErrorStatus();
	RESLT SetOutputDirectory(TCHAR *Directory);

	void SetSpecificExtractionAnimat(DWORD SpecificAnimat);
	USERPARAMS GetCurrentScenarioParams();
	FM_MEMALLOCINF GetMemoryAllocationDetails(USERPARAMS *pUser = NULL);
	void SetAnimatAcousticExposure(double SourceLat, double SourceLon, double *Array);
	BOOL SetSpeciesAtIndexAsSoundSource(int Index);

private: // Member Functions
	BOOL VerifySoundSourceModelsLoadedCount();
	BOOL VerifySoundSourceListPlacement();
	BOOL VerifyTotalSoundSourceCount();
	CSpecies *GetSpeciesAtIndex(int Index);

	int UpdateTotalAnimatCount();
	void SetRunCount(DWORD RunCount);
	int  AsserteSpeciesIndex(int SpeciesIndex); // simply returns the entered index if passes assertion
	void  CommandPromptOutput(TCHAR *sz);/**/ //code//
	RESLT VerifyScenarioRunSetup();

	void RunThread1(); // calls _RunScenario(), and is started by a call to CirThread's member function StartThread1().
	void _RunScenario(); // launched by RunThread1();
	void UpdateStatisticalCalculations(TAKE *SceTakes, TAKESTATS *SpeTakes, SCERESETTAKES *PrevTakes,
		SPECIESANIMATSTATEBUFFER *SpeAnimatBuffer);

	//****************************************************************************************************//
	// These either were not implemented correctly as a thread or need to be redone with the changes made
	void RunThread2();
	void _runUpdateThread();
	//****************************************************************************************************//
public:
	SCENARIOPARAMS GetScenarioParamsCopy(void);
};

#endif // !defined(AFX_SCENARIO_H__6CB0F29F_B295_4E16_9739_07589E780F32__INCLUDED_)
