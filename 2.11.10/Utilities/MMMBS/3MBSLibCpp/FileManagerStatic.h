#ifndef FILEMANAGERSTATIC_H
#define FILEMANAGERSTATIC_H
#include "Mutex.h"
#include "ListManager.h"
#include "dataTypes.h"
#include "dataTypes_statsAnalysis.h"
#include "OutputManager.h"
#include "Species.h"
#include "EnvironmentData.h"
#include "Bathymetry.h"
#include "ScenarioStatic.h"

typedef struct EnvironmentalDataStruct
{
	CBathymetry bathymetry;
	CEnvironmentData salinity;
	CEnvironmentData temperature;
}ENVDATA;

typedef struct FileManagerMemoryAllocInf
{
	DWORDLONG memoryAvailablePhysical; // Available physical system memory, value is set
		// by call to GlobalMemoryStatusEx()
	DWORDLONG memoryMaximumAllocPhysical; // Maximum amount of memory mbs would attempt
		// to allocate for a buffer.  Value is based on available physical memory scaled
		// down by value held by preprocessor definition 'AVAIL_PHYSICAL_MEM_SCALE_FACTOR'
	size_t numBytes; // Actual amount 3mb will attempt to allocate for a buffer.  Value is
		// based on value stored in 'memoryMaximumAllocPhysical' but adjusted to fit state
		// length.
	DWORD bufferIterationCapacity; // number of iterations (in terms of states per animat) a buffer of size 'numByte' will hold.

	size_t animatStateSize; // size, in bytes, of each animat state saved to file as a result of how the scenario was set up.
	size_t acousticSrcStateSize; // size, in bytes, of each acoustic state saved to file
}FM_MEMALLOCINF;


// For access to calling application... not used by this class otherwise.
typedef struct FileManagerBufferState
{
	size_t numBytes; // Actual size in bytes allocated for the state buffer.
	DWORD bufferCycleCount; // the number of times the buffer has been flushed
	DWORD bufferIterationLevel; // states iterations in terms of per animat count currently stored in the buffer.
	DWORD fileIterationRWCount; // iterations written to file so far
	size_t animatStateSize; // size, in bytes, of each animat state saved to file as a result of how the scenario was set up.
	size_t acousticSrcStateSize; // size, in bytes, of each acoustic state saved to file a a result of how the scenario was set up
	DWORD currentAnimatFlush;
	DWORD bufferIterationCapacity; // the number of iterations the buffer can hold
}FM_BUFFERSTATE;

typedef struct AcousticSourceStateFile
{
	float lat;
	float lon;
	UINT8 active; // 1 byte
	BYTE _reserved[3]; //3 bytes
} _ACST_SRC_STATE_FILE;


enum PLAYENUM
{
	STOP, // no playback or display.  The initial state.
	PLAY, // Animats displayed and animated
	PAUSE, // Animats diaplayed but frozen.
};

typedef struct ScenarioOutputFileInf
{
	// Stuff normally held in class CScenario.
	SCENARIOPARAMS sce;
	ENVDATA envData;
	SPECIESBINOUTINF *pSpeInf;
	NAMES **speBehaviorNames;

	// Individual animat associations to species index, pod index into associated species,
	// animat's index into associated pod in associated species, and compacted information
	// made up of animat membership (pod or individual), and pod leader type (animat or
	// calculated centroid.
	ANIMATASSCN *animatAssociations;

	//-----------//
	// State Data
	//-----------//

	// Acoustic source state for current iteration being played back.
	ACST_SRC_STATE acousticSourceState;

	// Animat state buffer for current iteration being played back
	ANIMATSTATE_FILEOUT *animatState;

}SCENARIOUTPUTFILEINF;

// Not accessed to outside calling applications.  An internal state. 
typedef struct _FileManagerBufferState
{
	BYTE *a; // pointer to allocated memory
	size_t bytes; // size of buffer in bytes

	// The number of times the buffer has been flushed during a scenario run or
	// refilled during a playback.
	DWORD bufferCycleCount;

	// The number of complete iterations (entire animat population state for every
	// iteration) the buffer can hold.
	DWORD bufferIterationCapacity; 

	// The number of complete iterations (entire animat population state for every
	// teration) the buffer currently holds.
	DWORD bufferIterationLevel; 

	// The number of complete iterations (entire animat population state for every
	// teration) written to file over the entire simulation or read from file over
	// the current playback.
	DWORD fileIterationRWCount;

	HANDLE hdl;
	BINARYSETUP binSetup;
	DWORD currentAnimatFlush;
	RESLT status;
}_FM_RUNSTATE;


typedef struct CalculateStateLocationInBufferStruct
{
	DWORDLONG bufferIterationLen; // length of the buffer in iterations (not bytes)
	DWORDLONG iterationWriteNum; // entire iteration writes (all animat states at a single second) to buffer so far.

	DWORDLONG numAnimats; // total number of animats present
	DWORDLONG animatIndex;  // index of animat in question.
	
	size_t animatStateSize; // size of individual animat state in bytes
	size_t acstSrcStateSize; // size of inidivual acoustic source state size in bytes
	DWORDLONG numSaveIterations;

	BOOL outputByTime; // output format: True means output by time, FALSE means output by animat
}CALCLOCINBUFF;



class CFileManagerStatic
{
public:

	CFileManagerStatic();
	virtual ~CFileManagerStatic();
private:
	C3mbStaticsLib m_staticLib;
public:
	FM_MEMALLOCINF GetMemoryAllocationDetails(
						int NumAnimats,
						int NumSavedIteriiuhyuyhgations,
						BINARYOUTPUT BinOut,
						DWORDLONG OtherAllocBytes = 0 // bytes anticipated to be dynamically allocated elsewhere that
						);							  // should be removed from consideration when allocated the
													  // output buffer.

	BINARYSETUP DetermineBinarySetup(SCENARIOPARAMS Sce, ENVDATA *pEnvData, CListManager <CSpecies> *pSpeciesList);
	_fSCENARIOPARAMS ConvertScenarioFormat(const SCENARIOPARAMS Sce, ENVDATA *pEnvData,  CListManager <CSpecies> *pSpeciesList);
	size_t CalculateConfigurationBytesPerAnimatState(BINARYOUTPUT BinOut);
	size_t CalculateConfigurationBytesPerAcousticState(BINARYOUTPUT BinOut);
	BOOL AssertBinaryOutFilePointers(HANDLE Hdl);
	__int64 CalculateAnimatStateBufferLocation(CALCLOCINBUFF CLB/*int AnimatIndex, DWORD TotalAnimats, BOOL OutputByTime*/);
	__int64 CalculateAcousticStateBufferLocation(CALCLOCINBUFF CLB/*DWORD TotalAnimats, BOOL OutputByTime*/);
	SCENARIOPARAMS ConvertScenarioFormat(const _fSCENARIOPARAMS FSce);
	BINARYOUTPUT TranslateBinFileOutConfiguration(DWORD Config);
	int BuffIndex(int AnimatNum, int NumAnimats, int StateNum, int TotalStates);


private:
};

#endif