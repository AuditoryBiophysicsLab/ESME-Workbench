#ifndef DATA_TYPES_SCENARIOTOFILE_H
#define DATA_TYPES_SCENARIOTOFILE_H
#include "dataTypes_General.h"
#include "dataTypes_SpeciesGroup.h"

typedef struct FileScenarioParamsSpeciesGroup
{
	TCHAR szGroup[SZGROUPTNAMEBUFFERLENGTH];

	float lvlAphys; // Level A Physiological threshold
	float lvlBphys; // Level B Physiological threshold
	float lvlBBeh_RiskA; // Risk transition sharpness parameter
	// The following have meaning but not implemented.
	float deactThreshB;

	UINT8 unitsPhys; // this is an enumeration.  see enum UNITS.  1 Bytes
	UINT8 unitsBeh;  // this is an enumeration.  see enum UNITS.  1 Bytes
	UINT8 decayfncB; // this is an enumeration.  see enum DECAYFUNCTION.  1 Bytes
	UINT8 _unusedUINT8;
	BYTE _reserved[SIZE_28];
}_fSCEPARMSSPECIESGROUP;


typedef struct
{
	DWORDLONG scenarioParams; // 4 bytes
	DWORDLONG bathyMap;
	DWORDLONG salinityMap;
	DWORDLONG temperatureMap;
	DWORDLONG postAnalysis;
	DWORDLONG speciesDesc; // species description
	DWORDLONG animatAssoc; // animat association to the species description
	DWORDLONG animatState; // single animat state
	DWORDLONG aeState; // species description (a DWORDLONG because is toward end of binary out file).
	BYTE _unused[8];
}BINOUT_FILEPOINTER;

typedef struct
{
	size_t scenarioParams; // 
	size_t bathyMap;
	size_t salinityMap;
	size_t temperatureMap;

	size_t postAnalysis;
	size_t speciesDesc; // species description
	size_t animatAssoc; // animat association to the species description
	size_t animatState; // bytes per animat state

	size_t aeState; // bytes per acoustics source state
	size_t _unused[3];
}BINOUT_SIZE;

typedef struct BinarySetup
{
	// not maintained continuously... generated upon request and at run time only.
	BINOUT_FILEPOINTER fp;  // file pointer
	BINOUT_SIZE store;// storage bytes
	DWORDLONG totalDiskSpace; // total stored bytes to hard drive
	DWORDLONG totalAnimatStateBytes; // total bytes taken by all animat states
	DWORDLONG totalAEStateBytes;
	BYTE _unused[8];
}BINARYSETUP; // chage this name.


typedef struct FileScenarioParameters
{
	// Save to the binary output file.
	TCHAR fileIdentifier[SIZE_16]; // “3MBBinaryOutput”
	DWORD libVerSuper; // 1
	DWORD libVerSub; // 2

	DWORD speciesVerSuper; // alpha 1
	DWORD speciesVerSub; // alpha 2

	DWORD outputVerSuper; // beta 1
	DWORD outputVerSub; // beta 2

	DWORD numSpecies; // 3
	DWORD totalNumAnimats; // 4

	DWORD duration; // 5 // The duration of the run.
	DWORD saveIterationCnt;// 6 // The saved subset of iteration states.
	DWORD startTime;// 7;
	DWORD binOutStateItemConfig; // 8~32 // goes into USERPARAMS user 

	//-----------------------------------------------------------------------
	DWORD sizeof_size_t_dataType;// So Matlab to knows the size of a size_t,
	//4 Byte Subregion
		UINT8 numSceParamsSpecGroupStored; // 8 bits
		UINT8 numSceParamsSpecGroupUsed; // Item 33,  8 bits
		UINT16 sceSetupBoolParams; //(Items 35~39, 41, 42), 16 bits
	DWORD seedValue; // 40
	BOOL acstSrceLimitOutput;
	//-----------------------------------------------------------------------

	DWORD _fNumAcstSrcTypes; // 43 Number of acoustic source types.
	DWORD _fTotalNumAcstcSrcs; // 44 Number of acoustic source populants.


	BOOL _fIntrvlOutputLimitEnabled; // Interval limiting-output enbabled, Added 7/20/2010 per NUWC requrest
	DWORD _fIntrvlOutputLimitStart; // First iteration outputted, Added 7/20/2010 per NUWC requrest
	//-----------------------------------------------------------------------
	DWORD _fIntrvlOutputLimitValue; // Interval of output, Added 7/20/2010 per NUWC requrest

	BYTE _reserved1[SIZE_28]; //OUTPUT FILE ONLY 

	BINARYSETUP diskInf; //OUTPUT FILE ONLY should be called fileInf, or storageInf
	_fSCEPARMSSPECIESGROUP group[NUM_SPEGROUPS_ALLOC]; // 34 // 10*64 bytes = 640 bytes

}_fSCENARIOPARAMS; // binary output version of scenario parameters
#endif