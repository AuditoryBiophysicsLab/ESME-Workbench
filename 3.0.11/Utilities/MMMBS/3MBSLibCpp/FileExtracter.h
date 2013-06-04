#pragma once
#include "Mutex.h"
#include "ListManager.h"
#include "dataTypes.h"
#include "dataTypes_statsAnalysis.h"
#include "dataTypes_ScenarioToFile.h"
#include "dataTypes_ScenarioSetup.h"
#include "OutputManager.h"
#include "Species.h"
#include "EnvironmentData.h"
#include "IRunnable.h"
#include "ScenarioStatic.h"

#include "FileManagerStatic.h"

#define  MAXANIMATFILESOPEN 250

//typedef NAMES* BEHAVIOR_NAMES;

// this doesn't belong in class CScenario no longer extracts data
typedef struct ExtractionSubStateDynamic
{
	// Continuously Updated
	// One at a time
	DWORD currentAnimat;
	DWORD lastAnimat;
	DWORD currentIteration;
	DWORD lastIteration;
}EXTHUBSTATE; // fields independed of data extraction and scenario execution (each needs it's own).

// this doesn't belong in class CScenario no longer extracts data
typedef struct ExtractionSubStateX
{
	BOOL outputByIteration;
	DWORD numAnimats;
	DWORD numIterations;
	DWORD saveIterationCnt;
}EXTHUBSTATEX; // fields independed of data extraction and scenario execution (each needs it's own).


typedef struct ScenarioDataExtractionSpeBinOutInf
{
	SPECIESBINOUTINF inf;
	NAMES *behName; // an array with a size equal to the number of behaviors found in this species.
}DATAEXTCT_SPE;

typedef struct findanameforthis
{
	DATAEXTCT_SPE *spe; // an array with a size equal to the number of species in the scenario
	ANIMATASSCN *ansm; // an array with a size equal to the number of animats in the scenario.
}DATEXTCT_SCE; // FIND A better name;


typedef struct AcousticExposureThresholdBookKeeping
{
	DWORD	exposureCntBBeh;
	BOOL	inExposureBBeh;

	DWORD	exposureCntB;
	BOOL	inExposureB;

	DWORD	exposureCntA;
	BOOL	inExposureA;
}THRESHOLDBOOKKEEPING;

typedef struct ReadCount
{
	DWORD   remainingToReadCurrentAnimat;
	DWORD	numToRead;
}READCOUNT;


// For internal access

class CFileExtracter : public IRunnable
{
public:
	CFileExtracter(void);
	~CFileExtracter(void);
private:
	C3mbStaticsLib m_staticLib;
public:

	// File Extraction
	RESLT ExtractBinaryResultsForPlayback(TCHAR *szFileName);
	RESLT ExtractBinaryResultsForPlayback(OPENFILENAME *ofn);

	RESLT ExtractBinaryResultsIntoTextFiles(TCHAR *szFileName,
											BOOL OutputToSubdirectory,
											BOOL SplitTxtFiles,
											int NumIterationsPerFile);
	RESLT ExtractBinaryResultsIntoTextFiles(OPENFILENAME *ofn,
											BOOL OutputToSubdirectory,
											BOOL SplitTxtFiles,
											int NumIterationsPerFile);

	FESETUP GetSetup();
	FESTATE GetState();
	void SetSpecificAnimatEnabled(BOOL EnableVal);
	void SetSpecificAnimatIndex(DWORD AnimatIndex);
	void SetSpecificAnimatGroup(DWORD AnimatGroup); // must be greater or equal to 1 as in 1st 500, 2nd 500...
	void Abort();

private:
	CAnimatStatics m_classAnimatStatics;
	CStaticScenario m_staticScenario;
	CEnvironmentalDataStatic m_staticEnvData;
	CFileManagerStatic m_fileManagerStatic;


	RESLT OpenFile();
	RESLT Uninitialize(RESLT Res);
	void CloseThread1ExtractProcess(RESLT Res);
	void _ExtractBinaryResultsIntoTextFiles();
	RESLT ExtractByAnimat(/*DATEXTCT_SCE *SpeSumm*/);
	RESLT ExtractByIteration(/*DATEXTCT_SCE *SpeSumm*/);
	RESLT ExtractIntoSingleFile(/*DATEXTCT_SCE *SpeSumm*/);
	RESLT ExtractIntoDisplayBuffer();

	// File extraction support functions
	__int64 BufferToAnimatState(ANIMATSTATE_FILEOUTPUT_CONFIG *Config, ANIMATSTATE_FILEOUT *AS, __int64 BytePos, BYTE *Buffer);
	RESLT ReadPostRunStatsResults();
	RESLT PrintPostRunStatsResults();
	RESLT SetUpSpeciesSummaryAssociation();
	BOOL PrintAnimatTextFileIteration(FILE *Fd, DWORD AnimatNumber, ANIMATSTATE_FILEOUT *pASFO, PACKAGED_STATE_DATA *pPSD, THRESHOLDBOOKKEEPING *pThrshBookeeping, DATEXTCT_SCE *SpeSumm, BOOL SpeciesInfAvail/*, BOOL FlagA, BOOL FlagB, BOOL FlagBbeh*/);
	TCHAR *BuildAnimatTextFileTitle(TCHAR *szAnimatFileTitle, UINT32 BuffSize, FESETUP *Setup, DATEXTCT_SCE *Sce, DWORD AnimatNum, DWORD OffScreenCount, DWORD SplitFileCount, DWORD SplitFileEnabled, const TCHAR *szSceFileTitle);
	TCHAR *BuildAnimatTextFileName(TCHAR *szAnimatFileName, UINT32 BuffSize, const TCHAR *szAnimatFileTitle, const TCHAR *szOutputFilePath);
	RESLT InitializeNewTextOutFile(const TCHAR *szFileName, FILE **Fd, STATE_FILEOUTPUT_CONFIG StateOutputConfg, _fSCENARIOPARAMS SceParms, DATEXTCT_SCE ScenarioSummary, DWORD AnimatNum);
	void RunThread1();

	HANDLE m_hdl;
	BOOL m_abort;
	CirThread m_runThread;

	TCHAR m_szFileName[BUFFERED_MAX_PATH];
	TCHAR m_szFileTitle[BUFFERED_MAX_PATH];
	TCHAR m_szFilePath[BUFFERED_MAX_PATH]; // The path the extrated files are saved to associated with a run.
	TCHAR m_szFileTrakFileSubPath[BUFFERED_MAX_PATH]; // The created sub folder associated with a run that holds all of the animat track files.
	BOOL m_splitTextOutput;	 // Specifies splitting text output file into multiple file (TRUE) or one (FALSE) (change name to splitTextFiles to match ESME script).
	int	m_iterationsPerFile; // itererations per file if splitting the text output into mulitple files is enabled.
	BOOL m_displayMode;
	FESETUP m_setup;
	FESTATE m_state;
	DATEXTCT_SCE m_speSum;

	TAKESTATS *m_speciesStats;
	TAKE m_statAnalysis;

};
