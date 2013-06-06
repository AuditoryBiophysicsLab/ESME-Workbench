#pragma once
#include "Mutex.h"
#include "ListManager.h"
#include "dataTypes.h"
#include "dataTypes_statsAnalysis.h"
#include "OutputManager.h"
#include "Species.h"
#include "EnvironmentData.h"
#include "Bathymetry.h"
#include "ScenarioStatic.h"

#include "FileManagerStatic.h"

// Demo for how to implement a class thread is from:
// http://www.codeproject.com/threads/ThreadClass.asp
struct IRunnableFMgr
{
	const SCENARIOPARAMS *pSce;
	virtual void RunThread1(const SCENARIOPARAMS *pSce) = 0;
	virtual void RunThread2(const SCENARIOPARAMS *pSce) = 0;
};

class CirFMgrThread
{
public:
	// Constructors
	CirFMgrThread(){ _threadObj = NULL;	}
	CirFMgrThread(IRunnableFMgr *ptr){	_threadObj = ptr;	}

	// Member variables
	BOOL m_thread1Running;
	BOOL m_thread1RunningAck;
	BOOL m_thread2Running; // Added for playback thread
	BOOL m_thread2RunningAck; // Added for playback thread

	// Class methods.
	void SetIRunnablePointer(IRunnableFMgr *ptr) { 	_threadObj = ptr; }
	
	void startThread1(const SCENARIOPARAMS *pSce)
	{
		DWORD threadID;
		pSce = pSce; // quiet compiler warning
		if(_threadObj == NULL)
			return;

		// use the Win32 API here
		::CreateThread(0, 0, threadProc, _threadObj, 0, &threadID);
	}


	// Added for playback thread
	void startThread2(const SCENARIOPARAMS *pSce)
	{
		DWORD threadID;
		pSce = pSce; // quiet compiler warning

		if(_threadObj == NULL)
			return;

		// use the Win32 API here
		::CreateThread(0, 0, thread2Proc, _threadObj, 0, &threadID);
	}


protected:
	// Win32 compatible thread parameter and procedure 
	IRunnableFMgr *_threadObj; 
	
	static unsigned long __stdcall threadProc(void* ptr)
	{
		((IRunnableFMgr*)ptr)->RunThread1(((IRunnableFMgr*)ptr)->pSce);
		return 0;
	}

	// Added for playback thread
	static unsigned long __stdcall thread2Proc(void* ptr)
	{
		((IRunnableFMgr*)ptr)->RunThread2(((IRunnableFMgr*)ptr)->pSce);
		return 0;
	}   
};



class CFileManager : public IRunnableFMgr
{
public:
	CFileManager(void);
	~CFileManager(void);

private:
	CStaticScenario m_staticScenario;
	CFileManagerStatic m_fileManagerStatic;
	C3mbStaticsLib m_staticLib;
	_FM_RUNSTATE m_runState;
	FILE *m_acSrcTrkFile;
	CirFMgrThread m_fmThread; // A thread class for all threads run by this class
	// Commas separated value-related member variables.
	COutputManager m_csvListMgr; // manages the clock times to be outputted to file.
	CWorkingList *m_csvList; // will be NULL if no file of save times were read in.

public:
	CWorkingList *GetWorkingList(int StartTime, int Duration, const OUTPTINTRVLLMT *pOutputLim);
	RESLT LoadScenario(TCHAR *szFileName, SCENARIOPARAMS *pSce, ENVDATA *pEnvData,
		CListManager <CSpecies> *pSpeciesList);
	RESLT SaveScenario(TCHAR *szFileName, const SCENARIOPARAMS *pSce, ENVDATA *pEnvData,
		CListManager <CSpecies> *pSpeciesList);

	// Output list manager functions
	BOOL CSVFileLoaded();
	RESLT ReadCSVListFromFile(TCHAR *szFileName);
	void ClearCSVList();
	int GetStateSaveToFileCount(int StartTime, int Duration, const OUTPTINTRVLLMT *pOutputLim);
	RESLT AcousticDataToFile(DWORDLONG Clock, double SrcLat, double SrcLon, DWORD ArrayLen, double *Array);
	//------------------------------//

	RESLT ResetAcousticSourceInf(ACOUSTICSRCEINF *AcstcSrcInfBuff, int AcstcSrcInfBuffLen);

	// General member functions
	FM_MEMALLOCINF GetMemoryAllocationDetails(SCENARIOPARAMS Sce);
	FM_BUFFERSTATE GetBufferStatus();
	BOOL IsFlushingBuffer();
	RESLT InitializeRun(TCHAR *szFileName,
						TCHAR *szAcstSrcTrackingName,
						SCENARIOPARAMS Sce,
						ENVDATA *pEnvData,
						CListManager <CSpecies> *pSpeciesList,
						BOOL *pAbort);
	RESLT InitializePlayback(TCHAR *szFileName, SCENARIOUTPUTFILEINF *PlaybackState);
	RESLT AllocateFileIOBuffer(SCENARIOPARAMS Sce, BOOL ForceAllIterations);
	RESLT UninitializeRun(RESLT Res);
	RESLT UninitializePlayback(SCENARIOUTPUTFILEINF *PlaybackState, RESLT Res);
	BOOL FlushBuffer(SCENARIOPARAMS *pSce, BOOL *pAbort);
	//BOOL ReadStateDataFromIOBuffer(const SCENARIOUTPUTFILEINF *pSceInf, BOOL *pAbort);
	BOOL ReadStateDataFromIOBuffer(SCENARIOPARAMS Sce, ANIMATSTATE_FILEOUT *AS, BOOL *pAbort);
	BOOL WriteStateDataToIOBuffer(SCENARIOPARAMS *pSce,
								  DWORDLONG Clock,
								  CListManager <CSpecies> *pSpeciesList,
								  ACST_SRC_STATE AcousticState,
								  BOOL *pAbort);
	RESLT FillBinFileStatsRegion(const SCENARIOPARAMS *pSce, TAKE *SceTakes, TAKESTATS *SpeTakes);

private:
	FILE *m_memLocReportFd;
	DWORD m_bufferWrite;
	DWORD m_memLocReportCnt;
	TCHAR m_szMemReportFileName[SIZE_256];
	void _BinOutFileToPlaybackBuffer(const SCENARIOPARAMS *pSce); // new
	void _BinOutBufferToBinOutFile(const SCENARIOPARAMS *pSce);

	void RunThread1(const SCENARIOPARAMS *pSce);
	void RunThread2(const SCENARIOPARAMS *pSce);
	

	// Reading from file for playback
	RESLT SingleAnimatStateFromInputBuffer(SCENARIOPARAMS Sce, int AnimatIndex, ANIMATSTATE_FILEOUT *AS);
	RESLT AcousticStateFromInputBuffer(SCENARIOPARAMS Sce, ACST_SRC_STATE *AcousticState);

	// Saving To File
	RESLT SingleAnimatStateToOutputBuffer(SCENARIOPARAMS Sce, int AnimatIndex, ANIMATSTATE_FILEOUT *AS);
	RESLT AcousticStateToBinOutBuffer(SCENARIOPARAMS Sce, ACST_SRC_STATE AcousticState);
};
