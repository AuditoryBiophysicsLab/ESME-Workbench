
#include "FileManager.h"
#include "3mbsLib.h"
#include "dataTypes_general.h"
#include "params.h"

//IRunnableFMgr *global_threadObj; 

CFileManager::CFileManager(void)
{
	memset(&m_runState, 0, sizeof(m_runState));

	m_acSrcTrkFile = NULL;

	m_fmThread.SetIRunnablePointer((IRunnableFMgr *) this);
	m_fmThread.m_thread1Running = FALSE;
	m_fmThread.m_thread1RunningAck = FALSE;
	m_csvList = NULL;
	m_memLocReportFd = NULL;
	m_bufferWrite = 0;
	m_memLocReportCnt = 0;
	memset(m_szMemReportFileName, 0, sizeof(m_szMemReportFileName));
}

CFileManager::~CFileManager(void)
{
	while(m_fmThread.m_thread1Running == TRUE)
		Sleep(1);
	while(m_fmThread.m_thread2Running == TRUE)
		Sleep(1);
}


// Opens 
RESLT CFileManager::InitializeRun(TCHAR *szFileBinName,
								  TCHAR *szAcstSrcTrackingName,
								  SCENARIOPARAMS Sce,
								  ENVDATA *pEnvData,
								  CListManager <CSpecies> *pSpeciesList,
								  BOOL *pAbort)
{
	_fSCENARIOPARAMS sceParamsBin;
	DWORD bytes=0;
	DWORD totalBytes = 0;
	RESLT res= OK;
	BOOL wrslt;
	SPECIESBINOUTINF speBinOutInf;
	DWORD i;
	int j, k;
	ANIMATASSCN ansm;
	ANIMATASSCN_COMPACTINF ansmc;
	BEHAVIOR_NAME *behName = NULL;
	ACOUSTICSRCEINF lAcstcSrcInf;
	TCHAR szTempBuff[SIZE_256];

	// Run assertions
	_ASSERT(szFileBinName != NULL);
	_ASSERT(szAcstSrcTrackingName != NULL);
	_ASSERT(pEnvData != NULL);
	_ASSERT(pSpeciesList != NULL);
	_ASSERT(pAbort != NULL);
	_ASSERT((DWORD)pSpeciesList->Length() == Sce.numSpecies); // No mutexes required for Length() function.
	_ASSERT(m_runState.a == NULL);
	_ASSERT(m_runState.hdl == NULL);
	_ASSERT(m_memLocReportFd == NULL);
	_ASSERT(m_bufferWrite == 0);
	_ASSERT(m_memLocReportCnt == 0);

	// Clear out the File Manager's state var.
	memset(&m_runState, 0, sizeof(m_runState));
	m_runState.status = OK;

	// If binary output is not enabled no reason to continue.
	if(Sce.user.durationless == TRUE || Sce.user.output.enabled == FALSE || *pAbort == TRUE)
		return OK;
#pragma message("FileManager.cpp line 67, be sure memory for m_csvList is deallocated")
	// Create the list of iteterations to be saved.  If none, m_csvList will be NULL.
	m_csvList = m_csvListMgr.GetWorkingList(Sce.startTime, Sce.duration, &Sce.intrvlOutptLim);

	// If the data exchange between an outside routine (such as ESME) is to be tracked, open the file.
	if(Sce.enableAcstSrcTracking == TRUE)
	{
		if(0 != fopen_s(&m_acSrcTrkFile, szAcstSrcTrackingName, "w"))
		{
			m_acSrcTrkFile = NULL;
			return UninitializeRun(OPEN_ESME_EXCHANGE_ERROR);
		}
	}
	
	// Open the binary file that data will be outputted to.
	m_runState.hdl = CreateFile(szFileBinName, GENERIC_WRITE, 0, 0, CREATE_ALWAYS, FILE_FLAG_RANDOM_ACCESS, NULL);
	if(INVALID_HANDLE_VALUE == m_runState.hdl)
		return UninitializeRun(CREATEBINARYOUTPUT_ERROR);

	// Convert the passed in scenario parameters (SCENARIOPARAMS) into a structure format
	// better suited for saving to file.  Save a copy of the binary setup into the state
	// variable that is later used during buffer flushes.
	sceParamsBin = m_fileManagerStatic.ConvertScenarioFormat(Sce, pEnvData, pSpeciesList);
	sceParamsBin.libVerSuper = MMBSLIB_VERSION_SUPER; // override with current 3mbs version number.
	sceParamsBin.libVerSub = MMBSLIB_VERSION_SUB;
	sceParamsBin.speciesVerSuper = MMMBLIB_SPECIES_VERSION_SUPER; // override with current 3mbs version number.
	sceParamsBin.speciesVerSub = MMMBLIB_SPECIES_VERSION_SUB;
	sceParamsBin.outputVerSuper = MMMBLIB_BINOUTPUT_VERSION_SUPER;
	sceParamsBin.outputVerSub = MMMBLIB_BINOUTPUT_VERSION_SUB;


	m_runState.binSetup = sceParamsBin.diskInf;

	/*FILE PTR ASSERTION:*/
	_ASSERT(totalBytes == 0); // Nothing written so far
	_ASSERT(sceParamsBin.diskInf.fp.scenarioParams == totalBytes); // scenario params occur at the start of the file.

	// Write the scenario parameters to file.
	wrslt = WriteFile(m_runState.hdl, &sceParamsBin, sizeof(sceParamsBin), &bytes, NULL);
	if(wrslt == FALSE || bytes != sizeof(sceParamsBin))
		return UninitializeRun(FILEWRITE_ERROR);

	// STORAGE BYTES ASSERTION:
	// Assert that calculated storage space for scenario params is equivelent to the bytes
	// written for it.
	totalBytes += bytes;
	_ASSERT(sceParamsBin.diskInf.store.scenarioParams == bytes);

	// Write the bathymetry map to file
	if(Sce.user.output.headerInf.bathyMap==TRUE)
	{
		/* FILE PTR ASSERTION:*/
		// The internal file pointer determined for the bathy map by the 
		// call to ConvertScenarioFormat() should
		// be equal to the number of bytes written so far
		_ASSERT(sceParamsBin.diskInf.fp.bathyMap == totalBytes);

		if(OK != (res = pEnvData->bathymetry.SaveToBinFile(m_runState.hdl, &bytes)))
			return UninitializeRun(res);

		/*STORAGE BYTES ASSERTION:*/
		_ASSERT((totalBytes += bytes) && sceParamsBin.diskInf.store.bathyMap == bytes);
	}

	// Write the salinity map to file
	if(Sce.user.output.headerInf.salinityMap==TRUE)
	{
		/* FILE PTR ASSERTION:*/
		// The internal file pointer determined for the salinity map should
		// be equal to the number of bytes written so far
		_ASSERT(sceParamsBin.diskInf.fp.salinityMap == totalBytes);

		if(OK != (res = pEnvData->salinity.SaveToBinFile(m_runState.hdl, &bytes)))
			return UninitializeRun(res);

		/*STORAGE BYTES ASSERTION:*/
		totalBytes += bytes;
		_ASSERT(sceParamsBin.diskInf.store.salinityMap == bytes);
	}

	// Write the temperature map to file
	if(Sce.user.output.headerInf.temperatureMap==TRUE)
	{
		 /*FILE PTR ASSERTION:*/
		// The internal file pointer determined for the temperature map should
		// be equal to the number of bytes written so far
		_ASSERT(sceParamsBin.diskInf.fp.temperatureMap == totalBytes);

		if(OK != (res = pEnvData->temperature.SaveToBinFile(m_runState.hdl, &bytes)))
			return UninitializeRun(res);

		/*STORAGE BYTES ASSERTION:*/
		totalBytes += bytes;
		_ASSERT(sceParamsBin.diskInf.store.temperatureMap == bytes);
	}

	// Create space in the file for the post analysis region that will be filled in later.
	if(Sce.user.output.headerInf.postRunAnalysis == TRUE)
	{
		DWORD statsRegionBytes = 0;
		TCHAR *szBuff;

		/*FILE PTR ASSERTION*/
		_ASSERT(sceParamsBin.diskInf.fp.postAnalysis == totalBytes);

		// Write a blank TAKE region to file, update the total number of bytes
		// written, and save the number of bytes written for this region in variable
		// statsRegionBytes.
		if(NULL == (szBuff = m_staticLib.MallocClearedBuffer(sizeof(TAKE))))
			return UninitializeRun(MEMALLOC_ERROR);

		wrslt = WriteFile(m_runState.hdl, szBuff, sizeof(TAKE), &bytes, NULL);
		free(szBuff);
		if(wrslt == FALSE || bytes != sizeof(TAKE))
			return UninitializeRun(FILEWRITE_ERROR);

		totalBytes += bytes;
		statsRegionBytes = bytes;

		// Write to file a blank region the number of bytes determined by mulitplying the
		// number of species present in the current scenario mulitplied by the size of a
		// TAKESTATS structure.
		
		// Sum the total number of bytes written so far, then ASSERT that the
		// calculated storage space for the statistical post analysis region matches the number
		// of bytes actually written for it.
		if(NULL == (szBuff = m_staticLib.MallocClearedBuffer(sizeof(TAKESTATS) * Sce.numSpecies)))
			return UninitializeRun(MEMALLOC_ERROR);
		wrslt = WriteFile(m_runState.hdl, szBuff, sizeof(TAKESTATS)*Sce.numSpecies, &bytes, NULL);
		free(szBuff);
		if(wrslt == FALSE || bytes != sizeof(TAKESTATS)*Sce.numSpecies)
			return UninitializeRun(FILEWRITE_ERROR);

		/*STORAGE BYTES ASSERTION*/
		totalBytes += bytes;
		statsRegionBytes += bytes;
		_ASSERT(sceParamsBin.diskInf.store.postAnalysis == statsRegionBytes);
	}

	// Save the Species Descriptions.  The following code accesses various linked list
	// methods that modify the list's internal pointers so wise to lock the list's mutex.
	if(Sce.user.output.headerInf.speInfAndAnimatAsscn == TRUE)
	{
		pSpeciesList->Lock();

		/*FILE POINTER ASSERTION*/
		_ASSERT(sceParamsBin.diskInf.fp.speciesDesc == totalBytes);

		for(i=0; i<Sce.numSpecies && *pAbort == FALSE; i++)
		{
			// For each species construct (reuse the same one) the SPECIESBINOUTINF struct
			// and write it to file.
			memset(&speBinOutInf, 0, sizeof(SPECIESBINOUTINF));
			speBinOutInf.description.mbsVerSuper = MMBSLIB_VERSION_SUPER; // override with current 3mbs version number.
			speBinOutInf.description.mbsVerSub = MMBSLIB_VERSION_SUB;
			speBinOutInf.description.speVerSuper = MMMBLIB_SPECIES_VERSION_SUPER; // override with current 3mbs version number.
			speBinOutInf.description.speVerSub = MMMBLIB_SPECIES_VERSION_SUB;
			

			(pSpeciesList->Get(i))->GetDisplayTitle(speBinOutInf.fileTitle, SPECIES_TITLE_BUFF_LEN);
			speBinOutInf.description = ((pSpeciesList->Get(i))->GetSpeciesModelStructCopy()).description;

			wrslt = WriteFile(m_runState.hdl, &speBinOutInf, sizeof(SPECIESBINOUTINF), &bytes, NULL);
			if(wrslt == FALSE || bytes != sizeof(SPECIESBINOUTINF))
				return UninitializeRun(FILEWRITE_ERROR);
			totalBytes += bytes;

			// Write the behavior names of the current species to file.
			if(NULL == (behName = new BEHAVIOR_NAME[speBinOutInf.description.numBehaviors]))
				return UninitializeRun(MEMALLOC_ERROR);
			memset(behName, 0, speBinOutInf.description.numBehaviors * sizeof(BEHAVIOR_NAME));

			// Copy the names of the behaviors into the allocated memory.
			(pSpeciesList->Get(i))->CopyBehaviorNames(behName, SZ_BEHAVIOR_LEN, speBinOutInf.description.numBehaviors);
			wrslt = WriteFile(m_runState.hdl,
							  behName,
							  speBinOutInf.description.numBehaviors * sizeof(BEHAVIOR_NAME),
							  &bytes, NULL);
			delete [] behName;
			if(wrslt == FALSE || bytes != speBinOutInf.description.numBehaviors * sizeof(BEHAVIOR_NAME))
				return UninitializeRun(FILEWRITE_ERROR);

			totalBytes += bytes;
		}

		/*STORAGE BYTES ASSERTION*/
		_ASSERT(sceParamsBin.diskInf.store.speciesDesc == totalBytes-sceParamsBin.diskInf.fp.speciesDesc);

		/*FILE POINTER ASSERTION*/
		_ASSERT(sceParamsBin.diskInf.fp.animatAssoc == totalBytes);

		// Write the animal summaries to file.
		for(i=0; i<Sce.numSpecies && *pAbort == FALSE; i++)
		{
			// Pods
			for(j=0; j<pSpeciesList->Get(i)->GetNumberOfPods() && *pAbort == FALSE; j++)
			{	// Animat in pod j
				for(k=0; k<pSpeciesList->Get(i)->GetNumberAnimatsInPod(j) && *pAbort == FALSE; k++)
				{	// Animat i in pod j
					ansm.speciesNumber = (UINT16)i;	// Cluster number (zero indexed).
					ansm.pod_id = (UINT16)j;		// Pod ID
					ansm.id = (UINT16)k;			// Animal's ID in the pod
					ansmc.individual = FALSE;// Not an individual (part of a pod).
					ansmc.podLeaderType = pSpeciesList->Get(i)->GetPodLeaderType(j);
					ansm.compactInf = m_staticScenario.CompctDecompctPodMembershipInf(ansmc);

					memset(&ansm.acstcSrc, 0, sizeof(ACOUSTICSRCEINF));
					if(pSpeciesList->Get(i)->GetSpeciesType() == SOUNDSOURCE)
					{
						//ansm.acstcSrc.isASoundSource = TRUE;
						lAcstcSrcInf = (pSpeciesList->Get(i)->GetPodMemberCoordinate(j, k)).acstcSrc;
						memcpy(&ansm.acstcSrc, &lAcstcSrcInf, sizeof(ACOUSTICSRCEINF));
						//memcpy(&ansm.acstcSrc, &(pSpeciesList->Get(i)->GetPodMemberCoordinate(j, k)).acstcSrc, sizeof(ACOUSTICSRCEINF));
					}
					wrslt = WriteFile(m_runState.hdl, &ansm, sizeof(ANIMATASSCN), &bytes, NULL);
					if(wrslt == FALSE || bytes != sizeof(ANIMATASSCN))
						return UninitializeRun(FILEWRITE_ERROR);

					totalBytes += bytes;
				}
			}

			// individuals.
			for(j=0; j<pSpeciesList->Get(i)->GetNumberOfIndividuals() && *pAbort == FALSE; j++)
			{
				ansm.speciesNumber = (UINT16)i;	// Cluster number (zero indexed).
				ansm.pod_id = (UINT16)j;		// Pod ID.
				ansm.id = 0;			// Animal's ID
				ansmc.individual = TRUE; // An individual (A pod of size 1).
				ansmc.podLeaderType = ANIMAT; // Arbitrary... since this is an individual.
				ansm.compactInf = m_staticScenario.CompctDecompctPodMembershipInf(ansmc);

				memset(&ansm.acstcSrc, 0, sizeof(ACOUSTICSRCEINF));
				if(pSpeciesList->Get(i)->GetSpeciesType() == SOUNDSOURCE)
				{
					//ansm.acstcSrc.isASoundSource = TRUE;
					lAcstcSrcInf = (pSpeciesList->Get(i)->GetIndividualCoordinate(j)).acstcSrc;
					memcpy(&ansm.acstcSrc, &lAcstcSrcInf, sizeof(ACOUSTICSRCEINF));
//					memcpy(&ansm.acstcSrc, &pSpeciesList->Get(i)->GetIndividualCoordinate(j), sizeof(ACOUSTICSRCEINF));
				}

				wrslt = WriteFile(m_runState.hdl, &ansm, sizeof(ANIMATASSCN), &bytes, NULL);
				if(wrslt == FALSE || bytes != sizeof(ANIMATASSCN))
					return UninitializeRun(FILEWRITE_ERROR);
				totalBytes += bytes;
			}
		}

		// STORAGE BYTES ASSERTION:
		_ASSERT(sceParamsBin.diskInf.store.animatAssoc == totalBytes-sceParamsBin.diskInf.fp.animatAssoc);
		pSpeciesList->Unlock();
	}

	m_staticLib.GetPathAndFileTitleFromFileName(szFileBinName, NULL, 0, szTempBuff, sizeof(szTempBuff));
	sprintf_s(m_szMemReportFileName, sizeof(m_szMemReportFileName), "memReport_%s_", szTempBuff);
	sprintf_s(szTempBuff, sizeof(szTempBuff), "%s%05d.mem", m_szMemReportFileName, m_memLocReportCnt);


	m_memLocReportFd = NULL;
#if 0
	if(0 == fopen_s(&m_memLocReportFd, szTempBuff, "wt"))
	{
		fprintf_s(m_memLocReportFd, "3MB Lib Ver %02d.%02d, ", Sce.libVerSuper, Sce.libVerSub);
		fprintf_s(m_memLocReportFd, "Output Ver: %02d.%02d\n", Sce.outputVerSuper, Sce.outputVerSub);
		fprintf_s(m_memLocReportFd, "Start Time %d, Duration %d\n", Sce.startTime, Sce.duration);
		fprintf_s(m_memLocReportFd, "Iterations to be saved: %d, total num animats:%d\n", Sce.numSaveIterations, Sce.totalNumAnimats);

	}
	else
	{
		m_memLocReportFd = NULL;
	}
#endif


	return OK;
}

CWorkingList *CFileManager::GetWorkingList(int StartTime, int Duration, const OUTPTINTRVLLMT *pOutputLim)
{
	return m_csvListMgr.GetWorkingList(StartTime, Duration, pOutputLim);
}

RESLT CFileManager::InitializePlayback(TCHAR *szFileName, SCENARIOUTPUTFILEINF *PlaybackState)
{
	DWORD readBytes = 0;
	DWORD i;
	TCHAR fileID[SIZE_16];
	DWORD bytes;
	_fSCENARIOPARAMS sceParamsBin;

	// Run Assertions
	_ASSERT(m_runState.a == NULL);
	_ASSERT(m_runState.hdl == NULL);
	_ASSERT(PlaybackState->pSpeInf == NULL);
	_ASSERT(PlaybackState->speBehaviorNames == NULL);
	
	// Clear out the File Manager's state var.
	memset(&m_runState, 0, sizeof(m_runState));
	m_runState.status = OK;


	//---------------------------------------------------------------------//
	// Create the binary file, read the scenario header information into it.	
	//---------------------------------------------------------------------//
	m_runState.hdl =
		CreateFile(szFileName, GENERIC_READ, NULL, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
	if(m_runState.hdl == INVALID_HANDLE_VALUE)
		return OPENFILEREAD_ERROR;

	// Verify the first 16 bytes are the file identifier
	if(ReadFile(m_runState.hdl, fileID, sizeof(fileID), &bytes, NULL) == FALSE || bytes != sizeof(fileID))
	{
		CloseHandle(m_runState.hdl);
		return FILEREAD_ERROR;
	}
	if(strcmp(fileID, SZ_OUTPUTFILEID) != 0)
	{
		CloseHandle(m_runState.hdl);
		return OBSOLETE_3MBS_VERSION;
	}


	if(FALSE == m_fileManagerStatic.AssertBinaryOutFilePointers(m_runState.hdl))
		return UninitializeRun(FILEFORMAT_ERROR);

	m_staticLib.MySetFilePointer(m_runState.hdl, 0, FILE_BEGIN);
	if(ReadFile(m_runState.hdl, &sceParamsBin, sizeof(sceParamsBin), &bytes, NULL) == FALSE)
		return UninitializeRun(FILEREAD_ERROR);

	PlaybackState->sce = m_fileManagerStatic.ConvertScenarioFormat(sceParamsBin);

	if(PlaybackState->sce.numSaveIterations * (PlaybackState->sce.totalNumAnimats) > MAX_NUM_PLAYBACK_STATES)
		return UninitializeRun(MAX_NUM_PLAYBACK_STATES_EXCEEDED);


	m_runState.binSetup = sceParamsBin.diskInf;

	// Initially no iterations have been read into the buffer
	m_runState.fileIterationRWCount = 0;
	m_runState.bufferIterationLevel = 0;

	//----------------------------------------------------------------------------------//
	// Bathymetry map
	//---------------//
	if(PlaybackState->sce.user.output.headerInf.bathyMap == TRUE)
	{
		if(OK != PlaybackState->envData.bathymetry.LoadFromBinFile(m_runState.hdl))
			return UninitializeRun(FILEREAD_ERROR);
	}

	//----------------------------------------------------------------------------------//
	// Salinity map
	//---------------//
	if(PlaybackState->sce.user.output.headerInf.salinityMap == TRUE)
	{
		if(OK != PlaybackState->envData.salinity.LoadFromBinFile(m_runState.hdl))
			return UninitializeRun(FILEREAD_ERROR);
	}

	//----------------------------------------------------------------------------------//
	// Temperature map
	//---------------//
	if(PlaybackState->sce.user.output.headerInf.temperatureMap == TRUE)
	{
		if(OK != PlaybackState->envData.temperature.LoadFromBinFile(m_runState.hdl))
			return UninitializeRun(FILEREAD_ERROR);

	}

	//----------------------------------------------------------------------------------//
	// Not Interested (for now) in Statistical data
	//---------------------------------------------//
	if(PlaybackState->sce.user.output.headerInf.postRunAnalysis == TRUE)
	{
		m_staticLib.MySetFilePointer(m_runState.hdl, sizeof(TAKE) + sizeof(TAKESTATS)*sceParamsBin.numSpecies, FILE_CURRENT);
#if 0
		ReadFile(m_runState.hdl, &statAnalysis, sizeof(TAKE), &readBytes, NULL);
		dwrd = m_staticLib.MySetFilePointer(m_runState.hdl, sizeof(TAKESTATS)*sceParamsBin.numSpecies, FILE_CURRENT);
		dwrd = readBytes + sizeof(TAKESTATS)*sceParamsBin.numSpecies;
#endif
	}

	//----------------------------------------------------------------------------------//
	// Animat to species association
	//------------------------------//
	if(PlaybackState->sce.user.output.headerInf.speInfAndAnimatAsscn == TRUE)
	{
		if(NULL == (PlaybackState->pSpeInf = new SPECIESBINOUTINF[PlaybackState->sce.numSpecies]))
			return UninitializeRun(MEMALLOC_ERROR);

		if(NULL == (PlaybackState->speBehaviorNames = new NAMES *[PlaybackState->sce.numSpecies]))
			return UninitializeRun(MEMALLOC_ERROR);

		memset(PlaybackState->speBehaviorNames, 0, sizeof(NAMES *) * PlaybackState->sce.numSpecies);
		// Add error handling

		for(i=0; i <sceParamsBin.numSpecies; i++)
		{
			if(FALSE == ReadFile(m_runState.hdl, &PlaybackState->pSpeInf[i], sizeof(SPECIESBINOUTINF), &readBytes, NULL))
				return UninitializeRun(FILEREAD_ERROR);

			if(NULL == (PlaybackState->speBehaviorNames[i] = new NAMES[PlaybackState->pSpeInf[i].description.numBehaviors]))
				return UninitializeRun(MEMALLOC_ERROR);
			memset(PlaybackState->speBehaviorNames[i], 0, sizeof(NAMES) * PlaybackState->pSpeInf[i].description.numBehaviors);
			// Add error handling

			// Read the names.
			if(FALSE == ReadFile(m_runState.hdl, PlaybackState->speBehaviorNames[i],
					sizeof(NAMES)*PlaybackState->pSpeInf[i].description.numBehaviors, &readBytes, NULL))
				return UninitializeRun(FILEREAD_ERROR);
		}

		// Animat Associations
		if(NULL == (PlaybackState->animatAssociations = new ANIMATASSCN[sceParamsBin.totalNumAnimats]))
			return UninitializeRun(MEMALLOC_ERROR);
		memset(PlaybackState->animatAssociations, 0, sizeof(ANIMATASSCN)*sceParamsBin.totalNumAnimats);

		if(FALSE == ReadFile(m_runState.hdl, PlaybackState->animatAssociations,
				sizeof(ANIMATASSCN)*sceParamsBin.totalNumAnimats, &readBytes, NULL))
			return UninitializeRun(FILEREAD_ERROR);
	}
	else
	{
		// Make some fake species and animat assoication information.
		PlaybackState->sce.numSpecies = 1;
		if(NULL == (PlaybackState->pSpeInf = new SPECIESBINOUTINF[1]))
			return UninitializeRun(MEMALLOC_ERROR);
		memset(PlaybackState->pSpeInf, 0, sizeof(SPECIESBINOUTINF));

		PlaybackState->pSpeInf[0].description.numBehaviors = 1;

		if(NULL == (PlaybackState->speBehaviorNames = new NAMES *[1]))
			return UninitializeRun(MEMALLOC_ERROR);
		PlaybackState->speBehaviorNames[0] = new NAMES[1];

		sprintf_s(PlaybackState->pSpeInf[0].fileTitle, sizeof(PlaybackState->pSpeInf[0].fileTitle), "Unknown Species");
		sprintf_s(PlaybackState->speBehaviorNames[0][0], sizeof(NAMES), "Unknown Beh Name");
		


		// Animat Associations
		if(NULL == (PlaybackState->animatAssociations = new ANIMATASSCN[sceParamsBin.totalNumAnimats]))
			return UninitializeRun(MEMALLOC_ERROR);
		memset(PlaybackState->animatAssociations, 0, sizeof(ANIMATASSCN)*sceParamsBin.totalNumAnimats);

		for(i=0; i<sceParamsBin.totalNumAnimats; i++)
		{
			PlaybackState->animatAssociations[i].compactInf = 1; // treat as an individual.
		}
	}


	//----------------------------------------------------------------------------------//
	// Allocate a buffer that stores all the animat's state for every iteration (or saved
	// state) for display on the bitmap.
	//----------------------------------//
	if(NULL == (PlaybackState->animatState = new ANIMATSTATE_FILEOUT[PlaybackState->sce.numSaveIterations * PlaybackState->sce.totalNumAnimats]))
		return UninitializeRun(MEMALLOC_ERROR);

	return OK;
}




//***********************************************
// *** CSV Manager interface member functions ***

// Has the CSV File Manager to read in from file a list of states to be saved.  Returns
// OK if successful
RESLT CFileManager::ReadCSVListFromFile(TCHAR *szFileName)
{
	return m_csvListMgr.ReadCSVListFromFile(szFileName);
}

BOOL CFileManager::CSVFileLoaded(){	return m_csvListMgr.CSVFileLoaded();}


// Has the CSV File Manager clear out the current list of states to save.
void CFileManager::ClearCSVList()
{
	return m_csvListMgr.ClearCSVList();
}

int CFileManager::GetStateSaveToFileCount(int StartTime, int Duration, const OUTPTINTRVLLMT *pOutputLim)
{
	return m_csvListMgr.GetFileReadListSaveStateCount(StartTime, Duration, pOutputLim);
}


/*****************************************************************************************
* MEMBER FUNCTION: GetMemoryAllocationDetails()
* 
* DESCRIPTION:  Gets details about memory allocation needed to suppport the current
* scenario.  Returns the information in a FM_MEMALLOCINF struct.  No information is
* actually allocated by this function.  Information returned to calling routine
* in the FM_MEMALLOCINF struct (see the definition of FM_MEMALLOCINF) includes:
*   * available physical memory available,
*	* maximum amount of memory 3mb would try to allocate for an output buffer
*	* actual amount of memory 3mb will try to allocate 
*	* the number of iterations the buffer must support even if ultimately file paging results
*	* the size (in bytes) each animat state outputs
*	* the size (in bytes) each acoustic state outputs.
*
* PARAMETERS:
*	A SCENARIOPARAMS struct that contains needed information to calculate the number of
*	bytes required for the output buffer, specifically
*	* the number of animats and and acoustic sources present
*	* the number of saved iterations to be saved or read in from file.
*
* MEMBER VARIABLES ALTERED:
*
* RETURN VALUE:
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   Unknown  (MJC)  Initial Coding
*	06/24/09 (MJC)  Commented/documented the function
*****************************************************************************************/
FM_MEMALLOCINF CFileManager::GetMemoryAllocationDetails(SCENARIOPARAMS Sce)
{
	FM_MEMALLOCINF mem = {0};
	int numSaveIts; // number of iterations saved or to be saved to file
	DWORDLONG acstcTestBytes; // number of dynamically allocated byes required for using an animat
	// as the acoutisct test source

	// No need to allocate a state to output file buffer if there is no output.
	if(Sce.user.output.enabled == FALSE || Sce.user.durationless == TRUE)
		return mem;

	// Allocate the buffer to hold as many states per animat (iterations) as needed as possible
	acstcTestBytes = 0;
	if(Sce.user.acousticAnimatActive == TRUE)
		acstcTestBytes = Sce.totalNumAnimats * (sizeof(COORD_DEPTH) + sizeof(double));

	// If a list of saved iterations is loaded get the number of iterations that will be
	// outputted to file as a result of the combination of that saved list, the start
	// time, and the duration of the scenaro.  If no list is loaded the return value will
	// be the scenario duration + 1 (+1 to account for the initial state of all animats).
	numSaveIts = m_csvListMgr.GetFileReadListSaveStateCount(Sce.startTime, Sce.duration, &Sce.intrvlOutptLim);

	mem = m_fileManagerStatic.GetMemoryAllocationDetails(Sce.totalNumAnimats, numSaveIts, Sce.user.output);

	return mem;
}

// Returns the maximum system memory of the memory available this process would
// attempt to dynamically allocate
// change AnimatStateSize to fileOutputAnimatStateSize
#ifndef UINT64
#define UINT64 unsigned __int64
#endif



// This function converts the scenario set up (in the SCENARIOPARAMS struct) into one better
// suitable for saving to file.  It is better suitable because disk saved is saved where
// possible by compressing information in the SCENARIOPARAMS struct where possible and
// reasonable to do so.


RESLT CFileManager::UninitializeRun(RESLT Res)
{
	//DWORD i;

	if(m_runState.hdl != NULL && m_runState.hdl != INVALID_HANDLE_VALUE)
		CloseHandle(m_runState.hdl);
	m_runState.hdl = NULL;

	if(m_runState.a != NULL)
	{
		delete [] m_runState.a;
		m_runState.a = NULL;
	}

	// ESME data exchage tracking file
	if(m_acSrcTrkFile != NULL)
		fclose(m_acSrcTrkFile);
	m_acSrcTrkFile = NULL;

	if(m_runState.status == OK)
		m_runState.status = Res;

	if(m_memLocReportFd != NULL)
	{
		fclose(m_memLocReportFd);
		m_memLocReportFd = NULL;
	}
	m_bufferWrite = 0;
	m_memLocReportCnt = 0;
	memset(m_szMemReportFileName, 0, sizeof(m_szMemReportFileName));
	return Res;
}

RESLT CFileManager::UninitializePlayback(SCENARIOUTPUTFILEINF *PlaybackState, RESLT Res)
{
	DWORD i;

	// ESME data exchage tracking file
	if(m_acSrcTrkFile != NULL)
		fclose(m_acSrcTrkFile);
	m_acSrcTrkFile = NULL;

	//----------------------------------------------------------//
	// Deallocate memory used for playback if playback was used.
	//----------------------------------------------------------//
	// Clear environmental information
	PlaybackState->envData.bathymetry.ClearData();
	PlaybackState->envData.salinity.ClearData();
	PlaybackState->envData.temperature.ClearData();

	// Deallocate species information
	if(PlaybackState->pSpeInf != NULL)
		delete [] PlaybackState->pSpeInf;
	PlaybackState->pSpeInf = NULL;

	// Deallocate behavior names of each species
	if(PlaybackState->speBehaviorNames != NULL)
	{
		for(i=0; i < PlaybackState->sce.numSpecies; i++)
		{
			if(PlaybackState->speBehaviorNames[i] != NULL)
				delete [] PlaybackState->speBehaviorNames[i];
		}
		delete [] PlaybackState->speBehaviorNames;
	}
	PlaybackState->speBehaviorNames = NULL;

	// Deallocate animat to species association
	if(PlaybackState->animatAssociations != NULL)
		delete [] PlaybackState->animatAssociations;
	PlaybackState->animatAssociations = NULL;

	if(PlaybackState->animatState != NULL)
		delete [] PlaybackState->animatState;
	PlaybackState->animatState = NULL;

	memset(PlaybackState, 0, sizeof(SCENARIOUTPUTFILEINF));

	return UninitializeRun(Res);
}


RESLT CFileManager::AcousticDataToFile(DWORDLONG Clock, double SrcLat, double SrcLon, DWORD ArrayLen, double *Array)
{
	DWORD i;
	int res;

	if(m_acSrcTrkFile == NULL)
		return OK;

	res = fprintf_s(m_acSrcTrkFile, "%06d: Acoutic Exposure Src (lat,lon) %19.14f,%19.14f\n\t", Clock, SrcLat, SrcLon);

	for(i=0; i<ArrayLen && res >= 0; i++)
		res = fprintf_s(m_acSrcTrkFile, "(%d):%f ", i, (float)Array[i]);

	if(res >= 0)
		fprintf_s(m_acSrcTrkFile, "\n");

	if(res < 0)
		return FILEWRITE_ERROR;

	return OK;
}

RESLT CFileManager::ResetAcousticSourceInf(ACOUSTICSRCEINF *AcstcSrcInf, int AcstcSrcInfBuffLen)
{
	return m_csvListMgr.ResetAcousticSourceInf(AcstcSrcInf, AcstcSrcInfBuffLen);
}


// Allocating the file output buffer is performed separately and after InitializeRun() so that as much dynamic memory as is possible
// is made available to the output buffer.  Calling routines should deallocate all unneccessary memory for this reason.
RESLT CFileManager::AllocateFileIOBuffer(SCENARIOPARAMS Sce, BOOL ForceAllIterations)
{
	FM_MEMALLOCINF mem;
	DWORD numSaveIts; // number of iterations saved or to be saved to file
	DWORDLONG acstcTestBytes; // number of dynamically allocated byes required for using an animat
								  // as the acoutisct test source

	// No need to allocate a state to output file buffer if there is no output.
	if(Sce.user.durationless == TRUE || Sce.user.output.enabled == FALSE)
		return OK;

	// The output file must be created before the buffer is allocated.
	_ASSERT(m_runState.hdl != NULL);
	_ASSERT(m_runState.a == NULL);
	if(m_runState.hdl == NULL)
		return FILE_NOT_OPEN_ERROR;
	if(m_runState.a != NULL)
		return MEM_ALREADY_ALLOC_ERROR;

	// Allocate the buffer to hold as many states per animat (iterations) as needed as possible
	acstcTestBytes = 0;
	if(Sce.user.acousticAnimatActive == TRUE)
		acstcTestBytes = Sce.totalNumAnimats * (sizeof(COORD_DEPTH) + sizeof(double));

	// If a list of saved iterations is loaded get the number of iterations that will be
	// outputted to file as a result of the combination of that saved list, the start
	// time, and the duration of the scenaro.  If no list is loaded the return value will
	// be the scenario duration + 1 (+1 to account for the initial state of all animats).
	numSaveIts = (DWORD)m_csvListMgr.GetFileReadListSaveStateCount(Sce.startTime, Sce.duration, &Sce.intrvlOutptLim);

	mem = m_fileManagerStatic.GetMemoryAllocationDetails(Sce.totalNumAnimats, numSaveIts, Sce.user.output, acstcTestBytes);

	if(ForceAllIterations == TRUE && numSaveIts != mem.bufferIterationCapacity)
		return FILESIZE_TOO_LARGE;

	if(NULL == (m_runState.a = new BYTE[mem.numBytes]))
		return UninitializeRun(MEMALLOC_ERROR);

	m_runState.bytes = mem.numBytes;
	m_runState.bufferIterationCapacity = mem.bufferIterationCapacity;

	if(m_memLocReportFd != NULL)
	{
		fprintf_s(m_memLocReportFd, "        Animat state size: %07d per animat\n", mem.animatStateSize);
		fprintf_s(m_memLocReportFd, "  Acoustic src state size: %07d\n", mem.acousticSrcStateSize);
		fprintf_s(m_memLocReportFd, "                           -------\n");
		fprintf_s(m_memLocReportFd, "                     Total: %07d\n\n", mem.animatStateSize + mem.acousticSrcStateSize);
		fprintf_s(m_memLocReportFd, "Number of Saved Iterations: %07d\n", numSaveIts);
		fprintf_s(m_memLocReportFd, "Buffer size: %d bytes (%d iterations) (%.1f states)\n\n",
			mem.numBytes, mem.bufferIterationCapacity, ((float)mem.numBytes/(float)(mem.animatStateSize + mem.acousticSrcStateSize)));

		fprintf_s(m_memLocReportFd, "       Buffer  Byte\n");
		fprintf_s(m_memLocReportFd, "Animat  Write   Pos\n");
		fprintf_s(m_memLocReportFd, "------ ------ -------\n");
	}
	return OK;
}


// Calling routine's access to the current animat states in the buffer to be played back.
// This function is called every iteration so code it to run fast over saving memory
//BOOL CFileManager::ReadStateDataFromIOBuffer(const SCENARIOUTPUTFILEINF *pSceInf, BOOL *pAbort)
BOOL CFileManager::ReadStateDataFromIOBuffer(SCENARIOPARAMS Sce, ANIMATSTATE_FILEOUT *AS, BOOL *pAbort)
{
	UINT i;

	_ASSERT(pAbort != NULL);

	// If there is no binary output for this scenario return
	if(Sce.user.output.enabled == FALSE || *pAbort == TRUE)
		return FALSE; // Returning without launching the buffer flush thread


	//-------------------------------------------------------------------------------------//
	// The function for dumping the output buffer to file shouldn't be running if this is
	// called.  If so, a serious programming error but handle it as best as possible by
	// sleeping to see if it ceases then exit if it doesn't
	//------------------------------------------------------//
	_ASSERT(m_fmThread.m_thread1Running == FALSE);
	_ASSERT(m_fmThread.m_thread2Running == FALSE);
	if(m_fmThread.m_thread1Running == TRUE && *pAbort == FALSE)
	{
		Sleep(1000);
		if(m_fmThread.m_thread1Running == TRUE && *pAbort == FALSE)
		{
			UninitializeRun(ALREADYRUNNING_ERROR);
			return FALSE;
		}
	}
	//-------------------------------------------------------------------------------------//


	//-------------------------------------------------------------------------------------//
	// If the input buffer is empty refill it.
	//----------------------------------------//
	if(m_runState.bufferIterationLevel == 0)
	{
		this->pSce = &Sce; // necessary for thread being called.
		m_fmThread.startThread2(&Sce);
		while(m_fmThread.m_thread2Running == FALSE)
			Sleep(1);
		m_fmThread.m_thread2RunningAck = TRUE;

		while(m_fmThread.m_thread2Running == TRUE)
			Sleep(1);
	}
	//-------------------------------------------------------------------------------------//

	for(i=0; i<(UINT)Sce.totalNumAnimats; i++)
	{
		SingleAnimatStateFromInputBuffer(Sce, i, &AS[i]);
	}

	// Reading in (and therefore removing) a single iteration from the input buffer so
	// decrement the count that indicates how many iterations remain.
	m_runState.bufferIterationLevel--;

	//-------------------------------------------------------------------------------------//
	// Testing and debug
	//-------------------//
//	if(m_runState.bufferIterationLevel == 0)
//		m_runState.bufferIterationLevel = 0;
	//-------------------------------------------------------------------------------------//

	return TRUE;
}


// Called by the scenario class to have its current animat states be put into the buffer that eventually gets flushed to file.
// This function is called every iteration so code it to run fast over saving memory
BOOL CFileManager::WriteStateDataToIOBuffer(SCENARIOPARAMS *pSce,
											DWORDLONG Clock,
											CListManager <CSpecies> *pSpeciesList,
											ACST_SRC_STATE AcousticState,
											BOOL *pAbort)
{
	UINT i, j, k;
	UINT animatIndex = 0;
	ANIMATSTATE_FILEOUT animatState;

	// Memory Saving
	int numPods;
	int numAnimats;

	// Run Assertions
	_ASSERT(m_fmThread.m_thread1Running == FALSE);
	_ASSERT(pSpeciesList != NULL);
	_ASSERT(pAbort != NULL);
	_ASSERT((DWORD)pSpeciesList->Length() == pSce->numSpecies);// Length() not dependent upon 
													  // internal pointers so no need to
													  // lock the mutex.

	// If there is no binary output for this scenario return
	if(pSce->user.output.enabled == FALSE || *pAbort == TRUE)
		return FALSE; // Returning without launching the buffer flush thread


	// The function for dumping the output buffer to file shouldn't be running if this is
	// called.  If so, a serious programming error but handle it as best as possible by
	// sleeping to see if it ceases then exit if it doesn't
	if(m_fmThread.m_thread1Running == TRUE && *pAbort == FALSE)
	{
		Sleep(1000);
		if(m_fmThread.m_thread1Running == TRUE && *pAbort == FALSE)
		{
			UninitializeRun(ALREADYRUNNING_ERROR);
			return FALSE;
		}
	}

	// FALSE is returned if no CSV linked list or clock value not found in it
	if(FALSE == m_csvListMgr.IsaSaveClockState(m_csvList, (int)Clock, TRUE))
		return FALSE;

	// replace all of this with a single loop through the animat state.
	// Lock the species list to prevent other threads from modifying the linked list's internal pointers.
	pSpeciesList->Lock();
	for(i=0; i<(UINT)pSce->numSpecies; i++)
	{
		// Assert proper species list maintainence.
		_ASSERT(pSpeciesList->Get(i)->GetNumberOfPods() >= 0 && pSpeciesList->Get(i)->GetNumberOfIndividuals() >= 0);
		_ASSERT(pSpeciesList->Get(i)->GetNumberOfPods() > 0 || pSpeciesList->Get(i)->GetNumberOfIndividuals() > 0);

		// Pods are always processed first.
		numPods = pSpeciesList->Get(i)->GetNumberOfPods();
		for(j=0; j<(UINT)numPods; j++)
		{
			numAnimats = pSpeciesList->Get(i)->GetNumberAnimatsInPod(j);
			_ASSERT(numAnimats >= 0);
			for(k=0; k<(UINT)numAnimats; k++)
			{
				// Get the animat state, copy components of it into the file buffer as 
				// based upon the scenario configuation
				pSpeciesList->Get(i)->GetPodMemberState(j,k, &animatState);
				SingleAnimatStateToOutputBuffer(*pSce, animatState.animatID, &animatState);

				// Assert animat ID matched buffer index
				_ASSERT(animatState.animatID == animatIndex++); 
			}
		}

		// Individuals are always processed second.
		numAnimats = pSpeciesList->Get(i)->GetNumberOfIndividuals();
		for(j=0; j<(UINT)numAnimats; j++)
		{
			pSpeciesList->Get(i)->GetIndividualState(j, &animatState);

			// Get the animat state, copy components of it into the file buffer as 
			// based upon the scenario configuation
			SingleAnimatStateToOutputBuffer(*pSce, animatState.animatID, &animatState);

			// Assert animat ID matched buffer index
			_ASSERT(animatState.animatID == animatIndex++);
		}
	}
	pSpeciesList->Unlock();

	AcousticStateToBinOutBuffer(*pSce, AcousticState);

	m_runState.bufferIterationLevel++;
	//----------------------------------------------------------------------------------//
	// If the buffer is full write its contents to file.  Then reset.
	//--------------------------------------------------------------//
	if(m_runState.bufferIterationLevel != m_runState.bufferIterationCapacity)
		return FALSE;

		// Launch a thread, wait for it to signal that it has started, then return
		// TRUE to indicate that the file manager is busy dumping the binary buffer
		// out to the binary file.

	this->pSce = pSce; // necessary for thread being called.
	m_fmThread.startThread1(pSce);
	while(m_fmThread.m_thread1Running == FALSE)
		Sleep(1);
	m_fmThread.m_thread1RunningAck = TRUE;
	return TRUE;
}

// Flushes the contents of the buffer.  Call when ending scenario.
BOOL CFileManager::FlushBuffer(SCENARIOPARAMS *pSce, BOOL *pAbort)
{
	pAbort = pAbort; // quiet compiler warning, implement later if needed...

	if(m_runState.bufferIterationLevel == 0)
		return FALSE;

	this->pSce = pSce;
	m_fmThread.startThread1(pSce);
	while(m_fmThread.m_thread1Running == FALSE)
		Sleep(1);
	m_fmThread.m_thread1RunningAck = TRUE;
	return TRUE;
}


void CFileManager::RunThread1(const SCENARIOPARAMS *pSce)
{
	//m_state.activity = ___SCE_INIT;
	_BinOutBufferToBinOutFile(pSce);
	//m_state.activity = __RUN_FINISHED;
}



void CFileManager::RunThread2(const SCENARIOPARAMS *pSce)
{
	//m_state.activity = ___SCE_INIT;
	_BinOutFileToPlaybackBuffer(pSce);
	//m_state.activity = __RUN_FINISHED;
}

// For reading animat states in for playback.
void CFileManager::_BinOutFileToPlaybackBuffer(const SCENARIOPARAMS *pSce)
{
	DWORD bytes;
	DWORDLONG i;
	__int64 fp; // file pointer (bytes)
	size_t bp; // buffer pointer (bytes), use a size_t.
	BOOL wrslt = TRUE;

	_ASSERTE(m_runState.hdl != NULL);
	_ASSERTE(pSce != NULL);
	CALCLOCINBUFF clb;

	// Critical values used in calculations, specfically using 64 bit DWORDLONG's here.
	DWORDLONG numAnimats = (DWORDLONG)pSce->totalNumAnimats;

	DWORDLONG aeStateBytes = (DWORDLONG)m_runState.binSetup.store.aeState;
	DWORDLONG animatStateBytes = (DWORDLONG)m_runState.binSetup.store.animatState;
	DWORDLONG numItsToBeRead = m_runState.bufferIterationCapacity;
	DWORDLONG numItsRead = m_runState.fileIterationRWCount;

	// Assumption is that no reads from the buffer have occured as a condition of reading
	// data from the file into the buffer.
	_ASSERT(m_runState.bufferIterationLevel == 0);
	_ASSERT(m_fmThread.m_thread1Running == FALSE);
	_ASSERT(m_fmThread.m_thread2Running == FALSE);

	//m_runState.fileIterationRemain = 0;
	m_runState.bufferIterationLevel = m_runState.bufferIterationCapacity;
	if(m_runState.bufferIterationLevel > m_runState.bufferIterationCapacity - m_runState.fileIterationRWCount)
		m_runState.bufferIterationLevel = m_runState.bufferIterationCapacity - m_runState.fileIterationRWCount;

	m_fmThread.m_thread2Running = TRUE;
	if(pSce->user.output.outputByTime == TRUE)
	{
		// When output by time the binary output buffer is is already formatted as it is
		// layed out in the binary file.  All that is needed then is to make a single
		// system ReadFile() call.
		// Calculate the byte location in the file this read must read data from.
		fp = m_runState.binSetup.fp.animatState + // the begining of the animat (and AE states)
			numItsRead * numAnimats * animatStateBytes + // total animat state bytes written to file so far
			numItsRead * aeStateBytes; // total AE bytes written to file so far.

		m_staticLib.MySetFilePointer(m_runState.hdl, fp, FILE_BEGIN);

		wrslt = ReadFile(
			m_runState.hdl,
			m_runState.a,
			(DWORD)((animatStateBytes * numAnimats + aeStateBytes) * m_runState.bufferIterationLevel),
			&bytes,
			NULL);

		if(FALSE == wrslt)
			m_runState.status = UninitializeRun(FILEWRITE_ERROR);
	}
	else
	{
		// Binary output is grouped by animat
		clb.bufferIterationLen = m_runState.bufferIterationCapacity; // length of the buffer in iterations (not bytes)
		clb.iterationWriteNum = 0;
		clb.numSaveIterations = pSce->numSaveIterations;

		clb.numAnimats = numAnimats; // total number of animats present
		
		//clb.animatStateSize = m_runState.animatStateSize; // size of individual animat state in bytes
		//clb.acstSrcStateSize = m_runState.acousticSrcStateSize; // size of inidivual acoustic source state size in bytes
		clb.animatStateSize = m_runState.binSetup.store.animatState; // size of individual animat state in bytes
		clb.acstSrcStateSize = m_runState.binSetup.store.aeState; // size of inidivual acoustic source state size in bytes

		clb.outputByTime = FALSE; //
		for(i=0; i<numAnimats && m_runState.status == OK; i++)
		{
			clb.animatIndex = i;  // index of animat in question.

			m_runState.currentAnimatFlush = (DWORD)(i+1);

			// Determine the location in the buffer that this animimat's states begin that are to
			// be transferred to the file.
			bp = (size_t)m_fileManagerStatic.CalculateAnimatStateBufferLocation(clb);

			// Determine the location in the file animat i writes into.
			fp = m_runState.binSetup.fp.animatState + // the location in file where animat states begin, in bytes
				(
					i * /*this animat's index*/
					numItsToBeRead + /* total iterations saved for each animat*/
					numItsRead // total iterations saved so far
				 )
				 * animatStateBytes; // bypes per animat state.

			m_staticLib.MySetFilePointer(m_runState.hdl, fp, FILE_BEGIN);

			wrslt = ReadFile(m_runState.hdl,
							  &m_runState.a[bp],
							  m_runState.binSetup.store.animatState * m_runState.bufferIterationLevel, // written into buffer
							  &bytes,
							  NULL);
			_ASSERT(wrslt != FALSE); // assuming parameters to WriteFile() are incorrectly written
			if(FALSE == wrslt)
				m_runState.status = UninitializeRun(FILEWRITE_ERROR);
		}

		if(pSce->user.output.AECoordinate == TRUE && m_runState.status == OK)
		{

			//----------------------------------------------------------------------------------//
			// Read the Acoustic State
			//-------------------------//
			// Determine the location in the buffer, in bytes, that the acoustic exposure states
			// begin (skip past the animats).
			bp = (size_t)(m_runState.bufferIterationCapacity * numAnimats) * m_runState.binSetup.store.animatState;

			// Calculate and set the file pointer for the acoustic exposure.
			fp = m_runState.binSetup.fp.aeState + (DWORDLONG)(numItsRead * m_runState.binSetup.store.aeState);

			m_staticLib.MySetFilePointer(m_runState.hdl, fp, FILE_BEGIN);

			wrslt = ReadFile(m_runState.hdl,
							  &m_runState.a[bp],
							  m_runState.binSetup.store.aeState * m_runState.bufferIterationLevel, // written into buffer
							  &bytes, NULL);
			_ASSERT(wrslt != FALSE); // assuming parameters to WriteFile() are incorrectly written
			if(FALSE == wrslt)
				m_runState.status = UninitializeRun(FILEWRITE_ERROR);
		}
	}

	// Update counts
	if(m_runState.status == OK)
	{
//		m_runState.
		m_runState.bufferCycleCount++;
		m_runState.fileIterationRWCount += m_runState.bufferIterationLevel;
	}

	while(m_fmThread.m_thread2RunningAck == FALSE)
		Sleep(1);

	m_fmThread.m_thread2Running = FALSE;
	m_fmThread.m_thread2RunningAck = FALSE;
}

void CFileManager::_BinOutBufferToBinOutFile(const SCENARIOPARAMS *pSce)
{
	DWORD bytes;
	DWORDLONG i;
	__int64 fp; // file pointer (bytes)
	size_t bp; // buffer pointer (bytes), use a size_t.
	BOOL wrslt = TRUE;

	_ASSERTE(m_runState.hdl != NULL);
	_ASSERTE(pSce != NULL);
	CALCLOCINBUFF clb;

	// Critical values used in calculations, specfically using 64 bit DWORDLONG's here.
	DWORDLONG numAnimats = (DWORDLONG)pSce->totalNumAnimats;
	//DWORDLONG aeStateBytes = (DWORDLONG)m_runState.acousticSrcStateSize;
	//DWORDLONG animatStateBytes = (DWORDLONG)m_runState.animatStateSize;
	DWORDLONG aeStateBytes = (DWORDLONG)m_runState.binSetup.store.aeState;
	DWORDLONG animatStateBytes = (DWORDLONG)m_runState.binSetup.store.animatState;
	//DWORDLONG numItsToBeSaved = m_runState.bufferIterationCapacity;
	//DWORDLONG numItsToBeSaved = m_runState.bufferIterationLevel;
	DWORDLONG numItsToBeSaved = pSce->numSaveIterations;
	DWORDLONG numItsSaved = m_runState.fileIterationRWCount;

	TCHAR szTempBuff[SIZE_256];

	_ASSERT(m_runState.bufferIterationLevel > 0);
	_ASSERT(m_fmThread.m_thread1Running == FALSE);


	if(m_memLocReportFd != NULL)
	{
		fclose(m_memLocReportFd);
		m_memLocReportFd = NULL;
		sprintf_s(szTempBuff, sizeof(szTempBuff), "%s%05d_BUFFERFLUSH.mem", m_szMemReportFileName, m_memLocReportCnt);

		if(0 != fopen_s(&m_memLocReportFd, szTempBuff, "wt"))
			m_memLocReportFd = NULL;
	}


	m_fmThread.m_thread1Running = TRUE;
	if(pSce->user.output.outputByTime == TRUE)
	{
		// When output by time the binary output buffer is is already formatted as it is
		// layed out in the binary file.  All that is needed then is to make a single
		// system WriteFile() call.

		// Calculate the byte location in the file this write must write into.
		fp = m_runState.binSetup.fp.animatState + // the begining of the animat (and AE states)
			numItsSaved * numAnimats * animatStateBytes + // total animat state bytes written to file so far
			numItsSaved * aeStateBytes; // total AE bytes written to file so far.

		if(m_memLocReportFd != NULL)
		{
			fprintf_s(m_memLocReportFd, "Output By Iteration\n");
			fprintf_s(m_memLocReportFd, "file pointer: 0x%x (%d)\n", fp, fp);
		}


		m_staticLib.MySetFilePointer(m_runState.hdl, fp, FILE_BEGIN);

		wrslt = WriteFile(
			m_runState.hdl,
			m_runState.a,
			(DWORD)((animatStateBytes * numAnimats + aeStateBytes) * m_runState.bufferIterationLevel),
			&bytes,
			NULL);

		if(FALSE == wrslt)
			m_runState.status = UninitializeRun(FILEWRITE_ERROR);
	}
	else
	{
		// Binary output is grouped by animat
		clb.bufferIterationLen = m_runState.bufferIterationCapacity; // length of the buffer in iterations (not bytes)
		clb.iterationWriteNum = 0;
		clb.numSaveIterations = pSce->numSaveIterations;

		clb.numAnimats = numAnimats; // total number of animats present
		
		//clb.animatStateSize = m_runState.animatStateSize; // size of individual animat state in bytes
		//clb.acstSrcStateSize = m_runState.acousticSrcStateSize; // size of inidivual acoustic source state size in bytes
		clb.animatStateSize = m_runState.binSetup.store.animatState; // size of individual animat state in bytes
		clb.acstSrcStateSize = m_runState.binSetup.store.aeState; // size of inidivual acoustic source state size in bytes

		clb.outputByTime = FALSE; //

		if(m_memLocReportFd != NULL)
		{
			fprintf_s(m_memLocReportFd, "Output By Animat:\n");
		}

		for(i=0; i<numAnimats && m_runState.status == OK; i++)
		{
			clb.animatIndex = i;  // index of animat in question.

			m_runState.currentAnimatFlush = (DWORD)(i+1);

			// Determine the location in the buffer that this animimat's states begin that are to
			// be transferred to the file.
			bp = (size_t)m_fileManagerStatic.CalculateAnimatStateBufferLocation(clb);

			// Determine the location in the file animat i writes into.
			fp = m_runState.binSetup.fp.animatState + // the location in file where animat states begin, in bytes
				(
					i * numItsToBeSaved + numItsSaved // total iterations saved so far
				 )
				 * animatStateBytes; // bypes per animat state.

			if(m_memLocReportFd != NULL)
			{
				fprintf_s(m_memLocReportFd, "Animat %5I64d, ", i);
				fprintf_s(m_memLocReportFd, "buffer pointer: %8xh (%010dd)  ", bp, bp);
				fprintf_s(m_memLocReportFd, "file pointer: %8I64xh (%010I64dd)\n",fp, fp);
			}

			m_staticLib.MySetFilePointer(m_runState.hdl, fp, FILE_BEGIN);

			wrslt = WriteFile(m_runState.hdl,
							  &m_runState.a[bp],
							  m_runState.binSetup.store.animatState * m_runState.bufferIterationLevel, // written into buffer
							  &bytes,
							  NULL);
			_ASSERT(wrslt != FALSE); // assuming parameters to WriteFile() are incorrectly written
			if(FALSE == wrslt)
				m_runState.status = UninitializeRun(FILEWRITE_ERROR);
		}

		if(pSce->user.output.AECoordinate == TRUE && m_runState.status == OK)
		{

			//----------------------------------------------------------------------------------//
			// Write the Acoustic State
			//-------------------------//
			// Determine the location in the buffer, in bytes, that the acoustic exposure states
			// begin (skip past the animats).
			bp = (size_t)(m_runState.bufferIterationCapacity * numAnimats) * m_runState.binSetup.store.animatState;

			// Calculate and set the file pointer for the acoustic exposure.
			fp = m_runState.binSetup.fp.aeState + (DWORDLONG)(numItsSaved * m_runState.binSetup.store.aeState);

			m_staticLib.MySetFilePointer(m_runState.hdl, fp, FILE_BEGIN);

			wrslt = WriteFile(m_runState.hdl,
							  &m_runState.a[bp],
							  m_runState.binSetup.store.aeState * m_runState.bufferIterationLevel, // written into buffer
							  &bytes, NULL);
			_ASSERT(wrslt != FALSE); // assuming parameters to WriteFile() are incorrectly written
			if(FALSE == wrslt)
				m_runState.status = UninitializeRun(FILEWRITE_ERROR);
		}
	}

	// Update counts
	if(m_runState.status == OK)
	{
		m_runState.bufferCycleCount++;
		m_runState.fileIterationRWCount += m_runState.bufferIterationLevel;
		m_runState.bufferIterationLevel = 0;
	}

	while(m_fmThread.m_thread1RunningAck == FALSE)
		Sleep(1);

	m_fmThread.m_thread1Running = FALSE;
	m_fmThread.m_thread1RunningAck = FALSE;


	if(m_memLocReportFd != NULL)
	{
		fclose(m_memLocReportFd);
		m_memLocReportFd = NULL;
		++m_memLocReportCnt;
		sprintf_s(szTempBuff, sizeof(szTempBuff), "%s%05d.mem", m_szMemReportFileName, m_memLocReportCnt);

		if(0 != fopen_s(&m_memLocReportFd, szTempBuff, "wt"))
			m_memLocReportFd = NULL;
	}


}

BOOL CFileManager::IsFlushingBuffer()
{
	return m_fmThread.m_thread1Running;
}

RESLT CFileManager::AcousticStateFromInputBuffer(SCENARIOPARAMS Sce, ACST_SRC_STATE *AcousticState)
{
	__int64 bytePos = 0;
	_ACST_SRC_STATE_FILE as;
	CALCLOCINBUFF clb;

	AcousticState = AcousticState; // quiet compiler warning for now


	if(Sce.user.output.enabled == FALSE || Sce.user.output.AECoordinate == FALSE)
		return OK;

	clb.bufferIterationLen = m_runState.bufferIterationCapacity; // length of the buffer in iterations (not bytes)
	clb.iterationWriteNum = m_runState.bufferIterationLevel; // number of iterations already written to the buffer

	clb.numAnimats = Sce.totalNumAnimats; // total number of animats present
	clb.numSaveIterations = Sce.numSaveIterations;
	
	//clb.animatStateSize = m_runState.animatStateSize; // size of individual animat state in bytes
	//clb.acstSrcStateSize = m_runState.acousticSrcStateSize; // size of inidivual acoustic source state size in bytes
	clb.animatStateSize = m_runState.binSetup.store.animatState; // size of individual animat state in bytes
	clb.acstSrcStateSize = m_runState.binSetup.store.aeState; // size of inidivual acoustic source state size in bytes

	clb.outputByTime = Sce.user.output.outputByTime; //
	clb.animatIndex = 0; // animat index doesn't effect an acoustic source's location in the binary file.

	bytePos =
		m_fileManagerStatic.CalculateAcousticStateBufferLocation(clb/*Sce.totalNumAnimats, Sce.user.output.outputByTime*/);


	//-------------------------------------------------------------------------//
	// Here to get past compiler warning and until further work is done/needed.
	memset(&as, 0, sizeof(_ACST_SRC_STATE_FILE));
	//-------------------------------------------------------------------------//

#if 0
	as.lat = AcousticState.lat;
	as.lon = AcousticState.lon;
	as.active = (UINT8)AcousticState.active;
#endif
	memcpy(&m_runState.a[bytePos], &as, sizeof(_ACST_SRC_STATE_FILE));
	return OK;
}


RESLT CFileManager::AcousticStateToBinOutBuffer(SCENARIOPARAMS Sce, ACST_SRC_STATE AcousticState)
{
	__int64 bytePos = 0;
	_ACST_SRC_STATE_FILE as;
	CALCLOCINBUFF clb;


	as.lat = AcousticState.lat;
	as.lon = AcousticState.lon;
	as.active = (UINT8)AcousticState.active;

	if(Sce.user.output.enabled == FALSE || Sce.user.output.AECoordinate == FALSE)
		return OK;

	clb.bufferIterationLen = m_runState.bufferIterationCapacity; // length of the buffer in iterations (not bytes)
	clb.iterationWriteNum = m_runState.bufferIterationLevel; // number of iterations already written to the buffer

	clb.numAnimats = Sce.totalNumAnimats; // total number of animats present
	
	//clb.animatStateSize = m_runState.animatStateSize; // size of individual animat state in bytes
	//clb.acstSrcStateSize = m_runState.acousticSrcStateSize; // size of inidivual acoustic source state size in bytes
	clb.animatStateSize = m_runState.binSetup.store.animatState; // size of individual animat state in bytes
	clb.acstSrcStateSize = m_runState.binSetup.store.aeState; // size of inidivual acoustic source state size in bytes

	clb.outputByTime = Sce.user.output.outputByTime; //
	clb.animatIndex = 0; // animat index doesn't effect an acoustic source's location in the binary file.
	clb.numSaveIterations = Sce.numSaveIterations;

	bytePos =
		m_fileManagerStatic.CalculateAcousticStateBufferLocation(clb/*Sce.totalNumAnimats, Sce.user.output.outputByTime*/);

	memcpy(&m_runState.a[bytePos], &as, sizeof(_ACST_SRC_STATE_FILE));
	return OK;
}

RESLT CFileManager::SingleAnimatStateFromInputBuffer(SCENARIOPARAMS Sce, int AnimatIndex, ANIMATSTATE_FILEOUT *AS)
{
	__int64 bytePos;
	CALCLOCINBUFF clb;
	
	_ASSERT(m_runState.bufferIterationCapacity > 0);

	if(Sce.user.output.enabled == FALSE)
		return OK;
	
	// Determine this animat's position, in terms of bytes, in the output buffer for this iteration.
	clb.bufferIterationLen = m_runState.bufferIterationCapacity; // length of the buffer in iterations (not bytes)
	clb.iterationWriteNum = m_runState.fileIterationRWCount; // number of iterations already written to the buffer
	clb.iterationWriteNum = clb.bufferIterationLen - m_runState.bufferIterationLevel;

	clb.numAnimats = Sce.totalNumAnimats; // total number of animats present
	
	//clb.animatStateSize = m_runState.animatStateSize; // size of individual animat state in bytes
	//clb.acstSrcStateSize = m_runState.acousticSrcStateSize; // size of inidivual acoustic source state size in bytes
	clb.animatStateSize = m_runState.binSetup.store.animatState; // size of individual animat state in bytes
	clb.acstSrcStateSize = m_runState.binSetup.store.aeState; // size of inidivual acoustic source state size in bytes

	clb.outputByTime = Sce.user.output.outputByTime; //
	clb.animatIndex = AnimatIndex;
	clb.numSaveIterations = Sce.numSaveIterations;

	bytePos = m_fileManagerStatic.CalculateAnimatStateBufferLocation(clb);

	if(Sce.user.output.animat.ID == TRUE)
	{
		memcpy(&AS->animatID, &m_runState.a[bytePos], sizeof(AS->animatID));
		bytePos += sizeof(AS->animatID);
	}
	if(Sce.user.output.animat.timeOfDay == TRUE)
	{
		memcpy(&AS->clock, &m_runState.a[bytePos], sizeof(AS->clock));
		bytePos += sizeof(AS->clock);
	}
	if(Sce.user.output.animat.coordinate == TRUE)
	{
		memcpy(&AS->lat, &m_runState.a[bytePos], sizeof(AS->lat));
		bytePos += sizeof(AS->lat);

		memcpy(&AS->lon, &m_runState.a[bytePos], sizeof(AS->lon));
		bytePos += sizeof(AS->lon);
	}
	if(Sce.user.output.animat.depth == TRUE)
	{
		memcpy(&AS->depth, &m_runState.a[bytePos], sizeof(AS->depth));
		bytePos += sizeof(AS->depth);
	}
	if(Sce.user.output.animat.bearing == TRUE)
	{
		memcpy(&AS->bearing, &m_runState.a[bytePos], sizeof(AS->bearing));
		bytePos += sizeof(AS->bearing);
	}
	if(Sce.user.output.animat.diveRate == TRUE)
	{
		memcpy(&AS->diveRate, &m_runState.a[bytePos], sizeof(AS->diveRate));
		bytePos += sizeof(AS->diveRate);
	}
	if(Sce.user.output.animat.travelRate == TRUE)
	{
		memcpy(&AS->travelRate, &m_runState.a[bytePos], sizeof(AS->travelRate));
		bytePos += sizeof(AS->travelRate);
	}
	if(Sce.user.output.animat.aeCmltve == TRUE)
	{
		memcpy(&AS->aeCmltve, &m_runState.a[bytePos], sizeof(AS->aeCmltve));
		bytePos += sizeof(AS->aeCmltve);
	}
	if(Sce.user.output.animat.aeMoment == TRUE)
	{
		memcpy(&AS->aeMoment, &m_runState.a[bytePos], sizeof(AS->aeMoment));
		bytePos += sizeof(AS->aeMoment);
	}
	if(Sce.user.output.animat.aeRelAngle == TRUE)
	{
		memcpy(&AS->aeRelAngle, &m_runState.a[bytePos], sizeof(AS->aeRelAngle));
		bytePos += sizeof(AS->aeRelAngle);
	}
	if(Sce.user.output.animat.aeTimeAvrt == TRUE)
	{
		memcpy(&AS->aeTimeAvrt, &m_runState.a[bytePos], sizeof(AS->aeTimeAvrt));
		bytePos += sizeof(AS->aeTimeAvrt);
	}
	if(Sce.user.output.animat.bathyDepth == TRUE)
	{
		memcpy(&AS->bathyDepth, &m_runState.a[bytePos], sizeof(AS->bathyDepth));
		bytePos += sizeof(AS->bathyDepth);
	}
	if(Sce.user.output.animat.salinity == TRUE)
	{
		memcpy(&AS->salinity, &m_runState.a[bytePos], sizeof(AS->salinity));
		bytePos += sizeof(AS->salinity);
	}
	if(Sce.user.output.animat.temperature == TRUE)
	{
		memcpy(&AS->temperature, &m_runState.a[bytePos], sizeof(AS->temperature));
		bytePos += sizeof(AS->temperature);
	}

	if(Sce.user.output.animat.packedData == TRUE)
	{
		memcpy(&AS->packedData, &m_runState.a[bytePos], sizeof(AS->packedData));
		bytePos += sizeof(AS->packedData);
	}

	if(Sce.user.output.animat.targetDepth == TRUE)
	{
		memcpy(&AS->targetDepth, &m_runState.a[bytePos], sizeof(AS->targetDepth));
		bytePos += sizeof(AS->targetDepth);
	}
	if(Sce.user.output.animat.calcDepth == TRUE)  //30
	{
		memcpy(&AS->calcDepth, &m_runState.a[bytePos], sizeof(AS->calcDepth));
		bytePos += sizeof(AS->calcDepth);
	}
	if(Sce.user.output.animat.xyDistance == TRUE) //41
	{
		memcpy(&AS->xDistance, &m_runState.a[bytePos], sizeof(AS->xDistance));
		bytePos += sizeof(AS->xDistance);

		memcpy(&AS->yDistance, &m_runState.a[bytePos], sizeof(AS->yDistance));
		bytePos += sizeof(AS->yDistance);
	}
	if(Sce.user.output.animat.risk == TRUE) //42
	{
		memcpy(&AS->risk, &m_runState.a[bytePos], sizeof(AS->risk));
		bytePos += sizeof(AS->risk);
	}

	return OK;
}


RESLT CFileManager::SingleAnimatStateToOutputBuffer(SCENARIOPARAMS Sce, int AnimatIndex, ANIMATSTATE_FILEOUT *AS)
{
	__int64 bytePos;
	CALCLOCINBUFF clb;
	TCHAR szTempBuff[SIZE_256];
	
	_ASSERT(m_runState.bufferIterationCapacity > 0);

	if(Sce.user.output.enabled == FALSE)
		return OK;

	// Initialize the CalculateStateLocationInBuffer struct (CALCLOCINBUFF)
	// Determine this animat's position, in terms of bytes, in the output buffer for this iteration.
	clb.bufferIterationLen = m_runState.bufferIterationCapacity; // length of the buffer in iterations (not bytes)
	clb.iterationWriteNum = m_runState.bufferIterationLevel; // number of iterations currently written to the buffer
	                                                         //(goes to zero after a buffer flush)
	clb.numAnimats = Sce.totalNumAnimats; // total number of animats present
	clb.animatStateSize = m_runState.binSetup.store.animatState; // size of individual animat state in bytes
	clb.acstSrcStateSize = m_runState.binSetup.store.aeState; // size of inidivual acoustic source state size in bytes
	clb.outputByTime = Sce.user.output.outputByTime; // TRUE if output format is by time, FALSE if by animat.
	clb.animatIndex = AnimatIndex;
	clb.numSaveIterations = Sce.numSaveIterations;
	

	bytePos = m_fileManagerStatic.CalculateAnimatStateBufferLocation(clb);

	if(m_memLocReportFd != NULL)
	{
		m_bufferWrite++;
		fprintf_s(m_memLocReportFd, "%6d %6d %07I64d\n", AnimatIndex, m_bufferWrite, bytePos);

		if(m_bufferWrite % Sce.totalNumAnimats == 0)
		{
			fclose(m_memLocReportFd);
			m_memLocReportFd = NULL;
			++m_memLocReportCnt;
			sprintf_s(szTempBuff, sizeof(szTempBuff), "%s%05d.mem", m_szMemReportFileName, m_memLocReportCnt);

			if(0 != fopen_s(&m_memLocReportFd, szTempBuff, "wt"))
				m_memLocReportFd = NULL;
		}
	}

	if(Sce.user.output.animat.ID == TRUE)
	{
		_ASSERT(bytePos < m_runState.bytes);
		memcpy(&m_runState.a[bytePos], &AS->animatID, sizeof(AS->animatID));
		bytePos += sizeof(AS->animatID);
	}
	if(Sce.user.output.animat.timeOfDay == TRUE)
	{
		_ASSERT(bytePos < m_runState.bytes);
		memcpy(&m_runState.a[bytePos], &AS->clock, sizeof(AS->clock));
		bytePos += sizeof(AS->clock);
	}
	if(Sce.user.output.animat.coordinate == TRUE)
	{
		_ASSERT(bytePos < m_runState.bytes);
		memcpy(&m_runState.a[bytePos], &AS->lat, sizeof(AS->lat));
		bytePos += sizeof(AS->lat);

		_ASSERT(bytePos < m_runState.bytes);
		memcpy(&m_runState.a[bytePos], &AS->lon, sizeof(AS->lon));
		bytePos += sizeof(AS->lon);
	}
	if(Sce.user.output.animat.depth == TRUE)
	{
		_ASSERT(bytePos < m_runState.bytes);
		memcpy(&m_runState.a[bytePos], &AS->depth, sizeof(AS->depth));
		bytePos += sizeof(AS->depth);
	}
	if(Sce.user.output.animat.bearing == TRUE)
	{
		_ASSERT(bytePos < m_runState.bytes);
		memcpy(&m_runState.a[bytePos], &AS->bearing, sizeof(AS->bearing));
		bytePos += sizeof(AS->bearing);
	}
	if(Sce.user.output.animat.diveRate == TRUE)
	{
		_ASSERT(bytePos < m_runState.bytes);
		memcpy(&m_runState.a[bytePos], &AS->diveRate, sizeof(AS->diveRate));
		bytePos += sizeof(AS->diveRate);
	}
	if(Sce.user.output.animat.travelRate == TRUE)
	{
		_ASSERT(bytePos < m_runState.bytes);
		memcpy(&m_runState.a[bytePos], &AS->travelRate, sizeof(AS->travelRate));
		bytePos += sizeof(AS->travelRate);
	}
	if(Sce.user.output.animat.aeCmltve == TRUE)
	{
		_ASSERT(bytePos < m_runState.bytes);
		memcpy(&m_runState.a[bytePos], &AS->aeCmltve, sizeof(AS->aeCmltve));
		bytePos += sizeof(AS->aeCmltve);
	}
	if(Sce.user.output.animat.aeMoment == TRUE)
	{
		_ASSERT(bytePos < m_runState.bytes);
		memcpy(&m_runState.a[bytePos], &AS->aeMoment, sizeof(AS->aeMoment));
		bytePos += sizeof(AS->aeMoment);
	}
	if(Sce.user.output.animat.aeRelAngle == TRUE)
	{
		_ASSERT(bytePos < m_runState.bytes);
		memcpy(&m_runState.a[bytePos], &AS->aeRelAngle, sizeof(AS->aeRelAngle));
		bytePos += sizeof(AS->aeRelAngle);
	}
	if(Sce.user.output.animat.aeTimeAvrt == TRUE)
	{
		_ASSERT(bytePos < m_runState.bytes);
		memcpy(&m_runState.a[bytePos], &AS->aeTimeAvrt, sizeof(AS->aeTimeAvrt));
		bytePos += sizeof(AS->aeTimeAvrt);
	}
	if(Sce.user.output.animat.bathyDepth == TRUE)
	{
		_ASSERT(bytePos < m_runState.bytes);
		memcpy(&m_runState.a[bytePos], &AS->bathyDepth, sizeof(AS->bathyDepth));
		bytePos += sizeof(AS->bathyDepth);
	}
	if(Sce.user.output.animat.salinity == TRUE)
	{
		_ASSERT(bytePos < m_runState.bytes);
		memcpy(&m_runState.a[bytePos], &AS->salinity, sizeof(AS->salinity));
		bytePos += sizeof(AS->salinity);
	}
	if(Sce.user.output.animat.temperature == TRUE)
	{
		_ASSERT(bytePos < m_runState.bytes);
		memcpy(&m_runState.a[bytePos], &AS->temperature, sizeof(AS->temperature));
		bytePos += sizeof(AS->temperature);
	}

	if(Sce.user.output.animat.packedData == TRUE)
	{
		_ASSERT(bytePos < m_runState.bytes);
		memcpy(&m_runState.a[bytePos], &AS->packedData, sizeof(AS->packedData));
		bytePos += sizeof(AS->packedData);
	}

	if(Sce.user.output.animat.targetDepth == TRUE)
	{
		_ASSERT(bytePos < m_runState.bytes);
		memcpy(&m_runState.a[bytePos], &AS->targetDepth, sizeof(AS->targetDepth));
		bytePos += sizeof(AS->targetDepth);
	}
	if(Sce.user.output.animat.calcDepth == TRUE)  //30
	{
		_ASSERT(bytePos < m_runState.bytes);
		memcpy(&m_runState.a[bytePos], &AS->calcDepth, sizeof(AS->calcDepth));
		bytePos += sizeof(AS->calcDepth);
	}
	if(Sce.user.output.animat.xyDistance == TRUE) //41
	{
		_ASSERT(bytePos < m_runState.bytes);
		memcpy(&m_runState.a[bytePos], &AS->xDistance, sizeof(AS->xDistance));
		bytePos += sizeof(AS->xDistance);

		_ASSERT(bytePos < m_runState.bytes);
		memcpy(&m_runState.a[bytePos], &AS->yDistance, sizeof(AS->yDistance));
		bytePos += sizeof(AS->yDistance);
	}
	if(Sce.user.output.animat.risk == TRUE) //42
	{
		_ASSERT(bytePos < m_runState.bytes);
		memcpy(&m_runState.a[bytePos], &AS->risk, sizeof(AS->risk));
		bytePos += sizeof(AS->risk);
	}

	return OK;
}

// iterationLengthOfBuffer
// numStatesAlreadyWritten

// animat state size
// acoustic srs state size






FM_BUFFERSTATE CFileManager::GetBufferStatus()
{
	FM_BUFFERSTATE s = {0};
	s.numBytes = m_runState.bytes; // the size of the allocated buffer in bytes.
	s.bufferCycleCount = m_runState.bufferCycleCount; // the number of times the buffer has been flushed
	s.bufferIterationLevel = m_runState.bufferIterationLevel; // number of iterations currently stored in the buffer.
	s.fileIterationRWCount = m_runState.fileIterationRWCount; // number of iterations currently written to file.
	//s.animatStateSize = m_runState.animatStateSize; // size, in bytes, of each animat state saved to file as a result of how the scenario was set up.
	//s.acousticSrcStateSize = m_runState.acousticSrcStateSize; // size, in bytes, of each acoustic state saved to file a a result of how the scenario was set up
	s.animatStateSize = m_runState.binSetup.store.animatState; // size, in bytes, of each animat state saved to file as a result of how the scenario was set up.
	s.acousticSrcStateSize = m_runState.binSetup.store.aeState; // size, in bytes, of each acoustic state saved to file a a result of how the scenario was set up
	s.bufferIterationCapacity = m_runState.bufferIterationCapacity; // number of iterations the buffer can hold
	return s;
}


// Determines requried space file pointers resulting from the current scenario configuration.



RESLT CFileManager::FillBinFileStatsRegion(const SCENARIOPARAMS *pSce, TAKE *SceTakes, TAKESTATS *SpeTakes)
{
	DWORD bytes, totalBytes=0;

	if(pSce->user.output.enabled == FALSE || pSce->user.output.headerInf.postRunAnalysis == FALSE)
		return OK;

	_ASSERT(m_runState.hdl != NULL);
	if(m_runState.hdl == NULL)
		return FILE_NOT_OPEN_ERROR;

	// Set the file handle to the post-analysis location in the file then write to file.
	m_staticLib.MySetFilePointer(m_runState.hdl, __int64(m_runState.binSetup.fp.postAnalysis), FILE_BEGIN);

	// Write it to file
	if(FALSE == WriteFile(m_runState.hdl, SceTakes, sizeof(TAKE), &bytes, NULL))
		return UninitializeRun(FILEWRITE_ERROR);
	totalBytes += bytes;
	if(FALSE == WriteFile(m_runState.hdl, SpeTakes, sizeof(TAKESTATS) * pSce->numSpecies, &bytes, NULL))
		return UninitializeRun(FILEWRITE_ERROR);
	totalBytes += bytes;
	_ASSERT(totalBytes == m_runState.binSetup.store.postAnalysis);

	return OK;
}




RESLT CFileManager::LoadScenario(TCHAR *szFileName,
								 SCENARIOPARAMS *pSce,
								 ENVDATA *pEnvData,
								 CListManager <CSpecies> *pSpeciesList/*,
								 CFileManager *pFileMgr*/)
{
	RESLT res = OK;

	HANDLE hd;
	DWORD bytes;
	DWORD i;
	TCHAR fileID[SIZE_16];
	_fSCENARIOPARAMS params;

	//---------------------------------------------------------------------//
	// Create the binary file, read the scenario header information into it.	
	//---------------------------------------------------------------------//
	hd = CreateFile(szFileName, GENERIC_READ, NULL, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
	if(hd == INVALID_HANDLE_VALUE)
		return OPENFILEREAD_ERROR;

	// Verify the first 16 bytes are the file identifier
	if(ReadFile(hd, fileID, sizeof(fileID), &bytes, NULL) == FALSE || bytes != sizeof(fileID))
	{
		CloseHandle(hd);
		return FILEREAD_ERROR;
	}
	if(strcmp(fileID, SZ_OUTPUTFILEID) != 0)
	{
		CloseHandle(hd);
		return OBSOLETE_3MBS_VERSION;
	}

	if(INVALID_SET_FILE_POINTER == m_staticLib.MySetFilePointer(hd, 0, FILE_BEGIN))
	{
		CloseHandle(hd);
		return SETFILEPOINTER_ERROR;
	}

	if(ReadFile(hd, &params, sizeof(params), &bytes, NULL) == FALSE)
	{
		CloseHandle(hd);
		return FILEREAD_ERROR;
	}

	//------------------------------------------------------------------------------//
	// Transfer data in the Scenario Header struct into member variables, verify the
	// header string is correct.
	//------------------------------------------------------------------------------//
	if(params.libVerSuper != MMBSLIB_VERSION_SUPER)// || params.libVerSub != MMBSLIB_VERSION_SUB)
	{
		CloseHandle(hd);
		return OBSOLETE_3MBS_VERSION;
	}

	//-----------------------------------------------------------------------------//
	// Simutaniously instantiate and read in each saved instance of class CSpecies.
	//-----------------------------------------------------------------------------//
	for(i=0; i<params.numSpecies && res == OK; i++)
	{
		if(OK != (res = pSpeciesList->Add()->LoadFromBinFile(hd)))
			return res;
	}

	pSce->intrvlOutptLim.enabled = params._fIntrvlOutputLimitEnabled;
	pSce->intrvlOutptLim.start = params._fIntrvlOutputLimitStart;
	pSce->intrvlOutptLim.interval = params._fIntrvlOutputLimitValue;
	//--------------------------------------------------------------------------//
	// Read environmental models to file (bathymetry, salinity, and temperature.
	//--------------------------------------------------------------------------//
	if(res == OK)
		res = pEnvData->bathymetry.LoadFromBinFile(hd);
	if(res == OK)
		res = pEnvData->salinity.LoadFromBinFile(hd);
	if(res == OK)
		res = pEnvData->temperature.LoadFromBinFile(hd);


	// Read the list of iterations to be saved
	if(res == OK)
		res = m_csvListMgr.Load(hd, params.acstSrceLimitOutput);

	CloseHandle(hd);

	if(res == OK)
		*pSce = m_fileManagerStatic.ConvertScenarioFormat(params);

	return res;
}

RESLT CFileManager::SaveScenario(TCHAR *szFileName,
								 const SCENARIOPARAMS *pSce,
								 ENVDATA *pEnvData,
								 CListManager <CSpecies> *pSpeciesList/*,
								 CFileManager *pFileMgr*/)
{
	RESLT res = OK;
	HANDLE hd;
	DWORD bytes;
	DWORD i;

	_fSCENARIOPARAMS params = m_fileManagerStatic.ConvertScenarioFormat(*pSce, pEnvData, pSpeciesList);
	params.libVerSuper = MMBSLIB_VERSION_SUPER;
	params.libVerSub = MMBSLIB_VERSION_SUB;

	strncpy_s(params.fileIdentifier, sizeof(params.fileIdentifier), SZ_OUTPUTFILEID, SIZE_16); 

	//params.speciesVersion = CombineSuperAndSubVer(MMMBLIB_SPECIES_VERSION_SUPER,MMMBLIB_SPECIES_VERSION_SUB);
	params.speciesVerSuper = MMMBLIB_SPECIES_VERSION_SUPER;
	params.speciesVerSub = MMMBLIB_SPECIES_VERSION_SUB;

	// The binary output version really has no meaning here but is included to be complete.
	params.outputVerSuper = MMMBLIB_BINOUTPUT_VERSION_SUPER;
	params.outputVerSub = MMMBLIB_BINOUTPUT_VERSION_SUB;

	params._fIntrvlOutputLimitEnabled = pSce->intrvlOutptLim.enabled;
	params._fIntrvlOutputLimitStart = pSce->intrvlOutptLim.start;
	params._fIntrvlOutputLimitValue = pSce->intrvlOutptLim.interval;



	_ASSERT(pSce->numSpecies == (DWORD)pSpeciesList->Length());
	//------------------------------------------------------------------//
	// Create the binary file, save the scenario header information to it.	
	//------------------------------------------------------------------//
	hd = CreateFile(szFileName, GENERIC_WRITE, 0, 0, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
	if(INVALID_HANDLE_VALUE == hd)
		return OPENFILEWRITE_ERROR;

	if(0 == WriteFile(hd, &params, sizeof(params), &bytes, NULL))
	{
		CloseHandle(hd);
		return FILEWRITE_ERROR;
	}

	//----------------------------------------------------------------------------------//
	// Have each instance of class CSpecies to write itself to file.
	//----------------------------------------------------------------------------------//
	for(i=0; i<params.numSpecies; i++)
	{
		if(OK != (res = pSpeciesList->Get(i)->SaveToBinFile(hd)))
			return res;
	}


	//----------------------------------------------------------------------------//
	// Write environmental models to file (bathymetry, salinity, and temperature).
	//----------------------------------------------------------------------------//
	if(OK != (res = pEnvData->bathymetry.SaveToBinFile(hd, &bytes)))
		return res;
	if(OK != (res = pEnvData->salinity.SaveToBinFile(hd, &bytes)))
		return res;
	if(OK != (res = pEnvData->temperature.SaveToBinFile(hd, &bytes)))
		return res;

	// Write the list of iterations to be saved
	if(res == OK)
		res = m_csvListMgr.Save(hd, params.acstSrceLimitOutput);

	CloseHandle(hd);
	return res;
}
