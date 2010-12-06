
#include "FileExtracter.h"
#include "3mbslib.h"

CFileExtracter::CFileExtracter(void)
{
	memset(m_szFileName, 0, sizeof(m_szFileName));
	memset(m_szFileTitle, 0, sizeof(m_szFileTitle));
	memset(m_szFileTrakFileSubPath, 0, sizeof(m_szFileTrakFileSubPath));
	memset(&m_setup, 0, sizeof(m_setup));
	memset(&m_speSum, 0, sizeof(m_speSum));
	memset(&m_speSum, 0, sizeof(m_speSum));
	memset(&m_statAnalysis, 0, sizeof(m_statAnalysis));
	m_hdl = NULL;
	m_abort = FALSE;
	m_speciesStats = NULL;

	//------------------------//
	// Thread Management
	//------------------------//
	m_runThread.SetIRunnablePointer((IRunnable *) this);
	m_runThread.m_thread1Running = FALSE;
	m_runThread.m_thread3Running = FALSE;
	m_runThread.m_thread2Running = FALSE;
	m_state.activity = EXTRACTOR_IDLE_PRERUN;
	//------------------------//
}

CFileExtracter::~CFileExtracter(void)
{
}

FESETUP CFileExtracter::GetSetup()
{
	return m_setup;
}

FESTATE CFileExtracter::GetState()
{
	return m_state;
}


void CFileExtracter::SetSpecificAnimatEnabled(BOOL EnableVal)
{
	m_setup.specificAnimatSet = EnableVal;
	m_setup.rangeGroup = 0;
}

void CFileExtracter::SetSpecificAnimatIndex(DWORD AnimatIndex)
{
	m_setup.specificAnimat = AnimatIndex;
	m_setup.rangeGroup = 0;
}

void CFileExtracter::SetSpecificAnimatGroup(DWORD AnimatGroup)
{
	m_setup.rangeGroup = AnimatGroup;
	m_setup.specificAnimatSet = FALSE;
}

void CFileExtracter::Abort()
{
	m_abort = TRUE;
}

void CFileExtracter::CloseThread1ExtractProcess(RESLT Res)
{
	UINT32 i;
	m_abort = TRUE;
	if(m_speSum.ansm != NULL)
		delete [] m_speSum.ansm;
	m_speSum.ansm = NULL;

	if(m_speSum.spe != NULL)
	{
		for(i=0; i<m_setup.sceParams.numSpecies; /*m_speSum.spe[i].inf.description.numBehaviors;*/ i++)
		{
			if(m_speSum.spe[i].behName != NULL)
				delete [] m_speSum.spe[i].behName;
			m_speSum.spe[i].behName = NULL;
		}
		delete [] m_speSum.spe;
	}
	m_speSum.spe = NULL;

	if(m_speciesStats != NULL)
	{
		delete [] m_speciesStats;
		m_speciesStats = NULL;
	}


	// In the future, if additional threads are used, will have to wait for them to cease.
	_ASSERT(m_runThread.m_thread2Running == FALSE);
	_ASSERT(m_runThread.m_thread3Running == FALSE);

	Uninitialize(Res);
	m_runThread.m_thread1Running = FALSE;
	m_state.activity = EXTRACTOR_IDLE_POSTRUN;
}

RESLT CFileExtracter::Uninitialize(RESLT Res)
{
	m_abort = FALSE;
	_ASSERT(m_speSum.ansm == NULL && m_speSum.spe == NULL);

	if(m_hdl != NULL && m_hdl != INVALID_HANDLE_VALUE)
		CloseHandle(m_hdl);
	m_hdl = NULL;

	memset(m_szFileName, 0, sizeof(m_szFileName));
	memset(m_szFileTitle, 0, sizeof(m_szFileTitle));
	memset(m_szFileTrakFileSubPath, 0, sizeof(m_szFileTrakFileSubPath));

	m_state.activity = EXTRACTOR_IDLE_POSTRUN;

	if(m_state.status == OK)
		m_state.status = Res;
	return Res;
}

RESLT CFileExtracter::OpenFile()
{
	m_hdl = CreateFile(m_szFileName, GENERIC_READ, NULL, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
	if(m_hdl == INVALID_HANDLE_VALUE || m_hdl == NULL)
		return Uninitialize(OPENFILEREAD_ERROR);

#if 0
	// Debug region
	_ExtractBinaryResultsIntoTextFiles();
	// Debug region
#else
	// Start the thread, return once thread indicates it is running
	m_runThread.StartThread1();
	while(m_runThread.m_thread1Running == FALSE)
		Sleep(1);
#endif
	return OK;
}

RESLT CFileExtracter::ExtractBinaryResultsForPlayback(OPENFILENAME *ofn)
{
	m_displayMode = TRUE;
	return ExtractBinaryResultsIntoTextFiles(ofn, TRUE, FALSE, 0);
}

RESLT CFileExtracter::ExtractBinaryResultsForPlayback(TCHAR *szFileName)
{
	m_displayMode = TRUE;
	return ExtractBinaryResultsIntoTextFiles(szFileName, TRUE, FALSE, 0);
}



RESLT CFileExtracter::ExtractBinaryResultsIntoTextFiles(TCHAR *szFileName,
														BOOL OutputToSubdirectory,
														BOOL SplitTxtFiles,
														int NumIterationsPerFile)
{
	if(m_runThread.m_thread1Running == TRUE)
		return ALREADYRUNNING_ERROR;

	m_state.activity = EXTRACTOR_IDLE_PRERUN;

	m_splitTextOutput = SplitTxtFiles;
	m_iterationsPerFile = NumIterationsPerFile;

	strncpy_s(m_szFileName, sizeof(m_szFileName), szFileName, sizeof(m_szFileName));
	m_staticLib.GetPathAndFileTitleFromFileName(szFileName,
									m_szFileTrakFileSubPath,
									sizeof(m_szFileTrakFileSubPath),
									m_szFileTitle,
									sizeof(m_szFileTitle));

	// Create the name of the folder that animat tracks will be saved in if paramater
	// OutputToSubdirectory is set to TRUE.  Otherwise m_szFileTrakFileSubPath
	// will simply hold the current directory and a new directory will not be
	// created.
	if(OutputToSubdirectory == TRUE)
	{
		strcpy_s(m_szFileTrakFileSubPath, sizeof(m_szFileTrakFileSubPath), m_szFileTitle);
		m_staticLib.RemoveExtension(m_szFileTrakFileSubPath);
	}

	// Open the binary file and launch the thread
	return OpenFile();
}

RESLT CFileExtracter::ExtractBinaryResultsIntoTextFiles(OPENFILENAME *ofn,
														BOOL OutputToSubdirectory,
														BOOL SplitTxtFiles,
														int NumIterationsPerFile)
{
	return ExtractBinaryResultsIntoTextFiles(ofn->lpstrFile,
											 OutputToSubdirectory,
											 SplitTxtFiles,
											 NumIterationsPerFile);
}

void CFileExtracter::RunThread1()
{
	_ExtractBinaryResultsIntoTextFiles();
}


void CFileExtracter::_ExtractBinaryResultsIntoTextFiles()
{
	DWORD bytes;
	TCHAR fileID[SIZE_16];

	// Indicate this thread is running
	m_runThread.m_thread1Running = TRUE;

	// Verify the first 16 bytes are the file identifier
	if(ReadFile(m_hdl, fileID, sizeof(fileID), &bytes, NULL) == FALSE || bytes != sizeof(fileID))
		return CloseThread1ExtractProcess(FILEREAD_ERROR);;

	if(strcmp(fileID, SZ_OUTPUTFILEID) != 0)
		return CloseThread1ExtractProcess(OBSOLETE_3MBS_VERSION);

	if(INVALID_SET_FILE_POINTER == m_staticLib.MySetFilePointer(m_hdl, 0, FILE_BEGIN))
		return CloseThread1ExtractProcess(SETFILEPOINTER_ERROR);

	// Verify proper file fomatting.
	if(FALSE == m_fileManagerStatic.AssertBinaryOutFilePointers(m_hdl))
	{
		_ASSERT(FALSE);
		return CloseThread1ExtractProcess(FILEFORMAT_ERROR);
	}

	//----------------------------------------------------------------------------------//
	// Read in the scenario parameters.
	//--------------------------------//
	SetFilePointer(m_hdl, 0, NULL, FILE_BEGIN);
	memset(&m_setup.sceParams, 0, sizeof(_fSCENARIOPARAMS));
	if(m_abort == FALSE && 0 == ReadFile(m_hdl, &m_setup.sceParams, sizeof(_fSCENARIOPARAMS), &bytes, NULL))
		return CloseThread1ExtractProcess(FILEREAD_ERROR);

	// Translate the bit-wise representation of the file configuration into a
	// variable-represented one.
	m_setup.bin = m_fileManagerStatic.TranslateBinFileOutConfiguration(m_setup.sceParams.binOutStateItemConfig);
	//----------------------------------------------------------------------------------//

	// Skip past environmental data
	if(m_setup.bin.headerInf.bathyMap == TRUE)
	{
		if(OK != (m_state.status = m_staticEnvData.SkipOverInBinFile(m_hdl, &bytes)))
			return CloseThread1ExtractProcess(m_state.status);
	}
	if(m_setup.bin.headerInf.salinityMap == TRUE)
	{
		if(OK != (m_state.status = m_staticEnvData.SkipOverInBinFile(m_hdl, &bytes)))
			return CloseThread1ExtractProcess(m_state.status);
	}
	if(m_setup.bin.headerInf.temperatureMap == TRUE)
	{
		if(OK != (m_state.status = m_staticEnvData.SkipOverInBinFile(m_hdl, &bytes)))
			return CloseThread1ExtractProcess(m_state.status);
	}

	// Create the directory that the file tracks are to be stored into.
	CreateDirectory(m_szFileTrakFileSubPath, NULL);

	// Read Statistical data in, generate stats report.
	if(OK != (m_state.status = ReadPostRunStatsResults()))
		return CloseThread1ExtractProcess(m_state.status);

	// Animat to Species Association
	if(OK != (m_state.status = SetUpSpeciesSummaryAssociation()))
		return CloseThread1ExtractProcess(m_state.status);

	if(OK != (m_state.status = PrintPostRunStatsResults()))
		return CloseThread1ExtractProcess(m_state.status);

	//----------------------------------------------------------------------------------//

#if 1
	if(m_setup.bin.outputByTime == TRUE)
	{
		if(OK != (m_state.status = ExtractByIteration()))
			return CloseThread1ExtractProcess(m_state.status);
	}
	else// if(TRUE)// outputted by animat
	{
		if(OK != (m_state.status = ExtractByAnimat()))
			return CloseThread1ExtractProcess(m_state.status);
	}
#else
	if(TRUE)
	{
		if(OK != (m_state.status = ExtractIntoSingleFile()))
			return CloseThread1ExtractProcess(m_state.status);
	}
	else
	{
		if(OK != (m_state.status = ExtractIntoDisplayBuffer()))
			return CloseThread1ExtractProcess(m_state.status);
	}
#endif

	// Close everything down
	CloseThread1ExtractProcess(OK);
}

typedef struct ExtractByIterationStruct
{
	FILE *fd; // file descriptor
	DWORD fileCnt; // file count;
	DWORD offscreenCnt; // animat off screen count
	THRESHOLDBOOKKEEPING thrsh; // threshold bookkeeping
}EXTRCBYIT;

RESLT CFileExtracter::ExtractByIteration(/*DATEXTCT_SCE *SpeSumm*/)
{
	//RESLT res = OK;
	DWORD i; // Iteration counter
	DWORD groupCounter;
	DWORD a; // Animat Counter 
	BYTE *inBuffer = NULL;
	DWORD bytes;
	__int64 bypePos = 0;
	ANIMATSTATE_FILEOUT ast;
	CALCLOCINBUFF clb;
	FM_MEMALLOCINF mallc;
	ANIMATFILENAMING ani; // Animat file naming.
	PACKAGED_STATE_DATA psd;

	EXTRCBYIT *extctIt;
	BOOL    splitFileFlag = FALSE;
	DWORD	bytesToRead;


	//OUTPUTBUFFERSIZE abi;

//	FILE **fdBuff = NULL;
//	DWORD	*fileCnt = NULL; // tracks number of files opened for the current animal if multiple if output is being split into multiple files.
//	DWORD   *offscreenCount = NULL;
//	THRESHOLDBOOKKEEPING *thrsh;
//	ANIMATASSCN	*ansm = NULL;

	int rwres = 1; // Must be intialized before using.

	// Differences between saved by iteration (this function) and saved by animat.
	int	*itsWrittenThisFile = NULL; // tracks number of iterations written for the current file if user slected to split text output.

	//--------------------------------//
	// New Stuff added for this method
	DWORD numTxtFiles; // the number of files that will be kept open
	DWORD ansTrsltd = 0; // the number of animats translated from binary into text file.
	DWORD aniToTraslt = 0; // the number of animats to translate from binary to text file.

	//DWORD numAnimatsRemaining = 0;
	DWORD fdIndex;

	DWORD itsRead; // a count of the number of iterations read in so far
	DWORD itsToRead; // the total number of iterations to read.

	//--------------------------------//

	_ASSERT(m_state.activity == EXTRACTOR_IDLE_PRERUN);
	m_state.activity = EXTRACTOR_INIT;


	// Set the number of file descriptors for the animats that will open simultaneously.
	if(m_setup.sceParams.totalNumAnimats < MAXANIMATFILESOPEN)
		numTxtFiles = m_setup.sceParams.totalNumAnimats;
	else
		numTxtFiles = MAXANIMATFILESOPEN;

	//----------------------------------------------------------------------------------//
	// Allocate needed memory
	//-----------------------//
	if(NULL == (extctIt = new EXTRCBYIT[numTxtFiles]))
		return MEMALLOC_ERROR;

	if(m_splitTextOutput == TRUE && NULL == (itsWrittenThisFile = new int[numTxtFiles]))
	{
		delete [] extctIt;
		return MEMALLOC_ERROR;
	}

	//----------------------------------------------------------------------------------//
	// Allocate a buffer to read animat and acoustic exposure states into.
	//-------------------------------------------------------------------//
	mallc = m_fileManagerStatic.GetMemoryAllocationDetails(
				m_setup.sceParams.totalNumAnimats,	// total number of animats in the scenario
				1,									// total number of iterations saved to file
				m_setup.bin,						// binary setup of the scenario
				0);									// additional required bytes to be allocated later

	if(NULL == (inBuffer = (BYTE *)malloc(mallc.numBytes)))
	{
		delete [] extctIt;
		if(itsWrittenThisFile != NULL)
			delete itsWrittenThisFile;
		return MEMALLOC_ERROR;
	}

	//----------------------------------------------------------------------------------//
	m_state.currentAnimat = 0;
	m_state.currentIteration = 0;
	groupCounter = 0;
	for(ansTrsltd=0; ansTrsltd < m_setup.sceParams.totalNumAnimats && m_abort == FALSE; ansTrsltd += aniToTraslt)
	{
		// If there is a specific animat being translated (rather than all animats) check
		// if that animat falls within the number of animat text files that can be opened
		// at once.                      0          1 (0~499)            500
		//                               0          2 (500 ~ 999)        500
		groupCounter++;
		if(m_setup.rangeGroup > 0 && groupCounter < m_setup.rangeGroup)
		{
			ansTrsltd += numTxtFiles;
			continue;
		}
		else if(m_setup.specificAnimatSet == TRUE && ansTrsltd + numTxtFiles < m_setup.specificAnimat)
		{
			ansTrsltd += numTxtFiles;
			continue;
		}

		if(m_setup.rangeGroup > 0 && groupCounter > m_setup.rangeGroup)
			break;
		if(m_setup.specificAnimatSet == TRUE && ansTrsltd >= m_setup.specificAnimat + numTxtFiles)
			break;

		//------------------------------------------------------------------------------//
		// Translate from binary to text the lesser of the number of animats in the scenario
		// and the number of file descriptors to open at once.
		//----------------------------------------------------//
		aniToTraslt = m_setup.sceParams.totalNumAnimats - ansTrsltd;
		if(aniToTraslt > numTxtFiles)
			aniToTraslt = numTxtFiles;

		m_state.lastAnimat = ansTrsltd + 1;
		m_state.currentAnimat = ansTrsltd + aniToTraslt;
		m_state.currentIteration = 0;
		m_state.activity = EXTRACTOR_EXTRACTING;

		//------------------------------------------------------------------------------//
		// Each iteration clear out buffers 
		//-------------------//
		memset(inBuffer, 0, sizeof(BYTE) * mallc.numBytes);
		memset(extctIt, 0, sizeof(EXTRCBYIT) * numTxtFiles);

		if(m_splitTextOutput == TRUE)
			memset(itsWrittenThisFile, 0, sizeof(int) * numTxtFiles);

		//------------------------------------------------------------------------------//
		// Open the animat text files.
		//-----------------------------//
		for(a=ansTrsltd, fdIndex=0; fdIndex < aniToTraslt && m_abort == FALSE && m_state.status == OK; a++, fdIndex++)
		{
			// If a specfic animat to extract is requested but this isn't that animat continue.
			if(m_setup.specificAnimatSet == TRUE && m_setup.specificAnimat != a)
				continue;

			// Build the name of the file to be created, open the file, and initialize
			BuildAnimatTextFileTitle(
				ani.fileTitle,
				sizeof(ani.fileTitle),
				&m_setup,
				&m_speSum,
				a,							// Animat number
				0,							// Offscreen Count
				0,							// Split File Count
				m_splitTextOutput,			// Split File Enabled.
				m_szFileTitle);

			BuildAnimatTextFileName(
				ani.fileName,			// Buffer for animat file name (output).
				sizeof(ani.fileName),	// Size of animat file name buffer.
				ani.fileTitle,			// Animat file title (input)
				m_szFileTrakFileSubPath);	// Output file file name path

			m_state.status = 
				InitializeNewTextOutFile(
					ani.fileName,				// Text File Name.
					&extctIt[fdIndex].fd,			// Address of File Descriptor Pointer
					m_setup.bin.headerInf,			// State output configuraton
					m_setup.sceParams,		// Scenario Parms.
					m_speSum,					// Binary file information.
					a);						// Animat Number

			// Opening the file was unsuccessful 
			if(m_state.status != OK)
				break;
			Sleep(10);
		}

		// Set the file pointer back to the begining of where animat states occur
		m_staticLib.MySetFilePointer(m_hdl, m_setup.sceParams.diskInf.fp.animatState, FILE_BEGIN);

		// For each file read, process the animats.
		itsToRead = 0; // this gets reset within the following for-loop.
		for(itsRead=0; itsRead<m_setup.sceParams.saveIterationCnt && m_abort==FALSE && m_state.status==OK; itsRead += itsToRead)
		{
			// This is where states are read in one at a time.
			// Set the number of iterations to read to the lesser of the remaining number
			// of iterations saved to binary file or the number of iterations the buffer
			// is set to hold.
			itsToRead = mallc.bufferIterationCapacity;
			if(itsToRead > m_setup.sceParams.saveIterationCnt - itsRead)
				itsToRead = itsRead;

			bytesToRead = itsToRead * 
							(m_setup.sceParams.diskInf.store.animatState * m_setup.sceParams.totalNumAnimats +
							m_setup.sceParams.diskInf.store.aeState);

			rwres = ReadFile(m_hdl, inBuffer, bytesToRead, &bytes, 0);
			if(rwres == 0 ||bytesToRead != bytes)
				m_state.status = FILEREAD_ERROR;

			// Process each individual iteration read in
			clb.bufferIterationLen =  mallc.bufferIterationCapacity; // length of the buffer in iterations (not bytes)
			clb.numSaveIterations = m_setup.sceParams.saveIterationCnt;

			clb.numAnimats = m_setup.sceParams.totalNumAnimats; // total number of animats present
			
			clb.animatStateSize = m_setup.sceParams.diskInf.store.animatState; // size of a single animat state in bytes
			clb.acstSrcStateSize = m_setup.sceParams.diskInf.store.aeState; // size of inidivual acoustic source state size in bytes

			clb.outputByTime = m_setup.bin.outputByTime; //
			_ASSERT(clb.outputByTime == TRUE);
			//clb.numSaveIterations = 

			clb.animatIndex = 0;

			for(i=0; i<itsToRead && m_abort == FALSE && m_state.status == OK; i++)
			{
				clb.iterationWriteNum = i;
				clb.animatIndex = ansTrsltd;

				bypePos = m_fileManagerStatic.CalculateAnimatStateBufferLocation(clb);

				for(a=ansTrsltd, fdIndex=0; fdIndex < aniToTraslt && m_abort == FALSE; a++, fdIndex++)
				{

					// Reintialize per-iteration variables.
					splitFileFlag = FALSE;

					bypePos = BufferToAnimatState(&m_setup.bin.animat, &ast, bypePos, inBuffer);

					psd = m_classAnimatStatics.UnPackageStateData(ast.packedData);

					if(m_setup.specificAnimatSet == TRUE && a != m_setup.specificAnimat)
						continue;

					// Check if it is time to open a new file associated with this animat.  New files are
					// opened if either the animat goes off screen or if, in the configuration, the user
					// specificied to have a maximum number of iterations per file.  Animats going
					// off-screen are treated as entirely new animats and the new file creation has
					// priority over creating a new file because the maximum number of iterations has
					// been reached.
					if(psd.offScreenAnimatReset == TRUE)
					{
						// Animat went off screen, so restart it as a new one and open a new file.
						//offScreenFlag = TRUE;
						extctIt[fdIndex].thrsh.exposureCntA = 0;
						extctIt[fdIndex].thrsh.exposureCntB = 0;
						extctIt[fdIndex].thrsh.exposureCntBBeh = 0;
						extctIt[fdIndex].fileCnt = 0;
						extctIt[fdIndex].offscreenCnt++;
						fclose(extctIt[fdIndex].fd);
						extctIt[fdIndex].fd = NULL;
					}
					else
					{
						// Check if time to create a new file because the maximum number of
						// iterations per file is reached.
						if(m_splitTextOutput == TRUE && itsWrittenThisFile[fdIndex] == m_iterationsPerFile)
						{
							extctIt[fdIndex].fileCnt++;
							splitFileFlag = TRUE;
							fclose(extctIt[fdIndex].fd);
							extctIt[fdIndex].fd = NULL;
						}
					}


					// If that which the file descriptor points to is NULL, a new file
					// for this animat must be opened.  This happens if either the animat
					// went off screen or the maximum number of iterations per file have
					// been reached (both requiring a new file) 
					if(extctIt[fdIndex].fd==NULL && (m_setup.specificAnimatSet==FALSE || m_setup.specificAnimat==a))
					{
						// Open a new file for this animat.
						itsWrittenThisFile = 0;

						// Build the file name.
						BuildAnimatTextFileTitle(ani.fileTitle,
												 sizeof(ani.fileTitle),
												 &m_setup,
												 &m_speSum/*SpeSumm*/,
												 a,							// Animat number
												 extctIt[fdIndex].offscreenCnt,			// Offscreen Count
												 extctIt[fdIndex].fileCnt,					// Split File Count
												 m_splitTextOutput,  // Split File Enabled.
												 m_szFileTitle);

						BuildAnimatTextFileName(ani.fileName,			// Buffer for animat file name (output).
												sizeof(ani.fileName), // Size of animat file name buffer.
												ani.fileTitle,		// Animat file title (input)
												m_szFileTrakFileSubPath);		// Output file file name path

					
						if(OK != InitializeNewTextOutFile(ani.fileName,	// Text File Name.
												 &extctIt[fdIndex].fd,		// File Descriptor
												 m_setup.bin.headerInf,	// State output configuraton
												 m_setup.sceParams,			// Scenario Parms.
												 m_speSum/**SpeSumm*/,			// Binary file information.
												 a))				// Animat Number
						{

								continue; // problem occurred opening this file, so skip the animat.
						}
					}
					
					if(m_setup.specificAnimatSet == FALSE || m_setup.specificAnimat == a)
					{
						if(FALSE == PrintAnimatTextFileIteration(
													 extctIt[fdIndex].fd,
													 a,
													 &ast,
													 &psd,
													 &extctIt[fdIndex].thrsh,
													 &m_speSum/*SpeSumm*/,
													 m_setup.bin.headerInf.speInfAndAnimatAsscn))
						{
							break;
						}
					}
				}
				// Skip over Acoustic State data if the scenaro was configured to save it.
				if(m_setup.bin.AECoordinate == TRUE)
					bypePos += m_setup.sceParams.diskInf.store.aeState;
			}
			m_state.currentIteration++;
		}

		// Close the animat files
		for(fdIndex = 0; fdIndex < numTxtFiles; fdIndex++)
		{
			if(extctIt[fdIndex].fd != NULL)
				fclose(extctIt[fdIndex].fd);
			extctIt[fdIndex].fd = NULL;
		}
		Sleep(100);
	} // for(ansTrsltd=0; ansTrsltd < m_setup.sceParams.totalNumAnimats && m_abort == FALSE; ansTrsltd += aniToTraslt)

	delete [] extctIt;

	if(itsWrittenThisFile != NULL)
		delete [] itsWrittenThisFile;

	return OK;
}

RESLT CFileExtracter::ExtractByAnimat(/*DATEXTCT_SCE *SpeSumm*/)
{
	//RESLT res = OK;
	DWORD i; // Iteration counter
	DWORD a; // Animat Counter 
	DWORD groupCounter;
	DWORDLONG groupBytesToSkip;
	BYTE *inBuffer = NULL;
	DWORD bytes;
	__int64 bypePos;
	ANIMATSTATE_FILEOUT ast;
	FM_MEMALLOCINF mallc;
	ANIMATFILENAMING ani; // Animat file naming.
	PACKAGED_STATE_DATA psd;

	FILE *fd = NULL;
	int	iterationsWrittenThisFile; // tracks number of iterations written for the current file if user slected to split text output.

	THRESHOLDBOOKKEEPING thrsh;
	READCOUNT iterations;
	DWORD	fileCnt; // tracks number of files opened for the current animal if multiple if output is being split into multiple files.
	BOOL    offScreenFlag;
	BOOL    splitFileFlag;
	DWORD   offscreenCount = 0;
	DWORD	bytesToRead;
	int rwres = 1; // Must be intialized before using.

//	STATE_FILEOUTPUT_CONFIG sxtateOutputConfg;
//	OUTPUTBUFFERSIZE abi;
//	TCHAR *spaces = "               ";

	m_state.lastAnimat = 0;
	m_state.currentAnimat = 0;
	m_state.currentIteration = 0;

	_ASSERT(m_state.activity == EXTRACTOR_IDLE_PRERUN);
	m_state.activity = EXTRACTOR_INIT;

	//----------------------------------------------------------------------------------//
	// Allocate a buffer to read animat and acoustic exposure states into.
	//-------------------------------------------------------------------//
	// The magic number 1 in calls to DetermineBufferSize() are because only a single
	// animat's states from the entire run need be read in at a time.
	mallc = m_fileManagerStatic.GetMemoryAllocationDetails(
				1,									// total number of animats in the scenario
				m_setup.sceParams.saveIterationCnt,	// total number of iterations saved to file
				m_setup.bin,						// binary setup of the scenario
				0);									// additional required bytes to be allocated later

	if(NULL == (inBuffer = (BYTE *)malloc(mallc.numBytes)))
		return MEMALLOC_ERROR;
	//----------------------------------------------------------------------------------//
	groupCounter = 0;
	groupBytesToSkip = 0;
	for(a=0; a<m_setup.sceParams.totalNumAnimats && m_abort == FALSE && rwres > 0; a++)
	{
		if(m_setup.rangeGroup > 0)
		{
			// 'a' is animat count.  When a mod MAXANIMATFILESOPEN is zero advance the group counter.
			if(a % MAXANIMATFILESOPEN == 0)
			{
				groupCounter++;
				Sleep(1);
			}


			// If the current group of which animat 'a' falls into is greater than the
			// desired animat group to be extracted then done here so break out of the
			// for-loop.
			if(groupCounter > m_setup.rangeGroup)
				break;
		}

		fileCnt = 0;
		iterations.remainingToReadCurrentAnimat = m_setup.sceParams.saveIterationCnt;
		iterationsWrittenThisFile = 0;
		memset(&thrsh, 0, sizeof(THRESHOLDBOOKKEEPING));
		splitFileFlag = FALSE;
		offScreenFlag = FALSE;
		offscreenCount = 0;
		m_state.currentIteration = 0;
		m_state.lastAnimat = m_state.currentAnimat;
		m_state.currentAnimat++;

		m_state.activity = EXTRACTOR_EXTRACTING;


		if((m_setup.specificAnimatSet == FALSE && m_setup.rangeGroup == 0) ||
			(m_setup.specificAnimatSet == TRUE && m_setup.specificAnimat == a) ||
			(m_setup.rangeGroup > 0 && m_setup.rangeGroup == groupCounter))
		{

			// Build the name of the file to be created.
			BuildAnimatTextFileTitle(ani.fileTitle,
									 sizeof(ani.fileTitle),
									 &m_setup,
									 &m_speSum/*SpeSumm*/,
									 a,							// Animat number
									 0,							// Offscreen Count
									 0,							// Split File Count
									 m_splitTextOutput,  // Split File Enabled.
									 m_szFileTitle);

			BuildAnimatTextFileName(ani.fileName,			// Buffer for animat file name (output).
									sizeof(ani.fileName), // Size of animat file name buffer.
									ani.fileTitle,		// Animat file title (input)
									m_szFileTrakFileSubPath);		// Output file file name path
			m_state.status =
				InitializeNewTextOutFile(ani.fileName,	// Text File Name.
										 &fd,				// File Descriptor
										 m_setup.bin.headerInf,	// State output configuraton
										 m_setup.sceParams,			// Scenario Parms.
										 m_speSum/**SpeSumm*/,			// Binary file information.
										 a);				// Animat Number
		}

		if(m_state.status != OK)
			break; // Break out of the for-loop.

		//bytesToRead = 0;
		while(iterations.remainingToReadCurrentAnimat > 0 && m_abort == FALSE && rwres > 0)
		{
			// Read into the fileTitle, update the pointer.  If the iterations.remainingToReadCurrentAnimat number.
			// ReadFile() returns 0 if it fails.	
			if(iterations.remainingToReadCurrentAnimat <= mallc.bufferIterationCapacity)
			{
				bytesToRead = m_setup.sceParams.diskInf.store.animatState * iterations.remainingToReadCurrentAnimat;
				iterations.numToRead = iterations.remainingToReadCurrentAnimat;
			}
			else
			{
				bytesToRead = m_setup.sceParams.diskInf.store.animatState*mallc.bufferIterationCapacity;
				iterations.numToRead = mallc.bufferIterationCapacity;
			}


#if 1

			if((m_setup.specificAnimatSet == TRUE && m_setup.specificAnimat != a) ||
				(m_setup.rangeGroup != 0 && m_setup.rangeGroup != groupCounter))
			{
				m_staticLib.MySetFilePointer(m_hdl, bytesToRead, FILE_CURRENT);
				if(iterations.numToRead == iterations.remainingToReadCurrentAnimat || iterations.numToRead == mallc.bufferIterationCapacity)
					iterations.remainingToReadCurrentAnimat -= iterations.numToRead;
				else
					iterations.remainingToReadCurrentAnimat = 0;
				continue;
			}
#else
			// m_setup.rangeGroup not equaling zero means it's been set to a specific group.
			if((m_setup.rangeGroup != 0) && (m_setup.rangeGroup != groupCounter))
			{
				_ASSERT(m_setup.specificAnimatSet == FALSE);
				_ASSERT(m_setup.rangeGroup >= groupCounter);
				groupBytesToSkip += bytesToRead;
			}
			else if(m_setup.specificAnimatSet == TRUE && m_setup.specificAnimat != a)
			{
				_ASSERT(m_setup.rangeGroup == 0);
				m_staticLib.MySetFilePointer(m_hdl, bytesToRead, FILE_CURRENT);
			}

			if((m_setup.specificAnimatSet == TRUE && m_setup.specificAnimat != a) ||
				(m_setup.rangeGroup != 0 && m_setup.rangeGroup != groupCounter))
			{
				if(iterations.numToRead == iterations.remainingToReadCurrentAnimat || iterations.numToRead == mallc.bufferIterationCapacity)
					iterations.remainingToReadCurrentAnimat -= iterations.numToRead;
				else
					iterations.remainingToReadCurrentAnimat = 0;
				continue;
			}

			if(groupBytesToSkip != 0 && m_setup.rangeGroup != 0 && (m_setup.rangeGroup) == groupCounter)
			{
				groupBytesToSkip = 0;
				m_staticLib.MySetFilePointer(m_hdl, groupBytesToSkip, FILE_CURRENT); // groupBytesToSkip did not advance this loop.
			}

#endif

#if 0
			// Test
			if(m_setup.rangeGroup != 0 && m_setup.rangeGroup == groupCounter)
				groupBytesToSkip = groupBytesToSkip; // groupBytesToSkip did not advance this loop.
#endif

			// Read the iterations in.
			rwres = ReadFile(m_hdl, inBuffer, bytesToRead, &bytes,	0);
			if(rwres == 0 ||bytesToRead != bytes)
				m_state.status = FILEREAD_ERROR;

			// Read the current animat's state for each iteration.
			bypePos = 0;
			for(i=0; i<iterations.numToRead && m_abort == FALSE && m_state.status == OK; i++)
			{
				m_state.currentIteration++;
				// Extract the current state from the buffer, update the bype position.
				bypePos = BufferToAnimatState(&m_setup.bin.animat, &ast, bypePos, inBuffer);
				psd = m_classAnimatStatics.UnPackageStateData(ast.packedData);

				// Reintialize per-iteration variables.
				splitFileFlag = FALSE;
				offScreenFlag = FALSE;

				if(psd.offScreenAnimatReset == TRUE)
				{
					offScreenFlag = TRUE;

					// Start a new animat.
					thrsh.exposureCntA = 0;
					thrsh.exposureCntB = 0;
					thrsh.exposureCntBBeh = 0;
					fileCnt = 0;
					offscreenCount++;
				}
				else
				{
					// Creating a new file because animat went off screen has priority
					// over creating a new file because enough iterations have passed
					// to split the files.
					if(m_splitTextOutput == TRUE && iterationsWrittenThisFile == m_iterationsPerFile)
					{
						fileCnt++;
						splitFileFlag = TRUE;
					}
				}


				if((splitFileFlag == TRUE || offScreenFlag == TRUE) &&
					((m_setup.specificAnimatSet == FALSE && m_setup.rangeGroup == 0) ||
					(m_setup.specificAnimatSet == TRUE && m_setup.specificAnimat == a) ||
					(m_setup.rangeGroup > 0 && m_setup.rangeGroup == groupCounter)))
				{
					fclose(fd);
					iterationsWrittenThisFile = 0;

					BuildAnimatTextFileTitle(ani.fileTitle,
											 sizeof(ani.fileTitle),
											 &m_setup,
											 &m_speSum/*SpeSumm*/,
											 a,							// Animat number
											 offscreenCount,			// Offscreen Count
											 fileCnt,					// Split File Count
											 m_splitTextOutput,  // Split File Enabled.
											 m_szFileTitle);

					BuildAnimatTextFileName(ani.fileName,			// Buffer for animat file name (output).
											sizeof(ani.fileName), // Size of animat file name buffer.
											ani.fileTitle,		// Animat file title (input)
											m_szFileTrakFileSubPath);		// Output file file name path

					m_state.status =
						InitializeNewTextOutFile(ani.fileName,	// Text File Name.
												 &fd,				// File Descriptor
												 m_setup.bin.headerInf,	// State output configuraton
												 m_setup.sceParams,			// Scenario Parms.
												 m_speSum/**SpeSumm*/,			// Binary file information.
												 a);				// Animat Number
					if(OK != m_state.status)
						break; // Break out of the for-loop.

				}

				if((m_setup.specificAnimatSet == FALSE && m_setup.rangeGroup == 0) ||
					(m_setup.specificAnimatSet == TRUE && m_setup.specificAnimat == a) ||
					(m_setup.rangeGroup > 0 && m_setup.rangeGroup == groupCounter))
				{
					if(FALSE == PrintAnimatTextFileIteration(
												 fd,
												 a,
												 &ast,
												 &psd,
												 &thrsh,
												 &m_speSum /*SpeSumm*/,
												 m_setup.bin.headerInf.speInfAndAnimatAsscn))
					{
						break;
					}
				}
				iterationsWrittenThisFile++;
			}

			if(iterations.numToRead == iterations.remainingToReadCurrentAnimat || iterations.numToRead == mallc.bufferIterationCapacity)
				iterations.remainingToReadCurrentAnimat -= iterations.numToRead;
			else
				iterations.remainingToReadCurrentAnimat = 0;
		}

		fflush(NULL);
		if(fd != NULL && ((m_setup.specificAnimatSet == FALSE && m_setup.rangeGroup == 0) ||
			(m_setup.specificAnimatSet == TRUE && m_setup.specificAnimat == a) ||
			(m_setup.rangeGroup > 0 && m_setup.rangeGroup == groupCounter)))
			fclose(fd);

		if(OK != m_state.status)
			break; // Break out of the for-loop.

		Sleep(10);
	}

	if(inBuffer != NULL)
		free(inBuffer);

	if(rwres < 0)
		return FILEWRITE_ERROR;

	return OK;
}


RESLT CFileExtracter::ExtractIntoDisplayBuffer()
{
	DWORD s; // state counter
	BYTE *inBuffer = NULL;
	DWORD bytes;
	__int64 bypePos;
	ANIMATSTATE_FILEOUT ast;
	FM_MEMALLOCINF mallc;
	//ANIMATFILENAMING ani; // Animat file naming.
	PACKAGED_STATE_DATA psd;

	FILE *fd = NULL;
	int	iterationsWrittenThisFile; // tracks number of iterations written for the current file if user slected to split text output.

	THRESHOLDBOOKKEEPING thrsh;
	READCOUNT iterations;
	DWORD	fileCnt; // tracks number of files opened for the current animal if multiple if output is being split into multiple files.
	BOOL    offScreenFlag;
	BOOL    splitFileFlag;
	DWORD   offscreenCount = 0;
	DWORD	bytesToRead;
	int rwres = 1; // Must be intialized before using.

	int totalStates = m_setup.sceParams.totalNumAnimats * m_setup.sceParams.saveIterationCnt;

//	STATE_FILEOUTPUT_CONFIG m_setup.bin.headerInf;
//	OUTPUTBUFFERSIZE abi;



	//----------------------------------------------------------------------------------//
	// Allocate a buffer to read animat and acoustic exposure states into.
	//-------------------------------------------------------------------//
	// The magic number 1 in this call to DetermineBufferSize() is because only a single
	// animat's states is read in at a time.
	mallc = m_fileManagerStatic.GetMemoryAllocationDetails(
				m_setup.sceParams.totalNumAnimats,	// total number of animats in the scenario
				m_setup.sceParams.saveIterationCnt,	// total number of iterations saved to file
				m_setup.bin,						// binary setup of the scenario
				0);									// additional required bytes to be allocated later

	if(NULL == (inBuffer = (BYTE *)malloc(mallc.numBytes)))
		m_state.status = MEMALLOC_ERROR;
	//----------------------------------------------------------------------------------//

	fileCnt = 0;
	for(s=0; s<(DWORD)totalStates && m_abort == FALSE && rwres > 0 && m_state.status == OK; s++)
	{
		iterations.remainingToReadCurrentAnimat = m_setup.sceParams.saveIterationCnt;
		iterationsWrittenThisFile = 0;
		memset(&thrsh, 0, sizeof(THRESHOLDBOOKKEEPING));
		splitFileFlag = FALSE;
		offScreenFlag = FALSE;

		bytesToRead = m_setup.sceParams.diskInf.store.animatState;

		// Read the iterations in.
		rwres = ReadFile(m_hdl, inBuffer, bytesToRead, &bytes,	0);
		if(rwres == 0 ||bytesToRead != bytes)
			m_state.status = FILEREAD_ERROR;

		// See if time and need to skip over acoustic state data that follows animat states when
		// output is by iteration
		if((m_setup.bin.outputByTime == TRUE) && ((s+1)%m_setup.sceParams.totalNumAnimats) == 0)
			SetFilePointer(m_hdl, m_setup.sceParams.diskInf.store.aeState, NULL, FILE_CURRENT);;

		// Read the current animat's state for each iteration.
		bypePos = 0;

		// Extract the current state from the buffer, update the bype position.
		bypePos = BufferToAnimatState(&m_setup.bin.animat, &ast, bypePos, inBuffer);
		if(m_setup.bin.outputByTime == TRUE)
			bypePos += m_setup.sceParams.diskInf.store.aeState;
		psd = m_classAnimatStatics.UnPackageStateData(ast.packedData);

		// Reintialize per-iteration variables.
		splitFileFlag = FALSE;
		offScreenFlag = FALSE;

		if(psd.offScreenAnimatReset == TRUE)
		{
			offScreenFlag = TRUE;

			// Start a new animat.
			thrsh.exposureCntA = 0;
			thrsh.exposureCntB = 0;
			thrsh.exposureCntBBeh = 0;
			offscreenCount++;
		}

		if(FALSE == PrintAnimatTextFileIteration(fd,
									 0,
									 &ast,
									 &psd,
									 &thrsh,
									 &m_speSum/*SpeSumm*/,
									 FALSE))
		{
			break;
		}

		iterationsWrittenThisFile++;

		if(OK != m_state.status)
		{
			m_abort = TRUE;
			break; // Break out of the for-loop.
		}

	}

	fflush(NULL);
	if(fd != NULL)
		fclose(fd);

	if(inBuffer != NULL)
		free(inBuffer);

	if(rwres < 0)
		return FILEWRITE_ERROR;

	return m_state.status;
}

RESLT CFileExtracter::ExtractIntoSingleFile(/*DATEXTCT_SCE *SpeSumm*/)
{
	DWORD s; // state counter
	BYTE *inBuffer = NULL;
	DWORD bytes;
	__int64 bypePos;
	ANIMATSTATE_FILEOUT ast;
	FM_MEMALLOCINF mallc;
	ANIMATFILENAMING ani; // Animat file naming.
	PACKAGED_STATE_DATA psd;

	FILE *fd = NULL;
	int	iterationsWrittenThisFile; // tracks number of iterations written for the current file if user slected to split text output.

	THRESHOLDBOOKKEEPING thrsh;
	READCOUNT iterations;
	DWORD	fileCnt; // tracks number of files opened for the current animal if multiple if output is being split into multiple files.
	BOOL    offScreenFlag;
	BOOL    splitFileFlag;
	DWORD   offscreenCount = 0;
	DWORD	bytesToRead;
	int rwres = 1; // Must be intialized before using.

	int totalStates = m_setup.sceParams.totalNumAnimats * m_setup.sceParams.saveIterationCnt;

//	STATE_FILEOUTPUT_CONFIG m_setup.bin.headerInf;
//	OUTPUTBUFFERSIZE abi;



	//----------------------------------------------------------------------------------//
	// Allocate a buffer to read animat and acoustic exposure states into.
	//-------------------------------------------------------------------//
	// The magic number 1 in this call to DetermineBufferSize() is because only a single
	// animat's states is read in at a time.
	mallc = m_fileManagerStatic.GetMemoryAllocationDetails(
				1,									// total number of animats in the scenario
				1,									// total number of iterations saved to file
				m_setup.bin,						// binary setup of the scenario
				0);									// additional required bytes to be allocated later

	if(NULL == (inBuffer = (BYTE *)malloc(mallc.numBytes)))
		m_state.status = MEMALLOC_ERROR;
	//----------------------------------------------------------------------------------//

	fileCnt = 0;
	for(s=0; s<(DWORD)totalStates && m_abort == FALSE && rwres > 0 && m_state.status == OK; s++)
	{
		splitFileFlag = FALSE;

		if(m_setup.bin.outputByTime == FALSE)
		{
			// Output by animat
			if((s % m_setup.sceParams.saveIterationCnt) == 0)
				splitFileFlag = TRUE;
		}
		else
		{
			// Output by time.
			if((s % m_setup.sceParams.totalNumAnimats) == 0)
				splitFileFlag = TRUE;
		}


		if(splitFileFlag == TRUE)
		{
			if(fd != NULL)
				fclose(fd);
			// Build the name of the file to be created.
			BuildAnimatTextFileTitle(ani.fileTitle,
									 sizeof(ani.fileTitle),
									 &m_setup,
									 &m_speSum/*SpeSumm*/,
									 0,							// Animat number
									 0,							// Offscreen Count
									 fileCnt,							// Split File Count
									 TRUE,  // Split File Enabled.
									 m_szFileTitle);

			BuildAnimatTextFileName(ani.fileName,			// Buffer for animat file name (output).
									sizeof(ani.fileName), // Size of animat file name buffer.
									ani.fileTitle,		// Animat file title (input)
									m_szFileTrakFileSubPath);		// Output file file name path

			m_setup.bin.headerInf.speInfAndAnimatAsscn =FALSE;

			m_state.status =
				InitializeNewTextOutFile(ani.fileName,	// Text File Name.
										 &fd,				// File Descriptor
										 m_setup.bin.headerInf,	// State output configuraton
										 m_setup.sceParams,			// Scenario Parms.
										 m_speSum/**SpeSumm*/,			// Binary file information.
										 0);				// Animat Number
			if(m_state.status != OK)
				break; // Break out of the for-loop.

			fileCnt++;
		}


		iterations.remainingToReadCurrentAnimat = m_setup.sceParams.saveIterationCnt;
		iterationsWrittenThisFile = 0;
		memset(&thrsh, 0, sizeof(THRESHOLDBOOKKEEPING));
		splitFileFlag = FALSE;
		offScreenFlag = FALSE;

		bytesToRead = m_setup.sceParams.diskInf.store.animatState;

		// Read the iterations in.
		rwres = ReadFile(m_hdl, inBuffer, bytesToRead, &bytes,	0);
		if(rwres == 0 ||bytesToRead != bytes)
			m_state.status = FILEREAD_ERROR;

		// See if time and need to skip over acoustic state data that follows animat states when
		// output is by iteration
		if((m_setup.bin.outputByTime == TRUE) && ((s+1)%m_setup.sceParams.totalNumAnimats) == 0)
			SetFilePointer(m_hdl, m_setup.sceParams.diskInf.store.aeState, NULL, FILE_CURRENT);;

		// Read the current animat's state for each iteration.
		bypePos = 0;

		// Extract the current state from the buffer, update the bype position.
		bypePos = BufferToAnimatState(&m_setup.bin.animat, &ast, bypePos, inBuffer);
		if(m_setup.bin.outputByTime == TRUE)
			bypePos += m_setup.sceParams.diskInf.store.aeState;
		psd = m_classAnimatStatics.UnPackageStateData(ast.packedData);

		// Reintialize per-iteration variables.
		splitFileFlag = FALSE;
		offScreenFlag = FALSE;

		if(psd.offScreenAnimatReset == TRUE)
		{
			offScreenFlag = TRUE;

			// Start a new animat.
			thrsh.exposureCntA = 0;
			thrsh.exposureCntB = 0;
			thrsh.exposureCntBBeh = 0;
			offscreenCount++;
		}

		if(FALSE == PrintAnimatTextFileIteration(fd,
									 0,
									 &ast,
									 &psd,
									 &thrsh,
									 &m_speSum/*SpeSumm*/,
									 FALSE))
		{
			break;
		}

		iterationsWrittenThisFile++;

		if(OK != m_state.status)
		{
			m_abort = TRUE;
			break; // Break out of the for-loop.
		}

	}

	fflush(NULL);
	if(fd != NULL)
		fclose(fd);

	if(inBuffer != NULL)
		free(inBuffer);

	if(rwres < 0)
		return FILEWRITE_ERROR;

	return m_state.status;
}

	
TCHAR *CFileExtracter::BuildAnimatTextFileTitle(TCHAR *szAnimatFileTitle,
										 UINT32 BuffSize,
										 FESETUP *Setup,
										 DATEXTCT_SCE *Sce,
										 DWORD AnimatNum,
										 DWORD OffScreenCount,
										 DWORD SplitFileCount,
										 DWORD SplitFileEnabled,
										 const TCHAR *szSceFileTitle)
{
	UINT16 speMembership;
	SPECIESBINOUTINF *speBinOut;
	TCHAR *ind = "ind";
	TCHAR *pod = "pod";
	TCHAR szBuff[BUFFERED_MAX_PATH];

	_ASSERT(szAnimatFileTitle != NULL);
	strcpy_s(szAnimatFileTitle, BuffSize, szSceFileTitle);

	m_staticLib.RemoveExtension(szAnimatFileTitle);
	sprintf_s(szBuff, sizeof(szBuff), "%05d.%03d_%s", AnimatNum, OffScreenCount, szAnimatFileTitle);

	// See if species and animat associations to species was included in the binary output file.
	if(Setup->bin.headerInf.speInfAndAnimatAsscn == FALSE)
	{
		strcpy_s(szAnimatFileTitle, BuffSize, szBuff);
		if(SplitFileEnabled == TRUE)
		{
			sprintf_s(szBuff, sizeof(szBuff), "set%02d", SplitFileCount);
			strcat_s(szAnimatFileTitle, BuffSize, szBuff);
		}
		return szAnimatFileTitle;
	}

	speMembership = Sce->ansm[AnimatNum].speciesNumber;
	_ASSERT(speMembership < Setup->sceParams.numSpecies);
	speBinOut = &Sce->spe[speMembership].inf;

	// Check if the animat is an individual or a pod member.
	if(m_staticScenario.CompctDecompctPodMembershipInf(Sce->ansm[AnimatNum].compactInf).individual == TRUE)
	{
		// Individuals
		// Build a name for this animat's text file.

		if(strlen(Sce->spe[speMembership].inf.fileTitle) > 0)
		{
			//szBuff[0] = 0;
			sprintf_s(szAnimatFileTitle, BuffSize, "%s_%s%02d_%s%03d",
				szBuff,
				Sce->spe[speMembership].inf.fileTitle,
				speMembership+1,
				ind,
				Sce->ansm[AnimatNum].pod_id+1);

			_ASSERT(BuffSize >= strlen(szBuff));
			//strncpy_s(szAnimatFileTitle, BuffSize, szBuff, BuffSize-1);
		}
		else
		{
			// This is here for ESME
			sprintf_s(szAnimatFileTitle, BuffSize, "%s_%s%03d",
				szBuff,
				ind,
				Sce->ansm[AnimatNum].pod_id+1);
		}
	}
	else
	{
		// Pods
		if(strlen(Sce->spe[speMembership].inf.fileTitle) > 0)
		{
			sprintf_s(szAnimatFileTitle, BuffSize,
				"%s_%s%02d_%s%03d_ani%04d",
				szBuff,
				Sce->spe[speMembership].inf.fileTitle,
				speMembership+1,
				pod,
				Sce->ansm[AnimatNum].pod_id+1,
				Sce->ansm[AnimatNum].id+1);
		}
		else
		{
			// This is here for ESME
			sprintf_s(szAnimatFileTitle, BuffSize, "%s_%s%03d_ani%04d",
				szBuff,
				pod,
				Sce->ansm[AnimatNum].pod_id+1,
				Sce->ansm[AnimatNum].id+1);
		}
	}

	if(SplitFileEnabled == TRUE)
	{
		sprintf_s(szBuff, sizeof(szBuff), "set%02d", SplitFileCount);
		strcat_s(szAnimatFileTitle, BuffSize, szBuff);
	}
	return szAnimatFileTitle;
}

// Find out what this function does, write an explanation about it, then update it.
TCHAR *CFileExtracter::BuildAnimatTextFileName(TCHAR *szAnimatFileName,
										 UINT32 BuffSize,
										 const TCHAR *szAnimatFileTitle,
										 const TCHAR *szOutputFilePath)
{
	char szCurrentDirectory[MAX_PATH];
	char szFileTitleCopy[MAX_PATH];
	size_t localPathLen; // Lenth of the folder being created to store the extracted files into.
	size_t titleLen; // The length of the file title being created
	size_t currentDirLen; // Current directory length.
	size_t fileLen;
	int index = 0;
	const DWORD MAXPATH = MAX_PATH;

	strcpy_s(szFileTitleCopy, szAnimatFileTitle);

	GetCurrentDirectory(MAXPATH, szCurrentDirectory);
	currentDirLen = strlen(szCurrentDirectory);
	localPathLen = strlen(szOutputFilePath);
	titleLen = strlen(szFileTitleCopy);

	fileLen = currentDirLen + localPathLen + titleLen + 2 + 4; // 2 for the two additional "\" and 4 for the ".trk"

	// Not sure why I needed the -1 but didn't work without it.
	while(fileLen > MAXPATH-1)
	{
		index++;
		fileLen--;
		titleLen = strlen(szFileTitleCopy);
		szFileTitleCopy[titleLen-1] = 0;
	}


	sprintf_s(szAnimatFileName, BuffSize, "%s\\%s", szOutputFilePath, szFileTitleCopy);
	strcat_s(szAnimatFileName, BuffSize, ".trk");

	fileLen = currentDirLen + strlen(szAnimatFileName) + 1; // plus 1 for "\"
	return szAnimatFileName;
}



RESLT CFileExtracter::InitializeNewTextOutFile(
										  const TCHAR *szFileName,
										  FILE **Fd,
										  STATE_FILEOUTPUT_CONFIG StateOutputConfg,
										  _fSCENARIOPARAMS SceParms,
										  DATEXTCT_SCE ScenarioSummary,
										  DWORD AnimatNum)
{

	UINT16 speMembership;
	SPECIESBINOUTINF *speBinOutInf = NULL;
	int rwres = 1;
	TCHAR sz1[SIZE_16] = {"----"};
	TCHAR sz2[SIZE_16] = {"----"};
	TCHAR sz3[SIZE_16] = {"----"};

	if(StateOutputConfg.speInfAndAnimatAsscn == TRUE)
	{
		speMembership = ScenarioSummary.ansm[AnimatNum].speciesNumber;
		speBinOutInf = &ScenarioSummary.spe[speMembership].inf;
	}

	// Open a text file associated with this animal, print the header information.
	if(0 != fopen_s(Fd, szFileName, "w"))
		return OPENTEXTOUTPUTFILE_ERROR;


	// fprintf returns a negative value if it fails.
	if(StateOutputConfg.speInfAndAnimatAsscn == TRUE)
	{
		//sprintf_s(sz1, sizeof(sz1), "%d", SceParms.acousticSourceLevel);
		//sprintf_s(sz2, sizeof(sz2), "%d", SceParms.acousticSourceBeginIteration);
		//sprintf_s(sz3, sizeof(sz3), "%d", SceParms.acousticSourceDutyPeriod);

		rwres = fprintf(*Fd, TXTHEADERPre,
			SceParms.libVerSuper,
			SceParms.libVerSub,
			speBinOutInf->description.speVerSuper,
			speBinOutInf->description.speVerSub,
			speBinOutInf->description.id,
			SceParms.group[speBinOutInf->description.group].szGroup,
			speBinOutInf->description.shoreFollowDepth,
			sz1,
			sz2,
			sz3);
	}
	else
	{
		rwres = fprintf(*Fd, TXTHEADERPre2, SceParms.libVerSuper, SceParms.libVerSub, sz1, sz2, sz3);
	}

	if(rwres > 0) rwres = fprintf(*Fd, "\n");

	// Without brackets this if-else combo doesn't work properly.
	if(StateOutputConfg.speInfAndAnimatAsscn == TRUE && speBinOutInf->description.group == SPECIALCONSIDRTNS)
	{
		if(rwres > 0) rwres = fprintf(*Fd, TXTHEADER0_SPECONS);
	}
	else
	{
		if(rwres > 0) rwres = fprintf(*Fd, TXTHEADER0);
	}

	if(StateOutputConfg.speInfAndAnimatAsscn == TRUE)
	{
		if(rwres > 0)
		{
			if(speBinOutInf->description.group == SPECIALCONSIDRTNS)
			{
				rwres = fprintf(*Fd, TXTHEADER1_SPECONS,
					SceParms.group[speBinOutInf->description.group].lvlAphys,
					SceParms.group[speBinOutInf->description.group].lvlBphys,
					//SceParms.acousticSourceLevel,
					//SceParms.acousticSourceBeginIteration,
					//SceParms.acousticSourceDutyPeriod,
					SceParms.group[speBinOutInf->description.group].lvlBBeh_RiskA); 
			}
			else
			{
				rwres = fprintf(*Fd, TXTHEADER1,
					SceParms.group[speBinOutInf->description.group].lvlAphys,
					SceParms.group[speBinOutInf->description.group].lvlBphys);
					//SceParms.acousticSourceLevel,
					//SceParms.acousticSourceBeginIteration,
					//SceParms.acousticSourceDutyPeriod);
					//SceParms.group[speBinOutInf->description.group].lvlBBeh_RiskA); 
			}
		}
	}
	else
	{
		if(rwres > 0) rwres = fprintf(*Fd, TXTHEADER1x);
	}

	if(StateOutputConfg.speInfAndAnimatAsscn == TRUE && speBinOutInf->description.group == SPECIALCONSIDRTNS)
	{
		if(rwres > 0) rwres = fprintf(*Fd, TXTHEADER2_SPECONS);
		if(rwres > 0) rwres = fprintf(*Fd, TXTHEADER3_SPECONS);
	}
	else
	{
		if(rwres > 0) rwres = fprintf(*Fd, TXTHEADER2);
		if(rwres > 0) rwres = fprintf(*Fd, TXTHEADER3);
	}

	if(rwres <= 0)
		return OPENFILEWRITE_ERROR;
	return OK;
}



BOOL CFileExtracter::PrintAnimatTextFileIteration(FILE *Fd, DWORD AnimatNumber, ANIMATSTATE_FILEOUT *pASFO, PACKAGED_STATE_DATA *pPSD, THRESHOLDBOOKKEEPING *pThrshBookeeping, DATEXTCT_SCE *SpeSumm, BOOL SpeciesInfAvail/*, BOOL FlagA, BOOL FlagB, BOOL FlagBbeh*/)
{
	BOOL rwres;
	UINT16 member; // Species membership.
	TCHAR szOB[SIZE_16] = "______________";
	TCHAR *spaces = "               ";
	TCHAR szBuff[SIZE_256];


	// Clock time.
	_ASSERT(Fd != NULL);
	if(Fd == NULL)
		return TRUE;

	if(0 > (rwres = fprintf(Fd, "%06d %07d %5i ", pASFO->animatID, pASFO->clock, pPSD->behavior+1))) return FALSE;

	 // Behavior Name
	if(SpeciesInfAvail == TRUE)
	{
		member = SpeSumm->ansm[AnimatNumber].speciesNumber;
		//speBinOutInf = &ScenarioSummary.spe[member].inf;
		if(pPSD->behavior >= 0 && strlen(SpeSumm->spe[member].behName[pPSD->behavior]) > 0)
		{
			memset(szBuff, 0, sizeof(szBuff));
			strncpy_s(szBuff, sizeof(szBuff), SpeSumm->spe[member].behName[pPSD->behavior], 15);
			strncpy_s(&szBuff[strlen(szBuff)], sizeof(szBuff)-strlen(szBuff), spaces, 15-strlen(szBuff));
			if(0 > (rwres = fprintf(Fd, "%15s ", szBuff))) return FALSE;
		}
		else
		{
			if(0 > (rwres = fprintf(Fd, "                "))) return FALSE;
		}
	}
	else
	{
		if(0 > (rwres = fprintf(Fd, "            %03d ", pPSD->behavior+1))) return FALSE;
	}


	// The current Overriding behavior
	memset(szBuff, 0, sizeof(szBuff));

	strncpy_s(szBuff, sizeof(szBuff), szOB, strlen(szOB));
	if(pPSD->overrideBehavior != 0)
	{
		if((pPSD->overrideBehavior & ACOUSTIC_AVERSION_SHIFT) == ACOUSTIC_AVERSION_SHIFT)
			szOB[0] = szOB[1] = 'A';
		if((pPSD->overrideBehavior & SHORE_FOLLOW_SHIFT) == SHORE_FOLLOW_SHIFT)
		{
			szOB[3] = 'S';
			szOB[4] = 'F';
		}
		if((pPSD->overrideBehavior & POD_FOLLOW_SHIFT) == POD_FOLLOW_SHIFT)
		{
			szOB[6] = 'P';
			szOB[7] = 'F';
		}
		if((pPSD->overrideBehavior & ENV_ATTR_DEPTH_SHIFT) == ENV_ATTR_DEPTH_SHIFT)
		{
			szOB[9] = 'D';
			szOB[10] = 'E';
		}
		if((pPSD->overrideBehavior & ENV_ATTR_TEMP_SHIFT) == ENV_ATTR_TEMP_SHIFT)
		{
			szOB[12] = 'T';
			szOB[13] = 'E';
		}
	}
	if(0 > (rwres = fprintf(Fd, "%14s ", szOB))) return FALSE;


	// The current activity.
	switch(pPSD->diveActivity)
	{
	case IN_SURFACE_INTERVAL:
		rwres = fprintf(Fd, "%10s ", "SurfIntrvl");
		break;
	case BOTTOM_FOLLOWING:
		rwres = fprintf(Fd, "%10s ", "BottmFlw");
		break;
	case ASCENDING:
		rwres = fprintf(Fd, "%10s ", "Ascending");
		break;
	case DESCENDING:
		rwres = fprintf(Fd, "%10s ", "Descending");
		break;
	case ASCENDING_REVERSAL:
		rwres = fprintf(Fd, "%10s ", "Reversal_A");
		break;
	case DESCENDING_REVERSAL:
		rwres = fprintf(Fd, "%10s ", "Reversal_D");
		break;

	case BEACHED:
		rwres = fprintf(Fd, "%10s ", "BEACHED");
		break;

	default:
		rwres = fprintf(Fd, "%10s ", "UNKNWN");
		break;
	}
	if(rwres < 0)
		return FALSE;

	// Only count transistions.
	if(pThrshBookeeping->inExposureA == FALSE && pPSD->threshFlagAPhy == TRUE) 
	{
		pThrshBookeeping->exposureCntA++;
		pThrshBookeeping->inExposureA = TRUE;
	}
	else if(pThrshBookeeping->inExposureA == TRUE && pPSD->threshFlagAPhy == FALSE)
	{
		pThrshBookeeping->inExposureA = FALSE;
	}

	// Only count transistions.
	if(pThrshBookeeping->inExposureB == FALSE && pPSD->threshFlagBPhy == TRUE) 
	{
		pThrshBookeeping->exposureCntB++;
		pThrshBookeeping->inExposureB = TRUE;
	}
	else if(pThrshBookeeping->inExposureB == TRUE && pPSD->threshFlagBPhy == FALSE)
	{
		pThrshBookeeping->inExposureB = FALSE;
	}

	if(SpeciesInfAvail == TRUE && SpeSumm->spe[member].inf.description.group == SPECIALCONSIDRTNS)
	{
		// Only count transistions.
		if(pThrshBookeeping->inExposureBBeh == FALSE && pPSD->threshFlagBBeh == TRUE) 
		{
			pThrshBookeeping->exposureCntBBeh++;
			pThrshBookeeping->inExposureBBeh = TRUE;
		}
		else if(pThrshBookeeping->inExposureBBeh == TRUE && pPSD->threshFlagBBeh == FALSE)
		{
			pThrshBookeeping->inExposureBBeh = FALSE;
		}
	}


	// Other stuff.
	if(0 >= (rwres = fprintf(Fd, "%8.2f %8.2f %9.2f %8.2f %8.2f %19.14f %19.14f ",
							 pASFO->diveRate, pASFO->depth, pASFO->bathyDepth, pASFO->travelRate,  pASFO->bearing,
							 pASFO->lon, pASFO->lat)))
							 return FALSE;



	if(SpeciesInfAvail == TRUE && SpeSumm->spe[member].inf.description.group == SPECIALCONSIDRTNS)
	{
		if(0 >= (rwres = fprintf(Fd, "%11.5f %11.5f %4i %5i  %4i %5i %4i %5i %6.2f %6d\n",
								 pASFO->aeCmltve, pASFO->aeMoment, pPSD->threshFlagAPhy, pThrshBookeeping->exposureCntA,
								 pPSD->threshFlagBPhy, pThrshBookeeping->exposureCntB, pPSD->threshFlagBBeh, pThrshBookeeping->exposureCntBBeh,
								 pASFO->aeRelAngle, pASFO->aeTimeAvrt)))
								 return FALSE;
	}
	else
	{

		if(0 >= (rwres = fprintf(Fd, "%11.5f %11.5f %4i %5i  %4i %5i  %10.4f %6.2f %6d\n",
								 pASFO->aeCmltve, pASFO->aeMoment, pPSD->threshFlagAPhy, pThrshBookeeping->exposureCntA,
								 pPSD->threshFlagBPhy, pThrshBookeeping->exposureCntB, pASFO->risk,
								 pASFO->aeRelAngle, pASFO->aeTimeAvrt)))
								 return FALSE;
	}

//	if(0 >= (rwres = fprintf(Fd, "%11.2f %11.2f %4i %5i %4i %5i %4i %5i %7.2f  %6d %5.3f\n",
//							 pASFO->aeCmltve, pASFO->aeMoment, pPSD->threshFlagAPhy, pThrshBookeeping->exposureCntA,
//							 pPSD->threshFlagBPhy, pThrshBookeeping->exposureCntB, /*pPSD->threshFlagBBeh,*/
//							 /*pThrshBookeeping->exposureCntBBeh,*/ pASFO->aeRelAngle, pASFO->aeTimeAvrt, pASFO->risk)))
//							 return FALSE;
	return TRUE;
}

RESLT CFileExtracter::SetUpSpeciesSummaryAssociation()
{
	DWORD i;
	DWORD bytes;
	UINT32 numBehaviors;

	if(m_setup.bin.headerInf.speInfAndAnimatAsscn == FALSE || m_abort == TRUE)
		return OK;

	// Allocate space for number for binary species information
	if(NULL == (m_speSum.spe = new DATAEXTCT_SPE[m_setup.sceParams.numSpecies]))
		return MEMALLOC_ERROR;
	memset(m_speSum.spe, 0, sizeof(DATAEXTCT_SPE)*m_setup.sceParams.numSpecies);

	// Allocate space for binary animat to species association information
	if(NULL == (m_speSum.ansm = new ANIMATASSCN[m_setup.sceParams.totalNumAnimats]))
		return MEMALLOC_ERROR;
	memset(m_speSum.ansm, 0, sizeof(ANIMATASSCN)*m_setup.sceParams.totalNumAnimats);

	// Set the file pointer to the location where species description information begins.
	m_staticLib.MySetFilePointer(m_hdl, m_setup.sceParams.diskInf.fp.speciesDesc, FILE_BEGIN);

	for(i=0; i<m_setup.sceParams.numSpecies && m_abort == FALSE; i++)
	{
		if(0 == ReadFile(m_hdl, &m_speSum.spe[i].inf, sizeof(SPECIESBINOUTINF), &bytes, NULL))
			return FILEREAD_ERROR; 

		numBehaviors = m_speSum.spe[i].inf.description.numBehaviors;

		// Allocate memory for the names.
		if(NULL == (m_speSum.spe[i].behName = new NAMES[numBehaviors]))
			return MEMALLOC_ERROR;

		// Read the names.
		if(0 == ReadFile(m_hdl, m_speSum.spe[i].behName, numBehaviors * sizeof(NAMES), &bytes, NULL))
			return FILEREAD_ERROR;
	}

	// Finally, read in the animat associations
	if(0 == ReadFile(m_hdl, m_speSum.ansm, m_setup.sceParams.totalNumAnimats * sizeof(ANIMATASSCN), &bytes, NULL))
		return FILEREAD_ERROR;

	return OK;
}

RESLT CFileExtracter::ReadPostRunStatsResults()
{

	DWORD readBytes;
 
	if(FALSE == m_setup.bin.headerInf.postRunAnalysis || m_abort == TRUE)
		return OK;

	// Read the statistical analysis region.
	if(FALSE == ReadFile(m_hdl, &m_statAnalysis, sizeof(TAKE), &readBytes, NULL))
		return FILEREAD_ERROR;

	// Read in the TAKESTATS statistical results structure allocated for each species
	// in the scenario.  Sum the total number of readBytes read so far, then ASSERT that the
	// calculated storage space for the statistical post analysis region matches the number
	// of readBytes actually read in for it.
	if(NULL == (m_speciesStats = new TAKESTATS[m_setup.sceParams.numSpecies]))
		return MEMALLOC_ERROR;
	memset(m_speciesStats, 0, sizeof(TAKESTATS)*m_setup.sceParams.numSpecies);

	if(0 == ReadFile(m_hdl, m_speciesStats, sizeof(TAKESTATS)*m_setup.sceParams.numSpecies, &readBytes, NULL))
	{
		delete [] m_speciesStats;
		return FILEREAD_ERROR;
	}

	return OK;
}


RESLT CFileExtracter::PrintPostRunStatsResults()
{
	DWORD i, j, outterCnt, innrCnt;
	HHMMSS hms;
	TCHAR statsOutputFileName[SIZE_256];
	TCHAR buff15chars[SIZE_32];
	TCHAR szBuff[SIZE_128];
	FILE *fd;
	int rwres;
	_fSCEPARMSSPECIESGROUP *grp = NULL;
	//int extraSpace;
	//int aa;
	//float flr;
	SYSTEMTIME sysTime;

	GetLocalTime(&sysTime);

	SCENARIOPARAMS sceParams = m_fileManagerStatic.ConvertScenarioFormat(m_setup.sceParams);
	//TCHAR szChar[2] = {230, 0};

	if(FALSE == m_setup.bin.headerInf.postRunAnalysis)
		return OK;

	// Generate the output file name for the stats information.
	memset(statsOutputFileName, 0, sizeof(statsOutputFileName));
	sprintf_s(statsOutputFileName, sizeof(statsOutputFileName)/sizeof(TCHAR), "%s\\%s", m_szFileTrakFileSubPath, m_szFileTitle);
	//sprintf_s(statsOutputFileName, sizeof(statsOutputFileName)/sizeof(TCHAR), "%s\\%s", m_szFilePath, m_szFileTitle);
	//strcpy_s(statsOutputFileName, sizeof(statsOutputFileName), m_szFileTitle);
	m_staticLib.RemoveExtension(statsOutputFileName);
	strcat_s(statsOutputFileName, sizeof(statsOutputFileName), ".sts");

	if(m_abort == TRUE)
		return OK;

	// Create the stats file, write to it.
	if(0 != fopen_s(&fd, statsOutputFileName, "w"))
		return OPENTEXTOUTPUTFILE_ERROR;

	grp = &m_setup.sceParams.group[0]; // used to make lines of code shorter.

	// fprintf returns a negative value if it fails.
	rwres = fprintf(fd, TXTSTATSDATE0, sysTime.wMonth, sysTime.wDay, sysTime.wYear, sysTime.wHour, sysTime.wMinute, sysTime.wSecond);
	if(rwres > 0) rwres = fprintf(fd, TXTSTATS0000, MMBSLIB_VERSION_SUPER, MMBSLIB_VERSION_SUB, __DATE__, __TIME__);
	if(rwres > 0) rwres = fprintf(fd, TXTSTATS0005, m_setup.sceParams.libVerSuper, m_setup.sceParams.libVerSub);
	if(rwres > 0) rwres = fprintf(fd, TXTSTATS0100, m_setup.sceParams.numSpecies - m_setup.sceParams._fNumAcstSrcTypes);

	if(rwres > 0) rwres = fprintf(fd, TXTSTATS0200);
	if(rwres > 0) rwres = fprintf(fd, TXTSTATS0220, (m_setup.sceParams.totalNumAnimats - m_setup.sceParams._fTotalNumAcstcSrcs));
	if(rwres > 0) rwres = fprintf(fd, TXTSTATS0230, m_statAnalysis.animat.offScreenCount);
	if(rwres > 0) rwres = fprintf(fd, TXTSTATS0240, m_setup.sceParams.totalNumAnimats - m_setup.sceParams._fTotalNumAcstcSrcs + m_statAnalysis.animat.offScreenCount);

	hms = m_staticLib.Time_To24HrMinSec(m_setup.sceParams.startTime);
	if(rwres > 0) rwres = fprintf(fd, TXTSTATS0300, hms.hour, hms.min, hms.sec);
	hms = m_staticLib.Time_ToHrMinSec(m_setup.sceParams.duration); // Duration, (so -1 to not count inital state)
	if(rwres > 0) rwres = fprintf(fd, TXTSTATS0400, hms.hour, hms.min, hms.sec);


	if(rwres > 0) rwres = fprintf(fd, TXTSTATS0420);
	//m_setup.bin = m_fileManagerStatic.TranslateBinFileOutConfiguration(m_setup.sceParams.binOutStateItemConfig);
	if(sceParams.user.acousticAnimatActive == TRUE)
	{
		;
//		if(rwres > 0) rwres = fprintf(fd, TXTSTATS0422, m_setup.sceParams.acousticSourceLevel);
//		if(rwres > 0) rwres = fprintf(fd, TXTSTATS0450, 100.0 * 1.0/(float)m_setup.sceParams.acousticSourceDutyPeriod, '%');

		// Add/replace acoustic source start time here...
//		hms = m_staticLib.Time_ToHrMinSec(m_setup.sceParams.acousticSourceBeginIteration + m_setup.sceParams.startTime);
//		if(rwres > 0) rwres = fprintf(fd, TXTSTATS0470, hms.hour, hms.min, hms.sec);
	}
	else
	{
		if(rwres > 0) rwres = fprintf(fd, TXTSTATS0423);
		if(rwres > 0) rwres = fprintf(fd, TXTSTATS0451);
		if(rwres > 0) rwres = fprintf(fd, TXTSTATS0471);
	}
	if(rwres > 0) rwres = fprintf(fd, "\n\n");

	// Threshold Description
	//sprintf_s(szBuff, sizeof(szBuff), TXTSTATS1170, (TCHAR)230, (TCHAR)230);
	if(rwres > 0) rwres = fprintf(fd, "%s%s%s%s%s", TXTSTATS0900, TXTSTATS1000, TXTSTATS1100, TXTSTATS1170, TXTSTATS1200);
	for(j=0; j<m_statAnalysis.numSpeGroups && rwres > 0 && m_abort == FALSE; j++)
	{
		if(j == SPECIALCONSIDRTNS)
			rwres = fprintf(fd, TXTSTATS1301, grp[j].szGroup, grp[j].lvlAphys, grp[j].lvlBphys, grp[j].lvlBBeh_RiskA);
		else if(j == SOUNDSOURCE)
			; // do nothing.
		else
			rwres = fprintf(fd, TXTSTATS1300, grp[j].szGroup, grp[j].lvlAphys, grp[j].lvlBphys);
	}


	// Scenario Results by species
	if(rwres > 0) rwres = fprintf(fd, "%s%s\n", TXTSTATS2000, TXTSTATS3000);

	//------------------//
	// Takes By Species
	//------------------//
	//outterCnt, innrCnt; m_setup.sceParams._fNumAcstSrcTypes
	DWORD numCols = 3;
	for(outterCnt = m_setup.sceParams._fNumAcstSrcTypes; outterCnt<m_setup.sceParams.numSpecies;)
	{

		//---------------------------------//
		// File Title
		//---------------------------------//
		innrCnt = outterCnt;
		if(rwres >0) rwres = fprintf(fd,TXTSTATS4005);
		for(i=0; innrCnt<m_setup.sceParams.numSpecies && i < numCols && rwres > 0 && m_abort == FALSE; i++, innrCnt++)
		{
			if(m_setup.bin.headerInf.speInfAndAnimatAsscn == TRUE)
			{
				strncpy_s(buff15chars, sizeof(buff15chars), m_speSum.spe[innrCnt].inf.fileTitle, 18);
			}
			else
			{
				sprintf_s(szBuff, sizeof(szBuff), "Unknown File Name");
				strncpy_s(buff15chars, sizeof(buff15chars), szBuff, 18);
			}
			rwres = fprintf(fd, TXTSTATS4810, buff15chars);
		}
		if(rwres > 0) rwres = fprintf(fd, "\n");


		//---------------------------------//
		// Species name.
		//---------------------------------//
		innrCnt = outterCnt;
		if(rwres >0) rwres = fprintf(fd,TXTSTATS4010);
		for(i=0; innrCnt<m_setup.sceParams.numSpecies && i < numCols && rwres > 0 && m_abort == FALSE; i++, innrCnt++)
		{
			if(m_setup.bin.headerInf.speInfAndAnimatAsscn == TRUE)
			{
				strncpy_s(buff15chars, sizeof(buff15chars),
					SZSPECIESNAMEBUFFER_LATIN_FIRSTINITIAL_SECONDNAME[m_speSum.spe[innrCnt].inf.description.name], 18);
			}
			else
			{
				// +1 for non-zero indexed printing, - m_setup.sceParams._fNumAcstSrcTypes for
				// ignoring sound source types
				sprintf_s(buff15chars, 18, "Species %d", innrCnt +1 - m_setup.sceParams._fNumAcstSrcTypes);
			}
			rwres = fprintf(fd, TXTSTATS4810, buff15chars);
		}
		if(rwres > 0) rwres = fprintf(fd, "\n");


		//---------------------------------//
		// Species Group
		//---------------------------------//
		innrCnt = outterCnt;
		if(rwres >0) rwres = fprintf(fd,TXTSTATS4012);
		for(i=0; innrCnt<m_setup.sceParams.numSpecies && i < numCols && rwres > 0 && m_abort == FALSE; i++, innrCnt++)
		{		
			if(m_setup.bin.headerInf.speInfAndAnimatAsscn == TRUE)
			{
				strncpy_s(buff15chars, sizeof(buff15chars),
					SZSPECIESGROUPNAMEBUFFER[m_speSum.spe[innrCnt].inf.description.group], 18);
			}
			else
			{
				sprintf_s(buff15chars, 18, "Unknown Spe grp");
			}
			rwres = fprintf(fd, TXTSTATS4810, buff15chars);
		}
		if(rwres > 0) rwres = fprintf(fd, "\n");


		//---------------------------------//
		// Initial Animat Count
		//---------------------------------//
		innrCnt = outterCnt;
		if(rwres >0) rwres = fprintf(fd,TXTSTATS4015);
		for(i=0; innrCnt<m_setup.sceParams.numSpecies && i < numCols && rwres > 0 && m_abort == FALSE; i++, innrCnt++)
		{
			sprintf_s(szBuff, sizeof(szBuff),
				"(%d/%d)", m_speciesStats[innrCnt].initialAnimatCount, m_speciesStats[innrCnt].initialAnimatCount + m_speciesStats[innrCnt].offScreenCount);
			strncpy_s(buff15chars, sizeof(buff15chars), szBuff, 18);
			rwres = fprintf(fd, TXTSTATS4810, buff15chars);
		}
		if(rwres > 0) rwres = fprintf(fd, "\n");
#if 0

		//---------------------------------//
		// Initial Animat Count
		//---------------------------------//
		innrCnt = outterCnt;
		if(rwres >0) rwres = fprintf(fd,TXTSTATS4015);
		for(i=0; innrCnt<m_setup.sceParams.numSpecies && i < numCols && rwres > 0 && m_abort == FALSE; i++, innrCnt++)
		{
			sprintf_s(szBuff, sizeof(szBuff), "%d", m_speciesStats[innrCnt].initialAnimatCount);
			strncpy_s(buff15chars, sizeof(buff15chars), szBuff, 18);
			rwres = fprintf(fd, TXTSTATS4810, buff15chars);
		}
		if(rwres > 0) rwres = fprintf(fd, "\n");


		//---------------------------------//
		// Final Animat Count
		//---------------------------------//
		innrCnt = outterCnt;
		if(rwres >0) rwres = fprintf(fd,TXTSTATS4020);
		for(i=0; innrCnt<m_setup.sceParams.numSpecies && i < numCols && rwres > 0 && m_abort == FALSE; i++, innrCnt++)
		{
			sprintf_s(szBuff, sizeof(szBuff), "%d", m_speciesStats[innrCnt].initialAnimatCount + m_speciesStats[innrCnt].offScreenCount);
			strncpy_s(buff15chars, sizeof(buff15chars), szBuff, 18);
			rwres = fprintf(fd, TXTSTATS4810, buff15chars);
		}
		if(rwres > 0) rwres = fprintf(fd, "\n");
#endif

		//---------------------------------//
		// Header --------------
		//---------------------------------//
		innrCnt = outterCnt;
		if(rwres > 0) rwres = fprintf(fd, TXTSTATS4000);
		for(i=0; innrCnt<m_setup.sceParams.numSpecies && i<numCols && rwres > 0 && m_abort == FALSE; i++, innrCnt++)
			rwres = fprintf(fd, TXTSTATS4820);
		if(rwres > 0) rwres = fprintf(fd, "\n");


		// Level B physical takes by species
		innrCnt = outterCnt;
		if(rwres > 0) rwres = fprintf(fd, TXTSTATS4821);
		for(i=0; innrCnt<m_setup.sceParams.numSpecies && i<numCols && rwres > 0 && m_abort == FALSE; i++, innrCnt++)
			rwres = fprintf(fd, TXTSTATS4840, m_speciesStats[innrCnt].lvlBPhysTakes);
		if(rwres > 0) rwres = fprintf(fd, "\n");


		// Level B behavioral (risk) takes by species
		innrCnt = outterCnt;
		if(rwres > 0) rwres = fprintf(fd, TXTSTATS4822);
		for(i=0; innrCnt<m_setup.sceParams.numSpecies && i<numCols && rwres > 0 && m_abort == FALSE; i++, innrCnt++)
		{
			if(m_setup.bin.headerInf.speInfAndAnimatAsscn == TRUE && m_speSum.spe[innrCnt].inf.description.group == SPECIALCONSIDRTNS)
				rwres = fprintf(fd, TXTSTATS4842, (int)m_staticLib.MyRound(m_speciesStats[innrCnt].lvlBBehTakes));
			else
				rwres = fprintf(fd, TXTSTATS4841, m_speciesStats[innrCnt].lvlBBehTakes, (int)m_staticLib.MyRound(m_speciesStats[innrCnt].lvlBBehTakes));
		}
		if(rwres > 0) rwres = fprintf(fd, "\n");


		// Level B total
		innrCnt = outterCnt;
		if(rwres > 0) rwres = fprintf(fd, TXTSTATS4830);
		for(i=0; innrCnt<m_setup.sceParams.numSpecies && i<numCols && rwres > 0 && m_abort == FALSE; i++, innrCnt++)
			rwres = fprintf(fd, TXTSTATS4840, m_speciesStats[innrCnt].lvlBTakesTotal);
		if(rwres > 0) rwres = fprintf(fd, "\n");


		// Level A takes by species
		innrCnt = outterCnt;
		if(rwres > 0) rwres = fprintf(fd, TXTSTATS4500);
		for(i=0; innrCnt<m_setup.sceParams.numSpecies && i<numCols && rwres > 0 && m_abort == FALSE; i++, innrCnt++)
			rwres = fprintf(fd, TXTSTATS4840, m_speciesStats[innrCnt].lvlAPhysTakes);
		if(rwres > 0) rwres = fprintf(fd, "\n");


		// Max Exposures
		innrCnt = outterCnt;
		if(rwres > 0) rwres = fprintf(fd, "%s%s", TXTSTATS4600, TXTSTATS4700);
		for(i=0; innrCnt<m_setup.sceParams.numSpecies && i<numCols && rwres > 0 && m_abort == FALSE; i++, innrCnt++)
			rwres = fprintf(fd, TXTSTATS400F, m_speciesStats[innrCnt].maxInstant);
		if(rwres > 0) rwres = fprintf(fd, "\n");


		innrCnt = outterCnt;
		if(rwres > 0) rwres = fprintf(fd, TXTSTATS4800);
		for(i=0; innrCnt<m_setup.sceParams.numSpecies && i<numCols && rwres > 0 && m_abort == FALSE; i++, innrCnt++)
			rwres = fprintf(fd, TXTSTATS400F, m_speciesStats[innrCnt].maxCumulative);
		if(rwres > 0) rwres = fprintf(fd, "\n");

		innrCnt = outterCnt;
		if(rwres > 0) rwres = fprintf(fd, TXTSTATS4900);
		for(i=0; innrCnt<m_setup.sceParams.numSpecies && i<numCols && rwres > 0 && m_abort == FALSE; i++, innrCnt++)
			rwres = fprintf(fd, TXTSTATS4840, m_speciesStats[innrCnt].numStranded);
		if(rwres > 0) rwres = fprintf(fd, "\n");

		if(rwres > 0) rwres = fprintf(fd, "\n\n");
		outterCnt = innrCnt;
	}



	//----------------//
	// Takes by group
	//----------------//
	if(m_setup.bin.headerInf.speInfAndAnimatAsscn == TRUE)
		if(rwres > 0) rwres = fprintf(fd, "\n\n\n\n%s", TXTSTATS5000);
	for(outterCnt=0; outterCnt<m_statAnalysis.numSpeGroups && m_setup.bin.headerInf.speInfAndAnimatAsscn == TRUE;)
	{
		while(outterCnt == SOUNDSOURCE)
			outterCnt++;

		if(outterCnt >= m_statAnalysis.numSpeGroups)
			break; // break out of the for-loop

		if(rwres >0) rwres = fprintf(fd,TXTSTATS4000);

		// Group names
		innrCnt = outterCnt;
		for(i=0; innrCnt<m_statAnalysis.numSpeGroups && i<numCols && rwres > 0 && m_abort == FALSE; i++, innrCnt++)
		{
			//-----------------------------------------------------------------------------------//
			// Don't print out sound source group information because they are not animats
			// or a species or a species group.  This is here in case at a future date more
			// species groups are added (and/or/leading to) the sound source indexing being
			// intermixed with species.  This will prevent the sound source group take data
			// from being printed out yet not count as a printed column
			//----------------------------------------------------------//
			if(innrCnt == SOUNDSOURCE)
			{
				i--; // this one won't be printed so don't count it as a printed column.
				continue; 
			}
			//-----------------------------------------------------------------------------------//
			strncpy_s(buff15chars, sizeof(buff15chars), grp[innrCnt].szGroup, strlen(grp[innrCnt].szGroup));
			rwres = fprintf(fd, TXTSTATS4810, buff15chars);
		}
		if(rwres > 0) rwres = fprintf(fd, "\n");

		



		innrCnt = outterCnt;
		if(rwres >0) rwres = fprintf(fd,TXTSTATS4015);
		for(i=0; innrCnt<m_statAnalysis.numSpeGroups && i<numCols && rwres > 0 && m_abort == FALSE; i++, innrCnt++)
		{
			//-----------------------------------------------------------------------------------//
			// Don't print out sound source group information because they are not animats
			// or a species or a species group.  This is here in case at a future date more
			// species groups are added (and/or/leading to) the sound source indexing being
			// intermixed with species.  This will prevent the sound source group take data
			// from being printed out yet not count as a printed column
			//----------------------------------------------------------//
			if(innrCnt == SOUNDSOURCE)
			{
				i--; // this one won't be printed so don't count it as a printed column.
				continue; 
			}
			//-----------------------------------------------------------------------------------//
			sprintf_s(szBuff, sizeof(szBuff),
				"(%d/%d)", m_statAnalysis.speGroup[innrCnt].initialAnimatCount, m_statAnalysis.speGroup[innrCnt].initialAnimatCount + m_statAnalysis.speGroup[innrCnt].offScreenCount);
			strncpy_s(buff15chars, sizeof(buff15chars), szBuff, 18);
			rwres = fprintf(fd, TXTSTATS4810, buff15chars);
		}
		if(rwres > 0) rwres = fprintf(fd, "\n");


		// Header --------------
		innrCnt = outterCnt;
		if(rwres > 0) rwres = fprintf(fd, "%s", TXTSTATS4000);
		for(i=0; innrCnt<m_statAnalysis.numSpeGroups && i<numCols && rwres > 0 && m_abort == FALSE; i++, innrCnt++)
		{
			//-----------------------------------------------------------------------------------//
			// Don't print out sound source group information because they are not animats
			// or a species or a species group.  This is here in case at a future date more
			// species groups are added (and/or/leading to) the sound source indexing being
			// intermixed with species.  This will prevent the sound source group take data
			// from being printed out yet not count as a printed column
			//----------------------------------------------------------//
			if(innrCnt == SOUNDSOURCE)
			{
				i--; // this one won't be printed so don't count it as a printed column.
				continue; 
			}
			//-----------------------------------------------------------------------------------//
			rwres = fprintf(fd, TXTSTATS4820);
		}
		if(rwres > 0) rwres = fprintf(fd, "\n");


		// Level B physical takes by species group
		innrCnt = outterCnt;
		if(rwres > 0) rwres = fprintf(fd, TXTSTATS4821);
		for(i=0; innrCnt<m_statAnalysis.numSpeGroups && i<numCols && rwres > 0 && m_abort == FALSE; i++, innrCnt++)
		{
			//-----------------------------------------------------------------------------------//
			// Don't print out sound source group information because they are not animats
			// or a species or a species group.  This is here in case at a future date more
			// species groups are added (and/or/leading to) the sound source indexing being
			// intermixed with species.  This will prevent the sound source group take data
			// from being printed out yet not count as a printed column
			//----------------------------------------------------------//
			if(innrCnt == SOUNDSOURCE)
			{
				i--; // this one won't be printed so don't count it as a printed column.
				continue; 
			}
			//-----------------------------------------------------------------------------------//

			rwres = fprintf(fd, TXTSTATS4840, m_statAnalysis.speGroup[innrCnt].lvlBPhysTakes);
		}
		if(rwres > 0) rwres = fprintf(fd, "\n");


		// Level B behavioral (risk) takes by species group
		innrCnt = outterCnt;
		if(rwres > 0) rwres = fprintf(fd, TXTSTATS4822);
		for(i=0; innrCnt<m_statAnalysis.numSpeGroups && i<numCols && rwres > 0 && m_abort == FALSE; i++, innrCnt++)
		{
			//-----------------------------------------------------------------------------------//
			// Don't print out sound source group information because they are not animats
			// or a species or a species group.  This is here in case at a future date more
			// species groups are added (and/or/leading to) the sound source indexing being
			// intermixed with species.  This will prevent the sound source group take data
			// from being printed out yet not count as a printed column
			//----------------------------------------------------------//
			if(innrCnt == SOUNDSOURCE)
			{
				i--; // this one won't be printed so don't count it as a printed column.
				continue; 
			}
			//-----------------------------------------------------------------------------------//

			if(innrCnt == SPECIALCONSIDRTNS)
				rwres = fprintf(fd, TXTSTATS4842, (int)m_staticLib.MyRound(m_statAnalysis.speGroup[innrCnt].lvlBBehTakes));
			else
				rwres = fprintf(fd, TXTSTATS4841, m_statAnalysis.speGroup[innrCnt].lvlBBehTakes, (int)m_staticLib.MyRound(m_statAnalysis.speGroup[innrCnt].lvlBBehTakes));
		}
		if(rwres > 0) rwres = fprintf(fd, "\n");



		// Level B total by species group
		innrCnt = outterCnt;
		if(rwres > 0) rwres = fprintf(fd, TXTSTATS4830);
		for(i=0; innrCnt<m_statAnalysis.numSpeGroups && i<numCols && rwres > 0 && m_abort == FALSE; i++, innrCnt++)
		{
			//-----------------------------------------------------------------------------------//
			// Don't print out sound source group information because they are not animats
			// or a species or a species group.  This is here in case at a future date more
			// species groups are added (and/or/leading to) the sound source indexing being
			// intermixed with species.  This will prevent the sound source group take data
			// from being printed out yet not count as a printed column
			//----------------------------------------------------------//
			if(innrCnt == SOUNDSOURCE)
			{
				i--; // this one won't be printed so don't count it as a printed column.
				continue; 
			}
			//-----------------------------------------------------------------------------------//

			rwres = fprintf(fd, TXTSTATS4840, m_statAnalysis.speGroup[innrCnt].lvlBTakesTotal);
		}
		if(rwres > 0) rwres = fprintf(fd, "\n");


		// Level A takes by species group
		innrCnt = outterCnt;
		if(rwres > 0) rwres = fprintf(fd, TXTSTATS4500);
		for(i=0; innrCnt<m_statAnalysis.numSpeGroups && i<numCols && rwres > 0 && m_abort == FALSE; i++, innrCnt++)
		{
			//-----------------------------------------------------------------------------------//
			// Don't print out sound source group information because they are not animats
			// or a species or a species group.  This is here in case at a future date more
			// species groups are added (and/or/leading to) the sound source indexing being
			// intermixed with species.  This will prevent the sound source group take data
			// from being printed out yet not count as a printed column
			//----------------------------------------------------------//
			if(innrCnt == SOUNDSOURCE)
			{
				i--; // this one won't be printed so don't count it as a printed column.
				continue; 
			}
			//-----------------------------------------------------------------------------------//

			rwres = fprintf(fd, TXTSTATS4840, m_statAnalysis.speGroup[innrCnt].lvlAPhysTakes);
		}
		if(rwres > 0) rwres = fprintf(fd, "\n");
		/////////////////////////////////////////////


		//////////////////////////////////

		if(rwres > 0) rwres = fprintf(fd, "\n\n");
		outterCnt = innrCnt;
	}



	//---------------//
	// Output Summary
	//---------------//
	if(rwres > 0) rwres = fprintf(fd, TXTSTATS6000);
	if(rwres > 0) rwres = fprintf(fd, TXTSTATS6400, m_statAnalysis.animat.lvlBPhysTakes);
	if(rwres > 0) rwres = fprintf(fd, TXTSTATS6500, (int)m_staticLib.MyRound(m_statAnalysis.animat.lvlBBehTakes), m_statAnalysis.animat.lvlBBehTakes);
	if(rwres > 0) rwres = fprintf(fd, TXTSTATS7000, m_statAnalysis.animat.lvlBTakesTotal);
	if(rwres > 0) rwres = fprintf(fd, TXTSTATS7100, m_statAnalysis.animat.lvlAPhysTakes);

	if(rwres > 0) rwres = fprintf(fd, TXTSTATS7200, m_statAnalysis.animat.numStranded);

	fclose(fd);

	if(rwres <= 0)
		return FILEWRITE_ERROR;

	return OK;
}

__int64 CFileExtracter::BufferToAnimatState(ANIMATSTATE_FILEOUTPUT_CONFIG *Config,
									   ANIMATSTATE_FILEOUT *AS,
									   __int64 BytePos,
									   BYTE *Buffer)
{

	/* These Booleans are set by the user in the scenario configuration window.
	 See the dialog window IDD_DIALOG_OUTPUT_CONFIG and 3mbsScenarioConfig.cpp
	*/

	memset(AS, 0, sizeof(ANIMATSTATE_FILEOUT));

	if(Config->ID == TRUE)
	{
		memcpy(&AS->animatID, &Buffer[BytePos], sizeof(AS->animatID));
		BytePos += sizeof(AS->animatID);
	}
	if(Config->timeOfDay == TRUE)
	{
		memcpy(&AS->clock, &Buffer[BytePos], sizeof(AS->clock));
		BytePos += sizeof(AS->clock);
	}
	if(Config->coordinate == TRUE)
	{
		memcpy(&AS->lat, &Buffer[BytePos], sizeof(AS->lat));
		BytePos += sizeof(AS->lat);

		memcpy(&AS->lon, &Buffer[BytePos], sizeof(AS->lon));
		BytePos += sizeof(AS->lon);
	}
	if(Config->depth == TRUE)
	{
		memcpy(&AS->depth, &Buffer[BytePos], sizeof(AS->depth));
		BytePos += sizeof(AS->depth);
	}
	if(Config->bearing == TRUE)
	{
		memcpy(&AS->bearing, &Buffer[BytePos], sizeof(AS->bearing));
		BytePos += sizeof(AS->bearing);
	}
	if(Config->diveRate == TRUE)
	{
		memcpy(&AS->diveRate, &Buffer[BytePos], sizeof(AS->diveRate));
		BytePos += sizeof(AS->diveRate);
	}
	if(Config->travelRate == TRUE)
	{
		memcpy(&AS->travelRate, &Buffer[BytePos], sizeof(AS->travelRate));
		BytePos += sizeof(AS->travelRate);
	}
	if(Config->aeCmltve == TRUE)
	{
		memcpy(&AS->aeCmltve, &Buffer[BytePos], sizeof(AS->aeCmltve));
		BytePos += sizeof(AS->aeCmltve);
	}
	if(Config->aeMoment == TRUE)
	{
		memcpy(&AS->aeMoment, &Buffer[BytePos], sizeof(AS->aeMoment));
		BytePos += sizeof(AS->aeMoment);
	}
	if(Config->aeRelAngle == TRUE)
	{
		memcpy(&AS->aeRelAngle, &Buffer[BytePos], sizeof(AS->aeRelAngle));
		BytePos += sizeof(AS->aeRelAngle);
	}
	if(Config->aeTimeAvrt == TRUE)
	{
		memcpy(&AS->aeTimeAvrt, &Buffer[BytePos], sizeof(AS->aeTimeAvrt));
		BytePos += sizeof(AS->aeTimeAvrt);
	}
	if(Config->bathyDepth == TRUE)
	{
		memcpy(&AS->bathyDepth, &Buffer[BytePos], sizeof(AS->bathyDepth));
		BytePos += sizeof(AS->bathyDepth);
	}
	if(Config->salinity == TRUE)
	{
		memcpy(&AS->salinity, &Buffer[BytePos], sizeof(AS->salinity));
		BytePos += sizeof(AS->salinity);
	}
	if(Config->temperature == TRUE)
	{
		memcpy(&AS->temperature, &Buffer[BytePos], sizeof(AS->temperature));
		BytePos += sizeof(AS->temperature);
	}

	if(Config->packedData == TRUE)
	{
		memcpy(&AS->packedData, &Buffer[BytePos], sizeof(AS->packedData));
		BytePos += sizeof(AS->packedData);
	}

	if(Config->targetDepth == TRUE)
	{
		memcpy(&AS->targetDepth, &Buffer[BytePos], sizeof(AS->targetDepth));
		BytePos += sizeof(AS->targetDepth);
	}
	if(Config->calcDepth == TRUE) //30
	{
		memcpy(&AS->calcDepth, &Buffer[BytePos], sizeof(AS->calcDepth));
		BytePos += sizeof(AS->calcDepth);
	}

	if(Config->xyDistance == TRUE) //41
	{
		memcpy(&AS->xDistance, &Buffer[BytePos], sizeof(AS->xDistance));
		BytePos += sizeof(AS->xDistance);

		memcpy(&AS->yDistance, &Buffer[BytePos], sizeof(AS->yDistance));
		BytePos += sizeof(AS->yDistance);
	}

	// Current risk value // 42
	if(Config->risk == TRUE)
	{
		memcpy(&AS->risk, &Buffer[BytePos], sizeof(AS->risk));
		BytePos += sizeof(AS->risk);
	}


	return BytePos;
}