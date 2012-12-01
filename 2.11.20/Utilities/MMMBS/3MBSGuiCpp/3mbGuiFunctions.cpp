#include "3mbGuiFunctions.h"
#include "3mbSceInterface.h"

extern C3mbStaticsLib staticLib;


RESLT MyGetOpenSaveFileName(BOOL Save, FILE_INFO *FI, HWND hWnd, const TCHAR *szFil, const TCHAR *szExt, BOOL *FileSel);
void FileSaved(FILE_INFO *fi, BOOL FileChanged, HWND DisplayWindow = NULL, const TCHAR* szFileExtension = NULL);
//----------------------------------------------------------------------------------------
//FUNCTION: MyRegisterStandardClass( ... )
//
//PURPOSE: Registers the window class.
//
//COMMENTS: This function is meant to facilitate registering a standard
//	window.
//
//PARAMETERS:
//	HINSTANCE	hInstance		The instance in which the window is created
//	WNDPROC		ProcName		The pointer to the window procedure
//	HBRUSH		hbrBackground	The background brush for the window.	DEFAULT:	NULL
//	LPCTSTR		MenuName		The Menu name or window identifier.		DEFAULT:	NULL
//	UINT		style			Style attributes.		DEFAULT:	CS_HREDRAW|CS_VREDRAW
//	HICON		hIcon			Handle to window Icon.	DEFAULT:	NULL
//	HICON		hIconSm			Handle to small Icon.	DEFAULT:	NULL
//	HCURSOR		hCursor			Handle to cursor		DEFAULT:	NULL
//----------------------------------------------------------------------------------------
ATOM RegisterBathyBitmapWindowClass(HINSTANCE hInstance, LPCTSTR ClassName, WNDPROC ProcName, HBRUSH hbrBackground,
							 LPCTSTR MenuName, UINT style, HICON hIcon, HICON hIconSm, HCURSOR hCursor)
{
	WNDCLASSEX	wcex;
	wcex.cbSize			= sizeof(WNDCLASSEX);		//Required
	wcex.cbClsExtra		= 0;					//Extra bytes after class structure, assumed zero
	wcex.cbWndExtra		= 0;					//Extra bytes after window instance, assumed zero
	wcex.hbrBackground	= hbrBackground;	//Set rest of the paramters according to 
	wcex.hCursor		= hCursor;				//function parameters
	wcex.hIcon			= hIcon;
	wcex.hIconSm		= hIconSm;
	wcex.hInstance		= hInstance;
	wcex.lpfnWndProc	= ProcName;
	wcex.lpszClassName	= ClassName;
	wcex.lpszMenuName	= MenuName;
	wcex.style			= style;

	return RegisterClassEx(&wcex);
}

// Returns TRUE if it updated
void UpdateRunScenario(GLOBALDATA *gdata, DWORD TotalAnimatCount, DWORD Duration, SPECIALCASES SpecialCase, DWORD StartTick)
{
	static DWORD lastUpdateTick;
	static DWORD startTick;
	static BOOL secsRemainingPosted;
	static DWORD callCnt;
	SCESTATE state;
	static SCEACTIVITY prevAct; // previous activity
	double prcnt;
	DWORD currentTickCount;
	DWORD totMs;

	static TCHAR  szBuff[SIZE_128] = {0};
	HHMMSS hhmmss;
	DWORD totAniCnt = TotalAnimatCount;
	DWORD duration = Duration;

	if(SpecialCase == INITIALIZE)
	{
		prevAct = __RUN_FINISHED;
		startTick = StartTick;
		lastUpdateTick = GetTickCount();
		callCnt = 0;
		strncpy_s(szBuff, sizeof(szBuff), "Running Simulation...", sizeof(szBuff));
		PostMessage(gdata->hwin, WM_ADD_LIST_BOX_MESSAGE , 0, (LPARAM)szBuff);
	}
	else if(SpecialCase == FINALIZE)
	{
		totMs = GetTickCount()-startTick;
		hhmmss = staticLib.Time_To24HrMinSec((totMs)/1000);
		
		_stprintf_s(szBuff, sizeof(szBuff), "Simulation Finished.  Total Time: %02d:%02d:%02d (%d ms)",
			hhmmss.hour, hhmmss.min, hhmmss.sec, totMs);
		PostMessage(gdata->hwin, WM_UPDATE_PROGRESS_MESSAGE , 0, (LPARAM)szBuff);
		return;
	}
	else
	{
		currentTickCount = GetTickCount();
		if(currentTickCount - lastUpdateTick < REFRESHTICKS && prevAct != __RUN_FINISHED)
			return;

		lastUpdateTick = GetTickCount();
	}

	state = gdata->sce.GetState();
	prevAct = state.activity;

	prcnt = state.currentAnimat; // 0 indexed
	prcnt = state.currentIteration; // 0 indexed
	//totAniCnt = gdata->sce.GetAnimatCount();
	//duration = gdata->sce.GetDurationSeconds();

	prcnt = (double)(state.currentIteration*(totAniCnt) + (state.currentAnimat + 1)) / (double) (totAniCnt * duration);
	prcnt *= 100;


	switch(state.activity)
	{
	case __RUN_FINISHED:
		totMs = GetTickCount()-startTick;
		hhmmss = staticLib.Time_To24HrMinSec((totMs)/1000);
		_stprintf_s(szBuff, sizeof(szBuff), "Simulation Finished.  Total Time: %02d:%02d:%02d (%d ms)",
			hhmmss.hour, hhmmss.min, hhmmss.sec, totMs);
		PostMessage(gdata->hwin, WM_UPDATE_PROGRESS_MESSAGE , 0, (LPARAM)szBuff);
		break;
	case ___ALLOCOUTPUTBUFF:
		_stprintf_s(szBuff, sizeof(szBuff), "Allocating buffer space... %s", PINWHEEL[callCnt%4]);
		PostMessage(gdata->hwin, WM_UPDATE_PROGRESS_MESSAGE , 0, (LPARAM)szBuff);
		break;
	case ___SCE_INIT:
		_stprintf_s(szBuff, sizeof(szBuff), "Initializing... %s", PINWHEEL[callCnt%4]);
		PostMessage(gdata->hwin, WM_UPDATE_PROGRESS_MESSAGE , 0, (LPARAM)szBuff);
		break;
	case ___SCE_INITANIMATS:
		//_stprintf_s(szBuff, sizeof(szBuff), "Initializing %d animats... %s", state.currentAnimat,
		//	PINWHEEL[callCnt%4]);
		_stprintf_s(szBuff, sizeof(szBuff), "Initializing animat population... %s", PINWHEEL[callCnt%4]);
		PostMessage(gdata->hwin, WM_UPDATE_PROGRESS_MESSAGE , 0, (LPARAM)szBuff);
		break;
	case ___SCE_RUNITERATING:		
		// Update the progress bar.
		_stprintf_s(szBuff, sizeof(szBuff), "Progress: iteration %04d of %04d, animat %04d of %04d",
			state.currentIteration+1, duration+1, state.currentAnimat+1, totAniCnt);
		PostMessage(gdata->hwin, WM_UPDATE_PROGRESS_MESSAGE , 0, (LPARAM)szBuff);
		PostMessage(gdata->hwin, WM_UPDATE_PROGRESS_BAR, (WPARAM)prcnt, 0);
		break;
	case ___SCE_RUNBUFFERFLUSH:
		// Update the message
		_stprintf_s(szBuff, sizeof(szBuff), "Flushing buffers to file: animat %04d of %04d",
				state.bufferState.currentAnimatFlush, state.bufferState.bufferIterationCapacity);
		PostMessage(gdata->hwin, WM_UPDATE_PROGRESS_MESSAGE , 0, (LPARAM)szBuff);
		break;
	case ___SCE_PAUSED:
		// Update the progress bar.
		_stprintf_s(szBuff, sizeof(szBuff), "Progress: %2d %c, paused...", prcnt, '%');
		PostMessage(gdata->hwin, WM_UPDATE_PROGRESS_MESSAGE , 0, (LPARAM)szBuff);
		break;
	}
	callCnt++;
}



/*****************************************************************************************
* FUNCTION: ScenarioThread() (a thread process)
* 
* PARAMETERS:
*	LPVOID lpParameter: A pointer to the GLOBALDATA struct.
*
* RETURN VALUE:
*	DWORD value 0.  Holds no meaning at this time.
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*
* DESCRIPTION:
*		This thread runs the 3mb scenario, runs the process that extracts data from a
*	file, and provides feedback to the parent process's GUI.  Scenario execution and
*	data extraction are done within this thread because 3mb has a GUI format and by
*	running these processes in threads the GUI doesn't lock up and feedback can be
*	presented to the user.
*****************************************************************************************/
DWORD WINAPI ScenarioThread(LPVOID lpParameter)
{
	GLOBALDATA *gdata = (GLOBALDATA *)lpParameter;
	//SCESTATE sceState; 
	RESLT mbsRes;
	HWND hwnd;
	int index;
	static TCHAR szBuff[SIZE_128];
	static TCHAR szBuff2[SIZE_128];
	static TCHAR szBuffCaption[SIZE_128];
	//size_t bufferLen; // length (not size) of the buffer in terms of the number of characters it holds.
	USERPARAMS usrPrms; // scenario user params
	DWORD startTick = GetTickCount();
	DWORD totAniCnt;
	DWORD duration;
	BOOL windowPreviouslyDisabled;
	SCEACTIVITY sceAct;

	//int remSecs = 0;
	//HHMMSS hhmmss;
	//INT_PTR	dlgRes = IDCANCEL;
	//BOOL ofnRes = FALSE;
	//DWORD updateCount;
	//BOOL abort;

	// Verify only a single instance of this thread is running.  Catch it in debug mode
	// with an assertion and handle it in release mode with a return.
	_ASSERT(gdata->sceRunThreadInf.running == FALSE);
	_ASSERT(gdata->sceRunThreadInf.exit == FALSE);
	if(gdata->sceRunThreadInf.running == TRUE)
		return 0;

	// Indicate This thread is running.
	gdata->sceRunThreadInf.running = TRUE;

	gdata->sce.SetOutputDirectory(gdata->szOutputFolder);

	//----------------------------------------------------------------------------------//
	// Launch the scenario process thread.
	//------------------------------------//
	totAniCnt = gdata->sce.GetAnimatCount();
	duration = gdata->sce.GetDurationSeconds();
	if(OK == (mbsRes = gdata->sce.RunScenario()))
	{
		UpdateRunScenario(gdata, totAniCnt, duration, INITIALIZE, startTick);

		PostMessage(gdata->hwin, WM_UPDATE_GUI, NULL, NULL);

		// Enable run button so user may abort run
		EnableWindow(GetDlgItem(gdata->hwin, ID_RUN_SIMULATION), TRUE);

		// Provide some initial feedback to the user
		hwnd = GetDlgItem(gdata->hwin, IDC_LIST_MESSAGES);
		index = SendMessage(hwnd, LB_ADDSTRING, 0, (LPARAM)"Initializing Simulation...");
		SendMessage(hwnd, LB_SETCARETINDEX , index, 0);
		PostMessage(gdata->hwin, WM_UPDATE_GUI, NULL, NULL);

		// Continuously poll the extraction process and update the GUI until either the
		// user aborts or the scenario process finishes.
		sceAct = gdata->sce.GetState().activity;
		while(gdata->sceRunThreadInf.exit == FALSE && sceAct != __RUN_FINISHED)
		{
			UpdateRunScenario(gdata, totAniCnt, duration);
#if 0
			windowPreviouslyDisabled = EnableWindow(gdata->hwin, TRUE);
			if(windowPreviouslyDisabled != FALSE)
				windowPreviouslyDisabled = FALSE;
#endif
			Sleep(25);
			sceAct = gdata->sce.GetState().activity;
		}

		// If the user aborted the simulation then signal the simulation process to abort
		// and wait until it indicates it is finished running.
		if(gdata->sceRunThreadInf.exit == TRUE)
		{
			gdata->sce.AbortRun();
			while(gdata->sce.GetState().activity != __RUN_FINISHED)
				Sleep(30);
		}
		UpdateRunScenario(gdata, totAniCnt, duration);
	}

	// Handle the various ways the scenario execution may have ceased.
	if(gdata->sceRunThreadInf.exit == TRUE)
	{
		// The user aborted.  Post a message.
		strncpy_s(szBuff, sizeof(szBuff), "Run aborted", sizeof(szBuff));
		PostMessage(gdata->hwin, WM_ADD_LIST_BOX_MESSAGE, 0, (LPARAM)szBuff);
	}
	else if(mbsRes != OK)
	{
		 // There was an error with the extraction process
		memset(szBuff, 0, sizeof(szBuff));
		staticLib.MbsResultToString(mbsRes, szBuff, sizeof(szBuff), szBuffCaption, sizeof(szBuff));
		MessageBox(gdata->hwin, szBuff, szBuffCaption, MB_ICONHAND);
	}
	else if(gdata->sce.GetState().errorStatus != OK)
	{
		memset(szBuff, 0, sizeof(szBuff));
		staticLib.MbsResultToString(gdata->sce.GetState().errorStatus, szBuff, sizeof(szBuff), szBuffCaption, sizeof(szBuffCaption));
		MessageBox(gdata->hwin, szBuff, szBuffCaption, MB_ICONHAND);
	}
	else // scenario finished execution
	{
		PostMessage(gdata->hwin, WM_UPDATE_PROGRESS_BAR, 100, 0);
		strncpy_s(szBuff, sizeof(szBuff), "Done", sizeof(szBuff));
		PostMessage(gdata->hwin, WM_ADD_LIST_BOX_MESSAGE , 0, (LPARAM)szBuff);
		UpdateRunScenario(gdata, totAniCnt, duration, FINALIZE);
	}
	////////////////////////////////////////////////////////////////////////////////////////////


	// Post simulation variable assignment
	gdata->sceRunThreadInf.running = FALSE;
	gdata->sceRunThreadInf.exit = FALSE;
	PostMessage(gdata->hwin, WM_UPDATE_GUI, NULL, NULL);

	if(gdata->textOutputConfig.enabled == TRUE && gdata->sce.GetConfiguration().output.enabled == TRUE)
	{

		//88888888888888888888888888888888888888888888888888888888888888888888888888888888888//
		_ASSERT(gdata->extractThreadInf.running == FALSE);

		if(gdata->extractThreadInf.running == TRUE)
		{			
			// Launch the proces that waits for the scenario execution and data
			// extractions processes to cease.
			gdata->sceAbortThreadInf.hdl = 
				CreateThread(NULL, 0, &AbortProcessThreadProc, gdata, 0, &gdata->sceAbortThreadInf.id);
		}
		while(gdata->extractThreadInf.running == TRUE)
			Sleep(100);


		//------------------------------------------------------------------------------//
		// Initialize the OFN (Open File Name) struct for the data extractor
		//------------------------------------------------------------------//
		usrPrms = gdata->sce.GetConfiguration();

		// Reference the OFN string pointers to point to the appropriate TCHAR buffers.

		// Commented out 11/2/09
		//gdata->extractThreadInf.fileInf.ofn.lpstrFile = gdata->extractThreadInf.fileInf.szFileName;
		//gdata->extractThreadInf.fileInf.ofn.lpstrFileTitle = gdata->extractThreadInf.fileInf.szTitleName;

		// Set up the file name (file name plus path).
		_stprintf_s(gdata->extractThreadInf.fileInf.szFileName,	    // Destination buffer
			TCHARBFLEN(gdata->extractThreadInf.fileInf.szFileName), // Destination buffer size (length)
			"%s\\%s",											    // Format
			usrPrms.szOutputDir, usrPrms.szScenarioTitle);		    // Format paramaters

		// Commented out 11/2/09
//		RemoveExtension(gdata->extractThreadInf.fileInf.szFileName);
		_tcsncat_s(gdata->extractThreadInf.fileInf.szFileName,
			TCHARBFLEN(gdata->extractThreadInf.fileInf.szFileName),
			".3mb",
			_tcslen(".3mb")+1);


		// Set up the file title (no path)
		_tcsncpy_s(gdata->extractThreadInf.fileInf.szTitleName,
			TCHARBFLEN(gdata->extractThreadInf.fileInf.szTitleName),
			usrPrms.szScenarioTitle,
			_tcslen(usrPrms.szScenarioTitle));

//		RemoveExtension(gdata->extractThreadInf.fileInf.szTitleName);
		_tcsncat_s(gdata->extractThreadInf.fileInf.szTitleName,
			TCHARBFLEN(gdata->extractThreadInf.fileInf.szTitleName),
			".3mb",
			_tcslen(".3mb")+1);
		//------------------------------------------------------------------------------//

		PostMessage(gdata->hwin, WM_3MBS_EXTRACT_BIN_TO_TEXT, NULL, NULL);
		//88888888888888888888888888888888888888888888888888888888888888888888888888888888888//
	}
	return 0;
}

DWORD WINAPI AbortProcessThreadProc(LPVOID lpParameter)
{
	GLOBALDATA *gdata = (GLOBALDATA *)lpParameter;
	//SCESTATE sceState; 

	// If the scenario is running, so this is a call to abort it.
	_ASSERT((gdata->sceRunThreadInf.running == TRUE && gdata->extractThreadInf.running == FALSE) ||
		(gdata->sceRunThreadInf.running == FALSE && gdata->extractThreadInf.running == TRUE));

	// Tell class Scenario to abort its run and the gui update thread to exit.  Wait for the
	// GUI update thread to exit.
	if(gdata->sceRunThreadInf.running == TRUE)
	{
		gdata->sceRunThreadInf.exit = TRUE;
		while(gdata->sceRunThreadInf.running == TRUE)
			Sleep(30);
	}
	else if(gdata->extractThreadInf.running == TRUE)
	{
		gdata->extractThreadInf.exit = TRUE;
		while(gdata->extractThreadInf.running == TRUE)
			Sleep(30);
	}
	PostMessage(gdata->hwin, WM_UPDATE_GUI, NULL, NULL);
	return 0;
}


DWORD WINAPI UpdateMemoryProc(LPVOID lpParameter)
{
	UPDATETHREADINF *tp = (UPDATETHREADINF *)lpParameter;

	tp->threadInf.running = TRUE;
	Sleep(2000); // give time for the GUI to intialize.

	while(tp->threadInf.exit == FALSE)
	{
		PostMessage(tp->hWnd, WM_UPDATE_GUI, NULL, NULL);
		Sleep(1000); // update the gui every second.
	}
	tp->threadInf.running = FALSE;
	CloseHandle(tp->threadInf.hdl);
	tp->threadInf.hdl = NULL;

	return 0;
}



RESLT
MyGetOpenFileName(FILE_INFO *FI, HWND OwnerWin, const TCHAR *szFilter, const TCHAR *szExtension, BOOL *FileSelected)
{
	return MyGetOpenSaveFileName(FALSE, FI, OwnerWin, szFilter, szExtension, FileSelected);
}

RESLT
MyGetSaveFileName(FILE_INFO *FI, HWND OwnerWin, const TCHAR *szFilter, const TCHAR *szExtension, BOOL *FileSelected)
{
	return MyGetOpenSaveFileName(TRUE, FI, OwnerWin, szFilter, szExtension, FileSelected);
}



//----------------------------------------------------------------------------------//
// Works I,
//const TCHAR *sz1 = TEXT("Bathymetry Files (*.BTH);Text Files (.TXT)\0*.bth;*.txt\0");
//const TCHAR *sz2 = TEXT("bth\0");
//----------------------------------------------------------------------------------//

//----------------------------------------------------------------------------------//
// Works I,
const TCHAR sz1[] = TEXT("Bathymetry Files (*.BTH);Text Files (.TXT)\0*.bth;*.txt\0");
const TCHAR sz2[] = TEXT("bth\0");
//----------------------------------------------------------------------------------//

/*----------------------------------------------------------------------------------------

  FUNCTION: MyGetOpenSaveFileName()
 
  DESCRIPTION:
    Creates an Open File dialog box for both opening and saving a file.  

  PARAMETERS:
    BOOL Save
		Indicates to create a SaveFileDlgBox rather than an OpenFileDlg Box.

	FILE_INFO *FI
		Pointer to a FILE_INFO struct that contains a buffer for file name, file title, if
		the the file has been named, and the current need-save state.

	HWND hWnd
		Owner window of the Open/Save FileDlgBox to be created.

	TCHAR *szFilter
		File type filter.

	BOOL *FileSel
		Indicates if the user selected a file.

  RETURN VALUE:
	UINT - One of the possible 

  REVISION HISTORY:
     Date    Name   Change
   --------  ----   ------

----------------------------------------------------------------------------------------*/
RESLT MyGetOpenSaveFileName(BOOL Save, FILE_INFO *FI, HWND hWnd, const TCHAR *szFil, const TCHAR *szExt, BOOL *FileSel)
{
	size_t len1, len2;
	OPENFILENAME ofn;

	// Verify that no NULL references were passed in.
	if(FI == NULL || szFil == NULL || szExt == NULL || FileSel == NULL)
		return PARAM_HAD_NULLREF_ERROR;

	*FileSel = FALSE;

	// Verify that the filter and extension have some value.
	if(_tcslen(szFil) == 0 || _tcslen(szExt) == 0)
		return PARAM_INVALID_ERROR;

	// Initialize local variables and zero out the passed in FILE_INFO struct
	memset(&ofn, 0, sizeof(OPENFILENAME));
	//memset(FI, 0, sizeof(FILE_INFO));

	InitOFN(&ofn, hWnd, szFil, szExt, FI->szFileName, FI->szTitleName);

	// If saving a file (rather than opening a file) set the flag that prompts the user to
	// confirm a file overrite should the file already exist.
	if(Save == TRUE)
		ofn.Flags = OFN_OVERWRITEPROMPT;

	// Prompt the user for the file to load.  A non-zero return value means a file was
	// selected.  A FALSE returned means either the user hit cancel or
	// the path is too long given the buffer size for the File Title and File Name.
#pragma message("check out the SHBrowseForFolder() function")
#if 1
	if(Save == FALSE)
	{
		if(0 == GetOpenFileName(&ofn))
			return OK; // no error even thought no file was selected.  Success remains FALSE.
	}
	else
	{
		if(0 == GetSaveFileName(&ofn))
			return OK; // no error even thought no file was selected.  Success remains FALSE.
	}
#endif
//	if(0 == GetSaveFileName(&ofn))
//		return OK; // no error even thought no file was selected.  Success remains FALSE.

	// A file was selected so set FileSel to TRUE.
	*FileSel = TRUE;

	// Verify the path is not too long.
	len1 = _tcslen(FI->szTitleName);
	len2 = _tcslen(FI->szFileName);
	if(_tcslen(FI->szTitleName) >= MAX_PATH || _tcslen(FI->szFileName) >= MAX_PATH)
		return FILE_PATH_TOO_LONG;
	return OK;
}


void InitOFN(OPENFILENAME *ofn, HWND hwnd, const TCHAR *szFilter, const TCHAR *szExtension, TCHAR *szFileName, TCHAR *szFileTitle)
{
	ofn->lpstrFilter       = szFilter;
	ofn->lpstrDefExt       = szExtension;
	ofn->lpstrFile         = szFileName;
	ofn->lpstrFileTitle    = szFileTitle;

	ofn->hwndOwner         = hwnd;

	ofn->lStructSize       = sizeof(OPENFILENAME);
	ofn->hInstance         = NULL;
	ofn->lpstrCustomFilter = NULL;
	ofn->nMaxCustFilter    = 0;
	ofn->nFilterIndex      = 0;
	ofn->nMaxFile          = BUFFERED_MAX_PATH;
	ofn->nMaxFileTitle     = BUFFERED_MAX_PATH;
	ofn->lpstrInitialDir   = NULL;
	ofn->lpstrTitle        = NULL;
	ofn->Flags             = OFN_HIDEREADONLY | OFN_CREATEPROMPT;
	ofn->nFileOffset       = 0;
	ofn->nFileExtension    = 0;
	ofn->lCustData         = 0L;
	ofn->lpfnHook          = NULL;
	ofn->lpTemplateName    = NULL;
}


void InitOFN(OPENFILENAME *ofn, HWND hwnd,  OLD_FILE_INFO *fi)
{
	ofn->lpstrFilter       = fi->szFilter;
	ofn->lpstrDefExt       = fi->szDefExt;
	ofn->hwndOwner         = hwnd ;
	ofn->lpstrFile         = fi->szFileName ;
	ofn->lpstrFileTitle    = fi->szTitleName ;

	ofn->lStructSize       = sizeof (OPENFILENAME) ;
	ofn->hInstance         = NULL ;
	ofn->lpstrCustomFilter = NULL ;
	ofn->nMaxCustFilter    = 0 ;
	ofn->nFilterIndex      = 0 ;
	ofn->nMaxFile          = MAX_PATH ;
	ofn->nMaxFileTitle     = MAX_PATH ;
	ofn->lpstrInitialDir   = NULL ;
	ofn->lpstrTitle        = NULL ;
	ofn->Flags             = OFN_HIDEREADONLY | OFN_CREATEPROMPT ;
	ofn->nFileOffset       = 0 ;
	ofn->nFileExtension    = 0 ;
	ofn->lCustData         = 0L ;
	ofn->lpfnHook          = NULL ;
	ofn->lpTemplateName    = NULL ;
}


// Local version of the FileSaved() function not accessable to the outside.
void FileSaved(FILE_INFO *fi, BOOL FileChanged, HWND DisplayWindow, const TCHAR* szFileExtension)
{
	TCHAR szDisplay[BUFFERED_MAX_PATH];
	size_t len;

	// _tcsnlen() returns zero if larger than the buffer length passed into it.
	memset(szDisplay, 0, sizeof(szDisplay));
	len = _tcsnlen(fi->szFileName, TCHARBFLEN(fi->szFileName));

	// Set the title to be displayed at the top of the window if a window handle was passed in.
	if(DisplayWindow != NULL)
	{
		if(fi->bNamed == FALSE)
		{
			if(szFileExtension != NULL)
				_stprintf_s(szDisplay, TCHARBFLEN(szDisplay), TEXT("Untitled.%s"), szFileExtension);
			else
				_stprintf_s(szDisplay, TCHARBFLEN(szDisplay), TEXT("Untitled"));
		}
		else
		{
			// Later on make the size that variable 'len' is compared to (currently 1 to force just
			// showing file title for now) variable and based on the size of the dialog box.
			// File title includes the file extension
			if(len == 0 || len > 1)
				_stprintf_s(szDisplay, TCHARBFLEN(szDisplay), TEXT("%s"), fi->szTitleName);
			else
				_stprintf_s(szDisplay, TCHARBFLEN(szDisplay), TEXT("%s"), fi->szFileName);
		}
	}

	// Handle if the file was changed.
	if(FileChanged == FALSE)
	{
		fi->bNeedSave = FALSE;
	}
	else
	{
		_tcscat_s(szDisplay, TCHARBFLEN(szDisplay), TEXT("*"));
		fi->bNeedSave = TRUE;
	}

	// Finally, set the dialog box display to show the file name/title.
	if(DisplayWindow != NULL)
		SetWindowText(DisplayWindow, szDisplay);
}

void FileSaved(FILE_INFO *fi, HWND DisplayWindow, const TCHAR* szFileExtension)
{
	FileSaved(fi, FALSE, DisplayWindow, szFileExtension);
}

void FileChanged(FILE_INFO *fi, HWND DisplayWindow, const TCHAR* szFileExtension)
{
	FileSaved(fi, TRUE, DisplayWindow, szFileExtension);
}

void UpdateGuiSpeciesListWindow(CScenario *pSce, HWND speListHwnd, int index)
{
	int listLength = pSce->GetSpeciesCount();
	int i;
	TCHAR szBuff[SIZE_96];
	SPECIESSUMMARY cs;

	// Negative 1 means to redraw the entire window
	if(index == -1)
	{
		SendMessage(speListHwnd, LB_RESETCONTENT, 0, 0);
		for(i=0; i<listLength; i++)
		{
			pSce->GetSpeciesDisplayTitle(i, szBuff, SIZE_96);
			cs = MbsGetSingleClusterSummary(pSce, i);
			MbsClusterSummaryToString(&cs, szBuff, SIZE_96);
			SendMessage(speListHwnd, LB_ADDSTRING, i, (LPARAM)szBuff);
		}
	}
	else
	{
		pSce->GetSpeciesDisplayTitle(index, szBuff, SIZE_96);
		cs = MbsGetSingleClusterSummary(pSce, index);
		MbsClusterSummaryToString(&cs, szBuff, SIZE_96);
		SendMessage(speListHwnd, LB_DELETESTRING, index, (LPARAM)NULL);
		SendMessage(speListHwnd, LB_INSERTSTRING, index, (LPARAM)szBuff);
	}
}


void UpdateGuiSpeciesListWindow(HWND speListHwnd, int index)
{
	int listLength = CBitmapEnvFunctions::GetSpeciesCount();
	int i;
	TCHAR szBuff[SIZE_96];
	SPECIESSUMMARY cs;

	// Negative 1 means to redraw the entire window
	if(index == -1)
	{
		SendMessage(speListHwnd, LB_RESETCONTENT, 0, 0);
		for(i=0; i<listLength; i++)
		{
			CBitmapEnvFunctions::GetSpeciesDisplayTitle(i, szBuff, SIZE_96);
			cs = MbsGetSingleClusterSummary(i);
			MbsClusterSummaryToString(&cs, szBuff, SIZE_96);
			SendMessage(speListHwnd, LB_ADDSTRING, i, (LPARAM)szBuff);
		}
	}
	else
	{
		CBitmapEnvFunctions::GetSpeciesDisplayTitle(index, szBuff, SIZE_96);
		cs = MbsGetSingleClusterSummary(index);
		MbsClusterSummaryToString(&cs, szBuff, SIZE_96);
		SendMessage(speListHwnd, LB_DELETESTRING, index, (LPARAM)NULL);
		SendMessage(speListHwnd, LB_INSERTSTRING, index, (LPARAM)szBuff);
	}
}

SPECIESSUMMARY MbsGetSingleClusterSummary(CScenario *pSce, int SpeciesIndex)
{
	SPECIESSUMMARY cs;
	memset(&cs, 0, sizeof(SPECIESSUMMARY));
	cs.animatCount = pSce->GetAnimatCount(SpeciesIndex);
	cs.individualCount = pSce->GetIndivdualCount(SpeciesIndex);
	cs.podCount = pSce->GetPodCount(SpeciesIndex);
	return cs;
}

SPECIESSUMMARY MbsGetSingleClusterSummary(int SpeciesIndex)
{
	SPECIESSUMMARY cs;
	memset(&cs, 0, sizeof(SPECIESSUMMARY));
	cs.animatCount = CBitmapEnvFunctions::GetAnimatCount(SpeciesIndex);
	cs.individualCount = CBitmapEnvFunctions::GetIndividualCount(SpeciesIndex);
	cs.podCount = CBitmapEnvFunctions::GetPodCount(SpeciesIndex);
	return cs;
}

TCHAR *MbsClusterSummaryToString(SPECIESSUMMARY *pCs, TCHAR *Buff, int BuffLen)
{
	TCHAR buff[SIZE_128];

	if(pCs->animatCount > 0)
	{
		if(pCs->podCount == 0)
		{
			_snprintf_s(buff, SIZE_128, SIZE_128-1, "\"%s\":  %dani/%dind ",
				Buff, pCs->animatCount, pCs->individualCount);
		}
		else if(pCs->individualCount == 0)
		{
			_snprintf_s(buff, SIZE_128, SIZE_128-1, "\"%s\":  %dani/%dpod ",
				Buff, pCs->animatCount, pCs->podCount);
		}
		else // both pods and individuals.
		{
			_snprintf_s(buff, SIZE_128, SIZE_128-1, "\"%s\":  %dani/%dpod/%dind ",
				Buff, pCs->animatCount, pCs->podCount, pCs->individualCount);
		}
	}
	else
	{
		_snprintf_s(buff, BuffLen, BuffLen-1, "\"%s\": unpopulated", Buff);
	}

	strcpy_s(Buff, BuffLen, buff);
	return Buff;
}

TCHAR *MbsGetPodSummaryString(int SpeciesIndex, int PodIndex, TCHAR *Buffer, int BufferLength)
{
	TCHAR *szFocal = "Centrd";
	int   podSize;
	double focalDist;

	memset(Buffer, 0, BufferLength);
	if(SpeciesIndex < 0 || SpeciesIndex >= CBitmapEnvFunctions::GetSpeciesCount() ||
		PodIndex < 0 || PodIndex >= CBitmapEnvFunctions::GetPodCount(SpeciesIndex))
	{
		_snprintf_s(Buffer, BufferLength, BufferLength, "Param Error, MbsGetPodSummaryString()");
		return Buffer;
	}

	// Get the focal leader type.
	if(CBitmapEnvFunctions::GetPodLeaderType(SpeciesIndex, PodIndex) == ANIMAT)
		szFocal = "Animat";

	focalDist = CBitmapEnvFunctions::GetPodLeaderFocalDistance(SpeciesIndex, PodIndex);
	podSize = CBitmapEnvFunctions::GetPodMemberCount(SpeciesIndex, PodIndex);

	// PodIndex + 1 displays the pod number rather than its index (a more natural feel
	// for the user).
	_snprintf_s(Buffer, BufferLength, BufferLength,"Pod %03d (%s\\%.1f\\%d)", PodIndex+1, szFocal, focalDist, podSize);

	return Buffer;
}

INHABITINF *AdjustCoordinateBufferSize(INHABITINF *pCoord, int *pBufferSize, int NumAnimats)
{
	if(*pBufferSize >= NumAnimats)
		return pCoord;

	if(pCoord != NULL)
		delete [] pCoord;
	pCoord = NULL;

	while(*pBufferSize < NumAnimats)
		*pBufferSize *= 2;

	pCoord = new INHABITINF[*pBufferSize];
	return pCoord;
}

void IOMessageBox(RESLT Res, HWND Hwnd, TCHAR *AppendBuffer)
{
	TCHAR header[SIZE_64];
	TCHAR message[SIZE_128];

	switch(Res)
	{
	case OK:
		break;
	case OK_EOFREACHED:
		strcpy_s(header, sizeof(header), "End Of File Reached");

		// the -1 in _snprintf_s() is to ensure room for terminating null character.
		if(_tcslen(AppendBuffer) != 0)
			_snprintf_s(message, SIZE_128, SIZE_128, "Reached end of file in file:\n  %s", AppendBuffer);
		else
			strcpy_s(message, sizeof(message), header);
		break;
	case MEMALLOC_ERROR:
		strcpy_s(header, sizeof(header), "Memory Allocation Error");
		if(_tcslen(AppendBuffer) != 0)
		{
			_snprintf_s(message, SIZE_128, SIZE_128,
				"Couldn't initialize enough memory for data:\n  %s\nTry closing some applications.", AppendBuffer);
		}
		else
		{
			strcpy_s(message, sizeof(message), header);
		}
		break;

	case OPENFILEREAD_ERROR:
	case OPENFILEWRITE_ERROR:
		strcpy_s(header, sizeof(header), "File Open Error");
		if(_tcslen(AppendBuffer) != 0)
			_snprintf_s(message, SIZE_128, SIZE_128, "Unable to open file:\n  %s", AppendBuffer);
		else
			strcpy_s(message, sizeof(message), header);
		break;

	case CREATEBINARYOUTPUT_ERROR:
	case OPENTEXTOUTPUTFILE_ERROR:
		break;

	case FILEREAD_ERROR:
		strcpy_s(header, sizeof(header), "Read Error, file not read in.");
		if(_tcslen(AppendBuffer) != 0)
		{
			_snprintf_s(message, SIZE_128, SIZE_128, "A Read Error occured while reading\nmemory associated with file:\n  %s",
				AppendBuffer);
		}
		else
		{
			strcpy_s(message, sizeof(message), header);
		}
		break;

	case FILEWRITE_ERROR:
		strcpy_s(header, sizeof(header), "Write Error, file may not have properly written to disk.");
		if(_tcslen(AppendBuffer) != 0)
		{
			_snprintf_s(message, SIZE_128, SIZE_128, "A Write Error occured while writing\nmemory associated with file:\n  %s",
				AppendBuffer);
		}
		else
		{
			strcpy_s(message, sizeof(message), header);
		}
		break;

	case WRONGFILETYPE_ERROR:
		strcpy_s(header, sizeof(header), "Wrong File Type Error");
		if(_tcslen(AppendBuffer) != 0)
			_snprintf_s(message, SIZE_128, SIZE_128, "%s\nis the wrong file type for this application", AppendBuffer);
		else
			strcpy_s(message, sizeof(message), header);
		break;
	case INVALIDHANDLE_ERROR:
		strcpy_s(header, sizeof(header), "Invalid Handle");
		if(_tcslen(AppendBuffer) != 0)
		{
			_snprintf_s(message, SIZE_128, SIZE_128, "The handle to file\n  %s\nis invalid so the file can't be accessed",
				AppendBuffer);
		}
		else
		{
			strcpy_s(message, sizeof(message), "The handle to the requested file is\ninvalid and therefore inaccessable");
		}
		break;

	case USERMODELLINELENGTHEXCEEDED_ERROR:
		strcpy_s(header, sizeof(header), "User Model Maximum Line Length Exceeded");
		if(_tcslen(AppendBuffer) != 0)
			_snprintf_s(message, SIZE_128, SIZE_128, "File %s\nhas too many columns of data.\nFile not read in.", AppendBuffer);
		else
			strcpy_s(message, sizeof(message), header);
		break;

	case UNRECOGNIZED_SPECIES_MATRIX_PARAM_ERROR:
		strcpy_s(header, sizeof(header), "Species File Param Unrecognized");
		if(_tcslen(AppendBuffer) != 0)
			_snprintf_s(message, SIZE_128, SIZE_128, "File %s\nhas an unrecognized model type.\nFile not read in.", AppendBuffer);
		else
			strcpy_s(message, sizeof(message), header);
		break;
	default:
		strcpy_s(header, sizeof(message), "Unknown Error");
		if(_tcslen(AppendBuffer) != 0)
			_snprintf_s(message, SIZE_128, SIZE_128, "File %s\nhas an unknown error.  File not read in.", AppendBuffer);
		else
			strcpy_s(message, sizeof(message), header);
		break;
	}
	MessageBox(Hwnd, message, header, MB_ICONEXCLAMATION);
}
