/*----------------------------------------------------------------------------------------
	3MBS.cpp : Defines the entry point for the application.
	
	Author:  Matt Cross
			 MattSnail@cox.net, mcross@rohan.sdsu.edu
			 261-8775

	Date:    Initial (GUI) coding 11/2004

	Description:
			 GUI dialog box code wrapped around prexisting application.
 

  //M:\MMMBS\hjmscripts\mmmb.in
/*--------------------------------------------------------------------------------------*/

#include "3mb.h"
#include "3MBSLib.h"
#include "3mbGuiFunctions.h"
#include "3mbScenarioConfig.h"
#include "3mbSpeciesMdl.h"
#include "3mbSeedDlgProc.h"
#include "OutputReader.h"
#include "FileExtracter.h"
#include "3mbSystemCheck.h"
#include "params.h"
#include "3mbSetOptLimiIntrvlDlg.h"
#include "staticLib.h"

//-----------------------//
// TESTING AND DEBUGGING
#include "BrowseFolder.h"
//-----------------------//


#define SZ_BATHYMETRY "Bathymetry"
#define SZ_SALINITY "Salinity"
#define SZ_TEMPERATURE "Temperature"
#define BATHYMETRY_INDEX 0
#define SALINITY_INDEX 1
#define TEMPERATURE_INDEX 2


GLOBALDATA *gdata;
C3MBRandom g_3mbRandom;
C3mbStaticsLib staticLib;

/*----------------------------------------------------------------------------------------
  Global Variables:
----------------------------------------------------------------------------------------*/
HINSTANCE hInst;								// current instance
TCHAR szTitle[BUFFERED_MAX_PATH];				// The title bar text
TCHAR szWindowClass[BUFFERED_MAX_PATH];			// The title bar text


/*----------------------------------------------------------------------------------------
  Function prototypes for callbacks and other GUI related functions
----------------------------------------------------------------------------------------*/
ATOM MyRegisterClass(HINSTANCE hInstance);
BOOL InitInstance(HINSTANCE, int, GLOBALDATA *);
LRESULT CALLBACK DlgProc(HWND, UINT, WPARAM, LPARAM);
LRESULT CALLBACK About(HWND, UINT, WPARAM, LPARAM);


LRESULT CALLBACK SceTimeDlgProc(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam);
LRESULT CALLBACK DisplayDefaultParams(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam);

void UpdateScenarioGUI(GLOBALDATA *gdata);
BOOL RedrawStaticWindows(GLOBALDATA *gdata, int ControlID, HDC DC);
//DWORD WINAPI ScenarioThread(LPVOID lpParameter);
DWORD WINAPI ExtractionThread(LPVOID lpParameter);
LRESULT CALLBACK ExamineBinOutDlgProc(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam);
//void UpdateRunScenario(GLOBALDATA *gdata, BOOL Initialize = FALSE);
//DWORD WINAPI AbortProcessThreadProc(LPVOID lpParameter);



typedef struct ExamineBinOutProcessStruct
{
	TCHAR *szFileName;
	//OPENFILENAME *openFileName;
	CFileExtracter *fileExtractor;
}EXMNBINOUTPROCSTRCT;


/*----------------------------------------------------------------------------------------
  General function prototypes
/*--------------------------------------------------------------------------------------*/
HICON MYICON;



/*----------------------------------------------------------------------------------------
	Just the main function.  Starts the program, handles window messages.
/*--------------------------------------------------------------------------------------*/
int APIENTRY WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
	MSG msg;
	HACCEL hAccelTable;
	GLOBALDATA GDATA;

	hPrevInstance = hPrevInstance; // keep the compiler warning quiet.

	// Initialize global strings                  
	LoadString(hInstance, IDS_APP_TITLE, szTitle, TCHARBFLEN(szTitle));
	LoadString(hInstance, IDC_MY3MBS, szWindowClass, TCHARBFLEN(szWindowClass));

	// Initialize the GUI
	GDATA.bNeedSave				= FALSE;
	GDATA.hwin					= NULL;
	GDATA.bMatlabDetected		= FALSE;
	GDATA.lpCmdLineScenarioFile	= NULL;
	GDATA.autoRunEnabled		= FALSE;
	GDATA.textOutputConfig.enabled = FALSE;
	GDATA.textOutputConfig.iterationsPerFile = 0;
	GDATA.textOutputConfig.splitTextOutput = FALSE;
	memset(&GDATA.sceRunThreadInf, 0, sizeof(THREAD_INF));
	memset(&GDATA.extractThreadInf, 0, sizeof(THREAD_OPENFILE_INF));

	GDATA.outputFolderSet = FALSE;
	GDATA.szOutputFolder[0] = NULL;
	if(strlen(lpCmdLine) > 0)
		GDATA.lpCmdLineScenarioFile = lpCmdLine;

	// make sure the randomizer is seeded.
	g_3mbRandom.mysrand(0);

	// So the appplication's icon appears in the task bar.
//	MYICON = (HICON)LoadImage(hInstance, MAKEINTRESOURCE(IDI_MY3MBSDOLPN),IMAGE_ICON,
//		GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON), 0);

	MYICON = (HICON)LoadImage(hInstance, MAKEINTRESOURCE(IDI_ICON_NEWMAINICON),IMAGE_ICON,
		GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON), 0);

	// Perform application initialization:
	if(!InitInstance (hInstance, nCmdShow, &GDATA)) 
		return FALSE;

	hAccelTable = LoadAccelerators(hInstance, (LPCTSTR)IDC_MY3MBS);

	// Main message loop:
	while(GetMessage(&msg, NULL, 0, 0)) 
	{
		if(!TranslateAccelerator(msg.hwnd, hAccelTable, &msg)) 
		{
			TranslateMessage(&msg);
			DispatchMessage(&msg);
		}
	}

	return msg.wParam;
}



//
//   FUNCTION: InitInstance(HANDLE, int)
//
//   PURPOSE: Saves instance handle and creates main window
//
//   COMMENTS:
//
//        In this function, we save the instance handle in a global variable and
//        create and display the main program window.
//
BOOL InitInstance(HINSTANCE hInstance, int nCmdShow, GLOBALDATA *gdata)
{
   HWND hWnd;

   hInst = hInstance; // Store instance handle in our global variable


   // Comment out the normal CreateWindow function, replace with dialog.
   //hWnd = CreateWindow(szWindowClass, szTitle, WS_OVERLAPPEDWINDOW,
      //CW_USEDEFAULT, 0, CW_USEDEFAULT, 0, NULL, NULL, hInstance, NULL);

   hWnd = CreateDialogParam(hInstance, MAKEINTRESOURCE(IDD_MAIN), NULL,(DLGPROC)DlgProc,
							(LPARAM)gdata);
   if(!hWnd)
   {
      return FALSE;
   }

   ShowWindow(hWnd, nCmdShow);
   UpdateWindow(hWnd);
   return TRUE;
}

/*----------------------------------------------------------------------------------------

	FUNCTION: DlgProc(HWND, unsigned, WORD, LONG)

	PURPOSE:  Processes messages for the main dialog.

	WM_COMMAND	- process the application's menus and buttons
		IDM_ABOUT - handle the menu item about
		ID_FILE_NEW_SCENARIO - menu item new scenario
		ID_FILE_SAVE_SCENARIO - menu item save scenario.
		ID_FILE_SAVE_SCENARIO_AS - menu item save scenario as
		ID_EXIT - exit
		IDM_EXIT - exit
		ID_BUTTON_CONFIG_OUTPUT - handle button press to configure output
		ID_FILE_CONFIGURE_OUTPUT - handle menu item to configure output
		ID_FILE_BATHYMETRY - handle menu item to load bathymetry file.
		IDC_BUTTON_BATHYMETRY - handle button press to load bathymetry file.
		ID_SPECIES_OPEN - handle menu item to open a species file.
		ID_BATHYMETRY_NEW - not used.  Future intent to edit bathymetry files.
		ID_BATHYMETRY_OPEN - not used.  Future intent to edit bathymetry files.
		IDC_BUTTON_TIME - handle button request to edit time of day (opens up another 
						dialog box.)
		IDC_BUTTON_DURATION - handle button request to edit duration of simulation (opens
						another dialog box).
		ID_FILE_ADD_SPECIES - handle menu item request to add species to the simulation.
		IDC_BUTTON_SPEC_ADD - handle button request to add species to the simulation.
		ID_FILE_DELETE_SPECIES - handle menu item request to remove species from the 
								simulation.
		IDC_BUTTON_SPEC_REMOVE - handle button request to remove species from the
								simulation.
		ID_FILE_RUN_SIMULATION - menu item selected to run the simulation.
		ID_RUN_SIMULATION - button pressed to run the simulation

	WM_PAINT	- Paint the main window
	WM_DESTROY	- post a quit message and return
	WM_INITDIALOG - Initialization before the dialog box gets drawn the first time.


/*--------------------------------------------------------------------------------------*/


LRESULT CALLBACK DlgProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
	//HMENU hmenu;
	//BOOL bVal1, bVal2; // general Boolean value usable wherever needed.
	//MENUITEMINFO menuItemAcstcPingInf, menuItemCSVItem, menuItemLimitOutputItem;

		   HHMMSS hr_min_sec;
		   LRESULT dlgResult;
		   INT_PTR dlgRes = IDCANCEL;
		   EXMNBINOUTPROCSTRCT e; // contains information passed into the dialog box that
								  // launches the extraction process.
		   int i;
		   int wmId, wmEvent;
		   USERPARAMS up;
		   HWND	 hwnd;
		   DWORD dwrd;
		   RESLT res;
		   FM_MEMALLOCINF mallcInf;
		   TCHAR szFileName[BUFFERED_MAX_PATH];
		   TCHAR szTitleName[BUFFERED_MAX_PATH];
		   TCHAR szBuff1[SIZE_128] = {0};
		   TCHAR szBuff2[SIZE_128] = {0};
		   TCHAR szBuff3[SIZE_128] = {0};
		   BOOL bFileSelected = FALSE;

	static BOOL	boolLinkStatusToggle = FALSE;
	static BOOL bathLoaded = FALSE;
	static BOOL saltLoaded = FALSE;
	static BOOL tempLoaded = FALSE;
	static SEED_DLG_PARAM bbmDlgParam;
	static SCECONFIGWINPARAMS sceConfigParam;
		   OUTPTINTRVLLMT outputLimIntrvlDlgParam;

	static FILE_INFO fiSce;
	static FILE_INFO fiBath;
	static FILE_INFO fiSalt;
	static FILE_INFO fiTemperature;
	static FILE_INFO fiSpe;
	static FILE_INFO fiCSV;
	static FILE_INFO fiTxt;
	static FILE_INFO fiEnv;

	static RUNDIANOSTICTHREADPARAM diagnosticInf = {0};

	BOOL debugVersion = FALSE;
#ifdef _DEBUG
	debugVersion = TRUE;
#endif

//	SPECIESSUMMARY	clusterSummary, clusterSummaryCmp;


//	PAINTSTRUCT ps;
//	HDC hdc;
	switch (message) 
	{
	case WM_INITDIALOG:
		memset(&fiBath, 0, sizeof(FILE_INFO));
		memset(&fiSalt, 0, sizeof(FILE_INFO));
		memset(&fiTemperature, 0, sizeof(FILE_INFO));
		memset(&fiSpe, 0, sizeof(FILE_INFO));
		memset(&fiTxt, 0, sizeof(FILE_INFO));
		memset(&fiCSV, 0, sizeof(FILE_INFO));

		// The parameter passed into the bathymetry bitmap dialog 
		memset(&bbmDlgParam, 0, sizeof(SEED_DLG_PARAM));
		bbmDlgParam.usageType = SEED_SCENARIO_TYPE;
		bbmDlgParam.hInstance = hInst;
		bbmDlgParam.hWndParent = hWnd;
		bbmDlgParam.pScenario = &gdata->sce;


		gdata = (GLOBALDATA *)lParam;
		gdata->hwin = hWnd;

		SetDlgItemText(hWnd, IDC_STATIC_SCENARIO_FILE,  TEXT("Untitled.sce"));
		SendMessage(hWnd, WM_SETICON, ICON_SMALL, (LPARAM)MYICON);

		// Get an update on ESME and Matlab
		UpdateScenarioGUI(gdata);
		SetDlgItemText(gdata->hwin, IDC_STATIC_CURRENT_TASK, "Current Task:");

		// If auto run is false, break now.  Otherwise, take steps to load in the file
		// named in the command line input and run the simulation.
		if(gdata->lpCmdLineScenarioFile != NULL)
			PostMessage(hWnd, WM_HANDLE_CMDLINE_ARG, 0, 0);

		sprintf_s(szBuff1,
			sizeof(szBuff1)/sizeof(TCHAR),
			"3MB: Marine Mammal Movement And Behavior -- ver %d.%02d",
			MMBSLIB_VERSION_SUPER,
			MMBSLIB_VERSION_SUB);

		if(debugVersion == TRUE)
			strcat_s(szBuff1, sizeof(szBuff1)/sizeof(TCHAR), " (debug)");
		SetWindowText(hWnd, szBuff1);
		break;
		

	case WM_HANDLE_CMDLINE_ARG:
		break;

		// A file name was passed in.  Prep to run the simulation.
	case WM_3MBS_LOAD_SCENARIO:
		if(OK != (res = gdata->sce.LoadFromBinFile(fiSce.szFileName)))
		{
			sprintf_s(szBuff1,
				sizeof(szBuff1)/sizeof(TCHAR),
				TEXT("Problem Loading Scenario %s:  %s"),
				fiSce.szTitleName,
				staticLib.MbsResultToString(res, szBuff2, sizeof(szBuff2)/sizeof(TCHAR)));
			MessageBox(hWnd, szBuff1, "Scenario Load Error", 0);
			break;
		}
		gdata->sce.SetScenarioTitle(fiSce.szTitleName);
		SetDlgItemText(hWnd, IDC_STATIC_SCENARIO_FILE,  TEXT(fiSce.szTitleName));
		fiSce.bNamed = TRUE;
		UpdateGuiSpeciesListWindow(&gdata->sce, GetDlgItem(hWnd, IDC_LIST_SPECIES));
		PostMessage(hWnd, WM_UPDATE_GUI, 0, 0);
		break;

//	case WM_3MB_LOAD_BIN_INTO_MEMORY:
		//if(OK != (res = gdata->fileMgr.InitializePlayback(gdata->displayInf.openFileThreadInf.fileInf.szFileName, &gdata->    displayInf.openFileThreadInf.fileInf

		break;

	case WM_3MBS_EXTRACT_BIN_TO_TEXT: // (2)
		_ASSERT(gdata->sceRunThreadInf.running == FALSE);

		EnableWindow(GetDlgItem(gdata->hwin, ID_RUN_SIMULATION), FALSE);
		EnableWindow(GetDlgItem(gdata->hwin, IDC_EXTRACT_TEXT_FROM_BIN), FALSE);

		// Clear the message list box
		SendMessage(GetDlgItem(hWnd, IDC_LIST_MESSAGES), LB_RESETCONTENT , 0, 0);
		SendMessage(gdata->hwin, WM_UPDATE_PROGRESS_BAR, 0, 0);

		//--------------------------------------------------------------------------//
		// User selected to extract a file.
		//---------------------------------//
		// Immediately disable the run simulation and file extract buttons and launch
		// the extraction thread.
		gdata->extractThreadInf.hdl = 
			CreateThread(NULL, 0, &ExtractionThread, gdata, 0, &gdata->extractThreadInf.id);
		break;

	case WM_ADD_LIST_BOX_MESSAGE:
		hwnd = GetDlgItem(hWnd, IDC_LIST_MESSAGES);
		i = SendMessage(hwnd, LB_ADDSTRING, 0, (LPARAM)lParam);
		SendMessage(hwnd, LB_SETCARETINDEX , i, 0);
		break;

	case WM_UPDATE_GUI:
		UpdateScenarioGUI(gdata);
		break;

	case WM_UPDATE_PROGRESS_MESSAGE:
		SetDlgItemText(hWnd, IDC_STATIC_PROGRESS,  (TCHAR *)lParam);
		break;

	case WM_UPDATE_PROGRESS_BAR:
		SendDlgItemMessage(gdata->hwin, IDC_PROGRESS1, PBM_SETPOS, (int)wParam, 0);
		break;

	case WM_CTLCOLORSTATIC:
		if(RedrawStaticWindows(gdata, GetDlgCtrlID((HWND)lParam), (HDC)wParam))
			return (DWORD)GetSysColorBrush(COLOR_BTNFACE);
		else
			return (DWORD)DefWindowProc(hWnd, message, wParam, lParam);

	case WM_PAINT:
		break;
/*
		UpdateScenarioGUI(gdata);
		hdc = BeginPaint(hWnd, &ps);
		RECT rt;
		GetClientRect(hWnd, &rt);
		EndPaint(hWnd, &ps);
		break;
*/
	case WM_COMMAND:
		wmId    = LOWORD(wParam); 
		wmEvent = HIWORD(wParam); 

		// Parse the menu selections:
		switch(wmId)
		{
		case ID_CATAGORY1_RUN:

			// Put in check to make sure this thread isn't alread running.
			diagnosticInf.sce = &gdata->sce;
			diagnosticInf.threadInf.exit = FALSE;
			diagnosticInf.threadInf.running = FALSE;
			diagnosticInf.threadInf.hdl =
				CreateThread(NULL, 0, &RunDiagnosticThreadProc, &diagnosticInf, 0, &diagnosticInf.threadInf.id);

			// Wrong!  Don't put this here..!!!
			while(diagnosticInf.threadInf.running== FALSE)
				Sleep(10);
			break;
		case IDM_ABOUT:
			DialogBox(hInst, (LPCTSTR)IDD_ABOUTBOX, hWnd, (DLGPROC)About);
			break;
		case ID_HELP_DEFAULTPARAMETERS:
			DialogBox(hInst, (LPCTSTR)IDD_DIALOG_DEFAULT_PARAMS, hWnd, (DLGPROC)DisplayDefaultParams);
			break;


		case ID_FILE_NEW_SCENARIO:
			// Remove entries in the list box.
			SendMessage(GetDlgItem(hWnd, IDC_LIST_SPECIES), LB_RESETCONTENT, 0, 0);

			// Delete the scenario's species. DeleteSpecies() w/o parameters deletes all
			// species in the scenario.
			gdata->sce.DeleteSpecies();

			// Remove environmental data
			gdata->sce.ClearBathymetry();
			gdata->sce.ClearSalinity();
			gdata->sce.ClearTemperature();

			szFileName[0] = 0;
			szTitleName[0] = 0;
			memset(&hr_min_sec, 0, sizeof(HHMMSS));
			gdata->sce.SetDuration(hr_min_sec);
			gdata->sce.SetStartTime(hr_min_sec);
			UpdateScenarioGUI(gdata);

			fiSce.bNamed = FALSE;
			FileSaved(&fiSce, GetDlgItem(hWnd, IDC_STATIC_SCENARIO_FILE), SZSCEDEFEXT);
			return 0;

		case ID_LIMITOUTPUT_PINGCYCLES:
			gdata->sce.SetAcousticSrceLimitOutput(!gdata->sce.GetAcousticSrceLimitOutput());
			UpdateScenarioGUI(gdata);
			break;

		case ID_LIMITOUTPUTBY_FIXEDINTERVAL:
			gdata->sce.GetItervalLimitOutput(&outputLimIntrvlDlgParam.enabled,
				&outputLimIntrvlDlgParam.start, &outputLimIntrvlDlgParam.interval);
			dlgResult = DialogBoxParam(hInst, MAKEINTRESOURCE(IDD_DIALOG_LIMIT_OUTPUT_INTRVL), hWnd,
							   (DLGPROC) DispalyOutputIntervalLimitDlgProc, (LPARAM) &outputLimIntrvlDlgParam);
			if(dlgResult == IDOK)
			{
				gdata->sce.SetItervalLimitOutput(outputLimIntrvlDlgParam.enabled,
					outputLimIntrvlDlgParam.start, outputLimIntrvlDlgParam.interval);
			}
			UpdateScenarioGUI(gdata);
		break;
			break;

		case ID_SPECIFICDATAPOINTTIMES_LOADFILE:
			//--------------------------------------------------------------------------//
			// Prompt the user for a file to open
			//-----------------------------------//
			if(OK == (res = MyGetOpenFileName(&fiCSV, hWnd, SZ_CSV_FILTER, SZ_CSV_DEFEXT, &bFileSelected)))
			{
				// No errors so check if user selected a file
				if(bFileSelected == FALSE)
					break; // file not selected so done here.
			}
			else if(FILE_PATH_TOO_LONG == res)
			{
				// Handle error.  Prepare the error message text.
				sprintf_s(szBuff1,
					sizeof(szBuff1)/sizeof(TCHAR),
					TEXT("Problem with selected CSV file %s:  %s"),
					fiCSV.szTitleName,
					staticLib.MbsResultToString(res, szBuff2, sizeof(szBuff2)/sizeof(TCHAR)));

				// Prepare the caption text
				MessageBox(hWnd, szBuff1, TEXT("File Path Too Long"), 0);
				break;
			}
			else
			{
				sprintf_s(szBuff1,
					sizeof(szBuff1)/sizeof(TCHAR),
					TEXT("Problem encountered while selecting CSV file:  %s"),
					staticLib.MbsResultToString(res, szBuff2, sizeof(szBuff2)/sizeof(TCHAR)));
				MessageBox(hWnd, szBuff1, "Select CSV File Error", 0);
				break;
			}
			//--------------------------------------------------------------------------//


			if(OK != (res = gdata->sce.ReadCSVListFromFile(fiCSV.szFileName)))
			{
				staticLib.MbsResultToString(res, szBuff2, sizeof(szBuff2)/sizeof(TCHAR));
				sprintf_s(szBuff1,
						  sizeof(szBuff1)/sizeof(TCHAR),
						  "CSV File Open Error (%s ):%s",
						  fiCSV.szTitleName,
						  szBuff2);
				MessageBox(hWnd, szBuff1, "Save Times Load Error", 0);
				break;
			}

			UpdateScenarioGUI(gdata);
			break;

		case ID_SPECIFICDATAPOINTTIMES_CLEAR:
			gdata->sce.ClearCSVList();
			UpdateScenarioGUI(gdata);
			break;


		//case IDC_PLAYBACK_FILE:
		case IDC_LOAD_BINFILE:
			memset(&bbmDlgParam, 0, sizeof(SEED_DLG_PARAM));	
			bbmDlgParam.usageType = PLAYBACK_TYPE;
			bbmDlgParam.hInstance = hInst;
			bbmDlgParam.hWndParent = hWnd;
			//bbmDlgParam.pScenario = &gdata->sce;
			dlgResult = DialogBoxParam(hInst, MAKEINTRESOURCE(IDD_DIALOG_VIEWBATHY), hWnd,
							   (DLGPROC) BathyBitmapDlgProc, (LPARAM) &bbmDlgParam);
			break;


/*
		case IDC_LOAD_BINFILE:
			memset(&bbmDlgParam, 0, sizeof(SEED_DLG_PARAM));	
			bbmDlgParam.usageType = PLAYBACK_TYPE;

			// Prompt the user for a .3mb file to open.  If the user selects cancel then
			// break.
			if(OK == (res = MyGetOpenFileName(&gdata->displayInf.openFileThreadInf.fileInf,
											  hWnd,
											  SZ3MBBINOUTFILTER,
											  SZ3MBBINOUTDEFEXT,
											  &bFileSelected)))
			{
				// No errors so check if user selected a file
				if(bFileSelected == FALSE)
					break; // file not selected so done here.
			}
			else if(FILE_PATH_TOO_LONG == res)
			{
				// Handle error.  Prepare the error message text.
				sprintf_s(szBuff1,
					sizeof(szBuff1)/sizeof(TCHAR),
					TEXT("Problem with selected 3MB binary output file %s:  %s"),
					gdata->displayInf.openFileThreadInf.fileInf.szTitleName,
					staticLib.MbsResultToString(res, szBuff2, sizeof(szBuff2)/sizeof(TCHAR)));

				// Prepare the caption text
				MessageBox(hWnd, szBuff1, TEXT("File Path Too Long"), 0);
				break;
			}
			else
			{
				sprintf_s(szBuff1,
					sizeof(szBuff1)/sizeof(TCHAR),
					TEXT("Problem encountered while selecting 3MB binary output file:  %s"),
					staticLib.MbsResultToString(res, szBuff2, sizeof(szBuff2)/sizeof(TCHAR)));
				MessageBox(hWnd, szBuff1, "Select 3MB Binary Output  File Error", 0);
				break;
			}
			//--------------------------------------------------------------------------//

////////////////////////////////////////

			memset(&bbmDlgParam, 0, sizeof(SEED_DLG_PARAM));
			bbmDlgParam.usageType = PLAYBACK_TYPE;
			bbmDlgParam.hInstance = hInst;
			bbmDlgParam.hWndParent = hWnd;
			bbmDlgParam.pScenario = NULL;

			dlgResult = DialogBoxParam(hInst, MAKEINTRESOURCE(IDD_DIALOG_VIEWBATHY), hWnd,
							   (DLGPROC) BathyBitmapDlgProc, (LPARAM) &bbmDlgParam);
			//UpdateGuiSpeciesListWindow(&gdata->sce, GetDlgItem(hWnd, IDC_LIST_SPECIES));
			//UpdateScenarioGUI(gdata);
////////////////////////////////////////


			// Load the scenario in from file.
			//SendMessage(hWnd, WM_3MB_LOAD_BIN_INTO_MEMORY, 0, 0);
			break;
*/

		case ID_FILE_LOAD_SCENARIO:
		case IDC_LOAD_SCENARIO:

			//--------------------------------------------------------------------------//
			// Prompt the user for a file to open
			//-----------------------------------//
			if(OK == (res = MyGetOpenFileName(&fiSce, hWnd, SZSCEFILTER, SZSCEDEFEXT, &bFileSelected)))
			{
				// No errors so check if user selected a file
				if(bFileSelected == FALSE)
					break; // file not selected so done here.
			}
			else if(FILE_PATH_TOO_LONG == res)
			{
				// Handle error.  Prepare the error message text.
				sprintf_s(szBuff1,
					sizeof(szBuff1)/sizeof(TCHAR),
					TEXT("Problem with selected scenario file %s:  %s"),
					fiSce.szTitleName,
					staticLib.MbsResultToString(res, szBuff2, sizeof(szBuff2)/sizeof(TCHAR)));

				// Prepare the caption text
				MessageBox(hWnd, szBuff1, TEXT("File Path Too Long"), 0);
				break;
			}
			else
			{
				sprintf_s(szBuff1,
					sizeof(szBuff1)/sizeof(TCHAR),
					TEXT("Problem encountered while selecting scenario file:  %s"),
					staticLib.MbsResultToString(res, szBuff2, sizeof(szBuff2)/sizeof(TCHAR)));
				MessageBox(hWnd, szBuff1, "Scenario Select File Error", 0);
				break;
			}
			//--------------------------------------------------------------------------//

			//*********************************************//
			// Clean up the existing file.
			//*********************************************//
			// Remove entries in the list box.
			SendMessage(GetDlgItem(hWnd, IDC_LIST_SPECIES), LB_RESETCONTENT, 0, 0);

			// Delete the scenario's species. DeleteSpecies() w/o parameters deletes all
			// species in the scenario.
			gdata->sce.DeleteSpecies();


			//*******************************************//
			// Load the scenario in from file.
			//*******************************************//
			SendMessage(hWnd, WM_3MBS_LOAD_SCENARIO, 0, 0);
			break;

		case IDC_EXTRACT_TEXT_FROM_BIN:
		case IDM_EXTRACT_TEXT_FROM_BIN:
			//--------------------------------------------------------------------------//
			// Extract Text File(s) From 3MB Binary Output (.3mb) File
			//--------------------------------------------------------//

			// Check for programming error when in debug mode.  The run-scenario thread
			// should not be running.
			_ASSERT(gdata->sceRunThreadInf.running == FALSE);

			// When not in debug mode check if run-scenario thread is running.  If it
			// is running kill it by launching the abort-process thread.
			if(gdata->extractThreadInf.running == TRUE)
			{			
				gdata->sceAbortThreadInf.hdl = 
					CreateThread(NULL, 0, &AbortProcessThreadProc, gdata, 0, &gdata->sceAbortThreadInf.id);
				break;
			}

			// Prompt the user for a .3mb file to open.  If the user selects cancel then
			// break.
			if(OK == (res = MyGetOpenFileName(&gdata->extractThreadInf.fileInf,
											  hWnd,
											  SZ3MBBINOUTFILTER,
											  SZ3MBBINOUTDEFEXT,
											  &bFileSelected)))
			{
				// No errors so check if user selected a file
				if(bFileSelected == FALSE)
					break; // file not selected so done here.
			}
			else if(FILE_PATH_TOO_LONG == res)
			{
				// Handle error.  Prepare the error message text.
				sprintf_s(szBuff1,
					sizeof(szBuff1)/sizeof(TCHAR),
					TEXT("Problem with selected 3MB binary output file %s:  %s"),
					gdata->extractThreadInf.fileInf.szTitleName,
					staticLib.MbsResultToString(res, szBuff2, sizeof(szBuff2)/sizeof(TCHAR)));

				// Prepare the caption text
				MessageBox(hWnd, szBuff1, TEXT("File Path Too Long"), 0);
				break;
			}
			else
			{
				sprintf_s(szBuff1,
					sizeof(szBuff1)/sizeof(TCHAR),
					TEXT("Problem encountered while selecting 3MB binary output file:  %s"),
					staticLib.MbsResultToString(res, szBuff2, sizeof(szBuff2)/sizeof(TCHAR)));
				MessageBox(hWnd, szBuff1, "Select 3MB Binary Output  File Error", 0);
				break;
			}
			//--------------------------------------------------------------------------//

			// Initialize the Examine Binary Output Struct
			memset(&e, 0, sizeof(EXMNBINOUTPROCSTRCT));
			//e.openFileName->lpstrFile = gdata->extractThreadInf.fileInf.szFileName;
			e.szFileName = gdata->extractThreadInf.fileInf.szFileName;
			e.fileExtractor = &gdata->fileExtractor;

			//--------------------------------------------------------------------------//
			// Show contents of the file.  Determine if the user selects a specific animat
			// to extract or all animats or hits cancel.  Break if user hits cancel.
			//----------------------------------------------------------------------//
			dlgRes = DialogBoxParam(hInst,
						MAKEINTRESOURCE(IDD_BINOUTPUT_EXAMINE),
						gdata->hwin,
						(DLGPROC)ExamineBinOutDlgProc,
						(LPARAM) &e);
			if(dlgRes == IDCANCEL)
				break;

			PostMessage(hWnd, WM_3MBS_EXTRACT_BIN_TO_TEXT, NULL, NULL);
			break;

		case ID_AUTOGENERATEFILENAME:
			//--------------------------------------------------------------------------//
			// Automatic Name Generation
			//--------------------------//
			up = gdata->sce.GetConfiguration();
			mallcInf = gdata->sce.GetMemoryAllocationDetails(&up);

			gdata->sce.GetBathymetryFileName(szBuff3, sizeof(szBuff3)/sizeof(TCHAR));
			staticLib.RemoveExtension(szBuff3);

			if(strlen(szBuff3) == 0)
				strcpy_s(szBuff3, sizeof(szBuff3)/sizeof(TCHAR), "NoBathymetry");

			// create a local scope region.
			{
				//----------------------------------//
				// Local scope for the ssz variable.
				//----------------------------------//
				TCHAR* ssz = TEXT("s%02da%05dd%06d[%s]Itn%02d");
				if(gdata->sce.GetConfiguration().output.outputByTime == FALSE)
					ssz = "s%02da%05dd%06d[%s]Ani%02d";

				sprintf_s(szBuff1,
						  sizeof(szBuff1)/sizeof(TCHAR),
						  ssz,
						  gdata->sce.GetSpeciesCount(),		// Species count
						  gdata->sce.GetAnimatCount(),		// Animat count
						  gdata->sce.GetDurationSeconds(),  // Duration
						  szBuff3,							// Bathymetry name
						  mallcInf.animatStateSize);		// Memory requirements per animat state
				
			}

			// Append the number of species

			// Concat each species' animat population count to the name.
			// Corrected 11/02/09 for not concatenating all species counts properly.
			for(i=0; i<gdata->sce.GetSpeciesCount(); i++)
			{
				_stprintf_s(szBuff2, sizeof(szBuff2)/sizeof(TCHAR), TEXT("(%d)"), gdata->sce.GetAnimatCount(i));
				_tcscat_s(szBuff1, sizeof(szBuff1)/sizeof(TCHAR), szBuff2);
			}

			// Concat "aa" if the acoustic source is active in this scenario.
			if(gdata->sce.GetConfiguration().acousticAnimatActive)
				_tcscat_s(szBuff1, sizeof(szBuff1)/sizeof(TCHAR), TEXT("aa"));

			_tcsncpy_s(fiSce.szTitleName, sizeof(szTitleName)/sizeof(TCHAR), szBuff1, _tcslen(szBuff1));
			staticLib.GetPathAndFileTitleFromFileName(fiSce.szFileName,
											szBuff1,					    // File path buffer
											sizeof(szBuff1)/sizeof(TCHAR),  // File path buffer length
											szBuff2,					    // File title buffer
											sizeof(szBuff2)/sizeof(TCHAR)); // File title buffer length
			_stprintf_s(fiSce.szFileName, sizeof(szFileName)/sizeof(TCHAR), TEXT("%s\\%s.sce"), szBuff1, fiSce.szTitleName);

			SetDlgItemText(hWnd, IDC_STATIC_SCENARIO_FILE,  TEXT(fiSce.szTitleName));
			gdata->sce.SetScenarioTitle(fiSce.szTitleName);
			fiSce.bNamed = FALSE; // force it to propmt the user when user wants to save the file.

			break;

		case ID_FILE_SAVE_SCENARIO_AS:
			if(OK == (res = MyGetSaveFileName(&fiSce, hWnd, SZSCEFILTER, SZSCEDEFEXT, &bFileSelected)))
			{
				// No errors so check if user selected a file
				if(bFileSelected == FALSE)
					break; // file not selected so done here.
			}
			else if(FILE_PATH_TOO_LONG == res)
			{
				// Handle error.  Prepare the error message text.
				sprintf_s(szBuff1,
					sizeof(szBuff1)/sizeof(TCHAR),
					TEXT("Problem with selected Scenario file %s:  %s"),
					fiCSV.szTitleName,
					staticLib.MbsResultToString(res, szBuff2, sizeof(szBuff2)/sizeof(TCHAR)));

				// Prepare the caption text
				MessageBox(hWnd, szBuff1, TEXT("File Path Too Long"), 0);
				break;
			}
			else
			{
				sprintf_s(szBuff1,
					sizeof(szBuff1)/sizeof(TCHAR),
					TEXT("Problem encountered while selecting scenario file:  %s"),
					staticLib.MbsResultToString(res, szBuff2, sizeof(szBuff2)/sizeof(TCHAR)));
				MessageBox(hWnd, szBuff1, "Select Scenario File Error", 0);
				break;
			}
			//--------------------------------------------------------------------------//

			SetDlgItemText(hWnd, IDC_STATIC_SCENARIO_FILE,  TEXT(fiSce.szTitleName));
			gdata->sce.SetScenarioTitle(fiSce.szTitleName);
			fiSce.bNamed = TRUE;
			// Fall through... (don't break;

		case ID_FILE_SAVE_SCENARIO:
		case IDC_SAVE_SCENARIO:
			if(fiSce.bNamed == FALSE)
			{
				PostMessage(hWnd, WM_COMMAND, ID_FILE_SAVE_SCENARIO_AS,	0);
				break;
			}
			
			if(OK != (res = gdata->sce.SaveToBinFile(fiSce.szFileName)))
			{
				sprintf_s(szBuff1,
						  sizeof(szBuff1)/sizeof(TCHAR),
						  TEXT("Unable To Save To File %s:  %s"),
						  fiSce.szTitleName,
						  staticLib.MbsResultToString(res, szBuff2, sizeof(szBuff2)/sizeof(TCHAR)));
				MessageBox(hWnd, szBuff1, TEXT("Save Scenario Error"), 0);
				break;
			}
			FileSaved(&fiSce, GetDlgItem(hWnd, IDC_STATIC_SCENARIO_FILE), SZSCEDEFEXT);
			break;

		case ID_EXIT:
		case WM_DESTROY: // the x in the upper right corner, not sure it is a WM_DESTROY.
			if(gdata->sce.IsActive())
			{
				gdata->sce.Exit();
				gdata->sceRunThreadInf.exit = TRUE;
			}
			DestroyWindow(hWnd);
			break;

		case ID_BUTTON_CONFIG_OUTPUT:
		case ID_FILE_CONFIGURE_OUTPUT:
		//case ID_FILE_CONFIGURESCENARIOOUTPUT:
			//config = gdata->sce.GetConfiguration();
			sceConfigParam.sce = &gdata->sce;
			sceConfigParam.texConfig = &gdata->textOutputConfig;
			sceConfigParam.outputFolderSet = &gdata->outputFolderSet;
			sceConfigParam.szOutputFolder = gdata->szOutputFolder;
			sceConfigParam.outputFolderBufferSize = sizeof(gdata->szOutputFolder)/sizeof(TCHAR);

			dlgResult = DialogBoxParam(hInst, MAKEINTRESOURCE (IDD_DIALOG_OUTPUT_CONFIG), hWnd, 
				(DLGPROC)ScenarioConfigProc, (LPARAM) &sceConfigParam);
			if(dlgResult == IDOK)
				FileChanged(&fiSce);
			UpdateScenarioGUI(gdata);
			break;

		case ID_FILE_BATHYMETRY:
		case IDC_BUTTON_BATHYMETRY:
			//--------------------------------------------------------------------------//
			// Prompt the user for a file to open
			//-----------------------------------//
			if(OK == (res = MyGetOpenFileName(&fiBath, hWnd, SZ_BATHY_FILTER, SZ_BATHY_DEFEXT, &bFileSelected)))
			{
				// No errors so check if user selected a file
				if(bFileSelected == FALSE)
					break; // file not selected so done here.
			}
			else if(FILE_PATH_TOO_LONG == res)
			{
				// Handle error.  Prepare the error message text.
				sprintf_s(szBuff1,
					sizeof(szBuff1)/sizeof(TCHAR),
					TEXT("Problem with selected bathymetry file %s:  %s"),
					fiBath.szTitleName,
					staticLib.MbsResultToString(res, szBuff2, sizeof(szBuff2)/sizeof(TCHAR)));

				// Prepare the caption text
				MessageBox(hWnd, szBuff1, TEXT("File Path Too Long"), 0);
				break;
			}
			else
			{
				sprintf_s(szBuff1,
					sizeof(szBuff1)/sizeof(TCHAR),
					TEXT("Problem encountered while selecting bathymetry file:  %s"),
					staticLib.MbsResultToString(res, szBuff2, sizeof(szBuff2)/sizeof(TCHAR)));
				MessageBox(hWnd, szBuff1, "Select Bathymetry File Error", 0);
				break;
			}
			//--------------------------------------------------------------------------//

			if(OK != (res = gdata->sce.LoadBathymetryFromTextFile(fiBath.szFileName)))
			{
				sprintf_s(szBuff1,
						  sizeof(szBuff1)/sizeof(TCHAR),
						  "Unable To Load File %s:  %s",
						  fiBath.szTitleName,
						  staticLib.MbsResultToString(res, szBuff2, sizeof(szBuff2)/sizeof(TCHAR)));
				MessageBox(hWnd, szBuff1, "Save Scenario Error", 0);
				break;
			}
			FileChanged(&fiSce);
			UpdateScenarioGUI(gdata);
			break;

		case IDC_BUTTON_BATHYMETRY_CLEAR:
			gdata->sce.ClearBathymetry();
			FileChanged(&fiSce);
			fiBath.szTitleName[0] = fiBath.szFileName[0] = NULL;
			UpdateScenarioGUI(gdata);
			break;


		case IDC_BUTTON_BATHYMETRY_TO_TXT_FILE:
			break;
//			if(!GetSaveFileName(&fiTxt.ofn))
//				break;

//			gdata->sce.BathymetryToTextFile(fiTxt.ofn.lpstrFile);
//			sprintf_s(szBuff1, sizeof(szBuff1), "Bathymetry Data saved to file %s", fiTxt.ofn.lpstrFile);
//			MessageBox(hWnd, szBuff1, "File Saved", MB_OK);
//			break;

		case ID_SPECIES_OPEN:
			dlgResult = DialogBoxParam(hInst, MAKEINTRESOURCE (IDD_SPECIES_BUILDER), hWnd,
							  (DLGPROC) SpeciesModelProc, (LPARAM) OPEN_EXISTING_FILE);
			break;

		case ID_SPECIES_NEW:
			dlgResult = DialogBoxParam(hInst, MAKEINTRESOURCE (IDD_SPECIES_BUILDER), hWnd,
							(DLGPROC) SpeciesModelProc, (LPARAM) MAKE_NEW);
			break;

		case IDC_BUTTON_TIME:
			hr_min_sec = gdata->sce.GetStartTime();
			dlgResult = DialogBoxParam(hInst, MAKEINTRESOURCE (IDD_TIMEOFDAY), hWnd,
							   (DLGPROC) SceTimeDlgProc, (LPARAM) &hr_min_sec);
			if(dlgResult == IDOK)
			{
				gdata->sce.SetStartTime(hr_min_sec);
				FileChanged(&fiSce);
				UpdateScenarioGUI(gdata);
			}
			break;

		case IDC_TESTBUTTON:
			// TESTING AND DEBUGGING
			break;


		case IDC_VIEWBATHYMETRY:
			memset(&bbmDlgParam, 0, sizeof(SEED_DLG_PARAM));
			bbmDlgParam.usageType = SEED_SCENARIO_TYPE;
			bbmDlgParam.hInstance = hInst;
			bbmDlgParam.hWndParent = hWnd;
			bbmDlgParam.pScenario = &gdata->sce;

			dlgResult = DialogBoxParam(hInst, MAKEINTRESOURCE(IDD_DIALOG_VIEWBATHY), hWnd,
							   (DLGPROC) BathyBitmapDlgProc, (LPARAM) &bbmDlgParam);
			UpdateGuiSpeciesListWindow(&gdata->sce, GetDlgItem(hWnd, IDC_LIST_SPECIES));
			UpdateScenarioGUI(gdata);

			break;

		case IDC_BUTTON_DURATION:
			hr_min_sec = gdata->sce.GetDuration();
			dlgResult =	DialogBoxParam(hInst, MAKEINTRESOURCE (IDD_DURATION), hWnd,
							   (DLGPROC)SceTimeDlgProc, (LPARAM)&hr_min_sec);
			if(dlgResult == IDOK)
			{
				dwrd = hr_min_sec.hour * 60;
				dwrd = (dwrd + hr_min_sec.min) * 60;
				dwrd = (dwrd + hr_min_sec.sec);

				if(dwrd > 7 * 24 * 60 * 60)
				{
					MessageBox(gdata->hwin, "Duration Reset To 7 Days", "Duration Exceeded 7 Days", 0);
					memset(&hr_min_sec, 0, sizeof(HHMMSS));
					hr_min_sec.hour = 7 * 24;
				}
				gdata->sce.SetDuration(hr_min_sec);
				FileChanged(&fiSce);
				UpdateScenarioGUI(gdata);
			}
			break;
		//-------------------------------------------------------------------------------------------------//

		//-------------------   Species Models   ----------------------------------------------------------//
		case IDC_LIST_SPECIES:
			switch(wmEvent)
			{
			case LBS_NOTIFY:
				UpdateScenarioGUI(gdata);
				break;

			case LBN_DBLCLK:
				break; // Needs improvement.
			}
			break;

		//-------------------   Environmental Models   ----------------------------------------------------//
		case IDC_LIST_ENVDAT:
			switch(wmEvent)
			{
			case LBS_NOTIFY:
				break;

			case LBN_DBLCLK:
				hwnd = GetDlgItem(hWnd, IDC_LIST_ENVDAT);
				i = (int)SendMessage(hwnd, LB_GETCURSEL, 0,0);

				// Determine the file type.
				if(i == BATHYMETRY_INDEX)
				{
					strcpy_s(szBuff1, sizeof(szBuff1), SZ_BATHYMETRY);  // double clicking
					if(gdata->sce.BathymetryLoaded() == TRUE)
					{
						// User wants to clear the loaded bathymetry data (verify though message box).
						if(IDYES == MessageBox(hWnd, "Clear bathymetry data?", "Bathymetry Data", MB_YESNO|MB_ICONQUESTION))
							gdata->sce.ClearBathymetry();
						else
							break;
					}
					else
					{
						//--------------------------------------------------------------------------//
						// Prompt the user for a file to open
						//-----------------------------------//
						if(OK == (res = MyGetOpenFileName(&fiBath, hWnd, SZ_BATHY_FILTER, SZ_BATHY_DEFEXT, &bFileSelected)))
						{
							// No errors so check if user selected a file
							if(bFileSelected == FALSE)
								break; // file not selected so done here.

							if(OK != (res = gdata->sce.LoadBathymetryFromTextFile(fiBath.szTitleName)))
							{
								sprintf_s(szBuff1,
										  sizeof(szBuff1)/sizeof(TCHAR),
										  "Unable To Load File %s:  %s",
										  fiBath.szTitleName,
										  staticLib.MbsResultToString(res, szBuff2, sizeof(szBuff2)/sizeof(TCHAR)));
								MessageBox(hWnd, szBuff1, "Load Bathymetry Error", 0);
								break;
							}
						}
						else if(FILE_PATH_TOO_LONG == res)
						{
							// Handle error.  Prepare the error message text.
							sprintf_s(szBuff1,
								sizeof(szBuff1)/sizeof(TCHAR),
								TEXT("Problem with selected bathymetry file %s:  %s"),
								fiBath.szTitleName,
								staticLib.MbsResultToString(res, szBuff2, sizeof(szBuff2)/sizeof(TCHAR)));

							// Prepare the caption text
							MessageBox(hWnd, szBuff1, TEXT("File Path Too Long"), 0);
							break;
						}
						else
						{
							sprintf_s(szBuff1,
								sizeof(szBuff1)/sizeof(TCHAR),
								TEXT("Problem encountered while selecting bathymetry file:  %s"),
								staticLib.MbsResultToString(res, szBuff2, sizeof(szBuff2)/sizeof(TCHAR)));
							MessageBox(hWnd, szBuff1, "Select Bathymetry File Error", 0);
							break;
						}
						//--------------------------------------------------------------------------//
					}
				}
#if 0
				else if(i == SALINITY_INDEX)
				{
					if(gdata->sce.SalinityLoaded() == TRUE)
					{
						// User wants to clear the loaded salt data (verify though message box).
						if(IDYES == MessageBox(hWnd, "Clear salinity data?", "Salinity Data", MB_YESNO|MB_ICONQUESTION))
							gdata->sce.ClearSalinity();
						else
							break;
					}
					else if(GetOpenFileName(&fiSalt.ofn) != NULL) // Load the salt data
					{
						if(OK != (res = gdata->sce.LoadSalinityFromTextFile(&fiSalt.ofn)))
						{
							sprintf_s(szBuff1, sizeof(szBuff1), "Unable To Load File %s:  ", fiSalt.szTitleName);
							staticLib.MbsResultToString(res, &szBuff1[strlen(szBuff1)], SIZE_128-strlen(szBuff1));
							MessageBox(hWnd, szBuff1, "Load Environmental Data Error", 0);
							break;
						}
					}
					else
						break;
				}
				else if(i == TEMPERATURE_INDEX)
				{
					if(gdata->sce.TemperatureLoaded() == TRUE)
					{
						// User wants to clear the loaded termperature data (verify though message box).
						if(IDYES == MessageBox(hWnd, "Clear temperature data?", "Temperature Data", MB_YESNO|MB_ICONQUESTION))
							gdata->sce.ClearTemperature();
						else
							break;
					}
					else if(GetOpenFileName(&fiTemperature.ofn) != NULL) // Load the temperature data
					{
						if(OK != (res = gdata->sce.LoadTemperatureFromTextFile(&fiTemperature.ofn)))
						{
							sprintf_s(szBuff1, sizeof(szBuff1), "Unable To Load File %s:  ", fiTemperature.szTitleName);
							staticLib.MbsResultToString(res, &szBuff1[strlen(szBuff1)], SIZE_128-strlen(szBuff1));
							MessageBox(hWnd, szBuff1, "Load Environmental Data Error", 0);
							break;
						}
					}
					else
						break;
				}
#endif
				UpdateScenarioGUI(gdata);
					break;
			}
			break;

#if 0 // not being used, pending feedback.
		case IDC_BUTTON_ENVDAT_ADD:
			if(GetOpenFileName(&fiEnv.ofn) != NULL)
			{
				// Determine the file type (bathymetry, salinity, or temperature).
				if(strcmp(&fiEnv.szFileName[fiEnv.ofn.nFileExtension], "bth") == 0)
				{	// Bathymetry Data
					// If already loaded, break.
					if(gdata->sce.BathymetryLoaded() == TRUE)
					{
						MessageBox(hWnd, "Delete already loaded bathymetry data before loading new", "Bathymetry Data Already Loaded", 0);
						break;
					}

					// Load the bathymetry data
					envDataResult = gdata->sce.LoadBathymetryFromTextFile(&fiEnv.ofn);
					if(envDataResult.code != ENVDATA_OK)
					{
						MessageBox(hWnd, "Error while loading bathymetry data", "Bad Bathymetry File", 0);
						break;
					}


				}
				else if(strcmp(&fiEnv.szFileName[fiEnv.ofn.nFileExtension], "slt") == 0)
				{	// Salinity Data
					// If already loaded, break.
					if(gdata->sce.SalinityLoaded() == TRUE)
					{
						MessageBox(hWnd, "Delete already loaded salinity data before loading new", "Salinity Data Already Loaded", 0);
						break;
					}
					// Load the salinity data
					gdata->sce.LoadSalinityFromTextFile(&fiEnv.ofn);
				}
				else if(strcmp(&fiEnv.szFileName[fiEnv.ofn.nFileExtension], "tem") == 0)
				{	// Temperature Data
					// If already loaded, break.
					if(gdata->sce.TemperatureLoaded() == TRUE)
					{
						MessageBox(hWnd, "Delete already loaded temperature data before loading new", "Temperature Data Already Loaded", 0);
						break;
					}
					// Load the temperature data
					gdata->sce.LoadTemperatureFromTextFile(&fiEnv.ofn);
				}
				else
					break;

				FileChanged(&fiSce);
				UpdateScenarioGUI(gdata);
			}
			break;
#endif

		case IDC_BUTTON_ENVDAT_REMOVE:
			hwnd = GetDlgItem(hWnd, IDC_LIST_ENVDAT);
			i = (int)SendMessage(hwnd, LB_GETCURSEL, 0,0);

			// Determine the file type.
			if(i == BATHYMETRY_INDEX)
				gdata->sce.ClearBathymetry();
			else if(i == SALINITY_INDEX)
				gdata->sce.ClearSalinity();
			else if(i == TEMPERATURE_INDEX)
				gdata->sce.ClearTemperature();
			UpdateScenarioGUI(gdata);
			FileChanged(&fiSce);
			break;



		case ID_FILE_RUN_SIMULATION:
		case ID_RUN_SIMULATION:
			// Immediately disable the run simulation and file extract buttons
			_ASSERT(gdata->extractThreadInf.running == FALSE);
			if(gdata->sceRunThreadInf.running == TRUE)
			{
				// Launch the proces that waits for the scenario execution and data
				// extractions processes to cease.
				gdata->sceAbortThreadInf.hdl = 
					CreateThread(NULL, 0, &AbortProcessThreadProc, gdata, 0, &gdata->sceAbortThreadInf.id);
				break;
			}


			EnableWindow(GetDlgItem(gdata->hwin, ID_RUN_SIMULATION), FALSE);
			EnableWindow(GetDlgItem(gdata->hwin, IDC_EXTRACT_TEXT_FROM_BIN), FALSE);

			// Clear the message list box
			SendMessage(GetDlgItem(hWnd, IDC_LIST_MESSAGES), LB_RESETCONTENT , 0, 0);
			SendMessage(gdata->hwin, WM_UPDATE_PROGRESS_BAR, 0, 0);

			// Launch the thread that runs the simulation/data extraction.
			gdata->sceRunThreadInf.hdl = 
				CreateThread(NULL, 0, &ScenarioThread, gdata, 0, &gdata->sceRunThreadInf.id);
			break;
		}
		break;  // end of WM_COMMAND
	case WM_DESTROY:
		//while(gdata->sceRunThreadInf.running == TRUE)
		//	Sleep(1);
		PostQuitMessage(0);
		break;
   }
   return 0;
}


// Left in here for an example.
// button text colors.  Not used, but keep for example for future work.
#define TEXT_COLOR_NOT_CONN		RGB(255, 255, 255)
#define TEXT_COLOR_CONN_FAIL	RGB(  0,   0,   0)
#define TEXT_COLOR_CONNECTED	RGB(  0,   0,   0)
#define TEXT_COLOR_PROG_ERRR	RGB(  6,  50,  90)

#define WND_COLOR_NOT_CONN		RGB(  0,   0,   0) 
#define WND_COLOR_CONNECTING	RGB(255, 255,   0)
#define WND_COLOR_CONN_FAIL		RGB(254,   0,   0) 
#define WND_COLOR_CONNECTED		RGB(  0, 254,   0) 

#define WND_COLOR_ATTENTION1	RGB(255, 220,   0) 
#define WND_COLOR_ATTENTION2	RGB(200,   0,   0) 

#define WND_COLOR_PROG_ERROR	RGB(200, 120,  45) 

BOOL RedrawStaticWindows(GLOBALDATA *gdata, int ControlID, HDC DC)
{
	switch(ControlID)
	{
	case 1 /*IDC_STATIC_MATLAB_DETECTED_WINDOW*/:
		if(gdata->bMatlabDetected == TRUE)
		{
			SetBkColor(DC, (COLORREF)WND_COLOR_CONNECTED);
			SetTextColor(DC, (COLORREF)TEXT_COLOR_CONNECTED);
		}
		else
		{
			SetBkColor(DC, (COLORREF)WND_COLOR_NOT_CONN);
			SetTextColor(DC, (COLORREF)TEXT_COLOR_NOT_CONN);
		}
		return TRUE;
	}
	return FALSE;
}

void UpdateScenarioGUI(GLOBALDATA *gdata)
{
	HHMMSS hrMinSec;

	MENUITEMINFO menuItemAcstcPing, menuItemCSV, menuItemInterval, menuItemLimitOutput;
	BOOL bVal1, bVal2, bVal3; // general Boolean value usable wherever needed.
	HMENU hmenu;

	HWND hwnd;
	TCHAR szBuff[BUFFERED_MAX_PATH];
	TCHAR szBuff2[BUFFERED_MAX_PATH];
	BOOL simThrdRunning  = gdata->sce.IsActive();
	BOOL extractionThrdRunning = FALSE;
	DWORDLONG dwrdBinStorage, dwrdTxtStorage;
	DWORDLONG binStorage;
	double txtStorage;
	//TCHAR *unit[] = {"KB", "MB", "GB"};
	//int i;
	//double fVal;
	//double divideBy;
	int cursorPos;
	//USERPARAMS userParams = gdata->sce.GetCurrentScenarioParams();
	//SCESTATE state = gdata->sce.GetState();
	FESTATE feState = gdata->fileExtractor.GetState();

	//config = gdata->sce.GetConfiguration();
	//STATE_FILEOUTPUT_CONFIG stateConfig = CScenario::TranslateBinFileOutConfiguration(config.binOutStateItemConfig);
	//BINARYSETUP binDisInf = gdata->sce.FileMgmt_DetermineDiskUsage(config.binOutStateItemConfig);

	if(feState.activity == EXTRACTOR_EXTRACTING)
		extractionThrdRunning = TRUE;
	else
		extractionThrdRunning = FALSE;


	//---------------------------------------------------------------------------------//
	// Menu items
	//-----------//
	hmenu = GetMenu(gdata->hwin); // The menu bar.

	// Set up the data structure to get the state of the menu item check.
	menuItemLimitOutput.cbSize = menuItemAcstcPing.cbSize = menuItemCSV.cbSize = menuItemInterval.cbSize = sizeof(MENUITEMINFO);
	menuItemLimitOutput.fMask = menuItemAcstcPing.fMask = menuItemCSV.fMask = menuItemInterval.fMask = MIIM_STATE;

	// Get the current state of the 'limit output to acoustic output pings' and 
	// the current state of 'loaded CSV value'.
	bVal1 = gdata->sce.GetAcousticSrceLimitOutput();
	bVal2 = gdata->sce.CSVFileLoaded();
	bVal3 = gdata->sce.IntervalLimitOutputEnabled();

	// Get the current state of the menu items.
	GetMenuItemInfo(GetSubMenu(hmenu, 0), 9, TRUE, &menuItemLimitOutput);
	GetMenuItemInfo(hmenu, ID_LIMITOUTPUT_PINGCYCLES, FALSE, &menuItemAcstcPing);
	GetMenuItemInfo(GetSubMenu(GetSubMenu(hmenu, 0), 9), 1, TRUE, &menuItemCSV);

	// Set next state.
	if(bVal1 == TRUE || bVal2 == TRUE || bVal3 == TRUE)
		menuItemLimitOutput.fState = MFS_CHECKED;
	else
		menuItemLimitOutput.fState = MFS_UNCHECKED;

	if(bVal1 == TRUE)
		menuItemAcstcPing.fState = MFS_CHECKED;
	else
		menuItemAcstcPing.fState = MFS_UNCHECKED;

	if(bVal2 == TRUE)
		menuItemCSV.fState = MFS_CHECKED;
	else
		menuItemCSV.fState = MFS_UNCHECKED;

	if(bVal3 == TRUE)
		menuItemInterval.fState = MFS_CHECKED;
	else
		menuItemInterval.fState = MFS_UNCHECKED;



	SetMenuItemInfo(GetSubMenu(hmenu, 0), 9, TRUE, &menuItemLimitOutput);
	SetMenuItemInfo(hmenu, ID_LIMITOUTPUT_PINGCYCLES, FALSE, &menuItemAcstcPing);
	SetMenuItemInfo(GetSubMenu(GetSubMenu(hmenu, 0), 9), 1, TRUE, &menuItemCSV);
	SetMenuItemInfo(hmenu, ID_LIMITOUTPUTBY_FIXEDINTERVAL, FALSE, &menuItemInterval);
	//---------------------------------------------------------------------------------//


	//---------------------------------------------------------------------------------//
	// Environmental Data List Box
	//----------------------------//
	// Get a handle to the environmental data list box
	hwnd = GetDlgItem(gdata->hwin, IDC_LIST_ENVDAT);


	// Display Bathymetry data file name, if any.
	strcpy_s(szBuff, sizeof(szBuff), SZ_BATHYMETRY);  // load scenario
	if(gdata->sce.BathymetryLoaded() == TRUE)
	{
		strcat_s(szBuff, sizeof(szBuff), ": ");
		gdata->sce.GetBathymetryFileName(szBuff2, sizeof(szBuff2));
		strcat_s(szBuff, sizeof(szBuff), szBuff2);
	}
	else
	{
		strcat_s(szBuff, sizeof(szBuff), " (not loaded)");
	}
	SendMessage(hwnd, LB_DELETESTRING, BATHYMETRY_INDEX, 0);
	SendMessage(hwnd, LB_INSERTSTRING, BATHYMETRY_INDEX, (LPARAM)szBuff);

	SetDlgItemInt(gdata->hwin, IDC_STATIC_NUM_ITERATIONS, gdata->sce.GetDurationSeconds()+1, FALSE);
	SetDlgItemInt(gdata->hwin, IDC_STATIC_TOT_ANIMALS, gdata->sce.GetAnimatCount(), FALSE);

	if(gdata->sce.GetAnimatCount() < 1)
	{
		txtStorage = 0;
		binStorage = 0;
		SetDlgItemInt(gdata->hwin, IDC_STATIC_NUM_SAVEDSTATES, 0, FALSE);
	}
	else
	{
		SetDlgItemInt(gdata->hwin, IDC_STATIC_NUM_SAVEDSTATES, gdata->sce.GetSaveStatesCount(), FALSE);
		gdata->sce.CalculateRequiredDiskSpace(&dwrdBinStorage, &dwrdTxtStorage);
		binStorage = dwrdBinStorage;
		txtStorage = (double)dwrdTxtStorage;
	}
	//-----------------------------------------------------------------------------------------------------//

	//-----------------------------------------------------------------------------------------------------//
	// Binary Storage
	//sprintf_s(szBuff, sizeof(szBuff), "%.2f", fVal);
	staticLib.MemoryValueToString(binStorage, szBuff, sizeof(szBuff));
	SetDlgItemText(gdata->hwin, IDC_STATIC_BINOUT_SPAC, szBuff);
	//SetDlgItemText(gdata->hwin, IDC_STATIC_BINUNIT, unit[i]);
	SetDlgItemText(gdata->hwin, IDC_STATIC_BINUNIT, "");

	staticLib.MemoryValueToString((DWORDLONG)txtStorage, szBuff, sizeof(szBuff));
	SetDlgItemText(gdata->hwin, IDC_STATIC_TXTOUT_SPAC, szBuff);

	if(gdata->textOutputConfig.enabled == FALSE)
		SetDlgItemText(gdata->hwin, IDC_STATIC_TXTOUT_SPAC, "(not set) 0 bytes");
	//-----------------------------------------------------------------------------------------------------//


	//----------------------  Menu bar items  -------------------------------------------------------------//
	if(simThrdRunning == TRUE || extractionThrdRunning == TRUE)
	{
		hmenu = GetSubMenu(GetMenu(gdata->hwin), 0);

		// The Menu Bar: File, New Scenario
		EnableMenuItem(hmenu, ID_FILE_NEW_SCENARIO, MF_GRAYED);
		// The Menu Bar: File,  Load Scenario
		EnableMenuItem(hmenu, ID_FILE_LOAD_SCENARIO, MF_GRAYED);
		// The Menu Bar: File, Save Scenario
		EnableMenuItem(hmenu, ID_FILE_SAVE_SCENARIO, MF_GRAYED);
		// The Menu Bar: File, Save Scenario as...
		EnableMenuItem(hmenu, ID_FILE_SAVE_SCENARIO_AS, MF_GRAYED);
		// The Menu Bar: File, Configure...
		EnableMenuItem(hmenu, ID_FILE_CONFIGURE_OUTPUT, MF_GRAYED);
		// The Menu Bar: File, Import Bathymetry
		EnableMenuItem(hmenu, ID_FILE_BATHYMETRY, MF_GRAYED);


		// Species Menu
		hmenu = GetSubMenu(GetMenu(gdata->hwin), 1);
		// The Species Bar: File, New Scenario
		EnableMenuItem(hmenu, ID_SPECIES_NEW, MF_GRAYED);
		// The Species Bar: File,  Load Scenario
		EnableMenuItem(hmenu, ID_SPECIES_OPEN, MF_GRAYED);


		// The Species List Of Files
		hwnd = GetDlgItem(gdata->hwin, IDC_STATIC_SPECIES_MSG);
		EnableWindow(hwnd, FALSE);

		hwnd = GetDlgItem(gdata->hwin, IDC_LIST_SPECIES);
		EnableWindow(hwnd, FALSE);
	}
	else
	{

		hmenu = GetSubMenu(GetMenu(gdata->hwin), 0);

		// The Menu Bar: File, New Scenario
		EnableMenuItem(hmenu, ID_FILE_NEW_SCENARIO, MF_ENABLED);
		// The Menu Bar: File,  Load Scenario
		EnableMenuItem(hmenu, ID_FILE_LOAD_SCENARIO, MF_ENABLED);
		// The Menu Bar: File, Save Scenario
		EnableMenuItem(hmenu, ID_FILE_SAVE_SCENARIO, MF_ENABLED);
		// The Menu Bar: File, Save Scenario as...
		EnableMenuItem(hmenu, ID_FILE_SAVE_SCENARIO_AS, MF_ENABLED);
		// The Menu Bar: File, Configure...
		EnableMenuItem(hmenu, ID_FILE_CONFIGURE_OUTPUT, MF_ENABLED);

		// The Menu Bar: File, Import Bathymetry
		EnableMenuItem(hmenu, ID_FILE_BATHYMETRY, MF_ENABLED);


		// Species Menu
		hmenu = GetSubMenu(GetMenu(gdata->hwin), 1);
		// The Species Bar: File, New Scenario
		EnableMenuItem(hmenu, ID_SPECIES_NEW, MF_ENABLED);
		// The Species Bar: File,  Load Scenario
		EnableMenuItem(hmenu, ID_SPECIES_OPEN, MF_ENABLED);

		// The Species List Of Files
		hwnd = GetDlgItem(gdata->hwin, IDC_STATIC_SPECIES_MSG);
		EnableWindow(hwnd, TRUE);

		hwnd = GetDlgItem(gdata->hwin, IDC_LIST_SPECIES);
		EnableWindow(hwnd, TRUE);
	}
	//-----------------------------------------------------------------------------------------------------//

	//---------------------------   Configuration, Load Scenario, and Save Scenario   ---------------------//
	if(simThrdRunning == TRUE || extractionThrdRunning == TRUE)
	{
		EnableWindow(GetDlgItem(gdata->hwin, ID_BUTTON_CONFIG_OUTPUT), FALSE);
		EnableWindow(GetDlgItem(gdata->hwin, IDC_SAVE_SCENARIO), FALSE);
		EnableWindow(GetDlgItem(gdata->hwin, IDC_LOAD_SCENARIO), FALSE);
		EnableWindow(GetDlgItem(gdata->hwin, IDC_LOAD_BINFILE), FALSE);
	}
	else
	{
		EnableWindow(GetDlgItem(gdata->hwin, ID_BUTTON_CONFIG_OUTPUT), TRUE);
		EnableWindow(GetDlgItem(gdata->hwin, IDC_SAVE_SCENARIO), TRUE);
		EnableWindow(GetDlgItem(gdata->hwin, IDC_LOAD_SCENARIO), TRUE);
		EnableWindow(GetDlgItem(gdata->hwin, IDC_LOAD_BINFILE), TRUE);
	}
	//-----------------------------------------------------------------------------------------------------//


	//--------------------------   Time of day   ----------------------------------------------------------//
	hwnd = GetDlgItem(gdata->hwin, IDC_BUTTON_TIME);
	if(simThrdRunning == TRUE || extractionThrdRunning == TRUE)
		EnableWindow(hwnd, FALSE);
	else
		EnableWindow(hwnd, TRUE);

	hwnd = GetDlgItem(gdata->hwin, IDC_STATIC_TIMEOFDAY);
	hrMinSec = gdata->sce.GetStartTime();
	sprintf_s(szBuff, sizeof(szBuff), TEXT("%02d:%02d:%02d"), hrMinSec.hour, hrMinSec.min, hrMinSec.sec);
	SetDlgItemText(gdata->hwin, IDC_STATIC_TIMEOFDAY,  szBuff);
	//-----------------------------------------------------------------------------------------------------//

	//--------------------------   Duration   -------------------------------------------------------------//
	hwnd = GetDlgItem(gdata->hwin, IDC_BUTTON_DURATION);
	if(simThrdRunning == TRUE || extractionThrdRunning == TRUE)
		EnableWindow(hwnd, FALSE);
	else
		EnableWindow(hwnd, TRUE);

	hwnd = GetDlgItem(gdata->hwin, IDC_DURATION);
	hrMinSec = gdata->sce.GetDuration();
	sprintf_s(szBuff, sizeof(szBuff), TEXT("%d:%02d:%02d"), hrMinSec.hour, hrMinSec.min, hrMinSec.sec);
	SetDlgItemText(gdata->hwin, IDC_DURATION,  szBuff);
	//-----------------------------------------------------------------------------------------------------//

	//--------------------------   Bathymetry   -----------------------------------------------------------//
	hwnd = GetDlgItem(gdata->hwin, IDC_BATHYMETRY_FILE);

	// The clear bathymetry button.
	if(gdata->sce.BathymetryLoaded() && simThrdRunning == FALSE && extractionThrdRunning == FALSE)
		EnableWindow(GetDlgItem(gdata->hwin, IDC_BUTTON_BATHYMETRY_CLEAR), TRUE);
	else
		EnableWindow(GetDlgItem(gdata->hwin, IDC_BUTTON_BATHYMETRY_CLEAR), FALSE);


	// The load bathymetry button.
	if(gdata->sce.BathymetryLoaded()|| simThrdRunning == TRUE || extractionThrdRunning == TRUE)
		EnableWindow(GetDlgItem(gdata->hwin, IDC_BUTTON_BATHYMETRY), FALSE);
	else
		EnableWindow(GetDlgItem(gdata->hwin, IDC_BUTTON_BATHYMETRY), TRUE);


	// The Bathymetry to text button and name of the file (if any).
	if(gdata->sce.BathymetryLoaded())
	{
		EnableWindow(GetDlgItem(gdata->hwin, IDC_BUTTON_BATHYMETRY_TO_TXT_FILE), TRUE);
		gdata->sce.GetBathymetryFileName(szBuff2, BUFFERED_MAX_PATH);
		sprintf_s(szBuff, sizeof(szBuff), "Bathymetry Data:  %s", szBuff2);
		SetDlgItemText(gdata->hwin, IDC_BATHYMETRY_FILE, szBuff);
	}
	else
	{
		EnableWindow(GetDlgItem(gdata->hwin, IDC_BUTTON_BATHYMETRY_TO_TXT_FILE), FALSE);
		SetDlgItemText(gdata->hwin, IDC_BATHYMETRY_FILE,  TEXT("Bathymetry Data:"));
	}
	//-----------------------------------------------------------------------------------------------------//


	//--------------------------   Species Models, Add and Remove  ----------------------------------------//
	cursorPos = (int)SendMessage(GetDlgItem(gdata->hwin, IDC_LIST_SPECIES), LB_GETCURSEL, 0,0);

	hwnd = GetDlgItem(gdata->hwin, IDC_BUTTON_SPEC_ADD);
	hmenu = GetSubMenu(GetMenu(gdata->hwin), 0);

	if(simThrdRunning || extractionThrdRunning)
	{
		EnableWindow(hwnd, FALSE);
		EnableMenuItem(hmenu, ID_FILE_ADD_SPECIES, MF_GRAYED);
	}
	else
	{
		EnableWindow(hwnd, TRUE);
		EnableMenuItem(hmenu, ID_FILE_ADD_SPECIES, MF_ENABLED);
	}

	// Remove and Populate buttons and menu items -- disable if simulation
	// thread running, no models loaded or no models highlighted.
	hwnd = GetDlgItem(gdata->hwin, IDC_BUTTON_SPEC_REMOVE);

	if(simThrdRunning == TRUE || extractionThrdRunning == TRUE || gdata->sce.GetSpeciesCount()==0 || cursorPos < 0)
	{
		EnableWindow(hwnd, FALSE);
		EnableMenuItem(hmenu, ID_FILE_DELETE_SPECIES, MF_GRAYED);
	}
	else
	{
		EnableWindow(hwnd, TRUE);
		EnableMenuItem(hmenu, ID_FILE_DELETE_SPECIES, MF_ENABLED);
	}
	//-----------------------------------------------------------------------------------------------------//

	//--------------------------   Environmental Data, Add and Remove  ------------------------------------//
	cursorPos = (int)SendMessage(GetDlgItem(gdata->hwin, IDC_LIST_ENVDAT), LB_GETCURSEL, 0,0);
	hwnd = GetDlgItem(gdata->hwin, IDC_BUTTON_ENVDAT_ADD);
	hmenu = GetSubMenu(GetMenu(gdata->hwin), 0);

	if(simThrdRunning || extractionThrdRunning)
	{
		EnableWindow(hwnd, FALSE);
		EnableMenuItem(hmenu, IDC_BUTTON_ENVDAT_ADD, MF_GRAYED);
	}
	else
	{
		EnableWindow(hwnd, TRUE);
		EnableMenuItem(hmenu, IDC_BUTTON_ENVDAT_ADD, MF_ENABLED);
	}

	// Remove and Populate buttons and menu items -- disable if simulation
	// thread running, no models loaded or no models highlighted.
	hwnd = GetDlgItem(gdata->hwin, IDC_BUTTON_ENVDAT_REMOVE);

	if(simThrdRunning == TRUE || extractionThrdRunning == TRUE || /*modelsLoaded == FALSE || */cursorPos < 0)
		EnableWindow(hwnd, FALSE);
	else
		EnableWindow(hwnd, TRUE);
	//-----------------------------------------------------------------------------------------------------//



	//-----------------------   Run Simulation Button   ---------------------------------------------------//
	// If there isn't at least one cluster or if the extraction thread is running, disable the run button.
	hwnd = GetDlgItem(gdata->hwin, ID_RUN_SIMULATION);
	if(gdata->sce.GetAnimatCount() < 1 || extractionThrdRunning == TRUE)
		EnableWindow(hwnd, FALSE);
	else
		EnableWindow(hwnd, TRUE);

	if(gdata->sce.IsActive() == TRUE)
		SetWindowText(hwnd, "   Abort Simulation");
	else
		SetWindowText(hwnd, "     Run Simulation");
	//-----------------------------------------------------------------------------------------------------//

	//-----------------------   Extract Data Button   -----------------------------------------------------//
	hwnd = GetDlgItem(gdata->hwin, IDC_EXTRACT_TEXT_FROM_BIN);

	if(simThrdRunning == TRUE)
		EnableWindow(hwnd, FALSE);
	else
		EnableWindow(hwnd, TRUE);

	if(extractionThrdRunning == TRUE)
		SetWindowText(hwnd, "    Abort Extraction");
	else
		SetWindowText(hwnd, "Extract Data");
	//-----------------------------------------------------------------------------------------------------//
}

const TCHAR *EXAMINEOUTPUTSTRING = "\
File: %s\n\n\
  File Format 3mbs version %d.%02d\n\
  Total Saved States: %d\n\
  Total Animats: %d\n\
  -----------------------\n\
  Scenario Times:\n\
    Start: %02d:%02d:%02d:%02d\n\
    End:   %02d:%02d:%02d:%02d\n\
    Duration: %d seconds\n\
  Output Format: by %s";





/*----------------------------------------------------------------------------------------
	The dialog procedure for editing the duration of the simulation
/*--------------------------------------------------------------------------------------*/
LRESULT CALLBACK SceTimeDlgProc(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
	static HHMMSS *t;
	static TCHAR szBuff[BUFFERED_MAX_PATH];

	switch (message)
	{
	case WM_INITDIALOG:
		t = (HHMMSS *) lParam;
		sprintf_s(szBuff, sizeof(szBuff), TEXT("%d"), t->hour);
		SetDlgItemText(hDlg, IDC_TIMEOFDAY_HH,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), TEXT("%02d"), t->min);
		SetDlgItemText(hDlg, IDC_TIMEOFDAY_MM,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), TEXT("%02d"), t->sec);
		SetDlgItemText(hDlg, IDC_TIMEOFDAY_SS,  szBuff);
		return TRUE;

	case WM_COMMAND:
		if(LOWORD(wParam) == IDOK) 
		{
			GetDlgItemText(hDlg, IDC_TIMEOFDAY_HH, szBuff, BUFFERED_MAX_PATH);
			t->hour = _ttol(szBuff);
			GetDlgItemText(hDlg, IDC_TIMEOFDAY_MM, szBuff, BUFFERED_MAX_PATH);
			t->min = _ttol(szBuff);
			GetDlgItemText(hDlg, IDC_TIMEOFDAY_SS, szBuff, BUFFERED_MAX_PATH);
			t->sec = _ttol(szBuff);

			if(t->hour < 0)
				t->hour = 0;

			if(t->min <0)
				t->min = 0;
			else if(t->min > 59)
				t->min = 59;

			if(t->sec <0)
				t->sec = 0;
			else if(t->sec > 59)
				t->sec = 59;

			EndDialog(hDlg, LOWORD(wParam));
			return TRUE;
		}

		if(LOWORD(wParam) == IDCANCEL) 
		{
			EndDialog(hDlg, LOWORD(wParam));
			return TRUE;
		}
	}
    return FALSE;
}


#define SZ_ABOUT_MMMBS			"Marine Mammal Movement and Behavior Simulator (3MB)\n\n"
#define SZ_ABOUT_VERSION		"     Version "
#define SZ_ABOUT_BUILD			"     GUI Build Date: "
#define SZ_ABOUT_BUILD_3MBSLIB	"     3MB Library Build Date: "
#define SZ_ABOUT_COPYWRITE		"     Copyright (C) 2006, 2007, 2008, 2009, 2010 Biomimetica\n\n"
#define SZ_ABOUT_CONCEPT		"     Program, Algorithim, and Concept Design by\n"
#define SZ_ABOUT_DORIAN			"           Dorian S. Houser, Biomimetica@cox.net\n\n"
#define SZ_ABOUT_IMPLEMENT		"     Implemented by\n"
#define SZ_ABOUT_MATTCROSS		"           Matt Cross, MatthewJanCross@gmail.com"


#define SZ_ABOUT_VERSION2		"3MB Version "
#define SZ_ABOUT_BUILD2			"GUI Build Date: "

#define SZ_ABOUT_BUILD_3MBSLIB2	"3MB Library Build Date: "


LRESULT CALLBACK DisplayDefaultParams(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
	TCHAR szBuff[800];
	TCHAR szDate[SIZE_32];
	TCHAR szTime[SIZE_32];
	CScenario sce;
	CStaticScenario sceStat;

	int i;
	//USERPARAMS params = sce.GetCurrentScenarioParams();
	SCENARIOPARAMS sceParams = sce.GetScenarioParamsCopy();

	sceStat.GetBuildDateTimeString(szDate, sizeof(szDate), szTime, sizeof(szTime));

	lParam = lParam; // keep the compiler warning quiet.
	switch (message)
	{
		case WM_INITDIALOG:
			sprintf_s(szBuff, sizeof(szBuff), "%s%d.%02d\n\n%s%s, %s\n%s%s, %s",
				SZ_ABOUT_VERSION2,
				MMBSLIB_VERSION_SUPER,
				MMBSLIB_VERSION_SUB,
				SZ_ABOUT_BUILD2,
				__DATE__,
				__TIME__,
				SZ_ABOUT_BUILD_3MBSLIB2,
				szDate,
				szTime
				);

			SetDlgItemText(hDlg, IDC_STATIC_ABOUT, szBuff);

			memset(szBuff, 0, sizeof(szBuff));

			sprintf_s(&szBuff[strlen(szBuff)], sizeof(szBuff)-strlen(szBuff), "Bathymetry depth that all animats beach: %5.2f\n", (float)BATHY_MIN_SEED_DEPTH );


			sprintf_s(&szBuff[strlen(szBuff)], sizeof(szBuff)-strlen(szBuff), "Max number of inhabitants:\n");
			sprintf_s(&szBuff[strlen(szBuff)], sizeof(szBuff)-strlen(szBuff), "\tAcoustic sources and animats combined: %d\n", MAX_NUM_ANIMATS);

			{
				BOOL multipleSourcesAllowed = MULTISOUNDSOURCEALLOWED; // to prevent compiler warning.
				if(multipleSourcesAllowed == FALSE)
					sprintf_s(&szBuff[strlen(szBuff)], sizeof(szBuff)-strlen(szBuff), "\tMax number of acoustic sources: %d\n", 1);
			}

			sprintf_s(&szBuff[strlen(szBuff)], sizeof(szBuff)-strlen(szBuff), "\nAcoustic Source Ping\n");
			sprintf_s(&szBuff[strlen(szBuff)], sizeof(szBuff)-strlen(szBuff), "\tOutput dB: %.1f\n", (float)ACSTC_SOURCE_LEVEL_DB);
			sprintf_s(&szBuff[strlen(szBuff)], sizeof(szBuff)-strlen(szBuff), "\tStart iteration: %d\n", ACSTC_SOURCE_BEGINS_ITERATION);
			sprintf_s(&szBuff[strlen(szBuff)], sizeof(szBuff)-strlen(szBuff), "\tDuty cycle: %03.2f\n", 1.0/ACSTC_SOURCE_DUTY_PERIOD);
			sprintf_s(&szBuff[strlen(szBuff)], sizeof(szBuff)-strlen(szBuff), "\n");


			// System Allocation
			sprintf_s(&szBuff[strlen(szBuff)], sizeof(szBuff)-strlen(szBuff), "Max scenario duration: 7 days (%d seconds)\n", 7 * 24 * 60 * 60);
			sprintf_s(&szBuff[strlen(szBuff)], sizeof(szBuff)-strlen(szBuff), "Max phys memory dynamically allocated for output buffer: %d GB.\n", ((MAXIMUM_PHYSICAL_MEMORY_ALLOC/1024)/1024)/1024);
			sprintf_s(&szBuff[strlen(szBuff)], sizeof(szBuff)-strlen(szBuff), "\n");
			SetDlgItemText(hDlg, IDC_STATIC_DEFAULT_VALUES, szBuff);


			memset(szBuff, 0, sizeof(szBuff));
			sprintf_s(&szBuff[strlen(szBuff)], sizeof(szBuff)-strlen(szBuff), "Species groups (%d):\n", NUM_SPEGROUPS_INUSE - 1); // minus 1 for the acoustic source

			for(i=0; i<(NUM_SPEGROUPS_INUSE)/2 && i<NUM_SPEGROUPS_INUSE-1; i++)
			{
				sprintf_s(&szBuff[strlen(szBuff)], sizeof(szBuff)-strlen(szBuff), "\t%s\n", SZSPECIESGROUPNAMEBUFFER[i]);
				sprintf_s(&szBuff[strlen(szBuff)], sizeof(szBuff)-strlen(szBuff), "\t\tLvl A Phys: %.1f\n", sceParams.speciesGroup[i].lvlAphys);
				sprintf_s(&szBuff[strlen(szBuff)], sizeof(szBuff)-strlen(szBuff), "\t\tLvl B Phys: %.1f\n", sceParams.speciesGroup[i].lvlBphys);
				if(i != SPECIALCONSIDRTNS)
				{
					sprintf_s(&szBuff[strlen(szBuff)], sizeof(szBuff)-strlen(szBuff), "\t\tRisk: %.1f\n", sceParams.speciesGroup[i].lvlBBeh_RiskA);
				}
				else
				{
					sprintf_s(&szBuff[strlen(szBuff)], sizeof(szBuff)-strlen(szBuff), "\t\tLvl B Beh: %.1f\n", sceParams.speciesGroup[i].lvlBBeh_RiskA);
				}
			}
			SetDlgItemText(hDlg, IDC_STATIC_DEFAULT_VALUES2, szBuff);

			memset(szBuff, 0, sizeof(szBuff));
			sprintf_s(&szBuff[strlen(szBuff)], sizeof(szBuff)-strlen(szBuff), "\n");
			for(; i<(NUM_SPEGROUPS_INUSE-1); i++)
			{
				sprintf_s(&szBuff[strlen(szBuff)], sizeof(szBuff)-strlen(szBuff), "\t%s\n", SZSPECIESGROUPNAMEBUFFER[i]);
				sprintf_s(&szBuff[strlen(szBuff)], sizeof(szBuff)-strlen(szBuff), "\t\tLvl A Phys: %.1f\n", sceParams.speciesGroup[i].lvlAphys);
				sprintf_s(&szBuff[strlen(szBuff)], sizeof(szBuff)-strlen(szBuff), "\t\tLvl B Phys: %.1f\n", sceParams.speciesGroup[i].lvlBphys);
				if(i != SPECIALCONSIDRTNS)
				{
					sprintf_s(&szBuff[strlen(szBuff)], sizeof(szBuff)-strlen(szBuff), "\t\tRisk: %.1f\n", sceParams.speciesGroup[i].lvlBBeh_RiskA);
				}
				else
				{
					sprintf_s(&szBuff[strlen(szBuff)], sizeof(szBuff)-strlen(szBuff), "\t\tLvl B Beh: %.1f\n", sceParams.speciesGroup[i].lvlBBeh_RiskA);
				}
			}
			SetDlgItemText(hDlg, IDC_STATIC_DEFAULT_VALUES3, szBuff);
			return TRUE;

		case WM_COMMAND:
			if (LOWORD(wParam) == IDOK || LOWORD(wParam) == IDCANCEL) 
			{
				EndDialog(hDlg, LOWORD(wParam));
				return TRUE;
			}
			break;
	}

//	{
//		TCHAR dog[100];
//		CommDlg_OpenSave_GetFolderPath(hDlg, 100, dog); 
//	}

    return FALSE;
}


// Mesage handler for about box.
LRESULT CALLBACK About(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
	TCHAR szBuff[800];
	TCHAR szDate[SIZE_32];
	TCHAR szTime[SIZE_32];
	CStaticScenario sceStat;

	sceStat.GetBuildDateTimeString(szDate, sizeof(szDate), szTime, sizeof(szTime));

	lParam = lParam; // keep the compiler warning quiet.
	switch (message)
	{
		case WM_INITDIALOG:
//			sprintf_s(szBuff, "%s%s%.2f\n\n%s %s\n\n%s%s%s%s%s", SZ_ABOUT_MMMBS, SZ_ABOUT_VERSION, SZ_ABOUT_BUILD, "dateTime", MMMBS_VERSION,
//				SZ_ABOUT_COPYWRITE, SZ_ABOUT_CONCEPT, SZ_ABOUT_DORIAN, SZ_ABOUT_IMPLEMENT, SZ_ABOUT_MATTCROSS);
			sprintf_s(szBuff, sizeof(szBuff), "%s %s%d.%02d\n\n%s%s, %s\n%s%s, %s\n\n%s%s%s%s%s",
				SZ_ABOUT_MMMBS,
				SZ_ABOUT_VERSION,
				MMBSLIB_VERSION_SUPER,
				MMBSLIB_VERSION_SUB,
				SZ_ABOUT_BUILD,
				__DATE__,
				__TIME__,
				SZ_ABOUT_BUILD_3MBSLIB,
				szDate,
				szTime,
				SZ_ABOUT_COPYWRITE,
				SZ_ABOUT_CONCEPT,
				SZ_ABOUT_DORIAN,
				SZ_ABOUT_IMPLEMENT,
				SZ_ABOUT_MATTCROSS);


			SetDlgItemText(hDlg, IDC_STATIC_ABOUT, szBuff);
				return TRUE;

		case WM_COMMAND:
			if (LOWORD(wParam) == IDOK || LOWORD(wParam) == IDCANCEL) 
			{
				EndDialog(hDlg, LOWORD(wParam));
				return TRUE;
			}
			break;
	}

//	{
//		TCHAR dog[100];
//		CommDlg_OpenSave_GetFolderPath(hDlg, 100, dog); 
//	}

    return FALSE;
}




// Returns TRUE if it updated
void UpdateDataExtraction(GLOBALDATA *gdata, BOOL Initialize = FALSE)
{
	static DWORD numAnimats;
	static DWORD lastUpdateTick;
	static TCHAR  szBuff[SIZE_128] = {0};
	static DWORD callCnt;
	//HHMMSS hhmmss;
	FESTATE state;
	FESETUP setUp;
	double prcnt;

	//PostMessage(gdata->hwin, WM_UPDATE_PROGRESS_MESSAGE , 0, (LPARAM)"need to implement feedback in UpdateDataExtraction()");

	if(Initialize == TRUE)
	{
		lastUpdateTick = 0;
		callCnt = 0;
	}
	else
	{
		if(GetTickCount() - lastUpdateTick < REFRESHTICKS)
			return;
		lastUpdateTick = GetTickCount();
	}

	setUp = gdata->fileExtractor.GetSetup();
	state = gdata->fileExtractor.GetState();

	if(state.activity != EXTRACTOR_EXTRACTING)
	{
		if(callCnt == 0)
		{
			PostMessage(gdata->hwin, WM_UPDATE_PROGRESS_MESSAGE , 0, (LPARAM)"Extractor Initializing");
			PostMessage(gdata->hwin, WM_UPDATE_PROGRESS_BAR, (int)0, /*seconds remaining*/15);
		}
		return;
	}


	// percent done = ((current animat) * current iteration) / (total number animats * num iterations)
	prcnt = 100* ((double)((state.lastAnimat-1)*setUp.sceParams.saveIterationCnt +
							(state.currentIteration+1)*(state.currentAnimat - (state.lastAnimat-1))) /
				(double)(setUp.sceParams.saveIterationCnt* setUp.sceParams.totalNumAnimats));


	if(setUp.specificAnimatSet == TRUE)
	{
		sprintf_s(szBuff, sizeof(szBuff), "Extracting animat %d", setUp.specificAnimat);
		prcnt = 100 * ((double)(state.currentIteration + 1) / (double)setUp.sceParams.saveIterationCnt);
	}
	else if(setUp.bin.outputByTime == TRUE)
	{
		sprintf_s(szBuff, sizeof(szBuff), "Extracting files: iteration %04d of %04d, animats %4d - %4d",
				state.currentIteration, setUp.sceParams.saveIterationCnt, state.lastAnimat, state.currentAnimat);
	}
	else// output by animat
	{
		sprintf_s(szBuff, sizeof(szBuff), "Extracting files: animat %4d of %4d, iteration %04d of %04d",
			state.currentAnimat, setUp.sceParams.totalNumAnimats, state.currentIteration,
			setUp.sceParams.saveIterationCnt);
	}

	PostMessage(gdata->hwin, WM_UPDATE_PROGRESS_MESSAGE , 0, (LPARAM)szBuff);
	PostMessage(gdata->hwin, WM_UPDATE_PROGRESS_BAR, (int)prcnt, /*seconds remaining*/15);
	callCnt++;
}




DWORD WINAPI ExtractionThread(LPVOID lpParameter)
{
	GLOBALDATA *gdata = (GLOBALDATA *)lpParameter;
	//FILE_INFO fiMbsOut = {0};
	RESLT mbsRes;
	//DWORD updateCount = 0;
	//BOOL abort = FALSE;
	static TCHAR szBuff[SIZE_128];
	static TCHAR szBuffCaption[SIZE_128];
	C3mbStaticsLib staticLib;


	// Verify only a single instance of this thread is running.  Catch it in debug mode
	// with an assertion and handle it in release mode with a return.
	_ASSERT(gdata->extractThreadInf.running == FALSE);
	_ASSERT(gdata->extractThreadInf.exit == FALSE);
	if(gdata->extractThreadInf.running == TRUE)
		return 0;

	// Indicate this thread is running.
	gdata->extractThreadInf.running = TRUE;


//	if(gdata->outputFolderSet == FALSE)
//		_ASSERT(strlen(gdata->szOutputFolder) == 0);


	//----------------------------------------------------------------------------------//
	// Launch the extraction process thread.
	//--------------------------------------//
	// If there is a problem on launching the extraction process then abort
	mbsRes = gdata->fileExtractor.ExtractBinaryResultsIntoTextFiles(
				gdata->extractThreadInf.fileInf.szFileName,
				TRUE,
				gdata->textOutputConfig.splitTextOutput,
				gdata->textOutputConfig.iterationsPerFile);

	if(mbsRes == OK)
	{
		PostMessage(gdata->hwin, WM_UPDATE_GUI, NULL, NULL);

		// Enable extract button so user may abort extraction process
		EnableWindow(GetDlgItem(gdata->hwin, IDC_EXTRACT_TEXT_FROM_BIN), TRUE);

		// Provide some initial feedback to the user
		PostMessage(gdata->hwin, WM_UPDATE_GUI, NULL, NULL);
		strcpy_s(szBuff, sizeof(szBuff), "Extracting text files...");
		PostMessage(gdata->hwin, WM_ADD_LIST_BOX_MESSAGE , 0, (LPARAM)szBuff);

		// Wait for the file extractor to spin up.
		while(gdata->fileExtractor.GetState().activity == EXTRACTOR_IDLE_PRERUN)
			Sleep(30);

		// Get initial information from the extraction process for updating the GUI.
		UpdateDataExtraction(gdata, TRUE); // TRUE means initialize

		// Continuously poll the extraction process and update the GUI until either the
		// user aborts or the extraction process finishes.
		while(gdata->extractThreadInf.exit == FALSE && gdata->fileExtractor.GetState().activity != EXTRACTOR_IDLE_POSTRUN)
		{
			UpdateDataExtraction(gdata);
			Sleep(25);
		}

		// If the user aborted the extraction then signal the extraction process to abort
		// and wait until it indicates it is finished running.
		if(gdata->extractThreadInf.exit == TRUE)
		{
			gdata->fileExtractor.Abort();
			while(gdata->fileExtractor.GetState().activity != EXTRACTOR_IDLE_POSTRUN)
				Sleep(30);
		}
		UpdateDataExtraction(gdata);
	}

	// Handle the reason the extraction ended
	if(gdata->extractThreadInf.exit == TRUE)
	{
		// The user aborted.  Post a message.
		strcpy_s(szBuff, sizeof(szBuff), "Extraction aborted");
		PostMessage(gdata->hwin, WM_ADD_LIST_BOX_MESSAGE, 0, (LPARAM)szBuff);
	}
	else if(mbsRes != OK)
	{
		 // There was an error with the extraction process
		memset(szBuff, 0, SIZE_128);
		staticLib.MbsResultToString(mbsRes, &szBuff[strlen(szBuff)], SIZE_128-strlen(szBuff), szBuffCaption, SIZE_128);
		MessageBox(NULL, szBuff, szBuffCaption, 0);
	}
	else // extraction process finished.
	{
		PostMessage(gdata->hwin, WM_UPDATE_PROGRESS_BAR, 100, 0);
		strcpy_s(szBuff, sizeof(szBuff), "Done");
		PostMessage(gdata->hwin, WM_ADD_LIST_BOX_MESSAGE , 0, (LPARAM)szBuff);
	}

	// Post simulation variable assignment
	gdata->extractThreadInf.running = FALSE;
	gdata->extractThreadInf.exit = FALSE;
	PostMessage(gdata->hwin, WM_UPDATE_GUI, NULL, NULL);
	return 0;
}


LRESULT CALLBACK ExamineBinOutDlgProc(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
	static HHMMSS *t;
	static TCHAR szBuff[SIZE_512];
	static EXMNBINOUTPROCSTRCT *param;
	static COutputReader reader;
	static SCENARIOPARAMS sceParams;
	static FESETUP feSetup;
	static TCHAR *szPtr;
	static TCHAR *szByAnimat = "animat";
	static TCHAR *szByIteration = "iteration";
	int wmId, wmEvent;
	CFileManagerStatic fileManagerStatic;
	//INT_PTR res;

	HHMMSS start;
	DDHHMMSS end;

	switch (message)
	{
	case WM_INITDIALOG:
		param = (EXMNBINOUTPROCSTRCT *) lParam;
		feSetup = param->fileExtractor->GetSetup();
		reader.OpenOutputFile(param->szFileName);
		sceParams = fileManagerStatic.ConvertScenarioFormat(reader.GetSceParams());

		EnableWindow(GetDlgItem(hDlg, IDC_EDIT_SPECIFICANIMAT), feSetup.specificAnimatSet);
		EnableWindow(GetDlgItem(hDlg, IDC_EDIT_SPECIFICANIMAT), feSetup.specificAnimatSet); 

		if(feSetup.specificAnimatSet == FALSE)
			SetDlgItemText(hDlg, IDC_BUTTON_ENABLESPECFICANIMAT, "Disabled");
		else
			SetDlgItemText(hDlg, IDC_BUTTON_ENABLESPECFICANIMAT, "Enabled");
		SetDlgItemText(hDlg, IDC_EDIT_SPECIFICANIMAT, "0");

		sprintf_s(szBuff, sizeof(szBuff), "Extract specific animat at index (%d max):", sceParams.totalNumAnimats-1);
		SetDlgItemText(hDlg, IDC_STATIC_SPECIFICANIMAT, szBuff);

		if(sceParams.user.output.outputByTime == TRUE)
			szPtr = szByIteration;
		else
			szPtr = szByAnimat;

		start = staticLib.Time_To24HrMinSec(sceParams.startTime);
		end = staticLib.Time_ToDayHrMinSec(sceParams.startTime + sceParams.duration);

		sprintf_s(szBuff,
			sizeof(szBuff),
			EXAMINEOUTPUTSTRING,  // The string itself
			param->szFileName, // File name
			sceParams.libVerSuper,			 // Super version number
			sceParams.libVerSub,				 // Sub version number
			sceParams.numSaveIterations, // Number of iterations saved
			sceParams.totalNumAnimats,			   // Total number of animats in the scenario
			0,
			start.hour,
			start.min,
			start.sec,				// Start time
			end.day,
			end.hour,
			end.min,
			end.sec,				// end time.
			sceParams.duration,		// duration
			szPtr);					// output format
		//IDC_STATIC
		SetDlgItemText(hDlg, IDC_STATIC, szBuff);
		return TRUE;

	case WM_COMMAND:
		wmId = LOWORD(wParam); 
		wmEvent = HIWORD(wParam); 

		if(LOWORD(wParam) == IDOK) 
		{
			reader.CloseOutputFile();
			param->fileExtractor->SetSpecificAnimatEnabled(feSetup.specificAnimatSet);
			if(feSetup.specificAnimatSet == TRUE)
			{
				GetDlgItemText(hDlg, IDC_EDIT_SPECIFICANIMAT, szBuff, sizeof(szBuff));
				if(strlen(szBuff) != 0)
					param->fileExtractor->SetSpecificAnimatIndex((DWORD)atoi(szBuff));
				else
					param->fileExtractor->SetSpecificAnimatEnabled(FALSE);
			}
			EndDialog(hDlg, IDOK);
			return TRUE;
		}

		if(LOWORD(wParam) == IDCANCEL) 
		{
			reader.CloseOutputFile();
			EndDialog(hDlg, IDCANCEL);
			return FALSE;
		}

		switch(wmId)
		{
		case IDC_BUTTON_ENABLESPECFICANIMAT:
			feSetup.specificAnimatSet = !feSetup.specificAnimatSet;
			EnableWindow(GetDlgItem(hDlg, IDC_EDIT_SPECIFICANIMAT), feSetup.specificAnimatSet); 
			if(feSetup.specificAnimatSet == FALSE)
				SetDlgItemText(hDlg, IDC_BUTTON_ENABLESPECFICANIMAT, "Disabled");
			else
				SetDlgItemText(hDlg, IDC_BUTTON_ENABLESPECFICANIMAT, "Enabled");
			break;
		}
	}
    return FALSE;
}