#include "3mbLoadShapeFileDlg.h"
#include "3mbGuiFunctions.h"
#include "3mbsLib.h"
#include "ListManager.h"

DWORD WINAPI ShapeFileSearchQueryThread(LPVOID lpParameter);
DWORD WINAPI ShapeFileLoadFromFileThread(LPVOID lpParameter);
DWORD WINAPI RunPDFThread(LPVOID lpParameter);
void HandlePreviousDataLoaded();

extern HWND g_hDlgSeed;
extern C3mbStaticsLib staticLib;


LRESULT CALLBACK LoadShapeFileDlgProc(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
	// No wait loops in GUIs!
	TCHAR szBuff0[SIZE_1024];
	TCHAR szBuff1[BUFFERED_MAX_PATH];
	RESLT res;
	int wmId, wmEvent;

	static DYNAMICSTRING s_dynamicStr = {0};
	static int s_killThreadCnt = 0;
	static SHAPE_PARAM *s_pParamRef = NULL;
	static SHAPE_FILE_THREAD_PARAM s_thread = {0};
	static USERCHOICELISTS s_userChoiceLists;
	static SHAPEFILE_LISTBOX_INDICES s_listBoxes;
	static BOOL s_mbSpeFileMatched = FALSE;
	static FILE_INFO s_speFileInf = {0};
	static double s_mean = 14.22, s_std = 7.59;
	static BOOL pdfParamsValid = TRUE;;
	static GAUSS_CUMULATIVE_PDF_ARRAY s_pdfParams;
	double ftemp;
	static C3MBRandom *s_p3mbRandomRef;


	FILE_INFO fileInf = {0};
	BOOL fileSelected = FALSE;
	
	switch(message)
	{
	case WM_INITDIALOG:
		s_p3mbRandomRef = new C3MBRandom();

		s_p3mbRandomRef->mysrand(3);

		s_pParamRef = (SHAPE_PARAM *)lParam;
		_ASSERT(s_pParamRef->pRefSce != NULL);
		sprintf_s(szBuff0, sizeof(szBuff0), "%.3f", s_mean);
		SetDlgItemText(hDlg, IDC_EDIT_AVE_DIST, szBuff0);
		sprintf_s(szBuff0, sizeof(szBuff0), "%.3f", s_std);
		SetDlgItemText(hDlg, IDC_EDIT_STD_DIST, szBuff0);

		SendMessage(hDlg, WM_3MB_RESET, NULL, NULL);

		//HandlePreviousDataLoaded();
		break;

	case WM_3MB_RESET:
		// Frees resources associated with loadingin a shape file.
		
		//------------------------------------------------------------------------------//
		// All threads need to have exited before posting or sending this window message.
		//------------------------------------------------------------------------------//
		_ASSERT(s_thread.threadInf.exit == FALSE && s_thread.threadInf.hdl == NULL && s_thread.threadInf.running == FALSE);

		// The following condition would result from a programming error and shouldn't actually happen.
		// Handle it anyway just in case.  Don't want while-loops in window messages because they
		// potentially lock up the GUI.
		if(s_thread.threadInf.running == TRUE  || s_thread.threadInf.hdl != NULL)
		{
			s_thread.threadInf.exit = TRUE;
			PostMessage(hDlg, WM_3MB_RESET, 0, 0);
			break;
		}
		s_thread.threadInf.exit = FALSE;
		//------------------------------------------------------------------------------//

		memset(&s_speFileInf, 0, sizeof(FILE_INFO));

		// Close shape file-related handles.
		if(s_thread.shpHdl != NULL)
			SHPClose(s_thread.shpHdl);
		s_thread.shpHdl = NULL;
		if(s_thread.dbfHdl != NULL)
			DBFClose(s_thread.dbfHdl);
		s_thread.dbfHdl = NULL;

		// Set the list box indices to a non-index value
		s_listBoxes.species = s_listBoxes.season = s_listBoxes.study = s_listBoxes.stock = -1;
		s_mbSpeFileMatched = FALSE;

		// Dallocate the dynamic string
		if(s_dynamicStr.szBuff != NULL)
			free(s_dynamicStr.szBuff);
		memset(&s_dynamicStr, 0, sizeof(s_dynamicStr));

		// Deallocate polygon list memory
		Deallocate3MBShapeFileInfStruct(&s_pParamRef->pdfInf);

		// Clear out variables.
		memset(&s_thread.threadInf, 0, sizeof(s_thread.threadInf));
		memset(&s_pParamRef->fileInf, 0, sizeof(FILE_INFO));

		_ASSERT(s_pParamRef->pdfInf.numPolygons == 0 && s_pParamRef->pdfInf.pPolygonInfBuff == NULL);

		memset(&s_pParamRef->pdfInf, 0, sizeof(SHAPEFILEINF));
		ClearUserChoiceLists(&s_userChoiceLists);


		// Set the dlg controls to initial state.
		SetWindowText(hDlg, "Load Shape File: -Biomimetica");
		SetDlgItemText(hDlg, IDC_STATIC_FILENAME, "Shape File: (not loaded)");
		SetDlgItemText(hDlg, IDC_STATIC_NUM_ENTRIES, "");
		//SetDlgItemText(hDlg, IDC_STATIC_SHAPE_TYPE, "");
		SetDlgItemText(hDlg, IDC_STATIC_SHAPE_BOUNDS_MIN, "");
		SetDlgItemText(hDlg, IDC_STATIC_SHAPE_BOUNDS_MAX, "");
		SetDlgItemText(hDlg, IDC_STATIC_SPECIES_FILE_NAME, "");
		SetDlgItemText(hDlg, IDC_STATIC_SPECIES_FILE_NAME2, "");
		SetDlgItemText(hDlg, IDC_STATIC_SPECIES_FILE_NAME3, "");
		SetDlgItemText(hDlg, IDC_EDIT_LOADSPEFILEFEEDBACK, "");

		EnableWindow(GetDlgItem(hDlg, IDC_BUTTON_PDF_SEED), FALSE);
		EnableWindow(GetDlgItem(hDlg, IDC_EDIT_AVE_DIST), FALSE);
		EnableWindow(GetDlgItem(hDlg, IDC_STATIC_AVEDIST), FALSE);
		EnableWindow(GetDlgItem(hDlg, IDC_EDIT_STD_DIST), FALSE);
		EnableWindow(GetDlgItem(hDlg, IDC_STATIC_STDEVDIST), FALSE);

		SetDlgItemText(hDlg, IDC_STATIC_POSSIBLESHAPEFILE_HEADER,
			"Search Query's (Possible) Associated 3MB Species Files (.spe):");


		SetDlgItemText(hDlg, IDC_STATIC_NUM_POLYGONS, "");
		SetDlgItemText(hDlg, IDC_STATIC_EXPADDCNT, "");
		SetDlgItemText(hDlg, IDC_STATIC_SEARCHQUERY, "{ }");

		SetDlgItemText(hDlg, IDC_STATIC_ACTUALCNT, "0");
		SetDlgItemText(hDlg, IDC_STATIC_ADD_STATUS, "");


		ShowWindow(GetDlgItem(hDlg, IDC_BUTTON_ENVDLG_SPEC_ADD), SW_HIDE);

		SendMessage(GetDlgItem(hDlg, IDC_LIST_SPECIES), LB_RESETCONTENT, (WPARAM)0, (LPARAM)0);
		SendMessage(GetDlgItem(hDlg, IDC_LIST_SEASON), LB_RESETCONTENT, (WPARAM)0, (LPARAM)0);
		SendMessage(GetDlgItem(hDlg, IDC_LIST_STUDY), LB_RESETCONTENT, (WPARAM)0, (LPARAM)0);
		SendMessage(GetDlgItem(hDlg, IDC_LIST_STOCK), LB_RESETCONTENT, (WPARAM)0, (LPARAM)0);

		SendMessage(GetDlgItem(hDlg, IDC_LIST_SPECIES), LB_SETCURSEL, (WPARAM)-1, 0);
		SendMessage(GetDlgItem(hDlg, IDC_LIST_SEASON), LB_SETCURSEL, (WPARAM)-1, 0);
		SendMessage(GetDlgItem(hDlg, IDC_LIST_STUDY), LB_SETCURSEL, (WPARAM)-1, 0);
		SendMessage(GetDlgItem(hDlg, IDC_LIST_STOCK), LB_SETCURSEL, (WPARAM)-1, 0);

		//SetDlgItemText(hDlg, IDC_EDIT_LOADSPEFILEFEEDBACK, "Walk The Dog");
		break;


	case WM_3MB_RUN_SEARCH_QUERY_THREAD:
		// This is the only place where the s_thread may be running already.
		// Wait for running therad to exit
		if(s_thread.threadInf.running == TRUE || s_thread.threadInf.hdl != NULL)
		{
			s_thread.threadInf.exit = TRUE;
			PostMessage(hDlg, WM_3MB_RUN_SEARCH_QUERY_THREAD, NULL, NULL);
			break;
		}
		s_thread.threadInf.exit = FALSE;

		memcpy(&s_thread.LBIndicesCpy, &s_listBoxes, sizeof(s_listBoxes));
		s_thread.pMbSpeFileMatchedRef = &s_mbSpeFileMatched;
		s_thread.p3mbRandomRef = s_p3mbRandomRef;
		s_thread.threadInf.hdl = CreateThread(NULL, 0, &ShapeFileSearchQueryThread, &s_thread, 0,
			&s_thread.threadInf.id);
		//EnableWindow(hDlg, TRUE); // prevent input
		break;

	case WM_SHUTDOWN_THREAD:
		if(s_thread.threadInf.running == TRUE  || s_thread.threadInf.hdl != NULL)
		{
			s_thread.threadInf.exit = TRUE;
			PostMessage(hDlg, WM_SHUTDOWN_THREAD, 0, 0);
			break;
		}
		s_thread.threadInf.exit = FALSE;
		break;

	case WM_USER_CANCEL:
		// Close any threads and resources then end the dialog box.
		if(s_thread.threadInf.running == TRUE  || s_thread.threadInf.hdl != NULL)
		{
			s_thread.threadInf.exit = TRUE;
			PostMessage(hDlg, WM_USER_CANCEL, 0, 0);
			break;
		}
		delete s_p3mbRandomRef;
		s_p3mbRandomRef = NULL;
		s_thread.threadInf.exit = FALSE;
		s_pParamRef->dlgHdl = NULL;

		// Re-enable various controls that were disabled while this dialog box was up.
		EnableWindow(GetDlgItem(g_hDlgSeed, IDC_LOAD_SCENARIO), TRUE);
		EnableWindow(GetDlgItem(g_hDlgSeed, IDC_BUTTON_ENVDLG_FIRSANIMATACSTSRCE), TRUE);
		EnableWindow(GetDlgItem(g_hDlgSeed, IDC_BUTTON_ENVDLG_SPEC_ADD), TRUE);
		EnableWindow(GetDlgItem(g_hDlgSeed, IDC_BUTTON_ENVDLG_SPEC_REMOVE), TRUE);
		EnableWindow(GetDlgItem(g_hDlgSeed, IDC_LIST_SPECIES), TRUE);
		EnableWindow(GetDlgItem(g_hDlgSeed, IDC_LIST_INDIVIDUAL), TRUE);
		EnableWindow(GetDlgItem(g_hDlgSeed, IDC_LIST_PODS), TRUE);
		EnableWindow(GetDlgItem(g_hDlgSeed, IDC_LIST_SEL_POP), TRUE);
		//EnableWindow(GetDlgItem(g_hDlgSeed, x), TRUE);
		//EnableWindow(g_hDlgSeed, FALSE);

		EndDialog(hDlg, LOWORD(wParam));
		break;

	case WM_3MB_CHECKSTATUS:
		pdfParamsValid = TRUE;
		pdfParamsValid &= (s_listBoxes.species != -1 && s_listBoxes.season != -1 && s_listBoxes.study != -1 &&
			s_listBoxes.stock != -1);
		pdfParamsValid &= (s_mean > 0 && s_std > 0);

		if(pdfParamsValid == TRUE)
		{
			staticLib.DeallocateGausianPDFCumulativeBuffStruct(&s_pdfParams);
			s_pdfParams = staticLib.GenerateCumInvGaussPDF(s_mean, s_std);
			_ASSERT(s_pdfParams.buffLen > 1);
			pdfParamsValid &= (s_pdfParams.pCumulativeArray[s_pdfParams.buffLen-1] >= 0.99);
		}

		EnableWindow(GetDlgItem(hDlg, IDC_BUTTON_PDF_SEED), pdfParamsValid);
		break;


	case WM_COMMAND:
		wmId    = LOWORD(wParam);
		wmEvent = HIWORD(wParam);

		switch(wmId)
		{
		case IDOK:
		case IDCANCEL:
			PostMessage(hDlg, WM_USER_CANCEL, wmId, 0);
			break;

		case IDC_EDIT_AVE_DIST:
		case IDC_EDIT_STD_DIST:
			_ASSERT(s_thread.threadInf.running == FALSE && s_thread.threadInf.hdl == NULL);
			_ASSERT(s_pParamRef->pRefSce != NULL);
			if(wmEvent == EN_CHANGE)
			{
				GetDlgItemText(hDlg, wmId, szBuff0, sizeof(szBuff0));
				if(strlen(szBuff0) == 0 || staticLib.StringIsALegalNumber(szBuff0) == FALSE)
				{
					PostMessage(hDlg, WM_3MB_CHECKSTATUS, 0, 0);
					break;
				}

				ftemp = atof(szBuff0);

				// Negative signs can be entered but negative values here are not allowed
				// so force it positve and reset the edit control with sans the negative
				// sign.
				if(ftemp < 0)
				{
					ftemp *= -1.0;
					sprintf_s(szBuff0, sizeof(szBuff0), "%.3f", ftemp);
					SetDlgItemText(hDlg, wmId, szBuff0); // will generate another EN_CHANGE msg so break w/o checking status
					break;
				}

				if(wmId == IDC_EDIT_AVE_DIST)
					s_mean = ftemp;
				else
					s_std = ftemp;
				PostMessage(hDlg, WM_3MB_CHECKSTATUS, 0, 0);
				break;
			}
			else if(wmEvent == EN_KILLFOCUS)
			{
				GetDlgItemText(hDlg, wmId, szBuff0, sizeof(szBuff0));
				if(strlen(szBuff0) == 0 || staticLib.StringIsALegalNumber(szBuff0) == FALSE)
				{
					sprintf_s(szBuff0, sizeof(szBuff0), "%.3f", 0);
					SetDlgItemText(hDlg, wmId, szBuff0);
					if(wmId == IDC_EDIT_AVE_DIST)
						s_mean = 0;
					else
						s_std = 0;
					pdfParamsValid = FALSE;
					EnableWindow(GetDlgItem(hDlg, IDC_BUTTON_PDF_SEED), pdfParamsValid);
					break;
				}
				break;
			}

			break;

		case IDC_BUTTON_PDF_SEED:

			//--------------------------------------------------------------------------//
			// Prevent repetitive input (button-clicking)
			//------------------------------------------//
			// Prevent the user from clicking the seed button a second time before the
			// thread gets running or is completed.  The thread itself wll enable the
			// seed button.
			EnableWindow(GetDlgItem(hDlg, IDC_BUTTON_PDF_SEED), FALSE);


			//--------------------------------------------------------------------------//
			// Cancel a running seed thread
			//------------------------------//
			// If a thread is currently running to seed animats then cancel.
			if(s_thread.threadInf.running == TRUE)
			{
				_ASSERT(s_thread.threadInf.running == TRUE && s_thread.threadInf.hdl != NULL);
				PostMessage(hDlg, WM_SHUTDOWN_THREAD, 0, 0);
				break;
			}
			_ASSERT(s_thread.threadInf.hdl == NULL);


			//--------------------------------------------------------------------------//
			// Copy the parameters for seeding then launch the thread.
			//--------------------------------------------------------//
			s_thread.mean = s_mean;
			s_thread.std = s_std;
			s_thread.p3mbRandomRef = s_p3mbRandomRef;
			memcpy(&s_thread.speciesFileInf, &s_speFileInf, sizeof(FILE_INFO));
			s_thread.threadInf.hdl = CreateThread(NULL, 0, &RunPDFThread, &s_thread, 0, &s_thread.threadInf.id);
			break;

		case IDC_BUTTON_ENVDLG_SPEC_ADD:
			_ASSERT(s_thread.threadInf.running == FALSE && s_thread.threadInf.hdl == NULL);
			_ASSERT(s_pParamRef->pRefSce != NULL);

			fileSelected = FALSE;
			if(s_mbSpeFileMatched == TRUE)
			{
				// Set the s_speFileInf FILE_INFO struct so it contains the file name and file title of the matched species.
				memset(&s_speFileInf, 0, sizeof(FILE_INFO));
				_ASSERT(strlen(s_pParamRef->pdfInf.szSpeciesFileName) > 4 && strlen(s_pParamRef->pdfInf.szSpeciesFileName) < sizeof(s_speFileInf.szFileName)-1);
				_ASSERT(strlen(s_pParamRef->pdfInf.szSpeciesFileTitle) > 4 && strlen(s_pParamRef->pdfInf.szSpeciesFileTitle) < sizeof(s_speFileInf.szTitleName)-1);

				strncpy_s(fileInf.szFileName, sizeof(fileInf.szFileName), s_pParamRef->pdfInf.szSpeciesFileName, sizeof(s_speFileInf.szFileName)-1);
				strncpy_s(fileInf.szTitleName, sizeof(fileInf.szTitleName), s_pParamRef->pdfInf.szSpeciesFileTitle, sizeof(s_speFileInf.szTitleName)-1);
				fileSelected = TRUE;
			}
			else
			{
				// Select a file name and title into the s_speFileInf FILE_INFO struct.

				// Load in a shape file.
				memset(&fileInf, 0, sizeof(FILE_INFO));
				if(OK != (res = MyGetOpenFileName(&fileInf, hDlg, SZSPEFILTER, SZSPEDEFEXT, &fileSelected)))
				{
					// Handle error.
					sprintf_s(szBuff0, sizeof(szBuff0), TEXT("Problem with selected species file (%s):  %s"),
						fileInf.szTitleName, staticLib.MbsResultToString(res, szBuff1, sizeof(szBuff1)));
					MessageBox(hDlg, szBuff0, "Load File Error", 0);
				}				
			}

			_ASSERT((fileSelected==TRUE && strlen(fileInf.szFileName)>4 && strlen(fileInf.szTitleName)>4) ||
				(fileSelected==FALSE && strlen(fileInf.szFileName)==0 && strlen(fileInf.szTitleName)==0));

			if(fileSelected == FALSE)
				break;

			// Determine if the Seed button should be enabled and do so.
			memcpy(&s_speFileInf, &fileInf, sizeof(FILE_INFO));
			SendMessage(hDlg, WM_3MB_CHECKSTATUS, 0, 0);

			SetDlgItemText(hDlg, IDC_STATIC_POSSIBLESHAPEFILE_HEADER, "Selected 3MB Species File (.spe):");
			SetDlgItemText(hDlg, IDC_STATIC_SPECIES_FILE_NAME2, s_speFileInf.szFileName);
			SetDlgItemText(hDlg, IDC_STATIC_SPECIES_FILE_NAME3, "");
			
			EnableWindow(GetDlgItem(hDlg, IDC_EDIT_AVE_DIST), TRUE);
			EnableWindow(GetDlgItem(hDlg, IDC_STATIC_AVEDIST), TRUE);
			EnableWindow(GetDlgItem(hDlg, IDC_EDIT_STD_DIST), TRUE);
			EnableWindow(GetDlgItem(hDlg, IDC_STATIC_STDEVDIST), TRUE);
			break;

		case IDC_BUTTON_LOAD_SHAPEFILE:
			_ASSERT(s_thread.threadInf.running == FALSE && s_thread.threadInf.hdl == NULL);

			// Load in a shape file.
			memset(&fileInf, 0, sizeof(fileInf));
			if(OK != (res = MyGetOpenFileName(&fileInf, hDlg, SZSHAPEFILTER, SZSHAPEDEFEXT, &fileSelected)))
			{
				// Handle error.
				sprintf_s(szBuff0, sizeof(szBuff0), TEXT("Problem with selected shape file (%s):  %s"),
					fileInf.szTitleName, staticLib.MbsResultToString(res, szBuff1, sizeof(szBuff1)));
				MessageBox(hDlg, szBuff0, "Load File Error", 0);
				break;
			}				

			// No errors so check if user selected a file
			if(fileSelected == FALSE)
				break;

			// Free resources and reset the dialog gox
			SendMessage(hDlg, WM_3MB_RESET, 0, 0);

			// Set the Window Texts
			sprintf_s(szBuff0, "Load Shape File: %s  -Biomimetica", fileInf.szTitleName);
			SetWindowText(hDlg, szBuff0);
			SetDlgItemText(hDlg, IDC_STATIC_FILENAME, fileInf.szFileName);

			//--------------------------------------------------------------------------//
			// Launch the thread that loads in the shape file.
			//------------------------------------------------//
			// Initialize the variable passed into the thread.
			memcpy(&s_pParamRef->fileInf, &fileInf, sizeof(fileInf));
			s_thread.pShapeRef = s_pParamRef;
			s_thread.pUserChoiceListRef = &s_userChoiceLists;
			s_thread.parentWin = hDlg;
			s_thread.p3mbRandomRef = s_p3mbRandomRef;
			memcpy(&s_thread.LBIndicesCpy, &s_listBoxes, sizeof(s_listBoxes));
			// Launch the thread.
			s_thread.threadInf.hdl = CreateThread(NULL, 0, &ShapeFileLoadFromFileThread, &s_thread, 0, &s_thread.threadInf.id);
			break;

		case IDC_LIST_SPECIES:
		case IDC_LIST_SEASON:
		case IDC_LIST_STUDY:
		case IDC_LIST_STOCK:
			if(wmEvent == LBS_NOTIFY)
			{
				EnableWindow(hDlg, FALSE); // prevent input
				if(wmId == IDC_LIST_SPECIES)
					s_listBoxes.species = SendMessage(GetDlgItem(hDlg, IDC_LIST_SPECIES), LB_GETCURSEL, 0, 0);
				else if(wmId == IDC_LIST_SEASON)
					s_listBoxes.season = SendMessage(GetDlgItem(hDlg, IDC_LIST_SEASON), LB_GETCURSEL, 0, 0);
				else if(wmId == IDC_LIST_STUDY)
					s_listBoxes.study = SendMessage(GetDlgItem(hDlg, IDC_LIST_STUDY), LB_GETCURSEL, 0, 0);
				else if(wmId == IDC_LIST_STOCK)
					s_listBoxes.stock = SendMessage(GetDlgItem(hDlg, IDC_LIST_STOCK), LB_GETCURSEL, 0, 0);
				else // unexpected error condition
					s_listBoxes.species = s_listBoxes.season = s_listBoxes.study = s_listBoxes.stock = -1;

				PostMessage(hDlg, WM_3MB_RUN_SEARCH_QUERY_THREAD, NULL, NULL);
				break;
			}
		}
		break;
	}
	return FALSE;
}


