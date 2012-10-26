#include "resource.h"
#include "3mbSpeciesMdl.h"
#include "3mbGetBehaviorName.h"
#include "3mbSpeciesVectorMdl.h"
#include "3mbGuiFunctions.h"

/*----------------------------------------------------------------------------------------
	The Species builder Dialog.
	
	Standard dialog box code.  Displays the dialog box and values, reads in values user 
	entered, changes display based upon state of the dialog.
/*--------------------------------------------------------------------------------------*/
LRESULT CALLBACK SpeciesModelProc(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
	static TCHAR		  szFilter[] = TEXT ("Species Files (*.spe)\0*.spe\0"); 
	static TCHAR		  szDefExt[] = TEXT ("spe") ;
	static TCHAR		  szBuff[BUFFERED_MAX_PATH];

	static TCHAR		  szTextFilter[] = TEXT("User Models (*.txt)\0*.txt\0");
	static TCHAR		  szTextDefExt[] = TEXT("txt");
//		   TCHAR		  szBuffer[BUFFERED_MAX_PATH];
	static CSpeciesModel sm;

	static FILE_INFO	  fiBin;
	static FILE_INFO	  fiText;
	static EDIT_MODE editMode;
//	HWND hwnd;
	int wmId, wmEvent;
//	int i, cursor;
//	LRESULT	dlgRes;
//	BEHAVIOR_NAME bm;
//	RESLT res;
//	MBSSPECIESVECTORMDLPROCPARAM vmdlParam;

	switch (message)
	{
	case WM_INITDIALOG:
		editMode = (EDIT_MODE)lParam;
		memset(&fiBin, 0, sizeof(FILE_INFO));
		memset(&fiText, 0, sizeof(FILE_INFO));
		sm.DefaultMemberVariables(); // remove this.

#if 0

		switch(editMode)
		{
		case MAKE_NEW:
			// Populate the mbs with default values.  Zero out the MBS_SPECIES 
			// structure, copy the zero'd out structure to the GUI (zero-ing out
			// sets everything to default state), have the window indicate a new
			// file is in the works... (unnamed, not saved, etc...).
			SpeciesToGuiWindows(&sm, hDlg);
			break;

		case OPEN_EXISTING_FILE:
			if(GetOpenFileName(&fiBin.ofn) != NULL)
			{
				sm.LoadFromBinFile(fiBin.ofn.lpstrFile);
				hwnd = GetDlgItem(hDlg, IDC_LIST_VECTOR_MODEL_BEHAV_NAMES);
				for(i=0; i<sm.m_behaviorModel_TOBEREMOVED.behavior.rowCnt; i++)
					SendMessage(hwnd, LB_ADDSTRING, i, (LPARAM)sm.m_behaviorNamesArray_TOBEREMOVED[i].sz);
				FileChanged(&fiBin);
			}

			
			// User is entering this dialog with a previously build MBS_SPECIES
			// in mind to view or edit.  Prompt the user for the name of the file,
			// load it in, then copy to the GUI.  Take care of administrative issues
			// of indicating thsi file already has a name and doesn't need to be saved
			// (because it isn't modified yet).
			// If user hits cancel, go back to main dialog.
			SpeciesToGuiWindows(&sm, hDlg);
			fiBin.bNamed = TRUE;
			fiBin.bNeedSave = FALSE;
			break;

		case MODIFY_EXISTING:  // change to VIEW
//			fiBin.bNamed = FALSE;
//			fiBin.bNeedSave = FALSE;
//			mbs = *speciesBuilderParams->pSpec;
//			SpeciesToGuiWindows(&sm, hDlg);

			// Disable most of the Menu Bar Items
#pragma message("MATT'S NOTE: See File " __FILE__ " examine MODIFY_EXISTING")
			EnableMenuItem(GetSubMenu(GetMenu(hDlg), 0), ID_STANDARD_FILE_OPEN, MF_GRAYED);
			EnableMenuItem(GetSubMenu(GetMenu(hDlg), 0), ID_STANDARD_FILE_SAVE, MF_GRAYED);
			EnableMenuItem(GetSubMenu(GetMenu(hDlg), 0), ID_STANDARD_FILE_SAVE_AS, MF_GRAYED);
			break;
		}
#endif
		// Populate the GUI with the values in the mbs.
		return TRUE;



	case WM_COMMAND:
		wmId    = LOWORD(wParam); 
		wmEvent = HIWORD(wParam); 

		switch(wmId)
		{
		case IDOK:
		case 2: // find out what this message is named.... It's the x at the upper right side of the screen.
		case ID_STANDARD_FILE_CLOSE:

			// Copy information the user may have entered via the GUI 
			// into the MBS_SPECIES structure
			GuiWindowsToSpecies(&sm, hDlg);

			// Override needing save for now until the code can be improved.
			sm.m_needSave = FALSE;
			if(sm.m_needSave == FALSE)
			{
				EndDialog(hDlg, LOWORD(wParam));
				return TRUE;
			}

			// Commented out 11/2/09
			/*
			if(GetSaveFileName(&fiBin.ofn) != 0)
			{
				GuiWindowsToSpecies(&sm, hDlg);

				// Commented out 11/2/09
//				res = sm.SaveToBinFile(fiBin.ofn.lpstrFile);
//				if(res != OK)
//					IOMessageBox(res, hDlg, fiBin.ofn.lpstrFileTitle);
				SpeciesToGuiWindows(&sm, hDlg);
				EndDialog(hDlg, LOWORD(wParam));
				return TRUE;
			}
			*/
			break;

		case IDC_BUTTON_BEH_VECTOR:
		case IDC_BUTTON_DIRECTION_VECTOR:
#if 0
			vmdlParam.modelType = wmId;
			vmdlParam.pSpeciesModel = &sm;
			DialogBoxParam(NULL, MAKEINTRESOURCE (IDD_DIALOG_2MATRICES1VECTOR), hDlg,
				(DLGPROC) SpeciesVectorModelProc, (LPARAM) &vmdlParam);
			SpeciesToGuiWindows(&sm, hDlg);
#endif
			break;

		case IDC_BUTTON_DIVEDEPTH_VECTOR:
		case IDC_BUTTON_SURFINTVL_VECTOR:
#if 0
			vmdlParam.modelType = wmId;
			vmdlParam.pSpeciesModel = &sm;
			DialogBoxParam(NULL, MAKEINTRESOURCE (IDD_DIALOG_1MATRIX1ELEMENT), hDlg,
				(DLGPROC) SpeciesVectorModelProc, (LPARAM) &vmdlParam);
			SpeciesToGuiWindows(&sm, hDlg);
#endif
			break;

		case IDC_BUTTON_TRAVEL_VECTOR:
		case IDC_BUTTON_DIVERATE_VECTOR:
		case IDC_BUTTON_ACENTRATE_VECTOR:
#if 0
			vmdlParam.modelType = wmId;
			vmdlParam.pSpeciesModel = &sm;
			DialogBoxParam(NULL, MAKEINTRESOURCE (IDD_DIALOG_1MATRIX1ELEMENT1VECTOR), hDlg,
				(DLGPROC) SpeciesVectorModelProc, (LPARAM) &vmdlParam);
			SpeciesToGuiWindows(&sm, hDlg);
#endif
			break;

		case IDC_BUTTON_REVERSAL_VECTOR:
#if 0
			vmdlParam.modelType = wmId;
			vmdlParam.pSpeciesModel = &sm;
			DialogBoxParam(NULL, MAKEINTRESOURCE (IDD_DIALOG_MATRIXVECTORMATRXELEMENT), hDlg,
				(DLGPROC) SpeciesVectorModelProc, (LPARAM) &vmdlParam);
			SpeciesToGuiWindows(&sm, hDlg);
#endif
			break;



		// The names of behaviors list box
		case IDC_LIST_VECTOR_MODEL_BEHAV_NAMES:
#if 0
			switch(wmEvent)
			{
			case LBN_DBLCLK:
				// Get cursor positions.
				hwnd = GetDlgItem(hDlg, IDC_LIST_VECTOR_MODEL_BEHAV_NAMES);
				cursor = (int)SendMessage(hwnd, LB_GETCURSEL, 0,0);
				strncpy_s(bm.sz, sizeof(bm.sz), sm.m_behaviorNamesArray_TOBEREMOVED[cursor].sz, SZ_BEHAVIOR_LEN-1);
				dlgRes = DialogBoxParam(NULL, MAKEINTRESOURCE(IDD_ENTER_BEHAV_NAME), hDlg,
							(DLGPROC)GetBehaviorNameString, (LPARAM)&bm);

				if(dlgRes == IDOK)
				{
					strncpy_s(sm.m_behaviorNamesArray_TOBEREMOVED[cursor].sz, sizeof(sm.m_behaviorNamesArray_TOBEREMOVED[cursor].sz), bm.sz, SZ_BEHAVIOR_LEN-1);
					SendMessage(hwnd, LB_DELETESTRING, cursor, (LPARAM)NULL);
					SendMessage(hwnd, LB_INSERTSTRING, cursor, (LPARAM)bm.sz);
				}				
				break;
			}
#endif
			break;

		case ID_STANDARD_FILE_OPEN:
			// Copy information the user may have entered via the GUI 
			// into the MBS_SPECIES structure
			//GuiWindowsToSpecies(&sm, hDlg);

			// Commented out 11/2/09
			//if(GetOpenFileName(&fiBin.ofn) != 0)
			//{
#if 0
				hwnd = GetDlgItem(hDlg, IDC_LIST_VECTOR_MODEL_BEHAV_NAMES);
				for(i=0; i<sm.m_behaviorModel_TOBEREMOVED.behavior.rowCnt; i++)
					SendMessage(hwnd, LB_DELETESTRING, 0, (LPARAM)0);
#endif
								// Commented out 11/2/09
				//sm.LoadFromBinFile(fiBin.ofn.lpstrFile);
#if 0
				hwnd = GetDlgItem(hDlg, IDC_LIST_VECTOR_MODEL_BEHAV_NAMES);
				for(i=0; i<sm.m_behaviorModel_TOBEREMOVED.behavior.rowCnt; i++)
					SendMessage(hwnd, LB_ADDSTRING, i, (LPARAM)sm.m_behaviorNamesArray_TOBEREMOVED[i].sz);

				SpeciesToGuiWindows(&sm, hDlg);
#endif
			//}
			break;


		case ID_STANDARD_FILE_SAVE:
							// Commented out 11/2/09
			/*
			if(GetSaveFileName (&fiBin.ofn) != NULL)
			{
				GuiWindowsToSpecies(&sm, hDlg);
				res = sm.SaveToBinFile(fiBin.ofn.lpstrFile);
				if(res != OK)
					IOMessageBox(res, hDlg, fiBin.ofn.lpstrFileTitle);
				SpeciesToGuiWindows(&sm, hDlg);
			}
			*/
			break;

		case ID_STANDARD_FILE_SAVE_AS:
				// Commented out 11/2/09
			/*
			if(GetSaveFileName (&fiBin.ofn) != NULL)
			{
				GuiWindowsToSpecies(&sm, hDlg);
				res = sm.SaveToBinFile(fiBin.ofn.lpstrFile);
				if(res != OK)
					IOMessageBox(res, hDlg, fiBin.ofn.lpstrFileTitle);
				SpeciesToGuiWindows(&sm, hDlg);
			}
			*/
			break;

		case IDC_SEL_USER_MODEL:
			// User selected to load a user model file.  Prompt the user for a user model
			// to load.

			// Pop up a dialog box, read in the user model vector, hold the name of
			// the user model text file.

			// Commented out 11/2/09
			/*
			if(GetOpenFileName(&fiText.ofn) != 0)
			{

				res = sm.LoadFromFile(fiText.ofn.lpstrFile);
				if(res != OK)
				{
					IOMessageBox(res, hDlg, fiText.ofn.lpstrFile);
					break;
				}


				//----------------------------------------------------------------------//
				// To avoid many string/character buffer handling every time
				// SpeciesToGuiWindows() is called, perform such handling when function
				// calls are made that affect the buffer contents
				//----------------------------------------------------------------------//
				// m_behaviorModel_TOBEREMOVED

				// m_travelModel_TOBEREMOVED

				// m_directionModel_TOBEREMOVED

				// m_depthModel_TOBEREMOVED


				//----------------------------------------------------------------------//
				// When loading in the species models, reversals needs to be set to
				// TRUE/yes here instead of SpeciesToGuiWindows() if a reveral model was
				// loaded in becaues SpeciesToGuiWindows() won't know that models were
				// newly loaded in
				//----------------//
#if 0
				if(sm.m_reversalModel_TOBEREMOVED.modelType == VECTOR && sm.GetSubModelVectorStatus(&sm.m_reversalModel_TOBEREMOVED) == VALID)
				{
					sm.m_reversalModel_TOBEREMOVED.reverses = TRUE;
					//sm.MatrixToText(&sm.m_reversalModel_TOBEREMOVED.vm.count, szBuff, sizeof(szBuff));
					//SetDlgItemText(hDlg, IDC_EDIT_REVERSAL_VECTOR, szBuff);
				}

				SpeciesToGuiWindows(&sm, hDlg);
				hwnd = GetDlgItem(hDlg, IDC_LIST_VECTOR_MODEL_BEHAV_NAMES);
				for(i=0; i<sm.m_behaviorModel_TOBEREMOVED.behavior.rowCnt; i++)
					SendMessage(hwnd, LB_ADDSTRING, i, (LPARAM)sm.m_behaviorNamesArray_TOBEREMOVED[i].sz);
#endif
			}
			*/
			break;

		case IDC_SEL_SKIP_USER_MODEL:
#if 0
			// User selected to skip user model file, so update the GUI and leave the
			// CSpeciesModel with it's default values.
			SpeciesToGuiWindows(&sm, hDlg);
#endif
			break;

		case IDC_USER_MODEL_CLEAR:
#if 0
			// User decided to clear out the model. Reset the CSpeciesModel instance
			// to default values and indicate again that the user needs to make an 
			// initial selection by making the GUI window not ready for editing (disable
			// most of the GUI.
			hwnd = GetDlgItem(hDlg, IDC_LIST_VECTOR_MODEL_BEHAV_NAMES);
			for(i=0; i<sm.m_behaviorModel_TOBEREMOVED.behavior.rowCnt; i++)
				SendMessage(hwnd, LB_DELETESTRING, 0, (LPARAM)0);

			SpeciesToGuiWindows(&sm, hDlg);
#endif
			break;


		case IDC_USER_MODEL_SAVE_TO_TEXT:
				// Commented out 11/2/09
			/*
			// User wants the current matrix models saved to a txt file.
			if(!GetSaveFileName(&fiText.ofn))
				break;

			GuiWindowsToSpecies(&sm, hDlg);
//			sm.SetName(fiText.ofn.lpstrFileTitle);
			sm.ModelToText(fiText.ofn.lpstrFile);
			sprintf_s(szBuffer, sizeof(szBuff), "User model saved to file %s", fiText.szFileName);
			MessageBox(hDlg, szBuffer, "File Saved", MB_OK);
			*/
			break;

#if 0
		case IDC_RADIO_DIVEDEPTH1:
			// Copy information the user may have entered via the GUI 
			// into the MBS_SPECIES structure
			GuiWindowsToSpecies(&sm, hDlg);
			sm.m_depthModel_TOBEREMOVED.modelType = UNIFORM;
			SpeciesToGuiWindows(&sm, hDlg);
			sm.m_needSave = TRUE;
			break;

		case IDC_RADIO_DIVEDEPTH2:
			// Copy information the user may have entered via the GUI 
			// into the MBS_SPECIES structure
			GuiWindowsToSpecies(&sm, hDlg);

			sm.m_depthModel_TOBEREMOVED.modelType = GAUSSIAN;
			SpeciesToGuiWindows(&sm, hDlg);
			sm.m_needSave = TRUE;
			break;

		case IDC_RADIO_DIVEDEPTH3:
			// Copy information the user may have entered via the GUI 
			// into the MBS_SPECIES structure
			GuiWindowsToSpecies(&sm, hDlg);

			sm.m_depthModel_TOBEREMOVED.modelType = VECTOR;
			SpeciesToGuiWindows(&sm, hDlg);
			sm.m_needSave = TRUE;
			break;


		case IDC_RADIO_AE_PODRES_STAYPOD:
			GuiWindowsToSpecies(&sm, hDlg);
			sm.m_ae_TOBEREMOVED.podResponse = REMAINS_INTACT_goesAway;
			SpeciesToGuiWindows(&sm, hDlg);
			sm.m_needSave = TRUE;
			break;

		case IDC_RADIO_AE_PODRES_BREAKUP:
			GuiWindowsToSpecies(&sm, hDlg);
			sm.m_ae_TOBEREMOVED.podResponse = BREAKS_APART_goesAway;
			SpeciesToGuiWindows(&sm, hDlg);
			sm.m_needSave = TRUE;
			break;
#endif
		//---------------------------//
		// Accoustic Exposure Options
		//---------------------------//
		case IDC_CHECK_AE_TRAVEL:
#if 0
			// Copy information the user may have entered via the GUI 
			// into the MBS_SPECIES structure
			GuiWindowsToSpecies(&sm, hDlg);
			sm.m_ae_TOBEREMOVED.travel = !sm.m_ae_TOBEREMOVED.travel;
			SpeciesToGuiWindows(&sm, hDlg);
			sm.m_needSave = TRUE;
#endif
			break;

		case IDC_CHECK_AE_DIRECTION:
#if 0
			// Copy information the user may have entered via the GUI 
			// into the MBS_SPECIES structure
			GuiWindowsToSpecies(&sm, hDlg);
			sm.m_ae_TOBEREMOVED.direction = !sm.m_ae_TOBEREMOVED.direction;
			SpeciesToGuiWindows(&sm, hDlg);
			sm.m_needSave = TRUE;
#endif
			break;

		case IDC_CHECK_AE_DIVING:
#if 0
			// Copy information the user may have entered via the GUI 
			// into the MBS_SPECIES structure
			GuiWindowsToSpecies(&sm, hDlg);
			sm.m_ae_TOBEREMOVED.diving = !sm.m_ae_TOBEREMOVED.diving;

			if(sm.m_ae_TOBEREMOVED.diving == TRUE)
				sm.m_depthModel_TOBEREMOVED.bottomFollows = FALSE;

			SpeciesToGuiWindows(&sm, hDlg);
			sm.m_needSave = TRUE;
#endif
			break;

		case IDC_CHECK_CUMMULATIVE_EXP:
#if 0
			// Copy information the user may have entered via the GUI 
			// into the MBS_SPECIES structure
			GuiWindowsToSpecies(&sm, hDlg);
			sm.m_ae_TOBEREMOVED.cummulativeExposure = !sm.m_ae_TOBEREMOVED.cummulativeExposure;
			SpeciesToGuiWindows(&sm, hDlg);
			sm.m_needSave = TRUE;
#endif
			break;

		case IDC_CHECK_ANIMATBEACHES:
#if 0
			// Copy information the user may have entered via the GUI 
			// into the MBS_SPECIES structure
			GuiWindowsToSpecies(&sm, hDlg);
			sm.m_ae_TOBEREMOVED.bAnimatBeaches = !sm.m_ae_TOBEREMOVED.bAnimatBeaches;
			SpeciesToGuiWindows(&sm, hDlg);
			sm.m_needSave = TRUE;
#endif
			break;

		// Radio Buttons, Dive Rate
		case IDC_RADIO_DIVERATE1:
#if 0
			// Copy information the user may have entered via the GUI 
			// into the MBS_SPECIES structure
			GuiWindowsToSpecies(&sm, hDlg);

			sm.m_diveRateModel_TOBEREMOVED.modelType = UNIFORM;
			SpeciesToGuiWindows(&sm, hDlg);
			sm.m_needSave = TRUE;
#endif
			break;

		case IDC_RADIO_DIVERATE2:
#if 0
			// Copy information the user may have entered via the GUI 
			// into the MBS_SPECIES structure
			GuiWindowsToSpecies(&sm, hDlg);

			sm.m_diveRateModel_TOBEREMOVED.modelType = GAUSSIAN;
			SpeciesToGuiWindows(&sm, hDlg);
			sm.m_needSave = TRUE;
#endif
			break;

		case IDC_RADIO_DIVERATE3:
#if 0
			// Copy information the user may have entered via the GUI 
			// into the MBS_SPECIES structure
			GuiWindowsToSpecies(&sm, hDlg);

			sm.m_diveRateModel_TOBEREMOVED.modelType = VECTOR;
			SpeciesToGuiWindows(&sm, hDlg);
			sm.m_needSave = TRUE;
#endif
			break;


		// Radio Buttons, Ascent Rate
		case IDC_RADIO_ACENTRATE1:
#if 0
			// Copy information the user may have entered via the GUI 
			// into the MBS_SPECIES structure
			GuiWindowsToSpecies(&sm, hDlg);

			sm.m_ascentRateModel_TOBEREMOVED.modelType = UNIFORM;
			SpeciesToGuiWindows(&sm, hDlg);
			sm.m_needSave = TRUE;
#endif
			break;

		case IDC_RADIO_ACENTRATE2:
#if 0
			// Copy information the user may have entered via the GUI 
			// into the MBS_SPECIES structure
			GuiWindowsToSpecies(&sm, hDlg);

			sm.m_ascentRateModel_TOBEREMOVED.modelType = GAUSSIAN;
			SpeciesToGuiWindows(&sm, hDlg);
			sm.m_needSave = TRUE;
#endif
			break;

		case IDC_RADIO_ACENTRATE3:
#if 0
			// Copy information the user may have entered via the GUI 
			// into the MBS_SPECIES structure
			GuiWindowsToSpecies(&sm, hDlg);

			sm.m_ascentRateModel_TOBEREMOVED.modelType = VECTOR;
			SpeciesToGuiWindows(&sm, hDlg);
			sm.m_needSave = TRUE;
#endif
			break;

		// Radio Buttons, travel rate
		case IDC_RADIO_TRAVEL1:
#if 0
			GuiWindowsToSpecies(&sm, hDlg);
			// Copy information the user may have entered via the GUI 
			// into the MBS_SPECIES structure

			sm.m_travelModel_TOBEREMOVED.modelType = UNIFORM;
			SpeciesToGuiWindows(&sm, hDlg);
			sm.m_needSave = TRUE;
#endif
			break;

		case IDC_RADIO_TRAVEL2:
#if 0
			// Copy information the user may have entered via the GUI 
			// into the MBS_SPECIES structure
			GuiWindowsToSpecies(&sm, hDlg);

			sm.m_travelModel_TOBEREMOVED.modelType = GAUSSIAN;
			SpeciesToGuiWindows(&sm, hDlg);
			sm.m_needSave = TRUE;
#endif
			break;

		case IDC_RADIO_TRAVEL3:
#if 0
			// Copy information the user may have entered via the GUI 
			// into the MBS_SPECIES structure
			GuiWindowsToSpecies(&sm, hDlg);

			sm.m_travelModel_TOBEREMOVED.modelType = VECTOR;
			SpeciesToGuiWindows(&sm, hDlg);
			sm.m_needSave = TRUE;
#endif
			break;


		// Radio Buttons, Direction Change
		case IDC_RADIO_DIRECTION_CHANGE1:
#if 0
			// Copy information the user may have entered via the GUI 
			// into the MBS_SPECIES structure
			GuiWindowsToSpecies(&sm, hDlg);
			sm.m_directionModel_TOBEREMOVED.modelType = RANDOM_WALK;
			SpeciesToGuiWindows(&sm, hDlg);
			sm.m_needSave = TRUE;
#endif
			break;

		case IDC_RADIO_DIRECTION_CHANGE2:
			// Copy information the user may have entered via the GUI 
			// into the MBS_SPECIES structure
#if 0
			GuiWindowsToSpecies(&sm, hDlg);
			sm.m_directionModel_TOBEREMOVED.modelType = CORRELATED_RANDOM_WALK;
			SpeciesToGuiWindows(&sm, hDlg);
			sm.m_needSave = TRUE;
#endif
			break;

		case IDC_RADIO_DIRECTION_CHANGE3:
#if 0
			// Copy information the user may have entered via the GUI 
			// into the MBS_SPECIES structure
			GuiWindowsToSpecies(&sm, hDlg);
			sm.m_directionModel_TOBEREMOVED.modelType = CORRELATED_RANDOM_WALK_WITH_DIR_BIASING;
			SpeciesToGuiWindows(&sm, hDlg);
			sm.m_needSave = TRUE;
#endif
			break;

		case IDC_RADIO_DIRECTION_CHANGE4:
			// Copy information the user may have entered via the GUI 
			// into the MBS_SPECIES structure
#if 0
			GuiWindowsToSpecies(&sm, hDlg);
			sm.m_directionModel_TOBEREMOVED.modelType = VECTOR_MODEL_DIRECTIONAL_NO_BIASING;
			SpeciesToGuiWindows(&sm, hDlg);
			sm.m_needSave = TRUE;
#endif
			break;

		case IDC_RADIO_DIRECTION_CHANGE5:
#if 0
			// Copy information the user may have entered via the GUI 
			// into the MBS_SPECIES structure
			GuiWindowsToSpecies(&sm, hDlg);
			sm.m_directionModel_TOBEREMOVED.modelType = VECTOR_MODEL_DIRECTIONAL_WITH_VECTOR_MODEL_BIASING;
			SpeciesToGuiWindows(&sm, hDlg);
			sm.m_needSave = TRUE;
#endif
			break;

		// Radio Buttons, Reversal
		case IDC_RADIO_REVERSAL1: // yes
#if 0
			// Copy information the user may have entered via the GUI 
			// into the MBS_SPECIES structure
			GuiWindowsToSpecies(&sm, hDlg);

			sm.m_reversalModel_TOBEREMOVED.reverses = TRUE;
			SpeciesToGuiWindows(&sm, hDlg);
			sm.m_needSave = TRUE;
#endif
			break;

		case IDC_RADIO_REVERSAL2: // no (default)
#if 0
			// Copy information the user may have entered via the GUI 
			// into the MBS_SPECIES structure
			GuiWindowsToSpecies(&sm, hDlg);

			sm.m_reversalModel_TOBEREMOVED.reverses = FALSE;
			SpeciesToGuiWindows(&sm, hDlg);
			sm.m_needSave = TRUE;
#endif
			break;

		case IDC_RADIO_REVERSAL3:
#if 0
			// Copy information the user may have entered via the GUI 
			// into the MBS_SPECIES structure
			GuiWindowsToSpecies(&sm, hDlg);
			sm.m_reversalModel_TOBEREMOVED.modelType = UNIFORM;
			SpeciesToGuiWindows(&sm, hDlg);
			sm.m_needSave = TRUE;
#endif
			break;

		case IDC_RADIO_REVERSAL4:
#if 0
			// Copy information the user may have entered via the GUI 
			// into the MBS_SPECIES structure
			GuiWindowsToSpecies(&sm, hDlg);
			sm.m_reversalModel_TOBEREMOVED.modelType = GAUSSIAN;
			SpeciesToGuiWindows(&sm, hDlg);
			sm.m_needSave = TRUE;
#endif
			break;

		case IDC_RADIO_REVERSAL5:
#if 0
			// Copy information the user may have entered via the GUI 
			// into the MBS_SPECIES structure
			GuiWindowsToSpecies(&sm, hDlg);
			sm.m_reversalModel_TOBEREMOVED.modelType = VECTOR;
			SpeciesToGuiWindows(&sm, hDlg);
			sm.m_needSave = TRUE;
#endif
			break;


		// Radio Buttons, Flat Bottom Dive
		case IDC_RADIO_SURFINTVL1: // yes
#if 0
			// Copy information the user may have entered via the GUI 
			// into the MBS_SPECIES structure
			GuiWindowsToSpecies(&sm, hDlg);
			sm.m_surfaceIntervalModel_TOBEREMOVED.modelType = GAUSSIAN;
			SpeciesToGuiWindows(&sm, hDlg);
			sm.m_needSave = TRUE;
#endif
			break;

		case IDC_RADIO_SURFINTVL2: // no
#if 0

			// Copy information the user may have entered via the GUI 
			// into the MBS_SPECIES structure
			GuiWindowsToSpecies(&sm, hDlg);
			sm.m_surfaceIntervalModel_TOBEREMOVED.modelType = VECTOR;
			SpeciesToGuiWindows(&sm, hDlg);
			sm.m_needSave = TRUE;
#endif
			break;


		// Radio Buttons, Flat Bottom Dive
		case IDC_RADIO_FLAT_BOTTOM_DIVE1: // yes
#if 0

			// Copy information the user may have entered via the GUI 
			// into the MBS_SPECIES structure
			GuiWindowsToSpecies(&sm, hDlg);
			sm.m_depthModel_TOBEREMOVED.bottomFollows = TRUE;
			SpeciesToGuiWindows(&sm, hDlg);
			sm.m_needSave = TRUE;
			break;
#endif
		case IDC_RADIO_FLAT_BOTTOM_DIVE2: // no
#if 0
			// Copy information the user may have entered via the GUI 
			// into the MBS_SPECIES structure
			GuiWindowsToSpecies(&sm, hDlg);
			sm.m_depthModel_TOBEREMOVED.bottomFollows = FALSE;
			SpeciesToGuiWindows(&sm, hDlg);
			sm.m_needSave = TRUE;
#endif
			break;
		}
		break;
	}
    return FALSE;
}




/*----------------------------------------------------------------------------------------
	Updates the GUI Species GUI window.
/*--------------------------------------------------------------------------------------*/
void SpeciesToGuiWindows(CSpeciesModel *sm, HWND par)
{
	// This Function not currently not being used.

	par = par; // quiet compiler warning.
	sm = sm;

#if 0
	TCHAR szBuff[BUFFERED_MAX_PATH];
	TCHAR *szMean		  = TEXT("mean");
	TCHAR *szMin		  = TEXT("min");
	TCHAR *szMax		  = TEXT("max");
	TCHAR *szStd		  = TEXT("std");
	TCHAR *szCoeff		  = TEXT("coeff");
	TCHAR *szBlank		  = TEXT(" ");
	TCHAR *szProb		  = TEXT("prob");

	TCHAR *szArcStep	  = TEXT("arc step");
//	TCHAR *szStep	  = TEXT("step");
//	TCHAR *szValue	  = TEXT("value");
	TCHAR *szPerturbation1 = TEXT("pertur-");
	TCHAR *szPerturbation2 = TEXT("bation");
	TCHAR *szDirection	   = TEXT("direction");
	TCHAR *szBias		   = TEXT("bias");

	TCHAR *szTime  = TEXT("time");
	TCHAR *szCount  = TEXT("count");
	HWND hwnd;


	//----------------------------------------------------------------------------------//
	// Behavior Modeling
	//------------------//
	// Text displayed in the edit box about the status of the vector models.  If the both
	// the a vector model is present and it is valid, display a summary of the contents
	// found in the vector model.  Otherwise, display dashes.
#if 0
	if(sm->m_behaviorModel_TOBEREMOVED.modelType == VECTOR_MODEL && sm->GetSubModelVectorStatus(&sm->m_behaviorModel_TOBEREMOVED) == VALID)
	{
		sprintf_s(szBuff, sizeof(szBuff), "%s = [%dx%d]%s%s = [%dx%d]%s%s = [%dx%d]",
			SZ_BEH, sm->m_behaviorModel_TOBEREMOVED.behavior.rowCnt, sm->m_behaviorModel_TOBEREMOVED.behavior.colCnt, CRLF,
			SZ_BEH_INIT, sm->m_behaviorModel_TOBEREMOVED.initial.rowCnt, sm->m_behaviorModel_TOBEREMOVED.initial.colCnt, CRLF,
			SZ_BEH_TERM, sm->m_behaviorModel_TOBEREMOVED.terminate.rowCnt, sm->m_behaviorModel_TOBEREMOVED.terminate.colCnt);
	}
	else
	{
		sprintf_s(szBuff, sizeof(szBuff), "%s = [----]%s%s = [----]%s%s = [----]", SZ_BEH, CRLF, 
			SZ_BEH_INIT, CRLF, SZ_BEH_TERM);
	}
#endif
	SetDlgItemText(par, IDC_EDIT_BEH_VECTOR, szBuff);
	

	// Enable or disable dialog dialog controls related to behavior based upon if the model   Behavior is only
	// modeled through vector models, so if there is any vector model 
#if 0
	if(sm->m_behaviorModel_TOBEREMOVED.modelType == VECTOR_MODEL)
	{
		// model type can only be set to vector if the user clicks on radio box or loads
		// a model in.
		EnableWindow(GetDlgItem(par, IDC_STATIC_VMBEHAVIOR), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_BEH_VECTOR), TRUE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_BEHNAMES), TRUE);
		EnableWindow(GetDlgItem(par, IDC_LIST_VECTOR_MODEL_BEHAV_NAMES), TRUE);
	}
	else
	{
		EnableWindow(GetDlgItem(par, IDC_STATIC_VMBEHAVIOR), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_BEH_VECTOR), FALSE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_BEHNAMES), FALSE);
		EnableWindow(GetDlgItem(par, IDC_LIST_VECTOR_MODEL_BEHAV_NAMES), FALSE);
	}
#endif









	//----------------------------------------------------------------------------------//
	// Depth
	//------------------------//
	if(sm->GetSubModelVectorStatus(&sm->m_depthModel_TOBEREMOVED) == VALID)
	{
		EnableWindow(GetDlgItem(par, IDC_RADIO_DIVEDEPTH3), TRUE);
	}
	else
	{
		EnableWindow(GetDlgItem(par, IDC_RADIO_DIVEDEPTH3), FALSE);
	}
	if(sm->m_depthModel_TOBEREMOVED.modelType == VECTOR && sm->GetSubModelVectorStatus(&sm->m_depthModel_TOBEREMOVED) == VALID)
	{
		sprintf_s(szBuff, sizeof(szBuff), "%s = [%dx%d]%s%s = [%dx%d]",
			SZ_DEPTH, sm->m_depthModel_TOBEREMOVED.vm.vector.rowCnt, sm->m_depthModel_TOBEREMOVED.vm.vector.colCnt, CRLF,
			SZ_DEPTH_STEP, sm->m_depthModel_TOBEREMOVED.vm.step.rowCnt, sm->m_depthModel_TOBEREMOVED.vm.step.colCnt);
	}
	else
	{
		sprintf_s(szBuff, sizeof(szBuff), "%s = [----]%s%s = [----]", SZ_DEPTH, CRLF, SZ_DEPTH_STEP, CRLF);
	}
	SetDlgItemText(par, IDC_EDIT_DIVEDEPTHL_VECTOR, szBuff);


	//-----------------------------------------------------------------------------------//
	// Surface Interval
	//---------------------------------//
	if(sm->GetSubModelVectorStatus(&sm->m_surfaceIntervalModel_TOBEREMOVED) == VALID)
	{
		EnableWindow(GetDlgItem(par, IDC_RADIO_SURFINTVL2), TRUE);
	}
	else
	{
		EnableWindow(GetDlgItem(par, IDC_RADIO_SURFINTVL2), FALSE);
	}
	if(sm->m_surfaceIntervalModel_TOBEREMOVED.modelType == VECTOR
		&& sm->GetSubModelVectorStatus(&sm->m_surfaceIntervalModel_TOBEREMOVED) == VALID)
	{
		sprintf_s(szBuff, sizeof(szBuff), "%s = [%dx%d]%s%s = [%dx%d]",
			SZ_SI, sm->m_surfaceIntervalModel_TOBEREMOVED.vm.vector.rowCnt, sm->m_surfaceIntervalModel_TOBEREMOVED.vm.vector.colCnt, CRLF, 
			SZ_SI_STEP,  sm->m_surfaceIntervalModel_TOBEREMOVED.vm.step.rowCnt, sm->m_surfaceIntervalModel_TOBEREMOVED.vm.step.colCnt, CRLF);
	}
	else
	{
		sprintf_s(szBuff, sizeof(szBuff), "%s = [----]%s%s = [----]", SZ_SI, CRLF, SZ_SI_STEP, CRLF);
	}
	SetDlgItemText(par, IDC_EDIT_SURINTVAL_VECTOR, szBuff);

	//-----------------------------------------------------------------------------------//
	// Travel
	//---------------------------------//
	if(sm->GetSubModelVectorStatus(&sm->m_travelModel_TOBEREMOVED) == VALID)
	{
		EnableWindow(GetDlgItem(par, IDC_RADIO_TRAVEL3), TRUE);
	}
	else
	{
		EnableWindow(GetDlgItem(par, IDC_RADIO_TRAVEL3), FALSE);
	}
	if(sm->m_travelModel_TOBEREMOVED.modelType == VECTOR && sm->GetSubModelVectorStatus(&sm->m_travelModel_TOBEREMOVED) == VALID)
	{
		sprintf_s(szBuff, sizeof(szBuff), "%s = [%dx%d]%s%s = [%dx%d]%s%s = [%dx%d]",
			SZ_ROT, sm->m_travelModel_TOBEREMOVED.vm.vector.rowCnt, sm->m_travelModel_TOBEREMOVED.vm.vector.colCnt, CRLF, 
			SZ_ROT_STEP,  sm->m_travelModel_TOBEREMOVED.vm.step.rowCnt, sm->m_travelModel_TOBEREMOVED.vm.step.colCnt, CRLF,
			SZ_ROT_TERM, sm->m_travelModel_TOBEREMOVED.vm.terminate.rowCnt, sm->m_travelModel_TOBEREMOVED.vm.terminate.colCnt);
	}
	else
	{
		sprintf_s(szBuff, sizeof(szBuff), "%s = [----]%s%s = [----]%s%s = [----]", SZ_ROT, CRLF, SZ_ROT_STEP, CRLF, SZ_ROT_TERM);
	}
	SetDlgItemText(par, IDC_EDIT_TRAVEL_VECTOR, szBuff);


	//-----------------------------------------------------------------------------------//
	// Rate of decent
	//---------------------------------//
	if(sm->GetSubModelVectorStatus(&sm->m_diveRateModel_TOBEREMOVED) == VALID)
	{
		EnableWindow(GetDlgItem(par, IDC_RADIO_DIVERATE3), TRUE);
	}
	else
	{
		EnableWindow(GetDlgItem(par, IDC_RADIO_DIVERATE3), FALSE);
	}
	if(sm->m_diveRateModel_TOBEREMOVED.modelType == VECTOR && sm->GetSubModelVectorStatus(&sm->m_diveRateModel_TOBEREMOVED) == VALID)
	{
		sprintf_s(szBuff, sizeof(szBuff), "%s = [%dx%d]%s%s = [%dx%d]%s%s = [%dx%d]",
			SZ_ROT, sm->m_diveRateModel_TOBEREMOVED.vm.vector.rowCnt, sm->m_diveRateModel_TOBEREMOVED.vm.vector.colCnt, CRLF, 
			SZ_ROT_STEP,  sm->m_diveRateModel_TOBEREMOVED.vm.step.rowCnt, sm->m_diveRateModel_TOBEREMOVED.vm.step.colCnt, CRLF,
			SZ_ROT_TERM, sm->m_diveRateModel_TOBEREMOVED.vm.terminate.rowCnt, sm->m_diveRateModel_TOBEREMOVED.vm.terminate.colCnt);
	}
	else
	{
		sprintf_s(szBuff, sizeof(szBuff), "%s = [----]%s%s = [----]%s%s = [----]", SZ_ROD, CRLF, SZ_ROD_STEP, CRLF, SZ_ROD_TERM);
	}
	SetDlgItemText(par, IDC_EDIT_DIVERATE_VECTOR, szBuff);


	//-----------------------------------------------------------------------------------//
	// Rate of Ascent
	//---------------------------------//
	if(sm->GetSubModelVectorStatus(&sm->m_ascentRateModel_TOBEREMOVED) == VALID)
	{
		EnableWindow(GetDlgItem(par, IDC_RADIO_ACENTRATE3), TRUE);
	}
	else
	{
		EnableWindow(GetDlgItem(par, IDC_RADIO_ACENTRATE3), FALSE);
	}
	if(sm->m_ascentRateModel_TOBEREMOVED.modelType == VECTOR && sm->GetSubModelVectorStatus(&sm->m_ascentRateModel_TOBEREMOVED) == VALID)
	{
		sprintf_s(szBuff, sizeof(szBuff), "%s = [%dx%d]%s%s = [%dx%d]%s%s = [%dx%d]",
			SZ_ROT, sm->m_ascentRateModel_TOBEREMOVED.vm.vector.rowCnt, sm->m_ascentRateModel_TOBEREMOVED.vm.vector.colCnt, CRLF, 
			SZ_ROT_STEP,  sm->m_ascentRateModel_TOBEREMOVED.vm.step.rowCnt, sm->m_ascentRateModel_TOBEREMOVED.vm.step.colCnt, CRLF,
			SZ_ROT_TERM, sm->m_ascentRateModel_TOBEREMOVED.vm.terminate.rowCnt, sm->m_ascentRateModel_TOBEREMOVED.vm.terminate.colCnt);
	}
	else
	{
		sprintf_s(szBuff, sizeof(szBuff), "%s = [----]%s%s = [----]%s%s = [----]", SZ_ROA, CRLF, SZ_ROA_STEP, CRLF, SZ_ROA_TERM);
	}
	SetDlgItemText(par, IDC_EDIT_ACENTRATE_VECTOR, szBuff);


	//-----------------------------------------------------------------------------------//
	// Direction
	// SZ_DIR, SZ_DIR_BIAS, SZ_DIR_TERM
	//---------------------------------//
	if(sm->GetSubModelVectorStatus(&sm->m_directionModel_TOBEREMOVED) == VALID)
	{
		EnableWindow(GetDlgItem(par, IDC_RADIO_DIRECTION_CHANGE4), TRUE);
		if(sm->m_directionModel_TOBEREMOVED.vm.directionalBias.a != NULL && sm->m_directionModel_TOBEREMOVED.vm.directionalBias.colCnt > 0 
			&& sm->m_directionModel_TOBEREMOVED.vm.directionalBias.rowCnt > 0)
		{
			EnableWindow(GetDlgItem(par, IDC_RADIO_DIRECTION_CHANGE5), TRUE);
		}
		else
		{
			EnableWindow(GetDlgItem(par, IDC_RADIO_DIRECTION_CHANGE5), FALSE);
		}
	}
	else
	{
		EnableWindow(GetDlgItem(par, IDC_RADIO_DIRECTION_CHANGE4), FALSE);
		EnableWindow(GetDlgItem(par, IDC_RADIO_DIRECTION_CHANGE5), FALSE);
	}

	if(sm->m_directionModel_TOBEREMOVED.modelType == VECTOR_MODEL_DIRECTIONAL_NO_BIASING &&
		 sm->GetSubModelVectorStatus(&sm->m_directionModel_TOBEREMOVED) == VALID)
	{
		sprintf_s(szBuff, sizeof(szBuff), "%s = [%dx%d]%s%s = [----]%s%s = [%dx%d]",
			SZ_DIR, sm->m_directionModel_TOBEREMOVED.vm.direction.rowCnt, sm->m_directionModel_TOBEREMOVED.vm.direction.colCnt,
			CRLF, SZ_DIR_BIAS, CRLF,
			SZ_DIR_TERM, sm->m_directionModel_TOBEREMOVED.vm.terminate.rowCnt, sm->m_directionModel_TOBEREMOVED.vm.terminate.colCnt);

	}
	else if(sm->m_directionModel_TOBEREMOVED.modelType == VECTOR_MODEL_DIRECTIONAL_WITH_VECTOR_MODEL_BIASING
		 && sm->GetSubModelVectorStatus(&sm->m_directionModel_TOBEREMOVED) == VALID)
	{
		sprintf_s(szBuff, sizeof(szBuff), "%s = [%dx%d]%s%s = [%dx%d]%s%s = [%dx%d]",
			SZ_DIR, sm->m_directionModel_TOBEREMOVED.vm.direction.rowCnt, sm->m_directionModel_TOBEREMOVED.vm.direction.colCnt,
			CRLF, SZ_DIR_BIAS, sm->m_directionModel_TOBEREMOVED.vm.directionalBias.rowCnt, sm->m_directionModel_TOBEREMOVED.vm.directionalBias.colCnt, CRLF,
			SZ_DIR_TERM, sm->m_directionModel_TOBEREMOVED.vm.terminate.rowCnt, sm->m_directionModel_TOBEREMOVED.vm.terminate.colCnt);

	}
	else
	{
		sprintf_s(szBuff, sizeof(szBuff), "%s = [----]%s%s = [----]%s%s = [----]", SZ_DIR,CRLF,SZ_DIR_BIAS,CRLF,SZ_DIR_TERM);
	}
	SetDlgItemText(par, IDC_EDIT_DIRECTION_VECTOR, szBuff);




	// Pod Response To Accoustics
	hwnd = GetDlgItem(par, IDC_RADIO_AE_PODRES_STAYPOD);
	EnableWindow(hwnd, TRUE);

	if(sm->m_ae_TOBEREMOVED.podResponse == REMAINS_INTACT_goesAway)
		SendMessage(hwnd, BM_SETCHECK, (WPARAM) BST_CHECKED,0);
	else
		SendMessage(hwnd, BM_SETCHECK, (WPARAM) BST_UNCHECKED,0);


	hwnd = GetDlgItem(par, IDC_RADIO_AE_PODRES_BREAKUP);
	EnableWindow(hwnd, TRUE);
	if(sm->m_ae_TOBEREMOVED.podResponse == BREAKS_APART_goesAway)
		SendMessage(hwnd, BM_SETCHECK, (WPARAM) BST_CHECKED,0);
	else
		SendMessage(hwnd, BM_SETCHECK, (WPARAM) BST_UNCHECKED,0);


	//---------------------------//
	// Accoustic Exposure Options
	if(sm->m_ae_TOBEREMOVED.travel == TRUE)
		CheckDlgButton(par, IDC_CHECK_AE_TRAVEL, BST_CHECKED);
	else
		CheckDlgButton(par, IDC_CHECK_AE_TRAVEL, BST_UNCHECKED);

	EnableWindow(GetDlgItem(par, IDC_CHECK_AE_DIRECTION), TRUE);
	if(sm->m_ae_TOBEREMOVED.direction == TRUE)
			CheckDlgButton(par, IDC_CHECK_AE_DIRECTION, BST_CHECKED);
	else
		CheckDlgButton(par, IDC_CHECK_AE_DIRECTION, BST_UNCHECKED);

	EnableWindow(GetDlgItem(par, IDC_CHECK_AE_DIVING), TRUE);
	if(sm->m_ae_TOBEREMOVED.diving == TRUE)
			CheckDlgButton(par, IDC_CHECK_AE_DIVING, BST_CHECKED);
	else
		CheckDlgButton(par, IDC_CHECK_AE_DIVING, BST_UNCHECKED);


	if(sm->m_ae_TOBEREMOVED.direction == TRUE)
	{
		EnableWindow(GetDlgItem(par, IDC_CHECK_ANIMATBEACHES), TRUE);
		if(sm->m_ae_TOBEREMOVED.bAnimatBeaches == TRUE)
			CheckDlgButton(par, IDC_CHECK_ANIMATBEACHES, BST_CHECKED);
	}
	else
	{
		EnableWindow(GetDlgItem(par, IDC_CHECK_ANIMATBEACHES), FALSE);
		CheckDlgButton(par, IDC_CHECK_ANIMATBEACHES, BST_UNCHECKED);
	}

	if(sm->m_ae_TOBEREMOVED.direction || sm->m_ae_TOBEREMOVED.diving || sm->m_ae_TOBEREMOVED.travel)
	{
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_LINE1), TRUE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_LINE2), TRUE);

		EnableWindow(GetDlgItem(par, IDC_CHECK_CUMMULATIVE_EXP), TRUE);
		if(sm->m_ae_TOBEREMOVED.cummulativeExposure == TRUE)
			CheckDlgButton(par, IDC_CHECK_CUMMULATIVE_EXP, BST_CHECKED);

		EnableWindow(GetDlgItem(par, IDC_EDIT_AE_THESHOLD), TRUE);
		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_ae_TOBEREMOVED.exposureThreshold);
		SetDlgItemText(par, IDC_EDIT_AE_THESHOLD,  szBuff);
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_EXP_THRESH), TRUE);


		EnableWindow(GetDlgItem(par, IDC_RADIO_AE_PODRES_STAYPOD), TRUE);
		EnableWindow(GetDlgItem(par, IDC_RADIO_AE_PODRES_BREAKUP), TRUE);

		if(sm->m_ae_TOBEREMOVED.podResponse == REMAINS_INTACT_goesAway)
		{
			CheckDlgButton(par, IDC_RADIO_AE_PODRES_STAYPOD, BST_CHECKED);
			CheckDlgButton(par, IDC_RADIO_AE_PODRES_BREAKUP, BST_UNCHECKED);
		}
		else
		{
			CheckDlgButton(par, IDC_RADIO_AE_PODRES_STAYPOD, BST_UNCHECKED);
			CheckDlgButton(par, IDC_RADIO_AE_PODRES_BREAKUP, BST_CHECKED);
		}
	}
	else
	{
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_LINE1), FALSE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_LINE2), FALSE);
		
		EnableWindow(GetDlgItem(par, IDC_CHECK_CUMMULATIVE_EXP), FALSE);
		CheckDlgButton(par, IDC_CHECK_CUMMULATIVE_EXP, BST_UNCHECKED);

		EnableWindow(GetDlgItem(par, IDC_EDIT_AE_THESHOLD), FALSE);
		SetDlgItemText(par, IDC_EDIT_AE_THESHOLD,  szBlank);
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_EXP_THRESH), FALSE);

		EnableWindow(GetDlgItem(par, IDC_RADIO_AE_PODRES_STAYPOD), FALSE);
		EnableWindow(GetDlgItem(par, IDC_RADIO_AE_PODRES_BREAKUP), FALSE);

		CheckDlgButton(par, IDC_RADIO_AE_PODRES_STAYPOD, BST_UNCHECKED);
		CheckDlgButton(par, IDC_RADIO_AE_PODRES_BREAKUP, BST_UNCHECKED);
	}

	//----------------------------------------------------------------------------------//
	// Travel Accoustic Exposure
	//--------------------------//
	if(sm->m_ae_TOBEREMOVED.travel == TRUE)
	{
		// Travel Rate
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_3), TRUE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_TRAVEL_AE_MEAN), TRUE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_TRAVEL_AE_STD), TRUE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_TRAVEL_AE_COEFF), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_TRAVEL_AE_MEAN), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_AE_STD), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_TRAVEL_AE_COEFF), TRUE);

		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_travelModel_TOBEREMOVED.ae.mean);
		SetDlgItemText(par, IDC_EDIT_TRAVEL_AE_MEAN,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_travelModel_TOBEREMOVED.ae.std);
		SetDlgItemText(par, IDC_EDIT_AE_STD,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_travelModel_TOBEREMOVED.ae.coeff);
		SetDlgItemText(par, IDC_EDIT_TRAVEL_AE_COEFF,  szBuff);

	}
	else
	{
		// Travel Rate
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_3), FALSE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_TRAVEL_AE_MEAN), FALSE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_TRAVEL_AE_STD), FALSE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_TRAVEL_AE_COEFF), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_TRAVEL_AE_MEAN), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_AE_STD), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_TRAVEL_AE_COEFF), FALSE);

		SetDlgItemText(par, IDC_EDIT_TRAVEL_AE_MEAN,  szBlank);
		SetDlgItemText(par, IDC_EDIT_AE_STD,  szBlank);
		SetDlgItemText(par, IDC_EDIT_TRAVEL_AE_COEFF,  szBlank);
	}

	//----------------------------------------------------------------------------------//
	// Direction Accoustic Exposure
	//-----------------------------//
	if(sm->m_ae_TOBEREMOVED.direction == TRUE)
	{
		// Direction
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_6), TRUE);

		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_DIRECTION_CHANGE_PERTURBATION1), TRUE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_DIRECTION_CHANGE_PERTURBATION2), TRUE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_DIRECTION_CHANGE_TERM_PROB1), TRUE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_DIRECTION_CHANGE_TERM_PROB2), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_AE_DIRECTION_CHANGE_PERTURBATION), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_AE_DIRECTION_CHANGE_TERM_PROB), TRUE);

		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_directionModel_TOBEREMOVED.ae.perturbation);
		SetDlgItemText(par, IDC_EDIT_AE_DIRECTION_CHANGE_PERTURBATION,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_directionModel_TOBEREMOVED.ae.termCoeff);
		SetDlgItemText(par, IDC_EDIT_AE_DIRECTION_CHANGE_TERM_PROB,  szBuff);
	}
	else
	{
		// Direction
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_6), FALSE);

		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_DIRECTION_CHANGE_PERTURBATION1), FALSE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_DIRECTION_CHANGE_PERTURBATION2), FALSE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_DIRECTION_CHANGE_TERM_PROB1), FALSE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_DIRECTION_CHANGE_TERM_PROB2), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_AE_DIRECTION_CHANGE_PERTURBATION), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_AE_DIRECTION_CHANGE_TERM_PROB), FALSE);

		SetDlgItemText(par, IDC_EDIT_AE_DIRECTION_CHANGE_PERTURBATION,  szBlank);
		SetDlgItemText(par, IDC_EDIT_AE_DIRECTION_CHANGE_TERM_PROB,  szBlank);
	}

	//--------------------------//
	// Diving Accoustic Exposure
	//--------------------------//
	if(sm->m_ae_TOBEREMOVED.diving == TRUE)
	{
		// Dive Depth
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_1), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIVEDEPTH_AE_MEAN), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIVEDEPTH_AE_STD), TRUE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_DIVEDEPTH_AE_MEAN), TRUE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_DIVEDEPTH_AE_STD), TRUE);

		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_depthModel_TOBEREMOVED.ae.mean);
		SetDlgItemText(par, IDC_EDIT_DIVEDEPTH_AE_MEAN,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_depthModel_TOBEREMOVED.ae.std);
		SetDlgItemText(par, IDC_EDIT_DIVEDEPTH_AE_STD,  szBuff);

		// Surface Intervals
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_2), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_SURFACE_INTERVAL_AE_MEAN), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_SURFACE_INTERVAL_AE_STD), TRUE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_SI_MEAN), TRUE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_SI_STD), TRUE);
		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_surfaceIntervalModel_TOBEREMOVED.ae.mean);
		SetDlgItemText(par, IDC_EDIT_SURFACE_INTERVAL_AE_MEAN,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_surfaceIntervalModel_TOBEREMOVED.ae.std);
		SetDlgItemText(par, IDC_EDIT_SURFACE_INTERVAL_AE_STD,  szBuff);

		// Dive Rate
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_4), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_AE_DIVERATE_MEAN), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_AE_DIVERATE_STD), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_AE_DIVERATE_COEFF), TRUE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_DIVERATE_MEAN), TRUE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_DIVERATE_STD), TRUE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_DIVERATE_COEFF), TRUE);
		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_diveRateModel_TOBEREMOVED.ae.mean);
		SetDlgItemText(par, IDC_EDIT_AE_DIVERATE_MEAN,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_diveRateModel_TOBEREMOVED.ae.std);
		SetDlgItemText(par, IDC_EDIT_AE_DIVERATE_STD,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_diveRateModel_TOBEREMOVED.ae.coeff);
		SetDlgItemText(par, IDC_EDIT_AE_DIVERATE_COEFF,  szBuff);


		// Ascent Rate
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_7), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_AE_ACENTRATE_MEAN), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_AE_ACENTRATE_STD), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_AE_ACENTRATE_COEFF), TRUE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_ACENTRATE_MEAN), TRUE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_ACENTRATE_STD), TRUE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_ACENTRATE_COEFF), TRUE);
		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_ascentRateModel_TOBEREMOVED.ae.mean);
		SetDlgItemText(par, IDC_EDIT_AE_ACENTRATE_MEAN,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_ascentRateModel_TOBEREMOVED.ae.std);
		SetDlgItemText(par, IDC_EDIT_AE_ACENTRATE_STD,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_ascentRateModel_TOBEREMOVED.ae.coeff);
		SetDlgItemText(par, IDC_EDIT_AE_ACENTRATE_COEFF,  szBuff);

		// Reversals
		if(sm->m_reversalModel_TOBEREMOVED.reverses == TRUE)
		{
			EnableWindow(GetDlgItem(par, IDC_STATIC_AE_5), TRUE);
			EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL_AE_NUM_MEAN), TRUE);
			EnableWindow(GetDlgItem(par, IDC_STATIC_AE_NUM_MEAN1), TRUE);
			EnableWindow(GetDlgItem(par, IDC_STATIC_AE_NUM_MEAN2), TRUE);
			EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL_AE_NUM_STD), TRUE);
			EnableWindow(GetDlgItem(par, IDC_STATIC_REVERSAL_AE_NUM_STD1), TRUE);
			EnableWindow(GetDlgItem(par, IDC_STATIC_REVERSAL_AE_NUM_STD2), TRUE);
			EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL_AE_DURATION_MEAN), TRUE);
			EnableWindow(GetDlgItem(par, IDC_STATIC_REVERSAL_AE_DURATION_MEAN1), TRUE);
			EnableWindow(GetDlgItem(par, IDC_STATIC_REVERSAL_AE_DURATION_MEAN2), TRUE);
			EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL_AE_DURATION_STD), TRUE);
			EnableWindow(GetDlgItem(par, IDC_STATIC_REVERSAL_AE_DURATION_STD1), TRUE);
			EnableWindow(GetDlgItem(par, IDC_STATIC_REVERSAL_AE_DURATION_STD2), TRUE);
			EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL_AE_PROBABILITY), TRUE);
			EnableWindow(GetDlgItem(par, IDC_STATIC_REVERSAL_AE_PROBABILITY), TRUE);
			sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_reversalModel_TOBEREMOVED.ae.count.mean);
			SetDlgItemText(par, IDC_EDIT_REVERSAL_AE_NUM_MEAN,  szBuff);
			sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_reversalModel_TOBEREMOVED.ae.count.std);
			SetDlgItemText(par, IDC_EDIT_REVERSAL_AE_NUM_STD,  szBuff);
			sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_reversalModel_TOBEREMOVED.ae.time.mean);
			SetDlgItemText(par, IDC_EDIT_REVERSAL_AE_DURATION_MEAN,  szBuff);
			sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_reversalModel_TOBEREMOVED.ae.time.std);
			SetDlgItemText(par, IDC_EDIT_REVERSAL_AE_DURATION_STD,  szBuff);
			sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_reversalModel_TOBEREMOVED.ae.probOfReversal);
			SetDlgItemText(par, IDC_EDIT_REVERSAL_AE_PROBABILITY,  szBuff);
		}
		else
		{
			EnableWindow(GetDlgItem(par, IDC_STATIC_AE_5), FALSE);
			EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL_AE_NUM_MEAN), FALSE);
			EnableWindow(GetDlgItem(par, IDC_STATIC_AE_NUM_MEAN1), FALSE);
			EnableWindow(GetDlgItem(par, IDC_STATIC_AE_NUM_MEAN2), FALSE);
			EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL_AE_NUM_STD), FALSE);
			EnableWindow(GetDlgItem(par, IDC_STATIC_REVERSAL_AE_NUM_STD1), FALSE);
			EnableWindow(GetDlgItem(par, IDC_STATIC_REVERSAL_AE_NUM_STD2), FALSE);
			EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL_AE_DURATION_MEAN), FALSE);
			EnableWindow(GetDlgItem(par, IDC_STATIC_REVERSAL_AE_DURATION_MEAN1), FALSE);
			EnableWindow(GetDlgItem(par, IDC_STATIC_REVERSAL_AE_DURATION_MEAN2), FALSE);
			EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL_AE_DURATION_STD), FALSE);
			EnableWindow(GetDlgItem(par, IDC_STATIC_REVERSAL_AE_DURATION_STD1), FALSE);
			EnableWindow(GetDlgItem(par, IDC_STATIC_REVERSAL_AE_DURATION_STD2), FALSE);
			EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL_AE_PROBABILITY), FALSE);
			EnableWindow(GetDlgItem(par, IDC_STATIC_REVERSAL_AE_PROBABILITY), FALSE);
			SetDlgItemText(par, IDC_EDIT_REVERSAL_AE_NUM_MEAN,  szBlank);
			SetDlgItemText(par, IDC_EDIT_REVERSAL_AE_NUM_STD,  szBlank);
			SetDlgItemText(par, IDC_EDIT_REVERSAL_AE_DURATION_MEAN,  szBlank);
			SetDlgItemText(par, IDC_EDIT_REVERSAL_AE_DURATION_STD,  szBlank);
			SetDlgItemText(par, IDC_EDIT_REVERSAL_AE_PROBABILITY,  szBlank);
		}
	}
	else
	{
		CheckDlgButton(par, IDC_CHECK_AE_DIVING, BST_UNCHECKED);
		
		// Dive Depth
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_1), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIVEDEPTH_AE_MEAN), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIVEDEPTH_AE_STD), FALSE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_DIVEDEPTH_AE_MEAN), FALSE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_DIVEDEPTH_AE_STD), FALSE);
		SetDlgItemText(par, IDC_EDIT_DIVEDEPTH_AE_MEAN,  szBlank);
		SetDlgItemText(par, IDC_EDIT_DIVEDEPTH_AE_STD,  szBlank);

		// Surface Intervals
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_2), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_SURFACE_INTERVAL_AE_MEAN), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_SURFACE_INTERVAL_AE_STD), FALSE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_SI_MEAN), FALSE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_SI_STD), FALSE);
		SetDlgItemText(par, IDC_EDIT_SURFACE_INTERVAL_AE_MEAN,  szBlank);
		SetDlgItemText(par, IDC_EDIT_SURFACE_INTERVAL_AE_STD,  szBlank);

		// Dive Rate
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_4), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_AE_DIVERATE_MEAN), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_AE_DIVERATE_STD), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_AE_DIVERATE_COEFF), FALSE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_DIVERATE_MEAN), FALSE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_DIVERATE_STD), FALSE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_DIVERATE_COEFF), FALSE);
		SetDlgItemText(par, IDC_EDIT_AE_DIVERATE_MEAN,  szBlank);
		SetDlgItemText(par, IDC_EDIT_AE_DIVERATE_STD,  szBlank);
		SetDlgItemText(par, IDC_EDIT_AE_DIVERATE_COEFF,  szBlank);

		// Ascent Rate
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_7), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_AE_ACENTRATE_MEAN), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_AE_ACENTRATE_STD), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_AE_ACENTRATE_COEFF), FALSE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_ACENTRATE_MEAN), FALSE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_ACENTRATE_STD), FALSE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_ACENTRATE_COEFF), FALSE);
		SetDlgItemText(par, IDC_EDIT_AE_ACENTRATE_MEAN,  szBlank);
		SetDlgItemText(par, IDC_EDIT_AE_ACENTRATE_STD,  szBlank);
		SetDlgItemText(par, IDC_EDIT_AE_ACENTRATE_COEFF,  szBlank);

		// Reversals
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_5), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL_AE_NUM_MEAN), FALSE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_NUM_MEAN1), FALSE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_AE_NUM_MEAN2), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL_AE_NUM_STD), FALSE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_REVERSAL_AE_NUM_STD1), FALSE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_REVERSAL_AE_NUM_STD2), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL_AE_DURATION_MEAN), FALSE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_REVERSAL_AE_DURATION_MEAN1), FALSE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_REVERSAL_AE_DURATION_MEAN2), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL_AE_DURATION_STD), FALSE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_REVERSAL_AE_DURATION_STD1), FALSE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_REVERSAL_AE_DURATION_STD2), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL_AE_PROBABILITY), FALSE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_REVERSAL_AE_PROBABILITY), FALSE);
		SetDlgItemText(par, IDC_EDIT_REVERSAL_AE_NUM_MEAN,  szBlank);
		SetDlgItemText(par, IDC_EDIT_REVERSAL_AE_NUM_STD,  szBlank);
		SetDlgItemText(par, IDC_EDIT_REVERSAL_AE_DURATION_MEAN,  szBlank);
		SetDlgItemText(par, IDC_EDIT_REVERSAL_AE_DURATION_STD,  szBlank);
		SetDlgItemText(par, IDC_EDIT_REVERSAL_AE_PROBABILITY,  szBlank);
	}


	//----------------------------------------------------------------------------------//
	// Dive Depth
	//-----------//
	// IDC_STATIC_VMDIVEDEPTH
	// IDC_EDIT_DIVEDEPTHL_VECTOR
	//---------------------------//
	if(sm->m_depthModel_TOBEREMOVED.modelType == VECTOR)
	{
		// model type can only be set to vector if the user clicks on radio box or loads
		// a model in.
		// Radio Buttons
		SendMessage(GetDlgItem(par, IDC_RADIO_DIVEDEPTH1), BM_SETCHECK, BST_UNCHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_DIVEDEPTH2), BM_SETCHECK, BST_UNCHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_DIVEDEPTH3), BM_SETCHECK, BST_CHECKED,0);

		// Static text and vector display
		EnableWindow(GetDlgItem(par, IDC_STATIC_VMDIVEDEPTH), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIVEDEPTHL_VECTOR), TRUE);


		//**************
		// Static text, write proper text.
		SetDlgItemText(par, IDC_STATIC_DIVEDEPTH1,  szBlank);
		SetDlgItemText(par, IDC_STATIC_DIVEDEPTH2,  szBlank);
		//SetDlgItemText(par, IDC_STATIC_DIVEDEPTH3,  szBlank);

		// Static text, enable/disable, then write proper text.
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIVEDEPTH1), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIVEDEPTH2), FALSE);
		//EnableWindow(GetDlgItem(par, IDC_EDIT_DIVEDEPTH3), FALSE);

		SetDlgItemText(par, IDC_EDIT_DIVEDEPTH1,  szBlank);
		SetDlgItemText(par, IDC_EDIT_DIVEDEPTH2,  szBlank);
		//SetDlgItemText(par, IDC_EDIT_DIVEDEPTH3,  szBlank);
		//*********

		//EnableWindow(GetDlgItem(par, IDC_EDIT_DIVEDEPTH_AE_MEAN), FALSE);
		//EnableWindow(GetDlgItem(par, IDC_EDIT_DIVEDEPTH_AE_STD), FALSE);
		//SetDlgItemText(par, IDC_EDIT_DIVEDEPTH_AE_MEAN,  szBlank);
		//SetDlgItemText(par, IDC_EDIT_DIVEDEPTH_AE_STD,  szBlank);

	}
	else if(sm->m_depthModel_TOBEREMOVED.modelType == UNIFORM)
	{
		// Radio Buttons
		SendMessage(GetDlgItem(par, IDC_RADIO_DIVEDEPTH1), BM_SETCHECK, BST_CHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_DIVEDEPTH2), BM_SETCHECK, BST_UNCHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_DIVEDEPTH3), BM_SETCHECK, BST_UNCHECKED,0);


		// Static text and vector display
		EnableWindow(GetDlgItem(par, IDC_STATIC_VMDIVEDEPTH), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIVEDEPTHL_VECTOR), FALSE);

		// Edit boxes
		// Static text, enable/disable, then write proper text.
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIVEDEPTH1), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIVEDEPTH2), TRUE);
		//EnableWindow(GetDlgItem(par, IDC_EDIT_DIVEDEPTH3), TRUE);


		// Static text, write proper text.
		SetDlgItemText(par, IDC_STATIC_DIVEDEPTH1,  szMax);
		SetDlgItemText(par, IDC_STATIC_DIVEDEPTH2,  szBlank);
		//SetDlgItemText(par, IDC_STATIC_DIVEDEPTH3,  szBlank);


		// Static text, enable/disable, then write proper text.
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIVEDEPTH1), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIVEDEPTH2), FALSE);
		//EnableWindow(GetDlgItem(par, IDC_EDIT_DIVEDEPTH3), FALSE);

		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_depthModel_TOBEREMOVED.rnd.max);
		SetDlgItemText(par, IDC_EDIT_DIVEDEPTH1,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), szBlank);
		SetDlgItemText(par, IDC_EDIT_DIVEDEPTH2,  szBuff);
		//sprintf_s(szBuff, sizeof(szBuff), szBlank);
		//SetDlgItemText(par, IDC_EDIT_DIVEDEPTH3,  szBuff);
	}
	else if(sm->m_depthModel_TOBEREMOVED.modelType == GAUSSIAN)
	{
		// Radio Buttons
		SendMessage(GetDlgItem(par, IDC_RADIO_DIVEDEPTH1), BM_SETCHECK, BST_UNCHECKED, 0);
		SendMessage(GetDlgItem(par, IDC_RADIO_DIVEDEPTH2), BM_SETCHECK, BST_CHECKED ,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_DIVEDEPTH3), BM_SETCHECK, BST_UNCHECKED, 0);

		// Static text and vector display
		EnableWindow(GetDlgItem(par, IDC_STATIC_VMDIVEDEPTH), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIVEDEPTHL_VECTOR), FALSE);

		// Edit boxes
		// Static text, enable/disable, then write proper text.
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIVEDEPTH1), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIVEDEPTH2), TRUE);
		//EnableWindow(GetDlgItem(par, IDC_EDIT_DIVEDEPTH3), TRUE);


		// Static text, write proper text.
		SetDlgItemText(par, IDC_STATIC_DIVEDEPTH1,  szMean);
		SetDlgItemText(par, IDC_STATIC_DIVEDEPTH2,  szStd);
		//SetDlgItemText(par, IDC_STATIC_DIVEDEPTH3,  szCoeff);


		// Static text, enable/disable, then write proper text.
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIVEDEPTH1), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIVEDEPTH2), TRUE);
		//EnableWindow(GetDlgItem(par, IDC_EDIT_DIVEDEPTH3), TRUE);

		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_depthModel_TOBEREMOVED.gauss.mean);
		SetDlgItemText(par, IDC_EDIT_DIVEDEPTH1,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_depthModel_TOBEREMOVED.gauss.std);
		SetDlgItemText(par, IDC_EDIT_DIVEDEPTH2,  szBuff);
	}

	//-------------------------------------------------------------------------------------//
	// Dive Rate
	//----------//
	// IDC_STATIC_VMDESCENT
	// IDC_EDIT_DIVERATE_VECTOR
	//-------------------------//
	if(sm->m_diveRateModel_TOBEREMOVED.modelType == VECTOR)
	{
		// model type can only be set to vector if the user clicks on radio box or loads
		// a model in.

		// Radio buttons
		SendMessage(GetDlgItem(par, IDC_RADIO_DIVERATE1), BM_SETCHECK, BST_UNCHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_DIVERATE2), BM_SETCHECK, BST_UNCHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_DIVERATE3), BM_SETCHECK, BST_CHECKED,0);

		// Static text and edit controls
		EnableWindow(GetDlgItem(par, IDC_STATIC_VMDESCENT), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIVERATE_VECTOR), TRUE);


		//*************//
		// Static text, write proper text.
		SetDlgItemText(par, IDC_STATIC_DIVERATE1,  szBlank);
		SetDlgItemText(par, IDC_STATIC_DIVERATE2,  szBlank);
		SetDlgItemText(par, IDC_STATIC_DIVERATE3,  szBlank);

		// Static text, enable/disable, then write proper text.
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIVERATE1), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIVERATE2), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIVERATE3), FALSE);

		SetDlgItemText(par, IDC_EDIT_DIVERATE1,  szBlank);
		SetDlgItemText(par, IDC_EDIT_DIVERATE2,  szBlank);
		SetDlgItemText(par, IDC_EDIT_DIVERATE3,  szBlank);
		//*********//

	}
	else if(sm->m_diveRateModel_TOBEREMOVED.modelType == UNIFORM)
	{

		// Radio buttons
		SendMessage(GetDlgItem(par, IDC_RADIO_DIVERATE1), BM_SETCHECK, BST_CHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_DIVERATE2), BM_SETCHECK, BST_UNCHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_DIVERATE3), BM_SETCHECK, BST_UNCHECKED,0);

		// Static text and edit controls
		EnableWindow(GetDlgItem(par, IDC_STATIC_VMDESCENT), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIVERATE_VECTOR), FALSE);

		// Static text, write proper text.
		SetDlgItemText(par, IDC_STATIC_DIVERATE1,  szMin);
		SetDlgItemText(par, IDC_STATIC_DIVERATE2,  szMax);
		SetDlgItemText(par, IDC_STATIC_DIVERATE3,  szCoeff);

		// Static text, enable/disable, then write proper text.
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIVERATE1), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIVERATE2), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIVERATE3), TRUE);

		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_diveRateModel_TOBEREMOVED.rnd.min);
		SetDlgItemText(par, IDC_EDIT_DIVERATE1,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_diveRateModel_TOBEREMOVED.rnd.max);
		SetDlgItemText(par, IDC_EDIT_DIVERATE2,  szBuff);
		sprintf_s(szBuff, TEXT("%.2f"), sm->m_diveRateModel_TOBEREMOVED.rnd.coeff);
		SetDlgItemText(par, IDC_EDIT_DIVERATE3,  szBuff);
	}
	else if(sm->m_diveRateModel_TOBEREMOVED.modelType == GAUSSIAN)
	{

		// Radio buttons
		SendMessage(GetDlgItem(par, IDC_RADIO_DIVERATE1), BM_SETCHECK, BST_UNCHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_DIVERATE2), BM_SETCHECK, BST_CHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_DIVERATE3), BM_SETCHECK, BST_UNCHECKED,0);

		// Static text and edit controls
		EnableWindow(GetDlgItem(par, IDC_STATIC_VMDESCENT), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIVERATE_VECTOR), FALSE);

		// Static text, enable/disable, then write proper text.
		//EnableWindow(GetDlgItem(par, IDC_EDIT_DIVERATE1), TRUE);
		//EnableWindow(GetDlgItem(par, IDC_EDIT_DIVERATE2), TRUE);
		//EnableWindow(GetDlgItem(par, IDC_EDIT_DIVERATE3), TRUE);

		// Static text, write proper text.
		SetDlgItemText(par, IDC_STATIC_DIVERATE1,  szMean);
		SetDlgItemText(par, IDC_STATIC_DIVERATE2,  szStd);
		SetDlgItemText(par, IDC_STATIC_DIVERATE3,  szCoeff);


		// Static text, enable/disable, then write proper text.
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIVERATE1), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIVERATE2), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIVERATE3), TRUE);

		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_diveRateModel_TOBEREMOVED.gauss.mean);
		SetDlgItemText(par, IDC_EDIT_DIVERATE1,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_diveRateModel_TOBEREMOVED.gauss.std);
		SetDlgItemText(par, IDC_EDIT_DIVERATE2,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_diveRateModel_TOBEREMOVED.gauss.coeff);
		SetDlgItemText(par, IDC_EDIT_DIVERATE3,  szBuff);
	}


	//----------------------------------------------------------------------------------//
	// Acsent Rate
	//------------//
	// IDC_STATIC_VMASCENT
	// IDC_EDIT_ACENTRATE_VECTOR
	//--------------------------//
	if(sm->m_ascentRateModel_TOBEREMOVED.modelType == VECTOR)
	{
		// model type can only be set to vector if the user clicks on radio box or loads
		// a model in.


		// The radio boxes to check Random or Gaussian
		SendMessage(GetDlgItem(par, IDC_RADIO_ACENTRATE1), BM_SETCHECK, BST_UNCHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_ACENTRATE2), BM_SETCHECK, BST_UNCHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_ACENTRATE3), BM_SETCHECK, BST_CHECKED,0);

		// Static and edit box
		EnableWindow(GetDlgItem(par, IDC_STATIC_VMASCENT), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_ACENTRATE_VECTOR), TRUE);

		// The static text windows under the edit boxes for mean, std, and coeff.
		SetDlgItemText(par, IDC_STATIC_ACENTRATE1,  szBlank);
		SetDlgItemText(par, IDC_STATIC_ACENTRATE2,  szBlank);
		SetDlgItemText(par, IDC_STATIC_ACENTRATE3,  szBlank);

		// The edit box windows for mean, std, and coeff.
		EnableWindow(GetDlgItem(par, IDC_EDIT_ACENTRATE1), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_ACENTRATE2), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_ACENTRATE3), FALSE);

		SetDlgItemText(par, IDC_EDIT_ACENTRATE1,  szBlank);
		SetDlgItemText(par, IDC_EDIT_ACENTRATE2,  szBlank);
		SetDlgItemText(par, IDC_EDIT_ACENTRATE3,  szBlank);

	}
	else if(sm->m_ascentRateModel_TOBEREMOVED.modelType == UNIFORM)
	{
		// The radio boxes to check Random or Gaussian
		SendMessage(GetDlgItem(par, IDC_RADIO_ACENTRATE1), BM_SETCHECK, BST_CHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_ACENTRATE2), BM_SETCHECK, BST_UNCHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_ACENTRATE3), BM_SETCHECK, BST_UNCHECKED,0);

		// Static and edit box
		EnableWindow(GetDlgItem(par, IDC_STATIC_VMASCENT), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_ACENTRATE_VECTOR), FALSE);

		// Static text, write proper text.
		SetDlgItemText(par, IDC_STATIC_ACENTRATE1,  szMin);
		SetDlgItemText(par, IDC_STATIC_ACENTRATE2,  szMax);
		SetDlgItemText(par, IDC_STATIC_ACENTRATE3,  szCoeff);


		// Static text, enable/disable, then write proper text.
		EnableWindow(GetDlgItem(par, IDC_EDIT_ACENTRATE1), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_ACENTRATE2), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_ACENTRATE3), TRUE);

		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_ascentRateModel_TOBEREMOVED.rnd.min);
		SetDlgItemText(par, IDC_EDIT_ACENTRATE1,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_ascentRateModel_TOBEREMOVED.rnd.max);
		SetDlgItemText(par, IDC_EDIT_ACENTRATE2,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_ascentRateModel_TOBEREMOVED.rnd.coeff);
		SetDlgItemText(par, IDC_EDIT_ACENTRATE3,  szBuff);
	}
	else if(sm->m_ascentRateModel_TOBEREMOVED.modelType == GAUSSIAN)
	{
		// The radio boxes to check Random or Gaussian
		SendMessage(GetDlgItem(par, IDC_RADIO_ACENTRATE1), BM_SETCHECK, BST_UNCHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_ACENTRATE2), BM_SETCHECK, BST_CHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_ACENTRATE3), BM_SETCHECK, BST_UNCHECKED,0);

		// Static and edit box
		EnableWindow(GetDlgItem(par, IDC_STATIC_VMASCENT), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_ACENTRATE_VECTOR), FALSE);

		// Static text, write proper text.
		SetDlgItemText(par, IDC_STATIC_ACENTRATE1,  szMean);
		SetDlgItemText(par, IDC_STATIC_ACENTRATE2,  szStd);
		SetDlgItemText(par, IDC_STATIC_ACENTRATE3,  szCoeff);


		// Static text, enable/disable, then write proper text.
		EnableWindow(GetDlgItem(par, IDC_EDIT_ACENTRATE1), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_ACENTRATE2), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_ACENTRATE3), TRUE);

		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_ascentRateModel_TOBEREMOVED.gauss.mean);
		SetDlgItemText(par, IDC_EDIT_ACENTRATE1,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_ascentRateModel_TOBEREMOVED.gauss.std);
		SetDlgItemText(par, IDC_EDIT_ACENTRATE2,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_ascentRateModel_TOBEREMOVED.gauss.coeff);
		SetDlgItemText(par, IDC_EDIT_ACENTRATE3,  szBuff);
	}


	//----------------------------------------------------------------------------------//
	// Travel
	//-------//
	// IDC_STATIC_VMTRAVEL
	// IDC_EDIT_TRAVEL_VECTOR
	//-----------------------//
	if(sm->m_travelModel_TOBEREMOVED.modelType == VECTOR)
	{
		// model type can only be set to vector if the user clicks on radio box or loads
		// a model in.


		// Radio buttons
		SendMessage(GetDlgItem(par, IDC_RADIO_TRAVEL1), BM_SETCHECK, BST_UNCHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_TRAVEL2), BM_SETCHECK, BST_UNCHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_TRAVEL3), BM_SETCHECK, BST_CHECKED,0);

		// Static and edit controls
		EnableWindow(GetDlgItem(par, IDC_STATIC_VMTRAVEL), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_TRAVEL_VECTOR), TRUE);

		// Static text, write proper text.
		SetDlgItemText(par, IDC_STATIC_TRAVEL1,  szBlank);
		SetDlgItemText(par, IDC_STATIC_TRAVEL2,  szBlank);
		SetDlgItemText(par, IDC_STATIC_TRAVEL3,  szBlank);

		// Static text, enable/disable, then write proper text.
		EnableWindow(GetDlgItem(par, IDC_EDIT_TRAVEL1), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_TRAVEL2), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_TRAVEL3), FALSE);

		SetDlgItemText(par, IDC_EDIT_TRAVEL1,  szBlank);
		SetDlgItemText(par, IDC_EDIT_TRAVEL2,  szBlank);
		SetDlgItemText(par, IDC_EDIT_TRAVEL3,  szBlank);
	}
	else if(sm->m_travelModel_TOBEREMOVED.modelType == UNIFORM)
	{
		// Radio buttons
		SendMessage(GetDlgItem(par, IDC_RADIO_TRAVEL1), BM_SETCHECK, BST_CHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_TRAVEL2), BM_SETCHECK, BST_UNCHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_TRAVEL3), BM_SETCHECK, BST_UNCHECKED,0);

		// Static and edit controls
		EnableWindow(GetDlgItem(par, IDC_STATIC_VMTRAVEL), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_TRAVEL_VECTOR), FALSE);

		// Static text, write proper text.
		SetDlgItemText(par, IDC_STATIC_TRAVEL1,  szMin);
		SetDlgItemText(par, IDC_STATIC_TRAVEL2,  szMax);
		SetDlgItemText(par, IDC_STATIC_TRAVEL3,  szCoeff);


		// Static text, enable/disable, then write proper text.
		EnableWindow(GetDlgItem(par, IDC_EDIT_TRAVEL1), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_TRAVEL2), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_TRAVEL3), TRUE);

		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_travelModel_TOBEREMOVED.rnd.min);
		SetDlgItemText(par, IDC_EDIT_TRAVEL1,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_travelModel_TOBEREMOVED.rnd.max);
		SetDlgItemText(par, IDC_EDIT_TRAVEL2,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_travelModel_TOBEREMOVED.rnd.coeff);
		SetDlgItemText(par, IDC_EDIT_TRAVEL3,  szBuff);
	}
	else  //GAUSSIAN
	{
		// Radio buttons
		SendMessage(GetDlgItem(par, IDC_RADIO_TRAVEL1), BM_SETCHECK, BST_UNCHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_TRAVEL2), BM_SETCHECK, BST_CHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_TRAVEL3), BM_SETCHECK, BST_UNCHECKED,0);

		// Static and edit controls
		EnableWindow(GetDlgItem(par, IDC_STATIC_VMTRAVEL), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_TRAVEL_VECTOR), FALSE);

		// Static text, write proper text.
		SetDlgItemText(par, IDC_STATIC_TRAVEL1,  szMean);
		SetDlgItemText(par, IDC_STATIC_TRAVEL2,  szStd);
		SetDlgItemText(par, IDC_STATIC_TRAVEL3,  szCoeff);

		// Static text, enable/disable, then write proper text.
		EnableWindow(GetDlgItem(par, IDC_EDIT_TRAVEL1), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_TRAVEL2), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_TRAVEL3), TRUE);

		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_travelModel_TOBEREMOVED.gauss.mean);
		SetDlgItemText(par, IDC_EDIT_TRAVEL1,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_travelModel_TOBEREMOVED.gauss.std);
		SetDlgItemText(par, IDC_EDIT_TRAVEL2,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_travelModel_TOBEREMOVED.gauss.coeff);
		SetDlgItemText(par, IDC_EDIT_TRAVEL3,  szBuff);
	}



	//----------------------------------------------------------------------------------//
	// Surface Interval
	//-----------------//
	// IDC_STATIC_VMSI
	// IDC_EDIT_SURINTVAL_VECTOR
	//--------------------------//
	_ASSERTE(sm->m_surfaceIntervalModel_TOBEREMOVED.modelType != UNIFORM);

	if(sm->m_surfaceIntervalModel_TOBEREMOVED.modelType == VECTOR)
	{
		// model type can only be set to vector if the user clicks on radio box or loads
		// a model in.


		// Radio Buttons
		SendMessage(GetDlgItem(par, IDC_RADIO_SURFINTVL1), BM_SETCHECK, BST_UNCHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_SURFINTVL2), BM_SETCHECK, BST_CHECKED,0);

		// Static and edit controls.
		EnableWindow(GetDlgItem(par, IDC_STATIC_VMSI), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_SURINTVAL_VECTOR), TRUE);

		// Static text, write proper text.
		SetDlgItemText(par, IDC_STATIC_SI_MEAN,  szBlank);
		SetDlgItemText(par, IDC_STATIC_SI_STD,  szBlank);

		// Static text, enable/disable, then write proper text.
		EnableWindow(GetDlgItem(par, IDC_EDIT_SI1), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_SI2), FALSE);

		SetDlgItemText(par, IDC_EDIT_SI1,  szBlank);
		SetDlgItemText(par, IDC_EDIT_SI2,  szBlank);

	}
	else if(sm->m_surfaceIntervalModel_TOBEREMOVED.modelType == GAUSSIAN)
	{
		// Radio Buttons
		SendMessage(GetDlgItem(par, IDC_RADIO_SURFINTVL1), BM_SETCHECK, BST_CHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_SURFINTVL2), BM_SETCHECK, BST_UNCHECKED,0);

		// Static and edit controls.
		EnableWindow(GetDlgItem(par, IDC_STATIC_VMSI), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_SURINTVAL_VECTOR), FALSE);

		// Edit boxes
		EnableWindow(GetDlgItem(par, IDC_EDIT_SI1), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_SI2), TRUE);
		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_surfaceIntervalModel_TOBEREMOVED.gauss.mean);
		SetDlgItemText(par, IDC_EDIT_SI1,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_surfaceIntervalModel_TOBEREMOVED.gauss.std);
		SetDlgItemText(par, IDC_EDIT_SI2,  szBuff);

		// Static text, write proper text.
		SetDlgItemText(par, IDC_STATIC_SI_MEAN,  szMean);
		SetDlgItemText(par, IDC_STATIC_SI_STD,  szStd);
		EnableWindow(GetDlgItem(par, IDC_STATIC_SI_MEAN), TRUE);
		EnableWindow(GetDlgItem(par, IDC_STATIC_SI_STD), TRUE);
	}


	//----------------------------------------------------------------------------------//
	// Direction Change
	//-----------------//
	// IDC_STATIC_VMDIRECTION
	// IDC_EDIT_DIRECTION_VECTOR
	//--------------------------//
	if(sm->m_directionModel_TOBEREMOVED.modelType == VECTOR_MODEL_DIRECTIONAL_NO_BIASING)
	{
		SendMessage(GetDlgItem(par, IDC_RADIO_DIRECTION_CHANGE1), BM_SETCHECK, BST_UNCHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_DIRECTION_CHANGE2), BM_SETCHECK, BST_UNCHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_DIRECTION_CHANGE3), BM_SETCHECK, BST_UNCHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_DIRECTION_CHANGE4), BM_SETCHECK, BST_CHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_DIRECTION_CHANGE5), BM_SETCHECK, BST_UNCHECKED,0);
	}
	else if(sm->m_directionModel_TOBEREMOVED.modelType == VECTOR_MODEL_DIRECTIONAL_WITH_VECTOR_MODEL_BIASING)
	{
		SendMessage(GetDlgItem(par, IDC_RADIO_DIRECTION_CHANGE1), BM_SETCHECK, BST_UNCHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_DIRECTION_CHANGE2), BM_SETCHECK, BST_UNCHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_DIRECTION_CHANGE3), BM_SETCHECK, BST_UNCHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_DIRECTION_CHANGE4), BM_SETCHECK, BST_UNCHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_DIRECTION_CHANGE5), BM_SETCHECK, BST_CHECKED,0);
	}

	if(sm->m_directionModel_TOBEREMOVED.modelType == VECTOR_MODEL_DIRECTIONAL_NO_BIASING ||
		sm->m_directionModel_TOBEREMOVED.modelType == VECTOR_MODEL_DIRECTIONAL_WITH_VECTOR_MODEL_BIASING)
	{
		// model type can only be set to vector if the user clicks on radio box or loads
		// a model in.
		EnableWindow(GetDlgItem(par, IDC_STATIC_VMDIRECTION), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIRECTION_VECTOR), TRUE);


		// Static text, write proper text.
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE1,  szBlank);
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE1A, szBlank);
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE2,  szBlank);
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE2A, szBlank);
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE3,  szBlank);
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE3A, szBlank);
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE4,  szBlank);
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE4A, szBlank);
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE5,  szBlank);
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE5A, szBlank);


		// Static text, enable/disable, then write proper text.
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIRECTION_CHANGE1), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIRECTION_CHANGE2), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIRECTION_CHANGE3), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIRECTION_CHANGE4), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIRECTION_CHANGE5), FALSE);

		SetDlgItemText(par, IDC_EDIT_DIRECTION_CHANGE1,  szBlank);
		SetDlgItemText(par, IDC_EDIT_DIRECTION_CHANGE2,  szBlank);
		SetDlgItemText(par, IDC_EDIT_DIRECTION_CHANGE3,  szBlank);
		SetDlgItemText(par, IDC_EDIT_DIRECTION_CHANGE4,  szBlank);
		SetDlgItemText(par, IDC_EDIT_DIRECTION_CHANGE5,  szBlank);
	}
	else if(sm->m_directionModel_TOBEREMOVED.modelType == RANDOM_WALK)
	{
		SendMessage(GetDlgItem(par, IDC_RADIO_DIRECTION_CHANGE1), BM_SETCHECK, BST_CHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_DIRECTION_CHANGE2), BM_SETCHECK, BST_UNCHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_DIRECTION_CHANGE3), BM_SETCHECK, BST_UNCHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_DIRECTION_CHANGE4), BM_SETCHECK, BST_UNCHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_DIRECTION_CHANGE5), BM_SETCHECK, BST_UNCHECKED,0);

		EnableWindow(GetDlgItem(par, IDC_STATIC_VMDIRECTION), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIRECTION_VECTOR), FALSE);

		// Static text, write proper text.
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE1,  szCoeff);
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE1A, szBlank);
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE2,  szBlank);
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE2A, szBlank);
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE3,  szBlank);
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE3A, szBlank);
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE4,  szBlank);
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE4A, szBlank);
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE5,  szBlank);
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE5A, szBlank);


		// Static text, enable/disable, then write proper text.
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIRECTION_CHANGE1), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIRECTION_CHANGE2), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIRECTION_CHANGE3), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIRECTION_CHANGE4), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIRECTION_CHANGE5), FALSE);

		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_directionModel_TOBEREMOVED.rndWalk.termCoeff);
		SetDlgItemText(par, IDC_EDIT_DIRECTION_CHANGE1,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), szBlank);
		SetDlgItemText(par, IDC_EDIT_DIRECTION_CHANGE2,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), szBlank);
		SetDlgItemText(par, IDC_EDIT_DIRECTION_CHANGE3,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), szBlank);
		SetDlgItemText(par, IDC_EDIT_DIRECTION_CHANGE4,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), szBlank);
		SetDlgItemText(par, IDC_EDIT_DIRECTION_CHANGE5,  szBuff);
	}
	else if(sm->m_directionModel_TOBEREMOVED.modelType == CORRELATED_RANDOM_WALK)
	{

		SendMessage(GetDlgItem(par, IDC_RADIO_DIRECTION_CHANGE1), BM_SETCHECK, BST_UNCHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_DIRECTION_CHANGE2), BM_SETCHECK, BST_CHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_DIRECTION_CHANGE3), BM_SETCHECK, BST_UNCHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_DIRECTION_CHANGE4), BM_SETCHECK, BST_UNCHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_DIRECTION_CHANGE5), BM_SETCHECK, BST_UNCHECKED,0);

		EnableWindow(GetDlgItem(par, IDC_STATIC_VMDIRECTION), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIRECTION_VECTOR), FALSE);


		// Static text, write proper text.
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE1,  szPerturbation1);
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE1A, szPerturbation2);
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE2,  szCoeff);
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE2A, szBlank);
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE3,  szBlank);
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE3A, szBlank);
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE4,  szBlank);
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE4A, szBlank);
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE5,  szBlank);
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE5A, szBlank);


		// Static text, enable/disable, then write proper text.
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIRECTION_CHANGE1), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIRECTION_CHANGE2), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIRECTION_CHANGE3), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIRECTION_CHANGE4), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIRECTION_CHANGE5), FALSE);

		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_directionModel_TOBEREMOVED.crRndWalk.perturbation);
		SetDlgItemText(par, IDC_EDIT_DIRECTION_CHANGE1,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_directionModel_TOBEREMOVED.crRndWalk.termCoeff);
		SetDlgItemText(par, IDC_EDIT_DIRECTION_CHANGE2,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), szBlank);
		SetDlgItemText(par, IDC_EDIT_DIRECTION_CHANGE3,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), szBlank);
		SetDlgItemText(par, IDC_EDIT_DIRECTION_CHANGE4,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), szBlank);
		SetDlgItemText(par, IDC_EDIT_DIRECTION_CHANGE5,  szBuff);

	}
	else //CORRELATED_RANDOM_WALK_WITH_DIR_BIASING
	{
//		CheckRadioButton(par, IDC_RADIO_DIRECTION_CHANGE1, IDC_RADIO_DIRECTION_CHANGE3, IDC_RADIO_DIRECTION_CHANGE3);
		SendMessage(GetDlgItem(par, IDC_RADIO_DIRECTION_CHANGE1), BM_SETCHECK, BST_UNCHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_DIRECTION_CHANGE2), BM_SETCHECK, BST_UNCHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_DIRECTION_CHANGE3), BM_SETCHECK, BST_CHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_DIRECTION_CHANGE4), BM_SETCHECK, BST_UNCHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_DIRECTION_CHANGE5), BM_SETCHECK, BST_UNCHECKED,0);

		EnableWindow(GetDlgItem(par, IDC_STATIC_VMDIRECTION), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIRECTION_VECTOR), FALSE);

		// Static text, write proper text.
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE1,  szPerturbation1);
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE1A, szPerturbation2);
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE2,  szBias);
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE2A, szDirection);
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE3,  szBias);
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE3A, szBlank);
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE4,  szArcStep);
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE4A, szBlank);
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE5,  szCoeff);
		SetDlgItemText(par, IDC_STATIC_DIRECTION_CHANGE5A, szBlank);


		// Static text, enable/disable, then write proper text.
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIRECTION_CHANGE1), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIRECTION_CHANGE2), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIRECTION_CHANGE3), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIRECTION_CHANGE4), TRUE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_DIRECTION_CHANGE5), TRUE);

		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_directionModel_TOBEREMOVED.crRndWalkDb.perturbation);
		SetDlgItemText(par, IDC_EDIT_DIRECTION_CHANGE1,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_directionModel_TOBEREMOVED.crRndWalkDb.directionOfBias);
		SetDlgItemText(par, IDC_EDIT_DIRECTION_CHANGE2,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_directionModel_TOBEREMOVED.crRndWalkDb.bias);
		SetDlgItemText(par, IDC_EDIT_DIRECTION_CHANGE3,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_directionModel_TOBEREMOVED.crRndWalkDb.arcStep);
		SetDlgItemText(par, IDC_EDIT_DIRECTION_CHANGE4,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_directionModel_TOBEREMOVED.crRndWalkDb.termCoeff);
		SetDlgItemText(par, IDC_EDIT_DIRECTION_CHANGE5,  szBuff);
	}


	//-------------------------------------------------------------------------------------//
	// Reversals
	//----------//
	// IDC_STATIC_VMREVERSAL
	// IDC_EDIT_REVERSAL_VECTOR
	//-------------------------//

	//-----------------------------------------------------------------------------------//
	// Reversals
	//---------------------------------//
	if(sm->m_reversalModel_TOBEREMOVED.reverses == FALSE) // Animat doesn't flat bottom dive.
	{
		SendMessage(GetDlgItem(par, IDC_RADIO_REVERSAL1), BM_SETCHECK, BST_UNCHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_REVERSAL2), BM_SETCHECK, BST_CHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_REVERSAL3), BM_SETCHECK, BST_UNCHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_REVERSAL4), BM_SETCHECK, BST_UNCHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_REVERSAL5), BM_SETCHECK, BST_UNCHECKED,0);
		//////////////////////////////////////////////////////////////////////

		EnableWindow(GetDlgItem(par, IDC_RADIO_REVERSAL3), FALSE);
		EnableWindow(GetDlgItem(par, IDC_RADIO_REVERSAL4), FALSE);
		EnableWindow(GetDlgItem(par, IDC_RADIO_REVERSAL5), FALSE);

		// Static and edit box
		EnableWindow(GetDlgItem(par, IDC_STATIC_VMREVERSAL), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL_VECTOR), FALSE);


		SetDlgItemText(par, IDC_STATIC_REVERSAL1,  szBlank);
		SetDlgItemText(par, IDC_STATIC_REVERSAL1A,  szBlank);
		SetDlgItemText(par, IDC_STATIC_REVERSAL2,  szBlank);
		SetDlgItemText(par, IDC_STATIC_REVERSAL2A,  szBlank);
		SetDlgItemText(par, IDC_STATIC_REVERSAL3,  szBlank);
		SetDlgItemText(par, IDC_STATIC_REVERSAL3A,  szBlank);
		SetDlgItemText(par, IDC_STATIC_REVERSAL4,  szBlank);
		SetDlgItemText(par, IDC_STATIC_REVERSAL4A,  szBlank);
		SetDlgItemText(par, IDC_STATIC_REVERSAL5,  szBlank);
		SetDlgItemText(par, IDC_STATIC_REVERSAL5A,  szBlank);


		// Static text, enable/disable, then write proper text.
		EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL1), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL2), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL3), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL4), FALSE);
		EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL5), FALSE);

		sprintf_s(szBuff, sizeof(szBuff), szBlank);
		SetDlgItemText(par, IDC_EDIT_REVERSAL1,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), szBlank);
		SetDlgItemText(par, IDC_EDIT_REVERSAL2,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), szBlank);
		SetDlgItemText(par, IDC_EDIT_REVERSAL3,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), szBlank);
		SetDlgItemText(par, IDC_EDIT_REVERSAL4,  szBuff);
		sprintf_s(szBuff, sizeof(szBuff), szBlank);
		SetDlgItemText(par, IDC_EDIT_REVERSAL5,  szBuff);


		sprintf_s(szBuff, sizeof(szBuff), "%s = [----]%s%s = [----]%s%s = [----]%s%s = [----]",
			SZ_REV, CRLF, SZ_REV_PROB, CRLF, SZ_REV_TIME, CRLF, SZ_REV_TIME_STEP);
		SetDlgItemText(par, IDC_EDIT_REVERSAL_VECTOR, szBuff);

	}
	else // yes, animat reverses
	{

		SendMessage(GetDlgItem(par, IDC_RADIO_REVERSAL1), BM_SETCHECK, BST_CHECKED,0);
		SendMessage(GetDlgItem(par, IDC_RADIO_REVERSAL2), BM_SETCHECK, BST_UNCHECKED,0);
		//////////////////////////////////////////////////////////////////////

		EnableWindow(GetDlgItem(par, IDC_RADIO_REVERSAL3), TRUE);
		EnableWindow(GetDlgItem(par, IDC_RADIO_REVERSAL4), TRUE);
		if(sm->GetSubModelVectorStatus(&sm->m_behaviorModel_TOBEREMOVED) == VALID)
			EnableWindow(GetDlgItem(par, IDC_RADIO_REVERSAL5), TRUE);
		else
			EnableWindow(GetDlgItem(par, IDC_RADIO_REVERSAL5), FALSE);

		if(sm->m_reversalModel_TOBEREMOVED.modelType == VECTOR)
		{

			if(sm->GetSubModelVectorStatus(&sm->m_behaviorModel_TOBEREMOVED) == VALID)
			{
				sprintf_s(szBuff, sizeof(szBuff), "%s = [%dx%d]%s%s = [%dx%d]%s%s = [%dx%d]%s%s = [%dx%d]",
					SZ_REV, sm->m_reversalModel_TOBEREMOVED.vm.count.rowCnt, sm->m_reversalModel_TOBEREMOVED.vm.count.colCnt, CRLF,
					SZ_REV_PROB, sm->m_reversalModel_TOBEREMOVED.vm.probOfReversal.rowCnt, sm->m_reversalModel_TOBEREMOVED.vm.probOfReversal.colCnt, CRLF, 
					SZ_REV_TIME, sm->m_reversalModel_TOBEREMOVED.vm.time.rowCnt, sm->m_reversalModel_TOBEREMOVED.vm.time.rowCnt, CRLF, 
					SZ_REV_TIME_STEP, sm->m_reversalModel_TOBEREMOVED.vm.timeStep.rowCnt,
					sm->m_reversalModel_TOBEREMOVED.vm.timeStep.colCnt);

			}
			else
			{
				sprintf_s(szBuff, sizeof(szBuff), "%s = [----]%s%s = [----]%s%s = [----]%s%s = [----]",
					SZ_REV, CRLF, SZ_REV_PROB, CRLF, SZ_REV_TIME, CRLF, SZ_REV_TIME_STEP);
			}
			SetDlgItemText(par, IDC_EDIT_REVERSAL_VECTOR, szBuff);


			// model type can only be set to vector if the user clicks on radio box or
			// loads a model in.


			SendMessage(GetDlgItem(par,IDC_RADIO_REVERSAL3),BM_SETCHECK, BST_UNCHECKED,0);
			SendMessage(GetDlgItem(par,IDC_RADIO_REVERSAL4),BM_SETCHECK, BST_UNCHECKED,0);
			SendMessage(GetDlgItem(par,IDC_RADIO_REVERSAL5),BM_SETCHECK, BST_CHECKED,0);

			// Static and edit box
			EnableWindow(GetDlgItem(par, IDC_STATIC_VMREVERSAL), TRUE);
			EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL_VECTOR), TRUE);


			//*************//
			// Static text, write proper text.
			SetDlgItemText(par, IDC_STATIC_REVERSAL1,  szBlank);
			SetDlgItemText(par, IDC_STATIC_REVERSAL1A,  szBlank);
			SetDlgItemText(par, IDC_STATIC_REVERSAL2,  szBlank);
			SetDlgItemText(par, IDC_STATIC_REVERSAL2A,  szBlank);
			SetDlgItemText(par, IDC_STATIC_REVERSAL3,  szBlank);
			SetDlgItemText(par, IDC_STATIC_REVERSAL3A,  szBlank);
			SetDlgItemText(par, IDC_STATIC_REVERSAL4,  szBlank);
			SetDlgItemText(par, IDC_STATIC_REVERSAL4A,  szBlank);
			SetDlgItemText(par, IDC_STATIC_REVERSAL5,  szBlank);
			SetDlgItemText(par, IDC_STATIC_REVERSAL5A,  szBlank);

			// Static text, enable/disable, then write proper text.
			EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL1), FALSE);
			EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL2), FALSE);
			EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL3), FALSE);
			EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL4), FALSE);
			EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL5), FALSE);


			SetDlgItemText(par, IDC_EDIT_REVERSAL1,  szBlank);
			SetDlgItemText(par, IDC_EDIT_REVERSAL2,  szBlank);
			SetDlgItemText(par, IDC_EDIT_REVERSAL3,  szBlank);
			SetDlgItemText(par, IDC_EDIT_REVERSAL4,  szBlank);
			SetDlgItemText(par, IDC_EDIT_REVERSAL5,  szBlank);
			//*********//

		}
		else if(sm->m_reversalModel_TOBEREMOVED.modelType == UNIFORM)  // Uniform
		{
			sprintf_s(szBuff, sizeof(szBuff), "%s = [----]%s%s = [----]%s%s = [----]%s%s = [----]",
				SZ_REV, CRLF, SZ_REV_PROB, CRLF, SZ_REV_TIME, CRLF, SZ_REV_TIME_STEP);
			SetDlgItemText(par, IDC_EDIT_REVERSAL_VECTOR, szBuff);


			SendMessage(GetDlgItem(par,IDC_RADIO_REVERSAL3),BM_SETCHECK, BST_CHECKED,0);
			SendMessage(GetDlgItem(par,IDC_RADIO_REVERSAL4),BM_SETCHECK, BST_UNCHECKED,0);
			SendMessage(GetDlgItem(par,IDC_RADIO_REVERSAL5),BM_SETCHECK, BST_UNCHECKED,0);

			// Static and edit box
			EnableWindow(GetDlgItem(par, IDC_STATIC_VMREVERSAL), FALSE);
			EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL_VECTOR), FALSE);


			SetDlgItemText(par, IDC_STATIC_REVERSAL1,  szMin);
			SetDlgItemText(par, IDC_STATIC_REVERSAL1A, szBlank);
			SetDlgItemText(par, IDC_STATIC_REVERSAL2,  szMax);
			SetDlgItemText(par, IDC_STATIC_REVERSAL2A, szBlank);
			SetDlgItemText(par, IDC_STATIC_REVERSAL3,  szProb);
			SetDlgItemText(par, IDC_STATIC_REVERSAL3A, szBlank);
			SetDlgItemText(par, IDC_STATIC_REVERSAL4,  szMean);
			SetDlgItemText(par, IDC_STATIC_REVERSAL4A, szTime);
			SetDlgItemText(par, IDC_STATIC_REVERSAL5,  szStd);
			SetDlgItemText(par, IDC_STATIC_REVERSAL5A, szTime);


			// Static text, enable/disable, then write proper text.
			EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL1), TRUE);
			EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL2), TRUE);
			EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL3), TRUE);
			EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL4), TRUE);
			EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL5), TRUE);

			sprintf_s(szBuff, sizeof(szBuff), TEXT("%d"), sm->m_reversalModel_TOBEREMOVED.rnd.count.min);
			SetDlgItemText(par, IDC_EDIT_REVERSAL1,  szBuff);
			sprintf_s(szBuff, sizeof(szBuff), TEXT("%d"), sm->m_reversalModel_TOBEREMOVED.rnd.count.max);
			SetDlgItemText(par, IDC_EDIT_REVERSAL2,  szBuff);
			sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_reversalModel_TOBEREMOVED.rnd.probOfReversal);
			SetDlgItemText(par, IDC_EDIT_REVERSAL3,  szBuff);
			sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_reversalModel_TOBEREMOVED.rnd.time.mean);
			SetDlgItemText(par, IDC_EDIT_REVERSAL4,  szBuff);
			sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_reversalModel_TOBEREMOVED.rnd.time.std);
			SetDlgItemText(par, IDC_EDIT_REVERSAL5,  szBuff);
		}
		else // GAUSS_RANDOM_DISTRIBUTION4
		{
			sprintf_s(szBuff, sizeof(szBuff), "%s = [----]%s%s = [----]%s%s = [----]%s%s = [----]",
				SZ_REV, CRLF, SZ_REV_PROB, CRLF, SZ_REV_TIME, CRLF, SZ_REV_TIME_STEP);
			SetDlgItemText(par, IDC_EDIT_REVERSAL_VECTOR, szBuff);


			SendMessage(GetDlgItem(par,IDC_RADIO_REVERSAL3),BM_SETCHECK, BST_UNCHECKED,0);
			SendMessage(GetDlgItem(par,IDC_RADIO_REVERSAL4),BM_SETCHECK, BST_CHECKED,0);
			SendMessage(GetDlgItem(par,IDC_RADIO_REVERSAL5),BM_SETCHECK, BST_UNCHECKED,0);

			// Static and edit box
			EnableWindow(GetDlgItem(par, IDC_STATIC_VMREVERSAL), FALSE);
			EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL_VECTOR), FALSE);


			SetDlgItemText(par, IDC_STATIC_REVERSAL1,  szMean);
			SetDlgItemText(par, IDC_STATIC_REVERSAL1A, szCount);
			SetDlgItemText(par, IDC_STATIC_REVERSAL2,  szStd);
			SetDlgItemText(par, IDC_STATIC_REVERSAL2A, szCount);
			SetDlgItemText(par, IDC_STATIC_REVERSAL3,  szMean);
			SetDlgItemText(par, IDC_STATIC_REVERSAL3A, szTime);
			SetDlgItemText(par, IDC_STATIC_REVERSAL4,  szStd);
			SetDlgItemText(par, IDC_STATIC_REVERSAL4A, szTime);
			SetDlgItemText(par, IDC_STATIC_REVERSAL5,  szProb);
			SetDlgItemText(par, IDC_STATIC_REVERSAL5A, szBlank);


			// Static text, enable/disable, then write proper text.
			EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL1), TRUE);
			EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL2), TRUE);
			EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL3), TRUE);
			EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL4), TRUE);
			EnableWindow(GetDlgItem(par, IDC_EDIT_REVERSAL5), TRUE);

			sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_reversalModel_TOBEREMOVED.gauss.count.mean);
			SetDlgItemText(par, IDC_EDIT_REVERSAL1,  szBuff);
			sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_reversalModel_TOBEREMOVED.gauss.count.std);
			SetDlgItemText(par, IDC_EDIT_REVERSAL2,  szBuff);
			sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_reversalModel_TOBEREMOVED.gauss.time.mean);
			SetDlgItemText(par, IDC_EDIT_REVERSAL3,  szBuff);
			sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_reversalModel_TOBEREMOVED.gauss.time.std);
			SetDlgItemText(par, IDC_EDIT_REVERSAL4,  szBuff);
			sprintf_s(szBuff, sizeof(szBuff), TEXT("%.2f"), sm->m_reversalModel_TOBEREMOVED.gauss.probOfReversal);
			SetDlgItemText(par, IDC_EDIT_REVERSAL5,  szBuff);		
		}
	}

	if(sm->m_ae_TOBEREMOVED.diving == TRUE)
	{
		EnableWindow(GetDlgItem(par, IDC_RADIO_FLAT_BOTTOM_DIVE1), FALSE);
		EnableWindow(GetDlgItem(par, IDC_RADIO_FLAT_BOTTOM_DIVE2), FALSE);
		CheckRadioButton(par, IDC_RADIO_FLAT_BOTTOM_DIVE1, IDC_RADIO_FLAT_BOTTOM_DIVE2, IDC_RADIO_FLAT_BOTTOM_DIVE2);
	}
	else
	{
		EnableWindow(GetDlgItem(par, IDC_RADIO_FLAT_BOTTOM_DIVE1), TRUE);
		EnableWindow(GetDlgItem(par, IDC_RADIO_FLAT_BOTTOM_DIVE2), TRUE);

		if(sm->m_depthModel_TOBEREMOVED.bottomFollows == FALSE)
			CheckRadioButton(par, IDC_RADIO_FLAT_BOTTOM_DIVE1, IDC_RADIO_FLAT_BOTTOM_DIVE2, IDC_RADIO_FLAT_BOTTOM_DIVE2);
		else
			CheckRadioButton(par, IDC_RADIO_FLAT_BOTTOM_DIVE1, IDC_RADIO_FLAT_BOTTOM_DIVE2, IDC_RADIO_FLAT_BOTTOM_DIVE1);
	}
#endif
}

void GuiWindowsToSpecies(CSpeciesModel *sm, HWND parent)
{
	// This Function not currently not being used.

	parent = parent; // quiet compiler warning.
	sm = sm;
#if 0
	TCHAR szBuff[BUFFERED_MAX_PATH];
	int i = 0;

	if(sm->m_ae_TOBEREMOVED.direction == TRUE || sm->m_ae_TOBEREMOVED.diving == TRUE || sm->m_ae_TOBEREMOVED.travel == TRUE)
	{
		GetDlgItemText(parent, IDC_EDIT_AE_THESHOLD,  szBuff, BUFFERED_MAX_PATH);
		sm->m_ae_TOBEREMOVED.exposureThreshold = atof(szBuff);
	}

	//-----------//
	// Dive Depth
	//-----------//
	if(sm->m_depthModel_TOBEREMOVED.modelType == VECTOR)
	{
		i = i; // do nothing.
	}
	else if(sm->m_depthModel_TOBEREMOVED.modelType == UNIFORM)  // random
	{
		GetDlgItemText(parent, IDC_EDIT_DIVEDEPTH1,  szBuff, BUFFERED_MAX_PATH);
		sm->m_depthModel_TOBEREMOVED.rnd.max = atof(szBuff);
	}
	else if(sm->m_depthModel_TOBEREMOVED.modelType == GAUSSIAN)
	{
		GetDlgItemText(parent, IDC_EDIT_DIVEDEPTH1,  szBuff, BUFFERED_MAX_PATH);
		sm->m_depthModel_TOBEREMOVED.gauss.mean = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_DIVEDEPTH2,  szBuff, BUFFERED_MAX_PATH);
		sm->m_depthModel_TOBEREMOVED.gauss.std = atof(szBuff);
	}

	//------------------------------//
	// Dive Depth Accoustic Exposure
	//------------------------------//
	if(sm->m_ae_TOBEREMOVED.diving)
	{
		GetDlgItemText(parent, IDC_EDIT_DIVEDEPTH_AE_MEAN,  szBuff, BUFFERED_MAX_PATH);
		sm->m_depthModel_TOBEREMOVED.ae.mean = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_DIVEDEPTH_AE_STD,  szBuff, BUFFERED_MAX_PATH);
		sm->m_depthModel_TOBEREMOVED.ae.std = atof(szBuff);
	}

	//----------//
	// Dive Rate
	//----------//
	if(sm->m_diveRateModel_TOBEREMOVED.modelType == VECTOR)
	{
		i = i; // do nothing.
	}
	else if(sm->m_diveRateModel_TOBEREMOVED.modelType == UNIFORM)
	{
		GetDlgItemText(parent, IDC_EDIT_DIVERATE1,  szBuff, BUFFERED_MAX_PATH);
		sm->m_diveRateModel_TOBEREMOVED.rnd.min = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_DIVERATE2,  szBuff, BUFFERED_MAX_PATH);
		sm->m_diveRateModel_TOBEREMOVED.rnd.max = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_DIVERATE3, szBuff, BUFFERED_MAX_PATH);
		sm->m_diveRateModel_TOBEREMOVED.rnd.coeff = atof(szBuff);
	}
	else if(sm->m_diveRateModel_TOBEREMOVED.modelType == GAUSSIAN)
	{
		GetDlgItemText(parent, IDC_EDIT_DIVERATE1,  szBuff, BUFFERED_MAX_PATH);
		sm->m_diveRateModel_TOBEREMOVED.gauss.mean = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_DIVERATE2,  szBuff, BUFFERED_MAX_PATH);
		sm->m_diveRateModel_TOBEREMOVED.gauss.std = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_DIVERATE3, szBuff, BUFFERED_MAX_PATH);
		sm->m_diveRateModel_TOBEREMOVED.gauss.coeff = atof(szBuff);
	}


	//------------//
	// Ascent Rate
	//------------//
	if(sm->m_ascentRateModel_TOBEREMOVED.modelType == VECTOR)
	{
		i = i; // do nothing.
	}
	else if(sm->m_ascentRateModel_TOBEREMOVED.modelType == UNIFORM)
	{
		GetDlgItemText(parent, IDC_EDIT_ACENTRATE1,  szBuff, BUFFERED_MAX_PATH);
		sm->m_ascentRateModel_TOBEREMOVED.rnd.min = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_ACENTRATE2,  szBuff, BUFFERED_MAX_PATH);
		sm->m_ascentRateModel_TOBEREMOVED.rnd.max = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_ACENTRATE3, szBuff, BUFFERED_MAX_PATH);
		sm->m_ascentRateModel_TOBEREMOVED.rnd.coeff = atof(szBuff);
	}
	else if(sm->m_ascentRateModel_TOBEREMOVED.modelType == GAUSSIAN)
	{
		GetDlgItemText(parent, IDC_EDIT_ACENTRATE1,  szBuff, BUFFERED_MAX_PATH);
		sm->m_ascentRateModel_TOBEREMOVED.gauss.mean = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_ACENTRATE2,  szBuff, BUFFERED_MAX_PATH);
		sm->m_ascentRateModel_TOBEREMOVED.gauss.std = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_ACENTRATE3, szBuff, BUFFERED_MAX_PATH);
		sm->m_ascentRateModel_TOBEREMOVED.gauss.coeff = atof(szBuff);
	}

	//----------------------------------------//
	// Dive and Ascent Rate Accoustic Exposure
	//----------------------------------------//
	if(sm->m_ae_TOBEREMOVED.diving == TRUE)
	{
		// Dive Rate
		GetDlgItemText(parent, IDC_EDIT_AE_DIVERATE_MEAN,  szBuff, BUFFERED_MAX_PATH);
		sm->m_diveRateModel_TOBEREMOVED.ae.mean = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_AE_DIVERATE_STD,  szBuff, BUFFERED_MAX_PATH);
		sm->m_diveRateModel_TOBEREMOVED.ae.std = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_AE_DIVERATE_COEFF, szBuff, BUFFERED_MAX_PATH);
		sm->m_diveRateModel_TOBEREMOVED.ae.coeff = atof(szBuff);

		// Ascent Rate
		GetDlgItemText(parent, IDC_EDIT_AE_ACENTRATE_MEAN,  szBuff, BUFFERED_MAX_PATH);
		sm->m_ascentRateModel_TOBEREMOVED.ae.mean = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_AE_ACENTRATE_STD,  szBuff, BUFFERED_MAX_PATH);
		sm->m_ascentRateModel_TOBEREMOVED.ae.std = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_AE_ACENTRATE_COEFF, szBuff, BUFFERED_MAX_PATH);
		sm->m_ascentRateModel_TOBEREMOVED.ae.coeff = atof(szBuff);
	}


	//-------//
	// Travel
	//-------//
	if(sm->m_travelModel_TOBEREMOVED.modelType == VECTOR)
	{
		i = i; // do nothing.
	}
	else if(sm->m_travelModel_TOBEREMOVED.modelType == UNIFORM)
	{
		GetDlgItemText(parent, IDC_EDIT_TRAVEL1,  szBuff, BUFFERED_MAX_PATH);
		sm->m_travelModel_TOBEREMOVED.rnd.min = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_TRAVEL2,  szBuff, BUFFERED_MAX_PATH);
		sm->m_travelModel_TOBEREMOVED.rnd.max = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_TRAVEL3, szBuff, BUFFERED_MAX_PATH);
		sm->m_travelModel_TOBEREMOVED.rnd.coeff = atof(szBuff);
	}
	else //GAUSSIAN
	{
		GetDlgItemText(parent, IDC_EDIT_TRAVEL1,  szBuff, BUFFERED_MAX_PATH);
		sm->m_travelModel_TOBEREMOVED.gauss.mean = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_TRAVEL2,  szBuff, BUFFERED_MAX_PATH);
		sm->m_travelModel_TOBEREMOVED.gauss.std = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_TRAVEL3, szBuff, BUFFERED_MAX_PATH);
		sm->m_travelModel_TOBEREMOVED.gauss.coeff = atof(szBuff);
	}

	//--------------------------//
	// Travel Accoustic Exposure
	//--------------------------//
	if(sm->m_ae_TOBEREMOVED.travel == TRUE)
	{
		GetDlgItemText(parent, IDC_EDIT_TRAVEL_AE_MEAN,  szBuff, BUFFERED_MAX_PATH);
		sm->m_travelModel_TOBEREMOVED.ae.mean = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_AE_STD,  szBuff, BUFFERED_MAX_PATH);
		sm->m_travelModel_TOBEREMOVED.ae.std = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_TRAVEL_AE_COEFF,  szBuff, BUFFERED_MAX_PATH);
		sm->m_travelModel_TOBEREMOVED.ae.coeff = atof(szBuff);
	}

	//-----------------//
	// Surface Interval
	//-----------------//
	_ASSERTE(sm->m_surfaceIntervalModel_TOBEREMOVED.modelType != UNIFORM);
	if(sm->m_surfaceIntervalModel_TOBEREMOVED.modelType == GAUSSIAN)
	{
		GetDlgItemText(parent, IDC_EDIT_SI1,  szBuff, BUFFERED_MAX_PATH);
		sm->m_surfaceIntervalModel_TOBEREMOVED.gauss.mean = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_SI2,  szBuff, BUFFERED_MAX_PATH);
		sm->m_surfaceIntervalModel_TOBEREMOVED.gauss.std = atof(szBuff);
	}

	//------------------------------------//
	// Surface Interval Accoustic Exposure
	//------------------------------------//
	if(sm->m_ae_TOBEREMOVED.diving == TRUE)
	{
		GetDlgItemText(parent, IDC_EDIT_SURFACE_INTERVAL_AE_MEAN,  szBuff, BUFFERED_MAX_PATH);
		sm->m_surfaceIntervalModel_TOBEREMOVED.ae.mean = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_SURFACE_INTERVAL_AE_STD,  szBuff, BUFFERED_MAX_PATH);
		sm->m_surfaceIntervalModel_TOBEREMOVED.ae.std = atof(szBuff);
	}


	//----------//
	// Direction
	//----------//
	if(sm->m_directionModel_TOBEREMOVED.modelType == VECTOR_MODEL_DIRECTIONAL_NO_BIASING ||
		sm->m_directionModel_TOBEREMOVED.modelType == 
		VECTOR_MODEL_DIRECTIONAL_WITH_VECTOR_MODEL_BIASING)
	{
		i = i; // do nothing.
	}
	else if(sm->m_directionModel_TOBEREMOVED.modelType == RANDOM_WALK)
	{
		GetDlgItemText(parent, IDC_EDIT_DIRECTION_CHANGE1,  szBuff, BUFFERED_MAX_PATH);
		sm->m_directionModel_TOBEREMOVED.rndWalk.termCoeff = atof(szBuff);
	}
	else if(sm->m_directionModel_TOBEREMOVED.modelType == CORRELATED_RANDOM_WALK)
	{
		GetDlgItemText(parent, IDC_EDIT_DIRECTION_CHANGE1,  szBuff, BUFFERED_MAX_PATH);
		sm->m_directionModel_TOBEREMOVED.crRndWalk.perturbation = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_DIRECTION_CHANGE2,  szBuff, BUFFERED_MAX_PATH);
		sm->m_directionModel_TOBEREMOVED.crRndWalk.termCoeff = atof(szBuff);
	}
	else //CORRELATED_RANDOM_WALK or CORRELATED_RANDOM_WALK_WITH_DIR_BIASING
	{
		GetDlgItemText(parent, IDC_EDIT_DIRECTION_CHANGE1,  szBuff, BUFFERED_MAX_PATH);
		sm->m_directionModel_TOBEREMOVED.crRndWalkDb.perturbation = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_DIRECTION_CHANGE2,  szBuff, BUFFERED_MAX_PATH);
		sm->m_directionModel_TOBEREMOVED.crRndWalkDb.directionOfBias = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_DIRECTION_CHANGE3,  szBuff, BUFFERED_MAX_PATH);
		sm->m_directionModel_TOBEREMOVED.crRndWalkDb.bias = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_DIRECTION_CHANGE4,  szBuff, BUFFERED_MAX_PATH);
		sm->m_directionModel_TOBEREMOVED.crRndWalkDb.arcStep = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_DIRECTION_CHANGE5,  szBuff, BUFFERED_MAX_PATH);
		sm->m_directionModel_TOBEREMOVED.crRndWalkDb.termCoeff = atof(szBuff);
	}

	//-----------------------------//
	// Direction Accoustic Exposure
	//-----------------------------//
	if(sm->m_ae_TOBEREMOVED.direction == TRUE)
	{
		GetDlgItemText(parent, IDC_EDIT_AE_DIRECTION_CHANGE_PERTURBATION,  szBuff, BUFFERED_MAX_PATH);
		sm->m_directionModel_TOBEREMOVED.ae.perturbation = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_AE_DIRECTION_CHANGE_TERM_PROB,  szBuff, BUFFERED_MAX_PATH);
		sm->m_directionModel_TOBEREMOVED.ae.termCoeff = atof(szBuff);
	}


	//---------//
	// Reversal
	//---------//
	if(sm->m_reversalModel_TOBEREMOVED.modelType == VECTOR || sm->m_reversalModel_TOBEREMOVED.reverses == FALSE)
	{
		i = i; // do nothing.
	}
	else if(sm->m_reversalModel_TOBEREMOVED.modelType == UNIFORM)
	{
		GetDlgItemText(parent, IDC_EDIT_REVERSAL1,  szBuff, BUFFERED_MAX_PATH);
		sm->m_reversalModel_TOBEREMOVED.rnd.count.min = atoi(szBuff);
		GetDlgItemText(parent, IDC_EDIT_REVERSAL2,  szBuff, BUFFERED_MAX_PATH);
		sm->m_reversalModel_TOBEREMOVED.rnd.count.max = atoi(szBuff);
		GetDlgItemText(parent, IDC_EDIT_REVERSAL3,  szBuff, BUFFERED_MAX_PATH);
		sm->m_reversalModel_TOBEREMOVED.rnd.probOfReversal = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_REVERSAL4,  szBuff, BUFFERED_MAX_PATH);
		sm->m_reversalModel_TOBEREMOVED.rnd.time.mean = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_REVERSAL5,  szBuff, BUFFERED_MAX_PATH);
		sm->m_reversalModel_TOBEREMOVED.rnd.time.std = atof(szBuff);
	}
	else // Gaussian
	{
		GetDlgItemText(parent, IDC_EDIT_REVERSAL1,  szBuff, BUFFERED_MAX_PATH);
		sm->m_reversalModel_TOBEREMOVED.gauss.count.mean = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_REVERSAL2,  szBuff, BUFFERED_MAX_PATH);
		sm->m_reversalModel_TOBEREMOVED.gauss.count.std = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_REVERSAL3,  szBuff, BUFFERED_MAX_PATH);
		sm->m_reversalModel_TOBEREMOVED.gauss.time.mean = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_REVERSAL4,  szBuff, BUFFERED_MAX_PATH);
		sm->m_reversalModel_TOBEREMOVED.gauss.time.std = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_REVERSAL5,  szBuff, BUFFERED_MAX_PATH);
		sm->m_reversalModel_TOBEREMOVED.gauss.probOfReversal = atof(szBuff);
	}// End Reversal.

	//----------------------------//
	// Reversal Accoustic Exposure
	//----------------------------//
	if((sm->m_ae_TOBEREMOVED.diving == TRUE) && (sm->m_reversalModel_TOBEREMOVED.reverses == TRUE))
	{
		GetDlgItemText(parent, IDC_EDIT_REVERSAL_AE_NUM_MEAN,  szBuff, BUFFERED_MAX_PATH);
		sm->m_reversalModel_TOBEREMOVED.ae.count.mean = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_REVERSAL_AE_NUM_STD,  szBuff, BUFFERED_MAX_PATH);
		sm->m_reversalModel_TOBEREMOVED.ae.count.std = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_REVERSAL_AE_DURATION_MEAN,  szBuff, BUFFERED_MAX_PATH);
		sm->m_reversalModel_TOBEREMOVED.ae.time.mean = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_REVERSAL_AE_DURATION_STD,  szBuff, BUFFERED_MAX_PATH);
		sm->m_reversalModel_TOBEREMOVED.ae.time.std = atof(szBuff);
		GetDlgItemText(parent, IDC_EDIT_REVERSAL_AE_PROBABILITY,  szBuff, BUFFERED_MAX_PATH);
		sm->m_reversalModel_TOBEREMOVED.ae.probOfReversal = atof(szBuff);
	}

	// Flat bottom Dive
	if(sm->m_depthModel_TOBEREMOVED.bottomFollows == FALSE)
		i = i; // do nothing.
	else
		i = i; // do nothing.
#endif
}

