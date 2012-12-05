#include "resource.h"
#include "3mbSpeciesVectorMdl.h"

# if 0
// someday look into this.
#include <string.h>
#include <iostream>
using namespace std ;
string s;
#endif


LRESULT CALLBACK SpeciesVectorModelProc(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
	
	// This Function not currently not being used.

	hDlg = hDlg; // quiet compiler warning.

	static	int modelType;
	static	CSpeciesModel smCopy;
			CSpeciesModel *pSmCopy;

	static  CSpeciesModel *smOriginal;
			int wmId, wmEvent;
//			TCHAR szBuff1[SIZE_8192];

	//ShowWindow(GetDlgItem(hDlg, IDC_STATIC_VECTORMDL4), SW_HIDE);

	pSmCopy = &smCopy;

	switch (message)
	{
	case WM_INITDIALOG:
		smOriginal  = ((MBSSPECIESVECTORMDLPROCPARAM *)lParam)->pSpeciesModel;
		smCopy.CopyModel(&smCopy, smOriginal);
		modelType = ((MBSSPECIESVECTORMDLPROCPARAM *)lParam)->modelType;
		pSmCopy = &smCopy;
#if 0
		switch(modelType)
		{
		case IDC_BUTTON_BEH_VECTOR:
			SetDlgItemText(hDlg, IDC_STATIC_VECTORMDL1, "behavior");
			pSmCopy->MatrixToText(&pSmCopy->m_behaviorModel_TOBEREMOVED.behavior, szBuff1, SIZE_8192);
			SetDlgItemText(hDlg, IDC_EDIT1, szBuff1);

			SetDlgItemText(hDlg, IDC_STATIC_VECTORMDL2, "initial_behavior");
			pSmCopy->MatrixToText(&pSmCopy->m_behaviorModel_TOBEREMOVED.initial, szBuff1, SIZE_8192);
			SetDlgItemText(hDlg, IDC_EDIT2, szBuff1);

			SetDlgItemText(hDlg, IDC_STATIC_VECTORMDL3, "terminate_behavior");
			pSmCopy->MatrixToText(&pSmCopy->m_behaviorModel_TOBEREMOVED.terminate, szBuff1, SIZE_8192);
			SetDlgItemText(hDlg, IDC_EDIT3, szBuff1);
			break;

		case IDC_BUTTON_DIVEDEPTH_VECTOR:
			SetDlgItemText(hDlg, IDC_STATIC_VECTORMDL1, "dive");
			pSmCopy->MatrixToText(&pSmCopy->m_depthModel_TOBEREMOVED.vm.vector, szBuff1, SIZE_8192);
			SetDlgItemText(hDlg, IDC_EDIT1, szBuff1);

			SetDlgItemText(hDlg, IDC_STATIC_VECTORMDL2, "depth_step_value");
			pSmCopy->MatrixToText(&pSmCopy->m_depthModel_TOBEREMOVED.vm.step, szBuff1, SIZE_8192);
			SetDlgItemText(hDlg, IDC_EDIT2, szBuff1);
			break;

		case IDC_BUTTON_SURFINTVL_VECTOR:
			SetDlgItemText(hDlg, IDC_STATIC_VECTORMDL1, "surface_interval");
			pSmCopy->MatrixToText(&pSmCopy->m_surfaceIntervalModel_TOBEREMOVED.vm.vector, szBuff1, SIZE_8192);
			SetDlgItemText(hDlg, IDC_EDIT1, szBuff1);

			SetDlgItemText(hDlg, IDC_STATIC_VECTORMDL2, "surface_interval_step_value");
			pSmCopy->MatrixToText(&pSmCopy->m_surfaceIntervalModel_TOBEREMOVED.vm.step, szBuff1, SIZE_8192);
			SetDlgItemText(hDlg, IDC_EDIT2, szBuff1);
			break;

		case IDC_BUTTON_TRAVEL_VECTOR:
			SetDlgItemText(hDlg, IDC_STATIC_VECTORMDL1, "rate_of_travel");
			SetDlgItemText(hDlg, IDC_STATIC_VECTORMDL2, "rate_of_travel_step_value");
			SetDlgItemText(hDlg, IDC_STATIC_VECTORMDL3, "terminate_rate_of_travel");

			pSmCopy->MatrixToText(&pSmCopy->m_travelModel_TOBEREMOVED.vm.vector, szBuff1, SIZE_8192);
			SetDlgItemText(hDlg, IDC_EDIT1, szBuff1);

			pSmCopy->MatrixToText(&pSmCopy->m_travelModel_TOBEREMOVED.vm.step, szBuff1, SIZE_8192);
			SetDlgItemText(hDlg, IDC_EDIT2, szBuff1);

			pSmCopy->MatrixToText(&pSmCopy->m_travelModel_TOBEREMOVED.vm.step, szBuff1, SIZE_8192);
			SetDlgItemText(hDlg, IDC_EDIT3, szBuff1);
			break;

		case IDC_BUTTON_DIVERATE_VECTOR:
			SetDlgItemText(hDlg, IDC_STATIC_VECTORMDL1, "rate_of_dive");
			SetDlgItemText(hDlg, IDC_STATIC_VECTORMDL2, "rate_of_dive_step_value");
			SetDlgItemText(hDlg, IDC_STATIC_VECTORMDL3, "terminate_rate_of_dive");

			pSmCopy->MatrixToText(&pSmCopy->m_diveRateModel_TOBEREMOVED.vm.vector, szBuff1, SIZE_8192);
			SetDlgItemText(hDlg, IDC_EDIT1, szBuff1);

			pSmCopy->MatrixToText(&pSmCopy->m_diveRateModel_TOBEREMOVED.vm.step, szBuff1, SIZE_8192);
			SetDlgItemText(hDlg, IDC_EDIT2, szBuff1);

			pSmCopy->MatrixToText(&pSmCopy->m_diveRateModel_TOBEREMOVED.vm.terminate, szBuff1, SIZE_8192);
			SetDlgItemText(hDlg, IDC_EDIT3, szBuff1);
			break;

		case IDC_BUTTON_ACENTRATE_VECTOR:
			SetDlgItemText(hDlg, IDC_STATIC_VECTORMDL1, "rate_of_ascent");
			SetDlgItemText(hDlg, IDC_STATIC_VECTORMDL2, "rate_of_ascent_step_value");
			SetDlgItemText(hDlg, IDC_STATIC_VECTORMDL3, "terminate_rate_of_ascent");

			pSmCopy->MatrixToText(&pSmCopy->m_ascentRateModel_TOBEREMOVED.vm.vector, szBuff1, SIZE_8192);
			SetDlgItemText(hDlg, IDC_EDIT1, szBuff1);

			pSmCopy->MatrixToText(&pSmCopy->m_ascentRateModel_TOBEREMOVED.vm.step, szBuff1, SIZE_8192);
			SetDlgItemText(hDlg, IDC_EDIT2, szBuff1);

			pSmCopy->MatrixToText(&pSmCopy->m_ascentRateModel_TOBEREMOVED.vm.terminate, szBuff1, SIZE_8192);
			SetDlgItemText(hDlg, IDC_EDIT3, szBuff1);
			break;

		case IDC_BUTTON_REVERSAL_VECTOR:
			SetDlgItemText(hDlg, IDC_STATIC_VECTORMDL1, "reversals");
			SetDlgItemText(hDlg, IDC_STATIC_VECTORMDL2, "prob_reversals");
			SetDlgItemText(hDlg, IDC_STATIC_VECTORMDL3, "time_reversed");
			SetDlgItemText(hDlg, IDC_STATIC_VECTORMDL4, "time_reversed_step_value");

			pSmCopy->MatrixToText(&pSmCopy->m_reversalModel_TOBEREMOVED.vm.count, szBuff1, SIZE_8192);
			SetDlgItemText(hDlg, IDC_EDIT1, szBuff1);

			pSmCopy->MatrixToText(&pSmCopy->m_reversalModel_TOBEREMOVED.vm.probOfReversal, szBuff1, SIZE_8192);
			SetDlgItemText(hDlg, IDC_EDIT2, szBuff1);

			pSmCopy->MatrixToText(&pSmCopy->m_reversalModel_TOBEREMOVED.vm.time, szBuff1, SIZE_8192);
			SetDlgItemText(hDlg, IDC_EDIT3, szBuff1);

			pSmCopy->MatrixToText(&pSmCopy->m_reversalModel_TOBEREMOVED.vm.timeStep, szBuff1, SIZE_8192);
			SetDlgItemText(hDlg, IDC_EDIT4, szBuff1);
			break;

		case IDC_BUTTON_DIRECTION_VECTOR:
			SetDlgItemText(hDlg, IDC_STATIC_VECTORMDL1, "direction");
			SetDlgItemText(hDlg, IDC_STATIC_VECTORMDL2, "direction_bias");
			SetDlgItemText(hDlg, IDC_STATIC_VECTORMDL3, "terminate_direction");

			pSmCopy->MatrixToText(&pSmCopy->m_directionModel_TOBEREMOVED.vm.direction, szBuff1, SIZE_8192);
			SetDlgItemText(hDlg, IDC_EDIT1, szBuff1);

			pSmCopy->MatrixToText(&pSmCopy->m_directionModel_TOBEREMOVED.vm.directionalBias, szBuff1, SIZE_8192);
			SetDlgItemText(hDlg, IDC_EDIT2, szBuff1);

			pSmCopy->MatrixToText(&pSmCopy->m_directionModel_TOBEREMOVED.vm.terminate, szBuff1, SIZE_8192);
			SetDlgItemText(hDlg, IDC_EDIT3, szBuff1);
			break;
		}
#endif
		break;



	case WM_COMMAND:
		wmId    = LOWORD(wParam); 
		wmEvent = HIWORD(wParam);
#if 0
		switch(wmId)
		{
		case IDOK:
		case ID_BUTTON_REFRESH:
			switch(modelType)
			{
			case IDC_BUTTON_BEH_VECTOR:

				// Convert the GUI text into a matrix.
				GetDlgItemText(hDlg, IDC_EDIT1, szBuff1, SIZE_8192);
				pSmCopy->TextToMatrix(&pSmCopy->m_behaviorModel_TOBEREMOVED.behavior, szBuff1);

				GetDlgItemText(hDlg, IDC_EDIT2, szBuff1, SIZE_8192);
				pSmCopy->TextToMatrix(&pSmCopy->m_behaviorModel_TOBEREMOVED.initial, szBuff1);

				GetDlgItemText(hDlg, IDC_EDIT3, szBuff1, SIZE_8192);
				pSmCopy->TextToMatrix(&pSmCopy->m_behaviorModel_TOBEREMOVED.terminate, szBuff1);


				// Place the matrix back into the GUI text
				pSmCopy->MatrixToText(&pSmCopy->m_behaviorModel_TOBEREMOVED.behavior, szBuff1, SIZE_8192);
				SetDlgItemText(hDlg, IDC_EDIT1, szBuff1);

				pSmCopy->MatrixToText(&pSmCopy->m_behaviorModel_TOBEREMOVED.initial, szBuff1, SIZE_8192);
				SetDlgItemText(hDlg, IDC_EDIT2, szBuff1);

				pSmCopy->MatrixToText(&pSmCopy->m_behaviorModel_TOBEREMOVED.terminate, szBuff1, SIZE_8192);
				SetDlgItemText(hDlg, IDC_EDIT3, szBuff1);
				break;

			case IDC_BUTTON_DIVEDEPTH_VECTOR:
				// Convert the GUI text into a matrix.
				GetDlgItemText(hDlg, IDC_EDIT1, szBuff1, SIZE_8192);
				pSmCopy->TextToMatrix(&pSmCopy->m_depthModel_TOBEREMOVED.vm.vector, szBuff1);

				GetDlgItemText(hDlg, IDC_EDIT2, szBuff1, SIZE_8192);
				pSmCopy->TextToMatrix(&pSmCopy->m_depthModel_TOBEREMOVED.vm.step, szBuff1);


				// Place the matrix back into the GUI text
				pSmCopy->MatrixToText(&pSmCopy->m_depthModel_TOBEREMOVED.vm.vector, szBuff1, SIZE_8192);
				SetDlgItemText(hDlg, IDC_EDIT1, szBuff1);

				pSmCopy->MatrixToText(&pSmCopy->m_depthModel_TOBEREMOVED.vm.step, szBuff1, SIZE_8192);
				SetDlgItemText(hDlg, IDC_EDIT2, szBuff1);
				break;

			case IDC_BUTTON_SURFINTVL_VECTOR:
				// Convert the GUI text into a matrix.
				GetDlgItemText(hDlg, IDC_EDIT1, szBuff1, SIZE_8192);
				pSmCopy->TextToMatrix(&pSmCopy->m_surfaceIntervalModel_TOBEREMOVED.vm.vector, szBuff1);

				GetDlgItemText(hDlg, IDC_EDIT2, szBuff1, SIZE_8192);
				pSmCopy->TextToMatrix(&pSmCopy->m_surfaceIntervalModel_TOBEREMOVED.vm.step, szBuff1);


				// Place the matrix back into the GUI text
				pSmCopy->MatrixToText(&pSmCopy->m_surfaceIntervalModel_TOBEREMOVED.vm.vector, szBuff1, SIZE_8192);
				SetDlgItemText(hDlg, IDC_EDIT1, szBuff1);

				pSmCopy->MatrixToText(&pSmCopy->m_surfaceIntervalModel_TOBEREMOVED.vm.step, szBuff1, SIZE_8192);
				SetDlgItemText(hDlg, IDC_EDIT2, szBuff1);
				break;

			case IDC_BUTTON_TRAVEL_VECTOR:
				// Convert the GUI text into a matrix.
				GetDlgItemText(hDlg, IDC_EDIT1, szBuff1, SIZE_8192);
				pSmCopy->TextToMatrix(&pSmCopy->m_travelModel_TOBEREMOVED.vm.vector, szBuff1);

				GetDlgItemText(hDlg, IDC_EDIT2, szBuff1, SIZE_8192);
				pSmCopy->TextToMatrix(&pSmCopy->m_travelModel_TOBEREMOVED.vm.step, szBuff1);

				GetDlgItemText(hDlg, IDC_EDIT3, szBuff1, SIZE_8192);
				pSmCopy->TextToMatrix(&pSmCopy->m_travelModel_TOBEREMOVED.vm.step, szBuff1);


				// Place the matrix back into the GUI text
				pSmCopy->MatrixToText(&pSmCopy->m_travelModel_TOBEREMOVED.vm.vector, szBuff1, SIZE_8192);
				SetDlgItemText(hDlg, IDC_EDIT1, szBuff1);

				pSmCopy->MatrixToText(&pSmCopy->m_travelModel_TOBEREMOVED.vm.step, szBuff1, SIZE_8192);
				SetDlgItemText(hDlg, IDC_EDIT2, szBuff1);

				pSmCopy->MatrixToText(&pSmCopy->m_travelModel_TOBEREMOVED.vm.step, szBuff1, SIZE_8192);
				SetDlgItemText(hDlg, IDC_EDIT3, szBuff1);
				break;

			case IDC_BUTTON_DIVERATE_VECTOR:
				// Convert the GUI text into a matrix.
				GetDlgItemText(hDlg, IDC_EDIT1, szBuff1, SIZE_8192);
				pSmCopy->TextToMatrix(&pSmCopy->m_diveRateModel_TOBEREMOVED.vm.vector, szBuff1);

				GetDlgItemText(hDlg, IDC_EDIT2, szBuff1, SIZE_8192);
				pSmCopy->TextToMatrix(&pSmCopy->m_diveRateModel_TOBEREMOVED.vm.step, szBuff1);

				GetDlgItemText(hDlg, IDC_EDIT3, szBuff1, SIZE_8192);
				pSmCopy->TextToMatrix(&pSmCopy->m_diveRateModel_TOBEREMOVED.vm.terminate, szBuff1);


				// Place the matrix back into the GUI text
				pSmCopy->MatrixToText(&pSmCopy->m_diveRateModel_TOBEREMOVED.vm.vector, szBuff1, SIZE_8192);
				SetDlgItemText(hDlg, IDC_EDIT1, szBuff1);

				pSmCopy->MatrixToText(&pSmCopy->m_diveRateModel_TOBEREMOVED.vm.step, szBuff1, SIZE_8192);
				SetDlgItemText(hDlg, IDC_EDIT2, szBuff1);

				pSmCopy->MatrixToText(&pSmCopy->m_diveRateModel_TOBEREMOVED.vm.terminate, szBuff1, SIZE_8192);
				SetDlgItemText(hDlg, IDC_EDIT3, szBuff1);
				break;

			case IDC_BUTTON_ACENTRATE_VECTOR:
				// Convert the GUI text into a matrix.
				GetDlgItemText(hDlg, IDC_EDIT1, szBuff1, SIZE_8192);
				pSmCopy->TextToMatrix(&pSmCopy->m_ascentRateModel_TOBEREMOVED.vm.vector, szBuff1);

				GetDlgItemText(hDlg, IDC_EDIT2, szBuff1, SIZE_8192);
				pSmCopy->TextToMatrix(&pSmCopy->m_ascentRateModel_TOBEREMOVED.vm.step, szBuff1);

				GetDlgItemText(hDlg, IDC_EDIT3, szBuff1, SIZE_8192);
				pSmCopy->TextToMatrix(&pSmCopy->m_ascentRateModel_TOBEREMOVED.vm.terminate, szBuff1);


				// Place the matrix back into the GUI text
				pSmCopy->MatrixToText(&pSmCopy->m_ascentRateModel_TOBEREMOVED.vm.vector, szBuff1, SIZE_8192);
				SetDlgItemText(hDlg, IDC_EDIT1, szBuff1);

				pSmCopy->MatrixToText(&pSmCopy->m_ascentRateModel_TOBEREMOVED.vm.step, szBuff1, SIZE_8192);
				SetDlgItemText(hDlg, IDC_EDIT2, szBuff1);

				pSmCopy->MatrixToText(&pSmCopy->m_ascentRateModel_TOBEREMOVED.vm.terminate, szBuff1, SIZE_8192);
				SetDlgItemText(hDlg, IDC_EDIT3, szBuff1);
				break;

			case IDC_BUTTON_REVERSAL_VECTOR:
				// Convert the GUI text into a matrix.
				GetDlgItemText(hDlg, IDC_EDIT1, szBuff1, SIZE_8192);
				pSmCopy->TextToMatrix(&pSmCopy->m_reversalModel_TOBEREMOVED.vm.count, szBuff1);

				GetDlgItemText(hDlg, IDC_EDIT2, szBuff1, SIZE_8192);
				pSmCopy->TextToMatrix(&pSmCopy->m_reversalModel_TOBEREMOVED.vm.probOfReversal, szBuff1);

				GetDlgItemText(hDlg, IDC_EDIT3, szBuff1, SIZE_8192);
				pSmCopy->TextToMatrix(&pSmCopy->m_reversalModel_TOBEREMOVED.vm.time, szBuff1);

				GetDlgItemText(hDlg, IDC_EDIT4, szBuff1, SIZE_8192);
				pSmCopy->TextToMatrix(&pSmCopy->m_reversalModel_TOBEREMOVED.vm.timeStep, szBuff1);


				// Place the matrix back into the GUI text
				pSmCopy->MatrixToText(&pSmCopy->m_reversalModel_TOBEREMOVED.vm.count, szBuff1, SIZE_8192);
				SetDlgItemText(hDlg, IDC_EDIT1, szBuff1);

				pSmCopy->MatrixToText(&pSmCopy->m_reversalModel_TOBEREMOVED.vm.probOfReversal, szBuff1, SIZE_8192);
				SetDlgItemText(hDlg, IDC_EDIT2, szBuff1);

				pSmCopy->MatrixToText(&pSmCopy->m_reversalModel_TOBEREMOVED.vm.time, szBuff1, SIZE_8192);
				SetDlgItemText(hDlg, IDC_EDIT3, szBuff1);

				pSmCopy->MatrixToText(&pSmCopy->m_reversalModel_TOBEREMOVED.vm.timeStep, szBuff1, SIZE_8192);
				SetDlgItemText(hDlg, IDC_EDIT4, szBuff1);
				break;

			case IDC_BUTTON_DIRECTION_VECTOR:
				// Convert the GUI text into a matrix.
				GetDlgItemText(hDlg, IDC_EDIT1, szBuff1, SIZE_8192);
				pSmCopy->TextToMatrix(&pSmCopy->m_directionModel_TOBEREMOVED.vm.direction, szBuff1);

				GetDlgItemText(hDlg, IDC_EDIT2, szBuff1, SIZE_8192);
				pSmCopy->TextToMatrix(&pSmCopy->m_directionModel_TOBEREMOVED.vm.directionalBias, szBuff1);

				GetDlgItemText(hDlg, IDC_EDIT3, szBuff1, SIZE_8192);
				pSmCopy->TextToMatrix(&pSmCopy->m_directionModel_TOBEREMOVED.vm.terminate, szBuff1);


				// Place the matrix back into the GUI text
				pSmCopy->MatrixToText(&pSmCopy->m_directionModel_TOBEREMOVED.vm.direction, szBuff1, SIZE_8192);
				SetDlgItemText(hDlg, IDC_EDIT1, szBuff1);

				pSmCopy->MatrixToText(&pSmCopy->m_directionModel_TOBEREMOVED.vm.directionalBias, szBuff1, SIZE_8192);
				SetDlgItemText(hDlg, IDC_EDIT2, szBuff1);

				pSmCopy->MatrixToText(&pSmCopy->m_directionModel_TOBEREMOVED.vm.terminate, szBuff1, SIZE_8192);
				SetDlgItemText(hDlg, IDC_EDIT3, szBuff1);
				break;
			}

			if(wmId == IDOK)
			{
				CSpeciesModel::CopyModel(smOriginal, &smCopy);
				EndDialog(hDlg, LOWORD(wParam));
				return IDOK;
			}
			break;

		case IDCANCEL: // 2
		case IDCLOSE:  // 8
			EndDialog(hDlg, LOWORD(wParam));
			return IDCANCEL;
		}
#endif
		break;
	}
    return FALSE;
}
