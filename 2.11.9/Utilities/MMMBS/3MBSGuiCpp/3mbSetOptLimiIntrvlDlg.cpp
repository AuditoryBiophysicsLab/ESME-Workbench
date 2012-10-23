#include "3mbSetOptLimiIntrvlDlg.h"
#include "resource.h"
#include "3mbsLib.h"
#include "FileManager.h"
extern C3mbStaticsLib staticLib;


LRESULT CALLBACK DispalyOutputIntervalLimitDlgProc(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
	TCHAR szBuff[SIZE_32];
	static BOOL bInterval = TRUE;
	static BOOL bStart = TRUE;
	int wmId, wmEvent;

	static OUTPTINTRVLLMT *pParam;

//		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_TEMPMAP), FALSE);
//		CheckDlgButton(hwndParent, IDC_CHECK_TEMPMAP, BST_UNCHECKED);


	switch (message)
	{
	case WM_INITDIALOG:
		pParam = (OUTPTINTRVLLMT *)lParam;
		if(pParam->enabled == TRUE)
		{
			CheckDlgButton(hDlg, IDC_CHECK_ENABLE_LIMOUTBYINTERVL, BST_CHECKED);
			EnableWindow(GetDlgItem(hDlg, IDC_EDIT_LIMOUTBYINTERVL_VALUE), TRUE);
			EnableWindow(GetDlgItem(hDlg, IDC_EDIT_LIMOUTBYINTERVL_START), TRUE);

			if(pParam->interval == 0)
				EnableWindow(GetDlgItem(hDlg, IDOK), FALSE);
		}
		else
		{
			CheckDlgButton(hDlg, IDC_CHECK_ENABLE_LIMOUTBYINTERVL, BST_UNCHECKED);
			EnableWindow(GetDlgItem(hDlg, IDC_EDIT_LIMOUTBYINTERVL_VALUE), FALSE);
			EnableWindow(GetDlgItem(hDlg, IDC_EDIT_LIMOUTBYINTERVL_START), FALSE);
		}

		sprintf_s(szBuff, sizeof(szBuff), "%d", pParam->start);
		SetDlgItemText(hDlg, IDC_EDIT_LIMOUTBYINTERVL_START, szBuff);
		sprintf_s(szBuff, sizeof(szBuff), "%d", pParam->interval);	
		SetDlgItemText(hDlg, IDC_EDIT_LIMOUTBYINTERVL_VALUE, szBuff);
		break;

	case WM_COMMAND:
		wmId    = LOWORD(wParam);
		wmEvent = HIWORD(wParam);

		switch(wmId)
		{
		case IDOK:
			GetDlgItemText(hDlg, IDC_EDIT_LIMOUTBYINTERVL_VALUE, szBuff, TCHARBFLEN(szBuff));
			pParam->interval = atoi(szBuff);
			GetDlgItemText(hDlg, IDC_EDIT_LIMOUTBYINTERVL_START, szBuff, TCHARBFLEN(szBuff));
			pParam->start = atoi(szBuff);
			EndDialog(hDlg, LOWORD(wParam));

			break;
		case IDCANCEL:
			EndDialog(hDlg, LOWORD(wParam));
			break;

		case IDC_CHECK_ENABLE_LIMOUTBYINTERVL:
			pParam->enabled = !pParam->enabled;
			if(pParam->enabled == TRUE)
			{
				CheckDlgButton(hDlg, IDC_CHECK_ENABLE_LIMOUTBYINTERVL, BST_CHECKED);
				EnableWindow(GetDlgItem(hDlg, IDC_EDIT_LIMOUTBYINTERVL_VALUE), TRUE);
				EnableWindow(GetDlgItem(hDlg, IDC_EDIT_LIMOUTBYINTERVL_START), TRUE);
			}
			else
			{
				CheckDlgButton(hDlg, IDC_CHECK_ENABLE_LIMOUTBYINTERVL, BST_UNCHECKED);
				EnableWindow(GetDlgItem(hDlg, IDC_EDIT_LIMOUTBYINTERVL_VALUE), FALSE);
				EnableWindow(GetDlgItem(hDlg, IDC_EDIT_LIMOUTBYINTERVL_START), FALSE);
			}	
			break;

		case IDC_EDIT_LIMOUTBYINTERVL_VALUE:
			if(wmEvent == EN_CHANGE)
			{
				GetDlgItemText(hDlg, IDC_EDIT_LIMOUTBYINTERVL_VALUE, szBuff, TCHARBFLEN(szBuff));

				if(strlen(szBuff) != 0 && staticLib.StringIsALegalNumber(szBuff) == TRUE)
					pParam->interval = atoi(szBuff);

				if(strlen(szBuff) == 0 || staticLib.StringIsALegalNumber(szBuff) == FALSE || pParam->interval == 0)
				{
					EnableWindow(GetDlgItem(hDlg, IDOK), FALSE);
					bInterval = FALSE;
					break;
				}
				bInterval = TRUE;

				if(bInterval == TRUE && bStart == TRUE)
					EnableWindow(GetDlgItem(hDlg, IDOK), TRUE);
			}
			break;

		case IDC_EDIT_LIMOUTBYINTERVL_START:
			if(wmEvent == EN_CHANGE)
			{
				GetDlgItemText(hDlg, IDC_EDIT_LIMOUTBYINTERVL_START, szBuff, TCHARBFLEN(szBuff));

				if(strlen(szBuff) != 0 && staticLib.StringIsALegalNumber(szBuff) == TRUE)
					pParam->start = atoi(szBuff);

				if(strlen(szBuff) == 0 || staticLib.StringIsALegalNumber(szBuff) == FALSE)
				{
					EnableWindow(GetDlgItem(hDlg, IDOK), FALSE);
					bStart = FALSE;
					break;
				}
				bStart = TRUE;

				if(bInterval == TRUE && bStart == TRUE)
					EnableWindow(GetDlgItem(hDlg, IDOK), TRUE);
			}
			break;
		}
		break;
	}
	return FALSE;
}

