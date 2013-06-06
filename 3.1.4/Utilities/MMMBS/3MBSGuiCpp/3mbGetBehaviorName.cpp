#include "3mbGetBehaviorName.h"
#include "resource.h"


LRESULT CALLBACK GetBehaviorNameString(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{

	static BEHAVIOR_NAME *bm;
	DWORD i;

	switch (message)
	{
	case WM_INITDIALOG:
		bm = (BEHAVIOR_NAME *)lParam;

		// Take care of general intialization.
		SetDlgItemText(hDlg, IDC_EDIT_BEH_NAME,  bm->sz);
		break;

	case WM_COMMAND:
		if(LOWORD(wParam) == IDOK)
		{
			GetDlgItemText(hDlg, IDC_EDIT_BEH_NAME, bm->sz, SZ_BEHAVIOR_LEN-1);
			for(i=0; i<strlen(bm->sz); i++)
			{
				if(bm->sz[i] == ' ')
				{
					bm->sz[i] = 0;
					MessageBox(hDlg, "Name was truncated.  Whitespaces in behavior name not allowed.",
						"Whitespace Found!", 0);
					break;
				}
			}
			EndDialog(hDlg, LOWORD(wParam));
			return TRUE;
		}
		else if(LOWORD(wParam) == IDCANCEL)
		{
			EndDialog(hDlg, LOWORD(wParam));
			return TRUE;
		}
	}
	return FALSE;
}