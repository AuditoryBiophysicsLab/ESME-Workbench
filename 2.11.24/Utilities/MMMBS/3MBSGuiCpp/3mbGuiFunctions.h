#ifndef _3MBSGUIFUNCTIONS_H
#define _3MBSGUIFUNCTIONS_H

#include "3mb.h"

DWORD WINAPI AbortProcessThreadProc(LPVOID lpParameter);

RESLT MyGetSaveFileName(FILE_INFO *FI, HWND OwnerWin, const TCHAR *szFilter, const TCHAR *szExtension, BOOL *FileSelected);
RESLT MyGetOpenFileName(FILE_INFO *FI, HWND OwnerWin, const TCHAR *szFilter, const TCHAR *szExtension, BOOL *FileSelected);
void InitOFN(OPENFILENAME *ofn, HWND hwnd, const TCHAR *szFilter, const TCHAR *szExtension, TCHAR *szFileName, TCHAR *szFileTitle);


//---------------------------------------------------------//
// Testing and Debugging
#define MAX_LOADSTRING 111
typedef struct Old_FileInfo
{
	BOOL bNeedSave;
	BOOL bNamed;
	TCHAR szFileName[MAX_LOADSTRING+1];
	TCHAR szTitleName[MAX_LOADSTRING+1];
	TCHAR szDisplay[MAX_LOADSTRING+1];
	TCHAR *szFilter;
	TCHAR *szDefExt;


	TCHAR szFilterArr[SIZE_128];
	TCHAR szDefExtArr[SIZE_128];

	OPENFILENAME ofn;
	HWND hwnd;
}OLD_FILE_INFO;
void InitOFN(OPENFILENAME *ofn, HWND hwnd,  OLD_FILE_INFO *fi);
//---------------------------------------------------------//

void UpdateGuiSpeciesListWindow(CScenario *pSce, HWND speListHwnd, int index = -1);

void UpdateGuiSpeciesListWindow(HWND speListHwnd, int index = -1);
SPECIESSUMMARY MbsGetSingleClusterSummary(CScenario *pSce, int SpeciesIndex);
SPECIESSUMMARY MbsGetSingleClusterSummary(int SpeciesIndex);
TCHAR *MbsGetPodSummaryString(int SpeciesIndex, int PodIndex, TCHAR *Buffer, int BufferLength);
TCHAR *MbsClusterSummaryToString(SPECIESSUMMARY *pCs, TCHAR *Buff, int BuffLen);
INHABITINF *AdjustCoordinateBufferSize(INHABITINF *pCoord, int *pBufferSize, int NumAnimats);
DWORD WINAPI ScenarioThread(LPVOID lpParameter);

void FileChanged(FILE_INFO *fi, HWND DisplayWindow = NULL, const TCHAR* szFileExtension = NULL);
void FileSaved(FILE_INFO *fi, HWND DisplayWindow = NULL, const TCHAR* szFileExtension = NULL);

void IOMessageBox(RESLT Res, HWND Hwnd, TCHAR *AppendBuffer);
enum SPECIALCASES
{
	INITIALIZE,
	FINALIZE,
	NONE,
};

void UpdateRunScenario(GLOBALDATA *gdata, DWORD TotalAnimatCount, DWORD Duration, SPECIALCASES SpecialCase = NONE, DWORD StartTick = 0);

ATOM RegisterBathyBitmapWindowClass(HINSTANCE hInstance,
									LPCTSTR	  ClassName,
									WNDPROC	  ProcName,
									HBRUSH	  hbrBackground = NULL,
									LPCTSTR	  MenuName		= NULL,
									UINT	  style			= CS_HREDRAW|CS_VREDRAW,
									HICON	  hIcon			= NULL,
									HICON	  hIconSm		= NULL,
									HCURSOR	  hCursor		= NULL);


#endif //_3MBSGUIFUNCTIONS_H