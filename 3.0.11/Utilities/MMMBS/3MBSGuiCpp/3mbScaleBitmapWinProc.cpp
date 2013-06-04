#include "3mbScaleBitmapWinProc.h"

extern HWND g_hDlgSeed;
extern HWND g_hwndSlopeScale;
extern HWND g_hwndDepthScale;
extern HWND g_hwndBathy;

enum SCALEMOUSEDOWNREGION
{
	NOREGION,
	UPPER,
	MID,
	LOWER,
};

typedef struct scaleBitmapWindowState
{
	BYTE *s_bathyData;
	BATHYBITMAPINFO	bitMapInfo;
	int bitMapDataBufferLen;
	HBITMAP hdlBitmap;
	short upperLineY;
	short lowerLineY;
	double *displayMax;
	double *displayMin;
	double *dataMax;
	double *dataMin;
	UINT mouseMoveMsg;
	BYTE *(*ScaleToBitmapFunction)(SCALETOBITMAPPARAM Param);
}SCALEBITMAPWINDOWSTATE;

LRESULT ScaleBitmapWindowProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam, SCALEBITMAPWINDOWSTATE *pState, BATHYSCALETYPE ScaleType);

LRESULT CALLBACK DepthScaleBitmapWindowProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
	LRESULT res;
	BATHYBITMAP_WIN_PARAM *pSeedInf;
	static SCALEBITMAPWINDOWSTATE state = {0};

	if(message == WM_CREATE)
	{
		pSeedInf = *(BATHYBITMAP_WIN_PARAM **)lParam;
		state.ScaleToBitmapFunction = &DepthScaleToBitmap;
		state.displayMax = &pSeedInf->bitmapInf.displayExtremes.depthMax;
		state.displayMin = &pSeedInf->bitmapInf.displayExtremes.depthMin;
		state.dataMax = &pSeedInf->bitmapInf.dataExtremes.depthMax;
		state.dataMin = &pSeedInf->bitmapInf.dataExtremes.depthMin;
		state.mouseMoveMsg = WM_SEED_MOUSE_MOVE_DEPTHSCALE;

		/* Initialize the Bathymetry Bitmap Information structure bitMapInfo structure
		 * that is the same as a BITMPAINFO struct but with the appropriate number of
		 * colors allowcated for the RGBQUAD member struct needed for this bitmap.
		 * A BITMAPINFO structure defines the dimensions and color information for a
		 * Windows device-independent bitmap (DIB).*/
		SetBitmapPallet((LPBITMAPINFO)&state.bitMapInfo);

	}
	res = ScaleBitmapWindowProc(hWnd, message, wParam, lParam, &state, DEPTHSCALETYPE);
	return res;
}

LRESULT CALLBACK SlopeScaleBitmapWindowProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
	LRESULT res;
	BATHYBITMAP_WIN_PARAM *pSeedInf;
	static SCALEBITMAPWINDOWSTATE state = {0};

	if(message == WM_CREATE)
	{
		pSeedInf = *(BATHYBITMAP_WIN_PARAM **)lParam;
		state.ScaleToBitmapFunction = &SlopeScaleToBitmap;
		state.displayMax = &pSeedInf->bitmapInf.displayExtremes.slopeMax;
		state.displayMin = &pSeedInf->bitmapInf.displayExtremes.slopeMin;
		state.dataMax = &pSeedInf->bitmapInf.dataExtremes.slopeMax;
		state.dataMin = &pSeedInf->bitmapInf.dataExtremes.slopeMin;
		state.mouseMoveMsg = WM_SEED_MOUSE_MOVE_SLOPESCALE;

		/* Initialize the Bathymetry Bitmap Information structure bitMapInfo structure
		 * that is the same as a BITMPAINFO struct but with the appropriate number of
		 * colors allowcated for the RGBQUAD member struct needed for this bitmap.
		 * A BITMAPINFO structure defines the dimensions and color information for a
		 * Windows device-independent bitmap (DIB).*/
		SetSlopeBitmapPallet((LPBITMAPINFO)&state.bitMapInfo);

	}
	res = ScaleBitmapWindowProc(hWnd, message, wParam, lParam, &state, SLOPESCALETYPE);
	return res;
}



LRESULT ScaleBitmapWindowProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam, SCALEBITMAPWINDOWSTATE *pState, BATHYSCALETYPE ScaleType)
{
	int	wmId, wmEvent;
	RECT rect;
	HDC	hdc1, hdc2;
	HGDIOBJ	hgdiobj;
	PAINTSTRUCT	 ps;
	short lowerCandidate;
	short upperCandidate;

	static short prevMouseY = -1;
	static TRACKMOUSEEVENT trackMseEvnt = {0};
	static ENVMOUSECLICK coordValues = {0};
	static SCALEMOUSEDOWNREGION mouseDown = NOREGION;

	SCALETOBITMAPPARAM param;

	//BITMAP test;


	switch(message) 
	{
	case WM_CREATE:
		prevMouseY = -1;
		memset(&trackMseEvnt, 0, sizeof(TRACKMOUSEEVENT));
		memset(&coordValues, 0, sizeof(ENVMOUSECLICK));
		mouseDown = NOREGION;

		break;


	case WM_MOUSELEAVE:
		memset(&trackMseEvnt, 0, sizeof(TRACKMOUSEEVENT));
		memset(&coordValues, 0, sizeof(ENVMOUSECLICK));
		if(mouseDown != NOREGION)
		{
			PostMessage(g_hwndBathy, WM_REDRAW_BITMAP_THREAD, TRUE, ScaleType);
		}
		mouseDown = NOREGION;
		GetClientRect(hWnd, &rect);
		InvalidateRect(hWnd, &rect, FALSE);
		break;

	case WM_RBUTTONDOWN: // reset scale.
		*pState->displayMax = *pState->dataMax;
		*pState->displayMin = *pState->dataMin;
		// Fall through

	case WM_SIZE:
		// Get the new upper and lower line positions
		GetClientRect(hWnd, &rect);
		pState->upperLineY = DepthToYPixel(rect.bottom, *pState->displayMax, *pState->dataMax, *pState->dataMin);
		pState->lowerLineY = DepthToYPixel(rect.bottom, *pState->displayMin, *pState->dataMax, *pState->dataMin);

		// Fall through
	case WM_REDRAW_BITMAP_THREAD:

		// Get the bitmap's current dimensions and confirm they are the same as those that
		// were passed in.
		GetClientRect(hWnd, &rect);

#pragma message("fix this")
		// See how this is happening
		if(rect.bottom == 0 || rect.right == 0)
			break;

		// Update the size of the bitmap in the LPBITMAPINFO struct.
		BmpInitInfo((LPBITMAPINFO)&pState->bitMapInfo, rect.right, rect.bottom);

		// Deallocate the display data buffer if it isn't large enough for the current
		// bitmap dimensions Get the bitmap's current dimensions so that the call to
		// BathymetryDataToBuffer() knows to reallocate space for a larger buffer.
		if(pState->bitMapDataBufferLen < rect.right  * rect.bottom)
		{
			if(pState->s_bathyData != NULL)
				delete [] pState->s_bathyData;
			pState->s_bathyData = NULL;
			pState->bitMapDataBufferLen = 0;
		}

		// Update the data buffer
		param.WinWidth = rect.right;
		param.WinHeight = rect.bottom;
		param.BitMapData = pState->s_bathyData;
		param.BitMapDataBufferLen = &pState->bitMapDataBufferLen;
		param.dataMin = *pState->dataMin;
		param.dataMax = *pState->dataMax;
		param.displayMin = *pState->displayMin;
		param.displayMax = *pState->displayMax;
		param.YLine1 = pState->upperLineY;
		param.YLine2 = pState->lowerLineY;
		param.Abort = NULL;
		pState->s_bathyData = pState->ScaleToBitmapFunction(param);

		PostMessage(hWnd, WM_UPDATE_BITMAP, NULL, NULL);
		if(message == WM_RBUTTONDOWN)
			PostMessage(g_hwndBathy, WM_REDRAW_BITMAP_THREAD, TRUE, ScaleType);

		break;


	case WM_UPDATE_BITMAP:

		DeleteObject(pState->hdlBitmap);

		// Get the DC associated with this window,
		hdc1 = GetDC(hWnd);
		if(hdc1 == NULL)
			break;


		// Create a device dependant bitmap based upon the information about the bitmap
		// being drawn, based upon the compatable DC in memory created above, and based
		// upon the data (var s_bathyData) supplied to it.
		// From the Microsoft Documentation: "The CreateDIBitmap function creates a
		// compatible bitmap (DDB) from a DIB and, optionally, sets the bitmap bits."
		pState->hdlBitmap = CreateDIBitmap(hdc1,//hdc2,
										(LPBITMAPINFOHEADER)&pState->bitMapInfo,
										CBM_INIT,
										pState->s_bathyData,
										(LPBITMAPINFO)&pState->bitMapInfo,
										DIB_RGB_COLORS);
		//-------------------------------------------------------------------------//
		// Debugging and testing
		// Experimental
		// If the bmBits member of the BITMAP structure is NULL the bitmap is a DFB
		// (DDB) (Device-format bitmap / Device-dependent bitmap rather than a 
		// Device-Independent Bitmap).
		//memset(&test, 0, sizeof(test));
		//GetObject(pState->hdlBitmap, sizeof(BITMAP), (LPVOID)&test);
		//-------------------------------------------------------------------------//

		//DeleteObject(hdlBitmap);
		ReleaseDC(hWnd, hdc1);
		GetClientRect(hWnd, &rect);
		InvalidateRect(hWnd, &rect, FALSE);
		break;


	case WM_LBUTTONDOWN: // clearly needed.
		memset(&coordValues, 0, sizeof(ENVMOUSECLICK));
		coordValues.coord.X = LOWORD(lParam);
		coordValues.coord.Y = HIWORD(lParam);
		prevMouseY = HIWORD(lParam);

		// See where the mouse went down.
		if(coordValues.coord.Y >= pState->upperLineY-4 && coordValues.coord.Y <= pState->upperLineY+4)
			mouseDown = UPPER;
		else if(coordValues.coord.Y >= pState->lowerLineY-4 && coordValues.coord.Y <= pState->lowerLineY+4)
			mouseDown = LOWER;
		else if(coordValues.coord.Y <= pState->lowerLineY && coordValues.coord.Y > pState->upperLineY)
			mouseDown = MID;
		else
			mouseDown = NOREGION;
		break;

	case WM_LBUTTONUP: // needed known when user lets up on the scaling.
		memset(&coordValues, 0, sizeof(ENVMOUSECLICK));
		mouseDown = NOREGION;
		PostMessage(g_hwndBathy, WM_REDRAW_BITMAP_THREAD, TRUE, ScaleType);
		break;

	case WM_MOUSEMOVE: // needed to know which way the user is draging a scale range.
		// IF the moust button is down then determine which of the three zones the it did so in.
		// The window then needs to be updated as the mouse moves.
		lowerCandidate = upperCandidate = HIWORD(lParam);
		GetClientRect(hWnd, &rect);
		coordValues.lat = LOWORD(lParam);
		coordValues.lon = HIWORD(lParam);
		coordValues.index = rect.right + (rect.bottom - 1 - HIWORD(lParam)); 
		coordValues.coord.X = LOWORD(lParam);
		coordValues.coord.Y = HIWORD(lParam);
		coordValues.depth = YPixelToDepth(HIWORD(lParam), rect.bottom, *pState->dataMax, *pState->dataMin);
		PostMessage(g_hDlgSeed, pState->mouseMoveMsg, NULL, (LPARAM)&coordValues);


		switch(mouseDown)
		{
		case NOREGION:
			break;

		case UPPER:
			if(upperCandidate == pState->upperLineY)
				return 0;
			else if(upperCandidate > pState->lowerLineY - 10)
				upperCandidate = pState->lowerLineY - 10;

			*pState->displayMax =
				YPixelToDepth(upperCandidate,
							  rect.bottom,
							  *pState->dataMax,
							  *pState->dataMin);

			pState->upperLineY = upperCandidate;

			// Update the data buffer
			param.WinWidth = rect.right;
			param.WinHeight = rect.bottom;
			param.BitMapData = pState->s_bathyData;
			param.BitMapDataBufferLen = &pState->bitMapDataBufferLen;
			param.dataMin = *pState->dataMin;
			param.dataMax = *pState->dataMax;
			param.displayMin = *pState->displayMin;
			param.displayMax = *pState->displayMax;
			param.YLine1 = pState->upperLineY;
			param.YLine2 = pState->lowerLineY;
			param.Abort = NULL;
			pState->s_bathyData = pState->ScaleToBitmapFunction(param);

			PostMessage(hWnd, WM_UPDATE_BITMAP, NULL, NULL);

			break;

		case MID:
			upperCandidate = pState->upperLineY + HIWORD(lParam) - prevMouseY;
			lowerCandidate = pState->lowerLineY + HIWORD(lParam) - prevMouseY;

			if(upperCandidate < 0 || lowerCandidate > rect.bottom-1)
				return 0;
			pState->upperLineY = upperCandidate;
			pState->lowerLineY = lowerCandidate;

			*pState->displayMax = YPixelToDepth(pState->upperLineY, rect.bottom, *pState->dataMax, *pState->dataMin);
			*pState->displayMin = YPixelToDepth(pState->lowerLineY, rect.bottom, *pState->dataMax, *pState->dataMin);

			param.WinWidth = rect.right;
			param.WinHeight = rect.bottom;
			param.BitMapData = pState->s_bathyData;
			param.BitMapDataBufferLen = &pState->bitMapDataBufferLen;
			param.dataMin = *pState->dataMin;
			param.dataMax = *pState->dataMax;
			param.displayMin = *pState->displayMin;
			param.displayMax = *pState->displayMax;
			param.YLine1 = pState->upperLineY;
			param.YLine2 = pState->lowerLineY;
			param.Abort = NULL;
			pState->s_bathyData = pState->ScaleToBitmapFunction(param);

			prevMouseY = HIWORD(lParam);
			PostMessage(hWnd, WM_UPDATE_BITMAP, NULL, NULL);	
			break;

		case LOWER:
			if(lowerCandidate == pState->lowerLineY)
				return 0;
			else if(lowerCandidate < pState->upperLineY + 10)
				lowerCandidate = pState->upperLineY + 10;

			*pState->displayMin = YPixelToDepth(lowerCandidate, rect.bottom, *pState->dataMax, *pState->dataMin);
			pState->lowerLineY = lowerCandidate;

			param.WinWidth = rect.right;
			param.WinHeight = rect.bottom;
			param.BitMapData = pState->s_bathyData;
			param.BitMapDataBufferLen = &pState->bitMapDataBufferLen;
			param.dataMin = *pState->dataMin;
			param.dataMax = *pState->dataMax;
			param.displayMin = *pState->displayMin;
			param.displayMax = *pState->displayMax;
			param.YLine1 = pState->upperLineY;
			param.YLine2 = pState->lowerLineY;
			param.Abort = NULL;
			pState->s_bathyData = pState->ScaleToBitmapFunction(param);

			PostMessage(hWnd, WM_UPDATE_BITMAP, NULL, NULL);
			break;
		default:
			break;
		}

		// Set this window up to recieve a message when the mouse cursor goes off the screen.
		trackMseEvnt.cbSize = sizeof(trackMseEvnt);
		trackMseEvnt.dwFlags = TME_LEAVE;
		trackMseEvnt.hwndTrack = hWnd;
		trackMseEvnt.dwHoverTime = 0;
		TrackMouseEvent(&trackMseEvnt);
		return 0;

	case WM_COMMAND:
		wmId    = LOWORD(wParam); 
		wmEvent = HIWORD(wParam); 
		// Parse the menu selections:
		switch (wmId)
		{
		case WM_DESTROY: // the x in the upper right corner.
		case IDM_EXIT:
			if(pState->s_bathyData != NULL)
				delete [] pState->s_bathyData;
			pState->s_bathyData = NULL;
			DeleteObject(pState->hdlBitmap);
			DestroyWindow(hWnd);
			break;

		default:
			return DefWindowProc(hWnd, message, wParam, lParam);
		}
		break;


	case WM_PAINT:
		hdc1 = BeginPaint(hWnd, &ps);

		GetClientRect(hWnd, &rect);
		hdc2 = CreateCompatibleDC(hdc1);
		hgdiobj = SelectObject(hdc2, pState->hdlBitmap);

		if(!BitBlt(hdc1, 0, 0, rect.right,rect.bottom, hdc2, 0, 0,SRCCOPY))
			MessageBox(NULL,"BitBlt Failed!","Error",MB_ICONWARNING);

		SelectObject(hdc2, hgdiobj);
		DeleteDC(hdc2);
		EndPaint(hWnd, &ps);
		break;
	default:
		return DefWindowProc(hWnd, message, wParam, lParam);
   }
	return 0;
}