#include "3mbSeedDlgProc.h"
#include "3mbSeedingFunctions.h"
#include "3mbSeedDlgLayoutConfig.h"
#include "3mbSceInterface.h"
#include "3mbLoadShapeFileDlg.h"
#include "params.h"
#include "3mbsLib.h"



HWND g_hwndBathy = NULL;
HWND g_hwndDepthScale = NULL;
HWND g_hwndSlopeScale = NULL;
HWND g_hDlgSeed = NULL;
BOOL g_bBlockNextMouseClick = FALSE;
extern C3MBRandom g_3mbRandom;
extern C3mbStaticsLib staticLib;


DWORD WINAPI DelayInputToBathyBitmapWindowThread(LPVOID lpParameter)
{
	THREAD_INF *threadInf = (THREAD_INF *)lpParameter;
	threadInf->running = TRUE;

	while(threadInf->noRepaint == FALSE)
		Sleep(10); // here noRepaint is being used as an acknowlegement that the thread is running.

	Sleep(500);
	threadInf->running = FALSE;
	g_bBlockNextMouseClick = FALSE;
	CloseHandle(threadInf->hdl);
	memset(threadInf, 0, sizeof(THREAD_INF));
	return 0;
}

//CScenario *gpScenario = NULL;



// Lat and Lon validated



/*

{
	HWND *arr = new arr[34];
}
*/


/*
 * Strategy:
 * Window maximizes.
 * Seeding-related controls moved to far left.
 * If bathymetry data height is greater than its width, determine if moving Map-related controls to the far right creates more viewing space for bathymetry map.

 * Calculations made for s
 * 
 * Minimum bathymetry width and height...
 * Max Dimension of width and height of bathyemtry data determine display dimensions
 * Minimum display dimension determines control placement.
 *
 * Static Factors:
 *	Margin distance on right side from controls
 *  Margin distance on bottom side from bottom controls
 *  Dimensions of the actual bathymetry map: wider vs taller
 *
 * Dynamic Factors
 *	Width of the window impacts x placement
 *	Height of the window impacts y placement
 *
 * Considerations:
 *	Center the map when wide or narrow (i.e., not square)
 *  Control placement with extra space?
 *  Filler for extra space?
 *
 *  (1) Bathymetry data has a width (Wdat) (meters) and height (Hdat) (meters) and ratio (Rdat = Wdat/Hdat)
 *	(2) Dlg window has initial width (Wdlg0) and height (Hdlg0).
 *	(3) Bathy window has initial width (Wbmp0) (pixels) and height (Hbmp0) (pixels) and ratio (Rbmp0 = Wbmp0/Hbmp0)
 *
 *	(4) Widow will be maximized to a new width (Wdlg) and height (Hdlg) resulting in change in width (dWdlg) and
 *		height (dHdlg) and controls shifted.
 *      
 *	(2)
*/

typedef struct BathymetryBitmapDialogState
{
}BATHYBITMAPDLGSTATE;


const char *SZNONREGION = "";
const char *SZLAND = "(land region)";
const char *SZSBEACH = "(beaching region)";
const char *SZSHELF = "(shelf region)";
const char *SZbasin = "(basin region)";
const char *SZSLOPE = "(slope region)";

LRESULT CALLBACK BathyBitmapDlgProc(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
	int	wmId;
	int wmEvent;

	int i;
	WNDPROP winProp;
	BOOL bVal;
	double dVal;
	int nVal;
	RESLT res;
	TCHAR szBuff1[SIZE_128] = {0};
	TCHAR szBuff2[SIZE_128] = {0};
	int speciesAddIndex;
	int	numAnimats, numPods, podSize;
	int listBoxCount;
	BOOL fileSelected;
	LRESULT dlgResult;

	static CFileExtracter fileExtractor;


	static BATHYMAPINF mapInf;
	static DLGLAYOUT layout;
	static DLGLAYOUTOPTIONS layoutOptions;
	static WNDPROP prevDlgProp, dlgProp;
	static WINDOWDISPLAYSTATE winDisplayState = {0};
	static BOOL multiSoundSourceAllowed = MULTISOUNDSOURCEALLOWED; // var is here to quiet compiler warning

	// Variables that hold space needed between the expandable bathymetry bit map and the
	// right and bottom edges of the dialog box.
	static int rightMargin = 0; // Right edge.
	static int bottomMargin = 0; // Bottomedge.
	static int bottomCntrlsStart = 5; // Start pixel AFTER the bathymetry bitmap window of the bottom controls.
	static WNDPROP *winPropArray = NULL;
	static TCHAR szBuff[BUFFERED_MAX_PATH];
	static double bitmapWidthToHeightRatio;

	static BOOL	bInitializedOnce = FALSE;
	static ENVMOUSECLICK *mseClk, mseClkDwn, mseClkUp;
	float flat, flon;

	BATHYCOMPARE bathyShelfCmp;
	BATHYCOMPARE bathyBasinCmp;
	BATHYCOMPARE bathySlopeCmp;

	BOOL overShelf;
	BOOL overBasin;
	BOOL onSlope;

	BOOL withinPrefDepthShelf;
	BOOL withinPrefDepthBasin; 
	BOOL withinPrefDepthSlope;


	// Movable buttons effort
	static TCHAR szBuffMouseMove[SIZE_64];
	static COORD mouseMove = {0};

	// Animat Coordinates
	static COORD_DEPTH coord;
	static INHABITINF inhabitantInf;
	static int popCoordLength    = 0;
	static INHABITINF *popCoordArray = NULL;
	static int subsetCoordLength = 0;
	static INHABITINF *subsetCoordArray  = NULL;

	// Mouse/screen Coordinates
	static COORD cMouseDwn  = {0};
	static COORD cMouseUp   = {0};

	static SEED_DLG_PARAM *dlgParam;
	static SCENARIOUTPUTFILEINF sceInf = {0};
	static BATHYBITMAP_WIN_PARAM s_bathyWinParam, seedInfCopy;   
	static SCALEINGINF bathyScaleInf = {0};
	static SCALEINGINF slopeScaleInf = {0};

	static FILE_INFO fiSpe;
	static FILE_INFO fiMBS;


	static RECT dlgPrevWinRect;
	static RECT dlgPrevCliRect;

	static RECT rWin;
	static RECT rect;
	static SHAPE_PARAM s_shapeFileDlgParam = {0};

	static GUISTATE	prevState; // here temporaryily.


	//----------------------------------------------------------------------------------//
	// Gui Controls State
	//-------------------//
	ACTIVE_LISTBOX activeLB;
	RADIO_SEEDTYPE seedType;
	RADIO_ADDMETHOD addMethod;
	static ACTIVE_LISTBOX s_prevActiveLB; 
	static RADIO_SEEDTYPE s_prevSeedType = ADD_NONE;
	static RADIO_ADDMETHOD s_prevAddMethod = NOT_SET;
	//ACTIVE_LISTINDEX indices;
	//----------------------------------------------------------------------------------//

	static THREAD_INF s_blockInputThreadInf;

	BATHYBITMAP_WIN_PARAM *pSeedInf = &s_bathyWinParam;

	if(popCoordLength == 0 || popCoordArray == NULL || subsetCoordLength == 0 || subsetCoordArray == NULL)
	{
		_ASSERTE(popCoordLength == 0);
		_ASSERTE(popCoordArray == NULL);
		_ASSERTE(subsetCoordLength == 0);
		_ASSERTE(subsetCoordArray == NULL);

		popCoordLength = SIZE_1024;
		popCoordArray = new INHABITINF[popCoordLength];
		subsetCoordLength = SIZE_128;
		subsetCoordArray = new INHABITINF[subsetCoordLength];
	}


	switch(message)
	{
	case WM_INITDIALOG:
		dlgParam = (SEED_DLG_PARAM *)lParam;

		g_hDlgSeed = hDlg;
		prevDlgProp = GetWinProps(hDlg);

		memset(&winDisplayState, 0, sizeof(winDisplayState)); // always starts off minimized and unzoomed.
		winDisplayState.bMaximized = FALSE;
		winDisplayState.bZoom = FALSE;

		// Initialize the previous listbox information to none.
		s_prevActiveLB.index.spe = s_prevActiveLB.index.ind = s_prevActiveLB.index.pod = s_prevActiveLB.index.pdm = -1;
		s_prevActiveLB.lstbox = NONE_LB;

		//-----------------------------------------------------------------------------//
		// Initialize the randomizer and get the control layout options
		// (only once per launching the 3MB)
		//-------------------------------------------------------------//
		if(bInitializedOnce == FALSE)
		{
			bInitializedOnce = TRUE;
			//mysrand(0);
			GetLayoutConfigs(hDlg, &layoutOptions);
		}

		if(dlgParam->usageType == SEED_SCENARIO_TYPE)
		{
			CBitmapEnvFunctions::InitializeSeeding(dlgParam->pScenario);
			// Initial setup for certain controls.
			SetDlgItemText(hDlg, IDC_LOAD_SCENARIO, "Setup Shapefile");
			MoveDlgControl(IDC_LOAD_SCENARIO, hDlg, -55, 0);
			ResizeDlgControl(IDC_LOAD_SCENARIO, hDlg, 55, 0);
			//SetDlgWinProps(hDlg, IDC_BUTTON_DENSITYDISTRIBUTE, NORMAL_DIST_BUTTON_PROP);
			s_bathyWinParam = GetInitializedSeedInf(&sceInf, SEED_SCENARIO_STATE);
		}
		else
		{
			SetWindowText(hDlg, "Output Display (No file loaded)");
			s_bathyWinParam = GetInitializedSeedInf(&sceInf, UNINITIALIZED_STATE);
		}
		

		//-----------------------------------------------------------------------------//
		// Initialize the animat seeding struct.
		//--------------------------------------//

		//-----------------------------------------------------------------------------//
		// Register the bitmaps
		//---------------------//
		// Register the bathy bitmap window
		RegisterBathyBitmapWindowClass(
			dlgParam->hInstance,		// HINSTANCE
			BATHYBITMAPWINDOWCLASSNAME,	// LPCTSTR ClassName
			BathyBitmapWindowProc,		// WNDPROC ProcName
			(HBRUSH)(COLOR_WINDOW+1));	// HBRUSH hbrBackground

		RegisterBathyBitmapWindowClass(
			dlgParam->hInstance,		// HINSTANCE
			SCALEBITMAPWINDOWCLASSNAME,	// LPCTSTR ClassName
			DepthScaleBitmapWindowProc,	// WNDPROC ProcName
			(HBRUSH)(COLOR_WINDOW+1));	// HBRUSH hbrBackground

		RegisterBathyBitmapWindowClass(
			dlgParam->hInstance,		// HINSTANCE
			SLOPEBITMAPWINDOWCLASSNAME,	// LPCTSTR ClassName
			SlopeScaleBitmapWindowProc,	// WNDPROC ProcName
			(HBRUSH)(COLOR_WINDOW+1));	// HBRUSH hbrBackground

		SendMessage(hDlg, WM_INITIALIZE_BATHYMAP_SETUP, NULL, NULL);
		SendMessage(hDlg, WM_3MB_CREATEBITMAPS, NULL, NULL);
		SendMessage(hDlg, WM_3MB_REDRAWALL, NULL, NULL);
		break;

	case WM_INITIALIZE_BATHYMAP_SETUP:
		//-----------------------------------------------------------------------------//
		// Initialize the animat seeding struct.
		//--------------------------------------//
		s_bathyWinParam.bitmapInf.dataExtremes = CBitmapEnvFunctions::GetBathyExtremes();
		s_bathyWinParam.bitmapInf.displayExtremes = s_bathyWinParam.bitmapInf.dataExtremes;
		s_bathyWinParam.pWindowDisplayState = &winDisplayState;

		if(s_bathyWinParam.bitmapInf.displayExtremes.depthMax > LAND_DEPTH)
			s_bathyWinParam.bitmapInf.displayExtremes.depthMax = LAND_DEPTH;
		s_bathyWinParam.layout = &layout;
		//-----------------------------------------------------------------------------//

		bathyScaleInf.bathyExtremes = CBitmapEnvFunctions::GetBathyExtremes();
		bathyScaleInf.yMinDisplay = bathyScaleInf.bathyExtremes.depthMin;
		bathyScaleInf.yMaxDisplay = bathyScaleInf.bathyExtremes.depthMax;

		slopeScaleInf.bathyExtremes = CBitmapEnvFunctions::GetBathyExtremes();
		slopeScaleInf.yMinDisplay = slopeScaleInf.bathyExtremes.slopeMin;
		slopeScaleInf.yMaxDisplay = slopeScaleInf.bathyExtremes.slopeMax;


		//-----------------------------------------------------------------------------//
		// Get information about the bathymetry file loaded being displayed then set up
		// the dialog box accordingly.
		//----------------------------//
		mapInf = GetBathyMapInf();
		//-----------------------------------------------------------------------------//

		//-----------------------------------------------------------------------------//

		layout = SelectDialogLayout(hDlg, prevDlgProp, &mapInf, layoutOptions.reqDimensions, &layoutOptions);

		// Determine the bathymetry map scale (lat and lon per pixel).
		s_bathyWinParam.bitmapInf.latPerPixel = 
			(s_bathyWinParam.bitmapInf.displayExtremes.xMax -
			s_bathyWinParam.bitmapInf.displayExtremes.xMin)/double(layout.bathy.height);
		s_bathyWinParam.bitmapInf.lonPerPixel =
			(s_bathyWinParam.bitmapInf.displayExtremes.yMax -
			s_bathyWinParam.bitmapInf.displayExtremes.yMin)/double(layout.bathy.width);


		//-----------------------------------------------------------------------------//
		// Set up the sliders on the scale bitmap window (this region of code needs to
		// be improved on once the sliders are made active.  Also, investigate the
		// rec.bottom-1 vs. just rect.bottom (or window height).
		//------------------------------------------------------//
		// Perform a check that the actual size of the bathymetry bitmap window is the
		// same as asked for.
		winProp = GetWinProps(g_hwndDepthScale);

		s_bathyWinParam.bitmapInf.metersPerPixelDepth =
			(s_bathyWinParam.bitmapInf.displayExtremes.depthMax + s_bathyWinParam.bitmapInf.displayExtremes.depthMin)/double(layout.depth.height);

		s_bathyWinParam.bitmapInf.topBarrier = rect.top;
		s_bathyWinParam.bitmapInf.bttmBarrier = rect.bottom;
		s_bathyWinParam.bitmapInf.topBarrierRangeExtension = SLIDER_UPPER_RANGE_MIN_EXTENSION;
		s_bathyWinParam.bitmapInf.bttmBarrierRangeExtension = SLIDER_LOWER_RANGE_MIN_EXTENSION;

		//-----------------------------------------------------------------------------//
		// Set the text of the static text boxes
		//--------------------------------------//
		if(CBitmapEnvFunctions::GetSpeciesCount() > 0)
			UpdateGuiSpeciesListWindow(GetDlgItem(hDlg, IDC_LIST_SPECIES));

		if(mapInf.heightMeters > 999.0)
			sprintf_s(szBuff, sizeof(szBuff), "Height: %.0f (km), %d data points", mapInf.heightMeters/1000.0, mapInf.dataPointCount.x);
		else
			sprintf_s(szBuff, sizeof(szBuff), "Height: %.2f (m), %d data points", mapInf.heightMeters, mapInf.dataPointCount.x);
		SetDlgItemText(hDlg, IDC_STATIC_MAPHEIGHT, (LPCTSTR)szBuff);

		if(mapInf.widthMeters > 999.0)
			sprintf_s(szBuff, sizeof(szBuff), "Width: %.0f (km), %d data points", mapInf.widthMeters/1000.0, mapInf.dataPointCount.y);
		else
			sprintf_s(szBuff, sizeof(szBuff), "Width: %.2f (m), %d data points", mapInf.widthMeters, mapInf.dataPointCount.y);
		SetDlgItemText(hDlg, IDC_STATIC_MAPWIDTH, (LPCTSTR)szBuff);	


		dVal = CBitmapEnvFunctions::GetTotalBathySufaceAreaMeters();
		if(dVal > 99999.0)
			sprintf_s(szBuff, sizeof(szBuff), "Surface Area: %.0f (km^2)", dVal/(1000*1000));
		else
			sprintf_s(szBuff, sizeof(szBuff), "Surface Area: %.2f (km^2)", dVal);
		SetDlgItemText(hDlg, IDC_STATIC_MAPSURFAREA, (LPCTSTR)szBuff);	


		dVal = CBitmapEnvFunctions::GetBathymtryWaterSurfaceAreaMeters();
		if(dVal > 99999.0)
			sprintf_s(szBuff, sizeof(szBuff), "Seedable Depths: %.0f (km^2)", dVal/(1000*1000));
		else
			sprintf_s(szBuff, sizeof(szBuff), "Seedable Depths: %.2f (m^2)", dVal);
		SetDlgItemText(hDlg, IDC_STATIC_MAPSURFAREA_WATER, (LPCTSTR)szBuff);	

		dVal = CBitmapEnvFunctions::GetBathymetryLandSufaceAreaMeters();
		if(dVal > 99999.0)
			sprintf_s(szBuff, sizeof(szBuff), "Non-Seedable Depths: %.0f (km^2)", dVal/(1000*1000));
		else
			sprintf_s(szBuff, sizeof(szBuff), "Non-Seedable Depths: %.2f (m^2)", dVal);
		SetDlgItemText(hDlg, IDC_STATIC_MAPSURFAREA_LAND, (LPCTSTR)szBuff);	

		sprintf_s(szBuff, sizeof(szBuff), "Lat. Meters Per Sample Point: %d ", (int)(mapInf.heightMeters/(mapInf.dataPointCount.x-1)));
		SetDlgItemText(hDlg, IDC_STATIC_RESOLUTION_LAT, (LPCTSTR)szBuff);

		// Keep longitude (y) at the top, vary latitude (x)
		mapInf.widthMeters = staticLib.MetersBetweenCoordinates((mapInf.latMax - mapInf.latMin)/2 + mapInf.latMin, mapInf.lonMin, (mapInf.latMax - mapInf.latMin)/2 + mapInf.latMin, mapInf.lonMax);

		sprintf_s(szBuff, sizeof(szBuff), "Lon. Meters Per Sample Point: %d ", (int)(mapInf.widthMeters/(mapInf.dataPointCount.y-1)));
		SetDlgItemText(hDlg, IDC_STATIC_RESOLUTION_LON, (LPCTSTR)szBuff);	

		UpdateEnvBitmapDlg(&s_bathyWinParam);

		GetWindowRect(hDlg, &dlgPrevWinRect);
		GetClientRect(hDlg, &dlgPrevCliRect);
		break;

		//PostMessage(hDlg, WM_3MB_CREATEBITMAPS, NULL, 


	case WM_3MB_CREATEBITMAPS:
		////////////////////////////////////////
		g_hwndDepthScale = CreateWindowEx(
			0,	// dwExStyle: The extended window style of the window being created.
			SCALEBITMAPWINDOWCLASSNAME, // lpClassName: Pointer to a null-terminated string or a class atom
			"ScaleBitmap", // lpWindowName: Pointer to a null-terminated string that specifies the window name
			WS_CLIPCHILDREN | WS_BORDER | WS_VISIBLE | WS_CHILD, //dwStyle: Specifies the style of the window
			layout.depth.x, // x: Initial horizontal position of the window
			layout.depth.y, // y: Initial vertical position of the window
			layout.depth.width + 2, // The width of the window in device units.  +2 for the border box.
			layout.depth.height + 2, // The height of the window in device units.  +2 for the border box.
			g_hDlgSeed, // hWndParent: Handle to the parent or owner window of the window being created.
			(HMENU) NULL, //hMenu: Handle to a menu, or specifies a child-window identifier, depending on the window style. 
			NULL,//hInstance: Handle to the instance of the module to be associated with the window.
			(LPVOID)(pSeedInf)); // lpParam: Pointer to a value to be passed to the window through the CREATESTRUCT
								 //          structure (lpCreateParams member) pointed to by the lParam param of
								 //			 the WM_CREATE message
		GetClientRect(g_hwndDepthScale, &rect);


		g_hwndSlopeScale  = CreateWindowEx(
			0,	// dwExStyle: The extended window style of the window being created.
			SLOPEBITMAPWINDOWCLASSNAME, // lpClassName: Pointer to a null-terminated string or a class atom
			"SlopeBitmap", // lpWindowName: Pointer to a null-terminated string that specifies the window name
			WS_CLIPCHILDREN | WS_BORDER | WS_VISIBLE | WS_CHILD, //dwStyle: Specifies the style of the window
			layout.slope.x, // x: Initial horizontal position of the window
			layout.slope.y, //layout.depth.y, // y: Initial vertical position of the window
			layout.slope.width + 2, // The width of the window in device units.  +2 for the border box.
			layout.slope.height + 2, // The height of the window in device units.  +2 for the border box.
			g_hDlgSeed, // hWndParent: Handle to the parent or owner window of the window being created.
			(HMENU) NULL, //hMenu: Handle to a menu, or specifies a child-window identifier, depending on the window style. 
			NULL,//hInstance: Handle to the instance of the module to be associated with the window.
			(LPVOID)(pSeedInf)); // lpParam: Pointer to a value to be passed to the window through the CREATESTRUCT
								 //          structure (lpCreateParams member) pointed to by the lParam param of
								 //			 the WM_CREATE message
		GetClientRect(g_hwndSlopeScale, &rect);


		g_hwndBathy  = CreateWindowEx(
			0,
			BATHYBITMAPWINDOWCLASSNAME,"BathyBitmap",
			WS_CLIPCHILDREN | WS_BORDER | WS_VISIBLE | WS_CHILD,
			DLGBOXLEFTSIDEMARGIN, //layout.bathy.x,
			DLBBOXTOPMARGIN, //layout.bathy.y,
			layout.bathy.width+2, //layout.bathy.width+2, // The width of the window in device units.  +2 for the border box.
			layout.bathy.height+2, // The height of the window in device units.  +2 for the border box.
			g_hDlgSeed, // parent window.
			(HMENU) NULL,
			NULL,
			(LPVOID)(pSeedInf));
		GetClientRect(g_hwndBathy, &rect);
		break;

		// Investigate why this is needed.  Not needed for the bathymetry map.
		//PostMessage(g_hwndDepthScale, WM_UPDATE_BITMAP, 0, 0);
		//-----------------------------------------------------------------------------//
		//////////////////////////////////////

	case WM_3MB_REDRAWALL:
		PostMessage(g_hwndBathy, WM_REDRAW_BITMAP_THREAD, TRUE, BITMAPZOOMTYPE);
		PostMessage(g_hwndDepthScale, WM_REDRAW_BITMAP_THREAD, NULL, NULL);
		PostMessage(g_hwndSlopeScale, WM_REDRAW_BITMAP_THREAD, NULL, NULL);
		// Second repeat ends here.
		return FALSE;

	case WM_MOUSEMOVE:
		break;
/*
	case WM_PAINT:
		hdc = BeginPaint(hWnd, &ps);
		EndPaint(hWnd, &ps);
		break;
*/
	case WM_RBUTTONDOWN:
		// The user clicked (somewhere/anywhere) on the dialog box.  Reset the dialog box
		// by dehighliting active list boxes.  This will allow the user to zoom out on the
		// bit map on the next click if they had previously zoomed in on it.

		// If the list box is not set to NONE_LB one or more listboxes are highlighted.
		activeLB = GetActiveListBox();

		if(activeLB.lstbox == NONE_LB)
		{
			// No list boxes are highlighted so tell the bitmap to zoom out.
			PostMessage(hDlg, WM_ZOOMOUTBITMAP, 0, 0);
			break;
		}

		s_prevAddMethod = SetAddMethodStateRadio(NOT_SET);
		s_prevSeedType = SetSeedTypeRadioState(ADD_NONE);
		s_prevActiveLB = ClearActiveListBoxes();

		// Update the state change throughout the GUI.
		UpdateEnvBitmapDlg(&s_bathyWinParam);
		SendMessage(g_hwndBathy, WM_UPDATE_ENV_ANIMATS, (WPARAM)0, (LPARAM)0);
		break;

	case WM_LBUTTONDOWN:
		//PostMessage(g_hwndDepthScale, WM_CLEARLINE, 0, 0);
		break;
	case WM_LBUTTONUP:
		break;
	case WM_RBUTTONUP:
		break;

	case WM_UPDATE_GUI:
		UpdateEnvBitmapDlg(&s_bathyWinParam);
		break;

	case WM_CAPTURE_MOUSE_DOWN:
		mseClk = (ENVMOUSECLICK *)lParam;
		// record the mouse down coordinates
		memcpy(&mseClkDwn, mseClk, sizeof(ENVMOUSECLICK));
		break;

	case WM_ZOOMOUTBITMAP:
		winDisplayState.bZoom = FALSE;

		mapInf = GetBathyMapInf();
		if(s_bathyWinParam.bitmapInf.displayExtremes.xMax == mapInf.latMax &&
			s_bathyWinParam.bitmapInf.displayExtremes.xMin == mapInf.latMin &&
			s_bathyWinParam.bitmapInf.displayExtremes.yMax == mapInf.lonMax &&
			s_bathyWinParam.bitmapInf.displayExtremes.yMin == mapInf.lonMin)
			break;

		layout = SelectDialogLayout(hDlg, prevDlgProp, &mapInf, layoutOptions.reqDimensions, &layoutOptions);
		s_bathyWinParam.bitmapInf.displayExtremes.xMax = mapInf.latMax;
		s_bathyWinParam.bitmapInf.displayExtremes.xMin = mapInf.latMin;
		s_bathyWinParam.bitmapInf.displayExtremes.yMax = mapInf.lonMax;
		s_bathyWinParam.bitmapInf.displayExtremes.yMin = mapInf.lonMin;

		// Determine the bathymetry map scale (lat and lon per pixel).
		s_bathyWinParam.bitmapInf.latPerPixel = 
			(s_bathyWinParam.bitmapInf.displayExtremes.xMax - s_bathyWinParam.bitmapInf.displayExtremes.xMin)/double(layout.bathy.height);
		s_bathyWinParam.bitmapInf.lonPerPixel =
			(s_bathyWinParam.bitmapInf.displayExtremes.yMax - s_bathyWinParam.bitmapInf.displayExtremes.yMin)/double(layout.bathy.width);

		// A call to PostMessage with the WM_REDRAW_BITMAP_THREAD as the message as well as the Invalidate Rect 
		// are not needed because a WM_RESIZE message is automatically generated.
		GetClientRect(hDlg, &rect);
		InvalidateRect(hDlg, &rect, TRUE);
		PostMessage(g_hwndBathy, WM_REDRAW_BITMAP_THREAD, TRUE, BITMAPZOOMTYPE); // TRUE means resize.
		break;

	case WM_ZOOMBITMAP:
		winDisplayState.bZoom = TRUE;

		mseClk = (ENVMOUSECLICK *)lParam;
		winProp = GetWinProps(g_hwndBathy);
		_ASSERT(mseClk != NULL);
		GetClientRect(g_hwndBathy, &rect);

		memcpy(&mseClkUp, mseClk, sizeof(ENVMOUSECLICK));

		mapInf = GetBathyMapInf(&mseClkDwn, &mseClkUp);

		layout = SelectDialogLayout(hDlg, prevDlgProp, &mapInf, layoutOptions.reqDimensions, &layoutOptions);

		s_bathyWinParam.bitmapInf.displayExtremes.xMax = mapInf.latMax;
		s_bathyWinParam.bitmapInf.displayExtremes.xMin = mapInf.latMin;
		s_bathyWinParam.bitmapInf.displayExtremes.yMax = mapInf.lonMax;
		s_bathyWinParam.bitmapInf.displayExtremes.yMin = mapInf.lonMin;


		// Determine the bathymetry map scale (lat and lon per pixel).
		s_bathyWinParam.bitmapInf.latPerPixel = 
			(s_bathyWinParam.bitmapInf.displayExtremes.xMax - s_bathyWinParam.bitmapInf.displayExtremes.xMin)/double(layout.bathy.height);
		s_bathyWinParam.bitmapInf.lonPerPixel =
			(s_bathyWinParam.bitmapInf.displayExtremes.yMax - s_bathyWinParam.bitmapInf.displayExtremes.yMin)/double(layout.bathy.width);


		// A call to PostMessage with the REDRAW BITMAP as the message as well as the Invalidate Rect 
		// are not needed because a RESIZE message is automatically generated.
		GetClientRect(hDlg, &rect);
		InvalidateRect(hDlg, &rect, TRUE);

		// Although the following message is posted and this region of code continues as
		// it is supposed to the repainting of the dialog box appears to wait on the  posting to finish.
		PostMessage(g_hwndBathy, WM_REDRAW_BITMAP_THREAD, TRUE, BITMAPZOOMTYPE); // TRUE means resize.
		break;

	case WM_SEED_COORDINATE:
		if(s_bathyWinParam.usageState != SEED_SCENARIO_STATE)
			break;
		activeLB = GetActiveListBox();
		seedType = GetSeedTypeRadioState();
		addMethod = GetAddMethodRadioState();

		// If no list box is active then do not add anything even though user clicked on
		// the bitmap.
#ifndef _DEBUG
		if(activeLB.lstbox != SPECIES_LB)
			break;
#else
		if(activeLB.lstbox != SPECIES_LB && (activeLB.lstbox != POD_LB || seedType != ADD_POD_MEMBER))
			break;
#endif
		bVal = CBitmapEnvFunctions::SpeciesIsASoundSourceModel(activeLB.index.spe);
		// Handle cases where mulitiple sound sources not allowed.
		if(multiSoundSourceAllowed == FALSE && bVal == TRUE)
		{	
			// For adds that add multiple animats at a time (seeding by density, by
			// number, pods... etc), temporarily set the seeding "add amount" to 1.
			memcpy(&seedInfCopy, &s_bathyWinParam, sizeof(BATHYBITMAP_WIN_PARAM));
			seedInfCopy.addAmount = 1;
			seedInfCopy.initialPodSize = 1;

			// If already populated with a sound source, break;
			nVal = CBitmapEnvFunctions::GetTotalSoundSourceCount();
			if(nVal > 0)
				break;

			pSeedInf = &seedInfCopy;
		}
		mseClk = (ENVMOUSECLICK *)lParam;
		
		if(addMethod == BOUNDINGBOX && s_bathyWinParam.boundingBoxPresent == TRUE)
		{
			// record the mouse up coordinates and populate
			_ASSERT(mseClk != NULL);
			memcpy(&mseClkUp, mseClk, sizeof(ENVMOUSECLICK));
			AddAnimatsAroundEnvData(&mseClkDwn, &mseClkUp, pSeedInf, dlgParam->pScenario);
		}
		else if(s_bathyWinParam.boundingBoxPresent == FALSE && addMethod != BOUNDINGBOX)
		{
			memset(&mseClkDwn, 0, sizeof(ENVMOUSECLICK));
			if(mseClk != NULL)
				memcpy(&mseClkDwn, mseClk, sizeof(ENVMOUSECLICK)); 
			AddAnimatsAroundEnvData(&mseClkDwn, NULL, pSeedInf, dlgParam->pScenario);
		}

		// Redraw the listing in the species list box that was modified, then put the
		// cursor on the list being modified.
		UpdateGuiSpeciesListWindow(GetDlgItem(hDlg, IDC_LIST_SPECIES), activeLB.index.spe);
		SendMessage(GetDlgItem(hDlg, IDC_LIST_SPECIES), LB_SETCURSEL, activeLB.index.spe, (LPARAM)0);


		//------------------------------------------------------------------------------//
		// Set cursors and redraw the species population lists based upon the type of add
		// conducted (individual, pod, or pod members).
		//---------------------------------------------//
		if(seedType == ADD_INDIVIDUAL)
		{
			// Dehighlight individual, pods, and pod member list boxes
			SendMessage(GetDlgItem(hDlg, IDC_LIST_INDIVIDUAL), LB_SETCURSEL, (WPARAM)-1, (LPARAM)0);
			SendMessage(GetDlgItem(hDlg, IDC_LIST_PODS),	   LB_SETCURSEL, (WPARAM)-1, (LPARAM)0);
			SendMessage(GetDlgItem(hDlg, IDC_LIST_SEL_POP), LB_SETCURSEL, (WPARAM)-1, (LPARAM)0);

			// Draw the newly added individuals.
			numAnimats = CBitmapEnvFunctions::GetIndividualCount(activeLB.index.spe);
			subsetCoordArray = AdjustCoordinateBufferSize(subsetCoordArray, &subsetCoordLength, numAnimats);
			CBitmapEnvFunctions::GetIndividualInitialCoordinates(activeLB.index.spe, subsetCoordArray);

			listBoxCount = SendMessage(GetDlgItem(hDlg, IDC_LIST_INDIVIDUAL), LB_GETCOUNT, (WPARAM)0, (LPARAM)0);
			for(i=listBoxCount; i<numAnimats && i < MAX_NUMBER_ANIAMTS_IN_LIST_BOX; i++)
			{
				sprintf_s(szBuff, sizeof(szBuff), "%14.10f, %14.10f", subsetCoordArray[i].coord.lat, subsetCoordArray[i].coord.lon);
				SendMessage(GetDlgItem(hDlg, IDC_LIST_INDIVIDUAL), LB_ADDSTRING, 0, (LPARAM)szBuff);
			}
		}
		else if(seedType == ADD_POD)
		{
			// One or more pod were added, so add the pods to the list box.

			// Make individual, pod, and pod members indexes -1.
			_ASSERTE(activeLB.index.spe != -1);

			// Dehighlight individual list boxes and clear out the pod member list box.
			SendMessage(GetDlgItem(hDlg, IDC_LIST_INDIVIDUAL), LB_SETCURSEL, (WPARAM)-1, (LPARAM)0);
			SendMessage(GetDlgItem(hDlg, IDC_LIST_SEL_POP), LB_RESETCONTENT, (WPARAM)0, (LPARAM)0);

			// Get the count on how many pods are already listed in the pod list box.
			listBoxCount = SendMessage(GetDlgItem(hDlg, IDC_LIST_PODS), LB_GETCOUNT, 0, (LPARAM)0);

			// Get the count on how many pods are actually in the active species.
			numPods = CBitmapEnvFunctions::GetPodCount(activeLB.index.spe);

			// Add the new pods to the pod list.
			for(i=listBoxCount; i<numPods; i++)
			{
				// Build the added pod's string.
				MbsGetPodSummaryString(activeLB.index.spe, i, szBuff, SIZE_32);

				// Add the string to the pod list box
				SendMessage(GetDlgItem(hDlg, IDC_LIST_PODS), LB_ADDSTRING, 0, (LPARAM)szBuff);
			}
		}
		else if(seedType == ADD_POD_MEMBER)
		{
			// A pod member was added
			SendMessage(GetDlgItem(hDlg, IDC_LIST_SEL_POP), LB_SETCURSEL, (WPARAM)-1, (LPARAM)0);

			// Update string of the pod being added to.
			MbsGetPodSummaryString(activeLB.index.spe, activeLB.index.pod, szBuff, SIZE_32);
			SendMessage(GetDlgItem(hDlg, IDC_LIST_PODS), LB_DELETESTRING, activeLB.index.pod, 0);
			SendMessage(GetDlgItem(hDlg, IDC_LIST_PODS), LB_INSERTSTRING, activeLB.index.pod, (LPARAM)szBuff);
			SendMessage(GetDlgItem(hDlg, IDC_LIST_PODS), LB_SETCURSEL, activeLB.index.pod, (LPARAM)0);

			// Populate the pod list box
			listBoxCount = SendMessage(GetDlgItem(hDlg, IDC_LIST_SEL_POP), LB_GETCOUNT, (WPARAM)0, (LPARAM)0);
			podSize	= CBitmapEnvFunctions::GetPodMemberCount(activeLB.index.spe, activeLB.index.pod);
			subsetCoordArray = AdjustCoordinateBufferSize(subsetCoordArray, &subsetCoordLength, podSize);
			CBitmapEnvFunctions::GetPodInitialCoordinates(activeLB.index.spe, activeLB.index.pod, subsetCoordArray);
			for(i=listBoxCount; i<podSize && i < MAX_NUMBER_ANIAMTS_IN_LIST_BOX; i++)
			{
				sprintf_s(szBuff, sizeof(szBuff), "%14.10f, %14.10f", subsetCoordArray[i].coord.lat, subsetCoordArray[i].coord.lon);
				SendMessage(GetDlgItem(hDlg, IDC_LIST_SEL_POP), LB_ADDSTRING, 0, (LPARAM)szBuff);
			}
		}
		UpdateEnvBitmapDlg(&s_bathyWinParam);
		SendMessage(g_hwndBathy, WM_UPDATE_ENV_ANIMATS, (WPARAM)0, (LPARAM)0);
		break;

	case WM_SEED_MOUSE_MOVE:
		mseClk = (ENVMOUSECLICK *)lParam;

		withinPrefDepthShelf = withinPrefDepthBasin = withinPrefDepthSlope = FALSE;
		if(mseClk->depth < 2.0)
		{
			bathyShelfCmp = staticLib.CompareBathyDepth(mseClk->depth, SHELF_ENVATTR_BATHY_SHALLOWER_THAN);
			overShelf = (bathyShelfCmp == SHALLOWER || bathyShelfCmp == EQUAL);
			withinPrefDepthShelf = overShelf && (mseClk->slopeAngle < SHELF_ENVATTR_DEG_LESS_THAN);

			bathyBasinCmp = staticLib.CompareBathyDepth(mseClk->depth, BASIN_ENVATTR_BATHY_DEEPER_THAN);
			overBasin = (bathyBasinCmp == DEEPER);
			withinPrefDepthBasin = overBasin && (mseClk->slopeAngle < BASIN_ENVATTR_DEG_LESS_THAN);

			bathySlopeCmp = staticLib.CompareBathyDepth(mseClk->depth, SLOPE_ENVATTR_BATHY_DEEPER_THAN);
			onSlope = (bathySlopeCmp == DEEPER);
			withinPrefDepthSlope = onSlope && (mseClk->slopeAngle > SLOPE_ENVATTR_DEG_GREATER_THAN);
		}

		if(mseClk->depth >= 0.0)
			strncpy_s(szBuff2, SZLAND, sizeof(szBuff2)-1);
		else if(mseClk->depth >= -2.0)
			strncpy_s(szBuff2, SZSBEACH, sizeof(szBuff2)-1);
		else if(withinPrefDepthShelf == TRUE)
			strncpy_s(szBuff2, SZSHELF, sizeof(szBuff2)-1);
		else if(withinPrefDepthBasin == TRUE)
			strncpy_s(szBuff2, SZbasin, sizeof(szBuff2)-1);
		else if(withinPrefDepthSlope == TRUE)
			strncpy_s(szBuff2, SZSLOPE, sizeof(szBuff2)-1);
		else
			strncpy_s(szBuff2, SZNONREGION, sizeof(szBuff2)-1);

		if(fabs(mseClk->lat) >= 1000.0 || fabs(mseClk->lon) >= 1000.0)
		{
			sprintf_s(szBuff, sizeof(szBuff), "Depth %.02f (m) %s Slope: %2.1f deg @ %3.1f deg %s",
				mseClk->depth, "   ", mseClk->slopeAngle, mseClk->slopeHeading, szBuff2);
		}
		else
		{
			sprintf_s(szBuff, sizeof(szBuff), "(Lat %11f, Lon %11f) %s Depth %.02f (m) %s Slope: %2.1f deg @ %3.1f deg %s",
				mseClk->lat, mseClk->lon, "   ", mseClk->depth, "   ", mseClk->slopeAngle, mseClk->slopeHeading, szBuff2);
		}
		SetDlgItemText(hDlg, IDC_STATIC_MOUSE_POSITION, (LPCTSTR)szBuff);
		break;

	case WM_SEED_MOUSE_MOVE_DEPTHSCALE:
		mseClk = (ENVMOUSECLICK *)lParam;
#ifdef _DEBUG
		sprintf_s(szBuff, sizeof(szBuff), "(y pixel %d) %s Depth %.02f (m)", (int)mseClk->lon, "   ", mseClk->depth);
#else
		sprintf_s(szBuff, sizeof(szBuff), "Depth %.02f (m)", mseClk->depth);
#endif
		SetDlgItemText(hDlg, IDC_STATIC_MOUSE_POSITION, (LPCTSTR)szBuff);
		break;

	case WM_SEED_MOUSE_MOVE_SLOPESCALE:
		mseClk = (ENVMOUSECLICK *)lParam;
#ifdef _DEBUG
		sprintf_s(szBuff, sizeof(szBuff), "(y pixel %d) %s Slope %.02f (m)", (int)mseClk->lon, "   ", mseClk->depth);
#else
		sprintf_s(szBuff, sizeof(szBuff), "Slope %.02f (deg) @ %02f", mseClk->depth, mseClk->slopeHeading);
#endif
		SetDlgItemText(hDlg, IDC_STATIC_MOUSE_POSITION, (LPCTSTR)szBuff);
		break;


	case WM_SIZE:
		winDisplayState.bMaximized = !winDisplayState.bMaximized;
		layout = SelectDialogLayout(hDlg, prevDlgProp, &mapInf, layoutOptions.reqDimensions, &layoutOptions);
		prevDlgProp = GetWinProps(hDlg);
		s_bathyWinParam.bitmapInf.latPerPixel = 
			(s_bathyWinParam.bitmapInf.displayExtremes.xMax - s_bathyWinParam.bitmapInf.displayExtremes.xMin)/double(layout.bathy.height);
		s_bathyWinParam.bitmapInf.lonPerPixel =
			(s_bathyWinParam.bitmapInf.displayExtremes.yMax - s_bathyWinParam.bitmapInf.displayExtremes.yMin)/double(layout.bathy.width);

		//-----------------------------------------------------------------------------//
		// Reset the initial locations of the controls under the bathymetry bitmap window
		//-----------------------------------------------------------------------------//
		GetClientRect(hDlg, &rect);
		InvalidateRect(hDlg, &rect, TRUE); 
		PostMessage(g_hwndBathy, WM_REDRAW_BITMAP_THREAD, TRUE, BITMAPZOOMTYPE); // TRUE means resize.
		break;

	case WM_CLOSE:
		// If the Shape File dialog box is running kill it.
		if(s_shapeFileDlgParam.dlgHdl != NULL)
			SendMessage(s_shapeFileDlgParam.dlgHdl, WM_USER_CANCEL, 0, 0);
		s_shapeFileDlgParam.dlgHdl = NULL;


		// Population Array
		if(popCoordArray != NULL)
			delete [] popCoordArray;
		popCoordArray = NULL;
		popCoordLength = 0;

		// Subse of the population array
		if(subsetCoordArray != NULL)
			delete [] subsetCoordArray;
		subsetCoordArray = NULL;
		subsetCoordLength = 0;

		CBitmapEnvFunctions::Uninitialize();

		if(g_hwndBathy != NULL)
		{
			_ASSERT(g_hwndDepthScale != NULL && g_hwndSlopeScale != NULL);
			DestroyWindow(g_hwndBathy);
			DestroyWindow(g_hwndDepthScale);
			DestroyWindow(g_hwndSlopeScale);
			g_hwndBathy = g_hwndDepthScale = g_hwndSlopeScale = NULL;
		}
		else
		{
			_ASSERT(g_hwndDepthScale == NULL && g_hwndSlopeScale == NULL);
		}

		g_hDlgSeed = NULL;


		//--------------------------------------------------------------------------//
		// Free shape file/PDF dyanamically allocated memory
		//--------------------------------------------------//
		// May reconsider freeing this later, but the information will have to be maintained in the main dialog box.
		_ASSERT((s_shapeFileDlgParam.pdfInf.pPolygonInfBuff != NULL && s_shapeFileDlgParam.pdfInf.numPolygons > 0) ||
			(s_shapeFileDlgParam.pdfInf.pPolygonInfBuff == NULL && s_shapeFileDlgParam.pdfInf.numPolygons == 0));
		Deallocate3MBShapeFileInfStruct(&s_shapeFileDlgParam.pdfInf);
		memset(&s_shapeFileDlgParam.pdfInf, 0, sizeof(s_shapeFileDlgParam.pdfInf));
		EndDialog(hDlg, LOWORD(wParam));
		return TRUE;

	case WM_3MBS_LOAD_SCENARIO:
		// Unitialize any previously loaded output file.
		CBitmapEnvFunctions::Uninitialize();

		//-----------------------------------------------------------------------------//
		// Initialize the animat seeding struct.
		//--------------------------------------//
		//s_bathyWinParam = GetInitializedSeedInf(&sceInf, PLAYBACK_STATE);

		// Clear the species, individual, pod, and pod member lists because they will not have
		// been populated yet.
		SendMessage(GetDlgItem(hDlg, IDC_LIST_SPECIES),	LB_RESETCONTENT, (WPARAM)0, (LPARAM)0); // The individual list box
		SendMessage(GetDlgItem(hDlg, IDC_LIST_INDIVIDUAL),	LB_RESETCONTENT, (WPARAM)0, (LPARAM)0); // The individual list box
		SendMessage(GetDlgItem(hDlg, IDC_LIST_PODS),		LB_RESETCONTENT, (WPARAM)0, (LPARAM)0); // The pod list box
		SendMessage(GetDlgItem(hDlg, IDC_LIST_SEL_POP),	LB_RESETCONTENT, (WPARAM)0, (LPARAM)0); // The pod member list box

 
		if(OK != (res = CBitmapEnvFunctions::InitializePlayback(fiMBS.szFileName, &sceInf)))
		{
			UpdateEnvBitmapDlg(&s_bathyWinParam);
			s_bathyWinParam.usageState = UNINITIALIZED_STATE;
			staticLib.MbsResultToString(res, szBuff1, TCHARBFLEN(szBuff1), szBuff2, TCHARBFLEN(szBuff2));
			MessageBox(hDlg, szBuff1, szBuff2, 0);
			break;
		}

		s_bathyWinParam.usageState = PLAYBACK_STATE;

		sprintf_s(szBuff1, sizeof(szBuff1), "%s Output Display", fiMBS.szTitleName);
		SetWindowText(hDlg, szBuff1);

		UpdateEnvBitmapDlg(&s_bathyWinParam);
		SendMessage(hDlg, WM_INITIALIZE_BATHYMAP_SETUP, NULL, NULL);
		SendMessage(hDlg, WM_3MB_REDRAWALL, NULL, NULL);
		PostMessage(g_hwndBathy, WM_UPDATE_ENV_ANIMATS, NULL, NULL);
		GetClientRect(hDlg, &rect);
		InvalidateRect(hDlg, &rect, TRUE); 
		break;

	case WM_COMMAND:
		wmId    = LOWORD(wParam);
		wmEvent = HIWORD(wParam);

		switch(wmId)
		{
		case IDCANCEL:
		case IDOK:
			//EndDialog(hDlg, LOWORD(wParam));
			PostMessage(hDlg, WM_CLOSE, NULL, NULL);
			return TRUE;

		case IDC_LOAD_SCENARIO:
			if(dlgParam->usageType == PLAYBACK_TYPE)
			{
				if(OK == (res = MyGetOpenFileName(&fiMBS, hDlg, SZ3MBBINOUTFILTER, SZ3MBBINOUTDEFEXT, &fileSelected)))
				{
					// No errors so check if user selected a file
					if(fileSelected == TRUE)
						PostMessage(hDlg, WM_3MBS_LOAD_SCENARIO, NULL, NULL);
					else
						break; // file not selected in so done here.
				}
				else
				{
					// Handle error.
					sprintf_s(szBuff1,
						sizeof(szBuff1),
						TEXT("Problem with file selected for playback (%s):  %s"),
						fiMBS.szTitleName,
						staticLib.MbsResultToString(res, szBuff2, sizeof(szBuff2)/sizeof(TCHAR)));
					MessageBox(hDlg, szBuff1, "Load File Error", 0);
					break;
				}
			}
			else if(dlgParam->usageType == SEED_SCENARIO_TYPE)
			{
				// Using the load scenario button to load in a shape file along with autoloading and
				// placement of species and animats.
				//EnableWindow(hDlg, FALSE);
				s_shapeFileDlgParam.pRefSce = dlgParam->pScenario;

				s_shapeFileDlgParam.pRefSce = dlgParam->pScenario;
				dlgResult = DialogBoxParam(NULL, MAKEINTRESOURCE(IDD_DIALOG_PDF_SEEDING_METHOD), hDlg,
								   (DLGPROC) LoadShapeFileDlgProc, (LPARAM) &s_shapeFileDlgParam);
				//EnableWindow(hDlg, TRUE);

				if(dlgResult == IDOK)
				{
					// Update the bitmap.
					SendMessage(g_hwndBathy, WM_UPDATE_ENV_ANIMATS, (WPARAM)0, (LPARAM)0);
				}
			}

			break;


		case IDC_BUTTON_ENVDLG_SPEC_ADD:
		case IDC_BUTTON_ENVDLG_FIRSANIMATACSTSRCE:
			//EnableWindow(g_hwndBathy, FALSE);

			g_bBlockNextMouseClick = TRUE;
			seedType = GetSeedTypeRadioState();

			//--------------------------------------------------------------------------//
			// Prompt the user for a file to open
			//-----------------------------------//
			// Set up the appropriate open file filter for either loading in animat
			// species or source sources
			// Get the name of the species or sound source file the user wants to load
			// in.  Return (break) if user hits cancel.
			// MyGetOpenFileName() returns OK even if the user doesn't select a file.
			if(wmId == IDC_BUTTON_ENVDLG_SPEC_ADD)
				res = MyGetOpenFileName(&fiSpe, hDlg, SZSPEFILTER, SZSPEDEFEXT, &bVal);
			else
				res = MyGetOpenFileName(&fiSpe, hDlg, szSrcFilter, szSrcDefExt, &bVal);

			if(OK == res)
			{
				// No errors so check if user selected a file
				if(bVal == FALSE)
				{
					g_bBlockNextMouseClick = FALSE;
					break; // file not selected so done here.
				}

			}
			else if(FILE_PATH_TOO_LONG == res)
			{
				// Handle error.  Prepare the error message text.
				sprintf_s(szBuff1,
					sizeof(szBuff1)/sizeof(TCHAR),
					TEXT("Problem with selected species or sound source file %s:  %s"),
					fiSpe.szTitleName,
					staticLib.MbsResultToString(res, szBuff2, sizeof(szBuff2)/sizeof(TCHAR)));

				// Prepare the caption text
				sprintf_s(szBuff2,
					sizeof(szBuff2)/sizeof(TCHAR),
					TEXT("File Path Length Exceeds %d characters"), MAX_PATH-1);
				MessageBox(hDlg, szBuff1, szBuff2, 0);
				g_bBlockNextMouseClick = FALSE;
				break;
			}
			else
			{
				sprintf_s(szBuff1,
					sizeof(szBuff1)/sizeof(TCHAR),
					TEXT("Problem encountered while selecting species or sound source:  %s"),
					staticLib.MbsResultToString(res, szBuff2, sizeof(szBuff2)/sizeof(TCHAR)));
				MessageBox(hDlg, szBuff1, "Select File Error", 0);
				g_bBlockNextMouseClick = FALSE;
				break;
			}
			//--------------------------------------------------------------------------//


			// No errors using the GetOpenFileName function.  Add the user-selected
			// species or sound source to the scenario.  Both are treated the
			// same way when loading in because the sound source is simply a special case
			// of the species definition.
			if(OK != (res = dlgParam->pScenario->AddSpecies(fiSpe.szFileName, &speciesAddIndex)))
			{
				sprintf_s(szBuff1,
					sizeof(szBuff1)/sizeof(TCHAR),
					TEXT("Problem loading species or sound source file %s:  %s"),
					fiSpe.szTitleName,
					staticLib.MbsResultToString(res, szBuff2, sizeof(szBuff2)/sizeof(TCHAR)));
				MessageBox(hDlg, szBuff1, "Load File Error", 0);
				g_bBlockNextMouseClick = FALSE;
				break;
			}

			// If a sound source was added and only single sound sources are allowed then disable the ability to add any
			// further sound source models.
			if(wmId == IDC_BUTTON_ENVDLG_FIRSANIMATACSTSRCE)
			{
				if(dlgParam->pScenario->SpeciesIsASoundSourceModel(speciesAddIndex) == FALSE)
				{
					MessageBox(hDlg, "Sorry, this file is not truely a sound source file", "Not A Sound Source File", 0);
					dlgParam->pScenario->DeleteSpecies(speciesAddIndex);
					g_bBlockNextMouseClick = FALSE;
					break;
				}
				else
				{
					bVal = dlgParam->pScenario->SoundSourceSpeciesPresent();
					_ASSERT(TRUE == bVal);
				}
			}

			// Get the species or sound source display name without the extension so it may
			// be displayed in the list box.
			strncpy_s(szBuff, sizeof(szBuff), fiSpe.szTitleName, BUFFERED_MAX_PATH);
			staticLib.RemoveExtension(szBuff);

			// Set the species display title in the scenario class.  It will occupy the
			// last index.
			//dlgParam->pScenario->SetSpeciesDisplayTitle(dlgParam->pScenario->GetSpeciesCount()-1, szBuff);
			dlgParam->pScenario->SetSpeciesDisplayTitle(speciesAddIndex, szBuff);

			// Prepare and display the species title (without the extension) in the list box.
			sprintf_s(szBuff, sizeof(szBuff), "%s (unpopulated)", szBuff);
			//SendMessage(GetDlgItem(hDlg, IDC_LIST_SPECIES),	LB_ADDSTRING, (WPARAM)0, (LPARAM)szBuff);
			SendMessage(GetDlgItem(hDlg, IDC_LIST_SPECIES),	LB_INSERTSTRING, (WPARAM)speciesAddIndex, (LPARAM)szBuff);

			// In the list box the new species index will be the number of items in the
			// list box minus 1.
			activeLB.index.spe = speciesAddIndex; //SendMessage(GetDlgItem(hDlg, IDC_LIST_SPECIES), LB_GETCOUNT, (WPARAM)0, (LPARAM)0) - 1;

			// Set the species' list box's cursor to highlight the newly added species.
			SendMessage(GetDlgItem(hDlg, IDC_LIST_SPECIES),	LB_SETCURSEL,(WPARAM)activeLB.index.spe, (LPARAM)0);

			// Clear the individual, pod, and pod member lists because they will not have
			// been populated yet.
			SendMessage(GetDlgItem(hDlg, IDC_LIST_INDIVIDUAL), LB_RESETCONTENT, (WPARAM)0, (LPARAM)0); // The individual list box
			SendMessage(GetDlgItem(hDlg, IDC_LIST_PODS), LB_RESETCONTENT, (WPARAM)0, (LPARAM)0); // The pod list box
			SendMessage(GetDlgItem(hDlg, IDC_LIST_SEL_POP),	LB_RESETCONTENT, (WPARAM)0, (LPARAM)0); // The pod member list box

			// Make sure add choice is either add pod or add individual and method is by
			// mouse click.
			if(seedType == ADD_POD_MEMBER || seedType == ADD_POD)
				SetSeedTypeRadioState(ADD_POD);
			else
				SetSeedTypeRadioState(ADD_INDIVIDUAL);
			SetAddMethodStateRadio(MOUSECLICK);

			// Update the Gui dialog box and the bathymetry map. 
			UpdateEnvBitmapDlg(&s_bathyWinParam);
			SendMessage(g_hwndBathy, WM_UPDATE_ENV_ANIMATS, (WPARAM)0, (LPARAM)0);

			memset(&s_blockInputThreadInf, 0, sizeof(s_blockInputThreadInf));
			s_blockInputThreadInf.hdl =
				CreateThread(NULL, 0, &DelayInputToBathyBitmapWindowThread, &s_blockInputThreadInf, 0, &s_blockInputThreadInf.id);
			while(s_blockInputThreadInf.running == FALSE)
				Sleep(1);
			s_blockInputThreadInf.noRepaint = TRUE;
			break;

		case IDC_BUTTON_ENVDLG_INDIVIDUAL_DELETE:
			activeLB = GetActiveListBox();
			_ASSERTE(activeLB.lstbox == INDIVIDUAL_LB);

			// Remove the selected individual from the scenario.
			dlgParam->pScenario->DeleteIndividual(activeLB.index.spe, activeLB.index.ind);

			// Update list boxes.
			SendMessage(GetDlgItem(hDlg, IDC_LIST_INDIVIDUAL), LB_DELETESTRING, (WPARAM)activeLB.index.ind, (LPARAM)0);

			// Redraw the listing in the species list box that was modified, then put the
			// cursor on the list being modified.
			UpdateGuiSpeciesListWindow(GetDlgItem(hDlg, IDC_LIST_SPECIES), activeLB.index.spe);
			SendMessage(GetDlgItem(hDlg, IDC_LIST_SPECIES), LB_SETCURSEL, activeLB.index.spe, (LPARAM)0);

			// Update the state change throughout the GUI.
			activeLB.index.ind = activeLB.index.pod = activeLB.index.pdm = -1;
			s_prevActiveLB = SetActiveListBox(activeLB.index);

			// Handle the case where the deleted animat is from a sound source species.
			bVal = CBitmapEnvFunctions::SpeciesIsASoundSourceModel(s_prevActiveLB.index.spe);
			if(bVal == TRUE)
			{
				s_prevSeedType = SetSeedTypeRadioState(ADD_INDIVIDUAL);
				s_prevAddMethod = SetAddMethodStateRadio(MOUSECLICK);
			}
			else
			{
				s_prevSeedType = SetSeedTypeRadioState(s_prevSeedType);
				s_prevAddMethod = SetAddMethodStateRadio(s_prevAddMethod);
			}

			UpdateEnvBitmapDlg(&s_bathyWinParam);
			SendMessage(g_hwndBathy, WM_UPDATE_ENV_ANIMATS, (WPARAM)0, (LPARAM)0);

			break;

		case IDC_BUTTON_ENVDLG_POD_DELETE:
			activeLB = GetActiveListBox();
			_ASSERTE(activeLB.lstbox == POD_LB);

			// Remove the selected pod and its members from the scenario.
			dlgParam->pScenario->DeletePod(activeLB.index.spe, activeLB.index.pod);

			// Update list boxes.
			SendMessage(GetDlgItem(hDlg, IDC_LIST_PODS), LB_RESETCONTENT, (WPARAM)0, (LPARAM)0); // The pod list box
			SendMessage(GetDlgItem(hDlg, IDC_LIST_SEL_POP),	LB_RESETCONTENT, (WPARAM)0, (LPARAM)0); // The pod member list box

			// Repopulate the pod list box
			numPods = CBitmapEnvFunctions::GetPodCount(activeLB.index.spe);
			for(i=0; i<numPods; i++)
			{
				MbsGetPodSummaryString(activeLB.index.spe, i, szBuff, SIZE_32);
				SendMessage(GetDlgItem(hDlg, IDC_LIST_PODS), LB_ADDSTRING, 0, (LPARAM)szBuff);
			}

			// Redraw the listing in the species list box that was modified, then put the
			// cursor on the list being modified.
			UpdateGuiSpeciesListWindow(GetDlgItem(hDlg, IDC_LIST_SPECIES), activeLB.index.spe);
			SendMessage(GetDlgItem(hDlg, IDC_LIST_SPECIES), LB_SETCURSEL, activeLB.index.spe, (LPARAM)0);

			// Update the state change throughout the GUI.
			activeLB.index.ind = activeLB.index.pod = activeLB.index.pdm = -1;
			s_prevActiveLB = SetActiveListBox(activeLB.index);
			s_prevSeedType = SetSeedTypeRadioState(s_prevSeedType);
			s_prevAddMethod = SetAddMethodStateRadio(s_prevAddMethod);

			UpdateEnvBitmapDlg(&s_bathyWinParam);
			SendMessage(g_hwndBathy, WM_UPDATE_ENV_ANIMATS, (WPARAM)0, (LPARAM)0);
			break;

		case IDC_BUTTON_ENVDLG_PODMEMBER_DELETE:
			activeLB = GetActiveListBox();
			_ASSERTE(activeLB.lstbox == MEMBER_LB);

			// Remove the selected pod members from the selected pod.
			dlgParam->pScenario->DeletePodMember(activeLB.index.spe, activeLB.index.pod, activeLB.index.pdm);

			// Update the species list box, maintain the cursor.
			UpdateGuiSpeciesListWindow(GetDlgItem(hDlg, IDC_LIST_SPECIES), activeLB.index.spe);
			SendMessage(GetDlgItem(hDlg, IDC_LIST_SPECIES), LB_SETCURSEL, activeLB.index.spe, (LPARAM)0);


			// Update the Pod list box, maintain the cursor
			SendMessage(GetDlgItem(hDlg, IDC_LIST_PODS), LB_DELETESTRING, (WPARAM)activeLB.index.pod, (LPARAM)0);
			MbsGetPodSummaryString(activeLB.index.spe, activeLB.index.pod, szBuff, SIZE_32);
			SendMessage(GetDlgItem(hDlg, IDC_LIST_PODS), LB_INSERTSTRING, (WPARAM)activeLB.index.pod, (LPARAM)szBuff);
			SendMessage(GetDlgItem(hDlg, IDC_LIST_PODS), LB_SETCURSEL, activeLB.index.pod, (LPARAM)0);

			// Update the pod member list box, no cursor maintained.
			SendMessage(GetDlgItem(hDlg, IDC_LIST_SEL_POP), LB_DELETESTRING, (WPARAM)activeLB.index.pdm, (LPARAM)0);

			// Update the state change throughout the GUI.
			activeLB.index.ind = activeLB.index.pdm = -1;
			s_prevActiveLB = SetActiveListBox(activeLB.index);
			UpdateEnvBitmapDlg(&s_bathyWinParam);
			SendMessage(g_hwndBathy, WM_UPDATE_ENV_ANIMATS, (WPARAM)0, (LPARAM)0);
			break;

		case IDC_BUTTON_TOGGLE_DATPTS:
			s_bathyWinParam.bDataPoints = !s_bathyWinParam.bDataPoints;
			SendMessage(g_hwndBathy, WM_UPDATE_TOGGLED_INF, (WPARAM)0, (LPARAM)0);
			break;

		case IDC_BUTTON_TOGGLE_SLOPEREGIONS:
			s_bathyWinParam.bSlopeHeading = !s_bathyWinParam.bSlopeHeading;
			SendMessage(g_hwndBathy, WM_3MB_TOGGLE_SLOPEHEADING, (WPARAM)0, (LPARAM)0);
			break;

		case IDC_BUTTON_DENSITYDISTRIBUTE:
			activeLB = GetActiveListBox();
			_ASSERTE(activeLB.lstbox == SPECIES_LB);
			PostMessage(hDlg, WM_SEED_COORDINATE, NULL, NULL);
			break;

		case IDC_BUTTON_PODSIZEINC:
			activeLB = GetActiveListBox();
			_ASSERTE(activeLB.lstbox == (int)POD_LB);

			// Add the pod member
			AddAnimatToPod(activeLB.index.spe, activeLB.index.pod, &s_bathyWinParam, dlgParam->pScenario);

			podSize = CBitmapEnvFunctions::GetPodMemberCount(activeLB.index.spe, activeLB.index.pod);

			// Update the species list box, maintain the cursor.
			UpdateGuiSpeciesListWindow(GetDlgItem(hDlg, IDC_LIST_SPECIES), activeLB.index.spe);
			SendMessage(GetDlgItem(hDlg, IDC_LIST_SPECIES), LB_SETCURSEL, activeLB.index.spe, (LPARAM)0);


			// Update the Pod list box, maintain the cursor
			SendMessage(GetDlgItem(hDlg, IDC_LIST_PODS), LB_DELETESTRING, (WPARAM)activeLB.index.pod, (LPARAM)0);
			MbsGetPodSummaryString(activeLB.index.spe, activeLB.index.pod, szBuff, SIZE_32);
			SendMessage(GetDlgItem(hDlg, IDC_LIST_PODS), LB_INSERTSTRING, (WPARAM)activeLB.index.pod, (LPARAM)szBuff);
			SendMessage(GetDlgItem(hDlg, IDC_LIST_PODS), LB_SETCURSEL, activeLB.index.pod, (LPARAM)0);

			// Update the pod member list box, no cursor maintained.
			activeLB.index.pdm = podSize-1;
			inhabitantInf = CBitmapEnvFunctions::GetPodMemberInitialCoordinate(activeLB.index.spe, activeLB.index.pod, activeLB.index.pdm);
			sprintf_s(szBuff, sizeof(szBuff), "%14.10f, %14.10f", inhabitantInf.coord.lat, inhabitantInf.coord.lon);
			SendMessage(GetDlgItem(hDlg, IDC_LIST_SEL_POP), LB_ADDSTRING, 0, (LPARAM)szBuff);

			// Update the state change throughout the GUI.
			UpdateEnvBitmapDlg(&s_bathyWinParam);
			SendMessage(g_hwndBathy, WM_UPDATE_ENV_ANIMATS, (WPARAM)0, (LPARAM)0);
			break;

		case IDC_BUTTON_PODSIZEDEC:
			activeLB = GetActiveListBox();
			_ASSERTE(activeLB.lstbox == POD_LB);
			podSize = CBitmapEnvFunctions::GetPodMemberCount(activeLB.index.spe, activeLB.index.pod);

			// Remove the selected pod members from the selected pod.
			dlgParam->pScenario->DeletePodMember(activeLB.index.spe, activeLB.index.pod, podSize-1);

			// Update the species list box, maintain the cursor.
			UpdateGuiSpeciesListWindow(GetDlgItem(hDlg, IDC_LIST_SPECIES), activeLB.index.spe);
			SendMessage(GetDlgItem(hDlg, IDC_LIST_SPECIES), LB_SETCURSEL, activeLB.index.spe, (LPARAM)0);


			// Update the Pod list box, maintain the cursor
			SendMessage(GetDlgItem(hDlg, IDC_LIST_PODS), LB_DELETESTRING, (WPARAM)activeLB.index.pod, (LPARAM)0);
			MbsGetPodSummaryString(activeLB.index.spe, activeLB.index.pod, szBuff, SIZE_32);
			SendMessage(GetDlgItem(hDlg, IDC_LIST_PODS), LB_INSERTSTRING, (WPARAM)activeLB.index.pod, (LPARAM)szBuff);
			SendMessage(GetDlgItem(hDlg, IDC_LIST_PODS), LB_SETCURSEL, activeLB.index.pod, (LPARAM)0);

			// Update the pod member list box, no cursor maintained.
			SendMessage(GetDlgItem(hDlg, IDC_LIST_SEL_POP), LB_DELETESTRING, (WPARAM)podSize-1, (LPARAM)0);

			// Update the state change throughout the GUI.
			UpdateEnvBitmapDlg(&s_bathyWinParam);
			SendMessage(g_hwndBathy, WM_UPDATE_ENV_ANIMATS, (WPARAM)0, (LPARAM)0);
			break;


		case IDC_BUTTON_ENVDLG_SPEC_REMOVE:
			activeLB = GetActiveListBox();
			seedType = GetSeedTypeRadioState();
			_ASSERTE(activeLB.lstbox != NONE_LB);

			// Remove the species from the scenario.
			dlgParam->pScenario->DeleteSpecies(activeLB.index.spe);

			// Update list boxes.
			SendMessage(GetDlgItem(hDlg, IDC_LIST_SPECIES),		LB_DELETESTRING, (WPARAM)activeLB.index.spe, (LPARAM)0);
			SendMessage(GetDlgItem(hDlg, IDC_LIST_SPECIES),		LB_SETCURSEL,	 (WPARAM)-1, (LPARAM)0); // The species list box
			SendMessage(GetDlgItem(hDlg, IDC_LIST_INDIVIDUAL),	LB_RESETCONTENT, (WPARAM)0, (LPARAM)0); // The individual list box
			SendMessage(GetDlgItem(hDlg, IDC_LIST_PODS),		LB_RESETCONTENT, (WPARAM)0, (LPARAM)0); // The pod list box
			SendMessage(GetDlgItem(hDlg, IDC_LIST_SEL_POP),	LB_RESETCONTENT, (WPARAM)0, (LPARAM)0); // The pod member list box

			// Make sure add choice is either add pod or add individual and method is by
			// mouse click.
			if(seedType == ADD_POD_MEMBER || seedType == ADD_POD)
				s_prevSeedType = SetSeedTypeRadioState(ADD_POD);
			else
				s_prevSeedType = SetSeedTypeRadioState(ADD_INDIVIDUAL);
			s_prevAddMethod = SetAddMethodStateRadio(MOUSECLICK);

			// Update the state change throughout the GUI.
			UpdateEnvBitmapDlg(&s_bathyWinParam);
			SendMessage(g_hwndBathy, WM_UPDATE_ENV_ANIMATS, (WPARAM)0, (LPARAM)0);
			break;

		case IDC_LIST_SPECIES:
			switch(wmEvent)
			{
			//case LBN_ERRSPACE:
			//case LBN_KILLFOCUS:
			//case LBN_SELCANCEL:
			//case LBN_SETFOCUS:
			//case LBN_DBLCLK:
			//	break;
			case LBN_SELCHANGE:// Species list box (IDC_LIST_SPECIES)

				/* four possibilities to consider:
				Situation (2) LISTBOX(same),    SPECIES(changed)
				RESULT: Remove prev species, pod, and pod member lists and replace with new species
						individuals and pod lists.
						Set radio add type to either individual or pod.

				Situation (3) LISTBOX(changed), SPECIES(same)
				RESULT: Dehighlight individual and pod lists
						Remove pod member list.
						Set radio add type to either individual or pod.

				Situation (4) LISTBOX(changed), SPECIES(changed)
				RESULT: Same as (2)

				Note 1: No matter which situation other than (1), the radio add type is
						  reset to either individual or pod.
				Note 2: Pod member list is always cleared out.
				*/

				// If there is no change in listbox state then break.
				activeLB = GetActiveListBox();
				if((activeLB.lstbox==SPECIES_LB) && (memcmp(&activeLB, &s_prevActiveLB, sizeof(ACTIVE_LISTBOX))==0))
					break;

				// The user will have clicked on the species list box but its proper state
				// needs to be set here because immediately after clicking it may be in an
				// indeterminate state.

				// The list box state is currently at something other than SPECIES_LB or
				// its index changed.  In either case, set the indices to a state the
				// properly results in the list box state set to SPECIES_LB by setting
				// ALL BUT the species index to -1.
				activeLB.index.ind = activeLB.index.pod = activeLB.index.pdm = -1;
				activeLB = SetActiveListBox(activeLB.index);

				// A Different species index then previoius was clicked on by the user.
				if(activeLB.index.spe != s_prevActiveLB.index.spe)
				{
					// Delete all lists in the individual, pod, and pod member lists.
					SendMessage(GetDlgItem(hDlg, IDC_LIST_INDIVIDUAL), LB_RESETCONTENT, 0, (LPARAM)0); // The individual list box
					SendMessage(GetDlgItem(hDlg, IDC_LIST_PODS), LB_RESETCONTENT, 0, (LPARAM)0); // The pod list box

					// Draw the individuals.
					numAnimats = CBitmapEnvFunctions::GetIndividualCount(activeLB.index.spe);
					subsetCoordArray = AdjustCoordinateBufferSize(subsetCoordArray, &subsetCoordLength, numAnimats);
					CBitmapEnvFunctions::GetIndividualInitialCoordinates(activeLB.index.spe, subsetCoordArray);

					// Repopulate the individual list box 
					for(i=0; i<numAnimats && i < MAX_NUMBER_ANIAMTS_IN_LIST_BOX; i++)
					{
						flat = (float)subsetCoordArray[i].coord.lat;
						flon = (float)subsetCoordArray[i].coord.lon;
						sprintf_s(szBuff, sizeof(szBuff), "%14.10f, %14.10f", flat, flon);
						SendMessage(GetDlgItem(hDlg, IDC_LIST_INDIVIDUAL), LB_ADDSTRING, 0, (LPARAM)szBuff);
					}

					// Repopulate pod list box
					numPods = CBitmapEnvFunctions::GetPodCount(activeLB.index.spe);
					for(i=0; i<numPods; i++)
					{
						MbsGetPodSummaryString(activeLB.index.spe, i, szBuff, SIZE_32);
						SendMessage(GetDlgItem(hDlg, IDC_LIST_PODS), LB_ADDSTRING, 0, (LPARAM)szBuff);
					}
				}
				else 
				{
					// The user clicked on the same species index but the changed from POD, POD MEMBER, or INDIVIDUAL.
					// Remove highlights from individual and pod lists, remove pod member list.
					SendMessage(GetDlgItem(hDlg, IDC_LIST_INDIVIDUAL),	LB_SETCURSEL, (WPARAM)-1, (LPARAM)0); // The individual list box
					SendMessage(GetDlgItem(hDlg, IDC_LIST_PODS),		LB_SETCURSEL, (WPARAM)-1, (LPARAM)0); // The pod list box
					SendMessage(GetDlgItem(hDlg, IDC_LIST_SEL_POP),	LB_RESETCONTENT, (WPARAM)0, (LPARAM)0); // The pod member list box
				}

				// See Note 1.
				// Set the radio add type to either individual or pod.
				bVal = CBitmapEnvFunctions::SpeciesIsASoundSourceModel(activeLB.index.spe);
				if(s_prevSeedType == ADD_NONE || bVal == TRUE)
					SetSeedTypeRadioState(ADD_INDIVIDUAL);
				else if(s_prevSeedType == ADD_POD_MEMBER)
					SetSeedTypeRadioState(ADD_POD);
				else
					SetSeedTypeRadioState(s_prevSeedType);
				seedType = GetSeedTypeRadioState();


				if(seedType == ADD_INDIVIDUAL)
				{
					if(s_prevAddMethod == BY_DENSITY || s_prevAddMethod == NOT_SET || bVal == TRUE)
						s_prevAddMethod = MOUSECLICK;
				}
				else if(seedType == ADD_POD)
				{
					if(s_prevAddMethod == SHAPE_FILE || s_prevAddMethod == BY_DENSITY || s_prevAddMethod == NOT_SET)
						s_prevAddMethod = MOUSECLICK;
				}
				s_prevAddMethod = SetAddMethodStateRadio(s_prevAddMethod);

				// See Note 2.
				SendMessage(GetDlgItem(hDlg, IDC_LIST_SEL_POP),	LB_RESETCONTENT, 0, (LPARAM)0); // The pod member list box

				// Update the GUI
				UpdateEnvBitmapDlg(&s_bathyWinParam);

				// Update the bitmap.
				SendMessage(g_hwndBathy, WM_UPDATE_ENV_ANIMATS, (WPARAM)0, (LPARAM)0);

				// List box was modified
				s_prevActiveLB = GetActiveListBox();;
				s_prevSeedType = GetSeedTypeRadioState();
				break;
			}
			break;

		case IDC_LIST_INDIVIDUAL:
			switch(wmEvent)
			{
			case LBS_NOTIFY: // Individual list box (IDC_LIST_INDIVIDUAL)
				/* Three possibilities to consider:
				Situation (1) LISTBOX(same),    INDIVIDUAL(same)
				SUMMARY: User reclicked on same individual already highlighted.  No change
						 in state.
				RESULT:  break

				Situation (2) LISTBOX(same),    INDIVIDUAL(changed)
				SUMMARY: A different individual has been selected
				RESULT:  Only the bit map needs to be updated.


				Situation (3) LISTBOX(changed)
				SUMMARY: User has gone from either pod or pod member list boxes to the
						 individual list box.
				RESULT:  Remove clear out pod member list and dehighlight pod list.
						 Set radio add type to either individual or pod.

				*/
				// If user clicked on same individual list box as before then no state change so break
				activeLB = GetActiveListBox();
				if((activeLB.lstbox==INDIVIDUAL_LB) && (memcmp(&activeLB, &s_prevActiveLB, sizeof(ACTIVE_LISTBOX))==0))
					break;

				// User has "newly" clicked on the individual box after some other list
				// box had been active. Do not modify previous states but get set current
				// ones.
				activeLB.index.pod = activeLB.index.pdm = -1;
				s_prevActiveLB = activeLB = SetActiveListBox(activeLB.index);

				seedType = GetSeedTypeRadioState();
				if(seedType == ADD_POD_MEMBER)
					s_prevSeedType = SetSeedTypeRadioState(ADD_INDIVIDUAL);

				// Siuation 2 is handled by default.
				// Update the GUI
				UpdateEnvBitmapDlg(&s_bathyWinParam);

				// Update the bitmap.
				SendMessage(g_hwndBathy, WM_UPDATE_ENV_ANIMATS, (WPARAM)0, (LPARAM)0);
				break;
			}
			break;

		case IDC_LIST_PODS:
			switch(wmEvent)
			{
			case LBS_NOTIFY: // The pod list box (IDC_LIST_PODS)
				// The user clicked on the pod list box.  
				
				/* Three possibilities to consider:
				Situation (1) LISTBOX(same),    POD(same)
				SUMMARY: User reclicked on same pod already highlighted.  No state change
				RESULT:  break

				Situation (2) LISTBOX(same),    POD(changed)
				SUMMARY: A different pod has been selected
				RESULT:  Clear out existing pod member list, repopulate with new pod.


				Situation (3) LISTBOX(changed)  POD (same)
				SUMMARY: User has gone from pod member list to the pod list
				RESULT:  Clear out pod member list and populate.


				Situation (4) LISTBOX(changed)  POD (changed)
				SUMMARY: User has gone from individual or species list to the pod list.
				RESULT:  Dehighlight the individual list.
						 Populate the pod member list corresponding to this pod.

				Note (1): In all situations (except 1) the pod member list gets cleared
						  then repopulated.
				*/
				// Situation (1)
				// Verify there's been a change in state, break if not.

				// For the list boxes here GetActiveListBox() returns the list box state
				// before the user click on the species list box.
				activeLB = GetActiveListBox();
				if((activeLB.lstbox==POD_LB) && (memcmp(&activeLB, &s_prevActiveLB, sizeof(ACTIVE_LISTBOX))==0))
					break;

				// User has clicked on the pod list after a different list box or
				// different pod index had been highlighted. Do not modify previous
				// states but get set current ones.
				activeLB.index.ind = activeLB.index.pdm = -1;
				activeLB = SetActiveListBox(activeLB.index);
				seedType = GetSeedTypeRadioState();
/*
				// Situation where the previous highlighted list box wasn't the pod's.
				// Could have been either species, individual, or pod member.
				if(activeLB.lstbox != s_prevActiveLB.lstbox)
				{
					// Dehighlighted the individual list and pod list
					SendMessage(GetDlgItem(hDlg, IDC_LIST_INDIVIDUAL),	LB_SETCURSEL, (WPARAM)-1, (LPARAM)0);
				}
*/

				// For when the user toggles back to Pod List if they were had clicked on
				// a different list.
				//if(s_prevSeedType == ADD_POD_MEMBER)
				//	s_prevAddMethod = SetSeedTypeRadioState(ADD_INDIVIDUAL);

				numAnimats = CBitmapEnvFunctions::GetPodMemberCount(activeLB.index.spe, activeLB.index.pod);
				subsetCoordArray = AdjustCoordinateBufferSize(subsetCoordArray, &subsetCoordLength, numAnimats);
				CBitmapEnvFunctions::GetPodInitialCoordinates(activeLB.index.spe, activeLB.index.pod, subsetCoordArray);

				// Note (1) for situations (2), (3), and (4): Clear members, then 
				// repopulate the pod member box.
				SendMessage(GetDlgItem(hDlg, IDC_LIST_SEL_POP),	LB_RESETCONTENT, 0, (LPARAM)0); // The pod members list box
				for(i=0; i<numAnimats && i < MAX_NUMBER_ANIAMTS_IN_LIST_BOX; i++)
				{
					sprintf_s(szBuff, sizeof(szBuff), "%14.10f, %14.10f", subsetCoordArray[i].coord.lat, subsetCoordArray[i].coord.lon);
					SendMessage(GetDlgItem(hDlg, IDC_LIST_SEL_POP), LB_ADDSTRING, 0, (LPARAM)szBuff);
				}

				// Update the GUI
				UpdateEnvBitmapDlg(&s_bathyWinParam);

				// Update the bitmap.
				SendMessage(g_hwndBathy, WM_UPDATE_ENV_ANIMATS, (WPARAM)0, (LPARAM)0);
				s_prevActiveLB = GetActiveListBox();
				break;
			}
			break;

		case IDC_LIST_SEL_POP:  // Pod member list box.
			switch(wmEvent)
			{

			case LBS_NOTIFY: // The pod member list box (IDC_LIST_SEL_POP)
				// The user clicked on one of the the pod member list box lists.
				
				/* Three possibilities to consider:
				Situation (1) LISTBOX(same),    PODMEMBER(same)
				SUMMARY: User reclicked on same pod member already highlighted.  No state change
				RESULT:  break

				Situation (2) LISTBOX(same),    PODMEMBER(changed)
				SUMMARY: A different pod member has been selected
				RESULT:  Only the bitmap needs to be updated.

				Situation (3) LISTBOX(changed)
				SUMMARY: User has gone from the pod list to the pod member list.
				RESULT:  Onnly the bitmap needs to be updated.

				Note 1: The only thing that happens if there has been a state change is 
						updating the bitmap.
				*/

				// Situation (1)
				// Verify there's been a change in state, break if not.
				activeLB = GetActiveListBox();
				if((activeLB.lstbox==MEMBER_LB) && (memcmp(&activeLB, &s_prevActiveLB, sizeof(ACTIVE_LISTBOX))==0))
					break; // No state change.

				activeLB.index.ind = -1;
				s_prevActiveLB = SetActiveListBox(activeLB.index);

				//if(s_prevSeedType == ADD_POD_MEMBER)
				//	s_prevAddMethod = SetSeedTypeRadioState(ADD_INDIVIDUAL);


				// Update the GUI
				UpdateEnvBitmapDlg(&s_bathyWinParam);

				// Situations (2) and (3).
				// Update the bitmap.
				SendMessage(g_hwndBathy, WM_UPDATE_ENV_ANIMATS, (WPARAM)0, (LPARAM)0);
				break;
			}
			
			break;



		//------------------------------------------------------------------------------//
		// "Seed Type" (Individual, Pod, or Pod Members) Radio Buttons within "Populants"
		//  Group Box
		//------------------------------------------------------------------------------//
		case IDC_RADIO_SEED_INDIVIDUAL:
			// If no change in state, break.
			if(s_prevSeedType == ADD_INDIVIDUAL)
				break;

			// Adding by density potenailly adds thousands of animats so force user to
			// confirm that choice.
			addMethod = GetAddMethodRadioState();
			if(addMethod == BY_DENSITY)
			{
				s_prevAddMethod = SetAddMethodStateRadio(MOUSECLICK);
			}
			else if(addMethod == NOT_SET)
			{
				if(s_prevAddMethod == BY_DENSITY)
					s_prevAddMethod = SetAddMethodStateRadio(MOUSECLICK);
				else
					s_prevAddMethod = SetAddMethodStateRadio(s_prevAddMethod);
			}

			s_prevSeedType = GetSeedTypeRadioState();
			// Update the dialog box.
			UpdateEnvBitmapDlg(&s_bathyWinParam);
			SendMessage(g_hwndBathy, WM_UPDATE_ENV_ANIMATS, (WPARAM)0, (LPARAM)0);
			break;
		case IDC_RADIO_ADD_POD:
			// If no change in state, break.
			if(s_prevSeedType == ADD_POD)
				break;

			// Adding by density potenailly adds thousands of animats so force user to
			// confirm that choice.  Adding pods by shape file is never allowed.
			addMethod = GetAddMethodRadioState();
			if(addMethod == BY_DENSITY || addMethod == SHAPE_FILE)
			{
				s_prevAddMethod = SetAddMethodStateRadio(MOUSECLICK);
			}
			else if(addMethod == NOT_SET)
			{
				if(s_prevAddMethod == BY_DENSITY)
					s_prevAddMethod = SetAddMethodStateRadio(MOUSECLICK);
				else
					s_prevAddMethod = SetAddMethodStateRadio(s_prevAddMethod);
			}

			s_prevSeedType = GetSeedTypeRadioState();
			UpdateEnvBitmapDlg(&s_bathyWinParam);
			SendMessage(g_hwndBathy, WM_UPDATE_ENV_ANIMATS, (WPARAM)0, (LPARAM)0);
			break;

		case IDC_BUTTON_DEBUG_ADDPODMEMBER:
			SetSeedTypeRadioState(ADD_POD_MEMBER);
			SetAddMethodStateRadio(MOUSECLICK);
			UpdateEnvBitmapDlg(&s_bathyWinParam);
			s_prevSeedType = GetSeedTypeRadioState();
			break;
		//------------------------------------------------------------------------------//
		// End of "Seed Type" (Individual, Pod, Pod Members) Radio Buttons within
		// "Populants" Group Box
		//------------------------------------------------------------------------------//


		//------------------------------------------------------------------------------//
		// "Seeding Methods" Radio Buttons within "Populants" Group Box
		//------------------------------------------------------------------------------//
		case IDC_RADIO_MOUSE_CLICKING: // "Seeding Methods"
			SetAddMethodStateRadio(MOUSECLICK); // Sets the states of the radio buttons.
			UpdateEnvBitmapDlg(&s_bathyWinParam);
			if(s_prevAddMethod == POLYGON)
				SendMessage(g_hwndBathy, WM_UPDATE_ENV_ANIMATS, (WPARAM)0, (LPARAM)0);
			s_prevAddMethod = GetAddMethodRadioState();
			break;

		case IDC_RADIO_N_ANIMATS_AROUND_CLICK:// "Seeding Methods"
			SetAddMethodStateRadio(N_AROUND_CLICK); // Sets the states of the radio buttons.
			UpdateEnvBitmapDlg(&s_bathyWinParam);
			if(s_prevAddMethod == POLYGON)
				SendMessage(g_hwndBathy, WM_UPDATE_ENV_ANIMATS, (WPARAM)0, (LPARAM)0);
			s_prevAddMethod = GetAddMethodRadioState();
			break;

		case IDC_RADIO_BOUNDING_BOX:// "Seeding Methods"
			SetAddMethodStateRadio(BOUNDINGBOX); // Sets the states of the radio buttons.
			UpdateEnvBitmapDlg(&s_bathyWinParam);
			if(s_prevAddMethod == POLYGON)
				SendMessage(g_hwndBathy, WM_UPDATE_ENV_ANIMATS, (WPARAM)0, (LPARAM)0);
			s_prevAddMethod = GetAddMethodRadioState();
			break;

		case IDC_RADIO_N_ANIMATS_OVERMAP:// "Seeding Methods"
			SetAddMethodStateRadio(THROUGHOUT_MAP); // Sets the states of the radio buttons.
			UpdateEnvBitmapDlg(&s_bathyWinParam);
			if(s_prevAddMethod == POLYGON)
				SendMessage(g_hwndBathy, WM_UPDATE_ENV_ANIMATS, (WPARAM)0, (LPARAM)0);
			s_prevAddMethod = GetAddMethodRadioState();
			break;

		case IDC_RADIO_POLYGON:// "Seeding Methods"
			SetAddMethodStateRadio(POLYGON); // Sets the states of the radio buttons.
			UpdateEnvBitmapDlg(&s_bathyWinParam);
			SendMessage(g_hwndBathy, WM_UPDATE_ENV_ANIMATS, (WPARAM)0, (LPARAM)0);
			s_prevAddMethod = GetAddMethodRadioState();
			break;

		case IDC_RADIO_ANIMATS_BY_DENSITY:// "Seeding Methods"
			SetAddMethodStateRadio(BY_DENSITY); // Sets the states of the radio buttons.
			UpdateEnvBitmapDlg(&s_bathyWinParam);
			if(s_prevAddMethod == POLYGON)
				SendMessage(g_hwndBathy, WM_UPDATE_ENV_ANIMATS, (WPARAM)0, (LPARAM)0);
			s_prevAddMethod = GetAddMethodRadioState();
			break;

		case IDC_RADIO_LOAD_SHAPE_FILE: // "Seeding Methods"
			if(s_shapeFileDlgParam.dlgHdl != NULL)
				break;
			SetAddMethodStateRadio(SHAPE_FILE); // Sets the states of the radio buttons.
			UpdateEnvBitmapDlg(&s_bathyWinParam);
			if(s_prevAddMethod == POLYGON)
				SendMessage(g_hwndBathy, WM_UPDATE_ENV_ANIMATS, (WPARAM)0, (LPARAM)0);
			s_prevAddMethod = GetAddMethodRadioState();
			break;
		//------------------------------------------------------------------------------//
		// End Of "Seeding Methods" Radio Buttons within "Populants" Group Box
		//------------------------------------------------------------------------------//



		case IDC_RADIO_PODLEADTYPE_ANIMAT: 
		case IDC_RADIO_PODLEADTYPE_CENTROID:
			activeLB = GetActiveListBox();
			if(wmId == IDC_RADIO_PODLEADTYPE_ANIMAT)
			{
				if(s_bathyWinParam.podLeaderType == ANIMAT)
					break; // no change in state.
				s_bathyWinParam.podLeaderType = ANIMAT;
			}
			else // wmEvent == IDC_RADIO_PODLEADTYPE_CENTROID
			{
				if(s_bathyWinParam.podLeaderType == CENTROID)
					break; // no change in state.
				s_bathyWinParam.podLeaderType = CENTROID;
			}
			if(activeLB.index.pod == -1)
				break; // done here.

			// Check for programming errors.
			_ASSERTE(activeLB.lstbox == POD_LB || activeLB.lstbox == MEMBER_LB);

			dlgParam->pScenario->SetPodLeaderType(activeLB.index.spe, activeLB.index.pod, s_bathyWinParam.podLeaderType);

			// Update the pod in the list box, and rehighlight it.
			MbsGetPodSummaryString(activeLB.index.spe, activeLB.index.pod, szBuff, SIZE_32);
			SendMessage(GetDlgItem(hDlg, IDC_LIST_PODS), LB_DELETESTRING, (WPARAM)activeLB.index.pod, (LPARAM)0);
			SendMessage(GetDlgItem(hDlg, IDC_LIST_PODS), LB_INSERTSTRING, (WPARAM)activeLB.index.pod, (LPARAM)szBuff);
			SendMessage(GetDlgItem(hDlg, IDC_LIST_PODS), LB_SETCURSEL,    (WPARAM)activeLB.index.pod, (LPARAM)0);
			break;


		case IDC_EDIT_NUM_ANIMATS:
			if(wmEvent == EN_CHANGE)
			{
				GetDlgItemText(hDlg, IDC_EDIT_NUM_ANIMATS, szBuff, SIZE_32);
				if(strncmp(szBuff, "", SIZE_16) == 0 || staticLib.StringIsALegalNumber(szBuff) == FALSE)
					break;
			
				s_bathyWinParam.addAmount = abs(atoi(szBuff));
				if(s_bathyWinParam.addAmount <= 0)
				{
					s_bathyWinParam.addAmount = 1;
					sprintf_s(szBuff, sizeof(szBuff), "%d", s_bathyWinParam.addAmount);
					SetDlgItemText(hDlg, IDC_EDIT_NUM_ANIMATS, szBuff);
				}
				if(s_bathyWinParam.addAmount > MAX_SEED_AT_ONCE)
				{
					s_bathyWinParam.addAmount = MAX_SEED_AT_ONCE;
					sprintf_s(szBuff, sizeof(szBuff), "%d", s_bathyWinParam.addAmount);
					SetDlgItemText(hDlg, IDC_EDIT_NUM_ANIMATS, szBuff);
				}
			
			}
			break;
		case IDC_EDIT_AVE_DIST:
			if(wmEvent == EN_CHANGE)
			{
				GetDlgItemText(hDlg, IDC_EDIT_AVE_DIST, szBuff, 20);
				if(strlen(szBuff) == 0 || staticLib.StringIsALegalNumber(szBuff) == FALSE)
					break;

				s_bathyWinParam.AveDist = atof(szBuff);
				if(s_bathyWinParam.AveDist < 0)
				{
					s_bathyWinParam.AveDist *= -1.0;
					sprintf_s(szBuff, sizeof(szBuff), "%.1f", s_bathyWinParam.AveDist);
					SetDlgItemText(hDlg, IDC_EDIT_AVE_DIST, szBuff);				
				}
			}
			break;

		case IDC_EDIT_DENSITY:
			if(wmEvent == EN_CHANGE)
			{
				GetDlgItemText(hDlg, IDC_EDIT_DENSITY, szBuff, 20);
				if(strlen(szBuff) == 0 || staticLib.StringIsALegalNumber(szBuff) == FALSE)
					break;

				s_bathyWinParam.seedDensityKm = atof(szBuff);

				// Negative signs can be entered by negative values here are not allowed
				// so force it positve and reset the edit control with sans the negative
				//sign.
				if(s_bathyWinParam.seedDensityKm < 0)
				{
					s_bathyWinParam.seedDensityKm *= -1.0;
					sprintf_s(szBuff, sizeof(szBuff), "%.6f", s_bathyWinParam.seedDensityKm);
					SetDlgItemText(hDlg, IDC_EDIT_DENSITY, szBuff);
				}
				sprintf_s(szBuff, sizeof(szBuff), "(add %d animats)", staticLib.CalcAnimatQTYByDensity(CBitmapEnvFunctions::GetBathymtryWaterSurfaceAreaMeters(), s_bathyWinParam.seedDensityKm));
				SetDlgItemText(hDlg, IDC_STATIC_DENSITYCALC, szBuff);
			}
			break;

		case IDC_EDIT_STD_DIST:
			if(wmEvent == EN_CHANGE)
			{
				GetDlgItemText(hDlg, IDC_EDIT_STD_DIST, szBuff, 20);
				if(strlen(szBuff) == 0 || staticLib.StringIsALegalNumber(szBuff) == FALSE)
					break;

				s_bathyWinParam.StdDevDist = atof(szBuff);
				if(s_bathyWinParam.StdDevDist < 0)
				{
					s_bathyWinParam.StdDevDist *= -1.0;
					sprintf_s(szBuff, sizeof(szBuff), "%.1f", s_bathyWinParam.StdDevDist);
					SetDlgItemText(hDlg, IDC_EDIT_STD_DIST, szBuff);
				}

			}
			break;

		case IDC_EDIT_ENVBITMAP_INIT_PODSIZE:
			if(wmEvent == EN_CHANGE)
			{
				GetDlgItemText(hDlg, IDC_EDIT_ENVBITMAP_INIT_PODSIZE, szBuff, SIZE_32);
				if(strncmp(szBuff, "", SIZE_16) == 0 || staticLib.StringIsALegalNumber(szBuff) == FALSE)
					break;
			
				s_bathyWinParam.initialPodSize = abs(atoi(szBuff));
				if(s_bathyWinParam.initialPodSize <= 0)
				{
					s_bathyWinParam.initialPodSize = 1;
					sprintf_s(szBuff, sizeof(szBuff), "%d", s_bathyWinParam.initialPodSize);
					SetDlgItemText(hDlg, IDC_EDIT_ENVBITMAP_INIT_PODSIZE, szBuff);
				}
				if(s_bathyWinParam.initialPodSize > MAX_SEED_AT_ONCE)
				{
					s_bathyWinParam.initialPodSize = MAX_SEED_AT_ONCE;
					sprintf_s(szBuff, sizeof(szBuff), "%d", s_bathyWinParam.initialPodSize);
					SetDlgItemText(hDlg, IDC_EDIT_ENVBITMAP_INIT_PODSIZE, szBuff);
				}

			}
			break;

		case IDC_EDIT_ENVBITMAP_FOCAL_DIST:
			if(wmEvent != EN_CHANGE)
				break;

			activeLB = GetActiveListBox();

			// Get the value stored in the focal distance edit box, verify it is a legal number and positive.
			GetDlgItemText(hDlg, IDC_EDIT_ENVBITMAP_FOCAL_DIST, szBuff, 20);
			if(strlen(szBuff) == 0 || staticLib.StringIsALegalNumber(szBuff) == FALSE)
				break;
			s_bathyWinParam.focalDist = atof(szBuff);
			if(s_bathyWinParam.focalDist < 0)
			{
				s_bathyWinParam.focalDist *= -1.0;
				sprintf_s(szBuff, sizeof(szBuff), "%.1f", s_bathyWinParam.focalDist);
				SetDlgItemText(hDlg, IDC_EDIT_ENVBITMAP_FOCAL_DIST, szBuff);
				break; // Resetting the edit box to a postive number will cause this to be called again.
			}

			// Only concerned if a pod or pod member is active (edit box shouldn't even be enbled if not)
			if((activeLB.lstbox != POD_LB && activeLB.lstbox != MEMBER_LB))
				break;

			//if(activeLB.index.pod == -1)
			//	break; // Done here.

			// Check for programming errors.
			//_ASSERTE(activeLB.lstbox == POD_LB || activeLB.lstbox == MEMBER_LB);
			_ASSERTE(activeLB.index.pod == SendMessage(GetDlgItem(hDlg, IDC_LIST_PODS), LB_GETCURSEL, (WPARAM)0, (LPARAM)0));

			dlgParam->pScenario->SetPodLeaderFocalDistance(activeLB.index.spe, activeLB.index.pod, s_bathyWinParam.focalDist);


			// Update the pod in the list box, and rehighlight it.
			MbsGetPodSummaryString(activeLB.index.spe, activeLB.index.pod, szBuff, SIZE_32);
			SendMessage(GetDlgItem(hDlg, IDC_LIST_PODS), LB_DELETESTRING, (WPARAM)activeLB.index.pod, (LPARAM)0);
			SendMessage(GetDlgItem(hDlg, IDC_LIST_PODS), LB_INSERTSTRING, (WPARAM)activeLB.index.pod, (LPARAM)szBuff);
			SendMessage(GetDlgItem(hDlg, IDC_LIST_PODS), LB_SETCURSEL,    (WPARAM)activeLB.index.pod, (LPARAM)0);		
			break;


		case IDC_TRACKTOGGLE_BUTTON:
			s_bathyWinParam.bAnimats = !s_bathyWinParam.bAnimats;
			UpdateEnvBitmapDlg(&s_bathyWinParam);
			PostMessage(g_hwndBathy, WM_UPDATE_ENV_ANIMATS, 0, 0);
			break;
		case IDC_VISUAL_INC_BUTTON:
			s_bathyWinParam.nPlaybackRate++;
			PlaybackIntegerToSeconds(s_bathyWinParam.nPlaybackRate, szBuff2, sizeof(szBuff2));
			sprintf_s(szBuff1, sizeof(szBuff1), "%s Output Playback (%s)", fiMBS.szTitleName, szBuff2);
			SetWindowText(hDlg, szBuff1);
			UpdateEnvBitmapDlg(&s_bathyWinParam);
			break;
		case IDC_VISUAL_DEC_BUTTON:
			s_bathyWinParam.nPlaybackRate--;
			PlaybackIntegerToSeconds(s_bathyWinParam.nPlaybackRate, szBuff2, sizeof(szBuff2));
			sprintf_s(szBuff1, sizeof(szBuff1), "%s Output Playback (%s)", fiMBS.szTitleName, szBuff2);
			SetWindowText(hDlg, szBuff1);
			UpdateEnvBitmapDlg(&s_bathyWinParam);
			break;
		case IDC_VISUAL_RESET_BUTTON:
			s_bathyWinParam.nPlaybackRate = 9;
			PlaybackIntegerToSeconds(s_bathyWinParam.nPlaybackRate, szBuff2, sizeof(szBuff2));
			sprintf_s(szBuff1, sizeof(szBuff1), "%s Output Playback (%s)", fiMBS.szTitleName, szBuff2);
			SetWindowText(hDlg, szBuff1);
			UpdateEnvBitmapDlg(&s_bathyWinParam);
			break;
		case IDC_VISUAL_STOP_BUTTON:
			s_bathyWinParam.playState = STOP;
			UpdateEnvBitmapDlg(&s_bathyWinParam);
			sprintf_s(szBuff1, sizeof(szBuff1), "%s Output Display", fiMBS.szTitleName);
			SetWindowText(hDlg, szBuff1);
			break;
		case IDC_VISUAL_PLAY_BUTTON: // Play/Pause button.
			switch(s_bathyWinParam.playState)
			{
			case STOP:
				s_bathyWinParam.playState = PLAY;
				break;
			case PLAY:
				s_bathyWinParam.playState = PAUSE;
				break;
			case PAUSE:
				s_bathyWinParam.playState = PLAY;
				break;
			}
			// If playing output then pause, otherwise play ouput.
			//sceInf.bPlayback = !sceInf.bPlayback;
			UpdateEnvBitmapDlg(&s_bathyWinParam);

			PlaybackIntegerToSeconds(s_bathyWinParam.nPlaybackRate, szBuff2, sizeof(szBuff2));
			sprintf_s(szBuff1, sizeof(szBuff1), "%s Output Playback (%s)", fiMBS.szTitleName, szBuff2);
			SetWindowText(hDlg, szBuff1);
			break;
		}
		// End of WM_COMMAND
		// Copy the current state into the previous state.
		break;
	}
	return FALSE;
}
