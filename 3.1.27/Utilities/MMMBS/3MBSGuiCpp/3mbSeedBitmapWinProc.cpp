#include "3mbSeedBitmapWinProc.h"
#include "3mbSeedingFunctions.h"
#include "AlphaBlendExample.h"
#include "ListManager.h"

/* It helps to keep in mind the following:

					Index.
	y   
0  \|/   90   91   92   93   94   95   96   97   98   99
1	i    80   81   82   83   84   85   86   87   88   89
2	n    70   71   72   73   74   75   76   77   78   79
3	c    60   61   62   63   64   65   66   67   68   69
4	r    50   51   52   53   54   55   56   57   58   59
5	e    40   41   42   43   44   45   46   47   48   49
6	a    30   31   32   33   34   35   36   37   38   39
7   s    20   21   22   23   24   25   26   27   28   29
8   e    10   11   12   13   14   15   16   17   18   19
9	s	  0    1    2    3    4    5    6    7    8    9

	 x->increases ......................................
          0    1    2    3    4    5    6    7    8    9
*/
extern HWND g_hDlgSeed;
extern HWND g_hwndBathy;
CMutex g_mutexDrawBitmap;
//CListManager <ANIMATSTATE_FILEOUT_PTR>
CListManager <ANIMATSTATE_FILEOUT_PTR> g_playBackHighLightList;

// The INHABITBUFFERINF buffer is used only for seeding.  Not used for viewing output. 
INHABITBUFFERINF g_inhabBuffInf = {0};

extern BOOL g_bBlockNextMouseClick;






LRESULT CALLBACK BathyBitmapWindowProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
#pragma message("Determine/set upper memory requirements for reading files back in and/or add dynamic read in")
	// See backup file MMMBS-2010_02_24_02_lib7.00Spe7.00_ToDorian.zip for a more complex version of this that uses bitblt.

	//---------------------------------//
	// Handles to Device Contexts (HDC)
	//---------------------------------//
	/* A device context is a structure that defines a set of graphic objects and their
	 * associated attributes, as well as the graphic modes that affect output. The graphic
	 * objects include a pen for line drawing, a brush for painting and filling, a bitmap
	 * for copying or scrolling parts of the screen, a palette for defining the set of
	 * available colors, a region for clipping and other operations, and a path for
	 * painting and drawing operations.
	 * See ms-help://MS.VSCC.v90/MS.MSDNQTR.v90.en/gdi/devcons_0g6r.htm */
	HDC	hdcBathy; // The DC for this window.
	HDC	hdc2;

	//------------------------------------------------------------//
	// Handles to a Graphics Device Interface GDI object (HGDIOBJ) 
	//------------------------------------------------------------//
	/* These are the handles to the returned Graphics Device Interface Object (GDI) object
	 * from the call to SelectObject().  SelectObject() selects a handle to a GDI object
	 * into a device context (DC) object.  The returned handle to a GDI object is supposed
	 * to be passed back back into the device context by a subsequent call to
	 * SelectObject() with that returned handle as the second paramater (the first
	 * parameter is the handle to the device context (DC/HDC).
	 * The specified object passed into SelectObject must have been created by using one
	 * of the following functions:
	 *	Returns a handle to a bitmap (HBITMAP):  CreateBitmap(), CreateBitmapIndirect(),
	 *		CreateCompatibleBitmap(), CreateDIBitmap(), CreateDIBSection()
	 *  Returns a handle to a brush (HBRUSH): CreateBrushIndirect(),
	 *		CreateDIBPatternBrush(), CreateDIBPatternBrushPt(), CreateHatchBrush(),
	 *		CreatePatternBrush(), CreateSolidBrush()
	 *	Returns a handle to a font (HFONT): CreateFont(), CreateFontIndirect(),
	 *  Returns a handle to a pen (HPEN): CreatePen(), CreatePenIndirect()
	 *	Returns a handle to a region (HREGION): CombineRgn(), CreateEllipticRgn(),
	 *		CreateEllipticRgnIndirect(), CreatePolygonRgn(), CreateRectRgn(),
	 *		CreateRectRgnIndirect()	*/
	HGDIOBJ hgdiobj;

	//-----------------------------//
	// Handles To Bitmaps (HBITMAP)
	//-----------------------------//
	static  HBITMAP s_hbitmapBathy = NULL; //(HBITMAPS are an HGDIOBJ)

	// Variables dealing with bitmaps
	PAINTSTRUCT ps;
	BATHYVALUE bathyValues;

	int x;
	int	numAnimats;
	RECT rect;
	//static LinkedList <ANIMATSTATE_FILEOUT_PTR> g_playBackHighLightList;

	static	BATHYBITMAP_WIN_PARAM *s_pbbmGdata = NULL;
	static double depthRange;
	static  ENVMOUSECLICK coordValues;

	//-----------------------------------------------//
	static DRAWBITMAPTHREADINF s_drawBitmapThreadParam;
	static THREAD_INF s_drawBitmapThreadInf;
	//-----------------------------------------------//

	//-----------------------------------------------//
	static ANIMATEANIMATPOPBITMAPTHREAD s_animateThreadParam;
	static CMutex s_animateMutex;
	//-----------------------------------------------//
	static BATHYBITMAPINFO s_bitMapInfo;
	static DISPLAYLAYERS s_displayLayers = {0};

	//----------------------------------------------------------------------------------//
	// Gui Controls State
	//-------------------//
	ACTIVE_LISTBOX activeLB;
	RADIO_ADDMETHOD addMethod;
	//----------------------------------------------------------------------------------//

	double value;
	static  COORD				 cMouseDwn  = {0};
	static  COORD				 cMouseUp   = {0};
	static  COORD				 *c1stVertex = NULL, *cLastVertex = NULL, *cNewVertex = NULL;
	static  COORD				 cPolyDynamic;
	static	BOOL				 bMouseDown = FALSE;
	static  TRACKMOUSEEVENT		 mouseEventLeaveScreen;// = sizeof(TRACKMOUSEEVENT), TME_LEAVE, hWnd, 0);
	static  TRACKMOUSEEVENT		 mouseEventLeaveCancel;// = sizeof(TRACKMOUSEEVENT), TME_CANCEL, hWnd, 0);
	int memcmpresult;
	POLYGONINF *poly;

	static CMutex redrawMutex;
	//static LinkedList <RECT> redrawList;
	static RECT resizeRect = {0};

	static int kej = 0;
	static RECT prevRect;
	static BOOL noPaint = FALSE;

	BOOL bDebugDefined = FALSE;

#ifdef _DEBUG
	bDebugDefined = TRUE;
#endif


	poly = &s_pbbmGdata->poly;

	switch(message) 
	{
	case WM_CREATE: 
		s_pbbmGdata = *(BATHYBITMAP_WIN_PARAM **)lParam;

		//------------------------------------------------------------------------------------//
		// Be sure previous call to this window properly managed (deleted) memory).
		//-----------------------------------------------------------------------//
		_ASSERT(s_displayLayers.bathy.buffer[0]==NULL && s_displayLayers.bathy.buffer[1]==NULL && s_displayLayers.bathy.buffer[2]==NULL);
		_ASSERT(s_displayLayers.slope.buffer[0]==NULL && s_displayLayers.slope.buffer[1]==NULL && s_displayLayers.slope.buffer[2]==NULL);
		_ASSERT(s_displayLayers.datapts.buffer[0]==NULL && s_displayLayers.datapts.buffer[1]==NULL && s_displayLayers.datapts.buffer[2]==NULL);
		_ASSERT(s_displayLayers.animats.buffer[0]==NULL && s_displayLayers.animats.buffer[1]==NULL && s_displayLayers.animats.buffer[2]==NULL);
		_ASSERT(s_displayLayers.buffLen[0]==0 && s_displayLayers.buffLen[1]==0 && s_displayLayers.buffLen[2]==0);
		_ASSERT(s_hbitmapBathy == NULL);
		//------------------------------------------------------------------------------------//

		//------------------------------------------------------------------------------------//
		// Initialize Variables.
		//-----------------------//
		memset(&s_displayLayers, 0, sizeof(DISPLAYLAYERS));
		memset(&s_animateThreadParam, 0, sizeof(ANIMATEANIMATPOPBITMAPTHREAD));
		memset(&s_bitMapInfo, 0, sizeof(BATHYBITMAPINFO));

		g_inhabBuffInf.popBuffLen = g_inhabBuffInf.highlightBuffLen = SIZE_128;
		s_displayLayers.bathy.enabled = &s_pbbmGdata->bBathymetry;
		s_displayLayers.slope.enabled = &s_pbbmGdata->bSlopeHeading;
		s_displayLayers.datapts.enabled = &s_pbbmGdata->bDataPoints;
		s_displayLayers.animats.enabled = &s_pbbmGdata->bAnimats;
		

		//------------------------------------------------------------------------------//
		// Initialize members of the drawing thread parameter that persist (references)
		//---------------------------------------------------------------------------//
		memset(&s_drawBitmapThreadParam, 0, sizeof(DRAWBITMAPTHREADINF));

		s_drawBitmapThreadParam.pDisplayLayers = &s_displayLayers;
		s_drawBitmapThreadParam.threadInf = &s_drawBitmapThreadInf;
		s_drawBitmapThreadParam.phbitmapBathy = &s_hbitmapBathy;
		s_drawBitmapThreadParam.pBitMapInfo = &s_bitMapInfo;



		//------------------------------------------------------------------------------//
		// Handle initial conditions for if the bathymetry map is to be used for seeding
		// or viewing output (playback).
		//------------------------------//
		if(s_pbbmGdata->usageState == SEED_SCENARIO_STATE)
		{
			// The bathymetry bitmap is to be used for seeding so allocate initial
			// memory to store animats in.  For seeding the INHABITBUFFERINF struct is
			// used that holds INHABITINF buffers that contain coordinate.  The population
			// buffer will hold only a single iteration (the initial state) for each
			// animat so make the 2-dimensional buffer dimensions 
			// 1(for the initial location)x128 (for 128 animats) initially.  Its size will
			// be increased later as needed.
			g_inhabBuffInf.popBuff = new INHABITINF[SIZE_128];

			// Do the same for the highlighted animats buffer.
			g_inhabBuffInf.highlightBuff = new INHABITINF[SIZE_128];
		}
		else
		{
			// The bathymetry bitmap is being used for viewing output (playback).  The
			// storage required depends on the number of animats, duration, and data 
			// included in the outputted.  This is determined when an output file is read
			// in.  For playback a buffer of ANIMATSTATE_FILEOUT is currently used to hold
			// each state of every animtats.  TODO: Determine upper limit, add dynamic
			// read in.


			// Launch the update thread.  The only thing the update thread does is
			// calculate which iteration is to be displayed and post a message when it is
			// time to do so.
			s_animateThreadParam.stateIndex = 0;
			s_animateThreadParam.pThreadRunningMutex = &s_animateMutex;
			s_animateThreadParam.playState = &s_pbbmGdata->playState;
			s_animateThreadParam.playbackRate = &s_pbbmGdata->nPlaybackRate;
			s_animateThreadParam.numStates = &s_pbbmGdata->playbackInf->sce.numSaveIterations;
			while(FALSE == s_animateMutex.Lock(500))
				Sleep(500);
			s_animateThreadParam.threadInf.hdl =
				CreateThread(NULL, 0, &AnimateThread, &s_animateThreadParam, 0, &s_animateThreadParam.threadInf.id);
			s_animateMutex.Unlock();
			while(s_animateThreadParam.threadInf.running == FALSE)
				Sleep(100);
		}

		memset(&coordValues, 0, sizeof(coordValues));

		/* Initialize the Bathymetry Bitmap Information structure s_bitMapInfo structure
		 * that is the same as a BITMPAINFO struct but with the appropriate number of
		 * colors allowcated for the RGBQUAD member struct needed for this bitmap.
		 * A BITMAPINFO structure defines the dimensions and color information for a
		 * Windows device-independent bitmap (DIB).*/
		GetClientRect(hWnd, &rect);
		BmpInitInfo((LPBITMAPINFO)&s_bitMapInfo, rect.right, rect.bottom);
		SetBitmapPallet((LPBITMAPINFO)&s_bitMapInfo);

		if(s_pbbmGdata->usageState == SEED_SCENARIO_STATE)
			PostMessage(hWnd, WM_UPDATE_ENV_ANIMATS, 0, 0);
		break;

//	case WM_3MB_TOGGLE_TRACK_RUN:
//		break;
	//case WM_3MB_TOGGLE_PLAYBACK_RUN:
	//	// Not sure this is needed.
	//	GetClientRect(hWnd, &rect);
	//	InvalidateRect(hWnd, &rect, FALSE);
	//	break;

	case WM_REDRAW_BITMAP_THREAD:
		/*------------------------------------------------------------------------------//
		 * Set the bitmap's information struct's new dimensions  if any, and reallocate
		 * buffer storage if needed.  Redraw is expected to be called when:
		 * (1) As a result of WM_RESIZE which happens when
		 *		(a) the seeding dialog box is first launched
		 *		(b) when the user resizes the seeding dialog box
		 * (2) As a result of zooming in or zooming out of the bitmap (see WM_ZOOMBITMAP
			   and WM_ZOOMOUTBITMAP messages).
		 *--------------------------*/

		//------------------------------------------------------------------------------//
		// Wait for the drawing thread to cease
		//-------------------------------------//
		// Should the redraw thread already be running (highly unlikely but not
		// impossible) it will be accessing the bathymetry bitmap data and dependend
		// on it's current dimensions so don't permit a change to these until it ceases.
		if(s_drawBitmapThreadParam.threadInf->hdl != NULL || FALSE == g_mutexDrawBitmap.Lock(100))
		{
			// wParam will be either TRUE for rescale or FALSE for no rescale.
			PostMessage(hWnd, WM_REDRAW_BITMAP_THREAD, wParam, lParam);
			break;
		}

		//------------------------------------------------------------------------------//
		// Run maintenance checks.
		//-----------------------//
		_ASSERT(s_drawBitmapThreadInf.hdl == NULL);
		_ASSERT(s_drawBitmapThreadInf.id == 0);	
		_ASSERT(s_drawBitmapThreadInf.running == FALSE);

		//------------------------------------------------------------------------------//
		// Initialize members of the the drawing thread parameter that potentially change
		//-----------------------------------------------------------------------------//
		memcpy(&s_drawBitmapThreadParam.pSeedInf, s_pbbmGdata, sizeof(BATHYBITMAP_WIN_PARAM)); // check for redundancy
		memcpy(&s_drawBitmapThreadParam.layout, s_pbbmGdata->layout, sizeof(DLGLAYOUT));

		s_drawBitmapThreadParam.bResize = FALSE;
		if(wParam == TRUE) // means changing the dlg box's size or zooming the bitmap
		{
			if((BATHYSCALETYPE)lParam == BITMAPZOOMTYPE)
			{
				s_drawBitmapThreadParam.bResize = TRUE;
				if(s_pbbmGdata->pWindowDisplayState->bZoom)
				{
					s_drawBitmapThreadParam.pDisplayLayers->displayType = ZOOM;
				}
				else if(s_pbbmGdata->pWindowDisplayState->bMaximized)
					s_drawBitmapThreadParam.pDisplayLayers->displayType = MAXIMIZED;
				else
					s_drawBitmapThreadParam.pDisplayLayers->displayType = MINIMIZED;
			}
			else if((BATHYSCALETYPE)lParam == DEPTHSCALETYPE)
			{
				free(s_drawBitmapThreadParam.pDisplayLayers[0].bathy.buffer[s_drawBitmapThreadParam.pDisplayLayers->displayType]);
				s_drawBitmapThreadParam.pDisplayLayers[0].bathy.buffer[s_drawBitmapThreadParam.pDisplayLayers->displayType] = NULL;
			}
			else if((BATHYSCALETYPE)lParam == SLOPESCALETYPE)
			{
				free(s_drawBitmapThreadParam.pDisplayLayers[0].slope.buffer[s_drawBitmapThreadParam.pDisplayLayers->displayType]);
				s_drawBitmapThreadParam.pDisplayLayers[0].slope.buffer[s_drawBitmapThreadParam.pDisplayLayers->displayType] = NULL;
			}
		}

		s_drawBitmapThreadParam.usageState = s_pbbmGdata->usageState; // check for reduancy
		s_drawBitmapThreadParam.playState = s_pbbmGdata->playState;

		// TODO: Add error handling if create thread fails.
		s_drawBitmapThreadInf.hdl = CreateThread(NULL, 0, &DisplayDataThread, &s_drawBitmapThreadParam, 0, &s_drawBitmapThreadInf.id);
		g_mutexDrawBitmap.Unlock();
		//------------------------------------------------------------------------------//

		break;


	case WM_UPDATE_BITMAP:
		GetClientRect(hWnd, &rect);
		/* Called only when drawing bit map thread finishes

		*/
		//-----------------------------------------------------------------------------//
		// GDI device context (DC), bitmap, and handle preparation
		//--------------------------------------------------------//
		// Get the DC associated with this window,
		if(s_hbitmapBathy != NULL)
			DeleteObject(s_hbitmapBathy);
		hdcBathy = GetDC(g_hwndBathy);

		// Create a device dependant bitmap based upon the information about the bitmap
		// being drawn, based upon the compatable DC in memory created above, and based
		// upon the data (var s_bathyData) supplied to it.
		// From the Microsoft Documentation: "The CreateDIBitmap function creates a
		// compatible bitmap (DDB) from a DIB and, optionally, sets the bitmap bits."
		s_hbitmapBathy = CreateDIBitmap(hdcBathy,
									   (LPBITMAPINFOHEADER)&s_bitMapInfo,
										CBM_INIT,
										s_displayLayers.bathy.buffer[s_displayLayers.displayType],
									   (LPBITMAPINFO)&s_bitMapInfo,
										DIB_RGB_COLORS);

		////////////////////////////////////// EXPERIMENTAL ///////////////////////////////////////

		if(NULL == (hdc2 = CreateCompatibleDC(hdcBathy)))
		{
			EndPaint(hWnd, &ps);
			prevRect = rect;
			break;
		}

		if(NULL == (hgdiobj = SelectObject(hdc2, s_hbitmapBathy)))
		{
			EndPaint(hWnd, &ps);
			prevRect = rect;
			break;
		}


		if(!BitBlt(hdcBathy, 0, 0, rect.right, rect.bottom, hdc2, 0, 0, SRCCOPY))
			MessageBox(NULL,"BitBlt Failed!","Error",MB_ICONWARNING);

		prevRect = rect;

		SelectObject(hdc2, hgdiobj);
		DeleteDC(hdc2);

		// Having sucessfully created and displayed the bathymetry data do some alpha blending.
		////////////////////////////////////////// SLOPE ///////////////////////////////////
		if(*s_displayLayers.slope.enabled == TRUE && s_displayLayers.slope.buffer[s_displayLayers.displayType] != NULL)
			BlendAlphaBlendIntoBitmap(rect, &s_hbitmapBathy, s_displayLayers.slope.buffer[s_displayLayers.displayType]);

		if(*s_displayLayers.datapts.enabled == TRUE && s_displayLayers.datapts.buffer[s_displayLayers.displayType] != NULL)
			BlendAlphaBlendIntoBitmap(rect, &s_hbitmapBathy, s_displayLayers.datapts.buffer[s_displayLayers.displayType]);

		if(*s_displayLayers.animats.enabled == TRUE && s_displayLayers.animats.buffer[s_displayLayers.displayType] != NULL)
			BlendAlphaBlendIntoBitmap(rect, &s_hbitmapBathy, s_displayLayers.animats.buffer[s_displayLayers.displayType]);

		////////////////////////////////////////// SLOPE ///////////////////////////////////


		////////////////////////////////////// EXPERIMENTAL ///////////////////////////////////////


		ReleaseDC(hWnd, hdcBathy);
		GetClientRect(hWnd, &rect);
		InvalidateRect(hWnd, &rect, FALSE);
		break;

	case WM_UPDATE_ENV_ANIMATS:

		if(s_drawBitmapThreadParam.threadInf->hdl != NULL || FALSE == g_mutexDrawBitmap.Lock(100))
		{
			PostMessage(hWnd, WM_UPDATE_ENV_ANIMATS, wParam, 0);
			break;
		}


		GetClientRect(hWnd, &rect);
		//------------------------------------------------------------------------------//
		// Grab species, individual, pod, and pod member index information for
		// highlighting animats.  If the bitmap is being used for seeding, in which case
		// the buffer holding animat location may need to be adjusted, adjust it as well.
		// Buffering when the bitmap is being used for examining output is hanled after.
		//------------------------------------------------------------------------------//
		g_inhabBuffInf.numHighlightedAnimats = 0;
		activeLB = GetActiveListBox();
		if(activeLB.lstbox == SPECIES_LB)
		{	
			// Get selected species from the species list box
			//activeLB.index.spe = SendMessage(GetDlgItem(g_hDlgSeed, IDC_LIST_SPECIES), LB_GETCURSEL, 0, (LPARAM)0);
			g_inhabBuffInf.numHighlightedAnimats = CBitmapEnvFunctions::GetAnimatCount(activeLB.index.spe);

			if(s_pbbmGdata->usageState == SEED_SCENARIO_STATE)
			{
				if(g_inhabBuffInf.highlightBuffLen < g_inhabBuffInf.numHighlightedAnimats)
				{
					g_inhabBuffInf.highlightBuff =
						AdjustCoordinateBufferSize(g_inhabBuffInf.highlightBuff,
												   &g_inhabBuffInf.highlightBuffLen,
												   g_inhabBuffInf.numHighlightedAnimats);
				}
				CBitmapEnvFunctions::GetAnimatInitialCoordinates(activeLB.index.spe, g_inhabBuffInf.numHighlightedAnimats, g_inhabBuffInf.highlightBuff);
			}
		}
		else if(activeLB.lstbox == INDIVIDUAL_LB)
		{
			// Get selected species and individuals from respective list boxes
			//activeLB.index.spe = SendMessage(GetDlgItem(g_hDlgSeed, IDC_LIST_SPECIES), LB_GETCURSEL, 0, (LPARAM)0);
			//activeLB.index.ind = SendMessage(GetDlgItem(g_hDlgSeed, IDC_LIST_INDIVIDUAL), LB_GETCURSEL, 0, (LPARAM)0);
			g_inhabBuffInf.numHighlightedAnimats = 1;

			if(s_pbbmGdata->usageState == SEED_SCENARIO_STATE)
			{
				if(g_inhabBuffInf.highlightBuffLen < g_inhabBuffInf.numHighlightedAnimats)
				{
					g_inhabBuffInf.highlightBuff =
						AdjustCoordinateBufferSize(g_inhabBuffInf.highlightBuff, &g_inhabBuffInf.highlightBuffLen, g_inhabBuffInf.numHighlightedAnimats);
				}
				g_inhabBuffInf.highlightBuff[0] = CBitmapEnvFunctions::GetIndividualInitialCoordinate(activeLB.index.spe, activeLB.index.ind);
			}
		}
		else if(activeLB.lstbox == POD_LB)
		{
			// Get selected species and pod indices from respective list boxes
			//activeLB.index.spe = SendMessage(GetDlgItem(g_hDlgSeed, IDC_LIST_SPECIES), LB_GETCURSEL, 0, (LPARAM)0);
			//activeLB.index.pod = SendMessage(GetDlgItem(g_hDlgSeed, IDC_LIST_PODS), LB_GETCURSEL, 0, (LPARAM)0);

			g_inhabBuffInf.numHighlightedAnimats = CBitmapEnvFunctions::GetPodMemberCount(activeLB.index.spe, activeLB.index.pod);
			if(s_pbbmGdata->usageState == SEED_SCENARIO_STATE)
			{
				if(g_inhabBuffInf.highlightBuffLen < g_inhabBuffInf.numHighlightedAnimats)
				{
					g_inhabBuffInf.highlightBuff =
						AdjustCoordinateBufferSize(g_inhabBuffInf.highlightBuff, &g_inhabBuffInf.highlightBuffLen, g_inhabBuffInf.numHighlightedAnimats);
				}
				CBitmapEnvFunctions::GetPodInitialCoordinates(activeLB.index.spe, activeLB.index.pod, g_inhabBuffInf.highlightBuff);
			}
		}
		else if(activeLB.lstbox == MEMBER_LB)
		{
			// Get selected species, pod and pod member indices from respective list boxes
			activeLB.index.spe = SendMessage(GetDlgItem(g_hDlgSeed, IDC_LIST_SPECIES), LB_GETCURSEL, 0, (LPARAM)0);
			activeLB.index.pod = SendMessage(GetDlgItem(g_hDlgSeed, IDC_LIST_PODS), LB_GETCURSEL, 0, (LPARAM)0);
			activeLB.index.pdm = SendMessage(GetDlgItem(g_hDlgSeed, IDC_LIST_SEL_POP), LB_GETCURSEL, 0, (LPARAM)0);

			g_inhabBuffInf.numHighlightedAnimats = 1;
			if(s_pbbmGdata->usageState == SEED_SCENARIO_STATE)
			{
				if(g_inhabBuffInf.highlightBuffLen < g_inhabBuffInf.numHighlightedAnimats)
				{
					g_inhabBuffInf.highlightBuff =
						AdjustCoordinateBufferSize(g_inhabBuffInf.highlightBuff, &g_inhabBuffInf.highlightBuffLen, g_inhabBuffInf.numHighlightedAnimats);
				}
				g_inhabBuffInf.highlightBuff[0] = CBitmapEnvFunctions::GetPodMemberInitialCoordinate(activeLB.index.spe, activeLB.index.pod, activeLB.index.pdm);
			}				
		}


		//------------------------------------------------------------------------------------//
		// Add the entire animat population.
		// If the entire animat population is to be highlighted, skip this.
		//-----------------------------------------------------------------//
		numAnimats = CBitmapEnvFunctions::GetAnimatCount();
		if(s_pbbmGdata->usageState == SEED_SCENARIO_STATE)
		{
			if(g_inhabBuffInf.popBuffLen < numAnimats)
				g_inhabBuffInf.popBuff = AdjustCoordinateBufferSize(g_inhabBuffInf.popBuff, &g_inhabBuffInf.popBuffLen, numAnimats);

			CBitmapEnvFunctions::GetAnimatInitialCoordinates(g_inhabBuffInf.popBuff);
			//AnimatsToBitmap(rect, s_bathyData, s_pbbmGdata, 1, numAnimats, g_inhabBuffInf.popBuff, FALSE);
		}

		//------------------------------------------------------------------------------------//
		// Add highlights to selected animats, if any.
		//--------------------------------------------//
		if(s_pbbmGdata->usageState == SEED_SCENARIO_STATE)
		{
			//AnimatsToBitmap(rect, s_bathyData, s_pbbmGdata, 1, g_inhabBuffInf.numHighlightedAnimats, g_inhabBuffInf.highlightBuff, TRUE);
		}

		if(s_pbbmGdata->usageState == PLAYBACK_STATE)
		{
			// The highlight list is deleted because it is going to be updated.
			g_playBackHighLightList.Lock();
			g_playBackHighLightList.DeleteAll();
			GenerateHightlightList(activeLB.index.spe, activeLB.index.pod, activeLB.index.pdm, activeLB.index.ind, &g_playBackHighLightList);
			g_playBackHighLightList.Unlock();
		}

		g_mutexDrawBitmap.Unlock();
		PostMessage(hWnd, WM_REDRAW_BITMAP_THREAD, 0, NONE_SCALETYPE);

/*
		//-----------------------------------------------------------------------------//
		// GDI device context (DC), bitmap, and handle preparation
		//--------------------------------------------------------//
		// Create a device dependant bitmap based upon the information about the bitmap
		// being drawn, based upon the compatable DC in memory created above, and based
		// upon the data (var s_bathyData) supplied to it.
		DeleteObject(s_hbitmapBathy);
		hdcBathy = GetDC(hWnd); // Get a handle to the Device Context associated with this window,
		s_hbitmapBathy = CreateDIBitmap(hdcBathy,
								(LPBITMAPINFOHEADER)&s_bitMapInfo,
								 CBM_INIT, s_bathyData,
								(LPBITMAPINFO)&s_bitMapInfo,
								 DIB_RGB_COLORS);
*/
		//ReleaseDC(hWnd,hdcBathy);
		//InvalidateRect(hWnd, &rect, FALSE);
		break;

	case WM_UPDATE_TOGGLED_INF:
		PostMessage(hWnd, WM_REDRAW_BITMAP_THREAD, 0, NONE_SCALETYPE);
		break;

	case WM_3MB_TOGGLE_SLOPEHEADING:
		PostMessage(hWnd, WM_REDRAW_BITMAP_THREAD, 0, NONE_SCALETYPE);
		break;


	case WM_REFRESH_ENV_BITMAP:
#if 0
		// Delete the current Device Independent Bitmap already in memory.
		DeleteObject(s_hbitmapBathy);

		// Create a device dependant bitmap based upon the information about the bitmap
		// being drawn, based upon the compatable DC in memory created above, and based
		// upon the data (var s_bathyData) supplied to it.
		GetClientRect(hWnd, &rect);
		hdcBathy = GetDC(hWnd); // Get a handle to the Device Context associated with this window
		s_hbitmapBathy = CreateDIBitmap(hdcBathy, (LPBITMAPINFOHEADER)&s_bitMapInfo, CBM_INIT, s_displayLayers.bathy.buffer[s_displayLayers.displayType], (LPBITMAPINFO)&s_bitMapInfo,
								 DIB_RGB_COLORS);
		ReleaseDC(hWnd,hdcBathy);
		InvalidateRect(hWnd, &rect, FALSE);
#endif
		break;

	case WM_RBUTTONDOWN:
		addMethod = GetAddMethodRadioState();
		if(addMethod == POLYGON)
		{
			if(poly->numVertices > 0)
			{
				// Clear out the polygon
				memset(poly, 0, sizeof(POLYGONINF));

				//PostMessage(hWnd, WM_REFRESH_ENV_BITMAP, NULL, (LPARAM)NULL);
				GetClientRect(hWnd, &rect);
				InvalidateRect(hWnd, &rect, FALSE);

				//PostMessage(g_hDlgSeed, WM_UPDATE_GUI, NULL, (LPARAM)NULL);
			}
			else 
			{
				SetAddMethodStateRadio(MOUSECLICK);
				PostMessage(g_hDlgSeed, WM_UPDATE_GUI, NULL, (LPARAM)NULL);
			}
		}
		else
		{
			bMouseDown = FALSE;
			ReleaseCapture();
			PostMessage(g_hDlgSeed, WM_RBUTTONDOWN, NULL, (LPARAM)NULL);
		}
		break;


	case WM_LBUTTONDOWN:
		if(g_bBlockNextMouseClick == TRUE)
			break;
		SetCapture(hWnd);
		GetClientRect(hWnd, &rect); // This will retrieve this windows size.
		memset(&coordValues, 0, sizeof(ENVMOUSECLICK));
		cMouseDwn.X = coordValues.coord.X = LOWORD(lParam);
		cMouseDwn.Y = coordValues.coord.Y = HIWORD(lParam);

		activeLB = GetActiveListBox();
		addMethod = GetAddMethodRadioState();

		// Bounding boxes only appear when the user drags the cursor with the mouse down
		// so left-clicking never itself can create a bounding box.
		s_pbbmGdata->boundingBoxPresent = FALSE;

		if(addMethod == BOUNDINGBOX && activeLB.lstbox == SPECIES_LB)
		{
			bMouseDown  = TRUE;
			// Clear existing bounding box if any.  
			BoundingBox(hWnd, cMouseDwn, cMouseDwn, FALSE);
			PostMessage(g_hDlgSeed, WM_CAPTURE_MOUSE_DOWN, NULL, (LPARAM)&coordValues);
		}
		// If the vertex is closed the only option is to remove it by right clicking (or other means).
		else if(addMethod == POLYGON && poly->isClosed == FALSE && activeLB.lstbox == SPECIES_LB)
		{
			if(poly->isClosed == TRUE)
				break;

			// Adding the first vertex
			if(poly->numVertices == 0)
			{
				poly->numVertices++;
				poly->cArray[0] = cMouseDwn;
				break;
			}

			// The current vertex cannot be the same as the one previous added (multiple
			// mouse clicks in the same location not allowed).
			if(memcmp(&poly->cArray[poly->numVertices-1], &cMouseDwn, sizeof(COORD)) == 0)
 				break;

			// Verify that, with the exception of the first vertex, the newly clicked vertex
			// doesn't match any previous vertex.
			memcmpresult = 1;
			for(x=1; x<=poly->numVertices-1; x++)
			{
				if(0==(memcmpresult = memcmp(&poly->cArray[x], &cMouseDwn, sizeof(COORD))))
					break;
			}
			if(memcmpresult == 0)
				break;


			// Handle the attempt to add a vertex based with consideration of how many
			// vertices are already present.
			if(poly->numVertices < 3 && poly->numVertices < POLYCORRDARRAYLEN)
			{
				// 1 or 2 vertices permits a line to be drawn between them w/o additional checking.
				poly->cArray[poly->numVertices] = cMouseDwn;
				poly->numVertices++;
				DrawLineTo(hWnd, s_pbbmGdata, poly->cArray[poly->numVertices-2], poly->cArray[poly->numVertices-1], RGB(100, 100, 100));
				break;
			}
			else if(/*poly->numVertices >= 3* && */poly->numVertices < POLYCORRDARRAYLEN)
			{
				// Enough room to add another vertex, so add it.
				c1stVertex = &poly->cArray[0];
				poly->cArray[poly->numVertices] = cMouseDwn;
				poly->numVertices++;

				// Calculate the distance between the pixel locations of the first vertex
				// of the polygon and the last to determine if the user attempted to
				// close the polygon.  Force the last vertex to be the same as the fist
				// and close the polygone only if at least 4 vertices present.
				if(poly->numVertices >= 4)
				{
					value = sqrt(pow(double(cMouseDwn.X-c1stVertex->X),2) *
								pow(double(cMouseDwn.Y-c1stVertex->Y),2));
					if(value <= ENDPOLYGONCLOSEBYDIST) // Don't auto complete unless 4 vertices.
					{
						poly->cArray[poly->numVertices-1] = poly->cArray[0];
						poly->isClosed = TRUE;
					}
				}

				// If there's only room left in the array for one more click then force the
				// polygon closed by adding that lat vertex and setting it equal to
				// the first vertex.
				if(poly->isClosed == FALSE && poly->numVertices == POLYCORRDARRAYLEN-1)
				{
					poly->cArray[poly->numVertices] = poly->cArray[0];
					poly->numVertices++;
					poly->isClosed = TRUE;

				}
			}

			// If the polygon has been closed, update it on the map by making it bright white.
			// Otherwise, just add a line to the polygon.
			if(poly->isClosed == TRUE)
			{
				RedrawPolygon(hWnd, s_pbbmGdata);
				SetPolyMinMax(&s_pbbmGdata->poly);
			}
			else
			{
				DrawLineTo(hWnd, s_pbbmGdata, poly->cArray[poly->numVertices-2], poly->cArray[poly->numVertices-1],
					RGB(100, 100, 100));
			}
		}
		else if(addMethod == POLYGON && poly->isClosed == TRUE  && activeLB.lstbox == SPECIES_LB)
		{
			// Test....
			PostMessage(g_hDlgSeed, WM_SEED_COORDINATE, NULL, (LPARAM)NULL);
		}
		else if(activeLB.lstbox == NONE_LB)
		{
			bMouseDown  = TRUE;
			// Clear existing bounding box if any.  
			BoundingBox(hWnd, cMouseDwn, cMouseDwn, FALSE);
			PostMessage(g_hDlgSeed, WM_CAPTURE_MOUSE_DOWN, NULL, (LPARAM)&coordValues);

			coordValues.lat = s_pbbmGdata->bitmapInf.displayExtremes.xMin + s_pbbmGdata->bitmapInf.latPerPixel * (rect.bottom - 1 - coordValues.coord.Y);
			coordValues.lon	= s_pbbmGdata->bitmapInf.displayExtremes.yMin + s_pbbmGdata->bitmapInf.lonPerPixel * coordValues.coord.X;
			bathyValues = CBitmapEnvFunctions::GetBathymetryValues(coordValues.lat, coordValues.lon);
			coordValues.depth = bathyValues.depth;
			coordValues.slopeAngle = bathyValues.slope;
			coordValues.slopeHeading = bathyValues.slopeHeading;
			coordValues.index = rect.right * (rect.bottom - 1 - coordValues.coord.Y) + coordValues.coord.X;
			//PostMessage(g_hDlgSeed, WM_SEED_COORDINATE, NULL, (LPARAM)&coordValues);
			PostMessage(g_hDlgSeed, WM_CAPTURE_MOUSE_DOWN, NULL, (LPARAM)&coordValues);
		}
		break;

	case WM_LBUTTONUP:
		if(g_bBlockNextMouseClick == TRUE)
			break;
		ReleaseCapture();
		GetClientRect(hWnd, &rect); // This will retrieve this windows size.
		memset(&coordValues, 0, sizeof(ENVMOUSECLICK));
		cMouseUp.X = coordValues.coord.X = LOWORD(lParam);
		cMouseUp.Y = coordValues.coord.Y = HIWORD(lParam);
		activeLB = GetActiveListBox();
		addMethod = GetAddMethodRadioState();

		if(cMouseUp.X > rect.right-1)
			cMouseUp.X = coordValues.coord.X = (short)(rect.right-1);
		if(cMouseUp.X < 0)
			cMouseUp.X = coordValues.coord.X = 0;
		if(cMouseUp.Y > rect.bottom-1)
			cMouseUp.Y = coordValues.coord.Y = (short)(rect.bottom-1);
		if(cMouseUp.Y < 0)
			cMouseUp.Y = coordValues.coord.Y = 0;


		coordValues.lat = s_pbbmGdata->bitmapInf.displayExtremes.xMin + s_pbbmGdata->bitmapInf.latPerPixel * (rect.bottom - 1 - coordValues.coord.Y);
		coordValues.lon	= s_pbbmGdata->bitmapInf.displayExtremes.yMin + s_pbbmGdata->bitmapInf.lonPerPixel * coordValues.coord.X;
		bathyValues = CBitmapEnvFunctions::GetBathymetryValues(coordValues.lat, coordValues.lon);
		coordValues.slopeAngle = bathyValues.slope;
		coordValues.slopeHeading = bathyValues.slopeHeading;
		coordValues.depth = bathyValues.depth;
		coordValues.index = rect.right * (rect.bottom - 1 - coordValues.coord.Y) + coordValues.coord.X;

		if(addMethod == BOUNDINGBOX && s_pbbmGdata->boundingBoxPresent == TRUE && activeLB.lstbox == SPECIES_LB)
		{
			BoundingBox(hWnd, cMouseDwn, cMouseUp, TRUE);
			PostMessage(g_hDlgSeed, WM_SEED_COORDINATE, NULL, (LPARAM)&coordValues);
		}
		// If adding by polygone then determine if time to close the polgon either because user
		// means to do so or because the polygone COORD array is almost full.  Closing the polygon
		// requres at least three vertices.
		else if(addMethod == POLYGON && poly->isClosed == FALSE && poly->numVertices >= 3 && activeLB.lstbox == SPECIES_LB)
		{
			;
		}
		else if(activeLB.lstbox == SPECIES_LB || (bDebugDefined && (activeLB.lstbox == POD_LB)))
		{
			coordValues.lat = s_pbbmGdata->bitmapInf.displayExtremes.xMin + s_pbbmGdata->bitmapInf.latPerPixel * (rect.bottom - 1 - coordValues.coord.Y);
			coordValues.lon	= s_pbbmGdata->bitmapInf.displayExtremes.yMin + s_pbbmGdata->bitmapInf.lonPerPixel * coordValues.coord.X;
			bathyValues = CBitmapEnvFunctions::GetBathymetryValues(coordValues.lat, coordValues.lon);
			coordValues.depth = bathyValues.depth;
			coordValues.slopeAngle = bathyValues.slope;
			coordValues.slopeHeading = bathyValues.slopeHeading;
			coordValues.index = rect.right * (rect.bottom - 1 - coordValues.coord.Y) + coordValues.coord.X;
			PostMessage(g_hDlgSeed, WM_SEED_COORDINATE, NULL, (LPARAM)&coordValues);
		}
		else if(bMouseDown == TRUE)
		{
			if(0 != memcmp(&cMouseDwn, &cMouseUp, sizeof(COORD)))
			{
				BoundingBox(hWnd, cMouseDwn, cMouseUp, TRUE);

				//----------------------------------------------------------------------------------//
				// Delete the bathymety display data buffers
				//------------------------------------------//
				if(s_displayLayers.bathy.buffer[ZOOM] != NULL)
					delete [] s_displayLayers.bathy.buffer[ZOOM];
				s_displayLayers.bathy.buffer[ZOOM] = NULL;

				//----------------------------------------------------------------------------------//
				// Delete the slope display data buffers
				//--------------------------------------//
				if(s_displayLayers.slope.buffer[ZOOM] != NULL)
					delete [] s_displayLayers.slope.buffer[ZOOM];
				s_displayLayers.slope.buffer[ZOOM] = NULL;

				//----------------------------------------------------------------------------------//
				// Delete the data points display data buffers
				//--------------------------------------------//
				if(s_displayLayers.datapts.buffer[ZOOM] != NULL)
					delete [] s_displayLayers.datapts.buffer[ZOOM];
				s_displayLayers.datapts.buffer[ZOOM] = NULL;


				//----------------------------------------------------------------------------------//
				// Delete the animats display data buffers
				//----------------------------------------//
				if(s_displayLayers.animats.buffer[ZOOM] != NULL)
					delete [] s_displayLayers.animats.buffer[ZOOM];
				s_displayLayers.animats.buffer[ZOOM] = NULL;

				s_displayLayers.buffLen[ZOOM] = 0;

				PostMessage(g_hDlgSeed, WM_ZOOMBITMAP, NULL, (LPARAM)&coordValues);
			}
		}
		bMouseDown = FALSE;
		PostMessage(g_hDlgSeed, WM_SEED_MOUSE_MOVE, NULL, (LPARAM)&coordValues);
		break;

	case WM_MOUSEMOVE:
		GetClientRect(hWnd, &rect); // This will retrieve this windows size.
		memset(&coordValues, 0, sizeof(ENVMOUSECLICK));
		coordValues.coord.X = LOWORD(lParam);
		coordValues.coord.Y = HIWORD(lParam);
		addMethod = GetAddMethodRadioState();

		coordValues.lat = s_pbbmGdata->bitmapInf.displayExtremes.xMin + s_pbbmGdata->bitmapInf.latPerPixel * (rect.bottom - 1 - coordValues.coord.Y);
		coordValues.lon	= s_pbbmGdata->bitmapInf.displayExtremes.yMin + s_pbbmGdata->bitmapInf.lonPerPixel * coordValues.coord.X;
		bathyValues = CBitmapEnvFunctions::GetBathymetryValues(coordValues.lat, coordValues.lon);
		coordValues.slopeAngle = bathyValues.slope;
		coordValues.slopeHeading = bathyValues.slopeHeading;
		coordValues.depth = bathyValues.depth;
		coordValues.index = rect.right * (rect.bottom - 1 - coordValues.coord.Y) + coordValues.coord.X;

		if(addMethod == BOUNDINGBOX && bMouseDown == TRUE)
		{
			// Create bounding box if not already intially drawn or expand if it is,
			// indicate bounding box is drawn
			s_pbbmGdata->boundingBoxPresent = TRUE;
			BoundingBox(hWnd, cMouseDwn, coordValues.coord, FALSE);
		}
		else if(addMethod == POLYGON && poly->numVertices > 0 && poly->isClosed == FALSE)
		{
			;
		}
		else if(bMouseDown == TRUE)
		{
			// Create bounding box if not already intially drawn or expand if it is,
			// indicate bounding box is drawn
			s_pbbmGdata->boundingBoxPresent = TRUE;
			BoundingBox(hWnd, cMouseDwn, coordValues.coord, FALSE);
		}

		PostMessage(g_hDlgSeed, WM_SEED_MOUSE_MOVE, NULL, (LPARAM)&coordValues);
		break;


	case WM_DESTROY:
		// Shut down the drawing thread if it happens to be running.
		s_drawBitmapThreadInf.exit = TRUE;
		s_animateThreadParam.threadInf.exit = TRUE;

		if(s_drawBitmapThreadInf.running == TRUE)
		{
			x = 0;
			while(s_drawBitmapThreadInf.running == TRUE && x++ <20)
				Sleep(100);
		}

		if(s_animateThreadParam.threadInf.running == TRUE)
		{
			x = 0;
			while(s_animateThreadParam.threadInf.running == TRUE && x++ <20)
				Sleep(100);
		}

		_ASSERT(s_drawBitmapThreadInf.running == FALSE);
		_ASSERT(s_animateThreadParam.threadInf.running == FALSE);
		s_drawBitmapThreadInf.exit = FALSE;

		for(x=0; x<3; x++)
		{
			//-----------------------------------------------------------------------------------//
			// Delete the bathymety display data buffers
			//------------------------------------------//
			if(s_displayLayers.bathy.buffer[x] != NULL)
				delete [] s_displayLayers.bathy.buffer[x];
			s_displayLayers.bathy.buffer[x] = NULL;

			//-----------------------------------------------------------------------------------//
			// Delete the slope display data buffers
			//--------------------------------------//
			if(s_displayLayers.slope.buffer[x] != NULL)
				delete [] s_displayLayers.slope.buffer[x];
			s_displayLayers.slope.buffer[x] = NULL;

			//-----------------------------------------------------------------------------------//
			// Delete the data points display data buffers
			//--------------------------------------------//
			if(s_displayLayers.datapts.buffer[x] != NULL)
				delete [] s_displayLayers.datapts.buffer[x];
			s_displayLayers.datapts.buffer[x] = NULL;


			//-----------------------------------------------------------------------------------//
			// Delete the animats display data buffers
			//----------------------------------------//
			if(s_displayLayers.animats.buffer[x] != NULL)
				delete [] s_displayLayers.animats.buffer[x];
			s_displayLayers.animats.buffer[x] = NULL;

			s_displayLayers.buffLen[x] = 0;

		}

		while(s_drawBitmapThreadParam.threadInf->hdl != NULL || FALSE == g_mutexDrawBitmap.Lock(100))
			Sleep(10);

		if(g_inhabBuffInf.popBuff != NULL)
			delete [] g_inhabBuffInf.popBuff;
		g_inhabBuffInf.popBuff  = NULL;
		g_inhabBuffInf.popBuffLen = 0;

		if(g_inhabBuffInf.highlightBuff != NULL)
			delete [] g_inhabBuffInf.highlightBuff;
		g_inhabBuffInf.highlightBuff = NULL;
		g_inhabBuffInf.highlightBuffLen = 0;
		g_mutexDrawBitmap.Unlock();

		g_playBackHighLightList.DeleteAll();

		DeleteObject(s_hbitmapBathy);
		s_hbitmapBathy = NULL;
		break;

		/*
	case WM_COMMAND:
		wmId    = LOWORD(wParam); 
		wmEvent = HIWORD(wParam); 
		// Parse the menu selections:
		switch (wmId)
		{
		case IDM_EXIT:
			break;

		default:
			return DefWindowProc(hWnd, message, wParam, lParam);
		}
		break;
*/

	case WM_PAINT:
		hdcBathy = BeginPaint(hWnd, &ps);
		GetClientRect(hWnd, &rect);
		addMethod = GetAddMethodRadioState();

		if(NULL == (hdc2 = CreateCompatibleDC(hdcBathy)))
		{
			EndPaint(hWnd, &ps);
			prevRect = rect;
			break;
		}

		if(NULL == (hgdiobj = SelectObject(hdc2, s_hbitmapBathy)))
		{
			EndPaint(hWnd, &ps);
			prevRect = rect;
			break;
		}


		if((rect.bottom == prevRect.bottom && rect.right == prevRect.right))
		{
			if(!BitBlt(hdcBathy, 0, 0, rect.right, rect.bottom, hdc2, 0, 0, SRCCOPY))
				MessageBox(NULL,"BitBlt Failed!","Error",MB_ICONWARNING);
		}
		else
		{
			if(!StretchBlt(hdcBathy, 0,0, rect.right, rect.bottom, hdc2, 0,0, prevRect.right, prevRect.bottom, SRCCOPY))
				MessageBox(NULL,"StrechBlt Failed!","Error",MB_ICONWARNING);
		}

		prevRect = rect;
		SelectObject(hdc2, hgdiobj);
		DeleteDC(hdc2);
		EndPaint(hWnd, &ps);

		// Stuff that goes on top of the bitmap.
		if(addMethod == POLYGON)
			RedrawPolygon(hWnd, s_pbbmGdata);

		if(s_pbbmGdata->usageState == PLAYBACK_STATE && (s_pbbmGdata->playState == PLAY || s_pbbmGdata->playState == PAUSE))
		{
			g_playBackHighLightList.Lock();
			AnimateAnimats(
				rect,
				s_pbbmGdata,
				s_pbbmGdata->playbackInf->sce.numSaveIterations,
				s_pbbmGdata->playbackInf->sce.totalNumAnimats,
				s_pbbmGdata->playbackInf->animatState,
				//(LPBITMAPINFO)&s_bitMapInfo,
				s_animateThreadParam.stateIndex,
				&g_playBackHighLightList,
				s_pbbmGdata->bAnimats);
			g_playBackHighLightList.Unlock();
		}
		break;
	default:
		return DefWindowProc(hWnd, message, wParam, lParam);
	}
	//s_prevActiveLB = GetActiveListBox(&s_prevSpe, &s_prevInd, &s_prevPod, &s_prevPdm);
	//s_prevSeedType = GetSeedTypeRadioState();
	//s_prevAddMethod = GetAddMethodRadioState();
   return 0;
}


