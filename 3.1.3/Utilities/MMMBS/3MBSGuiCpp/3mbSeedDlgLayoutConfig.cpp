#include "3mbSeedDlgLayoutConfig.h"

extern HWND g_hwndBathy;
extern HWND g_hwndDepthScale;
extern HWND g_hwndSlopeScale;
extern HWND g_hDlgSeed;



void GetLayoutDimensionRequirements(HWND hDlg, const DLGLAYOUTCONFIG *pLayoutArray, HEIGHTWIDTH *pDimensionReqArray);
DLGLAYOUTCONFIG GetDlgLayoutConfig(HWND hDlg, int ConfigNum);

void MoveWindowPropLocation(int Dx, int Dy, WNDPROP *pWp);
WNDPROP SetDlgControlLocation(WNDPROP Wn, HWND Hwnd, int Dx, int Dy);


void SetGroupConfigLocation(int X, int Y, BUTTONGRPCONFIG *pGrp);
void SetGroupConfigLocation(int X, int Y, ANIMATGRPCONFIG *pGrp);
void SetGroupConfigLocation(int X, int Y, MAPGRPCONFIG *pGrp);
void GetGroupConfig(HWND hDlg, int ConfigNum, MAPGRPCONFIG *pConfig);
void GetGroupConfig(HWND hDlg, int ConfigNum, ANIMATGRPCONFIG *pConfig);
void GetGroupConfig(HWND hDlg, int ConfigNum, BUTTONGRPCONFIG *pConfig);



void GetLayoutConfigs(HWND hDlg, DLGLAYOUTOPTIONS *pLayoutOptions)
{
	pLayoutOptions->config[0] = GetDlgLayoutConfig(hDlg, 0);
	pLayoutOptions->config[1] = GetDlgLayoutConfig(hDlg, 1);
	pLayoutOptions->config[2] = GetDlgLayoutConfig(hDlg, 2);
	pLayoutOptions->config[3] = GetDlgLayoutConfig(hDlg, 3);
	GetLayoutDimensionRequirements(hDlg, pLayoutOptions->config, pLayoutOptions->reqDimensions);
}


void GetLayoutDimensionRequirements(HWND hDlg, const DLGLAYOUTCONFIG *pLayoutArray, HEIGHTWIDTH *pDimensionReqArray)
{
	//WNDPROP dlgProp = GetWinProps(hDlg);
	//WNDPROP popGrpBoxProp = GetWinProps(GetDlgItem(hDlg,IDC_STATIC_POPULANTS_GROUP_BOX));
	//WNDPROP depthScale = GetWinProps(GetDlgItem(hDlg,IDC_STATIC_SCALE));
	//WNDPROP slopeScale = GetWinProps(GetDlgItem(hDlg,IDC_STATIC_SLOPE_SCALE));
	WNDPROP mouseMove = GetWinProps(GetDlgItem(hDlg,IDC_STATIC_MOUSE_POSITION));


	// Calculated required height and width include populants group box, mouse move static
	// label, the two sliders, and spacing between all controls including that of the
	// bathymetry bitmap.

	// Layout 0
	pDimensionReqArray[0].height = 
		pLayoutArray[0].height + // pLayoutArray 0's height
		CONTRLSPACEPIXELS + // space between pLayoutArray 0 and the mouse move label
		mouseMove.height + // height of the mouse move label
		CONTRLSPACEPIXELS; // distance between mouse move and the bathymetry bitmap.

	pDimensionReqArray[0].width =
		pLayoutArray[0].width + // width of pLayoutArray 0
		CONTRLSPACEPIXELS;  // pixels between pLayoutArray 0 and the populants group box


	// Layout 1
	pDimensionReqArray[1].height = 
		pLayoutArray[1].height + // pLayoutArray 1's height
		CONTRLSPACEPIXELS + // space between pLayoutArray 1 and the mouse move label
		mouseMove.height + // height of the mouse move label
		CONTRLSPACEPIXELS; // distance between mouse move and the bathymetry bitmap.
	pDimensionReqArray[1].width =
		pLayoutArray[1].width + // width of pLayoutArray 0
		CONTRLSPACEPIXELS;  // pixels between pLayoutArray 0 and the populants group box

	// Layout 2
	pDimensionReqArray[2].height = 
		pLayoutArray[2].height + // pLayoutArray 2's height
		CONTRLSPACEPIXELS + // space between pLayoutArray 2 and the mouse move label
		mouseMove.height + // height of the mouse move label
		CONTRLSPACEPIXELS; // distance between mouse move and the bathymetry bitmap.
	pDimensionReqArray[2].width =
		pLayoutArray[2].width + // width of pLayoutArray 0
		CONTRLSPACEPIXELS;  // pixels between pLayoutArray 0 and the populants group box

	// Layout 0
	pDimensionReqArray[3].width = 
		CONTRLSPACEPIXELS +    // pixel space needed between layout array 3 and populants group box.
		pLayoutArray[3].width +  // width needed by configuraiton 3
		CONTRLSPACEPIXELS; // pixel space needed between layout array 3 and scale bitmaps
	pDimensionReqArray[3].height = pLayoutArray[3].height;
}



/*
  Configuration 0              Configuration 1             Configuration 2
|-----------------|          |----------------------|    |---------------------------|
|                 |          |                      |    |                           |
|                 |          |                      |    |                           |
|-----------------|          |----------------------|    |---------------------------|
mousemovement___             mousemovement___            mousemovement___
mapinf0 buttons0 animat0     mapinf1 buttons0 animat0    mapinf2      buttons0 animat0 


  Configuration 3
|-------|  mapinf0
|       |
|       |  buttons2
|       |
|-------|  animat0
mousemovement___
*/

// Modifies the window property without changing the location of the actuall associated window.
void MoveWindowPropLocation(int Dx, int Dy, WNDPROP *pWp)
{
	pWp->x += Dx;
	pWp->y += Dy;
}

// Directly changes te window's location without the use of a Window Property (WNDPROP)
WNDPROP MoveDlgControl(int ControlID, HWND Parent, int Dx, int Dy)
{
	DWORD error;
	RECT clientRect; // control's client rect
	RECT windowRect; // control's window rect;
	POINT pnt;
	int height, width;
	WNDPROP w = {0};
	HWND hWnd = GetDlgItem(Parent, ControlID);

	GetClientRect(hWnd, &clientRect);
	GetWindowRect(hWnd, &windowRect);

	height = windowRect.bottom-windowRect.top;
	width = windowRect.right-windowRect.left;

	pnt.x = windowRect.left + Dx;
	pnt.y = windowRect.top + Dy;
	ScreenToClient(Parent, &pnt);

	if(FALSE == MoveWindow(hWnd, pnt.x, pnt.y, width, height, FALSE))
	{
		error = GetLastError();
		return w;
	}
	return GetWinProps(hWnd);
}

// Directly changes te window's location without the use of a Window Property (WNDPROP)
WNDPROP ResizeDlgControl(int ControlID, HWND Parent, int Dx, int Dy)
{
	DWORD error;
	RECT clientRect; // control's client rect
	RECT windowRect; // control's window rect;
	POINT pnt;
	int height, width;
	WNDPROP w = {0};
	HWND hWnd = GetDlgItem(Parent, ControlID);

	GetClientRect(hWnd, &clientRect);
	GetWindowRect(hWnd, &windowRect);

	height = windowRect.bottom-windowRect.top + Dy;
	width = windowRect.right-windowRect.left + Dx;

	pnt.x = windowRect.left;
	pnt.y = windowRect.top;
	ScreenToClient(Parent, &pnt);

	if(FALSE == MoveWindow(hWnd, pnt.x, pnt.y, width, height, FALSE))
	{
		error = GetLastError();
		return w;
	}
	return GetWinProps(hWnd);
}


WNDPROP SetDlgWinProps(HWND Parent, int ControlID, WNDPROP WndProp)
{
	DWORD error;
	WNDPROP w = {0};
	HWND hWnd = GetDlgItem(Parent, ControlID);
	if(TRUE == MoveWindow(hWnd, WndProp.x, WndProp.y, WndProp.width, WndProp.height, TRUE))
		return GetWinProps(hWnd);

	error = GetLastError();
	return w;	
}



WNDPROP SetDlgControlLocation(WNDPROP Wn, HWND Hwnd, int Dx, int Dy)
{
	DWORD error;

	if(FALSE == MoveWindow(Hwnd, Wn.x + Dx, Wn.y+Dy, Wn.width, Wn.height, FALSE))
	{
		error = GetLastError();
		memset(&Wn, 0, sizeof(WNDPROP));
		return Wn;
	}
	return Wn = GetWinProps(Hwnd);
}


void SetGroupConfigLocation(int X, int Y, MAPGRPCONFIG *pGrp)
{
	int dX = X - pGrp->grpBox.x;
	int dY = Y - pGrp->grpBox.y;
	pGrp->grpBox.x = X;
	pGrp->grpBox.y = Y;
	MoveWindowPropLocation(dX, dY, &pGrp->heightLabel);
	MoveWindowPropLocation(dX, dY, &pGrp->widthLabel);
	MoveWindowPropLocation(dX, dY, &pGrp->totSurfAreaLabel);
	MoveWindowPropLocation(dX, dY, &pGrp->totWaterAreaLabel);
	MoveWindowPropLocation(dX, dY, &pGrp->totLandAreaLabel);
	MoveWindowPropLocation(dX, dY, &pGrp->latResLabel);
	MoveWindowPropLocation(dX, dY, &pGrp->lonResLabel);
}

void SetGroupConfigLocation(int X, int Y, ANIMATGRPCONFIG *pGrp)
{
	int dX = X - pGrp->grpBox.x;
	int dY = Y - pGrp->grpBox.y;
	pGrp->grpBox.x = X;
	pGrp->grpBox.y = Y;
	MoveWindowPropLocation(dX, dY, &pGrp->densityLabel);
	MoveWindowPropLocation(dX, dY, &pGrp->popSizeLabel);
}

void SetGroupConfigLocation(int X, int Y, BUTTONGRPCONFIG *pGrp)
{
	int dX = X - pGrp->grpBox.x;
	int dY = Y - pGrp->grpBox.y;
	pGrp->grpBox.x = X;
	pGrp->grpBox.y = Y;
	MoveWindowPropLocation(dX, dY, &pGrp->samplePtsButton);
	MoveWindowPropLocation(dX, dY, &pGrp->slopeRegionsButton);
	//MoveWindowPropLocation(dX, dY, &pGrp->bathySlopeButton);
}

DLGLAYOUTCONFIG GetDlgLayoutConfig(HWND hDlg, int ConfigNum)
{
	DLGLAYOUTCONFIG s = {0};

	// Only a single configuration for the moust move label.  Always immediately below the bathymetry map.
	s.mouseMoveLabel = GetWinProps(GetDlgItem(hDlg, IDC_STATIC_MOUSE_POSITION));

	switch(ConfigNum)
	{
	case 0:
		GetGroupConfig(hDlg, 0, &s.map);
		GetGroupConfig(hDlg, 0, &s.animat);
		GetGroupConfig(hDlg, 0, &s.buttonGroup);
		break;
	case 1:
		GetGroupConfig(hDlg, 1, &s.map);
		GetGroupConfig(hDlg, 0, &s.buttonGroup);
		GetGroupConfig(hDlg, 0, &s.animat);
		break;
	case 2:
		GetGroupConfig(hDlg, 2, &s.map);
		GetGroupConfig(hDlg, 0, &s.buttonGroup);
		GetGroupConfig(hDlg, 0, &s.animat);
		break;
	case 3:
		GetGroupConfig(hDlg, 0, &s.map);
		GetGroupConfig(hDlg, 2, &s.buttonGroup);
		GetGroupConfig(hDlg, 0, &s.animat);
		break;
	}

	// Set the relative X and Y locations of the controls.
	if(ConfigNum == 0 || ConfigNum == 1 || ConfigNum == 2)
	{
		s.mouseMoveLabel.x = 0;
		s.mouseMoveLabel.y = 0;

		// Set the group boxes relative to the map information box
		SetGroupConfigLocation(
			0,
			s.mouseMoveLabel.y +
			s.mouseMoveLabel.height +
			CONTRLSPACEPIXELS,
			&s.map);

		SetGroupConfigLocation(
			s.map.grpBox.x + s.map.grpBox.width + CONTRLSPACEPIXELS,
			s.mouseMoveLabel.y +
			s.mouseMoveLabel.height +
			CONTRLSPACEPIXELS,
			&s.buttonGroup);

		SetGroupConfigLocation(
			s.buttonGroup.grpBox.x + s.buttonGroup.grpBox.width + CONTRLSPACEPIXELS,
			s.mouseMoveLabel.y +
			s.mouseMoveLabel.height +
			CONTRLSPACEPIXELS,
			&s.animat);

		s.width = s.animat.grpBox.x + s.animat.grpBox.width - s.map.grpBox.x;

		s.height = s.map.grpBox.height;
		if(s.height < s.buttonGroup.grpBox.height)
			s.height = s.buttonGroup.grpBox.height;
		if(s.height < s.animat.grpBox.height)
			s.height = s.animat.grpBox.height;

	}
	else if(ConfigNum == 3)
	{

		s.mouseMoveLabel.x = 0;
		s.mouseMoveLabel.y = 0;

		// Set the group boxes relative to the map information box
		SetGroupConfigLocation(0, 0, &s.map);
		SetGroupConfigLocation(0, s.map.grpBox.y + s.map.grpBox.height + CONTRLSPACEPIXELS, &s.buttonGroup);
		SetGroupConfigLocation(0, s.buttonGroup.grpBox.y + s.buttonGroup.grpBox.height + CONTRLSPACEPIXELS,
			&s.animat);

		// The width required will be the greater of the three between the map group box
		// width, the button group box width, and the animat density group box width.
		s.width = s.map.grpBox.width;
		if(s.width < s.buttonGroup.grpBox.width)
			s.width = s.buttonGroup.grpBox.width;
		if(s.width < s.animat.grpBox.width)
			s.width = s.animat.grpBox.width;

		s.height = s.animat.grpBox.y + s.animat.grpBox.height - s.map.grpBox.y;
	}

	return s;
}



WNDPROP GetWinProps(HWND HWnd)
{
	WNDPROP wp = {0};
	POINT p;
	RECT rC, rW;
	if(HWnd == NULL)
		return wp;

	// The GetClientRect function retrieves the coordinates of a window's client area.
	// The client coordinates specify the upper-left and lower-right corners of the client area.
	// Because client coordinates are relative to the upper-left corner of a window's client area,
	// the coordinates of the upper-left corner are (0,0). 
	GetClientRect(HWnd, &rC);
	wp.height = rC.bottom;
	wp.width = rC.right;

	// The GetWindowRect function retrieves the dimensions of the bounding rectangle of the specified window.
	// The dimensions are given in screen coordinates that are relative to the upper-left corner of the screen.
	GetWindowRect(HWnd, &rW);
	p.x = rW.left;
	p.y = rW.top;

	// Convert Window Rect to be relative to the parent window.
	if(HWnd != g_hDlgSeed)
		ScreenToClient(g_hDlgSeed, &p);

	wp.x = p.x;
	wp.y = p.y;
	//wp.hwnd = HWnd;
	return wp;
}


typedef struct SeedDialogSetup
{
	WNDPROP dlgWndProp; // the seed dialog
	POINT min; // minimum, left-most X and upper-most Y
	POINT max; // maximum, right-most X and bottom-most Y
}SEEDDLGSETUP;

SEEDDLGSETUP GetSeedDlgSetup()
{
	static BOOL calledOnce = FALSE;
	WNDPROP w;// = GetWinProps(g_hDlgSeed);
	static SEEDDLGSETUP s;

	// Handle values that need only be set once
	if(calledOnce == FALSE)
	{
		calledOnce = TRUE;
		w = GetWinProps(GetDlgItem(g_hDlgSeed, IDC_STATIC_BATHYBOX));
		s.min.x = w.x;
		s.min.y = w.y;
	}

	// Get the dialog box window's properties.  If there's been a change then update the
	// rest of the SEEDDLGSETUP stuct as needed.
	if(0 == memcmp(&s.dlgWndProp, &(w = GetWinProps(g_hDlgSeed)), sizeof(WNDPROP)))
		return s;

	// If only the dialog box's location has changed update only that and return.
	if(s.dlgWndProp.height == w.height && s.dlgWndProp.width == w.width)
	{
		// Height and width are identical but x and y are not.
		memcpy(&s.dlgWndProp, &w, sizeof(WNDPROP));
		return s;
	}

	memcpy(&s.dlgWndProp, &w, sizeof(WNDPROP));


	return s;
}

/* ********************************************************************************************************************
   *                          *                              *                     *                                  *
   *                          * IDC_STATIC_SCALE             *                     *                                  *
   *                          *                              *                     * IDC_STATIC_POPULANTS_GROUP_BOX   *
   *                          * IDC_STATIC_SLOPE_SCALE       *                     *      (popGrpBoxProp)             *
   *      bathyBitmapProp     *                              *                     *                                  *
   *                          *                              *                     ************************************
   *                          *   (scaleProp)                *                     |                                  *
   *                          *                              *         B           |                                  *
   *                          *                              *                     |                                  *
   *                          *                              *                     |                                  *
   ***********************************************************                     |                 C                *
   *                                                         |                     |                                  *
   *                                                         |                     |                                  *
   *                          A                              |                     |                                  *
   *                                                         |                     |                                  *
   *********************************************************************************************************************/
DLGLAYOUT SelectDialogLayout(HWND hDlg, WNDPROP PrevDlgProp, const BATHYMAPINF *pBathyMapInf, const HEIGHTWIDTH *pReqPixels, DLGLAYOUTOPTIONS *pLayoutOptions)
{
	//SEEDDLGSETUP sedg = GetSeedDlgSetup();
	static int scaleWidths = GetWinProps(GetDlgItem(hDlg,IDC_STATIC_SCALE)).width;
	static int minY = GetWinProps(GetDlgItem(hDlg,IDC_STATIC_SCALE)).y;
	int c, i;
	int dx, dy;
	const int CONTRLSPACEPIXELS = 7;
	//WNDPROP winProp;
	DLGLAYOUT dlgLayout;
	WNDPROP winProp;
	WNDPROP dlg = GetWinProps(hDlg);
	WNDPROP pop = GetWinProps(GetDlgItem(hDlg,IDC_STATIC_POPULANTS_GROUP_BOX));
	HEIGHTWIDTH bathyPixAvial[NUMCONFIGURATIONS] = {0}; // Pixels available for the bathymetry map
	HEIGHTWIDTH bathyDimResult[NUMCONFIGURATIONS] = {0}; //  Resulting dimensions of the bathymap
	HEIGHTWIDTH cntrlsPixAvail[NUMCONFIGURATIONS] = {0}; // Pixels available for the controls
	MAPGRPCONFIG m;
	ANIMATGRPCONFIG a;
	BUTTONGRPCONFIG b;


	// Get the current bathymetry bitmap properties.  Needed for its X Y values.
	if(g_hwndBathy == NULL)
	{
		_ASSERT(g_hwndDepthScale == NULL && g_hwndSlopeScale == NULL);

		// Bathymetry Map
		dlgLayout.bathy.x = DLGBOXLEFTSIDEMARGIN;
		dlgLayout.bathy.y = DLBBOXTOPMARGIN;
		dlgLayout.bathy.width = 0;
		dlgLayout.bathy.height = 0;

		// Widths for the scaling bitmaps is needed now.
		winProp = GetWinProps(GetDlgItem(hDlg,IDC_STATIC_SCALE));
		while((winProp.width % 16) != 0)
			winProp.width++;

		// Depth Scale
		dlgLayout.depth.x = 0;
		dlgLayout.depth.y = DLBBOXTOPMARGIN;
		dlgLayout.depth.width = winProp.width;
		dlgLayout.depth.height = winProp.height;


		dlgLayout.slope.x = 0;
		dlgLayout.slope.y = DLBBOXTOPMARGIN;
		dlgLayout.slope.width = winProp.width;
		dlgLayout.slope.height = winProp.height;

	}
	else
	{
//		_ASSERT(g_hwndDepthScale != NULL && g_hwndSlopeScale != NULL);

		dlgLayout.bathy = GetWinProps(g_hwndBathy);

		// Get the current depth and slope scale and window properties.  Needed for their
		// dimensions.
		dlgLayout.depth = GetWinProps(g_hwndDepthScale);
		dlgLayout.slope = GetWinProps(g_hwndSlopeScale);
	}


	// The first three layouts have a horizontal layout
	for(c=0; c<3; c++)
	{
		if(dlg.width < pReqPixels[c].width + pop.width)
			continue;

		// Calculate the pixels avalable for the bathymetry map
		bathyPixAvial[c].width = dlg.width - (pop.width +
									     dlgLayout.depth.width +
										 dlgLayout.slope.width +
										 3*CONTRLSPACEPIXELS +
										 DLGBOXLEFTSIDEMARGIN +
										 DLGBOXRIGHTSIDEMARGIN);
		bathyPixAvial[c].height = dlg.height - (pReqPixels[c].height +
										   DLBBOXTOPMARGIN);

		cntrlsPixAvail[c].width = dlg.width - (pop.width +
										  CONTRLSPACEPIXELS +
										  DLGBOXLEFTSIDEMARGIN +
										  DLGBOXRIGHTSIDEMARGIN);
		cntrlsPixAvail[c].height = dlg.height - (pop.width +
										    CONTRLSPACEPIXELS +
											DLBBOXTOPMARGIN);
	}

	// vertical layout
		// Calculate the pixels avalable for the bathymetry map
	bathyPixAvial[3].width =
		dlg.width - (
		DLGBOXRIGHTSIDEMARGIN + // margin on the dialog box's right side
		pop.width + // populants group box width
		CONTRLSPACEPIXELS + // pixels between the populants group box and this layout's setup	
		pReqPixels[3].width + // pixels required by the controls of this layout
		CONTRLSPACEPIXELS + // pixels between the this layout's setup and the depth bitmap slider
		dlgLayout.depth.width + // depth bitmap slider width
		CONTRLSPACEPIXELS + // pixels between the two sliders
		dlgLayout.slope.width + // slope bitmap slider width
		CONTRLSPACEPIXELS + // pixels between the slope slider and the bathymetry bitmap
		DLGBOXLEFTSIDEMARGIN); // margin on the left side of the dialog box.

	bathyPixAvial[3].height =
		dlg.height - (
		DLBBOXTOPMARGIN + // top of the dialog box margin
		7 + // space for the mouse move.
		//pReqPixels[3].height + // height of this layout
		DLBBOXTOPMARGIN);// bottom margin of the dialog box?



	cntrlsPixAvail[3].width = 
		dlg.width - (
		DLGBOXLEFTSIDEMARGIN + // left margin
		pop.width + // populants group box width
		CONTRLSPACEPIXELS + // space between the populants group box and depth slider
		dlgLayout.depth.width + // depth slider width
		CONTRLSPACEPIXELS + // space between depth slider and slope slider
		dlgLayout.slope.width + // slope slider width
		CONTRLSPACEPIXELS);// space between slope slider and bathymetry map.
	cntrlsPixAvail[3].height =
		dlg.height - (
		DLBBOXTOPMARGIN + // top margin
		0); // ? bottom margin???

	//----------------------------------------------------------------------------------//
	// Handle the dialog set up based on if the map runs horizontally (width being larger
	// than height or square) or vertically.
	//-------------------------------------//
	// Horizontal and square maps have all of the controls underneath (in the dialog box
	// design) remain their.  Vertical maps can have those controls repositioned when
	// possible to give the bitmap as much space as possible.
	if(pBathyMapInf->widthMeters >= pBathyMapInf->heightMeters /* Horizontal orientation or square*/)
	{
		// Cares about height available when determining width.  Grab the first the meets the
		// requirements starting with the widest (want the map as wide as possible)
		for(c=0; c<3; c++)
		{
			if(bathyPixAvial[c].width == 0)
				continue;

			bathyDimResult[c].width = bathyPixAvial[c].width-2; // -2 to be sure to save room for the border.
			while(bathyDimResult[c].width%16 != 0)
				bathyDimResult[c].width--;

			bathyDimResult[c].height =
				(int)((double)bathyDimResult[c].width * pBathyMapInf->heightMeters/pBathyMapInf->widthMeters);

			while(bathyDimResult[c].height > bathyPixAvial[c].height-2 || bathyDimResult[c].width%16 != 0)
			{
				while(--bathyDimResult[c].width%16 != 0)
					;
				bathyDimResult[c].height =
					(int)((double)(bathyDimResult[c].width * pBathyMapInf->heightMeters/pBathyMapInf->widthMeters));
			}
		}

		// Now determine which configuration allows for the maximum width
		c = 0;
		for(i=1; i<3; i++)
		{
			if(cntrlsPixAvail[c].width <= bathyDimResult[i].width)
				c = i;
		}

		_ASSERT(c >= 0 && c <=2);
	}
	else /* vertical orientation (height is greater than width)*/
	{
		c = 3;
		bathyDimResult[c].height = bathyPixAvial[c].height-2;
		bathyDimResult[c].width = (int)(bathyDimResult[c].height * pBathyMapInf->widthMeters/pBathyMapInf->heightMeters);
		while(bathyDimResult[c].width > bathyPixAvial[c].width || bathyDimResult[c].width%16 != 0)
		{
			while(--bathyDimResult[c].width%16 != 0)
				;
			_ASSERT(bathyDimResult[c].width%16 == 0);
			bathyDimResult[c].height = (int)(bathyDimResult[c].width * pBathyMapInf->heightMeters/pBathyMapInf->widthMeters);
		}
	}

	_ASSERT(c >= 0 && c <=3);

	// Get the configuration
	//layoutConfig = GetDlgLayoutConfig(

	//----------------------------------------------------------------------------------//
	// Set up the bathymetry map size on the dialog box.
	//-------------------------------------------------//
	dlgLayout.bathy.width = bathyDimResult[c].width;
	dlgLayout.bathy.height = bathyDimResult[c].height;
	_ASSERT(bathyDimResult[c].width%16 == 0);


	//----------------------------------------------------------------------------------//
	// Set the slope and depth scale locations and size on the dialog box.
	//--------------------------------------------------------------------//
	// The X locations are a function of the space the bathymetry bitmap take up plus
	// space inbetween
	dlgLayout.depth.x = dlgLayout.bathy.x + dlgLayout.bathy.width + CONTRLSPACEPIXELS;
	dlgLayout.depth.y = dlgLayout.bathy.y;
	dlgLayout.depth.height = dlgLayout.bathy.height;
#if 0
	if(g_hwndDepthScale == NULL)
	{
		// Depth scale
		g_hwndDepthScale = CreateWindowEx(
			0,	// dwExStyle: The extended window style of the window being created.
			SCALEBITMAPWINDOWCLASSNAME, // lpClassName: Pointer to a null-terminated string or a class atom
			"ScaleBitmap", // lpWindowName: Pointer to a null-terminated string that specifies the window name
			WS_CLIPCHILDREN | WS_BORDER | WS_VISIBLE | WS_CHILD, //dwStyle: Specifies the style of the window
			dlgLayout.depth.x, // x: Initial horizontal position of the window
			dlgLayout.depth.y, // y: Initial vertical position of the window
			dlgLayout.depth.width + 2, // The width of the window in device units.  +2 for the border box.
			dlgLayout.depth.height + 2, // The height of the window in device units.  +2 for the border box.
			g_hDlgSeed, // hWndParent: Handle to the parent or owner window of the window being created.
			(HMENU) NULL, //hMenu: Handle to a menu, or specifies a child-window identifier, depending on the window style. 
			NULL,//hInstance: Handle to the instance of the module to be associated with the window.
			(LPVOID)(pSeedInf)); // lpParam: Pointer to a value to be passed to the window through the CREATESTRUCT
								 //          structure (lpCreateParams member) pointed to by the lParam param of
								 //			 the WM_CREATE message
	}
	else
	{
		SetWindowPos(g_hwndDepthScale, HWND_TOP, dlgLayout.depth.x, dlgLayout.depth.y+2, dlgLayout.depth.width+2, dlgLayout.depth.height, SWP_SHOWWINDOW);
	}
#endif

	dlgLayout.slope.x = dlgLayout.depth.x + dlgLayout.depth.width + CONTRLSPACEPIXELS;
	dlgLayout.slope.y = dlgLayout.bathy.y;
	dlgLayout.slope.height = dlgLayout.bathy.height;

#if 0
	if(g_hwndSlopeScale == NULL)
	{
		g_hwndSlopeScale  = CreateWindowEx(
			0,	// dwExStyle: The extended window style of the window being created.
			SCALEBITMAPWINDOWCLASSNAME, // lpClassName: Pointer to a null-terminated string or a class atom
			"ScaleSlopeBitmap", // lpWindowName: Pointer to a null-terminated string that specifies the window name
			WS_CLIPCHILDREN | WS_BORDER | WS_VISIBLE | WS_CHILD, //dwStyle: Specifies the style of the window
			dlgLayout.slope.x, // x: Initial horizontal position of the window
			dlgLayout.slope.y, //layout.depth.y, // y: Initial vertical position of the window
			dlgLayout.slope.width + 2, // The width of the window in device units.  +2 for the border box.
			dlgLayout.slope.height + 2, // The height of the window in device units.  +2 for the border box.
			g_hDlgSeed, // hWndParent: Handle to the parent or owner window of the window being created.
			(HMENU) NULL, //hMenu: Handle to a menu, or specifies a child-window identifier, depending on the window style. 
			NULL,//hInstance: Handle to the instance of the module to be associated with the window.
			(LPVOID)(pSeedInf)); // lpParam: Pointer to a value to be passed to the window through the CREATESTRUCT
								 //          structure (lpCreateParams member) pointed to by the lParam param of
								 //			 the WM_CREATE message
	}
	else
	{
		SetWindowPos(g_hwndSlopeScale, HWND_TOP, dlgLayout.slope.x, dlgLayout.slope.y+2, dlgLayout.slope.width+2, dlgLayout.slope.height, SWP_SHOWWINDOW);
	}
#endif

#if 0
	if(g_hwndBathy == NULL)
	{
		// Bathymetry bitmap
		g_hwndBathy  = CreateWindowEx(
			0,
			BATHYBITMAPWINDOWCLASSNAME,"BathyBitmap",
			WS_CLIPCHILDREN | WS_BORDER | WS_VISIBLE | WS_CHILD,
			DLGBOXLEFTSIDEMARGIN, //layout.bathy.x,
			DLBBOXTOPMARGIN, //layout.bathy.y,
			dlgLayout.bathy.width+2, //layout.bathy.width+2, // The width of the window in device units.  +2 for the border box.
			dlgLayout.bathy.height+2, // The height of the window in device units.  +2 for the border box.
			g_hDlgSeed, // parent window.
			(HMENU) NULL,
			NULL,
			(LPVOID)(pSeedInf));
	}
	else
	{
		SetWindowPos(g_hwndBathy, HWND_TOP, NULL, NULL, dlgLayout.bathy.width+2, dlgLayout.bathy.height+2,
			SWP_NOMOVE|SWP_SHOWWINDOW);
	}
#endif
	//----------------------------------------------------------------------------------//
	// Get the change in the dialog box width and height.
	//--------------------------------------------------//
	// The change in width (dx) here tells how far to move the populants group box
	// controls to the left (negative) or right (positive)
	dx = dlg.width - PrevDlgProp.width;
	dy = dlg.height - PrevDlgProp.height;

	//----------------------------------------------------------------------------------//
	// Move the Populants Group Box controls.
	//---------------------------------------//
	MoveDlgControl(IDC_STATIC_POPULANTS_GROUP_BOX, hDlg, dx, 0);
	// Things under the IDC_STATIC_POPULANTS_GROUP_BOX control.
	MoveDlgControl(IDC_LOAD_SCENARIO, hDlg, dx, 0);
	MoveDlgControl(IDC_TRACKTOGGLE_BUTTON, hDlg, dx, 0);
	MoveDlgControl(IDC_VISUAL_INC_BUTTON, hDlg, dx, 0);
	MoveDlgControl(IDC_VISUAL_RESET_BUTTON, hDlg, dx, 0);
	MoveDlgControl(IDC_VISUAL_STOP_BUTTON, hDlg, dx, 0);
	MoveDlgControl(IDC_VISUAL_PLAY_BUTTON, hDlg, dx, 0);
	MoveDlgControl(IDC_VISUAL_DEC_BUTTON, hDlg, dx, 0);


	MoveDlgControl(IDC_STATIC_SPE_ACSRC, hDlg, dx, 0);
	MoveDlgControl(IDC_BUTTON_ENVDLG_FIRSANIMATACSTSRCE, hDlg, dx, 0);
	MoveDlgControl(IDC_BUTTON_ENVDLG_SPEC_ADD, hDlg, dx, 0);
	MoveDlgControl(IDC_BUTTON_ENVDLG_SPEC_REMOVE, hDlg, dx, 0);
	MoveDlgControl(IDC_LIST_SPECIES, hDlg, dx, 0);
	MoveDlgControl(IDC_STATIC_INDIVIDUAL, hDlg, dx, 0);
	MoveDlgControl(IDC_BUTTON_ENVDLG_INDIVIDUAL_DELETE, hDlg, dx, 0);
	MoveDlgControl(IDC_STATIC_PODS, hDlg, dx, 0);
	MoveDlgControl(IDC_BUTTON_ENVDLG_POD_DELETE, hDlg, dx, 0);
	MoveDlgControl(IDC_LIST_INDIVIDUAL, hDlg, dx, 0);
	MoveDlgControl(IDC_LIST_PODS, hDlg, dx, 0);
	MoveDlgControl(IDC_STATIC_POP_LOCATION, hDlg, dx, 0);
	MoveDlgControl(IDC_BUTTON_ENVDLG_PODMEMBER_DELETE, hDlg, dx, 0);
	MoveDlgControl(IDC_LIST_SEL_POP, hDlg, dx, 0);
	MoveDlgControl(IDC_STATIC_SEEDOPTNS_GRPBOX, hDlg, dx, 0);
	MoveDlgControl(IDC_RADIO_SEED_INDIVIDUAL, hDlg, dx, 0);
	MoveDlgControl(IDC_RADIO_ADD_POD, hDlg, dx, 0);
	MoveDlgControl(IDC_EDIT_ENVBITMAP_INIT_PODSIZE, hDlg, dx, 0);
	MoveDlgControl(IDC_STATIC_ENVBITMAP_INITPODSIZE, hDlg, dx, 0);
	MoveDlgControl(IDC_STATIC_LEADANIMATSETTING_GRPBOX, hDlg, dx, 0);
	MoveDlgControl(IDC_EDIT_ENVBITMAP_FOCAL_DIST, hDlg, dx, 0);
	MoveDlgControl(IDC_STATIC_ENVBITMAP_FOCAL_DIST, hDlg, dx, 0);
	MoveDlgControl(IDC_RADIO_PODLEADTYPE_ANIMAT, hDlg, dx, 0);
	MoveDlgControl(IDC_RADIO_PODLEADTYPE_CENTROID, hDlg, dx, 0);
	MoveDlgControl(IDC_STATIC_ADJUSTPODSIZE_GRPBOX, hDlg, dx, 0);
	MoveDlgControl(IDC_BUTTON_DEBUG_ADDPODMEMBER, hDlg, dx, 0);
	MoveDlgControl(IDC_STATIC_SEEDING_METHOD, hDlg, dx, 0);
	MoveDlgControl(IDC_RADIO_MOUSE_CLICKING, hDlg, dx, 0);
	MoveDlgControl(IDC_RADIO_N_ANIMATS_AROUND_CLICK, hDlg, dx, 0);
	MoveDlgControl(IDC_RADIO_BOUNDING_BOX, hDlg, dx, 0);
	MoveDlgControl(IDC_RADIO_N_ANIMATS_OVERMAP, hDlg, dx, 0);
	MoveDlgControl(IDC_RADIO_POLYGON, hDlg, dx, 0);
	MoveDlgControl(IDC_RADIO_ANIMATS_BY_DENSITY, hDlg, dx, 0);
	MoveDlgControl(IDC_EDIT_DENSITY, hDlg, dx, 0);
	MoveDlgControl(IDC_STATIC_DENSITY, hDlg, dx, 0);
	MoveDlgControl(IDC_STATIC_DENSITYCALC, hDlg, dx, 0);
	MoveDlgControl(IDC_RADIO_LOAD_SHAPE_FILE, hDlg, dx, 0);
	MoveDlgControl(IDC_EDIT_NUM_ANIMATS, hDlg, dx, 0);
	MoveDlgControl(IDC_STATIC_NUMANIMATS, hDlg, dx, 0);
	MoveDlgControl(IDC_EDIT_AVE_DIST, hDlg, dx, 0);
	MoveDlgControl(IDC_STATIC_AVEDIST, hDlg, dx, 0);
	MoveDlgControl(IDC_EDIT_STD_DIST, hDlg, dx, 0);
	MoveDlgControl(IDC_STATIC_STDEVDIST, hDlg, dx, 0);
	MoveDlgControl(IDC_VISUAL_RUN_BUTTON, hDlg, dx, 0);
	MoveDlgControl(IDC_BUTTON_DENSITYDISTRIBUTE, hDlg, dx, 0);
	MoveDlgControl(IDC_BUTTON_PODSIZEINC, hDlg, dx, 0);
	MoveDlgControl(IDC_BUTTON_PODSIZEDEC, hDlg, dx, 0);

	//----------------------------------------------------------------------------------//
	// Move the Done Button
	//---------------------//
	MoveDlgControl(IDOK, hDlg, dx, dy);

	//----------------------------------------------------------------------------------//
	// Configure and move the Map Information group box controls
	//-----------------------------------------------------------//
	dy = dlgLayout.bathy.y + dlgLayout.bathy.height + CONTRLSPACEPIXELS;
	dx = DLGBOXLEFTSIDEMARGIN;
	SetDlgControlLocation(pLayoutOptions->config[c].mouseMoveLabel, GetDlgItem(hDlg, IDC_STATIC_MOUSE_POSITION), dx, dy);

	if(c == 3)
	{
		dy = dlgLayout.bathy.y;
		dx = dlgLayout.slope.x + dlgLayout.slope.width + CONTRLSPACEPIXELS;
	}

	m = pLayoutOptions->config[c].map;
	SetDlgControlLocation(m.grpBox, GetDlgItem(hDlg, IDC_STATIC_MAPGROUPBOX), dx, dy);
	SetDlgControlLocation(m.heightLabel, GetDlgItem(hDlg, IDC_STATIC_MAPHEIGHT), dx, dy);
	SetDlgControlLocation(m.widthLabel, GetDlgItem(hDlg, IDC_STATIC_MAPWIDTH), dx, dy);
	SetDlgControlLocation(m.totSurfAreaLabel, GetDlgItem(hDlg, IDC_STATIC_MAPSURFAREA), dx, dy);
	SetDlgControlLocation(m.totWaterAreaLabel, GetDlgItem(hDlg, IDC_STATIC_MAPSURFAREA_WATER), dx, dy);
	SetDlgControlLocation(m.totLandAreaLabel, GetDlgItem(hDlg, IDC_STATIC_MAPSURFAREA_LAND), dx, dy);
	SetDlgControlLocation(m.latResLabel, GetDlgItem(hDlg, IDC_STATIC_RESOLUTION_LAT), dx, dy);
	SetDlgControlLocation(m.lonResLabel, GetDlgItem(hDlg, IDC_STATIC_RESOLUTION_LON), dx, dy);

	a = pLayoutOptions->config[c].animat;
	SetDlgControlLocation(a.grpBox, GetDlgItem(hDlg, IDC_STATIC_ANIMATDENSITYGROUPBOX), dx, dy);
	SetDlgControlLocation(a.densityLabel, GetDlgItem(hDlg, IDC_STATIC_ANIMATDENSITY), dx, dy);
	SetDlgControlLocation(a.popSizeLabel, GetDlgItem(hDlg, IDC_STATIC_ANIMATPOPSIZE), dx, dy);

	b = pLayoutOptions->config[c].buttonGroup;
	SetDlgControlLocation(b.grpBox, GetDlgItem(hDlg, IDC_STATIC_DISPLAYOPTSGROUPBOX), dx, dy);
	SetDlgControlLocation(b.samplePtsButton, GetDlgItem(hDlg, IDC_BUTTON_TOGGLE_DATPTS), dx, dy);
	//SetDlgControlLocation(b.bathySlopeButton, GetDlgItem(hDlg, IDC_BUTTON_TOGGLE_EXTRAS), dx, dy);
	SetDlgControlLocation(b.slopeRegionsButton, GetDlgItem(hDlg, IDC_BUTTON_TOGGLE_SLOPEREGIONS),  dx, dy);

	return dlgLayout;
}

/* Map Information Layout options
    Layout 0                     Layout 1                            Layout 2
Group Box--------| Group Box------------------------| Group Box-----------------------------------------|
|Height          | |Height           Total Land Area| |Height           Total Water Area  Lat resolution|
|Width           | |Width            Lat resolution | |Width            Total Land Area   Lat resolution|
|Total Surf Area | |Total Surf Area  Lon resolution | |Total Surf Area                                  |
|Total Water Area| |Total Water Area                | |--------------------------------------------------
|Total Land Area | |---------------------------------
|Lat resolution  | 
|Lon resolution  | 
|----------------|    */
void GetGroupConfig(HWND hDlg, int ConfigNum, MAPGRPCONFIG *pConfig)
{
	memset(pConfig, 0, sizeof(MAPGRPCONFIG));
	// Only three configurations fot the map information layout.

	_ASSERT(ConfigNum < 3);

	switch(ConfigNum)
	{
	case 0:
		pConfig->grpBox = GetWinProps(GetDlgItem(hDlg, IDC_STATIC_MAPGROUPBOX));
		pConfig->heightLabel = GetWinProps(GetDlgItem(hDlg, IDC_STATIC_MAPHEIGHT));
		pConfig->widthLabel = GetWinProps(GetDlgItem(hDlg, IDC_STATIC_MAPWIDTH));
		pConfig->totSurfAreaLabel = GetWinProps(GetDlgItem(hDlg, IDC_STATIC_MAPSURFAREA));
		pConfig->totWaterAreaLabel = GetWinProps(GetDlgItem(hDlg, IDC_STATIC_MAPSURFAREA_WATER));
		pConfig->totLandAreaLabel = GetWinProps(GetDlgItem(hDlg, IDC_STATIC_MAPSURFAREA_LAND));
		pConfig->latResLabel = GetWinProps(GetDlgItem(hDlg, IDC_STATIC_RESOLUTION_LAT));
		pConfig->lonResLabel = GetWinProps(GetDlgItem(hDlg, IDC_STATIC_RESOLUTION_LON));
		break;
	case 1:
		pConfig->grpBox = GetWinProps(GetDlgItem(hDlg, IDC_STATIC_MAPGROUPBOX2));
		pConfig->heightLabel = GetWinProps(GetDlgItem(hDlg, IDC_STATIC_MAPHEIGHT2));
		pConfig->widthLabel = GetWinProps(GetDlgItem(hDlg, IDC_STATIC_MAPWIDTH2));
		pConfig->totSurfAreaLabel = GetWinProps(GetDlgItem(hDlg, IDC_STATIC_MAPSURFAREA3));
		pConfig->totWaterAreaLabel = GetWinProps(GetDlgItem(hDlg, IDC_STATIC_MAPSURFAREA_WATER2));
		pConfig->totLandAreaLabel = GetWinProps(GetDlgItem(hDlg, IDC_STATIC_MAPSURFAREA_LAND2));
		pConfig->latResLabel = GetWinProps(GetDlgItem(hDlg, IDC_STATIC_RESOLUTION_LAT2));
		pConfig->lonResLabel = GetWinProps(GetDlgItem(hDlg, IDC_STATIC_RESOLUTION_LON2));
		break;
	case 2:
		pConfig->grpBox = GetWinProps(GetDlgItem(hDlg, IDC_STATIC_MAPGROUPBOX3));
		pConfig->heightLabel = GetWinProps(GetDlgItem(hDlg, IDC_STATIC_MAPHEIGHT3));
		pConfig->widthLabel = GetWinProps(GetDlgItem(hDlg, IDC_STATIC_MAPWIDTH3));
		pConfig->totSurfAreaLabel = GetWinProps(GetDlgItem(hDlg, IDC_STATIC_MAPSURFAREA4));
		pConfig->totWaterAreaLabel = GetWinProps(GetDlgItem(hDlg, IDC_STATIC_MAPSURFAREA_WATER3));
		pConfig->totLandAreaLabel = GetWinProps(GetDlgItem(hDlg, IDC_STATIC_MAPSURFAREA_LAND3));
		pConfig->latResLabel = GetWinProps(GetDlgItem(hDlg, IDC_STATIC_RESOLUTION_LAT3));
		pConfig->lonResLabel = GetWinProps(GetDlgItem(hDlg, IDC_STATIC_RESOLUTION_LON3));
		break;
	}
}


/*Animat Density Layout Options
  Layout 0         Layout 1            
Group Box--| Group Box---------|
|Pop Size  | |Pop Size  Density|
|Density   | |-----------------|
|----------|   */
void GetGroupConfig(HWND hDlg, int ConfigNum, ANIMATGRPCONFIG *pConfig)
{
	memset(pConfig, 0, sizeof(ANIMATGRPCONFIG));
	// Only two configurations fot the map information layout.
	_ASSERT(ConfigNum < 2);
	switch(ConfigNum)
	{
	case 0:
		pConfig->grpBox = GetWinProps(GetDlgItem(hDlg, IDC_STATIC_ANIMATDENSITYGROUPBOX));
		pConfig->popSizeLabel = GetWinProps(GetDlgItem(hDlg, IDC_STATIC_ANIMATPOPSIZE));
		pConfig->densityLabel = GetWinProps(GetDlgItem(hDlg, IDC_STATIC_ANIMATDENSITY));
		break;
	case 1:
		pConfig->grpBox = GetWinProps(GetDlgItem(hDlg, IDC_STATIC_ANIMATDENSITYGROUPBOX2));
		pConfig->popSizeLabel = GetWinProps(GetDlgItem(hDlg, IDC_STATIC_ANIMATPOPSIZE2));
		pConfig->densityLabel = GetWinProps(GetDlgItem(hDlg, IDC_STATIC_ANIMATDENSITY2));
		break;
	}
}



/* Button Group Layout Options
         Layout 0                              Layout 1                           Layout 2
---------------------------    -------------------------------------------      -------------
Sample Points  Bathy Regions   Sample Points  Bathy Regions  Slope Regions      Sample Points
Slope Regions                                                                   Slope Regions
                                                                                Bathy Regions  */
void GetGroupConfig(HWND hDlg, int ConfigNum, BUTTONGRPCONFIG *pConfig)
{
	memset(pConfig, 0, sizeof(ANIMATGRPCONFIG));

	// Only three configurations fot the map information layout.
	_ASSERT(ConfigNum < 3);
	switch(ConfigNum)
	{
	case 0:
		pConfig->grpBox = GetWinProps(GetDlgItem(hDlg, IDC_STATIC_DISPLAYOPTSGROUPBOX));
		pConfig->samplePtsButton = GetWinProps(GetDlgItem(hDlg, IDC_BUTTON_TOGGLE_DATPTS));
		pConfig->slopeRegionsButton = GetWinProps(GetDlgItem(hDlg, IDC_BUTTON_TOGGLE_SLOPEREGIONS));
		//pConfig->bathySlopeButton = GetWinProps(GetDlgItem(hDlg, IDC_BUTTON_TOGGLE_EXTRAS));
		break;
	case 1:
		pConfig->grpBox = GetWinProps(GetDlgItem(hDlg, IDC_STATIC_DISPLAYOPTSGROUPBOX2));
		pConfig->samplePtsButton = GetWinProps(GetDlgItem(hDlg, IDC_BUTTON_TOGGLE_DATPTS2));
		pConfig->slopeRegionsButton = GetWinProps(GetDlgItem(hDlg, IDC_BUTTON_TOGGLE_SLOPEREGIONS2));
		//pConfig->bathySlopeButton = GetWinProps(GetDlgItem(hDlg, IDC_BUTTON_TOGGLE_EXTRAS2));
		break;
	case 2:
		pConfig->grpBox = GetWinProps(GetDlgItem(hDlg, IDC_STATIC_DISPLAYOPTSGROUPBOX3));
		pConfig->samplePtsButton = GetWinProps(GetDlgItem(hDlg, IDC_BUTTON_TOGGLE_DATPTS3));
		pConfig->slopeRegionsButton = GetWinProps(GetDlgItem(hDlg, IDC_BUTTON_TOGGLE_SLOPEREGIONS3));
		//pConfig->bathySlopeButton = GetWinProps(GetDlgItem(hDlg, IDC_BUTTON_TOGGLE_EXTRAS3));
		break;
	}
}