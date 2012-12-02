#ifndef _3MBSEEDDLGLAYOUTCONFIG_H
#define _3MBSEEDDLGLAYOUTCONFIG_H
#include "3mb.h"


const int NUMCONFIGURATIONS = 4;
const int CONTRLSPACEPIXELS = 7;  // space inbetween controls.
const int DLGBOXLEFTSIDEMARGIN = 14;
const int DLGBOXRIGHTSIDEMARGIN = 14;
const int DLBBOXTOPMARGIN = 11;


typedef struct HeightWidth
{
	int width; // horizontal width-wise
	int height; // vertical height-wise
}HEIGHTWIDTH;

typedef struct MapGroupConfig
{
	WNDPROP grpBox;
	WNDPROP heightLabel;
	WNDPROP widthLabel;
	WNDPROP totSurfAreaLabel;
	WNDPROP totWaterAreaLabel;
	WNDPROP totLandAreaLabel;
	WNDPROP latResLabel;
	WNDPROP lonResLabel;
}MAPGRPCONFIG;
typedef struct AnimatInformationGroupConfig
{
	WNDPROP grpBox;
	WNDPROP popSizeLabel;
	WNDPROP densityLabel;
}ANIMATGRPCONFIG;

typedef struct ButtonGroupConfig
{
	WNDPROP grpBox;
	WNDPROP samplePtsButton;
	WNDPROP slopeRegionsButton;
	//WNDPROP bathySlopeButton;
}BUTTONGRPCONFIG;

typedef struct DlgLayoutConfig
{
	int width; 
	int height;
	WNDPROP mouseMoveLabel;
	MAPGRPCONFIG map;
	ANIMATGRPCONFIG animat;
	BUTTONGRPCONFIG buttonGroup;
}DLGLAYOUTCONFIG;

typedef struct DlgLayoutOptions
{
	DLGLAYOUTCONFIG config[4];
	HEIGHTWIDTH reqDimensions[4];
}DLGLAYOUTOPTIONS;

WNDPROP MoveDlgControl(int ControlID, HWND Parent, int Dx, int Dy);
WNDPROP ResizeDlgControl(int ControlID, HWND Parent, int Dx, int Dy);

void GetLayoutConfigs(HWND hDlg, DLGLAYOUTOPTIONS *pLayoutOptions);

WNDPROP GetWinProps(HWND HWnd);
WNDPROP SetDlgWinProps(HWND Parent, int ControlID, WNDPROP WndProp);
DLGLAYOUT SelectDialogLayout(HWND hDlg, WNDPROP PrevDlgProp, const BATHYMAPINF *pBathyMapInf, const HEIGHTWIDTH *pReqPixels, DLGLAYOUTOPTIONS *pLayoutOptions);

const WNDPROP NORMAL_DIST_BUTTON_PROP =    {920, 566, 50, 24};
const WNDPROP SHAPEFILE_DIST_BUTTON_PROP = {920, 595, 57, 24};//{870, 595, 57, 24};

#endif // _3MBSEEDDLGLAYOUTCONFIG_H

