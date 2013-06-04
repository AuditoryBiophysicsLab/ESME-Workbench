#ifndef _3MBSEEDINGfUNCTIONS_H
#define _3MBSEEDINGfUNCTIONS_H

#include <windows.h>
#include <commctrl.h>
#include <winuser.h>
#include "3mb.h"
#include "3mbSceInterface.h"

BATHYBITMAP_WIN_PARAM GetInitializedSeedInf(SCENARIOUTPUTFILEINF *pSceOutputFileInf, BATHYUSESTATE InitialState);
COORD_DEPTH GetValidRndCoordWithinFocalDistance(CScenario *pScenario, double Lat, double Lon, ENVMINMAX Bathy,
												double ShrFllwVal, double MinSeedDepth, double FocalDistance);

void AddAnimatsAroundEnvData(ENVMOUSECLICK *PCoordMouseClkDown, ENVMOUSECLICK *PCoordMouseClkUp,
							 BATHYBITMAP_WIN_PARAM *pEnvBitmpGdata, CScenario *pSce);
void AddAnimatToPod(int SpeIndex, int PodIndex, BATHYBITMAP_WIN_PARAM *pEnvBitmpGdata, CScenario *pSce);
COORD_DEPTH GetValidRndCoordAboutLatLon(CScenario *pScenario, double Lat, double Lon, ENVMINMAX Bathy,
										double ShrFllwVal, double MinSeedDepth,	double AveDist, double StdDist);

BATHYMAPINF GetBathyMapInf(ENVMOUSECLICK *PMseClkDwn, ENVMOUSECLICK *PMseClkUp);
BATHYMAPINF GetBathyMapInf();
void UpdateEnvBitmapDlg(BATHYBITMAP_WIN_PARAM *pEnvBitmpGdata);

ACTIVE_LISTBOX ClearActiveListBoxes();
ACTIVE_LISTBOX SetActiveListBox(ACTIVE_LISTINDEX Indices);
ACTIVE_LISTBOX GetActiveListBox();

RADIO_SEEDTYPE GetSeedTypeRadioState();
RADIO_SEEDTYPE SetSeedTypeRadioState(RADIO_SEEDTYPE SeedType);
RADIO_ADDMETHOD GetAddMethodRadioState();
RADIO_ADDMETHOD SetAddMethodStateRadio(RADIO_ADDMETHOD AddMethod);


#endif