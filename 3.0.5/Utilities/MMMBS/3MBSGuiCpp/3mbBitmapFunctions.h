#ifndef _3MBBITMAPFUNCTIONS_H
#define _3MBBITMAPFUNCTIONS_H
#include "3mb.h"
#include "3mbSceInterface.h"
#include "3mbSeedDlgLayoutConfig.h"
#include "IntManager.h"
/*
It helps to keep in mind the following:

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

int GetValidBitmapWidth(int TargetWidth);


void BathymetryDataToBuffer(RECT Rect, BYTE *BitMapData, BATHYBITMAP_WIN_PARAM *pSeedInf, BOOL *pAbort);
void SlopeDataToBuffer(RECT Rect, BATHYBITMAP_WIN_PARAM  *SeedInf, VOID *pvData);
void DataPointsToBuffer(RECT Rect, RAWENVIRONMENTALDATA *ed, BATHYEXTREMES DisplayExtremes, VOID *pvData);

// For drawing animat tracks
void AnimatsToDataBuffer(RECT Rect, VOID *pvData, BATHYBITMAP_WIN_PARAM *pBathyBmWinParam, int NumIterations,
						 int NumAnimats, ANIMATSTATE_FILEOUT *pCoord, CListManager <ANIMATSTATE_FILEOUT_PTR> *pHighLightList);
//void AnimatsToDataBuffer(RECT Rect, VOID *pvData, BATHYBITMAP_WIN_PARAM *pBathyBmWinParam, int NumIterations,
//						 int NumAnimats, ANIMATSTATE_FILEOUT *pCoord, CListManager <ANIMATSTATE_FILEOUT_PTR> *pHighLightList);


HBITMAP FinalizeBitmap(BATHYBITMAPINFO *pBitMapInfo, BOOL bBathy, BYTE *Bathy);



void BlendAlphaBlendIntoBitmap(RECT Rect, HBITMAP *phbitmap, VOID *pvData); // universal?


//HBITMAP SlopeToBitmap(RECT WindowRect, BATHYBITMAP_WIN_PARAM  *SeedInf);







void GenerateHightlightList(int SpeciesIndex, int PodIndex, int PodMemberIndex, int IndividualIndex, CListManager <ANIMATSTATE_FILEOUT_PTR> *pList);

// Used for seeding.
void AnimatsToBitmap(RECT Rect, VOID *pvData, BATHYBITMAP_WIN_PARAM *pBathyBmWinParam, int NumAnimats, INHABITINF *pCoord, int NumHighlights, INHABITINF *pHighLightList);



void AnimateAnimats(RECT Rect, BATHYBITMAP_WIN_PARAM *pBathyBmWinParam, int NumIterations, int NumAnimats, ANIMATSTATE_FILEOUT *pCoord, int StateIndex, CListManager <ANIMATSTATE_FILEOUT_PTR> *pHighLightList, BOOL Inverse);





typedef struct ScaleToBitmapParam
{
	int WinWidth;
	int WinHeight;
	BYTE *BitMapData;
	int *BitMapDataBufferLen;
	double dataMin;
	double dataMax;
	double displayMin;
	double displayMax;
	short YLine1;
	short YLine2;
	BOOL *Abort;
}SCALETOBITMAPPARAM;
BYTE *DepthScaleToBitmap(SCALETOBITMAPPARAM Param);
BYTE *SlopeScaleToBitmap(SCALETOBITMAPPARAM Param);


double YPixelToDepth(long Y, long WinHeight, double Shallow, double Deep);
short DepthToYPixel(long WinHeight, double Depth, double Shallow, double Deep);


void BmpInitInfo(LPBITMAPINFO bmpinfo, LONG width, LONG height);
BOOL SetBitmapPallet(LPBITMAPINFO bmpinfo);
BOOL SetSlopeBitmapPallet(LPBITMAPINFO bmpinfo);
//void BoundingBox(HWND hWnd, BATHYBITMAP_WIN_PARAM *bathyGdata, LPBITMAPINFO bmpinfo, BYTE *s_bathyData, COORD MouseDwn, COORD MousePos, BOOL Erase);
void BoundingBox(HWND hWnd, COORD MouseDwn, COORD MousePos, BOOL Erase);
void DrawLineTo(HWND hWnd, BATHYBITMAP_WIN_PARAM *bathyGdata, COORD FromCoord, COORD ToCoord, COLORREF Cr);
void DrawScaleLines(HWND hWnd, BATHYBITMAP_WIN_PARAM *pBathyBmWinParam, LPBITMAPINFO bmpinfo, BYTE *s_bathyData, short *pUpperLine, short *pLowerLine);
//void EraseLineTo(HWND hWnd, BATHYBITMAP_WIN_PARAM *bathyGdata, LPBITMAPINFO bmpinfo, BYTE *s_bathyData, COORD FromCoord, COORD ToCoord);
BOOL LatLonToScreenCoord(double Lat, double Lon, BATHYEXTREMES BathyExtremes, RECT WindowRect, COORD *pCoord);
void MapScaleToBitmapPaletIndex(RECT WindowRect, BYTE *BitMap, int BitMapBufferLength, BATHY_BITMAP_INF BitMapInf);
void RedrawPolygon(HWND hWnd, BATHYBITMAP_WIN_PARAM *bathyGdata);
void SetPolyMinMax(POLYGONINF *Poly);
int PlaybackIntegerToSeconds(int Integer, TCHAR *szBuff, size_t BuffSize);


enum DISPLAYTYPE
{
	MINIMIZED,
	MAXIMIZED,
	ZOOM,
};

typedef struct DisplayLayersData
{
	VOID *(buffer[3]); //
	BOOL *enabled;
}DISPLAYLAYERSDATA;

typedef struct BathyDisplayLayersData
{
	BYTE *(buffer[3]); //
	BOOL *enabled;
}BATHYDISPLAYLAYERDATA;

typedef struct DisplayLayers
{
	// The display layers are listed in draw order.
	BATHYDISPLAYLAYERDATA bathy;
	DISPLAYLAYERSDATA slope;

	DISPLAYLAYERSDATA envStimulus;

	DISPLAYLAYERSDATA datapts;
	DISPLAYLAYERSDATA animats;
	int buffLen[3];
	DISPLAYTYPE displayType; // either MINIMIZED, MAXIMIZED, OR ZOOM.
}DISPLAYLAYERS;



typedef struct DrawBitmapThreadInf
{
	// Members of this struct that once initialized persist.
	DISPLAYLAYERS *pDisplayLayers;
	THREAD_INF *threadInf;
	HBITMAP *phbitmapBathy;
	BATHYBITMAPINFO *pBitMapInfo;



	// Members that may change each time a draw is called.
	//INHABITBUFFERINF pInhab;
	DLGLAYOUT layout;
	BATHYBITMAP_WIN_PARAM pSeedInf; // don't think this is really needed.


	// Members that may change each time a draw is called.
	BOOL bResize;
	BATHYUSESTATE usageState;
	PLAYENUM playState;
}DRAWBITMAPTHREADINF;

typedef struct AnimateAnimatPopBitmapThreadInf
{
	CMutex *pThreadRunningMutex;
	THREAD_INF threadInf;
	PLAYENUM *playState;
	int stateIndex;
	DWORD *numStates; // the number of saved iterations
	int *playbackRate;

	//LinkedList <ANIMATSTATE_FILEOUT_PTR> *highLightPlaybackList;
}ANIMATEANIMATPOPBITMAPTHREAD;

DWORD WINAPI DisplayDataThread(LPVOID lpParameter);
DWORD WINAPI AnimateThread(LPVOID lpParameter);
#endif