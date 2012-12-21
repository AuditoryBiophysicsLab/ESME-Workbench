#include "3mbBitmapFunctions.h"
#include "AlphaBlendExample.h"
#include "dataTypes.h"


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

extern HWND g_hwndBathy;
extern HWND g_hwndDepthScale;
extern HWND g_hwndSlopeScale;
extern HWND g_hDlgSeed;
extern CMutex g_mutexDrawBitmap;
extern CListManager <ANIMATSTATE_FILEOUT_PTR> g_playBackHighLightList;
extern INHABITBUFFERINF g_inhabBuffInf;
extern C3mbStaticsLib staticLib;



int GetValidBitmapWidth(int TargetWidth)
{
	while(TargetWidth%16 != 0 && TargetWidth >= 0)
		TargetWidth--;
	if(TargetWidth == 0)
		TargetWidth = 16;
	return TargetWidth;
}


// Local Functions
BATHY_BITMAP_INF AdjustBitmapInfForDisplay(const BATHY_BITMAP_INF *BitMapInf, double refToZeroValue);
BATHYEXTREMES AdjustBitmapInfForDisplay(const BATHYEXTREMES *BathyExtremes, double refToZeroValue);

void SlopeDataToBuffer(RECT Rect, BATHYBITMAP_WIN_PARAM  *SeedInf, void *pvData);


BATHYEXTREMES AdjustBitmapInfForDisplay(const BATHYEXTREMES *BathyExtremes, double refToZeroValue)
{
	BATHYEXTREMES bathyExtremesCpy;

	memcpy(&bathyExtremesCpy, BathyExtremes, sizeof(BATHYEXTREMES));

	//----------------------------------------------------------------------------------//
	// Reference the input data to zero so all values are positive
	//------------------------------------------------------------//
	// Data
	bathyExtremesCpy.depthMin += refToZeroValue;
	bathyExtremesCpy.depthMax += refToZeroValue;
	_ASSERT(bathyExtremesCpy.depthMin == 0 && bathyExtremesCpy.depthMax >= 0);
	return bathyExtremesCpy;
}


// Returns reference to zero in case needed by calling app.
BATHY_BITMAP_INF AdjustBitmapInfForDisplay(const BATHY_BITMAP_INF *BitMapInf, double refToZeroValue)
{
	BATHY_BITMAP_INF bitmapInfCpy;

	memcpy(&bitmapInfCpy, BitMapInf, sizeof(BATHY_BITMAP_INF));

	// Assert proper entries
	_ASSERT(bitmapInfCpy.dataExtremes.depthMin <= bitmapInfCpy.dataExtremes.depthMax);
	_ASSERT(bitmapInfCpy.displayExtremes.depthMin >= bitmapInfCpy.dataExtremes.depthMin);
	_ASSERT(bitmapInfCpy.displayExtremes.depthMax <= bitmapInfCpy.displayExtremes.depthMax);

	//----------------------------------------------------------------------------------//
	// Reference the input data to zero so all values are positive
	//------------------------------------------------------------//
	// Data
	bitmapInfCpy.dataExtremes.depthMin += refToZeroValue;
	bitmapInfCpy.dataExtremes.depthMax += refToZeroValue;
	_ASSERT(bitmapInfCpy.dataExtremes.depthMin == 0 && bitmapInfCpy.dataExtremes.depthMax >= 0);

	// Display data
	bitmapInfCpy.displayExtremes.depthMin += refToZeroValue;
	bitmapInfCpy.displayExtremes.depthMax += refToZeroValue;
	_ASSERT(bitmapInfCpy.displayExtremes.depthMin >= 0 && bitmapInfCpy.displayExtremes.depthMax >= 0);

	return bitmapInfCpy;
}



void BmpInitInfo(LPBITMAPINFO bmpinfo, LONG width, LONG height)
{
	bmpinfo->bmiHeader.biSize			= sizeof(BITMAPINFOHEADER);
	bmpinfo->bmiHeader.biWidth			= width;
	bmpinfo->bmiHeader.biHeight			= height;
	bmpinfo->bmiHeader.biPlanes			= 1;
	bmpinfo->bmiHeader.biBitCount		= 8;
	bmpinfo->bmiHeader.biCompression	= BI_RGB;
	bmpinfo->bmiHeader.biXPelsPerMeter	= 0;
	bmpinfo->bmiHeader.biYPelsPerMeter	= 0;
	bmpinfo->bmiHeader.biSizeImage		= 0;
	bmpinfo->bmiHeader.biClrUsed		= 0;
	bmpinfo->bmiHeader.biClrImportant	= 0;
}


BOOL SetSlopeBitmapPallet(LPBITMAPINFO bmpinfo)
{
	unsigned int cnt;
	//int val;
	double numColors = (double)(SLOPE_BITMAP_NUMCOLORS_255);  // 1 color of the 256 is used for black
	//double rGrad, gGrad, bGrad; // Color gradient / resolution.  A factor determined by guessing.  Determines how rapidly the colors change from on
							// index entry to the next in the color map.

	// Initially set the bitmap pallet to a color not expected to be used that will stand
	// out so it is easier to verify all colors are properly initialized.
	for(cnt=0; cnt <numColors; cnt++)
	{
		bmpinfo->bmiColors[cnt].rgbRed	 = (BYTE)(50 + 205*((double)(cnt)/(numColors-1)));
		bmpinfo->bmiColors[cnt].rgbGreen = (BYTE)0;
		bmpinfo->bmiColors[cnt].rgbBlue	 = (BYTE)0;
	}


	// Black
	bmpinfo->bmiColors[SLOPE_BITMAP_BLACK_INDEX].rgbRed	 = 0;
	bmpinfo->bmiColors[SLOPE_BITMAP_BLACK_INDEX].rgbGreen = 0;
	bmpinfo->bmiColors[SLOPE_BITMAP_BLACK_INDEX].rgbBlue = 0;
	return TRUE;
}

BOOL SetBitmapPallet(LPBITMAPINFO bmpinfo)
{
	unsigned int cnt;
	int val;

	BYTE seaColor[10][3] = 
	{
		{105, 139, 105}, //darkseagreen 4
		{155, 205, 155}, // darkseagreen 3
		{180, 238, 180}, // darkseagreen 2
		{193, 255, 193}, // darkseagreen 1
		{60, 179, 113}, //mediumseagreen
		{46, 139, 87}, //seagreen 4 (seagreen)
		{67, 205, 128}, //seagreen 3
		{78, 238, 148}, //seagreen 2
		{84, 255, 159}, //seagreen 1
		{0, 199, 140}//turquoiseblue
	};

	// Scale the blues up
#if 1
	for(cnt=0; cnt< 10; cnt++)
	{
		val = (int)((double)seaColor[cnt][2] * 1.5);
		if(val <= 255)
			seaColor[cnt][2] = (BYTE)val;
		else 
			seaColor[cnt][2] = (BYTE)255;
	}
#endif

	/*
	http://cloford.com/resources/colours/500col.htm
	darkseagreen 4  		105  	139  	105
	darkseagreen 3 			155 	205 	155
	darkseagreen 2 			180 	238 	180
	darkseagreen 1 			193 	255 	193

	mediumseagreen			60  	179  	113

	seagreen 4 (seagreen)	46  	139  	87
	seagreen 3				67  	205  	128
	seagreen 2				78  	238  	148
	seagreen 1				84  	255  	159

	lightseagreen  			32  	178  	170
	turquoiseblue  			0  	199  	140
	*/

/*
	bmpinfo->bmiHeader.biSize			= sizeof(BITMAPINFOHEADER);
	bmpinfo->bmiHeader.biWidth			= width;
	bmpinfo->bmiHeader.biHeight			= height;
	bmpinfo->bmiHeader.biPlanes			= 1;
	bmpinfo->bmiHeader.biBitCount		= 8;
	bmpinfo->bmiHeader.biCompression	= BI_RGB;
	bmpinfo->bmiHeader.biXPelsPerMeter	= 0;
	bmpinfo->bmiHeader.biYPelsPerMeter	= 0;
	bmpinfo->bmiHeader.biSizeImage		= 0;
	bmpinfo->bmiHeader.biClrUsed		= 0;
	bmpinfo->bmiHeader.biClrImportant	= 0;
*/
	double numColors = (double)(NUMCOLORS_256 - 3);  // 3 of the colors available are used for things other than the bathymapping.
	double rGrad, gGrad, bGrad; // Color gradient / resolution.  A factor determined by guessing.  Determines how rapidly the colors change from on
							// index entry to the next in the color map.

	double lim1, lim2, lim3, lim4, lim5, lim6, lim7, lim8, lim9;

	//NumColors = 128;
	lim1 = (numColors*2.0 /18.0); // 254/8   =  31, (  0 -  30), 31 span
	lim2 = (numColors*4.0 /18.0); // 254*3/8 =  95, ( 31 -  94), 64 range
	lim3 = (numColors*6.0 /18.0); //         = 158, ( 95 - 157), 63 range
	lim4 = (numColors*8.0 /18.0); //         = 222, (158 - 221), 64 range
	lim5 = (numColors*10.0/18.0);           //         = 254, (222 - 253), 32 range	lim1 = (numColors*1.0/8.0); // 254/8   =  31, (  0 -  30), 31 span
	lim6 = (numColors*12.0/18.0); //
	lim7 = (numColors*14.0/18.0); //
	lim8 = (numColors*16.0/18.0); //
	lim9 = (numColors*18.0/18.0); // 

	// Initially set the bitmap pallet to a color not expected to be used that will stand
	// out so it is easier to verify all colors are properly initialized.
	for(cnt=0; cnt<NUMCOLORS_256; cnt++)
	{
		bmpinfo->bmiColors[cnt].rgbRed	 = (BYTE)220;
		bmpinfo->bmiColors[cnt].rgbGreen = (BYTE)20;
		bmpinfo->bmiColors[cnt].rgbBlue	 = (BYTE)60;
	}


	// Initialize the pallette
	for(cnt=0; cnt<(UINT)numColors; cnt++)
	{
		if(cnt <= (UINT)floor(lim1)) // 256:31, 192:23  128:15
		{
			// Deepest regions.
			rGrad = (seaColor[1][0] - seaColor[0][0])/(lim1 - 0);
			gGrad = (seaColor[1][1] - seaColor[0][1])/(lim1 - 0);
			bGrad = (seaColor[1][2] - seaColor[0][2])/(lim1 - 0);

			// bw:10
			bmpinfo->bmiColors[cnt].rgbRed	 = (BYTE)floor(seaColor[0][0] + rGrad*(cnt - 0));
			bmpinfo->bmiColors[cnt].rgbGreen = (BYTE)floor(seaColor[0][1] + gGrad*(cnt - 0));
			bmpinfo->bmiColors[cnt].rgbBlue	 = (BYTE)floor(seaColor[0][2] + bGrad*(cnt - 0));
		}
		else if(cnt <= (UINT)floor(lim2)) //256:95, 192:70, 128:47
		{
			rGrad = (seaColor[2][0] - seaColor[1][0])/(lim2 - lim1);
			gGrad = (seaColor[2][1] - seaColor[1][1])/(lim2 - lim1);
			bGrad = (seaColor[2][2] - seaColor[1][2])/(lim2 - lim1);

			// bw:40, no purple
			bmpinfo->bmiColors[cnt].rgbRed	 = (BYTE)floor(seaColor[1][0] + rGrad*(cnt - lim1));
			bmpinfo->bmiColors[cnt].rgbGreen = (BYTE)floor(seaColor[1][1] + gGrad*(cnt - lim1));
			bmpinfo->bmiColors[cnt].rgbBlue	 = (BYTE)floor(seaColor[1][2] + bGrad*(cnt - lim1));
		}
		else if(cnt <= (UINT)floor(lim3)) //256:159, 192:119, 128:79
		{

			rGrad = (seaColor[3][0] - seaColor[2][0])/(lim3 - lim2);
			gGrad = (seaColor[3][1] - seaColor[2][1])/(lim3 - lim2);
			bGrad = (seaColor[3][2] - seaColor[2][2])/(lim3 - lim2);

			// bw:70, no purple
			bmpinfo->bmiColors[cnt].rgbRed	 = (BYTE)floor(seaColor[2][0] + rGrad*(cnt - lim2));
			bmpinfo->bmiColors[cnt].rgbGreen = (BYTE)floor(seaColor[2][1] + gGrad*(cnt - lim2));
			bmpinfo->bmiColors[cnt].rgbBlue	 = (BYTE)floor(seaColor[2][2] + bGrad*(cnt - lim2));

		}
		else if(cnt <= (UINT)floor(lim4)) //256:223, 192:167, 128:111
		{

			rGrad = (seaColor[4][0] - seaColor[3][0])/(lim4 - lim3);
			gGrad = (seaColor[4][1] - seaColor[3][1])/(lim4 - lim3);
			bGrad = (seaColor[4][2] - seaColor[3][2])/(lim4 - lim3);

			// BW: 100 
			bmpinfo->bmiColors[cnt].rgbRed	 = (BYTE)floor(seaColor[3][0] + rGrad*(cnt - lim3));
			bmpinfo->bmiColors[cnt].rgbGreen = (BYTE)floor(seaColor[3][1] + gGrad*(cnt - lim3));
			bmpinfo->bmiColors[cnt].rgbBlue	 = (BYTE)floor(seaColor[3][2] + bGrad*(cnt - lim3));
		}
		else if(cnt <= (UINT)floor(lim5))
		{
			rGrad = (seaColor[5][0] - seaColor[4][0])/(lim5 - lim4);
			gGrad = (seaColor[5][1] - seaColor[4][1])/(lim5 - lim4);
			bGrad = (seaColor[5][2] - seaColor[4][2])/(lim5 - lim4);

			// bw:130, no purple
			bmpinfo->bmiColors[cnt].rgbRed	 = (BYTE)floor(seaColor[4][0] + rGrad*(cnt - lim4));
			bmpinfo->bmiColors[cnt].rgbGreen = (BYTE)floor(seaColor[4][1] + gGrad*(cnt - lim4));
			bmpinfo->bmiColors[cnt].rgbBlue	 = (BYTE)floor(seaColor[4][2] + bGrad*(cnt - lim4));
		}
		else if(cnt <= (UINT)floor(lim6))
		{
			rGrad = (seaColor[6][0] - seaColor[5][0])/(lim6 - lim5);
			gGrad = (seaColor[6][1] - seaColor[5][1])/(lim6 - lim5);
			bGrad = (seaColor[6][2] - seaColor[5][2])/(lim6 - lim5);

			// bw:160, purple
			bmpinfo->bmiColors[cnt].rgbRed	 = (BYTE)floor(seaColor[5][0] + rGrad*(cnt - lim5));
			bmpinfo->bmiColors[cnt].rgbGreen = (BYTE)floor(seaColor[5][1] + gGrad*(cnt - lim5));
			bmpinfo->bmiColors[cnt].rgbBlue	 = (BYTE)floor(seaColor[5][2] + bGrad*(cnt - lim5));

		}
		else if(cnt <= (UINT)floor(lim7))
		{
			rGrad = (seaColor[7][0] - seaColor[6][0])/(lim7 - lim6);
			gGrad = (seaColor[7][1] - seaColor[6][1])/(lim7 - lim6);
			bGrad = (seaColor[7][2] - seaColor[6][2])/(lim7 - lim6);

			// bw:190, no purple
			bmpinfo->bmiColors[cnt].rgbRed	 = (BYTE)floor(seaColor[6][0] + rGrad*(cnt - lim6));
			bmpinfo->bmiColors[cnt].rgbGreen = (BYTE)floor(seaColor[6][1] + gGrad*(cnt - lim6));
			bmpinfo->bmiColors[cnt].rgbBlue	 = (BYTE)floor(seaColor[6][2] + bGrad*(cnt - lim6));

		}
		else if(cnt <= (UINT)floor(lim8))
		{
			rGrad = (seaColor[8][0] - seaColor[7][0])/(lim8 - lim7);
			gGrad = (seaColor[8][1] - seaColor[7][1])/(lim8 - lim7);
			bGrad = (seaColor[8][2] - seaColor[7][2])/(lim8 - lim7);

			// bw:190, no purple
			bmpinfo->bmiColors[cnt].rgbRed	 = (BYTE)floor(seaColor[7][0] + rGrad*(cnt - lim7));
			bmpinfo->bmiColors[cnt].rgbGreen = (BYTE)floor(seaColor[7][1] + gGrad*(cnt - lim7));
			bmpinfo->bmiColors[cnt].rgbBlue	 = (BYTE)floor(seaColor[7][2] + bGrad*(cnt - lim7));

		}
		else
		{
			// Most shallow regions
			rGrad = (seaColor[9][0] - seaColor[8][0])/(lim9 - lim8);
			gGrad = (seaColor[9][1] - seaColor[8][1])/(lim9 - lim8);
			bGrad = (seaColor[9][2] - seaColor[8][2])/(lim9 - lim8);

			// bw:250, no purple
			bmpinfo->bmiColors[cnt].rgbRed	 = (BYTE)floor(seaColor[8][0] + rGrad*(cnt - lim8));
			bmpinfo->bmiColors[cnt].rgbGreen = (BYTE)floor(seaColor[8][1] + gGrad*(cnt - lim8));
			bmpinfo->bmiColors[cnt].rgbBlue	 = (BYTE)floor(seaColor[8][2] + bGrad*(cnt - lim8));
		}
	}

	// Grey DATA_PT_INDEX
	bmpinfo->bmiColors[DATA_PT_INDEX].rgbRed	= 1;
	bmpinfo->bmiColors[DATA_PT_INDEX].rgbGreen  = 1;
	bmpinfo->bmiColors[DATA_PT_INDEX].rgbBlue	= 1;


	// Red (for now, for actoustic animat)
	// Normal animat dot color).
	// Index 252, Acoustic Animat
	bmpinfo->bmiColors[ACOUSTICANIMATINDEX].rgbRed	 = 255;
	bmpinfo->bmiColors[ACOUSTICANIMATINDEX].rgbGreen = 0;
	bmpinfo->bmiColors[ACOUSTICANIMATINDEX].rgbBlue	 = 0;


	// Black, Normal animat dot color.
	bmpinfo->bmiColors[ANIMAT_COLOR_INDEX].rgbRed	 = 0;
	bmpinfo->bmiColors[ANIMAT_COLOR_INDEX].rgbGreen = 0;
	bmpinfo->bmiColors[ANIMAT_COLOR_INDEX].rgbBlue	 = 0;

	// (Animat Emphasis).
	bmpinfo->bmiColors[ANIMAT_HIGHLIGHT_COLOR_INDEX].rgbRed	 = 255;
	bmpinfo->bmiColors[ANIMAT_HIGHLIGHT_COLOR_INDEX].rgbGreen = 255;; 
	bmpinfo->bmiColors[ANIMAT_HIGHLIGHT_COLOR_INDEX].rgbBlue	 = 18;

	// Land (color: olive, http://cloford.com/resources/colours/500col.htm)
	bmpinfo->bmiColors[LAND_COLOR_INDEX].rgbRed	 = 128;//184;
	bmpinfo->bmiColors[LAND_COLOR_INDEX].rgbGreen = 128;//134;
	bmpinfo->bmiColors[LAND_COLOR_INDEX].rgbBlue	 = 0; //11;

	return TRUE;
}


typedef struct ColorRefBuffer
{
	COLORREF *buff;
	int len;
}COLORREFBUFFER;

COLORREFBUFFER gCrX1;
COLORREFBUFFER gCrX2;
COLORREFBUFFER gCrY1;
COLORREFBUFFER gCrY2;

#if 0
void MovingHorizontalLine(HWND hWnd,  BATHYBITMAP_WIN_PARAM *bathyGdata, LPBITMAPINFO bmpinfo, BYTE *s_bathyData, COORD MouseDwn, COORD MousePos, BOOL Erase)
{
	static	COORD	 prevMousePos  = {0};
			HDC		 hdcWnd;
			short	 x0, xf, x, y0, yf, y, i;
			int		 index1, index2, maxIndex;;
			RECT	 rect;
			UINT32 r,g,b;

	GetClientRect(hWnd, &rect);
	maxIndex = rect.bottom * rect.right - 1;
	hdcWnd	= GetDC(hWnd);


	// If there is already a line drawn, erase it.
	if(memcmp(&prevMouseDwn, &prevMousePos, sizeof(COORD)) != 0)
	{
		_ASSERT(gCrX1.len > 0 && gCrX1.buff != NULL);

		// Get the x and y starting and ending pixels.
		x0 = 0;
		xf = rect.right-1;

		y0 = prevMouseDwn.Y;
		yf = prevMousePos.Y;
		if(y0 > yf)
		{
			y0 = yf;
			yf = prevMouseDwn.Y;
		}


		//--------------------------------------------------------------------------//
		// Redraw the pixels to their proper color
		//----------------------------------------//
		// Left to right.
		index1 = rect.right * (rect.bottom - 1 - y0 - 1) + x0;

		for(x=x0, i=0; x<=xf; x++, i++)
		{
			// Top side of the box
			if(index1 >= 0 && index1 <= maxIndex)
				SetPixel(hdcWnd, x, y0, gCrX1.buff[i]);
			index1++;
		}


		// Clear memory of previous lines.
		memset(&prevMouseDwn, 0, sizeof(COORD));
		memset(&prevMousePos, 0, sizeof(COORD));
		delete [] gCrX1.buff;
		memset(&gCrX1, 0, sizeof(COLORREFBUFFER));
	}
	else
	{
		_ASSERT(gCrX1.len == 0 && gCrX1.buff == NULL);
	}


	// Start a new bounding box or clear existing one (WM_LBUTTONDOWN)
	if(memcmp(&MouseDwn, &MousePos, sizeof(COORD)) == 0)
	{
		;
	}
	else
	{
		// Draw the new box.
		// Get the x and y starting and ending pixels.
		x0 = MouseDwn.X;
		xf = MousePos.X;
		if(x0 > xf)
		{
			x0 = xf;
			//xf = prevMouseDwn.X;
			xf = MouseDwn.X;
		}

		y0 = MouseDwn.Y;
		yf = MousePos.Y;
		if(y0 > yf)
		{
			y0 = yf;
			//yf = prevMouseDwn.Y;
			yf = MouseDwn.Y;
		}

		// Copy the pixels so they may be redrawn when needed.
		gCrX1.len = gCrX2.len = abs(xf - x0) + 1;
		gCrY1.len = gCrY2.len = abs(yf - y0) + 1;

		gCrX1.buff = new COLORREF[gCrX1.len];
		gCrX2.buff = new COLORREF[gCrX2.len];
		gCrY1.buff = new COLORREF[gCrY1.len];
		gCrY2.buff = new COLORREF[gCrY2.len];

		index1 = rect.right * (rect.bottom - 1 - y0 - 1) + x0;
		index2 = rect.right * (rect.bottom - 1 - yf - 1) + x0;

#if 0
	{
		HDC hdcRR = GetDC(hWnd);
		DrawAlphaBlend(hWnd, hdcRR);
		ReleaseDC(hWnd,hdcRR);
	}
#endif

		for(x=x0, i=0; x<=xf; x++, i++)
		{
			// Top side of the box
			if(index1 >= 0 && index1 <= maxIndex)
			{
				gCrX1.buff[i] = GetPixel(hdcWnd, x, y0);
				r = GetRValue(gCrX1.buff[i]);
				g = GetGValue(gCrX1.buff[i]);
				b = GetBValue(gCrX1.buff[i]);
				if(r !=255 || g != 255 || b != 255)
					r = r;
			}

			// Bottom side of the box
			if(index2 >= 0 && index2 <= maxIndex)
			{
				gCrX2.buff[i] = GetPixel(hdcWnd, x, yf);
				r = GetRValue(gCrX2.buff[i]);
				g = GetGValue(gCrX2.buff[i]);
				b = GetBValue(gCrX2.buff[i]);
				if(r !=255 || g != 255 || b != 255)
					r = r;
			}

			index1++;
			index2++;
		}

		// down to top.
		for(y=y0, i=0; y<yf; y++, i++)
		{
			// Left side of box
			index1 = rect.right * (rect.bottom - 1 - y - 1) + x0;
			if(index1 >= 0 && index1 <= maxIndex)
			{
				gCrY1.buff[i] = GetPixel(hdcWnd, x0, y);
			}

			// Right side of box
			index2 = rect.right * (rect.bottom - 1 - y) + xf;
			if(index2 >= 0 && index2 <= maxIndex)
			{
				gCrY2.buff[i] = GetPixel(hdcWnd, xf, y);
			}
		}

		MoveToEx(hdcWnd, MouseDwn.X, MouseDwn.Y, NULL);
		LineTo(hdcWnd,   MouseDwn.X, MousePos.Y);
		LineTo(hdcWnd,   MousePos.X, MousePos.Y);
		LineTo(hdcWnd,   MousePos.X, MouseDwn.Y);
		LineTo(hdcWnd,   MouseDwn.X, MouseDwn.Y);
		memcpy(&prevMouseDwn, &MouseDwn, sizeof(COORD));
		memcpy(&prevMousePos, &MousePos, sizeof(COORD));
	}

	ReleaseDC(hWnd, hdcWnd);


	if(Erase == TRUE)
	{
		delete [] gCrX1.buff;
		delete [] gCrX2.buff;
		delete [] gCrY1.buff;
		delete [] gCrY2.buff;
		memset(&gCrX1, 0, sizeof(COLORREFBUFFER));
		memset(&gCrX2, 0, sizeof(COLORREFBUFFER));
		memset(&gCrY1, 0, sizeof(COLORREFBUFFER));
		memset(&gCrY2, 0, sizeof(COLORREFBUFFER));

		memset(&prevMouseDwn, 0, sizeof(COORD));
		memset(&prevMousePos, 0, sizeof(COORD));
	}
	return;
}
#endif

//void BoundingBox(HWND hWnd, BATHYBITMAP_WIN_PARAM *bathyGdata, LPBITMAPINFO bmpinfo, BYTE *s_bathyData, COORD MouseDwn, COORD MousePos, BOOL Erase)
void BoundingBox(HWND hWnd, COORD MouseDwn, COORD MousePos, BOOL Erase)
{
	static	COORD	 prevMouseDwn = {0};
	static	COORD	 prevMousePos  = {0};
			HDC		 hdcWnd;
			short	 x0, xf, x, y0, yf, y, i;
			int		 index1, index2, maxIndex;;
			RECT	 rect;
			UINT32 r,g,b;

	GetClientRect(hWnd, &rect);
	maxIndex = rect.bottom * rect.right - 1;
	hdcWnd	= GetDC(hWnd);


	// If there is already a box drawn, erase it.
	if(memcmp(&prevMouseDwn, &prevMousePos, sizeof(COORD)) != 0)
	{
		_ASSERT(gCrX1.len > 0 && gCrX1.buff != NULL);
		_ASSERT(gCrX2.len > 0 && gCrX2.buff != NULL);
		_ASSERT(gCrY1.len > 0 && gCrY1.buff != NULL);
		_ASSERT(gCrY2.len > 0 && gCrY2.buff != NULL);

		// Get the x and y starting and ending pixels.
		x0 = prevMouseDwn.X;
		xf = prevMousePos.X;
		if(x0 > xf)
		{
			x0 = xf;
			xf = prevMouseDwn.X;
		}

		y0 = prevMouseDwn.Y;
		yf = prevMousePos.Y;
		if(y0 > yf)
		{
			y0 = yf;
			yf = prevMouseDwn.Y;
		}


		//--------------------------------------------------------------------------//
		// Redraw the pixels to their proper color
		//----------------------------------------//
		// Left to right.
		index1 = rect.right * (rect.bottom - 1 - y0 - 1) + x0;
		index2 = rect.right * (rect.bottom - 1 - yf - 1) + x0;

		for(x=x0, i=0; x<=xf; x++, i++)
		{
			// Top side of the box
			if(index1 >= 0 && index1 <= maxIndex)
				SetPixel(hdcWnd, x, y0, gCrX1.buff[i]);

			// Bottom side of the box
			if(index2 >= 0 && index2 <= maxIndex)
				SetPixel(hdcWnd, x, yf, gCrX2.buff[i]);

			index1++;
			index2++;
		}

		// down to top.
		for(y=y0, i=0; y<yf; y++, i++)
		{
			// Left side of box
			index1 = rect.right * (rect.bottom - 1 - y - 1) + x0;
			if(index1 >= 0 && index1 <= maxIndex)
				SetPixel(hdcWnd, x0, y, gCrY1.buff[i]);

			// Right side of box
			index2 = rect.right * (rect.bottom - 1 - y) + xf;
			if(index2 >= 0 && index2 <= maxIndex)
				SetPixel(hdcWnd, xf, y, gCrY2.buff[i]);
		}

		// Clear memory of previous lines.
		memset(&prevMouseDwn, 0, sizeof(COORD));
		memset(&prevMousePos, 0, sizeof(COORD));
		delete [] gCrX1.buff;
		delete [] gCrX2.buff;
		delete [] gCrY1.buff;
		delete [] gCrY2.buff;
		memset(&gCrX1, 0, sizeof(COLORREFBUFFER));
		memset(&gCrX2, 0, sizeof(COLORREFBUFFER));
		memset(&gCrY1, 0, sizeof(COLORREFBUFFER));
		memset(&gCrY2, 0, sizeof(COLORREFBUFFER));
	}
	else
	{
		_ASSERT(gCrX1.len == 0 && gCrX1.buff == NULL);
		_ASSERT(gCrX2.len == 0 && gCrX2.buff == NULL);
		_ASSERT(gCrY1.len == 0 && gCrY1.buff == NULL);
		_ASSERT(gCrY2.len == 0 && gCrY2.buff == NULL);
	}


	// Start a new bounding box or clear existing one (WM_LBUTTONDOWN)
	if(memcmp(&MouseDwn, &MousePos, sizeof(COORD)) == 0)
	{
		;
	}
	else
	{
		// Draw the new box.
		// Get the x and y starting and ending pixels.
		x0 = MouseDwn.X;
		xf = MousePos.X;
		if(x0 > xf)
		{
			x0 = xf;
			//xf = prevMouseDwn.X;
			xf = MouseDwn.X;
		}

		y0 = MouseDwn.Y;
		yf = MousePos.Y;
		if(y0 > yf)
		{
			y0 = yf;
			//yf = prevMouseDwn.Y;
			yf = MouseDwn.Y;
		}

		// Copy the pixels so they may be redrawn when needed.
		gCrX1.len = gCrX2.len = abs(xf - x0) + 1;
		gCrY1.len = gCrY2.len = abs(yf - y0) + 1;

		gCrX1.buff = new COLORREF[gCrX1.len];
		gCrX2.buff = new COLORREF[gCrX2.len];
		gCrY1.buff = new COLORREF[gCrY1.len];
		gCrY2.buff = new COLORREF[gCrY2.len];

		index1 = rect.right * (rect.bottom - 1 - y0 - 1) + x0;
		index2 = rect.right * (rect.bottom - 1 - yf - 1) + x0;

#if 0
	{
		HDC hdcRR = GetDC(hWnd);
		DrawAlphaBlend(hWnd, hdcRR);
		ReleaseDC(hWnd,hdcRR);
	}
#endif

		for(x=x0, i=0; x<=xf; x++, i++)
		{
			// Top side of the box
			if(index1 >= 0 && index1 <= maxIndex)
			{
				gCrX1.buff[i] = GetPixel(hdcWnd, x, y0);
				r = GetRValue(gCrX1.buff[i]);
				g = GetGValue(gCrX1.buff[i]);
				b = GetBValue(gCrX1.buff[i]);
				if(r !=255 || g != 255 || b != 255)
					r = r;
			}

			// Bottom side of the box
			if(index2 >= 0 && index2 <= maxIndex)
			{
				gCrX2.buff[i] = GetPixel(hdcWnd, x, yf);
				r = GetRValue(gCrX2.buff[i]);
				g = GetGValue(gCrX2.buff[i]);
				b = GetBValue(gCrX2.buff[i]);
				if(r !=255 || g != 255 || b != 255)
					r = r;
			}

			index1++;
			index2++;
		}

		// down to top.
		for(y=y0, i=0; y<yf; y++, i++)
		{
			// Left side of box
			index1 = rect.right * (rect.bottom - 1 - y - 1) + x0;
			if(index1 >= 0 && index1 <= maxIndex)
			{
				gCrY1.buff[i] = GetPixel(hdcWnd, x0, y);
			}

			// Right side of box
			index2 = rect.right * (rect.bottom - 1 - y) + xf;
			if(index2 >= 0 && index2 <= maxIndex)
			{
				gCrY2.buff[i] = GetPixel(hdcWnd, xf, y);
			}
		}

		MoveToEx(hdcWnd, MouseDwn.X, MouseDwn.Y, NULL);
		LineTo(hdcWnd,   MouseDwn.X, MousePos.Y);
		LineTo(hdcWnd,   MousePos.X, MousePos.Y);
		LineTo(hdcWnd,   MousePos.X, MouseDwn.Y);
		LineTo(hdcWnd,   MouseDwn.X, MouseDwn.Y);
		memcpy(&prevMouseDwn, &MouseDwn, sizeof(COORD));
		memcpy(&prevMousePos, &MousePos, sizeof(COORD));
	}

	ReleaseDC(hWnd, hdcWnd);


	if(Erase == TRUE)
	{
		delete [] gCrX1.buff;
		delete [] gCrX2.buff;
		delete [] gCrY1.buff;
		delete [] gCrY2.buff;
		memset(&gCrX1, 0, sizeof(COLORREFBUFFER));
		memset(&gCrX2, 0, sizeof(COLORREFBUFFER));
		memset(&gCrY1, 0, sizeof(COLORREFBUFFER));
		memset(&gCrY2, 0, sizeof(COLORREFBUFFER));

		memset(&prevMouseDwn, 0, sizeof(COORD));
		memset(&prevMousePos, 0, sizeof(COORD));
	}
	return;
}



void AnimatsToBitmap(RECT Rect, VOID *pvData, BATHYBITMAP_WIN_PARAM *pBathyBmWinParam, int NumAnimats,
					 INHABITINF *pCoord, int NumHighlights, INHABITINF *pHighLightList)
{
	// Uses bitmap indices set aside for animat colors
	// See BmpInitInfo() for indexing color setup
	int	i, j, k;
	COORD c = {0};
	int index;
	//BYTE setValue = ANIMAT_COLOR_INDEX;
//int arrayLength = Rect.right * Rect.bottom;
	BOOL res;
	int highlightIndex = 0;

	// 1st stage Alpha blending vars.
    UCHAR ubAlpha;         // used for doing transparent gradient 
    UCHAR ubRed = 0x00, ubGreen = 0x00, ubBlue = 0x00;
    float fAlphaFactor;    // used to do premultiply

	ubAlpha = 255;
	fAlphaFactor = (float)ubAlpha / (float)0xff;

	// Black: ubRed = 0x00, ubGreen = 0x00, ubBlue = 0x00; (normal animat color)
	// Yellow: ubRed = 255, ubGreen = 255, ubBlue = 18; (highlighted animat color)
	// Red: ubRed = 255, ubGreen = 0x00, ubBlue = 0x00; (acoustic source color)

	// Default entire set of data to zero alpha.
	memset(pvData, 0, Rect.right * Rect.bottom * 4);

	for(i=0; i<NumAnimats; i++)
	{
		if(highlightIndex < NumHighlights && 0 == memcmp(&pCoord[i], &pHighLightList[highlightIndex], sizeof(INHABITINF))) // error out
		{
			highlightIndex++;
			continue;
		}

		// Set c 
		res = LatLonToScreenCoord(pCoord[i].coord.lat, pCoord[i].coord.lon, pBathyBmWinParam->bitmapInf.displayExtremes, Rect, &c);
		if(FALSE == res)
			continue;

		///////////////////////////////////////////////////////////////////////////
		// Set the pixels surrounding the animat.  Black if animat, red if sound source
		ubRed = 0, ubGreen = 0, ubBlue = 0; // Black
		if(CBitmapEnvFunctions::AnimatIsASoundSource(i))
		{
			ubRed = 255, ubGreen = 0x00, ubBlue = 0x00; // Red).
		}

		if(Rect.bottom - 1 - c.Y - 1 >= 0)
		{
			for(j=-2; j<=2; j++)
			{
				for(k=-2; k<=2; k++)
				{
					// Don't display corners... make it look round.
					if(j == -2 || j == 2)
					{
						if(k == -2 || k == 2)
							continue;
					}

					if(c.Y + j < 0 || c.Y + j > Rect.bottom || c.X + k < 0 || c.X + k >= Rect.right)
						continue;

					index = Rect.right * (Rect.bottom - 1 - c.Y + j) + c.X + k;
					if(index >= 0 && index < Rect.right * Rect.bottom)
					{
						//BitMapData[index] = setValue; // the spects that indicate animat locations.
				        ((UINT32 *)pvData)[index] 
							= (ubAlpha << 24) |                       //0xaa000000 
							 ((UCHAR)(ubRed * fAlphaFactor) << 16) |  //0x00rr0000 
							 ((UCHAR)(ubGreen * fAlphaFactor) << 8) | //0x0000gg00 
							 ((UCHAR)(ubBlue * fAlphaFactor));      //0x000000bb 
					}

				}
			}
		}

		// Set the center pixel animat color. Yellow if animat, black if sound source
		ubRed = 255, ubGreen = 255, ubBlue = 18; // yellow
		if(CBitmapEnvFunctions::AnimatIsASoundSource(i))
			ubRed = 0x00, ubGreen = 0x00, ubBlue = 0x00; //black.
		
		index = Rect.right * (Rect.bottom - 1 - c.Y) + c.X;

        ((UINT32 *)pvData)[index] 
            = (ubAlpha << 24) |                       //0xaa000000 
             ((UCHAR)(ubRed * fAlphaFactor) << 16) |  //0x00rr0000 
             ((UCHAR)(ubGreen * fAlphaFactor) << 8) | //0x0000gg00 
             ((UCHAR)(ubBlue * fAlphaFactor));      //0x000000bb 
	}


	//----------------------------------------------------------------------------------//
	// Draw highlighted animats
	//-------------------------//
	for(i=0; i<NumHighlights; i++)
	{
		res = LatLonToScreenCoord(pHighLightList[i].coord.lat, pHighLightList[i].coord.lon, pBathyBmWinParam->bitmapInf.displayExtremes, Rect, &c);
		if(FALSE == res)
			continue;

		///////////////////////////////////////////////////////////////////////////
		// Set the pixels surrounding the animat.  Yellow if animat, red if sound source
		ubRed = 255, ubGreen = 255, ubBlue = 18; // yellow
		if(pHighLightList[i].acstcSrc.isASoundSource == TRUE)
		{
			ubRed = 255, ubGreen = 0x00, ubBlue = 0x00; // Red).
		}

		if(Rect.bottom - 1 - c.Y - 1 >= 0)
		{
			for(j=-2; j<=2; j++)
			{
				for(k=-2; k<=2; k++)
				{
					// Don't display corners... make it look round.
					if(j == -2 || j == 2)
					{
						if(k == -2 || k == 2)
							continue;
					}

					if(c.Y + j < 0 || c.Y + j > Rect.bottom || c.X + k < 0 || c.X + k >= Rect.right)
						continue;

					index = Rect.right * (Rect.bottom - 1 - c.Y + j) + c.X + k;
					if(index >= 0 && index < Rect.right * Rect.bottom)
					{
						//BitMapData[index] = setValue; // the spects that indicate animat locations.
				        ((UINT32 *)pvData)[index] 
							= (ubAlpha << 24) |                       //0xaa000000 
							 ((UCHAR)(ubRed * fAlphaFactor) << 16) |  //0x00rr0000 
							 ((UCHAR)(ubGreen * fAlphaFactor) << 8) | //0x0000gg00 
							 ((UCHAR)(ubBlue * fAlphaFactor));      //0x000000bb 
					}
				}
			}
		}


		// Set the center pixel to black for both the sound source and the animats
		ubRed = 0x00, ubGreen = 0x00, ubBlue = 0x00; //black.

		index = Rect.right * (Rect.bottom - 1 - c.Y) + c.X;
		//BitMapData[index] = setValue;
        ((UINT32 *)pvData)[index] 
            = (ubAlpha << 24) |                       //0xaa000000 
             ((UCHAR)(ubRed * fAlphaFactor) << 16) |  //0x00rr0000 
             ((UCHAR)(ubGreen * fAlphaFactor) << 8) | //0x0000gg00 
             ((UCHAR)(ubBlue * fAlphaFactor));      //0x000000bb 
	}
}



// For populating animats.
void AnimatsToBitmapOLD(RECT WindowRect, BYTE *BitMapData, BATHYBITMAP_WIN_PARAM *pBathyBmWinParam,
							   int NumIterations, int NumAnimats, INHABITINF *pCoord, BOOL Highlight)
{
	// Uses bitmap indices set aside for animat colors
	// See BmpInitInfo() for indexing color setup
	int	h, i;
	short j, k;
	COORD c;
	int index;
	BYTE setValue = ANIMAT_COLOR_INDEX;
	int arrayLength = WindowRect.right * WindowRect.bottom;
	BOOL res;
	int buffIndex;
	CFileManagerStatic fileManagerStatic;

	h = 0;
	for(i=0; i<NumAnimats; i++)
	{
		for(h=0; h<NumIterations; h++)
		{
			setValue = ANIMAT_COLOR_INDEX;

			buffIndex = fileManagerStatic.BuffIndex(i, NumAnimats, h, NumIterations);


			if(pCoord[buffIndex].acstcSrc.outputValue > 0)
				setValue = ACOUSTICANIMATINDEX;
			else if(Highlight == TRUE)
				setValue = ANIMAT_HIGHLIGHT_COLOR_INDEX;

			res = LatLonToScreenCoord(
				pCoord[buffIndex].coord.lat,
				pCoord[buffIndex].coord.lon,
				pBathyBmWinParam->bitmapInf.displayExtremes,
				WindowRect,
				&c);
			if(FALSE == res)
				continue;

			// Emphasise the animat(s) to be highlighted by coloring an extra pixel nearby
			// below.
			if(WindowRect.bottom - 1 - c.Y - 1 >= 0)
			{
				for(j=-2; j<=2; j++)
				{
					for(k=-2; k<=2; k++)
					{
						// Don't display corners... make it look round.
						if(j == -2 || j == 2)
						{
							if(k == -2 || k == 2)
								continue;
						}

						if(c.Y + j < 0 || c.Y + j > WindowRect.bottom || c.X + k < 0 || c.X + k >= WindowRect.right)
							continue;


						index = WindowRect.right * (WindowRect.bottom - 1 - c.Y + j) + c.X + k;
						if(index >= 0 && index < arrayLength)
							BitMapData[index] = setValue; // the spects that indicate animat locations.
					}
				}
			}
			index = WindowRect.right * (WindowRect.bottom - 1 - c.Y) + c.X;
			if(Highlight == FALSE || pCoord[buffIndex].acstcSrc.outputValue > 0)
				BitMapData[index] = ANIMAT_HIGHLIGHT_COLOR_INDEX; // the spects that indicate animat locations.
			else
				BitMapData[index] = ANIMAT_COLOR_INDEX; // the spects that indicate animat locations.
		} // hre
	}
}

void GenerateHightlightList(int SpeciesIndex, int PodIndex, int PodMemberIndex, int IndividualIndex, CListManager <ANIMATSTATE_FILEOUT_PTR> *pList)
{
	int	pod, ani;
	ANIMATSTATE_FILEOUT_PTR *pAsfo = NULL;
	SIMPLEPOP *popRef = CBitmapEnvFunctions::GetPopRef(); // returns a reference to a linked list.  This need to be imporeved.

	_ASSERT(pList != NULL);
	pList->Lock();
	pList->DeleteAll();

	if(SpeciesIndex == -1)
	{
		pList->Unlock();
		return;
	}


	// Species 'SpeciesIndex' is selected to be highlighted.
	if(IndividualIndex > -1)
	{
		pAsfo = pList->Add();
		pAsfo->p = popRef->s.Get(SpeciesIndex)->i.Get(IndividualIndex)->p;
		pList->Unlock();
		return;
	}
	else if(PodIndex > -1)
	{
		// A entire pod of a species or a specfic pod member of a pod of the species is to be highlighted.
		if(PodMemberIndex != -1)
		{
			// A specfic pod member of a pod of the species is to be highlighted.
			pAsfo = pList->Add();
			pAsfo->p = popRef->s.Get(SpeciesIndex)->p.Get(PodIndex)->a.Get(PodMemberIndex)->p;
			pList->Unlock();
			return;
		}
		else
		{
			// The entire pod of the specified species is to be highlighted.
			for(ani=0; ani<popRef->s.Get(SpeciesIndex)->p.Get(PodIndex)->a.Length(); ani++)
			{
				pAsfo = pList->Add();
				pAsfo->p = popRef->s.Get(SpeciesIndex)->p.Get(PodIndex)->a.Get(ani)->p;
			}
			pList->Unlock();
			return;
		}
	}
	else
	{
		// The entire species is to be highlighted.
		// Start with the pod
		for(pod = 0; pod < popRef->s.Get(SpeciesIndex)->p.Length(); pod++)
		{
			for(ani=0; ani<popRef->s.Get(SpeciesIndex)->p.Get(pod)->a.Length(); ani++)
			{
				pAsfo = pList->Add();
				pAsfo->p = popRef->s.Get(SpeciesIndex)->p.Get(pod)->a.Get(ani)->p;
			}
		}

		// Next do the individuals.
		for(ani=0; ani<popRef->s.Get(SpeciesIndex)->i.Length(); ani++)
		{
			pAsfo = pList->Add();
			pAsfo->p = popRef->s.Get(SpeciesIndex)->i.Get(ani)->p;
		}
	}
	pList->Unlock();
	return;
}



void AnimatsToDataBuffer(RECT Rect, VOID *pvData, BATHYBITMAP_WIN_PARAM *pBathyBmWinParam,
							   int NumIterations, int NumAnimats, ANIMATSTATE_FILEOUT *pCoord,
							   CListManager <ANIMATSTATE_FILEOUT_PTR> *pHighLightList)
{
	// Uses bitmap indices set aside for animat colors
	// See BmpInitInfo() for indexing color setup
	int	h, i;
	COORD c;
	int colorIndex;
	//BYTE color = ANIMAT_COLOR_INDEX;
//int arrayLength = Rect.right * Rect.bottom;
	BOOL res;
	int index;
	int highlightIndex = 0;
	CFileManagerStatic fileManagerStatic;
	ANIMATSTATE_FILEOUT_PTR *pAsfo;

	// 1st stage Alpha blending vars.
    UCHAR ubAlpha;         // used for doing transparent gradient 
    UCHAR ubRed = 0x00, ubGreen = 0x00, ubBlue = 0x00;
    float fAlphaFactor;    // used to do premultiply

	ubAlpha = 255;
	fAlphaFactor = (float)ubAlpha / (float)0xff;

	pHighLightList->Lock();

	// Default entire set of data to zero alpha.
	memset(pvData, 0, Rect.right * Rect.bottom * 4);


	for(i=0; i<NumAnimats; i++)
	{
		_ASSERT((int)pCoord[i].animatID == i);

		if(pHighLightList->Length() > 0 && !CBitmapEnvFunctions::AnimatIsASoundSource(i))
		{
			if(highlightIndex < pHighLightList->Length() && pHighLightList->Get(highlightIndex)->p->animatID == (UINT32)i)
			{
				highlightIndex++;
				// Skip this loop.  This is a highlighted animat and will be highlighted
				// after the rest of the animats are drawn.
				continue;
			}
		}

		// Set the animat color.
		ubRed = 0x00, ubGreen = 0x00, ubBlue = 0x00;
		if(CBitmapEnvFunctions::AnimatIsASoundSource(pCoord[i].animatID))
			ubRed = 255, ubGreen = 0x00, ubBlue = 0x00; //color = ACOUSTICANIMATINDEX;

		for(h=0; h<NumIterations; h++)
		{
			index = fileManagerStatic.BuffIndex(i, NumAnimats, h, NumIterations);

			res = LatLonToScreenCoord(pCoord[index].lat, pCoord[index].lon, pBathyBmWinParam->bitmapInf.displayExtremes, Rect, &c);
			if(FALSE == res)
				continue;
			
			colorIndex = Rect.right * (Rect.bottom - 1 - c.Y) + c.X;
			//BitMapData[colorIndex] = color;

            ((UINT32 *)pvData)[colorIndex] 
                = (ubAlpha << 24) |                       //0xaa000000 
                 ((UCHAR)(ubRed * fAlphaFactor) << 16) |  //0x00rr0000 
                 ((UCHAR)(ubGreen * fAlphaFactor) << 8) | //0x0000gg00 
                 ((UCHAR)(ubBlue * fAlphaFactor));      //0x000000bb 

		}
	}


	// Draw the tracks of the specific animats to be highlighted.
	//color = ANIMAT_HIGHLIGHT_COLOR_INDEX;
	ubRed = 255, ubGreen = 255, ubBlue = 18;
	for(i=0; i<pHighLightList->Length(); i++)
	{
		pAsfo = pHighLightList->Get(i);
		if(CBitmapEnvFunctions::AnimatIsASoundSource(pAsfo->p->animatID) == TRUE)
			continue;

		for(h=0; h<NumIterations; h++)
		{
			index = fileManagerStatic.BuffIndex(pAsfo->p->animatID, NumAnimats, h, NumIterations);
			res = LatLonToScreenCoord(pCoord[index].lat, pCoord[index].lon, pBathyBmWinParam->bitmapInf.displayExtremes, Rect, &c);
			if(FALSE == res)
				continue;
			
			colorIndex = Rect.right * (Rect.bottom - 1 - c.Y) + c.X;
			//BitMapData[colorIndex] = color;
            ((UINT32 *)pvData)[colorIndex] 
                = (ubAlpha << 24) |                       //0xaa000000 
                 ((UCHAR)(ubRed * fAlphaFactor) << 16) |  //0x00rr0000 
                 ((UCHAR)(ubGreen * fAlphaFactor) << 8) | //0x0000gg00 
                 ((UCHAR)(ubBlue * fAlphaFactor));      //0x000000bb 

		}		
	}
	pHighLightList->Unlock();
}

void AnimatsToBitmapOld(RECT WinRect, BYTE *BitMapData, BATHYBITMAP_WIN_PARAM *pBathyBmWinParam,
							   int NumIterations, int NumAnimats, ANIMATSTATE_FILEOUT *pCoord, LinkedList <ANIMATSTATE_FILEOUT_PTR> *pHighLightList)
{
	// Uses bitmap indices set aside for animat colors
	// See BmpInitInfo() for indexing color setup
	int	h, i;
//short j, k;
	COORD c;
	int colorIndex;
	BYTE setValue = ANIMAT_COLOR_INDEX;
	//int arrayLength = WinRect.right * WinRect.bottom;
	BOOL res;
	int index;
	int highlightIndex = 0;
	ANIMATSTATE_FILEOUT_PTR *pAsfo;
	CFileManagerStatic fileManagerStatic;

	for(i=0; i<NumAnimats; i++)
	{
		_ASSERT((int)pCoord[i].animatID == i);

		if(pHighLightList->Length() > 0 && !CBitmapEnvFunctions::AnimatIsASoundSource(i))
		{
			if(highlightIndex < pHighLightList->Length() && pHighLightList->Get(highlightIndex)->p->animatID == (UINT32)i)
			{
				highlightIndex++;
				// Skip this loop.  This is a highlighted animat and will be highlighted
				// after the rest of the animats are drawn.
				continue;
			}
		}

		// Set the animat color.  With the exception of the sound source, if no animats are to
		// be specifically highlighted then highlight all of them (looks nice).
		setValue = ANIMAT_COLOR_INDEX;
		if(CBitmapEnvFunctions::AnimatIsASoundSource(pCoord[i].animatID))
			setValue = ACOUSTICANIMATINDEX;
		else if(pHighLightList->Length() == 0)
			setValue = ANIMAT_HIGHLIGHT_COLOR_INDEX;

		for(h=0; h<NumIterations; h++)
		{
			index = fileManagerStatic.BuffIndex(i, NumAnimats, h, NumIterations);

			res = LatLonToScreenCoord(pCoord[index].lat, pCoord[index].lon, pBathyBmWinParam->bitmapInf.displayExtremes, WinRect, &c);
			if(FALSE == res)
				continue;
			
			colorIndex = WinRect.right * (WinRect.bottom - 1 - c.Y) + c.X;
			BitMapData[colorIndex] = setValue;
		}
	}


	// Draw the tracks of the specific animats to be highlighted.
	setValue = ANIMAT_HIGHLIGHT_COLOR_INDEX;
	for(i=0; i<pHighLightList->Length(); i++)
	{
		pAsfo = pHighLightList->Get(i);
		if(CBitmapEnvFunctions::AnimatIsASoundSource(pAsfo->p->animatID) == TRUE)
			continue;

		for(h=0; h<NumIterations; h++)
		{
			index = fileManagerStatic.BuffIndex(pAsfo->p->animatID, NumAnimats, h, NumIterations);
			res = LatLonToScreenCoord(pCoord[index].lat, pCoord[index].lon, pBathyBmWinParam->bitmapInf.displayExtremes, WinRect, &c);
			if(FALSE == res)
				continue;
			
			colorIndex = WinRect.right * (WinRect.bottom - 1 - c.Y) + c.X;
			BitMapData[colorIndex] = setValue;
		}		
	}
}



void AnimatsToBitmapX(RECT WinRect, BYTE *BitMapData, BATHYBITMAP_WIN_PARAM *pBathyBmWinParam, int NumIterations,
					 int NumAnimats, ANIMATSTATE_FILEOUT *pCoord, LinkedList <ANIMATSTATE_FILEOUT_PTR> *pHighLightList)
{
	// Uses bitmap indices set aside for animat colors
	// See BmpInitInfo() for indexing color setup
	int h, i;
	//short j;
	//short k;
	COORD c;
	int colorIndex;
	BYTE setValue = ANIMAT_COLOR_INDEX;
	//int arrayLength = WinRect.right * WinRect.bottom;
	BOOL res;
	int index;
	//SIMPLEPOP *popRef = CBitmapEnvFunctions::GetPopRef(); // returns a reference to a linked list.  This need to be imporeved.
	int animatID;
	ANIMATSTATE_FILEOUT_PTR *pAsfo;
	CFileManagerStatic fileManagerStatic;
	//int numIterationsDisplay;

	for(i=0; i<pHighLightList->Length(); i++)
	{
		pAsfo = pHighLightList->Get(i);
		_ASSERT(pAsfo != NULL);
		if(pAsfo == NULL)
			continue;
		animatID = pAsfo->p->animatID;

		if(CBitmapEnvFunctions::AnimatIsASoundSource(animatID))
			setValue = ACOUSTICANIMATINDEX;
		else
			setValue = ANIMAT_HIGHLIGHT_COLOR_INDEX;


		for(h=0; h<NumIterations; h++)
		{
			index = fileManagerStatic.BuffIndex(animatID, NumAnimats, h, NumIterations);
			res = LatLonToScreenCoord(pCoord[index].lat, pCoord[index].lon, pBathyBmWinParam->bitmapInf.displayExtremes, WinRect, &c);
			if(FALSE == res)
				continue;
			colorIndex = WinRect.right * (WinRect.bottom - 1 - c.Y) + c.X;
			BitMapData[colorIndex] = setValue;
		}
	}
}


void AnimateAnimats(RECT Rect, BATHYBITMAP_WIN_PARAM *pBathyBmWinParam,
					int NumIterations, int NumAnimats, ANIMATSTATE_FILEOUT *pCoord,
					int StateIndex,
					CListManager <ANIMATSTATE_FILEOUT_PTR> *pHighLightList, BOOL Inverse)
{
	// Uses bitmap indices set aside for animat colors
	// See BmpInitInfo() for indexing dotColor setup
	int	i;
	short j, k;
	COORD c;
	int index;
	int buffIndex;
	//BYTE dotColor = ANIMAT_COLOR_INDEX;
	//BYTE surColor; // surrounding dotColor.
	int arrayLength = Rect.right * Rect.bottom;
	BOOL res;
	//COLORREF colorRefSet;
	static COLORREF colorRefAnimat = RGB(0, 0, 0);
	static COLORREF colorRefAcstcSrc = RGB(255, 0, 0);
	static COLORREF colorRefHighlight = RGB(255, 255, 18);
	COLORREF dot, sur;
	CFileManagerStatic fileManagerStatic;


	HDC hdc = GetDC(g_hwndBathy);

	int highlightIndex = 0;
	ANIMATSTATE_FILEOUT_PTR *ptr;
	//BOOL highLightThisAnimat;

	//ACOUSTICANIMATINDEX
	// ANIMAT_HIGHLIGHT_COLOR_INDEX
	// ANIMAT_COLOR_INDEX

	if(FALSE == IsWindowEnabled(g_hwndBathy))
		return;

	pHighLightList->Lock();

	for(i=0; i<NumAnimats; i++)
	{
		_ASSERT((int)pCoord[i].animatID == i);

		if(pHighLightList->Length() > 0 && !CBitmapEnvFunctions::AnimatIsASoundSource(i))
		{
			if(highlightIndex < pHighLightList->Length() && pHighLightList->Get(highlightIndex)->p->animatID == (UINT32)i)
			{
				highlightIndex++;
				// Skip this loop.  This is a highlighted animat and will be highlighted
				// after the rest of the animats are drawn.
				continue;
			}
		}

		buffIndex = fileManagerStatic.BuffIndex(i, NumAnimats, StateIndex, NumIterations);
		res = LatLonToScreenCoord(pCoord[buffIndex].lat, pCoord[buffIndex].lon,	pBathyBmWinParam->bitmapInf.displayExtremes, Rect, &c);
		if(FALSE == res)
			continue;
		if(c.Y < 0 || c.Y >= Rect.bottom || c.X < 0 || c.X >= Rect.right)
			continue;


		// Set the animat dotColor.
		if(Inverse == FALSE)
		{	// No track displayed.
			dot = colorRefHighlight;
			sur = colorRefAnimat;
			if(CBitmapEnvFunctions::AnimatIsASoundSource(pCoord[buffIndex].animatID))
			{
				dot = colorRefAnimat;
				sur = colorRefAcstcSrc;
			}
		}
		else
		{	// Track displayed.
			dot = colorRefAnimat;
			sur = colorRefHighlight;
			if(CBitmapEnvFunctions::AnimatIsASoundSource(pCoord[buffIndex].animatID))
			{
				dot = colorRefAcstcSrc;
				sur = colorRefAnimat;
			}
		}


		// Emphasise the animat(s) to be highlighted by coloring an extra pixel nearby
		// below.
		if(Rect.bottom - 1 - c.Y - 1 >= 0)
		{
			for(j=-2; j<=2; j++)
			{
				for(k=-2; k<=2; k++)
				{
					// Don't display corners... make it look round.
					if(j == -2 || j == 2)
					{
						if(k == -2 || k == 2)
							continue;
					}

					if(c.Y + j < 0 || c.Y + j > Rect.bottom || c.X + k < 0 || c.X + k >= Rect.right)
						continue;

					index = Rect.right * (Rect.bottom - 1 - c.Y + j) + c.X + k;
					if(index >= 0 && index < arrayLength)
						SetPixel(hdc, c.X+k, c.Y+j, sur);					
				}
			}
		}
		SetPixel(hdc, c.X, c.Y, dot);
	}


	// Animate the highlighted animats.
	for(i=0; i<pHighLightList->Length(); i++)
	{
		ptr = pHighLightList->Get(i);

		buffIndex = fileManagerStatic.BuffIndex(ptr->p->animatID, NumAnimats, StateIndex, NumIterations);
		res = LatLonToScreenCoord(pCoord[buffIndex].lat, pCoord[buffIndex].lon,	pBathyBmWinParam->bitmapInf.displayExtremes, Rect, &c);

		if(FALSE == res)
			continue;
		if(c.Y < 0 || c.Y >= Rect.bottom || c.X < 0 || c.X >= Rect.right)
			continue;


		// Set the animat dotColor.
		if(Inverse == FALSE)
		{	// No track displayed.
			dot = colorRefAnimat;
			sur = colorRefHighlight;
			if(CBitmapEnvFunctions::AnimatIsASoundSource(ptr->p->animatID))
			{
				dot = colorRefAnimat;
				sur = colorRefAcstcSrc;
			}
		}
		else
		{	// Track displayed.
			dot = colorRefHighlight;
			sur = colorRefAnimat;
			if(CBitmapEnvFunctions::AnimatIsASoundSource(ptr->p->animatID))
			{
				dot = colorRefAcstcSrc;
				sur = colorRefAnimat;
			}
		}


		// Emphasise the animat(s) to be highlighted by coloring an extra pixel nearby
		// below.
		if(Rect.bottom - 1 - c.Y - 1 >= 0)
		{
			for(j=-2; j<=2; j++)
			{
				for(k=-2; k<=2; k++)
				{
					// Don't display corners... make it look round.
					if(j == -2 || j == 2)
					{
						if(k == -2 || k == 2)
							continue;
					}

					if(c.Y + j < 0 || c.Y + j > Rect.bottom || c.X + k < 0 || c.X + k >= Rect.right)
						continue;

					index = Rect.right * (Rect.bottom - 1 - c.Y + j) + c.X + k;
					if(index >= 0 && index < arrayLength)
						SetPixel(hdc, c.X+k, c.Y+j, sur);
				}
			}
		}
		SetPixel(hdc, c.X, c.Y, dot);
	}

	ReleaseDC(g_hwndBathy, hdc);
	pHighLightList->Unlock();

}

void DrawLineTo(HWND hWnd, BATHYBITMAP_WIN_PARAM *bathyGdata, COORD FromCoord, COORD ToCoord, COLORREF Cr)
{
	HDC		 hdcWnd;
	RECT	 rect;
	int	maxIndex;
	HGDIOBJ prevObject;

	// Quiet compiler warning.  Look into why this is passed in.
	bathyGdata = bathyGdata;

	GetClientRect(hWnd, &rect);
	maxIndex = rect.bottom * rect.right - 1;
	hdcWnd	= GetDC(hWnd);
	prevObject = SelectObject(hdcWnd, CreatePen(PS_SOLID, 1, Cr)); // create and select a pen into the DC

	MoveToEx(hdcWnd, FromCoord.X, FromCoord.Y, NULL);
	LineTo(hdcWnd, ToCoord.X, ToCoord.Y);

	// Must select the previous object back into the DC
	prevObject = SelectObject(hdcWnd, prevObject); // create and select a pen into the DC

	// Delete the created object.
	DeleteObject(prevObject);

	ReleaseDC(hWnd,hdcWnd);
	return;
}

void DrawScaleLines(HWND hWnd, BATHYBITMAP_WIN_PARAM *pBathyBmWinParam, LPBITMAPINFO bmpinfo, BYTE *s_bathyData, short *pUpperLine, short *pLowerLine)
{
	HDC		 hdcWnd;
	//short x,y;
	short	 x0, xf, y0, yf;
	//short m;
	//int		 index1, index2;
	int maxIndex;
	RECT	 rect;
	//COLORREF cr;

	//-----------------------------------------------------------//
	// Quiet compiler warning until I get this working correctly
	bmpinfo = bmpinfo;
	s_bathyData = s_bathyData;
	//-----------------------------------------------------------//

	GetClientRect(hWnd, &rect);
	maxIndex = rect.bottom * rect.right - 1;
	hdcWnd	= GetDC(hWnd);


	//*pUpperLine = (short)(pBathyBmWinParam->bitmapInf.topBarrier + pBathyBmWinParam->bitmapInf.topBarrierRangeExtension);
	//*pLowerLine = (short)(pBathyBmWinParam->bitmapInf.bttmBarrier - pBathyBmWinParam->bitmapInf.bttmBarrierRangeExtension);

	*pUpperLine = DepthToYPixel(rect.bottom,
							  pBathyBmWinParam->bitmapInf.displayExtremes.depthMax,
							  pBathyBmWinParam->bitmapInf.dataExtremes.depthMax,
							  pBathyBmWinParam->bitmapInf.dataExtremes.depthMin);
	*pLowerLine = DepthToYPixel(rect.bottom,
							  pBathyBmWinParam->bitmapInf.displayExtremes.depthMin,
							  pBathyBmWinParam->bitmapInf.dataExtremes.depthMax,
							  pBathyBmWinParam->bitmapInf.dataExtremes.depthMin);

	//-------------------//
	// These don't change
	x0 = 0;
	xf = (short)rect.right-1;
	//-------------------//

	y0 = 0;
	yf = *pUpperLine;
//	MoveToEx(hdcWnd, x0, y0, NULL);
	MoveToEx(hdcWnd, x0, yf, NULL);
	//LineTo(hdcWnd,   x0, yf);
	LineTo(hdcWnd,   xf, yf);
	//LineTo(hdcWnd,   xf, y0);

	y0 = *pLowerLine;
	//yf = (short)rect.bottom-1;
	//MoveToEx(hdcWnd, x0, yf, NULL);
	MoveToEx(hdcWnd, x0, y0, NULL);
//	LineTo(hdcWnd,   x0, y0);
	LineTo(hdcWnd,   xf, y0);
//	LineTo(hdcWnd,   xf, yf);

	ReleaseDC(hWnd,hdcWnd);
	return;
}

#if 0
void EraseLineTo(HWND hWnd, BATHYBITMAP_WIN_PARAM *bathyGdata, LPBITMAPINFO bmpinfo, BYTE *s_bathyData, COORD FromCoord, COORD ToCoord)
{

	bathyGdata = bathyGdata;
	s_bathyData = s_bathyData;
	FromCoord = FromCoord;
	ToCoord = ToCoord;
	bmpinfo = bmpinfo;
#if 0

	HDC hdcWnd;
	RECT rect;
	COLORREF cr;
	int	pixel, maxIndex;
	int	 x0, xf, x, y0, yf, y, m;
	int  i;

	int lineLength;
	double angle;
#endif

	return;

	// doesn't work... yet
#if 0
	GetClientRect(hWnd, &rect);
	maxIndex = rect.bottom * rect.right - 1;
	hdcWnd	= GetDC(hWnd);

	x0 = FromCoord.X;
	xf = ToCoord.X;
	y0 = FromCoord.Y;
	yf = ToCoord.Y;


	lineLength = int(ceil(sqrt(pow(double(xf-x0),2) + pow(double(yf-y0),2))));
	angle = atan2(double(yf-y0), double(xf-x0));


	for(i=0; i<lineLength; i++)
	{
		x = x0 + int(i*cos(angle));
		y = y0 + int(i*sin(angle));

		pixel = rect.right * (rect.bottom - 1 - y - 1) + x;

		//if(pixel < 0 && pixel > maxIndex)
		//	continue;
		if(pixel >= 0 && pixel <= maxIndex)
		{
			m = s_bathyData[pixel];
			cr = RGB(bmpinfo->bmiColors[m].rgbRed, bmpinfo->bmiColors[m].rgbGreen, bmpinfo->bmiColors[m].rgbBlue);
			SetPixel(hdcWnd, x, y, cr);
		}
#if 0
		pixel = rect.right * (rect.bottom - 1 - y - 1 - 1) + x;
		if(pixel >= 0 && pixel <= maxIndex)
		{
			m = s_bathyData[pixel];
			cr = RGB(bmpinfo->bmiColors[m].rgbRed, bmpinfo->bmiColors[m].rgbGreen, bmpinfo->bmiColors[m].rgbBlue);
			SetPixel(hdcWnd, x, y, cr);
		}


		pixel = rect.right * (rect.bottom - 1 - y - 1 + 1) + x;
		if(pixel >= 0 && pixel <= maxIndex)
		{
			m = s_bathyData[pixel];
			cr = RGB(bmpinfo->bmiColors[m].rgbRed, bmpinfo->bmiColors[m].rgbGreen, bmpinfo->bmiColors[m].rgbBlue);
			SetPixel(hdcWnd, x, y, cr);
		}


		pixel = rect.right * (rect.bottom - 1 - y - 1) + x-1;
		if(pixel >= 0 && pixel <= maxIndex)
		{
			m = s_bathyData[pixel];
			cr = RGB(bmpinfo->bmiColors[m].rgbRed, bmpinfo->bmiColors[m].rgbGreen, bmpinfo->bmiColors[m].rgbBlue);
			SetPixel(hdcWnd, x, y, cr);
		}


		pixel = rect.right * (rect.bottom - 1 - y - 1 - 1) + x-1;
		if(pixel >= 0 && pixel <= maxIndex)
		{
			m = s_bathyData[pixel];
			cr = RGB(bmpinfo->bmiColors[m].rgbRed, bmpinfo->bmiColors[m].rgbGreen, bmpinfo->bmiColors[m].rgbBlue);
			SetPixel(hdcWnd, x, y, cr);
		}

		pixel = rect.right * (rect.bottom - 1 - y - 1 + 1) + x-1;
		if(pixel >= 0 && pixel <= maxIndex)
		{
			m = s_bathyData[pixel];
			cr = RGB(bmpinfo->bmiColors[m].rgbRed, bmpinfo->bmiColors[m].rgbGreen, bmpinfo->bmiColors[m].rgbBlue);
			SetPixel(hdcWnd, x, y, cr);
		}

		pixel = rect.right * (rect.bottom - 1 - y - 1) + x+1;
		if(pixel >= 0 && pixel <= maxIndex)
		{
			m = s_bathyData[pixel];
			cr = RGB(bmpinfo->bmiColors[m].rgbRed, bmpinfo->bmiColors[m].rgbGreen, bmpinfo->bmiColors[m].rgbBlue);
			SetPixel(hdcWnd, x, y, cr);
		}

		pixel = rect.right * (rect.bottom - 1 - y - 1 - 1) + x+1;
		if(pixel >= 0 && pixel <= maxIndex)
		{
			m = s_bathyData[pixel];
			cr = RGB(bmpinfo->bmiColors[m].rgbRed, bmpinfo->bmiColors[m].rgbGreen, bmpinfo->bmiColors[m].rgbBlue);
			SetPixel(hdcWnd, x, y, cr);
		}

		pixel = rect.right * (rect.bottom - 1 - y - 1 + 1) + x + 1;
		if(pixel >= 0 && pixel <= maxIndex)
		{
			m = s_bathyData[pixel];
			cr = RGB(bmpinfo->bmiColors[m].rgbRed, bmpinfo->bmiColors[m].rgbGreen, bmpinfo->bmiColors[m].rgbBlue);
			SetPixel(hdcWnd, x, y, cr);
		}

#endif
	}
#endif
#if 0
	// Get the x and y starting and ending pixels.
	x0 = FromCoord.X;
	xf = ToCoord.X;
	xf = ToCoord.X;
	if(x0 > xf)
	{
//		x0 = xf;
//		xf = ToCoord.X;
		xInc = -1;
	}

	y0 = FromCoord.Y;
	yf = ToCoord.Y;
	if(y0 > yf)
	{
		//y0 = yf;
		//yf = FromCoord.Y;
		yInc = -1;
	}
	//--------------------------------------------------------------------------//
	// Redraw the pixels to their proper color
	//-----------
	for(x=x0; x!=xf; x = x + xInc)
	{
		for(y=y0; y!=yf; y = y + yInc)
		{
			pixel = rect.right * (rect.bottom - 1 - y - 1) + x;
			if(pixel < 0 && pixel > maxIndex)
				continue;

			m = s_bathyData[pixel];
			cr = RGB(bmpinfo->bmiColors[m].rgbRed, bmpinfo->bmiColors[m].rgbGreen, bmpinfo->bmiColors[m].rgbBlue);
			SetPixel(hdcWnd, x, y, cr);
		}
	}
#endif

//	ReleaseDC(hWnd,hdcWnd);

	return;
}
#endif

/*
	BathyExtremes.xMax = MaxLat;
	BathyExtremes.xMin = MinLat;
	BathyExtremes.yMax = MaxLon;
	BathyExtremes.yMin = MinLon;
*/
BOOL LatLonToScreenCoord(double Lat, double Lon, BATHYEXTREMES BathyExtremes, RECT WindowRect, COORD *pCoord)
{
	//RECT rect;
	double lonStep, latStep;
	int maxIndex;
	double tempVal;
	long tempX, tempY;


	//GetClientRect(ScreenHwnd, &rect); // This will retrieve this windows size.
	maxIndex = WindowRect.bottom * WindowRect.right - 1;
	latStep = (BathyExtremes.xMax - BathyExtremes.xMin)/double(WindowRect.bottom-1);
	lonStep	= (BathyExtremes.yMax - BathyExtremes.yMin)/double(WindowRect.right-1);


	//------------------------------------------------------------------------//
	// changed 11/2/09
	//pCoord->Y = (short)((int)floor((EnvMinMax.xMax-Lat)/latStep));
	tempVal = (BathyExtremes.xMax-Lat)/latStep;
	tempY = (long)floor(tempVal);
	if(tempVal - floor(tempVal) >= 0.5)
		tempY++;

	//pCoord->X = (short)((int)floor((Lon-EnvMinMax.yMin)/lonStep) + 1);
	tempVal = (Lon-BathyExtremes.yMin)/lonStep;
	tempX = (long)floor(tempVal);
	if(tempVal - floor(tempVal) >= 0.5)
		tempX++;
	//------------------------------------------------------------------------//


	if(tempY < 0 || tempY >= WindowRect.bottom || tempX < 0 || tempX >= WindowRect.right)
		return FALSE;

	_ASSERT(tempY < pow(2.0, (double)(sizeof(short)*8)) && tempX < pow(2.0, (double)(sizeof(short)*8)));

	pCoord->Y = (short)tempY;
	pCoord->X = (short)tempX;

	return TRUE;
}

double YPixelToDepth(long Y, long WinHeight, double Shallow, double Deep)
{
	double d;

	// prevent division by zero.
	if(WinHeight <= 1)
		return Deep - Shallow;

	if(Y == WinHeight-1)
		return Deep;

	if(Y == 0)
		return Shallow;
		
	d = Shallow + (Deep - Shallow) * (double)Y/(double)(WinHeight-1);
	return d;
}

short DepthToYPixel(long WinHeight, double Depth, double Shallow, double Deep)
{
	short s;
	if(WinHeight <= 1)
		WinHeight++; // prevent division by zero.

	s = (short)((double)(WinHeight-1)*(Depth - Shallow)/((Deep - Shallow)));

	//Depth = Shallow + (Deep - Shallow) * (double)s/(double)(WinHeight-1);
	return s;
}

BYTE *SlopeScaleToBitmap(SCALETOBITMAPPARAM Param)
{
	// Uses bitmap indices set aside for bathymetry
	// See BmpInitInfo() for indexing color setup
	LONG x,y, index; // pixel indices SCALETOBITMAPPARAM
	double value;
	double preValue;
	double dColorIndex;
	BYTE colorIndex;
	double dataPercent;
	static ENVDATA_INDEX lastSector = {0};
//double lat,lon; // latitude and longitude
	double refToZeroValue;
	//BATHY_BITMAP_INF bitmapInf;
	//BATHYVALUE bathyValues;
	BYTE byteValue;

	double dataMax;
	double dataMin;
	double displayMax;
	double displayMin;

	_ASSERT(Param.BitMapDataBufferLen != NULL);
	_ASSERT((Param.BitMapData == NULL && *Param.BitMapDataBufferLen == 0) || (Param.BitMapData != NULL && *Param.BitMapDataBufferLen != 0));

	if(Param.BitMapData == NULL || *Param.BitMapDataBufferLen == 0)
	{
		if(Param.BitMapData != NULL)
			delete [] Param.BitMapData;
		*Param.BitMapDataBufferLen = Param.WinWidth * Param.WinHeight;
		Param.BitMapData = new BYTE[*Param.BitMapDataBufferLen];
	}
	memset(Param.BitMapData, 0, sizeof(BYTE) * (*Param.BitMapDataBufferLen));

	// Set the lowest of the data extremes to zero and adjust related data by same amount
	refToZeroValue = -1*Param.dataMin;

	dataMax = Param.dataMax + refToZeroValue;
	dataMin = Param.dataMin + refToZeroValue;
	displayMax = Param.displayMax + refToZeroValue;
	displayMin = Param.displayMin + refToZeroValue;

	//bitmapInf = AdjustBitmapInfForDisplay(&SeedInf->bitmapInf, refToZeroValue);

	// top-down, left-right
	for(y=0; y<Param.WinHeight; y++)
	{
		if(Param.Abort != NULL && *Param.Abort == TRUE)
			break;

		//------------------------//
		// Testing and Debugging
		if(y == Param.WinHeight - 1)
			y = y;
		//------------------------//

		// Calculate the index in the bitmap array for row y.  The number are tricky
		// because the bitmap is 2-dimensional and goes from top-down, left right while
		// the bitmap array is indexed linarly.
		// '.right' is the width of the window in pixels
		// '.bottom' is the height of the window, in pixels.
		index = Param.WinWidth * (Param.WinHeight - 1 - y); // Incremented for each x.

		preValue = YPixelToDepth(y, Param.WinHeight, Param.dataMax, Param.dataMin);

		// Get the bathymetry data at the current latitude and longitude.
		//value = refToZeroValue + Sce->GetBathymetryDepth(lat, lon);
		//bathyValues = GetValueAtCoordinate(lat, lon);
		value = preValue + refToZeroValue;
		_ASSERT(value >= 0);
		if(value < 0) // have to look into this.  A possible result of extrapolation/interpolation.
			value = 0;

		if(y == Param.YLine1 || y == Param.YLine1-1 || y == Param.YLine1+1 || y == Param.YLine2 || y == Param.YLine2-1 || y == Param.YLine2+1)
		{
			byteValue = (BYTE)SLOPE_BITMAP_BLACK_INDEX;
		}
		else if(value >= displayMax)
		{
			// Region below land but more shallower than desired for display purposes.
			byteValue = (BYTE)SLOPE_BITMAP_BLACK_INDEX;; // minus 1 for zero-based indexing
		}
		else if(value <= displayMin)
		{
			// Deeper than desired for display purposes.
			byteValue = (BYTE)SLOPE_BITMAP_BLACK_INDEX;;
		}
		else
		{
			// Scalible depth display region.
			dataPercent = (value - displayMin)/(displayMax - displayMin);
			dColorIndex = (SLOPE_BITMAP_NUMCOLORS_255 - 1) * dataPercent; // minus 1 for zero indexing
			_ASSERT(dColorIndex >= 0 && dColorIndex <= 255);

			colorIndex = (BYTE)staticLib.MyRound(dColorIndex); // value is a negative number
			_ASSERT(colorIndex < SLOPE_BITMAP_BLACK_INDEX);
			byteValue = colorIndex;

			_ASSERT(colorIndex < SLOPE_BITMAP_BLACK_INDEX);
			_ASSERT(colorIndex >= 0);
		}


		for(x=0; x<Param.WinWidth; x++)
		{
			if(Param.Abort != NULL && *Param.Abort == TRUE)
				break;
			Param.BitMapData[index] = byteValue;
			index++;
		}
	}

	return Param.BitMapData;
}

BYTE *DepthScaleToBitmap(SCALETOBITMAPPARAM Param)
{
	// Uses bitmap indices set aside for bathymetry
	// See BmpInitInfo() for indexing color setup
	LONG x,y, index; // pixel indices SCALETOBITMAPPARAM
	double value;
	double preValue;
	double dColorIndex;
	BYTE colorIndex;
	double dataPercent;
	static ENVDATA_INDEX lastSector = {0};
//double lat,lon; // latitude and longitude
	double refToZeroValue;
	//BATHY_BITMAP_INF bitmapInf;
	//BATHYVALUE bathyValues;
	BYTE byteValue;

	double dataMax;
	double dataMin;
	double displayMax;
	double displayMin;


	_ASSERT(Param.BitMapDataBufferLen != NULL);
	_ASSERT((Param.BitMapData == NULL && *Param.BitMapDataBufferLen == 0) || (Param.BitMapData != NULL && *Param.BitMapDataBufferLen != 0));

	if(Param.BitMapData == NULL || *Param.BitMapDataBufferLen == 0)
	{
		if(Param.BitMapData != NULL)
			delete [] Param.BitMapData;
		*Param.BitMapDataBufferLen = Param.WinWidth * Param.WinHeight;
		Param.BitMapData = new BYTE[*Param.BitMapDataBufferLen];
	}
	memset(Param.BitMapData, 0, sizeof(BYTE) * (*Param.BitMapDataBufferLen));

	// Set the lowest of the data extremes to zero and adjust related data by same amount
	refToZeroValue = -1*Param.dataMin;

	dataMax = Param.dataMax + refToZeroValue;
	dataMin = Param.dataMin + refToZeroValue;
	displayMax = Param.displayMax + refToZeroValue;
	displayMin = Param.displayMin + refToZeroValue;

	//bitmapInf = AdjustBitmapInfForDisplay(&SeedInf->bitmapInf, refToZeroValue);

	// top-down, left-right
	for(y=0; y<Param.WinHeight; y++)
	{
		if(Param.Abort != NULL && *Param.Abort == TRUE)
			break;

		// Calculate the index in the bitmap array for row y.  The number are tricky
		// because the bitmap is 2-dimensional and goes from top-down, left right while
		// the bitmap array is indexed linarly.
		// '.right' is the width of the window in pixels
		// '.bottom' is the height of the window, in pixels.
		index = Param.WinWidth * (Param.WinHeight - 1 - y); // Incremented for each x.

		preValue = YPixelToDepth(y, Param.WinHeight, Param.dataMax, Param.dataMin);

		// Get the bathymetry data at the current latitude and longitude.
		//value = refToZeroValue + Sce->GetBathymetryDepth(lat, lon);
		//bathyValues = GetValueAtCoordinate(lat, lon);
		value = preValue + refToZeroValue;
		_ASSERT(value >= 0);
		if(value < 0) // have to look into this.  A possible result of extrapolation/interpolation.
			value = 0;

		if(y == Param.YLine1 || y == Param.YLine1-1 || y == Param.YLine1+1 || y == Param.YLine2 || y == Param.YLine2-1 || y == Param.YLine2+1)
		{
			byteValue = (BYTE)ANIMAT_COLOR_INDEX;
		}
		else if(value >= displayMax)
		{
			// Region below land but more shallower than desired for display purposes.
			byteValue = (BYTE)(COLORINDEXSTART_BATHY + NUMCOLORS_BATHY-1); // minus 1 for zero-based indexing
		}
		else if(value <= displayMin)
		{
			// Deeper than desired for display purposes.
			byteValue = COLORINDEXSTART_BATHY;
		}
		else
		{
			// Scalible depth display region.
			dataPercent = (value - displayMin)/(displayMax - displayMin);
			dColorIndex = COLORINDEXSTART_BATHY + (NUMCOLORS_BATHY - 1) * dataPercent; // minus 1 for zero indexing
			_ASSERT(dColorIndex >= 0 && dColorIndex <= 255);

			colorIndex = (BYTE)staticLib.MyRound(dColorIndex); // value is a negative number
			_ASSERT(colorIndex < NUMCOLORS_BATHY);
			byteValue = colorIndex;

			_ASSERT(colorIndex <= NUMCOLORS_BATHY + COLORINDEXSTART_BATHY);
			_ASSERT(colorIndex >= COLORINDEXSTART_BATHY);
		}


		for(x=0; x<Param.WinWidth; x++)
		{
			if(Param.Abort != NULL && *Param.Abort == TRUE)
				break;
			Param.BitMapData[index] = byteValue;
			index++;
		}
	}

	return Param.BitMapData;
}

BYTE *DepthScaleToBitmap(int WinWidth, int WinHeight, BYTE *BitMapData, int *BitMapDataBufferLen, BATHYBITMAP_WIN_PARAM *SeedInf, short YLine1, short YLine2, BOOL *Abort)
{
	// Uses bitmap indices set aside for bathymetry
	// See BmpInitInfo() for indexing color setup
	LONG x,y, index; // pixel indices SCALETOBITMAPPARAM
	double value;
	double preValue;
	double dColorIndex;
	BYTE colorIndex;
	double dataPercent;
	static ENVDATA_INDEX lastSector = {0};
//double lat,lon; // latitude and longitude
	double refToZeroValue;
	BATHY_BITMAP_INF bitmapInf;
	//BATHYVALUE bathyValues;

	BYTE byteValue;


	_ASSERT(BitMapDataBufferLen != NULL && SeedInf != NULL);
	_ASSERT((BitMapData == NULL && *BitMapDataBufferLen == 0) || (BitMapData != NULL && *BitMapDataBufferLen != 0));

	if(BitMapData == NULL || *BitMapDataBufferLen == 0)
	{
		if(BitMapData != NULL)
			delete [] BitMapData;
		*BitMapDataBufferLen = WinWidth * WinHeight;
		BitMapData = new BYTE[*BitMapDataBufferLen];
	}
	memset(BitMapData, 0, sizeof(BYTE) * (*BitMapDataBufferLen));

	// Set the lowest of the data extremes to zero and adjust related data by same amount
	refToZeroValue = -1*SeedInf->bitmapInf.dataExtremes.depthMin;
	bitmapInf = AdjustBitmapInfForDisplay(&SeedInf->bitmapInf, refToZeroValue);

	// top-down, left-right
	for(y=0; y<WinHeight; y++)
	{
		if(Abort != NULL && *Abort == TRUE)
			break;

		// Calculate the index in the bitmap array for row y.  The number are tricky
		// because the bitmap is 2-dimensional and goes from top-down, left right while
		// the bitmap array is indexed linarly.
		// '.right' is the width of the window in pixels
		// '.bottom' is the height of the window, in pixels.
		index = WinWidth * (WinHeight - 1 - y); // Incremented for each x.

		preValue =
			YPixelToDepth(y, WinHeight, SeedInf->bitmapInf.dataExtremes.depthMax, SeedInf->bitmapInf.dataExtremes.depthMin);

		// Get the bathymetry data at the current latitude and longitude.
		//value = refToZeroValue + Sce->GetBathymetryDepth(lat, lon);
		//bathyValues = GetValueAtCoordinate(lat, lon);
		value = preValue + refToZeroValue;
		_ASSERT(value >= 0);
		if(value < 0) // have to look into this.  A possible result of extrapolation/interpolation.
			value = 0;

		if(y == YLine1 || y == YLine1-1 || y == YLine1+1 || y == YLine2 || y == YLine2-1 || y == YLine2+1)
		{
			byteValue = (BYTE)ANIMAT_COLOR_INDEX;
		}
		else if(value >= bitmapInf.displayExtremes.depthMax)
		{
			// Region below land but more shallower than desired for display purposes.
			byteValue = (BYTE)(COLORINDEXSTART_BATHY + NUMCOLORS_BATHY-1); // minus 1 for zero-based indexing
		}
		else if(value <= bitmapInf.displayExtremes.depthMin)
		{
			// Deeper than desired for display purposes.
			byteValue = COLORINDEXSTART_BATHY;
		}
		else
		{
			// Scalible depth display region.
			dataPercent = (value - bitmapInf.displayExtremes.depthMin)/
				(bitmapInf.displayExtremes.depthMax - bitmapInf.displayExtremes.depthMin);
			dColorIndex = COLORINDEXSTART_BATHY + (NUMCOLORS_BATHY - 1) * dataPercent; // minus 1 for zero indexing
			_ASSERT(dColorIndex >= 0 && dColorIndex <= 255);

			colorIndex = (BYTE)staticLib.MyRound(dColorIndex); // value is a negative number
			_ASSERT(colorIndex < NUMCOLORS_BATHY);
			byteValue = colorIndex;

			_ASSERT(colorIndex <= NUMCOLORS_BATHY + COLORINDEXSTART_BATHY);
			_ASSERT(colorIndex >= COLORINDEXSTART_BATHY);
		}



		for(x=0; x<WinWidth; x++)
		{
			if(Abort != NULL && *Abort == TRUE)
				break;
			BitMapData[index] = byteValue;
			index++;
		}
	}

	return BitMapData;
}


// Deallocates and reallocates memory as needed.
void BathymetryDataToBuffer(RECT Rect, BYTE *BitMapData, BATHYBITMAP_WIN_PARAM *pSeedInf, BOOL *pAbort)
{
	// Uses bitmap indices set aside for bathymetry
	// See BmpInitInfo() for indexing color setup
	LONG x,y, index; // pixel indices
	double value;
	double preValue;
	double dColorIndex;
	BYTE colorIndex;
	double dataPercent;
	static ENVDATA_INDEX lastSector = {0};
	double lat,lon; // latitude and longitude
	double refToZeroValue;
	BATHY_BITMAP_INF bitmapInf;
	BATHYVALUE bathyValues;

	_ASSERT(pSeedInf != NULL);

	// Set the lowest of the data extremes to zero and adjust related data by same amount
	refToZeroValue = -1*pSeedInf->bitmapInf.dataExtremes.depthMin;
	bitmapInf = AdjustBitmapInfForDisplay(&pSeedInf->bitmapInf, refToZeroValue);


	// top-down, left-right
	for(y=0; y<Rect.bottom && *pAbort == FALSE; y++)
	{
		// Calculate current latitude for the bitmaps row y.
		lat	= bitmapInf.displayExtremes.xMin +	bitmapInf.latPerPixel * (Rect.bottom - 1 - y);
		_ASSERT(bitmapInf.dataExtremes.xMin <= lat);
		_ASSERT(bitmapInf.dataExtremes.xMax >= lat);

		// Calculate the index in the bitmap array for row y.  The number are tricky
		// because the bitmap is 2-dimensional and goes from top-down, left right while
		// the bitmap array is indexed linarly.
		// '.right' is the width of the window in pixels
		// '.bottom' is the height of the window, in pixels.
		index = Rect.right * (Rect.bottom - 1 - y); // Incremented for each x.

		for(x=0; x<Rect.right && *pAbort == FALSE; x++)
		{
			//if(x%10 != 0)
			//	continue;

			lon = bitmapInf.displayExtremes.yMin + bitmapInf.lonPerPixel * x;
			_ASSERT(bitmapInf.dataExtremes.yMin <= lon);
			_ASSERT(bitmapInf.dataExtremes.yMax >= lon);

			// Get the bathymetry data at the current latitude and longitude.
			//value = refToZeroValue + Sce->GetBathymetryDepth(lat, lon);
			bathyValues = CBitmapEnvFunctions::GetValueAtCoordinate(lat, lon);
			preValue = bathyValues.depth;
			value = preValue + refToZeroValue;
			if(value < 0) // have to look into this.  A possible result of extrapolation/interpolation.
				value = 0;
			_ASSERT(value >= 0);

			if(value >= LAND_DEPTH + refToZeroValue)
			{
				// An exception that gets a special mapping and doesn't take scaling into
				// account.  Can potentially add several of these.
				BitMapData[index] = LAND_COLOR_INDEX;
			}
			else if(value >= bitmapInf.displayExtremes.depthMax)
			{
				// Region below land but more shallower than desired for display purposes.
				BitMapData[index] = (BYTE)(COLORINDEXSTART_BATHY + NUMCOLORS_BATHY-1); // minus 1 for zero-based indexing
			}
			else if(value <= bitmapInf.displayExtremes.depthMin)
			{
				// Deeper than desired for display purposes.
				BitMapData[index] = COLORINDEXSTART_BATHY;
			}
			else
			{
				// Scalible depth display region.
				dataPercent = (value - bitmapInf.displayExtremes.depthMin)/
					(bitmapInf.displayExtremes.depthMax - bitmapInf.displayExtremes.depthMin);
				dColorIndex = COLORINDEXSTART_BATHY + (NUMCOLORS_BATHY - 1) * dataPercent; // minus 1 for zero indexing
				_ASSERT(dColorIndex >= 0 && dColorIndex <= 255);

				colorIndex = (BYTE)staticLib.MyRound(dColorIndex); // value is a negative number
				_ASSERT(colorIndex < NUMCOLORS_BATHY);
				BitMapData[index] = colorIndex;

				_ASSERT(colorIndex <= NUMCOLORS_BATHY + COLORINDEXSTART_BATHY);
				_ASSERT(colorIndex >= COLORINDEXSTART_BATHY);
			}
			index++;
		}
	}
}



//HBITMAP DataPointsToBuffer(RECT Rect, INTENSITY Intensity, RAWENVIRONMENTALDATA *ed, BATHYEXTREMES DisplayExtremes)
void DataPointsToBuffer(RECT Rect, RAWENVIRONMENTALDATA *ed, BATHYEXTREMES DisplayExtremes, VOID *pvData)
{
	LONG x,y; //, index; // pixel indices
	LONG j,k;
	COORD c;
	int index;
	int extraPixels = 0;//Intensity-1;
	int arrayLength = Rect.right * Rect.bottom;
	int dataPtCnt = 0;


    UCHAR ubAlpha;         // used for doing transparent gradient 
    UCHAR ubRed = 0, ubGreen = 0, ubBlue = 0;
    float fAlphaFactor;    // used to do premultiply

	double dataPtDensity;
	//double dataPtDisplayThickDensityLow = 20.0; // Datapoints per square pixels
	//double dataPtDisplayThickDensityMed = 40.0;// Datapoints per square pixels

	//DisplayExtremes.xMin is the minimum latitude
	//DisplayExtremes.xMax is the maximum latitude
	//DisplayExtremes.yMin is the minimum longitude
	//DisplayExtremes.yMax is the maximum longitude


	// Default entire set of data to zero alpha.
	memset(pvData, 0, Rect.right * Rect.bottom * 4);

	for(y=0; y<ed->Ylen; y++)
	{
		for(x=0; x<ed->Xlen; x++)
		{
			if(FALSE == LatLonToScreenCoord(ed->X[x], ed->Y[y], DisplayExtremes, Rect, &c))
				continue;

			dataPtCnt++;

			ubAlpha = 100;
			fAlphaFactor = (float)ubAlpha / (float)0xff;

			index = Rect.right * (Rect.bottom - 1 - c.Y) + c.X;

            ((UINT32 *)pvData)[index] 
                = (ubAlpha << 24) |                       //0xaa000000 
                 ((UCHAR)(ubRed * fAlphaFactor) << 16) |  //0x00rr0000 
                 ((UCHAR)(ubGreen * fAlphaFactor) << 8) | //0x0000gg00 
                 ((UCHAR)(ubBlue * fAlphaFactor));      //0x000000bb 
		}
	}

	if(Rect.right*Rect.bottom <= 0)
		dataPtDensity = 0;
	else
		dataPtDensity = (double)dataPtCnt/((double)(Rect.right*Rect.bottom));


	if(dataPtDensity < .001)
		extraPixels = 2;
	else if(dataPtDensity < .004)
		extraPixels = 1;

	ubRed = 55;
	ubGreen = 55;
	ubBlue = 55;
	for(y=0; y<ed->Ylen && extraPixels > 0; y++)
	{
		for(x=0; x<ed->Xlen; x++)
		{
			if(FALSE == LatLonToScreenCoord(ed->X[x], ed->Y[y], DisplayExtremes, Rect, &c))
				continue;

			ubAlpha = 100;
			fAlphaFactor = (float)ubAlpha / (float)0xff;
/*
			index = Rect.right * (Rect.bottom - 1 - c.Y) + c.X;

            ((UINT32 *)pvData)[index] 
                = (ubAlpha << 24) |                       //0xaa000000 
                 ((UCHAR)(ubRed * fAlphaFactor) << 16) |  //0x00rr0000 
                 ((UCHAR)(ubGreen * fAlphaFactor) << 8) | //0x0000gg00 
                 ((UCHAR)(ubBlue * fAlphaFactor));      //0x000000bb 

			ubAlpha = 50;
			fAlphaFactor = (float)ubAlpha / (float)0xff;
*/
			if(Rect.bottom - 1 - c.Y - 1 >= 0)
			{
				for(j=-(int)extraPixels; j<=(int)extraPixels; j++)
				{
					for(k=-(int)extraPixels; k<=(int)extraPixels; k++)
					{
						// Don't display corners... make it look round.
						if(j == -(int)extraPixels || j == (int)extraPixels)
						{
							if(k == -(int)extraPixels || k == (int)extraPixels)
								continue;
						}
						if(j==0 && k == 0)
						{
							//continue;
							ubRed = 255;
							ubGreen = 255;
							ubBlue = 255;
						}
						else
						{
							ubRed = 5;
							ubGreen = 5;
							ubBlue = 5;
						}

						index = Rect.right * (Rect.bottom - 1 - c.Y + j) + c.X + k;
						if(index >= 0 && index < arrayLength)
						{
							((UINT32 *)pvData)[index] 
								= (ubAlpha << 24) |                       //0xaa000000 
								 ((UCHAR)(ubRed * fAlphaFactor) << 16) |  //0x00rr0000 
								 ((UCHAR)(ubGreen * fAlphaFactor) << 8) | //0x0000gg00 
								 ((UCHAR)(ubBlue * fAlphaFactor));      //0x000000bb 
							//pvData[index] = ANIMAT_COLOR_INDEX;
							//SetPixel(hdc, c.X + k, c.Y + j, RGB(0, 0, 0));	
						}
					}
				}
			}
		}
	}
}

const UCHAR SLOPE_ALPHA_RED = 0xff;
const UCHAR SLOPE_ALPHA_GREEN = 0x00;
const UCHAR SLOPE_ALPHA_BLUE = 0x00;

// From: http://web.njit.edu/~kevin/rgb.txt.html
// NavajoWhite
const UCHAR SHELF_ENVATTR_ALPHA_RED = 255;
const UCHAR SHELF_ENVATTR_ALPHA_GREEN = 222;
const UCHAR SHELF_ENVATTR_ALPHA_BLUE = 173;

// DarkGoldenrod1
const UCHAR BASIN_ENVATTR_ALPHA_RED = 255;
const UCHAR BASIN_ENVATTR_ALPHA_GREEN = 185;
const UCHAR BASIN_ENVATTR_ALPHA_BLUE = 15;

// coral
const UCHAR SLOPE_ENVATTR_ALPHA_RED = 255;
const UCHAR SLOPE_ENVATTR_ALPHA_GREEN = 127;
const UCHAR SLOPE_ENVATTR_ALPHA_BLUE = 0x00;



void SlopeDataToBuffer(RECT Rect, BATHYBITMAP_WIN_PARAM  *SeedInf, void *pvData)
{
	// Draws on the bitmap, so function BmpInitInfo() doesn't apply here.
	LONG x,y;//, index; // pixel indices
	static ENVDATA_INDEX lastSector = {0};
	double lat,lon; // latitude and longitude
	BATHY_BITMAP_INF bitmapInf;
	BATHYVALUE bathyValues;
	BATHYEXTREMES bathyExtemes;
	double range;
	bool landRegion;
	bool beachingRegion;
	bool shelfRegion;
	bool basinRegion;
	bool slopeRegion;
	bool slopeDisplay;

    UCHAR ubAlpha;         // used for doing transparent gradient 
    UCHAR ubRed = 0x00;
	UCHAR ubGreen = 0x00;
	UCHAR ubBlue = 0x00;

    float fAlphaFactor;    // used to do premultiply
	long index;

	bitmapInf = SeedInf->bitmapInf;

	bathyExtemes = CBitmapEnvFunctions::GetBathyExtremes();
	range = bitmapInf.displayExtremes.slopeMax - bitmapInf.displayExtremes.slopeMin;

	// top-down, left-right
	for(y=0; y<Rect.bottom; y++)
	{
		// Calculate current latitude for the bitmaps row y.
		lat	= bitmapInf.displayExtremes.xMin + bitmapInf.latPerPixel * (Rect.bottom - 1 - y);
		_ASSERT(bitmapInf.dataExtremes.xMin <= lat);
		_ASSERT(bitmapInf.dataExtremes.xMax >= lat);

		// Calculate the index in the bitmap array for row y.  The number are tricky
		// because the bitmap is 2-dimensional and goes from top-down, left right while
		// the bitmap array is indexed linarly.
		// '.right' is the width of the window in pixels
		// '.bottom' is the height of the window, in pixels.
		//index = Rect.right * (Rect.bottom - 1 - y); // Incremented for each x.

		for(x=0; x<Rect.right; x++)
		{
			ubAlpha = 0x00;
			ubRed = 0x00;
			ubGreen = 0x00;
			ubBlue = 0x00;

			index = Rect.right * (Rect.bottom - 1 - y) + x;
			lon = bitmapInf.displayExtremes.yMin + bitmapInf.lonPerPixel * x;
			bathyValues = CBitmapEnvFunctions::GetBathymetryValues(lat, lon);

			landRegion = (bathyValues.depth >= 0.0);

			beachingRegion = (bathyValues.depth >= -2.00 && bathyValues.depth < 0);

			shelfRegion = (bathyValues.depth >= SHELF_ENVATTR_BATHY_SHALLOWER_THAN) &&
				(bathyValues.slope < SHELF_ENVATTR_DEG_LESS_THAN);

			basinRegion = (bathyValues.depth < BASIN_ENVATTR_BATHY_DEEPER_THAN) &&
				(bathyValues.slope < BASIN_ENVATTR_DEG_LESS_THAN);

			slopeRegion = (bathyValues.depth < SLOPE_ENVATTR_BATHY_DEEPER_THAN) && 
				(bathyValues.slope > SLOPE_ENVATTR_DEG_GREATER_THAN);

			slopeDisplay = (bathyValues.slope >= bitmapInf.displayExtremes.slopeMin) &&
				(bathyValues.slope < bitmapInf.displayExtremes.slopeMax);



			//shelfRegion = FALSE;
			//basinRegion = FALSE;
			//slopeRegion = FALSE;
			//slopeDisplay = FALSE;



			if(landRegion)
			{
				; // do nothing.
			}
			else if(beachingRegion)
			{
				ubAlpha = 200;
				ubRed = 0;
				ubGreen = 0;
				ubBlue = 0;
			}
			else if(shelfRegion)
			{
				// Shelf environmental attractor
				ubAlpha = 205;
				ubRed = SHELF_ENVATTR_ALPHA_RED;
				ubGreen = SHELF_ENVATTR_ALPHA_GREEN;
				ubBlue = SHELF_ENVATTR_ALPHA_BLUE;
				_ASSERT(basinRegion == FALSE && slopeRegion == FALSE);
			}
			else if(basinRegion)
			{
				ubAlpha = 205;
				ubRed = BASIN_ENVATTR_ALPHA_RED;
				ubGreen = BASIN_ENVATTR_ALPHA_GREEN;
				ubBlue = BASIN_ENVATTR_ALPHA_BLUE;
				// Basin environmental attractor
				_ASSERT(slopeRegion == FALSE);
			}
			else if(slopeRegion)
			{
				ubAlpha = 205;
				ubRed = SLOPE_ENVATTR_ALPHA_RED;
				ubGreen = SLOPE_ENVATTR_ALPHA_GREEN;
				ubBlue = SLOPE_ENVATTR_ALPHA_BLUE;
				// Slope environmental attractor
			}
			else if(slopeDisplay)
			{
				ubAlpha = 50 + (BYTE)(205*(bathyValues.slope - bitmapInf.displayExtremes.slopeMin)/range);
				ubRed = SLOPE_ALPHA_RED;
				ubGreen = SLOPE_ALPHA_GREEN;
				ubBlue = SLOPE_ALPHA_BLUE;
			}

            fAlphaFactor = (float)ubAlpha / (float)0xff; 
            // multiply each pixel by fAlphaFactor, so each component  
            // is less than or equal to the alpha value. 
            ((UINT32 *)pvData)[index] //
                = (ubAlpha << 24) |                       //0xaa000000 
                 ((UCHAR)(ubRed * fAlphaFactor) << 16) |  //0x00rr0000 
                 ((UCHAR)(ubGreen * fAlphaFactor) << 8) | //0x0000gg00 
                 ((UCHAR)(ubBlue * fAlphaFactor));      //0x000000bb 
		}
	}
}


#if 0
backup of previous SlopeDataToBuffer.
void SlopeDataToBuffer(RECT Rect, BATHYBITMAP_WIN_PARAM  *SeedInf, void *pvData)
{
	// Draws on the bitmap, so function BmpInitInfo() doesn't apply here.
	LONG x,y;//, index; // pixel indices
	static ENVDATA_INDEX lastSector = {0};
	double lat,lon; // latitude and longitude
	BATHY_BITMAP_INF bitmapInf;
	BATHYVALUE bathyValues;
	BATHYEXTREMES bathyExtemes;
	double range;

    UCHAR ubAlpha;         // used for doing transparent gradient 
    UCHAR ubRed = 0xff;
	UCHAR ubGreen = 0x00;
	UCHAR ubBlue = 0x00;

    float fAlphaFactor;    // used to do premultiply
	long index;

	bitmapInf = SeedInf->bitmapInf;

	bathyExtemes = CBitmapEnvFunctions::GetBathyExtremes();
	range = bitmapInf.displayExtremes.slopeMax - bitmapInf.displayExtremes.slopeMin;

	// top-down, left-right
	for(y=0; y<Rect.bottom; y++)
	{
		// Calculate current latitude for the bitmaps row y.
		lat	= bitmapInf.displayExtremes.xMin + bitmapInf.latPerPixel * (Rect.bottom - 1 - y);
		_ASSERT(bitmapInf.dataExtremes.xMin <= lat);
		_ASSERT(bitmapInf.dataExtremes.xMax >= lat);

		// Calculate the index in the bitmap array for row y.  The number are tricky
		// because the bitmap is 2-dimensional and goes from top-down, left right while
		// the bitmap array is indexed linarly.
		// '.right' is the width of the window in pixels
		// '.bottom' is the height of the window, in pixels.
		//index = Rect.right * (Rect.bottom - 1 - y); // Incremented for each x.

		for(x=0; x<Rect.right; x++)
		{
			index = Rect.right * (Rect.bottom - 1 - y) + x;
			lon = bitmapInf.displayExtremes.yMin + bitmapInf.lonPerPixel * x;

			bathyValues = CBitmapEnvFunctions::GetBathymetryValues(lat, lon);

			if(bathyValues.slope < bitmapInf.displayExtremes.slopeMin || bathyValues.slope > bitmapInf.displayExtremes.slopeMax)
				ubAlpha = 0;
			else
				ubAlpha = 50 + (BYTE)(205*(bathyValues.slope - bitmapInf.displayExtremes.slopeMin)/range);

            fAlphaFactor = (float)ubAlpha / (float)0xff; 
            // multiply each pixel by fAlphaFactor, so each component  
            // is less than or equal to the alpha value. 
            ((UINT32 *)pvData)[index] //
                = (ubAlpha << 24) |                       //0xaa000000 
                 ((UCHAR)(ubRed * fAlphaFactor) << 16) |  //0x00rr0000 
                 ((UCHAR)(ubGreen * fAlphaFactor) << 8) | //0x0000gg00 
                 ((UCHAR)(ubBlue * fAlphaFactor));      //0x000000bb 
		}
	}
}
#endif

void BlendAlphaBlendIntoBitmap(RECT Rect, HBITMAP *phbitmap, VOID *pvData)
{
    BLENDFUNCTION bf;      // structure for alpha blending 
    HBITMAP hbitmap;       // bitmap handle 
	HDC hdc;
	HDC hdcwnd;
    VOID *pvBits;          // pointer to DIB section 
    BITMAPINFO bmi;        // bitmap header 

    // zero the memory for the bitmap info 
    ZeroMemory(&bmi, sizeof(BITMAPINFO));

    bmi.bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
    bmi.bmiHeader.biWidth = Rect.right;//
    bmi.bmiHeader.biHeight = Rect.bottom;//
    bmi.bmiHeader.biPlanes = 1; // Specifies the number of planes for the target device. This value must be set to 1. 
    bmi.bmiHeader.biBitCount = 32; // four 8-bit components.  Specifies the number of bits-per-pixel. The biBitCount member
								   // of the BITMAPINFOHEADER structure determines the number of bits that define each pixel
								   // and the maximum number of colors in the bitmap
    bmi.bmiHeader.biCompression = BI_RGB; // ( BI_RGB is an uncompressed format.) //Specifies the type of compression for a
										  // compressed bottom-up bitmap (top-down DIBs cannot be compressed).
    bmi.bmiHeader.biSizeImage = Rect.right * Rect.bottom * 4; // Specifies the size, in bytes, of the image. This may be
																// set to zero for BI_RGB bitmaps. 


	////////////////////////////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////

    bf.BlendOp = AC_SRC_OVER;
    bf.BlendFlags = 0;
    bf.AlphaFormat = AC_SRC_ALPHA;   // use source alpha  
    bf.SourceConstantAlpha = 0xff;   // use constant alpha, with  
                                     // 75% opaqueness 


    // create a DC for our bitmap -- the source DC for AlphaBlend  
	hdcwnd = GetDC(g_hwndBathy);
    hdc = CreateCompatibleDC(hdcwnd);
    // create our DIB (Device Independent Bitmap) section and select the bitmap into the dc 
    hbitmap = CreateDIBSection(hdc, &bmi, DIB_RGB_COLORS, &pvBits, NULL, 0x0);
    SelectObject(hdc, hbitmap);

	memcpy(pvBits, pvData, Rect.right * Rect.bottom * 4);

    if(!AlphaBlend(
		hdcwnd, // handle to destination DC
		0,//winWidth/5, // x-coord of upper-left corner
		0,//winHeight/5, // y-coord of upper-left corner
		Rect.right, // destination width
		Rect.bottom, // destination height
		hdc, // handle to source DC
		0, // x-coord of upper-left corner
		0, // y-coord of upper-left corner
		Rect.right, // source width
		Rect.bottom, // source height
		bf)) // alpha-blending function
        return;                     // alpha blend failed 

    // do cleanup 
    DeleteObject(hbitmap);
    DeleteDC(hdc);   
	ReleaseDC(g_hwndBathy, hdcwnd);


	// Replace the current bitmap handle with one created based upon the alphablended display.

	HDC hdcBathy = GetDC(g_hwndBathy); // Get a handle to the Device Context associated with this window,
	HDC hdc2 = CreateCompatibleDC(hdcBathy);

	if(*phbitmap != NULL)
		DeleteObject(*phbitmap);
	*phbitmap = CreateCompatibleBitmap(hdcBathy, Rect.right, Rect.bottom);

	// Select original back in if this works.
	if(NULL != SelectObject(hdc2, *phbitmap))
	{
		if(!BitBlt(hdc2, 
			   0,0, 
			   Rect.right, Rect.bottom, 
			   hdcBathy, 
			   0,0, 
			   SRCCOPY))
		{
			hdc2 = hdc2;
		}

		ReleaseDC(g_hwndBathy, hdcBathy);
		DeleteDC(hdc2);
	}
}


/*
// This is the original.  It goes away.
HBITMAP SlopeToBitmap(RECT WindowRect, BATHYBITMAP_WIN_PARAM  *SeedInf)
{
	// Draws on the bitmap, so function BmpInitInfo() doesn't apply here.
	LONG x,y;//, index; // pixel indices
	HDC hdcwnd;
	HDC hdc;

	static ENVDATA_INDEX lastSector = {0};
	double lat,lon; // latitude and longitude
	BATHY_BITMAP_INF bitmapInf;
	BATHYVALUE bathyValues;
	BATHYEXTREMES bathyExtemes;// = SeedInf->bitmapInf.;
	double range;

    UCHAR ubAlpha;         // used for doing transparent gradient 
    UCHAR ubRed;        
    UCHAR ubGreen;
    UCHAR ubBlue;
    float fAlphaFactor;    // used to do premultiply
    BLENDFUNCTION bf;      // structure for alpha blending 
    HBITMAP hbitmap;       // bitmap handle 
    BITMAPINFO bmi;        // bitmap header 
    VOID *pvBits;          // pointer to DIB section 
	long index;



    // zero the memory for the bitmap info 
    ZeroMemory(&bmi, sizeof(BITMAPINFO));

    bmi.bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
    bmi.bmiHeader.biWidth = WindowRect.right;//
    bmi.bmiHeader.biHeight = WindowRect.bottom;//
    bmi.bmiHeader.biPlanes = 1; // Specifies the number of planes for the target device. This value must be set to 1. 
    bmi.bmiHeader.biBitCount = 32; // four 8-bit components.  Specifies the number of bits-per-pixel. The biBitCount member
								   // of the BITMAPINFOHEADER structure determines the number of bits that define each pixel
								   // and the maximum number of colors in the bitmap
    bmi.bmiHeader.biCompression = BI_RGB; // ( BI_RGB is an uncompressed format.) //Specifies the type of compression for a
										  // compressed bottom-up bitmap (top-down DIBs cannot be compressed).
    bmi.bmiHeader.biSizeImage = WindowRect.right * WindowRect.bottom * 4; // Specifies the size, in bytes, of the image. This may be
																// set to zero for BI_RGB bitmaps. 

    // create a DC for our bitmap -- the source DC for AlphaBlend  
	hdcwnd = GetDC(g_hwndBathy);
    hdc = CreateCompatibleDC(hdcwnd);

    // create our DIB (Device Independent Bitmap) section and select the bitmap into the dc 
    hbitmap = CreateDIBSection(hdc, &bmi, DIB_RGB_COLORS, &pvBits, NULL, 0x0);
    SelectObject(hdc, hbitmap);


	bitmapInf = SeedInf->bitmapInf;
	//memset(BitMapData, 0, sizeof(BYTE) * WindowRect.right * WindowRect.bottom);

	bathyExtemes = CBitmapEnvFunctions::GetBathyExtremes();
	range = bitmapInf.displayExtremes.slopeMax - bitmapInf.displayExtremes.slopeMin;

    ubRed = 0xff;
    ubGreen = 0x00;
    ubBlue = 0x00;

	// top-down, left-right
	for(y=0; y<WindowRect.bottom; y++)
	{
		// Calculate current latitude for the bitmaps row y.
		lat	= bitmapInf.displayExtremes.xMin + bitmapInf.latPerPixel * (WindowRect.bottom - 1 - y);
		_ASSERT(bitmapInf.dataExtremes.xMin <= lat);
		_ASSERT(bitmapInf.dataExtremes.xMax >= lat);

		// Calculate the index in the bitmap array for row y.  The number are tricky
		// because the bitmap is 2-dimensional and goes from top-down, left right while
		// the bitmap array is indexed linarly.
		// '.right' is the width of the window in pixels
		// '.bottom' is the height of the window, in pixels.
		//index = WindowRect.right * (WindowRect.bottom - 1 - y); // Incremented for each x.

		for(x=0; x<WindowRect.right; x++)
		{
			index = WindowRect.right * (WindowRect.bottom - 1 - y) + x;
			lon = bitmapInf.displayExtremes.yMin + bitmapInf.lonPerPixel * x;

			if(x == WindowRect.right-1)
				x = x;

			bathyValues = CBitmapEnvFunctions::GetBathymetryValues(lat, lon);

			if(bathyValues.slope < bitmapInf.displayExtremes.slopeMin || bathyValues.slope > bitmapInf.displayExtremes.slopeMax)
				ubAlpha = 0;
			else
				ubAlpha = 50 + (BYTE)(205*(bathyValues.slope - bitmapInf.displayExtremes.slopeMin)/range);

            fAlphaFactor = (float)ubAlpha / (float)0xff; 
            // multiply each pixel by fAlphaFactor, so each component  
            // is less than or equal to the alpha value. 
            ((UINT32 *)pvBits)[index] 
                = (ubAlpha << 24) |                       //0xaa000000 
                 ((UCHAR)(ubRed * fAlphaFactor) << 16) |  //0x00rr0000 
                 ((UCHAR)(ubGreen * fAlphaFactor) << 8) | //0x0000gg00 
                 ((UCHAR)(ubBlue * fAlphaFactor));      //0x000000bb 
		}


	}

    bf.BlendOp = AC_SRC_OVER;
    bf.BlendFlags = 0;
    bf.AlphaFormat = AC_SRC_ALPHA;   // use source alpha  
    bf.SourceConstantAlpha = 0xff;   // use constant alpha, with  
                                     // 75% opaqueness 
    if(!AlphaBlend(
		hdcwnd, // handle to destination DC
		0,//winWidth/5, // x-coord of upper-left corner
		0,//winHeight/5, // y-coord of upper-left corner
		WindowRect.right, // destination width
		WindowRect.bottom, // destination height
		hdc, // handle to source DC
		0, // x-coord of upper-left corner
		0, // y-coord of upper-left corner
		WindowRect.right, // source width
		WindowRect.bottom, // source height
		bf)) // alpha-blending function
        return hbitmap;                     // alpha blend failed 


    // do cleanup 
    //DeleteObject(hbitmap);
    DeleteDC(hdc);   
	ReleaseDC(g_hwndBathy, hdcwnd);

	return hbitmap;
}
*/

void MapScaleToBitmapPaletIndex(RECT WindowRect,
							   BYTE *BitMap, // Buffer that the bitmap is drawn from
							   int BitMapBufferLength, // Size of the buffer
							   BATHY_BITMAP_INF BitMapInf)
{
	LONG x,y, index; // pixel indices
	double value;
	double dColorIndex;
	BYTE colorIndex;
	double dataPercent;
	BATHY_BITMAP_INF bitMapInf;

	// Set the lowest of the data extremes to zero and adjust related data by same amount
	bitMapInf = AdjustBitmapInfForDisplay(&BitMapInf, -1*BitMapInf.dataExtremes.depthMin);

	memset(BitMap, 0, sizeof(BYTE) * WindowRect.right * WindowRect.bottom);

	//---------------------------------------------------------------------//
	// Quiet compiler warning until this is finished and working correctly
	BitMapBufferLength = BitMapBufferLength;
	//---------------------------------------------------------------------//

	// top-down, left-right
	//value = bitMapInf.dataExtremes.depthMax;
	for(y=0; y<WindowRect.bottom; y++)
	{
		// Calculate current depth 
		value = bitMapInf.dataExtremes.depthMax - y * bitMapInf.metersPerPixelDepth;

		// Calculate the index in the bitmap array for row y.  The number are tricky
		// because the bitmap is 2-dimensional and goes from top-down, left right while
		// the bitmap array is indexed linarly.
		// '.right' is the width of the window in pixels
		// '.bottom' is the height of the window, in pixels.
		index = WindowRect.right * (WindowRect.bottom - 1 - y);

		for(x=0; x<WindowRect.right; x++)
		{
			if(value >= bitMapInf.displayExtremes.depthMax)
			{
				BitMap[index] = (BYTE)(COLORINDEXSTART_BATHY + NUMCOLORS_BATHY - 1); // minus 1 for zero-based indexing
			}
			else if(value <= bitMapInf.displayExtremes.depthMin)
			{
				BitMap[index] = COLORINDEXSTART_BATHY;
			}
			else
			{
				dataPercent = (value - bitMapInf.displayExtremes.depthMin)/(bitMapInf.displayExtremes.depthMax - bitMapInf.displayExtremes.depthMin);
				dColorIndex = COLORINDEXSTART_BATHY + (NUMCOLORS_BATHY - 1) * dataPercent; // minus 1 for zero indexing
				_ASSERT(dColorIndex >= 0 && dColorIndex <= 255);

				colorIndex = (BYTE)staticLib.MyRound(dColorIndex); // value is a negative number
				_ASSERT(colorIndex < NUMCOLORS_BATHY);
				BitMap[index] = colorIndex;

				_ASSERT(colorIndex <= NUMCOLORS_BATHY + COLORINDEXSTART_BATHY);
				_ASSERT(colorIndex >= COLORINDEXSTART_BATHY);
			}
			index++;
		}
		//value -= bitMapInf.metersPerPixelDepth;
	}
}

void RedrawPolygon(HWND hWnd, BATHYBITMAP_WIN_PARAM *bathyGdata)
{
	int i;
	COLORREF cr = RGB(255, 255, 255);
	POLYGONINF *p = &bathyGdata->poly;

	if(p->isClosed == FALSE)
		cr = RGB(100, 100, 100);

	for(i=0; i<p->numVertices-1; i++)
		DrawLineTo(hWnd, bathyGdata, p->cArray[i], p->cArray[i+1], cr); 
}

void SetPolyMinMax(POLYGONINF *Poly)
{
	int i;
	short minX = 0, maxX = 0, minY = 0, maxY = 0;
	memset(&Poly->minMax, 0, sizeof(ENVMINMAX));

	if(Poly->numVertices > 1)
	{
		minX = maxX = Poly->cArray[0].X;
		minY = maxY = Poly->cArray[0].Y;
	}

	for(i=1; i<Poly->numVertices; i++)
	{
		if(minX > Poly->cArray[i].X)
			minX = Poly->cArray[i].X;

		if(maxX < Poly->cArray[i].X)
			maxX = Poly->cArray[i].X;

		if(minY > Poly->cArray[i].Y)
			minY = Poly->cArray[i].Y;

		if(maxY < Poly->cArray[i].Y)
			maxY = Poly->cArray[i].Y;
	}

	Poly->minMax.xMax = (double)maxX;
	Poly->minMax.xMin = (double)minX;
	Poly->minMax.yMax = (double)maxY;
	Poly->minMax.yMin = (double)minY;
}

int PlaybackIntegerToSeconds(int Integer, TCHAR *szBuff, size_t BuffSize)
{
	BOOL neg = FALSE;
	int sec;

	if(Integer < 0)
	{
		neg = TRUE;
		Integer++;
	}

	switch(abs(Integer))
	{
	case 0:
		sec = 1; // 1 iteration per update
		if(szBuff != NULL && BuffSize > 0)
			sprintf_s(szBuff, BuffSize, "Approx 1 Iteration per Second");
		break;
	case 1:
		sec = 2; // 2 iterations per update;
		if(szBuff != NULL && BuffSize > 0)
			sprintf_s(szBuff, BuffSize, "Approx 2 Iterations per Second");
		break;
	case 2:
		sec = 5; // 5 iterations per update;
		if(szBuff != NULL && BuffSize > 0)
			sprintf_s(szBuff, BuffSize, "Approx 5 Iterations per Second");
		break;
	case 3:
		sec = 10; // 10 iterations per update
		if(szBuff != NULL && BuffSize > 0)
			sprintf_s(szBuff, BuffSize, "Approx 10 Iterations per Second");
		break;
	case 4:
		sec = 30; // 30 iterations per update (1/2 min)
		if(szBuff != NULL && BuffSize > 0)
			sprintf_s(szBuff, BuffSize, "Approx 30 Iterations per Second");
		break;
	case 5:
		sec = 60; // 60 iterations per update (1 min);
		if(szBuff != NULL && BuffSize > 0)
			sprintf_s(szBuff, BuffSize, "Approx 60 Iterations per Second");
		break;
	case 6:
		sec = 2*60; // 120 iterations per update (2 min)
		if(szBuff != NULL && BuffSize > 0)
			sprintf_s(szBuff, BuffSize, "Approx 120 Iterations per Second");
		break;
	case 7:
		sec = 5*60; // 300 iterations per update (5 min)
		if(szBuff != NULL && BuffSize > 0)
			sprintf_s(szBuff, BuffSize, "Approx 300 Iterations (5 min) per Second");
		break;
	case 8:
		sec = 10*60; // 600 iterations per update (10 min);
		if(szBuff != NULL && BuffSize > 0)
			sprintf_s(szBuff, BuffSize, "Approx 600 Iterations (10 min) per Second");
		break;
	case 9:
		sec = 30*60; // 1800 iterations per update (30 min);
		if(szBuff != NULL && BuffSize > 0)
		sprintf_s(szBuff, BuffSize, "Approx 1800 Iterations (30 min) per Second");
			break;
	case 10:
		sec = 60*60; // 3600 iterations per update (1 hour);
		if(szBuff != NULL && BuffSize > 0)
		sprintf_s(szBuff, BuffSize, "Approx 3600 Iterations (1 hr) per Second");
			break;
	case 11:
		sec = 90*60; // 5400 iterations per update (1.5 hours)
		if(szBuff != NULL && BuffSize > 0)
			sprintf_s(szBuff, BuffSize, "Approx 5400 Iterations (1.5 hrs) per Second");
		break;
	case 12:
		sec = 2*60*60; // 7200 iterations per update (2 hours);
		if(szBuff != NULL && BuffSize > 0)
			sprintf_s(szBuff, BuffSize, "Approx 7200 Iterations (2 hrs) per Second");
		break;
	case 13:
		sec = 3*60*60; // 10800 iterations per update (3 hours);
		if(szBuff != NULL && BuffSize > 0)
			sprintf_s(szBuff, BuffSize, "Approx 10800 Iterations (3 hrs) per Second");
		break;
	default:
		sec = (abs(Integer) - 10)*60*60; // ((Integer - 10) hours)
		if(szBuff != NULL && BuffSize > 0)
			sprintf_s(szBuff, BuffSize, "Approx %d Iterations (%d hrs) per Second", sec, (abs(Integer) - 10));
		break;
	}

	if(neg == TRUE)
	{
		sec = -(sec);
		if(szBuff != NULL && BuffSize > 0)
			strcat_s(szBuff, BuffSize, " REWINDING");
	}
	return sec;
}

DWORD WINAPI AnimateThread(LPVOID lpParameter)
{
	DWORD lastTickCount = 0; // millisecs
	DWORD currentTickCount = 0; // millisecs
	int stateIndex = 0;
	//int prevStateIndex = 0;
	RECT rect;
	int indexMultiplier;

	ANIMATEANIMATPOPBITMAPTHREAD *inf = (ANIMATEANIMATPOPBITMAPTHREAD *)lpParameter;

	// Verify only a single instance of this thread is running.  Catch it in debug mode
	// with an assertion and handle it in release mode with a return.
	_ASSERT(inf->threadInf.running == FALSE);
	_ASSERT(inf->threadInf.exit == FALSE);

	// Indicate this thread is running and lock the mutex so no changes to the data buffer
	// are made.
	while(FALSE == g_mutexDrawBitmap.Lock(500))
		Sleep(100);
	inf->threadInf.running = TRUE;
	g_mutexDrawBitmap.Unlock();
	Sleep(100);

	// Want 4 visible updates per second.
	//      4 visible updatges per 1000 ms
	// 1000/4 = 250: make 200.

	lastTickCount = GetTickCount();
	while(inf->threadInf.exit == FALSE)
	{
		while(FALSE == g_mutexDrawBitmap.Lock(25))
			Sleep(25);

		indexMultiplier = PlaybackIntegerToSeconds(*inf->playbackRate, NULL, 0);
		switch(*inf->playState)
		{
		case STOP:
			lastTickCount = currentTickCount = GetTickCount();
			stateIndex = 0;
			break;
		case PLAY:
			currentTickCount = GetTickCount();
			stateIndex += (int)staticLib.MyRound((double)((indexMultiplier * (int)(currentTickCount - lastTickCount))/1000.0));
			while(stateIndex < 0)
				stateIndex += *inf->numStates;

			if(*inf->numStates != 0)
				stateIndex %= *inf->numStates;
			break;
		case PAUSE:
			lastTickCount = currentTickCount = GetTickCount();
			break;
		}

		if(inf->stateIndex != stateIndex)
		{
			inf->stateIndex = stateIndex;
			lastTickCount = currentTickCount;
			GetClientRect(g_hwndBathy, &rect);
			InvalidateRect(g_hwndBathy, &rect, FALSE);
		}

		g_mutexDrawBitmap.Unlock();

		if(*inf->playState == STOP)
			Sleep(1000);
		else
			Sleep(50);
	}



	// Post drawing bitmap finished.
	while(FALSE == g_mutexDrawBitmap.Lock(500))
		Sleep(500);
	inf->threadInf.hdl = NULL;
	inf->threadInf.id = 0;
	inf->threadInf.exit = FALSE;
	inf->threadInf.running = FALSE;
	g_mutexDrawBitmap.Unlock();
	return 0;
}

// This probably needs to be split into four separate threads.
DWORD WINAPI DisplayDataThread(LPVOID lpParameter)
{
	DRAWBITMAPTHREADINF *inf = NULL;
	RECT rect = {0};
	int numAnimats;
	// Local copies of input parameters to keep the code easier to understand.
	DISPLAYTYPE displayType;
	BOOL bBathy; // show bathmetry
	BOOL bSlope; // show slope
	BOOL bDataPts; // show data points;
	BOOL bAnimats; // show animats
	BYTE **pBathyBuffer;
	VOID **pSlopeBuffer;
	VOID **pDataPtsBuffer;
	VOID **pAnimatsBuffer;
	int *bufferLength;



	inf = (DRAWBITMAPTHREADINF *)lpParameter;;

	// Prevent user input until this thread finishes.
	EnableWindow(g_hwndBathy, FALSE);
	EnableWindow(g_hDlgSeed, FALSE);
	EnableWindow(g_hwndDepthScale, FALSE);
	EnableWindow(g_hwndSlopeScale, FALSE);


	//BYTE *pByte;

	// Verify only a single instance of this thread is running.  Catch it in debug mode
	// with an assertion and handle it in release by waiting.
	_ASSERT(inf->pDisplayLayers != NULL);
	//_ASSERT(inf->disableMouse != NULL); // see if this can be removed.
	//_ASSERT(inf->highLightPlaybackList != NULL);
	_ASSERT(inf->threadInf != NULL);
	_ASSERT(inf->phbitmapBathy != NULL);

	_ASSERT(inf->threadInf->running == FALSE);
	_ASSERT(inf->threadInf->exit == FALSE);



	// Indicate this thread is running and lock the mutex so no changes to the data buffer
	// are made.
	while(inf->threadInf->running == TRUE || FALSE == g_mutexDrawBitmap.Lock(500))
		Sleep(500);
	inf->threadInf->running = TRUE;

	//-------------------------------------------------------------------------------------//
	// To keep code easy to understand copy certain references to shorter local variables.
	//----------------------------------------------------------------------------------//
	displayType = inf->pDisplayLayers->displayType;
	bBathy = *inf->pDisplayLayers->bathy.enabled;
	bSlope = *inf->pDisplayLayers->slope.enabled;
	bDataPts = *inf->pDisplayLayers->datapts.enabled;
	bAnimats = *inf->pDisplayLayers->animats.enabled;

	pBathyBuffer = &inf->pDisplayLayers->bathy.buffer[displayType];
	pSlopeBuffer = &inf->pDisplayLayers->slope.buffer[displayType];
	pDataPtsBuffer = &inf->pDisplayLayers->datapts.buffer[displayType];
	pAnimatsBuffer = &inf->pDisplayLayers->animats.buffer[displayType];
	bufferLength = &inf->pDisplayLayers->buffLen[displayType];

	// Force a memory realoocation if this is a zoom.
	if(displayType == ZOOM && inf->bResize == TRUE)
		*bufferLength = 0;
	//----------------------------------------------------------------------------------//

	//-------------------------------------------------------------------------------------//
	// Resize and relocate the bitmap windows.
	//----------------------------------------//
	if(inf->bResize == TRUE)
	{
		SetWindowPos(g_hwndDepthScale, HWND_TOP, inf->layout.depth.x, inf->layout.depth.y, inf->layout.depth.width+2, inf->layout.depth.height+2, SWP_SHOWWINDOW);
		Sleep(10);
		SetWindowPos(g_hwndSlopeScale, HWND_TOP, inf->layout.slope.x, inf->layout.slope.y, inf->layout.slope.width+2, inf->layout.slope.height+2, SWP_SHOWWINDOW);
		Sleep(10);
		SetWindowPos(g_hwndBathy, HWND_TOP, NULL, NULL, inf->layout.bathy.width+2, inf->layout.bathy.height+2,
			SWP_NOMOVE|SWP_SHOWWINDOW);
		Sleep(10);

		// Update the size of the bitmap in the LPBITMAPINFO struct.
		BmpInitInfo((LPBITMAPINFO)inf->pBitMapInfo, inf->layout.bathy.width, inf->layout.bathy.height);
	}

	GetClientRect(g_hwndBathy, &rect);
	_ASSERT(rect.right == inf->layout.bathy.width);
	_ASSERT(rect.bottom == inf->layout.bathy.height);
	//-------------------------------------------------------------------------------------//

	//-------------------------------------------------------------------------------------//
	// In case the user minimized or maximized but the layout was slightly different than
	// when initially drawn - a last defense.
	if(displayType != ZOOM && *bufferLength != inf->layout.bathy.width * inf->layout.bathy.height)
	{
		// Bathymetry data
		if(*pBathyBuffer != NULL)
			delete [] *pBathyBuffer;
		*pBathyBuffer = NULL;

		// Slope data
		if(*pSlopeBuffer != NULL)
			free(*pSlopeBuffer);
		*pSlopeBuffer = NULL;

		// Data points
		if(*pDataPtsBuffer != NULL)
			free(*pDataPtsBuffer);
		*pDataPtsBuffer = NULL;

		// Animat tracks
		if(*pAnimatsBuffer != NULL)
			free(*pAnimatsBuffer);
		*pAnimatsBuffer = NULL;
	}

	//-------------------------------------------------------------------------------------//


	//-------------------------------------------------------------------------------------//
	// Update the bathymetry data display
	//-----------------------------------//
	// The process that launched this thread needs to have set *pBathyBuffer to NULL if a redraw is needed.
	_ASSERT(bBathy == TRUE); // currently there is no reason for this to not be true.
	if(bBathy == TRUE && (*pBathyBuffer == NULL || *bufferLength < inf->layout.bathy.width * inf->layout.bathy.height))
	{
		// Deallocate bitmap data storage and storage for it's (pure) copy.
		if(*pBathyBuffer != NULL)
			delete [] *pBathyBuffer;
		_ASSERT(inf->layout.bathy.width == GetValidBitmapWidth(inf->layout.bathy.width));
		*pBathyBuffer = new BYTE[inf->layout.bathy.width * inf->layout.bathy.height];
		memset(*pBathyBuffer, 0, inf->layout.bathy.width * inf->layout.bathy.height);

		// Update the data buffer
		BathymetryDataToBuffer(rect, *pBathyBuffer, &inf->pSeedInf, &inf->threadInf->exit);
	}


	//-------------------------------------------------------------------------------------//
	// Update the slope-heading data display
	//--------------------------------------//
	if(bSlope == TRUE && (*pSlopeBuffer == NULL || *bufferLength < inf->layout.bathy.width * inf->layout.bathy.height))
	{
		if(*pSlopeBuffer != NULL)
			free(*pSlopeBuffer);
		*pSlopeBuffer = malloc(inf->layout.bathy.width * inf->layout.bathy.height * 4);
			
		// Update the data buffer
		SlopeDataToBuffer(rect, &inf->pSeedInf, *pSlopeBuffer);
	}


	//-------------------------------------------------------------------------------------//
	// Update the datapoints data display
	//--------------------------------------//
	// The process that launched this thread needs to have set *pDataPtsBuffer to NULL if a 
	// redraw is needed.
	if(bDataPts && (*pDataPtsBuffer == NULL || *bufferLength < inf->layout.bathy.width * inf->layout.bathy.height))
	{
		if(*pDataPtsBuffer != NULL)
			free(*pDataPtsBuffer);
		*pDataPtsBuffer = malloc(inf->layout.bathy.width * inf->layout.bathy.height * 4);

		ENVDATAPOINTCOUNT cnt = CBitmapEnvFunctions::GetDataPointCounts(FALSE);
		RAWENVIRONMENTALDATA ed = {new double[cnt.x], new double[cnt.y], NULL, NULL, NULL, cnt.x, cnt.y, 0, 0};
		CBitmapEnvFunctions::GetRawDataCopy(&ed, FALSE);
		DataPointsToBuffer(rect, &ed, inf->pSeedInf.bitmapInf.displayExtremes, *pDataPtsBuffer);
		CBitmapEnvFunctions::DeallocateRawDataCopyMemory(&ed);
	}


	//-------------------------------------------------------------------------------------//
	// Update the animats data display
	//--------------------------------------//
	numAnimats = CBitmapEnvFunctions::GetAnimatCount();
	if(inf->usageState == SEED_SCENARIO_STATE)
	{
		if(bAnimats && (*pAnimatsBuffer == NULL || *bufferLength < inf->layout.bathy.width * inf->layout.bathy.height))
		{
			if(*pAnimatsBuffer != NULL)
				free(*pAnimatsBuffer);
			*pAnimatsBuffer = malloc(inf->layout.bathy.width * inf->layout.bathy.height * 4);
		}
		memset(*pAnimatsBuffer, 0, inf->layout.bathy.width * inf->layout.bathy.height * 4);
		AnimatsToBitmap(rect, *pAnimatsBuffer, &inf->pSeedInf, numAnimats, g_inhabBuffInf.popBuff, g_inhabBuffInf.numHighlightedAnimats, g_inhabBuffInf.highlightBuff);

	}
	else if(inf->usageState == PLAYBACK_STATE)
	{
		// The process that launched this thread needs to have set *pDataPtsBuffer to NULL if a
		// redraw is needed.
		if(bAnimats && (*pAnimatsBuffer == NULL || *bufferLength < inf->layout.bathy.width * inf->layout.bathy.height))
		{
			if(*pAnimatsBuffer != NULL)
				free(*pAnimatsBuffer);
			*pAnimatsBuffer = malloc(inf->layout.bathy.width * inf->layout.bathy.height * 4);

		}
		g_playBackHighLightList.Lock();
		AnimatsToDataBuffer(rect, *pAnimatsBuffer, &inf->pSeedInf, inf->pSeedInf.playbackInf->sce.numSaveIterations,
			inf->pSeedInf.playbackInf->sce.totalNumAnimats, inf->pSeedInf.playbackInf->animatState, &g_playBackHighLightList);
		g_playBackHighLightList.Unlock();

	}

	// Update the buffer length.
	*bufferLength = inf->layout.bathy.width * inf->layout.bathy.height;


	// All of this gets moved out.
/*
	// Alpha blend all the data.
	 DeleteObject(*inf->phbitmapBathy);
	*inf->phbitmapBathy = 
		FinalizeBitmap(inf->pBitMapInfo, bBathy, *pBathyBuffer, bSlope, *pSlopeBuffer, bDataPts, *pDataPtsBuffer, bAnimats, *pAnimatsBuffer);


	//VOID *dog = malloc(inf->layout.bathy.width * inf->layout.bathy.height * 4);
	if(bSlope == TRUE && *pSlopeBuffer != NULL)
	{
		//BlendAlphaBlendIntoBitmap(rect, dog);
		BlendAlphaBlendIntoBitmap(rect, *pSlopeBuffer);

		{
			HDC hdcBathy = GetDC(g_hwndBathy); // Get a handle to the Device Context associated with this window,
			HDC hdc2 = CreateCompatibleDC(hdcBathy);

			DeleteObject(*inf->phbitmapBathy);
			*inf->phbitmapBathy = CreateCompatibleBitmap(hdcBathy, rect.right, rect.bottom);

			// Select original back in if this works.
			if(NULL != SelectObject(hdc2, *inf->phbitmapBathy))
			{
				if(!BitBlt(hdc2, 
					   0,0, 
					   rect.right, rect.bottom, 
					   hdcBathy, 
					   0,0, 
					   SRCCOPY))
				{
					hdc2 = hdc2;
				}

				ReleaseDC(g_hwndBathy,hdcBathy);
				DeleteDC(hdc2);
			}
		}
	}
*/

	// Generating bitmap data is done.  Handle shutting down thread..
	inf->threadInf->hdl = NULL;
	inf->threadInf->id = 0;
	inf->threadInf->exit = FALSE;
	inf->threadInf->running = FALSE;
	g_mutexDrawBitmap.Unlock();

	EnableWindow(g_hwndBathy, TRUE);
	EnableWindow(g_hDlgSeed, TRUE);
	EnableWindow(g_hwndDepthScale, TRUE);
	EnableWindow(g_hwndSlopeScale, TRUE);


	// Tell the bitmap to update (comment back in after debugging)
	SendMessage(g_hwndBathy, WM_UPDATE_BITMAP, NULL, NULL);

	//InvalidateRect(g_hwndBathy, &rect, FALSE);
	return 0;
}

// DeleteObject(s_hbitmapBathy); needs to be called before calling this.
// s_hbitmapBathy = 
//HBITMAP FinalizeBitmap(BATHYBITMAPINFO *pBitMapInfo, BOOL bBathy, BYTE *Bathy, BOOL bSlope, VOID *Slope, BOOL bDataPts, VOID *DataPt, BOOL bAnimat, VOID *Animat)
HBITMAP FinalizeBitmap(BATHYBITMAPINFO *pBitMapInfo, BOOL bBathy, BYTE *Bathy)
{
	HBITMAP hbitmap;
	HDC hdcBathy;
	RECT rect;

	bBathy = bBathy;


	//----------------------------------------------------------------------------------//
	// (1) Create the initial bathymetry bitmap
	//-----------------------------------------//
	// Get the DC associated with the bitmap window.
	hdcBathy = GetDC(g_hwndBathy);

	// Create a device dependant bitmap based upon the information about the bitmap
	// being drawn, based upon the compatable DC in memory created above, and based
	// upon the data (var s_bathyData) supplied to it.
	// From the Microsoft Documentation: "The CreateDIBitmap function creates a
	// compatible bitmap (DDB) from a DIB and, optionally, sets the bitmap bits."
	hbitmap = CreateDIBitmap(hdcBathy,
							(LPBITMAPINFOHEADER)pBitMapInfo,
							 CBM_INIT, Bathy,
							(LPBITMAPINFO)pBitMapInfo,
							 DIB_RGB_COLORS);
	ReleaseDC(g_hwndBathy, hdcBathy);

	// Won't work anyway.
	GetClientRect(g_hwndBathy, &rect);
	InvalidateRect(g_hwndBathy, &rect, FALSE);
	Sleep(100);

/*
	// Paint it.
	{//////////////////////////////////////////////////
		PAINTSTRUCT ps;
		HDC hdc2;
		HGDIOBJ hgdiobj;

		HDC hdcBathy = BeginPaint(g_hwndBathy, &ps); // not allowed.
		GetClientRect(g_hwndBathy, &rect);

		if(NULL == (hdc2 = CreateCompatibleDC(hdcBathy)))
		{
			EndPaint(g_hwndBathy, &ps);
			//prevRect = rect;
			//break;
		}

		if(NULL == (hgdiobj = SelectObject(hdc2, hbitmap)))
		{
			EndPaint(g_hwndBathy, &ps);
			//prevRect = rect;
			//break;
		}


		if(!BitBlt(hdcBathy, 0, 0, rect.right, rect.bottom, hdc2, 0, 0, SRCCOPY))
			MessageBox(NULL,"BitBlt Failed!","Error",MB_ICONWARNING);


		SelectObject(hdc2, hgdiobj);
		DeleteDC(hdc2);
		EndPaint(g_hwndBathy, &ps);

	}/////////////////////////////////////////////////

	// (2) Alpha blend the slope data

	// (3) Alpha blend the data point data

	// (4) Alpha blend the animat data.
*/
	return hbitmap;
}