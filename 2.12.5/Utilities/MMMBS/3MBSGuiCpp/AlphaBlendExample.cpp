#include "AlphaBlendExample.h"
extern HWND g_hwndBathy;
void DrawAlphaBlendExample(HWND hWnd, HDC hdcwnd)
{
    HDC hdc;               // handle of the DC we will create  
    BLENDFUNCTION bf;      // structure for alpha blending 
    HBITMAP hbitmap;       // bitmap handle 
    BITMAPINFO bmi;        // bitmap header 
    VOID *pvBits;          // pointer to DIB section 
    ULONG winWidth, winHeight; // window width/height 
    ULONG bitmapWidth, bitmapHeight; // bitmap width/height 
    RECT rt;            // used for getting window dimensions 
    UINT32 x,y;          // stepping variables 
    UCHAR ubAlpha;         // used for doing transparent gradient 
    UCHAR ubRed;        
    UCHAR ubGreen;
    UCHAR ubBlue;
    float fAlphaFactor;    // used to do premultiply 
            
    // get window dimensions 
    GetClientRect(hWnd, &rt);
    
    // calculate window width/height 
    winWidth = rt.right - rt.left;  
    winHeight = rt.bottom - rt.top;  

    // make sure we have at least some window size 
    if((!winWidth) || (!winHeight))
        return;

    // divide the window into 3 horizontal areas 
    winHeight = winHeight / 3;

    // create a DC for our bitmap -- the source DC for AlphaBlend  
    hdc = CreateCompatibleDC(hdcwnd);
    
    // zero the memory for the bitmap info 
    ZeroMemory(&bmi, sizeof(BITMAPINFO));

    // Setup bitmap info  
    // Set the bitmap width and height to 60% of the width and height of each of the three
	// horizontal areas. Later on, the blending will occur in the center of each of the
	// three areas. 
    bmi.bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
    bmi.bmiHeader.biWidth = bitmapWidth = winWidth - (winWidth/5)*2; // 1.0 - 0.40 = .60  // width of the bitmap
    bmi.bmiHeader.biHeight = bitmapHeight = winHeight - (winHeight/5)*2; // height of the bitmap
    bmi.bmiHeader.biPlanes = 1; // Specifies the number of planes for the target device. This value must be set to 1. 
    bmi.bmiHeader.biBitCount = 32; // four 8-bit components.  Specifies the number of bits-per-pixel. The biBitCount member
								   // of the BITMAPINFOHEADER structure determines the number of bits that define each pixel
								   // and the maximum number of colors in the bitmap
    bmi.bmiHeader.biCompression = BI_RGB; // ( BI_RGB is an uncompressed format.) //Specifies the type of compression for a
										  // compressed bottom-up bitmap (top-down DIBs cannot be compressed).
    bmi.bmiHeader.biSizeImage = bitmapWidth * bitmapHeight * 4; // Specifies the size, in bytes, of the image. This may be
																// set to zero for BI_RGB bitmaps. 



    // create our DIB (Device Independent Bitmap) section and select the bitmap into the dc 
    hbitmap = CreateDIBSection(hdc, &bmi, DIB_RGB_COLORS, &pvBits, NULL, 0x0);
    SelectObject(hdc, hbitmap);

	//----------------------------------------------------------------------------------//
	// Top Window
	//-----------//
    // in top window area, constant alpha = 50%, but no source alpha 
    // the color format for each pixel is 0xaarrggbb  
    // set all pixels to blue and set source alpha to zero 
    for (y = 0; y < bitmapHeight; y++)
        for (x = 0; x < bitmapWidth; x++)
            ((UINT32 *)pvBits)[x + y * bitmapWidth] = 0x000000ff; 

    bf.BlendOp = AC_SRC_OVER;
    bf.BlendFlags = 0;
    bf.SourceConstantAlpha = 0x7f;  // half of 0xff = 50% transparency 
    bf.AlphaFormat = 0;             // ignore source alpha channel 

    if(!AlphaBlend(
		hdcwnd, // handle to destination DC
		winWidth/5, // x-coord of upper-left corner
		winHeight/5, // y-coord of upper-left corner
		bitmapWidth, // destination width
		bitmapHeight, // destination height
		hdc, // handle to source DC
		0, // x-coord of upper-left corner
		0, // y-coord of upper-left corner
		bitmapWidth, // source width
		bitmapHeight, // source height
		bf)) // alpha-blending function
        return;                     // alpha blend failed 
	//----------------------------------------------------------------------------------//



	//----------------------------------------------------------------------------------//
	// Middle Window
	//--------------//
    // in middle window area, constant alpha = 100% (disabled), source  
    // alpha is 0 in middle of bitmap and opaque in rest of bitmap  
    for (y = 0; y < bitmapHeight; y++)
	{
        for (x = 0; x < bitmapWidth; x++)
		{
            if ((x > (int)(bitmapWidth/5)) && (x < (bitmapWidth-bitmapWidth/5)) &&
                (y > (int)(bitmapHeight/5)) && (y < (bitmapHeight-bitmapHeight/5)))
			{
                //in middle of bitmap: source alpha = 0 (transparent). 
                // This means multiply each color component by 0x00. 
                // Thus, after AlphaBlend, we have a, 0x00 * r,  
                // 0x00 * g,and 0x00 * b (which is 0x00000000) 
                // for now, set all pixels to red 
                ((UINT32 *)pvBits)[x + y * bitmapWidth] = 0x80ff0000; // was 0x00ff0000
			}
            else
			{
                // in the rest of bitmap, source alpha = 0xff (opaque)  
                // and set all pixels to blue  
                ((UINT32 *)pvBits)[x + y * bitmapWidth] = 0xff0000ff; // was 0xff0000ff
			}
		}
	}


    bf.BlendOp = AC_SRC_OVER;
    bf.BlendFlags = 0;
    bf.AlphaFormat = AC_SRC_ALPHA;  // use source alpha  
    bf.SourceConstantAlpha = 0x70;  // opaque (disable constant alpha) 
   
    if (!AlphaBlend(hdcwnd, winWidth/5, winHeight/5+winHeight, bitmapWidth, bitmapHeight, hdc, 0, 0, bitmapWidth, bitmapHeight, bf))
        return;
	//----------------------------------------------------------------------------------//


	//----------------------------------------------------------------------------------//
	// Bottom Window
	//--------------//
    // bottom window area, use constant alpha = 75% and a changing 
    // source alpha. Create a gradient effect using source alpha, and  
    // then fade it even more with constant alpha 
    ubRed = 0x00;
    ubGreen = 0x00;
    ubBlue = 0xff;
    
    for (y = 0; y < bitmapHeight; y++)
        for (x = 0; x < bitmapWidth; x++)
        {
            // for a simple gradient, base the alpha value on the x  
            // value of the pixel  
            ubAlpha = (UCHAR)((float)x / (float)bitmapWidth * 255);
            //calculate the factor by which we multiply each component 
            fAlphaFactor = (float)ubAlpha / (float)0xff; 
            // multiply each pixel by fAlphaFactor, so each component  
            // is less than or equal to the alpha value. 
            ((UINT32 *)pvBits)[x + y * bitmapWidth] 
                = (ubAlpha << 24) |                       //0xaa000000 
                 ((UCHAR)(ubRed * fAlphaFactor) << 16) |  //0x00rr0000 
                 ((UCHAR)(ubGreen * fAlphaFactor) << 8) | //0x0000gg00 
                 ((UCHAR)(ubBlue   * fAlphaFactor));      //0x000000bb 
        }

    bf.BlendOp = AC_SRC_OVER;
    bf.BlendFlags = 0;
    bf.AlphaFormat = AC_SRC_ALPHA;   // use source alpha  
    bf.SourceConstantAlpha = 0xbf;   // use constant alpha, with  
                                     // 75% opaqueness 

    AlphaBlend(hdcwnd, winWidth/5, 
               winHeight/5+2*winHeight, bitmapWidth, 
               bitmapHeight, hdc, 0, 0, bitmapWidth, 
               bitmapHeight, bf);
	//----------------------------------------------------------------------------------//



    // do cleanup 
    DeleteObject(hbitmap);
    DeleteDC(hdc);   
}



void DrawAlphaBlend(HWND hWnd, HDC hdcwnd)
{
    HDC hdc;               // handle of the DC we will create  
    BLENDFUNCTION bf;      // structure for alpha blending 
    HBITMAP hbitmap;       // bitmap handle 
    BITMAPINFO bmi;        // bitmap header 
    VOID *pvBits;          // pointer to DIB section 
    ULONG winWidth, winHeight; // window width/height 
    ULONG bitmapWidth, bitmapHeight; // bitmap width/height 
    RECT rt;            // used for getting window dimensions 
    UINT32 x,y;          // stepping variables 
    UCHAR ubAlpha;         // used for doing transparent gradient 
    UCHAR ubRed;        
    UCHAR ubGreen;
    UCHAR ubBlue;
    float fAlphaFactor;    // used to do premultiply

	long index;
	//COLORREF cr = 0;
//UINT32 r,g,b;

    // get window dimensions 
    GetClientRect(hWnd, &rt);
    
    // calculate window width/height 
    winWidth = rt.right - rt.left;  
    winHeight = rt.bottom - rt.top;  

    // make sure we have at least some window size 
    if((!winWidth) || (!winHeight))
        return;

    // divide the window into 3 horizontal areas 
    //winHeight = winHeight / 3;

    // create a DC for our bitmap -- the source DC for AlphaBlend  
    hdc = CreateCompatibleDC(hdcwnd);
    
    // zero the memory for the bitmap info 
    ZeroMemory(&bmi, sizeof(BITMAPINFO));

    // Setup bitmap info  
    // Set the bitmap width and height to 60% of the width and height of each of the three
	// horizontal areas. Later on, the blending will occur in the center of each of the
	// three areas. 
	bitmapWidth = winWidth;// - (winWidth/5)*2; // 1.0 - 0.40 = .60  // width of the bitmap
	bitmapHeight = winHeight;// - (winHeight/5)*2; // height of the bitmap

    bmi.bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
    bmi.bmiHeader.biWidth = bitmapWidth;//
    bmi.bmiHeader.biHeight = bitmapHeight;//
    bmi.bmiHeader.biPlanes = 1; // Specifies the number of planes for the target device. This value must be set to 1. 
    bmi.bmiHeader.biBitCount = 32; // four 8-bit components.  Specifies the number of bits-per-pixel. The biBitCount member
								   // of the BITMAPINFOHEADER structure determines the number of bits that define each pixel
								   // and the maximum number of colors in the bitmap
    bmi.bmiHeader.biCompression = BI_RGB; // ( BI_RGB is an uncompressed format.) //Specifies the type of compression for a
										  // compressed bottom-up bitmap (top-down DIBs cannot be compressed).
    bmi.bmiHeader.biSizeImage = bitmapWidth * bitmapHeight * 4; // Specifies the size, in bytes, of the image. This may be
																// set to zero for BI_RGB bitmaps. 



    // create our DIB (Device Independent Bitmap) section and select the bitmap into the dc 
    hbitmap = CreateDIBSection(hdc, &bmi, DIB_RGB_COLORS, &pvBits, NULL, 0x0);
    SelectObject(hdc, hbitmap);

	//----------------------------------------------------------------------------------//
	// Top Window
	//-----------//
    // in top window area, constant alpha = 50%, but no source alpha 
    // the color format for each pixel is 0xaarrggbb  
    // set all pixels to blue and set source alpha to zero
#if 0
    for (y = 0; y < bitmapHeight; y++)
	{
        for (x = 0; x < bitmapWidth; x++)
		{
			index = bitmapWidth * (bitmapHeight - 1 - y) + x;
            //((UINT32 *)pvBits)[x + y * bitmapWidth] = RGB(0, 0, 255);
            ((UINT32 *)pvBits)[x + y * bitmapWidth] = 0x000000ff; // blue
            //((UINT32 *)pvBits)[x + y * bitmapWidth] = 0x0000ff00; // green
            //((UINT32 *)pvBits)[x + y * bitmapWidth] = 0x00ff0000; // red

			//((UINT32 *)pvBits)[x + y * bitmapWidth] = GetPixel(hdcwnd, x, y);
			if(cr != GetPixel(hdcwnd, x, y))
				cr = GetPixel(hdcwnd, x, y);
			r = GetRValue(cr);
			g = GetGValue(cr);
			b = GetBValue(cr);

			if((r !=255 || g != 255 || b != 255) && (r != g && r != b && g != b))
				r = r;

			//((UINT32 *)pvBits)[index] = GetPixel(hdcwnd, x, y);
			//((UINT32 *)pvBits)[index] = cr;
			//((UINT32 *)pvBits)[index] = RGB(b, g, r);
			//((UINT32 *)pvBits)[index] = RGB(b, g, 255);
		}
	}

    bf.BlendOp = AC_SRC_OVER;
    bf.BlendFlags = 0;
    bf.SourceConstantAlpha = 0x7f;  // half of 0xff = 50% transparency 
    bf.AlphaFormat = 0;             // ignore source alpha channel 

    if(!AlphaBlend(
		hdcwnd, // handle to destination DC
		0,//winWidth/5, // x-coord of upper-left corner
		0,//winHeight/5, // y-coord of upper-left corner
		bitmapWidth, // destination width
		bitmapHeight, // destination height
		hdc, // handle to source DC
		0, // x-coord of upper-left corner
		0, // y-coord of upper-left corner
		bitmapWidth, // source width
		bitmapHeight, // source height
		bf)) // alpha-blending function
        return;                     // alpha blend failed 
	//----------------------------------------------------------------------------------//
#endif


#if 0
	//----------------------------------------------------------------------------------//
	// Middle Window
	//--------------//
    // in middle window area, constant alpha = 100% (disabled), source  
    // alpha is 0 in middle of bitmap and opaque in rest of bitmap  
    for (y = 0; y < bitmapHeight; y++)
	{
        for (x = 0; x < bitmapWidth; x++)
		{
            if ((x > (int)(bitmapWidth/5)) && (x < (bitmapWidth-bitmapWidth/5)) &&
                (y > (int)(bitmapHeight/5)) && (y < (bitmapHeight-bitmapHeight/5)))
			{
                //in middle of bitmap: source alpha = 0 (transparent). 
                // This means multiply each color component by 0x00. 
                // Thus, after AlphaBlend, we have a, 0x00 * r,  
                // 0x00 * g,and 0x00 * b (which is 0x00000000) 
                // for now, set all pixels to red 
                ((UINT32 *)pvBits)[x + y * bitmapWidth] = 0x80ff0000; // was 0x00ff0000
			}
            else
			{
                // in the rest of bitmap, source alpha = 0xff (opaque)  
                // and set all pixels to blue  
                ((UINT32 *)pvBits)[x + y * bitmapWidth] = 0xff0000ff; // was 0xff0000ff
			}
		}
	}


    bf.BlendOp = AC_SRC_OVER;
    bf.BlendFlags = 0;
    bf.AlphaFormat = AC_SRC_ALPHA;  // use source alpha  
    bf.SourceConstantAlpha = 0x70;  // opaque (disable constant alpha) 
   
    if (!AlphaBlend(hdcwnd, winWidth/5, winHeight/5+winHeight, bitmapWidth, bitmapHeight, hdc, 0, 0, bitmapWidth, bitmapHeight, bf))
        return;
	//----------------------------------------------------------------------------------//
#endif
#if 1
	//----------------------------------------------------------------------------------//
	// Bottom Window
	//--------------//
    // bottom window area, use constant alpha = 75% and a changing 
    // source alpha. Create a gradient effect using source alpha, and  
    // then fade it even more with constant alpha 
    ubRed = 0x00;
    ubGreen = 0x00;
    ubBlue = 0x00;
    
    for (y = 0; y < bitmapHeight; y++)
	{
        for (x = 0; x < bitmapWidth; x++)
        {
			index = bitmapWidth * (bitmapHeight - 1 - y) + x;
			//index = x + y * bitmapWidth;

            // for a simple gradient, base the alpha value on the x  
            // value of the pixel
			if(index %5 == 0 || index % 6 == 0 || index % 7 == 0 || index % 8 == 0 || index %9 == 0)
				ubAlpha = 0;
			else
				ubAlpha = (UCHAR)((float)x / (float)bitmapWidth * 255);
            //calculate the factor by which we multiply each component 
            fAlphaFactor = (float)ubAlpha / (float)0xff; 
            // multiply each pixel by fAlphaFactor, so each component  
            // is less than or equal to the alpha value. 
            ((UINT32 *)pvBits)[index] 
                = (ubAlpha << 24) |                       //0xaa000000 
                 ((UCHAR)(ubRed * fAlphaFactor) << 16) |  //0x00rr0000 
                 ((UCHAR)(ubGreen * fAlphaFactor) << 8) | //0x0000gg00 
                 ((UCHAR)(ubBlue   * fAlphaFactor));      //0x000000bb 
        }
	}

    bf.BlendOp = AC_SRC_OVER;
    bf.BlendFlags = 0;
    bf.AlphaFormat = AC_SRC_ALPHA;   // use source alpha  
    bf.SourceConstantAlpha = 0xbf;   // use constant alpha, with  
                                     // 75% opaqueness 

    //AlphaBlend(hdcwnd, winWidth/5, 
      //         winHeight/5+2*winHeight, bitmapWidth, 
        //       bitmapHeight, hdc, 0, 0, bitmapWidth, 
          //     bitmapHeight, bf);

    if(!AlphaBlend(
		hdcwnd, // handle to destination DC
		0,//winWidth/5, // x-coord of upper-left corner
		0,//winHeight/5, // y-coord of upper-left corner
		bitmapWidth, // destination width
		bitmapHeight, // destination height
		hdc, // handle to source DC
		0, // x-coord of upper-left corner
		0, // y-coord of upper-left corner
		bitmapWidth, // source width
		bitmapHeight, // source height
		bf)) // alpha-blending function
        return;                     // alpha blend failed 
#endif
	//----------------------------------------------------------------------------------//



    // do cleanup 
    DeleteObject(hbitmap);
    DeleteDC(hdc);   
}