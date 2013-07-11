#ifndef ALPHABLENDEXAMPLE_H
#define ALPHABLENDEXAMPLE_H

#include <windows.h>
#include <commctrl.h>
#include <winuser.h>
#include "3mb.h"
#include "3mbBitmapFunctions.h"
#include "3mbGuiFunctions.h"
#include "3mbScaleBitmapWinProc.h"
#include "resource.h"

void DrawAlphaBlendExample(HWND hWnd, HDC hdcwnd);
void DrawAlphaBlend(HWND hWnd, HDC hdcwnd);


#endif //ALPHABLENDEXAMPLE_H