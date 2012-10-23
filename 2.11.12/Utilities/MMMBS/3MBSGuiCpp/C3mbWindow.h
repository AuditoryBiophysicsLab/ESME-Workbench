#ifndef C3MBWINDOW_H
#define C3MBWINDOW_H
#pragma once

#include <windows.h>
#include <commctrl.h>
#include <winuser.h>
#include "3mb.h"


class C3mbWindow
{
public:
	C3mbWindow(HWND ParentWindow, int ContrlID);
	~C3mbWindow(void);

	HWND GetParentHandle();
	HWND GetWindowHandle();
	int GetControlID();

	int GetX();
	int GetY();
	int GetHeight();
	int GetWidth();

	BOOL SetX(int Value, BOOL Repaint);
	BOOL SetY(int Value, BOOL Repaint);
	BOOL SetHeight(int Value, BOOL Repaint);
	BOOL SetWidth(int Value, BOOL Repaint);
	BOOL SetLocation(int X, int Y, BOOL Repaint);
	BOOL SetDimensions(int Width, int Height, BOOL Repaint);
	BOOL SetLocationAndDimensions(int X, int Y, int Width, int Height, BOOL Repaint);


private:
	HWND m_parent;
	int m_cntrlID;
	HWND m_hWnd;
//	RECT m_client;
//	RECT m_window;

	POINT m_p;

	int m_width;
	int m_height;
};


#endif // C3MBWINDOW_H