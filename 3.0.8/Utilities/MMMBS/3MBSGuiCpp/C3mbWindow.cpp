
#include "C3mbWindow.h"

C3mbWindow::C3mbWindow(HWND ParentWindow, int ContrlID)
{
	RECT r;

	m_parent = ParentWindow;
	m_cntrlID = ContrlID;
	m_hWnd = GetDlgItem(ParentWindow, ContrlID);

	GetClientRect(m_hWnd, &r);
	m_width = r.right;
	m_height = r.bottom;

	GetWindowRect(m_hWnd, &r);
	m_p.x = r.left;
	m_p.y = r.top;

	ScreenToClient(m_parent, &m_p);
}

C3mbWindow::~C3mbWindow(void)
{
}


HWND C3mbWindow::GetParentHandle(){ return m_parent; }
HWND C3mbWindow::GetWindowHandle(){ return m_hWnd; }
int C3mbWindow::GetControlID(){ return m_cntrlID; }

// Locations are realtive to parent window.
int C3mbWindow::GetX() { return m_p.x; }
int C3mbWindow::GetY() { return m_p.y; }
int C3mbWindow::GetHeight() { return m_height; }
int C3mbWindow::GetWidth() { return m_width; }

BOOL C3mbWindow::SetX(int Value, BOOL Repaint) { return SetLocationAndDimensions(Value, m_p.y, m_width, m_height, Repaint); }
BOOL C3mbWindow::SetY(int Value, BOOL Repaint) { return SetLocationAndDimensions(m_p.x, Value, m_width, m_height, Repaint); }
BOOL C3mbWindow::SetWidth(int Value, BOOL Repaint) { return SetLocationAndDimensions(m_p.x, m_p.y, Value, m_height, Repaint); }
BOOL C3mbWindow::SetHeight(int Value, BOOL Repaint) { return SetLocationAndDimensions(m_p.x, m_p.y, m_width, Value, Repaint); }
BOOL C3mbWindow::SetLocation(int X, int Y, BOOL Repaint) { return SetLocationAndDimensions(X, Y, m_width, m_height, Repaint); }
BOOL C3mbWindow::SetDimensions(int Width, int Height, BOOL Repaint) { return SetLocationAndDimensions(m_p.x, m_p.y, Width, Height, Repaint); }

BOOL C3mbWindow::SetLocationAndDimensions(int X, int Y, int Width, int Height, BOOL Repaint)
{
	if(FALSE == MoveWindow(m_hWnd, X, Y, Width, Height, Repaint))
		return FALSE;

	m_p.x = X;
	m_p.y = Y;
	m_width = Width;
	m_height = Height;

	return TRUE;
}
