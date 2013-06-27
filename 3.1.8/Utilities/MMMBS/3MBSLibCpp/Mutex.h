// Mutex.h: interface for the CMutex class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_MUTEX_H__53BF20C8_F372_4D12_B6BD_651983F22B1A__INCLUDED_)
#define AFX_MUTEX_H__53BF20C8_F372_4D12_B6BD_651983F22B1A__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <windows.h>

#ifndef SIZE_64
#define SIZE_64 64
#endif

class CMutex  
{
public:
	CMutex();
	virtual ~CMutex();

	BOOL  Initialize(TCHAR *szID, TCHAR *szMutexName = NULL);
	BOOL  IsMutexLocked();
	void  Unlock();
	BOOL  Lock(DWORD WaitTicks = INFINITE);
	int   GetLockCount();
	void  Close();
	TCHAR* MutexStatusAsString(TCHAR *szBuff, int nBufferLength);

private:
	// Increments with each instance of a Class CMutex.  Permits each mutex to have its
	// own unique identification.
	static int	s_mutexID;

	// A universal mutex handle for all instances of class CMutex.  Ensures that only a
	// single instance of CMutex is created or destroyed at a time.
	static HANDLE s_mutexSharedHandle;

	HANDLE  m_mutex;
	BOOL	m_locked;
	int		m_lockCount;
	TCHAR    m_mutexID[SIZE_64];


};

#endif // !defined(AFX_MUTEX_H__53BF20C8_F372_4D12_B6BD_651983F22B1A__INCLUDED_)
