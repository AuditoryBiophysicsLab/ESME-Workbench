// Mutex.cpp: implementation of the CMutex class.
//
//////////////////////////////////////////////////////////////////////

#include "Mutex.h"
#include <stdio.h>

const TCHAR szLocked[]   = "LOCKED";
const TCHAR szUnlocked[] = "UNLCKD";


// Static Vars
int	CMutex::s_mutexID = 0;
HANDLE CMutex::s_mutexSharedHandle = CreateMutex(NULL, FALSE, NULL);

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CMutex::CMutex()
{
	m_locked	= FALSE;
	m_lockCount = 0;
	m_mutex		= NULL;
	memset(m_mutexID, 0, SIZE_64);

	// Automatically create the mutex associated with this instance.
	Initialize(NULL, NULL);
}

CMutex::~CMutex()
{
	Close();
}

TCHAR* CMutex::MutexStatusAsString(TCHAR *szBuff, int nBufferLength)
{
	TCHAR szp[16];

	if(szBuff == NULL)
		return NULL;

	if(m_locked)
		strcpy_s(szp, 16, szLocked);
	else
		strcpy_s(szp, 16, szUnlocked);

	_snprintf_s(szBuff, nBufferLength, nBufferLength, "Mutex (%s) %s, lock cnt(%d)", m_mutexID, szp, m_lockCount);

	return szBuff;
}


// Input param szMutexName is for creating named mutex across applications
BOOL CMutex::Initialize(TCHAR *szID, TCHAR *szMutexName)
{
	// Close the current mutex associated with this instance.
	Close();

	// Grab the Static mutex shared among all instances of this class so
	// mutex count can be incremented and this class can get a unique IF
	// if no ID name was passed in.
	if(WaitForSingleObject(s_mutexSharedHandle, INFINITE) != WAIT_TIMEOUT)
	{
		if(szMutexName == NULL)
			sprintf_s(m_mutexID, sizeof(m_mutexID), "%d", s_mutexID);
		else
			sprintf_s(m_mutexID, sizeof(m_mutexID), "%s", szID);
		s_mutexID++;
		ReleaseMutex(s_mutexSharedHandle);
	}
	else
		return FALSE;

	// Now actually create the mutex for this class instance.
	if(INVALID_HANDLE_VALUE == (m_mutex = CreateMutex(NULL, FALSE, szMutexName)))
	{
		m_mutex = NULL;
		return FALSE;
	}
	return TRUE;
}

void CMutex::Close()
{
	if(m_mutex == NULL)
		return;

	while(m_lockCount > 0)
		Unlock();

	CloseHandle(m_mutex);	// Memory map mutex

	m_locked	= FALSE;
	m_lockCount = 0;
	m_mutex		= NULL;
	memset(&m_mutexID, 0, SIZE_64);

	// Grab the Static mutex shared among all instances of this class so
	// mutex count can be decremented.
	if(WaitForSingleObject(s_mutexSharedHandle, INFINITE) != WAIT_TIMEOUT)
	{
		s_mutexID--;
		ReleaseMutex(s_mutexSharedHandle);
	}
}

BOOL CMutex::Lock(DWORD WaitTicks)
{
	if(m_mutex == NULL)
		return FALSE;

	if(WaitForSingleObject(m_mutex, WaitTicks) == WAIT_TIMEOUT)
		return FALSE;

	m_locked = TRUE;
	m_lockCount++;
	return TRUE;
}



void CMutex::Unlock()
{
	if(m_mutex == NULL)
		return;

	ReleaseMutex(m_mutex);
	if(m_lockCount > 0)
		m_lockCount--;

	if(m_lockCount == 0)
		m_locked = FALSE;

}


BOOL CMutex::IsMutexLocked()
{
	return m_locked;
}

BOOL CMutex::GetLockCount()
{
	return m_lockCount;
}