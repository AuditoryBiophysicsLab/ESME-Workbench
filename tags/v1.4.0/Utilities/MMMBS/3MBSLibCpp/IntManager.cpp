#include "IntManager.h"

int CIntManager::GetValue()
{
	int ret;
	m_mutex.Lock(INFINITE);
	ret = m_value;
	m_mutex.Unlock();
	return ret;
}

CIntManager::CIntManager(void)
{
	memset(m_mutexID, 0, SIZE_64);
	m_mutex.Initialize(NULL, NULL);
	m_value = 0;
}

CIntManager::~CIntManager(void)
{
	m_mutex.Close();
}

void CIntManager::Add(int Value)
{
	m_mutex.Lock(INFINITE);
	m_value += Value;
	m_mutex.Unlock();
}

void CIntManager::Subtract(int Value)
{
	m_mutex.Lock(INFINITE);
	m_value -= Value;
	m_mutex.Unlock();
}

void CIntManager::Reset(int Value)
{
	m_mutex.Lock(INFINITE);
	m_value = Value;
	m_mutex.Unlock();
}


BOOL CIntManager::Initialize(TCHAR *szID, TCHAR *szMutexName)
{
	// Close the current mutex if any, then open a new one.
	m_mutex.Close();
	if(FALSE == m_mutex.Initialize(szID, szMutexName))
		return FALSE;
	m_mutex.Lock(INFINITE);
	m_value = 0;
	m_mutex.Unlock();
	return TRUE;
}



// For threaded applications this function Lock(), along with function Unlock(), must be
// called when the calling function accesses and manipulats the item held in the list
// and should be called (but is not necessary) when iteration through loops that access
// the linked list such as Add(), Get(), Insert(), and so on.
BOOL CIntManager::Lock(DWORD WaitMilliSeconds)
{
	m_mutex.Lock(WaitMilliSeconds);
	return TRUE;
}

// This Unlock() function must be called every time Lock() is called after the calling
// function is finished manipulating the linked list or items held in it.
BOOL CIntManager::Unlock()
{
	m_mutex.Unlock();
	return TRUE;
}


void CIntManager::CloseMutex()
{
	// Close the current mutex if any, then open a new one.
	m_mutex.Close();
}
