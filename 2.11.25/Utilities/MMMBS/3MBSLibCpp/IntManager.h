#ifndef INTMANAGER_H
#define INTMANAGER_H
#include "Mutex.h"


class CIntManager
{
public:
	CIntManager(void);
	~CIntManager(void);

	BOOL Initialize(TCHAR *szID, TCHAR *szMutexName = NULL);
	void CloseMutex(); // Meant for debugging only.
	BOOL Lock(DWORD WaitMilliSeconds = INFINITE);
	BOOL Unlock();

	void Add(int Value = 1);
	void Subtract(int Value = 1);
	void Reset(int Value = 0);
	int GetValue();


private:
	CMutex m_mutex;
	TCHAR m_mutexID[SIZE_64];
	int m_value;
};




#endif // INTMANAGER_H