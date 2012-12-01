#pragma once

#ifndef CLISTMANAGER_H
#define CLISTMANAGER_H
#include "LinkedList.h"
#include "Mutex.h"

#ifndef SIZE_64
#define SIZE_64 64
#endif


template <class T>
class CListManager
{
public:
	CListManager(void);
	~CListManager(void);
	BOOL Initialize(TCHAR *szID, TCHAR *szMutexName = NULL);
	void CloseMutex(); // Meant for debugging only.
	BOOL Lock();
	BOOL Unlock();

	T* GetListDataInBuffer(int *BufferLen);
	void SetBufferDataIntoList(int BufferLen, T *Buffer);

	void Add(T *t); // Is Thread safe for functions manipulating the list item T
	T*	 Add();// Not thread safe for function manipulating the list data item T

	BOOL Delete(int Index);
	void DeleteAll();
	//T   *Get();
	BOOL Get(int Index, T *t);// Is Thread safe for functions manipulating the list item T
	T*	 Get(int Index);// Not thread safe for function manipulating the list data item T
	BOOL Set(int Index, T *t);// Is Thread safe for functions manipulating the list item T
	BOOL Insert(int Index, T *t);// Is Thread safe for functions manipulating the list item T
	T* Insert(int Index);// Not thread safe for function manipulating the list data item T
	int  Length();
	int  Move(int From, int To);
	//T   *Next() ;
	//T   *Previous() ;
	//int  SetTo(int Index) ;
	//int  SetToBegining() ;
	//int  SetToEnd() ; 
	//int  Switch(int Location1, int Location2) ;


private:
	LinkedList <T> List;
	CMutex m_mutex;
	TCHAR   m_mutexID[SIZE_64];
};



template <class T>
CListManager <T>::CListManager(void)
{
	memset(m_mutexID, 0, SIZE_64);
}

template <class T>
CListManager <T>::~CListManager(void)
{
	m_mutex.Close();
}


template <class T>
T* CListManager <T>::GetListDataInBuffer(int *BufferLen)
{
	T* t;
	m_mutex.Lock();
	t = List.GetListDataInBuffer(BufferLen);
	m_mutex.Unlock();
	return t;
}

template <class T>
void CListManager <T>::SetBufferDataIntoList(int BufferLen, T *Buffer)
{
	int i;
	m_mutex.Lock();
	List.DeleteAll();
	for(i=0; i<BufferLen; i++)
		*List.Add() = Buffer[i];

	// This needs work.
	//List.SetBufferDataIntoList(BufferLen, Buffer);
	m_mutex.Unlock();
}


// For threaded applications this function Lock(), along with function Unlock(), must be
// called when the calling function accesses and manipulats the item held in the list
// and should be called (but is not necessary) when iteration through loops that access
// the linked list such as Add(), Get(), Insert(), and so on.
template <class T>
BOOL CListManager <T>::Lock()
{
	m_mutex.Lock();
	return TRUE;
}

// This Unlock() function must be called every time Lock() is called after the calling
// function is finished manipulating the linked list or items held in it.
template <class T>
BOOL CListManager <T>::Unlock()
{
	m_mutex.Unlock();
	return TRUE;
}


template <class T>
void CListManager <T>::CloseMutex()
{
	// Close the current mutex if any, then open a new one.
	m_mutex.Close();
}

template <class T>
BOOL CListManager <T>::Initialize(TCHAR *szID, TCHAR *szMutexName)
{
	// Close the current mutex if any, then open a new one.
	m_mutex.Close();
	return m_mutex.Initialize(szID, szMutexName);
}


template <class T>
void CListManager <T>::Add(T *t)
{
	m_mutex.Lock();
	memcpy(List.Add(), t, sizeof(T));
	m_mutex.Unlock();
}

// Not thread safe for function manipulating the list data item T
template <class T>
T* CListManager <T>::Add()
{
	T* t;
	m_mutex.Lock();
	t = List.Add();
	m_mutex.Unlock();
	return t;
}


template <class T>
BOOL CListManager <T>::Delete(int Index)
{
	BOOL ret = FALSE;
	m_mutex.Lock();
	if(Index < List.Length())
	{
		List.Delete(Index);
		ret = TRUE;
	}
	m_mutex.Unlock();
	return ret;
}

template <class T>
void CListManager <T>::DeleteAll()
{
	m_mutex.Lock();
	List.DeleteAll();
	m_mutex.Unlock();
}

template <class T>
BOOL Set(int Index, T *t)
{
	BOOL ret = FALSE;
	m_mutex.Lock();
	if(Index < List.Length())
	{
		memcpy(List.Get(location), t, sizeof(T));
		ret = TRUE;
	}
	m_mutex.Unlock();
	return ret;
}

template <class T>
BOOL CListManager <T>::Get(int Index, T *t)
{
	BOOL ret = FALSE;

	memset(t, 0, sizeof(T));
	m_mutex.Lock();
	if(Index < List.Length())
	{
		memcpy(t, List.Get(Index), sizeof(T));
		ret = TRUE;
	}
	m_mutex.Unlock();
	return ret;
}

// Not thread safe for function manipulating the list data item T
template <class T>
T* CListManager <T>::Get(int Index)
{
	T* t = NULL;
	m_mutex.Lock();
	if(Index < List.Length())
		t = List.Get(Index);
	m_mutex.Unlock();
	return t;
}

template <class T>
BOOL CListManager <T>::Insert(int Index, T *t)
{
	BOOL ret = FALSE;
	m_mutex.Lock();
	if(Index <= List.Length())
	{
		memcpy(List.Insert(Index), t, sizeof(T));
		ret = TRUE;
	}
	m_mutex.Unlock();
	return ret;
}

// Not thread safe for function manipulating the list data item T
template <class T>
T* CListManager <T>::Insert(int Index)
{
	T* t=NULL;
	m_mutex.Lock();
	if(Index <= List.Length())
		t = List.Insert(Index);
	m_mutex.Unlock();
	return t;
}


template <class T>
int CListManager <T>::Length()
{
	int t;
	m_mutex.Lock();
	t = List.Length();
	m_mutex.Unlock();
	return t;
}



template <class T>
int CListManager <T>::Move(int From, int To)
{
	int k;
	m_mutex.Lock();
	k = List.Move(From, To);
	m_mutex.Unlock();
	return k;
}

#endif//CLISTMANAGER_H