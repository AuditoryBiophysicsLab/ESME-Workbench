// irThread.h: interface for the CirThread class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_IRTHREAD_H__498A8A33_3966_423D_9247_568590C002D1__INCLUDED_)
#define AFX_IRTHREAD_H__498A8A33_3966_423D_9247_568590C002D1__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

// From:
// http://www.codeproject.com/threads/ThreadClass.asp

#include <windows.h>

struct IRunnable
{
	virtual void RunThread1() = 0;
	virtual void RunThread2(){};
	virtual void RunThread3(TCHAR *szFileTitle, TCHAR *szFileName)
	{
		// Quiet compiler warnings
		szFileTitle = szFileTitle;
		szFileName = szFileName;
	};
};

class CirThread  
{
public:
	BOOL m_thread1Running;
	BOOL m_thread1RunningACK;
	BOOL m_thread2Running;
	BOOL m_thread3Running;

	CirThread()
	{
		_threadObj = NULL;
	}

	CirThread(IRunnable *ptr)
	{
		_threadObj = ptr;
	}

	void SetIRunnablePointer(IRunnable *ptr)
	{
		_threadObj = ptr;
	}
	
	void StartThread1()
	{
		if(_threadObj == NULL)
			return;
		// use the Win32 API here
		DWORD threadID;
		::CreateThread(0, 0, ThreadProc1, _threadObj, 0, &threadID);
	}

	void StartThread2()
	{
		if(_threadObj == NULL)
			return;

		// use the Win32 API here
		DWORD threadID;
		::CreateThread(0, 0, ThreadProc2, _threadObj, 0, &threadID);
	}

	void StartThread3()
	{
		if(_threadObj == NULL)
			return;

		// use the Win32 API here
		DWORD threadID;
		::CreateThread(0, 0, ThreadProc3, _threadObj, 0, &threadID);
	}


protected:
	// Win32 compatible thread parameter and procedure 
	IRunnable *_threadObj; 
	
	static unsigned long __stdcall ThreadProc1(void* ptr)
	{
		((IRunnable*)ptr)->RunThread1();
		return 0;
	}   


	static unsigned long __stdcall ThreadProc2(void* ptr)
	{
		((IRunnable*)ptr)->RunThread2();
		return 0;
	}   

	static unsigned long __stdcall ThreadProc3(void* ptr)
	{
		((IRunnable*)ptr)->RunThread3(NULL, NULL);
		return 0;
	}   

private:

};

#endif // !defined(AFX_IRTHREAD_H__498A8A33_3966_423D_9247_568590C002D1__INCLUDED_)
