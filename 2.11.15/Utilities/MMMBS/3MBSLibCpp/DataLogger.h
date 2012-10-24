// DataLogger.h: interface for the CDataLogger class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_DATALOGGER_H__43B55BC3_8E27_4BC4_88E6_9C3BE4FB0EAE__INCLUDED_)
#define AFX_DATALOGGER_H__43B55BC3_8E27_4BC4_88E6_9C3BE4FB0EAE__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <stdio.h>
#include "IRunnable.h"
//#include "LinkedList.h"
//#include "datatypes.h"
#include "ListManager.h"
#include "3mbsLib.h"
#include "Mutex.h"

#define SIZE_128 128

typedef struct OUTPUT
{
	TCHAR sz[SIZE_128];
	SYSTEMTIME time;
	BOOL timeStampWanted;
}OUTPUT;

class CDataLogger : public IRunnable  
{
public:
	BOOL SetOutputDirectory(const TCHAR *szNewOutputDir);
	BOOL CreateLogFile(TCHAR *szFileName, BOOL CreateNewFile = TRUE, TCHAR *szPath = NULL);
	CDataLogger();
	virtual ~CDataLogger();

	void LogDebugMessage(TCHAR *Message);
	void LogMessage(TCHAR *Message, BOOL IncludeTimeStamp = TRUE);
	void Exit();
	BOOL IsRunning();
	void RunThread1();

private:
	BOOL OutputMessageToFile(OUTPUT *pOut);
	BOOL		m_running;
	BOOL		m_exit;
	CListManager <OUTPUT> m_listMgr;
	//CMutex		m_logMutex;
	FILE		*m_outFd;
	TCHAR		m_szSharedBuffer[SIZE_128];
	TCHAR		m_szFileOutTitle[SIZE_128];
	TCHAR		m_szFileOutPath[SIZE_128];

};

#endif // !defined(AFX_DATALOGGER_H__43B55BC3_8E27_4BC4_88E6_9C3BE4FB0EAE__INCLUDED_)
