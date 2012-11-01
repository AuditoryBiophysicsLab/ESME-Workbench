// DataLogger.cpp: implementation of the CDataLogger class.
//
//////////////////////////////////////////////////////////////////////

#include "DataLogger.h"
#include "staticLib.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CDataLogger::CDataLogger()
{
	memset(&m_szSharedBuffer, 0, SIZE_128);
	memset(&m_szFileOutTitle, 0, SIZE_128);
	memset(&m_szFileOutPath, 0, SIZE_128);

	m_listMgr.Initialize("DataLogger");

	m_outFd	  = NULL;
	m_running = FALSE;
	m_exit	  = FALSE;
	memset(&m_szSharedBuffer, 0, SIZE_128);
}

CDataLogger::~CDataLogger()
{
	fflush(NULL);
	m_exit = TRUE;

	if(m_outFd != NULL)
		fclose(m_outFd);
}

void CDataLogger::LogMessage(TCHAR *Message, BOOL IncludeTimeStamp)
{
	OUTPUT outputPtr;
	outputPtr.timeStampWanted = IncludeTimeStamp;
	_snprintf_s(outputPtr.sz, sizeof(outputPtr.sz), sizeof(outputPtr.sz),"%s\n", Message);
	GetLocalTime(&outputPtr.time);
	m_listMgr.Add(&outputPtr);
}

// Set CreateNewFile to TRUE for a new file, FALSE to append to existing.
BOOL CDataLogger::CreateLogFile(TCHAR *szFileName, BOOL CreateNewFile, TCHAR *szPath)
{
	C3mbStaticsLib staticLib;

	m_listMgr.Lock();

	if(m_outFd != NULL)
	{
		m_listMgr.Unlock();
		return FALSE;
	}

	// GetPathAndFileTitleFromFileName() will separate the file name from the file path if
	// 'szFileName' was passed in with both the path and file title.  Otherwise it will
	// justplace the file title into 'm_szFileOutTitle' and the current directory into
	// 'm_szFileOutPath'
	staticLib.GetPathAndFileTitleFromFileName(szFileName, m_szFileOutPath, SIZE_128, m_szFileOutTitle, SIZE_128);
	if(strlen(m_szFileOutTitle) == 0)
	{
		m_listMgr.Unlock();
		return FALSE;
	}

	// If szPath was passed in, it overrides the path passed in with szFileName or the
	// current directory placed into 'm_szFileOutPath' by the call to
	// GetPathAndFileTitleFromFileName().
	if(szPath != NULL)
		strncpy_s(m_szFileOutPath, sizeof(m_szFileOutPath), szPath, SIZE_128);

	if(strlen(m_szFileOutPath) > 0)
		_snprintf_s(m_szSharedBuffer, sizeof(m_szSharedBuffer), sizeof(m_szSharedBuffer), "%s\\%s", m_szFileOutPath, m_szFileOutTitle);
	else
		strncpy_s(m_szSharedBuffer, sizeof(m_szSharedBuffer), m_szFileOutTitle, SIZE_128);

	if(CreateNewFile == TRUE)
		fopen_s(&m_outFd, m_szSharedBuffer, "w");
	else
		fopen_s(&m_outFd, m_szSharedBuffer, "a");

	if(m_outFd == NULL)
	{
		m_listMgr.Unlock();
		return FALSE;
	}

	m_listMgr.Unlock();
	return TRUE;
}


void CDataLogger::Exit()
{
	m_exit = TRUE;
	while(m_running == TRUE)
		Sleep(1);
}

BOOL CDataLogger::IsRunning()
{
	return m_running;
}

// this doesn't appear to be implemented as a thread, or at least not properly.  Look into it.
void CDataLogger::RunThread1()
{
	int	listLength, i;
	OUTPUT o;

	m_running = TRUE;

	// Run the thread.
	while(m_exit == FALSE)
	{
		Sleep(250);
		if(m_outFd == NULL)
			continue;

		m_listMgr.Lock();
		listLength = m_listMgr.Length();
		for(i=0; i<listLength; i++)
		{
			m_listMgr.Get(i, &o);
			OutputMessageToFile(&o);
		}
		m_listMgr.DeleteAll();
		m_listMgr.Unlock();
	}


	// The application has exited.  Empty out any remaining messages.
	m_listMgr.Lock();
	listLength = m_listMgr.Length();
	for(i=0; i<listLength && m_outFd != NULL; i++)
	{
		m_listMgr.Get(i, &o);
		OutputMessageToFile(&o);
	}
	m_listMgr.DeleteAll();
	m_listMgr.Unlock();

	fclose(m_outFd);
	m_outFd = NULL;
	m_running = FALSE;
}

BOOL CDataLogger::SetOutputDirectory(const TCHAR *szNewOutputDir)
{
	TCHAR	szBuff1[SIZE_128];
	TCHAR	szBuff2[SIZE_128];
	int		listLength, i;
	TCHAR	*szTemp = "3mbsEsmeEibTempLog.txt";
	OUTPUT	o;


	// (1) If the log file name isn't set, then simply return.
	if(strlen(m_szFileOutTitle) == 0)
		return FALSE;


	// (2) If the new directory is the same as the old directory, simply return without
	// doing anything.
	if(strlen(m_szFileOutPath) == 0)
	{
		GetCurrentDirectory(SIZE_128, m_szFileOutPath);
		if(strncmp(m_szFileOutPath, szNewOutputDir, SIZE_128) == 0)
			return TRUE;
	}
	else
	{
		if(strncmp(m_szFileOutPath, szNewOutputDir, SIZE_128) == 0)
			return TRUE;
	}

	// (3) If the new directory doesn't exist create it
	CreateDirectory(szNewOutputDir, NULL);


	// (4) If the data log file isn't open yet, just set the path member variable to the
	// passed in variable.
	if(m_outFd == NULL)
	{
		strncpy_s(m_szFileOutPath, sizeof(m_szFileOutPath), szNewOutputDir, SIZE_128);
		return TRUE;
	}

	// (5) Grab the mutex
	m_listMgr.Lock();

	// (6) Write out any remaining messages
	listLength = m_listMgr.Length();
	for(i=0; i<listLength; i++)
	{
		m_listMgr.Get(i, &o);
		OutputMessageToFile(&o);
	}
	m_listMgr.DeleteAll();
	m_listMgr.Unlock();


	// (7) Close the current file.
	fclose(m_outFd);
	m_outFd = NULL;

	// (8) Make a copy of the current file into the new directory under a different name
	_snprintf_s(szBuff1, sizeof(szBuff1), sizeof(szBuff1), "%s\\%s", m_szFileOutPath, m_szFileOutTitle); // current file
	_snprintf_s(szBuff2, sizeof(szBuff2), sizeof(szBuff2), "%s\\%s", szNewOutputDir, szTemp); // new file
	CopyFile(szBuff1, szBuff2, FALSE);

	// (9) Delete a previous file with the same name if there still is one (won't be if
	// its the same directory as before.  Doesnt' matter if this function fails or not.
	DeleteFile(szBuff1);

	// (10) Rename the copy with the different name
	_snprintf_s(szBuff1, sizeof(szBuff1), sizeof(szBuff1), "%s\\%s", szNewOutputDir, m_szFileOutTitle); // current
	MoveFile(szBuff2, szBuff1);

	// (10) Update the member variable m_szFileOutPath
	strncpy_s(m_szFileOutPath, sizeof(m_szFileOutPath), szNewOutputDir, SIZE_128);

	// (11) Reopen the file in append mode.
	_snprintf_s(szBuff1, sizeof(szBuff1), sizeof(szBuff1), "%s\\%s", szNewOutputDir, m_szFileOutTitle);
	fopen_s(&m_outFd , szBuff1, "a");

	return TRUE;
}

BOOL CDataLogger::OutputMessageToFile(OUTPUT *pOut)
{
	TCHAR szBuff[SIZE_128];

	if(pOut->timeStampWanted == TRUE)
	{
		sprintf_s(szBuff, sizeof(szBuff), "%02d:%02d:%02d:%03d ", pOut->time.wHour, pOut->time.wMinute,
			pOut->time.wSecond, pOut->time.wMilliseconds);
		fprintf(m_outFd, szBuff);
	}
	fprintf(m_outFd, pOut->sz);
	fflush(NULL);
	return TRUE;
}
