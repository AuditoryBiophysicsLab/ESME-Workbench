#ifndef _3MBSYSTEMCHECK_H
#define _3MBSYSTEMCHECK_H

#include "3mb.h"

typedef struct RunDiagnosticThreadParam
{
	THREAD_INF threadInf;
	CScenario *sce;
}RUNDIANOSTICTHREADPARAM;

DWORD WINAPI RunDiagnosticThreadProc(LPVOID lpParameter);

#endif