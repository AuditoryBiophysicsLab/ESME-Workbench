#include "3mbSystemCheck.h"


DWORD WINAPI RunDiagnosticThreadProc(LPVOID lpParameter)
{
	// Variable declaration and initialization
	RUNDIANOSTICTHREADPARAM *param = (RUNDIANOSTICTHREADPARAM *)lpParameter;
	THREAD_INF *threadInf = &param->threadInf;
	CScenario *sce = param->sce;
	USERPARAMS userParams = {0};

	// Assertions
	_ASSERT(threadInf->hdl != NULL);
	_ASSERT(threadInf->id != 0);
	_ASSERT(threadInf->exit == FALSE);
	_ASSERT(threadInf->running == FALSE);
	_ASSERT(sce != NULL);
	_ASSERT(sce->BathymetryLoaded() == TRUE);
	_ASSERT(sce->GetAnimatCount() > 0);


	// Initialize thread stuff


	// Indicate thread is running
	threadInf->running = TRUE;

	// Enter while-loop
	while(threadInf->exit == FALSE)
	{
		// Create subscenarios from the one currently loaded in.

		// Run scenarios that do not get saved to file.
		userParams = sce->GetConfiguration();
		userParams.output.enabled = FALSE; // HOD THIS though
		sce->SetConfiguration(userParams);
		sce->SetDuration(3600*24); // three days
		sce->RunScenario();

		while(sce->IsActive() == FALSE)
			Sleep(1);

		while(sce->IsActive() == TRUE)
			Sleep(10);

		break;
	}

	// Deinitialize

	// Indicate thread is done running
	threadInf->running = FALSE;

	// Exit thread.
	return 0;
}