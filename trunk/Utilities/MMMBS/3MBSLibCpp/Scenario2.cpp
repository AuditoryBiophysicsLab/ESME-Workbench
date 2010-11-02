//
//////////////////////////////////////////////////////////////////////
#include <process.h>
#include "Scenario.h"
#include "3mbsLib.h"
#include "DataLogger.h"
#include "scenario.h"
#include "Windows.h"
#include ".\scenario.h"
#include "dataTypes_statsAnalysis.h"
#include "dataTypes_SpeciesGroup.h"
#include "params.h"
#include "EnvironmentalDataStatic.h"


//**************************************************************************************//
// RunScenario Functions
//		RunScenario()
//		RunThread1()
//		_RunScenario()
//**************************************************************************************//
/*
  If NumSteps is 0 or positive, that number of steps will be run.
  If NumSteps is negative, the entire scenario is run and additional calls to
  RunScenario() are ignored until the scenario completes.
*/
// Called by user
// User may enter -1 to finish run.
RESLT CScenario::RunScenario(int NumIterations)
{
#ifndef NOCSENARIOFUNCTIONS
	if(m_exit == TRUE)
		return OK;

	//while(m_throttleIterations != 0)
		//Sleep(2);

	// Since this is called by an external thread lock the active mutex.
	m_mbsActiveMutex.Lock(INFINITE);
	m_state.errorStatus = OK;

	// RunScenario() may be called only when the system is either not running
	// (FINISHED) or paused (RUNPAUSED) waiting on additional iterations
	// to be set by the call to this function.
	if(m_state.activity != __RUN_FINISHED && m_state.activity != ___SCE_PAUSED)
	{
		m_mbsActiveMutex.Unlock();
		return ALREADYRUNNING_ERROR;
	}

	// Can only be at this point in the code if m_throttleIterations equals zero.  The
	// code needs to have been written properly for m_runState_old to be set to either
	// FINISHED or RUNPAUSED, so do an assertion check.  Regardless of the
	// value m_throttleIterations is set to in this function (if it gets set) it is reset
	// back to zero when the application launches and at the end of a simulation (both
	// aborted and finished)
	_ASSERTE(m_throttleIterations == 0);

	// Check the run status.  The run simulation thread can only be launched if the run
	// state is on standby/finished.
	if(m_state.activity == __RUN_FINISHED)
	{
		// Properly written code will have m_runThread.m_thread1Running set to FALSE here.
		_ASSERTE(m_runThread.m_thread1Running == FALSE);

		m_throttleIterations = NumIterations;

		m_runThread.StartThread1();

		// Don't release the mutex until the thread has indicated it is running.
		while(m_runThread.m_thread1Running == FALSE || m_state.activity == __RUN_FINISHED)
			Sleep(1);
		m_mbsActiveMutex.Unlock();

		m_runThread.m_thread1RunningACK = TRUE;

		// Don't return until the thread is ready for the next input.
		return OK; // OK status
	}

	// Properly written code will have m_runThread.m_thread1Running set to TRUE here.
	_ASSERTE(m_runThread.m_thread1Running == TRUE);

	// Set the next number quantity of iterations the simulation is to run.
	m_throttleIterations = NumIterations;
	m_mbsActiveMutex.Unlock();

	m_mbsPausedMutex.Lock();
	m_mbsPausedMutex.Unlock();
#endif//NOCSENARIOFUNCTIONS
	return OK;
}
