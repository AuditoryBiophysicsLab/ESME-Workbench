// Scenario.cpp: implementation of the CScenario class.
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



//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////
CScenario::CScenario()
{
#ifndef NOCSENARIOFUNCTIONS
	_ASSERTE(sizeof(TAKESTATS)%16 == 0);
	_ASSERTE(sizeof(TAKE)%16 == 0);

	m_SpeTakesCopy = NULL;

	SZSPECIESNAMEBUFFER_LATIN[2][1];

	//----------------------------------------------------------------------------------//
	// Scenario Setup/Configuration
	//-----------------------------//
	// Sets the configuration to minimum
	memset(&m_mbSce, 0, sizeof(SCENARIOPARAMS));
	m_mbSce.numSaveIterations = 1; // When application launches the number of saved iterations is the duration (which is zero) + 1;

	m_firstAnimatStateHoldArray = NULL;
	m_animatStateHoldArrayLength = 0;

	// Non user configurable values
	m_mbSce.libVerSuper = MMBSLIB_VERSION_SUPER;
	m_mbSce.libVerSub = MMBSLIB_VERSION_SUB;

	m_mbSce.outputVerSuper = MMMBLIB_BINOUTPUT_VERSION_SUPER;
	m_mbSce.outputVerSub = MMMBLIB_BINOUTPUT_VERSION_SUB;

	// Binary file output
	m_mbSce.user.output.enabled = TRUE;
	m_mbSce.user.output.outputByTime = TRUE;
	m_staticScenario.SetMinBinOutFileConfiguration(&m_mbSce.user.output);

	m_mbSce.speciesGroupCnt = NUM_SPEGROUPS_INUSE;
	memset(&m_mbSce.speciesGroup, 0, sizeof(SCEPARMSSPECIESGROUP) * m_mbSce.speciesGroupCnt);

	// The order here must match the layout the C# file SpeciesModel.cs in the 'speciesNameIndex 'accessor,
	strcpy_s(m_mbSce.speciesGroup[MYSTICETE].szGroup,
		sizeof(m_mbSce.speciesGroup[MYSTICETE].szGroup),
		SZSPECIESGROUPNAMEBUFFER[MYSTICETE]);
	m_mbSce.speciesGroup[MYSTICETE].lvlAphys = 215.0;
	m_mbSce.speciesGroup[MYSTICETE].lvlBphys = 195.0;
	m_mbSce.speciesGroup[MYSTICETE].lvlBBeh_RiskA = 8; // Risk transition sharpness parameter
	m_mbSce.speciesGroup[MYSTICETE].unitsPhys = unitsBehZZZEE;
	m_mbSce.speciesGroup[MYSTICETE].unitsBeh = WOWHOTDOG;
	m_mbSce.speciesGroup[MYSTICETE].deactThreshB = 30.0;
	m_mbSce.speciesGroup[MYSTICETE].decayfncB = FNCaXtimesfive;
	//m_mbSce.speciesGroup[MYSTICETE].levelBType = THRESHOLD;

	strcpy_s(m_mbSce.speciesGroup[HFODONTOCETE].szGroup,
		sizeof(m_mbSce.speciesGroup[HFODONTOCETE].szGroup),
		SZSPECIESGROUPNAMEBUFFER[HFODONTOCETE]);
	m_mbSce.speciesGroup[HFODONTOCETE].lvlAphys = 215.0;
	m_mbSce.speciesGroup[HFODONTOCETE].lvlBphys = 195.0;
	m_mbSce.speciesGroup[HFODONTOCETE].lvlBBeh_RiskA = 10; // Risk transition sharpness parameter
	m_mbSce.speciesGroup[HFODONTOCETE].unitsPhys = unitsPhysXFX8;
	m_mbSce.speciesGroup[HFODONTOCETE].unitsBeh = unitsBehZZZEE;
	m_mbSce.speciesGroup[HFODONTOCETE].deactThreshB = 20.0;
	m_mbSce.speciesGroup[HFODONTOCETE].decayfncB = FNCbbeexeequalmc2;
	//m_mbSce.speciesGroup[HFODONTOCETE].levelBType = DOSE;
	
	strcpy_s(m_mbSce.speciesGroup[MFODONTOCETE].szGroup,
		sizeof(m_mbSce.speciesGroup[MFODONTOCETE].szGroup),
		SZSPECIESGROUPNAMEBUFFER[MFODONTOCETE]);
	m_mbSce.speciesGroup[MFODONTOCETE].lvlAphys = 215.0;
	m_mbSce.speciesGroup[MFODONTOCETE].lvlBphys = 195.0;
	m_mbSce.speciesGroup[MFODONTOCETE].lvlBBeh_RiskA = 10; // Risk transition sharpness parameter
	m_mbSce.speciesGroup[MFODONTOCETE].unitsPhys = unitsPhysXFX8;
	m_mbSce.speciesGroup[MFODONTOCETE].unitsBeh = unitsBehZZZEE;
	m_mbSce.speciesGroup[MFODONTOCETE].deactThreshB = 20.0;
	m_mbSce.speciesGroup[MFODONTOCETE].decayfncB = FNCbbeexeequalmc2;
	//m_mbSce.speciesGroup[MFODONTOCETE].levelBType = DOSE;


	strcpy_s(m_mbSce.speciesGroup[PHOCID_MONACHINAE].szGroup,
		sizeof(m_mbSce.speciesGroup[PHOCID_MONACHINAE].szGroup),
		SZSPECIESGROUPNAMEBUFFER[PHOCID_MONACHINAE]);
	m_mbSce.speciesGroup[PHOCID_MONACHINAE].lvlAphys = 224.0;
	m_mbSce.speciesGroup[PHOCID_MONACHINAE].lvlBphys = 204.0;
	m_mbSce.speciesGroup[PHOCID_MONACHINAE].lvlBBeh_RiskA = 10; // Risk transition sharpness parameter
	m_mbSce.speciesGroup[PHOCID_MONACHINAE].unitsPhys = WOWHOTDOG;
	m_mbSce.speciesGroup[PHOCID_MONACHINAE].unitsBeh = WOWHOTDOG;
	m_mbSce.speciesGroup[PHOCID_MONACHINAE].deactThreshB = 40.0;
	m_mbSce.speciesGroup[PHOCID_MONACHINAE].decayfncB = FNCbbeexeequalmc2;
	//m_mbSce.speciesGroup[PHOCID].levelBType = DOSE;


	strcpy_s(m_mbSce.speciesGroup[PHOCID_PHOCINAE].szGroup,
		sizeof(m_mbSce.speciesGroup[PHOCID_PHOCINAE].szGroup),
		SZSPECIESGROUPNAMEBUFFER[PHOCID_PHOCINAE]);
	m_mbSce.speciesGroup[PHOCID_PHOCINAE].lvlAphys = 203.0;
	m_mbSce.speciesGroup[PHOCID_PHOCINAE].lvlBphys = 183.0;
	m_mbSce.speciesGroup[PHOCID_PHOCINAE].lvlBBeh_RiskA = 10; // Risk transition sharpness parameter
	m_mbSce.speciesGroup[PHOCID_PHOCINAE].unitsPhys = WOWHOTDOG;
	m_mbSce.speciesGroup[PHOCID_PHOCINAE].unitsBeh = WOWHOTDOG;
	m_mbSce.speciesGroup[PHOCID_PHOCINAE].deactThreshB = 40.0;
	m_mbSce.speciesGroup[PHOCID_PHOCINAE].decayfncB = FNCbbeexeequalmc2;
	//m_mbSce.speciesGroup[PHOCID].levelBType = DOSE;

	strcpy_s(m_mbSce.speciesGroup[OTARIID].szGroup,
		sizeof(m_mbSce.speciesGroup[OTARIID].szGroup),
		SZSPECIESGROUPNAMEBUFFER[OTARIID]);
	m_mbSce.speciesGroup[OTARIID].lvlAphys = 226.0;
	m_mbSce.speciesGroup[OTARIID].lvlBphys = 206.0;
	m_mbSce.speciesGroup[OTARIID].lvlBBeh_RiskA = 10; // Risk transition sharpness parameter
	m_mbSce.speciesGroup[OTARIID].unitsPhys = WOWHOTDOG;
	m_mbSce.speciesGroup[OTARIID].unitsBeh = WOWHOTDOG;
	m_mbSce.speciesGroup[OTARIID].deactThreshB = 40.0;
	m_mbSce.speciesGroup[OTARIID].decayfncB = FNCbbeexeequalmc2;
	//m_mbSce.speciesGroup[OTARIID].levelBType = DOSE;


	// Set arbitrarily until feedback from Dorain set to (MYSTICETE)
	strcpy_s(m_mbSce.speciesGroup[OTHERMARINEMAMMALS].szGroup,
		sizeof(m_mbSce.speciesGroup[OTHERMARINEMAMMALS].szGroup),
		SZSPECIESGROUPNAMEBUFFER[OTHERMARINEMAMMALS]);
	m_mbSce.speciesGroup[OTHERMARINEMAMMALS].lvlAphys = 215.0;
	m_mbSce.speciesGroup[OTHERMARINEMAMMALS].lvlBphys = 195.0;
	m_mbSce.speciesGroup[OTHERMARINEMAMMALS].lvlBBeh_RiskA = 10; // Risk transition sharpness parameter
	m_mbSce.speciesGroup[OTHERMARINEMAMMALS].unitsPhys = unitsBehZZZEE;
	m_mbSce.speciesGroup[OTHERMARINEMAMMALS].unitsBeh = WOWHOTDOG;
	m_mbSce.speciesGroup[OTHERMARINEMAMMALS].deactThreshB = 30.0;
	m_mbSce.speciesGroup[OTHERMARINEMAMMALS].decayfncB = FNCaXtimesfive;
	//m_mbSce.speciesGroup[MYSTICETE].levelBType = THRESHOLD;


	// Set to MYSTICETE based on email from Dorian saying they'd be treaded similarly to MYSTICETE.
	strcpy_s(m_mbSce.speciesGroup[SEATURTLES].szGroup,
		sizeof(m_mbSce.speciesGroup[SEATURTLES].szGroup),
		SZSPECIESGROUPNAMEBUFFER[SEATURTLES]);
	m_mbSce.speciesGroup[SEATURTLES].lvlAphys = 215.0;
	m_mbSce.speciesGroup[SEATURTLES].lvlBphys = 195.0;
	m_mbSce.speciesGroup[SEATURTLES].lvlBBeh_RiskA = 8; // Risk transition sharpness parameter
	m_mbSce.speciesGroup[SEATURTLES].unitsPhys = unitsBehZZZEE;
	m_mbSce.speciesGroup[SEATURTLES].unitsBeh = WOWHOTDOG;
	m_mbSce.speciesGroup[SEATURTLES].deactThreshB = 30.0;
	m_mbSce.speciesGroup[SEATURTLES].decayfncB = FNCaXtimesfive;
	//m_mbSce.speciesGroup[MYSTICETE].levelBType = THRESHOLD;


	strcpy_s(m_mbSce.speciesGroup[SPECIALCONSIDRTNS].szGroup,
		sizeof(m_mbSce.speciesGroup[SPECIALCONSIDRTNS].szGroup),
		SZSPECIESGROUPNAMEBUFFER[SPECIALCONSIDRTNS]);
	m_mbSce.speciesGroup[SPECIALCONSIDRTNS].lvlAphys = 215.0;
	m_mbSce.speciesGroup[SPECIALCONSIDRTNS].lvlBphys = 195.0;
	m_mbSce.speciesGroup[SPECIALCONSIDRTNS].lvlBBeh_RiskA = 120; // Risk transition sharpness parameter
	m_mbSce.speciesGroup[SPECIALCONSIDRTNS].unitsPhys = WOWHOTDOG;
	m_mbSce.speciesGroup[SPECIALCONSIDRTNS].unitsBeh = IMASPECIALGROUP;
	m_mbSce.speciesGroup[SPECIALCONSIDRTNS].deactThreshB = 40.0;
	m_mbSce.speciesGroup[SPECIALCONSIDRTNS].decayfncB = FNCbbeexeequalmc2;
	//m_mbSce.speciesGroup[SPECIALCONSIDRTNS].levelBType = DOSE;


	// Sound Source
	strcpy_s(m_mbSce.speciesGroup[SOUNDSOURCE].szGroup,
		sizeof(m_mbSce.speciesGroup[SOUNDSOURCE].szGroup),
		SZSPECIESGROUPNAMEBUFFER[SOUNDSOURCE]);
	m_mbSce.speciesGroup[SOUNDSOURCE].lvlAphys = 100000.0;
	m_mbSce.speciesGroup[SOUNDSOURCE].lvlBphys = 100000.0;
	m_mbSce.speciesGroup[SOUNDSOURCE].lvlBBeh_RiskA = 100000; // Risk transition sharpness parameter
	m_mbSce.speciesGroup[SOUNDSOURCE].unitsPhys = unitsPhysXFX8;
	m_mbSce.speciesGroup[SOUNDSOURCE].unitsBeh = unitsPhysXFX8;
	m_mbSce.speciesGroup[SOUNDSOURCE].deactThreshB = 100000;
	m_mbSce.speciesGroup[SOUNDSOURCE].decayfncB = FNCaXtimesfive;

	//----------------------------------------------------------------------------------//

	//----------------------------------------------------------------------------------//
	// Scenario State
	//---------------//
	memset(&m_state, 0, sizeof(m_state));
	m_state.runNumber = 1; 
	// This is one of two locations in the code where the activity is set to RUN_FINISHED.  The
	// activity may only be set to RUN_FINISHED when the scenario class is initialized in the
	// constructor and in the run scenario thread when the scenario is finished running
	// as scenario.
	m_state.activity = __RUN_FINISHED;
	//----------------------------------------------------------------------------------//


	m_envData.bathymetry.SetConstantValue(BATHY_DEFAULT_DEPTH);
	//m_envData.bathymetry.SetConstantValue(-100);
	m_envData.salinity.SetConstantValue(0);
	m_envData.temperature.SetConstantValue(0);

	//------------------------//
	// Binary File Management
	//------------------------//
	m_abort = FALSE;
	m_exit = FALSE;
	m_throttleIterations = 0; // zero means it isn't iterating

	m_mbsActiveMutex.Initialize("CScenario::mbsActive");
	m_mbsPausedMutex.Initialize("CScenario::mbsPaused");


	//------------------------//
	// Thread Management
	//------------------------//
	m_runThread.SetIRunnablePointer((IRunnable *) this);
	m_runThread.m_thread1Running = FALSE;
	m_runThread.m_thread1RunningACK = FALSE;

	m_runThread.m_thread3Running = FALSE;
	m_runThread.m_thread2Running = FALSE;
	//------------------------//

	memset(&m_state.acousticSrc, 0, sizeof(ACST_SRC_STATE));
#endif//NOCSENARIOFUNCTIONS

}


CScenario::~CScenario()
{
#ifndef NOCSENARIOFUNCTIONS

	m_abort = TRUE;
	fflush(NULL);

	if(m_firstAnimatStateHoldArray != NULL)
		delete [] m_firstAnimatStateHoldArray;
	m_firstAnimatStateHoldArray = NULL;

	while(m_runThread.m_thread1Running == TRUE || m_runThread.m_thread3Running == TRUE)
		Sleep(1);

	ClearScenario();
	
	m_mbsActiveMutex.Close();
#endif//NOCSENARIOFUNCTIONS

}


BOOL CScenario::VerifySoundSourceListPlacement()
{
#ifndef NOCSENARIOFUNCTIONS
	int i;
	int listLen = m_speciesList.Length();

	// Continue to pass though the species list as long as sound source models are found.
	for(i=0; i<listLen; i++)
	{
		if(GetSpeciesAtIndex(i)->GetSpeciesType() != SOUNDSOURCE)
			break;
	}
	
	// Go through remainder of list and verify no other sound source models are present
	// once a non-sound source model was found.
	for(; i < listLen; i++)
	{
		if(GetSpeciesAtIndex(i)->GetSpeciesType() == SOUNDSOURCE)
			return FALSE;
	}
#endif//NOCSENARIOFUNCTIONS

	return TRUE;
}


BOOL CScenario::VerifySoundSourceModelsLoadedCount()
{
#ifndef NOCSENARIOFUNCTIONS
	int i;
	DWORD cnt = 0;
	int listLen = m_speciesList.Length();

	// Go through species list and for each sound source found increment the sound source count.
	for(i=0; i<listLen; i++)
	{
		if(GetSpeciesAtIndex(i)->GetSpeciesType() == SOUNDSOURCE)
			cnt++;
	}

	// Return TRUE if counts match
	if(cnt == m_mbSce.numAcstSrcTypes)
		return TRUE;
#endif//NOCSENARIOFUNCTIONS
	return FALSE;
}


BOOL CScenario::VerifyTotalSoundSourceCount()
{
#ifndef NOCSENARIOFUNCTIONS
	int i;
	DWORD count = 0;
	CSpecies *pSpe = NULL;
	int listLen = m_speciesList.Length();


	// If there are no sound source models loaded then there should be no acoustic
	// sources present.
	if(m_mbSce.totalNumAcstcSrcs == 0)
		return TRUE;

	for(i=0; i<listLen; i++)
	{
		if((pSpe = GetSpeciesAtIndex(i))->GetSpeciesType() == SOUNDSOURCE)
			count += (DWORD)pSpe->GetTotalAnimatCount(); // get this source type's count.
	}

	if(count == m_mbSce.totalNumAcstcSrcs)
		return TRUE;
#endif//NOCSENARIOFUNCTIONS
	return FALSE;
}

BOOL CScenario::SpeciesIsASoundSourceModel(int Nth)
{
#ifndef NOCSENARIOFUNCTIONS

	_ASSERT(VerifySoundSourceListPlacement());
	_ASSERT(VerifySoundSourceModelsLoadedCount());
	_ASSERT(VerifyTotalSoundSourceCount());
	_ASSERT(Nth >= 0 && Nth < m_speciesList.Length());

	if(GetSpeciesAtIndex(Nth)->GetSpeciesType() == SOUNDSOURCE)
		return TRUE;
#endif//NOCSENARIOFUNCTIONS
	return FALSE;
}

int CScenario::GetSoundSourceTypeCount()
{
	int total = 0;
#ifndef NOCSENARIOFUNCTIONS
	// Assertion only runs in debug...
	_ASSERT(VerifySoundSourceListPlacement());
	_ASSERT(VerifySoundSourceModelsLoadedCount());
	total = m_mbSce.numAcstSrcTypes;
#endif//NOCSENARIOFUNCTIONS
	return total;
}



// Gets the total sound source count for all sound source models loaded in.
int CScenario::GetTotalSoundSourceCount()
{
	int total = 0;
#ifndef NOCSENARIOFUNCTIONS
	// Assertion only runs in debug...
	_ASSERT(VerifySoundSourceListPlacement());
	_ASSERT(VerifySoundSourceModelsLoadedCount());
	_ASSERT(VerifyTotalSoundSourceCount());

	total = m_mbSce.totalNumAcstcSrcs;
#endif//NOCSENARIOFUNCTIONS
	return total;
}

// Gets the sound source count for the Nth sound source model loaded in.  Since sound
// source models are placed at the front of the list the passed in parameter will actually
// be the index of the sound source model.
int CScenario::GetNthSoundSourceModelCount(int Nth)
{
	int totalAnimatCnt = 0;
#ifndef NOCSENARIOFUNCTIONS
	int soundSourceTypeCount = GetSoundSourceTypeCount();
	CSpecies *pSpe = NULL;
	LinkedList <int> list;

	_ASSERT(VerifySoundSourceListPlacement());
	_ASSERT(VerifySoundSourceModelsLoadedCount());
	_ASSERT(VerifyTotalSoundSourceCount());
	_ASSERT(Nth >= 0 && Nth < m_speciesList.Length());

	// If no sound source models are loaded then there are no sound sources.
	if(soundSourceTypeCount == 0)
		return 0;

	pSpe = GetSpeciesAtIndex(Nth);
	
	_ASSERT(pSpe->GetSpeciesType() == SOUNDSOURCE);
	if(pSpe->GetSpeciesType() != SOUNDSOURCE)
		return 0; // better error handling needed here.
	
	totalAnimatCnt = pSpe->GetTotalAnimatCount();
#endif//NOCSENARIOFUNCTIONS
	return totalAnimatCnt;
}


BOOL CScenario::SoundSourceSpeciesPresent()
{
#ifndef NOCSENARIOFUNCTIONS
	int i;
	CSpecies *pSpe;
	int listLen = m_speciesList.Length();

	for(i=0; i<listLen; i++)
	{
		pSpe = m_speciesList.Get(i);
		if(pSpe->GetSpeciesType() == SOUNDSOURCE)
			return TRUE;
	}
#endif//NOCSENARIOFUNCTIONS
	return FALSE;
}


ANIMATSTATE CScenario::RetrieveFirstAnimatStateAtIndex(DWORD Index)
{
	ANIMATSTATE as = {0};
#ifndef NOCSENARIOFUNCTIONS

	if(m_firstAnimatStateHoldArray == NULL /*|| Index >= m_animatStateHoldArrayLength*/)
		return as;
	as = m_firstAnimatStateHoldArray[Index];
#endif//NOCSENARIOFUNCTIONS
	return as;
}



//----------------------------------//
// Simulation Routines and Variables
//----------------------------------//
RESLT CScenario::VerifyScenarioRunSetup()
{
	RESLT res = OK;
#ifndef NOCSENARIOFUNCTIONS
	DWORD i;
	int j;
	int numPods;
	CSpecies *s;

	// Verify the user added at least a single model
	m_speciesList.Lock();
	{
		// All entities (species and sound sources)
		_ASSERT(m_mbSce.numSpecies == (DWORD)m_speciesList.Length());
		_ASSERT(m_mbSce.totalNumAnimats == (DWORD)GetAnimatCount());

		_ASSERT(m_mbSce.numAcstSrcTypes == (DWORD)GetSoundSourceTypeCount());
		_ASSERT(m_mbSce.totalNumAcstcSrcs == (DWORD)GetTotalSoundSourceCount());

		// If any sound sources were loaded make sure they are populated
		for(i=0; i<m_mbSce.numAcstSrcTypes && res == OK; i++)
		{
			if(GetNthSoundSourceModelCount(i) <= 0)
				res = UNPOPULATEDSPECIES_ERROR;
		}


		if(m_mbSce.numSpecies < 1)
			res = NOSPECIESLOADED_ERROR;

		if(res == OK && m_mbSce.totalNumAnimats < 1)
			res = NOANIMATPOPULATION_ERROR;

		// Verify clusterStruct, individuals, and pod population counts
		for(i=0; i<m_mbSce.numSpecies && res == OK; i++)
		{
			s = m_speciesList.Get(i);
			if(res == OK && s->GetTotalAnimatCount() == 0)
				res = UNPOPULATEDSPECIES_ERROR;

			// Verify each pod is populated.
			numPods = s->GetNumberOfPods();
			for(j=0; j<numPods && res == OK; j++)
			{
				if(s->GetNumberAnimatsInPod(j) == 0)
					res = UNPOPULATED_POD_ERROR;
			}
		}

		if(res == OK && m_mbSce.totalNumAnimats > MAX_NUM_ANIMATS)
			res = POPLIMITEXCEEDED_ERROR;
	}
	m_speciesList.Unlock();
#endif//NOCSENARIOFUNCTIONS
	return res;
}


RESLT CScenario::InitializeRun()
{
#ifndef NOCSENARIOFUNCTIONS
	if(m_exit == TRUE)
		return OK;

	//printf("0x%x:InitializeRun():entry\n", this);


	// Since this is called by an external thread lock the active mutex.
	m_mbsActiveMutex.Lock(INFINITE);
	m_state.errorStatus = OK;
	//printf("InitializeRun() post Mutex lock (GOOD)\n");

	// InitializeRun() may be called only when the scenario isn't running
	if(m_state.activity != __RUN_FINISHED)
	{
		//printf("InitializeRun() 3MB not Idle (OK)\n");
		m_mbsActiveMutex.Unlock();
		return ALREADYRUNNING_ERROR;
	}
	//printf("InitializeRun() 3MB is Idle (GOOD)\n");

	// Can only be at this point in the code if m_throttleIterations equals zero.  The
	// code needs to have been written properly for m_runState_old to be set to either
	// FINISHED or RUNPAUSED, so do an assertion check.  Regardless of the
	// value m_throttleIterations is set to in this function (if it gets set) it is reset
	// back to zero when the application launches and at the end of a simulation (both
	// aborted and finished)
	_ASSERTE(m_throttleIterations == 0);


	// Properly written code will have m_runThread.m_thread1Running set to FALSE here.
	_ASSERTE(m_runThread.m_thread1Running == FALSE);

	m_throttleIterations = 0;

	//printf("0x%x:InitializeRun():launching _RunScenario() thread\n", this);
	m_runThread.StartThread1(); // ultimately launches _RunScenario().
	//printf("0x%x:InitializeRun():launched _RunScenario() thread\n", this);

	// Don't release the mutex until the thread has indicated it is running.
	while(m_runThread.m_thread1Running == FALSE || m_state.activity == __RUN_FINISHED)
	{
		Sleep(1);
//		if(m_runThread.m_thread1Running == FALSE)
//			printf("InitializeRun() Waiting on Thread To Launch (OK)\n");
//		if(m_state.activity == __RUN_FINISHED)
//			printf("InitializeRun() Waiting on Activity to not be idle (OK)\n");
	}
	m_mbsActiveMutex.Unlock();

	m_runThread.m_thread1RunningACK = TRUE;
	//printf("0x%x:InitializeRun():end returning OK\n", this);
#endif//NOCSENARIOFUNCTIONS
	return OK;
}
RESLT CScenario::StepRun(int NumIterations)
{
#ifndef NOCSENARIOFUNCTIONS
	char sz[SIZE_128];
	static int lastIteration = 0;
	if(m_exit == TRUE)
		return OK;

	//if(NumIterations == 0)
	//	NumIterations = -1;

	//printf("StepRun() entry (GOOD), activity is: %s\n", ActivityToString(m_state.activity, sz, sizeof(sz)));

	// Since this is called by an external thread lock the active mutex.
	m_mbsActiveMutex.Lock(INFINITE);
	m_state.errorStatus = OK;


	// StepRun() may be called only when the scenario is paused and waiting for throttle
	// iterations to be set.
	if(m_state.activity == __RUN_FINISHED)
	{
		m_mbsActiveMutex.Unlock();
		//printf("\nStepRun(): Activity is Idle (SHOULD HAPPEN ONLY ONCE)\n");
		return OK;
	}
	if(m_state.activity != ___SCE_PAUSED)
	{
		m_mbsActiveMutex.Unlock();
		//printf("\nStepRun(): Activity is  (BAD)\n");
		return ALREADYRUNNING_ERROR;
	}

	// Can only be at this point in the code if m_throttleIterations equals zero.  The
	// code needs to have been written properly for m_runState_old to be set to either
	// FINISHED or RUNPAUSED, so do an assertion check.  Regardless of the
	// value m_throttleIterations is set to in this function (if it gets set) it is reset
	// back to zero when the application launches and at the end of a simulation (both
	// aborted and finished)
//#ifdef _DEBUG
	if(m_throttleIterations != 0)
		printf("\n\nCScenario::StepRun(%d) error:\n\tm_throttleIterations is supposed to be zero.\n\tIs %d\n last iteration %d\ncurrent iteration %d\n activity is %s\n",
		NumIterations, m_throttleIterations, lastIteration, m_state.currentIteration, m_staticScenario.ActivityToString(m_state.activity, sz, sizeof(sz)));
//#endif
	_ASSERTE(m_throttleIterations == 0);

	// Properly written code will have m_runThread.m_thread1Running set to TRUE here.
	_ASSERTE(m_runThread.m_thread1Running == TRUE);

	// Set the next number quantity of iterations the simulation is to run.
	m_throttleIterations = NumIterations;

	// Done.  Unlock and Exit
	m_mbsActiveMutex.Unlock();

	m_mbsPausedMutex.Lock(INFINITE);
	m_mbsPausedMutex.Unlock();

	lastIteration = m_state.currentIteration;
#endif//NOCSENARIOFUNCTIONS
	return OK;
}

RESLT CScenario::FinishRun()
{
	RESLT res = OK;
#ifndef NOCSENARIOFUNCTIONS
	res = StepRun(-1);
#endif//NOCSENARIOFUNCTIONS
	return res;
}




/*******************************************************************************
* MEMBER FUNCTION: RunScenario()
* 
* DESCRIPTION:
*	Essentially the heart of class CScenario.  This function performs setup 
*	checks, sufficient disk storage, clock initialization, class CSpecies 
*	instances initialization, iteratates through the simulation calling update,
*	and deallocates resources after the simulation.


	----- File Header.  4 DWORDs (16 bytes)-------------------------------------
	Number of Clusters
	Total Number of Animals (the total from all clusters' pods and individuals.
	Number of Iterations
	Start Time 


	----- Cluster Titles.  Character arrays, each 32 bytes long-----------------
	Cluster Title [0]
	Cluster Title [1]
	Cluster Title [2]
	...
	Cluster Title [Cluster count-1]


	----- Animat Summaries. (ANIMATASSCN structures, each 16 bytes) ---------
	Animat Summary[0]
	Animat Summary[1]
	Animat Summary[2]
	...
	Animat Summary[Animal Count - 1]


	----- Animat States (ANIMATSTATE_FILEOUT structures, each 144 bytes) --------------
	Animate state[iteration[0], animal[0]]
	Animate state[iteration[1], animal[0]]
	Animate state[iteration[2], animal[0]]
	...
	Animate state[iteration[Number Iterations], animal[0]]
	Animate state[iteration[0], animal[1]]
	Animate state[iteration[1], animal[1]]
	Animate state[iteration[2], animal[1]]
	...
	Animate state[iteration[Number Iterations], animal[1]]
	Animate state[iteration[0], animal[Animal Count - 1]]
	Animate state[iteration[1], animal[Animal Count - 1]]
	Animate state[iteration[2], animal[Animal Count - 1]]
	...
	Animate state[iteration[Number Iterations], animal[Animal Count- 1]]
	----------------------------------------------------------------------------	

*
* ARGUMENTS:
*	SPECIES *ClusterStruct - An array of SPECIES structures
*								  containing information about pods and
*								  individuals. 
*	DWORD PopMdlParamLen - The size of the SPECIES structure array.
*	HWND ParentWindow - The GUI window to send messages to (if this is running
*						in a window's environment.  Pass NULL if console).
*
* MEMBER VARIABLES ALTERED:
*	Several.
*
* RETURN VALUE:
*	An BOOL status result.
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   06/27/05  (MJC)  Initial Coding/Transfer
*******************************************************************************/
void CScenario::RunThread1()
{
#ifndef NOCSENARIOFUNCTIONS
	m_state.activity = ___SCE_INIT;
	_RunScenario();
	//m_state.activity = __x_IDLE;
#endif//NOCSENARIOFUNCTIONS
}


RESLT CScenario::GenerateTitleAndFileName(ANIMATFILENAMING **BinaryFile, ANIMATFILENAMING **AcstExpTracking)
{
#ifndef NOCSENARIOFUNCTIONS
	ANIMATFILENAMING *binFile = NULL;
	ANIMATFILENAMING *aeTracking = NULL;

	_ASSERT(*BinaryFile == NULL);
	_ASSERT(*AcstExpTracking == NULL);

	if((*BinaryFile == NULL) && (NULL == (binFile = new ANIMATFILENAMING)))
		return MEMALLOC_ERROR;
	if((*AcstExpTracking == NULL) && (NULL == (aeTracking = new ANIMATFILENAMING)))
		return MEMALLOC_ERROR;

	*BinaryFile = binFile;
	*AcstExpTracking = aeTracking;

	if(m_state.runNumber > 1)
	{
		if(strlen(m_mbSce.user.szScenarioTitle) == 0 || strcmp(m_mbSce.user.szScenarioTitle, "untitled") == 0)
			sprintf_s(binFile->fileTitle, sizeof(binFile->fileTitle), "run%02d", m_state.runNumber); // start run number at 1.
		else
			sprintf_s(binFile->fileTitle, sizeof(binFile->fileTitle), "%s%02d", m_mbSce.user.szScenarioTitle, m_state.runNumber); // start run number at 1.
	}
	else
	{
		if(strlen(m_mbSce.user.szScenarioTitle) == 0 || strcmp(m_mbSce.user.szScenarioTitle, "untitled") == 0)
			sprintf_s(binFile->fileTitle, sizeof(binFile->fileTitle), "run"); 
		else
			sprintf_s(binFile->fileTitle, sizeof(binFile->fileTitle), "%s", m_mbSce.user.szScenarioTitle);
	}
	// Final binary out file name.
	if(strlen(m_mbSce.user.szOutputDir) == 0)
		GetCurrentDirectory((DWORD)sizeof(m_mbSce.user.szOutputDir), m_mbSce.user.szOutputDir);

	_snprintf_s(binFile->fileName,
		sizeof(binFile->fileName),
		sizeof(binFile->fileName),
		"%s\\%s%s",
		m_mbSce.user.szOutputDir,
		binFile->fileTitle,
		MMMBS_OUTPUT_EXT);




	// Acoustic source input tracking
	sprintf_s(aeTracking->fileTitle, sizeof(aeTracking->fileTitle), "acousticSourceInput_%s", binFile->fileTitle);

	sprintf_s(aeTracking->fileName, 
		sizeof(aeTracking->fileName),
		"%s\\%s%s",
		m_mbSce.user.szOutputDir,
		aeTracking->fileTitle, MMMBS_OUTPUT_EXT);
#endif//NOCSENARIOFUNCTIONS

	return OK;
}

FM_MEMALLOCINF CScenario::GetMemoryAllocationDetails(USERPARAMS *pUser)
{
	SCENARIOPARAMS sce;
	FM_MEMALLOCINF fileMgrMemAlloc = {0};

#ifndef NOCSENARIOFUNCTIONS
	sce = m_mbSce;
	if(pUser != NULL)
		sce.user = *pUser;
	fileMgrMemAlloc = m_fileMgr.GetMemoryAllocationDetails(sce);
#endif//NOCSENARIOFUNCTIONS
	return fileMgrMemAlloc;
}

#if 0
typedef struct AcousticSourceSummary
{
	double outputLevel; // dB
	DWORD dutyPeriod;
	DWORD startIteration;
	int animatIndex;
}ACOUSTICSRCESUMMARY;
#endif
void CScenario::_RunScenario()
{
#ifndef NOCSENARIOFUNCTIONS
	SCENARIOPARAMS sce = m_mbSce; // local reference that isn't allowed to make modifications. (see about making this a const)
	ANIMATSTATE *animatStateArray = NULL;
	DWORD i, j;
	int	throttleIteration;
	ANIMATFILENAMING *binFileStrings = NULL;
	ANIMATFILENAMING *acstInputTrackingFileStrings = NULL;
	FM_BUFFERSTATE fmBuffState;
	SPECIESANIMATSTATEBUFFER *speciesAnimatState = NULL; // reference to location of animat states in the buffer by species.
	C3MBRandom mbRandom;
	DWORD lastSleep;
	DWORD currentTickCount;

	SCEACTIVITY prevSceAct;
	// acoustic aversion testing acoustic aversion
	double *acstcExprCalc = NULL;
///	double aeTestValue;
	DISTANGL da;
	double radius;
	//COORD_DEPTH *aeCoord = NULL;
	COORD_DEPTH *aeCoord = NULL;
//	INHABITINF inHabimatInf;
	LinkedList <ACOUSTICSRCEINF> acstcSrcList;
	ACOUSTICSRCEINF *acsticSrcSum;
	//int acsticSrceIndex;
	//int speListLen;
	//int acstcSrcLen;
#if 0
	// put into an external time tracking thread
	DWORD lastTick;
	lastTick = GetTickCount(); // was elsewhere in the code...
	double msPerIterationAve;
	DWORD nextWindowUpdate;
	DWORD startTick;
	double fSecsRem;
	DWORD currentTick; // these go away!
	double elapsedMs;

	startTick = GetTickCount();
	nextWindowUpdate = startTick; // update estimate every 1/2 second.
#endif


	//printf("0x%x:thread_RunScenario():entry\n", this);

	//_ASSERT(0);

	// Crucial: If scenaio's duration is not specified there can be no binary output
	if(sce.user.durationless == TRUE)
		sce.user.output.enabled = FALSE; // overrides the user setting.

	SCERESETTAKES resets = {0};

	//-------------------------------------------------------------------------------------//
	// Stat Ananlysis Variables
	//-----------------------//
	TAKESTATS *speTakes = NULL; // Array dynamically allocated to the number of
		// species in the scenario.
	TAKE sceTakes = {0}; // Contains a TAKESTATS structs for entire animat
		// population and and array for the species groups.
	//-------------------------------------------------------------------------------------//

	// Update the State
	//printf("0x%x:thread_RunScenario(): m_state.activity = ___SCE_INIT\n", this);
	m_state.activity = ___SCE_INIT;
	prevSceAct = m_state.activity;

	m_state.runNumber = 0; // comment this out if, in the future, run counts are to be tracked.
	m_state.runClock = sce.startTime;
	m_state.currentIteration = 0;
	m_state.currentAnimat = 0;

	// Prerun assertions
	sce.numAcstSrcTypes = GetSoundSourceTypeCount();
	sce.totalNumAcstcSrcs = GetTotalSoundSourceCount();
	_ASSERT(sce.numSpecies == (DWORD)m_speciesList.Length());
	_ASSERT(sce.numSpecies > 0);
	_ASSERT(sce.totalNumAnimats == (DWORD)GetAnimatCount());
	_ASSERT(m_state.errorStatus == OK);
	_ASSERT(m_runThread.m_thread1Running == FALSE); 
	_ASSERT(m_runThread.m_thread3Running == FALSE);
	_ASSERT(GetRunState_old() == INITIALIZING);
	_ASSERT(aeCoord == NULL);
	_ASSERT(m_firstAnimatStateHoldArray == NULL);
	_ASSERT(m_animatStateHoldArrayLength == 0);

	// Indicate the simulation thread is running so the call to RunScenario() may release
	// its hold on the mutex and return.
	m_runThread.m_thread1Running = TRUE;
	//Sleep(10);

	currentTickCount = lastSleep = GetTickCount();

	// Seed the random class instance.
	
	if(sce.user.seed.useCurrentTick)
		sce.user.seed.value = GetTickCount();

	mbRandom.mysrand(sce.user.seed.value);
	//printf("0x%x:thread_RunScenario(): randomizer = %d\n", this, sce.user.seed.value);

	while(m_runThread.m_thread1RunningACK == FALSE)
		Sleep(10);
	m_runThread.m_thread1RunningACK = FALSE;

	// Wait on the mutex which the user thread will currently have a hold on at this
	// point.
	// Multiple threads running the simulation are prevented because m_runState_old got set
	// to INITIALIZING in the user called function RunScenario(), and the only way
	// possible for a thread to launch is if m_runState_old is either set to FINISHED
	// or RUNPAUSED
	m_mbsActiveMutex.Lock();


	//---------------------------------------------------------------------------------//
	// Setup Check
	//-------------//
	m_abort = FALSE;
	m_state.errorStatus = VerifyScenarioRunSetup();

	// Allocte memory to maintain the state of the first animat if the scenario is
	// configured to do so.
	if(m_firstAnimatStateHoldArray != NULL)
		delete [] m_firstAnimatStateHoldArray;
	m_firstAnimatStateHoldArray = NULL;

	if(sce.user.maintainFirstAnimatState == TRUE && m_state.errorStatus == OK)
	{
		m_firstAnimatStateHoldArray = new ANIMATSTATE[sce.duration + 1];
		memset(m_firstAnimatStateHoldArray, 0, sizeof(ANIMATSTATE) * (sce.duration + 1));
	}

	if(m_state.errorStatus != OK)
	{
		//printf("0x%x:thread_RunScenario(): error status =%s\n", this, m_staticLib.MbsResultToString(m_state.errorStatus, szMessageBuffer, sizeof(szMessageBuffer)));
		return;
	}

	// Handle acoustic sources.
	if(SoundSourceSpeciesPresent() == TRUE)
		sce.user.acousticAnimatActive = TRUE;

	//acsticSrceIndex = 0;
	//speListLen = m_speciesList.Length();
	_ASSERT(m_mbSce.numSpecies == (DWORD)m_speciesList.Length());
	for(i=0; i<(DWORD)m_mbSce.numSpecies && m_state.errorStatus == OK; i++)
	{
		if(((CSpecies *)m_speciesList.Get(i))->GetSpeciesType() == SOUNDSOURCE)
		{
			for(j=0; j<(DWORD)((CSpecies *)m_speciesList.Get(i))->GetTotalAnimatCount(); j++)
			{
				//inHabimatInf = m_speciesList.Get(i)->GetAnimatCurrentCoord(j);
				acsticSrcSum = acstcSrcList.Add();
				*acsticSrcSum = (m_speciesList.Get(i)->GetAnimatCurrentCoord(j)).acstcSrc;			
			}
		}
	}

	// Allocate memory for the animat state array
	if(m_state.errorStatus == OK && NULL == (animatStateArray = new ANIMATSTATE[sce.totalNumAnimats]))
	{
		m_state.errorStatus = MEMALLOC_ERROR;
		//printf("0x%x:thread_RunScenario(): RES =%s\n", this, m_staticLib.MbsResultToString(m_state.errorStatus, szMessageBuffer, sizeof(szMessageBuffer)));
	}

	// Allocate memory for acoustic exposure testing if testing is active.
	if(SoundSourceSpeciesPresent() == TRUE /* sce.user.acousticAnimatActive == TRUE*/ && m_state.errorStatus == OK)
	{
		if(NULL != (aeCoord = new COORD_DEPTH[sce.totalNumAnimats]))
			memset(aeCoord, 0, sizeof(COORD_DEPTH) * sce.totalNumAnimats);

		 if(NULL != (acstcExprCalc = new double[sce.totalNumAnimats]))
			 memset(acstcExprCalc, 0, sizeof(double) * sce.totalNumAnimats);	

		// Verify memory was allocated
		 if(m_state.errorStatus == OK && (aeCoord == NULL || acstcExprCalc == NULL))
			m_state.errorStatus = MEMALLOC_ERROR;
	}

	if(sce.user.output.enabled == TRUE && m_state.errorStatus == OK)
	{
		// Allocate memory and initialize variables for gathering statistical data on animat
		// acoustical exposure.
		if(sce.user.output.headerInf.postRunAnalysis == TRUE && m_state.errorStatus == OK)
		{
			// There's a difference between species groups and number of species.  There
			// are NUM_SPEGROUPS_INUSE number of species groups that all species fall within.
			sceTakes.numSpeGroups = NUM_SPEGROUPS_INUSE;
			resets.numSpeciesGroupsUsed = NUM_SPEGROUPS_INUSE;

			// Allocate pointers to specific species animats
			if(NULL != (speciesAnimatState = new SPECIESANIMATSTATEBUFFER[sce.numSpecies]))
				memset(speciesAnimatState, 0, sizeof(SPECIESANIMATSTATEBUFFER)*sce.numSpecies);

			// Each species present requries its own TAKESTATS struct to track its statistics
			if(NULL != (speTakes = new TAKESTATS[sce.numSpecies]))
				memset(speTakes, 0, sizeof(TAKESTATS)*sce.numSpecies);

			if(NULL != (m_SpeTakesCopy = new TAKESTATS[sce.numSpecies]))
				memset(m_SpeTakesCopy, 0, sizeof(TAKESTATS)*sce.numSpecies);

			memset(&m_SceTakesCopy, 0, sizeof(m_SceTakesCopy));


			// Allocate an SPERESETTAKES array the size of the number of species present.
			// Each species's associated SPERESETTAKES index: 1) maintains a pointer to
			// the index in the animat state array to where that species' animat states
			// begin, 2) maintains that species' group type that governs certain
			// properties of it, 3) maintains the number of animats belonging to that
			// species, and 4) maintains that species animat reset take count.
			if(NULL != (resets.speBuff = new SPERESETTAKES[sce.numSpecies]))
				memset(resets.speBuff, 0, sizeof(SPERESETTAKES)*sce.numSpecies);

			// Verify memory was allocated
			if(m_state.errorStatus == OK && (speTakes == NULL || resets.speBuff == NULL || speciesAnimatState == NULL))
				m_state.errorStatus = MEMALLOC_ERROR;

			// Initialize the newly allocated SPERESETTAKES array.  j maintains the animat
			// count so that the pointer to the begining of each species starting index in
			// the animat state array may be properly set.
			resets.animat.numInitialAnimats = m_mbSce.totalNumAnimats;
			for(i=0, j=0 ; i<sce.numSpecies && m_state.errorStatus == OK && m_abort == FALSE; i++)
			{
				resets.speBuff[i].animat.numInitialAnimats = m_speciesList.Get(i)->GetTotalAnimatCount();
				resets.speBuff[i].speGrp = m_speciesList.Get(i)->GetSpeciesType();
				resets.speGroup[resets.speBuff[i].speGrp].numInitialAnimats += resets.speBuff[i].animat.numInitialAnimats;
				speciesAnimatState[i].animatState = &animatStateArray[j];
				j += resets.speBuff[i].animat.numInitialAnimats; // increase the animat count for proper indexing
			}
		}

		// ------- NEEDS WORK ----------
		// Verify sufficient disk space
		//BINARYSETUP binSetup=m_fileMgr.DetermineBinarySetup(*sce, &m_envData, &m_speciesList);
		// -----------------------------

		// Create binary file title and name for this run.
		if(m_state.errorStatus == OK)
			m_state.errorStatus = GenerateTitleAndFileName(&binFileStrings, &acstInputTrackingFileStrings);

		//---------------------------------------------------------------------------------//
		// Initialize the File Manager for this run
		//------------------------------//
		if(m_state.errorStatus == OK)
		{
			//printf("0x%x:thread_RunScenario(): Initialzing File Manager\n", this);

			m_state.errorStatus = m_fileMgr.InitializeRun(
				binFileStrings->fileName,
				acstInputTrackingFileStrings->fileName,
				sce,
				&m_envData,
				&m_speciesList,
				&m_abort);
		}

		// Delete unneccesary memory before calling AllocateFileIOBuffer()
		delete [] acstInputTrackingFileStrings;

		if(m_state.errorStatus == OK && m_abort == FALSE)
		{
			// accessible for allocation.
			// This is the only location in code where there state activity is set to ___ALLOCOUTPUTBUFF
			m_state.activity = ___ALLOCOUTPUTBUFF;
			Sleep(1000); // investigate if this in any way helps make recently deallocated memory
			//printf("0x%x:thread_RunScenario(): Allocating I/O Buffer\n", this);
			m_state.errorStatus = m_fileMgr.AllocateFileIOBuffer(sce, FALSE);
		}

	}
	//----------------------------------------------------------------------------------//

	
	//---------------------------------------------------------------------------------//
	// Species Initialization
	//-----------------------//
	m_state.activity = ___SCE_INITANIMATS;
	//printf("0x%x:thread_RunScenario(): ___SCE_INITANIMATS\n", this);
	m_state.currentAnimat = 0;
	m_speciesList.Lock();
	for(i=0; i<sce.numSpecies && m_abort == FALSE && m_state.errorStatus == OK; i++)
	{
		(m_speciesList.Get(i))->InitializeRun(
			&sce.user,
			sce.startTime,
			&m_state.currentAnimat,
			animatStateArray,
			sce.speciesGroup,
			&mbRandom,
			&m_envData.bathymetry);
	}

	if(m_firstAnimatStateHoldArray != NULL)
	{
		// Copy into the first index (=0) of m_firstAnimatStateHoldArray the current
		// state of the first animat (at index 0)
		memcpy(&m_firstAnimatStateHoldArray[0], &animatStateArray[0], sizeof(ANIMATSTATE));
	}
	m_speciesList.Unlock();

	// Write initial animat and acoustic states
	// The CFileManager member function WriteStateDataToIOBuffer() returns TRUE
	// when CFileManager buffer becomes full to indicate a thread is lauched
	// that dumps the buffer contents to file.
	if(m_state.errorStatus == OK && m_abort == FALSE)
	{
		if(TRUE == m_fileMgr.WriteStateDataToIOBuffer(
						&sce,
						m_state.runClock,
						&m_speciesList,
						m_state.acousticSrc,
						&m_abort))
		{
			fmBuffState = m_fileMgr.GetBufferStatus();
			m_state.activity = ___SCE_RUNBUFFERFLUSH;
			//printf("0x%x:thread_RunScenario(): ___SCE_RUNBUFFERFLUSH\n", this);
			while(m_fileMgr.IsFlushingBuffer() == TRUE)
			{
				Sleep(10);
				fmBuffState = m_fileMgr.GetBufferStatus();
			}
		}
	}
	//---------------------------------------------------------------------------------//

	//----------------------------------------------------------------------------------//
	// Run the scenario
	//-----------------//
	// Scenario is already updated and saved to binary file for initial values.
	//
	// Variable 'm_state.sceExe.runClock' is defined to be the time at the start of the current execution
	// loop, and the current execution of the while loop is the transition
	//
	//		t(m_state.sceExe.runClock) to t(m_state.sceExe.runClock+1).
	//
	// When initially set up then, the transition is
	//
	//		t(start time) to t(start time + 1).
	//
	// Therefore if the simulation began at 12:00:00 in the morning (000000), the first
	// step through the while-loop takes the simulation from 12:00:00 (000000) to
	// 12:00:01 (000001).
	//
	// Variable 'iteration' is similar to variable 'm_state.sceExe.runClock' but starts at zero.
	/*----------------------------------------------------------------------------------//
	  Update the State
	//----------------*/
	// Indicate the thread is iterating.  This is the first of only two locations in the
	// code where the activity is set to SCE_RUNITERATING
	m_state.activity = ___SCE_RUNITERATING;
	//printf("0x%x:thread_RunScenario(): ___SCE_RUNITERATING\n", this);
	prevSceAct = m_state.activity;

	//----------------------------------------------------------------------------------//

	throttleIteration = m_throttleIterations;
	m_state.currentIteration++;
	static BOOL printed = FALSE;
/*
	printf("0x%x:thread_RunScenario(): entering iteration loop\n", this);
	printf("                           current iteration: %d\n", m_state.currentIteration);
	printf("                           durationless:      %d\n", sce.user.durationless);
	printf("                           duration:          %d\n", sce.duration);
	printf("                           throttleIteration  %d\n", throttleIteration);
	printf("                           RESULT:            %s\n", m_staticLib.MbsResultToString(m_state.errorStatus, szMessageBuffer, sizeof(szMessageBuffer)));
*/
	while((m_state.currentIteration <= sce.duration || sce.user.durationless == TRUE) && m_state.errorStatus == OK)
	{
		// This while loop is for throttleing
		while(throttleIteration == 0 && (m_state.currentIteration <= sce.duration || sce.user.durationless == TRUE))
		{
			if(m_abort == TRUE || m_state.errorStatus != OK)
				break;
			/*--------------------------------------------------------------------------//
			  Update the State
			//----------------*/
			// Signal to the calling process/thread that 3mbs scenario is paused and
			// waiting on additional iterations to be set by it.  This is the only place
			// in the code where the state activity is set to ___SCE_PAUSED.
			m_state.activity = ___SCE_PAUSED;
			if(prevSceAct != m_state.activity)
			{
				//printf("0x%x:thread_RunScenario(): ___SCE_PAUSED\n", this);
				prevSceAct = m_state.activity;
			}

			//--------------------------------------------------------------------------//
			
										// activty(___SCE_PAUSED), active(locked), pause(unlocked)
			m_mbsPausedMutex.Lock();

			// Give the calling process an opportunity to set the number of iterations to
			// run.						
										// activty(___SCE_PAUSED), active(locked), pause(locked)
			m_mbsActiveMutex.Unlock(); 

										// activty(___SCE_PAUSED), active(unlocked), pause(locked)
			Sleep(1); // Give the external process a chance to grab the mutex, set the
					   // throttle iterations, and advance.


			m_mbsActiveMutex.Lock();

			m_mbsPausedMutex.Unlock();
			// m_throttleIterations gets changed by a call from the outside calling
			// thread/process.
			if(m_throttleIterations != 0)
			{
				// An external application set the number of throttle iterations.  Update
				// the local copy of throttleIteration and sleep for a moment to give the
				// external application a moment to advance before continuing.
				throttleIteration = m_throttleIterations;
			}
			//Sleep(1);
			printed = TRUE;
		}// end while(throttleIteration == 0 && (m_state.currentIteration < sce.duration || sce.user.durationless == TRUE))


		printed = FALSE;

		if(m_abort == TRUE || m_state.errorStatus != OK)
			break;

		//------------------------------------------------------------------------------//
		// 3MBS Iteration/Simulation Phase
		//--------------------------------//
		m_state.runClock++; // Advance m_state.sceExe.runClock and iteration.
		m_state.currentAnimat = 0;

		/*------------------------------------------------------------------------------//
		  Update the State
		//----------------*/
		// Indicate the thread has resumed iterating.  This is the second of only two 
		// locations in code where the activity is set to SCE_RUNITERATING
		m_state.activity = ___SCE_RUNITERATING;
		if(prevSceAct != m_state.activity)
		{
			//printf("0x%x:thread_RunScenario(): ___SCE_RUNITERATING\n", this);
			prevSceAct = m_state.activity;
		}
		//------------------------------------------------------------------------------//

		//------------------------------------------------------------------------------//
		// Acoustic exposure from acoustic animat (send only every 5 seconds after the
		// 10th second).
		//--------------//
		BOOL anySorceActiveThisIteration = FALSE;

		// Determine if any acoustic source is active this iteration.
		for(i=0; i<sce.totalNumAcstcSrcs && anySorceActiveThisIteration == FALSE; i++)
		{
			if(AcousticSourcePings(m_state.currentIteration, *acstcSrcList.Get(i)) == TRUE)
				anySorceActiveThisIteration = TRUE;
		}


		// To (for now) prevent mulitple sound sources from firing at the same iteration.
		BOOL acstSourcePingedThisIteration = FALSE;

		if(sce.totalNumAcstcSrcs > 0 && anySorceActiveThisIteration == TRUE && sce.totalNumAnimats > 0 && m_state.currentIteration > 0)
		{

			// Get the coordinates of the very first animat in the scenario.
			//animatPopulationCoordBuffer = m_speciesList.Get(0)->GetAnimatCoord(0);
			GetAnimatPopulationCurrentCoordinates(aeCoord);

			// Set the acoustic exposure values for all animats except the first.
			memset(acstcExprCalc, 0, sizeof(double)*sce.totalNumAnimats);


			// Vector summation will be required for each active animat... for now, will
			// break if multiple acoustic sources active at once.
			//acstcSrcLen = acstcSrcList.Length();
			_ASSERT(m_mbSce.totalNumAcstcSrcs == (DWORD)acstcSrcList.Length());
			for(i=0; i<(DWORD)m_mbSce.totalNumAcstcSrcs && acstSourcePingedThisIteration == FALSE; i++)
			{
				// Get acoustic source i' summary
				acsticSrcSum = acstcSrcList.Get(i);

				// Interested only in the first acoustic source that pings this iteration.
				if(AcousticSourcePings(m_state.currentIteration, *acsticSrcSum) == FALSE)
					continue;

				acstSourcePingedThisIteration = TRUE;

				// Set the animat exposure.  Some sort of summantion and vector addition required.
				for(j=0; j<sce.totalNumAnimats; j++)
				{
					if(j < (DWORD)m_mbSce.totalNumAcstcSrcs)
					{
						acstcExprCalc[j] = 0;
						continue;
					}

					// m_staticLib.DetermineBearing() also returns distance.  Distance is needed to determine the acoustic exposure
					// values.
					da = m_staticLib.DetermineBearing(aeCoord[i].lat, aeCoord[i].lon, aeCoord[j].lat, aeCoord[j].lon);
					radius = sqrt(pow(da.distance, 2) + pow(aeCoord[i].depth - aeCoord[i].depth, 2));

					// Fill in animat j's expousre to acoustic source i.
					acstcExprCalc[j] = acsticSrcSum->outputValue;
					if(radius > 1)
						acstcExprCalc[j] = acsticSrcSum->outputValue - 20*log10(radius);
				}

				// The lat/lon coordinate will have to be a composite if multiple animats are used in the future.
				SetAnimatAcousticExposure(aeCoord[i].lat, aeCoord[i].lon, acstcExprCalc);
			}

		}
		//------------------------------------------------------------------------------//


		//------------------------------------------------------------------------------//
		// Update the animats
		//-------------------//
		_ASSERT(m_mbSce.numSpecies == (DWORD)m_speciesList.Length());
		for(i=0; i <m_mbSce.numSpecies  && m_abort == FALSE && m_state.errorStatus == OK; i++)
		{
			m_speciesList.Get(i)->Update(&m_state.currentAnimat);
#if 0
			if(currentTickCount - lastSleep > 1500)
			{
				Sleep(1);
				currentTickCount = lastSleep = GetTickCount();
			}
			else
			{
				currentTickCount = GetTickCount();
			}
#endif
		}


		if(m_firstAnimatStateHoldArray != NULL)
		{
			// Copy into the current iteration's index (=m_state.currentIteration + 1) of
			// m_firstAnimatStateHoldArray the current state of the first animat (at
			// index 0)
			memcpy(&m_firstAnimatStateHoldArray[m_state.currentIteration],
				&animatStateArray[0],
				sizeof(ANIMATSTATE));
		}

		//------------------------------------------------------------------------------------//
		// Statistical Information
		// Update the sceTakes each iteration if output is enabled and post run stat analysis set.
		//------------------------------------------------------------------------------------//
		if(sce.user.output.enabled == TRUE && sce.user.output.headerInf.postRunAnalysis == TRUE)
			UpdateStatisticalCalculations(
				&sceTakes, // compleletely recalculated each iteration
				speTakes, //compleletely recalculated each iteration
				&resets, // updated each iteration
				speciesAnimatState // animat state array arranged by species
				);
		//------------------------------------------------------------------------------------//

		//------------------------------------------------------------------------------------//
		// Output animat state data for this iteration to the buffer
		//----------------------------------------------------------//
		// If TRUE is returned the buffer reached capacity and a thread was launched to empty
		// the buffer contents.  Wait for the thread to exit.
		if(TRUE == m_fileMgr.WriteStateDataToIOBuffer(&sce, m_state.runClock, &m_speciesList, m_state.acousticSrc, &m_abort))
		{
			fmBuffState = m_fileMgr.GetBufferStatus();
			m_state.activity = ___SCE_RUNBUFFERFLUSH;
			while(m_fileMgr.IsFlushingBuffer() == TRUE)
			{
				Sleep(10);
				fmBuffState = m_fileMgr.GetBufferStatus();
			}
		}
		//------------------------------------------------------------------------------------//
		throttleIteration = --m_throttleIterations;
		m_state.currentIteration++;
	}

#if 0
	this was originally inside the above loop.
		// Calculate progress update
		currentTick = GetTickCount();
		if(currentTick >= nextWindowUpdate)
		{
			// Calulate percent done.
			m_state.percentDone = 
				(DWORD)floor(100 * (double)m_state.currentIteration / (double)sce.duration);
			m_state.elapsedTicks = GetTickCount() - m_state.startTick;

			// Calculate approximate time remaining
			// Calculate average iterations per ms second.
			elapsedMs = currentTick - startTick;
			msPerIterationAve = elapsedMs/(double)m_state.currentIteration;

			// Calculate number of iterations remaining.
			// Multiply for number of ms.
			fSecsRem = (msPerIterationAve*(sce.duration - m_state.currentIteration))/1000.0;
			if(m_bufferFlushed == TRUE)
				m_state.secsRemaining = (DWORD)fSecsRem;
			else
				m_state.secsRemaining = 0;

			// post percent done and number of minuites estimated remaining.
			lastTick = currentTick;
			nextWindowUpdate += 500;
		}
#endif

	//----------------------------------------------------------------------------------//
	// Flush the remaining contents of the buffers.  Buffer flushes have the CFileManager
	// instance launch a separate thread, so wait for the thread to exit before
	// continuing.
	if(m_abort == FALSE && m_exit == FALSE && m_state.errorStatus == OK && TRUE == m_fileMgr.FlushBuffer(&sce, &m_abort))
	{
		fmBuffState = m_fileMgr.GetBufferStatus();
		m_state.activity = ___SCE_RUNBUFFERFLUSH;
		while(m_fileMgr.IsFlushingBuffer() == TRUE)
		{
			Sleep(10);
			fmBuffState = m_fileMgr.GetBufferStatus();
		}
	}

	// Write statistical data to file.
	if(m_state.errorStatus == OK)
		m_fileMgr.FillBinFileStatsRegion(&sce, &sceTakes, speTakes);

#if 0
	m_state.elapsedTicks = GetTickCount() - m_state.startTick;
#endif

	// Used when an animat was set to be the acoustic source.
	if(acstcExprCalc != NULL)
		delete [] acstcExprCalc;
	acstcExprCalc = NULL;

	// Have the File Manager close the handle to the binary file and close down.
	m_fileMgr.UninitializeRun(OK);

	//---------------------------------------------------------------------------------//
	// Signal to the user "how" this simulation ended
	//-----------------------------------------------//
	if(m_abort == FALSE)
		m_state.runNumber++;

	//-------------------------------------------------------------------------------------//
	// Deallocate memory
	//------------------//
	if(aeCoord != NULL)
		delete [] aeCoord;
	aeCoord = NULL;

	_ASSERT(m_mbSce.numSpecies == (DWORD)m_speciesList.Length());
	for(i=0; i<(DWORD)m_mbSce.numSpecies; i++)
	{
		((CSpecies *)m_speciesList.Get(i))->DeinitializeRun();
	}

	if(animatStateArray != NULL)
		delete [] animatStateArray;
	animatStateArray = NULL;

	if(speTakes != NULL)
		delete [] speTakes;
	speTakes = NULL;


	if(m_SpeTakesCopy != NULL)
		delete [] m_SpeTakesCopy;
	m_SpeTakesCopy = NULL;

	if(resets.speBuff != NULL)
		delete [] resets.speBuff;
	resets.speBuff = NULL;


	acstcSrcList.DeleteAll();

#if 0
	//this gets moved
	if(sce.user.output.enabled == TRUE && sce.user.output.   text.enabled == TRUE && m_abort == FALSE)
	{
		_ExtractBinaryResultsIntoTextFiles(binFileStrings->fileTitle, binFileStrings->fileName); // Not running in a thread here.
	}
#endif

	/*----------------------------------------------------------------------------------//
	  Post simulation resets.
	//----------------*/
	m_state.currentIteration = 0;
//	m_state.startTick = 0;
	m_throttleIterations = 0;

#if 0
	// m_state.percentDone is moved.
	if(m_abort == FALSE)
		m_state.percentDone = 100;
#endif
	//----------------------------------------------------------------------------------//

	// This is one of two locations in the code where the activity is set to RUN_FINISHED.  The
	// activity may only be set to RUN_FINISHED when the scenario class is initialized in the
	// constructor and in the run scenario thread when the scenario is finished running
	// as scenario.
	m_state.activity = __RUN_FINISHED;

	// Indicate scenario is not running
	m_runThread.m_thread1Running = FALSE;
	m_mbsActiveMutex.Unlock();

	// Post simulation assertions.
	_ASSERTE(m_runThread.m_thread1Running == FALSE); 
	_ASSERTE(m_runThread.m_thread3Running == FALSE);
#endif//NOCSENARIOFUNCTIONS

	//printf("_RunScenario() exiting\n");

}


BOOL CScenario::AcousticSourcePings(DWORD CurrentIteration, ACOUSTICSRCEINF AcstcSrc)
{
#ifndef NOCSENARIOFUNCTIONS
	if(CurrentIteration < AcstcSrc.beginIteration || AcstcSrc.dutyPeriod == 0)
		return FALSE;
	
	if((CurrentIteration - AcstcSrc.beginIteration) % AcstcSrc.dutyPeriod != 0)
		return FALSE;
#endif//NOCSENARIOFUNCTIONS

	return TRUE;
}

	// For command prompt
RESLT CScenario::ExtractBinaryDataToText(TCHAR *szFileName)
{
#ifndef NOCSENARIOFUNCTIONS
	// Keep compiler warning quiet until this function implemented
	szFileName = szFileName;
	//CFileExtracter ext;
#endif//NOCSENARIOFUNCTIONS

	return OK;
	// For command prompt
	//m_fileMgr.extract
}

CWorkingList *CScenario::GetWorkingList(int StartTime, int Duration)
{
	CWorkingList *pWorkingList = NULL;

#ifndef NOCSENARIOFUNCTIONS
	pWorkingList = m_fileMgr.GetWorkingList(StartTime, Duration, &m_mbSce.intrvlOutptLim);
#endif//NOCSENARIOFUNCTIONS
	return pWorkingList;
}


// \.
// \
// \\
// empty
// .
// ..
RESLT CScenario::SetOutputDirectory(TCHAR *Directory)
{
#ifndef NOCSENARIOFUNCTIONS

	//int valu;
	int strLen = strlen(Directory);
	int index = strLen-1;
	WIN32_FIND_DATA FindFileData;
	HANDLE hdl;

	if(Directory == NULL || strlen(Directory) == 0)
		return OK;

	// Remove any backslashes, back up the index.
	while(Directory[index] == '\\')
		Directory[index--] = NULL;

	// Verify there are characters remaining
	if(strlen(Directory) == 0)
		return OK;

	// Verify this is a valid directory
	//valu = INVALID_HANDLE_VALUE;
	if(INVALID_HANDLE_VALUE == (hdl = FindFirstFile(Directory, &FindFileData)))
		return INVALID_DIRECTORY;

	FindClose(hdl);
	
	strncpy_s(m_mbSce.user.szOutputDir, sizeof(m_mbSce.user.szOutputDir), Directory, strlen(Directory));
#endif//NOCSENARIOFUNCTIONS

	return OK;
}


// Remove this later... for now it doesn't do anything and is kept as an example.
void CScenario::RunThread2()
{
#ifndef NOCSENARIOFUNCTIONS
	_runUpdateThread();
#endif//NOCSENARIOFUNCTIONS
}

// Remove this later... for now it doesn't do anything and is kept as an example.
void CScenario::_runUpdateThread()
{
#ifndef NOCSENARIOFUNCTIONS

	m_runThread.m_thread2Running = TRUE;
	while(m_exit == FALSE)
	{
		Sleep(200);
	}
	m_runThread.m_thread2Running = FALSE;
#endif//NOCSENARIOFUNCTIONS
}


/*******************************************************************************
* MEMBER FUNCTION: AbortRun()
* 
* DESCRIPTION:
*	Called by the running environment's main thread to stop execution of the
*	simulation and data extraction.
*
* ARGUMENTS:
*	None.
*
* MEMBER VARIABLES ALTERED:
*	Sets m_abort to TRUE.
*
* RETURN VALUE:
*	None
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   06/27/05  (MJC)  Initial Coding/Transfer
*******************************************************************************/
void CScenario::AbortRun()
{	
#ifndef NOCSENARIOFUNCTIONS
	m_abort = TRUE;
#endif//NOCSENARIOFUNCTIONS
}

void CScenario::Exit()
{	
#ifndef NOCSENARIOFUNCTIONS
	m_exit = TRUE;
	m_abort = TRUE;
#endif//NOCSENARIOFUNCTIONS
}


/*******************************************************************************
* MEMBER FUNCTION: AddSpecies()
* 
* DESCRIPTION:
*	Adds a cluster to to the m_speciesList array of pointers to clusters.  Passed in
*	is the title of a binary species model (class CSpeciesModel) to load into 
*	the cluster.  To add the cluster to the array, the original array needs to 
*	be deallocated and a new array (with one addition array cell) allocated.
*	The information from the original array needs to be copied into the new
*	array.
*
*	A linked list would have worked nicely here, but I didn't use one.  Instead,
*	the m_speciesList member variable is a double pointer.  The allocated or 
*	deallocated memory is really just an array of memory address where each
*	cell of the array points to an instance of a CSpecies class. 
*
*	While programming, it helps to keep in mind that the member variable
*	m_speciesList is a double pointer to a CSpecies, but is thought of as an array
*	of pointers to instances of class CSpecies
*
*		CSpecies **m_speciesList; // Declaration
*
*		CSpecies *m_speciesList[100]; // If it were a fixed length.
*
* ARGUMENTS:
*	OPENFILENAME *ofn - a pointer to an OPENFILENAME structure.  ofn contains
*						the title of the binary CSpeciesModel file to load
*						into the cluster being added.
*
* MEMBER VARIABLES ALTERED:
*	Appears to increase the length of m_speciesList by 1.  In actuallity, it
*	repoints m_speciesList to a newly allocated memory that is an array with one
*	more cell than the one it was previously pointing to.
*
* RETURN VALUE:
*	Returns the newly added species' index number
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   06/27/05  (MJC)  Initial Coding/Transfer
*******************************************************************************/
RESLT CScenario::AddSpecies(OPENFILENAME *ofn, int *Index)
{
	RESLT res = OK;
#ifndef NOCSENARIOFUNCTIONS
	res = AddSpecies(ofn->lpstrFile, Index);
#endif//NOCSENARIOFUNCTIONS
	return res;
}

// Updated 6-17-09 to include sound source counts.
// Places sound sources at the begining of the linked list
RESLT CScenario::AddSpecies(TCHAR *FileName, int *Index)
{
	RESLT res = OK;
#ifndef NOCSENARIOFUNCTIONS
	int i, index;
	CSpecies *spe;

	// Verify counts are correct.
	_ASSERT(m_mbSce.numSpecies == (DWORD)m_speciesList.Length());

	// Add a species to the list of species.
	spe = m_speciesList.Add();
	if(spe == NULL)
		return MEMALLOC_ERROR;

	
	// Attempt to load the speceis in from file.
	if(OK != (res = spe->LoadModelFromBinFile(FileName)))
	{
		// Attempt to load species in from file failed so delete the entry in the list,
		// decrement the count of the number of species in the list, and return result.
		m_speciesList.Delete(m_speciesList.Length()-1);
		return res;
	}

	// Increment the number of species present and set variable 'index' to the end of the
	// list.
	m_mbSce.numSpecies++;
	index = m_mbSce.numSpecies - 1;
	int speListLen;


	// Handle cases where species type loaded in was really a sound source model by moving
	// location of it in the list toward the top of the list but after any previously
	// loaded sound source models (if any).
	if(GetSpeciesAtIndex(index)->GetSpeciesType() == SOUNDSOURCE)
	{
		// Increment the number of sound sources present.
		m_mbSce.numAcstSrcTypes++;

		// Determine the index to set the newly added sound source into.  Looking for
		// index at the front of the list but after any sound sources models already present.
		speListLen = m_speciesList.Length();
		for(i=0; i<speListLen; i++)
		{
			if(GetSpeciesAtIndex(i)->GetSpeciesType() != SOUNDSOURCE)
				break;
		}
		
		// If non sound source found ('i' will be less than the length of the species
		// list) move the newly added sound source into that non-sound source's location.
		// Move() will insert the sound souce into 'i's' location and move remaining
		// species models back one.
		if(i < speListLen/* && i != index*/)
		{
			m_speciesList.Move(index, i);
			index = i;
		}

		// Verify that all sound source model types reside at the front of the list.
		for(i=0; i<GetSoundSourceTypeCount(); i++)
			_ASSERT(GetSpeciesAtIndex(i)->GetSpeciesType() == SOUNDSOURCE);
	}

	if(Index != NULL)
		*Index = index;

#endif//NOCSENARIOFUNCTIONS
	return res;
}

int CScenario::GetSpeciesCount()
{
	int cnt = 0;
#ifndef NOCSENARIOFUNCTIONS
	_ASSERT(m_mbSce.numSpecies == (DWORD)m_speciesList.Length());
	cnt = m_mbSce.numSpecies;
#endif//NOCSENARIOFUNCTIONS
	return cnt;
}

int CScenario::GetPodCount()
{
	DWORD i;
	int count=0;
#ifndef NOCSENARIOFUNCTIONS
	_ASSERT(m_mbSce.numSpecies == (DWORD)m_speciesList.Length());

	// Get a total count of pods in all species.
	for(i=0; i<m_mbSce.numSpecies && m_exit==FALSE; i++)
		count += (m_speciesList.Get(i))->GetNumberOfPods();
#endif//NOCSENARIOFUNCTIONS
	return count;
}

int CScenario::GetPodCount(int SpeciesIndex)
{
	int cnt = 0;
#ifndef NOCSENARIOFUNCTIONS
	AsserteSpeciesIndex(SpeciesIndex);
	cnt = (m_speciesList.Get(SpeciesIndex))->GetNumberOfPods();
#endif//NOCSENARIOFUNCTIONS
	return cnt;
}


// Returns the total count of individuals in all species.
int CScenario::GetIndivdualCount()
{
	DWORD i;
	int count=0;
#ifndef NOCSENARIOFUNCTIONS
	_ASSERT(m_mbSce.numSpecies == (DWORD)m_speciesList.Length());
	for(i=0; i<m_mbSce.numSpecies; i++)
		count += (m_speciesList.Get(i))->GetNumberOfIndividuals();
#endif//NOCSENARIOFUNCTIONS
	return count;
}

int CScenario::GetIndivdualCount(int SpeciesIndex)
{
	int cnt = 0;
#ifndef NOCSENARIOFUNCTIONS
	AsserteSpeciesIndex(SpeciesIndex);
	cnt = (m_speciesList.Get(SpeciesIndex))->GetNumberOfIndividuals();
#endif//NOCSENARIOFUNCTIONS
	return cnt;
}

// Returns the total animat count of all species in scenario.
int CScenario::GetAnimatCount()
{
	int ret = 0;

#ifndef NOCSENARIOFUNCTIONS
#ifdef _DEBUG
	DWORD i = 0;
	int count = 0;

	if(m_abort == FALSE && m_exit == FALSE)
	{
		count = m_speciesList.Length();
		_ASSERT(m_mbSce.numSpecies == (DWORD)count);
		count = 0;
		for(i=0; i<m_mbSce.numSpecies; i++)
			count += (m_speciesList.Get(i))->GetTotalAnimatCount();
		_ASSERT(m_mbSce.totalNumAnimats == (DWORD)count);
	}
#endif
	ret =  m_mbSce.totalNumAnimats;
#endif//NOCSENARIOFUNCTIONS

	return ret;
}

// Updated 6-17-09 to include sound source counts.
int CScenario::UpdateTotalAnimatCount()
{
	int ret = 0;
#ifndef NOCSENARIOFUNCTIONS
	DWORD i;
	CSpecies *spe;

	// Always wise to check a few assertions.  Num species currently includes the number of
	// sound source models.
	_ASSERT(m_mbSce.numSpecies == (DWORD)m_speciesList.Length());
	//_ASSERT(m_mbSce.

	// Rest the animat and acoustic source counts to zero.
	m_mbSce.totalNumAnimats = 0;
	m_mbSce.totalNumAcstcSrcs = 0;

	// For each species or sound source present update total counts.
	for(i=0; i<m_mbSce.numSpecies; i++)
	{
		// Currently the total animat count includes sound sources and species.
		spe = GetSpeciesAtIndex(i);
		m_mbSce.totalNumAnimats += spe->GetTotalAnimatCount();

		// Update total sound source counts.
		if(spe->GetSpeciesType() == SOUNDSOURCE)
			m_mbSce.totalNumAcstcSrcs += spe->GetTotalAnimatCount();
	}
	ret = m_mbSce.totalNumAnimats;
#endif//NOCSENARIOFUNCTIONS

	return ret;
}


BOOL CScenario::SetSpeciesAtIndexAsSoundSource(int Index)
{
#ifndef NOCSENARIOFUNCTIONS
	CSpecies *pSpe = NULL;
	if(Index < 0 || Index >= m_speciesList.Length())
		return FALSE;
	pSpe = m_speciesList.Get(AsserteSpeciesIndex(Index));
	pSpe->SetAsSoundSource();
#endif//NOCSENARIOFUNCTIONS
	return TRUE;
}

CSpecies *CScenario::GetSpeciesAtIndex(int Index)
{
	CSpecies *spe = NULL;
#ifndef NOCSENARIOFUNCTIONS
	spe = m_speciesList.Get(AsserteSpeciesIndex(Index));
#endif//NOCSENARIOFUNCTIONS
	return spe;
}

int CScenario::GetAnimatCount(int SpeciesIndex)
{
	int cnt = 0;
#ifndef NOCSENARIOFUNCTIONS
	cnt = GetSpeciesAtIndex(SpeciesIndex)->GetTotalAnimatCount();
#endif//NOCSENARIOFUNCTIONS
	return cnt;
}

int CScenario::GetPodMemberCount(int SpeciesIndex, int PodIndex)
{
	int cnt = 0;
#ifndef NOCSENARIOFUNCTIONS
	cnt = GetSpeciesAtIndex(SpeciesIndex)->GetNumberAnimatsInPod(PodIndex);
#endif//NOCSENARIOFUNCTIONS
	return cnt;
}

// Dynamically sets acoustic source properties (ping cycle, output dB, starting
// iteration, etc.) based on the number of acoustic sources present.  So each
// time an acoustic source is added or removed this function is called.
RESLT CScenario::ResetAcousticSourceInf()
{
	RESLT res = OK;
#ifndef NOCSENARIOFUNCTIONS
	ACOUSTICSRCEINF *asInf;
	int i,j;
	DWORD cnt;
	CSpecies *spe;
	int speListLen;

	// Assert prpoper animat and acoustic source counts.
	_ASSERT(m_mbSce.totalNumAnimats == (DWORD)GetAnimatCount());
	_ASSERT(m_mbSce.totalNumAcstcSrcs == (DWORD)GetTotalSoundSourceCount());

	// Allocate memory for an array to hold information (starting iteration, ping cycle,
	// etc) about each acoustic sound source present.
	if(NULL == (asInf = new ACOUSTICSRCEINF[m_mbSce.totalNumAcstcSrcs]))
		return MEMALLOC_ERROR;

	// Fill in the array that contains information about each acoustic source.
	cnt = 0;
	speListLen = m_speciesList.Length();
	for(i=0; i<speListLen; i++)
	{
		if(m_speciesList.Get(i)->GetSpeciesType() != SOUNDSOURCE)
			continue;// Not an acoustic source so don't care.

		// This changes the animat's acoustic source setup.
		for(j=0; j<((CSpecies *)m_speciesList.Get(i))->GetTotalAnimatCount(); j++)
		{
			asInf[cnt].beginIteration = ACSTC_SOURCE_BEGINS_ITERATION + cnt;
			asInf[cnt].dutyPeriod = ACSTC_SOURCE_DUTY_PERIOD + cnt;
			asInf[cnt].outputValue = ACSTC_SOURCE_LEVEL_DB;
			m_speciesList.Get(i)->SetInhabitantAcousticSourceInf(j, asInf[cnt]);
			cnt++;
		}
	}

	// Clears the acoustic sources from the file manager's (the output manager's,
	// actually) list of acoustic sources.  'acousticPingCycleOutputLimit' is the flag
	// that indicates output is limited to acoustic source ping cycles.
	if(m_mbSce.acousticPingCycleOutputLimit == FALSE)
	{
		// If output is not limited to acoustic ping cycles then done here.
		delete [] asInf;
		return m_fileMgr.ResetAcousticSourceInf(NULL, 0);
	}


	// Otherwise, set the file manager's (the output manager's, actually) acoustic
	// sources list
	cnt = 0;
	for(i=0; i<speListLen && cnt < m_mbSce.totalNumAcstcSrcs; i++)
	{
		if((spe = m_speciesList.Get(i))->GetSpeciesType() != SOUNDSOURCE)
			continue;

		for(j=0; j<spe->GetTotalAnimatCount() && cnt < m_mbSce.totalNumAcstcSrcs; j++)
		{
			// Function GetAnimatCurrentCoord() needs to have its name changed to SetInhabinantInformation().
			asInf[cnt] = spe->GetAnimatCurrentCoord(j).acstcSrc;
			cnt++;
		}
	}

	_ASSERT(cnt == m_mbSce.totalNumAcstcSrcs);
	res = m_fileMgr.ResetAcousticSourceInf(asInf, m_mbSce.totalNumAcstcSrcs);

	delete [] asInf;
#endif//NOCSENARIOFUNCTIONS
	return res;
}

RESLT CScenario::AddPod(int SpeciesIndex, PODLEADERTYPE LeaderType, double FocalDistance, int BufferLength, INHABITINF *InitialCond)
{
	RESLT res = OK;
#ifndef NOCSENARIOFUNCTIONS
	AsserteSpeciesIndex(SpeciesIndex);
	if(OK != (res = GetSpeciesAtIndex(SpeciesIndex)->AddPod(LeaderType, FocalDistance, BufferLength, InitialCond)))
		return res;
	UpdateTotalAnimatCount();
	res = ResetAcousticSourceInf();

	// These are needed for all animat and species adds and deletions because the addition
	// or deletion of an acoustic source inhabitant when the scenario is setup to limit
	// output to the pings of an acoustic source (or sources) effects the output.
	if(SpeciesIsASoundSourceModel(SpeciesIndex) && m_mbSce.acousticPingCycleOutputLimit == TRUE)
		m_mbSce.numSaveIterations = m_fileMgr.GetStateSaveToFileCount(m_mbSce.startTime, m_mbSce.duration, &m_mbSce.intrvlOutptLim);
	_ASSERT(m_mbSce.numSaveIterations == (DWORD)m_fileMgr.GetStateSaveToFileCount(m_mbSce.startTime, m_mbSce.duration, &m_mbSce.intrvlOutptLim));
#endif//NOCSENARIOFUNCTIONS
	return res;
}

RESLT CScenario::AddPodMembers(int SpeciesIndex, int PodIndex, INHABITINF *InitialCond, int AnimatCount)
{
	RESLT res = OK;
#ifndef NOCSENARIOFUNCTIONS
	AsserteSpeciesIndex(SpeciesIndex);
	if(OK != (res = GetSpeciesAtIndex(SpeciesIndex)->AddPodMembers(PodIndex, InitialCond, AnimatCount)))
		return res;
	UpdateTotalAnimatCount();
	res = ResetAcousticSourceInf();

	// These are needed for all animat and species adds and deletions because the addition
	// or deletion of an acoustic source inhabitant when the scenario is setup to limit
	// output to the pings of an acoustic source (or sources) effects the output.
	if(SpeciesIsASoundSourceModel(SpeciesIndex) && m_mbSce.acousticPingCycleOutputLimit == TRUE)
		m_mbSce.numSaveIterations = m_fileMgr.GetStateSaveToFileCount(m_mbSce.startTime, m_mbSce.duration, &m_mbSce.intrvlOutptLim);
	_ASSERT(m_mbSce.numSaveIterations == (DWORD)m_fileMgr.GetStateSaveToFileCount(m_mbSce.startTime, m_mbSce.duration, &m_mbSce.intrvlOutptLim));
#endif//NOCSENARIOFUNCTIONS
	return res;
}

RESLT CScenario::AddPodMember(int SpeciesIndex, int PodIndex, INHABITINF InitialCond)
{
	RESLT res = OK;
#ifndef NOCSENARIOFUNCTIONS
	if(OK != (res = m_speciesList.Get(AsserteSpeciesIndex(SpeciesIndex))->AddPodMembers(PodIndex, &InitialCond, 1)))
		return res;
	UpdateTotalAnimatCount();
	res = ResetAcousticSourceInf();

	// These are needed for all animat and species adds and deletions because the addition
	// or deletion of an acoustic source inhabitant when the scenario is setup to limit
	// output to the pings of an acoustic source (or sources) effects the output.
	if(SpeciesIsASoundSourceModel(SpeciesIndex) && m_mbSce.acousticPingCycleOutputLimit == TRUE)
		m_mbSce.numSaveIterations = m_fileMgr.GetStateSaveToFileCount(m_mbSce.startTime, m_mbSce.duration, &m_mbSce.intrvlOutptLim);
	_ASSERT(m_mbSce.numSaveIterations == (DWORD)m_fileMgr.GetStateSaveToFileCount(m_mbSce.startTime, m_mbSce.duration, &m_mbSce.intrvlOutptLim));
#endif//NOCSENARIOFUNCTIONS
	return res;
}


RESLT CScenario::AddIndividuals(int SpeciesIndex, INHABITINF *InitialCond, int AnimatCount)
{
	RESLT res = OK;
#ifndef NOCSENARIOFUNCTIONS
	if(OK != (res = (m_speciesList.Get(AsserteSpeciesIndex(SpeciesIndex)))->AddIndividuals(InitialCond, AnimatCount)))
		return res;
	UpdateTotalAnimatCount();
	res = ResetAcousticSourceInf();

	// These are needed for all animat and species adds and deletions because the addition
	// or deletion of an acoustic source inhabitant when the scenario is setup to limit
	// output to the pings of an acoustic source (or sources) effects the output.
	if(SpeciesIsASoundSourceModel(SpeciesIndex) && m_mbSce.acousticPingCycleOutputLimit == TRUE)
		m_mbSce.numSaveIterations = m_fileMgr.GetStateSaveToFileCount(m_mbSce.startTime, m_mbSce.duration, &m_mbSce.intrvlOutptLim);
	_ASSERT(m_mbSce.numSaveIterations == (DWORD)m_fileMgr.GetStateSaveToFileCount(m_mbSce.startTime, m_mbSce.duration, &m_mbSce.intrvlOutptLim));
#endif//NOCSENARIOFUNCTIONS
	return res;
}

RESLT CScenario::AddIndividual(int SpeciesIndex, INHABITINF InitialCond)
{
	RESLT res = OK;
#ifndef NOCSENARIOFUNCTIONS
	if(OK != (res = GetSpeciesAtIndex(AsserteSpeciesIndex(SpeciesIndex))->AddIndividuals(&InitialCond, 1)))
		return res;
	UpdateTotalAnimatCount();
	res = ResetAcousticSourceInf();

	// These are needed for all animat and species adds and deletions because the addition
	// or deletion of an acoustic source inhabitant when the scenario is setup to limit
	// output to the pings of an acoustic source (or sources) effects the output.
	if(SpeciesIsASoundSourceModel(SpeciesIndex) && m_mbSce.acousticPingCycleOutputLimit == TRUE)
		m_mbSce.numSaveIterations = m_fileMgr.GetStateSaveToFileCount(m_mbSce.startTime, m_mbSce.duration, &m_mbSce.intrvlOutptLim);
	_ASSERT(m_mbSce.numSaveIterations == (DWORD)m_fileMgr.GetStateSaveToFileCount(m_mbSce.startTime, m_mbSce.duration, &m_mbSce.intrvlOutptLim));
#endif//NOCSENARIOFUNCTIONS
	return res;
}


void CScenario::DeletePod(int SpeciesIndex, int PodIndex)
{
#ifndef NOCSENARIOFUNCTIONS
	(m_speciesList.Get(AsserteSpeciesIndex(SpeciesIndex)))->DeletePod(PodIndex);
	UpdateTotalAnimatCount();
	ResetAcousticSourceInf();

	// These are needed for all animat and species adds and deletions because the addition
	// or deletion of an acoustic source inhabitant when the scenario is setup to limit
	// output to the pings of an acoustic source (or sources) effects the output.
	if(SpeciesIsASoundSourceModel(SpeciesIndex) && m_mbSce.acousticPingCycleOutputLimit == TRUE)
		m_mbSce.numSaveIterations = m_fileMgr.GetStateSaveToFileCount(m_mbSce.startTime, m_mbSce.duration, &m_mbSce.intrvlOutptLim);
	_ASSERT(m_mbSce.numSaveIterations == (DWORD)m_fileMgr.GetStateSaveToFileCount(m_mbSce.startTime, m_mbSce.duration, &m_mbSce.intrvlOutptLim));
#endif//NOCSENARIOFUNCTIONS
}

void CScenario::DeleteIndividual(int SpeciesIndex, int IndividualIndex)
{
#ifndef NOCSENARIOFUNCTIONS
	(m_speciesList.Get(AsserteSpeciesIndex(SpeciesIndex)))->DeleteIndividual(IndividualIndex);
	UpdateTotalAnimatCount();
	ResetAcousticSourceInf();

	// These are needed for all animat and species adds and deletions because the addition
	// or deletion of an acoustic source inhabitant when the scenario is setup to limit
	// output to the pings of an acoustic source (or sources) effects the output.
	if(SpeciesIsASoundSourceModel(SpeciesIndex) && m_mbSce.acousticPingCycleOutputLimit == TRUE)
		m_mbSce.numSaveIterations = m_fileMgr.GetStateSaveToFileCount(m_mbSce.startTime, m_mbSce.duration, &m_mbSce.intrvlOutptLim);
	_ASSERT(m_mbSce.numSaveIterations == (DWORD)m_fileMgr.GetStateSaveToFileCount(m_mbSce.startTime, m_mbSce.duration, &m_mbSce.intrvlOutptLim));
#endif//NOCSENARIOFUNCTIONS
}

void CScenario::DeleteIndividuals(int SpeciesIndex)
{
#ifndef NOCSENARIOFUNCTIONS
	(m_speciesList.Get(AsserteSpeciesIndex(SpeciesIndex)))->DeleteAllIndividuals();
	UpdateTotalAnimatCount();
	ResetAcousticSourceInf();
#endif//NOCSENARIOFUNCTIONS

}

void CScenario::DeletePods(int SpeciesIndex)
{
#ifndef NOCSENARIOFUNCTIONS
	(m_speciesList.Get(AsserteSpeciesIndex(SpeciesIndex)))->DeleteAllPods();
	UpdateTotalAnimatCount();
	ResetAcousticSourceInf();

	// These are needed for all animat and species adds and deletions because the addition
	// or deletion of an acoustic source inhabitant when the scenario is setup to limit
	// output to the pings of an acoustic source (or sources) effects the output.
	if(SpeciesIsASoundSourceModel(SpeciesIndex) && m_mbSce.acousticPingCycleOutputLimit == TRUE)
		m_mbSce.numSaveIterations = m_fileMgr.GetStateSaveToFileCount(m_mbSce.startTime, m_mbSce.duration, &m_mbSce.intrvlOutptLim);
	_ASSERT(m_mbSce.numSaveIterations == (DWORD)m_fileMgr.GetStateSaveToFileCount(m_mbSce.startTime, m_mbSce.duration, &m_mbSce.intrvlOutptLim));
#endif//NOCSENARIOFUNCTIONS
}

void CScenario::DeletePodMember(int SpeciesIndex, int PodIndex, int AnimatIndex)
{
#ifndef NOCSENARIOFUNCTIONS
	(m_speciesList.Get(AsserteSpeciesIndex(SpeciesIndex)))->DeletePodMember(PodIndex, AnimatIndex);
	UpdateTotalAnimatCount();
	ResetAcousticSourceInf();

	// These are needed for all animat and species adds and deletions because the addition
	// or deletion of an acoustic source inhabitant when the scenario is setup to limit
	// output to the pings of an acoustic source (or sources) effects the output.
	if(SpeciesIsASoundSourceModel(SpeciesIndex) && m_mbSce.acousticPingCycleOutputLimit == TRUE)
		m_mbSce.numSaveIterations = m_fileMgr.GetStateSaveToFileCount(m_mbSce.startTime, m_mbSce.duration, &m_mbSce.intrvlOutptLim);
	_ASSERT(m_mbSce.numSaveIterations == (DWORD)m_fileMgr.GetStateSaveToFileCount(m_mbSce.startTime, m_mbSce.duration, &m_mbSce.intrvlOutptLim));
#endif//NOCSENARIOFUNCTIONS
}


COORD_DEPTH CScenario::GetPodFocalCoordinate(int SpeciesIndex, int PodIndex)
{
	COORD_DEPTH cd = {0};
#ifndef NOCSENARIOFUNCTIONS
	AsserteSpeciesIndex(SpeciesIndex);
	cd = (m_speciesList.Get(SpeciesIndex))->GetPodFocalCoordinate(PodIndex);
#endif//NOCSENARIOFUNCTIONS
	return cd;
}

double CScenario::GetPodLeaderFocalDistance(int SpeciesIndex, int PodIndex)
{
	double dbl = 0;
#ifndef NOCSENARIOFUNCTIONS
	AsserteSpeciesIndex(SpeciesIndex);
	dbl = (m_speciesList.Get(SpeciesIndex))->GetPodLeaderFocalDistance(PodIndex);
#endif//NOCSENARIOFUNCTIONS
	return dbl;
}

void CScenario::SetPodLeaderFocalDistance(int SpeciesIndex, int PodIndex, double FocalDistance)
{
#ifndef NOCSENARIOFUNCTIONS
	AsserteSpeciesIndex(SpeciesIndex);
	(m_speciesList.Get(SpeciesIndex))->SetPodLeaderFocalDistance(PodIndex, FocalDistance);
#endif//NOCSENARIOFUNCTIONS
}


double CScenario::GetShoreFollowingDepth(int SpeciesIndex)
{
	double ret = 0;
#ifndef NOCSENARIOFUNCTIONS
	AsserteSpeciesIndex(SpeciesIndex);
	ret = (m_speciesList.Get(SpeciesIndex))->GetShoreFollowingDepth();
#endif//NOCSENARIOFUNCTIONS
	return ret;
}

double CScenario::GetMinimumSeededingDepth(int SpeciesIndex)
{
	double ret = 0;
#ifndef NOCSENARIOFUNCTIONS
	AsserteSpeciesIndex(SpeciesIndex);
	ret = (m_speciesList.Get(SpeciesIndex))->GetMinimumSeedingDepth();
#endif//NOCSENARIOFUNCTIONS
	return ret;
}


PODLEADERTYPE CScenario::GetPodLeaderType(int SpeciesIndex, int PodIndex)
{
	PODLEADERTYPE ret = ANIMAT;
#ifndef NOCSENARIOFUNCTIONS
	AsserteSpeciesIndex(SpeciesIndex);
	ret = (m_speciesList.Get(SpeciesIndex))->GetPodLeaderType(PodIndex);
#endif//NOCSENARIOFUNCTIONS
	return ret;
}

void CScenario::SetPodLeaderType(int SpeciesIndex, int PodIndex, PODLEADERTYPE Type)
{
#ifndef NOCSENARIOFUNCTIONS
	AsserteSpeciesIndex(SpeciesIndex);
	(m_speciesList.Get(SpeciesIndex))->SetPodLeaderType(PodIndex, Type);
#endif//NOCSENARIOFUNCTIONS
}

DWORD CScenario::GetNumberOfBehaviorsModeledInScenario()
{
	DWORD num = 0;
#ifndef NOCSENARIOFUNCTIONS
	DWORD i;
	_ASSERT(m_mbSce.numSpecies == (DWORD)m_speciesList.Length());
	for(i=0; i<m_mbSce.numSpecies; i++)
		num += GetNumberOfBehaviorsModeledInSpecies(i);
#endif//NOCSENARIOFUNCTIONS
	return num;
}

DWORD CScenario::GetNumberOfBehaviorsModeledInSpecies(int SpeciesIndex)
{
	DWORD ret = 0;
#ifndef NOCSENARIOFUNCTIONS
	AsserteSpeciesIndex(SpeciesIndex);
	ret = (m_speciesList.Get(SpeciesIndex))->GetNumBehaviorsModeled();
#endif//NOCSENARIOFUNCTIONS
	return ret;
}

void CScenario::GetCopyOfBehaviorNamesModeledInSpecies(int SpeciesIndex, BEHAVIOR_NAME *CopyBuffer, DWORD BufferSize)
{
#ifndef NOCSENARIOFUNCTIONS
	AsserteSpeciesIndex(SpeciesIndex);
	(m_speciesList.Get(SpeciesIndex))->CopyBehaviorNames(CopyBuffer, SZ_BEHAVIOR_LEN, BufferSize);
#endif//NOCSENARIOFUNCTIONS
}

void CScenario::EnableEsmeAcousticExposureSetTracking()
{
#ifndef NOCSENARIOFUNCTIONS
	m_mbSce.enableAcstSrcTracking = TRUE;
#endif//NOCSENARIOFUNCTIONS
}

/*******************************************************************************
* MEMBER FUNCTION: SetSpeciesDisplayTitle()
* 
* DESCRIPTION:
*	Tells the instance of class CSpecies at a certain index to set it's display
*	title to the string passed in.  The display title is used by the GUI to
*	associate a title with a populated model.  Typically, the title will be
*	the title of the binary file the user selected to load into the scenario
*	(but with the extension removed).
*
* ARGUMENTS:
*	DWORD Index - The index of the instance of class CSpecies to have set its
*				  title.
*	TCHAR *Title - The title.
*
* MEMBER VARIABLES ALTERED:
*	None.
*
* RETURN VALUE:
*	None.
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   06/27/05  (MJC)  Initial Coding/Transfer
*******************************************************************************/
void CScenario::SetSpeciesDisplayTitle(int Index, TCHAR *Title)
{
#ifndef NOCSENARIOFUNCTIONS
	AsserteSpeciesIndex(Index);
	m_speciesList.Get(Index)->SetDisplayTitle(Title);
#endif//NOCSENARIOFUNCTIONS
}

/*******************************************************************************
* MEMBER FUNCTION: GetSpeciesDisplayTitle()
* 
* DESCRIPTION:
*	Retrieves the display title from the instance of class CSpecies at the
*	specified index.
*
* ARGUMENTS:
*	DWORD Index - The index in the array of pointers to instances of class
*				  CSpecies from which to retrieve the display title.
*	TCHAR *Buff  - A buffer to store the title.
*	DWORD BufferSize - The size of the buffer that will store the title.
*
* MEMBER VARIABLES ALTERED:
*	None
*
* RETURN VALUE:
*	None
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   06/27/05  (MJC)  Initial Coding/Transfer
*******************************************************************************/
void CScenario::GetSpeciesDisplayTitle(int Index, TCHAR *Buff, DWORD BufferSize)
{
#ifndef NOCSENARIOFUNCTIONS
	AsserteSpeciesIndex(Index);
	memset(Buff, 0, BufferSize);
	m_speciesList.Get(Index)->GetDisplayTitle(Buff, BufferSize);
#endif//NOCSENARIOFUNCTIONS
}


/*******************************************************************************
* MEMBER FUNCTION: DeleteSpecies()
* 
* DESCRIPTION:
*	Removes a cluster from the array of pointers to clusters (m_speciesList).
*	Essentially the reverse of AddSpecies(), so see AddSpecies() for details of
*	how this is implemented.
*
* ARGUMENTS:
*	DWORD Index - The index of the CSpecies instance and memory pointer to the
*				  CSpecies index to delete.
*
* MEMBER VARIABLES ALTERED:
*	m_speciesList
*
* RETURN VALUE:
*	None
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   06/27/05  (MJC)  Initial Coding/Transfer
*******************************************************************************/
void CScenario::DeleteSpecies(int Index)
{
#ifndef NOCSENARIOFUNCTIONS
	CSpecies *spe;
	AsserteSpeciesIndex(Index);
	BOOL updateSaveIteraionCnt = FALSE;

	spe = m_speciesList.Get(Index);

	if(spe->GetSpeciesType() == SOUNDSOURCE)
	{
		m_mbSce.numAcstSrcTypes--;
		m_mbSce.totalNumAcstcSrcs -= spe->GetTotalAnimatCount();

		if(m_mbSce.acousticPingCycleOutputLimit == TRUE)
			updateSaveIteraionCnt = TRUE;
	}	

	spe->DeletePopulation();
	m_speciesList.Delete(Index);
	m_mbSce.numSpecies--;
	_ASSERT(m_mbSce.numSpecies == (DWORD)m_speciesList.Length());
	UpdateTotalAnimatCount();
	ResetAcousticSourceInf();

	if(updateSaveIteraionCnt == TRUE)
		m_mbSce.numSaveIterations = m_fileMgr.GetStateSaveToFileCount(m_mbSce.startTime, m_mbSce.duration, &m_mbSce.intrvlOutptLim);
	_ASSERT(m_mbSce.numSaveIterations == (DWORD)m_fileMgr.GetStateSaveToFileCount(m_mbSce.startTime, m_mbSce.duration, &m_mbSce.intrvlOutptLim));
#endif//NOCSENARIOFUNCTIONS
}


void CScenario::DeleteSpecies()
{
#ifndef NOCSENARIOFUNCTIONS
	DWORD i;
	_ASSERT(m_mbSce.numSpecies == (DWORD)m_speciesList.Length());

	for(i=0; i<m_mbSce.numSpecies; i++)
		(m_speciesList.Get(i))->DeletePopulation();
	m_speciesList.DeleteAll();
	m_mbSce.numSpecies = 0;
	m_mbSce.totalNumAnimats = 0;
	m_mbSce.numAcstSrcTypes = 0;
	m_mbSce.totalNumAcstcSrcs = 0;
	ResetAcousticSourceInf();

	if(m_mbSce.acousticPingCycleOutputLimit == TRUE)
		m_mbSce.numSaveIterations = m_fileMgr.GetStateSaveToFileCount(m_mbSce.startTime, m_mbSce.duration, &m_mbSce.intrvlOutptLim);
	_ASSERT(m_mbSce.numSaveIterations == (DWORD)m_fileMgr.GetStateSaveToFileCount(m_mbSce.startTime, m_mbSce.duration, &m_mbSce.intrvlOutptLim));
#endif//NOCSENARIOFUNCTIONS
}


/*******************************************************************************
* MEMBER FUNCTION: SetConfiguration()
* 
* DESCRIPTION:
*
* ARGUMENTS:
*
* MEMBER VARIABLES ALTERED:
*	m_config
*
* RETURN VALUE:
*	None
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   06/27/05  (MJC)  Initial Coding/Transfer
*******************************************************************************/
void CScenario::SetConfiguration(USERPARAMS Configuration)
{
#ifndef NOCSENARIOFUNCTIONS
	m_mbSce.user = Configuration;

	// Override specific user configuration values that are no longer user cnfigurable.

	// Starting version 5.3 and on June 15, 2009, user can not directly configure
	// the first animat to be an active sound source.  Instead sound sources are 
	// added through a file.
	if(GetTotalSoundSourceCount() == 0)
		m_mbSce.user.acousticAnimatActive = FALSE; // this is needed for post scenario processing
	else
		m_mbSce.user.acousticAnimatActive = TRUE;
#endif//NOCSENARIOFUNCTIONS
}

/*******************************************************************************
* MEMBER FUNCTION: GetConfiguration()
* 
* DESCRIPTION:
*	Returns a copy of the member variable m_config.
*
* ARGUMENTS:
*	None.
*
* MEMBER VARIABLES ALTERED:
*	None
*
* RETURN VALUE:
*	None.
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   06/27/05  (MJC)  Initial Coding/Transfer
*******************************************************************************/
USERPARAMS CScenario::GetConfiguration()
{
	USERPARAMS ret = {0};
#ifndef NOCSENARIOFUNCTIONS
	ret = m_mbSce.user;
#endif//NOCSENARIOFUNCTIONS
	return ret;
}


/*******************************************************************************
* MEMBER FUNCTION: SetDuration()
* 
* DESCRIPTION:
*	Sets the member variable m_mbSce.duration to the duration passed in (converted to
*	seconds).
*
* ARGUMENTS:
*	HHMMSS Duration - a structure type HHMMSS holding the duration for the 
*					  simulation to run in hours, minuites, and seconds.
*
* MEMBER VARIABLES ALTERED:
*	m_mbSce.duration
*
* RETURN VALUE:
*	None.
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   06/27/05  (MJC)  Initial Coding/Transfer
*******************************************************************************/
void CScenario::SetDuration(HHMMSS Duration)
{
#ifndef NOCSENARIOFUNCTIONS
	m_mbSce.duration = m_staticLib.Time_ToSeconds(Duration);
	SetDuration(m_mbSce.duration);
#endif//NOCSENARIOFUNCTIONS
}
void CScenario::SetDuration(int Seconds)
{
#ifndef NOCSENARIOFUNCTIONS
	if(Seconds < 0)
		Seconds = 0;
	m_mbSce.duration = Seconds;
	ResetAcousticSourceInf();
	m_mbSce.numSaveIterations = m_fileMgr.GetStateSaveToFileCount(m_mbSce.startTime, m_mbSce.duration, &m_mbSce.intrvlOutptLim);
#endif//NOCSENARIOFUNCTIONS
}

int CScenario::SetAcousticSrceLimitOutput(BOOL Limit)
{
	int ret = 0;
#ifndef NOCSENARIOFUNCTIONS
	m_mbSce.acousticPingCycleOutputLimit = Limit;

	// Sets the acoustic properties of each sound source populated in the scenario to
	// default values and indicates to the file manager that the acoustic limit is set
	// or unset so it can determine the save states to file.
	ResetAcousticSourceInf();
	ret = m_mbSce.numSaveIterations = m_fileMgr.GetStateSaveToFileCount(m_mbSce.startTime, m_mbSce.duration, &m_mbSce.intrvlOutptLim);
#endif//NOCSENARIOFUNCTIONS
	return ret;
}

BOOL CScenario::GetAcousticSrceLimitOutput()
{
	BOOL ret = FALSE;
#ifndef NOCSENARIOFUNCTIONS
	ret = m_mbSce.acousticPingCycleOutputLimit;
#endif//NOCSENARIOFUNCTIONS
	return ret;
}

BOOL CScenario::CSVFileLoaded()
{
	BOOL ret = FALSE;
#ifndef NOCSENARIOFUNCTIONS
	ret = m_fileMgr.CSVFileLoaded();
#endif//NOCSENARIOFUNCTIONS
	return ret;
}

// Tells the scenario to read in a list of specific time states that are to be saved to file.
RESLT CScenario::ReadCSVListFromFile(TCHAR *szFileName)
{
	RESLT res = OK;
#ifndef NOCSENARIOFUNCTIONS
	if(OK != (res = m_fileMgr.ReadCSVListFromFile(szFileName)))
		return res;
	m_mbSce.numSaveIterations = m_fileMgr.GetStateSaveToFileCount(m_mbSce.startTime, m_mbSce.duration, &m_mbSce.intrvlOutptLim);
#endif//NOCSENARIOFUNCTIONS
	return OK;
}

// Tells the scenario to clear the list of specific time states save to file.
void CScenario::ClearCSVList()
{
#ifndef NOCSENARIOFUNCTIONS
	m_fileMgr.ClearCSVList();
	m_mbSce.numSaveIterations = m_fileMgr.GetStateSaveToFileCount(m_mbSce.startTime, m_mbSce.duration, &m_mbSce.intrvlOutptLim);
#endif//NOCSENARIOFUNCTIONS
}

void CScenario::SetItervalLimitOutput(BOOL Enable, DWORD Start, DWORD Interval)
{
#ifndef NOCSENARIOFUNCTIONS
	m_mbSce.intrvlOutptLim.enabled = Enable;
	m_mbSce.intrvlOutptLim.start = Start;
	m_mbSce.intrvlOutptLim.interval = Interval;
	m_mbSce.numSaveIterations = m_fileMgr.GetStateSaveToFileCount(m_mbSce.startTime, m_mbSce.duration, &m_mbSce.intrvlOutptLim);
#endif//NOCSENARIOFUNCTIONS
}


void CScenario::GetItervalLimitOutput(BOOL *pEnable, DWORD *pStart, DWORD *pInterval)
{
#ifndef NOCSENARIOFUNCTIONS
	*pEnable = m_mbSce.intrvlOutptLim.enabled;
	*pStart = m_mbSce.intrvlOutptLim.start;
	*pInterval = m_mbSce.intrvlOutptLim.interval;
#endif//NOCSENARIOFUNCTIONS
}

BOOL CScenario::IntervalLimitOutputEnabled()
{
	BOOL ret = FALSE;
#ifndef NOCSENARIOFUNCTIONS
	ret = m_mbSce.intrvlOutptLim.enabled;
#endif//NOCSENARIOFUNCTIONS
	return ret;
}

/*******************************************************************************
* MEMBER FUNCTION: GetDuration()
* 
* DESCRIPTION:
*	Retrives the duration the simulation is to run.
*
* ARGUMENTS:
*	None.
*
* MEMBER VARIABLES ALTERED:
*	None.
*
* RETURN VALUE:
*	A HHMMSS structure containing the duration the simulation is to run in an
*	hour, min, sec format.
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   06/27/05  (MJC)  Initial Coding/Transfer
*******************************************************************************/
HHMMSS CScenario::GetDuration()
{
	HHMMSS ret = {0};
#ifndef NOCSENARIOFUNCTIONS
	ret = m_staticLib.Time_ToHrMinSec(m_mbSce.duration);
#endif//NOCSENARIOFUNCTIONS
	return ret;
}


int CScenario::GetDurationSeconds()
{
	int ret = 0;
#ifndef NOCSENARIOFUNCTIONS
	ret = m_mbSce.duration;
#endif//NOCSENARIOFUNCTIONS
	return ret;
}

/*******************************************************************************
* MEMBER FUNCTION: SetStartTime()
* 
* DESCRIPTION:
*	Sets the member variable m_mbSce.user.startTime to the time passed in (converted
*	to seconds).
*
* ARGUMENTS:
*	HHMMSS StartTime - a structure type HHMMSS holding the start time of the  
*					   simulation in an hour:minuite:second format.
*
* MEMBER VARIABLES ALTERED:
*	m_mbSce.user.startTime
*
* RETURN VALUE:
*	None.
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   06/27/05  (MJC)  Initial Coding/Transfer
*******************************************************************************/
void CScenario::SetStartTime(HHMMSS StartTime)
{
#ifndef NOCSENARIOFUNCTIONS
	m_mbSce.startTime = m_staticLib.Time_To24HrClockSeconds(StartTime);	
	m_mbSce.numSaveIterations = m_fileMgr.GetStateSaveToFileCount(m_mbSce.startTime, m_mbSce.duration, &m_mbSce.intrvlOutptLim);
#endif//NOCSENARIOFUNCTIONS
}

/*******************************************************************************
* MEMBER FUNCTION: GetStartTime()
* 
* DESCRIPTION:
*	Retrieves the start time of the simulation.
*
* ARGUMENTS:
*	None.
*
* MEMBER VARIABLES ALTERED:
*	None.
*
* RETURN VALUE:
*	A HHMMSS structure containing the start time in an hour:min:sec format.
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   06/27/05  (MJC)  Initial Coding/Transfer
*******************************************************************************/
HHMMSS CScenario::GetStartTime()
{
	HHMMSS ret = {0};
#ifndef NOCSENARIOFUNCTIONS
	ret =  m_staticLib.Time_ToHrMinSec(m_mbSce.startTime);
#endif//NOCSENARIOFUNCTIONS
	return ret;
}



RESLT CScenario::SeedingCoordinateIsValid(UINT SpeciesIndex, double Lat, double Lon)
{
#ifndef NOCSENARIOFUNCTIONS
	BOOL bVal;
	BATHYVALUE bathyValue;
	ENVMINMAX envMinMax;
	BATHYEXTREMES bathyExtremes;
	double shoreFollowingDepth;
	double minimumSeedingDepth;
	AsserteSpeciesIndex(SpeciesIndex);

	//_ASSERT(0);

	if((int)SpeciesIndex >= m_speciesList.Length())
		return INVALID_SPECIES_INDEX_ERROR;

	bVal = m_envData.bathymetry.ConstantValueIsSet();

	// Setting a constant depth is used for demo (as with the SpeciesBuilder application,
	// debugging, and testing so under such scenarios all seeding coordates are
	// permitted to be valid.
	if(TRUE == bVal)
		return OK;

	// Check if coordinate is within bathymetry map boundaries.
	bathyValue = GetBathymetryDepth(Lat, Lon);
	bathyExtremes = m_envData.bathymetry.GetExtremes();
	envMinMax = m_staticLib.BathyExtremesToEnvExtemes(bathyExtremes);

	bVal = m_staticLib.LatLonWithinBoundaries(Lat, Lon, envMinMax);
	if(bVal == FALSE)
		return COORDINATE_OFF_MAP;

	// More shallow than
	minimumSeedingDepth = GetMinimumSeededingDepth(SpeciesIndex);
	if((bathyValue.depth > minimumSeedingDepth) || (bathyValue.depth > BATHY_MIN_SEED_DEPTH))
		return INVALID_SEEEDING_DEPTH;


	// More shallow than
	shoreFollowingDepth = GetShoreFollowingDepth(SpeciesIndex);
	if(bathyValue.depth > shoreFollowingDepth)
		return INVALID_SPECIES_SEEEDING_DEPTH;
#endif//NOCSENARIOFUNCTIONS
	return OK;
}


/*******************************************************************************
* MEMBER FUNCTION: LoadBathymetryFromTextFile()
* 
* DESCRIPTION:
*	Instructs the member variable m_envData.bathymetry to load a bathymetry file into
*	memory.
*
* ARGUMENTS:
*	TCHAR *FileName - a string that is the path and title of the bathymetry data
*					to load.
*
* MEMBER VARIABLES ALTERED:
*	None, although m_envData.bathymetry is altered internally.
*
* RETURN VALUE:
*	A BOOL status result.
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   06/27/05  (MJC)  Initial Coding/Transfer
*******************************************************************************/
RESLT CScenario::LoadBathymetryFromTextFile(OPENFILENAME *Ofn)
{
	RESLT ret = OK;
#ifndef NOCSENARIOFUNCTIONS
	TCHAR szBuff[SIZE_16];
	BOOL isText = FALSE;
	m_staticLib.GetExtension(Ofn->lpstrFileTitle, szBuff, TCHARBFLEN(szBuff));
	if(strcmp(szBuff, ".txt") == 0)
		isText = TRUE;
	ret =  m_envData.bathymetry.LoadFromTextFile(Ofn, isText);
#endif//NOCSENARIOFUNCTIONS
	return ret;
}
RESLT CScenario::LoadBathymetryFromTextFile(TCHAR *FileName)
{
	RESLT ret = OK;
#ifndef NOCSENARIOFUNCTIONS
	TCHAR szBuff[SIZE_16];
	BOOL isText = FALSE;
	m_staticLib.GetExtension(FileName, szBuff, TCHARBFLEN(szBuff));
	if(strcmp(szBuff, ".txt") == 0)
		isText = TRUE;
	ret =  m_envData.bathymetry.LoadFromTextFile(FileName, isText);
#endif//NOCSENARIOFUNCTIONS
	return ret;
}


/*******************************************************************************
* MEMBER FUNCTION: ClearBathymetry()
* 
* DESCRIPTION:
*	Instructs the member variable m_envData.bathymetry to clear its a bathymetry file.
*
* ARGUMENTS:
*	None.
*
* MEMBER VARIABLES ALTERED:
*	None, although m_envData.bathymetry is altered internally.
*
* RETURN VALUE:
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   06/27/05  (MJC)  Initial Coding/Transfer
*******************************************************************************/
void CScenario::ClearBathymetry()
{
#ifndef NOCSENARIOFUNCTIONS
	m_envData.bathymetry.ClearData();
#endif//NOCSENARIOFUNCTIONS
}

/*******************************************************************************
* MEMBER FUNCTION: BathymetryLoaded()
* 
* DESCRIPTION:
*	Returns TRUE if the member variable m_envData.bathymetry has bathymetry data loaded,
*	FALSE otherwise.
*
* ARGUMENTS:
*	None.
*
* MEMBER VARIABLES ALTERED:
*	None.
*
* RETURN VALUE:
*	BOOL
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   06/27/05  (MJC)  Initial Coding/Transfer
*******************************************************************************/
BOOL CScenario::BathymetryLoaded()
{
	BOOL ret = FALSE;
#ifndef NOCSENARIOFUNCTIONS
	ret =  m_envData.bathymetry.IsDataLoaded();
#endif//NOCSENARIOFUNCTIONS
	return ret;
}


/*******************************************************************************
* MEMBER FUNCTION: GetBathymetryFileName()
* 
* DESCRIPTION:
*	Retrieves the title of the binary file from which the bathymetry data was 
*	loaded into the member variable m_envData.bathymetry.
*
* ARGUMENTS:
*	TCHAR *FileNameBuffer - A character buffer to hold the title.
*	int BufferLength - The size of the buffer to hold the title.
*
* MEMBER VARIABLES ALTERED:
*	None.
*
* RETURN VALUE:
*	None.
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   06/27/05  (MJC)  Initial Coding/Transfer
*******************************************************************************/
void CScenario::GetBathymetryFileName(TCHAR *FileNameBuffer, int BufferLength)
{
#ifndef NOCSENARIOFUNCTIONS
	m_envData.bathymetry.GetFileName(FileNameBuffer, BufferLength);
#endif//NOCSENARIOFUNCTIONS
}

/*******************************************************************************
* MEMBER FUNCTION: LoadSalinityFromTextFile()
* 
* DESCRIPTION:
*	Instructs the member variable m_envData.salinity to load a Salinity file into
*	memory.
*
* ARGUMENTS:
*	TCHAR *FileName - a string that is the path and title of the Salinity data
*					to load.
*
* MEMBER VARIABLES ALTERED:
*	None, although m_envData.salinity is altered internally.
*
* RETURN VALUE:
*	A BOOL status result.
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   06/27/05  (MJC)  Initial Coding/Transfer
*******************************************************************************/
RESLT CScenario::LoadSalinityFromTextFile(OPENFILENAME *Ofn)
{
	RESLT ret = OK;
#ifndef NOCSENARIOFUNCTIONS
	TCHAR szBuff[SIZE_16];
	BOOL isText = FALSE;
	m_staticLib.GetExtension(Ofn->lpstrFileTitle, szBuff, TCHARBFLEN(szBuff));
	if(strcmp(szBuff, ".txt") == 0)
		isText = TRUE;

	ret =  m_envData.salinity.LoadFromTextFile(Ofn, isText);
#endif//NOCSENARIOFUNCTIONS
	return ret;
}
RESLT CScenario::LoadSalinityFromTextFile(TCHAR *FileName)
{
	RESLT ret = OK;
#ifndef NOCSENARIOFUNCTIONS
	TCHAR szBuff[SIZE_16];
	BOOL isText = FALSE;
	m_staticLib.GetExtension(FileName, szBuff, TCHARBFLEN(szBuff));
	if(strcmp(szBuff, ".txt") == 0)
		isText = TRUE;

	ret =  m_envData.salinity.LoadFromTextFile(FileName, isText);
#endif//NOCSENARIOFUNCTIONS
	return ret;
}


/*******************************************************************************
* MEMBER FUNCTION: ClearSalinity()
* 
* DESCRIPTION:
*	Instructs the member variable m_envData.salinity to clear its a Salinity file.
*
* ARGUMENTS:
*	None.
*
* MEMBER VARIABLES ALTERED:
*	None, although m_envData.salinity is altered internally.
*
* RETURN VALUE:
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   06/27/05  (MJC)  Initial Coding/Transfer
*******************************************************************************/
void CScenario::ClearSalinity()
{
#ifndef NOCSENARIOFUNCTIONS
	m_envData.salinity.ClearData();
#endif//NOCSENARIOFUNCTIONS
}

/*******************************************************************************
* MEMBER FUNCTION: SalinityLoaded()
* 
* DESCRIPTION:
*	Returns TRUE if the member variable m_envData.salinity has Salinity data loaded,
*	FALSE otherwise.
*
* ARGUMENTS:
*	None.
*
* MEMBER VARIABLES ALTERED:
*	None.
*
* RETURN VALUE:
*	BOOL
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   06/27/05  (MJC)  Initial Coding/Transfer
*******************************************************************************/
BOOL CScenario::SalinityLoaded()
{
	BOOL ret = OK;
#ifndef NOCSENARIOFUNCTIONS
	ret =  m_envData.salinity.IsDataLoaded();
#endif//NOCSENARIOFUNCTIONS
	return ret;
}


/*******************************************************************************
* MEMBER FUNCTION: GetSalinityFileName()
* 
* DESCRIPTION:
*	Retrieves the title of the binary file from which the Salinity data was 
*	loaded into the member variable m_envData.salinity.
*
* ARGUMENTS:
*	TCHAR *FileNameBuffer - A character buffer to hold the title.
*	int BufferLength - The size of the buffer to hold the title.
*
* MEMBER VARIABLES ALTERED:
*	None.
*
* RETURN VALUE:
*	None.
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   06/27/05  (MJC)  Initial Coding/Transfer
*******************************************************************************/
void CScenario::GetSalinityFileName(TCHAR *FileNameBuffer, int BufferLength)
{
#ifndef NOCSENARIOFUNCTIONS
	m_envData.salinity.GetFileName(FileNameBuffer, BufferLength);
#endif//NOCSENARIOFUNCTIONS
}

/*******************************************************************************
* MEMBER FUNCTION: LoadTemperatureFromTextFile()
* 
* DESCRIPTION:
*	Instructs the member variable m_envData.temperature to load a Temperature file into
*	memory.
*
* ARGUMENTS:
*	TCHAR *FileName - a string that is the path and title of the Temperature data
*					to load.
*
* MEMBER VARIABLES ALTERED:
*	None, although m_envData.temperature is altered internally.
*
* RETURN VALUE:
*	A BOOL status result.
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   06/27/05  (MJC)  Initial Coding/Transfer
*******************************************************************************/
RESLT CScenario::LoadTemperatureFromTextFile(OPENFILENAME *Ofn)
{
	RESLT ret = OK;
#ifndef NOCSENARIOFUNCTIONS
	TCHAR szBuff[SIZE_16];
	BOOL isText = FALSE;
	m_staticLib.GetExtension(Ofn->lpstrFileTitle, szBuff, TCHARBFLEN(szBuff));
	if(strcmp(szBuff, ".txt") == 0)
		isText = TRUE;

	ret =  m_envData.temperature.LoadFromTextFile(Ofn, isText);
#endif//NOCSENARIOFUNCTIONS
	return ret;
}
RESLT CScenario::LoadTemperatureFromTextFile(TCHAR *FileName)
{
	RESLT ret = OK;
#ifndef NOCSENARIOFUNCTIONS
	TCHAR szBuff[SIZE_16];
	BOOL isText = FALSE;
	m_staticLib.GetExtension(FileName, szBuff, TCHARBFLEN(szBuff));
	if(strcmp(szBuff, ".txt") == 0)
		isText = TRUE;

	ret =  m_envData.temperature.LoadFromTextFile(FileName,isText);
#endif//NOCSENARIOFUNCTIONS
	return ret;
}
/*******************************************************************************
* MEMBER FUNCTION: ClearTemperature()
* 
* DESCRIPTION:
*	Instructs the member variable m_envData.temperature to clear its a Temperature file.
*
* ARGUMENTS:
*	None.
*
* MEMBER VARIABLES ALTERED:
*	None, although m_envData.temperature is altered internally.
*
* RETURN VALUE:
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   06/27/05  (MJC)  Initial Coding/Transfer
*******************************************************************************/
void CScenario::ClearTemperature()
{
#ifndef NOCSENARIOFUNCTIONS
	m_envData.temperature.ClearData();
#endif//NOCSENARIOFUNCTIONS
}

/*******************************************************************************
* MEMBER FUNCTION: TemperatureLoaded()
* 
* DESCRIPTION:
*	Returns TRUE if the member variable m_envData.temperature has Temperature data loaded,
*	FALSE otherwise.
*
* ARGUMENTS:
*	None.
*
* MEMBER VARIABLES ALTERED:
*	None.
*
* RETURN VALUE:
*	BOOL
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   06/27/05  (MJC)  Initial Coding/Transfer
*******************************************************************************/
BOOL CScenario::TemperatureLoaded()
{
	BOOL ret = FALSE;
#ifndef NOCSENARIOFUNCTIONS
	ret =  m_envData.temperature.IsDataLoaded();
#endif//NOCSENARIOFUNCTIONS
	return ret;
}


void CScenario::ClearScenario()
{
#ifndef NOCSENARIOFUNCTIONS
	DeleteSpecies();
	ClearTemperature();
	ClearSalinity();
	ClearBathymetry();

	///////////////////////////////////////////////////////////////////////////////////////////////
	// Added 10/19/09
	// Sets the configuration to minimum
//	memset(&m_mbSce, 0, sizeof(SCENARIOPARAMS));

//	return;
	if(m_firstAnimatStateHoldArray != NULL)
		delete [] m_firstAnimatStateHoldArray;
	m_firstAnimatStateHoldArray = NULL;
	m_animatStateHoldArrayLength = 0;

	// Binary file output
	m_mbSce.user.output.enabled = TRUE;
	m_mbSce.user.output.outputByTime = TRUE;
	m_staticScenario.SetMinBinOutFileConfiguration(&m_mbSce.user.output);

	memset(&m_state, 0, sizeof(m_state));

	m_envData.bathymetry.SetConstantValue(BATHY_DEFAULT_DEPTH);
	m_envData.salinity.SetConstantValue(0);
	m_envData.temperature.SetConstantValue(0);

	//------------------------//
	// Binary File Management
	//------------------------//
	m_abort = FALSE;
	m_exit = FALSE;
	m_throttleIterations = 0; // zero means it isn't iterating

	memset(&m_state.acousticSrc, 0, sizeof(ACST_SRC_STATE));
#endif//NOCSENARIOFUNCTIONS
}

/*******************************************************************************
* MEMBER FUNCTION: GetTemperatureFileName()
* 
* DESCRIPTION:
*	Retrieves the title of the binary file from which the Temperature data was 
*	loaded into the member variable m_envData.temperature.
*
* ARGUMENTS:
*	TCHAR *FileNameBuffer - A character buffer to hold the title.
*	int BufferLength - The size of the buffer to hold the title.
*
* MEMBER VARIABLES ALTERED:
*	None.
*
* RETURN VALUE:
*	None.
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   06/27/05  (MJC)  Initial Coding/Transfer
*******************************************************************************/
void CScenario::GetTemperatureFileName(TCHAR *FileNameBuffer, int BufferLength)
{
#ifndef NOCSENARIOFUNCTIONS
	m_envData.temperature.GetFileName(FileNameBuffer, BufferLength);
#endif//NOCSENARIOFUNCTIONS
}


/*******************************************************************************
* MEMBER FUNCTION: LoadFromBinFile()
* 
* DESCRIPTION:
*	Loads a previously saved scenario.
*
* ARGUMENTS:
*	TCHAR *FileName - File title and path to saved scenario data.
*
* MEMBER VARIABLES ALTERED:
*	Several
*
* RETURN VALUE:
*	A BOOL status result.
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   06/27/05  (MJC)  Initial Coding/Transfer
*******************************************************************************/
RESLT CScenario::LoadFromBinFile(TCHAR *FileName)
{
#ifndef NOCSENARIOFUNCTIONS
	RESLT res;
	// Verify nothing is already loaded.
	ClearScenario();
	if(OK != (res = m_fileMgr.LoadScenario(FileName, &m_mbSce, &m_envData, &m_speciesList)))
	{
		ResetAcousticSourceInf();
		m_mbSce.numSaveIterations = m_fileMgr.GetStateSaveToFileCount(m_mbSce.startTime, m_mbSce.duration, &m_mbSce.intrvlOutptLim);
		return res;
	}
	ResetAcousticSourceInf();
	m_mbSce.numSaveIterations = m_fileMgr.GetStateSaveToFileCount(m_mbSce.startTime, m_mbSce.duration, &m_mbSce.intrvlOutptLim);
#endif//NOCSENARIOFUNCTIONS
	return OK;
}

/*******************************************************************************
* MEMBER FUNCTION: SaveToBinFile()
* 
* DESCRIPTION:
*	Saves the current scenario to file so it may be used again without having
*	to set it up.
*
* ARGUMENTS:
*	TCHAR *FileName - File title and path to save the scenario data to.
*
* MEMBER VARIABLES ALTERED:
*	None.
*
* RETURN VALUE:
*	A BOOL status result.
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   06/27/05  (MJC)  Initial Coding/Transfer
*******************************************************************************/
RESLT CScenario::SaveToBinFile(TCHAR *FileName)
{
	RESLT ret = OK;
#ifndef NOCSENARIOFUNCTIONS
	ret = m_fileMgr.SaveScenario(FileName, &m_mbSce, &m_envData, &m_speciesList);
#endif//NOCSENARIOFUNCTIONS
	return ret;
}


void CScenario::SetScenarioTitle(char *Title)
{	
#ifndef NOCSENARIOFUNCTIONS
	char szPath[SIZE_256];
	//char szFileName[SIZE_256];
	char szFileTitle[SIZE_256];
	m_staticLib.GetPathAndFileTitleFromFileName(Title, szPath, sizeof(szPath), szFileTitle, sizeof(szFileTitle));
	strncpy_s(m_mbSce.user.szScenarioTitle, sizeof(m_mbSce.user.szScenarioTitle), szFileTitle, SIZE_128);
	m_staticLib.RemoveExtension(m_mbSce.user.szScenarioTitle);
#endif//NOCSENARIOFUNCTIONS
}


void CScenario::CalculateRequiredDiskSpace(DWORDLONG *BinStorage, DWORDLONG *TextStorage)
{
#ifndef NOCSENARIOFUNCTIONS
	BINARYSETUP b;
	DWORDLONG headerBytes;

//	_ASSERT(m_mbSce.totalNumAnimats == GetAnimatCount());
//	_ASSERT(m_mbSce.numSpecies == m_speciesList.Length());

	// Protect member variables from being modified while this function is active.
	m_mbsActiveMutex.Lock();

	b = m_fileManagerStatic.DetermineBinarySetup(m_mbSce, &m_envData, &m_speciesList);

	*BinStorage = (DWORDLONG)b.totalDiskSpace;

	headerBytes = sizeof(TXTHEADER1) + sizeof(TXTHEADER2) + sizeof(TXTHEADER1)*(m_mbSce.numSaveIterations);
	*TextStorage = headerBytes * (DWORDLONG)GetAnimatCount();
	m_mbsActiveMutex.Unlock();
#endif//NOCSENARIOFUNCTIONS
}



void CScenario::CommandPromptOutput(TCHAR *sz)
{
#ifndef NOCSENARIOFUNCTIONS
	printf(sz);
#endif//NOCSENARIOFUNCTIONS
}

CBathymetry *CScenario::GetBathymetryClassRef()
{
	CBathymetry *ret = NULL;
#ifndef NOCSENARIOFUNCTIONS
	ret = &m_envData.bathymetry;
#endif//NOCSENARIOFUNCTIONS
	return ret;
}

double CScenario::SetBaythyConstantDepth(double Depth)
{
	double res = 0;
#ifndef NOCSENARIOFUNCTIONS
	if(Depth >= ANIMAT_BEACHES_DEPTH_DEFAULT)
		Depth = ANIMAT_BEACHES_DEPTH_DEFAULT;

	res = m_envData.bathymetry.SetConstantValue(Depth);

	_ASSERT(m_envData.bathymetry.ConstantValueIsSet() == TRUE);
#endif//NOCSENARIOFUNCTIONS
	return res;
}


BATHYVALUE CScenario::GetBathymetryDepth(double lat, double lon)
{
	BATHYVALUE ret = {0};
#ifndef NOCSENARIOFUNCTIONS
	ret = m_envData.bathymetry.GetValueAtCoordinate(lat, lon);
#endif//NOCSENARIOFUNCTIONS
	return ret;
}
ENVDATAPOINTCOUNT CScenario::GetBathymetryDataPointCounts()
{
	ENVDATAPOINTCOUNT ret = {0};
#ifndef NOCSENARIOFUNCTIONS
	ret = m_envData.bathymetry.GetDataPointCounts(FALSE);
#endif//NOCSENARIOFUNCTIONS
	return ret;
}
BATHYEXTREMES CScenario::GetBathymetryExtremes()
{
	BATHYEXTREMES ret = {0};
#ifndef NOCSENARIOFUNCTIONS
	ret = m_envData.bathymetry.GetExtremes();
#endif//NOCSENARIOFUNCTIONS
	return ret;
}
double CScenario::GetTotalBathySufaceAreaMeters()
{
	double ret = 0;
#ifndef NOCSENARIOFUNCTIONS
	ret = m_envData.bathymetry.GetTotalSufaceAreaMeters();
#endif//NOCSENARIOFUNCTIONS
	return ret;
}
double CScenario::GetBathymetryLandSufaceAreaMeters()
{	
	double ret = 0;
#ifndef NOCSENARIOFUNCTIONS
	ret = m_envData.bathymetry.GetLandSufaceAreaMeters();
#endif//NOCSENARIOFUNCTIONS
	return ret;
}
double CScenario::GetBathymetryWaterSufaceAreaMeters()
{
	double ret = 0;
#ifndef NOCSENARIOFUNCTIONS
	ret = m_envData.bathymetry.GetWaterSurfaceAreaMeters();
#endif//NOCSENARIOFUNCTIONS
	return ret;
}

int CScenario::AsserteSpeciesIndex(int SpeciesIndex)
{
#ifndef NOCSENARIOFUNCTIONS
	_ASSERTE(SpeciesIndex >= 0 && SpeciesIndex <m_speciesList.Length());
#endif//NOCSENARIOFUNCTIONS
	return SpeciesIndex;
}

INHABITINF CScenario::GetPodMemberInitialCoordinate(int SpeciesIndex, int PodIndex, int PodMemberIndex)
{
	INHABITINF coord = {0};
#ifndef NOCSENARIOFUNCTIONS
	AsserteSpeciesIndex(SpeciesIndex);
	m_mbsActiveMutex.Lock();
	coord = (m_speciesList.Get(SpeciesIndex))->GetPodMemberInitialCoordinate(PodIndex, PodMemberIndex);
	m_mbsActiveMutex.Unlock();
#endif//NOCSENARIOFUNCTIONS
	return coord;
}

INHABITINF *CScenario::GetPodInitialCoordinates(int SpeciesIndex, int PodIndex, INHABITINF *IC)
{
	INHABITINF *ic = IC;
#ifndef NOCSENARIOFUNCTIONS
	AsserteSpeciesIndex(SpeciesIndex);

	m_mbsActiveMutex.Lock();
	if(ic == NULL)
		ic = new INHABITINF[(m_speciesList.Get(SpeciesIndex))->GetNumberAnimatsInPod(PodIndex)];
	(m_speciesList.Get(SpeciesIndex))->GetAllPodMemberInitialCoordinates(PodIndex, IC);
	m_mbsActiveMutex.Unlock();
#endif//NOCSENARIOFUNCTIONS
	return ic;
}


INHABITINF CScenario::GetIndividualInitialCoordinate(int SpeciesIndex, int IndividualIndex)
{
	INHABITINF coord = {0};
#ifndef NOCSENARIOFUNCTIONS
	AsserteSpeciesIndex(SpeciesIndex);

	m_mbsActiveMutex.Lock();
	coord = (m_speciesList.Get(SpeciesIndex))->GetIndividualInitialCoordinate(IndividualIndex);
	m_mbsActiveMutex.Unlock();
#endif//NOCSENARIOFUNCTIONS
	return coord;
}

INHABITINF *CScenario::GetIndividualInitialCoordinates(int SpeciesIndex, INHABITINF *IC)
{
	INHABITINF *ic = IC;
#ifndef NOCSENARIOFUNCTIONS
	AsserteSpeciesIndex(SpeciesIndex);

	m_mbsActiveMutex.Lock();
	if(ic == NULL)
		ic = new INHABITINF[(m_speciesList.Get(SpeciesIndex))->GetNumberOfIndividuals()];
	(m_speciesList.Get(SpeciesIndex))->GetAllIndividualInitialCoordinates(ic);
	m_mbsActiveMutex.Unlock();
#endif//NOCSENARIOFUNCTIONS
	return ic;
}


INHABITINF CScenario::GetIndividualCoordinate(int SpeciesIndex, int IndividualIndex)
{
	INHABITINF coord = {0};
#ifndef NOCSENARIOFUNCTIONS
	AsserteSpeciesIndex(SpeciesIndex);

	m_mbsActiveMutex.Lock();
	coord = (m_speciesList.Get(SpeciesIndex))->GetIndividualCoordinate(IndividualIndex); 
	m_mbsActiveMutex.Unlock();
#endif//NOCSENARIOFUNCTIONS
	return coord;
}


// Memory allocation check must be made by the calling function
// Caller must deallocate returned buffer!!!
INHABITINF *CScenario::GetIndividualCoordinates(int SpeciesIndex, INHABITINF *CoordBuffer)
{
#ifndef NOCSENARIOFUNCTIONS
	AsserteSpeciesIndex(SpeciesIndex);

	m_mbsActiveMutex.Lock();
	if(CoordBuffer == NULL)
		CoordBuffer = new INHABITINF[(m_speciesList.Get(SpeciesIndex))->GetNumberOfIndividuals()];
	(m_speciesList.Get(SpeciesIndex))->GetAllIndividualCurrentCoordinates(CoordBuffer);
	m_mbsActiveMutex.Unlock();
#endif//NOCSENARIOFUNCTIONS
	return CoordBuffer;
}

INHABITINF CScenario::GetPodMemberCoordinate(int SpeciesIndex, int PodIndex, int PodMemberIndex)
{
	INHABITINF coord = {0};
#ifndef NOCSENARIOFUNCTIONS
	AsserteSpeciesIndex(SpeciesIndex);

	// Prevent multiple threads from accessing and modifiying the Species linked list.
	m_mbsActiveMutex.Lock();
	coord = (m_speciesList.Get(SpeciesIndex))->GetPodMemberCoordinate(PodIndex, PodMemberIndex);
	m_mbsActiveMutex.Unlock();
#endif//NOCSENARIOFUNCTIONS
	return coord;
}


// Memory allocation check must be made by the calling function
// Caller must deallocate returned buffer!!!
INHABITINF *CScenario::GetPodMemberCoordinates(int SpeciesIndex, int PodIndex, INHABITINF *CoordBuffer)
{
	INHABITINF *coord = CoordBuffer;
#ifndef NOCSENARIOFUNCTIONS
	AsserteSpeciesIndex(SpeciesIndex);

	// Prevent multiple threads from accessing and modifiying the Species linked list.
	m_mbsActiveMutex.Lock();
	if(coord == NULL)
		coord = new INHABITINF[(m_speciesList.Get(SpeciesIndex))->GetNumberAnimatsInPod(PodIndex)];
	(m_speciesList.Get(SpeciesIndex))->GetAllPodMemberCurrentCoordinates(PodIndex, CoordBuffer);
	m_mbsActiveMutex.Unlock();
#endif//NOCSENARIOFUNCTIONS
	return coord;
}


// Get the entire animat population's initial coordinates.  If a NULL pointer is passed
// in, memory is dynamically allocated that the calling process is responsible to
// manage and deallocate.
INHABITINF *CScenario::GetAnimatPopulationInitialCoordinates(INHABITINF *IC)
{
#ifndef NOCSENARIOFUNCTIONS
	DWORD i;
	int startIndex = 0;
	CSpecies *pSpe;

	// Protect member variables from being modified while this function is active.
	if(TRUE == m_mbsActiveMutex.Lock())
	{
		_ASSERT(m_mbSce.totalNumAnimats == (DWORD)GetAnimatCount());
		_ASSERT(m_mbSce.numSpecies == (DWORD)m_speciesList.Length());

		if(IC == NULL)
			IC = new INHABITINF[m_mbSce.totalNumAnimats];

		for(i=0; i<m_mbSce.numSpecies && m_exit == FALSE && m_abort == FALSE; i++)
		{
			pSpe = m_speciesList.Get(i);

			// CSpecies member function GetAllAnimatInitialCoordinates() returns the number
			// of animats assigned to that species.
			startIndex += pSpe->GetAllAnimatInitialCoordinates(&IC[startIndex]);
		}
	}
	m_mbsActiveMutex.Unlock();
#endif//NOCSENARIOFUNCTIONS
	return IC;
}



// Calling applicaiton must deallocate returned buffer
COORD_DEPTH *CScenario::GetAnimatPopulationCurrentCoordinates(COORD_DEPTH *CoordBuffer)
{
#ifndef NOCSENARIOFUNCTIONS
	int i;
	INHABITINF *pIn = NULL;

	_ASSERT(m_mbSce.totalNumAnimats == (DWORD)GetAnimatCount());
	_ASSERT(m_mbSce.numSpecies == (DWORD)m_speciesList.Length());

	if(NULL == (pIn = GetAnimatPopulationCurrentCoordinates(pIn)))
		return CoordBuffer;

	// Allocate memory if needed.
	if((CoordBuffer == NULL) && (NULL == (CoordBuffer = new COORD_DEPTH[m_mbSce.totalNumAnimats])))
	{
		delete [] pIn;
		return CoordBuffer;
	}

	// Copy information
	for(i=0; i<(int)m_mbSce.totalNumAnimats; i++)
		CoordBuffer[i] = pIn[i].coord;

	delete [] pIn;
#endif//NOCSENARIOFUNCTIONS
	return CoordBuffer;
}


// Memory allocation check must be made by the calling function
// Caller must deallocate returned buffer!!!
INHABITINF *CScenario::GetAnimatPopulationCurrentCoordinates(INHABITINF *CoordBuffer)
{
#ifndef NOCSENARIOFUNCTIONS
	DWORD i;
	DWORD startIndex = 0;
	CSpecies *pSpe;

	// Protect member variables from being modified while this function is active.
	if(FALSE == m_mbsActiveMutex.Lock())
		return CoordBuffer;

	_ASSERT(m_mbSce.totalNumAnimats == (DWORD)GetAnimatCount());
	_ASSERT(m_mbSce.numSpecies == (DWORD)m_speciesList.Length());

	if(CoordBuffer == NULL)
		CoordBuffer = new INHABITINF[m_mbSce.totalNumAnimats];

	for(i=0; i<m_mbSce.numSpecies && m_exit == FALSE && m_abort == FALSE; i++)
	{
		pSpe = m_speciesList.Get(i);

		// CSpecies member function GetAllAnimatCurrentCoordinates() returns the number
		// of animats assigned to that species used to determine the starting animat
		// indices of subsequent species.
		startIndex += pSpe->GetAllAnimatCurrentCoordinates(&CoordBuffer[startIndex]);
	}

	if(m_exit==FALSE && m_abort==FALSE)
	{
		_ASSERTE(startIndex == m_mbSce.totalNumAnimats);
	}

	m_mbsActiveMutex.Unlock();
#endif//NOCSENARIOFUNCTIONS
	return CoordBuffer;
}


void CScenario::SetAnimatAcousticExposure(double SourceLat, double SourceLon, double *Array)
{
#ifndef NOCSENARIOFUNCTIONS
	DWORD i;
	DWORD startIndex = 0;
	CSpecies *pSpe;

	// Protect member variables from being modified while this function is active.
	if(FALSE == m_mbsActiveMutex.Lock())
		return;

	_ASSERT(m_mbSce.totalNumAnimats == (DWORD)GetAnimatCount());
	_ASSERT(m_mbSce.numSpecies == (DWORD)m_speciesList.Length());

	memset(&m_state.acousticSrc, 0, sizeof(ACST_SRC_STATE));
	m_state.acousticSrc.lat = (float)SourceLat;
	m_state.acousticSrc.lon = (float)SourceLon;

	// If any value is set, treat the acoustic exposure as being active.
	for(i=0; i<m_mbSce.totalNumAnimats && m_state.acousticSrc.active==FALSE && m_exit==FALSE && m_abort==FALSE; i++)
	{
		if(SourceLat != 0 || SourceLon != 0 || Array[i] != 0)
		{
			m_state.acousticSrc.active = TRUE;
		}
	}

	for(i=0; i<m_mbSce.numSpecies && m_exit == FALSE && m_abort == FALSE; i++)
	{
		pSpe = m_speciesList.Get(i);

		// CSpecies member function SetAcousticExposureAllAnimats() returns the number
		// of animats assigned to that species used to determine the starting animat
		// indices of subsequent species.
		startIndex += pSpe->SetAcousticExposureAllAnimats(SourceLat, SourceLon, &Array[startIndex]);
	}

	if(m_exit==FALSE && m_abort==FALSE)
		_ASSERTE(startIndex == m_mbSce.totalNumAnimats);


	if(m_mbSce.enableAcstSrcTracking ==TRUE)
		m_fileMgr.AcousticDataToFile(m_state.runClock, SourceLat, SourceLon, m_mbSce.totalNumAnimats, Array);

	m_mbsActiveMutex.Unlock();
#endif//NOCSENARIOFUNCTIONS
}

void CScenario::SetAnimatBathymetry(double *Array)
{
	//Array = Array; // quiet compiler warning until this function is removed.

	// This goes away and is replaced by a set up call that passes in a bathymetry array
#if 0
	DWORD i;
	DWORD startIndex = 0;

	// Protect member variables from being modified while this function is active.
	if(FALSE == m_mbsActiveMutex.Lock())
		return;

	_ASSERT(m_mbSce.totalNumAnimats == (DWORD)GetAnimatCount());
	_ASSERT(m_mbSce.numSpecies == (DWORD)m_speciesList.Length());

	for(i=0; i<m_mbSce.numSpecies && m_exit == FALSE && m_abort == FALSE; i++)
	{
		// CSpecies member function SetBathyAllAnimats() returns the number
		// of animats assigned to that species used to determine the starting animat
		// indices of subsequent species.
		startIndex += (m_speciesList.Get(i))->SetBathyAllAnimats(&Array[startIndex]);
	}

	_ASSERTE(startIndex == m_mbSce.numSpecies);

	if(m_mbSce.enableAcstSrcTracking == TRUE && m_esmeInputFp != NULL && m_exit==FALSE && m_abort==FALSE)
	{
		fprintf(m_esmeInputFp, "%06d: Animat Bathymetry:\n\t", m_state.runClock);
		for(i=0; i<m_mbSce.numSpecies; i++)
		{
			fprintf(m_esmeInputFp, "(%d):%.2f ", i, (float)Array[i]);
		}
		fprintf(m_esmeInputFp, "\n");
	}
	m_mbsActiveMutex.Unlock();
#endif
}



// The entire animat population of a single species was requested.
INHABITINF *CScenario::GetAnimatInitialCoordinates(int SpeciesIndex, int BufferLength, INHABITINF *IC)
{
#ifndef NOCSENARIOFUNCTIONS
	int numAnimats;

	// Protect member variables from being modified while this function is active.
	m_mbsActiveMutex.Lock();

	_ASSERT(m_mbSce.totalNumAnimats == (DWORD)GetAnimatCount());
	_ASSERT(m_mbSce.numSpecies == (DWORD)m_speciesList.Length());

	numAnimats = (m_speciesList.Get(SpeciesIndex))->GetTotalAnimatCount();

	// If the passed in buffer to hold the animat coordinates is either NULL
	// or is too small, reallocate memory for it.
	if(IC != NULL && BufferLength < numAnimats)
	{
		delete [] IC;
		IC = NULL;
	}
	if(IC == NULL)
		IC = new INHABITINF[numAnimats];

	(m_speciesList.Get(SpeciesIndex))->GetAllAnimatInitialCoordinates(IC);
	m_mbsActiveMutex.Unlock();
#endif//NOCSENARIOFUNCTIONS
	return IC;
}


INHABITINF *CScenario::GetAnimatCurrentCoordinates(int SpeciesIndex, int BufferLength, INHABITINF *CoordBuffer)
{
#ifndef NOCSENARIOFUNCTIONS

	int numAnimats;

	// Protect member variables from being modified while this function is active.
	m_mbsActiveMutex.Lock();

	_ASSERT(m_mbSce.totalNumAnimats == (DWORD)GetAnimatCount());
	_ASSERT(m_mbSce.numSpecies == (DWORD)m_speciesList.Length());
	AsserteSpeciesIndex(SpeciesIndex);

	numAnimats = (m_speciesList.Get(SpeciesIndex))->GetTotalAnimatCount();

	// If the passed in buffer to hold the animat coordinates is either NULL
	// or is too small, reallocate memory for it.
	if(CoordBuffer != NULL && BufferLength < numAnimats)
	{
		delete [] CoordBuffer;
		CoordBuffer = NULL;
	}
	if(CoordBuffer == NULL)
		CoordBuffer = new INHABITINF[numAnimats];

	(m_speciesList.Get(SpeciesIndex))->GetAllAnimatCurrentCoordinates(CoordBuffer);
	m_mbsActiveMutex.Unlock();
#endif//NOCSENARIOFUNCTIONS
	return CoordBuffer;
}

// Returns TRUE if either the simulation is running or data is being extracted from a
// binary file.
BOOL CScenario::IsActive()
{
#ifndef NOCSENARIOFUNCTIONS
	if(m_state.activity == __RUN_FINISHED)
		return FALSE;
#endif//NOCSENARIOFUNCTIONS
	return TRUE;
}




RUNSTATE CScenario::GetRunState_old()
{
#ifndef NOCSENARIOFUNCTIONS
	// This goes away
	SCESTATE state = GetState();

	switch(state.activity)
	{
	case __RUN_FINISHED: // no actiivty
		return FINISHED;

	case ___ALLOCOUTPUTBUFF: // allocating memory for animat output to file buffer
	case ___SCE_INIT: // initializing scenaro
	case ___SCE_INITANIMATS: // looping through each animat to initialize them
		return  INITIALIZING;

	case ___SCE_RUNITERATING: // Iterating
	case ___SCE_RUNBUFFERFLUSH: // flushing output buffer to file
		return  RUNNING;

	case ___SCE_PAUSED: // running, but paused waiting on calling application input
		return  RUNPAUSED;
	}

	_ASSERT(0); // should never get here.
#endif//NOCSENARIOFUNCTIONS
	return UNKNOWN;
}


SCESTATE CScenario::GetState()
{
	SCESTATE s;
	memset(&s, 0, sizeof(s));
#ifndef NOCSENARIOFUNCTIONS

	// Static when scenario is running
	s.activity = m_state.activity;

	// Dynamic changing or set during scenario execution
	s.runClock = m_state.activity;
	s.currentAnimat = m_state.currentAnimat;
	s.currentIteration = m_state.currentIteration;// animat state number underway

	s.runNumber = m_state.runNumber;
	s.errorStatus = m_state.errorStatus;
	memcpy(&s.acousticSrc, &m_state.acousticSrc, sizeof(ACST_SRC_STATE));

	s.bufferState = m_fileMgr.GetBufferStatus();
#endif//NOCSENARIOFUNCTIONS
	return s;
}



RESLT CScenario::CompareFileOutput(TCHAR *FileName1, TCHAR *FileName2)
{
	// Get the path and file name of the input file.
	RESLT ret = OK;
#ifndef NOCSENARIOFUNCTIONS
	HANDLE hd1;
	HANDLE hd2;
	hd1 = CreateFile(FileName1, GENERIC_READ, NULL, NULL, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, 0);
	if(hd1 == INVALID_HANDLE_VALUE)
		return OPENFILEREAD_ERROR; // error status

	hd2 = CreateFile(FileName2, GENERIC_READ, NULL, NULL, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, 0);
	if(hd2 == INVALID_HANDLE_VALUE)
	{
		CloseHandle(hd1);
		return OPENFILEREAD_ERROR; // error status
	}

	ret = CompareFileOutput(hd1, hd2);
	CloseHandle(hd1);
	CloseHandle(hd2);
#endif//NOCSENARIOFUNCTIONS
	return ret;
}

RESLT CScenario::CompareFileOutput(HANDLE Hd1, HANDLE Hd2)
{
	RESLT ret = OK;
#ifndef NOCSENARIOFUNCTIONS

	int		 numSpecies;
	int		 numAnimals;
	int		 numIterationsEachAnimal;
	int		 remainingIterationsToReadThisAnimal;
	DWORD	 bytesRead;
	int		 iterationsRead;
	int		 totalIterationsReadThisAnimal;
	int		 animatStateLen;
	int		 i, n;
	int		 intCmp;
	double   version;
	double   dlbCmp;
	int		 iores;
	DWORD	 totalBytesRead;
	CEnvironmentalDataStatic envDataStatic;


	float depth, depthCmp, lat, latCmp, lon, lonCmp;

	SPECIESBINOUTINF *clusterInf1, *clusterInf2;
	ANIMATASSCN		*ansm1, *ansm2;
	ANIMATSTATE_FILEOUT		*ast1, *ast2;


	envDataStatic.SkipOverInBinFile(Hd1, &bytesRead);
	envDataStatic.SkipOverInBinFile(Hd1, &bytesRead);
	envDataStatic.SkipOverInBinFile(Hd1, &bytesRead);

	envDataStatic.SkipOverInBinFile(Hd2, &bytesRead);
	envDataStatic.SkipOverInBinFile(Hd2, &bytesRead);
	envDataStatic.SkipOverInBinFile(Hd2, &bytesRead);

	// Compare the number of Species in the two scenarios.
	totalBytesRead = 0;
	iores = 1;
	iores &= ReadFile(Hd1, &version, sizeof(double), &bytesRead, NULL); totalBytesRead += bytesRead;
	iores &= ReadFile(Hd2, &dlbCmp, sizeof(double), &bytesRead, NULL);
	iores &= ReadFile(Hd1, &numSpecies, sizeof(int), &bytesRead, NULL); totalBytesRead += bytesRead;
	iores &= ReadFile(Hd2, &intCmp, sizeof(int), &bytesRead, NULL);
	if(iores == 0)
		return FILEREAD_ERROR;
	if(numSpecies != intCmp)
		return OK_FILESNOTMATCH;

	// Compare the total number of animats in the two scenarios.
	iores &= ReadFile(Hd1, &numAnimals, sizeof(int), &bytesRead, NULL); totalBytesRead += bytesRead;
	iores &= ReadFile(Hd2, &intCmp, sizeof(int), &bytesRead, NULL);
	if(iores == 0)
		return FILEREAD_ERROR;
	if(numAnimals != intCmp)
		return OK_FILESNOTMATCH;

	// Compare the total number of iterations for each animat in the two scenarios.
	iores &= ReadFile(Hd1, &numIterationsEachAnimal, sizeof(int), &bytesRead, NULL); totalBytesRead += bytesRead;
	iores &= ReadFile(Hd2, &intCmp, sizeof(int), &bytesRead, NULL);
	if(iores == 0)
		return FILEREAD_ERROR;
	if(numIterationsEachAnimal != intCmp)
		return OK_FILESNOTMATCH;

	_ASSERTE(totalBytesRead <= SIZE_128);
	// Skip past start time in the two scenarios and the space reserverd for future use.
//	SetFilePointer(Hd1, sizeof(int), NULL, FILE_CURRENT);
//	SetFilePointer(Hd2, sizeof(int), NULL, FILE_CURRENT);
	m_staticLib.MySetFilePointer(Hd1, __int64(SIZE_128) - bytesRead, FILE_CURRENT);
	m_staticLib.MySetFilePointer(Hd2, __int64(SIZE_128) - bytesRead, FILE_CURRENT);

	numIterationsEachAnimal++; // there's one extra iteration representing the initial state.

	// Allocate needed memory.
	if(NULL == (clusterInf1	= new SPECIESBINOUTINF[numSpecies]))
		ret = MEMALLOC_ERROR;
	if(NULL == (ansm1 = new ANIMATASSCN[numAnimals]))
		ret = MEMALLOC_ERROR;
	ast1 = NULL;

	if(NULL == (clusterInf2	= new SPECIESBINOUTINF[numSpecies]))
		ret = MEMALLOC_ERROR;
	if(NULL == (ansm2 = new ANIMATASSCN[numAnimals]))
		ret = MEMALLOC_ERROR;
	ast2 = NULL;

	animatStateLen		= 60*60*2; // *2 because it immediatly divides the length by two in the next calc.

	while(ast1 == NULL && animatStateLen > 2)
	{
		animatStateLen /= 2;
		ast1 = new ANIMATSTATE_FILEOUT[animatStateLen];
		if(ast1 == NULL)
			continue;

		ast2 = new ANIMATSTATE_FILEOUT[animatStateLen];

		if(ast2 == NULL)
		{
			delete [] ast1;
			ast1 = NULL;
		}
	}

	if(ast1 == NULL)
		ret = MEMALLOC_ERROR;

	// Read in all the cluster titles and animat summaries
	for(i=0; i<numSpecies && ret == OK; i++)
	{
		iores &= ReadFile(Hd1, &clusterInf1[i], sizeof(SPECIESBINOUTINF), &bytesRead, NULL);
		iores &= ReadFile(Hd2, &clusterInf2[i], sizeof(SPECIESBINOUTINF), &bytesRead, NULL);
		if(iores == 0)
		{
			ret = FILEREAD_ERROR;
			break;
		}

		m_staticLib.MySetFilePointer(Hd1, clusterInf1[i].description.numBehaviors * sizeof(NAMES), FILE_CURRENT);

		m_staticLib.MySetFilePointer(Hd2, clusterInf2[i].description.numBehaviors * sizeof(NAMES), FILE_CURRENT);
	}

	iores &= ReadFile(Hd1, ansm1, numAnimals * sizeof(ANIMATASSCN), &bytesRead, NULL);
	iores &= ReadFile(Hd2, ansm2, numAnimals * sizeof(ANIMATASSCN), &bytesRead, NULL);
	if(iores == 0)
		ret = FILEREAD_ERROR;

	for(i=0; i<numAnimals && ret == OK; i++)
	{
		remainingIterationsToReadThisAnimal = numIterationsEachAnimal;
		totalIterationsReadThisAnimal = 0;
		while(remainingIterationsToReadThisAnimal > 0 && ret == OK)
		{
			// Read into the fileTitleer, update the pointer.  If the remainingIterationsToReadThisAnimal number 
			if(remainingIterationsToReadThisAnimal <= animatStateLen)
			{
				iores &= ReadFile(Hd1, ast1, sizeof(ANIMATSTATE_FILEOUT)*remainingIterationsToReadThisAnimal, &bytesRead, 0);
				iores &= ReadFile(Hd2, ast2, sizeof(ANIMATSTATE_FILEOUT)*remainingIterationsToReadThisAnimal, &bytesRead, 0);
			}
			else
			{
				iores &= ReadFile(Hd1, ast1, sizeof(ANIMATSTATE_FILEOUT)*animatStateLen, &bytesRead, 0);
				iores &= ReadFile(Hd2, ast2, sizeof(ANIMATSTATE_FILEOUT)*animatStateLen, &bytesRead, 0);
			}

			if(iores == 0)
			{
				ret = FILEREAD_ERROR;
				break;
			}
			iterationsRead = bytesRead / sizeof(ANIMATSTATE_FILEOUT);

			// Read the current animat's states for each iteration.
			for(n=0; n<iterationsRead && ret == OK; n++)
			{
				depth	 = (float)ast1[n].depth;
				depthCmp = (float)ast2[n].depth;
				lat		 = (float)ast1[n].lat ;
				latCmp	 = (float)ast2[n].lat ;
				lon		 = (float)ast1[n].lon;
				lonCmp	 = (float)ast2[n].lon;

				if(depth!=depthCmp || lat!=latCmp || lon!=lonCmp)
					ret = OK_FILESNOTMATCH;
			}

			totalIterationsReadThisAnimal += iterationsRead;
			if(iterationsRead == remainingIterationsToReadThisAnimal || iterationsRead == animatStateLen)
				remainingIterationsToReadThisAnimal -= iterationsRead;
			else
				remainingIterationsToReadThisAnimal = 0;
		}
	}
	if(ast1 != NULL)		delete [] ast1;
	if(clusterInf1 != NULL) delete [] clusterInf1;
	if(ansm1 != NULL)		delete [] ansm1;

	if(ast2 != NULL)		delete [] ast2;
	if(clusterInf2 != NULL) delete [] clusterInf2;
	if(ansm2 != NULL)		delete [] ansm2;
#endif//NOCSENARIOFUNCTIONS
	return ret;
}

void CScenario::ResetRunCount()
{
#ifndef NOCSENARIOFUNCTIONS
	m_state.runNumber = 1;
#endif//NOCSENARIOFUNCTIONS
}

void CScenario::SetRunCount(DWORD RunCount)
{
#ifndef NOCSENARIOFUNCTIONS
	m_state.runNumber = RunCount;
#endif//NOCSENARIOFUNCTIONS
}

DWORD CScenario::GetRunCount()
{
	DWORD ret = 0;
#ifndef NOCSENARIOFUNCTIONS
	ret = m_state.runNumber;
#endif//NOCSENARIOFUNCTIONS
	return ret;
}



RESLT CScenario::SpeciesToText(int SpeciesIndex, FILE *fd)
{
	RESLT ret = OK;
#ifndef NOCSENARIOFUNCTIONS
	AsserteSpeciesIndex(SpeciesIndex);
	ret = (m_speciesList.Get(SpeciesIndex))->ModelToText(fd);
#endif//NOCSENARIOFUNCTIONS
	return ret;
}


RESLT CScenario::SpeciesToText(int SpeciesIndex, TCHAR *FileName)
{
	RESLT ret = OK;
#ifndef NOCSENARIOFUNCTIONS
	AsserteSpeciesIndex(SpeciesIndex);
	ret = (m_speciesList.Get(SpeciesIndex))->ModelToText(FileName);
#endif//NOCSENARIOFUNCTIONS
	return ret;
}


RESLT CScenario::ScenarioToText(TCHAR *FileName)
{
	RESLT result = OK;
#ifndef NOCSENARIOFUNCTIONS
	FILE *fd;

	if(m_exit == TRUE)
		return OK;
	
	fopen_s(&fd, FileName, "w");
	if(NULL == fd )
		return OPENFILEWRITE_ERROR;
	result = ScenarioToText(fd);
	fclose(fd);
#endif//NOCSENARIOFUNCTIONS
	return result;
}

RESLT CScenario::ScenarioToText(FILE *fd)
{
	fd = fd; // quiet compiler warning until this function is worked on.
#if 0
	TCHAR szBuffer[SIZE_128] = {0};
	int i, numSpecies;
	RESLT res = OK;

	if(m_exit == TRUE)
		return res;

	numSpecies = GetSpeciesCount();

	if(-1 == fprintf(fd, "3MBS Lib Ver: %02d.02d", MMBSLIB_VERSION_SUPER, MMBSLIB_VERSION_SUB))
		return FILEWRITE_ERROR;

	// Configuration
	if(OK != (res = ConfigurationToTextFile(fd, &m_config)))
		return res;

	// Run params
	if(OK != (res = RunParamsToTextFile(fd)))
		return res;

	// Species Modeling
	for(i=0; i<numSpecies; i++)
	{
		GetSpeciesDisplayTitle(i, szBuffer, SIZE_128);
		if(-1 == fprintf(fd, "\n\n"))
			return FILEWRITE_ERROR;
		if(-1 == fprintf(fd, "---- Species %d Model Definition----------------------------------------------------------------\n", i))
			return FILEWRITE_ERROR;
		if(-1 == fprintf(fd, "title: %s\n", szBuffer))
			return FILEWRITE_ERROR;
		if(OK != (res = SpeciesToText(i, fd)))
			return res;
		if(-1 == fprintf(fd, "------------------------------------------------------------------------------------------------\n"))
			return FILEWRITE_ERROR;

	}
#endif
	return OK;
}

RESLT CScenario::RunParamsToTextFile(FILE *fd)
{
#ifndef NOCSENARIOFUNCTIONS
	HHMMSS hhmmss;
	int i,j,k;
	int speciesCount = GetSpeciesCount();
	int numPods, numIndividuals, numPodMembers;
	INHABITINF ic;
	TCHAR szFileName[SIZE_256];


	if(-1 == fprintf(fd, "RUN PARAMS\n"))
		return FILEWRITE_ERROR;
	if(-1 == fprintf(fd, "------------------------------------------------------------------------------------------------\n"))
		return FILEWRITE_ERROR;

	hhmmss = m_staticLib.Time_To24HrMinSec(m_mbSce.startTime);
	if(-1 == fprintf(fd, "Start Time\n"))
		return FILEWRITE_ERROR;
	if(-1 == fprintf(fd, "          entered value: %8d\n", m_mbSce.startTime))
		return FILEWRITE_ERROR;
	if(-1 == fprintf(fd, "    translated to clock: %02d:%02d:%02d (HH:MM:SS)\n", hhmmss.hour, hhmmss.min, hhmmss.sec))
		return FILEWRITE_ERROR;

	hhmmss = m_staticLib.Time_ToHrMinSec(m_mbSce.duration);
	if(-1 == fprintf(fd, "\nDuration\n"))
		return FILEWRITE_ERROR;
	if(-1 == fprintf(fd, "          entered value: %8d seconds\n", m_mbSce.duration))
		return FILEWRITE_ERROR;
	if(-1 == fprintf(fd, "    translated to clock: %02d:%02d:%02d (HH:MM:SS)\n", hhmmss.hour, hhmmss.min, hhmmss.sec))
		return FILEWRITE_ERROR;

	if(speciesCount == 0)
	{
		if(-1 == fprintf(fd, "\nZero Species\n"))
			return FILEWRITE_ERROR;
	}

	for(i=0; i<speciesCount; i++)
	{
		if(-1 == fprintf(fd, "\nSpecies %d:\n", i+1))
			return FILEWRITE_ERROR;

		numPods = GetPodCount(i);
		if(numPods == 0)
		{
			if(-1 == fprintf(fd, "  Zero Pods\n"))
				return FILEWRITE_ERROR;
		}
		for(j=0; j<numPods; j++)
		{
			if(-1 == fprintf(fd, "  Pod %d:\n", j+1))
				return FILEWRITE_ERROR;

			if(GetPodLeaderType(i,j) == ANIMAT)
			{
				if(-1 == fprintf(fd, "    pod leader: ANIMAT\n"))
					return FILEWRITE_ERROR;
			}
			else
			{
				if(-1 == fprintf(fd, "    pod leader: calculated CENTROID\n"))
					return FILEWRITE_ERROR;
			}
			if(-1 == fprintf(fd, "    focal distance: %8.3f meters\n", GetPodLeaderFocalDistance(i,j)))
				return FILEWRITE_ERROR;

			numPodMembers = GetPodMemberCount(i,j);
			if(numPodMembers == 0)
			{
				if(-1 == fprintf(fd, "    Zero animats\n"))
					return FILEWRITE_ERROR;
			}
			else
			{
				if(-1 == fprintf(fd, "    Pod Member(s) Initial Coordinates:\n"))
					return FILEWRITE_ERROR;
			}
			for(k=0; k<numPodMembers; k++)
			{
				ic = GetPodMemberInitialCoordinate(i,j,k);
				if(-1 == fprintf(fd, "      Pod Member %d: Lat(%16.14f) Lon(%16.14f) Depth(CMbsWrapper initializes)\n", +1, ic.coord.lat, ic.coord.lon))
					return FILEWRITE_ERROR;
			}
		}

		// Individuals
		numIndividuals = GetIndivdualCount(i);
		if(numIndividuals == 0)
		{
			if(-1 == fprintf(fd, "\n  Zero Individuals"))
				return FILEWRITE_ERROR;
		}

		if(-1 == fprintf(fd, "\n"))
			return FILEWRITE_ERROR;
		for(j=0; j<numIndividuals; j++)
		{
			ic = GetIndividualInitialCoordinate(i,j);
			if(-1 == fprintf(fd, "  Individual %d: Lat(%16.14f) Lon(%16.14f) Depth(CMbsWrapper initializes)\n", +1, ic.coord.lat, ic.coord.lon))
				return FILEWRITE_ERROR;
		}

	}

	if(-1 == fprintf(fd, "\nEnvironmental Data\n"))
		return FILEWRITE_ERROR;

	// Bathymetry
	if(-1 == fprintf(fd, "  Bathymetry:  "))
		return FILEWRITE_ERROR;
	if(BathymetryLoaded() == TRUE)
	{
		GetBathymetryFileName(szFileName, SIZE_256);
		if(-1 == fprintf(fd, "%s\n", szFileName))
			return FILEWRITE_ERROR;
	}
	else
	{
		if(-1 == fprintf(fd, "not loaded\n"))
			return FILEWRITE_ERROR;
	}


	// Salinity
	if(-1 == fprintf(fd, "  Salinity:    "))
		return FILEWRITE_ERROR;
	if(SalinityLoaded() == TRUE)
	{
		GetSalinityFileName(szFileName, SIZE_256);
		if(-1 == fprintf(fd, "%s\n", szFileName))
			return FILEWRITE_ERROR;
	}
	else
	{
		if(-1 == fprintf(fd, "not loaded\n"))
			return FILEWRITE_ERROR;
	}


	// Temperature
	if(-1 == fprintf(fd, "  Temperature: "))
		return FILEWRITE_ERROR;
	if(TemperatureLoaded() == TRUE)
	{
		GetTemperatureFileName(szFileName, SIZE_256);
		if(-1 == fprintf(fd, "%s\n", szFileName))
			return FILEWRITE_ERROR;
	}
	else
	{
		if(-1 == fprintf(fd, "not loaded\n"))
			return FILEWRITE_ERROR;
	}
	if(-1 == fprintf(fd, "------------------------------------------------------------------------------------------------\n\n\n"))
		return FILEWRITE_ERROR;
#endif//NOCSENARIOFUNCTIONS
	return OK;
}


RESLT CScenario::GetErrorStatus()
{
	RESLT ret = OK;
#ifndef NOCSENARIOFUNCTIONS
	ret = m_state.errorStatus;
#endif//NOCSENARIOFUNCTIONS
	return ret;
}






//-------------------------------------------------------------------------------------//
// Builds a _fSCENARIOPARAMS struct based on current scenario setup.
//---------------------------------//
USERPARAMS CScenario::GetCurrentScenarioParams()
{
	USERPARAMS ret = {0};
#ifndef NOCSENARIOFUNCTIONS
	ret = m_mbSce.user;
#endif//NOCSENARIOFUNCTIONS
	return ret;
}

int CScenario::GetSaveStatesCount()
{
	int ret = 0;
#ifndef NOCSENARIOFUNCTIONS
	_ASSERT(m_mbSce.numSaveIterations == (DWORD)m_fileMgr.GetStateSaveToFileCount(m_mbSce.startTime, m_mbSce.duration, &m_mbSce.intrvlOutptLim));
	ret = m_mbSce.numSaveIterations;
#endif//NOCSENARIOFUNCTIONS
	return ret;
}

// SCERESETTAKES *Resets: A record of acoustic state information of animats that
// previously went off the map and were reset.  Resets values of off screen animats
// are by animat, species group, and population.  Updated every iteration.

// SceTakes and SpeTakes are what get saved to file and here are completly recalculated
// each iteration.  TAKE strucs (for SceTakes) contains storage for the animat take
// counts and all species groups in the form of TAKESTATS.  TAKESTATS (for SpeTakes)
// contains dynamically allocated storage for each species presents.  Dynamic because
// the number of species is not known in advance.
void CScenario::UpdateStatisticalCalculations(TAKE *SceTakes, TAKESTATS *SpeTakes, SCERESETTAKES *PrevTakes,
											  SPECIESANIMATSTATEBUFFER *SpeAnimatBuffer)
{
#ifndef NOCSENARIOFUNCTIONS
	int i, j;

	// These local vars are here to make lines of code shorter and easier to read
	ANIMATSTATE *as;
	SPECIESGROUP speGrp;
	int speListLen;

	// If user aborted then return immediately.
	if(m_abort == TRUE)
		return;

	speListLen = m_speciesList.Length();


	//----------------------------------------------------------------------------------//
	// Assertions
	//-----------//
	// Assert that the statistical analysis value for the number of used species groups
	// is set to the same value as the population states because this value is critical
	// when the statistical analysis region is read back in from file for allowing the 
	// code to know how many actual species groups there are rather than just the space
	// allocated.  Actual and allocated may not match for backwards compatability in the
	// future.
	_ASSERT(PrevTakes->numSpeciesGroupsUsed == SceTakes->numSpeGroups);
	//----------------------------------------------------------------------------------//

	//----------------------------------------------------------------------------------//
	// Reset stats analysis structs and buffers of structs to zero
	//------------------------------------------------------------//
	memset(SceTakes, 0, sizeof(TAKE)); // Animats and species groups
	memset(SpeTakes, 0, sizeof(TAKESTATS)*m_speciesList.Length()); // loaded species

	SceTakes->numSpeGroups = PrevTakes->numSpeciesGroupsUsed;

	SceTakes->animat.maxInstant = m_SceTakesCopy.animat.maxInstant;
	SceTakes->animat.maxCumulative = m_SceTakesCopy.animat.maxCumulative;

	for(i=0; i<(int)SceTakes->numSpeGroups; i++)
	{
		SceTakes->speGroup[i].maxInstant = m_SceTakesCopy.speGroup[i].maxInstant;
		SceTakes->speGroup[i].maxCumulative = m_SceTakesCopy.speGroup[i].maxCumulative;
	}

	for(i=0; i<speListLen; i++)
	{
		SpeTakes[i].maxInstant = m_SpeTakesCopy[i].maxInstant;
		SpeTakes[i].maxCumulative = m_SpeTakesCopy[i].maxCumulative;
	}

	//SceTakes, sizeof(TAKE));
	//memcpy(m_SpeTakesCopy, SpeTakes, sizeof(TAKESTATS)*m_speciesList.Length());

	//----------------------------------------------------------------------------------//

	//-------------------------------------------------------------------------------------//
	// Copy take counts of animats that went off screen currently stored in the SCERESETTAKES
	// structure (PrevTakes variable) passed in.  Reset taken animats are animats that were
	// taken before they happened to go off rhe map.
	//--------------------------------------------------------------------------//
	// Animat
	SceTakes->animat.lvlAPhysTakes = PrevTakes->animat.lvlAPhysTakesTotal;
	SceTakes->animat.lvlBPhysTakes = PrevTakes->animat.lvlBPhysTakesTotal;
	SceTakes->animat.lvlBBehTakes = (float)PrevTakes->animat.lvlBBehTakesTotal;
	SceTakes->animat.offScreenCount = PrevTakes->animat.offScreenCount;
	SceTakes->animat.initialAnimatCount = PrevTakes->animat.numInitialAnimats;

	// Species Group
	for(i=0; i<PrevTakes->numSpeciesGroupsUsed; i++)
	{
		SceTakes->speGroup[i].lvlAPhysTakes = PrevTakes->speGroup[i].lvlAPhysTakesTotal;
		SceTakes->speGroup[i].lvlBPhysTakes = PrevTakes->speGroup[i].lvlBPhysTakesTotal;
		SceTakes->speGroup[i].lvlBBehTakes = (float)PrevTakes->speGroup[i].lvlBBehTakesTotal;
		SceTakes->speGroup[i].offScreenCount = PrevTakes->speGroup[i].offScreenCount;
		SceTakes->speGroup[i].initialAnimatCount = PrevTakes->speGroup[i].numInitialAnimats;
	}

	// Species
	for(i=0; i<speListLen && m_abort == FALSE; i++)
	{
		SpeTakes[i].lvlAPhysTakes = PrevTakes->speBuff[i].animat.lvlAPhysTakesTotal;
		SpeTakes[i].lvlBPhysTakes = PrevTakes->speBuff[i].animat.lvlBPhysTakesTotal;
		SpeTakes[i].lvlBBehTakes = (float)PrevTakes->speBuff[i].animat.lvlBBehTakesTotal;
		SpeTakes[i].offScreenCount = PrevTakes->speBuff[i].animat.offScreenCount;
		SpeTakes[i].initialAnimatCount = PrevTakes->speBuff[i].animat.numInitialAnimats;
	}
	//-------------------------------------------------------------------------------------//


	//-------------------------------------------------------------------------------------//
	// Update counts based on the current animat states (input variable 'PrevTakes' maintains
	// a pointer to the animat state buffer array even though the animat state buffer array
	// is not impacted by this function).
	for(i=0; i<speListLen && m_abort == FALSE; i++)
	{

		// For each species in the scenario...
		// Get a pointer
		if(SOUNDSOURCE == (speGrp = m_speciesList.Get(i)->GetSpeciesType()))
			continue;

		for(j=0; j<PrevTakes->speBuff[i].animat.numInitialAnimats; j++)
		{
			// 'as' is the current animat being looked at.
			//as = &PrevTakes->speBuff[i].animatState[j];
			as = &SpeAnimatBuffer[i].animatState[j];

			if(as->acstcExp.lvlAPhysFlag == TRUE)
			{
				//----------------------------------//
				// Current Level A physical exposure
				//---------------------------------//
				SceTakes->animat.lvlAPhysTakes++;  // Animat Summary
				SceTakes->speGroup[speGrp].lvlAPhysTakes++; // Species Group Summary
				SpeTakes[i].lvlAPhysTakes++; // Species Summary
			}
			else if(as->acstcExp.lvlBPhysFlag == TRUE)
			{
				//----------------------------------//
				// Current Level B physical exposure
				//----------------------------------//
				SceTakes->animat.lvlBPhysTakes++; // Animat Summary
				SceTakes->speGroup[speGrp].lvlBPhysTakes++; // Species Group Summary
				SpeTakes[i].lvlBPhysTakes++; // Species Summary
			}
			else
			{
				if(speGrp == SPECIALCONSIDRTNS && as->acstcExp.lvlBBehFlag == TRUE)
				{
					SceTakes->animat.lvlBBehTakes += (float)1.0; // Animat Summary
					SceTakes->speGroup[speGrp].lvlBBehTakes += (float)1.0; // Species Group Summary
					SpeTakes[i].lvlBBehTakes += (float)1.0; // Species Summary
				}
				else
				{
					//--------------------------------------//
					// Current Level B behavior (Risk) Takes
					//--------------------------------------//
					SceTakes->animat.lvlBBehTakes += (float)as->acstcExp.risk; // Animat Summary
					SceTakes->speGroup[speGrp].lvlBBehTakes += (float)as->acstcExp.risk; // Species Group Summary
					SpeTakes[i].lvlBBehTakes += (float)as->acstcExp.risk; // Species Summary
				}
			}


			// Handle if if the animat when offscreen during the previous iteration.
			if(as->offScreenInf.offScreen == TRUE)
			{
				SceTakes->animat.offScreenCount++;  // Animat Summary
				SceTakes->speGroup[speGrp].offScreenCount++; // Species Group Summary
				SpeTakes[i].offScreenCount++; // Species Summary

				PrevTakes->animat.offScreenCount++;
				PrevTakes->speGroup[speGrp].offScreenCount++;
				PrevTakes->speBuff[i].animat.offScreenCount++;


				// If the animat went off-screen while in a level A take during the previous
				// iteration, update the counts.
				if(as->offScreenInf.lvlAPhysFlagPrev == TRUE)
				{
					SceTakes->animat.lvlAPhysTakes++;  // Animat Summary
					SceTakes->speGroup[speGrp].lvlAPhysTakes++; // Species Group Summary
					SpeTakes[i].lvlAPhysTakes++; // Species Summary

					PrevTakes->animat.lvlAPhysTakesTotal++;
					PrevTakes->speGroup[speGrp].lvlAPhysTakesTotal++;
					PrevTakes->speBuff[i].animat.lvlAPhysTakesTotal++;
				}
				else if(as->offScreenInf.lvlBPhysFlagPrev == TRUE)
				{
					SceTakes->animat.lvlBPhysTakes++;  // Animat Summary
					SceTakes->speGroup[speGrp].lvlBPhysTakes++; // Species Group Summary
					SpeTakes[i].lvlBPhysTakes++; // Species Summary

					PrevTakes->animat.lvlBPhysTakesTotal++;
					PrevTakes->speGroup[speGrp].lvlBPhysTakesTotal++;
					PrevTakes->speBuff[i].animat.lvlBPhysTakesTotal++;
				}
				else if(as->offScreenInf.lvlBBehFlagPrev == TRUE)
				{
					SceTakes->animat.lvlBBehTakes += (float)as->offScreenInf.lvlBBehRiskValue;  // Animat Summary
					SceTakes->speGroup[speGrp].lvlBBehTakes += (float)as->offScreenInf.lvlBBehRiskValue; // Species Group Summary
					SpeTakes[i].lvlBBehTakes += (float)as->offScreenInf.lvlBBehRiskValue;

					PrevTakes->animat.lvlBBehTakesTotal += (float)as->offScreenInf.lvlBBehRiskValue;
					PrevTakes->speGroup[speGrp].lvlBBehTakesTotal += (float)as->offScreenInf.lvlBBehRiskValue;
					PrevTakes->speBuff[i].animat.lvlBBehTakesTotal += (float)as->offScreenInf.lvlBBehRiskValue;
				}
			}

			//-------------------------------//
			// Level B Total Takes
			//-------------------------------//
			SceTakes->animat.lvlBTakesTotal = (DWORD)m_staticLib.MyRound(SceTakes->animat.lvlBBehTakes) + SceTakes->animat.lvlBPhysTakes; // Animat Summary
			SceTakes->speGroup[speGrp].lvlBTakesTotal = (DWORD)m_staticLib.MyRound(SceTakes->speGroup[speGrp].lvlBBehTakes) + SceTakes->speGroup[speGrp].lvlBPhysTakes; // Animat Summary
			SpeTakes[i].lvlBTakesTotal = (DWORD)m_staticLib.MyRound(SpeTakes[i].lvlBBehTakes) + SpeTakes[i].lvlBPhysTakes; // Animat Summary; // Species Summary


			//-------------------------//
			// Animat beached/stranded
			//-------------------------//
			if(as->beached == TRUE)
			{
				SceTakes->animat.numStranded++; // Animat Summary
				SceTakes->speGroup[speGrp].numStranded++; // Species Group Summary
				SpeTakes[i].numStranded++; // Species Summary
			}



			//---------------------//
			// Max instant exposure 
			//---------------------//
			if(SceTakes->animat.maxInstant < as->acstcExp.actualSrcInstantValue)
				SceTakes->animat.maxInstant = (float)as->acstcExp.actualSrcInstantValue; // Animat Summary
			if(as->offScreenInf.offScreen == TRUE && SceTakes->animat.maxInstant < as->offScreenInf.actualSrcInstantValue)
				SceTakes->animat.maxInstant = (float)as->offScreenInf.actualSrcInstantValue; // Animat Summary

			if(SceTakes->speGroup[speGrp].maxInstant < as->acstcExp.actualSrcInstantValue)
				SceTakes->speGroup[speGrp].maxInstant = (float)as->acstcExp.actualSrcInstantValue; // Species Group Summary
			if(as->offScreenInf.offScreen == TRUE && SceTakes->speGroup[speGrp].maxInstant < as->offScreenInf.actualSrcInstantValue)
				SceTakes->speGroup[speGrp].maxInstant = (float)as->offScreenInf.actualSrcInstantValue; // Species Group Summary

			if(SpeTakes[i].maxInstant < as->acstcExp.actualSrcInstantValue)
				SpeTakes[i].maxInstant = (float)as->acstcExp.actualSrcInstantValue; // Species Group Summary
			if(as->offScreenInf.offScreen == TRUE && SpeTakes[i].maxInstant < as->offScreenInf.actualSrcInstantValue)
				SpeTakes[i].maxInstant = (float)as->offScreenInf.actualSrcInstantValue; // Species Group Summary

			//------------------------//
			// Max cumulative exposure
			//------------------------//
			if(SceTakes->animat.maxCumulative < as->acstcExp.cumulativeValue)
				SceTakes->animat.maxCumulative = (float)as->acstcExp.cumulativeValue; // Animat Summary
			if(as->offScreenInf.offScreen == TRUE && SceTakes->animat.maxCumulative < as->offScreenInf.cumulativeValue)
				SceTakes->animat.maxCumulative = (float)as->offScreenInf.cumulativeValue; // Animat Summary
	
			if(SceTakes->speGroup[speGrp].maxCumulative < as->acstcExp.cumulativeValue)
				SceTakes->speGroup[speGrp].maxCumulative = (float)as->acstcExp.cumulativeValue; // Species Group Summary
			if(as->offScreenInf.offScreen == TRUE && SceTakes->speGroup[speGrp].maxCumulative < as->offScreenInf.cumulativeValue)
				SceTakes->animat.maxCumulative = (float)as->offScreenInf.cumulativeValue; // Species Group Summary

			if(SpeTakes[i].maxCumulative < as->acstcExp.cumulativeValue)
				SpeTakes[i].maxCumulative = (float)as->acstcExp.cumulativeValue; // Species Summary
			if(as->offScreenInf.offScreen == TRUE && SpeTakes[i].maxCumulative < as->offScreenInf.cumulativeValue)
				SpeTakes[i].maxCumulative = (float)as->offScreenInf.cumulativeValue; // Species Summary
		}
	}
	//-------------------------------------------------------------------------------------//
	memcpy(&m_SceTakesCopy, SceTakes, sizeof(TAKE));
	memcpy(m_SpeTakesCopy, SpeTakes, sizeof(TAKESTATS)*m_speciesList.Length());
	
//	TAKESTATS *m_SpeTakesCopy;
#endif//NOCSENARIOFUNCTIONS

}


SCENARIOPARAMS CScenario::GetScenarioParamsCopy(void)
{
	SCENARIOPARAMS ret = {0};
#ifndef NOCSENARIOFUNCTIONS
	ret = SCENARIOPARAMS(m_mbSce);
#endif//NOCSENARIOFUNCTIONS
	return ret;
}

