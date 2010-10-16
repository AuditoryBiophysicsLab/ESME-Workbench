#include "3mbSceInterface.h"

CScenario *CBitmapEnvFunctions::m_sce = NULL;
SCENARIOUTPUTFILEINF *CBitmapEnvFunctions::m_playback = NULL;
BATHYUSESTATE CBitmapEnvFunctions::m_state = UNINITIALIZED_STATE;
CFileManager CBitmapEnvFunctions::m_fileMgr;
SIMPLEPOP CBitmapEnvFunctions::m_pop;
int CBitmapEnvFunctions::m_numSoundSourcePresent = 0;
//UNIQUEIDMAP *CBitmapEnvFunctions::m_animatIDMapArray = NULL;



CBitmapEnvFunctions::CBitmapEnvFunctions(void)
{
	m_sce = NULL;
	m_playback = NULL;
	m_state = UNINITIALIZED_STATE;
	m_numSoundSourcePresent = 0;
	//m_animatIDMapArray = NULL;
}

CBitmapEnvFunctions::~CBitmapEnvFunctions(void)
{
	m_state = UNINITIALIZED_STATE;

	if(m_playback != NULL)
		m_fileMgr.UninitializePlayback(m_playback, OK);

	m_sce = NULL;
	m_playback = NULL;
	m_numSoundSourcePresent = 0;
}

void CBitmapEnvFunctions::DeallocateSimplePop()
{
	int i,j;
	SIMPLESPECIES *s;
	SIMPLEPOD *p;
	int numSpe = m_pop.s.Length();
	int numPods;

	for(i=numSpe-1; i>=0; i--)
	{
		s = m_pop.s.Get(i);
		numPods = s->p.Length();

		for(j=numPods-1; j>=0; j--)
		{
			p = s->p.Get(j);
			p->a.DeleteAll();
		}
		s->i.DeleteAll();
	}
	m_pop.s.DeleteAll();

	m_numSoundSourcePresent = 0;
}




SIMPLEPOP* CBitmapEnvFunctions::GetPopRef()
{
	return &m_pop;
}

BATHYUSESTATE CBitmapEnvFunctions::GetUsageState()
{
	return m_state;
}


void CBitmapEnvFunctions::InitializeSeeding(CScenario *pScenario)
{
	if(m_playback != NULL)
		m_fileMgr.UninitializePlayback(m_playback, OK);
	m_playback = NULL;


	DeallocateSimplePop();

	m_sce = pScenario;
	m_state = SEED_SCENARIO_STATE;
	m_numSoundSourcePresent = 0;
}



RESLT CBitmapEnvFunctions::InitializePlayback(TCHAR *szFileName, SCENARIOUTPUTFILEINF *pPlaybackInf)
{
	int i,j;
	RESLT res;
	BOOL abort = FALSE;
	SIMPLESPECIES *s;
	SIMPLEPOD *p;
	ANIMATSTATE_FILEOUT_PTR *a_ptr;
	double minLat, maxLat, minLon, maxLon;
	int buffIndex;
	CFileManagerStatic fileManagerStatic;

	if(m_playback != NULL)
		m_fileMgr.UninitializePlayback(m_playback, OK);
	m_playback = pPlaybackInf;
	m_state = UNINITIALIZED_STATE;


	// Allocation 1: sizeof(ANIMATSTATE_FILEOUT) * num animats * num states.
	//				 m_playback->animatState[] buffer
	if(OK != (res = m_fileMgr.InitializePlayback(szFileName, pPlaybackInf)))
	{
		m_fileMgr.UninitializePlayback(pPlaybackInf, res);
		return res;
	}

	// pPlaybackInf->sce.totalNumAnimats includes the acoustic source count
	if(pPlaybackInf->sce.numSaveIterations * (pPlaybackInf->sce.totalNumAnimats) > MAX_NUM_PLAYBACK_STATES)
	{
		m_fileMgr.UninitializePlayback(pPlaybackInf, res);
		return MAX_NUM_PLAYBACK_STATES_EXCEEDED;
	}

	m_state = PLAYBACK_STATE;

	//localAnimatFileOut = new ANIMATSTATE_FILEOUT[PlaybackState->sce.numSaveIterations

	// Allocation 2: minimum num bytes needed * num animats * num states.
	//			     m_runState.a = new BYTE[mem.numBytes]
	if(OK != (res = m_fileMgr.AllocateFileIOBuffer(m_playback->sce, TRUE)))
	{
		// more bad news.  Add error handling/status
		return m_fileMgr.UninitializePlayback(m_playback, res);
	}

	for(i=0; (DWORD)i<m_playback->sce.numSaveIterations; i++)
	{
		buffIndex = fileManagerStatic.BuffIndex(0, m_playback->sce.totalNumAnimats, i, m_playback->sce.numSaveIterations);
		m_fileMgr.ReadStateDataFromIOBuffer(m_playback->sce, &m_playback->animatState[buffIndex], &abort);
	}


	// If no bathymetry file was include in the output generate one.
	if(m_playback->sce.user.output.headerInf.bathyMap == FALSE && m_playback->sce.totalNumAnimats > 0 &&
		m_playback->sce.numSaveIterations > 0)
	{
		buffIndex = fileManagerStatic.BuffIndex(0, m_playback->sce.totalNumAnimats, 0, m_playback->sce.numSaveIterations);
		minLat = maxLat = m_playback->animatState[buffIndex].lat;
		minLon = maxLon = m_playback->animatState[buffIndex].lon;

		for(i=0; (DWORD)i<m_playback->sce.numSaveIterations; i++)
		{
			for(j=0; (DWORD)j<m_playback->sce.totalNumAnimats; j++)
			{
				buffIndex = fileManagerStatic.BuffIndex(j, m_playback->sce.totalNumAnimats, i, m_playback->sce.numSaveIterations);

				if(minLat > m_playback->animatState[buffIndex].lat)
					minLat = m_playback->animatState[buffIndex].lat;

				if(maxLat < m_playback->animatState[buffIndex].lat)
					maxLat = m_playback->animatState[buffIndex].lat;

				if(minLon > m_playback->animatState[buffIndex].lon)
					minLon = m_playback->animatState[buffIndex].lon;

				if(maxLon < m_playback->animatState[buffIndex].lon)
					maxLon = m_playback->animatState[buffIndex].lon;

			}
		}
		m_playback->envData.bathymetry.CreateFakeData(minLat, maxLat, minLon, maxLon);
		m_playback->sce.user.output.headerInf.bathyMap = TRUE;
	}


	// Add the appropriate number of species.
	for(i=0; (DWORD)i<m_playback->sce.numSpecies; i++)
		m_pop.s.Add();

	m_numSoundSourcePresent = 0;
	for(i=0; (DWORD)i<m_playback->sce.totalNumAnimats; i++)
	{
		s = m_pop.s.Get(m_playback->animatAssociations[i].speciesNumber);
		if((m_playback->animatAssociations[i].compactInf & 0x00000001) == 0)
		{
			// Determine if the animat beign added is a member of the current pod or if a
			// additional pod needs to be added.
			if(s->p.Length() <= m_playback->animatAssociations[i].pod_id)
				p = s->p.Add(); // Add a new pod/
			else
				p = s->p.Get(m_playback->animatAssociations[i].pod_id); // Get the pod at index

			a_ptr = p->a.Add(); // Add an animat to the new or already existing pod.

		}
		else
		{
			a_ptr = s->i.Add(); // Adds an individual.
		}

		// Have the list entry point to the initial state in the buffer. 0 in the call
		// that follows is for the initial state.
		buffIndex = fileManagerStatic.BuffIndex(i, m_playback->sce.totalNumAnimats, 0, m_playback->sce.numSaveIterations);
		a_ptr->p = &m_playback->animatState[buffIndex]; // crash.
		_ASSERT(a_ptr->p->animatID == (UINT)i);

		if(m_playback->animatAssociations[i].acstcSrc.isASoundSource == TRUE)
			m_numSoundSourcePresent++;
	}

	return res;
}

void CBitmapEnvFunctions::Uninitialize()
{
	switch(m_state)
	{
	case UNINITIALIZED_STATE:
	case SEED_SCENARIO_STATE:
		break;
	case PLAYBACK_STATE:
		m_fileMgr.UninitializePlayback(m_playback, OK);
		DeallocateSimplePop();
	}
	m_state = UNINITIALIZED_STATE;
}

PODLEADERTYPE CBitmapEnvFunctions::GetPodLeaderType(int SpeciesIndex, int PodIndex)
{
	UINT32 animatID;
	switch(m_state)
	{
	case UNINITIALIZED_STATE:
		return ANIMAT;
	case SEED_SCENARIO_STATE:
		return m_sce->GetPodLeaderType(SpeciesIndex, PodIndex);
	case PLAYBACK_STATE:
		// 0 = pod
		// 1 = individual
		animatID = m_pop.s.Get(SpeciesIndex)->p.Get(PodIndex)->a.Get(0)->p->animatID;
		_ASSERT((m_playback->animatAssociations[animatID].compactInf & 0x00000001) != 1);
		if(((m_playback->animatAssociations[animatID].compactInf & 0x00000001) != 1) &&
			((m_playback->animatAssociations[animatID].compactInf & 0x00000002) == 2))
			return CENTROID;
		return ANIMAT;

	}
	return ANIMAT;
}

void CBitmapEnvFunctions::BogusFunction()
{
	switch(m_state)
	{
	case UNINITIALIZED_STATE:
		break;
	case SEED_SCENARIO_STATE:
		break;;
	case PLAYBACK_STATE:
		break;
	}

	switch(m_state)
	{
	case UNINITIALIZED_STATE:
	case PLAYBACK_STATE:
		break; // not yet supported for non-specified and playback.
	case SEED_SCENARIO_STATE:
		break;
	}

	switch(m_state)
	{
	case UNINITIALIZED_STATE:
		return;
	case SEED_SCENARIO_STATE:
		return;;
	case PLAYBACK_STATE:
		return;
	}

	switch(m_state)
	{
	case UNINITIALIZED_STATE:
	case PLAYBACK_STATE:
		return; // not yet supported for non-specified and playback.
	case SEED_SCENARIO_STATE:
		return;
	}
	return;
}

CScenario *CBitmapEnvFunctions::GetSceRef()
{
	switch(m_state)
	{
	case UNINITIALIZED_STATE:
	case PLAYBACK_STATE:
		return NULL;
	case SEED_SCENARIO_STATE:
		return m_sce;
	}
	return NULL;
}

double CBitmapEnvFunctions::GetMinimumSeededingDepth(int SpeIndex)
{
	switch(m_state)
	{
	case UNINITIALIZED_STATE:
	case PLAYBACK_STATE:
		return 0; // not yet supported for non-specified and playback.
	case SEED_SCENARIO_STATE:
		return m_sce->GetMinimumSeededingDepth(SpeIndex);
	}
	return 0;
}


double CBitmapEnvFunctions::GetShoreFollowingDepth(int SpeIndex)
{
	switch(m_state)
	{
	case UNINITIALIZED_STATE:
	case PLAYBACK_STATE:
		return 0; // not yet supported for non-specified and playback.
	case SEED_SCENARIO_STATE:
		return m_sce->GetShoreFollowingDepth(SpeIndex);
	}
	return 0;
}

BOOL CBitmapEnvFunctions::SoundSourceIsPresent()
{
	switch(m_state)
	{
	case UNINITIALIZED_STATE:
	case PLAYBACK_STATE:
		return FALSE;// not yet supported for non-specified and playback.
	case SEED_SCENARIO_STATE:
		return m_sce->SoundSourceSpeciesPresent();
	}
	return FALSE;
}

double CBitmapEnvFunctions::GetPodLeaderFocalDistance(int SpeciesIndex, int PodIndex)
{
	switch(m_state)
	{
	case UNINITIALIZED_STATE:
		return 0;
	case SEED_SCENARIO_STATE:
		return m_sce->GetPodLeaderFocalDistance(SpeciesIndex, PodIndex);
	case PLAYBACK_STATE:
		return 0;
#pragma message("TODO: Add focal distance to this function")
/*
		animatID = m_pop.s.Get(SpeciesIndex)->p.Get(PodIndex)->a.Get(0)->animatID;
		_ASSERT(m_playback->animatAssociations[animatID].compactInf 0x00000001 != 1);

		m_playback->animatAssociations[animatID].


		if(m_playback->animatAssociations[animatID].compactInf 0x00000001 != 1 &&
			m_playback->animatAssociations[animatID].compactInf 0x00000002 == 1)
			return CENTROID;
		return ANIMAT
*/
	}
	return ANIMAT;
}


BATHYVALUE CBitmapEnvFunctions::GetValueAtCoordinate(double Lat, double Lon)
{
	BATHYVALUE v = {0};

	switch(m_state)
	{
	case UNINITIALIZED_STATE:
		return v;
	case SEED_SCENARIO_STATE:
		return m_sce->GetBathymetryDepth(Lat, Lon);
	case PLAYBACK_STATE:
		return m_playback->envData.bathymetry.GetValueAtCoordinate(Lat, Lon);
	}
	return v;
}

BOOL CBitmapEnvFunctions::BathyFileLoaded()
{
	switch(m_state)
	{
	case UNINITIALIZED_STATE:
		return FALSE;
	case SEED_SCENARIO_STATE:
		return m_sce->BathymetryLoaded();
	case PLAYBACK_STATE:
		return m_playback->envData.bathymetry.IsDataLoaded();
	}
	return FALSE;
}

BOOL CBitmapEnvFunctions::SaltinityFileLoaded()
{
	switch(m_state)
	{
	case UNINITIALIZED_STATE:
		return FALSE;
	case SEED_SCENARIO_STATE:
		return m_sce->SalinityLoaded();
	case PLAYBACK_STATE:
		return m_playback->envData.salinity.IsDataLoaded();
	}
	return FALSE;
}

BOOL CBitmapEnvFunctions::TemperatureFileLoaded()
{
	switch(m_state)
	{
	case UNINITIALIZED_STATE:
		return FALSE;
	case SEED_SCENARIO_STATE:
		return m_sce->TemperatureLoaded();
	case PLAYBACK_STATE:
		return m_playback->envData.temperature.IsDataLoaded();
	}
	return FALSE;
}

BOOL CBitmapEnvFunctions::AnyEnvFileLoaded()
{
	return (BathyFileLoaded() || SaltinityFileLoaded() || TemperatureFileLoaded());
}


double CBitmapEnvFunctions::GetTotalBathySufaceAreaMeters()
{
	switch(m_state)
	{
	case UNINITIALIZED_STATE:
		return 10;
	case SEED_SCENARIO_STATE:
		return m_sce->GetTotalBathySufaceAreaMeters();
	case PLAYBACK_STATE:
		return m_playback->envData.bathymetry.GetTotalSufaceAreaMeters();
	}
	return 0;
}


double CBitmapEnvFunctions::GetBathymtryWaterSurfaceAreaMeters()
{
	switch(m_state)
	{
	case UNINITIALIZED_STATE:
		return 10;
	case SEED_SCENARIO_STATE:
		return m_sce->GetBathymetryWaterSufaceAreaMeters();
	case PLAYBACK_STATE:
		return m_playback->envData.bathymetry.GetWaterSurfaceAreaMeters();
	}
	return 0;
}

double CBitmapEnvFunctions::GetBathymetryLandSufaceAreaMeters()
{
	switch(m_state)
	{
	case UNINITIALIZED_STATE:
		return 10;
	case SEED_SCENARIO_STATE:
		return m_sce->GetBathymetryLandSufaceAreaMeters();
	case PLAYBACK_STATE:
		return m_playback->envData.bathymetry.GetLandSufaceAreaMeters();
	}
	return 0;
}

double CBitmapEnvFunctions::GetAnimatDensity()
{
	double numAnimats;
	double waterArea;

	switch(m_state)
	{
	case UNINITIALIZED_STATE:
		return 0;
	case SEED_SCENARIO_STATE:
		numAnimats = (double)m_sce->GetAnimatCount();
		waterArea = m_sce->GetBathymetryWaterSufaceAreaMeters();
		_ASSERT(waterArea > 0);
		if(waterArea <= 0)
			return 0;
		return numAnimats/waterArea;
	case PLAYBACK_STATE:
		return -1; // this needs to be implemented.
	}
	return 0;
}


int CBitmapEnvFunctions::GetAnimatCount()
{
	switch(m_state)
	{
	case UNINITIALIZED_STATE:
		return 0;
	case SEED_SCENARIO_STATE:
		return m_sce->GetAnimatCount();
	case PLAYBACK_STATE:
		return m_playback->sce.totalNumAnimats;
	}
	return 0;
}

int CBitmapEnvFunctions::GetIndividualCount()
{
	int i, cnt = 0;
	switch(m_state)
	{
	case UNINITIALIZED_STATE:
		return 0;
	case SEED_SCENARIO_STATE:
		return m_sce->GetIndivdualCount();
	case PLAYBACK_STATE:
		for(i=0; i < m_pop.s.Length(); i++)
			cnt += m_pop.s.Get(i)->i.Length();
		return cnt;
	}
	return 0;
}

int CBitmapEnvFunctions::GetIndividualCount(int SpeciesIndex)
{
	switch(m_state)
	{
	case UNINITIALIZED_STATE:
		return 0;
	case SEED_SCENARIO_STATE:
		return m_sce->GetIndivdualCount(SpeciesIndex);
	case PLAYBACK_STATE:
		return m_pop.s.Get(SpeciesIndex)->i.Length();
	}
	return 0;
}


int CBitmapEnvFunctions::GetAnimatCount(int SpeciesIndex)
{
	int i, cnt;
	switch(m_state)
	{
	case UNINITIALIZED_STATE:
		return 0;
	case SEED_SCENARIO_STATE:
		return m_sce->GetAnimatCount(SpeciesIndex);
	case PLAYBACK_STATE:
		cnt = m_pop.s.Get(SpeciesIndex)->i.Length();
		for(i=0; i<m_pop.s.Get(SpeciesIndex)->p.Length(); i++)
			cnt += m_pop.s.Get(SpeciesIndex)->p.Get(i)->a.Length();
		return cnt;
	}
	return 0;
}

int CBitmapEnvFunctions::GetPodMemberCount(int SpeciesIndex, int PodIndex)
{
	switch(m_state)
	{
	case UNINITIALIZED_STATE:
		return 0;
	case SEED_SCENARIO_STATE:
		return m_sce->GetPodMemberCount(SpeciesIndex, PodIndex);
	case PLAYBACK_STATE:
		return m_pop.s.Get(SpeciesIndex)->p.Get(PodIndex)->a.Length();
	}
	return 0;
}

int CBitmapEnvFunctions::GetPodCount(int SpeciesIndex)
{
	switch(m_state)
	{
	case UNINITIALIZED_STATE:
		return 0;
	case SEED_SCENARIO_STATE:
		return m_sce->GetPodCount(SpeciesIndex);
	case PLAYBACK_STATE:
		return m_pop.s.Get(SpeciesIndex)->p.Length();
	}
	return 0;
}

INHABITINF *CBitmapEnvFunctions::GetAnimatInitialCoordinates(INHABITINF *IC)
{
	int i, j, k;
	int cnt;
	ANIMATSTATE_FILEOUT_PTR *a;

	switch(m_state)
	{
	case UNINITIALIZED_STATE:
		return NULL;
	case SEED_SCENARIO_STATE:
		IC = m_sce->GetAnimatPopulationInitialCoordinates(IC);
		return IC;
	case PLAYBACK_STATE:
		if(IC == NULL)
			IC = new INHABITINF[m_playback->sce.totalNumAnimats];
		memset(IC, 0, sizeof(INHABITINF)*m_playback->sce.totalNumAnimats);

		for(cnt=0, i=0; i<m_pop.s.Length(); i++)
		{
			// Pods
			for(j=0; j<m_pop.s.Get(i)->p.Length(); j++)
			{
				// Pod members
				for(k=0; k<m_pop.s.Get(i)->p.Get(j)->a.Length(); k++)
				{
					a = m_pop.s.Get(i)->p.Get(j)->a.Get(k);
					IC[cnt].coord.lat = a->p->lat;
					IC[cnt].coord.lon = a->p->lon;
					IC[cnt].coord.depth = a->p->depth;
					IC[cnt].acstcSrc = m_playback->animatAssociations[i].acstcSrc;
					cnt++;
				}
			}

			// Individuals
			for(j=0; j<m_pop.s.Get(i)->i.Length(); j++)
			{
				a = m_pop.s.Get(i)->i.Get(j);
				IC[cnt].coord.lat = a->p->lat;
				IC[cnt].coord.lon = a->p->lon;
				IC[cnt].coord.depth = a->p->depth;
				IC[cnt].acstcSrc = m_playback->animatAssociations[i].acstcSrc;
				cnt++;
			}
		}
		return IC;
	}
	return NULL;
}


INHABITINF *CBitmapEnvFunctions::GetAnimatInitialCoordinates(int SpeciesIndex, int BufferLen, INHABITINF *IC)
{
	int i,j;
	int cnt;
	ANIMATSTATE_FILEOUT_PTR *a;

	switch(m_state)
	{
	case UNINITIALIZED_STATE:
		return NULL;
	case SEED_SCENARIO_STATE:
		IC = m_sce->GetAnimatInitialCoordinates(SpeciesIndex, BufferLen, IC);
		return IC;
	case PLAYBACK_STATE:
		_ASSERT((BufferLen == 0 && IC==NULL) || (BufferLen >= GetAnimatCount(SpeciesIndex)));
		if(IC == NULL)
			IC = new INHABITINF[GetAnimatCount(SpeciesIndex)];
		memset(IC, 0, sizeof(INHABITINF)*BufferLen);
		cnt = 0;

		// Copy pods
		for(i=0; i<m_pop.s.Get(SpeciesIndex)->p.Length(); i++)
		{
			for(j=0; j<m_pop.s.Get(SpeciesIndex)->p.Get(i)->a.Length(); j++)
			{
				a = m_pop.s.Get(SpeciesIndex)->p.Get(i)->a.Get(j);
				IC[cnt].coord.lat = a->p->lat;
				IC[cnt].coord.lon = a->p->lon;
				IC[cnt].coord.depth = a->p->depth;
				IC[cnt].acstcSrc = m_playback->animatAssociations[SpeciesIndex].acstcSrc;
				cnt++;
			}
		}

		// Copy Individuals
		for(i=0; i<m_pop.s.Get(SpeciesIndex)->i.Length(); i++)
		{
			a = m_pop.s.Get(SpeciesIndex)->i.Get(i);
			IC[cnt].coord.lat = a->p->lat;
			IC[cnt].coord.lon = a->p->lon;
			IC[cnt].coord.depth = a->p->depth;
			IC[cnt].acstcSrc = m_playback->animatAssociations[SpeciesIndex].acstcSrc;
			cnt++;
		}
		return IC;
	}
	return NULL;
}

INHABITINF *CBitmapEnvFunctions::GetPodInitialCoordinates(int SpeciesIndex, int PodIndex, INHABITINF *IC)
{
	ANIMATSTATE_FILEOUT_PTR *a;
	int i;
	int cnt;

	switch(m_state)
	{
	case UNINITIALIZED_STATE:
		return IC;
	case SEED_SCENARIO_STATE:
		IC = m_sce->GetPodInitialCoordinates(SpeciesIndex, PodIndex, IC);
		return IC;
	case PLAYBACK_STATE:
		cnt = 0;
		if(IC == NULL)
			IC = new INHABITINF[GetPodMemberCount(SpeciesIndex, PodIndex)];
		for(i=0; i<m_pop.s.Get(SpeciesIndex)->p.Get(PodIndex)->a.Length(); i++)
		{
			a = m_pop.s.Get(SpeciesIndex)->p.Get(PodIndex)->a.Get(i);
			IC[cnt].coord.lat = a->p->lat;
			IC[cnt].coord.lon = a->p->lon;
			IC[cnt].coord.depth = a->p->depth;
			IC[cnt].acstcSrc = m_playback->animatAssociations[SpeciesIndex].acstcSrc;
			cnt++;
		}
		return IC;
	}
	return NULL;
}


INHABITINF *CBitmapEnvFunctions::GetIndividualInitialCoordinates(int SpeciesIndex, INHABITINF *IC)
{
	ANIMATSTATE_FILEOUT_PTR *a;
	int cnt;
	int i;

	switch(m_state)
	{
	case UNINITIALIZED_STATE:
		return IC;
	case SEED_SCENARIO_STATE:
		IC = m_sce->GetIndividualCoordinates(SpeciesIndex, IC);
		return IC;
	case PLAYBACK_STATE:
		cnt = 0;
		if(IC == NULL)
			IC = new INHABITINF[GetIndividualCount(SpeciesIndex)];
		for(i=0; i<m_pop.s.Get(SpeciesIndex)->i.Length(); i++)
		{
			a = m_pop.s.Get(SpeciesIndex)->i.Get(i);
			IC[cnt].coord.lat = a->p->lat;
			IC[cnt].coord.lon = a->p->lon;
			IC[cnt].coord.depth = a->p->depth;
			IC[cnt].acstcSrc = m_playback->animatAssociations[SpeciesIndex].acstcSrc;
			cnt++;
		}
		return IC;

	}
	return IC;
}


INHABITINF CBitmapEnvFunctions::GetIndividualInitialCoordinate(int SpeciesIndex, int IndividualIndex)
{
	INHABITINF IC = {0};
	ANIMATSTATE_FILEOUT_PTR *a;

	switch(m_state)
	{
	case UNINITIALIZED_STATE:
		return IC;
	case SEED_SCENARIO_STATE:
		return m_sce->GetIndividualInitialCoordinate(SpeciesIndex, IndividualIndex);
	case PLAYBACK_STATE:
		a = m_pop.s.Get(SpeciesIndex)->i.Get(IndividualIndex);
		IC.coord.lat = a->p->lat;
		IC.coord.lon = a->p->lon;
		IC.coord.depth = a->p->depth;
		IC.acstcSrc = m_playback->animatAssociations[SpeciesIndex].acstcSrc;
		return IC;
	}
	return IC;
}


INHABITINF CBitmapEnvFunctions::GetPodMemberInitialCoordinate(int SpeciesIndex, int PodIndex, int MemberIndex)
{
	INHABITINF IC = {0};
	ANIMATSTATE_FILEOUT_PTR *a;

	switch(m_state)
	{
	case UNINITIALIZED_STATE:
		return IC;
	case SEED_SCENARIO_STATE:
		return m_sce->GetPodMemberInitialCoordinate(SpeciesIndex, PodIndex, MemberIndex);
	case PLAYBACK_STATE:
		a = m_pop.s.Get(SpeciesIndex)->p.Get(PodIndex)->a.Get(MemberIndex);
		IC.coord.lat = a->p->lat;
		IC.coord.lon = a->p->lon;
		IC.coord.depth = a->p->depth;
		IC.acstcSrc = m_playback->animatAssociations[SpeciesIndex].acstcSrc;
		return IC;
	}
	return IC;
}

BATHYVALUE CBitmapEnvFunctions::GetBathymetryValues(double Lat, double Lon)
{
	BATHYVALUE ret = {0};

	switch(m_state)
	{
	case UNINITIALIZED_STATE:
		return ret;
	case SEED_SCENARIO_STATE:
		return m_sce->GetBathymetryDepth(Lat, Lon);
	case PLAYBACK_STATE:
		return m_playback->envData.bathymetry.GetValueAtCoordinate(Lat, Lon);
	}
	return ret;
}

ENVDATAPOINTCOUNT CBitmapEnvFunctions::GetDataPointCounts(BOOL GetSlope)
{
	ENVDATAPOINTCOUNT ret = {0};

	switch(m_state)
	{
	case UNINITIALIZED_STATE:
		return ret;
	case SEED_SCENARIO_STATE:
		return (m_sce->GetBathymetryClassRef())->GetDataPointCounts(GetSlope);
	case PLAYBACK_STATE:
		return m_playback->envData.bathymetry.GetDataPointCounts(GetSlope);
	}
	return ret;
}

void CBitmapEnvFunctions::GetSpeciesDisplayTitle(int Index, char *szBuff, DWORD BufferSize)
{
	switch(m_state)
	{
	case UNINITIALIZED_STATE:
		return;
	case SEED_SCENARIO_STATE:
		m_sce->GetSpeciesDisplayTitle(Index, szBuff, BufferSize);
		return;
	case PLAYBACK_STATE:
		strcpy_s(szBuff, BufferSize, m_playback->pSpeInf[Index].fileTitle);
		return;
	}
}


int CBitmapEnvFunctions::GetSpeciesCount()
{
	switch(m_state)
	{
	case UNINITIALIZED_STATE:
		return 0;
	case SEED_SCENARIO_STATE:
		return m_sce->GetSpeciesCount();
	case PLAYBACK_STATE:
		return m_playback->sce.numSpecies;
	}
	return 0;
}


BATHYEXTREMES CBitmapEnvFunctions::GetBathyExtremes()
{
	BATHYEXTREMES ret = {0};
	switch(m_state)
	{
	case UNINITIALIZED_STATE:
		ret.depthMax = 0;
		ret.depthMin = -1000;
		ret.xMin = -1;
		ret.xMax = 1;
		ret.yMin = -1;
		ret.yMax = 1;
		return ret;
	case SEED_SCENARIO_STATE:
		return (m_sce->GetBathymetryClassRef())->GetExtremes();
	case PLAYBACK_STATE:
		return m_playback->envData.bathymetry.GetExtremes();
	}
	return ret;
}

RESLT CBitmapEnvFunctions::GetRawDataCopy(RAWENVIRONMENTALDATA *pEnvironmentalData, BOOL GetSlope)
{
	switch(m_state)
	{
	case UNINITIALIZED_STATE:
		return OK;
	case SEED_SCENARIO_STATE:
		return (m_sce->GetBathymetryClassRef())->GetRawDataCopy(pEnvironmentalData, GetSlope);
	case PLAYBACK_STATE:
		return m_playback->envData.bathymetry.GetRawDataCopy(pEnvironmentalData, GetSlope);
	}
	return PARAM_INVALID_ERROR;
}

void CBitmapEnvFunctions::DeallocateRawDataCopyMemory(RAWENVIRONMENTALDATA *pEnvironmentalData)
{
	switch(m_state)
	{
	case UNINITIALIZED_STATE:
		return;
	case SEED_SCENARIO_STATE:
		(m_sce->GetBathymetryClassRef())->DeallocateRawDataCopyMemory(pEnvironmentalData);
	case PLAYBACK_STATE:
		m_playback->envData.bathymetry.DeallocateRawDataCopyMemory(pEnvironmentalData);
	}
}

BOOL CBitmapEnvFunctions::AnimatIsASoundSource(int AnimatIndex)
{
	//INHABITINF *ic;
	//BOOL ret = FALSE;
	switch(m_state)
	{
	case UNINITIALIZED_STATE:
		return OK;
	case SEED_SCENARIO_STATE:
		if(AnimatIndex < GetTotalSoundSourceCount())
			return TRUE;
		break;
	case PLAYBACK_STATE:
		return m_playback->animatAssociations[AnimatIndex].acstcSrc.isASoundSource;
	}
	return FALSE;
}


BOOL CBitmapEnvFunctions::SpeciesIsASoundSourceModel(int SpeciesIndex)
{
	switch(m_state)
	{
	case UNINITIALIZED_STATE:
		return OK;
	case SEED_SCENARIO_STATE:
		return m_sce->SpeciesIsASoundSourceModel(SpeciesIndex);
	case PLAYBACK_STATE:
		return m_playback->pSpeInf[SpeciesIndex].description.group == SOUNDSOURCE;
	}
	return FALSE;
}

BOOL CBitmapEnvFunctions::SetSpeciesAsASoundSource(int SpeciesIndex)
{
	switch(m_state)
	{
	case UNINITIALIZED_STATE:
		return TRUE;
	case SEED_SCENARIO_STATE:
		return m_sce->SetSpeciesAtIndexAsSoundSource(SpeciesIndex);
	case PLAYBACK_STATE:
		return TRUE;
	}
	return TRUE;
}


int CBitmapEnvFunctions::GetTotalSoundSourceCount()
{
	switch(m_state)
	{
	case UNINITIALIZED_STATE:
		return 0;
	case SEED_SCENARIO_STATE:
		return m_sce->GetTotalSoundSourceCount();
	case PLAYBACK_STATE:
		return m_numSoundSourcePresent;
	}
	return 0;
}
