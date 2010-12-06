#include "Species.h"
// Cluster.cpp: implementation of the CSpecies class.
//
//////////////////////////////////////////////////////////////////////

#include "Species.h"
#include "3mbsLib.h"

//---------------------------------------------//
// Constructor, destructor, memory deallocation
//---------------------------------------------//
CSpecies::CSpecies()
{
	memset(m_szDisplayTitle, 0, sizeof(m_szDisplayTitle));
	m_podList.Initialize("CSpecies:m_podList");
	m_individualList.Initialize("CSpecies:m_individualList");
}


CSpecies::~CSpecies()
{
	int listLength, i;

	// Delete the dynamically allocated pod list
	listLength = m_podList.Length();
	for(i=0; i<listLength; i++)
		(m_podList.Get(i))->DeleteAllAnimats();
	m_podList.DeleteAll();

	// Delete the dynamically allocated individual list
	listLength = m_individualList.Length();
	for(i=0; i<listLength; i++)
		(m_individualList.Get(i))->DeleteAllAnimats();
	m_individualList.DeleteAll();
}


//--------------------//
// Simulation Routines
//--------------------//
void CSpecies::DeinitializeRun()
{
	int i;
	int listLen = m_podList.Length();

	// Intialize the pod.
	for(i=0; i<listLen; i++)
		m_podList.Get(i)->DeinitializeRun();

	listLen = m_individualList.Length();
	// Initialize the individuals
	for(i=0; i<listLen; i++)
		m_individualList.Get(i)->DeinitializeRun();
}

void CSpecies::InitializeRun(
	const USERPARAMS *pUserSce,
	C3MBRandom **mbRndPtrArr,
	DWORD StartTime,
	DWORD *UniqueID,
	ANIMATSTATE *animatStateArray,
	SCEPARMSSPECIESGROUP *pSpeGroupParamsArray,
	CBathymetry *pBathymetry)
{
	int i;
	SCEPARMSSPECIESGROUP *pSpeGrpPrms;
	int listLen;

	pSpeGrpPrms = &pSpeGroupParamsArray[m_speciesModel.m_speciesModel.description.group];

	// Intialize the pods that are associated with this species.
	listLen = m_podList.Length();
	for(i=0; i<listLen; i++)
	{
		m_podList.Get(i)->InitializeRun(
			pUserSce,
			mbRndPtrArr,
			StartTime,
			&m_speciesModel,
			UniqueID,
			animatStateArray,
			pSpeGrpPrms,
			pBathymetry);
	}

	// Initialize the individuals associated with this species.
	listLen = m_individualList.Length();
	for(i=0; i<listLen; i++)
	{
		m_individualList.Get(i)->InitializeRun(
			pUserSce,
			mbRndPtrArr,
			StartTime,
			&m_speciesModel,
			UniqueID,
			animatStateArray,
			pSpeGrpPrms,
			pBathymetry);
	}
}

/*******************************************************************************
* MEMBER FUNCTION: Update()
* 
* DESCRIPTION:
*	Directs the pods and individuals (both are arrays of class CPod) to have 
*	their animalList iterate one simulated second.
*
* ARGUMENTS:
*	None.
*
* MEMBER VARIABLES ALTERED:
*	None directly.  Instances of CPod as well as their instances of CAnimat wil
*	alter themselves internally.
*
* RETURN VALUE:
*	None.
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   06/27/05  (MJC)  Initial Coding/Transfer	
*******************************************************************************/
void CSpecies::Update(DWORD *pAnimatNumber)
{
	int listLength, i;

	// Animats in Pods
	listLength = m_podList.Length();	
	for(i=0; i<listLength; i++)
		(m_podList.Get(i))->Update(pAnimatNumber);

	// Individual Animats (still in CCpod, however).
	listLength = m_individualList.Length();
	for(i=0; i<listLength; i++)
		(m_individualList.Get(i))->Update(pAnimatNumber);
	return;
}




/*******************************************************************************
* MEMBER FUNCTION: SaveToBinFile()
* 
* DESCRIPTION:
*	Saves this instance to a binary file.  Allows the user to create a scenario
*	and save it to file so they may run the same senario later without having to
*	rebuild it.  See LoadFromBinFile().
*
* ARGUMENTS:
*	HANDLE hd - Handle to the binary file.  The binary file needs to have
*				already been opened by the calling function.
*
* MEMBER VARIABLES ALTERED:
*	None.
*
* RETURN VALUE:
*	None
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   06/27/05  (MJC)  Initial Coding/Transfer
*******************************************************************************/
RESLT CSpecies::SaveToBinFile(HANDLE hd)
{
	DWORD	 bytes;
	TCHAR	_reservedBuffer[SIZE_32] = {0};
	int		 i, listLength; // used for number of pods and number of individuals.
	RESLT ret = OK;

	//----------------------------------------------------------------------------------//
	// SPECIES MODEL
	//----------------------------------------------------------------------------------//
	// Write the file header and the species model.
	if(0 == WriteFile(hd, m_szDisplayTitle, sizeof(m_szDisplayTitle), &bytes, NULL))
		return FILEWRITE_ERROR;

	// Write the governing species model to file.
	if(OK != (ret = m_speciesModel.SaveToBinFile(hd)))
		return ret;

	//----------------------------------------------------------------------------------//
	// SPECIES POPULATION
	//----------------------------------------------------------------------------------//
	// Write the pods
	listLength = m_podList.Length();
	if(0 == WriteFile(hd, &listLength, sizeof(DWORD), &bytes, NULL))
		return FILEWRITE_ERROR;
	if(0 == WriteFile(hd, &_reservedBuffer, SIZE_32-sizeof(DWORD), &bytes, NULL)) // Start the pods 16-byte aligned
		return FILEWRITE_ERROR;

	for(i=0; i<listLength; i++)
	{
		if(OK != (ret = (m_podList.Get(i))->SaveToBinFile(hd)))
			return ret;
	}

	// Write the individuals.
	listLength = m_individualList.Length();
	if(0 == WriteFile(hd, &listLength, sizeof(DWORD), &bytes, NULL))
		return FILEWRITE_ERROR;
	if(0 == WriteFile(hd, &_reservedBuffer, SIZE_32-sizeof(DWORD), &bytes, NULL)) // Start the pods 16-byte aligned
		return FILEWRITE_ERROR;

	for(i=0; i<listLength; i++)
	{
		if(OK != (ret = (m_individualList.Get(i))->SaveToBinFile(hd)))
			return ret;
	}
	return ret;
}


/*******************************************************************************
* MEMBER FUNCTION: LoadFromBinFile()
* 
* DESCRIPTION:
*	Loads from a binary file member variables for this instance of class CSpecies.
*	Allows the user to load a saved scenario so they don't have to rebuild one
*	every time.  See SaveToBinFile().
*
* ARGUMENTS:
*	HANDLE hd - Handle to the binary file.  The binary file needs to have
*				already been opened by the calling function.
*
* MEMBER VARIABLES ALTERED:
*	None.
*
* RETURN VALUE:
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   06/27/05  (MJC)  Initial Coding/Transfer
*******************************************************************************/
RESLT CSpecies::LoadFromBinFile(HANDLE hd)
{
	DWORD	bytes;
	BYTE	_reservedBuffer[SIZE_32] = {0};
	int		 i, listLength; // used for number of pods and number of individuals.
	RESLT ret = OK;
	CPod	*pPod;

	// Read in the file header and the species model.
	if(0 == ReadFile(hd, m_szDisplayTitle, sizeof(m_szDisplayTitle), &bytes, NULL))
		return FILEREAD_ERROR;

	if(OK != (ret = m_speciesModel.LoadFromBinFile(hd)))
		return ret;

	// Read the pods
	if(0 == ReadFile(hd, &listLength, sizeof(DWORD), &bytes, NULL))
		return FILEREAD_ERROR;
	if(0 == ReadFile(hd, &_reservedBuffer, SIZE_32-sizeof(DWORD), &bytes, NULL)) // Start the pods 16-byte aligned
		return FILEREAD_ERROR;
	for(i=0; i<listLength; i++)
	{
		// Add a pod, check for allocation errors.
		if(NULL == (pPod = m_podList.Add()))
		{
			m_podList.DeleteAll();
			return MEMALLOC_ERROR;
		}

		// Load pod information into the newly allocated pod, check for file read errors.
		if(OK != (ret = pPod->LoadFromBinFile(hd)))
		{
			m_podList.DeleteAll();
			return ret;
		}
	}

	// Write the individuals.
	if(0 == ReadFile(hd, &listLength, sizeof(DWORD), &bytes, NULL))
		return FILEREAD_ERROR;
	if(0 == ReadFile(hd, &_reservedBuffer, SIZE_32-sizeof(DWORD), &bytes, NULL)) // Start the pods 16-byte aligned
		return FILEREAD_ERROR;
	for(i=0; i<listLength; i++)
	{
		// Add an individual, check for allocation errors.
		if(NULL == (pPod = m_individualList.Add()))
		{
			m_podList.DeleteAll();
			m_individualList.DeleteAll();
			return MEMALLOC_ERROR;
		}
		// Load individual information into the newly allocated pod, check for file read errors.
		if(OK != (ret = pPod->LoadFromBinFile(hd)))
		{
			m_podList.DeleteAll();
			m_individualList.DeleteAll();
			return ret;
		}
	}
	return ret;
}


/*******************************************************************************
* MEMBER FUNCTION: LoadModelFromBinFile()
* 
* DESCRIPTION:
*	Instructs this Cluster's species model (CSpeciesModel) to load a binary 
*	species model.  The binary file will have been created when the user used
*	the species builder.
*
* ARGUMENTS:
*	None.
*
* MEMBER VARIABLES ALTERED:
*	TCHAR *FileName - The title of the binary file to load.
*
* RETURN VALUE:
*	A BOOL status.
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   06/27/05  (MJC)  Initial Coding/Transfer
*******************************************************************************/
RESLT CSpecies::LoadModelFromBinFile(TCHAR *FileName)
{
	RESLT res;
	res = m_speciesModel.LoadFromBinFile(FileName);
	return res;
}

/*******************************************************************************
* MEMBER FUNCTION:
* 
* DESCRIPTION:
*	Instructs this Cluster's species model (CSpeciesModel) to load a binary 
*	species model.  The binary file will have been created when the user used
*	the species builder.  In this case, the CSpeciesModel binary data is part
*	of a larger file making up the scenario.  
*
* ARGUMENTS:
*	HANDLE hd - An opened handle to the binary file with the handle pointer 
*	at the location of the binary model data.
*
* MEMBER VARIABLES ALTERED:
*	None.
*
* RETURN VALUE:
*	A BOOL status.
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   06/27/05  (MJC)  Initial Coding/Transfer
*******************************************************************************/
RESLT CSpecies::LoadModelFromBinFile(HANDLE hd)
{
	RESLT res = m_speciesModel.LoadFromBinFile(hd);
	return res;
}


/*******************************************************************************
* MEMBER FUNCTION: ModelToText()
* 
* DESCRIPTION:
*	Tells the member variable m_speciesModel to output itself to a text file so the
*	user can see it in text form.
*
* ARGUMENTS:
*	None.
*
* MEMBER VARIABLES ALTERED:
*	None.
*
* RETURN VALUE:
*	A BOOL status.
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   06/27/05  (MJC)  Initial Coding/Transfer
*******************************************************************************/
RESLT CSpecies::ModelToText(FILE *fd)
{
	return m_speciesModel.ModelToText(fd);
}

RESLT CSpecies::ModelToText(TCHAR *FileName)
{
	RESLT res;
	FILE *fd;

	fopen_s(&fd, FileName, "w");
	if(NULL == fd)
		return OPENFILEWRITE_ERROR;

	res = m_speciesModel.ModelToText(fd);
	fclose(fd);
	return res;
}





/*******************************************************************************
* MEMBER FUNCTION: SetDisplayTitle()
* 
* DESCRIPTION:
*	Sets the member variable m_szDisplayTitle to the title passed in.  This 
*	title has use in, for example, the GUI and setting up text file ouput title
*	prefixes that get passed downward to the instantiations of class CAnimat.
*
* ARGUMENTS:
*	TCHAR *Title - A string that is the name of the title.
*
* MEMBER VARIABLES ALTERED:
*	m_szDisplayTitle
*
* RETURN VALUE:
*	None.
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   06/27/05  (MJC)  Initial Coding/Transfer
*******************************************************************************/
void CSpecies::SetDisplayTitle(TCHAR *Title)
{
	strncpy_s((TCHAR *)m_szDisplayTitle, sizeof(m_szDisplayTitle), Title, sizeof(m_szDisplayTitle)-1);
}


/*******************************************************************************
* MEMBER FUNCTION: GetDisplayTitle()
* 
* DESCRIPTION:
*	Gets a copy of the  member variable m_szDisplayTitle.  This 
*	title has use in, for example, the GUI and setting up text file ouput title
*	prefixes that get passed downward to the instantiations of class CAnimat.
*
* ARGUMENTS:
*	TCHAR *szBuffer - buffer to hold a copy of the display title.
*	DWORD BufferSize - the size of the buffer to hold the display title.
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
void CSpecies::GetDisplayTitle(TCHAR *szBuffer, DWORD BufferSize)
{
	int strLen = strlen(m_szDisplayTitle);
	int memToClear = BufferSize - strLen;
	strncpy_s(szBuffer, BufferSize, m_szDisplayTitle, strLen);
	memset(&szBuffer[strLen], 0, memToClear); // remove unasked for microsoft paddding chars.
}


DWORD CSpecies::GetNumBehaviorsModeled()
{
	return m_speciesModel.m_speciesModel.description.numBehaviors;
}


void CSpecies::SetAsSoundSource()
{
	m_speciesModel.m_speciesModel.description.group = SOUNDSOURCE;
}


SPECIES_MDL CSpecies::GetSpeciesModelStructCopy()
{
	return m_speciesModel.m_speciesModel;
}

void CSpecies::CopyBehaviorNames(BEHAVIOR_NAME *CopyBuffer, DWORD BufferLength, UINT32 BehaviorCnt)
{
	int copyCnt = BehaviorCnt;
	int i;
	int strLen;


	if(m_speciesModel.m_speciesModel.description.numBehaviors < BehaviorCnt)
		copyCnt = m_speciesModel.m_speciesModel.description.numBehaviors;

	for(i=0; i<copyCnt; i++)
	{
		strLen = strlen(m_speciesModel.m_speciesModel.p.behavior[i].szName);
		strncpy_s(CopyBuffer[i].sz, BufferLength, m_speciesModel.m_speciesModel.p.behavior[i].szName, strLen);
		// remove unasked for microsoft paddding chars.
		memset(&CopyBuffer[i].sz[strLen], 0, BufferLength - strLen);

	}
}




int CSpecies::GetTotalAnimatCount()
{
	int entireSpeciesPopulation = 0;
	int i, listLength= m_podList.Length();

	AssertePodsAndIndividualPopulation();

	for(i=0; i<listLength; i++)
		entireSpeciesPopulation += (m_podList.Get(i))->GetAnimatCount();

	return entireSpeciesPopulation + m_individualList.Length();
}


void CSpecies::DeletePopulation()
{
	DeleteAllPods();
	DeleteAllIndividuals();
}

RESLT CSpecies::AddPod(PODLEADERTYPE LeaderType, double FocalDistance, int BufferLength, INHABITINF *Buffer)
{
	CPod	 *pPod = NULL;
	RESLT  ret = OK;

	_ASSERTE((BufferLength==0 && Buffer==NULL) || (BufferLength>0 && Buffer!=NULL));

	if(NULL == (pPod = m_podList.Add()))
		return MEMALLOC_ERROR;
	pPod->SetLeaderType(LeaderType);
	pPod->SetLeaderFocalDistance(FocalDistance);

	if(BufferLength > 0)
		ret = pPod->AddAnimats(Buffer, BufferLength);

	return ret;
}

int CSpecies::GetNumberOfPods()
{
	return m_podList.Length();
}


void CSpecies::DeleteAllPods()
{
	int listLength = m_podList.Length(), i;
	for(i=0; i<listLength; i++)
		(m_podList.Get(i))->DeleteAllAnimats();
	m_podList.DeleteAll();
}

void CSpecies::DeletePod(int PodIndex)
{
	AssertePodIndex(PodIndex);
	(m_podList.Get(PodIndex))->DeleteAllAnimats();
	m_podList.Delete(PodIndex);
}


RESLT CSpecies::AddPodMembers(int PodIndex, INHABITINF *InitCond, int BufferLength)
{
	AssertePodIndex(PodIndex);
	return (m_podList.Get(PodIndex))->AddAnimats(InitCond, BufferLength);
}

int CSpecies::GetNumberAnimatsInPod(int PodIndex)
{
	AssertePodIndex(PodIndex);
	return (m_podList.Get(PodIndex))->GetAnimatCount();
}

void CSpecies::DeletePodMember(int PodIndex, int MemberIndex)
{
	AssertePodIndex(PodIndex);
	(m_podList.Get(PodIndex))->DeleteAnimat(MemberIndex);
}



RESLT CSpecies::AddIndividuals(INHABITINF *InitCond, int BufferLength)
{
	RESLT ret = OK;
	CPod	 *pPod;
	int		 i;

	for(i=0; (i<BufferLength) && (ret == OK); i++)
	{
		if(NULL == (pPod = m_individualList.Add()))
			ret = MEMALLOC_ERROR;
		else
			ret = pPod->AddAnimats(&InitCond[i], 1);
	}

	return ret;
}

SPECIESGROUP CSpecies::GetSpeciesType()
{
	return (SPECIESGROUP)m_speciesModel.m_speciesModel.description.group;
}


int CSpecies::GetNumberOfIndividuals()
{
	return m_individualList.Length();
}

void CSpecies::DeleteAllIndividuals()
{
	int listLength = m_individualList.Length(), i;
	for(i=0; i<listLength; i++)
	{
		_ASSERT(m_individualList.Get(i) != NULL);
		(m_individualList.Get(i))->DeleteAllAnimats();
	}
	m_individualList.DeleteAll();
}

void CSpecies::DeleteIndividual(int IndividualIndex)
{
	AsserteIndividualIndex(IndividualIndex);
	(m_individualList.Get(IndividualIndex))->DeleteAllAnimats();
	m_individualList.Delete(IndividualIndex);
}


void CSpecies::SetPodLeaderType(int PodIndex, PODLEADERTYPE Type)
{
	AssertePodIndex(PodIndex);
	(m_podList.Get(PodIndex))->SetLeaderType(Type);
}

PODLEADERTYPE CSpecies::GetPodLeaderType(int PodIndex)
{
	AssertePodIndex(PodIndex);
	return (m_podList.Get(PodIndex))->GetLeaderType();
}

void CSpecies::SetPodLeaderFocalDistance(int PodIndex, double FocalDistance)
{
	AssertePodIndex(PodIndex);
	(m_podList.Get(PodIndex))->SetLeaderFocalDistance(FocalDistance);
}

double CSpecies::GetPodLeaderFocalDistance(int PodIndex)
{
	AssertePodIndex(PodIndex);
	return (m_podList.Get(PodIndex))->GetLeaderFocalDistance();
}

double CSpecies::GetShoreFollowingDepth()
{
	return m_speciesModel.m_speciesModel.description.shoreFollowDepth;
}

double CSpecies::GetMinimumSeedingDepth()
{
	return m_speciesModel.m_speciesModel.description.minSeedingDepth;
}


COORD_DEPTH CSpecies::GetPodFocalCoordinate(int PodIndex)
{
	PODLEADERTYPE t;
	AssertePodIndex(PodIndex);
	CPod *p;
	COORD_DEPTH c = {0};

	t = GetPodLeaderType(PodIndex); 
	p = m_podList.Get(PodIndex);
	if(p->GetAnimatCount() == 0)
		return c;

	if(t == ANIMAT)
		return p->GetCurrentCoordinate(0).coord; // Leader is first animat in the pod.
	return p->CalculateCentroid();
}

INHABITINF CSpecies::GetPodMemberInitialCoordinate(int PodIndex, int MemberIndex)
{
	AssertePodIndex(PodIndex);
	return (m_podList.Get(PodIndex))->GetAnimatInitialCondition(MemberIndex);
}

INHABITINF CSpecies::GetIndividualInitialCoordinate(int IndividualIndex)
{
	AsserteIndividualIndex(IndividualIndex);
	return (m_individualList.Get(IndividualIndex))->GetAnimatInitialCondition(0);
}


INHABITINF CSpecies::GetIndividualCoordinate(int IndividualIndex)
{
	AsserteIndividualIndex(IndividualIndex);
	return (m_individualList.Get(IndividualIndex))->GetCurrentCoordinate(0);
}

INHABITINF CSpecies::GetPodMemberCoordinate(int PodIndex, int MemberIndex)
{
	AssertePodIndex(PodIndex);
	return (m_podList.Get(PodIndex))->GetCurrentCoordinate(MemberIndex);
}

int CSpecies::SetAcousticExposureAllAnimats(double Lat, double Lon, double *dbBuffer)
{
	int i, totalAnimats=0, listLength;

	// Set the Pods acoustic exposure
	listLength = m_podList.Length();
	for(i=0; i<listLength; i++)
		totalAnimats += (m_podList.Get(i))->SetAcousticExposure(Lat, Lon, &dbBuffer[totalAnimats]);

	// Set the Individuals' acoustic exposure
	listLength = m_individualList.Length();
	for(i=0; i<listLength; i++)
		totalAnimats += (m_individualList.Get(i))->SetAcousticExposure(Lat, Lon, &dbBuffer[totalAnimats]);

	return totalAnimats;
}

int CSpecies::SetBathyAllAnimats(double *Buffer)
{
	int i, totalAnimats=0, listLength;

	// Set the Pods acoustic exposure
	listLength = m_podList.Length();
	for(i=0; i<listLength; i++)
		totalAnimats += (m_podList.Get(i))->SetBathymetryDepth(&Buffer[totalAnimats]);

	// Set the Individuals' acoustic exposure
	listLength = m_individualList.Length();
	for(i=0; i<listLength; i++)
		totalAnimats += (m_individualList.Get(i))->SetBathymetryDepth(&Buffer[totalAnimats]);

	return totalAnimats;
}

INHABITINF CSpecies::GetAnimatCurrentCoord(int AnimatIndex)
{
	INHABITINF cd = {0};
	INHABITINF *cdBuff = NULL;
	int totalAnimatCount = GetTotalAnimatCount();

	if(totalAnimatCount < 1 || AnimatIndex < 0 || AnimatIndex >= totalAnimatCount)
		return cd;

	cdBuff = new INHABITINF[totalAnimatCount];
	GetAllAnimatCurrentCoordinates(cdBuff);

	cd = cdBuff[AnimatIndex];
	delete [] cdBuff;
	return cd;
}

void CSpecies::GetPodMemberState(int PodIndex, int MemberIndex, ANIMATSTATE_FILEOUT *pState)
{
	_ASSERT(pState != NULL);
	if(PodIndex < 0 || PodIndex >= m_podList.Length())
	{
		memset(pState, 0, sizeof(ANIMATSTATE_FILEOUT));
		return;
	}
	m_podList.Get(PodIndex)->GetPodMemberState(MemberIndex, pState);
}
void CSpecies::GetIndividualState(int IndividualIndex, ANIMATSTATE_FILEOUT *pState)
{
	_ASSERT(pState != NULL);
	AsserteIndividualIndex(IndividualIndex);
	m_individualList.Get(IndividualIndex)->GetPodMemberState(0, pState);
}


int CSpecies::GetAllAnimatInitialCoordinates(INHABITINF *IC)
{
	int i, totalAnimats=0, listLength = m_podList.Length();

	if(IC == NULL)
		return 0;

	// Pods
	for(i=0; i<listLength; i++)
		totalAnimats += GetAllPodMemberInitialCoordinates(i, &IC[totalAnimats]);

	// Individuals
	totalAnimats += GetAllIndividualInitialCoordinates(&IC[totalAnimats]);

	_ASSERTE(totalAnimats == GetTotalAnimatCount());
	return totalAnimats;
}

BOOL CSpecies::SetInhabitantAcousticSourceInf(int InhabitantIndex, ACOUSTICSRCEINF AcstSrcInf)
{
	int i;
	int cnt = 0;
	int newIndex = InhabitantIndex;
	int listLen;

	// Check the pod list
	listLen = m_podList.Length();
	for(i=0; i<listLen; i++)
	{
		cnt += m_podList.Get(i)->GetAnimatCount();
		if(InhabitantIndex < cnt)
		{
			m_podList.Get(i)->SetInhabitantAcousticSourceInf(newIndex, AcstSrcInf);
			return TRUE;
		}
		newIndex -= m_podList.Get(i)->GetAnimatCount();
	}

	// Check the individuals list
	listLen = m_individualList.Length();
	for(i=0; i<listLen; i++)
	{
		cnt += m_individualList.Get(i)->GetAnimatCount();
		if(InhabitantIndex < cnt)
		{
			_ASSERT(newIndex == 0);
			m_individualList.Get(i)->SetInhabitantAcousticSourceInf(newIndex, AcstSrcInf);
			return TRUE;
		}
		newIndex -= m_individualList.Get(i)->GetAnimatCount();
	}
	
	return FALSE;
}


int CSpecies::GetAllAnimatCurrentCoordinates(INHABITINF *CoordBuffer)
{
	int i, totalAnimats=0, listLength = m_podList.Length();

	if(CoordBuffer == NULL)
		return 0;

	// Pods
	for(i=0; i<listLength; i++)
		totalAnimats += GetAllPodMemberCurrentCoordinates(i, &CoordBuffer[totalAnimats]);

	// Individuals
	totalAnimats += GetAllIndividualCurrentCoordinates(&CoordBuffer[totalAnimats]);

	_ASSERTE(totalAnimats == GetTotalAnimatCount());
	return totalAnimats;
}


int CSpecies::GetAllPodMemberCurrentCoordinates(int PodIndex, INHABITINF *CoordBuffer)
{
	AssertePodIndex(PodIndex);
	return (m_podList.Get(PodIndex))->GetAllCurrentCoordinates(CoordBuffer);
}

int CSpecies::GetAllIndividualCurrentCoordinates(INHABITINF *CoordBuffer)
{
	int i, listLength = m_individualList.Length();

	for(i=0; i<listLength; i++)
		(m_individualList.Get(i))->GetAllCurrentCoordinates(&CoordBuffer[i]);

	return listLength;
}

int CSpecies::GetAllPodMemberInitialCoordinates(int PodIndex, INHABITINF *IC)
{
	AssertePodIndex(PodIndex);
	return (m_podList.Get(PodIndex))->GetAllInitialCoordinates(IC);
}

int CSpecies::GetAllIndividualInitialCoordinates(INHABITINF *IC)
{
	int i, listLength = m_individualList.Length();

	for(i=0; i<listLength; i++)
		(m_individualList.Get(i))->GetAllInitialCoordinates(&IC[i]);

	return listLength;
}



// ---- Assertions ------

void CSpecies::AssertePodIndex(int Index)
{
	_ASSERTE(Index >= 0 && Index < m_podList.Length());
}

void CSpecies::AsserteIndividualIndex(int Index)
{
	_ASSERTE(Index >= 0 && Index < m_individualList.Length());
}


void CSpecies::AssertePodsAndIndividualPopulation()
{
	int listLength, i;

	// Verify the Pods have a population greater than 0
	listLength = m_podList.Length();
	for(i=0; i<listLength; i++)
		_ASSERTE((m_podList.Get(i))->GetAnimatCount() >= 0);

	// Verfiy the indivduals (pods) have a population of 1.
	listLength = m_individualList.Length();
	for(i=0; i<listLength; i++)
		_ASSERTE((m_individualList.Get(i))->GetAnimatCount() == 1);
	
}
