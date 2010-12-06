// Pod.cpp: implementation of the CPod class.
//
//////////////////////////////////////////////////////////////////////

#include "Pod.h"
#include "3mbsLib.h"

#include "Windows.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////
CPod::CPod()
{
	m_maxFocalDistance	= 0;
	m_podLeaderType		= ANIMAT;
	//m_aeExposed			= FALSE;
	m_animatList.Initialize("CPod:m_animatList");
}

CPod::~CPod()
{
	DeleteAllAnimats();
}




//--------------------//
// Simulation Routines
//--------------------//

/*******************************************************************************
* MEMBER FUNCTION: InitializeRun()
* 
* DESCRIPTION:
*	Initializes pod member variables for the about-to-run scenario and
*	instantates instances of CAnimat.  This function is called for both Pods of
*	animats and individual animats. 
*
* ARGUMENTS:
*	RunParam - A POD_RUNPARAMS struct containing scenario parameters for the
*		about-to-run scenario (see Pod.h for description of the parameters).
*	MaxFocalAnimalDist - If this is a pod (not an individual), this specifies
*		the maximum distance animats are to be from the lead animat.
*
* RETURN VALUE:
*	An UINT, indicating result of this function.
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   06/27/05  (MJC)  Initial Coding/Transfer
*
*******************************************************************************/
void CPod::DeinitializeRun()
{
	int i;
	int listLen;

	// Initialize each animat in this pod.
	m_animatList.Lock();
	AssertePopulated();
	listLen = m_animatList.Length();
	for(i=0; i<listLen; i++)
	{
		(m_animatList.Get(i))->DeinitializeRun();
	}
	m_animatList.Unlock();
}

void CPod::InitializeRun(const USERPARAMS *pUserSce,
						 C3MBRandom **mbRndPtrArr,
						 DWORD StartTime,
						 CSpeciesModel *pSpeMdl,
						 DWORD *UniqueID,
						 ANIMATSTATE *animatStateArray,
						 SCEPARMSSPECIESGROUP *pSpeGroupParams,
						 CBathymetry *pBathymetry)
{
	int i;
	int listLen;

	// Initialize each animat in this pod.
	m_animatList.Lock();
	AssertePopulated();
	listLen = m_animatList.Length();
	for(i=0; i<listLen; i++)
	{
		(m_animatList.Get(i))->InitializeRun(pUserSce,
											 mbRndPtrArr,
											 StartTime,
											 pSpeMdl,
											 UniqueID,
											 animatStateArray,
											 m_maxFocalDistance,
											 pSpeGroupParams,
											 pBathymetry);
	}
	m_animatList.Unlock();
}

/*******************************************************************************
* MEMBER FUNCTION: Update()
* 
* DESCRIPTION:
*	Tells each animal in the pod to update, which is to iterate one second of 
*	simulation time.
*
*	The first animal in the pod is the focal animal and is solely governed by
*	the species model (CSpeciesModel) the user selected to govern the behavior
*	of this pod with.  The rest are governed by the species model as well, but
*	may have this overriden based upon rules of pod behavior.
*
*	For now, the only pod behavior rule is to stay within a certain distance of
*	the focal animal.
*
* ARGUMENTS:
*	None.
*
* MEMBER VARIABLES ALTERED:
*	None directly.  Animals (CAnimat) variables will alter themselves internally.
*
* RETURN VALUE:
*	UINT.
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   06/27/05  (MJC)  Initial Coding/Transfer
*******************************************************************************/
void CPod::Update(DWORD *pAnimatNumber)
{
	int listLength, i;
	CAnimat *pAnimat;
	INHABITINF focalCoordDepth = {0};
	COORDINATE focalCoord;
	//BOOL		 leadBeached = FALSE;

	// The animat at index 0 is the focus animal.  He updates w/o parameters or concern for following a focus animal
	// because he is the lead animal.  This works whether or not this is truly a pod or just a cluster of
	// individuals.
	m_animatList.Lock();
	{
		AssertePopulated();
		listLength = m_animatList.Length();
		pAnimat = m_animatList.Get(0);
		if(m_podLeaderType == CENTROID)
		{
			// If pod leader type is centroid but this pod is currently in acoustic exposure,
			focalCoordDepth.coord = CalculateCentroid();
			focalCoord.lat = focalCoordDepth.coord.lat;
			focalCoord.lon = focalCoordDepth.coord.lon;
			pAnimat->SetFocalCoordinate(&focalCoord);
			pAnimat->Update(pAnimatNumber);
		}
		else //(m_podLeaderType == ANIMAT)
		{
			pAnimat->Update(pAnimatNumber); // The lead animat (index 0) doesn't get its focal coordiante set.
			focalCoordDepth = pAnimat->GetCurrentCoordinates(); // The lead animat's coordinates
			focalCoord.lat = focalCoordDepth.coord.lat;
			focalCoord.lon = focalCoordDepth.coord.lon;
		}

		// Update remaing animats in this pod.  If this pod is a single individual
		// this for loop won't execute. (because its index starts at 1)
		for(i=1; i<listLength; i++)
		{
			(m_animatList.Get(i))->SetFocalCoordinate(&focalCoord);
				(m_animatList.Get(i))->Update(pAnimatNumber);
		}
	}
	m_animatList.Unlock();
}



void CPod::GetPodMemberState(int MemberIndex, ANIMATSTATE_FILEOUT *pState)
{
	if(pState == NULL)
		return;

	m_animatList.Lock();
	if(MemberIndex < 0 || MemberIndex >= m_animatList.Length())
	{
		m_animatList.Unlock();
		memset(pState, 0, sizeof(ANIMATSTATE_FILEOUT));
		return;
	}
	m_animatList.Get(MemberIndex)->GetFileOutStateCopy(pState);
	m_animatList.Unlock();
}

/*----------------------------------------------------------------------------------------
  MEMBER FUNCTION: AnimatGetAllCoordinates()
 
  DESCRIPTION:
    When called, each animat belonging to this pod has it's AnimatGetAllCoordinates()
	function called.

  INPUT PARAMETER(S):
	A pointer to the universal coordinate buffer.

  RETURN VALUE:
	None

  REVISION HISTORY:
     Date    Name   Change
   --------  ----   ------
   05/24/05  (MJC)  Initial Coding/Transfer
----------------------------------------------------------------------------------------*/
int CPod::GetAllCurrentCoordinates(INHABITINF *CoordBuffer)
{
	int	i, listLength;

	if(CoordBuffer == NULL)
		return 0;

	m_animatList.Lock();
	{
		listLength = m_animatList.Length();
		for(i=0; i<listLength; i++)
			CoordBuffer[i] = (m_animatList.Get(i))->GetCurrentCoordinates();
	}
	m_animatList.Unlock();
	return listLength;
}

int CPod::GetAllInitialCoordinates(INHABITINF *IC)
{
	int	i, listLength;

	if(IC == NULL)
		return 0;

	m_animatList.Lock(); // lock it so list length can't change.
	{
		listLength = m_animatList.Length();
		for(i=0; i<listLength; i++)
			IC[i] = (m_animatList.Get(i))->GetInhabitantSpecific();
	}
	m_animatList.Unlock();
	return listLength;
}

/*----------------------------------------------------------------------------------------
  MEMBER FUNCTION: SetAcousticExposure()
 
  DESCRIPTION:
    When called, each animat belonging to this pod has it's SetAcousticExposure()
	function called.
 

  INPUT PARAMETER(S):
	A pointer to the universal acoustic exposure buffer.

  RETURN VALUE:
	Returns the number of animats in this pod

  REVISION HISTORY:
     Date    Name   Change
   --------  ----   ------
   05/24/05  (MJC)  Initial Coding/Transfer
----------------------------------------------------------------------------------------*/
int CPod::SetAcousticExposure(double Lat, double Lon, double *dbBuffer)
{
	int	listLength, i;
	CAnimat *a;

	m_animatList.Lock();
	{
		listLength = m_animatList.Length();
		for(i=0; i<listLength; i++)
		{
			a = m_animatList.Get(i);
			a->SetAcoustics(Lat, Lon, dbBuffer[i]);
		}
	}
	m_animatList.Unlock();
	return listLength;
}

int CPod::SetBathymetryDepth(double *Buffer)
{
	int	listLength, i;
	CAnimat *a;

	m_animatList.Lock();
	{
		listLength = m_animatList.Length();
		for(i=0; i<listLength; i++)
		{
			a = m_animatList.Get(i);
			a->SetBathymetryDepth(Buffer[i]);
		}
	}
	m_animatList.Unlock();

	return listLength;
}


RESLT CPod::AddAnimats(INHABITINF *pInitCondArray, int ArrayLength)
{
	int	i;
	CAnimat *pAnimat;
	RESLT ret = OK;

	_ASSERTE(pInitCondArray!=NULL && ArrayLength>0);
	m_animatList.Lock();
	{
		for(i=0; i<ArrayLength; i++)
		{	
			if(NULL == (pAnimat = m_animatList.Add()))
			{
				ret = MEMALLOC_ERROR;
				break;
			}
			//pAnimat->SetInitialCoordinate(pInitCondArray[i].lat, pInitCondArray[i].lon);
			pAnimat->SetInitialConditions(pInitCondArray[i]);
		}
	}
	m_animatList.Unlock();

	return ret;
}

int CPod::SetInhabitantAcousticSourceInf(int PodMemberIndex, ACOUSTICSRCEINF AcstSrcInf)
{
	m_animatList.Lock();
	m_animatList.Get(PodMemberIndex)->SetInhabitantAcousticSourceInf(AcstSrcInf);
	m_animatList.Unlock();
	return 0;
}


INHABITINF CPod::GetCurrentCoordinate(int Index)
{
	INHABITINF cd = {0};
	m_animatList.Lock();
	{
		AsserteAnimatIndex(Index);
		cd = (m_animatList.Get(Index))->GetCurrentCoordinates();
	}
	m_animatList.Unlock();
	return cd;
}


INHABITINF CPod::GetAnimatInitialCondition(int Index)
{
	INHABITINF cd = {0};
	m_animatList.Lock();
	{
		AsserteAnimatIndex(Index);
		cd = (m_animatList.Get(Index))->GetInhabitantSpecific();
	}
	m_animatList.Unlock();
	return cd;
}


COORD_DEPTH CPod::CalculateCentroid()
{
	COORD_DEPTH	 coord;
	COORD_DEPTH	 centroidCoord = {0};
	int			 i, listLength;
	CAnimat		*pAnimat;

	m_animatList.Lock();
	{
		memset(&centroidCoord, 0, sizeof(COORD_DEPTH));
		listLength = m_animatList.Length();
		for(i=0; i<listLength; i++)
		{
			pAnimat = m_animatList.Get(i);
			coord = pAnimat->GetCurrentCoordinates().coord;
			centroidCoord.lat += coord.lat;
			centroidCoord.lon += coord.lon;
			centroidCoord.depth += coord.depth;
		}
	}
	m_animatList.Unlock();

	centroidCoord.lat /= listLength;
	centroidCoord.lon /= listLength;
	centroidCoord.depth /= listLength;
	return centroidCoord;
}

PODLEADERTYPE CPod::GetLeaderType()
{
	return m_podLeaderType;
}

void CPod::SetLeaderType(PODLEADERTYPE Type)
{
	m_podLeaderType = Type;
}

double CPod::GetLeaderFocalDistance()
{
	return m_maxFocalDistance;
}

void CPod::SetLeaderFocalDistance(double FocalDistance)
{
	m_maxFocalDistance = FocalDistance;
}


void CPod::DeleteAllAnimats()
{
	m_animatList.DeleteAll();
}


void CPod::DeleteAnimat(int Index)
{
	m_animatList.Lock();
	{
		AsserteAnimatIndex(Index);
		m_animatList.Delete(Index);
	}
	m_animatList.Unlock();
}

int CPod::GetAnimatCount()
{
	return m_animatList.Length();
}


RESLT CPod::SaveToBinFile(HANDLE hd)
{
	DWORD		bytes;
	int			listLength; // number of animats
	TCHAR		header[SIZE_16];
	TCHAR		_reserve[SIZE_32];
	INHABITINF *initCond;
	RESLT	ret = OK;
	memset(&_reserve, 0, sizeof(_reserve));


	//----------------------------------------------------------------------------------//
	// Write the header "Pod Begins"
	//------------------------------//
	memset(&header, 0, sizeof(header));
	strncpy_s(header, sizeof(header), "PodBegins", SIZE_16);
	if(0 == WriteFile(hd, header, SIZE_16, &bytes, NULL) || bytes != SIZE_16) // 16 bytes
		return FILEWRITE_ERROR;

	m_animatList.Lock();
	{
		listLength = m_animatList.Length();
		if(0 == WriteFile(hd, &listLength, sizeof(int), &bytes, NULL) || bytes != sizeof(int)) // 4 bytes
			ret = FILEWRITE_ERROR;
		if(0 == WriteFile(hd, &m_podLeaderType, sizeof(PODLEADERTYPE), &bytes, NULL) || bytes != sizeof(PODLEADERTYPE)) // 4 bytes
			ret =  FILEWRITE_ERROR;
		if(0 == WriteFile(hd, &m_maxFocalDistance, sizeof(double), &bytes, NULL) || bytes != sizeof(double)) // 8 bytes
			ret = FILEWRITE_ERROR;
		if(0 == WriteFile(hd, &_reserve, SIZE_32, &bytes, NULL) || bytes != SIZE_32) // 32 bytes
			ret =  FILEWRITE_ERROR;
		if(ret != OK)
		{
			m_animatList.Unlock();
			return ret;
		}
		// Get the animat coordinates
		if(NULL == (initCond = new INHABITINF[listLength]))
		{
			m_animatList.Unlock();
			return MEMALLOC_ERROR;
		}
		GetAllInitialCoordinates(initCond);
	}
	m_animatList.Unlock();

	if(0 == WriteFile(hd, initCond, sizeof(INHABITINF)*listLength, &bytes, NULL))
		ret = FILEWRITE_ERROR;
	if(initCond != NULL)
		delete [] initCond;

	return ret;
}

RESLT CPod::LoadFromBinFile(HANDLE hd)
{
	DWORD		bytes;
	int			listLength; // number of animats
	TCHAR		header[SIZE_16];

	TCHAR		_reserve[SIZE_32];
	INHABITINF *initCond;
	RESLT	ret = OK;

	memset(&header, 0, SIZE_16);
	memset(&_reserve, 0, SIZE_32);

	if(0 == (ReadFile(hd, header, SIZE_16, &bytes, NULL)) || bytes != SIZE_16) // 16 bytes
		return FILEREAD_ERROR;
	if(0 == (ReadFile(hd, &listLength, sizeof(int), &bytes, NULL)) || bytes != sizeof(int)) // 4 bytes
		return FILEREAD_ERROR;
	if(0 == (ReadFile(hd, &m_podLeaderType, sizeof(PODLEADERTYPE), &bytes, NULL)) || bytes != sizeof(PODLEADERTYPE)) // 4 bytes
		return FILEREAD_ERROR;
	if(0 == (ReadFile(hd, &m_maxFocalDistance, sizeof(double), &bytes, NULL)) || bytes != sizeof(double)) // 8 bytes
		return FILEREAD_ERROR;
	if(0 == ReadFile(hd, &_reserve, SIZE_32, &bytes, NULL) || bytes != SIZE_32) // 32 bytes
		return FILEREAD_ERROR;

	// Get the animat coordinates
	if(NULL == (initCond = new INHABITINF[listLength]))
		return MEMALLOC_ERROR;
	
	if(0 == (ReadFile(hd, initCond, sizeof(INHABITINF)*listLength, &bytes, NULL)))
	{
		delete [] initCond;
		return FILEREAD_ERROR;
	}
	ret = AddAnimats(initCond, listLength);
	delete [] initCond;

	return ret;
}



void CPod::AsserteAnimatIndex(int Index)
{
	_ASSERTE(Index >= 0 && Index < m_animatList.Length());
}

void CPod::AssertePopulated()
{
	_ASSERTE(m_animatList.Length() > 0);
}

