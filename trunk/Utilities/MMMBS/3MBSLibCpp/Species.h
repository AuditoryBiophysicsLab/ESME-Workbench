// Cluster.h: interface for the CSpecies class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_PODCLUSTER_H__9D4D6348_BACF_4ECC_87DE_5EACC7AFF83F__INCLUDED_)
#define AFX_PODCLUSTER_H__9D4D6348_BACF_4ECC_87DE_5EACC7AFF83F__INCLUDED_

#include "datatypes.h"
#include "Pod.h"

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

/*----------------------------------------------------------------------------------------
	STRUCT: POD_RUNPARAMS:
		Used for passing initialization parameters to each animat during calls to
		function InitializeRun().

	STRUCT MEMBERS:
		simulationClock:		A pointer to class scenario's clock that maintains the simulation
						time.
		distCalcMethod:	The method to use in calculating this animat's traveled distance
						at each iteration.
		numIterations:  The number of iterations to be run in the current scenario.  Class
						CPod uses this to calculate data placement (specifically, animat
						states at each execution) in the binary output file generated when
						the scenario is run.
		bathymetry:		A pointer to the bathymetry environmental data used in the current
						scenario.
		ID:				This cluster ID, used for generating file names for animat text
						output files.
		sztitlePrefix:	A string pointer that makes up the prefix to this cluster's text
						file if the user chooses to output a text file.
		clusterStruct:	Information on pods, individuals, and animats to populate this 
						cluster with.
----------------------------------------------------------------------------------------*/
#if 0
typedef struct SPECIES_RUNPARAMS
{
	DWORD startTime;
	DISTCALC distCalcMethod;
	CEnvironmentData *bathymetry;
	CEnvironmentData *salinity;
	CEnvironmentData *temperature;
	DWORD ID;
	TCHAR *szTitlePrefix;
	SCEPARMSSPECIESGROUP *speciesGroupParams;
	ANIMATSTATE *animatStateArray;
}SPECIES_RUNPARAMS;
#endif

typedef LinkedList <CPod> PODLIST;

class CSpecies  
{
public:
	//--------------//
	// Constructors
	//--------------//
	CSpecies();
	virtual ~CSpecies();

	int GetAllAnimatCurrentCoordinates(INHABITINF *CoordBuffer);
	int GetAllAnimatInitialCoordinates(INHABITINF *IC);

	INHABITINF GetIndividualInitialCoordinate(int IndividualIndex);
	int GetAllIndividualInitialCoordinates(INHABITINF *IC);
	int GetAllIndividualCurrentCoordinates(INHABITINF *CoordBuffer);

	INHABITINF GetPodMemberInitialCoordinate(int PodIndex, int MemberIndex);
	int GetAllPodMemberInitialCoordinates(int PodIndex, INHABITINF *IC);
	int GetAllPodMemberCurrentCoordinates(int PodIndex, INHABITINF *CoordBuffer);

	void GetPodMemberState(int PodIndex, int MemberIndex, ANIMATSTATE_FILEOUT *pState);
	void GetIndividualState(int IndividualIndex, ANIMATSTATE_FILEOUT *pState);

	INHABITINF GetPodMemberCoordinate(int PodIndex, int MemberIndex);
	INHABITINF GetIndividualCoordinate(int IndividualIndex);

	SPECIESGROUP GetSpeciesType();

	//-----------------------------------//
	// Population Access and Modification
	//-----------------------------------//
	// Entire Population
	INHABITINF GetAnimatCurrentCoord(int AnimatIndex);
	int  GetTotalAnimatCount();

	BOOL SetInhabitantAcousticSourceInf(int InhabitantIndex, ACOUSTICSRCEINF AcstSrcInf);

	int  GetNumberOfPods();
	int  GetNumberAnimatsInPod(int PodIndex);
	int  GetNumberOfIndividuals();
	void DeletePopulation();
	double GetShoreFollowingDepth();
	double GetMinimumSeedingDepth();

	// Pods
	RESLT AddPod(PODLEADERTYPE LeaderType=ANIMAT, double FocalDistance=0, int BufferLength=0, INHABITINF *Buffer=NULL);
	RESLT AddPodMembers(int PodIndex, INHABITINF *CoordBuffer, int BufferLength=1);
	void DeletePodMember(int PodIndex, int MemberIndex);
	void DeletePod(int PodIndex);
	void DeleteAllPods();

	COORD_DEPTH GetPodFocalCoordinate(int PodIndex);
	double GetPodLeaderFocalDistance(int PodIndex);
	void SetPodLeaderFocalDistance(int PodIndex, double FocalDistance);
	PODLEADERTYPE GetPodLeaderType(int PodIndex);
	void SetPodLeaderType(int PodIndex, PODLEADERTYPE Type);


	// Individuals
	RESLT AddIndividuals(INHABITINF *InitCond, int BufferLength = 1);
	void DeleteIndividual(int IndividualIndex);
	void DeleteAllIndividuals();


	//--------------------//
	// Simulation Routines
	//--------------------//
	void Update(DWORD *pAnimatNumber);
	//void InitializeRun(SPECIES_RUNPARAMS runParam, DWORD *UniqueID);
	void DeinitializeRun();
	void InitializeRun(
		const USERPARAMS *pUserSce,
		DWORD StartTime,
		DWORD *UniqueID,
		ANIMATSTATE *animatStateArray,
		SCEPARMSSPECIESGROUP *pSpeGroupParamsArray,
		C3MBRandom *pRandomRef,
		CBathymetry *pBathymetry);

	int SetAcousticExposureAllAnimats(double Lat, double Lon, double *dbBuffer);
	int SetBathyAllAnimats(double *Buffer);
	//-----------------------------//
	// File Functions and Variables
	//-----------------------------//
	RESLT ModelToText(FILE *fd);
	RESLT LoadModelFromBinFile(HANDLE hd);
	RESLT LoadModelFromBinFile(TCHAR *FileName);
	RESLT ModelToText(TCHAR *FileName);
	RESLT LoadFromBinFile(HANDLE hd);
	RESLT SaveToBinFile(HANDLE hd);


	//--------------------------//
	// Member variable accessors
	//--------------------------//
	SPECIES_MDL GetSpeciesModelStructCopy();
	void  SetAsSoundSource();
	void  SetDisplayTitle(TCHAR *Title);
	void  GetDisplayTitle(TCHAR *szBuffer, DWORD BufferSize);
	DWORD GetNumBehaviorsModeled();
	void  CopyBehaviorNames(BEHAVIOR_NAME *CopyBuffer, DWORD BufferLength, UINT32 BehaviorCnt);

	//------------------//
	// Population Access
	//------------------//
private:
	void AsserteIndividualIndex(int Index);
	void AssertePodsAndIndividualPopulation();
	void AssertePodIndex(int Index);
	//---------------------//
	// Simulation Variables
	//---------------------//
	CListManager <CPod> m_podList;
	CListManager <CPod> m_individualList;
	CSpeciesModel m_speciesModel;	// The species model that governs pods and individuals in this
	//PODLIST m_podList;
	//PODLIST	m_individualList;
	TCHAR	m_szDisplayTitle[SIZE_64]; // Display title (taken from the file title).
							// cluster
};

#endif // !defined(AFX_PODCLUSTER_H__9D4D6348_BACF_4ECC_87DE_5EACC7AFF83F__INCLUDED_)
