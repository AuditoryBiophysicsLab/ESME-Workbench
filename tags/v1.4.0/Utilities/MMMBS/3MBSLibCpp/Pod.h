// Pod.h: interface for the CPod class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_POD_H__E1CEB7FE_72D4_4353_AEBA_75CAD87C6CE2__INCLUDED_)
#define AFX_POD_H__E1CEB7FE_72D4_4353_AEBA_75CAD87C6CE2__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "datatypes.h"
#include "Animat.h"
#include "SpeciesModel.h"

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
		speciesModel:	A pointer to the species model defining this animat's movement,
						behavior, and reponse to acoustic exposure.
		bathymetry:		A pointer to the bathymetry environmental data used in the current
						scenario.
		titlePrefix:	A string pointer that makes up the prefix to this animat's text
						file if the user chooses to output a text file.
		coordArray:		An array of type COORD_DEPTH that holds each animat in the pod's
						initial location.
		coordArrayLen:	The length of the array coordArray... effectively the number of 
						animats that are part of this pod.
----------------------------------------------------------------------------------------*/
#if 0
typedef struct POD_RUNPARAMS
{
	int startTime;
	DISTCALC distCalcMethod;
	SPECIES_MDL *speciesDef;
	CEnvironmentData *bathymetry;
	CEnvironmentData *salinity;
	CEnvironmentData *temperature;
	//TCHAR			 *titlePrefix;
	SCEPARMSSPECIESGROUP *speciesGroupParams;
}POD_RUNPARAMS;
#endif
//typedef LinkedList <CAnimat> ANIMATLIST;

class CPod  
{
public:
	//------------------------------------------------//
	// Constructor, destructor and memory deallocation
	//------------------------------------------------//
	CPod();
	virtual ~CPod();


	RESLT	LoadFromBinFile(HANDLE hd);
	RESLT	SaveToBinFile(HANDLE hd);

	// Populating With Animats
	RESLT AddAnimats(INHABITINF *pInitCondArray, int ArrayLength);
	void	 DeleteAnimat(int Index);
	void	 DeleteAllAnimats();

	// Animat Population
	int GetAnimatCount();

	INHABITINF GetAnimatInitialCondition(int Index);
	int GetAnimatUniqueID(int Index);

	int SetInhabitantAcousticSourceInf(int PodMemberIndex, ACOUSTICSRCEINF AcstSrcInf);

	int	GetAllInitialCoordinates(INHABITINF *IC);

	INHABITINF GetCurrentCoordinate(int Index);
	int	GetAllCurrentCoordinates(INHABITINF *CoordBuffer);

	// Leader Focal Distance and Type
	void		  SetLeaderFocalDistance(double FocalDistance);
	double		  GetLeaderFocalDistance();
	void		  SetLeaderType(PODLEADERTYPE Type);
	PODLEADERTYPE GetLeaderType();
	COORD_DEPTH	  CalculateCentroid();


	int SetAcousticExposure(double Lat, double Lon, double *dbBuffer);
	int SetBathymetryDepth(double *Buffer);

	//--------------------//
	// Simulation Routines
	//--------------------//
	//void InitializeRun(POD_RUNPARAMS RunParam, DWORD *UniqueID, ANIMATSTATE *animatStateArray);
	void DeinitializeRun();
	void InitializeRun(const USERPARAMS *pUserSce,
			DWORD StartTime,
			CSpeciesModel *pSpeMdl,
			DWORD *UniqueID,
			ANIMATSTATE *animatStateArray,
			SCEPARMSSPECIESGROUP *pSpeGroupParams,
			CBathymetry *pBathymetry);
	void Update(DWORD *pAnimatNumber);

	//-----------------------------//
	// File Functions and Variables
	//-----------------------------//
	void GetPodMemberState(int MemberIndex, ANIMATSTATE_FILEOUT *pState);

private:
	void AssertePopulated();
	void AsserteAnimatIndex(int Index);

	//---------------------//
	// Simulation Variables
	//---------------------//
	CListManager	 <CAnimat> m_animatList;
	double			 m_maxFocalDistance;	// Max distance pod animals may be from the focal animal.
	PODLEADERTYPE	 m_podLeaderType; // Focal animat or centroid


	//-----------------------------//
	// Acoustic Exposure Variables
	//-----------------------------//
	//BOOL			 m_aeExposed;
};
#endif // !defined(AFX_POD_H__E1CEB7FE_72D4_4353_AEBA_75CAD87C6CE2__INCLUDED_)
