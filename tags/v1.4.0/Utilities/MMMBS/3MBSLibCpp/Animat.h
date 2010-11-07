// Animat.h: interface for the CAnimat class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_ANIMAT_H__387E759C_7D9A_4E20_B644_5494FFBDCB5E__INCLUDED_)
#define AFX_ANIMAT_H__387E759C_7D9A_4E20_B644_5494FFBDCB5E__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "datatypes.h"
#include "ListManager.h"
#include "SpeciesModel.h"
#include "EnvironmentData.h"
#include "Bathymetry.h"
#include "staticLib.h"
//#include "FileManager.h"


/*
	Design Decisions
	* Members of struct BEHSETPOINTS can only be set (never calculated) based upon what
	  behaviors determine.
	* Calculated depth is local to individual behaviors.  A copy is saved in struct
	  BEHSETPOINTS.

*/

/*----------------------------------------------------------------------------------------
	STRUCT: ANIMAT_RUNPARAMS:
		Used for passing initialization parameters to each animat during calls to
		function InitializeRun().

	STRUCT MEMBERS:
		simulationClock:		A pointer to class scenario's clock that maintains the simulation
						time.
		distCalcMethod:	The method to use in calculating this animat's traveled distance
						at each iteration.
		initialCoord:	This animat's initial coordinate.
		speciesModel:	A pointer to the species model defining this animat's movement,
						behavior, and reponse to acoustic exposure.
		bathymetry:		A pointer to the bathymetry environmental data used in the current
						scenario.
----------------------------------------------------------------------------------------*/
#if 0
typedef struct ANIMAT_RUNPARAMS
{
	// Not saved to file.  Pointers OK.
	int startTime;
	DISTCALC distCalcMethod;
	SPECIES_MDL *speciesDef;
	CEnvironmentData *bathymetry;
	CEnvironmentData *salinity;
	CEnvironmentData *temperature;
	double focalDistance;
	SCEPARMSSPECIESGROUP *speciesGroupParams;
}ANIMAT_RUNPARAMS;
#endif

class CAnimatStatics
{
public:
	// Constructor and destructor
	CAnimatStatics();
	virtual ~CAnimatStatics();

	// Static Functions
	DWORD PackageStateData(PACKAGED_STATE_DATA Data);
	PACKAGED_STATE_DATA UnPackageStateData(DWORD Data);
};

class CAnimat  
{
public:
	// Constructor and destructor
	CAnimat();
	virtual ~CAnimat();

private:
	C3mbStaticsLib m_staticLib;
public:

	//--------------------//
	// Simulation Routines
	//--------------------//
	void InitializeRun(const USERPARAMS *pUserSce,	//
			DWORD StartTime,						// Starting time of day in seconds
			CSpeciesModel *pSpeMdl,					// A pointer to the species model that governs this animat's movments and behavior
			DWORD *UniqueID,						// This animat's unique identification in the scenario among all animats and sound sources
			ANIMATSTATE *pAnimatState,
			double focalDistance,
			SCEPARMSSPECIESGROUP *pSpeGroupParams,
			CBathymetry *pBathymetryRef
			); // Returns UniqueID + 1 
	void SetFocalCoordinate(COORDINATE *FocalCoord); // externally called.

	void DeinitializeRun();

	// All Env Attractor functions need work.
	void SetTempEnvAttractor(double Lat, double Lon, double Value);
	//void SetDepthEnvAttractor(double Lat, double Lon, double Value);
	void SetAcoustics(double Lat, double Lon, double Value);
	void Update(DWORD *pAnimatNumber);
	double CalculateRisk(SCEPARMSSPECIESGROUP *pSpeciesGroupParams, double RecievedLeveldB);
	void SetBathymetryDepth(double Value);
	void GetFileOutStateCopy(ANIMATSTATE_FILEOUT *State);
	double DepthCalc(double Rate, double Depth, ACTIVITY Activity);

	//COORD_DEPTH GetInitialCoordinate();
	//void SetInitialCoordinate(double Lat, double Lon);

	INHABITINF GetCurrentCoordinates(); // Retrieves an animals floating point coordinates.


	void SetInhabitantAcousticSourceInf(ACOUSTICSRCEINF AcstSrcInf);

	void SetInitialConditions(INHABITINF IS);
	INHABITINF GetInhabitantSpecific(); // Retrieves an animals floating point coordinates.


	BOOL IsAcousticSource();
	//BOOL IsAverting();
	BOOL IsBeached();

	int GetUniqueID();

private:
	CAnimatStatics m_classAnimatStatics;
	int m_uniqueId;
	double m_fRiskThreshold; // 
	CSpeciesModel *m_pRefSpeciesModel;
	C3MBRandom *m_pRefRandom;

	COORDINATE m_initialCoord; // the animat's initial location.  Same as seeding coord unless animat goes off screen.
	COORDINATE m_seedingCoord; // the location the user placed the animat while seeding.
	COORDINATE m_currentCoord; // the animat's current coordinate.  

	ACOUSTICSRCEINF m_acstcSrcInf;

	ANIMATSTATE *m_state;
	SPECIES_MDL *m_speciesDef;
	SCEPARMSSPECIESGROUP *m_pSpeciesGroupParams;
	DISTCALC m_distCalcMethod;  // Uses either lat/lon or planar geometry.
	BOOL m_focalCoordSet;
	BOOL m_tempEnvAttCoordSet;
	//BOOL m_depthEnvAttCoordSet;
	//BOOL m_acousticExpSet;
	BOOL m_bathyDepthSet; // this is for bathymetry depths set by external application such as ESME.
	double m_initialBathyDepth;
	//ENVDATA *m_pEnvRef;
	CBathymetry *m_bathymetry;			// Pointer to this species bathymetry class
	ENVDATA_INDEX m_bathySector;		// Location in the bathy data a value was last retrieved a value from.

	void EnforceDepthValueRules();

	//-------------------------//
	// Stimulus-based behaviors
	//-------------------------//
	BOOL CoordinatesOnScreen(const COORDINATE *Crd, ENVMINMAX *EnvMinMax);
	void HandleOffScreeen();
	void RunPodFollowingResponse();
	void PodFollowingAdjustState(DIRCTNMDL *TravelDirMdl);
	void RunAcousticResponse();
	void AcousticAversionAdjustModelAndState(RATEMDL *TravelRateMdl, DIRCTNMDL *TravelDirMdl, DIVEMDL *DiveMdl);

	void RunEnvAttrctrResponse(DEPTH_ENV_ATTRACTOR_MDL *Mdl, ENVATTRACTORSTATE *pEnvAttractorStateRef, const ANIMATSTATE *pRefAnimatState);
	//void RunEnvAttrctrResponse(BOOL *CoordIsSet, ENVATTRACTORMDL *Mdl, ENVATTRACTORSTATE *EnvRs);
	//void EnvAttractorAdjustState(DIRCTNMDL *TravelDirMdl, ENVATTRACTORSTATE *EnvRs);

	void HandleDepthEnvironmentalAttractor();


	void HandleNearbyShore(const RATEMDL *TravelRateMdl, const DIRCTNMDL *TravelDirMdl);
	double GetShoreFollowingBearing(double NrmlHeading);
	void BehaviorTransitionx();
	MATRIX *GetCurrentBehaviorTransitionMatrix(int *TimeRow);
	void NormalTravel();
	BOOL RunTravelRateModel(const RATEMDL *RateModel, RATESTATE *State);
	BOOL RunDirectionModel(const DIRCTNMDL *DirMdl, DIRECTIONSTATE *DirectionState);
	void NormalDive();
	BOOL RunDiveModel(const DIVEMDL *DiveMdl);
	double MergeDesiredHeadings();
	COORDINATE CalcNewCoord(COORDINATE InitCoord, COORDINATE CurrentCoord, double Heading, double Rate, FLOAT_XY *pDeltaXY, DISTCALC DistanceCalcMethod);
};


#endif // !defined(AFX_ANIMAT_H__387E759C_7D9A_4E20_B644_5494FFBDCB5E__INCLUDED_)
