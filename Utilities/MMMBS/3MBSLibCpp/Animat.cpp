// Animat.cpp: implementation of the CAnimat class.
//
//////////////////////////////////////////////////////////////////////
#include <float.h>
#include <stdio.h>

#include "Animat.h"
#include "Random.h"
#include "3mbsLib.h"
#include ".\animat.h"
#include "params.h"




//-----------------------------------//
// Constructor, destructor
//-----------------------------------//
CAnimat::CAnimat()
{
	// Initialize member variables.
	m_state = NULL;
	m_pRefSpeciesModel = NULL;

	m_speciesDef = NULL;
	m_distCalcMethod = PLANAR_GEOMETRY;
	m_bathymetry = NULL;
	memset(&m_bathySector, 0, sizeof(ENVDATA_INDEX));
	m_focalCoordSet = FALSE;
	m_tempEnvAttCoordSet = FALSE;
	//m_depthEnvAttCoordSet = FALSE;
	m_pSpeciesGroupParams = NULL;
	m_bathyDepthSet = FALSE;
	m_uniqueId = -1;
}


CAnimat::~CAnimat()
{
}



/*----------------------------------------------------------------------------------------
  MEMBER FUNCTION: CAnimat::InitializeRun()
 
  DESCRIPTION:
    Initializes animat member variables.  

  ARGUMENTS:
	ANIMAT_RUNPARAMS RunParam - A structure containing this animats initialization and
		runtime parameters.

  MEMBER VARIABLES ALTERED:
	Pretty much all member variables are set some. 

  RETURN VALUE:
	UINT - One of the possible 

  REVISION HISTORY:
     Date    Name   Change
   --------  ----   ------
   05/24/05  (MJC)  Initial Coding/Transfer
   07/20/06  (MJC)  Modified to handle animat being initialized on land.  It will be
                    up the to interfaces to NOT let this happen.  
----------------------------------------------------------------------------------------*/
void CAnimat::DeinitializeRun()
{
	m_state = NULL;
}



void CAnimat::InitializeRun(const USERPARAMS *pUserSce,
							DWORD StartTime,
							CSpeciesModel *pSpeMdl,
							DWORD *UniqueID,
							ANIMATSTATE *pAnimatState,
							double focalDistance,
							SCEPARMSSPECIESGROUP *pSpeGroupParams,
							CBathymetry *pBathymetryRef)
{
	m_state = &pAnimatState[*UniqueID];
	memset(m_state, 0, sizeof(ANIMATSTATE));
	int intVal;
	BEHTRAN *behTran;
	double deepest;
	double shallowest;
	int loopCnt;
	BATHYVALUE bathyVal;

	//_ASSERT(0 == 1);

	m_pRefRandom = pSpeMdl->m_pC3MBRandomRef;


	// member variables
	m_focalCoordSet = FALSE;
	m_tempEnvAttCoordSet = FALSE;
	//m_depthEnvAttCoordSet = FALSE;

	m_state->coord = m_initialCoord = m_seedingCoord;
	m_state->bathyDepth = m_initialBathyDepth;
#pragma message("Need initialization for slope and slope heading")
	m_pSpeciesGroupParams = NULL;
	m_fRiskThreshold = m_pRefRandom->rndreal(0.0, 1.0);


	//-----------------//
	// Passed In Params
	//-----------------//
	m_state->simClock = StartTime;
	m_distCalcMethod = pUserSce->distCalcMethod;
	m_pRefSpeciesModel = pSpeMdl;
	m_speciesDef = &pSpeMdl->m_speciesModel;
	m_bathymetry = pBathymetryRef;
	// Salinity and temperature ignored for now.
	m_state->podFollowFocal.maxFocalDistance = focalDistance;
	m_pSpeciesGroupParams = pSpeGroupParams;

	//--------------------------------------//
	// Bathymetry depth for initial location.
	//--------------------------------------//
	memset(&m_bathySector, 0, sizeof(ENVDATA_INDEX));
	if(m_bathyDepthSet == FALSE)
	{
		m_state->bathyDepth = BATHY_DEFAULT_DEPTH;
		m_state->bathySlope = 0;
		m_state->bathySlopeHeading = 0;

		if(m_bathymetry != NULL && m_bathymetry->IsDataLoaded() == TRUE)
		{
			bathyVal = m_bathymetry->GetValueAtCoordinate(m_initialCoord.lat, m_initialCoord.lon, &m_bathySector);
			m_state->bathyDepth = bathyVal.depth;
			m_state->bathySlope = bathyVal.slope;
			m_state->bathySlopeHeading = bathyVal.slopeHeading;
		}
	}

	if(m_state->bathyDepth > 0)
		m_state->bathyDepth = 0; // keep the animats in the ocean.

	//---------------------//
	// Initialize Behaviors
	//---------------------//
	loopCnt = 0; // a means to prevent infinte loop if all else fails.
	_ASSERT(m_speciesDef->initBehSpanCnt >= 1);
	do
	{
		intVal = m_pRefRandom->rnd(0, m_speciesDef->initBehSpanCnt-1);
		_ASSERT(0 <= intVal && intVal < m_speciesDef->initBehSpanCnt);
		behTran = &m_speciesDef->initialBehavior.arr[intVal];	
	}while((behTran->depthSpan.shallow < m_state->bathyDepth) && loopCnt++ < 1000);
	deepest = behTran->depthSpan.deep;
	shallowest = behTran->depthSpan.shallow;

/*
	// A) An initial behavior is needed to determine initial depth
	// B) Initial behavior depth spans dictate which behaviors are allowed initially at any given depth.
	// therefore,
	// (1) Randomly select an initial depth span.
	// (2) Use that depth span to determine initial behavior.
	// (3) Use that initial behavior to determine an initial depth.
	// (4) If initial depth is beyond depth span try again (start at step 1) up to 100 times.
	_ASSERT(m_speciesDef->initBehSpanCnt >= 1);
	if(m_speciesDef->initBehSpanCnt == 1)
	{
		intVal = 0;
	}
	else
	{
		loopCnt = 0;
		do
		{
			intVal = m_pRefRandom->rnd(0, m_speciesDef->initBehSpanCnt-1);
			_ASSERT(0 <= intVal && intVal < m_speciesDef->initBehSpanCnt);
			behTran = &m_speciesDef->initialBehavior.arr[intVal];
			if(behTran->depthSpan.deep != behTran->depthSpan.shallow)
				break;
		}while(loopCnt < 100);

		if(loopCnt == 100)
			intVal = 0;
	}

	_ASSERT(0 <= intVal && intVal < m_speciesDef->initBehSpanCnt);
	behTran = &m_speciesDef->initialBehavior.arr[intVal];
	deepest = behTran->depthSpan.deep;
	shallowest = behTran->depthSpan.shallow;
*/
	
	// Behavior transition submodel
	intVal = m_pRefSpeciesModel->IntialBehavior(m_state->simClock, &behTran->m);
	_ASSERT((intVal >= 0) && (intVal < 65536 /*Max number a 16-bit unsigned integer can hold*/) && ((UINT32)intVal <m_speciesDef->description.numBehaviors));
	m_state->behState = (UINT16)intVal;


	_ASSERT(m_state->behState >= 0);

	m_state->submdl.behavior.timeLapsed = 0; // this may need to be changed.
	m_state->submdl.behavior.endDice = m_pRefRandom->myrand();
	m_state->behTransActive = FALSE;
	m_state->nextBehState = -1;

	// Animat's initial depth must be shallower than or equal to bathy depth.
	m_state->depth = m_pRefSpeciesModel->DepthModel(&m_speciesDef->p.behavior[m_state->behState].dive.depth);

	if(deepest < m_state->bathyDepth)
		deepest = m_state->bathyDepth;
	if(shallowest < m_state->bathyDepth)
		shallowest = m_state->bathyDepth;
	if(shallowest < deepest)
		shallowest = deepest;

	if(m_state->depth < deepest)
		m_state->depth = deepest;
	if(m_state->depth > shallowest)
		m_state->depth = shallowest;


	// Animat's intial depth must also be shallower than or equal to its maximum seeding
	// depth, if any.
	if(m_speciesDef->description.deepWaterSeedingDepthEnabled == TRUE)
	{
		if(m_state->depth < m_speciesDef->description.deepWaterSeedingDepth)
			m_state->depth = m_speciesDef->description.deepWaterSeedingDepth;
	}

	m_state->submdl.dive.projectedDepth = m_state->depth;


	// Travel rate submodel
	m_state->submdl.travelRate.rate = m_pRefSpeciesModel->RateModel(&m_speciesDef->p.behavior[m_state->behState].travelRate);
	m_state->submdl.travelRate.timeLapsed = 0;
	m_state->submdl.travelRate.endDice = m_pRefRandom->myrand();

	// Bearing
	m_state->submdl.travelDirection.bearing = m_pRefRandom->myrand() * 360;
	m_state->submdl.travelDirection.timeLapsed = 0;
	m_state->submdl.travelDirection.endDice = m_pRefRandom->myrand();


	// Dive Rate
	if(m_pRefRandom->myrand() > 0.5)
	{
		m_state->submdl.dive.activity = ASCENDING;
		m_state->submdl.dive.rate.rate = m_pRefSpeciesModel->RateModel(&m_speciesDef->p.behavior[m_state->behState].dive.ascentRate);
	}
	else
	{
		m_state->submdl.dive.activity = DESCENDING;
		m_state->submdl.dive.rate.rate = m_pRefSpeciesModel->RateModel(&m_speciesDef->p.behavior[m_state->behState].dive.descentRate);
	}
	m_state->submdl.dive.rate.timeLapsed = 0;
	m_state->submdl.dive.rate.endDice = m_pRefRandom->myrand();


	// Dive Depth
	m_state->submdl.dive.targetDepth = m_pRefSpeciesModel->DepthModel(&m_speciesDef->p.behavior[m_state->behState].dive.depth);
	if(m_state->submdl.dive.targetDepth > m_state->depth)
		m_state->submdl.dive.targetDepth = m_state->depth;

	// Beached?
	if(m_state->bathyDepth >= m_speciesDef->acousticAversion.beachingDepth ||
		m_state->bathyDepth >= m_speciesDef->description.shoreFollowDepth)
	{
		m_state->beached = TRUE;
		m_state->submdl.travelRate.rate = 0;
		m_state->submdl.dive.rate.rate = 0;
		m_state->submdl.dive.activity = BEACHED;
	}

	// Set initialization to handle acoustic response and pod following?
	// Merge behaviors???
	m_state->setDiveRate = m_state->submdl.dive.rate.rate;
	m_state->setHeading = m_state->submdl.travelDirection.bearing;
	m_state->setTravelRate = m_state->submdl.travelRate.rate;

	m_uniqueId = *UniqueID;
	*UniqueID = *UniqueID + 1;
}

BOOL CAnimat::CoordinatesOnScreen(const COORDINATE *Crd, ENVMINMAX *EnvMinMax)
{
	if(Crd->lat>EnvMinMax->xMax || Crd->lat<EnvMinMax->xMin || Crd->lon>EnvMinMax->yMax || Crd->lon<EnvMinMax->yMin)
		return FALSE;
	return TRUE;
}

/* Handles animat's that stray off the screen by replacing them with new animats. The
effort to replace strayed animats includes clearing the animat state, determining a proper
location on the screen to place the new animat, and initializing the new animat.  Some
state variables such as those used for generating the take report, need to be preserved in
the animat's state and so are therefor saved to a temporary variable and copied back into
the animat's state after initialization is completed.

The animat state struct member offScreenInf(type OFFSCREEN_INF) is only set here when the animat goes off screen.
*/
void CAnimat::HandleOffScreeen(ANIMATSTATE *State)
{
	State = State; // quiet compiler warning until I finish this up.
#if 0
	OFFSCREEN_INF offScrn = {0};
	COORDINATE tempFocalCoord = {0};

	// If no bathymetry loaded there is nothing to check.  
	if(m_bathymetry->IsDataLoaded() == FALSE)
		return;

	// If animat did not go off screen then there is nothing to do.
	if(CoordinatesOnScreen(&State->coord, &m_bathymetry->GetExtremes()) == TRUE)
		return;

	//----------------------------------------------------------------------------------//
	// Copy or place persitent needed information from the animat state into local
	// variables to copy back into the animat state after it is cleared out.
	//----------------------------------------------------------------------------//
	// Indicate the animat went off screen.
	offScrn.offScreen = TRUE;

	// Save the animat's PREVIOUS lvl A, B phys states, current instantaneous and cumulative
	// acoustic exposure values.
	offScrn.lvlAPhysFlagPrev = State->acstcExp.lvlAPhysFlag; 
	offScrn.lvlBPhysFlagPrev = State->acstcExp.lvlBPhysFlag;
	offScrn.actualSrcInstantValue = State->acstcExp.actualSrcInstantValue;
	offScrn.cumulativeValue = State->acstcExp.cumulativeValue;
	if(State->acstcExp.risk > 0)
	{
		offScrn.lvlBBehFlagPrev = TRUE;
		offScrn.lvlBBehRiskValue = State->acstcExp.risk;
	}

	// Make temporary copies of variable values that will not change.
	tempFocalCoord = State->podFollowFocal.focalCoord;
	tempClock = State->simClock;
	COORDINATE currentCoord = State->coord;
#if 0

	m_focalCoordSet = m_focalCoordSet; // does not change.
	memset(m_state, 0, sizeof(ANIMATSTATE));
	memset(&m_bathySector, 0, sizeof(ENVDATA_INDEX));

	State->simClock = tempClock;

	// Copy the offscreen information into the animat state
	memcpy(OffInf, &offscreen, sizeof(OFFSCREEN_INF));


	// Replace the animat that went off screen.
	double bathyDepth = BATHY_MIN_SEED_DEPTH;


	// Continue on the other side of the screen
	State->coord = m_staticLib.ContinueOtherSideOfScreenLatLon(currentCoord, envMinMax);
	if(TRUE == m_staticLib.LatLonWithinBoundaries(State->coord.lat, State->coord.lon, envMinMax))
		bathyDepth = m_bathymetry->GetValueAtCoordinate(State->coord.lat, State->coord.lon, &m_bathySector);

	// In case the bathymetry on the other side is land...
	while(bathyDepth >= BATHY_MIN_SEED_DEPTH || bathyDepth >= m_speciesDef->description.shoreFollowDepth
				|| bathyDepth >= m_speciesDef->description.minSeedingDepth)
	{
		State->coord = m_staticLib.RandomLatLon(envMinMaxenvMinMax, m_pRefRandom);
		bathyDepth = m_bathymetry->GetValueAtCoordinate(State->coord.lat, State->coord.lon, &m_bathySector);
	}

	m_initialCoord = State->coord;
	State->bathyDepth = bathyDepth;


	////////////////////////////////////////////////////////////////
	//---------------------//
	// Initialize Behaviors
	//---------------------//
	// Behavior transition submodel
	intVal = m_pRefSpeciesModel->IntialBehavior(State->simClock, &m_speciesDef->initialBehavior);
	_ASSERT((intVal >= 0) && (intVal < 65536 /*Max number a 16-bit unsigned integer can hold*/));
	State->behState = (UINT16)intVal;
	_ASSERT(State->behState < m_speciesDef->description.numBehaviors && State->behState >= 0);
	State->submdl.behavior.timeLapsed = 0;
	State->submdl.behavior.endDice = m_pRefRandom->myrand();

	// Depth must be shallower than or equal to bathy depth.
	State->depth = m_pRefSpeciesModel->DepthModel(&m_speciesDef->p.behavior[State->behState].dive.depth);
	if(State->depth < State->bathyDepth)
		State->depth = State->bathyDepth;
	State->submdl.dive.projectedDepth = State->depth;

	// Travel rate submodel
//		State->submdl.travelRate.rate = m_pRefSpeciesModel->RateModel(&m_speciesDef->p.behavior[State->behState].travelRate);
//		State->submdl.travelRate.timeLapsed = 0;
//		State->submdl.travelRate.endDice = m_pRefRandom->myrand();

	// Bearing
//		State->submdl.travelDirection.bearing = m_pRefRandom->myrand() * 360;
//		State->submdl.travelDirection.timeLapsed = 0;
//		State->submdl.travelDirection.endDice = m_pRefRandom->myrand();

	// Dive Rate
	if(m_pRefRandom->myrand() > 0.5)
	{
		State->submdl.dive.activity = ASCENDING;
		State->submdl.dive.rate.rate = m_pRefSpeciesModel->RateModel(&m_speciesDef->p.behavior[State->behState].dive.ascentRate);
	}
	else
	{
		State->submdl.dive.activity = DESCENDING;
		State->submdl.dive.rate.rate = m_pRefSpeciesModel->RateModel(&m_speciesDef->p.behavior[State->behState].dive.descentRate);
	}
	State->submdl.dive.rate.timeLapsed = 0;
	State->submdl.dive.rate.endDice = m_pRefRandom->myrand();

	// Dive Depth
	State->submdl.dive.targetDepth = m_pRefSpeciesModel->DepthModel(&m_speciesDef->p.behavior[State->behState].dive.depth);
	if(State->submdl.dive.targetDepth > State->depth)
		State->submdl.dive.targetDepth = State->depth;

	// Beached?
	if(State->bathyDepth >= m_speciesDef->acousticAversion.beachingDepth || State->bathyDepth >= m_speciesDef->description.shoreFollowDepth)
	{
		State->beached = TRUE;
		State->submdl.travelRate.rate = 0;
		State->submdl.dive.rate.rate = 0;
		State->submdl.dive.activity = BEACHED;
	}

	// Set initialization to handle acoustic response and pod following?
	// Merge behaviors???
	State->setDiveRate = State->submdl.dive.rate.rate;
	State->setHeading = State->submdl.travelDirection.bearing;
	State->setTravelRate = State->submdl.travelRate.rate;
	///////////////////////OFF Screen ////////////////////////////////////
#endif
#endif
}


/*****************************************************************************************
* MEMBER FUNCTION:  CAnimat::Update()
* 
* DESCRIPTION:
*	Part of the CAnimat Simulation Routines.
*	Iterates the simulation one second of simulation time.
*
* ARGUMENTS: None
*
* RETURN VALUE: None.
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   05/24/05  (MJC)  Initial Coding/Transfer
*****************************************************************************************/
#define IMMEDIATETRANSITION 0

#ifndef _DEBUG
#undef IMMEDIATETRANSITION
#define IMMEDIATETRANSITION 0
#endif

void CAnimat::Update(DWORD *pAnimatNumber)
{
	COORDINATE tempFocalCoord = {0};
	int tempClock = 0;
	OFFSCREEN_INF offscreen = {0};
	int intVal;
	BATHYEXTREMES envMinMax = m_bathymetry->GetExtremes();
	MATRIX *nrmlBehTransMatrix;
	GAUSS gauss;
	int timeIndex;
	double bathyDepth;
	BATHYVALUE bathyValue;
	BOOL directionModelUpdated; // Direction model terminated then started a new direction
	BOOL travelRateModelUpdated; // Direction model terminated then started a new direction


	BEHTRAN *behTran;
	double deepest;
	double shallowest;
	int loopCnt;

//	BOOL immediateTransitionEnabled = FALSE;
//	if(IMMEDIATETRANSITION == 1)
//		immediateTransitionEnabled = TRUE;

	*pAnimatNumber = m_uniqueId;

	//----------------------//
	// 1. Advance the animat
	//----------------------//
	// Update the state clock.
	m_state->simClock++;

	// Calculate animat position, depth, and environmental values at its location based on
	// setpoints from previous iteration (rates, heading, dive activities, etc..)  Actual
	// depth and projected depth will diverge if animat is a bottom follower or animat
	// hits the bottom.

	if(m_state->beached == TRUE)
	{
		_ASSERT(m_state->submdl.dive.activity == BEACHED);
		return;  // Once beached, always beached.  Do nothing else with this animat.
	}

	m_state->coord = CalcNewCoord(m_initialCoord, m_state->coord, m_state->setHeading,  m_state->setTravelRate,
									&m_state->deltaXY, m_distCalcMethod);
	m_currentCoord = m_state->coord;




	// OFF SCREEN 
	// ************* Handle Off Map Animats *********************************************//
	// Determine if the animat went off screen
	double tempTravelRate;
	double tempTravelHeading;
	HandleOffScreeen(m_state);
#if 1
	memset(&m_state->offScreenInf, 0, sizeof(OFFSCREEN_INF));
	if(m_bathymetry->IsDataLoaded() == TRUE && (m_state->coord.lat > envMinMax.xMax || m_state->coord.lat < envMinMax.xMin ||
		m_state->coord.lon > envMinMax.yMax || m_state->coord.lon < envMinMax.yMin))
	{
		// The animat went off screen and is to be reset.  Copy its last acoustic
		// exposure information into the state so it may be accounted for in the
		// acoustic exposure statical analysis.

		// Assign a new risk threshold
		m_fRiskThreshold = m_pRefRandom->rndreal(0.0, 1.0);

		offscreen.offScreen = TRUE; // Indicates the animat went off map

		// Save its current lvl A, B phys states
		offscreen.lvlAPhysFlagPrev = m_state->acstcExp.lvlAPhysFlag; 
		offscreen.lvlBPhysFlagPrev = m_state->acstcExp.lvlBPhysFlag;


		offscreen.lvlBBehFlagPrev = TRUE;
		offscreen.lvlBBehRiskValue = m_state->acstcExp.risk;

		// Save the instant and cumulative values for..?
		offscreen.actualSrcInstantValue = m_state->acstcExp.actualSrcInstantValue;
		offscreen.cumulativeValue = m_state->acstcExp.cumulativeValue;


		// Make temporary copies of variable values that will not change.
		if(m_focalCoordSet == TRUE)
			tempFocalCoord = m_state->podFollowFocal.focalCoord;
		tempClock = m_state->simClock;
		COORDINATE currentCoord = m_state->coord;

		m_focalCoordSet = m_focalCoordSet; // does not change.
		tempTravelRate = m_state->setTravelRate;
		tempTravelHeading = m_state->setHeading;

		memset(m_state, 0, sizeof(ANIMATSTATE));
		memset(&m_bathySector, 0, sizeof(ENVDATA_INDEX));

		m_state->simClock = tempClock;

		// Copy the offscreen information into the animat state
		memcpy(&m_state->offScreenInf, &offscreen, sizeof(OFFSCREEN_INF));


		// Replace the animat that went off screen.
		bathyDepth = BATHY_MIN_SEED_DEPTH;

		// Continue on the other side of the screen
		m_state->coord = m_staticLib.ContinueOtherSideOfScreenLatLon(currentCoord, envMinMax);
		if(TRUE == m_staticLib.LatLonWithinBoundaries(m_state->coord.lat, m_state->coord.lon, envMinMax))
		{
			bathyValue = m_bathymetry->GetValueAtCoordinate(m_state->coord.lat, m_state->coord.lon, &m_bathySector);
			bathyDepth = bathyValue.depth;
		}

		// In case the bathymetry on the other side is land...
		while(bathyDepth >= BATHY_MIN_SEED_DEPTH || bathyDepth >= m_speciesDef->description.shoreFollowDepth
					|| bathyDepth >= m_speciesDef->description.minSeedingDepth)
		{
			m_state->coord = m_staticLib.RandomLatLon(envMinMax, m_pRefRandom);
			bathyValue = m_bathymetry->GetValueAtCoordinate(m_state->coord.lat, m_state->coord.lon, &m_bathySector);
			bathyDepth = bathyValue.depth;
		}

		m_initialCoord = m_state->coord;
		m_state->bathyDepth = bathyDepth;

		if(m_state->bathyDepth > 0)
			m_state->bathyDepth = 0; // keep the animats in the ocean.


		////////////////////////////////////////////////////////////////
		//-----------------------//
		// Reinitialize Behaviors
		//-----------------------//
		loopCnt = 0; // a means to prevent infinte loops.
		_ASSERT(m_speciesDef->initBehSpanCnt >= 1);
		do
		{
			intVal = m_pRefRandom->rnd(0, m_speciesDef->initBehSpanCnt-1);
			_ASSERT(0 <= intVal && intVal < m_speciesDef->initBehSpanCnt);
			behTran = &m_speciesDef->initialBehavior.arr[intVal];
		}while(behTran->depthSpan.shallow < m_state->bathyDepth && loopCnt++ < 1000);
		deepest = behTran->depthSpan.deep;
		shallowest = behTran->depthSpan.shallow;
		intVal = m_pRefSpeciesModel->IntialBehavior(m_state->simClock, &behTran->m);
		_ASSERT((intVal >= 0) && (intVal < 65536 /*Max number a 16-bit unsigned integer can hold*/) && ((UINT32)intVal <m_speciesDef->description.numBehaviors));
		m_state->behState = (UINT16)intVal;

		_ASSERT(m_state->behState < m_speciesDef->description.numBehaviors && m_state->behState >= 0);
		m_state->submdl.behavior.timeLapsed = 0;
		m_state->submdl.behavior.endDice = m_pRefRandom->myrand();

		// Depth must be shallower than or equal to bathy depth.
		m_state->depth = m_pRefSpeciesModel->DepthModel(&m_speciesDef->p.behavior[m_state->behState].dive.depth);

		if(deepest < m_state->bathyDepth)
			deepest = m_state->bathyDepth;
		if(shallowest < m_state->bathyDepth)
			shallowest = m_state->bathyDepth;
		if(shallowest < deepest)
			shallowest = deepest;


		if(m_state->depth < deepest)
			m_state->depth = deepest;
		if(m_state->depth > shallowest)
			m_state->depth = shallowest;

		// Animat's intial depth must also be shallower than or equal to its maximum seeding
		// depth, if any.
		if(m_speciesDef->description.deepWaterSeedingDepthEnabled == TRUE)
		{
			if(m_state->depth < m_speciesDef->description.deepWaterSeedingDepth)
				m_state->depth = m_speciesDef->description.deepWaterSeedingDepth;
		}

		m_state->submdl.dive.projectedDepth = m_state->depth;


		// Travel rate submodel
		m_state->submdl.travelRate.rate = tempTravelRate; //m_pRefSpeciesModel->RateModel(&m_speciesDef->p.behavior[m_state->behState].travelRate);
		m_state->submdl.travelRate.timeLapsed = 0;
		m_state->submdl.travelRate.endDice = m_pRefRandom->myrand();

		// Bearing
		m_state->submdl.travelDirection.bearing = tempTravelHeading;
		m_state->submdl.travelDirection.timeLapsed = 0;
		m_state->submdl.travelDirection.endDice = m_pRefRandom->myrand();

		// Dive Rate
		if(m_pRefRandom->myrand() > 0.5)
		{
			m_state->submdl.dive.activity = ASCENDING;
			m_state->submdl.dive.rate.rate = m_pRefSpeciesModel->RateModel(&m_speciesDef->p.behavior[m_state->behState].dive.ascentRate);
		}
		else
		{
			m_state->submdl.dive.activity = DESCENDING;
			m_state->submdl.dive.rate.rate = m_pRefSpeciesModel->RateModel(&m_speciesDef->p.behavior[m_state->behState].dive.descentRate);
		}
		m_state->submdl.dive.rate.timeLapsed = 0;
		m_state->submdl.dive.rate.endDice = m_pRefRandom->myrand();

		// Dive Depth
		m_state->submdl.dive.targetDepth = m_pRefSpeciesModel->DepthModel(&m_speciesDef->p.behavior[m_state->behState].dive.depth);
		if(m_state->submdl.dive.targetDepth > m_state->depth)
			m_state->submdl.dive.targetDepth = m_state->depth;

		// Beached?
		if(m_state->bathyDepth >= m_speciesDef->acousticAversion.beachingDepth || m_state->bathyDepth >= m_speciesDef->description.shoreFollowDepth)
		{
			m_state->beached = TRUE;
			m_state->submdl.travelRate.rate = 0;
			m_state->submdl.dive.rate.rate = 0;
			m_state->submdl.dive.activity = BEACHED;
		}

		// Set initialization to handle acoustic response and pod following?
		// Merge behaviors???
		m_state->setDiveRate = m_state->submdl.dive.rate.rate;
		m_state->setHeading = m_state->submdl.travelDirection.bearing;
		m_state->setTravelRate = m_state->submdl.travelRate.rate;
		///////////////////////OFF Screen ////////////////////////////////////
	}
#endif

	m_state->depth = DepthCalc(m_state->setDiveRate, m_state->depth, m_state->submdl.dive.activity);
	m_state->submdl.dive.projectedDepth = DepthCalc(m_state->setDiveRate, m_state->submdl.dive.projectedDepth, m_state->submdl.dive.activity);

	if(m_bathyDepthSet == FALSE)
	{
		bathyValue = m_bathymetry->GetValueAtCoordinate(m_state->coord.lat, m_state->coord.lon, &m_bathySector);
		m_state->bathyDepth = bathyValue.depth;
		m_state->bathySlope = bathyValue.slope;
		m_state->bathySlopeHeading = bathyValue.slopeHeading;

	}

	// Enforce animat depth values
	EnforceDepthValueRules();

	// Check for Beaching
	if(m_state->bathyDepth >= m_speciesDef->acousticAversion.beachingDepth ||
		m_state->bathyDepth >= m_speciesDef->description.shoreFollowDepth)
	{
		m_state->beached = TRUE;
		m_state->setDiveRate = 0;
		m_state->setTravelRate = 0;
		m_state->submdl.travelRate.rate = 0;
		m_state->submdl.dive.rate.rate = 0;
		m_state->submdl.dive.activity = BEACHED;
		m_state->shoreFollow.isActive = FALSE;
		m_state->acstcExp.isActive = FALSE;
		m_state->podFollowFocal.isActive = FALSE;
		m_state->depthEnvAtt.isActive = FALSE;
		m_state->tempEnvAtt.isActive = FALSE;
		//memset(&m_state->acstcExp, 0, sizeof(ACSTCAVRSN_STATE));
		return;
	}


	// Restore to previous modeled values if any modifiers temporarily changed the state values.
	m_state->setDiveRate = m_state->submdl.dive.rate.rate;
	m_state->setHeading = m_state->submdl.travelDirection.bearing;
	m_state->setTravelRate = m_state->submdl.travelRate.rate;

	//------------------------------------------//
	// 2. Increment time lapses
	//------------------------------------------//
	// Normal behavior time lapses
	m_state->submdl.behavior.timeLapsed++;
	m_state->submdl.travelRate.timeLapsed++;
	m_state->submdl.travelDirection.timeLapsed++;
	m_state->submdl.dive.rate.timeLapsed++;
	m_state->submdl.dive.reversal.timeLapsed++;
	m_state->submdl.dive.surfInterval.timeLapsed++;

	// Acoustic aversion time lapses.
	if(m_state->acstcExp.isActive == TRUE)
		m_state->acstcExp.timeLapsed++;

	// Pod following
	if(m_state->podFollowFocal.isActive == TRUE)
		m_state->podFollowFocal.timeLapsed++;

	// Depth environmental attractors
	if(m_state->depthEnvAtt.isActive == TRUE)
		m_state->depthEnvAtt.timeLapsed++;

	// Temperature environmental attractors
	if(m_state->tempEnvAtt.isActive == TRUE)
		m_state->tempEnvAtt.timeLapsed++;

	// Shore following has no time lapse.

	//----------------------------------------------------------------------------------//
	// 3. Determine which stimuli-activated behaviors have activated due to the animat's
	//    new location and environmental data.
	//----------------------------------------------------------------------------------//
	// Determine which, if any, of the three depthenvironmental attractors has been
	// activated and the desired heading.  Desired head is a function of the slope's
	// heading.
	RunEnvAttrctrResponse(&m_speciesDef->p.behavior[m_state->behState].depthEnvAtt,
		&m_state->depthEnvAtt, m_state);

	RunEnvAttrctrResponse(&m_tempEnvAttCoordSet, &m_speciesDef->p.behavior[m_state->behState].tempEnvAtt,
		&m_state->tempEnvAtt);
	RunPodFollowingResponse();
	RunAcousticResponse();

	//-------------------------------------------------//
	// 4. Determine the animat's current behavior state
	//-------------------------------------------------//
	// Changed 1/21/10 pending discussion with Dorian.
	// Determine if time for a behavior transition and what the next behavior is if so.
//	if(m_state->submdl.dive.surfInterval.timeLapsed >= m_state->submdl.dive.surfInterval.setDuration && m_state->depth==0)
//		BehaviorTransition(); // moved to dive
	_ASSERT(m_speciesDef->p.behavior[m_state->behState].nrmlBehTransCnt >= 1);

	/* Only check for behavior transition if there are multiple behaviors and a next
	 * behavior has not been determined. */
	if(m_speciesDef->description.numBehaviors > 1)
		BehaviorTransition();

	/* Actual behavior transitions occur only at the end of a surface interval the animat
	 * is at the surface, and a next behavior has been determined..*/
	if(m_state->submdl.dive.surfInterval.timeLapsed >= m_state->submdl.dive.surfInterval.setDuration &&
		m_state->submdl.dive.activity == IN_SURFACE_INTERVAL && 
		m_state->behTransActive == TRUE)
	{
		_ASSERT(m_state->nextBehState >= 0);
		m_state->behState = (UINT16)m_state->nextBehState;
		_ASSERT(m_state->behState < m_speciesDef->description.numBehaviors && m_state->behState >= 0);

		// Maintained for both behavior termination formulea
		m_state->submdl.behavior.timeLapsed = 0;
#if 1
		if(m_speciesDef->p.behavior[m_state->behState].nrmlBehTermType == T50_K_TERM)
		{
			m_state->submdl.behavior.endDice = m_pRefRandom->myrand();
			m_state->submdl.behavior.timeRemaining = 0;
		}
		else //ModelType == GAUSSIAN_TERM
		{
			nrmlBehTransMatrix = GetCurrentBehaviorTransitionMatrix(&timeIndex);
			// Multiply the modeled Gaussian mean and standard values by 60 because they
			// are in minutes but calculations during scenario execution are in seconds.
			gauss.mean = nrmlBehTransMatrix->p.ppa[timeIndex][nrmlBehTransMatrix->colCnt-2]*60.0;
			gauss.std = nrmlBehTransMatrix->p.ppa[timeIndex][nrmlBehTransMatrix->colCnt-1]*60.0;
			m_state->submdl.behavior.timeRemaining = (int)m_pRefSpeciesModel->RateModel(&gauss);
			m_state->submdl.behavior.endDice = 0;
		}
#endif
		m_state->behTransActive = FALSE;
		m_state->nextBehState = -1;
	}


	//-----------------------------------------------------------------------------//
	// 5. Assemble the governing models for animat movement (rate, heading, etc...).
	//-----------------------------------------------------------------------------//
	// Initially set the governing models to to normal behaviors.
	RATEMDL trvlRateMdl = m_speciesDef->p.behavior[m_state->behState].travelRate;
	DIRCTNMDL travelDirMdl = m_speciesDef->p.behavior[m_state->behState].travelDirection;;
	DIVEMDL diveMdl = m_speciesDef->p.behavior[m_state->behState].dive;;

	// Bottom Following will override animat travel rate.
	if(m_state->submdl.dive.activity == BOTTOM_FOLLOWING && diveMdl.bttmFollow.type == BOTTOMFOLLOWS_REPLACEMENT_MDL)
		trvlRateMdl = diveMdl.bttmFollow.rateMdl;

	// Acoustic aversion, if active, may modify cetain submodels and impact transition.
	AcousticAversionAdjustModelAndState(&trvlRateMdl, &travelDirMdl, &diveMdl);

	// Other active stimulus-based behaviors only impact transition.  Shore following
	// doesn't.
	PodFollowingAdjustState(&travelDirMdl);
	EnvAttractorAdjustState(&travelDirMdl, &m_state->tempEnvAtt);
	//EnvAttractorAdjustState(&travelDirMdl, &m_state->depthEnvAtt);



	//----------------------------------------------------------------------------------//
	// 6. Set the direction of bias on the CRWDB model in the governing direction model by
	//    merging the disired headings of the various stimulus-based behaviors (except
	//    shore following).
	//----------------------------------------------------------------------------------//
	// Note 1: If none of the stimulus-based behaviors have a desired heading then the 
	// direction of bias won't change.
	// Note 2: If the model is normally not CRWDB then setting this value won't impact
	// modeling unless one or more of the stimulus-based behaviors are
	// currently active and modified, temporarily, the travel model to CRWDB.
	travelDirMdl.crRndWalkDb.directionOfBias = MergeDesiredHeadings();

	//----------------------------------------------------------//
	// 7. Run the behaviors using the assembed governing models.
	//----------------------------------------------------------//
	// These will set the behavior's settings for rates, direction, dive rate, and so on
	// into the animat's state.
	if(TRUE == RunTravelRateModel(&trvlRateMdl, &m_state->submdl.travelRate)) // determine (for next iteration) travel rate
		m_state->setTravelRate = m_state->submdl.travelRate.rate;

	directionModelUpdated = RunDirectionModel(&travelDirMdl, &m_state->submdl.travelDirection);

	// Don't try setting m_state->setHeading or m_state->setHeading in this function because
	// they get set in HandleNearbyShore() at the end of this function.


	// Determine (for next iteration) dive states.  If TRUE is returned it means either
	// the animat has started or ended flat bottom diving.
	if(TRUE == RunDiveModel(&diveMdl))
	{
		if(m_state->submdl.dive.activity == BOTTOM_FOLLOWING)
		{
			// The animat has begun bottom following (flat bottom diving).  Determine
			// a new travel rate using teh bottom following travel rate model.
			m_state->submdl.travelRate.rate = m_pRefSpeciesModel->RateModel(&diveMdl.bttmFollow.rateMdl);
		}
		else
		{ 
			// The animat ceased begun bottom following.  Determine a new travel rate
			// using the normal travel rate model.
			m_state->submdl.travelRate.rate =
				m_pRefSpeciesModel->RateModel(&m_speciesDef->p.behavior[m_state->behState].travelRate);
		}
		m_state->submdl.travelRate.timeLapsed = 0;
		m_state->submdl.travelRate.endDice = m_pRefRandom->myrand();
	}



	//----------------------------------------------------------------------------------//
	// 8. Finalize setpoints dive rate, travel rate, and travel direction.  Function
	//    HandleNearbyShore() adjusts the final setpoints for heading and travel rate if
	//    necessary to avoid the shoreline.  Dive rate is set equal to the dive rate
	//    found in function RunDiveModel() above.
	//----------------------------------------------------------------------------------//

	// HandleNearbyShore() determines and ultimately sets the animat's final travel rate
	// (m_state->setTravelRate) and travel direction (m_state->setHeading).
	HandleDepthEnvironmentalAttractor();
	HandleNearbyShore(&trvlRateMdl, &travelDirMdl); 
	m_state->setDiveRate = m_state->submdl.dive.rate.rate;

	// Do not set these here because they are set in HandleNearbyShore().
	//		m_state->setTravelRate = m_state->submdl.travelRate.rate;
	//		m_state->setHeading = m_state->submdl.travelDirection.bearing;

	m_bathyDepthSet = FALSE;
}

// Input:RecievedLeveldB = Recieved Level (RL) in dB provided by 3mb
double CAnimat::CalculateRisk(SCEPARMSSPECIESGROUP *pSpeciesGroupParams, double RecievedLeveldB)
{
	double L = RecievedLeveldB; // Recieved Level (RL) in dB 
	double B = 120; // basement RL in dB; (set to 120, as per Document provided by Dorian)
	double K = 45;	// the RL increment about basement in dB a which there is a 50 percent
					// risk (set to 45, as per Dorian)
	double A = pSpeciesGroupParams->lvlBBeh_RiskA; // risk transition sharpness parameter

	double intrmedVal;
	double numerator;
	double denominator;
	double retVal;

	intrmedVal = (L-B)/K;

	// Numerator
	numerator = 1 - pow(intrmedVal, -A);

	// Denominator
	denominator = 1 - pow(intrmedVal, -2*A);

	if(denominator != 0)
		retVal = numerator/denominator;
	else
		return 0;

	return retVal;

//	return (1-pow(intrmedVal, -A))/(1-pow(intrmedVal, -2*A));
}

void CAnimat::SetFocalCoordinate(COORDINATE *FocalCoord)
{
	m_focalCoordSet = TRUE;
	m_state->podFollowFocal.focalCoord.lat = FocalCoord->lat;
	m_state->podFollowFocal.focalCoord.lon = FocalCoord->lon;

	///////////////////////
	// Testing and debug //
	//m_focalCoordSet = FALSE; //
	// This cased the animat to behave normally, no pod following repsponse (this is expected).
	///////////////////////////
}
void CAnimat::SetTempEnvAttractor(double Lat, double Lon, double Value)
{
	m_tempEnvAttCoordSet = TRUE;
	m_state->tempEnvAtt.coordTo.lat = Lat;
	m_state->tempEnvAtt.coordTo.lon = Lon;
	m_state->tempEnvAtt.value = Value;
}
/*
void CAnimat::SetDepthEnvAttractor(double Lat, double Lon, double Value)
{
	m_depthEnvAttCoordSet = TRUE;
	m_state->depthEnvAtt.coordTo.lat = Lat;
	m_state->depthEnvAtt.coordTo.lon = Lon;
	m_state->depthEnvAtt.value = Value;
}
*/
void CAnimat::SetAcoustics(double Lat, double Lon, double Value)
{
	m_state->acstcExp.isSet = TRUE;
	m_state->acstcExp.actualSrcCoord.lat = Lat;
	m_state->acstcExp.actualSrcCoord.lon = Lon;
	m_state->acstcExp.actualSrcInstantValue = Value;


	// Calculate the cumulative.
	if(m_state->acstcExp.cumulativeValue == 0)
		 m_state->acstcExp.cumulativeValue = Value;
	else
		m_state->acstcExp.cumulativeValue = 10*log10(pow(10, m_state->acstcExp.cumulativeValue/10) + pow(10, Value/10));
}

/* 10-22-09
Added going into aversion if risk exceeds risk threshold.
*/
void CAnimat::RunAcousticResponse()
{
	DISTANGL da;

	// Local reference copies (to make lines of code shorter and easier to read).
	//ACSTCAVRSN_STATE *state = &m_state->acstcExp;
	m_state->acstcExp.lvlAPhysFlag = m_state->acstcExp.lvlBPhysFlag = FALSE;
	double /*maxAE*/ risk;

	_ASSERTE(m_pSpeciesGroupParams != NULL);

	// Once active, acoustic aversion is always active.  If not active and not set
	// return.  Nothing to do here.
	if(m_state->acstcExp.isActive == FALSE && m_state->acstcExp.isSet == FALSE)
	{
		m_state->acstcExp.actualSrcInstantValue = 0;
		return;
	}

	m_state->acstcExp.isSet = FALSE;

	// Either acoustic response is active or the acoustic exposure was set for this
	// iteration.

	// Determine which is greater: cumulative or instantaneous.
//	maxAE = m_state->acstcExp.cumulativeValue;
//	if(maxAE < m_state->acstcExp.actualSrcInstantValue)
//		maxAE = m_state->acstcExp.actualSrcInstantValue;

	if(m_state->acstcExp.cumulativeValue >= m_pSpeciesGroupParams->lvlAphys && m_pSpeciesGroupParams->lvlAphys != 0)
	{
		// Check if acoustic threshold A physiological exceeded.
		m_state->acstcExp.lvlAPhysFlagCnt++;
		m_state->acstcExp.lvlAPhysFlag = TRUE;
		m_state->acstcExp.isActive = TRUE;
	}
	else if(m_state->acstcExp.cumulativeValue >= m_pSpeciesGroupParams->lvlBphys && m_pSpeciesGroupParams->lvlBphys != 0)
	{
		// Check if acoustic threshold B physiological exceeded.
		m_state->acstcExp.lvlBPhysFlagCnt++;
		m_state->acstcExp.lvlBPhysFlag = TRUE;
		m_state->acstcExp.isActive = TRUE;
	}
	else if(m_pSpeciesGroupParams->unitsBeh == IMASPECIALGROUP)
	{
		if(m_state->acstcExp.actualSrcInstantValue >= m_pSpeciesGroupParams->lvlBBeh_RiskA && m_pSpeciesGroupParams->lvlBBeh_RiskA != 0)
		{
			//if(m_state->acstcExp.maxInstant < m_state->acstcExp.actualSrcInstantValue)
			//	m_state->acstcExp.maxInstant = m_state->acstcExp.actualSrcInstantValue;
			m_state->acstcExp.lvlBBehFlagCnt++;
			m_state->acstcExp.lvlBBehFlag = TRUE;
			m_state->acstcExp.isActive = TRUE;
		}
	}
	// Determine current Risk value
	else if(m_state->acstcExp.actualSrcInstantValue >= 120 && m_state->acstcExp.actualSrcInstantValue > m_state->acstcExp.maxInstant)
	{
		//m_state->acstcExp.maxInstant = m_state->acstcExp.actualSrcInstantValue;
		risk = CalculateRisk(m_pSpeciesGroupParams, m_state->acstcExp.actualSrcInstantValue);
		if(m_state->acstcExp.risk < risk)
			m_state->acstcExp.risk = risk;

		if(risk >= m_fRiskThreshold)
			m_state->acstcExp.isActive = TRUE;
	}


	if(m_state->acstcExp.maxInstant < m_state->acstcExp.actualSrcInstantValue)
		m_state->acstcExp.maxInstant = m_state->acstcExp.actualSrcInstantValue;

	// Determine the angle of the acoustic source relative to the animat
	// Previoiusly this was performed only if acoustic aversion became active.
	// This is changed to account for levl b behavioral Risk which is calculated but
	// doesn't have the animat go into acoustic response.
	da = m_staticLib.DetermineBearing(m_state->coord.lat, m_state->coord.lon, m_state->acstcExp.actualSrcCoord.lat, m_state->acstcExp.actualSrcCoord.lon);
	m_state->acstcExp.responseSrcAngle = da.angle;


	// If not activated, return;
	if(m_state->acstcExp.isActive == FALSE)
		return;

	// Determine response heading.
	m_state->acstcExp.desiredHeading = m_staticLib.KeepWithin360(m_state->acstcExp.responseSrcAngle + 180);
}


void CAnimat::RunPodFollowingResponse()
{
	DISTANGL da;// = {0};

	m_state->podFollowFocal.isActive = FALSE;// explicitly stating...

	if(m_focalCoordSet == FALSE)
	{
		m_state->podFollowFocal.timeLapsed = 0;
		return;
	}

	// Determine focal bearing and distance.  If distance is greater than focal
	// distance, set pod following active.
	da = m_staticLib.DetermineBearing(m_state->coord, m_state->podFollowFocal.focalCoord);

	// Debugging
	//da.angle = 0; // Animat does do crwdb in the forced angle.
	//

	m_state->podFollowFocal.desiredHeading = da.angle;
	m_state->podFollowFocal.distanceTo = da.distance;

	// Commenting these out made the animat behave normally not under pod following influence, as expected.
	if(da.distance > m_state->podFollowFocal.maxFocalDistance)
		m_state->podFollowFocal.isActive = TRUE;
	else
		m_state->podFollowFocal.timeLapsed = 0;

	m_focalCoordSet = FALSE;
}

// Needs work when more is known, especially delta considerations.
void CAnimat::RunEnvAttrctrResponse(DEPTH_ENV_ATTRACTOR_MDL *Mdl, ENVATTRACTORSTATE *pEnvAttractorStateRef,
									const ANIMATSTATE *pRefAnimatState)
{
	DISTANGL da;
	pEnvAttractorStateRef->isActive = FALSE;// explicitly stating...

	BATHYCOMPARE bathyShelfCmp;
	BATHYCOMPARE bathyBasinCmp;
	BATHYCOMPARE bathySlopeCmp;

	BOOL overShelf;
	BOOL overBasin;
	BOOL overSlope;
	
	//_ASSERT(0);


	_ASSERT( // Only one environmental attractor may be enbabled.
		(Mdl->shelfIsEnabled == FALSE && Mdl->basinIsEnabled == FALSE && Mdl->slopeIsEnabled == FALSE) ||
		(Mdl->shelfIsEnabled == TRUE && Mdl->basinIsEnabled == FALSE && Mdl->slopeIsEnabled == FALSE) ||
		(Mdl->shelfIsEnabled == FALSE && Mdl->basinIsEnabled == TRUE && Mdl->slopeIsEnabled == FALSE) ||
		(Mdl->shelfIsEnabled == FALSE && Mdl->basinIsEnabled == FALSE && Mdl->slopeIsEnabled == TRUE));


	if(Mdl->shelfIsEnabled == FALSE && Mdl->basinIsEnabled == FALSE && Mdl->slopeIsEnabled == FALSE)
	{
		memset(pEnvAttractorStateRef, 0, sizeof(ENVATTRACTORSTATE));
		return;
	}


	// Check which, if any, of the environmental attactors activate
	// Important to remember that depth values below sea level are negative and those above are postive.
	bathyShelfCmp = m_staticLib.CompareBathyDepth(pRefAnimatState->bathyDepth, Mdl->shelfDepth);
	overShelf = (bathyShelfCmp == SHALLOWER || bathyShelfCmp == EQUAL);
	overShelf &= (pRefAnimatState->bathySlope < Mdl->shelfSlope);

	bathyBasinCmp = m_staticLib.CompareBathyDepth(pRefAnimatState->bathyDepth, Mdl->basinDepth);
	overBasin = (bathyBasinCmp == DEEPER);
	overBasin &= (pRefAnimatState->bathySlope < Mdl->basinSlope);

	bathySlopeCmp = m_staticLib.CompareBathyDepth(pRefAnimatState->bathyDepth, Mdl->slopeDepth);
	overSlope = (bathySlopeCmp == DEEPER);
	overSlope &= (pRefAnimatState->bathySlope > Mdl->slopeSlope);

	_ASSERT(
		(!overShelf && !overBasin && !overSlope) ||
		( overShelf && !overBasin && !overSlope) ||
		(!overShelf &&  overBasin && !overSlope) ||
		(!overShelf && !overBasin &&  overSlope));


	if(Mdl->shelfIsEnabled == TRUE)
	{
		if(overBasin || overSlope)
		{
			pEnvAttractorStateRef->desiredHeading = pRefAnimatState->bathySlopeHeading + 180.0;// Stimuli's angle.
			pEnvAttractorStateRef->value = pRefAnimatState->setTravelRate; // Stimuli's magnitude
			pEnvAttractorStateRef->timeLapsed++;
			pEnvAttractorStateRef->isActive = TRUE;
		}
		else
		{
			memset(pEnvAttractorStateRef, 0, sizeof(ENVATTRACTORSTATE));
		}
		return;
	}
	else if(Mdl->basinIsEnabled == TRUE)
	{
		if(overShelf || overSlope)
		{
			pEnvAttractorStateRef->desiredHeading = pRefAnimatState->bathySlopeHeading; // Stimuli's angle.
			pEnvAttractorStateRef->value = pRefAnimatState->setTravelRate; // Stimuli's magnitude
			pEnvAttractorStateRef->timeLapsed++;
			pEnvAttractorStateRef->isActive = TRUE;
		}
		else
		{
			memset(pEnvAttractorStateRef, 0, sizeof(ENVATTRACTORSTATE));
		}
	}
	else if(Mdl->slopeIsEnabled == TRUE)
	{
		if(!overShelf && !overBasin)
		{
			memset(pEnvAttractorStateRef, 0, sizeof(ENVATTRACTORSTATE));
			return;
		}

		pEnvAttractorStateRef->value = pRefAnimatState->setTravelRate; // Stimuli's magnitude
		pEnvAttractorStateRef->timeLapsed++;
		pEnvAttractorStateRef->isActive = TRUE;

		// Slope
		if(overShelf == TRUE)
			pEnvAttractorStateRef->desiredHeading = pRefAnimatState->bathySlopeHeading; // Stimuli's angle.
		else if(overBasin == TRUE)
			pEnvAttractorStateRef->desiredHeading = pRefAnimatState->bathySlopeHeading + 180; // Stimuli's angle.
		else
			memset(pEnvAttractorStateRef, 0, sizeof(ENVATTRACTORSTATE));
	}
}


// Needs work when more is known, especially delta considerations.
void CAnimat::RunEnvAttrctrResponse(BOOL *CoordIsSet, ENVATTRACTORMDL *Mdl, ENVATTRACTORSTATE *State)
{
	DISTANGL da;
	State->isActive = FALSE;// explicitly stating...

	if(*CoordIsSet == FALSE)
	{
		State->timeLapsed = 0;
		return;
	}

	if((Mdl->maxIsEnabled == TRUE && State->value >= Mdl->max) ||
		(Mdl->minIsEnabled == TRUE && State->value <= Mdl->min) ||
		(Mdl->deltaIsEnabled && State->value == Mdl->delta))
	{
		// Determine focal bearing and distance.  If distance is greater than focal
		// distance, set pod following active.
		da = m_staticLib.DetermineBearing(m_state->coord, State->coordTo);
		State->desiredHeading = da.angle;
		State->distanceTo = da.distance;

		// For now, just set active to TRUE.
		State->isActive = TRUE;
	}
	else
		State->timeLapsed = 0;
	*CoordIsSet = FALSE;
}

void CAnimat::HandleDepthEnvironmentalAttractor()
{
	const DIRECTIONSTATE *dirStateCpy = &m_state->submdl.travelDirection;
	const RATESTATE *rateStateCpy = &m_state->submdl.travelRate;
	double Ax;
	double Ay;
	double Ex;
	double Ey;
	double Cx;
	double Cy;
	double angleRads;
	double resAngle = 0;

	//----------------------------------------------//
	// Set up for multiple acoustic aversion sources
	//----------------------------------------------//
	if(m_state->acstcExp.isActive == TRUE && m_speciesDef->acousticAversion.travelDirectionAffected == TRUE)
		return;
	if(m_state->podFollowFocal.isActive == TRUE)
		return;
	if(m_state->depthEnvAtt.isActive == FALSE)
		return;

	angleRads = dirStateCpy->bearing *PI/180.0;
	Ax = rateStateCpy->rate * cos(angleRads);
	Ay = rateStateCpy->rate * sin(angleRads);

	angleRads = m_state->depthEnvAtt.desiredHeading*PI/180.0;
	Ex = rateStateCpy->rate * cos(angleRads);
	Ey = rateStateCpy->rate * sin(angleRads);

	Cx = Ax + Ex;
	Cy = Ay + Ey;

	resAngle = atan2(Cy, Cx)*180.0/PI;
	//double resRate = pow(pow(Cx,2) + pow(Cy,2), .5); resRate = resRate; // quiet compiler warning.
	
	m_state->depthEnvAtt.desiredHeading = resAngle;
}


// Takes into account shore following vs. beaching during acoustic aversion.
// Determines final setpoints.

void CAnimat::HandleNearbyShore(const RATEMDL *TravelRateMdl, const DIRCTNMDL *TravelDirMdl)
{
	int i, j;
	COORDINATE coord;
	double depth;
	FLOAT_XY deltaCopy;
	double avoidDepth;
	const DIRECTIONSTATE dirStateCpy = m_state->submdl.travelDirection;
	const RATESTATE rateStateCpy = m_state->submdl.travelRate;
	DIRECTIONSTATE wrkingDirStateCpy;
	RATESTATE wrkingRateStateCpy;
	double shoreFollowHeading;
	double maxTravelRate = m_pRefSpeciesModel->GetMaxRate(TravelRateMdl);
	BOOL directionModelUpdated; // Direction model terminated then started a new direction
	BOOL travelRateModelUpdated; // Direction model terminated then started a new direction

	// Set the final heading and travel rate to the desired for this iteration.  Shore
	// following will next override if the animat isn't a beacher, engaged in acoustic
	// aversin, and is close to shore.

	// If the animat is engaged in acoustic aversion and belongs to a species that beaches
	// do not run shore following.  Just let the animat beach if acoustic aversion drives
	// it to do so.
	if((TRUE == m_state->acstcExp.isActive) && (TRUE == m_speciesDef->acousticAversion.beaches))
	{
		m_state->setHeading = dirStateCpy.bearing;
		m_state->setTravelRate = rateStateCpy.rate;
		return; 
	}


	// Determine if shore following will alter the animat's direction.
	shoreFollowHeading = GetShoreFollowingBearing(dirStateCpy.bearing);

	// Detetmine new coordinate and depth in next iteration based upon the set travel rate
	// and heading.  If the animat doesn't beach return.
	deltaCopy = m_state->deltaXY;
	coord = CalcNewCoord(m_initialCoord, m_state->coord, shoreFollowHeading, rateStateCpy.rate, &deltaCopy, m_distCalcMethod);
	depth = m_bathymetry->GetValueAtCoordinate(coord.lat, coord.lon, &m_bathySector).depth;
	if(depth < m_speciesDef->description.shoreFollowDepth && depth < m_speciesDef->acousticAversion.beachingDepth)
	{
		// No change to heading or travel rate.
		// No change to heading or travel rate.
		if(m_state->depthEnvAtt.isActive == TRUE)
			m_state->setHeading = m_state->depthEnvAtt.desiredHeading;
		else
			m_state->setHeading = shoreFollowHeading;
		m_state->setTravelRate = rateStateCpy.rate;
		return; 
	}

	// Make sure the set heading won't beach the animat in the next iteration or trap it on
	// land.  Copy the decided upon heading and rate into the copies of the states.

	// The shore following behavior should prevent that situation so place an assertion for
	// debugging purposes.
	// Essentially, the following code checks against shore following failures and
	// indicates a something may have been missed in the programming.


	//----------------------------------------------------------------------------------//
	// The shore following behavior failed to prevent a beaching.
	// More work on this behavior is needed. 
	// Place an assertion to indicate shore following's falure while debugging but
	// use stochastic processes to try to find an alternative heading and, if needed,
	// an alternaive travel rate.
	//----------------------------------------------------------------------------------//
	//_ASSERT(FALSE); // should not have gotten here.

	// Use copies of the states because, while the following lines of code use the
	// stochastic modeling, the shore following behavior isn't meant to alter states
	// that affect behaviors. (phrase this better....)

	// is shore following is shallower than beaching
	if(m_speciesDef->description.shoreFollowDepth > m_speciesDef->acousticAversion.beachingDepth)
		avoidDepth = m_speciesDef->acousticAversion.beachingDepth;
	else
		avoidDepth = m_speciesDef->description.shoreFollowDepth; // shore following is deeper than beaching.


	//-------------------------------//
	// Attempt to modify heading only
	//-------------------------------//
	wrkingRateStateCpy = rateStateCpy;;
	for(i=0; i < SHOREFOLLOW_MODELBASED_CHANGE_ATTEMPTS; i++)
	{
		wrkingDirStateCpy = dirStateCpy;
		wrkingDirStateCpy.endDice = -1; // forces a change

		directionModelUpdated = RunDirectionModel(TravelDirMdl, &wrkingDirStateCpy); // determine (for next iteration) travel direction
		if(FALSE == directionModelUpdated)
			continue;


		deltaCopy = m_state->deltaXY; // we don't want the actual delta recordings altered.
		coord =	CalcNewCoord(m_initialCoord, m_state->coord, wrkingDirStateCpy.bearing, wrkingRateStateCpy.rate,
								&deltaCopy, m_distCalcMethod);

		depth = m_bathymetry->GetValueAtCoordinate(coord.lat, coord.lon, &m_bathySector).depth;
		if(depth < avoidDepth)
		{
			m_state->setHeading = wrkingDirStateCpy.bearing;
			return;
		}
	}
	//--------------------------------------------------//
	// Attempt to modify heading and rate stochastically
	//--------------------------------------------------//
	for(i=0; i < SHOREFOLLOW_MODELBASED_CHANGE_ATTEMPTS; i++)
	{
		wrkingRateStateCpy = rateStateCpy;;
		wrkingRateStateCpy.endDice = -1; // forces a change
		wrkingDirStateCpy = dirStateCpy;
		wrkingDirStateCpy.endDice = -1; // forces a change

		//determine (for next iteration) travel direction and rate.
		directionModelUpdated = RunDirectionModel(TravelDirMdl, &wrkingDirStateCpy);
		travelRateModelUpdated = RunTravelRateModel(TravelRateMdl, &wrkingRateStateCpy);
		if((FALSE == directionModelUpdated) && (FALSE == travelRateModelUpdated)) 
			continue;
		
		deltaCopy = m_state->deltaXY;
		coord = CalcNewCoord(m_initialCoord, m_state->coord, wrkingDirStateCpy.bearing, wrkingRateStateCpy.rate, &deltaCopy, m_distCalcMethod);
		depth = m_bathymetry->GetValueAtCoordinate(coord.lat, coord.lon, &m_bathySector).depth;

		if(depth < avoidDepth)
		{
			m_state->setHeading = wrkingDirStateCpy.bearing;	
			m_state->setTravelRate = wrkingRateStateCpy.rate;
			return;
		}
	}

	//-------------------------------------------------------------------------------//
	// Try changing heading stocastically while, in a controlled manner, reducing the
	// original rate.
	//---------------//
	wrkingRateStateCpy = rateStateCpy;;
	for(i=0; i < SHOREFOLLOW_ARTIFICIAL_CHANGE_ATTEMPS; i++)
	{
		wrkingDirStateCpy = dirStateCpy;

		// Decrease the travel rate.
		wrkingRateStateCpy.rate *= 0.90;

		// Set if, at the reduced travel rate, a heading can be found that doesn't beach
		// the animat.
		for(j=0; j < SHOREFOLLOW_MODELBASED_CHANGE_ATTEMPTS; j++)
		{
			wrkingDirStateCpy.endDice = -1; // forces a change
			directionModelUpdated = RunDirectionModel(TravelDirMdl, &wrkingDirStateCpy);

			if(FALSE == directionModelUpdated)
				continue;

			deltaCopy = m_state->deltaXY;
			coord = CalcNewCoord(m_initialCoord, m_state->coord, wrkingDirStateCpy.bearing, wrkingRateStateCpy.rate, &deltaCopy, m_distCalcMethod);
			depth = m_bathymetry->GetValueAtCoordinate(coord.lat, coord.lon, &m_bathySector).depth;

			if(depth < avoidDepth)
			{
				m_state->setHeading = wrkingDirStateCpy.bearing;	
				m_state->setTravelRate = wrkingRateStateCpy.rate;
				return;
			}
		}
	}

	//-------------------------------------------------------------------------------//
	// Try changing heading stocastically while, in a controlled manner, increasing
	// the original rate.
	//---------------//
	wrkingRateStateCpy = rateStateCpy;
	wrkingRateStateCpy.rate *= 1.10;
	for(i=0; i < SHOREFOLLOW_ARTIFICIAL_CHANGE_ATTEMPS && wrkingRateStateCpy.rate <= maxTravelRate; i++)
	{
		wrkingDirStateCpy = dirStateCpy;

		// See if, at the increased travel rate, a heading can be found that doesn't beach
		// the animat.
		for(j=0; j < SHOREFOLLOW_MODELBASED_CHANGE_ATTEMPTS; j++)
		{
			wrkingDirStateCpy.endDice = -1; // forces a change
			directionModelUpdated = RunDirectionModel(TravelDirMdl, &wrkingDirStateCpy);
			if(FALSE == directionModelUpdated)
				continue;

			deltaCopy = m_state->deltaXY;
			coord = CalcNewCoord(m_initialCoord, m_state->coord, wrkingDirStateCpy.bearing, wrkingRateStateCpy.rate, &deltaCopy, m_distCalcMethod);
			depth = m_bathymetry->GetValueAtCoordinate(coord.lat, coord.lon, &m_bathySector).depth;

			if(depth < avoidDepth)
			{
				m_state->setHeading = wrkingDirStateCpy.bearing;	
				m_state->setTravelRate = wrkingRateStateCpy.rate;
				return;
			}
		}
		// Increase the travel rate.
		wrkingRateStateCpy.rate *= 1.10;
	}

	//----------------------------------------------------------------------------------//
	// Seach the entire 360 around the animat incrementing half a degree and reducing and 
	// increasing the travel rate, to see if there's a direction it may head.
	//----------------------------------------------------------------------------------//
	wrkingDirStateCpy = dirStateCpy;
	// Adjust 90 degrees
	wrkingDirStateCpy.bearing = m_staticLib.KeepWithin360(90 + wrkingDirStateCpy.bearing);
	for(i=0; i<(int)floor(360.0/SHOREFOLLOW_ARTIFICIAL_DEG_INCR); i++)
	{
		// Increment bearing by SHOREFOLLOW_ARTIFICIAL_DEG_INCR
		wrkingDirStateCpy.bearing = m_staticLib.KeepWithin360(SHOREFOLLOW_ARTIFICIAL_DEG_INCR + wrkingDirStateCpy.bearing);
		wrkingRateStateCpy = rateStateCpy;
		for(j=0; j<SHOREFOLLOW_ARTIFICIAL_CHANGE_ATTEMPS; j++)
		{
			wrkingRateStateCpy.rate *= .90;

			deltaCopy = m_state->deltaXY;
			coord =	CalcNewCoord(m_initialCoord,
								 m_state->coord,
								 wrkingDirStateCpy.bearing,
								 wrkingRateStateCpy.rate,
								 &deltaCopy,
								 m_distCalcMethod);

			depth = m_bathymetry->GetValueAtCoordinate(coord.lat, coord.lon, &m_bathySector).depth;

			if(depth < avoidDepth)
			{
				m_state->setHeading = wrkingDirStateCpy.bearing;
				m_state->setTravelRate = wrkingRateStateCpy.rate;
				return;
			}
		}

		wrkingRateStateCpy = rateStateCpy;
		wrkingRateStateCpy.rate *= 1.10;
		for(j=0; j<SHOREFOLLOW_ARTIFICIAL_CHANGE_ATTEMPS && wrkingRateStateCpy.rate <= maxTravelRate; j++)
		{
			deltaCopy = m_state->deltaXY;
			coord =	CalcNewCoord(m_initialCoord, m_state->coord, wrkingDirStateCpy.bearing, wrkingRateStateCpy.rate, &deltaCopy, m_distCalcMethod);
			depth = m_bathymetry->GetValueAtCoordinate(coord.lat, coord.lon, &m_bathySector).depth;

			if(depth < avoidDepth)
			{
				m_state->setHeading = wrkingDirStateCpy.bearing;
				m_state->setTravelRate = wrkingRateStateCpy.rate;
				return;
			}
			wrkingRateStateCpy.rate *= 1.10;
		}
	}

	//------------------------------------------//
	// Animat dosen't move in the next iteration
	//------------------------------------------//
	m_state->setHeading = dirStateCpy.bearing;
	m_state->setTravelRate = 0;
}

#define NUMBEARINGCHECK 5

/*****************************************************************************************
* MEMBER FUNCTION:  CAnimat::GetShoreFollowingBearing()
* 
* DESCRIPTION:
*
* ARGUMENTS: None
*
* RETURN VALUE: None.
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   05/24/05  (MJC)  Initial Coding/Transfer
*****************************************************************************************/
double CAnimat::GetShoreFollowingBearing(double NrmlHeading)
{
	int i;
	int bearingCheckAngle[NUMBEARINGCHECK] = {1, 5, 12, 24, 45};
	int bearingAdjust = 0;
	double bearingCheck;
	COORDINATE coord;
	double depth;
	FLOAT_XY deltaCopy;

	// Need to consider if shore is nearby, if acoustic aversion is active, and if beaching
	// occurs during acoustic aversion.

	m_state->shoreFollow.isActive = FALSE;
	m_state->shoreFollow.adjustedHeading = 0;

	for(i=0; i<NUMBEARINGCHECK && bearingAdjust==0; i++)
	{
		// Positive rotation
		deltaCopy = m_state->deltaXY;
		bearingCheck = m_staticLib.KeepWithin360(bearingCheckAngle[i] + m_state->setHeading);
		coord = CalcNewCoord(m_initialCoord, // animat's initial coordinate when simulation begain.
							 m_state->coord, // animat's current heading
							 bearingCheck, // heading currently being tested
							 m_state->submdl.travelRate.rate, // current travel rate
							 &deltaCopy, // Copy of the animats change in x and y since the scenario began.
							 m_distCalcMethod); // Calculation method: either planar geometry or lat/lon

		depth = m_bathymetry->GetValueAtCoordinate(coord.lat, coord.lon, &m_bathySector).depth;

		if(depth >= m_speciesDef->description.shoreFollowDepth ||
			depth >= m_speciesDef->acousticAversion.beachingDepth)
			bearingAdjust = bearingCheckAngle[i] - 90;

		// Negative rotation
		deltaCopy = m_state->deltaXY;
		bearingCheck = m_staticLib.KeepWithin360(-bearingCheckAngle[i] + m_state->setHeading);
		coord = CalcNewCoord(m_initialCoord,
							 m_state->coord,
							 bearingCheck,
							 m_state->submdl.travelRate.rate,
							 &deltaCopy,
							 m_distCalcMethod);

		depth = m_bathymetry->GetValueAtCoordinate(coord.lat, coord.lon, &m_bathySector).depth;

		if(depth >= m_speciesDef->description.shoreFollowDepth ||
			depth >= m_speciesDef->acousticAversion.beachingDepth)
			bearingAdjust = 90 - bearingCheckAngle[i];
	}

	// directly ahead
	deltaCopy = m_state->deltaXY;
	coord = CalcNewCoord(m_initialCoord,
						 m_state->coord,
						 m_state->setHeading,
						 m_state->submdl.travelRate.rate,
						 &deltaCopy,
						 m_distCalcMethod);

	depth = m_bathymetry->GetValueAtCoordinate(coord.lat, coord.lon, &m_bathySector).depth;

	if(depth >= m_speciesDef->description.shoreFollowDepth || depth >= m_speciesDef->acousticAversion.beachingDepth)
	{
		if(bearingAdjust == 0)
		{
			if(m_pRefRandom->myrand() >= 0.5)
				bearingAdjust = -90;
			else
				bearingAdjust = 90;
		}
		else if(bearingAdjust < 0)
			bearingAdjust = -90;
		else
			bearingAdjust = 90;
	}

	if(bearingAdjust != 0)
	{
		m_state->shoreFollow.isActive = TRUE;
		m_state->shoreFollow.adjustedHeading = bearingAdjust;
		return m_staticLib.KeepWithin360(m_state->setHeading + bearingAdjust);
	}

	return NrmlHeading;
}


double CAnimat::MergeDesiredHeadings()
{
	// Acoustic averion, pod following, shore following, envattractor
	BOOL aa = m_state->acstcExp.isActive; // acoustic aversion 1 active;
	BOOL sf = FALSE; // shore following (not considered here, see HandleNearbyShore()).
	BOOL pf = m_state->podFollowFocal.isActive; // pod follow active;
	BOOL ea = m_state->depthEnvAtt.isActive||m_state->tempEnvAtt.isActive;// env att active;
	double acousticAversionHeading = 0;
	double envAttHeading = 0;
	double tempHeading1;

	//----------------------------------------------//
	// Set up for multiple acoustic aversion sources
	//----------------------------------------------//
	if(aa)
	{
		// Pod Following override
		if(m_speciesDef->acousticAversion.podBreaksUp)
			pf = FALSE;
		acousticAversionHeading = m_state->acstcExp.desiredHeading;
	}

	//---------------------------------------------//
	// Set up for multiple environmental attractors
	//---------------------------------------------//
	if(ea)
	{
		if(m_state->depthEnvAtt.isActive == TRUE && m_state->tempEnvAtt.isActive == FALSE)
			envAttHeading = m_state->depthEnvAtt.desiredHeading;
		else if(m_state->depthEnvAtt.isActive == FALSE && m_state->tempEnvAtt.isActive == TRUE)
			envAttHeading = m_state->tempEnvAtt.desiredHeading;
		else if(m_speciesDef->p.behavior[m_state->behState].depthHasPriorityOverTemp == TRUE)
			envAttHeading = m_state->depthEnvAtt.desiredHeading;
		else
			envAttHeading = m_state->tempEnvAtt.desiredHeading;
	}

	//-----------------------------------//
	// Consider each possible combination
	//-----------------------------------//
	// If acoustic aversion has no impact on direction then here make sure it doesn't.
	if(m_speciesDef->acousticAversion.travelDirectionAffected == FALSE)
		aa = FALSE;
	if(!aa && !sf && !pf && !ea)
	{
		// Non are active, return whatever the current normal behavior's CRWDB bias
		// direction would be.  This doesn't mean, however, that the model itself uses CRWDB.
		return m_speciesDef->p.behavior[m_state->behState].travelDirection.crRndWalkDb.directionOfBias;
	}
	else if(aa && !sf && !pf && !ea) // Acoustic aversion alone.
		return acousticAversionHeading;
	else if(!aa && sf && !pf && !ea) // Shore following alone
		return m_state->shoreFollow.adjustedHeading;
	else if(!aa && !sf && pf && !ea) // Pod following alone
		return m_state->podFollowFocal.desiredHeading;
	else if(!aa && !sf && !pf && ea) // Environmental attractor alone
		return envAttHeading;
	else if(aa && sf && !pf && !ea) // Simultaneous acoustic aversion and shore follow. Aversion gets more weight.
		return m_staticLib.KeepWithin360(m_staticLib.AddAngles(2, acousticAversionHeading, 1, m_state->shoreFollow.adjustedHeading));
	else if(aa && !sf && pf && !ea) // Simultaneous acoustic aversion and pod follow. Pod follow wins.
		return m_staticLib.KeepWithin360(m_staticLib.AddAngles(0, acousticAversionHeading, 1, m_state->podFollowFocal.desiredHeading));
	else if(aa && !sf && !pf && ea) // Simultaneous acoustic aversion and environmental attraction. Aversion only.
		return acousticAversionHeading;
	else if(!aa && sf && pf && !ea)// Simultaneous shore follow and pod follow. Shore follow wins
		return m_staticLib.KeepWithin360(m_staticLib.AddAngles(1,m_state->shoreFollow.adjustedHeading,0,m_state->podFollowFocal.desiredHeading));
	else if(!aa && sf && !pf && ea)// Simultaneous shore follow and EA. EA wins
		return m_staticLib.KeepWithin360(m_staticLib.AddAngles(0, m_state->shoreFollow.adjustedHeading, 1, envAttHeading));
	else if(!aa && !sf && pf && ea)// Simultaneous pod follow and environmental attractor follow. Pod follow wins
		return m_state->podFollowFocal.desiredHeading;
	else if(aa && sf && pf && !ea)// Simultaneous aversion, shore and pod follow.
	{
		tempHeading1 = m_staticLib.AddAngles(1,m_state->shoreFollow.adjustedHeading,1,m_state->podFollowFocal.desiredHeading);
		return m_staticLib.KeepWithin360(m_staticLib.AddAngles(2, acousticAversionHeading, 1, tempHeading1));
	}
	else if(aa && sf && !pf && ea)// Simultaneous aversion, shore and environmental attractor.
		return m_staticLib.KeepWithin360(m_staticLib.AddAngles(2, acousticAversionHeading, 1, m_state->podFollowFocal.desiredHeading));
	else if(!aa && sf && pf && ea)// Simultaneous shore follow, pod follow, and environmental attractor.
	{
		tempHeading1 = m_staticLib.AddAngles(1,m_state->podFollowFocal.desiredHeading, 1,envAttHeading);
		return m_staticLib.KeepWithin360(m_staticLib.AddAngles(2, m_state->shoreFollow.adjustedHeading, 1, tempHeading1));
	}
	else if(aa && sf && pf && ea)// All of them.  Leave out environmental attractor.
	{
		tempHeading1 = m_staticLib.AddAngles(1,m_state->shoreFollow.adjustedHeading,1,m_state->podFollowFocal.desiredHeading);
		return m_staticLib.KeepWithin360(m_staticLib.AddAngles(2, acousticAversionHeading, 1, tempHeading1));
	}
	_ASSERT(FALSE);
	return m_speciesDef->p.behavior[m_state->behState].travelDirection.crRndWalkDb.directionOfBias;
}


void CAnimat::EnforceDepthValueRules()
{

	// Three depths to keep in mind:
	// (1) bathy depth at the animat's location
	// (2) the animat's actual depth
	// (3) the animat's projected depth.

	// Cetain activities dictate a relation among these threee.

	// Since position and depth ultimately are calulated based upon behavioral
	// settings (rate, heading), make sure that calculated values do not violate
	// reasonable expectations that the behaviors couldn't account for.  Behaviors
	// probably could be developed that handle enforcement, but unless deemed necessary
	// enforcing them here is fine.

	// Rule 1: Actual depth must not ever go below bathy depth.
	// Rule 2: Actual depth must never go above sea level unless enforcement of this rule
	//         violates rule 1.
	// Rule 3: Projected depth must be reset to the same value as actual depth unless the
	//         animat is descending or bottom following.
	// Rule 4: When bottom following, the animat's actual depth must equal the bathy depth.
	// Note:   Other behaviors (land follow, beach, and manuver free) are meant to
	//         certain that the animat never hits land or goes above sea level
	if(m_state->depth > 0 && m_state->bathyDepth <= 0) // Over water away from land (normal surfacing).
		m_state->depth = 0;
	else if(m_state->depth < m_state->bathyDepth) // Underground
		m_state->depth = m_state->bathyDepth;
	else if(m_state->depth > m_state->bathyDepth && m_state->depth > 0) // In the air over ground
		m_state->depth = m_state->bathyDepth;


	if(m_state->submdl.dive.activity == BOTTOM_FOLLOWING)
		m_state->depth = m_state->bathyDepth;
	else
		m_state->submdl.dive.projectedDepth = m_state->depth;

#if 0
	// If the animat is not bottom following or descending or descending reversal
	// set the projected depth equal to the actual.
	if(m_state->submdl.dive.activity != BOTTOM_FOLLOWING && m_state->submdl.dive.activity != DESCENDING &&
		m_state->submdl.dive.activity != DESCENDING_REVERSAL)
		m_state->submdl.dive.projectedDepth = m_state->depth;
	else if(m_state->submdl.dive.activity == BOTTOM_FOLLOWING)
		m_state->depth = m_state->bathyDepth;
#endif


}

// Used for both depth and temperature.
void CAnimat::EnvAttractorAdjustState(DIRCTNMDL *TravelDirMdl, ENVATTRACTORSTATE *EnvRs)
{
	if(EnvRs->isActive == TRUE && EnvRs->timeLapsed == 0)
		m_state->submdl.travelDirection.endDice = -1; //forces a change in travel direction

	if(EnvRs->isActive == TRUE)
		TravelDirMdl->modelType = CORRELATED_RANDOM_WALK_WITH_DIR_BIASING;
}


void CAnimat::PodFollowingAdjustState(DIRCTNMDL *TravelDirMdl)
{
	if(m_state->podFollowFocal.isActive == TRUE/* && m_state->podFollowFocal.timeLapsed == 0*/)
		m_state->submdl.travelDirection.endDice = -1; //forces a change in travel direction

	if(m_state->podFollowFocal.isActive == TRUE)
		TravelDirMdl->modelType = CORRELATED_RANDOM_WALK_WITH_DIR_BIASING;
}


// forces model to update rates, directions, etc...
void CAnimat::AcousticAversionAdjustModelAndState(RATEMDL *TravelRateMdl, DIRCTNMDL *TravelDirMdl, DIVEMDL *DiveMdl)
{
	ACSTCAVRSNMDL *avrsnMdl = &m_speciesDef->acousticAversion;

	if(m_state->acstcExp.isActive == FALSE)
		return;

	//--------------------//
	// Modeling Overrides
	//--------------------//
	// Travel Rate
	if(avrsnMdl->travelRateAffected == TRUE)
	{
		TravelRateMdl->modelType = GAUSSIAN;
		TravelRateMdl->gauss = avrsnMdl->travelRate;
	}

	// Travel Direction
	if(avrsnMdl->travelDirectionAffected)
	{
		TravelDirMdl->modelType = CORRELATED_RANDOM_WALK_WITH_DIR_BIASING;
		TravelDirMdl->crRndWalkDb = avrsnMdl->travel;
	}

	// Ascent Rate impact on descent rate
	if(TRUE == avrsnMdl->ascentRateAffected)
	{
		DiveMdl->ascentRate.modelType = GAUSSIAN;
		DiveMdl->ascentRate.gauss = avrsnMdl->ascentRate;

		// Acoustic aversion begining-only state overrides
		if(m_state->submdl.dive.activity == ASCENDING || m_state->submdl.dive.activity == ASCENDING_REVERSAL)
			DiveMdl->reversal.diveRateType = NO_INDEPENDENT; // remove the independed reversal dive rate, if any.
	}

	// Acoustic Aversion impact on descent rate
	if(TRUE == avrsnMdl->descentRateAffected)
	{
		DiveMdl->descentRate.modelType = GAUSSIAN;
		DiveMdl->descentRate.gauss = avrsnMdl->descentRate;

		// Acoustic aversion begining-only state overrides
		if(m_state->submdl.dive.activity == DESCENDING || m_state->submdl.dive.activity == DESCENDING_REVERSAL)
			DiveMdl->reversal.diveRateType = NO_INDEPENDENT; // remove the independed reversal dive rate, if any.
	}

	// Depth
	if(TRUE == avrsnMdl->depthAffected)
	{
		DiveMdl->depth.modelType = GAUSSIAN;
		DiveMdl->depth.gauss = avrsnMdl->depth;
	}

	// No bottom following is allowed during acoustic aversion.
	// Modified on 7-7-09 for the species version 5.0 with the new bottom following
	//DiveMdl->bottomFollows = FALSE;
	DiveMdl->bttmFollow.type = NO_BTTMFLLWNG;

	// Reversals
	if(TRUE == avrsnMdl->reversalAffected)
	{
		//DiveMdl->reversal.hasaIndependentDiveRate = FALSE; here or above?
		DiveMdl->reversal.modelType = GAUSSIAN;
		DiveMdl->reversal.gauss = avrsnMdl->reversal;
	}

	// Surface Interval
	if(TRUE == avrsnMdl->surfaceIntervalAffected)
	{
		DiveMdl->srfInv.modelType = GAUSSIAN;
		DiveMdl->srfInv.gauss = avrsnMdl->surfaceInterval;
	}


	//---------------------//
	// Forced State Changes
	//---------------------//
	// Only trigger state changes if acoustic exposure just began.
	if(m_state->acstcExp.timeLapsed != 0)
		return;

	// Travel Rate
	if(avrsnMdl->travelRateAffected == TRUE)
		m_state->submdl.travelRate.endDice = -1;  //forces a change in travel rate this iteration

	// Travel Direction
	if(avrsnMdl->travelDirectionAffected == TRUE)
		m_state->submdl.travelDirection.endDice = -1; //forces a change in travel direction

	// Ascent Rate
	if(avrsnMdl->ascentRateAffected == TRUE &&
		(m_state->submdl.dive.activity == ASCENDING || m_state->submdl.dive.activity == ASCENDING_REVERSAL))
		m_state->submdl.dive.rate.endDice = -1;//forces a change in ascent rate this iteration

	// Descent Rate
	if(avrsnMdl->descentRateAffected == TRUE &&
		(m_state->submdl.dive.activity == DESCENDING || m_state->submdl.dive.activity == DESCENDING_REVERSAL))
		m_state->submdl.dive.rate.endDice = -1; //forces a change in descent rate this iteration

	if(/*avrsnMdl->flatBottomDiveAffected (TRUE && */m_state->submdl.dive.activity == BOTTOM_FOLLOWING)
		m_state->submdl.dive.targetDepth = 1; //forces an end to bottom folllowing this iteration

	// Reversals 
	if(avrsnMdl->reversalAffected == TRUE &&
		(m_state->submdl.dive.activity == ASCENDING_REVERSAL || m_state->submdl.dive.activity == DESCENDING_REVERSAL))
	{
		int newDur = m_pRefSpeciesModel->ReversalDuration(&DiveMdl->reversal.gauss);
		if(m_state->submdl.dive.reversal.legDuration > newDur)
			m_state->submdl.dive.reversal.legDuration = newDur;
	}

	// Surface Interval
	if(avrsnMdl->surfaceIntervalAffected == TRUE && m_state->submdl.dive.activity == IN_SURFACE_INTERVAL)
	{
		int newSI = m_pRefSpeciesModel->SurfaceInterval(&DiveMdl->srfInv.gauss);
		if(m_state->submdl.dive.surfInterval.setDuration > newSI)
			m_state->submdl.dive.surfInterval.setDuration = newSI;
	}
}

MATRIX *CAnimat::GetCurrentBehaviorTransitionMatrix(int *TimeRow)
{
	int i, maxIndex, timeRow = -1;// m_state->simClock
	MATRIX *m = NULL;

	maxIndex = m_speciesDef->p.behavior[m_state->behState].nrmlBehTransCnt - 1;
	if(m_state->depth >= m_speciesDef->p.behavior[m_state->behState].nrmlBehTrans.arr[0].depthSpan.deep)
	{
		// The animat's depth is shallower than the first spand's 'deep' value.
		m = &m_speciesDef->p.behavior[m_state->behState].nrmlBehTrans.arr[0].m;
	}
	else if(m_state->depth <= m_speciesDef->p.behavior[m_state->behState].nrmlBehTrans.arr[maxIndex].depthSpan.deep)
	{
		// The animat's depth is deeper than the last spand's 'deep' value, so set the matrix to the that of the last
		// span's.
		m = &m_speciesDef->p.behavior[m_state->behState].nrmlBehTrans.arr[maxIndex].m;
	}
	else
	{
		// Determine which depth span the animat falls within.
		for(i=1; i<m_speciesDef->p.behavior[m_state->behState].nrmlBehTransCnt; i++)
		{
			_ASSERT(m_speciesDef->p.behavior[m_state->behState].nrmlBehTrans.arr[i].depthSpan.shallow >=
				m_speciesDef->p.behavior[m_state->behState].nrmlBehTrans.arr[i].depthSpan.deep);

			// Working with negative numbers.
			if(m_speciesDef->p.behavior[m_state->behState].nrmlBehTrans.arr[i].depthSpan.shallow > m_state->depth &&
				m_state->depth >= m_speciesDef->p.behavior[m_state->behState].nrmlBehTrans.arr[i].depthSpan.deep)
			{
				m = &m_speciesDef->p.behavior[m_state->behState].nrmlBehTrans.arr[i].m;
				break;
			}
		}
		// Assert i is a proper span index.
		_ASSERT(i < m_speciesDef->p.behavior[m_state->behState].nrmlBehTransCnt);
	}

	_ASSERT(m != NULL);
	if(m == NULL)
		m = &m_speciesDef->p.behavior[m_state->behState].nrmlBehTrans.arr[0].m;


	timeRow = -1;
	for(i=0; i< m->rowCnt && timeRow == -1; i++)
	{
		if(m_staticLib.ClockTimeIsWithin(m_state->simClock, m->p.ppa[i][0], m->p.ppa[i][1]) == TRUE)
			timeRow = i;
	}
	
	// If not time match found (bad modeling and error catching on the part of the GUI
	// and species designer) set the vector to the zero-th row.
	if(timeRow == -1)
		timeRow = 0;

	*TimeRow = timeRow;

	return m;
}

/*****************************************************************************************
* MEMBER FUNCTION: BehaviorTransition()
* 
* ARGUMENTS: None.
*
* MEMBER VARIABLES ALTERED:
*	Part of the CAnimat Simulation Routines.
*	m_state->normal.behavior, an TRANSITNSTATE structure containing the behavior state, 
*		time the behavior begain, and the dice to end value for changing the
*		behavior.
*
* RETURN VALUE: None.
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   05/24/05  (MJC)  Initial Coding/Transfer
*	02/03/09  (MJC)  Modified code to check for and handle a -1 integer being returned by
*					 call to m_pRefSpeciesModel->BehaviorTransition() when no normal behavior
*					 transition occurs.
*
* DESCRIPTION:
*	Determines the new behavior of the animat.  First verifies behaviors are
*	being used (which, at the time of documentation, only happens via user 
*	matrix entry).  If behaviors are being used, then calculates time lapse and
*	calls upon the species model to determine if it is time for a new behavior.
*	If so, it updates the member variable m_state->normal.behavior, an TRANSITNSTATE
*	structure holding relevant behavior information.
*
*****************************************************************************************/
void CAnimat::BehaviorTransition()
{
	int transtionRow;
	BEHTRANSMDL *behTrans = NULL;
	MATRIX *nrmlBehTransMatrix = NULL;

	// m_state->behTransActive is set to true either when the current behavior's
	// BEHTRANS_TERM_MODEL is set to T50_K_TERM and the next behavior has been determined
	// using time elapsed or when BEHTRANS_TERM_MODEL is set to T50_K_TERM and the
	// remaining time in the behavior reaches zero.
	if(m_state->behTransActive == TRUE)
	{
		_ASSERT(m_state->nextBehState >= 0);
		return;
	}
	else if(m_speciesDef->p.behavior[m_state->behState].nrmlBehTermType == GAUSSIAN_TERM && m_state->submdl.behavior.timeRemaining > 0)
	{
		m_state->submdl.behavior.timeRemaining--;
		return;
	}


	//_ASSERT(m_state->nextBehState == -1);
	//_ASSERT(m_speciesDef->p.behavior[m_state->behState].nrmlBehTransCnt >= 1);

	//----------------------------------------------------------------------------------//
	// Determine which behavior transition matrix to use.
	//--------------------------------------------------//
	nrmlBehTransMatrix = GetCurrentBehaviorTransitionMatrix(&transtionRow);
	//----------------------------------------------------------------------------------//

	//----------------------------------------------------------------------------------//
	// Environmental Attractors Override (NOT CURRENTLY IMPLEMENTED SO VERIFY NOT ACTIVE)
	//----------------------------------------------------------------------------------//
	_ASSERT(m_state->tempEnvAtt.isActive == FALSE);
//	if(m_state->depthEnvAtt.isActive && m_state->tempEnvAtt.isActive)
//	{
//		if(m_speciesDef->p.behavior[m_state->behState].depthHasPriorityOverTemp == TRUE)
//			behTrans = &m_speciesDef->p.behavior[m_state->behState].depthEnvAttBehTrans;
//		else
//			behTrans = &m_speciesDef->p.behavior[m_state->behState].tempEnvAttBehTrans;
//	}
//	else if(m_state->depthEnvAtt.isActive)
//		behTrans = &m_speciesDef->p.behavior[m_state->behState].depthEnvAttBehTrans;
	//else
	if(m_state->tempEnvAtt.isActive)
		behTrans = &m_speciesDef->p.behavior[m_state->behState].tempEnvAttBehTrans;
	//-----------------------------------------------------------------------------------//


	//----------------------------------------------------------------------------------//
	// Behavior Transition
	//---------------------//
	// Determine if there is a behavior transition.  BehaviorTransition() returning a -1
	// means no transition.
	m_state->nextBehState = (INT16)m_pRefSpeciesModel->BehaviorTransition(m_speciesDef->p.behavior[m_state->behState].nrmlBehTermType,
		&m_state->submdl.behavior, nrmlBehTransMatrix, m_state->simClock);
	if(m_state->nextBehState == -1)
		return;

	// If the normal behavior did change assert that the index is a positive number
	// greater than 0. 65536 is the max number a 16-bit unsigned integer can hold.
	_ASSERT((m_state->nextBehState >= 0) && (m_state->nextBehState < 65536));
	m_state->behTransActive = TRUE;
}


/*****************************************************************************************
* MEMBER FUNCTION:
*	CAnimat::NormalTravel(const RATE_MODEL *M, ANIMAT_RATE CurrentRate)
*
* ARGUMENTS:
*	A pointer to a RATE_MODEL structure (from the species model making up this
*	pod) and a ANIMAT_RATE structure (from class Animat).  Note that nothing passed
*   in is altered.  The pointer to structure RATE_MODEL is passed in as a 
*   constant so no altering is allowed, and the structure ANIMAT_RATE is passed by
*	value so it is actually a copy of a ANIMAT_RATE.
*
* MEMBER VARIABLES ALTERED:
*	None.  It uses several class variables, but altering is done externally 
*	to this routine by means of the returned structure ANIMAT_RATE.
*
* RETURN VALUE:
*	A ANIMAT_RATE structure, with updated values if any.
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   05/17/05  (MJC)  Initial Coding/Transfer
*
* DESCRIPTION:
*	Determines if it's time to establish a new rate.  This function is used for
*	travel rates (rate of travel, or, rot as seen in the original 3MB program)
*	and dive rates (rate of dive, or rod, as seen in the original 3MB program).
*	If this routine determines it is time for a new rate, it calculates that
*	rate and returns the values in the returned ANIMAT_RATE structure.
*   
* CORRESPONDING FUNCTION IN ORIGINAL 3MB PROGRAM, IF ANY
*	Combines ChangeRODVectorModel(), UniformRandomROD(), and GaussianRandomROD()
*   in file vertical_movement.cpp and ChangeROTVectorModel() UniformRandomROT()
*   and GaussianRandomROD() in horizontal_movement.cpp.
*
*****************************************************************************************/
BOOL CAnimat::RunTravelRateModel(const RATEMDL *RateModel, RATESTATE *State)
{
	if(m_pRefSpeciesModel->RateTerminates(RateModel, State))
	{
		// Time to find a new travel rate.
		State->rate = m_pRefSpeciesModel->RateModel(RateModel);
		State->timeLapsed = 0;
		State->endDice = m_pRefRandom->myrand();
		return TRUE;
	}
	return FALSE;
}


void CAnimat::NormalTravel()
{
#if 0
	STANDARD_MODEL_TYPE mdlOverride = NOOVERRIDE;	// Model Override (default is not to over ride).
	int timeLapse;								// Time the animat spent at current rate of travel
	BOOL terminate;									// A Flag indicating the current rate of travel terminates.
	CSpeciesModel *S = m_speMdl;				// A ptr to the species model to make following code lines shorter
	const RATE_MODEL *M = &S->m_travelModel_TOBEREMOVED;		// A ptr to the travel model to make following code lines shorter. 

	// Determine the time lapse since starting the current rate of travel.
	timeLapse = *m_simulationClock - m_state->normal.travel.startTime;

	// Check if this animat's accoustic exposure threshold is exceeded, and if so, if
	// this model deals with it when determining rate of travel.
	if((m_aeAdversionEngaged == TRUE) && (S->m_ae_TOBEREMOVED.travel == TRUE))
	{
		mdlOverride = ADVERSION;

		// The first iteration (when threashold exposure is exceeded and time averting
		// equals zero) ALWAYS has a new rate of travel calculated (this happens at the
		// end of this function because the code will NOT have returned before reaching
		// the end of this function).

		// After the first iteration a probability check is performed.  So the following
		// check is done only if the time the animat has spent averting is greater than
		// zero.
		
		// Return from this function if its not time to terminate current travel rate.
		if(m_aeTimeAverting > 1)
		{
			terminate =	S->RateTerminates(M, m_state->normal.behavior.state, timeLapse, m_state->normal.travel.endDice, mdlOverride);
			if(terminate == FALSE)
				return;
		}
	}
	else // Normal conditions.  Terminate if not time to terminate current rate of travel.
	{
		terminate = S->RateTerminates(M, m_state->normal.behavior.state, timeLapse, m_state->normal.travel.endDice);
		if(terminate == FALSE)
			return;  // return if not time to change travel rate
	}

	// Time to find a new travel rate.
	m_state->normal.travel.rate = S->TravelModel(m_state->normal.behavior.state, mdlOverride);
	m_state->normal.travel.startTime = *m_simulationClock;
	m_state->normal.travel.endDice = m_pRefRandom->myrand();
#endif
}





/*****************************************************************************************
* MEMBER FUNCTION:
* CAnimat::NormalDirection(DIRECTION_MODEL_thisGoesAway *M)
* 
* ARGUMENTS:
*	Either none or the following when pod behavior is being used.
*	COORD_DEPTH *pFocusCoord - floating point coordinates of a focus animal
*	double MaxDistance - maximum distance animals in a pod may be from the focus 
*						animal.
*
* MEMBER VARIABLES ALTERED:
*	m_probTurning is altered, but that member variable is also only used in this
*	function.  No other member variables are altered, but certain other member
*	variables are used for calculation.
*
* RETURN VALUE:
*	A DIRECTIONSTATE structure, containing results of this function.  
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   05/23/05  (MJC)  Initial Coding/Transfer
*
* DESCRIPTION:
*	Probabilistically determines based upon time if an animal to changes direction. 
*	If it does determine that it is time to change direction, it uses the 
*	DIRECTION_MODEL_thisGoesAway and the class SpeciesModel to determine a new direction.

  Takes into account pod behavior and acoustic response.
*
*	In the case of pod behavior, if the this animal gets beyond the maximum distance
*	to the focus animal, this animal is forced to choose to go toward the focus
*	animal until it is no longer beyond the maximum distance.
*	After determining the new direction, a check is made against bathymetry data 
*	(if bathymetry data is being used) to verify there isn't a conflict with the 
*	next update's depth.*
*****************************************************************************************/
BOOL CAnimat::RunDirectionModel(const DIRCTNMDL *DirMdl, DIRECTIONSTATE *DirectionState)
{
	// See if the current heading terminates.
	if(FALSE == m_pRefSpeciesModel->DirectionTerminates(DirMdl, DirectionState))
		return FALSE;

    DirectionState->bearing = m_pRefSpeciesModel->Direction(DirMdl, DirectionState);
	DirectionState->timeLapsed = 0;
	DirectionState->endDice = m_pRefRandom->myrand();

	// Add stuff from old NormalDirection() below, get beaching depth and beaching enabled from
	// the species model
	return TRUE;
}


/*****************************************************************************************
* MEMBER FUNCTION:
*	void CAnimat::NormalDive()
* 
* ARGUMENTS:
*	None.
*
* MEMBER VARIABLES ALTERED:
*	Several
*
* RETURN VALUE:
*	Returns TRUE if the horizontal travel rate needs to be changed as a result of going into
*   or coming out of a flat bottom dive period.  Sets the FLATBTTMDIVE_HZTL_RATE parameter
*   indicate which model horizontal model (either the one defined for flat bottom dive
*   periods or normal modeling) is to be used for the transition.
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   05/18/05  (MJC)  Initial Coding/Transfer
*
* DESCRIPTION:
*	Part of the CAnimat Simulation Routines.
*	Decides next dive activity based upon current dive activity, dive model, and
*	various member variables.
*
* CORRESPONDING FUNCTION IN ORIGINAL 3MB PROGRAM, IF ANY
*	NormalDive(), in update.cpp
*
*****************************************************************************************/
BOOL CAnimat::RunDiveModel(const DIVEMDL *Model)
{
	DIVESTATE *state = &m_state->submdl.dive; // keep lines shorter.
	BOOL diveRateTerminates;
	const RATEMDL *rp = &Model->descentRate;
	BOOL ret = FALSE;

	//----------------------------------------------//
	// (1) Check for a change in descent/ascent rate
	//----------------------------------------------//
	if(state->activity == IN_SURFACE_INTERVAL || state->activity == BEACHED)
		diveRateTerminates = FALSE;
	else if(state->activity == ASCENDING || state->activity == ASCENDING_REVERSAL)
	{
		rp = &Model->ascentRate;
		if(state->activity == ASCENDING_REVERSAL && Model->reversal.diveRateType != NO_INDEPENDENT)
		{
			if(Model->reversal.diveRateType == INDEPENDENT)
				diveRateTerminates = m_pRefSpeciesModel->RateTerminates(&Model->reversal.diveRate, &state->rate);
			else // Model->reversal.diveRateType == INDEPENDENT_DIVE_AND_ASCENT
				diveRateTerminates = m_pRefSpeciesModel->RateTerminates(&Model->reversal.ascentRate, &state->rate);
		}
		else
		{
			diveRateTerminates = m_pRefSpeciesModel->RateTerminates(rp, &state->rate);
		}
	}
	else // DESCENDING, DESCENDING_REVERSAL, BOTTOM_FOLLOWING
	{
		rp = &Model->descentRate;
		if(Model->reversal.diveRateType != NO_INDEPENDENT && state->activity == DESCENDING_REVERSAL)
		{
			// Model->reversal.diveRate models descent and ascent rates if diveRateType is
			// INDEPENDENT and descent if diveRateType is INDEPENDENT_DIVE_AND_ASCENT
			diveRateTerminates = m_pRefSpeciesModel->RateTerminates(&Model->reversal.diveRate, &state->rate);
		}
		else
		{
			diveRateTerminates = m_pRefSpeciesModel->RateTerminates(rp, &state->rate);
		}
	}

	if(diveRateTerminates == TRUE)
	{
		if(Model->reversal.diveRateType != NO_INDEPENDENT && state->activity == DESCENDING_REVERSAL)
		{
			state->rate.rate = m_pRefSpeciesModel->RateModel(&Model->reversal.diveRate);
		}
		else if(Model->reversal.diveRateType != NO_INDEPENDENT && state->activity == ASCENDING_REVERSAL)
		{
			if(Model->reversal.diveRateType == INDEPENDENT)
			{
				// used for both ascent and descent rates in this case.
				state->rate.rate = m_pRefSpeciesModel->RateModel(&Model->reversal.diveRate);
			}
			else //Model->reversal.diveRateType == INDEPENDENT_DIVE_AND_ASCENT
			{
				state->rate.rate = m_pRefSpeciesModel->RateModel(&Model->reversal.ascentRate);
			}
		}
		else
		{
			state->rate.rate = m_pRefSpeciesModel->RateModel(rp);
		}

		state->rate.timeLapsed = 0;
		state->rate.endDice = m_pRefRandom->myrand();
	}

	//----------------------------------------------------//
	// (2) Handle dive state transitions if time to do so.
	//----------------------------------------------------//
	switch(state->activity) // Current dive activity.
	{
	case IN_SURFACE_INTERVAL:
		// The following assertation is correct except that this can happen if behaviors
		// meant to prevent the animat from reaching the surface don't work properly.
		//_ASSERTE(0 == m_state->projDepth); 

		// If IN_SURFACE_INTERVAL hasn't expired break. Otherwise transition to DESCENDING
		if(state->surfInterval.timeLapsed <= state->surfInterval.setDuration)
			break;

//		BehaviorTransition(); 

		state->activity = DESCENDING;
		state->rate.rate = m_pRefSpeciesModel->RateModel(&Model->descentRate);
		state->rate.timeLapsed = 0;
		state->rate.endDice = m_pRefRandom->myrand();

		// Set target depth.
		state->targetDepth = m_pRefSpeciesModel->DepthModel(&Model->depth);
		break;

	case BOTTOM_FOLLOWING:
		_ASSERTE(m_state->depth == m_state->bathyDepth);

		// If the projected depth is shallower than the target depth break, otherwise 
		// transition to ASCENDING.
		if(state->projectedDepth > state->targetDepth)
			break;

		state->activity = ASCENDING;
		state->rate.rate = m_pRefSpeciesModel->RateModel(&Model->ascentRate);
		state->rate.timeLapsed = 0;
		state->rate.endDice = m_pRefRandom->myrand();

		ret = TRUE; // Indicate a transition out of flat bottom diving.
		break;

	case ASCENDING:
		_ASSERTE(state->projectedDepth == m_state->depth);

		// If the animat hasn't reached the surface break.
		if(m_state->depth < 0)
			break;

		// Change activity to surface interval.
		state->activity = IN_SURFACE_INTERVAL;
		state->rate.rate = 0;
		state->rate.timeLapsed = 0;

		// Determine the surface interval duration, set the start time
		state->surfInterval.setDuration = m_pRefSpeciesModel->SurfaceInterval(&Model->srfInv);
		state->surfInterval.timeLapsed = 0;
		break;


	case DESCENDING:
		_ASSERTE(m_state->depth >= m_state->bathyDepth);

		// Check if the animat will transition into bottom follow, reversals, or ascend.
		if((m_state->depth <= state->targetDepth) && (Model->reversal.reverses == TRUE) &&
			(0 != (state->reversal.remainingQty = m_pRefSpeciesModel->NumberOfReversals(&Model->reversal))))
		{
			// Transition to ascending reversal, determine new dive rate and new dive depth.  Set
			// descent rate, start time, and end dice.
			state->activity = ASCENDING_REVERSAL;
			if(Model->reversal.diveRateType == INDEPENDENT)
				state->rate.rate = m_pRefSpeciesModel->RateModel(&Model->reversal.diveRate);
			else if(Model->reversal.diveRateType == INDEPENDENT_DIVE_AND_ASCENT)
				state->rate.rate = m_pRefSpeciesModel->RateModel(&Model->reversal.ascentRate);
			else
				state->rate.rate = m_pRefSpeciesModel->RateModel(&Model->descentRate);
			state->rate.timeLapsed = 0;
			state->rate.endDice = m_pRefRandom->myrand();

			// Determine duration of the ascending leg.	
			state->reversal.timeLapsed = 0;
			state->reversal.legDuration = m_pRefSpeciesModel->ReversalDuration(&Model->reversal);
		}
		else if(m_state->depth == m_state->bathyDepth && Model->bttmFollow.type != NO_BTTMFLLWNG)
		{
			state->activity = BOTTOM_FOLLOWING;
			ret = TRUE; // Indicate a transition into flat bottom diving.
		}
		else if((m_state->depth == m_state->bathyDepth) || (m_state->depth <= state->targetDepth))
		{
			state->activity = ASCENDING;
			state->rate.rate = m_pRefSpeciesModel->RateModel(&Model->ascentRate);
			state->rate.timeLapsed = 0;
			state->rate.endDice = m_pRefRandom->myrand();
		}
		break;

	case ASCENDING_REVERSAL:
		_ASSERTE(state->projectedDepth == m_state->depth);

		if(m_state->depth == 0)
		{
			state->activity = IN_SURFACE_INTERVAL;
			state->rate.rate = 0;
			state->surfInterval.timeLapsed = 0;
			state->surfInterval.setDuration = m_pRefSpeciesModel->SurfaceInterval(&Model->srfInv);
		}
		else if(state->reversal.timeLapsed > state->reversal.legDuration)
		{
			// Transition to descending reversal, determine new dive rate and new dive
			// depth.  Set descent rate, start time, and end dice.
			state->activity = DESCENDING_REVERSAL;
			if(Model->reversal.diveRateType != NO_INDEPENDENT)
				state->rate.rate = m_pRefSpeciesModel->RateModel(&Model->reversal.diveRate);
			else
				state->rate.rate = m_pRefSpeciesModel->RateModel(&Model->descentRate);
			state->rate.timeLapsed = 0;
			state->rate.endDice = m_pRefRandom->myrand();

			// Determine duration of the descending leg.
			state->reversal.timeLapsed = 0;
			state->reversal.legDuration = m_pRefSpeciesModel->ReversalDuration(&Model->reversal);
			state->reversal.remainingQty--;
		}
		break;

	case DESCENDING_REVERSAL:
		_ASSERTE(state->projectedDepth == m_state->depth);
	
		// Switch to either ascend or reveral ascend if the duration of the current leg
		// is up or bethos reached.
		if((state->reversal.timeLapsed > state->reversal.legDuration) || (m_state->depth == m_state->bathyDepth))
		{
			state->activity = ASCENDING;
			state->rate.rate = m_pRefSpeciesModel->RateModel(&Model->ascentRate);
			if(state->reversal.remainingQty > 0)
			{
				state->activity = ASCENDING_REVERSAL;

				if(Model->reversal.diveRateType == INDEPENDENT)
					state->rate.rate = m_pRefSpeciesModel->RateModel(&Model->reversal.diveRate);
				else if(Model->reversal.diveRateType == INDEPENDENT_DIVE_AND_ASCENT)
					state->rate.rate = m_pRefSpeciesModel->RateModel(&Model->reversal.ascentRate);
				state->reversal.timeLapsed = 0;
				state->reversal.legDuration = m_pRefSpeciesModel->ReversalDuration(&Model->reversal);
				state->reversal.remainingQty--;
			}
			state->rate.timeLapsed = 0;
			state->rate.endDice = m_pRefRandom->myrand();
		}
		break;
	}
	return ret;
}


//******************************************************************************
// Utility Functions
//******************************************************************************


/*****************************************************************************************
* STATIC FUNCTION: CAnimat::CalcNewCoordinatePlanarGeom()
* 
* ARGUMENTS:
*	COORD_DEPTH *StartingCoordinate    - Initial coordiants to base calculation upon.
*   double Bearing    - Bearing, in degrees, to base calculation upon.
*	double RunTravelRateModel - Travel rate to base calculation upon.
*
* MEMBER VARIABLES ALTERED:
*	NONE - 
*
* RETURN VALUE:
*	A structure of type COORD_DEPTH, which holds the new coordinates calculated.
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   05/23/05  (MJC)  Initial Coding/Transfer
*
* DESCRIPTION:
*	Part of the CAnimat Utility Routines.
*	Calculates new coordinates based upon present coordinates, bearing, and 
*   rate an object is moving.
*****************************************************************************************/

COORDINATE CAnimat::CalcNewCoord(COORDINATE InitCoord, COORDINATE CurrentCoord, double Heading, double Rate, FLOAT_XY *pDeltaXY, DISTCALC DistanceCalcMethod)
{
	COORDINATE *pRefCoord; // 
	double meters;			// meters traveled this iteration.
	double bear; // animal bearing in radians
	double radsTraveled, temp_x, temp_y;
	COORDINATE c;

	bear = (PI / 180) * Heading;
	if(DistanceCalcMethod == PLANAR_GEOMETRY)
	{
		// Add the XY components of the distance traveled this iteration to the total XY
		// distance traveled for the entire simulation. 
		pDeltaXY->x += Rate * cos(bear);
		pDeltaXY->y += Rate * sin(bear);

		// Calculate the distance (in meters) and angle from the animat's starting to its current coordinate.
		meters = fabs(sqrt(pow(pDeltaXY->x, 2) + pow(pDeltaXY->y, 2)));
		bear = atan2(pDeltaXY->y,pDeltaXY->x);
		pRefCoord = &InitCoord;
	}
	else //DistanceCalcMethod = LAT_LON
	{
		meters = Rate;
		pRefCoord = &CurrentCoord;
	}


	//----------------------------------------------------------------------------------//
	// Second part of function
	//------------------------//
	// Convert the degrees to radians (note that the sign is changed on the longitude to
	// account for the convention used in these equations of designating W as positive)
	//----------------------------------------------------------------------------------//
	// In calculating the reference lat/lon in radians if the distance calculation method
	// is PLANAR_GEOMETRY then this is the initial animat lat/lon in radians from the
	// initial animat state at the very start of the simulation.  If the distance
	// calculation method is LAT_LON then this is the radians of the lat/lon from the
	// previous state last step.
	pRefCoord->lon = (PI / 180) * (-pRefCoord->lon);
	pRefCoord->lat = (PI / 180) * (pRefCoord->lat);

	// In the next line of code variables 'meters' and 'radsTraveled' will either be total
	// traveled since scenario began if this is using PLANAR_GEOMETRY method or since the
	// last step if using the LAT_LON method.
	// Calculate distance traveled (in radians).
	// Need to convert meters to Nautical Miles to degrees to radians by:
	//    1.  Converting meters to Nautical Miles by dividing by 1852.
	//	  2.  Converting Nautical Miles to degrees by multiplying by 60
	//	  3.  Converting degrees to radians by multiplying by PI/180.
	//	  Note that this method is accurate at the equator but strays as you head away from the equator 
	//    because of the decreased distance between longitudal lines.
	radsTraveled = (PI / (180 * 60)) * (meters / 1852);

	// Calculate intermediates
	temp_y = ((pRefCoord->lon - asin(sin(bear) * sin(radsTraveled) / cos(pRefCoord->lat)) + PI));
	temp_x = (2 * PI);

	// latitude
	c.lat = (180/PI)*(asin(sin(pRefCoord->lat) * cos(radsTraveled) + cos(pRefCoord->lat)*sin(radsTraveled)*cos(bear)));
	// longitude
	c.lon = -(180/PI) * ((temp_y - (temp_x * floor(temp_y / temp_x))) - PI);
	return c;
}


int CAnimat::GetUniqueID()
{
	return m_uniqueId;
}


//******************************************************************************
// File Functions
//******************************************************************************

void CAnimat::GetFileOutStateCopy(ANIMATSTATE_FILEOUT *State)
{
	PACKAGED_STATE_DATA psd = {0};

	memset(State, 0, sizeof(ANIMATSTATE_FILEOUT));

	psd.behavior = m_state->behState;
	psd.nextBehavior = m_state->nextBehState;  // added 1/26/10
	psd.nextBehaviorSelected = m_state->behTransActive;  // added 1/26/10
	psd.threshFlagAPhy = m_state->acstcExp.lvlAPhysFlag;
	psd.threshFlagBPhy = m_state->acstcExp.lvlBPhysFlag;
	psd.threshFlagBBeh = m_state->acstcExp.lvlBBehFlag;
	psd.diveActivity = m_state->submdl.dive.activity;
	psd.offScreenAnimatReset = m_state->offScreenInf.offScreen;


	if(m_state->beached == TRUE)
		psd.diveActivity = BEACHED;

	if(m_state->acstcExp.isActive)
		psd.overrideBehavior |= ACOUSTIC_AVERSION_SHIFT;
	if(m_state->podFollowFocal.isActive)
		psd.overrideBehavior |= POD_FOLLOW_SHIFT;
	if(m_state->shoreFollow.isActive)
		psd.overrideBehavior |= SHORE_FOLLOW_SHIFT;
	if(m_state->depthEnvAtt.isActive)
		psd.overrideBehavior |= ENV_ATTR_DEPTH_SHIFT;
	if(m_state->tempEnvAtt.isActive)
		psd.overrideBehavior |= ENV_ATTR_TEMP_SHIFT;

	State->animatID = m_uniqueId;
	State->lat = (float)m_state->coord.lat;
	State->lon = (float)m_state->coord.lon;
	State->depth = (float)m_state->depth;
	State->bearing = (float)m_state->setHeading;
	State->diveRate = (float)m_state->setDiveRate;
	State->travelRate = (float)m_state->setTravelRate;
	State->aeCmltve = (float)m_state->acstcExp.cumulativeValue;
	State->aeMoment	= (float)m_state->acstcExp.actualSrcInstantValue;
	State->aeRelAngle = (float)m_state->acstcExp.responseSrcAngle;
	State->aeTimeAvrt = m_state->acstcExp.timeLapsed;
	State->clock = m_state->simClock;

#pragma message("Add Slope and slop heading???")
	State->bathyDepth = (float)m_state->bathyDepth;
	State->salinity = 0;//(float)m_salinity;
	State->temperature = 0;//(float)m_temperature;

	State->targetDepth = (float)m_state->submdl.dive.targetDepth;
	State->calcDepth = (float)m_state->submdl.dive.projectedDepth; //30
	State->xDistance = (float)m_state->deltaXY.x; // 41.A
	State->yDistance = (float)m_state->deltaXY.y; // 41.B

	if(m_state->acstcExp.risk > 0) // 41.B
		m_state->acstcExp.risk = m_state->acstcExp.risk;
	State->risk = (float)m_state->acstcExp.risk; // 41.B

	State->packedData = m_classAnimatStatics.PackageStateData(psd);
}



/*****************************************************************************************
* MEMBER FUNCTION: GetfLatLon()
* 
* DESCRIPTION:
*	Returns a copy of an animals floating point coordinates.
*
* ARGUMENTS:
*	None.
*
* MEMBER VARIABLES ALTERED:
*	None.
*
* RETURN VALUE:
*	A COORD_DEPTH structure, which is a copy of this animals floating point
*	coordinates.
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   06/27/05  (MJC)  Initial Coding/Transfer
*
*****************************************************************************************/
// Corrected bug: Only the latitude was beign set, and was being set to longitude. 11-19-08
// modified 11-19-08 for wrapper class interface upgrade.
INHABITINF CAnimat::GetCurrentCoordinates()
{
	INHABITINF  d = {0};
	d.acstcSrc = m_acstcSrcInf;
	d.coord.lat = m_currentCoord.lat;
	d.coord.lon = m_currentCoord.lon;

	// This may need work.
	if(m_state == NULL)
		d.coord.depth = 0;
	else
		d.coord.depth = m_state->depth;
	return d;
}

void CAnimat::SetInhabitantAcousticSourceInf(ACOUSTICSRCEINF AcstSrcInf)
{
	m_acstcSrcInf = AcstSrcInf;
}


// Added setting current coord on 11-19-08 
// Change this name.INHABITINF
#if 0
void CAnimat::SetInitialCoordinate(double Lat, double Lon) 
{
	m_currentCoord.lat = m_seedingCoord.lat = m_initialCoord.lat = Lat;
	m_currentCoord.lon = m_seedingCoord.lon = m_initialCoord.lon = Lon;
	
}

COORD_DEPTH CAnimat::GetInitialCoordinate()
{
	COORD_DEPTH d = {0};
	d.lat = m_seedingCoord.lat;
	d.lon = m_seedingCoord.lon;
	//d.depth = m_state->depth;
	return d;
}

#endif


// Added setting current coord on 11-19-08 
// Change this name.INHABITINF
void CAnimat::SetInitialConditions(INHABITINF IS) 
{
	m_currentCoord.lat = m_seedingCoord.lat = m_initialCoord.lat = IS.coord.lat;
	m_currentCoord.lon = m_seedingCoord.lon = m_initialCoord.lon = IS.coord.lon;

	m_acstcSrcInf = IS.acstcSrc;
}



// Change this name.
INHABITINF CAnimat::GetInhabitantSpecific()
{
	INHABITINF d = {0};
	d.coord.lat = m_seedingCoord.lat;
	d.coord.lon = m_seedingCoord.lon;
	d.acstcSrc = m_acstcSrcInf;
	return d;
}



/*----------------------------------------------------------------------------------------
  MEMBER FUNCTION: SetAcousticExposure()
 
  DESCRIPTION:
    When called, this instance of CAnimat retrieves from the universal exposure buffer its
	current momentary acoustic exposure.  This function exists as part of the ESME-3MBS
	interface.

  INPUT PARAMETER(S):
	A pointer to the universal acoustic exposure buffer.

  RETURN VALUE:
	None.

  REVISION HISTORY:
     Date    Name   Change
   --------  ----   ------
   05/24/05  (MJC)  Initial Coding/Transfer
----------------------------------------------------------------------------------------*/



void CAnimat::SetBathymetryDepth(double Value)
{
	m_initialBathyDepth = Value;
	m_bathyDepthSet = TRUE;
}


BOOL CAnimat::IsBeached(){return m_state->beached;}




/*****************************************************************************************
* CLASS METHOD:
*	CAnimat::DepthCalc()
* 
* ARGUMENTS:
*	None -- This is a class function using only class variables.
*
* RETURN VALUE:
*   Returns type double, the calculated depth.
*
* MEMBER VARIABLES ALTERED:
*	None  - 
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   05/17/05  (MJC)  Initial Transfer
*
* DESCRIPTION
*	Part of the CAnimat Utility Routines.
*   Returns a depth calculation based upon it's current dive rate, current
*	depth, and dive direction (acending or decending).  
*
* CORRESPONDING FUNCTION IN ORIGINAL 3MB PROGRAM, IF ANY
*	DepthCalc() in file vertical_movement.cpp\
*
*****************************************************************************************/
double CAnimat::DepthCalc(double Rate, double Depth, ACTIVITY Activity)
{
	double newDepth = Depth;

	if(Activity == ASCENDING || Activity == ASCENDING_REVERSAL)
		newDepth = newDepth + Rate;
	else if(Activity == DESCENDING || Activity == DESCENDING_REVERSAL || Activity == BOTTOM_FOLLOWING)
		newDepth = newDepth - Rate;

	if(newDepth > 0)
		newDepth = 0.0;
	return newDepth;
}

//--------------------------------------------------------------------------------------//
// Animat Statics
//--------------------------------------------------------------------------------------//
CAnimatStatics::CAnimatStatics(){}
CAnimatStatics::~CAnimatStatics(){}

DWORD CAnimatStatics::PackageStateData(PACKAGED_STATE_DATA Data)
{
	// See dataTypes.h for packed data format.
	DWORD p; // 32 bytes
	p = 0;

	p |= (Data.behavior & 0x7f) << 0;
	p |= (Data.threshFlagAPhy & 0x1)  << 7;
	p |= (Data.diveActivity & 0x7)  << 8;
	p |= (Data.overrideBehavior & 0x1f) << 11;
	p |= (Data.threshFlagBPhy & 0x1) << 16;
	p |= (Data.threshFlagBBeh & 0x1) << 17;
	p |= (Data.offScreenAnimatReset & 0x1) << 18;
	p |= (Data.nextBehaviorSelected & 0x1) << 19;
	p |= (Data.nextBehavior & 0x7f) << 20;

	PACKAGED_STATE_DATA z = UnPackageStateData(p);
	_ASSERT(z.behavior == Data.behavior);
	_ASSERT(z.threshFlagAPhy == Data.threshFlagAPhy);
	_ASSERT(z.diveActivity == Data.diveActivity);
	_ASSERT(z.overrideBehavior == Data.overrideBehavior);
	_ASSERT(z.threshFlagBPhy == Data.threshFlagBPhy);
	_ASSERT(z.threshFlagBBeh == Data.threshFlagBBeh);
	_ASSERT(z.offScreenAnimatReset == Data.offScreenAnimatReset);
	_ASSERT(z.nextBehaviorSelected == Data.nextBehaviorSelected);
	if(z.nextBehaviorSelected == TRUE)
		_ASSERT(z.nextBehavior == (int)Data.nextBehavior);
	else
		_ASSERT(z.nextBehavior == -1); // -1 used when next behavior hasn't been determined.

	return p;
}

PACKAGED_STATE_DATA CAnimatStatics::UnPackageStateData(DWORD Data)
{
	PACKAGED_STATE_DATA p;
	memset(&p, 0, sizeof(PACKAGED_STATE_DATA));
	p.behavior = (Data >> 0) & 0x7f;
	p.threshFlagAPhy = (Data >> 7) & 0x1;
	p.diveActivity = (ACTIVITY)((Data >> 8) & 0x7);
	p.overrideBehavior = ((Data >> 11) & 0x1f);
	p.threshFlagBPhy = (Data >> 16) & 0x1;
	p.threshFlagBBeh = (Data >> 17) & 0x1;
	p.offScreenAnimatReset = (Data >> 18) & 0x1;
	p.nextBehaviorSelected = (Data >> 19) & 0x1;
	if(p.nextBehaviorSelected == TRUE)
		p.nextBehavior = (Data >> 20) & 0x7f;
	else
		p.nextBehavior = -1;
	return p;
}



