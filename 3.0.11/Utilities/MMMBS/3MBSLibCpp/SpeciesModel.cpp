// Species.cpp: implementation of the CSpeciesModel class.
//
//////////////////////////////////////////////////////////////////////
#include "SpeciesModel.h"
#include "3mbsLib.h"
#include "SpeciesUpgrader.h"

#define MAX_LINE_LENGTH 512
#define MAX_USER_LINE_LENGTH 500

#include <float.h>
#include ".\speciesmodel.h"

const TCHAR *szBinExtension  = ".mdl";
const TCHAR *szTxtExtension = ".txt";



//---------------------------------------------//
// Constructor, destructor, memory deallocation
//---------------------------------------------//
CSpeciesModel::CSpeciesModel()
{
	memset(&m_speciesModel, 0, sizeof(SPECIES_MDL));
	sprintf_s(m_speciesModel.description.szHeader, sizeof(m_speciesModel.description.szHeader), "%s", SZ_SPECIESSPECIFICATION_HEADER);

	_ASSERT(sizeof(SPECIESSPECIFICATION)%16 == 0);
	_ASSERT(sizeof(_fSCEPARMSSPECIESGROUP)%16 == 0);
	_ASSERT(sizeof(BINOUT_INF)%16 == 0);
	_ASSERT(sizeof(BINOUT_SIZE)%16 == 0);
	_ASSERT(sizeof(BINARYSETUP)%16 == 0);
	_ASSERT(sizeof(_fSCENARIOPARAMS)%16 == 0);
	_ASSERT(sizeof(ANIMATASSCN)%16 == 0);
	_ASSERT(sizeof(SPECIESBINOUTINF)%16 == 0);
	_ASSERT(sizeof(SPECIES_MDL)%16 == 0);
	ClearMemberVariables();
}

CSpeciesModel::~CSpeciesModel()
{
	ClearMemberVariables();
}



/*----------------------------------------------------------------------------------------
  MEMBER FUNCTION: CSpeciesModel::ClearMemberVariables()
 
  DESCRIPTION:
    Deallocates all dynamically allocated memory used in class SpeciesModel, sets all
	associated pointers to dynamic memory to NULL, sets all other values to defaults.

  INPUT PARAMETER(S):
	None

  RETURN VALUE:
	None.

  REVISION HISTORY:
     Date    Name   Change
   --------  ----   ------
   05/24/05  (MJC)  Initial Coding/Transfer
----------------------------------------------------------------------------------------*/
void  CSpeciesModel::ClearMemberVariables()
{
	UINT32 i;
	INT32 j;
	//----------------------------------------------------------------------------------//
	// Deallocate dynamic memory
	//--------------------------//
	// Initial behavior matrix
	//FreeMatrix(&m_speciesModel.initialBehavior);
	for(j=0; j<m_speciesModel.initBehSpanCnt; j++)
	{
		FreeMatrix(&m_speciesModel.initialBehavior.arr[j].m);
	}
	delete [] m_speciesModel.initialBehavior.arr;
	m_speciesModel.initialBehavior.arr = NULL;

	// Behavior Transition matrix models
	for(i=0; i<m_speciesModel.description.numBehaviors; i++)
	{
		// Normal behavior transition
		for(j=0; j<m_speciesModel.p.behavior[i].nrmlBehTransCnt; j++)
		{
			FreeMatrix(&m_speciesModel.p.behavior[i].nrmlBehTrans.arr[j].m);
		}
		delete [] m_speciesModel.p.behavior[i].nrmlBehTrans.arr;
		m_speciesModel.p.behavior[i].nrmlBehTrans.arr = NULL;

		//FreeMatrix(&m_speciesModel.p.behavior[i].nrmlBehTrans.behavior);
		//FreeMatrix(&m_speciesModel.p.behavior[i].nrmlBehTrans.terminate);

		// Depth environmenatal attractor behavior transition
		FreeMatrix(&m_speciesModel.p.behavior[i].depthEnvAttBehTrans.behavior);
		FreeMatrix(&m_speciesModel.p.behavior[i].depthEnvAttBehTrans.terminate);

		// Depth temperature attractor behavior transition
		FreeMatrix(&m_speciesModel.p.behavior[i].tempEnvAttBehTrans.behavior);
		FreeMatrix(&m_speciesModel.p.behavior[i].tempEnvAttBehTrans.terminate);

		// Travel Rate
		FreeMatrix(&m_speciesModel.p.behavior[i].travelRate.vm.step);
		FreeMatrix(&m_speciesModel.p.behavior[i].travelRate.vm.terminate);
		FreeMatrix(&m_speciesModel.p.behavior[i].travelRate.vm.vector);

		// Travel direction
		FreeMatrix(&m_speciesModel.p.behavior[i].travelDirection.vm.direction);
		FreeMatrix(&m_speciesModel.p.behavior[i].travelDirection.vm.directionalBias);
		FreeMatrix(&m_speciesModel.p.behavior[i].travelDirection.vm.terminate);

		// Descent rate
		FreeMatrix(&m_speciesModel.p.behavior[i].dive.descentRate.vm.step);
		FreeMatrix(&m_speciesModel.p.behavior[i].dive.descentRate.vm.terminate);
		FreeMatrix(&m_speciesModel.p.behavior[i].dive.descentRate.vm.vector);

		// Ascent rate
		FreeMatrix(&m_speciesModel.p.behavior[i].dive.ascentRate.vm.step);
		FreeMatrix(&m_speciesModel.p.behavior[i].dive.ascentRate.vm.terminate);
		FreeMatrix(&m_speciesModel.p.behavior[i].dive.ascentRate.vm.vector);

		// Surface Interval
		FreeMatrix(&m_speciesModel.p.behavior[i].dive.srfInv.vm.step);
		FreeMatrix(&m_speciesModel.p.behavior[i].dive.srfInv.vm.vector);

		// Reversal
		FreeMatrix(&m_speciesModel.p.behavior[i].dive.reversal.vm.count);
		FreeMatrix(&m_speciesModel.p.behavior[i].dive.reversal.vm.probOfReversal);
		FreeMatrix(&m_speciesModel.p.behavior[i].dive.reversal.vm.time);
		FreeMatrix(&m_speciesModel.p.behavior[i].dive.reversal.vm.timeStep);

		// Depth Model
		FreeMatrix(&m_speciesModel.p.behavior[i].dive.depth.vm.step);
		FreeMatrix(&m_speciesModel.p.behavior[i].dive.depth.vm.vector);
	}
	free(m_speciesModel.p.behavior);
	memset(&m_speciesModel, 0, sizeof(SPECIES_MDL));
}

BOOL CSpeciesModel::DefaultMemberVariables() // change to two functions, clear and default.
{
	//----------------------------------------------------------------------------------//
	// Set sub models and their struct variables to appropriate/default values
	//------------------------------------------------------------------------//
	// Note that each sub model may have a variable step that is a struct ELEMENT.
	// ELEMENTs, unlike thier related structures MATRIX and ARRAY, have no dynamically
	// allocated memory.  Instead, their structs have an int for number of rows, an int
	// for number of columns, and a single statically allocated double.  Therefore, set
	// each sub model's step variable to have a single row and a single column to indicate
	// that the double is a matrix with a single row and a single column.

	//------------------//
	// Acoustic Aversion
	//------------------//
//	m_speciesModel.acousticAversion.actThreshA = 56;
//	m_speciesModel.acousticAversion.actThreshB = 56;
	m_speciesModel.acousticAversion.ascentRate.coeff = 4;
	m_speciesModel.acousticAversion.ascentRate.mean = 4;
	m_speciesModel.acousticAversion.ascentRate.std = 4;
	m_speciesModel.acousticAversion.ascentRateAffected = FALSE;
	m_speciesModel.acousticAversion.beaches = FALSE;
	m_speciesModel.acousticAversion.beachingDepth = ANIMAT_BEACHES_DEPTH_DEFAULT;
//	m_speciesModel.acousticAversion.deactThreshA = 0;
//	m_speciesModel.acousticAversion.deactThreshB = 0;
//	m_speciesModel.acousticAversion.decayfncA = FNCa;
//	m_speciesModel.acousticAversion.decayfncB = FNCa;
	m_speciesModel.acousticAversion.depth.coeff = 4;
	m_speciesModel.acousticAversion.depth.mean = 4;
	m_speciesModel.acousticAversion.depth.std = 4;
	m_speciesModel.acousticAversion.depthAffected = FALSE;
	m_speciesModel.acousticAversion.descentRate.coeff =4;
	m_speciesModel.acousticAversion.descentRate.mean = 4;
	m_speciesModel.acousticAversion.descentRate.std = 4;
	m_speciesModel.acousticAversion.descentRateAffected = FALSE;
	m_speciesModel.acousticAversion.flatBottomDiveAffected = TRUE;
	m_speciesModel.acousticAversion.flatBottomDives = FALSE;
	m_speciesModel.acousticAversion.podBreaksUp = FALSE;
	m_speciesModel.acousticAversion.reversal.count.coeff = 4;
	m_speciesModel.acousticAversion.reversal.count.mean = 4;
	m_speciesModel.acousticAversion.reversal.count.std = 4;
	m_speciesModel.acousticAversion.reversal.probOfReversal = 4;
	m_speciesModel.acousticAversion.reversal.time.coeff = 4;
	m_speciesModel.acousticAversion.reversal.time.mean = 4;
	m_speciesModel.acousticAversion.reversal.time.std = 4;
	m_speciesModel.acousticAversion.reversalAffected = FALSE;
	m_speciesModel.acousticAversion.surfaceInterval.coeff = 4;
	m_speciesModel.acousticAversion.surfaceInterval.mean = 4;
	m_speciesModel.acousticAversion.surfaceInterval.std = 4;
	m_speciesModel.acousticAversion.surfaceIntervalAffected = TRUE;
	m_speciesModel.acousticAversion.travel.arcStep = 4;
	m_speciesModel.acousticAversion.travel.bias = 4;
	m_speciesModel.acousticAversion.travel.directionOfBias = 4;
	m_speciesModel.acousticAversion.travel.perturbation = 4;
	m_speciesModel.acousticAversion.travel.termCoeff = 4;
	m_speciesModel.acousticAversion.travelDirectionAffected = TRUE;
	m_speciesModel.acousticAversion.travelRate.coeff = 4;
	m_speciesModel.acousticAversion.travelRate.mean = 4;
	m_speciesModel.acousticAversion.travelRate.std = 4;
	m_speciesModel.acousticAversion.travelRateAffected = TRUE;
//	m_speciesModel.acousticAversion.unitsPhys = unitsPhys;
//	m_speciesModel.acousticAversion.unitsBeh = unitsPhys;


	//----------------------------------//
	// Species Specification Description
	//----------------------------------//
	memset(&m_speciesModel.description, 0, sizeof(SPECIESSPECIFICATION));

	m_speciesModel.description.mbsVerSuper = MMBSLIB_VERSION_SUPER;
	m_speciesModel.description.mbsVerSub = MMBSLIB_VERSION_SUB;
	m_speciesModel.description.speVerSuper = MMMBLIB_SPECIES_VERSION_SUPER;
	m_speciesModel.description.speVerSub = MMMBLIB_SPECIES_VERSION_SUB;
	//sprintf_s(szBuff, SIZE_256, "");
	//strncpy_s(m_speciesModel.description.speciesShrtDscrptn, SPECIES_DESCRPTN_MAX_LEN, szBuff, strlen(szBuff));
	//sprintf_s(szBuff, SIZE_256, "");
	//strncpy_s(m_speciesModel.description.speciesComment, SPECIES_COMMENT_MAX_LEN, szBuff, strlen(szBuff));
	m_speciesModel.description.group = MYSTICETE;
	m_speciesModel.description.name = GENERIC_MYSTICETES;


	m_speciesModel.description.numBehaviors = 1;

	//-------------------//
	// Behaviors (normal)
	//-------------------//
	m_speciesModel.p.behavior = (NRMLBEHMDL *)malloc(m_speciesModel.description.numBehaviors * sizeof(NRMLBEHMDL));
	memset(m_speciesModel.p.behavior, 0, 1 * sizeof(NRMLBEHMDL));

	// Depth environmental attractor stimulus.
	m_speciesModel.p.behavior[0].depthEnvAtt.shelfIsEnabled = FALSE;
	m_speciesModel.p.behavior[0].depthEnvAtt.shelfDepth = -550;
	m_speciesModel.p.behavior[0].depthEnvAtt.shelfSlope = 1.0;

	m_speciesModel.p.behavior[0].depthEnvAtt.basinIsEnabled = FALSE;
	m_speciesModel.p.behavior[0].depthEnvAtt.basinDepth = -550;
	m_speciesModel.p.behavior[0].depthEnvAtt.basinSlope = 1.0;

	m_speciesModel.p.behavior[0].depthEnvAtt.slopeIsEnabled = FALSE;
	m_speciesModel.p.behavior[0].depthEnvAtt.slopeDepth = -150;
	m_speciesModel.p.behavior[0].depthEnvAtt.slopeSlope = 1.0;

	// Depth environmental attractor behavior transition matrix.
	if(FALSE == AllocateMatrix(&m_speciesModel.p.behavior[0].depthEnvAttBehTrans.behavior, 1, 1))
		return FALSE;
	m_speciesModel.p.behavior[0].depthEnvAttBehTrans.behavior.p.pa[0] = 1;

	if(FALSE == AllocateMatrix(&m_speciesModel.p.behavior[0].depthEnvAttBehTrans.terminate, 1, 1))
		return FALSE;
	m_speciesModel.p.behavior[0].depthEnvAttBehTrans.terminate.a = 1;

	m_speciesModel.p.behavior[0].depthEnvAttBehTrans.meanTimeInBeh = 60;
	m_speciesModel.p.behavior[0].depthEnvAttBehTrans.slopeCoefficient = 7;



	// Environmental attractor priority
	m_speciesModel.p.behavior[0].depthHasPriorityOverTemp = TRUE;


	// Dive ascent rate
	m_speciesModel.p.behavior[0].dive.ascentRate.gauss.coeff = 4;
	m_speciesModel.p.behavior[0].dive.ascentRate.gauss.mean = 4;
	m_speciesModel.p.behavior[0].dive.ascentRate.gauss.std = 4;
	m_speciesModel.p.behavior[0].dive.ascentRate.modelType = GAUSSIAN;
	m_speciesModel.p.behavior[0].dive.ascentRate.rnd.coeff = 4;
	m_speciesModel.p.behavior[0].dive.ascentRate.rnd.max = 4;
	m_speciesModel.p.behavior[0].dive.ascentRate.rnd.min = 4;
	if(FALSE == AllocateMatrix(&m_speciesModel.p.behavior[0].dive.ascentRate.vm.step, 1, 1))
		return FALSE;
	m_speciesModel.p.behavior[0].dive.ascentRate.vm.step.a = 1;
	if(FALSE == AllocateMatrix(&m_speciesModel.p.behavior[0].dive.ascentRate.vm.terminate, 1, 1))
		return FALSE;
	m_speciesModel.p.behavior[0].dive.ascentRate.vm.terminate.a = 1;
	if(FALSE == AllocateMatrix(&m_speciesModel.p.behavior[0].dive.ascentRate.vm.vector, 1, 1))
		return FALSE;
	m_speciesModel.p.behavior[0].dive.ascentRate.vm.vector.p.pa[0] = 1;

	// Dive Bottom Following
	memset(&m_speciesModel.p.behavior[0].dive.bttmFollow, 0, sizeof(BTTMFLLW));
	m_speciesModel.p.behavior[0].dive.bttmFollow.type = NO_BTTMFLLWNG;

	// Dive Depth
	m_speciesModel.p.behavior[0].dive.depth.gauss.coeff = 4;
	m_speciesModel.p.behavior[0].dive.depth.gauss.mean = 500.0;
	m_speciesModel.p.behavior[0].dive.depth.gauss.std = 50;
	m_speciesModel.p.behavior[0].dive.depth.modelType = GAUSSIAN;
	m_speciesModel.p.behavior[0].dive.depth.rnd.coeff = 4;
	m_speciesModel.p.behavior[0].dive.depth.rnd.max = 4;
	m_speciesModel.p.behavior[0].dive.depth.rnd.min = 4;
	if(FALSE == AllocateMatrix(&m_speciesModel.p.behavior[0].dive.depth.vm.step, 1, 1))
		return FALSE;
	m_speciesModel.p.behavior[0].dive.depth.vm.step.a = 1;
	if(FALSE == AllocateMatrix(&m_speciesModel.p.behavior[0].dive.depth.vm.vector, 1, 1))
		return FALSE;
	m_speciesModel.p.behavior[0].dive.depth.vm.vector.p.pa[0] = 1;

	// Dive descent rate
	m_speciesModel.p.behavior[0].dive.descentRate.gauss.coeff = 4;
	m_speciesModel.p.behavior[0].dive.descentRate.gauss.mean = 4;
	m_speciesModel.p.behavior[0].dive.descentRate.gauss.std = 4;
	m_speciesModel.p.behavior[0].dive.descentRate.modelType = GAUSSIAN;
	m_speciesModel.p.behavior[0].dive.descentRate.rnd.coeff = 4;
	m_speciesModel.p.behavior[0].dive.descentRate.rnd.max = 4;
	m_speciesModel.p.behavior[0].dive.descentRate.rnd.min = 4;
	if(FALSE == AllocateMatrix(&m_speciesModel.p.behavior[0].dive.descentRate.vm.step, 1, 1))
		return FALSE;
	m_speciesModel.p.behavior[0].dive.descentRate.vm.step.a = 1;
	if(FALSE == AllocateMatrix(&m_speciesModel.p.behavior[0].dive.descentRate.vm.terminate, 1, 1))
		return FALSE;
	m_speciesModel.p.behavior[0].dive.descentRate.vm.terminate.a = 1;
	if(FALSE == AllocateMatrix(&m_speciesModel.p.behavior[0].dive.descentRate.vm.vector, 1 ,1))
		return FALSE;
	m_speciesModel.p.behavior[0].dive.descentRate.vm.vector.p.pa[0] = 1;

	// Dive Reversals dive rate
	m_speciesModel.p.behavior[0].dive.reversal.diveRate.coeff = 4;
	m_speciesModel.p.behavior[0].dive.reversal.diveRate.mean = 4;
	m_speciesModel.p.behavior[0].dive.reversal.diveRate.std = 4;

	// Dive Reversals Reversal count/duration
	m_speciesModel.p.behavior[0].dive.reversal.modelType = GAUSSIAN;
	m_speciesModel.p.behavior[0].dive.reversal.gauss.count.coeff = 4;
	m_speciesModel.p.behavior[0].dive.reversal.gauss.count.mean = 4;
	m_speciesModel.p.behavior[0].dive.reversal.gauss.count.std = 4;
	m_speciesModel.p.behavior[0].dive.reversal.gauss.probOfReversal = 4;
	m_speciesModel.p.behavior[0].dive.reversal.gauss.time.coeff = 4;
	m_speciesModel.p.behavior[0].dive.reversal.gauss.time.mean = 4;
	m_speciesModel.p.behavior[0].dive.reversal.gauss.time.std = 4;
	m_speciesModel.p.behavior[0].dive.reversal.rnd.count.max = 4;
	m_speciesModel.p.behavior[0].dive.reversal.rnd.count.min = 4;
	m_speciesModel.p.behavior[0].dive.reversal.rnd.probOfReversal = 4;
	m_speciesModel.p.behavior[0].dive.reversal.rnd.time.coeff = 4;
	m_speciesModel.p.behavior[0].dive.reversal.rnd.time.mean = 4;
	m_speciesModel.p.behavior[0].dive.reversal.rnd.time.std = 4;
	if(FALSE == AllocateMatrix(&m_speciesModel.p.behavior[0].dive.reversal.vm.count, 1, 1))
		return FALSE;
	m_speciesModel.p.behavior[0].dive.reversal.vm.count.p.pa[0] = 1;
	if(FALSE == AllocateMatrix(&m_speciesModel.p.behavior[0].dive.reversal.vm.probOfReversal, 1, 1))
		return FALSE;
	m_speciesModel.p.behavior[0].dive.reversal.vm.probOfReversal.a = 1;
	if(FALSE == AllocateMatrix(&m_speciesModel.p.behavior[0].dive.reversal.vm.time, 1, 1))
		return FALSE;
	m_speciesModel.p.behavior[0].dive.reversal.vm.time.p.pa[0] = 1;
	if(FALSE == AllocateMatrix(&m_speciesModel.p.behavior[0].dive.reversal.vm.timeStep, 1, 1))
		return FALSE;
	m_speciesModel.p.behavior[0].dive.reversal.vm.timeStep.a = 1;

	// Dive Surface interval
	m_speciesModel.p.behavior[0].dive.srfInv.gauss.coeff = 4;
	m_speciesModel.p.behavior[0].dive.srfInv.gauss.mean = 4;
	m_speciesModel.p.behavior[0].dive.srfInv.gauss.std = 4;
	m_speciesModel.p.behavior[0].dive.srfInv.modelType = GAUSSIAN;
	if(FALSE == AllocateMatrix(&m_speciesModel.p.behavior[0].dive.srfInv.vm.step, 1, 1))
		return FALSE;
	m_speciesModel.p.behavior[0].dive.srfInv.vm.step.a = 1;
	if(FALSE == AllocateMatrix(&m_speciesModel.p.behavior[0].dive.srfInv.vm.vector, 1, 1))
		return FALSE;
	m_speciesModel.p.behavior[0].dive.srfInv.vm.vector.p.pa[0] = 1;

	// 6 in the following allocation because want to allocate a matrix for one behavior.
	// In the behavior transition matrix there are number of behaviors + 5 columns.
	// [Start Time][End Time][Zero Element][Behavior][T50][Slope]
	m_speciesModel.p.behavior[0].nrmlBehTransCnt = 1;
	m_speciesModel.p.behavior[0].nrmlBehTrans.arr = new BEHTRAN[m_speciesModel.p.behavior[0].nrmlBehTransCnt];
	memset(m_speciesModel.p.behavior[0].nrmlBehTrans.arr, 0, sizeof(BEHTRAN)* m_speciesModel.p.behavior[0].nrmlBehTransCnt);
	if(FALSE == AllocateMatrix(&m_speciesModel.p.behavior[0].nrmlBehTrans.arr[0].m, 1, 6))
		return FALSE;
	// row 0, and fill in the columns with initial values.
	m_speciesModel.p.behavior[0].nrmlBehTrans.arr[0].depthSpan.shallow = DEFAULT_DEPTH_SPAN_SHALLOW;
	m_speciesModel.p.behavior[0].nrmlBehTrans.arr[0].depthSpan.deep = DEFAULT_DEPTH_SPAN_DEEP;
	m_speciesModel.p.behavior[0].nrmlBehTrans.arr[0].m.p.ppa[0][0] = 0.0; // start time of transition
	m_speciesModel.p.behavior[0].nrmlBehTrans.arr[0].m.p.ppa[0][1] = 0.0; // end time of transition
	m_speciesModel.p.behavior[0].nrmlBehTrans.arr[0].m.p.ppa[0][2] = 0.0; // zero probability element
	m_speciesModel.p.behavior[0].nrmlBehTrans.arr[0].m.p.ppa[0][3] = 1.0; // probability of transition to behavior 1 probability
	m_speciesModel.p.behavior[0].nrmlBehTrans.arr[0].m.p.ppa[0][4] = 60.0; // T50, mean time in behavior (minuites)
	m_speciesModel.p.behavior[0].nrmlBehTrans.arr[0].m.p.ppa[0][5] = 7.0; // k, slope

	// Behavior Name
	strncpy_s(m_speciesModel.p.behavior[0].szName, sizeof(m_speciesModel.p.behavior[0].szName),
		"Behavior 1", strlen("Behavior 1"));

	// Temperature Environmental Attractor
	m_speciesModel.p.behavior[0].tempEnvAtt.delta = 4;
	m_speciesModel.p.behavior[0].tempEnvAtt.max = 4;
	m_speciesModel.p.behavior[0].tempEnvAtt.min = 4;

	// Temperature Environmental Attractor behavior transition
	if(FALSE == AllocateMatrix(&m_speciesModel.p.behavior[0].tempEnvAttBehTrans.behavior, 1, 1))
		return FALSE;
	m_speciesModel.p.behavior[0].tempEnvAttBehTrans.behavior.p.pa[0] = 1;
	if(FALSE == AllocateMatrix(&m_speciesModel.p.behavior[0].tempEnvAttBehTrans.terminate, 1, 1))
		return FALSE;
	m_speciesModel.p.behavior[0].tempEnvAttBehTrans.terminate.a = 1;

	m_speciesModel.p.behavior[0].tempEnvAttBehTrans.meanTimeInBeh = 60;
	m_speciesModel.p.behavior[0].tempEnvAttBehTrans.slopeCoefficient = 7;

	// Travel Direction
	m_speciesModel.p.behavior[0].travelDirection.crRndWalk.perturbation = 4;
	m_speciesModel.p.behavior[0].travelDirection.crRndWalk.termCoeff = 4;
	m_speciesModel.p.behavior[0].travelDirection.crRndWalkDb.arcStep = 4;
	m_speciesModel.p.behavior[0].travelDirection.crRndWalkDb.bias = 4;
	m_speciesModel.p.behavior[0].travelDirection.crRndWalkDb.directionOfBias = 4;
	m_speciesModel.p.behavior[0].travelDirection.crRndWalkDb.perturbation = 4;
	m_speciesModel.p.behavior[0].travelDirection.crRndWalkDb.termCoeff = 4;
	m_speciesModel.p.behavior[0].travelDirection.modelType = RANDOM_WALK;
	m_speciesModel.p.behavior[0].travelDirection.rndWalk.termCoeff = 4;
	if(FALSE == AllocateMatrix(&m_speciesModel.p.behavior[0].travelDirection.vm.direction, 1, 1))
		return FALSE;
	m_speciesModel.p.behavior[0].travelDirection.vm.direction.p.pa[0] = 1;
	if(FALSE == AllocateMatrix(&m_speciesModel.p.behavior[0].travelDirection.vm.directionalBias, 1, 1))
		return FALSE;
	m_speciesModel.p.behavior[0].travelDirection.vm.directionalBias.p.ppa[0][0] = 1;
	if(FALSE == AllocateMatrix(&m_speciesModel.p.behavior[0].travelDirection.vm.terminate, 1, 1))
		return FALSE;
	m_speciesModel.p.behavior[0].travelDirection.vm.terminate.a = 1;

	// Travel Rate
	m_speciesModel.p.behavior[0].travelRate.gauss.coeff = 4;
	m_speciesModel.p.behavior[0].travelRate.gauss.mean = 4;
	m_speciesModel.p.behavior[0].travelRate.gauss.std = 4;
	m_speciesModel.p.behavior[0].travelRate.modelType = GAUSSIAN;
	m_speciesModel.p.behavior[0].travelRate.rnd.coeff = 4;
	m_speciesModel.p.behavior[0].travelRate.rnd.max = 4;
	m_speciesModel.p.behavior[0].travelRate.rnd.min = 4;
	if(FALSE == AllocateMatrix(&m_speciesModel.p.behavior[0].travelRate.vm.step, 1, 1))
		return FALSE;
	m_speciesModel.p.behavior[0].travelRate.vm.step.a = 1;
	if(FALSE == AllocateMatrix(&m_speciesModel.p.behavior[0].travelRate.vm.terminate, 1, 1))
		return FALSE;
	m_speciesModel.p.behavior[0].travelRate.vm.terminate.a = 1;
	if(FALSE == AllocateMatrix(&m_speciesModel.p.behavior[0].travelRate.vm.vector, 1, 1))
		return FALSE;
	m_speciesModel.p.behavior[0].travelRate.vm.vector.p.pa[0] = 1;

	//---------------//
	// Behavior Count
	//---------------//
	m_speciesModel.description.numBehaviors = 1;

	//-----------------//
	// Initial Behavior
	//-----------------//
	m_speciesModel.initBehSpanCnt = 1;
	m_speciesModel.initialBehavior.arr = new BEHTRAN[1];
	if(FALSE == AllocateMatrix(&m_speciesModel.initialBehavior.arr[0].m, 1, 4))
		return FALSE;
	m_speciesModel.initialBehavior.arr[0].m.p.ppa[0][0] = 0.0;
	m_speciesModel.initialBehavior.arr[0].m.p.ppa[0][1] = 24.0;
	m_speciesModel.initialBehavior.arr[0].m.p.ppa[0][2] = 0;
	m_speciesModel.initialBehavior.arr[0].m.p.ppa[0][3] = 1;
	m_speciesModel.initialBehavior.arr[0].depthSpan.shallow = 0;
	m_speciesModel.initialBehavior.arr[0].depthSpan.deep = -3500;


//	if(FALSE == AllocateMatrix(&m_speciesModel.initialBehavior, 1, 4))
//		return FALSE;
//	m_speciesModel.initialBehavior.p.ppa[0][0] = 0.0;
//	m_speciesModel.initialBehavior.p.ppa[0][1] = 24.0;
//	m_speciesModel.initialBehavior.p.ppa[0][2] = 0;
//	m_speciesModel.initialBehavior.p.ppa[0][3] = 1;

	//----------------//
	// Shore Following
	//----------------//
	m_speciesModel.description.shoreFollowDepth = SHORE_FOLLOW_DEPTH_DEFAULT;

	// Need Save
	m_needSave = FALSE;
	return TRUE;
}




/*----------------------------------------------------------------------------------------
  MEMBER FUNCTION: ProbTermination()
 
  DESCRIPTION:
    XXX

  INPUT PARAMETER(S):
	XXX

  RETURN VALUE:
	XXX

  REVISION HISTORY:
     Date    Name   Change
   --------  ----   ------
   05/24/05  (MJC)  Initial Coding/Transfer
----------------------------------------------------------------------------------------*/
void CSpeciesModel::CopyModel(CSpeciesModel *pCpyMdl, const CSpeciesModel *pMdl)
{
	UINT32 i;
	INT32 j;
	if(pCpyMdl == NULL)
		return;
	pCpyMdl->ClearMemberVariables();

	if(pMdl == NULL)
		return;

	NRMLBEHMDL *dest;
	NRMLBEHMDL *srce;

	//----------------------------------------------------------------------------------//
	// Behavior Transition
	//---------//
	// Copy the struct fields.
	memcpy(&pCpyMdl->m_speciesModel, &pMdl->m_speciesModel, sizeof(SPECIES_MDL));
	pCpyMdl->m_speciesModel.p.behavior =
		(NRMLBEHMDL *)malloc(pCpyMdl->m_speciesModel.description.numBehaviors * sizeof(NRMLBEHMDL));

	for(i=0; i<pCpyMdl->m_speciesModel.description.numBehaviors; i++)
	{
		dest = &pCpyMdl->m_speciesModel.p.behavior[i];
		srce = &pMdl->m_speciesModel.p.behavior[i];

		//--------------------------------//
		// Clear incorrect memory pointers
		//--------------------------------//
		// Normal behavior transition
		for(j=0; j<dest->nrmlBehTransCnt; j++)
			ClearMatrix(&dest->nrmlBehTrans.arr[j].m);
		delete [] dest->nrmlBehTrans.arr;
		dest->nrmlBehTrans.arr = NULL;
			

//		ClearMatrix(&dest->nrmlBehTransMatrix);
//		ClearMatrix(&dest->nrmlBehTrans.behavior);
//		ClearMatrix(&dest->nrmlBehTrans.terminate);

		// Depth environmenatal attractor behavior transition
		ClearMatrix(&dest->depthEnvAttBehTrans.behavior);
		ClearMatrix(&dest->depthEnvAttBehTrans.terminate);

		// Depth temperature attractor behavior transition
		ClearMatrix(&dest->tempEnvAttBehTrans.behavior);
		ClearMatrix(&dest->tempEnvAttBehTrans.terminate);

		// Travel Rate
		ClearMatrix(&dest->travelRate.vm.step);
		ClearMatrix(&dest->travelRate.vm.terminate);
		ClearMatrix(&dest->travelRate.vm.vector);

		// Travel direction
		ClearMatrix(&dest->travelDirection.vm.direction);
		ClearMatrix(&dest->travelDirection.vm.directionalBias);
		ClearMatrix(&dest->travelDirection.vm.terminate);

		// Descent rate
		ClearMatrix(&dest->dive.descentRate.vm.step);
		ClearMatrix(&dest->dive.descentRate.vm.terminate);
		ClearMatrix(&dest->dive.descentRate.vm.vector);

		// Ascent rate
		ClearMatrix(&dest->dive.ascentRate.vm.step);
		ClearMatrix(&dest->dive.ascentRate.vm.terminate);
		ClearMatrix(&dest->dive.ascentRate.vm.vector);

		// Surface Interval
		ClearMatrix(&dest->dive.srfInv.vm.step);
		ClearMatrix(&dest->dive.srfInv.vm.vector);

		// Reversal
		ClearMatrix(&dest->dive.reversal.vm.count);
		ClearMatrix(&dest->dive.reversal.vm.probOfReversal);
		ClearMatrix(&dest->dive.reversal.vm.time);
		ClearMatrix(&dest->dive.reversal.vm.timeStep);

		// Depth Model
		ClearMatrix(&dest->dive.depth.vm.step);
		ClearMatrix(&dest->dive.depth.vm.vector);


		//----------------//
		// Allocate Memory
		//----------------//
		dest->nrmlBehTransCnt = srce->nrmlBehTransCnt;
		dest->nrmlBehTrans.arr = new BEHTRAN[srce->nrmlBehTransCnt];
		memset(dest->nrmlBehTrans.arr, 0, sizeof(BEHTRAN)* srce->nrmlBehTransCnt);
		for(j=0; j<srce->nrmlBehTransCnt; j++)
			AllocateMatrix(&dest->nrmlBehTrans.arr[j].m, srce->nrmlBehTrans.arr[j].m.rowCnt, srce->nrmlBehTrans.arr[j].m.colCnt);
//		AllocateMatrix(&dest->nrmlBehTrans.behavior, srce->nrmlBehTrans.behavior.rowCnt, srce->nrmlBehTrans.behavior.colCnt);
//		AllocateMatrix(&dest->nrmlBehTrans.terminate, srce->nrmlBehTrans.terminate.rowCnt, srce->nrmlBehTrans.terminate.colCnt);

		// Depth environmenatal attractor behavior transition
		AllocateMatrix(&dest->depthEnvAttBehTrans.behavior, srce->depthEnvAttBehTrans.behavior.rowCnt, srce->depthEnvAttBehTrans.behavior.colCnt);
		AllocateMatrix(&dest->depthEnvAttBehTrans.terminate, srce->depthEnvAttBehTrans.terminate.rowCnt, srce->depthEnvAttBehTrans.terminate.colCnt);

		// Depth temperature attractor behavior transition
		AllocateMatrix(&dest->tempEnvAttBehTrans.behavior, srce->tempEnvAttBehTrans.behavior.rowCnt, srce->tempEnvAttBehTrans.behavior.colCnt);
		AllocateMatrix(&dest->tempEnvAttBehTrans.terminate, srce->tempEnvAttBehTrans.terminate.rowCnt, srce->tempEnvAttBehTrans.terminate.colCnt);

		// Travel Rate
		AllocateMatrix(&dest->travelRate.vm.step, srce->travelRate.vm.step.rowCnt, srce->travelRate.vm.step.colCnt);
		AllocateMatrix(&dest->travelRate.vm.terminate, srce->travelRate.vm.terminate.rowCnt, srce->travelRate.vm.terminate.colCnt);
		AllocateMatrix(&dest->travelRate.vm.vector, srce->travelRate.vm.vector.rowCnt, srce->travelRate.vm.vector.colCnt);

		// Travel direction
		AllocateMatrix(&dest->travelDirection.vm.direction, srce->travelDirection.vm.direction.rowCnt, srce->travelDirection.vm.direction.colCnt);
		AllocateMatrix(&dest->travelDirection.vm.directionalBias, srce->travelDirection.vm.directionalBias.rowCnt, srce->travelDirection.vm.directionalBias.colCnt);
		AllocateMatrix(&dest->travelDirection.vm.terminate, srce->travelDirection.vm.terminate.rowCnt, srce->travelDirection.vm.terminate.colCnt);

		// Descent rate
		AllocateMatrix(&dest->dive.descentRate.vm.step, srce->dive.descentRate.vm.step.rowCnt, srce->dive.descentRate.vm.step.colCnt);
		AllocateMatrix(&dest->dive.descentRate.vm.terminate, srce->dive.descentRate.vm.terminate.rowCnt, srce->dive.descentRate.vm.terminate.colCnt);
		AllocateMatrix(&dest->dive.descentRate.vm.vector, srce->dive.descentRate.vm.vector.rowCnt, srce->dive.descentRate.vm.vector.colCnt);

		// Ascent rate
		AllocateMatrix(&dest->dive.ascentRate.vm.step, srce->dive.ascentRate.vm.step.rowCnt, srce->dive.ascentRate.vm.step.colCnt);
		AllocateMatrix(&dest->dive.ascentRate.vm.terminate, srce->dive.ascentRate.vm.terminate.rowCnt, srce->dive.ascentRate.vm.terminate.colCnt);
		AllocateMatrix(&dest->dive.ascentRate.vm.vector, srce->dive.ascentRate.vm.vector.rowCnt, srce->dive.ascentRate.vm.vector.colCnt);

		// Surface Interval
		AllocateMatrix(&dest->dive.srfInv.vm.step, srce->dive.srfInv.vm.step.rowCnt, srce->dive.srfInv.vm.step.colCnt);
		AllocateMatrix(&dest->dive.srfInv.vm.vector, srce->dive.srfInv.vm.vector.rowCnt, srce->dive.srfInv.vm.vector.colCnt);

		// Reversal
		AllocateMatrix(&dest->dive.reversal.vm.count, srce->dive.reversal.vm.count.rowCnt, srce->dive.reversal.vm.count.colCnt);
		AllocateMatrix(&dest->dive.reversal.vm.probOfReversal, srce->dive.reversal.vm.probOfReversal.rowCnt, srce->dive.reversal.vm.probOfReversal.colCnt);
		AllocateMatrix(&dest->dive.reversal.vm.time, srce->dive.reversal.vm.time.rowCnt, srce->dive.reversal.vm.time.colCnt);
		AllocateMatrix(&dest->dive.reversal.vm.timeStep, srce->dive.reversal.vm.timeStep.rowCnt, srce->dive.reversal.vm.timeStep.colCnt);

		// Depth Model
		AllocateMatrix(&dest->dive.depth.vm.step, srce->dive.depth.vm.step.rowCnt, srce->dive.depth.vm.step.colCnt);
		AllocateMatrix(&dest->dive.depth.vm.vector, srce->dive.depth.vm.vector.rowCnt, srce->dive.depth.vm.vector.colCnt);

		//--------------//
		// Copy Models
		//--------------//
		// Normal behavior transition
		for(j=0; j<srce->nrmlBehTransCnt; j++)
		{
			dest->nrmlBehTrans.arr[j].depthSpan = srce->nrmlBehTrans.arr[j].depthSpan;
			CopyMatrix(&dest->nrmlBehTrans.arr[j].m, &srce->nrmlBehTrans.arr[j].m);
		}

//		CopyMatrix(&dest->nrmlBehTransMatrix, &srce->nrmlBehTransMatrix);

//		CopyMatrix(&dest->nrmlBehTrans.behavior, &srce->nrmlBehTrans.behavior);
//		CopyMatrix(&dest->nrmlBehTrans.terminate, &srce->nrmlBehTrans.terminate);
//		dest->nrmlBehTrans.meanTimeInBeh = srce->nrmlBehTrans.meanTimeInBeh;
//		dest->nrmlBehTrans.slopeCoefficient = srce->nrmlBehTrans.slopeCoefficient;

		// Depth environmenatal attractor behavior transition
		CopyMatrix(&dest->depthEnvAttBehTrans.behavior, &srce->depthEnvAttBehTrans.behavior);
		CopyMatrix(&dest->depthEnvAttBehTrans.terminate, &srce->depthEnvAttBehTrans.terminate);

		dest->depthEnvAttBehTrans.meanTimeInBeh = srce->depthEnvAttBehTrans.meanTimeInBeh;
		dest->depthEnvAttBehTrans.slopeCoefficient = srce->depthEnvAttBehTrans.slopeCoefficient;

		// Depth temperature attractor behavior transition
		CopyMatrix(&dest->tempEnvAttBehTrans.behavior, &srce->tempEnvAttBehTrans.behavior);
		CopyMatrix(&dest->tempEnvAttBehTrans.terminate, &srce->tempEnvAttBehTrans.terminate);

		dest->tempEnvAttBehTrans.meanTimeInBeh = srce->tempEnvAttBehTrans.meanTimeInBeh;
		dest->tempEnvAttBehTrans.slopeCoefficient = srce->tempEnvAttBehTrans.slopeCoefficient;

		// Travel Rate
		CopyMatrix(&dest->travelRate.vm.step, &srce->travelRate.vm.step);
		CopyMatrix(&dest->travelRate.vm.terminate, &srce->travelRate.vm.terminate);
		CopyMatrix(&dest->travelRate.vm.vector, &srce->travelRate.vm.vector);

		// Travel direction
		CopyMatrix(&dest->travelDirection.vm.direction, &srce->travelDirection.vm.direction);
		CopyMatrix(&dest->travelDirection.vm.directionalBias, &srce->travelDirection.vm.directionalBias);
		CopyMatrix(&dest->travelDirection.vm.terminate, &srce->travelDirection.vm.terminate);

		// Descent rate
		CopyMatrix(&dest->dive.descentRate.vm.step, &srce->dive.descentRate.vm.step);
		CopyMatrix(&dest->dive.descentRate.vm.terminate, &srce->dive.descentRate.vm.terminate);
		CopyMatrix(&dest->dive.descentRate.vm.vector, &srce->dive.descentRate.vm.vector);

		// Ascent rate
		CopyMatrix(&dest->dive.ascentRate.vm.step, &srce->dive.ascentRate.vm.step);
		CopyMatrix(&dest->dive.ascentRate.vm.terminate, &srce->dive.ascentRate.vm.terminate);
		CopyMatrix(&dest->dive.ascentRate.vm.vector, &srce->dive.ascentRate.vm.vector);

		// Surface Interval
		CopyMatrix(&dest->dive.srfInv.vm.step, &srce->dive.srfInv.vm.step);
		CopyMatrix(&dest->dive.srfInv.vm.vector, &srce->dive.srfInv.vm.vector);

		// Reversal
		CopyMatrix(&dest->dive.reversal.vm.count, &srce->dive.reversal.vm.count);
		CopyMatrix(&dest->dive.reversal.vm.probOfReversal, &srce->dive.reversal.vm.probOfReversal);
		CopyMatrix(&dest->dive.reversal.vm.time, &srce->dive.reversal.vm.time);
		CopyMatrix(&dest->dive.reversal.vm.timeStep, &srce->dive.reversal.vm.timeStep);

		// Depth Model
		CopyMatrix(&dest->dive.depth.vm.step, &srce->dive.depth.vm.step);
		CopyMatrix(&dest->dive.depth.vm.vector, &srce->dive.depth.vm.vector);
	}
}




void CSpeciesModel::InitialBehaviorControlTest(SNGLBEHTRANSTRUCT *pBehTrans, MATRIX *M, int NumTrials)
{
	int i;
	int j;
	int clkStart;
	C3MBRandom Random;

	Random.mysrand(0);

	// Initialize the SNGLBEHTRANSTRUCT struct
	pBehTrans->cnt = new CNTBIN* [M->rowCnt]; // one for each transitional period
	for(i=0; i<M->rowCnt; i++)
	{
		pBehTrans->cnt[i] = new CNTBIN[NumTrials]; // one for each tiral.
		memset(pBehTrans->cnt[i], 0, sizeof(CNTBIN)*NumTrials);
	}

	// i indexes the transitional period being tested.
	// j indexes the trial being run on the ith transitional period.
	for(i=0; i<M->rowCnt; i++)
	{
		// index zero of the behavior transition matrix as a function of time holds the
		// starting clock time of a transition.  Time is in fractions of hours so mulitply
		// by 3660 to get seconds.
		clkStart = (int)floor(M->p.ppa[i][0]*3660);
		memset(pBehTrans->cnt[i], 0, sizeof(CNTBIN)*NumTrials);

		for(j=0; j<NumTrials; j++)
		{
			// Behavior transition returns -1 if no transition occurs.
			pBehTrans->cnt[i][j].trans = IntialBehavior(clkStart, &Random, M);
			pBehTrans->cnt[i][j].sec = 0; // holds no meaning for this test.
		}
	}
}
//////////////////

// returns the index of the starting behavior.
int CSpeciesModel::IntialBehavior(int AbsClock, C3MBRandom *pRefRand, MATRIX *Md)
{
	double clk;       /* time of day in seconds. */
	int    timeRow;   /* row */
	int    behvCol;   /* column */
	double dice;
	double start;     /* time of day in seconds. */
	double end;       /* time of day in seconds. */

	/* Input assertions. */
	_ASSERT(pRefRand);

	/* Initialize Variables. */
	clk = (double)m_staticLib.Time_To24HrClockSeconds(AbsClock)/3600.0; 
	dice = pRefRand->myrand();

	/* Match the timeRow of day with the appropriate start_matrix row. */
	timeRow = 0;
	while(timeRow+1 < Md->rowCnt)
	{
		start = Md->p.ppa[timeRow][0];
		end = Md->p.ppa[timeRow][1];

		if( ( (start < end) && (start <= clk) && (clk <= end) ) || ( (end < start) && (end <= clk) && (clk <= start) ) )
			break;
		timeRow++;
	}

	/* Determine the 0-indexed initial behavior. */
	behvCol = 2;
	while( (behvCol+2 < Md->colCnt ) && ( !(Md->p.ppa[timeRow][behvCol] < dice && dice <= Md->p.ppa[timeRow][behvCol+1]) ) )
		behvCol++;
	return behvCol-2;
}


void CSpeciesModel::RunBehaviorTransitionControlTest(SNGLBEHTRANSTRUCT *pBehTrans, MATRIX *M, int BehaviorIndex, int NumTrials)
{
	int           i;
	int           j;
	int           clkStart;
	int           newState;
	TRANSITNSTATE transState;
	int           loopCnt;
	C3MBRandom    random;        /* 3mb random class instance used for random number generation.  This method is permitted to have
							    * it's own instance because it is not called upon during scenario runs. */

	/* Input Assertions. */
	_ASSERT(pBehTrans);
	_ASSERT(M);
	_ASSERT(BehaviorIndex >= 0);
	_ASSERT(NumTrials >= 1);

	/* Initialize local variables. */
	random.mysrand(0);

	/* Initialize the SNGLBEHTRANSTRUCT struct passed in. */
	pBehTrans->cnt = new CNTBIN* [M->rowCnt]; // one for each transitional period
	for(i=0; i<M->rowCnt; i++)
	{
		pBehTrans->cnt[i] = new CNTBIN[NumTrials]; // one for each trial.
		memset(pBehTrans->cnt[i], 0, sizeof(CNTBIN)*NumTrials);
	}

	// i indexes the transitional period being tested.
	// j indexes the trial being run on the ith transitional period.
	for(i=0; i<M->rowCnt; i++)
	{
		/* index zero of the behavior transition matrix as a function of time holds the starting clock time of a transition.
		 * Time is in fractions of hours so mulitply by 3660 to get seconds. */
		clkStart = (int)floor(M->p.ppa[i][0]*3660);
		memset(pBehTrans->cnt[i], 0, sizeof(CNTBIN)*NumTrials);

		for(j=0; j<NumTrials; j++)
		{
			transState.timeLapsed =0;
			transState.endDice = random.myrand();

			// Behavior transition returns -1 if no transition occurs.
			loopCnt = 0;
			while(-1 == (newState = BehaviorTransition(T50_K_TERM, &transState, M, &random, clkStart))) // TODO: comback to and remove the T50_K_TERM
			{
				// Ensure no infinite loop by give up to two days without a translation (craziness!).
				if(transState.timeLapsed > NUMSECSPERDAY*2 || loopCnt++ >= 100000)
				{
					newState = BehaviorIndex;
					break;
				}
				transState.timeLapsed++;
			}
			pBehTrans->cnt[i][j].trans = newState;
			pBehTrans->cnt[i][j].sec = transState.timeLapsed;
		}
	}
}


/*****************************************************************************************
* MEMBER FUNCTION: BehaviorTransition()
* 
* DESCRIPTION: Determines (1) if time to transition to a new state, and
*						  (2) what that next state is.
*
* PARAMETERS:
*	TRANSITNSTATE *Bs: A pointer to a struct containing the time (int seconds) the animat has
*		spent in its current normal behavior and a value in determining if that behavior
*		comes to an end.
*
*	MATRIX *pM: A pointer to a matrix structure containing the transition matrix as a
*		function of time used for determining the next behavior the animat engages in
*		if the animat does transition behaviors.  Each row of the transition matrix
*		corresponds to a transition period on a 24-hour clock.
*
*	int AbsClock: The current time of day, in seconds, on a 24 hour clock.  Used to
*		determine the specific row on the transition matrix to be used for determining
*		the behavior transition.
*
*	int CurrentBehIndex: The index of the animat's current behavior that is returned by
*		this function if no behavior transition occurs.  Note that this
*		parameter has no impact on the function 
*
* MEMBER VARIABLES ALTERED: None (this is a Static function)
*
* RETURN VALUE: An integer that is an index that into either the current normal behavior
*	(passed in through parameter CurrentState) or the next next normal behavior if a
*	behavior transition occurs.
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*	05/24/05  (MJC)  Initial Coding/Transfer
*   02/03/09  (MJC)  Removed current behavior index as a parameter and set to return -1
*					 if a normal behavior transition does not occur and, as previous,
*					 the index of the new behavior if one does.
*
*****************************************************************************************/
int CSpeciesModel::BehaviorTransition(BEHTRANS_TERM_MODEL ModelType, const TRANSITNSTATE *Bs, const MATRIX *pM, C3MBRandom *pRefRand, int AbsClock)
{
	// Get a local constant reference to the behavior model.  Making it constant prevents this routine from altering
	// anything in the model. 
	int i, j;
	int timeRow;
	double* transVector;
	double probTerm;			// probability of terminating a behavioral bout
	int behCol;	//column
	double dice;
	int numBehaviors;

	double T50;// >mean Time In Beh;
	double k;// = slope Coefficient;
	double t; // in minuites

	double v1,v2,v3;

	//--------------------------------------------------------------------------//
	// Determine which time-based row to use to determine the transition vector.
	//--------------------------------------------------------------------------//
	timeRow = -1;
	for(i=0; i<pM->rowCnt && timeRow == -1; i++)
	{
		if(m_staticLib.ClockTimeIsWithin(AbsClock, pM->p.ppa[i][0], pM->p.ppa[i][1]) == TRUE)
			timeRow = i;
	}
	
	// If not time match found (bad modeling and error catching on the part of the GUI
	// and species designer) set the vector to the zero-th row.
	if(timeRow == -1)
		timeRow = 0;
	//--------------------------------------------------------------------------//

	//--------------------------------------------------------------------------//
	// Set the T50 and slople coefficient based upon the transition vector determined
	//--------------------------------------------------------------------------//
	// In the behavior transition matrix the number of columns is 5 plus the number of
	// behaviors and the number of rows is user defined (no established upper limit).
	// [start time] [end time] [0] [behavior n] [T50] [k]

	// In the behavior transition matrix T50 comes first followed by k.
	v1 = pM->p.ppa[timeRow][pM->colCnt - 2]; // zero-based indexing, second to the last column
	v2 = pM->p.ppa[timeRow][pM->colCnt - 1]; // zero-based indexing, last column
	v3 = Bs->timeLapsed; // divide by 60 to get time in minuites
	//--------------------------------------------------------------------------//

	if(ModelType == T50_K_TERM)
	{
		T50 = v1;
		k = v2;
		t = v3/60.0; // convert seconds to minuites by diving by 60.
		// Determine the probability of terminating the behavior.  These probabilities do not
		// include a back-calculation to t = 0, i.e. a minimum amount of time spent in a
		// behavior is assumed once it is entrained (minimum t in behavior = 1 min (60 s)).
		T50 = abs(T50); // make sure no neg numbers to into the log function
		k = abs(k); // make sure no neg numbers to into the log function
		if(T50 == 0)
			T50 = 0.000001;
		if(k == 0)
			k = 0.000001;
		probTerm = 1/(1 + pow(10, ((log10(T50)-log10(t))*k)));

		// Check for underflow of the termination equation and termination of the behavioral bout
		if (_isnan(probTerm) || (probTerm < 0.0))
			probTerm = 0.0;

		// Determine if a transition in behavior occurs, return -1 if not/
		if(Bs->endDice >= probTerm)
			return -1;

	}
	else //ModelType == GAUSSIAN_TERM
	{
		;// Nothing to do except return.
	}


	// Determine the next behavior.

	// Draw a random number
	dice = pRefRand->myrand();

	// Make a "friendlier" transition matrix for determining the next behavior in the
	// sequence.
	numBehaviors = pM->colCnt - 5;
	transVector = new double[numBehaviors+1]; // +1 accomidates the zero-probability
											  // element.

	// Copy just the elemetns of the vector specific for determining transition
	for(i=0, j=2; i<numBehaviors+1; i++, j++)
		transVector[i] = pM->p.ppa[timeRow][j];

	// Determine the next behavior in sequence
	behCol = 0;
	while(behCol < numBehaviors && (transVector[behCol]<dice && dice<=transVector[behCol+1]) == FALSE)
		behCol++;

	// Bullet proofing in case of bad entry by the user not caught by the GUI.
	if(behCol == numBehaviors)
		behCol = 0; 

	delete [] transVector;
	return behCol;
}

double CSpeciesModel::ProbTermination(double TermCoeff, double TimeLapse)
{
	double prob = TermCoeff * log(TimeLapse);;
	// Catch values that fall below zero or report isnan.  Force to zero.
	if (_isnan(prob) || (prob < 0.0))
		prob = 0.0;
	return prob;
}

void CSpeciesModel::CopyMatrix(MATRIX *pCpyM, const MATRIX *pM)
{
	int i;

	_ASSERTE(pCpyM->p.ppa != NULL && pM->p.ppa != NULL && pCpyM->rowCnt==pM->rowCnt && pCpyM->colCnt==pM->colCnt);

	for(i=0; i<pM->rowCnt; i++)
		memcpy(pCpyM->p.ppa[i], pM->p.ppa[i], pM->colCnt * sizeof(double));
}
void CSpeciesModel::CopyMatrix(ARRAY *pCpyM, const ARRAY *pM)
{
	_ASSERTE(pCpyM->p.pa!=NULL && (pM->p.pa!=NULL) && (pCpyM->rowCnt == pM->rowCnt) && (pCpyM->colCnt == pM->colCnt));
	memcpy(pCpyM->p.pa, pM->p.pa, pM->colCnt * sizeof(double));
}
void CSpeciesModel::CopyMatrix(ELEMENT *pCpyM, const ELEMENT *pM)
{
	_ASSERTE(pCpyM->rowCnt==pM->rowCnt && pCpyM->colCnt==pM->colCnt);
	memcpy(&pCpyM->a, &pM->a, sizeof(double));
}


/*******************************************************************************
* MEMBER FUNCTION:
* 
* ARGUMENTS:
*	None.
*
* MEMBER VARIABLES ALTERED:
*
* RETURN VALUE:
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   05/18/05  (MJC)  Initial Coding/Transfer
*
* DESCRIPTION:
*
* CORRESPONDING FUNCTION IN ORIGINAL 3MB PROGRAM, IF ANY
*
*******************************************************************************/
double CSpeciesModel::RateModel(const RANDOM *R, C3MBRandom *pRefRand)
{
	int loopCount = 0;
	double r = -1.0;
	// If rate of travel is terminated, calculate the new rate of travel
	while(r < 0.0 && loopCount++ < 100)
		r = (pRefRand->myrand() * (R->max - R->min)) + R->min;

	if(r < 0.00)
		r = 1.0;
	return r;
}
double CSpeciesModel::RateModel(const GAUSS *R, C3MBRandom *pRefRand)
{
	int loopCount = 0;
	double r = -1.0;
	while(r < 0.0 && loopCount++ < 100)
		r = pRefRand->noise(R->mean, R->std);
	if(r < 0.00)
		r = 1.0;
	return r;
}
double CSpeciesModel::RateModel(const RATEVCTRMDLPARAM *R, C3MBRandom *pRefRand)
{
	double dice = pRefRand->myrand();
	int rateCol;

	// Randomly determine the upper and lower rate bounds.
	for(rateCol=1; rateCol < R->vector.colCnt; rateCol++)
	{
		if(R->vector.p.pa[rateCol-1] < dice && dice <= R->vector.p.pa[rateCol])
			break;
	}

	// Calculate the new rate. rateCol+1 takes uppper rate bound into account, myRand()
	// the lower bount.
	return fabs((rateCol-pRefRand->myrand()) * R->step.a);
}
double CSpeciesModel::RateModel(const RATEMDL *R, C3MBRandom *pRefRand)
{
	// Calculate new rate.
	switch(R->modelType)
	{
	case VECTOR:
		return RateModel(&R->vm, pRefRand);
	case UNIFORM:
		return RateModel(&R->rnd, pRefRand);
	case GAUSSIAN:
		return RateModel(&R->gauss, pRefRand);
	}
	return 0;
}

double CSpeciesModel::GetMaxRate(const RATEMDL *R)
{
	// Return Max Rate.
	switch(R->modelType)
	{
	case VECTOR:
		return GetMaxRate(&R->vm);
	case UNIFORM:
		return GetMaxRate(&R->rnd);
	case GAUSSIAN:
		return GetMaxRate(&R->gauss);
	}
	return 0;
}

// Get Dorian's approval on these...
double CSpeciesModel::GetMaxRate(const RATEVCTRMDLPARAM *R)
{
	return fabs(R->vector.colCnt * R->step.a);
}
double CSpeciesModel::GetMaxRate(const RANDOM *R)
{
	return R->max;
}
double CSpeciesModel::GetMaxRate(const GAUSS *R)
{
	return R->mean + R->std * 3; // this is not a true max rate.
}

/*******************************************************************************
* MEMBER FUNCTION:
* CSpeciesModel::Direction(double Bearing, int BehaviorTransition)
* 
* ARGUMENTS:
*	Bearing - Starting bearing used as a starting point to determine next
*					bearing.
*
* MEMBER VARIABLES ALTERED:
*	No other member variables are altered, but certain other member
*	variables are used for calculation.
*
* RETURN VALUE:
*	A new bearing based upon the model and parameters entered.  
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   05/23/05  (MJC)  Initial Coding/Transfer
*
* DESCRIPTION:
*	Uses the DIRECTION_MODEL_thisGoesAway structure M and passed in parameters to determine a 
*	new direction, if any, for an animat to head in.  This function works with
*	all DIRECTION_MODEL_thisGoesAway model types.
*
* CORRESPONDING FUNCTION(S) IN ORIGINAL 3MB PROGRAM, IF ANY
* ChangeDirectionVectorModel(), RandomWalk(), CorrelatedRandomWalk(), and 
* CRW_WithDirectionalBias() in horizontal_movement.cpp.
*
*******************************************************************************/
double CSpeciesModel::Direction(const RANDOMWALK *M, C3MBRandom *pRefRand)
{
	double fVal;    /* Holds a randomly generated number and retun value */
	RANDOMWALK m;   /* Random walk model.  Not needed here, but used to keep compiler warning quite about the RANDOMWALK param
					 * passed in that is also not needed but is passed in to keep the signature of all Direction Model-related
					 * functions the same. */

	/* Assert input(s) and handle cases when not correct. */
	_ASSERT(M);
	_ASSERT(pRefRand);
	if(M)
		memcpy(&m, M, sizeof(RANDOMWALK));

	fVal = pRefRand->myrand();
	fVal *= 360.0;
	fVal = m_staticLib.KeepWithin360(fVal);
	return fVal;
}
double CSpeciesModel::Direction(const CORRANDWALK *M, double Bearing, C3MBRandom *pRefRand)
{
	double fVal;    /* Holds a randomly generated number. */

	/* Assert input(s) and handle cases when not correct. */
	_ASSERT(M);
	_ASSERT(pRefRand);
	if(!M || !pRefRand)
		return 0;

	fVal = pRefRand->noise(0, M->perturbation);
	fVal += Bearing;
	fVal = m_staticLib.KeepWithin360(fVal);
	return fVal;
}
double CSpeciesModel::Direction(const CORRANDWALKDB *M, double Bearing, C3MBRandom *pRefRand)
{
	double left, right;		// degree arc, to the right or left, between current bearing and
	double pert;	// a change in direction applied to the current bearing.
	double prob_turn_left;	// probability of turning left
	int    i;
	int loopCnt;

	// Calculate the change in arc (direction of turn not yet decided)
	pert = fabs(pRefRand->noise(0, M->perturbation));

	// Determine which is closer, left or right turn
	right = left = 0.0;
	if(Bearing > M->directionOfBias)
	{
		left = Bearing - M->directionOfBias;
		right = (360 - Bearing) + M->directionOfBias;
	}
	else if(Bearing < M->directionOfBias)
	{
		left = Bearing + (360 - M->directionOfBias);
		right = M->directionOfBias - Bearing;
	}

	// Implement the bias calculation. Calculate the probability of turning left.
	i = 0;
	if(left < right)
	{
		loopCnt = 0;
		while( ((M->arcStep / 2) * (i + 1) <= left) && (loopCnt++ < 10000))
			i++;
		prob_turn_left = (i * M->bias) + 0.5;
	}
	else if(right < left)
	{
		loopCnt = 0;
		while( ((M->arcStep / 2) * (i + 1) <= right) && (loopCnt++ < 10000))
			i++;
		prob_turn_left = 0.5 - (i * M->bias);
	}
	else
		prob_turn_left = 0.5;

	// Draw a random number and determine if the turn is to the left
	// (condition met) or to the right (condition not met)
	if(pRefRand->myrand() <= prob_turn_left) 
		pert *= -1;

	// Calculate bearing
	return m_staticLib.KeepWithin360(Bearing + pert);
}

double CSpeciesModel::Direction(const DIRVCTRMDLPARAM *M, const DIRECTIONAL_MODEL_TYPE Type, double Bearing, C3MBRandom *pRefRand)
{
	double dice;
	double temp_sum = 0;	// summation of the probTurningArray array used in calculating
							// weighted probabilities
	double divisor = 360.0 / M->direction.colCnt;;
	int index, j, c=0;	// column
	double *probTurningArray = new double[M->direction.colCnt];

	_ASSERT(M->direction.colCnt > 0);

	if(probTurningArray == NULL || M->direction.colCnt < 1)
		return 0; // work on this solution...

	Bearing = m_staticLib.KeepWithin360(Bearing);

	// Copy the direction array into the probability of turning array
	memcpy(probTurningArray, M->direction.p.pa, M->direction.colCnt * sizeof(double));

	// Adjust the probability of turning array if there biasing used.
	if(Type == VECTOR_MODEL_DIRECTIONAL_WITH_VECTOR_MODEL_BIASING)
	{
		// Determine the appropriate biasing row given the animat's current heading.
		int bearingRowIndex = (int)floor(Bearing / divisor);

		// Use when directional bias and behavioral influence exist to calculate a new Change in Direction vector.
		// Determine row of the direction bias matrix to use.  Apply the weighting
		for(c = 0; c < M->directionalBias.colCnt; c++)
		{
			probTurningArray[c] *= M->directionalBias.p.ppa[bearingRowIndex][c];
			temp_sum += probTurningArray[c];
		}
	}
	// Determine the new (temporary) distribution (cumulative proportion) of turn angles. Avoid divide by zero by 
	// checking that temp_sum is not zero.
	if(temp_sum == 0)
		temp_sum = 1;
	probTurningArray[0] /= temp_sum; 
	for(j=1; j<M->directionalBias.colCnt; j++)
		probTurningArray[j] = probTurningArray[j-1] + probTurningArray[j] / temp_sum ;

	// Determine the arc within which the change of direction will occur
	dice = pRefRand->myrand();
	for(index=1; index < M->direction.colCnt; index++)
	{
		if((probTurningArray[index-1] < dice) && (dice <= probTurningArray[index]))
			break;
	}

	// Determine the new bearing, bearing equals old bearing plus a change.
	delete [] probTurningArray;
	return m_staticLib.KeepWithin360(Bearing - 180 + (index - pRefRand->myrand()) * divisor);
}

double CSpeciesModel::Direction(const DIRCTNMDL *Dp, const DIRECTIONSTATE *Ds, C3MBRandom *pRefRand)
{
	switch(Dp->modelType)
	{
	case VECTOR_MODEL_DIRECTIONAL_NO_BIASING:
	case VECTOR_MODEL_DIRECTIONAL_WITH_VECTOR_MODEL_BIASING:
		return Direction(&Dp->vm, Dp->modelType, Ds->bearing, pRefRand);
	case RANDOM_WALK:
		return Direction(&Dp->rndWalk, pRefRand);
	case CORRELATED_RANDOM_WALK:
		return Direction(&Dp->crRndWalk, Ds->bearing, pRefRand);
	case CORRELATED_RANDOM_WALK_WITH_DIR_BIASING:
		return Direction(&Dp->crRndWalkDb, Ds->bearing, pRefRand);
	}
	return 0;
}

/*******************************************************************************
* MEMBER FUNCTION:
*	CAnimat::SetMaxDepthNextDive(DEPTH_MODEL_goesAway *M)
* 
* ARGUMENTS:
*	A pointer to a DEPTH_MODEL_goesAway structure.
*
* MEMBER VARIABLES ALTERED:
*	max_depth_current_dive.
*
* RETURN VALUE:
*	None. 
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   05/17/05  (MJC)  Initial Coding/Transfer
*
* DESCRIPTION:
*	Uses the species depth model to calculate the maximum depth an animat will
*	decent to in it's next dive.  The type of model determines how the
*	maximum depth is calculated.  Depth models (structure DEPTH_MODEL_goesAway) can be
*	vectors, uniform random, or gaussian types.  The depth model "knows" what
*	type it is.
*
* CORRESPONDING FUNCTION IN ORIGINAL 3MB PROGRAM, IF ANY
*	FindDepthNextDive(), in file vertical_movement.cpp
*
*******************************************************************************/
double CSpeciesModel::DepthModel(const RANDOM *D, C3MBRandom *pRefRand)
{
	double d = 1;
	int loopCnt = 0;
	double maxDepth = D->max;
	if(maxDepth < 0)
		maxDepth = -maxDepth;

	while(d > 0.0 && loopCnt++ < 10000)
		d = (-1) * pRefRand->myrand() * maxDepth;

	if(loopCnt >= 10000)
		d = -1*maxDepth;

	return d;
}
	
double CSpeciesModel::DepthModel(const GAUSS *D, C3MBRandom *pRefRand)
{
	return (-1) * fabs(pRefRand->noise(D->mean, D->std));			
}
double CSpeciesModel::DepthModel(const VCTRMDLPARAM *D, C3MBRandom *pRefRand)
{
	int depthCol;
	double dice = pRefRand->myrand();

	for(depthCol = 1; depthCol < D->vector.colCnt; depthCol++)
	{
		if(D->vector.p.pa[depthCol-1] < dice && dice <= D->vector.p.pa[depthCol])
			break;
	}
	
	// Get a random number then calculate the new depth.
	return (-1)*fabs((depthCol-pRefRand->myrand()) * D->step.a);
}

double CSpeciesModel::DepthModel(const DEPTHPARAM *D, C3MBRandom *pRefRand)
{
	switch(D->modelType)
	{
	case VECTOR:
		return DepthModel(&D->vm, pRefRand);
	case UNIFORM:
		return DepthModel(&D->rnd, pRefRand);
	case GAUSSIAN:
		return DepthModel(&D->gauss, pRefRand);
	}
	return 0;
}


/*******************************************************************************
* MEMBER FUNCTION:
*	CAnimat::DetermineSurfaceInterval(const SURFACE_INTERVAL_MODEL_goesAway *M)
*
* ARGUMENTS:
*	A (constant) pointer to a SURFACE_INTERVAL_MODEL_goesAway.
*
* MEMBER VARIABLES ALTERED:
*	None.
*
* RETURN VALUE:
*	A type double which is a new surface interval
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   05/17/05  (MJC)  Initial Coding/Transfer
*
* DESCRIPTION:
*	Calculates a new surface interval using the model in the structure 
*	SURFACE_INTERVAL_MODEL_goesAway passed into it.  
*
* CORRESPONDING FUNCTION IN ORIGINAL 3MB PROGRAM, IF ANY
*	SurfaceIntervalVectorModel() and GaussianRandomSurfaceInterval().
*
*******************************************************************************/
int CSpeciesModel::SurfaceInterval(const GAUSS *S, C3MBRandom *pRefRand)
{
	double si = -1.0;
	int loopCnt = 0;

	while (si < 0.0 && loopCnt++ < 10000)
		si = pRefRand->noise(S->mean, S->std);

	if(loopCnt >= 10000)
		si = 1;

	return (int)floor(si);
}

int CSpeciesModel::SurfaceInterval(const VCTRMDLPARAM *S, C3MBRandom *pRefRand)
{
	int c; // row and column
	double dice; // a random number
	double si = 0;
	int cnt=0; // avoid infinite loops.

	do{
		dice = pRefRand->myrand();
		c = 1;
		while(c < S->vector.colCnt-1)
		{
			if((S->vector.p.pa[c-1] < dice) && (dice <= S->vector.p.pa[c]))
				break;
			c++;
		}
		// Calculate the number of seconds.
		si = (c - pRefRand->myrand()) * S->step.a;
	}while(si < 0.0 && cnt++ < 100);

	return (int)floor(fabs(si));
}

int CSpeciesModel::SurfaceInterval(const SURFINTRVLPARAM *Sp, C3MBRandom *pRefRand)
{

	switch(Sp->modelType)
	{
	case VECTOR:
		return SurfaceInterval(&Sp->vm, pRefRand);
	case GAUSSIAN:
		return SurfaceInterval(&Sp->gauss, pRefRand);
	case UNIFORM:
		_ASSERTE(Sp->modelType != UNIFORM);
		return 0;
	}
	return 0;
}

/*******************************************************************************
* MEMBER FUNCTION:	ReversalDuration()
* 
* ARGUMENTS:
*	BehaviorTransition - an integer representing the state of the animat calling
*					  this function.
*
* MEMBER VARIABLES ALTERED:
*
* RETURN VALUE:
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   05/24/05  (MJC)  Initial Coding/Transfer
*
* DESCRIPTION:
*
* CORRESPONDING FUNCTION IN ORIGINAL 3MB PROGRAM, IF ANY
*	TimeReversedVectorModel()
*
*******************************************************************************/
int CSpeciesModel::ReversalDuration(const REVERSAL_RND *Rp, C3MBRandom *pRefRand)
{
	int loopCount = 0;
	double time = -1;

	while(time <= 0 && loopCount++ < 100)
		time = pRefRand->noise(Rp->time.mean, Rp->time.std);

	if(time <= 0)
		time = 1;

	return (int)time;
}
int CSpeciesModel::ReversalDuration(const REVERSAL_GAUSS *Rp, C3MBRandom *pRefRand)
{
	int loopCount = 0;
	double time = -1;

	while(time <= 0 && loopCount++ < 100)
		time = pRefRand->noise(Rp->time.mean, Rp->time.std);

	if(time <= 0)
		time = 1;

	return (int)time;
}
int CSpeciesModel::ReversalDuration(const REVVCTRMDLPARAM *Rp, C3MBRandom *pRefRand)
{
	int c=0;
	double dice;
	double time = -1;
	int cnt = 0;
	int retTime;

	while(time <= 0 && cnt++ < 100)
	{
		dice = pRefRand->myrand();
		for(c=1; c<Rp->time.colCnt; c++)
		{
			if((Rp->time.p.pa[c-1] < dice) && (dice <= Rp->time.p.pa[c]))
				break;
		}
		time = (c - pRefRand->myrand()) * Rp->timeStep.a;
	}

	time = fabs(time);
	retTime = (int)time;
	if(retTime == 0)
		retTime = 1;

	return retTime;
}

int CSpeciesModel::ReversalDuration(const REVERSAL_DEF *Rp, C3MBRandom *pRefRand)
{
	if(Rp->reverses == FALSE)
		return 0;

	switch(Rp->modelType)
	{
	case VECTOR:
		return ReversalDuration(&Rp->vm, pRefRand);
	case GAUSSIAN:
		return ReversalDuration(&Rp->gauss, pRefRand);
	case UNIFORM:
		return ReversalDuration(&Rp->rnd, pRefRand);
	}
	return 0;
}



/*******************************************************************************
* MEMBER FUNCTION:
*	CAnimat::NumberOfReversals(const REVERSAL_MODEL_goesAway *M)
* 
* ARGUMENTS:
*	A pointer to a REVERSAL_MODEL_goesAway structure.  Note that it wasn't neccesary to 
*	actually pass in the pointer to the REVERSAL_MODEL_goesAway structure, since it comes
*	from the member variable m_model, a pointer to class CSpeciesModel already
*	part of class CAnimat.  But it made for neater code.
*
* MEMBER VARIABLES ALTERED:
*	None - 
*
* RETURN VALUE:
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   05/18/05  (MJC)  Initial Coding/Transfer
*
* DESCRIPTION:
*	
*
* CORRESPONDING FUNCTION IN ORIGINAL 3MB PROGRAM, IF ANY
*	ReversalVectorModel(), UniformRandomReversal(), and 
*	GaussianRandomReversal() from vertical_movement.cpp
*******************************************************************************/
int CSpeciesModel::NumberOfReversals(const REVERSAL_GAUSS *R, C3MBRandom *pRefRand)
{
	int numRev;
	int loopCnt;
	double dice;

	/* Assert input(s) and handle bad instances of it. */
	_ASSERT(R);
	_ASSERT(pRefRand);
	if(!R || !pRefRand)
		return 0;

	dice = pRefRand->myrand();
	if(dice >= R->probOfReversal)
		return 0;

	numRev = 0;
	loopCnt = 0;
	while(numRev <= 0 && loopCnt++ < 100) 
		numRev = (int)pRefRand->noise(R->count.mean, R->count.std);

	if(numRev == 0)
		numRev = 1;

	return numRev;
}
int CSpeciesModel::NumberOfReversals(const REVERSAL_RND *R, C3MBRandom *pRefRand)
{
	int numRev;   /* The determined number of reversals. */
	double dice;  /* Holds a randomly determined number. */

	/* Assert input(s) and handle bad instances of it. */
	_ASSERT(R);
	_ASSERT(pRefRand);
	if(!R || !pRefRand)
		return 0;

	dice = pRefRand->myrand();
	if(dice >= R->probOfReversal)
		return 0;

	numRev = pRefRand->rnd(R->count.min, R->count.max);
	numRev = abs(numRev);
	return numRev;
}
int CSpeciesModel::NumberOfReversals(const REVVCTRMDLPARAM *R, C3MBRandom *pRefRand)
{
	double dice;  /* Holds a randomly determined number. */
	int c;		  /* column */

	/* Assert input(s) and handle bad instances of it. */
	_ASSERT(R);
	_ASSERT(pRefRand);
	if(!R || !pRefRand)
		return 0;


	dice = pRefRand->myrand();
	if(dice >= R->probOfReversal.a)
		return 0;

	dice = pRefRand->myrand();
	for(c=1; c<R->count.colCnt; c++)
	{
		if(R->count.p.pa[c-1] < dice && dice <= R->count.p.pa[c])
			break;
	}
	// Mention this to Dorian.
#pragma message("Ask Dorian about this: c * 2.  SpeciesModel.cpp, NumberOfReversals(), around line 1645")
	return c * 2;
}
int CSpeciesModel::NumberOfReversals(const REVERSAL_DEF *Rp, C3MBRandom *pRefRand)
{
	// If reversal modeling not being used just return (the switch has already been set to false), otherwise, fill in
	// and determine needed information.
	if(Rp->reverses == FALSE)
		return 0;

	switch(Rp->modelType)
	{
	case VECTOR:
		return NumberOfReversals(&Rp->vm, pRefRand);
	case GAUSSIAN:
		return NumberOfReversals(&Rp->gauss, pRefRand);
	case UNIFORM:
		return NumberOfReversals(&Rp->rnd, pRefRand);
	}
	return 0;
}

BOOL CSpeciesModel::DirectionTerminates(const DIRCTNMDL *Dp, const DIRECTIONSTATE *Ds)
{
	double prob=0;	// probability of changing rate of travel
	if(Dp->modelType == VECTOR_MODEL_DIRECTIONAL_NO_BIASING || Dp->modelType == VECTOR_MODEL_DIRECTIONAL_WITH_VECTOR_MODEL_BIASING)
		prob = ProbTermination(Dp->vm.terminate.a, Ds->timeLapsed);
	else if(Dp->modelType == RANDOM_WALK)
		prob = ProbTermination(Dp->rndWalk.termCoeff, Ds->timeLapsed);
	else if(Dp->modelType == CORRELATED_RANDOM_WALK)
		prob = ProbTermination(Dp->crRndWalk.termCoeff, Ds->timeLapsed);
	else if(Dp->modelType == CORRELATED_RANDOM_WALK_WITH_DIR_BIASING)
		prob = ProbTermination(Dp->crRndWalkDb.termCoeff, Ds->timeLapsed);

	if(prob >= Ds->endDice)
		return TRUE;
	return FALSE;
}

BOOL CSpeciesModel::RateTerminates(const RANDOM *Rp, const RATESTATE *Rs)
{
	double prob = ProbTermination(Rp->coeff, Rs->timeLapsed);
	if(prob >= Rs->endDice)
		return TRUE;
	return FALSE;
}
BOOL CSpeciesModel::RateTerminates(const GAUSS *Rp, const RATESTATE *Rs)
{
	double prob = ProbTermination(Rp->coeff, Rs->timeLapsed);
	if(prob >= Rs->endDice)
		return TRUE;
	return FALSE;
}
BOOL CSpeciesModel::RateTerminates(const RATEVCTRMDLPARAM *Rp, const RATESTATE *Rs)
{
	double prob = ProbTermination(Rp->terminate.a, Rs->timeLapsed);
	if(prob >= Rs->endDice)
		return TRUE;
	return FALSE;
}

BOOL CSpeciesModel::RateTerminates(const RATEMDL *Rp, const RATESTATE *Rs)
{
	if(Rp->modelType == VECTOR)
		return RateTerminates(&Rp->vm, Rs);
	else if(Rp->modelType == UNIFORM)
		return RateTerminates(&Rp->rnd, Rs);
	else
		return RateTerminates(&Rp->gauss, Rs);
}

//---------------------------------------------------------------------------------//
// Public File Functions
//---------------------------------------------------------------------------------//
RESLT CSpeciesModel::SaveToBinFile(TCHAR *FileName)
{
	HANDLE hd;
	RESLT res;
	hd = CreateFile(FileName, GENERIC_WRITE, 0, 0, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
	if(hd == INVALID_HANDLE_VALUE)
		return OPENFILEWRITE_ERROR;

	res = SaveToBinFile(hd);
	CloseHandle(hd);
	return res;
}

RESLT CSpeciesModel::SaveToBinFile(HANDLE hd)
{
	RESLT res = SaveToBinFile(hd, &this->m_speciesModel, TRUE);
	if(res == OK)
		m_needSave = FALSE;
	return res;
}

RESLT CSpeciesModel::SaveToBinFile(TCHAR *FileName, SPECIES_MDL* pSpeMdl, BOOL UpdateSaveDateTime)
{
	HANDLE hd;
	RESLT res;

	hd = CreateFile(FileName, GENERIC_WRITE, 0, 0, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
	if(hd == INVALID_HANDLE_VALUE)
		return OPENFILEWRITE_ERROR;

	res = SaveToBinFile(hd, pSpeMdl, UpdateSaveDateTime);
	CloseHandle(hd);
	return res;
}


RESLT CSpeciesModel::SaveToBinFile(HANDLE hd, SPECIES_MDL* pSpeMdl, BOOL UpdateSaveDateTime)
{
	RESLT res = OK;
	UINT32 i;
	INT32 j;
	DWORD bytes = 0;
	BOOL wrslt = 1;
	NRMLBEHMDL *beh;

	SYSTEMTIME sysTime;

	UpdateSaveDateTime = UpdateSaveDateTime; // quiet compiler warning... this isn't used yet.

	//********************************************//
	//SPECIES_MDL_NEW temp;
	//memset(&temp, 0, sizeof(SPECIES_MDL_NEW));
	//********************************************//
	GetLocalTime(&sysTime);
	pSpeMdl->description.year = sysTime.wYear; // 2 digits, 00 ~ 99:   7 bits
	pSpeMdl->description.month = sysTime.wMonth; // 2 digits, 00 ~ 12: 4 bits
	pSpeMdl->description.day = sysTime.wDay; // 2 digits, 00 ~ 31:     5 bits
	pSpeMdl->description.hour = sysTime.wHour; // 2 digits, 00 ~ 59:   6 bits
	pSpeMdl->description.min = sysTime.wMinute; // 2 digits, 00 ~ 59:  6 bits
	pSpeMdl->description.sec = sysTime.wSecond; // 2 digits, 00 ~ 59:  6 bits
	// milliseconds 3 digits, 000 ~ 999:  % 16 = 0 ~ 15              10 bits/4 bits
	//                                                                      31 bits
	pSpeMdl->description.id = 0;

	pSpeMdl->description.id =
		pSpeMdl->description.month << 27 | // 4 + 6 + 6 + 6 + 5 = 27
		pSpeMdl->description.day   << 22 | // 4 + 6 + 6 + 6     = 22
		pSpeMdl->description.hour  << 16 | // 4 + 6 + 6         = 16
		pSpeMdl->description.min   << 10 | // 4 + 6             = 10
		pSpeMdl->description.sec   << 4  | // 4                 = 4
		(sysTime.wMilliseconds % 16);


	sprintf_s(pSpeMdl->description.szHeader, sizeof(pSpeMdl->description.szHeader), "%s", SZ_SPECIESSPECIFICATION_HEADER);

	// Write the SPECIES_MDL structure to file.
	pSpeMdl->description.mbsVerSuper = MMBSLIB_VERSION_SUPER;
	pSpeMdl->description.mbsVerSub = MMBSLIB_VERSION_SUB;
	pSpeMdl->description.speVerSuper = MMMBLIB_SPECIES_VERSION_SUPER;
	pSpeMdl->description.speVerSub = MMMBLIB_SPECIES_VERSION_SUB;


	if(0 == WriteFile(hd, pSpeMdl, sizeof(SPECIES_MDL), &bytes, NULL))
		return FILEWRITE_ERROR;

	//-----------------//
	// Initial Behavior
	//-----------------//
	for(j=0; j<pSpeMdl->initBehSpanCnt; j++)
	{
		if(FALSE == (wrslt &= WriteFile(hd, &pSpeMdl->initialBehavior.arr[j], sizeof(BEHTRAN), &bytes, NULL)))
			return FILEWRITE_ERROR;
		res = WriteVector(hd, &pSpeMdl->initialBehavior.arr[j].m); if(res != OK) return res;
	}

	//res = WriteVector(hd, &pSpeMdl->initialBehavior); if(res != OK) return res;
	//_ASSERT(0);
	// Write each behavior to file
	//_ASSERT(sizeof(SPECIES_MDL)%16 == 0);
	for(i=0; i<pSpeMdl->description.numBehaviors; i++)
	{
		beh = &pSpeMdl->p.behavior[i];
		
		// Write the behavior structure to file
		_ASSERT(sizeof(SPECIES_MDL)%16 == 0);
		if(FALSE == (wrslt &= WriteFile(hd, beh, sizeof(NRMLBEHMDL), &bytes, NULL)))
			return FILEWRITE_ERROR;

		// Handle all the dynamically allocated memory contained in substructures of 
		// this structure
		// Normal behavior transition
		//res = WriteVector(hd, &beh->nrmlBehTransMatrix); if(res != OK) return res;
		for(j=0; j<beh->nrmlBehTransCnt; j++)
		{
			if(FALSE == (wrslt &= WriteFile(hd, &beh->nrmlBehTrans.arr[j], sizeof(BEHTRAN), &bytes, NULL)))
				return FILEWRITE_ERROR;

			res = WriteVector(hd, &beh->nrmlBehTrans.arr[j].m); if(res != OK) return res;
		}

		// Depth environmenatal attractor behavior transition
		res = WriteVector(hd, &beh->depthEnvAttBehTrans.behavior); if(res != OK) return res;
		res = WriteVector(hd, &beh->depthEnvAttBehTrans.terminate); if(res != OK) return res;

		// Depth temperature attractor behavior transition
		res = WriteVector(hd, &beh->tempEnvAttBehTrans.behavior); if(res != OK) return res;
		res = WriteVector(hd, &beh->tempEnvAttBehTrans.terminate); if(res != OK) return res;

		// Travel Rate
		res = WriteVector(hd, &beh->travelRate.vm.step); if(res != OK) return res;
		res = WriteVector(hd, &beh->travelRate.vm.terminate); if(res != OK) return res;
		res = WriteVector(hd, &beh->travelRate.vm.vector); if(res != OK) return res;

		// Travel direction
		res = WriteVector(hd, &beh->travelDirection.vm.direction); if(res != OK) return res;
		res = WriteVector(hd, &beh->travelDirection.vm.directionalBias); if(res != OK) return res;
		res = WriteVector(hd, &beh->travelDirection.vm.terminate); if(res != OK) return res;

		// Descent rate
		res = WriteVector(hd, &beh->dive.descentRate.vm.step); if(res != OK) return res;
		res = WriteVector(hd, &beh->dive.descentRate.vm.terminate); if(res != OK) return res;
		res = WriteVector(hd, &beh->dive.descentRate.vm.vector); if(res != OK) return res;

		// Ascent rate
		res = WriteVector(hd, &beh->dive.ascentRate.vm.step); if(res != OK) return res;
		res = WriteVector(hd, &beh->dive.ascentRate.vm.terminate); if(res != OK) return res;
		res = WriteVector(hd, &beh->dive.ascentRate.vm.vector); if(res != OK) return res;

		// Surface Interval
		res = WriteVector(hd, &beh->dive.srfInv.vm.step); if(res != OK) return res;
		res = WriteVector(hd, &beh->dive.srfInv.vm.vector); if(res != OK) return res;

		// Reversal
		res = WriteVector(hd, &beh->dive.reversal.vm.count); if(res != OK) return res;
		res = WriteVector(hd, &beh->dive.reversal.vm.probOfReversal); if(res != OK) return res;
		res = WriteVector(hd, &beh->dive.reversal.vm.time); if(res != OK) return res;
		res = WriteVector(hd, &beh->dive.reversal.vm.timeStep); if(res != OK) return res;

		// Depth Model
		res = WriteVector(hd, &beh->dive.depth.vm.step); if(res != OK) return res;
		res = WriteVector(hd, &beh->dive.depth.vm.vector); if(res != OK) return res;
	}

	//ModelToText("crazyJoey.txt");
	return res;
}


RESLT CSpeciesModel::LoadFromBinFile(TCHAR *FileName)
{
	HANDLE hd;
	RESLT res;

#ifdef _DEBUG
//	FILE *fd;
//	fopen_s(&fd, "LoadFromBinFile.txt", "a");
//	fprintf_s(fd, "CSpeciesModel::LoadFromBinFile(%s)", FileName);
//	fclose(fd);
//	ExitProcess(0);
#endif

	hd = CreateFile(FileName, GENERIC_READ, NULL, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
	if(hd == INVALID_HANDLE_VALUE)
		return OPENFILEREAD_ERROR;

	res = LoadFromBinFile(hd);
	CloseHandle(hd);
	return res;
}


RESLT CSpeciesModel::LoadFromBinFile(HANDLE Hd)
{
	RESLT res;
	CSpeciesUpgrader speUpgrader;

	m_needSave = FALSE;

	// Dealloacte memory.
	ClearMemberVariables();
	memset(&m_speciesModel, 0, sizeof(SPECIES_MDL));

	//_ASSERT(0);

	// Read the SPECIES_MDL structure from file. (code is implemented in SpecesUpgrader.cpp)
	res = speUpgrader.ReadSpeciesModel(Hd, &m_speciesModel);;
	return res;
}


RESLT CSpeciesModel::LoadFromFile(TCHAR *FileName)
{
	 // ERROR REPORTING - improper extensions.
	int len = strlen(FileName);

	// Make sure the file name AT LEAST has a proper extension, ".mtx" or ".txt" will be at least 4 characters.  File
	// name should be longer than that.
	if(len < 4)
		return WRONGFILETYPE_ERROR;

	// Either load a model from a binary file or a text file.
	if(strcmp(szBinExtension, &FileName[len-4]) == 0)
		return LoadFromBinFile(FileName);

	if(strcmp(szTxtExtension, &FileName[len-4]) == 0)
		return ReadTextVectorModel(FileName);

	// The file did not have the proper extension.  Report this.
	return WRONGFILETYPE_ERROR;
}


RESLT CSpeciesModel::ModelToText(TCHAR *FileName)
{
	RESLT res;
	FILE *fd;

	fopen_s(&fd, FileName, "w");
	if(fd == NULL)
		return OPENFILEWRITE_ERROR;
	res = ModelToText(fd);
	fclose(fd);
	return res;
}

// Error checking severly limited.  This is more of a debugging tool and an error is highly unlikely.
RESLT CSpeciesModel::ModelToText(FILE *fd)
{
	// Not currently supported
	RESLT res = OK;
	TCHAR buf[SIZE_128];
	int bytes;
	UINT32 i;

	ACSTCAVRSNMDL *av;
	NRMLBEHMDL *beh;

	if(fd == NULL)
		return INVALIDHANDLE_ERROR;

	if(-1 == fprintf(fd, "*************************\n"))
		return FILEWRITE_ERROR;
	if(-1 == fprintf(fd, "* Saved Under\n*     3mbs version %d.%02d *\n*  species version %d.%02d *\n", m_speciesModel.description.mbsVerSuper,
		m_speciesModel.description.mbsVerSub, m_speciesModel.description.speVerSuper, m_speciesModel.description.speVerSub))
		return FILEWRITE_ERROR;
	if(-1 == fprintf(fd, "*************************\n\n"))
		return FILEWRITE_ERROR;

    //--------------------//
    // (3) Shore Following
    //--------------------//
	if(-1 == fprintf(fd, "SHORE FOLLOWING:\n             depth: %4.2f\n\n", m_speciesModel.description.shoreFollowDepth))
		return FILEWRITE_ERROR;

    //----------------------//
    // (1) Acoustic Aversion
    // 47 parameters
    //----------------------//
	av = &m_speciesModel.acousticAversion;

	if(-1 == fprintf(fd, "ACOUSTIC AVERSION:\n"))
		return FILEWRITE_ERROR;

//	if(-1 == fprintf(fd, "  Source 1:\n        Activation: %f\n      Deactivation: %f\n             units: GROG\n          decayfnc: GROG\n\n",
//		av->actThreshA, av->deactThreshA))
//		return FILEWRITE_ERROR;
//	if(-1 == fprintf(fd, "  Source 2:\n        Activation: %f\n      Deactivation: %f\n             units: GROG\n          decayfnc: GROG",
//		av->actThreshB, av->deactThreshB))
//		return FILEWRITE_ERROR;

	if(-1 == fprintf(fd, "\n\n"))
		return FILEWRITE_ERROR;
	if(-1 == fprintf(fd, "  Affected Modeling:\n"))
		return FILEWRITE_ERROR;
	if(-1 == fprintf(fd, "       Travel Rate: %s\n", m_staticLib.YesNoString(av->travelRateAffected, buf, sizeof(buf))))
		return FILEWRITE_ERROR;
	if(-1 == fprintf(fd, "  Travel Direction: %s\n", m_staticLib.YesNoString(av->travelDirectionAffected, buf, sizeof(buf))))
		return FILEWRITE_ERROR;
	if(-1 == fprintf(fd, "  Surface Interval: %s\n", m_staticLib.YesNoString(av->surfaceIntervalAffected, buf, sizeof(buf))))
		return FILEWRITE_ERROR;
	if(-1 == fprintf(fd, "       Ascent Rate: %s\n", m_staticLib.YesNoString(av->ascentRateAffected, buf, sizeof(buf))))
		return FILEWRITE_ERROR;
	if(-1 == fprintf(fd, "      Descent Rate: %s\n", m_staticLib.YesNoString(av->descentRateAffected, buf, sizeof(buf))))
		return FILEWRITE_ERROR;
	if(-1 == fprintf(fd, "         Reversals: %s\n", m_staticLib.YesNoString(av->reversalAffected, buf, sizeof(buf))))
		return FILEWRITE_ERROR;
	if(-1 == fprintf(fd, "  Bottom Following: %s\n", m_staticLib.YesNoString(av->flatBottomDiveAffected, buf, sizeof(buf))))
		return FILEWRITE_ERROR;
	if(-1 == fprintf(fd, "        Dive Depth: %s\n", m_staticLib.YesNoString(av->depthAffected, buf, sizeof(buf))))
		return FILEWRITE_ERROR;

	if(-1 == fprintf(fd, "    Animat Beaches: %s\n", m_staticLib.YesNoString(av->beaches, buf, sizeof(buf))))
		return FILEWRITE_ERROR;
	if(-1 == fprintf(fd, "              Pods: "))
		return FILEWRITE_ERROR;

	if(av->podBreaksUp == FALSE)
		bytes = fprintf(fd, "Remain intact\n");
	else
		bytes = fprintf(fd, "Break into individuals\n");
	if(bytes <= 0)
		return FILEWRITE_ERROR;

	if(-1 == fprintf(fd, "\n"))
		return FILEWRITE_ERROR;
	if(-1 == fprintf(fd, "  Acoustic Aversion Modeling:\n"))
		return FILEWRITE_ERROR;
	if(-1 == fprintf(fd, "       Travel Rate:  mean %7.3f   std %7.3f  coeff %7.3f\n", av->travelRate.mean, av->travelRate.std, av->travelRate.coeff))
		return FILEWRITE_ERROR;
	if(-1 == fprintf(fd, "  Travel Direction:  pert %7.3f coeff %7.3f   bias %7.3f  arcstep %7.3f\n", av->travel.perturbation, av->travel.termCoeff, av->travel.bias, av->travel.arcStep))
		return FILEWRITE_ERROR;
	if(-1 == fprintf(fd, "             Depth:  mean %7.3f   std %7.3f  coeff %7.3f\n", av->depth.mean, av->depth.std, av->depth.coeff))
		return FILEWRITE_ERROR;
	if(-1 == fprintf(fd, "      Descent Rate:  mean %7.3f   std %7.3f  coeff %7.3f\n", av->descentRate.mean, av->descentRate.std, av->descentRate.coeff))
		return FILEWRITE_ERROR;
	if(-1 == fprintf(fd, "       Ascent Rate:  mean %7.3f   std %7.3f  coeff %7.3f\n", av->ascentRate.mean, av->ascentRate.std, av->ascentRate.coeff))
		return FILEWRITE_ERROR;
	if(-1 == fprintf(fd, "  Surface Interval:  mean %7.3f   std %7.3f  coeff %7.3f\n", av->surfaceInterval.mean, av->surfaceInterval.std, av->surfaceInterval.coeff))
		return FILEWRITE_ERROR;
	if(-1 == fprintf(fd, " Reversals\n"))
		return FILEWRITE_ERROR;
	if(-1 == fprintf(fd, "       probability:  coeff %7.3f\n", av->reversal.probOfReversal))
		return FILEWRITE_ERROR;
	if(-1 == fprintf(fd, "             count:  mean  %7.3f   std  %7.3f\n", av->reversal.count.mean, av->reversal.count.std))
		return FILEWRITE_ERROR;
	if(-1 == fprintf(fd, "          duration:  mean  %7.3f   std  %7.3f\n\n", av->reversal.time.mean, av->reversal.time.std))
		return FILEWRITE_ERROR;


//	if(-1 == fprintf(fd, "------------------------------------------------------------------------------------------------\n\n\n"))
//		return FILEWRITE_ERROR;

	if(-1 == fprintf(fd, "\n\n"))
		return FILEWRITE_ERROR;
//	if(-1 == fprintf(fd, "**************************\n"))
//		return FILEWRITE_ERROR;

    //-------------------//
    // (2) Behavior Count
    //-------------------//
    //------------------------//
    // (5) Behavior Allocation
    //------------------------//
	if(-1 == fprintf(fd, "Behavior count: %d\n", m_speciesModel.description.numBehaviors))
		return FILEWRITE_ERROR;


    //---------------------//
    // (4) Initial Behavior
    //---------------------//
//	if(OK != (res = MatrixToText(&m_speciesModel.initialBehavior, "***************************\n* Initial Behavior Matrix *\n***************************", fd)))
//		return res;
//	if(-1 == fprintf(fd, "**************************\n"))
//		return FILEWRITE_ERROR;

//	if(-1 == fprintf(fd, "------------------------------------------------------------------------------------------------\n\n\n"))
//		return FILEWRITE_ERROR;


//	if(-1 == fprintf(fd, "------------------------------------------------------------------------------------------------\n"))
//		return FILEWRITE_ERROR;
//	if(-1 == fprintf(fd, "------------------------------------------------------------------------------------------------\n\n\n"))
//		return FILEWRITE_ERROR;

	

	for(i=0; i<m_speciesModel.description.numBehaviors; i++)
	{
		beh = &m_speciesModel.p.behavior[i];

        //------------------//
        // (7) Behavior Name
        //------------------//
		if(-1 == fprintf(fd, "\n\n************************************************************\n"))
			return FILEWRITE_ERROR;
		if(-1 == fprintf(fd, "Behavior %d\n", i+1))
			return FILEWRITE_ERROR;
		if(-1 == fprintf(fd, "             Name:\"%s\"\n", m_speciesModel.p.behavior[i].szName))
			return FILEWRITE_ERROR;
		if(-1 == fprintf(fd, "************************************************************\n\n"))
			return FILEWRITE_ERROR;


        //-------------------------------------//
        // (8) Environmental Attractor Priority
        //-------------------------------------//
		if(-1 == fprintf(fd, " Env Attr Priority:"))
			return FILEWRITE_ERROR;
		if(beh->depthHasPriorityOverTemp == TRUE)
		{
			if(-1 == fprintf(fd, " Depth\n\n"))
				return FILEWRITE_ERROR;
		}
		else
		{
			if(-1 == fprintf(fd, " Temperature\n\n"))
				return FILEWRITE_ERROR;
		}



        //----------------------------------------//
        // (9) Depth environmental attractor
        // 8 parameters, including 2 vector models
        //----------------------------------------//
		//if(-1 == fprintf(fd, "  DEPTH ENVIRONMENTAL ATTRACTOR:"))
		//	return FILEWRITE_ERROR;

		if(-1 == fprintf(fd, "  NORMAL (TIME-BASED) BEHAVIOR TRANSITION:\n"))
			return FILEWRITE_ERROR;
//		if(OK != (res = MatrixToText(&beh->nrmlBehTransMatrix, "    Transition", fd)))
//			return res;
//		if(OK != (res = MatrixToText(&beh->nrmlBehTrans.behavior, "    Transition", fd)))
//			return res;
//		if(OK != (res = MatrixToText(&beh->nrmlBehTrans.terminate, "    Termination", fd)))
//			return res;


		if(-1 == fprintf(fd, "  DEPTH ENVIRONMENTAL ATTRACTOR BEHAVIOR TRANSITION:\n"))
			return FILEWRITE_ERROR;
		if(OK != (res = MatrixToText(&beh->depthEnvAttBehTrans.behavior, "    Transition", fd)))
			return res;
		if(OK != (res = MatrixToText(&beh->depthEnvAttBehTrans.terminate, "    Termination", fd)))
			return res;


		if(-1 == fprintf(fd, "  TEMPERATURE ENVIRONMENTAL ATTRACTOR BEHAVIOR TRANSITION:\n"))
			return FILEWRITE_ERROR;
		if(OK != (res = MatrixToText(&beh->tempEnvAttBehTrans.behavior, "    Transition", fd)))
			return res;
		if(OK != (res = MatrixToText(&beh->tempEnvAttBehTrans.terminate, "    Termination", fd)))
			return res;

#if 0
        beh->envAttrDepth
        beh->envAttrDepth.shelfEnabled
        beh->envAttrDepth.deep
        beh->envAttrDepth.deepEnabled
        beh->envAttrDepth.shallow
        beh->envAttrDepth.shallowEnabled
        beh->depthBehTrans.vector
        beh->depthBehTrans.element

        //-----------------------------------------//
        // (10) Temperature Environmental Attractor
        // 8 parameters, including 2 vector models
        //-----------------------------------------//
        beh->envAttrTemp.front
        beh->envAttrTemp.frontEnabled
        beh->envAttrTemp.warm
        beh->envAttrTemp.coldEnabled
        beh->envAttrTemp.cold
        beh->envAttrTemp.warmEnabled
        beh->temperatureBehTrans.vector
        beh->temperatureBehTrans.element
#endif



		if(-1 == fprintf(fd, "  MODEL TYPES:\n"))
			return FILEWRITE_ERROR;
		if(-1 == fprintf(fd, "      Travel Rate: %s\n", m_staticLib.ModelTypeToString(beh->travelRate.modelType, buf, sizeof(buf))))
			return FILEWRITE_ERROR;
		if(-1 == fprintf(fd, " Travel Direction: %s\n", m_staticLib.ModelTypeToString(beh->travelDirection.modelType, buf, sizeof(buf))))
			return FILEWRITE_ERROR;
		if(-1 == fprintf(fd, "       Dive Depth: %s\n", m_staticLib.ModelTypeToString(beh->dive.depth.modelType, buf, sizeof(buf))))
			return FILEWRITE_ERROR;
		if(-1 == fprintf(fd, " Dive Ascent Rate: %s\n", m_staticLib.ModelTypeToString(beh->dive.ascentRate.modelType, buf, sizeof(buf))))
			return FILEWRITE_ERROR;
		if(-1 == fprintf(fd, "Dive Descent Rate: %s\n", m_staticLib.ModelTypeToString(beh->dive.descentRate.modelType, buf, sizeof(buf))))
			return FILEWRITE_ERROR;
		if(-1 == fprintf(fd, " Surface Interval: %s\n", m_staticLib.ModelTypeToString(beh->dive.srfInv.modelType, buf, sizeof(buf))))
			return FILEWRITE_ERROR;
		if(-1 == fprintf(fd, "    Dive Reversal: "))
			return FILEWRITE_ERROR;
		if(beh->dive.reversal.reverses == FALSE)
		{
			if(-1 == fprintf(fd, "Does not engage in reversals while in this behavior"))
				return FILEWRITE_ERROR;
		}
		else
		{
			if(-1 == fprintf(fd, "%s\n", m_staticLib.ModelTypeToString(beh->dive.reversal.modelType, buf, sizeof(buf))))
				return FILEWRITE_ERROR;
			if(beh->dive.reversal.diveRateType == NO_INDEPENDENT)
			{
				if(-1 == fprintf(fd, "\t\t\tusing normal Dive/Acsent Rates."))
					return FILEWRITE_ERROR;
			}
			else if(beh->dive.reversal.diveRateType == INDEPENDENT)
			{
				if(-1 == fprintf(fd, "\t\t\tusing gaussian dive/ascent rates defined by mean %7.3f   std %7.3f  coeff %7.3f",
					beh->dive.reversal.diveRate.mean, beh->dive.reversal.diveRate.std,
					beh->dive.reversal.diveRate.coeff))
					return FILEWRITE_ERROR;
			}
			else // INDEPENDENT_DIVE_AND_ASCENT
			{
				if(-1 == fprintf(fd, "\t\t\tusing gaussian dive rates defined by mean %7.3f   std %7.3f  coeff %7.3f",
					beh->dive.reversal.diveRate.mean, beh->dive.reversal.diveRate.std,
					beh->dive.reversal.diveRate.coeff))
					return FILEWRITE_ERROR;
				if(-1 == fprintf(fd, "\t\t\tand  gaussian ascent rates defined by mean %7.3f   std %7.3f  coeff %7.3f",
					beh->dive.reversal.ascentRate.mean, beh->dive.reversal.ascentRate.std,
					beh->dive.reversal.ascentRate.coeff))
					return FILEWRITE_ERROR;
			}
		}

		
		//if(-1 == fprintf(fd, "USER DEFINED MATRIX MODEL\n"))
		//	return FILEWRITE_ERROR;
		//if(-1 == fprintf(fd, "------------------------------------------------------------------------------------------------\n"))
		//	return FILEWRITE_ERROR;
		if(-1 == fprintf(fd, "\n\n"))
			return FILEWRITE_ERROR;
//		if(OK != (res = MatrixToText(&beh->nrmlBehTransMatrix, "  Behavior Transition", fd)))
//			return res;
//		if(OK != (res = MatrixToText(&beh->nrmlBehTrans.behavior, "  Behavior Transition", fd)))
//			return res;
//		if(OK != (res = MatrixToText(&beh->nrmlBehTrans.terminate, "  Terminate Behavior", fd)))
//			return res;
		if(OK != (res = MatrixToText(&beh->travelRate.vm.vector, "  Rate Of Travel", fd)))
			return res;
		if(OK != (res = MatrixToText(&beh->travelRate.vm.step, "  Rate Of Travel Step", fd)))
			return res;
		if(OK != (res = MatrixToText(&beh->travelRate.vm.terminate, "  Terminate Rate Of Travel", fd)))
			return res;
		if(OK != (res = MatrixToText(&beh->travelDirection.vm.direction, "  Travel Direction", fd)))
			return res;
		if(OK != (res = MatrixToText(&beh->travelDirection.vm.directionalBias, "Travel Direction Bias", fd)))
			return res;
		if(OK != (res = MatrixToText(&beh->travelDirection.vm.terminate, "  Terminate Travel Direction", fd)))
			return res;
		if(OK != (res = MatrixToText(&beh->dive.depth.vm.vector, "  Dive Depth", fd)))
			return res;
		if(OK != (res = MatrixToText(&beh->dive.depth.vm.step, "  Dive Depth Step", fd)))
			return res;
		if(OK != (res = MatrixToText(&beh->dive.descentRate.vm.vector, "  Dive Descent Rate", fd)))
			return res;
		if(OK != (res = MatrixToText(&beh->dive.descentRate.vm.step, "  Dive Descent Rate Step", fd)))
			return res;
		if(OK != (res = MatrixToText(&beh->dive.descentRate.vm.terminate, "  Terminate Dive Descent Rate", fd)))
			return res;

		if(OK != (res = MatrixToText(&beh->dive.ascentRate.vm.vector, "  Dive Ascent Rate", fd)))
			return res;
		if(OK != (res = MatrixToText(&beh->dive.ascentRate.vm.step, "  Dive Ascent Rate Step", fd)))
			return res;
		if(OK != (res = MatrixToText(&beh->dive.ascentRate.vm.terminate, "  Terminate Dive Ascent Rate", fd)))
			return res;

		if(OK != (res = MatrixToText(&beh->dive.srfInv.vm.vector, "  Dive Surface Interval", fd)))
			return res;
		if(OK != (res = MatrixToText(&beh->dive.srfInv.vm.step,"  Dive Surface Interval Step",fd)))
			return res;

		if(OK != (res = MatrixToText(&beh->dive.reversal.vm.count, "  Dive Reversals", fd)))
			return res;
		if(OK != (res = MatrixToText(&beh->dive.reversal.vm.probOfReversal, "  Dive Reversals Probability", fd)))
			return res;
		if(OK != (res = MatrixToText(&beh->dive.reversal.vm.time, "  Dive Reversals Duration", fd)))
			return res;
		if(OK != (res = MatrixToText(&beh->dive.reversal.vm.timeStep, "  Dive Reversals Duration Step", fd)))
			return res;
		//fprintf(fd, "------------------------------------------------------------------------------------------------\n\n\n");

		fprintf(fd, "\n\n");

//		if(-1 == fprintf(fd, "ALTERNATIVE DISTRIBUTION MODEL DEFINITIONS\n"))
//			return FILEWRITE_ERROR;
//		if(-1 == fprintf(fd, "------------------------------------------------------------------------------------------------\n"))
//			return FILEWRITE_ERROR;
		if(-1 == fprintf(fd, "  Travel Rate ADM\n"))
			return FILEWRITE_ERROR;
//		if(-1 == fprintf(fd, "-----------------\n"))
//			return FILEWRITE_ERROR;
		if(-1 == fprintf(fd, "             gauss:  mean %7.3f   std %7.3f  coeff %7.3f\n", beh->travelRate.gauss.mean, beh->travelRate.gauss.std, beh->travelRate.gauss.coeff))
			return FILEWRITE_ERROR;
		if(-1 == fprintf(fd, "      uniform dist:   max %7.3f   min %7.3f  coeff %7.3f\n", beh->travelRate.rnd.max, beh->travelRate.rnd.min, beh->travelRate.rnd.coeff))
			return FILEWRITE_ERROR;

		
		fprintf(fd, "\n");
		if(-1 == fprintf(fd, "  Travel Direction ADM\n"))
			return FILEWRITE_ERROR;
//		if(-1 == fprintf(fd, "---------------\n"))
//			return FILEWRITE_ERROR;
		if(-1 == fprintf(fd, "          rnd walk: coeff %7.3f\n", beh->travelDirection.rndWalk.termCoeff))
			return FILEWRITE_ERROR;
		if(-1 == fprintf(fd, "     crld rnd walk:  pert %7.3f coeff %7.3f\n", beh->travelDirection.crRndWalk.perturbation, beh->travelDirection.crRndWalk.termCoeff))
			return FILEWRITE_ERROR;
		if(-1 == fprintf(fd, "  crld rnd walk db:  pert %7.3f   dir %7.3f   bias %7.3f  arc step %7.3f  coeff %7.3f\n",
			beh->travelDirection.crRndWalkDb.perturbation, beh->travelDirection.crRndWalkDb.directionOfBias,
			beh->travelDirection.crRndWalkDb.bias, beh->travelDirection.crRndWalkDb.arcStep,
			beh->travelDirection.crRndWalkDb.termCoeff))
			return FILEWRITE_ERROR;

		fprintf(fd, "\n");
		if(-1 == fprintf(fd, "  Dive Depth ADM\n"))
			return FILEWRITE_ERROR;
//		if(-1 == fprintf(fd, "-----------\n"))
//			return FILEWRITE_ERROR;
		if(-1 == fprintf(fd, "             gauss:  mean %7.3f   std %7.3f\n", beh->dive.depth.gauss.mean, beh->dive.depth.gauss.std))
			return FILEWRITE_ERROR;
		if(-1 == fprintf(fd, "      uniform dist:   max %7.3f\n", beh->dive.depth.rnd.max))
			return FILEWRITE_ERROR;

		fprintf(fd, "\n");
		if(-1 == fprintf(fd, "  Dive Descent Rate ADM\n"))
			return FILEWRITE_ERROR;
//		if(-1 == fprintf(fd, "---------------\n"))
//			return FILEWRITE_ERROR;
		if(-1 == fprintf(fd, "             gauss:  mean %7.3f   std %7.3f  coeff %7.3f\n", beh->dive.descentRate.gauss.mean, beh->dive.descentRate.gauss.std, beh->dive.descentRate.gauss.coeff))
			return FILEWRITE_ERROR;
		if(-1 == fprintf(fd, "      uniform dist:   max %7.3f   min %7.3f  coeff %7.3f\n", beh->dive.descentRate.rnd.max, beh->dive.descentRate.rnd.min, beh->dive.descentRate.rnd.coeff))
			return FILEWRITE_ERROR;


		fprintf(fd, "\n");
		if(-1 == fprintf(fd, "  Dive Ascent Rate ADM\n"))
			return FILEWRITE_ERROR;
//		if(-1 == fprintf(fd, "-----------------\n"))
//			return FILEWRITE_ERROR;
		if(-1 == fprintf(fd, "             gauss:  mean %7.3f   std %7.3f  coeff %7.3f\n", beh->dive.ascentRate.gauss.mean, beh->dive.ascentRate.gauss.std, beh->dive.ascentRate.gauss.coeff))
			return FILEWRITE_ERROR;
		if(-1 == fprintf(fd, "      uniform dist:   max %7.3f   min %7.3f  coeff %7.3f\n", beh->dive.ascentRate.rnd.max, beh->dive.ascentRate.rnd.min, beh->dive.ascentRate.rnd.coeff))
			return FILEWRITE_ERROR;


		// Surface Interval Model
		fprintf(fd, "\n");
		if(-1 == fprintf(fd, "   Dive Surface Interval ADM\n"))
			return FILEWRITE_ERROR;
		if(-1 == fprintf(fd, "----------------------\n"))
			return FILEWRITE_ERROR;
		if(-1 == fprintf(fd, "             gauss:  mean %7.3f   std %7.3f\n", beh->dive.srfInv.gauss.mean, beh->dive.srfInv.gauss.std))
			return FILEWRITE_ERROR;

		// Reversal Model
		fprintf(fd, "\n");
		if(-1 == fprintf(fd, "  Dive Reversal ADM\n"))
			return FILEWRITE_ERROR;
//		if(-1 == fprintf(fd, "--------------\n"))
//			return FILEWRITE_ERROR;
		if(-1 == fprintf(fd, "        gauss:      coeff %7.3f\n", beh->dive.reversal.gauss.probOfReversal))
			return FILEWRITE_ERROR;
		if(-1 == fprintf(fd, "             count:  mean %7.3f   std %7.3f\n", beh->dive.reversal.gauss.count.mean, beh->dive.reversal.gauss.count.std))
			return FILEWRITE_ERROR;
		if(-1 == fprintf(fd, "          duration:  mean %7.3f   std %7.3f\n\n", beh->dive.reversal.gauss.time.mean, beh->dive.reversal.gauss.time.std))
			return FILEWRITE_ERROR;
		if(-1 == fprintf(fd, " uniform dist:      coeff %7.3f\n", beh->dive.reversal.rnd.probOfReversal))
			return FILEWRITE_ERROR;
		if(-1 == fprintf(fd, "             count:   min %7.3f   max %7.3f\n", (float)beh->dive.reversal.rnd.count.min, (float)beh->dive.reversal.rnd.count.max))
			return FILEWRITE_ERROR;
		if(-1 == fprintf(fd, "          duration:  mean %7.3f   std %7.3f\n\n", beh->dive.reversal.rnd.time.mean, beh->dive.reversal.rnd.time.std))
			return FILEWRITE_ERROR;
	}
	return res;
}



///////////////////////////////////////////////
// I used fgets() instead of gets() because I could specify the max num characters to enter.
RESLT CSpeciesModel::ReadTextVectorModel(TCHAR *FileName)
{
//	RESLT res = OK;
//	TCHAR szBuff1[MAX_LINE_LENGTH];
//	int strLen;
//	int i = 0;

	FILE *fd = NULL;
	fopen_s(&fd, FileName, "r");
	if(fd == NULL)
		return OPENFILEREAD_ERROR;

	//**************************//
	// Don't implement this yet.
	//**************************//
#if 0

	while(res == OK)
	{
		res = ProcessNextLine(fd, szBuff1, MAX_USER_LINE_LENGTH, &strLen);
		if(res == OK_EOFREACHED)
		{
			res = OK;
			break;  // break out of the while-loop.
		}
		else if(res != OK)
			return res;

		// Remove the %
		if(strLen > 0)
			szBuff1[strLen - 1] = 0;

		// Skip Blank lines
		if(strcmp(szBuff1, "") == 0)
			i = 0; // Quiet compiler warning.

		// BehaviorTransition
		else if(strcmp(szBuff1, "behavior") == 0)
			res = ProcessTextVectorModel(fd, &m_behaviorModel_TOBEREMOVED.behavior);

		// Initial BehaviorTransition Matrix
		else if(strcmp(szBuff1, "initial_behavior") == 0)
			res = ProcessTextVectorModel(fd, &m_speciesModel.initialBehavior); 

		// Terminate BehaviorTransition Matrix
		else if(strcmp(szBuff1, "terminate_behavior") == 0)
			res = ProcessTextVectorModel(fd, &m_behaviorModel_TOBEREMOVED.terminate); 

		// TravelModel Matrix
		else if(strcmp(szBuff1, "rate_of_travel") == 0)
		{
			res = ProcessTextVectorModel(fd, &beh->travelRate.vm.vector);
			beh->travelRate.modelType = VECTOR;
		}

		// Rate Of TravelModel Step Value
		else if(strcmp(szBuff1, "rate_of_travel_step_value") == 0)
			res = ProcessTextVectorModel(fd, &beh->travelRate.vm.step);

		// Terminagt Rate Of TravelModel.
		else if(strcmp(szBuff1, "terminate_rate_of_travel") == 0)
			res = ProcessTextVectorModel(fd, &beh->travelRate.vm.terminate); 

		// Direction Matrix
		else if(strcmp(szBuff1, "direction") == 0)
		{
			res = ProcessTextVectorModel(fd, &beh->travelDirection.vm.direction);
			if(beh->travelDirection.modelType != VECTOR_MODEL_DIRECTIONAL_WITH_VECTOR_MODEL_BIASING)
				beh->travelDirection.modelType = VECTOR_MODEL_DIRECTIONAL_NO_BIASING;
		}

		// Direction Bias Matrix
		else if(strcmp(szBuff1, "direction_bias") == 0)
		{
			res = ProcessTextVectorModel(fd, &beh->travelDirection.vm.directionalBias);
			beh->travelDirection.modelType = VECTOR_MODEL_DIRECTIONAL_WITH_VECTOR_MODEL_BIASING;
		}

		// Terminate Direction Matrix
		else if(strcmp(szBuff1, "terminate_direction") == 0)
			res = ProcessTextVectorModel(fd, &beh->travelDirection.vm.terminate); 

		// Dive Matrix
		else if(strcmp(szBuff1, "dive") == 0)
		{
			res = ProcessTextVectorModel(fd, &beh->dive.depth.vm.vector);
			beh->dive.depth.modelType = VECTOR;
		}

		// Dive Step Value
		else if(strcmp(szBuff1, "depth_step_value") == 0)
			ProcessTextVectorModel(fd, &beh->dive.depth.vm.step);

		// Ascent Rate Matrix
		else if(strcmp(szBuff1, "rate_of_ascent") == 0)
		{
			res = ProcessTextVectorModel(fd, &beh->dive.ascentRate.vm.vector);
			beh->dive.ascentRate.modelType = VECTOR;
		}

		// Ascent Step Value
		else if(strcmp(szBuff1, "rate_of_ascent_step_value") == 0)
			res = ProcessTextVectorModel(fd, &beh->dive.ascentRate.vm.step);

		// Terminate Rate Of Ascent.
		else if(strcmp(szBuff1, "terminate_rate_of_ascent") == 0)
			res = ProcessTextVectorModel(fd, &beh->dive.ascentRate.vm.terminate);
			
		// Dive Rate
		else if(strcmp(szBuff1, "rate_of_dive") == 0)
		{
			res = ProcessTextVectorModel(fd, &beh->dive.descentRate.vm.vector);
			beh->dive.descentRate.modelType = VECTOR;
		}

		// Rate Of Dive Step Value
		else if(strcmp(szBuff1, "rate_of_dive_step_value") == 0)
			res = ProcessTextVectorModel(fd, &beh->dive.descentRate.vm.step);

		// Terminate Rate Of Dive
		else if(strcmp(szBuff1, "terminate_rate_of_dive") == 0)
			res = ProcessTextVectorModel(fd, &beh->dive.descentRate.vm.terminate); 

		// Surface Interval Matrix
		else if(strcmp(szBuff1, "surface_interval") == 0)
		{
			res = ProcessTextVectorModel(fd, &beh->dive.srfInv.vm.vector);
			beh->dive.srfInv.modelType = VECTOR;
		}

		// Surface Interval Step Value
		else if(strcmp(szBuff1, "surface_interval_step_value") == 0)
			res = ProcessTextVectorModel(fd, &beh->dive.srfInv.vm.step);

		// Reversals
		else if(strcmp(szBuff1, "reversals") == 0)
		{
			res = ProcessTextVectorModel(fd, &beh->dive.reversal.vm.count);
			beh->dive.reversal.modelType = VECTOR;
		}

		// Probability of Reversals.
		else if(strcmp(szBuff1, "prob_reversals") == 0)
			res = ProcessTextVectorModel(fd, &beh->dive.reversal.vm.probOfReversal); 

		// Time Reversed Matrix
		else if(strcmp(szBuff1, "time_reversed") == 0)
			res = ProcessTextVectorModel(fd, &beh->dive.reversal.vm.time); 

		// Time Reversed Step Value
		else if(strcmp(szBuff1, "time_reversed_step_value") == 0)
			res = ProcessTextVectorModel(fd, &beh->dive.reversal.vm.timeStep); 
		else
			return UNRECOGNIZED_SPECIES_MATRIX_PARAM_ERROR;
	}
#endif

	fclose(fd);
//	return res;
	return OPENFILEREAD_ERROR; // put back to return res when implemented.
	fclose(fd); // keep so compiler warning will serve as a reminder to come back to this.

}



RESLT CSpeciesModel::WriteVector(HANDLE hd, MATRIX *M)
{
	DWORD bytes;
	int i;
	TCHAR _byte = 0;
	int totalBytesWritten = 0;

	for(i=0; i< M->rowCnt; i++)
	{
		if(WriteFile(hd, M->p.ppa[i], M->colCnt * sizeof(double), &bytes, NULL) == FALSE)
			return FILEWRITE_ERROR;
		totalBytesWritten += (int)bytes;
	}

	// Maintain 16-bytes alignment in the file.
	for(i=0; i<(16 - totalBytesWritten%16)%16; i++)
	{
		if(WriteFile(hd, &_byte, sizeof(TCHAR), &bytes, NULL) == FALSE)
			return FILEWRITE_ERROR;
	}

	return OK;
}

RESLT CSpeciesModel::WriteVector(HANDLE hd, ARRAY *M)
{
	DWORD bytes;
	int i;
	TCHAR _byte = 0;
	int totalBytesWritten = 0;

	for(i=0; i< M->rowCnt; i++)
	{
		if(WriteFile(hd, &M->p.pa[i], M->colCnt * sizeof(double), &bytes, NULL) == FALSE)
			return FILEWRITE_ERROR;
		totalBytesWritten += (int)bytes;
	}
	
	// Maintain 16-bytes alignment in the file.
	for(i=0; i<(16 - totalBytesWritten%16)%16; i++)
	{
		if(WriteFile(hd, &_byte, sizeof(TCHAR), &bytes, NULL) == FALSE)
			return FILEWRITE_ERROR;
	}	
	return OK;
}

RESLT CSpeciesModel::WriteVector(HANDLE hd, ELEMENT *M)
{
	DWORD bytes;
	int i;
	TCHAR _byte = 0;
	int totalBytesWritten = 0;

	for(i=0; i< M->rowCnt; i++)
	{
		if(WriteFile(hd, &M->a, M->colCnt * sizeof(double), &bytes, NULL) == FALSE)
			return FILEWRITE_ERROR;
		totalBytesWritten += (int)bytes;
	}

	// Maintain 16-bytes alignment in the file.
	for(i=0; i<(16 - totalBytesWritten%16)%16; i++)
	{
		if(WriteFile(hd, &_byte, sizeof(TCHAR), &bytes, NULL) == FALSE)
			return FILEWRITE_ERROR;
	}
	return OK;
}

RESLT CSpeciesModel::ReadVector(HANDLE hd, MATRIX *M)
{
	DWORD bytes;
	RESLT res = OK;
	int i;
	TCHAR _byte = 0;
	int totalBytesRead = 0;

	// Allocate memory for each row, return error if there is a memory allocation error.
	if(M->rowCnt > 0)
	{
		M->p.ppa = (double **)malloc(M->rowCnt * sizeof(double *));
		if(M->p.ppa == NULL)
			return MEMALLOC_ERROR;
	}

	// Initialize the newly allocated pointers to NULL, just in case there is a memory allocation error when allocating
	// memory for the pointers to point to.
	for(i=0; i< M->rowCnt; i++)
		M->p.ppa[i] = NULL;

	// Allocate memory for each column, verify the memory got allocated, and read in the value from file. Return error 
	// if problem.
	for(i=0; i< M->rowCnt && res == OK; i++)
	{
		M->p.ppa[i] = (double *)malloc(M->colCnt * sizeof(double));
		if(M->p.ppa[i] == NULL)
			res = MEMALLOC_ERROR;

		memset(M->p.ppa[i], 0, M->colCnt * sizeof(double));
		if(ReadFile(hd, M->p.ppa[i], M->colCnt * sizeof(double), &bytes, NULL) == FALSE)
			res = FILEREAD_ERROR;

		totalBytesRead += (int)bytes;
	}

	// Read in the extra bytes written to maintain 16-bytes alignment in the file.
	for(i=0; i<(16 - totalBytesRead%16)%16; i++)
	{
		if(ReadFile(hd, &_byte, sizeof(TCHAR), &bytes, NULL) == FALSE)
			return FILEWRITE_ERROR;
	}
	return res;
}

RESLT CSpeciesModel::ReadVector(HANDLE hd, ARRAY *M)
{
	DWORD bytes;
	TCHAR _byte = 0;
	int totalBytesRead = 0;
	int i;

	if(M->rowCnt <= 0)
		return OK;

	// Allocate memory for the array.
	M->p.pa = (double *)malloc(M->colCnt * sizeof(double));
	if(M->p.pa == NULL)
		return MEMALLOC_ERROR;

	memset(M->p.pa, 0, M->colCnt * sizeof(double));
	if(ReadFile(hd, M->p.pa, M->colCnt * sizeof(double), &bytes, NULL) == FALSE)
		return FILEREAD_ERROR;

	totalBytesRead = (int)bytes;

	// Read in the extra bytes written to maintain 16-bytes alignment in the file.
	for(i=0; i<(16 - totalBytesRead%16)%16; i++)
	{
		if(ReadFile(hd, &_byte, sizeof(TCHAR), &bytes, NULL) == FALSE)
			return FILEWRITE_ERROR;
	}
	return OK;
}

RESLT CSpeciesModel::ReadVector(HANDLE hd, ELEMENT *M)
{
	DWORD bytes;
	TCHAR _byte = 0;
	int totalBytesRead = 0;
	int i;

	M->a = 0;
	if(ReadFile(hd, &M->a, sizeof(double), &bytes, NULL) == FALSE)
		return FILEREAD_ERROR;

	totalBytesRead = (int)bytes;

	for(i=0; i<(16 - totalBytesRead%16)%16; i++)
	{
		if(ReadFile(hd, &_byte, sizeof(TCHAR), &bytes, NULL) == FALSE)
			return FILEWRITE_ERROR;
	}
	return OK;
}


RESLT CSpeciesModel::ProcessTextVectorModel(FILE *Fd, MATRIX *M)
{
	RESLT res = OK;

	int i, j;
	TCHAR szBuff[MAX_LINE_LENGTH]; // Buffer for data read from txt file
	float value  = 1.f; // need to have this assignment due to bug in microsoft when using sscanf_s().
	int strLen;
	int bufferPointer = 0;

	if(OK != (res = ProcessNextLine(Fd, szBuff, MAX_USER_LINE_LENGTH, &strLen)))
		return res;

	if(0 == sscanf_s(szBuff, "%d\t%d", &M->rowCnt, &M->colCnt))
		return FILEREAD_ERROR;

	if(M->rowCnt > 0)
		M->p.ppa = (double **)malloc(M->rowCnt * sizeof(double *));
	if(M->p.ppa == NULL)
		return MEMALLOC_ERROR;

	for(i=0; i<M->rowCnt; i++)
	{
		M->p.ppa[i] = (double *)malloc(M->colCnt * sizeof(double));
		if(M->p.ppa[i] == NULL)
			return MEMALLOC_ERROR;
	}

	for(i=0; i<M->rowCnt; i++)
	{
		if(OK != (res = ProcessNextLine(Fd, szBuff, MAX_USER_LINE_LENGTH, &strLen)))
			return res;
		bufferPointer = 0;
		for(j=0; j<M->colCnt; j++)
		{
			if(0 == sscanf_s(&szBuff[bufferPointer], "%f", &value))
				return FILEREAD_ERROR;
			M->p.ppa[i][j] = (double)value;

			// advance the buffer pointer
			while(szBuff[bufferPointer] != '\t' && bufferPointer < strLen)
				bufferPointer++;
			bufferPointer++;
		}
	}
	return res;
}


RESLT CSpeciesModel::ProcessTextVectorModel(FILE *Fd, ARRAY *M)
{
	RESLT res = OK;

	int j;
	TCHAR szBuff[MAX_LINE_LENGTH]; // Buffer for data read from txt file
	float value  = 1.f; // need to have this assignment due to bug in microsoft when using sscanf_s().
	int strLen;
	int bufferPointer = 0;

	// There will still be a num rows and num colums.  Num rows will be 1.
	if(OK != (res = ProcessNextLine(Fd, szBuff, MAX_USER_LINE_LENGTH, &strLen)))
		return res;

	if(0 == sscanf_s(szBuff, "%d\t%d", &M->rowCnt, &M->colCnt))
		return FILEREAD_ERROR;

	if(M->rowCnt <= 0)
		return OK;

	if(NULL == (M->p.pa = (double *)malloc(M->colCnt * sizeof(double))))
		return MEMALLOC_ERROR;

	// Read in the single line making up the row.
	if(OK != (res = ProcessNextLine(Fd, szBuff, MAX_USER_LINE_LENGTH, &strLen)))
		return res;
	bufferPointer = 0;

	for(j=0; j<M->colCnt; j++)
	{
		if(0 == sscanf_s(&szBuff[bufferPointer], "%f", &value))
			return FILEREAD_ERROR;
		M->p.pa[j] = (double)value;

		// advance the buffer pointer
		while(szBuff[bufferPointer] != '\t' && bufferPointer < strLen)
			bufferPointer++;
		bufferPointer++;
	}
	return res;
}


RESLT CSpeciesModel::ProcessTextVectorModel(FILE *Fd, ELEMENT *M)
{
	RESLT res = OK;

	TCHAR szBuff[MAX_LINE_LENGTH]; // Buffer for data read from txt file
	float value  = 1.f; // need to have this assignment due to bug in microsoft when using sscanf_s().
	int strLen;
	int bufferPointer = 0;

	if(OK != (res = ProcessNextLine(Fd, szBuff, MAX_USER_LINE_LENGTH, &strLen)))
		return res;

	if(0 == sscanf_s(szBuff, "%d\t%d", &M->rowCnt, &M->colCnt))
		return FILEREAD_ERROR;

	if(OK != (res = ProcessNextLine(Fd, szBuff, MAX_USER_LINE_LENGTH, &strLen)))
		return res;

	if(0 == sscanf_s(&szBuff[bufferPointer], "%f", &value))
		return FILEREAD_ERROR;

	M->a = (double)value;
	return res;
}


// Returns line length or -1 if maximum line length is exceeded.
// Assumes *Buffer's length is greater than MaxLength
RESLT CSpeciesModel::ProcessNextLine(FILE *Fd, TCHAR *Buffer, int MaxLength, int *StringLength)
{
	if(fgets(Buffer, MaxLength, Fd) == NULL)
		return OK_EOFREACHED; // end of file reached.

	*StringLength = strlen(Buffer);
	if(Buffer[*StringLength-1] == '\n')
	{
		Buffer[*StringLength-1] = 0;
		*StringLength = *StringLength - 1;
	}

	if(*StringLength > MaxLength)
		return USERMODELLINELENGTHEXCEEDED_ERROR;

	return OK;
}

void CSpeciesModel::FreeMatrix(MATRIX *M)
{
	int i;

	if(M->p.ppa == NULL)
		return;

	for(i=M->rowCnt-1; i>=0; i--)
	{
		// If this row never got allocated, skip it...
		if(M->p.ppa[i] == NULL)
			continue;
		free(M->p.ppa[i]);
		M->p.ppa[i] = 0;
	}
	free(M->p.ppa);
	M->p.ppa = NULL;
	M->colCnt = 0;
	M->rowCnt = 0;
}

void CSpeciesModel::FreeMatrix(ARRAY *M)
{
	if(M->p.pa == NULL)
		return;

	free(M->p.pa);
	M->p.pa = NULL;
	M->colCnt = 0;
	M->rowCnt = 0;
}

void CSpeciesModel::FreeMatrix(ELEMENT *M)
{
	M->a = 0;
	M->colCnt = 1;
	M->rowCnt = 1;
}

// No memory deallocation, use with caution.
void CSpeciesModel::ClearMatrix(MATRIX *M)
{
	M->p.ppa = NULL;
	M->colCnt = 0;
	M->rowCnt = 0;
}

// No memory deallocation, use with caution.
void CSpeciesModel::ClearMatrix(ARRAY *M)
{
	M->p.pa = NULL;
	M->colCnt = 0;
	M->rowCnt = 0;
}

// No memory deallocation, use with caution.
void CSpeciesModel::ClearMatrix(ELEMENT *M)
{
	M->a = 0;
	M->colCnt = 1;
	M->rowCnt = 1;
}


RESLT CSpeciesModel::MatrixToText(ELEMENT *M, TCHAR *MatrixTitle, FILE *Fd)
{
	if(!M || M->a <= 0)
		return OK;

	if(-1 == fprintf(Fd, "%s\n1\t1\n%6.4f\n\n\n", MatrixTitle, M->a))
		return FILEWRITE_ERROR;

	return OK;
}


RESLT CSpeciesModel::MatrixToText(ARRAY *M, TCHAR *MatrixTitle, FILE *Fd)
{
	int j;

	if(!M->p.pa)
		return OK;

	if(-1 == fprintf(Fd, "%s\n", MatrixTitle))
		return FILEWRITE_ERROR;
	if(-1 == fprintf(Fd, "1\t%d\n", M->colCnt))
		return FILEWRITE_ERROR;

	for(j=0; j<M->colCnt; j++)
	{
		if(-1 == fprintf(Fd, "%6.4f\t", M->p.pa[j]))
			return FILEWRITE_ERROR;
	}
	if(-1 == fprintf(Fd, "\n\n"))
		return FILEWRITE_ERROR;

	return OK;
}

RESLT CSpeciesModel::MatrixToText(MATRIX *M, TCHAR *MatrixTitle, FILE *Fd)
{
	int i,j;

	if(!M->p.ppa)
		return OK;

	if(-1 == fprintf(Fd, "%s\n", MatrixTitle))
		return FILEWRITE_ERROR;
	if(-1 == fprintf(Fd, "%d\t%d\n", M->rowCnt, M->colCnt))
		return FILEWRITE_ERROR;
	for(i=0; i<M->rowCnt; i++)
	{
		for(j=0; j<M->colCnt; j++)
		{
			if(-1 == fprintf(Fd, "%6.4f\t", M->p.ppa[i][j]))
				return FILEWRITE_ERROR;
		}
		if(-1 == fprintf(Fd, "\n"))
			return FILEWRITE_ERROR;
	}
	if(-1 == fprintf(Fd, "\n\n"))
		return FILEWRITE_ERROR;

	return OK;
}

int CSpeciesModel::MatrixToText(ELEMENT *M, TCHAR *szBuffer, int BufferSize)
{
	if(!M || M->a <= 0)
		return _snprintf_s(szBuffer, BufferSize, BufferSize, "[]");

	return _snprintf_s(szBuffer, BufferSize, BufferSize, "%6.4f", M->a);
}


int CSpeciesModel::MatrixToText(ARRAY *M, TCHAR *szBuffer, int BufferSize)
{
	int j;
	int numBytes = 0;

	if(!M->p.pa)
		return _snprintf_s(szBuffer, BufferSize, BufferSize, "[]");

	for(j=0; j<M->colCnt-1; j++)
		numBytes += _snprintf_s(&szBuffer[numBytes], BufferSize-numBytes, BufferSize-numBytes, "%6.4f ", M->p.pa[j]);
	numBytes += _snprintf_s(&szBuffer[numBytes], BufferSize-numBytes, BufferSize-numBytes, "%6.4f", M->p.pa[j]);

	return numBytes;
}

int CSpeciesModel::MatrixToText(MATRIX *M, TCHAR *szBuffer, int BufferSize)
{
	int i,j;
	int numBytes = 0;
	BOOL isTrue = FALSE;

	if(M->p.ppa == NULL || M->colCnt == 0 || M->rowCnt == 0)
		return _snprintf_s(szBuffer, BufferSize, BufferSize, "[]");

	// See if any numbers are greater than or equal to 10.
	for(i=0; i<M->rowCnt && isTrue == FALSE; i++)
		for(j=0; j<M->colCnt && isTrue == FALSE; j++)
			if(M->p.ppa[i][j] >= 9.99999999999)
				isTrue = TRUE;

	for(i=0; i<M->rowCnt; i++)
	{
		for(j=0; j<M->colCnt-1; j++)
		{
			if(isTrue == FALSE)
				numBytes += _snprintf_s(&szBuffer[numBytes], BufferSize-numBytes, BufferSize-numBytes,"%6.4f ", M->p.ppa[i][j]);
			else
				numBytes += _snprintf_s(&szBuffer[numBytes], BufferSize-numBytes, BufferSize-numBytes,"%7.4f ", M->p.ppa[i][j]);

		}
		if(isTrue == FALSE)
			numBytes += _snprintf_s(&szBuffer[numBytes], BufferSize-numBytes, BufferSize-numBytes,"%6.4f%s", M->p.ppa[i][j], CRLF);
		else
			numBytes += _snprintf_s(&szBuffer[numBytes], BufferSize-numBytes, BufferSize-numBytes,"%7.4f%s", M->p.ppa[i][j], CRLF);

	}
	return numBytes;
}

// Modify this so it returns something useful.
int CSpeciesModel::TextToMatrix(ELEMENT *M, TCHAR *szBuffer)
{
	int   rowCnt = 0;
	int   numColumns = 0;
	float f;
	int   index;

	RemoveUnneededCharacters(szBuffer);
	rowCnt = CountNumRows(szBuffer);
	numColumns = CountNumColums(szBuffer);

	// Verify proper GUI handling of user input
	_ASSERTE(rowCnt <= 1);
	if(rowCnt == 0)
        _ASSERTE(numColumns == 0);
	else
        _ASSERTE(numColumns != 0);

	// Force rowCnt and num columns to 1 since this is an element.
	M->rowCnt = M->colCnt = 1;
	if(rowCnt == 0)
	{
		M->a = 0;
		return 0;
	}

	// Skip over any spaces
	index = 0;
	while(szBuffer[index] == ' ')
		index++;

	sscanf_s(szBuffer, "%f", &f);
	M->a = (double)f;
	return 0;
}


int CSpeciesModel::TextToMatrix(ARRAY *M, TCHAR *szBuffer)
{
	int   rowCnt = 0;
	int   numColumns = 0;
	int	  numAssigned;
	float f;
	int   buffIndex;
	int   strLen;
	int   columnNumber;

	RemoveUnneededCharacters(szBuffer);
	rowCnt = CountNumRows(szBuffer);
	numColumns = CountNumColums(szBuffer);

	//-----------------------------------------//
	// Verify proper GUI handling of user input
	//-----------------------------------------//
	_ASSERTE(rowCnt <= 1);
	if(rowCnt == 0)
        _ASSERTE(numColumns == 0);
	else
        _ASSERTE(numColumns > 0);
	//-----------------------------------------//

	// Allocate the array
	if(M->colCnt != numColumns)
	{
		FreeMatrix(M);
		AllocateMatrix(M, 1, numColumns);
	}

	strLen		 = strlen(szBuffer);
	buffIndex	 = 0;
	columnNumber = 0;

    memset(M->p.pa, 0, M->colCnt*sizeof(double));

	while(buffIndex < strLen && szBuffer[buffIndex] != 13 /*CR*/ && szBuffer[buffIndex] != 10)
	{
		if(szBuffer[buffIndex] != ' ')
		{
			// Has to be either a number, decimal point, or minus sign.
			if(0 != (numAssigned = sscanf_s(&szBuffer[buffIndex], "%f", &f)))
				M->p.pa[columnNumber] = (double)f;
			else
				M->p.pa[columnNumber] = 0;

			columnNumber++;
			// Skip over the remaining non-space characters.
			while(buffIndex < strLen && szBuffer[buffIndex] != ' ')
				buffIndex++;
			continue; //loop back to the top of the while-loop.
		}
		buffIndex++;
	}
	return 0;
}

int CSpeciesModel::TextToMatrix(MATRIX *M, TCHAR *szBuffer)
{
	int rowCnt = 0;
	int numColumns = 0;
	int numAssigned;
	float f;
	int   i;
	int   index;
	int   strLen;
	int   columnNumber;
	int   rowNumber;

	RemoveUnneededCharacters(szBuffer);
	rowCnt = CountNumRows(szBuffer);
	numColumns = CountNumColums(szBuffer);

	// Handle case when rows and/or columns equal 0.
	if(rowCnt == 0 || numColumns == 0)
        _ASSERTE(numColumns == 0 && rowCnt == 0);
	else if(rowCnt >= 1 || numColumns >= 1)
        _ASSERTE(numColumns >= 1 && rowCnt >= 1);

	// Allocate the matrix
	if(M->colCnt != numColumns || M->rowCnt != rowCnt)
	{
		FreeMatrix(M);
		AllocateMatrix(M, rowCnt, numColumns);
	}

	strLen		 = strlen(szBuffer);
	index		 = 0;
	rowNumber    = 0;
	columnNumber = 0;

	for(i=0; i<M->rowCnt; i++)
        memset(M->p.ppa[i], 0, M->colCnt*sizeof(double));
	while(index < strLen)
	{
		// Skip over all spaces.
		if(szBuffer[index] != ' ')
		{
			// Either a number charater encountered ('0'-'9', '-', or '.') or 13 (CR) or
			// 10 (LF)
			if(szBuffer[index] == 13 /*CR*/)
			{
				rowNumber++;
				columnNumber = 0;

				// Skip past the CRLF
				index++;
				if(szBuffer[index] == 10 /*LF*/)
					index++;
				continue; // This will take the loop to the top of the while loop (skips past the index increment.
			}
			else
			{
				// A number is reached in the buffer.
				if(0 != (numAssigned = sscanf_s(&szBuffer[index], "%f", &f)))
					M->p.ppa[rowNumber][columnNumber] = (double)f;
				else
					M->p.ppa[rowNumber][columnNumber] = 0;
				columnNumber++;

				// Skip past the remainder of the numeric string.
				while(index < strLen && szBuffer[index] != ' ' && szBuffer[index] != 13 && szBuffer[index] != 10)
					index++;
				continue; // This will take the loop to the top of the while loop (skips past the index increment.
			}
		}
		index++;
	}
	return 0;
}

BOOL CSpeciesModel::AllocateMatrix(MATRIX *M, int NumRows, int NumCols)
{
	int i, j;

	// Programming debug checks
	_ASSERT(M->p.ppa == NULL && M->rowCnt == 0 && M->colCnt == 0);
	_ASSERT(NumRows !=0 && NumCols != 0);
	memset(M, 0, sizeof(MATRIX));

	if(NumRows == 0 || NumCols == 0)
	{
		M->rowCnt = M->colCnt = 0;
		return TRUE;
	}

	M->p.ppa = (double **)malloc(NumRows * sizeof(double *));
	if(M->p.ppa == NULL)
		return FALSE; //MEMALLOC_ERROR;

	for(i=0; i<NumRows; i++)
	{
		M->p.ppa[i] = (double *)malloc(NumCols * sizeof(double));
		if(M->p.ppa[i] == NULL)
		{
			for(j=0; j<i; j++)
				free(M->p.ppa[j]);
			free(M->p.ppa);
			memset(M, 0, sizeof(MATRIX));
			return FALSE; //MEMALLOC_ERROR;
		}
	}
	M->rowCnt = NumRows;
	M->colCnt = NumCols;
	return TRUE;
}
BOOL CSpeciesModel::AllocateMatrix(ARRAY *M, int NumRows, int NumCols)
{
	// Programming debug checks
	_ASSERT(M->p.pa == NULL && M->rowCnt == 0 && M->colCnt == 0);
	_ASSERT(NumCols != 0);
	_ASSERT(NumRows == 1);

	memset(M, 0, sizeof(ARRAY));

	if(NumCols == 0)
		return TRUE;

	if(NULL == (M->p.pa = (double *)malloc(NumCols * sizeof(double))))
	{
		memset(M, 0, sizeof(ARRAY));
		return FALSE; //MEMALLOC_ERROR;
	}

	M->rowCnt = 1;
	M->colCnt = NumCols;

	return TRUE;
}

BOOL CSpeciesModel::AllocateMatrix(ELEMENT *M, int NumRows, int NumCols)
{
	// Programming debug checks
	_ASSERT(M->rowCnt == 0 && M->colCnt == 0);
	_ASSERT(NumCols == 1 && NumRows == 1);

	memset(M, 0, sizeof(ELEMENT));

	M->rowCnt = 1;
	M->colCnt = 1;
	return TRUE;
}


void CSpeciesModel::RemoveUnneededCharacters(TCHAR *szBuff)
{
	int strLen = strlen(szBuff);
	int i;

	for(i=0; i<strLen; i++)
	{
		if(szBuff[i] != 32 /*space*/ && szBuff[i] != 10 /*LF*/ && szBuff[i] != 13 /*CR*/ && szBuff[i] != 46 && /*.*/
			(szBuff[i] < 48  || szBuff[i] > 57)) /* below '0' or above '9' */
			szBuff[i] = 32; // make it a space.
	}
}

int CSpeciesModel::CountNumRows(TCHAR *szBuff)
{
	int strLen = 0;
	int i = -1;
	int lineCount = 0;

	// Make sure a valid string was passed in, get the string length.
	if(szBuff != NULL)
		strLen = strlen(szBuff);
	if(strLen == 0)
		return 0;

	while(i<strLen)
	{
		while(++i < strLen)
		{
			// Check for digits and decimal points.  Count the first one as a line then break.
			if((szBuff[i]>=48 && szBuff[i]<= 57) || szBuff[i] == '.')
			{
				lineCount++;
				break;
			}
		}

		// Advance in the buffer until a linefeed is detected. 
		while(i++ < strLen && szBuff[i]!=13 /*LF*/)
			;
	}
	return lineCount;
}

int CSpeciesModel::CountNumColums(TCHAR *szBuff)
{
	int  strLen = 0;
	int  i = -1;
	int  columnCount = 0;
	int  maxColumnCount = 0;
	int	 rowCount = 0;
	BOOL firstRowChecked = FALSE;

	// Make sure a valid string was passed in, get the string length.
	if(0 == (rowCount = CountNumRows(szBuff)))
		return 0;
	if(szBuff != NULL)
		strLen = strlen(szBuff);
	if(strLen == 0)
		return 0;

	while(++i<strLen)
	{
		while(i<strLen && szBuff[i] != 10 /*LF*/)
		{
			// Check if a number or decimal point reached (anything other than a space or
			// CR).
			if(szBuff[i] != ' ' && szBuff[i] != 13 /*CR*/)
			{
				columnCount++;
				// Skip past remaining numbers and decimal places in for this element in
				// this column.
				i++;
				while(i < strLen && (szBuff[i]>=48 && szBuff[i]<= 57) || szBuff[i] == '.')
					i++;
				continue;
			}
			i++;
		}

		// End of the row has been reached.
		if(firstRowChecked == FALSE)
		{
			firstRowChecked = TRUE;
			maxColumnCount = columnCount;
		}
		else
		{
			if(maxColumnCount < columnCount)
				maxColumnCount = columnCount;
		}
		columnCount = 0;
	}
	return maxColumnCount;
}

/*****************************************************************************************
* MEMBER FUNCTION: GetSubModelVectorStatus(BEHAVIOR_MODEL_GETSREPLACEd* Md) for the BehaviorTransition Vectpr
*                  Model.
* 
* DESCRIPTION: Gets the Vector Model Status (valid, invalid, etc..) for the 
*              behavioral vector model.
*
* ARGUMENTS:
*	Submodel: A pointer to a BEHAVIOR_MODEL_GETSREPLACEd structure.
*
* RETURN VALUE:
* A SUBMODEL_VECTOR_STATUS
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   02/27/07  (MJC)  Initial Coding/
*
*
* NOTES:
*   ** BehaviorTransition Vector Model **
*   For N behaviors:
*      number of rows:    N (user specified).
*      number of columns: N + 1
*    Columns are N+1 because the first column is populated with leading zeros.
*
*   ** Initial BehaviorTransition Vector Model (optional) **
*   For N behaviors:
*      number of rows:    variable (user specified) or zero if not used.
*      number of columns: N + 3    or zero if not used.
*   Rows are variable because each row represents a time span in the 24-hour clock and
*   the user may define the individual time span however they want with varying
*   granularity.
*   Columns are N+3 because the first two are time span, the third holds the leading
*   zeros, and the remainder are one for each behavior.
*
*   ** Terminate BehaviorTransition Vector Model **
*   For N behaviors:
*      number of rows:    1
*      number of columns: N
*****************************************************************************************/
#if 0
SUBMODEL_VECTOR_STATUS CSpeciesModel::GetSubModelVectorStatus(BEHAVIOR_MODEL_GETSREPLACEd *Mdl)
{// delte this
	return VALID;
}
#endif
#if 0

SUBMODEL_VECTOR_STATUS CSpeciesModel::GetSubModelVectorStatus(BEHTRANSMDL* Md, MATRIX *Initial)
{
	int N = Md->behavior.rowCnt; // Number of behaviors modeled.

	//----------------------------------------------------------------------------------//
	// BehaviorTransition Model Vector 
	//----------------------//
	// Check if defined.
	if(Md->behavior.rowCnt == 0 || Md->behavior.colCnt == 0 || Md->behavior.a == NULL)
	{
		_ASSERTE(Md->behavior.colCnt == 0 && Md->behavior.rowCnt == 0 && Md->behavior.a == NULL);
		return BEHAVIOR_UNDEFINED;
	}

	if(Md->behavior.colCnt != N+1)
		return BEHAVIOR_BADFORMAT;

	//----------------------------------------------------------------------------------//
	// Initial BehaviorTransition Model Vector (optional, so missing initial behavior is OK).
	//------------------------------//
	// Check if defined.  If it is defined, its number of rows must match the number of
	// columns in the BehaviorTransition Model Vector.
	if(Initial->rowCnt == 0 || Initial->colCnt == 0 || Initial->a == NULL)
		_ASSERTE(Initial->colCnt == 0 && Initial->rowCnt == 0 && Initial->a == NULL);
	else if(Initial->colCnt != N+3)
		return INITBEHAVIOR_BEHAVIOR_MISMATCH;
		

	//----------------------------------------------------------------------------------//
	// Terminate BehaviorTransition Model Vector 
	//--------------------------------//
	if(Md->terminate.rowCnt == 0 || Md->terminate.colCnt == 0 || Md->terminate.a == NULL)
	{
		_ASSERTE(Md->terminate.rowCnt == 0 && Md->terminate.colCnt == 0 && Md->terminate.a == NULL);
		return TERMBEHAVIOR_UNDEFINED;
	}
	else
	{	
		// Can only have a single row.  If it doesn't, a programming error.
		_ASSERTE(Md->terminate.rowCnt == 1 && Md->terminate.a != NULL);
		if(Md->terminate.colCnt != N)
			return TERMBEHAVIOR_BEHAVIOR_MISMATCH;
	}
	return VALID;
}
#endif

/*****************************************************************************************
* MEMBER FUNCTION: GetSubModelVectorStatus(RATE_MODEL* Md) for the Rate Vector Models.
* 
* DESCRIPTION: Gets the Vector Model Status (valid, invalid, etc..) for rate vector models
*
* ARGUMENTS:
*	Md: A pointer to a RATE_MODEL structure.
*
* RETURN VALUE:
* A SUBMODEL_VECTOR_STATUS
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   02/27/07  (MJC)  Initial Coding/
*
*
* NOTES:
*   ** Rate Vector Model **
*   For N behaviors:
*      number of rows:    1 or N (1 (single vector) if no behavioral influence).
*      number of columns: variable (user specified).
*    Columns are N+1 because the first column is populated with leading zeros.
*
*   ** Step Size **
*      number of rows:    1 (always).
*      number of columns: 1 (always)
*
*   ** Terminate Rate Vector Model **
*   For N behaviors:
*      number of rows:    1
*      number of columns: 1 or N (1 if behaviors do not influence termination)
*****************************************************************************************/
# if 0
SUBMODEL_VECTOR_STATUS CSpeciesModel::GetSubModelVectorStatus(RATE_MODEL* Md)
{
	int N = m_behaviorModel_TOBEREMOVED.behavior.rowCnt; // Number of behaviors in the BehaviorTransition
											   // Vector Model.

	// Check the BehaviorTransition Matrix Model
	if(VALID != GetSubModelVectorStatus(&m_behaviorModel_TOBEREMOVED))
		return BEHAVIOR_MDL;

	//----------------------------------------------------------------------------------//
	// Rate Vector Model
	//------------------//
	if(Md->vm.vector.rowCnt == 0 ||  Md->vm.vector.colCnt == 0 || Md->vm.vector.a == NULL)
	{
		_ASSERTE(Md->vm.vector.rowCnt == 0 && Md->vm.vector.colCnt == 0 && Md->vm.vector.a == NULL);
		return VECTOR_UNDEFINED;
	}

	if(1 != Md->vm.vector.rowCnt && N != Md->vm.vector.rowCnt)
		return VECTOR_LENGTH_MISMATCH;

	//----------------------------------------------------------------------------------//
	// Terminate Rate
	//---------------//
	if(Md->vm.terminate.rowCnt == 0 || Md->vm.terminate.colCnt == 0 || Md->vm.terminate.a == NULL)
	{
		_ASSERTE(Md->vm.terminate.rowCnt == 0 && Md->vm.terminate.colCnt == 0 && Md->vm.terminate.a == NULL);
		return TERMINATE_UNDEFINED;
	}
	
	if(Md->vm.terminate.colCnt != 1 && Md->vm.terminate.colCnt != N)
		return TERMINATE_MISMATCH;
	return VALID;
}
#endif


/*****************************************************************************************
* MEMBER FUNCTION: GetSubModelVectorStatus(DIRECTION_MODEL_thisGoesAway* Md) for the Direction
*                  Vector Models.
* 
* DESCRIPTION: Gets the Vector Model Status (valid, invalid, etc..) for direction vector
*              models
*
* ARGUMENTS:
*	Md: A pointer to a DIRECTION_MODEL_thisGoesAway structure.
*
* RETURN VALUE:
* A SUBMODEL_VECTOR_STATUS
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   02/27/07  (MJC)  Initial Coding/
*
*
* NOTES: (As of 2/27/2007 waiting until I hear from Dorian).
*   ** Change Of Direction **
*   For N behaviors:
*      number of rows:    1 or N
*      number of columns: M (variable, user specified).
*    Columns are N+1 because the first column is populated with leading zeros.
*
*   ** Bias (MxM) **
*      number of rows:    M (same as number of columns in the behavior matrix).
*      number of columns: M (same as number of columns in the behavior matrix)
*
*   ** Terminate **
*   For N behaviors:
*      number of rows:    1
*      number of columns: 1 or N (1 if behaviors do not influence termination)
*****************************************************************************************/
#if 0
SUBMODEL_VECTOR_STATUS CSpeciesModel::GetSubModelVectorStatus(DIRECTION_MODEL_thisGoesAway* Md)
{
	int N = m_behaviorModel_TOBEREMOVED.behavior.rowCnt; // Number of behaviors in the BehaviorTransition
											   // Vector Model.

	// Check the BehaviorTransition Matrix Model
	if(VALID != GetSubModelVectorStatus(&m_behaviorModel_TOBEREMOVED))
		return BEHAVIOR_MDL;


	//----------------------------------------------------------------------------------//
	// Change Of Direction Matrix
	//---------------------------//
	if(Md->vm.direction.rowCnt == 0 ||  Md->vm.direction.colCnt == 0 || Md->vm.direction.a == NULL)
	{
		_ASSERTE(Md->vm.direction.rowCnt && Md->vm.direction.colCnt == 0 && Md->vm.direction.a == NULL);
		return VECTOR_UNDEFINED;
	}

	if(1 != Md->vm.direction.rowCnt && N != Md->vm.direction.rowCnt)
		return VECTOR_LENGTH_MISMATCH;

	//----------------------------------------------------------------------------------//
	// Bias Matrix
	//------------//
	if(Md->vm.directionalBias.colCnt == 0 || Md->vm.directionalBias.rowCnt == 0 || Md->vm.directionalBias.a == NULL)
	{
		_ASSERTE(Md->vm.directionalBias.colCnt == 0 && Md->vm.directionalBias.rowCnt == 0 && Md->vm.directionalBias.a == NULL);
		return VECTOR_UNDEFINED;
	}

	if(Md->vm.directionalBias.colCnt != Md->vm.direction.colCnt || Md->vm.directionalBias.rowCnt != Md->vm.direction.colCnt)
		return VECTOR_LENGTH_MISMATCH;
	//----------------------------------------------------------------------------------//
	// Terminate Array
	//----------------//
	if(Md->vm.terminate.rowCnt == 0 || Md->vm.terminate.colCnt == 0 || Md->vm.terminate.a == NULL)
	{
		_ASSERTE(Md->vm.terminate.rowCnt == 0 && Md->vm.terminate.colCnt == 0 && Md->vm.terminate.a == NULL);
		return TERMINATE_UNDEFINED;
	}
	
	if(Md->vm.terminate.colCnt != 1 && Md->vm.terminate.colCnt != N)
		return TERMINATE_MISMATCH;

	return VALID;
}
#endif

/*****************************************************************************************
* MEMBER FUNCTION: GetSubModelVectorStatus(DEPTH_MODEL_goesAway* Md) for the (dive) Depth Vector
*                  Model.
* 
* DESCRIPTION: Gets the Vector Model Status (valid, invalid, etc..) for depth vector
*              models
*
* ARGUMENTS:
*	Md: A pointer to a DEPTH_MODEL_goesAway structure.
*
* RETURN VALUE:
* A SUBMODEL_VECTOR_STATUS
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   02/27/07  (MJC)  Initial Coding/
*
*
* NOTES:
*   ** Depth **
*   For N behaviors:
*      number of rows:    1 or N
*      number of columns: variable (user specified).
*    Columns are N+1 because the first column is populated with leading zeros.
*
*   ** Step **
*   For N behaviors:
*      number of rows:    1
*      number of columns: 1
*****************************************************************************************/
#if 0
SUBMODEL_VECTOR_STATUS CSpeciesModel::GetSubModelVectorStatus(DEPTH_MODEL_goesAway* Md)
{
	int N = m_behaviorModel_TOBEREMOVED.behavior.rowCnt; // Number of behaviors in the BehaviorTransition
											   // Vector Model.

	//----------------------------------------------------------------------------------//
	// Check the BehaviorTransition Matrix Model
	if(VALID != GetSubModelVectorStatus(&m_behaviorModel_TOBEREMOVED))
		return BEHAVIOR_MDL;

	if(Md->vm.vector.colCnt == 0 && Md->vm.vector.rowCnt == 0 && Md->vm.vector.a == NULL)
	{
		_ASSERTE(Md->vm.vector.colCnt == 0 && Md->vm.vector.rowCnt == 0 && Md->vm.vector.a == NULL);
		return VECTOR_UNDEFINED;
	}

	if(1 != Md->vm.vector.rowCnt && N != Md->vm.vector.rowCnt)
		return VECTOR_LENGTH_MISMATCH;
	return VALID;
}
#endif

/*****************************************************************************************
* MEMBER FUNCTION: GetSubModelVectorStatus(SURFACE_INTERVAL_MODEL_goesAway* Md) for the
*				   Surface Interval Vector Model.
* 
* DESCRIPTION: Gets the Vector Model Status (valid, invalid, etc..) for depth vector
*              models
*
* ARGUMENTS:
*	Md: A pointer to a SURFACE_INTERVAL_MODEL_goesAway structure.
*
* RETURN VALUE:
* A SUBMODEL_VECTOR_STATUS
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   02/27/07  (MJC)  Initial Coding/
*
*
* NOTES:
*   ** Surface Interval **
*   For N behaviors:
*      number of rows:    1 or N
*      number of columns: variable (user specified).
*    Columns are N+1 because the first column is populated with leading zeros.
*
*   ** Step **
*   For N behaviors:
*      number of rows:    1
*      number of columns: 1
*****************************************************************************************/
#if 0
SUBMODEL_VECTOR_STATUS CSpeciesModel::GetSubModelVectorStatus(SURFACE_INTERVAL_MODEL_goesAway* Md)
{
	int N = m_behaviorModel_TOBEREMOVED.behavior.rowCnt; // Number of behaviors in the BehaviorTransition
											   // Vector Model.

	//----------------------------------------------------------------------------------//
	// Check the BehaviorTransition Matrix Model
	if(VALID != GetSubModelVectorStatus(&m_behaviorModel_TOBEREMOVED))
		return BEHAVIOR_MDL;

	if(Md->vm.vector.colCnt == 0 && Md->vm.vector.rowCnt == 0 && Md->vm.vector.a == NULL)
	{
		_ASSERTE(Md->vm.vector.colCnt == 0 && Md->vm.vector.rowCnt == 0 && Md->vm.vector.a == NULL);
		return VECTOR_UNDEFINED;
	}

	if(1 != Md->vm.vector.rowCnt && N != Md->vm.vector.rowCnt)
		return VECTOR_LENGTH_MISMATCH;
	return VALID;
}
#endif

/*****************************************************************************************
* MEMBER FUNCTION: GetSubModelVectorStatus(REVERSAL_MODEL_goesAway* Md) for the Reversal Vector
*				   Model.
* 
* DESCRIPTION: Gets the Vector Model Status (valid, invalid, etc..) for the reversal
*			   vector model
*
* ARGUMENTS:
*	Md: A pointer to a REVERSAL_MODEL_goesAway structure.
*
* RETURN VALUE:
* A SUBMODEL_VECTOR_STATUS
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   02/27/07  (MJC)  Initial Coding/
*
*
*   ** Reversal Vector **
*   For N behaviors:
*      number of rows:    1 or N
*      number of columns: variable (user specified).
*    Columns are N+1 because the first column is populated with leading zeros.
*
*   ** Revesal Probability **
*   For N behaviors:
*      number of rows:    1
*      number of columns: 1 or N
*
*   ** Time Reversed **
*   For N behaviors:
*      number of rows:    1 or N
*      number of columns: variable (user specified).
*
*   ** Time Reversed Step **
*   For N behaviors:
*      number of rows:    1
*      number of columns: 1
*****************************************************************************************/
#if 0
SUBMODEL_VECTOR_STATUS CSpeciesModel::GetSubModelVectorStatus(REVERSAL_MODEL_goesAway* Md)
{
	int N = m_behaviorModel_TOBEREMOVED.behavior.rowCnt; // Number of behaviors in the BehaviorTransition
											   // Vector Model.

	//----------------------------------------------------------------------------------//
	// Check the BehaviorTransition Matrix Model
	//--------------------------------//
	if(VALID != GetSubModelVectorStatus(&m_behaviorModel_TOBEREMOVED))
		return BEHAVIOR_MDL;

	//----------------------------------------------------------------------------------//
	// Reversal Count Vector Model
	//----------------------------//
	if(Md->vm.count.colCnt == 0 || Md->vm.count.rowCnt == 0 || Md->vm.count.a == NULL)
	{
		_ASSERTE(Md->vm.count.colCnt == 0 && Md->vm.count.rowCnt == 0 && Md->vm.count.a == NULL);
		return VECTOR_UNDEFINED;
	}

	if(1 != Md->vm.count.rowCnt && N != Md->vm.count.rowCnt)
		return VECTOR_LENGTH_MISMATCH;

	//----------------------------------------------------------------------------------//
	// Probability of Reversal Vector Model
	//-------------------------------------//
	if(Md->vm.probOfReversal.colCnt == 0 || Md->vm.probOfReversal.rowCnt == 0 || Md->vm.probOfReversal.a == NULL)
	{
		_ASSERTE(Md->vm.probOfReversal.colCnt == 0 && Md->vm.probOfReversal.rowCnt == 0 && Md->vm.probOfReversal.a == NULL);
		return VECTOR_UNDEFINED;
	}

	if(1 != Md->vm.probOfReversal.colCnt && N != Md->vm.probOfReversal.colCnt)
		return VECTOR_LENGTH_MISMATCH;

	//----------------------------------------------------------------------------------//
	// Reversal Time Vector Model
	//---------------------------//
	if(Md->vm.time.colCnt == 0 || Md->vm.time.rowCnt == 0 || Md->vm.time.a == NULL)
	{
		_ASSERTE(Md->vm.time.colCnt == 0 && Md->vm.time.rowCnt == 0 && Md->vm.time.a == NULL);
		return VECTOR_UNDEFINED;
	}

	if(1 != Md->vm.time.rowCnt && N != Md->vm.time.rowCnt)
		return VECTOR_LENGTH_MISMATCH;
	return VALID;
}
#endif


/*----------------------------------------------------------------------------------------
  MEMBER FUNCTION: XXX
 
  DESCRIPTION:
    XXX

  INPUT PARAMETER(S):
	XXX

  RETURN VALUE:
	XXX

  REVISION HISTORY:
     Date    Name   Change
   --------  ----   ------
   05/24/05  (MJC)  Initial Coding/Transfer
----------------------------------------------------------------------------------------*/

