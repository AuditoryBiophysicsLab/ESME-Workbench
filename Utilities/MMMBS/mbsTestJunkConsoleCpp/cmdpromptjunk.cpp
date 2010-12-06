//#include "DataLogger.h"

#include <stdio.h>
#include <process.h>
#include "3mbsLib.h"
#include "dataTypes_statsAnalysis.h"
#include "ListManager.h"
#include "SpeDataTypesV5.h"
#include "SpeDataTypesV6.h"
#include "OutputReader.h"

#define NUM 20

#define MINX_TEST -22
#define MAXX_TEST 3.2
#define RESX_TEST .4

#define MINY_TEST 0
#define MAXY_TEST 3.2
#define RESY_TEST .2

#define MINZ_TEST 0
#define MAXZ_TEST 3.2
#define RESZ_TEST .2

#define MINX_LATLO 31.5
#define MAXX_LATLO 35
#define RESX_LATLO .2

#define MINY_LATLO -122
#define MAXY_LATLO -166
#define RESY_LATLO .5

#define MINZ_LATLO -5
#define MAXZ_LATLO 0
#define RESZ_LATLO .5


#ifdef USING_LATLO

#define MINX MINX_LATLO
#define MAXX MAXX_LATLO
#define RESX RESX_LATLO

#define MINY MINY_LATLO
#define MAXY MAXY_LATLO
#define RESY RESY_LATLO

#define MINZ MINZ_LATLO
#define MAXZ MAXZ_LATLO
#define RESZ RESZ_LATLO

#else

#define MINX MINX_TEST
#define MAXX MAXX_TEST
#define RESX RESX_TEST

#define MINY MINY_TEST
#define MAXY MAXY_TEST
#define RESY RESY_TEST

#define MINZ MINZ_TEST
#define MAXZ MAXZ_TEST
#define RESZ RESZ_TEST

#endif

MEMORYSTATUS ms1={0}, ms2={0}, ms3={0};
MEMORYSTATUS ls1={0}, ls2={0};

int Cow()
{
	SIZE_T t1;
	char intarray[1024*400];
	intarray[0] = 0;
	GlobalMemoryStatus(&ls1);
	t1 = ls1.dwAvailPhys/(1024);
	return (int)0;
}

int Zoo()
{
	SIZE_T t1;
	char intarray[1024*800];
	intarray[0] = 0;
	GlobalMemoryStatus(&ls2);
	t1 = ls2.dwAvailPhys/(1024);
	return (int)0;
}

size_t SizeOf_GAUSS()
{
	size_t s = 0;
	GAUSS u;
	memset(&u, 0, sizeof(u));

	s += sizeof(u.mean);
	s += sizeof(u.std);
	s += sizeof(u.coeff);
	s += sizeof(u._RESERVED);
	return s;
}


size_t SizeOf_RANDOM()
{
	size_t s = 0;
	RANDOM u;
	memset(&u, 0, sizeof(u));

	s += sizeof(u.max);
	s += sizeof(u.min);
	s += sizeof(u.coeff);
	s += sizeof(u._RESERVED);
	return s;
}


size_t SizeOf_ENVATTRACTORMDL()
{
	size_t s = 0;
	ENVATTRACTORMDL u;
	memset(&u, 0, sizeof(u));

	s += sizeof(u.max);
	s += sizeof(u.min);
	s += sizeof(u.delta);

	s += sizeof(u.maxIsEnabled);
	s += sizeof(u.minIsEnabled);
	s += sizeof(u.deltaIsEnabled);

	s += sizeof(u._reserved);
	return s;
}

size_t SizeOf_DEPTH_ENV_ATTRACTOR_MDL()
{
	size_t s = 0;
	DEPTH_ENV_ATTRACTOR_MDL u;
	memset(&u, 0, sizeof(u));
	s += sizeof(u.shelfDepth);
	s += sizeof(u.basinDepth);
	s += sizeof(u.slopeDepth);

	s += sizeof(u.shelfIsEnabled);
	s += sizeof(u.basinIsEnabled);
	s += sizeof(u.slopeIsEnabled);

	s += sizeof(u.shelfSlope);
	s += sizeof(u.basinSlope);
	s += sizeof(u.slopeSlope);

	s += sizeof(u._reserved);

	return s;
}


size_t SizeOf_MATRIX()
{
	size_t s = 0;
	MATRIX u;
	memset(&u, 0, sizeof(u));

	s += sizeof(u.rowCnt);
	s += sizeof(u.colCnt);
	s += sizeof(u.p);
	return s;
}

size_t SizeOf_ARRAY()
{
	size_t s = 0;
	ARRAY u;
	memset(&u, 0, sizeof(u));

	s += sizeof(u.rowCnt);
	s += sizeof(u.colCnt);
	s += sizeof(u.p);
	return s;
}

size_t SizeOf_ELEMENT()
{
	size_t s = 0;
	ELEMENT u;
	memset(&u, 0, sizeof(u));

	s += sizeof(u.rowCnt);
	s += sizeof(u.colCnt);
	s += sizeof(u.a);
	return s;
}

size_t SizeOf_RATEVCTRMDLPARAM()
{
	size_t s = 0;
	RATEVCTRMDLPARAM u;
	memset(&u, 0, sizeof(u));
	s += SizeOf_ARRAY(); //vetor
	s += SizeOf_ELEMENT(); // step
	s += SizeOf_ELEMENT(); // terminate
	s += sizeof(u._RESERVED);
	return s;
}


size_t SizeOf_RATEMDL()
{
	size_t s = 0;
	RATEMDL u;
	memset(&u, 0, sizeof(u));

	s += SizeOf_RANDOM();
	s += SizeOf_GAUSS();
	s += SizeOf_RATEVCTRMDLPARAM();
	s += sizeof(u.modelType);
	s += sizeof(u._RESERVED);
	return s;
}

size_t SizeOf_SPECIESSPECIFICATION()
{
	SPECIESSPECIFICATION u;
	size_t s1, s2, s3, s4, s5, s6, s7, s8, s9;
	memset(&u, 0, sizeof(u));

	s1 = sizeof(u.szHeader);
	s2 = sizeof(u.mbsVerSuper) + sizeof(u.mbsVerSub) + sizeof(u.speVerSuper) + sizeof(u.speVerSub);
	s3 = sizeof(u.year) + sizeof(u.month) + sizeof(u.day) + sizeof(u.hour);
	s4 = sizeof(u.min) + sizeof(u.sec) + sizeof(u.id) + sizeof(u.numBehaviors);
	s5 = sizeof(u.group) + sizeof(u.name) + sizeof(u.deepWaterSeedingDepthEnabled) + sizeof(u._reserved1);
	s6 = sizeof(u.shoreFollowDepth) + sizeof(u.minSeedingDepth);
	s7 = sizeof(u.deepWaterSeedingDepth) + sizeof(u._reserved2);
	s8 = sizeof(u.speciesShrtDscrptn);
	s9 = sizeof(u.speciesComment);
	return s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8 + s9;
}

size_t SizeOf_SPECIES_MDL()
{
	SPECIES_MDL u;
	size_t s = 0;
	memset(&u, 0, sizeof(u)); // quiet compiler warning

	s += SizeOf_SPECIESSPECIFICATION();
	s += sizeof(u.p) + sizeof(u._reserved1);
	s += sizeof(u.acousticAversion);
	s += sizeof(u.initialBehavior) + sizeof(u._reserved2);
	return s;
}

size_t SizeOf_BEHTRANSMDL()
{
	size_t s = 0;
	BEHTRANSMDL u;
	memset(&u, 0, sizeof(u)); // quiet compiler warning

	s += sizeof(u.meanTimeInBeh);
	s += sizeof(u.slopeCoefficient);
	s += sizeof(u._reserved);
	s += SizeOf_ARRAY(); // behavior
	s += SizeOf_ELEMENT(); // terminate
	return s;
}

size_t SizeOf_RANDOMWALK()
{
	RANDOMWALK u;
	memset(&u, 0, sizeof(u)); // quiet compiler warning
	size_t s = 0;
	s += sizeof(u.termCoeff);
	s += sizeof(u._RESERVED);
	return s;
}
size_t SizeOf_CORRANDWALK()
{
	CORRANDWALK u;
	memset(&u, 0, sizeof(u)); // quiet compiler warning
	size_t s = 0;
	s += sizeof(u.perturbation);
	s += sizeof(u.termCoeff);
	s += sizeof(u._RESERVED);
	return s;
}
size_t SizeOf_CORRANDWALKDB()
{
	CORRANDWALKDB u;
	size_t s = 0;
	memset(&u, 0, sizeof(u)); // quiet compiler warning
	s += sizeof(u.perturbation);
	s += sizeof(u.directionOfBias);
	s += sizeof(u.bias);
	s += sizeof(u.arcStep);
	s += sizeof(u.termCoeff);
	s += sizeof(u._RESERVED);
	return s;
}

size_t SizeOf_DIRVCTRMDLPARAM()
{
	//DIRVCTRMDLPARAM u;
	size_t s = 0;
	s += SizeOf_ARRAY();
	s += SizeOf_MATRIX();
	s += SizeOf_ELEMENT();
	return s;
}


size_t SizeOf_DIRCTNMDL()
{
	DIRCTNMDL u;
	size_t s = 0;
	memset(&u, 0, sizeof(u)); // quiet compiler warning
	s += SizeOf_RANDOMWALK();
	s += SizeOf_CORRANDWALK();
	s += SizeOf_CORRANDWALKDB();
	s += SizeOf_DIRVCTRMDLPARAM();
	s += sizeof(u.modelType);
	s += sizeof(u._RESERVED1);
	return s;
}


size_t SizeOf_VCTRMDLPARAM()
{
	VCTRMDLPARAM u;
	size_t s = 0;
	memset(&u, 0, sizeof(u)); // quiet compiler warning
	s += SizeOf_ARRAY(); // vector
	s += SizeOf_ELEMENT(); // step
	s += sizeof(u._RESERVED);
	return s;
}


size_t SizeOf_SURFINTRVLPARAM()
{
	SURFINTRVLPARAM u;
	size_t s = 0;
	memset(&u, 0, sizeof(u)); // quiet compiler warning
	s += SizeOf_GAUSS();
	s += SizeOf_VCTRMDLPARAM();
	s += sizeof(u.modelType);
	s += sizeof(u._RESERVED1);
	return s;
}

size_t SizeOf_REV_INT_RND()
{
	REV_INT_RND u;
	size_t s = 0;
	memset(&u, 0, sizeof(u)); // quiet compiler warning
	s += sizeof(u.max);
	s += sizeof(u.min);
	return s;
}

size_t SizeOf_REVERSAL_RND()
{
	REVERSAL_RND u;
	size_t s = 0;
	memset(&u, 0, sizeof(u)); // quiet compiler warning
	s += sizeof(u.probOfReversal);
	s += SizeOf_REV_INT_RND();
	s += SizeOf_GAUSS();
	return s;
}
size_t SizeOf_REVERSAL_GAUSS()
{
	REVERSAL_GAUSS u;
	size_t s = 0;
	memset(&u, 0, sizeof(u)); // quiet compiler warning
	s += sizeof(u.probOfReversal);
	s += SizeOf_GAUSS();
	s += SizeOf_GAUSS();
	s += sizeof(u._RESERVED);
	return s;
}
size_t SizeOf_REVVCTRMDLPARAM()
{
	//REVVCTRMDLPARAM u;
	size_t s = 0;
	s += SizeOf_ELEMENT();
	s += SizeOf_ARRAY();
	s += SizeOf_ARRAY();
	s += SizeOf_ELEMENT();
	return s;
}
size_t SizeOf_REVERSAL_DEF()
{
	REVERSAL_DEF u;
	size_t s = 0;
	memset(&u, 0, sizeof(u)); // quiet compiler warning
	s += SizeOf_REVERSAL_RND();
	s += SizeOf_REVERSAL_GAUSS();
	s += SizeOf_REVVCTRMDLPARAM();
	s += sizeof(u.diveRateType);
	s += SizeOf_GAUSS(); // dive rate
	s += sizeof(u.reverses); // reverses
	s += sizeof(u.modelType); // model type
	s += sizeof(u._RESERVED2);
	s += SizeOf_GAUSS(); // Ascent rate
	return s;
}

size_t SizeOf_DEPTHPARAM()
{
	DEPTHPARAM u;
	size_t s = 0;
	memset(&u, 0, sizeof(u)); // quiet compiler warning
	s += SizeOf_RANDOM();
	s += SizeOf_GAUSS();
	s += SizeOf_VCTRMDLPARAM();
	s += sizeof(u.modelType);
	s += sizeof(u._RESERVED);
	return s;
}

size_t SizeOf_BTTMFLLW()
{
	BTTMFLLW u;
	memset(&u, 0, sizeof(u)); // quiet compiler warning
	size_t s = 0;
	s += sizeof(u.type);
	s += sizeof(u._RESERVED1);
	s += SizeOf_RATEMDL();
	s += sizeof(u._RESERVED2);
	return s;
}

size_t SizeOf_DIVEMDL()
{
	DIVEMDL u;
	size_t s = 0;
	memset(&u, 0, sizeof(u)); // quiet compiler warning
	s += SizeOf_RATEMDL(); // descentRate
	s += SizeOf_RATEMDL(); // ascentRate
	s += SizeOf_SURFINTRVLPARAM(); // srfInv
	s += SizeOf_REVERSAL_DEF();
	s += SizeOf_DEPTHPARAM();
	s += SizeOf_BTTMFLLW();
	s += sizeof(u._RESERVED);
	return s;
}


size_t SizeOf_NRMLBEHMDL()
{
	NRMLBEHMDL u;
	size_t s = 0;
	memset(&u, 0, sizeof(u)); // quiet compiler warning
	s += sizeof(u.szName);
	s += sizeof(u.nrmlBehTransCnt);
	s += sizeof(u.nrmlBehTermType);
	s += sizeof(u.depthHasPriorityOverTemp);
	s += sizeof(u._RES1);
	s += sizeof(u.nrmlBehTrans);
	s += sizeof(u._RES2);
	s += SizeOf_BEHTRANSMDL(); // depthEnvAttBehTrans
	s += SizeOf_BEHTRANSMDL(); // tempEnvAttBehTrans
	s += SizeOf_DEPTH_ENV_ATTRACTOR_MDL(); // depthEnvAtt
	s += SizeOf_ENVATTRACTORMDL(); // tempEnvAtt
	s += SizeOf_RATEMDL(); // travel rate
	s += SizeOf_DIRCTNMDL(); //travelDirection
	s += SizeOf_DIVEMDL();
	s += sizeof(u._RES3);
	return s;
}

size_t SizeOf_ACOUSTICSRCEINF()
{
	ACOUSTICSRCEINF u;
	memset(&u, 0, sizeof(u)); // quiet compiler warning
	size_t s = 0;
	s += sizeof(u.beginIteration) + sizeof(u.dutyPeriod) + sizeof(u.outputValue) + sizeof(u.isASoundSource) +
		sizeof(u._RESERVED);
	return s;
}

size_t SizeOf_ACSTCAVRSNMDL()
{
	ACSTCAVRSNMDL u;
	size_t s = 0;
	memset(&u, 0, sizeof(u)); // quiet compiler warning
	s += sizeof(u.travelDirectionAffected) + sizeof(u.travelRateAffected) + sizeof(u.depthAffected) + sizeof(u.surfaceIntervalAffected);
	s += sizeof(u.ascentRateAffected) + sizeof(u.descentRateAffected) + sizeof(u.reversalAffected) + sizeof(u.flatBottomDiveAffected);
	s += sizeof(u.beachingDepth) + sizeof(u.podBreaksUp) + sizeof(u.beaches);
	s += sizeof(u.flatBottomDives) + sizeof(u._RESERVED);
	s += SizeOf_GAUSS(); // travel rate
	s += SizeOf_CORRANDWALKDB(); // travel
	s += SizeOf_REVERSAL_GAUSS(); // trversal.
	s += SizeOf_GAUSS(); // surface interval
	s += SizeOf_GAUSS(); // depth
	s += SizeOf_GAUSS(); // descent rate
	s += SizeOf_GAUSS(); // ascent rate
	return s;
}

size_t SizeOf_ANIMATASSCN()
{
	ANIMATASSCN u;
	size_t s = 0;
	memset(&u, 0, sizeof(u)); // quiet compiler warning
	s += sizeof(u.speciesNumber) + sizeof(u.pod_id) + sizeof(u.id) + sizeof(u.compactInf) + sizeof(u._RESERVED1);
	s += SizeOf_ACOUSTICSRCEINF();
	s += sizeof(u._RESERVED2);
	return s;
}

size_t SizeOf_DEPTHSPAN()
{
	DEPTHSPAN u;
	memset(&u, 0, sizeof(u)); // quiet compiler warning
	size_t s = 0;
	s += sizeof(u.shallow) + sizeof(u.deep) + sizeof(u._RES);
	return s;
}

size_t SizeOf_BEHTRAN()
{
	BEHTRAN u;
	memset(&u, 0, sizeof(u)); // quiet compiler warning
	size_t s = 0;
	s += sizeof(u._RES1);
	s += SizeOf_DEPTHSPAN();
	s += SizeOf_MATRIX();
	return s;
}

size_t SizeOf_BINOUT_FILEPOINTER()
{
	BINOUT_FILEPOINTER u;
	memset(&u, 0, sizeof(u)); // quiet compiler warning
	size_t s = 0;
	s += sizeof(u.scenarioParams) + sizeof(u.bathyMap) + sizeof(u.salinityMap) + sizeof(u.temperatureMap) + sizeof(u.postAnalysis) +
		sizeof(u.speciesDesc) + sizeof(u.animatAssoc) + sizeof(u.animatState) + sizeof(u.aeState) + sizeof(u._unused); 
	return s;
}
size_t SizeOf_BINOUT_SIZE()
{
	BINOUT_SIZE u;
	memset(&u, 0, sizeof(u)); // quiet compiler warning
	size_t s = 0;
	s += sizeof(u.scenarioParams) + sizeof(u.bathyMap) + sizeof(u.salinityMap) + sizeof(u.temperatureMap) + sizeof(u.postAnalysis) +
		sizeof(u.speciesDesc) + sizeof(u.animatAssoc) + sizeof(u.animatState) + sizeof(u.aeState) + sizeof(u._unused); 
	return s;
}
size_t SizeOf_BINARYSETUP()
{
	BINARYSETUP u;
	size_t s = 0;
	memset(&u, 0, sizeof(u)); // quiet compiler warning
	s += SizeOf_BINOUT_FILEPOINTER();
	s += SizeOf_BINOUT_SIZE();
	s += sizeof(u.totalDiskSpace);
	s += sizeof(u.totalAnimatStateBytes);
	s += sizeof(u.totalAEStateBytes);
	s += sizeof(u._unused);
	return s;
}

size_t SizeOf_COORD_DEPTH()
{
	COORD_DEPTH u;
	size_t s = 0;
	memset(&u, 0, sizeof(u)); // quiet compiler warning
	s += sizeof(u.lat) + sizeof(u.lon) + sizeof(u.depth) + sizeof(u._res);
	return s;
}
size_t SizeOf_INHABITINF()
{
	//INHABITINF u;
	size_t s = 0;
	s += SizeOf_COORD_DEPTH();
	s += SizeOf_ACOUSTICSRCEINF();
	return s;
}

size_t SizeOf_SCEPARMSSPECIESGROUP()
{
	SCEPARMSSPECIESGROUP u;
	size_t s = 0;
	memset(&u, 0, sizeof(u)); // quiet compiler warning
	s += sizeof(u.szGroup) + sizeof(u.lvlAphys) + sizeof(u.lvlBphys) + sizeof(u.lvlBBeh_RiskA) +
		sizeof(u.deactThreshB) + sizeof(u.unitsPhys) + sizeof(u.unitsBeh) + sizeof(u.decayfncB);
	return s;
}

size_t SizeOf_STATE_FILEOUTPUT_CONFIG()
{
	//STATE_FILEOUTPUT_CONFIG u;
	size_t s = 0;
	s = 5*sizeof(BOOL);
	return s;
}

size_t SizeOf_ANIMATSTATE_FILEOUTPUT_CONFIG()
{
	//ANIMATSTATE_FILEOUTPUT_CONFIG u;
	size_t s = 0;
	s += 19 * sizeof(BOOL);
	return s;
}


size_t SizeOf_BINARYOUTPUT()
{
	BINARYOUTPUT u;
	size_t s = 0;
	memset(&u, 0, sizeof(u)); // quiet compiler warning
	s += sizeof(u.enabled);
	s += SizeOf_STATE_FILEOUTPUT_CONFIG();
	s += SizeOf_ANIMATSTATE_FILEOUTPUT_CONFIG();
	s += sizeof(u.AECoordinate);
	s += sizeof(u.outputByTime);
	return s;
}

size_t SizeOf_SEEDING()
{
	SEEDING u;
	memset(&u, 0, sizeof(u)); // quiet compiler warning
	size_t s = 0;
	s += sizeof(u.useCurrentTick) + sizeof(u.value) + sizeof(u.independentAnimatRandomGen);
	return s;
}
size_t SizeOf_USERPARAMS()
{
	USERPARAMS u;
	size_t s = 0;
	memset(&u, 0, sizeof(u)); // quiet compiler warning
	s += SizeOf_BINARYOUTPUT();
	s += sizeof(u.durationless) + sizeof(u.acousticAnimatActive) + sizeof(u.distCalcMethod);
	s += SizeOf_SEEDING();
	s += sizeof(u.szOutputDir) + sizeof(u.szScenarioTitle) + sizeof(u.maintainFirstAnimatState);
	return s;
}


size_t SizeOf_SCENARIOPARAMS()
{
	SCENARIOPARAMS u;
	size_t s = 0;
	memset(&u, 0, sizeof(u)); // quiet compiler warning
	s += sizeof(u.libVerSuper) + sizeof(u.libVerSub) + sizeof(u.speciesVerSuper) + sizeof(u.speciesVerSub);
	s += sizeof(u.outputVerSuper) + sizeof(u.outputVerSub) + sizeof(u.numSpecies) + sizeof(u.totalNumAnimats);
	s += sizeof(u.numAcstSrcTypes) + sizeof(u.totalNumAcstcSrcs) + sizeof(u.duration) + sizeof(u.numSaveIterations);
	s += sizeof(u.startTime) + sizeof(u.speciesGroupCnt);
	s += SizeOf_SCEPARMSSPECIESGROUP();
	s += sizeof(u.enableAcstSrcTracking) + sizeof(u.acousticPingCycleOutputLimit);
	s += SizeOf_USERPARAMS();
	return s;
}
size_t SizeOf__fSCEPARMSSPECIESGROUP()
{
	_fSCEPARMSSPECIESGROUP u;
	memset(&u, 0, sizeof(u)); // quiet compiler warning
	size_t s = 0;
	s += sizeof(u.szGroup) + 4*sizeof(float) + 4*sizeof(UINT8) + sizeof(u._reserved);
	return s;
}

size_t SizeOf__fSCENARIOPARAMS()
{
	_fSCENARIOPARAMS u;
	memset(&u, 0, sizeof(u)); // quiet compiler warning
	size_t s = 0;
	s += sizeof(u.fileIdentifier) + 13 * sizeof(DWORD) + 2*sizeof(UINT8) + sizeof(UINT16) + sizeof(u.seedValue) +
		sizeof(u.acstSrceLimitOutput) + 2*sizeof(DWORD) + sizeof(u._reserved1);
	s += SizeOf_BINARYSETUP();
	s += NUM_SPEGROUPS_ALLOC * SizeOf__fSCEPARMSSPECIESGROUP();
	return s;
}

void SizeOfs()
{
	//int s;
	printf("                          As Reported by\n");
	printf("                           the sizeof()        Manually\n");
	printf("      Struct Type           Operator          Calculated\n");
	printf("-----------------------  ----------------   ---------------\n");
	printf("ACOUSTICSRCEINF:         %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(ACOUSTICSRCEINF), sizeof(ACOUSTICSRCEINF)%16, SizeOf_ACOUSTICSRCEINF(), SizeOf_ACOUSTICSRCEINF()%16);
	printf("ACSTCAVRSNMDL:           %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(ACSTCAVRSNMDL), sizeof(ACSTCAVRSNMDL)%16, SizeOf_ACSTCAVRSNMDL(), SizeOf_ACSTCAVRSNMDL()%16);
	printf("ANIMATASSCN:             %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(ANIMATASSCN), sizeof(ANIMATASSCN)%16, SizeOf_ANIMATASSCN(), SizeOf_ANIMATASSCN()%16);
	printf("ARRAY:                   %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(ARRAY), sizeof(ARRAY)%16, SizeOf_ARRAY(), SizeOf_ARRAY()%16);

	printf("\n");
	printf("BEHTRANSMDL:             %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(BEHTRANSMDL), sizeof(BEHTRANSMDL)%16, SizeOf_BEHTRANSMDL(), SizeOf_BEHTRANSMDL()%16);
	printf("BEHTRAN:                 %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(BEHTRAN), sizeof(BEHTRAN)%16, SizeOf_BEHTRAN(), SizeOf_BEHTRAN()%16);
	printf("BINARYSETUP:             %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(BINARYSETUP), sizeof(BINARYSETUP)%16, SizeOf_BINARYSETUP(), SizeOf_BINARYSETUP()%16);
	printf("BINOUT_FILEPOINTER:      %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(BINOUT_FILEPOINTER), sizeof(BINOUT_FILEPOINTER)%16, SizeOf_BINOUT_FILEPOINTER(), SizeOf_BINOUT_FILEPOINTER()%16);
	printf("BINOUT_SIZE:             %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(BINOUT_SIZE), sizeof(BINOUT_SIZE)%16, SizeOf_BINOUT_SIZE(), SizeOf_BINOUT_SIZE()%16);
	printf("BTTMFLLW:                %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(BTTMFLLW), sizeof(BTTMFLLW)%16, SizeOf_BTTMFLLW(), SizeOf_BTTMFLLW()%16);

	printf("\n");
	printf("COORD_DEPTH:             %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(COORD_DEPTH), sizeof(COORD_DEPTH)%16, SizeOf_COORD_DEPTH(), + SizeOf_COORD_DEPTH()%16);
	printf("CORRANDWALK:             %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(CORRANDWALK), sizeof(CORRANDWALK)%16, SizeOf_CORRANDWALK(), SizeOf_CORRANDWALK()%16);
	printf("CORRANDWALKDB:           %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(CORRANDWALKDB), sizeof(CORRANDWALKDB)%16, SizeOf_CORRANDWALKDB(), SizeOf_CORRANDWALKDB()%16);

	printf("\n");
	printf("DEPTH_ENV_ATTRACTOR_MDL: %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(DEPTH_ENV_ATTRACTOR_MDL), sizeof(DEPTH_ENV_ATTRACTOR_MDL)%16, SizeOf_DEPTH_ENV_ATTRACTOR_MDL(), SizeOf_DEPTH_ENV_ATTRACTOR_MDL()%16);
	printf("DEPTHPARAM:              %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(DEPTHPARAM), sizeof(DEPTHPARAM)%16, SizeOf_DEPTHPARAM(), SizeOf_DEPTHPARAM()%16);
	printf("DEPTHSPAN:               %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(DEPTHSPAN), sizeof(DEPTHSPAN)%16, SizeOf_DEPTHSPAN(), SizeOf_DEPTHSPAN()%16);
	printf("DIRCTNMDL:               %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(DIRCTNMDL), sizeof(DIRCTNMDL)%16, SizeOf_DIRCTNMDL(), SizeOf_DIRCTNMDL()%16);
	printf("DIRVCTRMDLPARAM:         %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(DIRVCTRMDLPARAM), sizeof(DIRVCTRMDLPARAM)%16, SizeOf_DIRVCTRMDLPARAM(), SizeOf_DIRVCTRMDLPARAM()%16);
	printf("DIVEMDL:                 %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(DIVEMDL), sizeof(DIVEMDL)%16, SizeOf_DIVEMDL(), SizeOf_DIVEMDL()%16);
	printf("DWORDLONG:               %5d, mod16:%2d\n", sizeof(DWORDLONG), sizeof(DWORDLONG)%16);
 	
	printf("\n");
	printf("EIGHTBYTEPTRS:           %5d, mod16:%2d\n", sizeof(EIGHTBYTEPTRS), sizeof(EIGHTBYTEPTRS)%16);
	printf("ELEMENT:                 %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(ELEMENT), sizeof(ELEMENT)%16, SizeOf_ELEMENT(), SizeOf_ELEMENT()%16);
	printf("ENVATTRACTORMDL:         %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(ENVATTRACTORMDL), sizeof(ENVATTRACTORMDL)%16, SizeOf_ENVATTRACTORMDL(), SizeOf_ENVATTRACTORMDL()%16);

	printf("\n");
	printf("GAUSS:                   %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(GAUSS), sizeof(GAUSS)%16, SizeOf_GAUSS(), SizeOf_GAUSS()%16);

	printf("\n");
	printf("INHABITINF:              %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(INHABITINF), sizeof(INHABITINF)%16, SizeOf_INHABITINF(), SizeOf_INHABITINF()%16);

	printf("\n");
	printf("MATRIX:                  %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(MATRIX), sizeof(MATRIX)%16, SizeOf_MATRIX(), SizeOf_MATRIX()%16);

	printf("\n");
	printf("NRMLBEHMDL8BYTEPTR:      %5d, mod16:%2d (union)\n", sizeof(NRMLBEHMDL8BYTEPTR), sizeof(NRMLBEHMDL8BYTEPTR)%16);
	printf("NRMLBEHMDL:              %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(NRMLBEHMDL), sizeof(NRMLBEHMDL)%16, SizeOf_NRMLBEHMDL(), SizeOf_NRMLBEHMDL()%16);


	printf("\n");

	printf("\n");
	printf("RANDOM:                  %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(RANDOM), sizeof(RANDOM)%16, SizeOf_RANDOM(), SizeOf_RANDOM()%16);
	printf("RANDOMWALK:              %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(RANDOMWALK), sizeof(RANDOMWALK)%16, SizeOf_RANDOMWALK(), SizeOf_RANDOMWALK()%16);
	printf("RATEMDL:                 %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(RATEMDL), sizeof(RATEMDL)%16, SizeOf_RATEMDL(), SizeOf_RATEMDL()%16);
	printf("RATEVCTRMDLPARAM:        %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(RATEVCTRMDLPARAM), sizeof(RATEVCTRMDLPARAM)%16, SizeOf_RATEVCTRMDLPARAM(), SizeOf_RATEVCTRMDLPARAM()%16);
	printf("REVERSAL_DEF:            %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(REVERSAL_DEF), sizeof(REVERSAL_DEF)%16, SizeOf_REVERSAL_DEF(), SizeOf_REVERSAL_DEF()%16);
	printf("REVERSAL_GAUSS:          %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(REVERSAL_GAUSS), sizeof(REVERSAL_GAUSS)%16, SizeOf_REVERSAL_GAUSS(), SizeOf_REVERSAL_GAUSS()%16);
	printf("REVERSAL_RND:            %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(REVERSAL_RND), sizeof(REVERSAL_RND)%16, SizeOf_REVERSAL_RND(), SizeOf_REVERSAL_RND()%16);
	printf("REVVCTRMDLPARAM:         %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(REVVCTRMDLPARAM), sizeof(REVVCTRMDLPARAM)%16, SizeOf_REVVCTRMDLPARAM(), SizeOf_REVVCTRMDLPARAM()%16);
 
	printf("\n");
	printf("SCENARIOPARAMS:          %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(SCENARIOPARAMS), sizeof(SCENARIOPARAMS)%16, SizeOf_SCENARIOPARAMS(), SizeOf_SCENARIOPARAMS()%16);
	printf("_fSCENARIOPARAMS:        %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(_fSCENARIOPARAMS), sizeof(_fSCENARIOPARAMS)%16, SizeOf__fSCENARIOPARAMS(), SizeOf__fSCENARIOPARAMS()%16);
	printf("SCEPARMSSPECIESGROUP:    %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(SCEPARMSSPECIESGROUP), sizeof(SCEPARMSSPECIESGROUP)%16, SizeOf_SCEPARMSSPECIESGROUP(), SizeOf_SCEPARMSSPECIESGROUP()%16);
	printf("_fSCEPARMSSPECIESGROUP:  %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(_fSCEPARMSSPECIESGROUP), sizeof(_fSCEPARMSSPECIESGROUP)%16, SizeOf__fSCEPARMSSPECIESGROUP(), SizeOf__fSCEPARMSSPECIESGROUP()%16);
	printf("SPECIES_MDL:             %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(SPECIES_MDL), sizeof(SPECIES_MDL)%16, SizeOf_SPECIES_MDL(), SizeOf_SPECIES_MDL()%16);
	printf("SPECIESBINOUTINF:        %5d, mod16:%2d\n", sizeof(SPECIESBINOUTINF), sizeof(SPECIESBINOUTINF)%16);
	printf("SPECIESSPECIFICATION:    %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(SPECIESSPECIFICATION), sizeof(SPECIESSPECIFICATION)%16, SizeOf_SPECIESSPECIFICATION(), SizeOf_SPECIESSPECIFICATION()%16);
	printf("SURFINTRVLPARAM:         %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(SURFINTRVLPARAM), sizeof(SURFINTRVLPARAM)%16, SizeOf_SURFINTRVLPARAM(), SizeOf_SURFINTRVLPARAM()%16);

	printf("\n");
	printf("TAKE:                    %5d, mod16:%2d\n", sizeof(TAKE), sizeof(TAKE)%16);
	printf("TAKESTATS:               %5d, mod16:%2d\n", sizeof(TAKESTATS), sizeof(TAKESTATS)%16);
	printf("TCHAR:                   %5d, mod16:%2d\n", sizeof(TCHAR), sizeof(TCHAR)%16);


	printf("\n");
	printf("VCTRMDLPARAM:            %5d, mod16:%2d   %5d, mod16:%2d\n", sizeof(VCTRMDLPARAM), sizeof(VCTRMDLPARAM)%16, SizeOf_VCTRMDLPARAM(), SizeOf_VCTRMDLPARAM()%16);



	printf("\nVersion 5\n");
	printf("NRMLBEHMDL8BYTEPTR_VER5: %5d, mod16:%2d\n", sizeof(NRMLBEHMDL8BYTEPTR_VER5), sizeof(NRMLBEHMDL8BYTEPTR_VER5)%16);
	printf("NRMLBEHMDL_VER5:         %5d, mod16:%2d\n", sizeof(NRMLBEHMDL_VER5), sizeof(NRMLBEHMDL_VER5)%16);
	printf("SPECIES_MDL_V5:          %5d, mod16:%2d\n", sizeof(SPECIES_MDL_V5), sizeof(SPECIES_MDL_V5)%16);

	printf("\nVersion 6\n");
	printf("NRMLBEHMDL8BYTEPTR_VER6: %5d, mod16:%2d\n", sizeof(NRMLBEHMDL8BYTEPTR_VER6), sizeof(NRMLBEHMDL8BYTEPTR_VER6)%16);
	printf("NRMLBEHMDL_VER6:         %5d, mod16:%2d\n", sizeof(NRMLBEHMDL_VER6), sizeof(NRMLBEHMDL_VER6)%16);
	printf("SPECIES_MDL_V6:          %5d, mod16:%2d\n", sizeof(SPECIES_MDL_V6), sizeof(SPECIES_MDL_V6)%16);
}


void main(int argc, char *argv[])
{
/*
	BOOL bRes;
	RESLT res;
	_fSCENARIOPARAMS params;
	BINARYOUTPUT bin;
	COutputReader reader;

	SetCurrentDirectory("C:\\3mb\\workingFiles");

	bRes = reader.IsOpen();
	res = reader.OpenOutputFile("run.3mb");
	params = reader.GetSceParams();
	bin = CFileManager::TranslateBinFileOutConfiguration(params.binOutStateItemConfig);
	reader.CloseOutputFile();
	return;
*/

	SizeOfs();
#if 0
	DWORD i, j;
	DWORD supIn, supOut, ver1, ver2;
	DWORD subIn, subOut;

	for(i=0; i<65536; i++)
	{
		supIn = i;
		for(j=0; j<65536; j++)
		{
			subIn = j;
			ver1 = CombineSuperAndSubVer(supIn, subIn);
			supOut = GetSuperVersion(ver1);
			subOut = GetSubVersion(ver1);

			if(supIn != supOut || subIn != subOut)
			{
				printf("failure: %d != %d or %d != %d\n", supIn, supOut, subIn, subOut);
				break;
			}
		}

		if(supIn != supOut || subIn != subOut)
			break;

	}



	ver2 = 9;
#endif

#if 0
	//SizeOfs();
	CEnvironmentData env;
	char *szIn, *szOut;
	szIn = argv[1];
	szOut = argv[2];
	double scale = atof(argv[3]);

	printf("%s, %s, %s, %f\n", argv[0], argv[1], argv[2], scale);

	if(FALSE == env.LoadScaleAndOutputTextFile(szIn, szOut, scale))
		printf("Failed\n");
	printf("Success\n");
#endif

#if 0
	COORDINATE curCoord, newCoord;
	ENVMINMAX envMinMax;

	envMinMax.xMin = -10;
	envMinMax.xMax = 20;

	envMinMax.yMin = -10;
	envMinMax.yMax = 20;


	// Test 1, should return (4,4)
	curCoord.lon = 4;
	curCoord.lat = 4;
	newCoord = ContinueOtherSideOfScreenLatLon(curCoord, envMinMax);


	// Test 2 should return (-10, -10)
	curCoord.lon = 20; // on the right-most border
	curCoord.lat = 20; // on the top-most boarder
	newCoord = ContinueOtherSideOfScreenLatLon(curCoord, envMinMax);

	// Test 3 should return (-9, -9)
	curCoord.lon = 21; // past the right-most boarder by 1
	curCoord.lat = 21; // past the top-most boarder by 1
	newCoord = ContinueOtherSideOfScreenLatLon(curCoord, envMinMax);


	// Test 4 should return (19, 19)
	curCoord.lon = -11; // before the left-most boarder by 1
	curCoord.lat = -11; // before the top-most border by 1.
	newCoord = ContinueOtherSideOfScreenLatLon(curCoord, envMinMax);
#endif



#if 0
	CScenario sce;
	COORD_DEPTH c;
	c.lat = 0;
	c.lon = 0;

	RESLT r;

	r = sce.AddSpecies("aaaTestBlueWhale.spe");
	r = sce.AddIndividual(0, c);
	sce.SetDuration(3600);
	r = sce.RunScenario(-1);

	while(sce.IsActive() == TRUE)
	{
		Sleep(1);
		//printf(".\n");
	}

	printf(".\n");
	//SizeOfs();
#endif
}

#if 0
	SYSTEM_INFO si;

	char cbuff[1];
	char *pCbuff;
 
    GetSystemInfo(&si);
    printf("The page size for this system is %u bytes\n", si.dwPageSize);
	GlobalMemoryStatus(&ms1);
	//pCbuff = new char[1024*1024];
	//GlobalMemoryStatus(&ms2);
	//delete [] pCbuff;
	//GlobalMemoryStatus(&ms3);
	Cow();
	Zoo();
	GlobalMemoryStatus(&ms2);
	int i = 0;

	printf("availPhysMem: %d  availVirtualMem: %d\n", ms2.dwAvailPhys/(1024*1024), ms2.dwAvailVirtual/(1024*1024));


	Sleep(45000);
	return;

#endif
#if 0
	int i = 3;

	LinkedList <int> LL;
	CListManager <int> CMLL;

	CMLL.Length();
	CMLL.Insert(0,&i);
#endif
#if 0
	int x;
	printf("Sizeof SCEPARMSSPECIESGROUP: %d, mod16: %d\n", sizeof(SCEPARMSSPECIESGROUP), sizeof(SCEPARMSSPECIESGROUP)%16);
	printf("Sizeof _fSCENARIOPARAMS: %d, mod16: %d\n", sizeof(_fSCENARIOPARAMS), sizeof(_fSCENARIOPARAMS)%16);
	printf("Sizeof TAKE: %d, mod16: %d\n", sizeof(TAKE), sizeof(TAKE)%16);
	printf("Sizeof TAKESTATS: %d, mod16: %d\n", sizeof(TAKESTATS), sizeof(TAKESTATS)%16);
	printf("Sizeof SPECIESBINOUTINF: %d, mod16: %d\n", sizeof(SPECIESBINOUTINF), sizeof(SPECIESBINOUTINF)%16);
	printf("Sizeof SPECIESSPECIFICATION: %d, mod16: %d\n", sizeof(SPECIESSPECIFICATION), sizeof(SPECIESSPECIFICATION)%16);
	printf("Sizeof ANIMATASSCN: %d, mod16: %d\n", sizeof(ANIMATASSCN), sizeof(ANIMATASSCN)%16);
	printf("Sizeof ANIMATSTATE_FILEOUT: %d, mod16: %d\n", sizeof(ANIMATSTATE_FILEOUT), sizeof(ANIMATSTATE_FILEOUT)%16);

	x = 5;
#endif
#if 0
	CSpecies spe;
	CSpeciesModel speMdl;
	speMdl.ModelToText("Dogabcdef.txt");
	speMdl.SaveToBinFile("cow.spe");

	speMdl.LoadFromBinFile("cow.spe");
#endif
#if 0
	int res[360];
	int i, k;

	memset(res, 0, 360*sizeof(int));
	warmup_random(0);

	for(i=0; i<100000; i++)
	{

		res[rnd(0,359)]++;
	}

	k=0;
	while(k<360)
	{
		for(i=0; i<4,k<360; i++, k++)
			printf("%03d: %d\t", k, res[k]);
		printf("\n");
	}

	return;
	k=0;
	while(k<360)
	{
		if(res[k] < 200)
			printf("%d", k);
		k++;
	}
#endif 
#if 0
	CScenario sce;

	sce.ExtractBinaryResultsIntoTextFiles("dog.txt");
#endif
#if 0

	DWORD size = 0;

	printf("float                    = %4d\n", sizeof(float));
	printf("double                   = %4d\n", sizeof(double));
	printf("double*                  = %4d\n", sizeof(double *));
	printf("long                     = %4d\n", sizeof(long));
	printf("DWORD                    = %4d\n", sizeof(DWORD));
	printf("----------------------------------------------\n");

	printf("MATRIX                   = %4d (/16 = %3.1f)\n", sizeof(MATRIX), sizeof(MATRIX)/16.0f);
	printf("ARRAY                    = %4d (/16 = %3.1f)\n", sizeof(ARRAY), sizeof(ARRAY)/16.0f);
	printf("ELEMENT                  = %4d (/16 = %3.1f)\n", sizeof(ELEMENT), sizeof(ELEMENT)/16.0f);
	printf("----------------------------------------------\n");


	printf("COORD_DEPTH               = %4d\n", sizeof(COORD_DEPTH));
	printf("ANIMATSTATE_FILEOUT             = %4d\n", sizeof(ANIMATSTATE_FILEOUT));
	printf("----------------------------------------------\n");


	printf("SPECIESMODELFILEHEADER          = %4d (/16 = %3.1f)\n", sizeof(SPECIESMODELFILEHEADER), sizeof(SPECIESMODELFILEHEADER)/16.0f);
	printf("BEHAVIOR_MODEL_GETSREPLACEd           = %4d (/16 = %3.1f)\n", sizeof(BEHAVIOR_MODEL_GETSREPLACEd), sizeof(BEHAVIOR_MODEL_GETSREPLACEd)/16.0f);
	printf("RATE_MODEL               = %4d (/16 = %3.1f)\n", sizeof(RATE_MODEL), sizeof(RATE_MODEL)/16.0f);
	printf("DIRECTION_MODEL_thisGoesAway          = %4d (/16 = %3.1f)\n", sizeof(DIRECTION_MODEL_thisGoesAway), sizeof(DIRECTION_MODEL_thisGoesAway)/16.0f);
	printf("DEPTH_MODEL_goesAway              = %4d (/16 = %3.1f)\n", sizeof(DEPTH_MODEL_goesAway), sizeof(DEPTH_MODEL_goesAway)/16.0f);
	printf("SURFACE_INTERVAL_MODEL_goesAway   = %4d (/16 = %3.1f)\n", sizeof(SURFACE_INTERVAL_MODEL_goesAway), sizeof(SURFACE_INTERVAL_MODEL_goesAway)/16.0f);
	printf("REVERSAL_MODEL_goesAway           = %4d (/16 = %3.1f)\n", sizeof(REVERSAL_MODEL_goesAway), sizeof(REVERSAL_MODEL_goesAway)/16.0f);
	printf("ACOUSTIC_EXPR_MODEL_goesAway      = %4d (/16 = %3.1f)\n", sizeof(ACOUSTIC_EXPR_MODEL_goesAway), sizeof(ACOUSTIC_EXPR_MODEL_goesAway)/16.0f);


	size = sizeof(BEHAVIOR_MODEL_GETSREPLACEd)         + sizeof(RATE_MODEL)     +
		   sizeof(DIRECTION_MODEL_thisGoesAway)        + sizeof(DEPTH_MODEL_goesAway)    +
		   sizeof(RATE_MODEL)             + sizeof(RATE_MODEL)     +
		   sizeof(SURFACE_INTERVAL_MODEL_goesAway) + sizeof(REVERSAL_MODEL_goesAway) +
		   sizeof(ACOUSTIC_EXPR_MODEL_goesAway) + 16 + 16 + 128; // 16 header, 8 version, 8 reserved, 16 footer

	printf("Total Species model size (minus matries) = %4d (/16 = %3.1f)\n", size, size/16.0f);
	Sleep(10000);
#endif



#if 0

	//double initVal = 10;
	double aveDist = 5;
	double stdDist = 5;
	double res;
	int i;

	mysrand(0);

	for(i=0; i<12; i++)
	{
		res = /*fabs*/(noise(aveDist, stdDist));
		printf("Result: %f\n", res);
	}
#endif



#if 0

	DISTANGL distBear;
	double lat0=37, lon0 = -116, latf=35, lonf=-116;
	
	distBear = DetermineBearing(lat0, lon0, latf, lonf);
	
	printf("distance = %f meters, bearing = %f\n", distBear.distance, distBear.angle);
#endif

#if 0
	int i, j;
	double deg;
	double lat0, lon0, latf, lonf;
	double lat[3] = {35.0, 36.0, 37.0};
	double lon[3] = {-147.0, -146.0, -145.0};

	lat0 = 36.0;
	lon0 = -146.0;

	for(i=0; i<3; i++)
	{
		for(j=0; j<3; j++)
		{
			latf = lat[i];
			lonf = lon[j];

			deg = DetermineBearing(lat0, lon0, latf, lonf);
			while(deg < 0)
				deg += 360.0;

			printf("(lat,lon) (%f,%f) to (%f,%f) (", lat0, lon0, latf, lonf);

			if(lonf < lon0)
				printf("-,");
			else if(lonf > lon0)
				printf("+,");
			else
				printf("0,");


			if(latf < lat0)
				printf("-)");
			else if(latf > lat0)
				printf("+)");
			else
				printf("0)");
			
			printf(" = %f\n", deg);
		}
	}

	printf("done\n");
#endif


#if 0
	int i;
	int		populationSize = 22;

	// Calculating distance about a click (Input vars)
	double	latInput = 0.0;  // 32
	double	lonInput = 0.0;  // -121
	double	aveDist =  20.0; // meters
	double	stdDist =  10.0; // meters

	// Intermediate calculations.
	double	bear;
	double	bearDeg;
	double	meters;
	double	temp_x, temp_y;
	double	lat, lon, rads;


	// Output calculations
	double	latOutput;
	double	lonOutput;

	mysrand(0);


	printf("input: (%f, %f) average distance(%f) std distance(%f)\n\n", latInput, lonInput, aveDist, stdDist);
	for(i=0; i<populationSize; i++)
	{

		bear	= myrand()*2*PI;
		//bear = -90*PI/180;
		bearDeg = bear*180/PI;
		meters	= noise(aveDist, stdDist);

		lon  = (PI / 180) * (-lonInput);
		lat  = (PI / 180) * (latInput);
		rads = (PI / (180 * 60)) * (meters / 1852);
		
		// Calculate intermediates
		temp_y = ((lon - asin(sin(bear) * sin(rads) / cos(lat)) + PI));
		temp_x = (2 * PI);

		// latitude
		latOutput = (180/PI)*(asin(sin(lat) * cos(rads) + cos(lat)*sin(rads)*cos(bear)));
		// longitude
		lonOutput = -(180/PI) * ((temp_y - (temp_x * floor(temp_y / temp_x))) - PI);

		printf("animat %2d: %7.3f (m) %7.3f deg  (%19.15f, %19.15f)\n", i+1, meters, bearDeg, latOutput, lonOutput);
	}
#endif



#if 0
	CAnimat ani;
	COORD_DEPTH Coord;
	HANDLE hd;

	ani.SetLocation(3456.89, -1234.8975);

	hd = CreateFile("ani.bin", GENERIC_WRITE, 0, 0, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
	ani.SaveToBinFile(hd);
	CloseHandle(hd);

	ani.SetLocation(0, 0);

	hd = CreateFile("ani.bin", GENERIC_READ, 0, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
	ani.LoadFromBinFile(hd);
	Coord = ani.GetAnimatCoordinates();
	CloseHandle(hd);
#endif





#if 0
	int i;
	if(NULL == FindWindow(NULL, "Acoustics Workbench for Marine Animats, June 2006"))
		i = 0;
	else
		i = 1;
	
#endif
#if 0
	PROCESS_INFORMATION	 procInf;
	STARTUPINFO			 startUpInf;
	BOOL b;

	char *exe = "c:\\\\Program Files\\mmmbs\\3mbsc.exe";
	GetStartupInfo(&startUpInf);


	b = 
	CreateProcess(exe, // name of executable module
				"d:\\esmeoutput\\mmmb.in",			// command line string
				0,					// lpProcessAttributes,
				0,					// lpThreadAttributes, 
				FALSE,				// handle inheritance option
				CREATE_NEW_CONSOLE,		// creation flags
				0,					// new environment block
				0,					// current directory name
				&startUpInf,		// startup information
				&procInf);			// process information

	if(b == 1)
		b = b;
	else
		b = b;

#endif

#if 0
	CDataLogger log;
	CirThread logThread((IRunnable *) &log);

	logThread.start();
	while(log.IsRunning() == FALSE)
		Sleep(1);

	log.CreateLogFile("dog2.txt", TRUE);
	log.LogMessage("message 1");
	log.LogMessage("message 2");
	log.LogMessage("message 3");
	log.LogMessage("message 4");
	log.LogMessage("message 5");

	log.SetOutputDirectory("D:\\esmeOutput");

	log.Exit();
#endif

#if 0
	//_execl("dir", "n");
	system("copy cmdpromptjunk.cpp dog.cpp");
	system("dir");
#endif

	//


#if 0
	char name[256];
	DWORD dog;

	dog = 256;
	GetUserName(name, &dog);

	printf("%s\n", name);
#endif
#if 0
	ENVDATA_INDEX sector = {0};
	FILE *fd;
	CEnvironmentData envdat;
	double x,y,z = 0;
	int res;
	double value;
	int numDimensions;
	HANDLE hd;

//	fd = fopen_s("D:\\testData\\SouthCal5minNegShallow.bth", "r");
//	fd = fopen_s("D:\\testData\\1x1x1aout.txt", "r");
//	fd = fopen_s("D:\\testData\\aout.txt", "r");

	fd = fopen_s("D:\\testData\\2x2aout.txt", "r");
	envdat.LoadFromTextFile(fd);
	fclose(fd);

	hd = CreateFile("cow.env", GENERIC_WRITE, 0, 0, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
	envdat.SaveToBinFile(hd);
	CloseHandle(hd);

	envdat.ClearData();
	hd = CreateFile("cow.env", GENERIC_READ, 0, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
	envdat.LoadFromBinFile(hd);
	


	numDimensions = envdat.GetNumDimensions();

	fd = fopen_s("D:\\testData\\output.txt", "w");

	x = MINX;
	while(x <= MAXX)
	{
		y = MINY;
		while(y <= MAXY)
		{
			if(numDimensions == 2)
			{

				res = envdat.GetValueAtCoordinate(x, y, 0, &value, &sector);
				fprintf(fd, "Sector(%2d, %2d) for (%7.4f,%8.4f), value = %8.4f", sector.x, sector.y, x, y, value);
				if(res & ENVXL)
					fprintf(fd," Xl");
				if(res & ENVXU)
					fprintf(fd," Xu");
				if(res & ENVYL)
					fprintf(fd," Yl");
				if(res & ENVYU)
					fprintf(fd," Yu");
				fprintf(fd,"\n");
				z += RESZ;
			}
			else if(numDimensions == 3)
			{
				z = MINZ;
				while(z <= MAXZ)
				{
					res = envdat.GetValueAtCoordinate(x, y, z, &value, &sector);
					fprintf(fd, "Sector(%2d, %2d, %2d) for (%7.4f,%8.4f, %2.2f), value = %8.4f", sector.x, sector.y, sector.z, x, y, z, value);
					if(res & ENVXL)
						fprintf(fd," Xl");
					if(res & ENVXU)
						fprintf(fd," Xu");
					if(res & ENVYL)
						fprintf(fd," Yl");
					if(res & ENVYU)
						fprintf(fd," Yu");
					if(res & ENVZL)
						fprintf(fd," Zl");
					if(res & ENVZU)
						fprintf(fd," Zu");
					fprintf(fd,"\n");
					z += RESZ;
				}
			}
			y += RESY;
		}
		fprintf(fd,"\n");
		x += RESX;
	}
	fclose(fd);
#endif
