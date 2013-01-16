#ifndef SPEDATATYPESCOMMON_H
#define SPEDATATYPESCOMMON_H

#include "dataTypes.h"
typedef struct DivingSpeciesModelVersions_2_3_and_4
{
	RATEMDL descentRate;
	RATEMDL ascentRate;
	SURFINTRVLPARAM srfInv;
	REVERSAL_DEF reversal;
	DEPTHPARAM depth;

	BOOL bottomFollows;
	BYTE _RESERVED[SIZE_28];
}DIVEMDL_VER_2_3_4;


typedef struct BehaviorModel_ver2_ver3
{
	TCHAR szName[SZ_BEHAVIOR_LEN];

	// Behavior Transitions
	BEHTRANSMDL nrmlBehTrans; // normal over time.  Version 4 changes this to a MATRIX.
	BEHTRANSMDL depthEnvAttBehTrans; // while in depth environmental attractor
	BEHTRANSMDL tempEnvAttBehTrans; // while in temperature environmental attractor

	// Environmental Attractors
	ENVATTRACTORMDL depthEnvAtt;
	ENVATTRACTORMDL tempEnvAtt;

	// Travel
	RATEMDL travelRate;
	DIRCTNMDL travelDirection;

	// Dive;
	DIVEMDL_VER_2_3_4 dive;

	BOOL depthHasPriorityOverTemp;
}NRMLBEHMDL_V2_V3; // This changes in version 4

union NRMLBEHMDL8BYTEPTR_V2_V3
{
	NRMLBEHMDL_V2_V3 *behavior; // 4 bytes on a 32 bit machine, 8 bytes on a 64 bit machine.
	TCHAR __space[SIZE_8];
	//EIGHTBYTES bytes;
}; // Changes in version 4


#endif// SPEDATATYPESCOMMON_H