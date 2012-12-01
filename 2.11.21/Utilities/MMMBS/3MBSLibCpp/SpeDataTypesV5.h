#ifndef SPEDATATYPESVERSION_5_H
#define SPEDATATYPESVERSION_5_H

typedef struct BehaviorModel_version5
{
	TCHAR szName[SZ_BEHAVIOR_LEN];

	// Behavior Transitions
	//BEHTRANSMDL nrmlBehTrans; // normal over time.  Version 4 changes this to a MATRIX.
	MATRIX nrmlBehTransMatrix;
	BEHTRANSMDL depthEnvAttBehTrans; // while in depth environmental attractor
	BEHTRANSMDL tempEnvAttBehTrans; // while in temperature environmental attractor

	// Environmental Attractors
	ENVATTRACTORMDL depthEnvAtt;
	ENVATTRACTORMDL tempEnvAtt;

	// Travel
	RATEMDL travelRate;
	DIRCTNMDL travelDirection;

	// Dive;
	DIVEMDL dive;

	BOOL depthHasPriorityOverTemp;
}NRMLBEHMDL_VER5;

union NRMLBEHMDL8BYTEPTR_VER5
{
	NRMLBEHMDL_VER5 *behavior; // 4 bytes compiled as 32 bit addressing, 8 bytes as 64 bit.
	TCHAR __space[SIZE_8];
};// 8 bytes regardless of 32- or 64-bit compilation.

typedef struct SpeciesModel_version5
{
	SPECIESSPECIFICATION description;
	NRMLBEHMDL8BYTEPTR_VER5 p;
	BYTE _reserved[SIZE_68];

	ACSTCAVRSNMDL acousticAversion;
	MATRIX initialBehavior;
}SPECIES_MDL_V5;


#endif
