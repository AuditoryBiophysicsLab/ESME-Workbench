#ifndef SPEDATATYPESVERSION_6_H
#define SPEDATATYPESVERSION_6_H

// This structure is written to file.
typedef struct BehaviorModel_version6
{
	TCHAR szName[SZ_BEHAVIOR_LEN];

	// Behavior Transitions
	int nrmlBehTransCnt;
	BEHTRAN8BYTEPTR nrmlBehTrans;

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
}NRMLBEHMDL_VER6;

union NRMLBEHMDL8BYTEPTR_VER6
{
	NRMLBEHMDL_VER6 *behavior; // 4 bytes compiled as 32 bit addressing, 8 bytes as 64 bit.
	TCHAR __space[SIZE_8];
};// 8 bytes regardless of 32- or 64-bit compilation.

// Used in both SpeciesBinaryOutInformation (SPECIESBINOUTINF) struct and 
// SpeciesModel (SPECIES_MDL) struct

//-----------------//
// Species Modeling
//-----------------//
typedef struct SpeciesModel_version6
{
	SPECIESSPECIFICATION description;
	NRMLBEHMDL8BYTEPTR_VER6 p;
	BYTE _reserved[SIZE_68];

	ACSTCAVRSNMDL acousticAversion;
	MATRIX initialBehavior;
}SPECIES_MDL_V6;


#endif //SPEDATATYPESVERSION_6_H