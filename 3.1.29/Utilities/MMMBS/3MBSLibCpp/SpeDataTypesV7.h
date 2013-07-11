#ifndef SPEDATATYPESVERSION_7_H
#define SPEDATATYPESVERSION_7_H

typedef struct BehaviorModel_version7
{
	TCHAR szName[SZ_BEHAVIOR_LEN];

	// Behavior Transitions
	int nrmlBehTransCnt;
	BEHTRANS_TERM_MODEL nrmlBehTermType; // Added for behavior model version 7.
	BOOL depthHasPriorityOverTemp;
	BYTE _RES1[SIZE_20];

	BEHTRAN8BYTEPTR nrmlBehTrans;
	BYTE _RES2[SIZE_24];

	BEHTRANSMDL depthEnvAttBehTrans; // while in depth environmental attractor (not used at this time)
	BEHTRANSMDL tempEnvAttBehTrans; // while in temperature environmental attractor (not used at this time)

	// Environmental Attractors
	ENVATTRACTORMDL  depthEnvAtt;
	ENVATTRACTORMDL tempEnvAtt;

	// Travel
	RATEMDL travelRate;
	DIRCTNMDL travelDirection;

	// Dive;
	DIVEMDL dive;

	// More reserved space.
	BYTE _RES3[SIZE_64];
}NRMLBEHMDL_VER7;

union NRMLBEHMDL8BYTEPTR_VER7
{
	NRMLBEHMDL_VER7 *behavior; // 4 bytes compiled as 32 bit addressing, 8 bytes as 64 bit.
	TCHAR __space[SIZE_8];
};// 8 bytes regardless of 32- or 64-bit compilation.


typedef struct SpeciesModel_ver7
{
	SPECIESSPECIFICATION description;
	NRMLBEHMDL8BYTEPTR_VER7 p;
	BYTE _reserved[SIZE_72]; // from 68 to 72

	ACSTCAVRSNMDL acousticAversion;
	MATRIX initialBehavior;
}SPECIES_MDL_V7;


#endif // SPEDATATYPESVERSION_7_H