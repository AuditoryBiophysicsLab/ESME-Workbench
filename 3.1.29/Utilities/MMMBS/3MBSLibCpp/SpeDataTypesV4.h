#ifndef SPEDATATYPESVERSION_4_H
#define SPEDATATYPESVERSION_4_H

typedef struct BehaviorModel_version4
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
	DIVEMDL_VER_2_3_4 dive;

	BOOL depthHasPriorityOverTemp;
}NRMLBEHMDL_VER4;


//-----------------//
// Species Modeling
//-----------------//
union NRMLBEHMDL8BYTEPTR_VER4
{
	NRMLBEHMDL_VER4 *behavior; // 4 bytes on a 32 bit machine, 8 bytes on a 64 bit machine.
	TCHAR __space[SIZE_8];
};


typedef struct SpeciesModelVer4
{
	SPECIESSPECIFICATION description;
	NRMLBEHMDL8BYTEPTR_VER4 p;
	BYTE _reserved[SIZE_68];

	ACSTCAVRSNMDL acousticAversion;
	MATRIX initialBehavior;
}SPECIES_MDL_V4;

#endif// SPEDATATYPESVERSION_4_H
