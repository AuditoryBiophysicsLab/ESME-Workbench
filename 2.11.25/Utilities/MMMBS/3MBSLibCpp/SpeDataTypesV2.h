#ifndef SPEDATATYPESVER2_H
#define SPEDATATYPESVER2_H

#include "SpeDataTypesCmmn.h"


typedef struct SpeciesSpecification_v2
{
	// No header string

	// starting version 3 these become sets of UINT32's
	double mbsVer;
	double speVer;

	TCHAR speciesShrtDscrptn[SIZE_32];
	TCHAR speciesComment[SIZE_1024];
	double shoreFollowDepth;
	int numBehaviors;
	SPECIESGROUP group;

	SPECIESNAME name;
	int year;
	int month;
	int day;

	int hour;
	int min;
	int sec;
	int id;
}SPECIESSPECIFICATION_V2;

typedef struct SpeciesModel_v2
{
	SPECIESSPECIFICATION_V2 description;
	NRMLBEHMDL8BYTEPTR_V2_V3 p;
	BYTE _reserved[SIZE_68];

	ACSTCAVRSNMDL acousticAversion;
	MATRIX initialBehavior;
}SPECIES_MDL_V2;


#endif // SPEDATATYPESVER2_H