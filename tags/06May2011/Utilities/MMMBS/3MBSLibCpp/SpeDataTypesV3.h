#ifndef SPEDATATYPESVER3_H
#define SPEDATATYPESVER3_H

#include "SpeDataTypesCmmn.h"

typedef struct SpeciesModel_v3
{
	SPECIESSPECIFICATION description;
	NRMLBEHMDL8BYTEPTR_V2_V3 p; // Changes in version 4
	BYTE _reserved[SIZE_68];

	ACSTCAVRSNMDL acousticAversion;
	MATRIX initialBehavior;
}SPECIES_MDL_V3;


#endif // SPEDATATYPESVER3_H