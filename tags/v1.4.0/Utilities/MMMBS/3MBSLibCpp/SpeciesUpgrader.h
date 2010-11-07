#ifndef SPECIESUPGRADER_H
#define SPECIESUPGRADER_H

// SpeciesVersionUpgrader.cpp : Defines the entry point for the console application.
//
#include "dataTypes.h"
#include "SpeDataTypesV2.h"
#include "SpeDataTypesV3.h"
#include "SpeDataTypesV4.h"
#include "SpeDataTypesV5.h"
#include "SpeDataTypesV6.h"
#include "SpeDataTypesV7.h"
#include "3mbsLib.h"
#include "staticLib.h"


class CSpeciesUpgrader
{
public:
	CSpeciesUpgrader();
	virtual ~CSpeciesUpgrader();
private:
	C3mbStaticsLib m_staticLib;
public:

	CSpeciesModel m_speciesModel;

	// current version
	RESLT Save(const TCHAR *szFileName, SPECIES_MDL *pSpeMdl);
	RESLT ReadSpeciesModel(HANDLE Hd, SPECIES_MDL *pSpeMdl);


	// version 2 to version 3
	RESLT UpgradeV2(HANDLE Hd, __int64 SpecesFileLocation, SPECIES_MDL *pSpeMdl);
	RESLT ReadV2(HANDLE Hd, __int64 SpecesFileLocation, SPECIES_MDL_V2 *pSpeMdl);
	RESLT ReadV2Behaviors(HANDLE Hd, int BehaviorCount, NRMLBEHMDL_V2_V3 *BehBuff);
	RESLT ConvertV2toV3(SPECIES_MDL_V3 *pV3, SPECIES_MDL_V2 *pV2);

	// version 3 to version 4
	RESLT UpgradeV3(HANDLE Hd, __int64 SpecesFileLocation, SPECIES_MDL *pSpeMdl);
	RESLT ReadV3(HANDLE Hd, __int64 SpecesFileLocation, SPECIES_MDL_V3 *pSpeMdl);
	RESLT ReadV3Behaviors(HANDLE Hd, int BehaviorCount, NRMLBEHMDL_V2_V3 *BehBuff);
	RESLT ConvertV3toV4(SPECIES_MDL_V4 *pV4, SPECIES_MDL_V3 *pV3);

	// version 4 to version 5
	RESLT UpgradeV4(HANDLE Hd, __int64 SpecesFileLocation, SPECIES_MDL *pSpeMdl);
	RESLT ReadV4(HANDLE Hd, __int64 SpecesFileLocation, SPECIES_MDL_V4 *pV4);
	//RESLT ReadV4Behaviors(HANDLE Hd, int BehaviorCount, NRMLBEHMDL_V2_V3 *BehBuff); // Implemented in ReadV4
	RESLT ConvertV4toV5(SPECIES_MDL_V5 *pV5, SPECIES_MDL_V4 *pV4);



	// Version 5 to version 6
	RESLT UpgradeV5(HANDLE Hd, __int64 SpecesFileLocation, SPECIES_MDL *pSpeMdl);
	RESLT ReadV5(HANDLE Hd, __int64 SpecesFileLocation, SPECIES_MDL_V5 *pV5);
	RESLT ConvertV5toV6(SPECIES_MDL_V6 *pV6, SPECIES_MDL_V5 *pV5);


	// Current Version
	RESLT UpgradeV6(HANDLE Hd, __int64 SpecesFileLocation, SPECIES_MDL *pSpeMdl);
	RESLT ReadV6(HANDLE Hd, __int64 SpecesFileLocation, SPECIES_MDL_V6 *pV6);
	RESLT ConvertV6toV7(SPECIES_MDL_V7 *pV7, SPECIES_MDL_V6 *pV6);


	// Current Version
	RESLT UpgradeV7(HANDLE Hd, __int64 SpecesFileLocation, SPECIES_MDL *pSpeMdl); // added
	RESLT ReadV7(HANDLE Hd, __int64 SpecesFileLocation, SPECIES_MDL_V7* pV7);
	RESLT ConvertV7toV8(SPECIES_MDL *pV8, SPECIES_MDL_V7 *pV7); // added


	RESLT ReadV8(HANDLE Hd, __int64 SpecesFileLocation, SPECIES_MDL* pV8); // added

private:
	C3MBRandom m_3MBRandom; // Not actually needed but is requred for CSpecies instantiation.

};


typedef struct FileName
{
	TCHAR sz[SIZE_256];
}FNAME;


#endif // SPECIESUPGRADER_H