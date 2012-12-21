#include "SpeciesUpgrader.h"

const TCHAR* SZFILESTRING = "v%d.%02d%s.spe";

CSpeciesUpgrader::CSpeciesUpgrader()
{
}
CSpeciesUpgrader::~CSpeciesUpgrader()
{
}


RESLT CSpeciesUpgrader::ReadSpeciesModel(HANDLE Hd, SPECIES_MDL *pSpeMdl)
{
	TCHAR *szBuff = NULL;
	DWORD bytes;
	RESLT res = OK;
	UINT32 super, sub;
	__int64 speciesMdlFileLocation = SetFilePointer(Hd, 0, NULL, FILE_CURRENT);

	szBuff = new TCHAR[sizeof(SZ_SPECIESSPECIFICATION_HEADER)];


	// Read in the first 16 bytes of the file into a character buffer to determine if the
	// file version is 3.xx or higher.  All 3.xx or higer versions have a 16 byte
	// identifying string defined in SZ_SPECIESSPECIFICATION_HEADER
	if(0 == ReadFile(Hd, szBuff, sizeof(SZ_SPECIESSPECIFICATION_HEADER), &bytes, NULL))
	{
		delete [] szBuff;
		szBuff = NULL;
		return FILEREAD_ERROR;
	}

	// Check for version earlier than 3.xx
	if(strcmp(szBuff, SZ_SPECIESSPECIFICATION_HEADER) != 0)
	{
		delete [] szBuff;
		szBuff = NULL;
		// Version 2.4 was the only species version released and used regularly.  Version
		// 2.4 had no header string so a non-mach in the string compare above indicates
		// this is version 2.4.  That it is indeed version 2.4 is verified in function
		// ConvertV2toV3().

		// Convert version 2 to version 4.
		if(OK != (res = UpgradeV2(Hd, speciesMdlFileLocation,  pSpeMdl)))
			return res;
	}
	else
	{
		delete [] szBuff;
		szBuff = NULL;

		// The first 16 bytes match the species models string header
		// SZ_SPECIESSPECIFICATION_HEADER so it is version 3 or later.  Determine which
		// version it is and take action based based on that.

		// The first 16 bytes have already been read in putting the file pointer past the
		// header string.  Regardless of which version and therefore which version header
		// struct starts the file all version starting at and following version 3 will
		// have the first 16 bytes be a header string, the second 16 bytes making up
		// the 3mbs version number it was saved under (make up itself of two UINT32's
		// that are 4 bytes each in the format of superVer.subVer) and the third 16 bytes
		// making up the species version number the model is (also make up of two UINT32's
		// also in the order of superVer.subVer.

		// Place the file pointer at teh location in file that stores the species version
		// information.
		m_staticLib.MySetFilePointer(Hd, sizeof(UINT32)* 2, FILE_CURRENT);

		//------------------------------//
		// Read in the species version.
		//------------------------------//
		// Super version
		ReadFile(Hd, (LPVOID)&super, (DWORD)sizeof(UINT32), (LPDWORD)&bytes, NULL);
		// Sub version (not important, typically)
		ReadFile(Hd, (LPVOID)&sub, (DWORD)sizeof(UINT32), (LPDWORD)&bytes, NULL);

		switch(super)
		{
		case 3:
			res = UpgradeV3(Hd, speciesMdlFileLocation, pSpeMdl);
			break;

		case 4: // as of January 18, 2009 version 4 is the current version.
			//res = ReadV4(Hd, speciesMdlFileLocation, pSpeMdl);
			res = UpgradeV4(Hd, speciesMdlFileLocation, pSpeMdl);
			break;

		case 5: // as of early July 2009 version 5 is the current version.
			res = UpgradeV5(Hd, speciesMdlFileLocation, pSpeMdl);
			break;

		case 6: // as of December 2009 version 6 is the current version.
			res = UpgradeV6(Hd, speciesMdlFileLocation, pSpeMdl);
			break;

		case 7: // as of Feburary 22 2010 version 7 is the current version.
			res = UpgradeV7(Hd, speciesMdlFileLocation, pSpeMdl);
			break;
		case 8:
			res = ReadV8(Hd, speciesMdlFileLocation, pSpeMdl);
			break;
		default:
			res = OBSOLETE_3MBS_VERSION;
		}
		return res;
	}
	return OK;
}

RESLT CSpeciesUpgrader::UpgradeV2(HANDLE Hd, __int64 SpecesFileLocation, SPECIES_MDL *pSpeMdl)
{
	RESLT res;
	SPECIES_MDL_V2 speMdl_v2 = {0};
	SPECIES_MDL_V3 speMdl_v3 = {0};
	SPECIES_MDL_V4 speMdl_v4 = {0};
	SPECIES_MDL_V5 speMdl_v5 = {0};
	SPECIES_MDL_V6 speMdl_v6 = {0};
	SPECIES_MDL_V7 speMdl_v7 = {0};

	if(OK != (res = ReadV2(Hd, SpecesFileLocation, &speMdl_v2)))
		return res;

	// Upgrade version 2 to version 3
	if(OK != (res = ConvertV2toV3(&speMdl_v3, &speMdl_v2)))
		return res;

	// Convert version 3 to version 4
	if(OK != (res = ConvertV3toV4(&speMdl_v4, &speMdl_v3)))
		return res;

	// Convert version 4 to version 5
	if(OK != (res = ConvertV4toV5(&speMdl_v5, &speMdl_v4)))
		return res;

	// Convert version 5 to version 6
	if(OK != (res = ConvertV5toV6(&speMdl_v6, &speMdl_v5)))
		return res;

	// Convert version 6 to verion 7
	if(OK != (res = ConvertV6toV7(&speMdl_v7, &speMdl_v6)))
		return res;

	// Convert version 7 to verion 8
	if(OK != (res = ConvertV7toV8(pSpeMdl, &speMdl_v7)))
		return res;

	return OK;
}


RESLT CSpeciesUpgrader::ReadV2(HANDLE Hd, __int64 SpecesFileLocation, SPECIES_MDL_V2 *pSpeMdl)
{
	// Version 2.04
	DWORD bytes;
	double mbVer;
	double speVer;
	RESLT res;

	m_staticLib.MySetFilePointer(Hd, SpecesFileLocation, FILE_BEGIN);

	if(0 == ReadFile(Hd, &mbVer, sizeof(double), &bytes, NULL))return FILEREAD_ERROR;
	if(0 == ReadFile(Hd, &speVer, sizeof(double), &bytes, NULL))return FILEREAD_ERROR;

	// Verify the species version is 2.04, the last species version that used
	// a double to store version informaiton.
	if(speVer != 2.04)
		return OBSOLETE_SPECIES_VERSION;

	// Set the file pointer back to the begining of the file so the entire struct can
	// be read in.
	m_staticLib.MySetFilePointer(Hd, 0, FILE_BEGIN);

	// The species file being read in is version 2.04.  Read in and convert for
	// compatability.
	// Read the SPECIES_MDL_V2 structure from file.
	if(0 == ReadFile(Hd, pSpeMdl, sizeof(SPECIES_MDL_V2), &bytes, NULL))
		return FILEREAD_ERROR;

	if(OK != (res = m_speciesModel.ReadVector(Hd, &pSpeMdl->initialBehavior)))
		return res;
	pSpeMdl->p.behavior = NULL;

	//  each behavior to file
	pSpeMdl->p.behavior = (NRMLBEHMDL_V2_V3 *)malloc(pSpeMdl->description.numBehaviors * sizeof(NRMLBEHMDL_V2_V3));

	return(ReadV2Behaviors(Hd, pSpeMdl->description.numBehaviors, pSpeMdl->p.behavior));
}

// The process for reading in behaviors is the same for all of version 2 and version 3
RESLT CSpeciesUpgrader::ReadV2Behaviors(HANDLE Hd, int BehaviorCount, NRMLBEHMDL_V2_V3 *BehBuff)
{
	int i;
	DWORD bytes;
	NRMLBEHMDL_V2_V3 *beh;
	RESLT res;
	BOOL rslt = TRUE;

	for(i=0; i<BehaviorCount; i++)
	{
		beh = &BehBuff[i];
		memset(beh, 0, sizeof(NRMLBEHMDL_V2_V3));

		// Read the behavior structure from file
		rslt &= ReadFile(Hd, beh, sizeof(NRMLBEHMDL_V2_V3), &bytes, NULL);
		if(rslt == FALSE)
			return FILEWRITE_ERROR;

		// Handle all the dynamically allocated memory contained in substructures of 
		// this structure
		// Normal behavior transition
		res = m_speciesModel.ReadVector(Hd, &beh->nrmlBehTrans.behavior); if(res != OK) return res;
		res = m_speciesModel.ReadVector(Hd, &beh->nrmlBehTrans.terminate); if(res != OK) return res;

		// Depth environmenatal attractor behavior transition
		res = m_speciesModel.ReadVector(Hd, &beh->depthEnvAttBehTrans.behavior); if(res != OK) return res;
		res = m_speciesModel.ReadVector(Hd, &beh->depthEnvAttBehTrans.terminate); if(res != OK) return res;

		// Depth temperature attractor behavior transition
		res = m_speciesModel.ReadVector(Hd, &beh->tempEnvAttBehTrans.behavior); if(res != OK) return res;
		res = m_speciesModel.ReadVector(Hd, &beh->tempEnvAttBehTrans.terminate); if(res != OK) return res;

		// Travel Rate
		res = m_speciesModel.ReadVector(Hd, &beh->travelRate.vm.step); if(res != OK) return res;
		res = m_speciesModel.ReadVector(Hd, &beh->travelRate.vm.terminate); if(res != OK) return res;
		res = m_speciesModel.ReadVector(Hd, &beh->travelRate.vm.vector); if(res != OK) return res;

		// Travel direction
		res = m_speciesModel.ReadVector(Hd, &beh->travelDirection.vm.direction); if(res != OK) return res;
		res = m_speciesModel.ReadVector(Hd, &beh->travelDirection.vm.directionalBias); if(res != OK) return res;
		res = m_speciesModel.ReadVector(Hd, &beh->travelDirection.vm.terminate); if(res != OK) return res;

		// Descent rate
		res = m_speciesModel.ReadVector(Hd, &beh->dive.descentRate.vm.step); if(res != OK) return res;
		res = m_speciesModel.ReadVector(Hd, &beh->dive.descentRate.vm.terminate); if(res != OK) return res;
		res = m_speciesModel.ReadVector(Hd, &beh->dive.descentRate.vm.vector); if(res != OK) return res;

		// Ascent rate
		res = m_speciesModel.ReadVector(Hd, &beh->dive.ascentRate.vm.step); if(res != OK) return res;
		res = m_speciesModel.ReadVector(Hd, &beh->dive.ascentRate.vm.terminate); if(res != OK) return res;
		res = m_speciesModel.ReadVector(Hd, &beh->dive.ascentRate.vm.vector); if(res != OK) return res;

		// Surface Interval
		res = m_speciesModel.ReadVector(Hd, &beh->dive.srfInv.vm.step); if(res != OK) return res;
		res = m_speciesModel.ReadVector(Hd, &beh->dive.srfInv.vm.vector); if(res != OK) return res;

		// Reversal
		res = m_speciesModel.ReadVector(Hd, &beh->dive.reversal.vm.count); if(res != OK) return res;
		res = m_speciesModel.ReadVector(Hd, &beh->dive.reversal.vm.probOfReversal); if(res != OK) return res;
		res = m_speciesModel.ReadVector(Hd, &beh->dive.reversal.vm.time); if(res != OK) return res;
		res = m_speciesModel.ReadVector(Hd, &beh->dive.reversal.vm.timeStep); if(res != OK) return res;

		// Depth Model
		res = m_speciesModel.ReadVector(Hd, &beh->dive.depth.vm.step); if(res != OK) return res;
		res = m_speciesModel.ReadVector(Hd, &beh->dive.depth.vm.vector); if(res != OK) return res;
	}
	return OK;
}


RESLT CSpeciesUpgrader::ConvertV2toV3(SPECIES_MDL_V3 *pV3, SPECIES_MDL_V2 *pV2)
{

	memset(pV3, 0, sizeof(SPECIES_MDL_V3));

	// Add the string header
	sprintf_s(pV3->description.szHeader, sizeof(pV3->description.szHeader), "%s", SZ_SPECIESSPECIFICATION_HEADER);

	// Currently there is to added value to the species model to maintain the original
	// 3mb version information the species was saved under to update it to the current
	// 3mb version.
	pV3->description.mbsVerSuper = MMBSLIB_VERSION_SUPER;
	pV3->description.mbsVerSub = MMBSLIB_VERSION_SUB;

	// Version 3 is obsoleted so set the species version to the last species version before
	// version 3 became obsoleted.  Setting the value here would not matter normally but
	// if the user launches this conversion program with the save switch set to true
	// this version three version will be saved to file and so it will matter.
	pV3->description.speVerSuper = UINT32(floor(pV2->description.mbsVer));
	pV3->description.speVerSub = UINT32((pV2->description.mbsVer - floor(pV2->description.mbsVer)) * 10);

	pV3->description.year = pV2->description.year; // Save Year
	pV3->description.month = pV2->description.month;// Save Month
	
	pV3->description.day = pV2->description.day;// Save Day
	pV3->description.hour = pV2->description.hour;// Save Hour
	pV3->description.min = pV2->description.min;// Save Min
	pV3->description.sec = pV2->description.sec;// Save Sec
	
	pV3->description.id = pV2->description.id; 
	pV3->description.numBehaviors = pV2->description.numBehaviors;
	pV3->description.group = pV2->description.group;  // Species Group enumeration
	pV3->description.name = pV2->description.name; // Species name enumeration.

	pV3->description.shoreFollowDepth = pV2->description.shoreFollowDepth;

	// A minimum seeding depth was not defined in species models previous to version 3 so
	// for the conversion from version 2 to version 3 set the minimum seeding depth to
	// the value defined for minimum shore following.
	pV3->description.minSeedingDepth = pV3->description.shoreFollowDepth;

	//----------------------------------------------------------------------------------//
	// Deep water seeding depth and deep water seeding enabled were not defined prior to
	// version 3 so default these values.
	//----------------------------------------------------------------------------------//
	pV3->description.deepWaterSeedingDepth = -500000;
	pV3->description.deepWaterSeedingDepthEnabled = FALSE;
	//----------------------------------------------------------------------------------//

	strncpy_s(pV3->description.speciesShrtDscrptn, sizeof(pV3->description.speciesShrtDscrptn), pV2->description.speciesShrtDscrptn, strlen(pV2->description.speciesShrtDscrptn));
	strncpy_s(pV3->description.speciesComment, sizeof(pV3->description.speciesComment),pV2->description.speciesComment, strlen(pV2->description.speciesComment));

	// The behavior for version 3 will be loaded in separately.
	pV3->p.behavior = pV2->p.behavior;
	pV3->initialBehavior = pV2->initialBehavior;
	pV3->acousticAversion = pV2->acousticAversion;
	return OK;
}


RESLT CSpeciesUpgrader::UpgradeV3(HANDLE Hd, __int64 SpecesFileLocation, SPECIES_MDL *pSpeMdl)
{
	RESLT res;
	SPECIES_MDL_V3 speMdl_v3 = {0}; // this SPECIES_MDL_V3 is expected to change to SPECIES_MDL_3xx
	SPECIES_MDL_V4 speMdl_v4 = {0}; // this SPECIES_MDL_V3 is expected to change to SPECIES_MDL_3xx
	SPECIES_MDL_V5 speMdl_v5 = {0};
	SPECIES_MDL_V6 speMdl_v6 = {0};
	SPECIES_MDL_V7 speMdl_v7 = {0};

	// Read in model version 3
	if(OK != (res = ReadV3(Hd, SpecesFileLocation, &speMdl_v3)))
		return res;

	// Upgrade version 3 to version 4
	if(OK != (res = ConvertV3toV4(&speMdl_v4, &speMdl_v3)))
		return res;

	if(OK != (res = ConvertV4toV5(&speMdl_v5, &speMdl_v4)))
		return res;

	if(OK != (res = ConvertV5toV6(&speMdl_v6, &speMdl_v5)))
		return res;

	// Convert version 6 to verion 7
	if(OK != (res = ConvertV6toV7(&speMdl_v7, &speMdl_v6)))
		return res;

	// Convert version 7 to verion 8
	if(OK != (res = ConvertV7toV8(pSpeMdl, &speMdl_v7)))
		return res;

	return OK;
}


RESLT CSpeciesUpgrader::ReadV3(HANDLE Hd, __int64 SpecesFileLocation, SPECIES_MDL_V3 *pSpeMdl)
{
	UINT32 speVerSuper = 0, speVerSub = 0;
	DWORD bytes = 0;
	RESLT res;
	TCHAR szBuff[SIZE_256];

	m_staticLib.MySetFilePointer(Hd, SpecesFileLocation, FILE_BEGIN);

	// Read in the first 16 bytes to initially determine the species version number.
	// If the first 16 bytes hold the value defined in SZ_SPECIESSPECIFICATION_HEADER,
	// then it is a version 2.xx species.
	if(0 == ReadFile(Hd, szBuff, sizeof(SZ_SPECIESSPECIFICATION_HEADER), &bytes, NULL))
		return FILEREAD_ERROR;

	if(strcmp(szBuff, SZ_SPECIESSPECIFICATION_HEADER) != 0)
		return OBSOLETE_SPECIES_VERSION;

	// Version information will be 24 bytes into the file (header string is the first
	// 16, then two UINT32's for mbs version for the next 8).  Set the file pointer
	// to 24 bits into the file to verify this species file being read in is the current version.
	m_staticLib.MySetFilePointer(Hd, SIZE_8, FILE_CURRENT);

	if(0 == ReadFile(Hd, &speVerSuper, sizeof(UINT32), &bytes, NULL))
		return FILEREAD_ERROR;

	if(0 == ReadFile(Hd, &speVerSub, sizeof(UINT32), &bytes, NULL))
		return FILEREAD_ERROR;

	// Only care that this is a version 3 species file.  Sub-version not important.
	if(speVerSuper != 3)
		return OBSOLETE_SPECIES_VERSION;

	// Set the file pointer back to the begining then read the SPECIES_MDL structure from
	// file.
	m_staticLib.MySetFilePointer(Hd, 0, FILE_BEGIN);
	if(0 == ReadFile(Hd, pSpeMdl, sizeof(SPECIES_MDL_V3), &bytes, NULL))
		return FILEREAD_ERROR;

	if(OK != (res = m_speciesModel.ReadVector(Hd, &pSpeMdl->initialBehavior)))
		return res;

	pSpeMdl->p.behavior = NULL;

	//  each behavior to file
	pSpeMdl->p.behavior = (NRMLBEHMDL_V2_V3 *)malloc(pSpeMdl->description.numBehaviors * sizeof(NRMLBEHMDL_V2_V3));
	if(pSpeMdl->p.behavior == NULL)
		return MEMALLOC_ERROR;

	return ReadV3Behaviors(Hd, pSpeMdl->description.numBehaviors, pSpeMdl->p.behavior);
}

RESLT CSpeciesUpgrader::ReadV3Behaviors(HANDLE Hd, int BehaviorCount, NRMLBEHMDL_V2_V3 *BehBuff)
{
	// Same process for both version 2 and version 3 species models.
	return ReadV2Behaviors(Hd, BehaviorCount, BehBuff);
}

RESLT CSpeciesUpgrader::ConvertV3toV4(SPECIES_MDL_V4 *pV4, SPECIES_MDL_V3 *pV3)
{
	UINT32 i, j;
	UINT32 numBehaviors;
	NRMLBEHMDL_VER4 *beh4;
	NRMLBEHMDL_V2_V3 *beh3;

	//----------------------------------------------------------------------------------//
	// Species Description
	//--------------------//
	// No changes between versions 3 and 4, but update the version numbers.
	memcpy(&pV4->description, &pV3->description, sizeof(SPECIESSPECIFICATION));

	// Update the versioning on the species description
	pV4->description.mbsVerSuper = pV3->description.mbsVerSuper; //MMBSLIB_VERSION_SUPER;
	pV4->description.mbsVerSub = pV3->description.mbsVerSub; //MMBSLIB_VERSION_SUB;
	pV4->description.speVerSuper = pV3->description.speVerSuper; //MMMBLIB_SPECIES_VERSION_SUPER;
	pV4->description.speVerSub = pV3->description.speVerSub; //MMMBLIB_SPECIES_VERSION_SUB;
	//----------------------------------------------------------------------------------//

	//----------------------------------------------------------------------------------//
	// Normal Behaviors
	//--------------------//
	// Difference between Version 4 and Version 3 is that the behavior transition matix
	// that defines behavior transitions as a function of current behavior and time of
	// day replaces the behavior transition vector associated with each behavior of
	// version 3.

	// Allocate space for each behavior.
	pV4->p.behavior = (NRMLBEHMDL_VER4 *)malloc(pV4->description.numBehaviors * sizeof(NRMLBEHMDL_VER4));
	if(pV4->p.behavior == NULL)
		return MEMALLOC_ERROR;

	numBehaviors = pV4->description.numBehaviors;
	for(i=0; i<numBehaviors; i++)
	{
		beh4 = &pV4->p.behavior[i];
		beh3 = &pV3->p.behavior[i];
		memset(beh4, 0, sizeof(NRMLBEHMDL_VER4));

		// Copy behavior name.
		strncpy_s(beh4->szName, sizeof(beh4->szName), beh3->szName, strlen(beh3->szName));

		//------------------------------------------------------------------------------//
		//------------------------------------------------------------------------------//
		// Transition Matrix from transition vectors
		// (this is the major difference between version 3 and version 4)
		//---------------------------------------------------------------//
		// Use the transition vectors of each behavior of version 3 to build a first row
		// of each behavior's transition matrix of version 4.  The version converstion
		// will give the behavior transition matrices only a single row.
		if(FALSE == m_speciesModel.AllocateMatrix(&beh4->nrmlBehTransMatrix, 1, pV4->description.numBehaviors + 5))
			return MEMALLOC_ERROR;

		// Transition time span: start time (column 0), end time (column 1)
		beh4->nrmlBehTransMatrix.p.ppa[0][0] = 0.0;
		beh4->nrmlBehTransMatrix.p.ppa[0][1] = 24.0;

		// Copy the probability of transition values from the version 3 vector into this
		// newly created first row of the transition matrix into the associated column for
		// each behavior.
		for(j=0; j<=numBehaviors; j++)
			beh4->nrmlBehTransMatrix.p.ppa[0][j+2] = beh3->nrmlBehTrans.behavior.p.pa[j];

		// Mean time in behavior (T50) and slope coefficient (k).
		beh4->nrmlBehTransMatrix.p.ppa[0][numBehaviors + 5 - 2] = beh3->nrmlBehTrans.meanTimeInBeh;
		beh4->nrmlBehTransMatrix.p.ppa[0][numBehaviors + 5 - 1] = beh3->nrmlBehTrans.slopeCoefficient;
		//------------------------------------------------------------------------------//
		//------------------------------------------------------------------------------//

		// Environmental attractor transitions
		beh4->depthEnvAttBehTrans = beh3->depthEnvAttBehTrans;
		beh4->tempEnvAttBehTrans = beh3->tempEnvAttBehTrans;

		// Environmental attractors
		beh4->depthEnvAtt = beh3->depthEnvAtt;
		beh4->tempEnvAtt = beh3->tempEnvAtt;

		// Travel Rate
		beh4->travelRate = beh3->travelRate;

		// Travel direction
		beh4->travelDirection = beh3->travelDirection;

		// Dive
		beh4->dive = beh3->dive;

		// Environmental attractor priority
		beh4->depthHasPriorityOverTemp = beh3->depthHasPriorityOverTemp;
	}
	//----------------------------------------------------------------------------------//
	
	//----------------------------------------------------------------------------------//
	// Acoustic Aversion
	//------------------//
	// No changes between versions 3 and 4
	pV4->acousticAversion = pV3->acousticAversion;
	//----------------------------------------------------------------------------------//

	//----------------------------------------------------------------------------------//
	// Initial Behavior matrix
	//------------------------//
	// No changes between versions 3 and 4
	pV4->initialBehavior = pV3->initialBehavior;
	//m_speciesModel.CopyMatrix(&pV4->initialBehavior, &pV3->initialBehavior);
	//----------------------------------------------------------------------------------//

	return OK;
}


RESLT CSpeciesUpgrader::UpgradeV4(HANDLE Hd, __int64 SpecesFileLocation, SPECIES_MDL *pSpeMdl)
{
	RESLT res;
	SPECIES_MDL_V4 speMdl_v4 = {0}; // this SPECIES_MDL_V3 is expected to change to SPECIES_MDL_3xx
	SPECIES_MDL_V5 speMdl_v5 = {0};
	SPECIES_MDL_V6 speMdl_v6 = {0};
	SPECIES_MDL_V7 speMdl_v7 = {0};

	// Read in model version 3
	if(OK != (res = ReadV4(Hd, SpecesFileLocation, &speMdl_v4)))
		return res;

	if(OK != (res = ConvertV4toV5(&speMdl_v5, &speMdl_v4)))
		return res;

	if(OK != (res = ConvertV5toV6(&speMdl_v6, &speMdl_v5)))
		return res;

	// Convert version 6 to verion 7
	if(OK != (res = ConvertV6toV7(&speMdl_v7, &speMdl_v6)))
		return res;

	// Convert version 7 to verion 8
	if(OK != (res = ConvertV7toV8(pSpeMdl, &speMdl_v7)))
		return res;

	return OK;
}


// Version 4 is already loaded into memory.  This fnction transfers the information into version 5
RESLT CSpeciesUpgrader::ConvertV4toV5(SPECIES_MDL_V5 *pV5, SPECIES_MDL_V4* pV4)
{
	UINT32 i;
	NRMLBEHMDL_VER4 *beh4;
	NRMLBEHMDL_VER5 *beh5;
	//----------------------------------------------------------------------------------//
	// Species Description
	//--------------------//
	// No changes between versions 4 and 5, but update the version numbers.
	memcpy(&pV5->description, &pV4->description, sizeof(SPECIESSPECIFICATION));
	//----------------------------------------------------------------------------------//

	//----------------------------------------------------------------------------------//
	// Normal Behaviors
	//--------------------//
	// Difference between Version 4 and Version 5 is that the behavior transition matix
	// that defines behavior transitions as a function of current behavior and time of
	// day replaces the behavior transition vector associated with each behavior of
	// version 3.

	// Allocate space for each behavior.
	if(NULL == (pV5->p.behavior = (NRMLBEHMDL_VER5 *)malloc(pV5->description.numBehaviors * sizeof(NRMLBEHMDL_VER5))))
		return MEMALLOC_ERROR;

	for(i=0; i<pV5->description.numBehaviors; i++)
	{
		beh5 = &pV5->p.behavior[i];
		beh4 = &pV4->p.behavior[i];
		memset(beh5, 0, sizeof(NRMLBEHMDL_VER5));

		// Copy the behavior name.
		strncpy_s(beh5->szName, sizeof(beh5->szName), beh4->szName, strlen(beh4->szName));


		//------------------------------------------------------------------------------//
		// Transition Matrix from transition vectors
		// (no changes between version 4 and version 5)
		//---------------------------------------------------------------//
		beh5->nrmlBehTransMatrix = beh4->nrmlBehTransMatrix;
		//------------------------------------------------------------------------------//
		// Environmental attractor transitions
		beh5->depthEnvAttBehTrans = beh4->depthEnvAttBehTrans;
		beh5->tempEnvAttBehTrans = beh4->tempEnvAttBehTrans;

		// Environmental attractors
		beh5->depthEnvAtt = beh4->depthEnvAtt;
		beh5->tempEnvAtt = beh4->tempEnvAtt;

		// Travel Rate
		beh5->travelRate = beh4->travelRate;

		// Travel direction
		beh5->travelDirection = beh4->travelDirection;

		//------------------------------------------------------------------------------//
		// Dive
		// Dive is the only change from versin 4 to version 5.  Flat bottom diving now
		// has the option to control hoizontal dive rates.
		beh5->dive.descentRate = beh4->dive.descentRate;
		beh5->dive.ascentRate = beh4->dive.ascentRate;
		beh5->dive.srfInv = beh4->dive.srfInv;
		beh5->dive.reversal = beh4->dive.reversal;
		beh5->dive.depth = beh4->dive.depth;
		if(beh4->dive.bottomFollows == TRUE)
			beh5->dive.bttmFollow.type = BOTTOMFOLLOWS_NORML_MDL;
		else
			beh5->dive.bttmFollow.type = NO_BTTMFLLWNG;
		beh5->dive.bttmFollow.rateMdl.modelType = GAUSSIAN;
		//------------------------------------------------------------------------------//

		// Environmental attractor priority
		beh5->depthHasPriorityOverTemp = beh4->depthHasPriorityOverTemp;
	}
	//----------------------------------------------------------------------------------//
	
	//----------------------------------------------------------------------------------//
	// Acoustic Aversion
	//------------------//
	// No changes between versions 4 and 5
	pV5->acousticAversion = pV4->acousticAversion;
	//----------------------------------------------------------------------------------//

	//----------------------------------------------------------------------------------//
	// Initial Behavior matrix
	//------------------------//
	// No changes between versions 4 and 5
	pV5->initialBehavior = pV4->initialBehavior;
	//m_speciesModel.CopyMatrix(&pV5->initialBehavior, &pV4->initialBehavior);
	//----------------------------------------------------------------------------------//
	return OK;
}


RESLT CSpeciesUpgrader::ReadV4(HANDLE Hd, __int64 SpecesFileLocation, SPECIES_MDL_V4 *pV4)
{
	UINT32 i;
	DWORD bytes = 0;
	RESLT res;
	//TCHAR szBuff[SIZE_16] = {0};
	NRMLBEHMDL_VER4 *beh;
//	__int64 startPtr;

	//startPtr = (__int64)m_staticLib.MySetFilePointer(Hd, 0, FILE_CURRENT);

	m_staticLib.MySetFilePointer(Hd, SpecesFileLocation, FILE_BEGIN);

	if(0 == ReadFile(Hd, pV4, sizeof(SPECIES_MDL_V4), &bytes, NULL))
		return FILEREAD_ERROR;

	if(OK != (res = m_speciesModel.ReadVector(Hd, &pV4->initialBehavior)))
		return res;

	pV4->p.behavior = NULL;

	//  each behavior to file
	pV4->p.behavior = (NRMLBEHMDL_VER4 *)malloc(pV4->description.numBehaviors * sizeof(NRMLBEHMDL_VER4));
	if(pV4->p.behavior == NULL)
		return MEMALLOC_ERROR;

	for(i=0; i<pV4->description.numBehaviors; i++)
	{
		beh = &pV4->p.behavior[i];
		memset(beh, 0, sizeof(NRMLBEHMDL_VER4));

		// Read the behavior structure from file
		if(0 == ReadFile(Hd, beh, sizeof(NRMLBEHMDL_VER4), &bytes, NULL))
			return FILEWRITE_ERROR;

		// Handle all the dynamically allocated memory contained in substructures of 
		// this structure
		// Normal behavior transition
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->nrmlBehTransMatrix)))
			return res;

		// Depth environmenatal attractor behavior transition
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->depthEnvAttBehTrans.behavior)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->depthEnvAttBehTrans.terminate)))
			return res;

		// Depth temperature attractor behavior transition
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->tempEnvAttBehTrans.behavior)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->tempEnvAttBehTrans.terminate)))
			return res;

		// Travel Rate
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->travelRate.vm.step)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->travelRate.vm.terminate)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->travelRate.vm.vector)))
			return res;

		// Travel direction
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->travelDirection.vm.direction)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->travelDirection.vm.directionalBias)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->travelDirection.vm.terminate)))
			return res;

		// Descent rate
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.descentRate.vm.step)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.descentRate.vm.terminate)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.descentRate.vm.vector)))
			return res;

		// Ascent rate
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.ascentRate.vm.step)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.ascentRate.vm.terminate)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.ascentRate.vm.vector)))
			return res;

		// Surface Interval
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.srfInv.vm.step)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.srfInv.vm.vector)))
			return res;

		// Reversal
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.reversal.vm.count)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.reversal.vm.probOfReversal)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.reversal.vm.time)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.reversal.vm.timeStep)))
			return res;

		// Depth Model
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.depth.vm.step)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.depth.vm.vector)))
			return res;
	}
	return OK;
}


RESLT CSpeciesUpgrader::ReadV5(HANDLE Hd, __int64 SpecesFileLocation, SPECIES_MDL_V5 *pV5)
{
	UINT32 i;
	DWORD bytes = 0;
	RESLT res;
	//TCHAR szBuff[SIZE_16] = {0};
	NRMLBEHMDL_VER5 *beh;
//	__int64 startPtr;

	m_staticLib.MySetFilePointer(Hd, SpecesFileLocation, FILE_BEGIN);

	if(0 == ReadFile(Hd, pV5, sizeof(SPECIES_MDL_V5), &bytes, NULL))
		return FILEREAD_ERROR;

	if(OK != (res = m_speciesModel.ReadVector(Hd, &pV5->initialBehavior)))
		return res;

	pV5->p.behavior = NULL;

	//  each behavior to file
	pV5->p.behavior = (NRMLBEHMDL_VER5 *)malloc(pV5->description.numBehaviors * sizeof(NRMLBEHMDL_VER5));
	if(pV5->p.behavior == NULL)
		return MEMALLOC_ERROR;

	for(i=0; i<pV5->description.numBehaviors; i++)
	{
		beh = &pV5->p.behavior[i];
		memset(beh, 0, sizeof(NRMLBEHMDL_VER5));

		// Read the behavior structure from file
		if(0 == ReadFile(Hd, beh, sizeof(NRMLBEHMDL_VER5), &bytes, NULL))
			return FILEWRITE_ERROR;

		// Handle all the dynamically allocated memory contained in substructures of 
		// this structure
		// Normal behavior transition
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->nrmlBehTransMatrix)))
			return res;

		// Depth environmenatal attractor behavior transition
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->depthEnvAttBehTrans.behavior)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->depthEnvAttBehTrans.terminate)))
			return res;

		// Depth temperature attractor behavior transition
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->tempEnvAttBehTrans.behavior)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->tempEnvAttBehTrans.terminate)))
			return res;

		// Travel Rate
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->travelRate.vm.step)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->travelRate.vm.terminate)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->travelRate.vm.vector)))
			return res;

		// Travel direction
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->travelDirection.vm.direction)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->travelDirection.vm.directionalBias)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->travelDirection.vm.terminate)))
			return res;

		// Descent rate
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.descentRate.vm.step)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.descentRate.vm.terminate)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.descentRate.vm.vector)))
			return res;

		// Ascent rate
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.ascentRate.vm.step)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.ascentRate.vm.terminate)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.ascentRate.vm.vector)))
			return res;

		// Surface Interval
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.srfInv.vm.step)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.srfInv.vm.vector)))
			return res;

		// Reversal
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.reversal.vm.count)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.reversal.vm.probOfReversal)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.reversal.vm.time)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.reversal.vm.timeStep)))
			return res;

		// Depth Model
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.depth.vm.step)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.depth.vm.vector)))
			return res;
	}
	return OK;
}

RESLT CSpeciesUpgrader::UpgradeV5(HANDLE Hd, __int64 SpecesFileLocation, SPECIES_MDL *pSpeMdl)
{
	RESLT res;
	SPECIES_MDL_V5 speMdl_v5 = {0};
	SPECIES_MDL_V6 speMdl_v6 = {0};
	SPECIES_MDL_V7 speMdl_v7 = {0};

	// Read in model version 3
	if(OK != (res = ReadV5(Hd, SpecesFileLocation, &speMdl_v5)))
		return res;

	if(OK != (res = ConvertV5toV6(&speMdl_v6, &speMdl_v5)))
		return res;

	// Convert version 6 to verion 7
	if(OK != (res = ConvertV6toV7(&speMdl_v7, &speMdl_v6)))
		return res;
	
	// Convert version 7 to verion 8
	if(OK != (res = ConvertV7toV8(pSpeMdl, &speMdl_v7)))
		return res;

	return OK;
}

RESLT CSpeciesUpgrader::ConvertV5toV6(SPECIES_MDL_V6 *pV6, SPECIES_MDL_V5 *pV5)
{
	UINT32 i;
	NRMLBEHMDL_VER5 *beh5;
	NRMLBEHMDL_VER6 *beh6;
	//----------------------------------------------------------------------------------//
	// Species Description
	//--------------------//
	// No changes between versions 6 and 5.
	memcpy(&pV6->description, &pV5->description, sizeof(SPECIESSPECIFICATION));
	//----------------------------------------------------------------------------------//

	//----------------------------------------------------------------------------------//
	// Normal Behaviors
	//--------------------//
	// Allocate space for each behavior.
	if(NULL == (pV6->p.behavior = (NRMLBEHMDL_VER6 *)malloc(pV6->description.numBehaviors * sizeof(NRMLBEHMDL_VER6))))
		return MEMALLOC_ERROR;

	for(i=0; i<pV6->description.numBehaviors; i++)
	{
		beh6 = &pV6->p.behavior[i];
		beh5 = &pV5->p.behavior[i];
		memset(beh6, 0, sizeof(NRMLBEHMDL8BYTEPTR_VER6));

		// Copy the behavior name.
		strncpy_s(beh6->szName, sizeof(beh6->szName), beh5->szName, strlen(beh5->szName));

		// ----------------- Area of change between versions 5 and 6 -------------------//
		// Behavior transitions based on depth did not exist prior to version 6 so by
		// default previous versions are assinged a single depth span transition.
		beh6->nrmlBehTransCnt = 1;									  
		beh6->nrmlBehTrans.arr = new BEHTRAN[beh6->nrmlBehTransCnt];
		memset(beh6->nrmlBehTrans.arr, 0, sizeof(BEHTRAN)*beh6->nrmlBehTransCnt);

		// Indexing 0 because there is only a single depth span transition.
		beh6->nrmlBehTrans.arr[0].depthSpan.shallow = DEFAULT_DEPTH_SPAN_SHALLOW;
		beh6->nrmlBehTrans.arr[0].depthSpan.deep = DEFAULT_DEPTH_SPAN_DEEP;
		//------------------------------------------------------------------------------//


		// Transition Matrix from transition vectors
		beh6->nrmlBehTrans.arr[0].m = beh5->nrmlBehTransMatrix;

		// Environmental attractor transitions
		beh6->depthEnvAttBehTrans = beh5->depthEnvAttBehTrans;
		beh6->tempEnvAttBehTrans = beh5->tempEnvAttBehTrans;

		// Environmental attractors
		beh6->depthEnvAtt = beh5->depthEnvAtt;
		beh6->tempEnvAtt = beh5->tempEnvAtt;

		// Travel Rate
		beh6->travelRate = beh5->travelRate;

		// Travel direction
		beh6->travelDirection = beh5->travelDirection;

		// Dive
		beh6->dive = beh5->dive;

		// Environmental attractor priority
		beh6->depthHasPriorityOverTemp = beh5->depthHasPriorityOverTemp;
	}
	//----------------------------------------------------------------------------------//
	
	//----------------------------------------------------------------------------------//
	// Acoustic Aversion
	//------------------//
	// No changes between versions 4 and 5
	pV6->acousticAversion = pV5->acousticAversion;
	//----------------------------------------------------------------------------------//

	//----------------------------------------------------------------------------------//
	// Initial Behavior matrix
	//------------------------//
	// No changes between versions 4 and 5
	pV6->initialBehavior = pV5->initialBehavior;
	//m_speciesModel.CopyMatrix(&pV6->initialBehavior, &pV5->initialBehavior);
	//----------------------------------------------------------------------------------//

	return OK;
}


RESLT CSpeciesUpgrader::ReadV6(HANDLE Hd, __int64 SpecesFileLocation, SPECIES_MDL_V6 *pV6)
{
	UINT32 i;
	int j;
	DWORD bytes = 0;
	RESLT res;
	NRMLBEHMDL_VER6 *beh;
//	__int64 startPtr;
//	startPtr = (__int64)m_staticLib.MySetFilePointer(Hd, 0, FILE_CURRENT);

	m_staticLib.MySetFilePointer(Hd, SpecesFileLocation, FILE_BEGIN);

	if(0 == ReadFile(Hd, pV6, sizeof(SPECIES_MDL), &bytes, NULL))
		return FILEREAD_ERROR;

	if(OK != (res = m_speciesModel.ReadVector(Hd, &pV6->initialBehavior)))
		return res;

	pV6->p.behavior = NULL;

	//  each behavior to file
	//pV6->p.behavior = (SPECIES_MDL *)malloc(pV6->description.numBehaviors * sizeof(SPECIES_MDL));
	pV6->p.behavior = (NRMLBEHMDL_VER6 *)malloc(pV6->description.numBehaviors * sizeof(NRMLBEHMDL_VER6));
	if(pV6->p.behavior == NULL)
		return MEMALLOC_ERROR;

	for(i=0; i<pV6->description.numBehaviors; i++)
	{
		beh = &pV6->p.behavior[i];
		//memset(beh, 0, sizeof(SPECIES_MDL));

		// Read the behavior structure from file
		if(0 == ReadFile(Hd, beh, sizeof(NRMLBEHMDL_VER6), &bytes, NULL))
			return FILEWRITE_ERROR;

		beh->nrmlBehTrans.arr = (BEHTRAN *)malloc(beh->nrmlBehTransCnt * sizeof(BEHTRAN));

		for(j=0; j<beh->nrmlBehTransCnt; j++)
		{
			if(0 == ReadFile(Hd, &beh->nrmlBehTrans.arr[j], sizeof(BEHTRAN), &bytes, NULL))
				return FILEREAD_ERROR;

			if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->nrmlBehTrans.arr[j].m)))
				return res;
		}


		// Handle all the dynamically allocated memory contained in substructures of 
		// this structure
		// Normal behavior transition
		//if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->nrmlBehTransMatrix)))
		//	return res;

		// Depth environmenatal attractor behavior transition
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->depthEnvAttBehTrans.behavior)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->depthEnvAttBehTrans.terminate)))
			return res;

		// Depth temperature attractor behavior transition
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->tempEnvAttBehTrans.behavior)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->tempEnvAttBehTrans.terminate)))
			return res;

		// Travel Rate
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->travelRate.vm.step)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->travelRate.vm.terminate)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->travelRate.vm.vector)))
			return res;

		// Travel direction
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->travelDirection.vm.direction)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->travelDirection.vm.directionalBias)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->travelDirection.vm.terminate)))
			return res;

		// Descent rate
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.descentRate.vm.step)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.descentRate.vm.terminate)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.descentRate.vm.vector)))
			return res;

		// Ascent rate
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.ascentRate.vm.step)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.ascentRate.vm.terminate)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.ascentRate.vm.vector)))
			return res;

		// Surface Interval
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.srfInv.vm.step)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.srfInv.vm.vector)))
			return res;

		// Reversal
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.reversal.vm.count)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.reversal.vm.probOfReversal)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.reversal.vm.time)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.reversal.vm.timeStep)))
			return res;

		// Depth Model
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.depth.vm.step)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.depth.vm.vector)))
			return res;
	}
	return OK;
}

RESLT CSpeciesUpgrader::UpgradeV6(HANDLE Hd, __int64 SpecesFileLocation, SPECIES_MDL *pSpeMdl)
{
	RESLT res;
	SPECIES_MDL_V6 speMdl_v6 = {0};
	SPECIES_MDL_V7 speMdl_v7 = {0};

	// Read in model version 6
	if(OK != (res = ReadV6(Hd, SpecesFileLocation, &speMdl_v6)))
		return res;

	// Convert 6 to 7
	if(OK != (res = ConvertV6toV7(&speMdl_v7, &speMdl_v6)))
		return res;

	// Convert version 7 to verion 8
	if(OK != (res = ConvertV7toV8(pSpeMdl, &speMdl_v7)))
		return res;

	return OK;
}

RESLT CSpeciesUpgrader::ConvertV6toV7(SPECIES_MDL_V7 *pV7, SPECIES_MDL_V6 *pV6)
{
	UINT32 i;
	NRMLBEHMDL_VER6 *beh6;
	NRMLBEHMDL_VER7 *beh7;
	//----------------------------------------------------------------------------------//
	// Species Description
	//--------------------//
	// No changes between versions 6 and 7.
	memcpy(&pV7->description, &pV6->description, sizeof(SPECIESSPECIFICATION));
	//----------------------------------------------------------------------------------//

	//----------------------------------------------------------------------------------//
	// Normal Behaviors
	//--------------------//
	// Allocate space for each behavior for version 7.
	if(NULL == (pV7->p.behavior = (NRMLBEHMDL_VER7 *)malloc(pV7->description.numBehaviors * sizeof(NRMLBEHMDL_VER7))))
		return MEMALLOC_ERROR;
	memset(pV7->p.behavior, 0, pV7->description.numBehaviors * sizeof(NRMLBEHMDL_VER7));

	// Copy Version 7 behaviors to version 7.
	for(i=0; i<pV7->description.numBehaviors; i++)
	{
		beh7 = &pV7->p.behavior[i];
		beh6 = &pV6->p.behavior[i];
		memset(beh7, 0, sizeof(NRMLBEHMDL_VER7));
		
		// Copy the behavior name. (no change between versions)
		strncpy_s(beh7->szName, sizeof(beh7->szName), beh6->szName, strlen(beh6->szName));
		// Behavior transition count.  (no change between versions)
		beh7->nrmlBehTransCnt = beh6->nrmlBehTransCnt;

		//------------------------------------------------------------------------------//
		// Changed/Added for Version 7
		beh7->nrmlBehTermType = T50_K_TERM;
		//------------------------------------------------------------------------------//

		// Environmental attractor priority.  (no change between versions)
		beh7->depthHasPriorityOverTemp = beh6->depthHasPriorityOverTemp;

		beh7->nrmlBehTrans = beh6->nrmlBehTrans;

		// Environmental attractor transitions.  (no change between versions)
		beh7->depthEnvAttBehTrans = beh6->depthEnvAttBehTrans;
		beh7->tempEnvAttBehTrans = beh6->tempEnvAttBehTrans;

		// Environmental attractors.  (no change between versions)
		// Depth environmental attractor didn't start to be used until version 8 (unless
		// species version 8 gets released before I finish adding environmental attractors
		// which will cause it to be version 9).
		beh7->depthEnvAtt = beh6->depthEnvAtt; //replaced with lines that follow:
		beh7->tempEnvAtt = beh6->tempEnvAtt;

		// Travel Rate.  (no change between versions)
		beh7->travelRate = beh6->travelRate;

		// Travel direction.  (no change between versions)
		beh7->travelDirection = beh6->travelDirection;

		// Dive.  (no change between versions)
		beh7->dive = beh6->dive;
	}
	//----------------------------------------------------------------------------------//
	
	//----------------------------------------------------------------------------------//
	// Acoustic Aversion
	//------------------//
	// No changes between versions 4 and 5
	pV7->acousticAversion = pV6->acousticAversion;
	//----------------------------------------------------------------------------------//

	//----------------------------------------------------------------------------------//
	// Initial Behavior matrix
	//------------------------//
	// No changes between versions 4 and 5
	pV7->initialBehavior = pV6->initialBehavior;
	//m_speciesModel.CopyMatrix(&pV7->initialBehavior, &pV6->initialBehavior);
	//----------------------------------------------------------------------------------//
	return OK;
}

RESLT CSpeciesUpgrader::ReadV7(HANDLE Hd, __int64 SpecesFileLocation, SPECIES_MDL_V7* pV7)
{
	UINT32 i;
	int j;
	DWORD bytes = 0;
	DWORD address;
	RESLT res;
	//TCHAR szBuff[SIZE_16] = {0};
	NRMLBEHMDL_VER7 *beh;
//	__int64 startPtr;

	//startPtr = (__int64)m_staticLib.MySetFilePointer(Hd, 0, FILE_CURRENT);

	address = m_staticLib.MySetFilePointer(Hd, SpecesFileLocation, FILE_BEGIN);

	if(0 == ReadFile(Hd, pV7, sizeof(SPECIES_MDL_V7), &bytes, NULL))
		return FILEREAD_ERROR;

	if(OK != (res = m_speciesModel.ReadVector(Hd, &pV7->initialBehavior)))
		return res;

	pV7->p.behavior = NULL;

	//  each behavior to file
	//pV7->p.behavior = (SPECIES_MDL_V7 *)malloc(pV7->description.numBehaviors * sizeof(SPECIES_MDL_V7));
	pV7->p.behavior = (NRMLBEHMDL_VER7 *)malloc(pV7->description.numBehaviors * sizeof(NRMLBEHMDL_VER7));
	if(pV7->p.behavior == NULL)
		return MEMALLOC_ERROR;

	for(i=0; i<pV7->description.numBehaviors; i++)
	{
		beh = &pV7->p.behavior[i];
		//memset(beh, 0, sizeof(SPECIES_MDL_V7));

		// Read the behavior structure from file
		if(0 == ReadFile(Hd, beh, sizeof(NRMLBEHMDL_VER7), &bytes, NULL))
			return FILEWRITE_ERROR;

		beh->nrmlBehTrans.arr = (BEHTRAN *)malloc(beh->nrmlBehTransCnt * sizeof(BEHTRAN));

		for(j=0; j<beh->nrmlBehTransCnt; j++)
		{
			if(0 == ReadFile(Hd, &beh->nrmlBehTrans.arr[j], sizeof(BEHTRAN), &bytes, NULL))
				return FILEREAD_ERROR;

			if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->nrmlBehTrans.arr[j].m)))
				return res;
		}


		// Handle all the dynamically allocated memory contained in substructures of 
		// this structure
		// Normal behavior transition
		//if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->nrmlBehTransMatrix)))
		//	return res;

		// Depth environmenatal attractor behavior transition
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->depthEnvAttBehTrans.behavior)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->depthEnvAttBehTrans.terminate)))
			return res;

		// Depth temperature attractor behavior transition
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->tempEnvAttBehTrans.behavior)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->tempEnvAttBehTrans.terminate)))
			return res;

		// Travel Rate
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->travelRate.vm.step)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->travelRate.vm.terminate)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->travelRate.vm.vector)))
			return res;

		// Travel direction
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->travelDirection.vm.direction)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->travelDirection.vm.directionalBias)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->travelDirection.vm.terminate)))
			return res;

		// Descent rate
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.descentRate.vm.step)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.descentRate.vm.terminate)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.descentRate.vm.vector)))
			return res;

		// Ascent rate
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.ascentRate.vm.step)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.ascentRate.vm.terminate)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.ascentRate.vm.vector)))
			return res;

		// Surface Interval
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.srfInv.vm.step)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.srfInv.vm.vector)))
			return res;

		// Reversal
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.reversal.vm.count)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.reversal.vm.probOfReversal)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.reversal.vm.time)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.reversal.vm.timeStep)))
			return res;

		// Depth Model
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.depth.vm.step)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.depth.vm.vector)))
			return res;
	}
	return OK;
}

RESLT CSpeciesUpgrader::UpgradeV7(HANDLE Hd, __int64 SpecesFileLocation, SPECIES_MDL *pSpeMdl)
{
	RESLT res;
	SPECIES_MDL_V7 speMdl_v7 = {0};

	// Read in model version 7
	if(OK != (res = ReadV7(Hd, SpecesFileLocation, &speMdl_v7)))
		return res;

	// Convert version 7 to verion 8
	if(OK != (res = ConvertV7toV8(pSpeMdl, &speMdl_v7)))
		return res;

	return OK;
}


RESLT CSpeciesUpgrader::ConvertV7toV8(SPECIES_MDL *pV8, SPECIES_MDL_V7 *pV7)
{
	unsigned int i;
	memset(pV8, 0, sizeof(SPECIES_MDL));

	memcpy(&pV8->description, &pV7->description, sizeof(SPECIESSPECIFICATION));
	memcpy(&pV8->p, &pV7->p, sizeof(NRMLBEHMDL8BYTEPTR));
	memcpy(&pV8->acousticAversion, &pV7->acousticAversion, sizeof(ACSTCAVRSNMDL));

	pV8->initBehSpanCnt = 1;
	pV8->initialBehavior.arr = new BEHTRAN[1];
	pV8->initialBehavior.arr[0].depthSpan.deep = -3500;
	pV8->initialBehavior.arr[0].depthSpan.shallow = 0;
	pV8->initialBehavior.arr[0].m = pV7->initialBehavior;


	for(i=0; i<pV8->description.numBehaviors; i++)
	{
		pV8->p.behavior[i].depthEnvAtt.shelfIsEnabled = FALSE;
		pV8->p.behavior[i].depthEnvAtt.shelfDepth = -550.0;
		pV8->p.behavior[i].depthEnvAtt.shelfSlope = 1.0;

		pV8->p.behavior[i].depthEnvAtt.basinIsEnabled = FALSE;
		pV8->p.behavior[i].depthEnvAtt.basinDepth = -550.0;
		pV8->p.behavior[i].depthEnvAtt.basinSlope = 1.0;

		pV8->p.behavior[i].depthEnvAtt.slopeIsEnabled = FALSE;
		pV8->p.behavior[i].depthEnvAtt.slopeDepth = -150.0;
		pV8->p.behavior[i].depthEnvAtt.slopeSlope = 1.0;
	}
	return OK;
}


RESLT CSpeciesUpgrader::ReadV8(HANDLE Hd, __int64 SpecesFileLocation, SPECIES_MDL* pV8)
{
	UINT32 i;
	int j;
	DWORD bytes = 0;
	DWORD address;
	RESLT res;
	NRMLBEHMDL *beh;

	address = m_staticLib.MySetFilePointer(Hd, SpecesFileLocation, FILE_BEGIN);

	if(0 == ReadFile(Hd, pV8, sizeof(SPECIES_MDL), &bytes, NULL))
		return FILEREAD_ERROR;

	pV8->initialBehavior.arr = new BEHTRAN[pV8->initBehSpanCnt];
	for(j=0; j<pV8->initBehSpanCnt; j++)
	{
		if(0 == ReadFile(Hd, &pV8->initialBehavior.arr[j], sizeof(BEHTRAN), &bytes, NULL))
			return FILEREAD_ERROR;

		if(OK != (res = m_speciesModel.ReadVector(Hd, &pV8->initialBehavior.arr[j].m)))
			return res;
	}

	//if(OK != (res = m_speciesModel.ReadVector(Hd, &pV8->initialBehavior)))
	//	return res;

	pV8->p.behavior = NULL;

	//  each behavior to file
	//pV8->p.behavior = (SPECIES_MDL *)malloc(pV8->description.numBehaviors * sizeof(SPECIES_MDL));
	pV8->p.behavior = (NRMLBEHMDL *)malloc(pV8->description.numBehaviors * sizeof(NRMLBEHMDL));
	if(pV8->p.behavior == NULL)
		return MEMALLOC_ERROR;

	for(i=0; i<pV8->description.numBehaviors; i++)
	{
		beh = &pV8->p.behavior[i];
		//memset(beh, 0, sizeof(SPECIES_MDL));

		// Read the behavior structure from file
		if(0 == ReadFile(Hd, beh, sizeof(NRMLBEHMDL), &bytes, NULL))
			return FILEWRITE_ERROR;

		//------------------------------------------------------------------------------//
		// Upgrade version 8 models previous to version 8.5
		//-------------------------------------------------//
		// Species version 8.5 activated the depth environmental response, which replaced
		// the previoius placeholder struct ENVATTRACTORMDL with DEPTH_ENV_ATTRACTOR_MDL.
		// Both of these structs had the same storage space requirements which allows for
		// the following code:
		if(pV8->description.speVerSub < 5)
		{
			// Depth over temperature priority setting not really needed since temperature
			// environmental attractor isn't activated but it's nice to be explict
			// sometimes.
			beh->depthHasPriorityOverTemp = TRUE;
#pragma message("Come back here and set the enabled flags to FALSE after development")

			memset(&beh->depthEnvAtt, 0, sizeof(DEPTH_ENV_ATTRACTOR_MDL));
			beh->depthEnvAtt.shelfIsEnabled = FALSE;
			beh->depthEnvAtt.shelfDepth = -550.0;
			beh->depthEnvAtt.shelfSlope = 1.0;

			beh->depthEnvAtt.basinIsEnabled = FALSE;
			beh->depthEnvAtt.basinDepth = -550.0;
			beh->depthEnvAtt.basinSlope = 1.0;

			beh->depthEnvAtt.slopeIsEnabled = FALSE;
			beh->depthEnvAtt.slopeDepth = -150.0;
			beh->depthEnvAtt.slopeSlope = 1.0;
		}
		//------------------------------------------------------------------------------//

		beh->nrmlBehTrans.arr = (BEHTRAN *)malloc(beh->nrmlBehTransCnt * sizeof(BEHTRAN));

		for(j=0; j<beh->nrmlBehTransCnt; j++)
		{
			if(0 == ReadFile(Hd, &beh->nrmlBehTrans.arr[j], sizeof(BEHTRAN), &bytes, NULL))
				return FILEREAD_ERROR;

			if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->nrmlBehTrans.arr[j].m)))
				return res;
		}


		// Handle all the dynamically allocated memory contained in substructures of 
		// this structure
		// Normal behavior transition
		//if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->nrmlBehTransMatrix)))
		//	return res;

		// Depth environmenatal attractor behavior transition
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->depthEnvAttBehTrans.behavior)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->depthEnvAttBehTrans.terminate)))
			return res;

		// Depth temperature attractor behavior transition
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->tempEnvAttBehTrans.behavior)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->tempEnvAttBehTrans.terminate)))
			return res;

		// Travel Rate
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->travelRate.vm.step)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->travelRate.vm.terminate)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->travelRate.vm.vector)))
			return res;

		// Travel direction
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->travelDirection.vm.direction)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->travelDirection.vm.directionalBias)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->travelDirection.vm.terminate)))
			return res;

		// Descent rate
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.descentRate.vm.step)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.descentRate.vm.terminate)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.descentRate.vm.vector)))
			return res;

		// Ascent rate
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.ascentRate.vm.step)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.ascentRate.vm.terminate)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.ascentRate.vm.vector)))
			return res;

		// Surface Interval
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.srfInv.vm.step)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.srfInv.vm.vector)))
			return res;

		// Reversal
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.reversal.vm.count)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.reversal.vm.probOfReversal)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.reversal.vm.time)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.reversal.vm.timeStep)))
			return res;

		// Depth Model
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.depth.vm.step)))
			return res;
		if(OK != (res = m_speciesModel.ReadVector(Hd, &beh->dive.depth.vm.vector)))
			return res;
	}
	return OK;
}


// Saves the current version to file.
RESLT CSpeciesUpgrader::Save(const TCHAR *szFileName, SPECIES_MDL *pSpeMdl)
{
	TCHAR szBuff[SIZE_256]; // needed because call to m_speciesModel.SaveToBinFile()
						   // doesn't take the file name as a const.

	strncpy_s(szBuff, sizeof(szBuff), szFileName, strlen(szFileName)); 

	// The FALSE in the SaveToBinFile() function calls tells it to not update
	// the species's model file save date, time, and unique ID.
	return m_speciesModel.SaveToBinFile(szBuff, pSpeMdl, FALSE);
}