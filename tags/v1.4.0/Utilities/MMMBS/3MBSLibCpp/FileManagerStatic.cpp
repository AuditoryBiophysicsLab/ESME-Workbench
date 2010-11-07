#include "FileManagerStatic.h"
#include "staticLib.h"

CFileManagerStatic::CFileManagerStatic(){}
CFileManagerStatic::~CFileManagerStatic(){}

FM_MEMALLOCINF CFileManagerStatic::GetMemoryAllocationDetails(int NumAnimats,
														int NumSavedIterations,
														BINARYOUTPUT BinOut,
														DWORDLONG OtherAllocBytes)
{
	FM_MEMALLOCINF mem = {0};
	MEMORYSTATUSEX memuse;
	UINT64 numBytes;

	// Determine the amount of memory required for each animat state.
	mem.animatStateSize = CalculateConfigurationBytesPerAnimatState(BinOut);
	mem.acousticSrcStateSize = CalculateConfigurationBytesPerAcousticState(BinOut);

	/* Make a system call to determine memory available. */
	memuse.dwLength = sizeof(MEMORYSTATUSEX);
	GlobalMemoryStatusEx(&memuse);

	/* Set the physical memory available*/
	mem.memoryAvailablePhysical = memuse.ullAvailPhys;

	/* Determine the maximum memory mbs is willing to allocate. Set allocation to available memory, subtrack known
	quantities allocated elsewhere in the 3mb application, then scale down*/

	mem.memoryMaximumAllocPhysical = memuse.ullAvailPhys;
	mem.memoryMaximumAllocPhysical -= (DWORDLONG)((DWORDLONG)sizeof(ANIMATSTATE) * (DWORDLONG)NumAnimats);
	mem.memoryMaximumAllocPhysical -= OtherAllocBytes;
	mem.memoryMaximumAllocPhysical = (DWORDLONG)((double)memuse.ullAvailPhys*double(AVAIL_PHYSICAL_MEM_SCALE_FACTOR));

	if(mem.memoryMaximumAllocPhysical > MAXIMUM_PHYSICAL_MEMORY_ALLOC)
		mem.memoryMaximumAllocPhysical = MAXIMUM_PHYSICAL_MEMORY_ALLOC;

	/* Determine the size in bytes the buffer would have to be to hold each state of
	every animat and each state of all acoustic data.*/
	mem.bufferIterationCapacity = 0;
	if(BinOut.enabled == TRUE)
		mem.bufferIterationCapacity = NumSavedIterations;

	numBytes = (((UINT64)mem.animatStateSize * (UINT64)NumAnimats) + (UINT64)mem.acousticSrcStateSize) * (UINT64)mem.bufferIterationCapacity;
		
	

	/* While the bytes required to store each state for each iteration is greater than the
	maximum physical memory mbs is willing to allocate reduce the number of iterations
	storable in the buffer to a minimum of a single iteration(hense the magic number 1 in
	the while loop)*/
	while(numBytes > mem.memoryMaximumAllocPhysical && mem.bufferIterationCapacity > 1)
		numBytes = (((UINT64)mem.animatStateSize * (UINT64)NumAnimats) + (UINT64)mem.acousticSrcStateSize) * (UINT64)(--mem.bufferIterationCapacity);


	//---------------------------------------------------------------------------------------------------//
	// Debug region
//	mem.bufferIterationCapacity = 1;
//	mem.numBytes = ((mem.animatStateSize * NumAnimats) + mem.acousticSrcStateSize) * (mem.bufferIterationCapacity);
	//---------------------------------------------------------------------------------------------------//

	mem.numBytes = (size_t)numBytes;

	return mem;
}

_fSCENARIOPARAMS CFileManagerStatic::ConvertScenarioFormat(const SCENARIOPARAMS Sce, ENVDATA *pEnvData, CListManager <CSpecies> *pSpeciesList)
{
	_fSCENARIOPARAMS sce = {0};
	DWORD i;

	strncpy_s(sce.fileIdentifier, sizeof(sce.fileIdentifier), SZ_OUTPUTFILEID, strlen(SZ_OUTPUTFILEID));

	sce.libVerSuper = Sce.libVerSuper; //1
	sce.libVerSub = Sce.libVerSub; //2

	sce.speciesVerSuper = Sce.speciesVerSuper;// alpha 1
	sce.speciesVerSub = Sce.speciesVerSub; // alpha 2

	sce.outputVerSuper = Sce.outputVerSuper;// beta 1
	sce.outputVerSub = Sce.outputVerSub; // beta 2


	sce.numSpecies = Sce.numSpecies; //3
	sce.totalNumAnimats = Sce.totalNumAnimats; //4

	sce._fNumAcstSrcTypes = Sce.numAcstSrcTypes; // 43
	sce._fTotalNumAcstcSrcs = Sce.totalNumAcstcSrcs; // 44


	sce.duration = Sce.duration; //5 // The duration of the run.
	sce.saveIterationCnt = Sce.numSaveIterations; //6 // The saved subset of iteration states.
	sce.startTime = Sce.startTime; //7

	// 8 ~ 32
	{ //sce.binOutStateItemConfig begins
		sce.binOutStateItemConfig = 0;

		if(Sce.user.output.enabled == TRUE)
			sce.binOutStateItemConfig |= OUTCONFIG_ENABLED;  //8
		// Scenario parameters are always written to file.
		if(Sce.user.output.headerInf.bathyMap == TRUE)
			sce.binOutStateItemConfig |= OUTCONFIG_BATHYMAP;//9
		if(Sce.user.output.headerInf.salinityMap == TRUE)
			sce.binOutStateItemConfig |= OUTCONFIG_SALINITYMAP;//10
		if(Sce.user.output.headerInf.temperatureMap == TRUE)
			sce.binOutStateItemConfig |= OUTCONFIG_TEMPERATUREMAP;//11
		if(Sce.user.output.headerInf.postRunAnalysis == TRUE)
			sce.binOutStateItemConfig |= OUTCONFIG_POSTRUNANALYSIS;//12
		if(Sce.user.output.headerInf.speInfAndAnimatAsscn == TRUE)
			sce.binOutStateItemConfig |= OUTCONFIG_SPECSINFANIMATASSION;//13

		// Animat States Begin
		if(Sce.user.output.animat.ID == TRUE)
			sce.binOutStateItemConfig |= OUTCONFIG_ANIMATID; //14
		if(Sce.user.output.animat.timeOfDay == TRUE)
			sce.binOutStateItemConfig |= OUTCONFIG_TIMEOFDAY;//15
		if(Sce.user.output.animat.coordinate == TRUE)
			sce.binOutStateItemConfig |= OUTCONFIG_COORDINATE;//16
		if(Sce.user.output.animat.depth == TRUE)
			sce.binOutStateItemConfig |= OUTCONFIG_DEPTH;//17
		if(Sce.user.output.animat.bearing == TRUE)
			sce.binOutStateItemConfig |= OUTCONFIG_BEARING;//18
		if(Sce.user.output.animat.diveRate == TRUE)
			sce.binOutStateItemConfig |= OUTCONFIG_DIVERATE;//19
		if(Sce.user.output.animat.travelRate == TRUE)
			sce.binOutStateItemConfig |= OUTCONFIG_TRAVELRATE;//20
		if(Sce.user.output.animat.aeCmltve == TRUE)
			sce.binOutStateItemConfig |= OUTCONFIG_AECUMULATIVE; //21 // This become cumulative.
		if(Sce.user.output.animat.aeMoment == TRUE)
			sce.binOutStateItemConfig |= OUTCONFIG_aeMoment;//22
		if(Sce.user.output.animat.aeRelAngle == TRUE)
			sce.binOutStateItemConfig |= OUTCONFIG_AERELATIVEANGLE;//23 // change this to acoustic source bearing or something
		if(Sce.user.output.animat.aeTimeAvrt == TRUE)
			sce.binOutStateItemConfig |= OUTCONFIG_AETIMEAVERTING;//24	// aversion cycle (~ 1/s) tally
		if(Sce.user.output.animat.bathyDepth == TRUE)
			sce.binOutStateItemConfig |= OUTCONFIG_BATHYDEPTH; //25
		if(Sce.user.output.animat.salinity == TRUE)
			sce.binOutStateItemConfig |= OUTCONFIG_SALINITY; //26
		if(Sce.user.output.animat.temperature == TRUE)
			sce.binOutStateItemConfig |= OUTCONFIG_TEMPERATURE; //27
		if(Sce.user.output.animat.packedData == TRUE)
			sce.binOutStateItemConfig |= OUTCONFIG_PACKEDDATA; //28
		if(Sce.user.output.animat.targetDepth == TRUE)
			sce.binOutStateItemConfig |= OUTCONFIG_TARGETDEPTH; //29
		if(Sce.user.output.animat.calcDepth == TRUE)
			sce.binOutStateItemConfig |= OUTCONFIG_CALCULATEDDEPTH; //30
		if(Sce.user.output.animat.xyDistance == TRUE)
			sce.binOutStateItemConfig |= OUTCONFIG_XYDISTANCE; //41
		if(Sce.user.output.animat.risk == TRUE)
			sce.binOutStateItemConfig |= OUTCONFIG_RISK; //42

		// Acoustic Exposure State
		if(Sce.user.output.AECoordinate == TRUE)
			sce.binOutStateItemConfig |= OUTCONFIG_AECOORDINATE; //31

		if(Sce.user.output.outputByTime == TRUE)
			sce.binOutStateItemConfig |= OUTCONFIG_OUTPUTBYTIME; //32
	} // sce.binOutStateItemConfig ends


	sce.acstSrceLimitOutput = Sce.acousticPingCycleOutputLimit;

	sce.sizeof_size_t_dataType = sizeof(size_t); // OUTPUT FILE ONLY here for Matlab to know the size of a size_t,

	sce.numSceParamsSpecGroupStored = NUM_SPEGROUPS_ALLOC;  // OUTPUT FILE ONLY 1 byte

	sce.numSceParamsSpecGroupUsed = Sce.speciesGroupCnt; //33
	
	{//_fSCENARIOPARAMS.sceSetupBoolParams from SCENARIOPARAMS members begins
		sce.sceSetupBoolParams = 0;

		if(Sce.enableAcstSrcTracking == TRUE)
			sce.sceSetupBoolParams |= SCEBOOL_ENABLEESMETRACKING; // 35

		if(Sce.user.durationless == TRUE)
			sce.sceSetupBoolParams |= SCEBOOL_DURATIONLESS;// 36

		if(Sce.user.acousticAnimatActive == TRUE)
			sce.sceSetupBoolParams |= SCEBOOL_ACOUSTICANIMATACTIVE; // 37

		if(Sce.user.distCalcMethod == LAT_LON)
			sce.sceSetupBoolParams |= SCEBOOL_DISTSTANCECALCMETHOD; // 38

		if(Sce.user.seed.useCurrentTick == TRUE)
			sce.sceSetupBoolParams |= SCEBOOL_USECURRENTTICK; // 39
	}// _fSCENARIOPARAMS.sceSetupBoolParams from SCENARIOPARAMS members ends

	sce.seedValue = Sce.user.seed.value; // 40


	// Get the binary file setup that hold information about internal file pointers,
	// storage size of the various regions to be saved, and total size (in bytes) of the
	// file generated when the run is complete.
	sce.diskInf = DetermineBinarySetup(Sce, pEnvData, pSpeciesList);// OUTPUT FILE ONLY 1 byte

	for(i=0; i<sce.numSceParamsSpecGroupUsed; i++) // 34
	{
		memcpy(sce.group[i].szGroup, Sce.speciesGroup[i].szGroup, sizeof(Sce.speciesGroup[i].szGroup));
		sce.group[i].lvlAphys = (float)Sce.speciesGroup[i].lvlAphys;
		sce.group[i].lvlBphys = (float)Sce.speciesGroup[i].lvlBphys;
		sce.group[i].lvlBBeh_RiskA = (float)Sce.speciesGroup[i].lvlBBeh_RiskA;
		sce.group[i].deactThreshB = (float)Sce.speciesGroup[i].deactThreshB;
		sce.group[i].unitsPhys = Sce.speciesGroup[i].unitsPhys;
		sce.group[i].unitsBeh = Sce.speciesGroup[i].unitsBeh;
		sce.group[i].decayfncB = Sce.speciesGroup[i].decayfncB;
	}

	_ASSERT(sce.numSceParamsSpecGroupStored >= sce.numSceParamsSpecGroupUsed);
	return sce;
}

int CFileManagerStatic::BuffIndex(int AnimatNum, int NumAnimats, int StateNum, int TotalStates)
{
	int index = AnimatNum + NumAnimats * StateNum;
	_ASSERT(index < NumAnimats * TotalStates);
	if(index > (NumAnimats * TotalStates) - 1)
		index = (NumAnimats * TotalStates) - 1;
	return index;
}


BINARYOUTPUT CFileManagerStatic::TranslateBinFileOutConfiguration(DWORD Config)
{
	BINARYOUTPUT c = {0};

	// Scenario parameters are always written to file.
	if(Config & OUTCONFIG_ENABLED){c.enabled = TRUE;}
	if(Config & OUTCONFIG_BATHYMAP){c.headerInf.bathyMap = TRUE;}
	if(Config & OUTCONFIG_SALINITYMAP){c.headerInf.salinityMap = TRUE;}
	if(Config & OUTCONFIG_TEMPERATUREMAP){c.headerInf.temperatureMap = TRUE;}
	if(Config & OUTCONFIG_POSTRUNANALYSIS){c.headerInf.postRunAnalysis = TRUE;}
	if(Config & OUTCONFIG_SPECSINFANIMATASSION){c.headerInf.speInfAndAnimatAsscn = TRUE;}

	// Animat States Begin
	if(Config & OUTCONFIG_ANIMATID){c.animat.ID = TRUE;}
	if(Config & OUTCONFIG_TIMEOFDAY){c.animat.timeOfDay = TRUE;}
	if(Config & OUTCONFIG_COORDINATE){c.animat.coordinate = TRUE;}
	if(Config & OUTCONFIG_DEPTH){c.animat.depth = TRUE;}
	if(Config & OUTCONFIG_BEARING){c.animat.bearing = TRUE;}
	if(Config & OUTCONFIG_DIVERATE){c.animat.diveRate = TRUE;}
	if(Config & OUTCONFIG_TRAVELRATE){c.animat.travelRate = TRUE;}
	if(Config & OUTCONFIG_AECUMULATIVE){c.animat.aeCmltve = TRUE;} // This become cumulative.
	if(Config & OUTCONFIG_aeMoment){c.animat.aeMoment = TRUE;}
	if(Config & OUTCONFIG_AERELATIVEANGLE){c.animat.aeRelAngle = TRUE;} // change this to acoustic source bearing or something
	if(Config & OUTCONFIG_AETIMEAVERTING){c.animat.aeTimeAvrt = TRUE;}			// aversion cycle (~ 1/s) tally
	if(Config & OUTCONFIG_BATHYDEPTH){c.animat.bathyDepth = TRUE;}
	if(Config & OUTCONFIG_SALINITY){c.animat.salinity = TRUE;}
	if(Config & OUTCONFIG_TEMPERATURE){c.animat.temperature = TRUE;}
	if(Config & OUTCONFIG_PACKEDDATA){c.animat.packedData = TRUE;}

	// Animat packedData is always written to file.
	if(Config & OUTCONFIG_TARGETDEPTH){c.animat.targetDepth = TRUE;}
	if(Config & OUTCONFIG_CALCULATEDDEPTH){c.animat.calcDepth = TRUE;} //30
	if(Config & OUTCONFIG_XYDISTANCE){c.animat.xyDistance = TRUE;}; //41
	if(Config & OUTCONFIG_RISK){c.animat.risk = TRUE;}; //42
	// Acoustic Exposure State
	if(Config & OUTCONFIG_AECOORDINATE){c.AECoordinate = TRUE;}

	if(Config & OUTCONFIG_OUTPUTBYTIME){c.outputByTime = TRUE;}
	return c;
}

// This function converts the file version of the scenario set up (the _fSCENARIOPARAMS
// struct) into one better suitable for programming and accessing.
SCENARIOPARAMS CFileManagerStatic::ConvertScenarioFormat(const _fSCENARIOPARAMS Sce)
{
	SCENARIOPARAMS sce = {0};
	DWORD i;

	sce.libVerSuper = Sce.libVerSuper;// 1
	sce.libVerSub = Sce.libVerSub; //2

	sce.speciesVerSuper = Sce.speciesVerSuper;// alpha 1
	sce.speciesVerSub = Sce.speciesVerSub; // alpha 2

	sce.outputVerSuper = Sce.outputVerSuper;// beta 1
	sce.outputVerSub = Sce.outputVerSub; // beta 2

	sce.numSpecies = Sce.numSpecies; //3
	sce.totalNumAnimats = Sce.totalNumAnimats; //4

	sce.numAcstSrcTypes = Sce._fNumAcstSrcTypes; // 43
	sce.totalNumAcstcSrcs = Sce._fTotalNumAcstcSrcs; // 44

	sce.duration = Sce.duration; //5 // The duration of the run.
	sce.numSaveIterations = Sce.saveIterationCnt; //6 // The saved subset of iteration states.
	sce.startTime = Sce.startTime;  //7

	{ //sce.binOutStateItemConfig begins
		if(Sce.binOutStateItemConfig & OUTCONFIG_ENABLED)
			sce.user.output.enabled = TRUE; //8
		// Scenario parameters are always written to file.
		if(Sce.binOutStateItemConfig & OUTCONFIG_BATHYMAP)
			sce.user.output.headerInf.bathyMap = TRUE; //9
		if(Sce.binOutStateItemConfig & OUTCONFIG_SALINITYMAP)
			sce.user.output.headerInf.salinityMap = TRUE; //10
		if(Sce.binOutStateItemConfig & OUTCONFIG_TEMPERATUREMAP)
			sce.user.output.headerInf.temperatureMap = TRUE; //11
		if(Sce.binOutStateItemConfig & OUTCONFIG_POSTRUNANALYSIS)
			sce.user.output.headerInf.postRunAnalysis = TRUE; //12
		if(Sce.binOutStateItemConfig & OUTCONFIG_SPECSINFANIMATASSION)
		sce.user.output.headerInf.speInfAndAnimatAsscn = TRUE; //13

		// Animat States Begin
		if(Sce.binOutStateItemConfig & OUTCONFIG_ANIMATID)
			sce.user.output.animat.ID = TRUE; //14
		if(Sce.binOutStateItemConfig & OUTCONFIG_TIMEOFDAY)
			sce.user.output.animat.timeOfDay = TRUE; //15
		if(Sce.binOutStateItemConfig & OUTCONFIG_COORDINATE)
			sce.user.output.animat.coordinate = TRUE;//16
		if(Sce.binOutStateItemConfig & OUTCONFIG_DEPTH)
			sce.user.output.animat.depth = TRUE;//17
		if(Sce.binOutStateItemConfig & OUTCONFIG_BEARING)
			sce.user.output.animat.bearing = TRUE;//18
		if(Sce.binOutStateItemConfig & OUTCONFIG_DIVERATE)
			sce.user.output.animat.diveRate = TRUE;//19
		if(Sce.binOutStateItemConfig & OUTCONFIG_TRAVELRATE)
			sce.user.output.animat.travelRate = TRUE;//20
		if(Sce.binOutStateItemConfig & OUTCONFIG_AECUMULATIVE)
			sce.user.output.animat.aeCmltve = TRUE; //21 // This become cumulative.
		if(Sce.binOutStateItemConfig & OUTCONFIG_aeMoment)
			sce.user.output.animat.aeMoment = TRUE ;//22
		if(Sce.binOutStateItemConfig & OUTCONFIG_AERELATIVEANGLE)
			sce.user.output.animat.aeRelAngle = TRUE;//23 // change this to acoustic source bearing or something
		if(Sce.binOutStateItemConfig & OUTCONFIG_AETIMEAVERTING)
			sce.user.output.animat.aeTimeAvrt = TRUE;//24	// aversion cycle (~ 1/s) tally
		if(Sce.binOutStateItemConfig & OUTCONFIG_BATHYDEPTH)
			sce.user.output.animat.bathyDepth = TRUE;//25
		if(Sce.binOutStateItemConfig & OUTCONFIG_SALINITY)
			sce.user.output.animat.salinity = TRUE;//26
		if(Sce.binOutStateItemConfig & OUTCONFIG_TEMPERATURE)
			sce.user.output.animat.temperature = TRUE;//27
		if(Sce.binOutStateItemConfig & OUTCONFIG_PACKEDDATA)
			sce.user.output.animat.packedData = TRUE;//28
		if(Sce.binOutStateItemConfig & OUTCONFIG_TARGETDEPTH)
			sce.user.output.animat.targetDepth = TRUE;//29
		if(Sce.binOutStateItemConfig & OUTCONFIG_CALCULATEDDEPTH)
			sce.user.output.animat.calcDepth = TRUE;//30
		if(Sce.binOutStateItemConfig & OUTCONFIG_XYDISTANCE)
			sce.user.output.animat.xyDistance = TRUE;//41
		if(Sce.binOutStateItemConfig & OUTCONFIG_RISK)
			sce.user.output.animat.risk = TRUE;//42

		// Acoustic Exposure State
		if(Sce.binOutStateItemConfig & OUTCONFIG_AECOORDINATE)
			sce.user.output.AECoordinate = TRUE; //31

		if(Sce.binOutStateItemConfig & OUTCONFIG_OUTPUTBYTIME)
			sce.user.output.outputByTime = TRUE;//32

	}//sce.binOutStateItemConfig ends

	sce.speciesGroupCnt = Sce.numSceParamsSpecGroupUsed; // 33

	for(i=0; i<sce.speciesGroupCnt; i++) //34
	{
		memcpy(sce.speciesGroup[i].szGroup, Sce.group[i].szGroup, sizeof(sce.speciesGroup[i].szGroup));
		sce.speciesGroup[i].lvlAphys = Sce.group[i].lvlAphys;
		sce.speciesGroup[i].lvlBphys = Sce.group[i].lvlBphys;
		sce.speciesGroup[i].lvlBBeh_RiskA = Sce.group[i].lvlBBeh_RiskA;
		sce.speciesGroup[i].deactThreshB = Sce.group[i].deactThreshB;
		sce.speciesGroup[i].unitsPhys = Sce.group[i].unitsPhys;
		sce.speciesGroup[i].unitsBeh = Sce.group[i].unitsBeh;
		sce.speciesGroup[i].decayfncB = Sce.group[i].decayfncB;
	}

	{ //SCENARIOPARAMS members from _fSCENARIOPARAMS.sceSetupBoolParams begins
		if(Sce.sceSetupBoolParams & SCEBOOL_ENABLEESMETRACKING)
			sce.enableAcstSrcTracking = TRUE; // 35
		if(Sce.sceSetupBoolParams & SCEBOOL_DURATIONLESS)
			sce.user.durationless = TRUE; // 36
		if(Sce.sceSetupBoolParams & SCEBOOL_ACOUSTICANIMATACTIVE)
			sce.user.acousticAnimatActive = TRUE; // 37

		// 38
		sce.user.distCalcMethod = PLANAR_GEOMETRY;
		if((Sce.sceSetupBoolParams & SCEBOOL_DISTSTANCECALCMETHOD) == TRUE)
			sce.user.distCalcMethod = LAT_LON;

		if(Sce.sceSetupBoolParams & SCEBOOL_USECURRENTTICK)
			sce.user.seed.useCurrentTick = TRUE; // 39
	}//SCENARIOPARAMS members from _fSCENARIOPARAMS.sceSetupBoolParams begins

	sce.acousticPingCycleOutputLimit = Sce.acstSrceLimitOutput;

	sce.intrvlOutptLim.enabled = Sce._fIntrvlOutputLimitEnabled;
	sce.intrvlOutptLim.start = Sce._fIntrvlOutputLimitStart;
	sce.intrvlOutptLim.interval = Sce._fIntrvlOutputLimitValue;

	sce.user.seed.value = Sce.seedValue; //40

	return sce;
}

BINARYSETUP CFileManagerStatic::DetermineBinarySetup(SCENARIOPARAMS Sce, ENVDATA *pEnvData,  CListManager <CSpecies> *pSpeciesList)
{
	BINARYSETUP bs = {0};
	DWORD i;

	_ASSERT((DWORD)pSpeciesList->Length() == Sce.numSpecies);

	if(Sce.user.output.enabled == FALSE)
		return bs; // no binary setup because no binary output


	//----------------------------------------------------------------------------------//
	// Update storage and file location calculations
	//----------------------------------------------//
	bs.totalDiskSpace = 0;

	// Scenario Params: always ouputted if there is any binary output
	bs.fp.scenarioParams = 0; // At the begining of the file.
	bs.store.scenarioParams = sizeof(_fSCENARIOPARAMS);
	bs.totalDiskSpace += bs.store.scenarioParams;

	// Bathy Map Environmental Data
	if(Sce.user.output.headerInf.bathyMap == TRUE)
	{
		bs.fp.bathyMap = bs.totalDiskSpace;
		bs.store.bathyMap = pEnvData->bathymetry.CalculateStorageBytes();
		bs.totalDiskSpace += bs.store.bathyMap;
	}
	
	// Salinity Map Environmental Data
	if(Sce.user.output.headerInf.salinityMap == TRUE)
	{
		bs.fp.salinityMap = bs.totalDiskSpace;
		bs.store.salinityMap = pEnvData->salinity.CalculateStorageBytes();
		bs.totalDiskSpace += bs.store.salinityMap;
	}

	// Temperature Map Environmental Data
	if(Sce.user.output.headerInf.temperatureMap == TRUE)
	{
		bs.fp.temperatureMap = bs.totalDiskSpace;
		bs.store.temperatureMap = pEnvData->temperature.CalculateStorageBytes();
		bs.totalDiskSpace += bs.store.temperatureMap;
	}

	// Post Analysis
	if(Sce.user.output.headerInf.postRunAnalysis == TRUE)
	{
		bs.fp.postAnalysis = bs.totalDiskSpace;
		bs.store.postAnalysis = sizeof(TAKE) + sizeof(TAKESTATS)*Sce.numSpecies;
		bs.totalDiskSpace += bs.store.postAnalysis;
	}

	// Species Information and Animat Association
	if(Sce.user.output.headerInf.speInfAndAnimatAsscn == TRUE)
	{
		bs.fp.speciesDesc = bs.totalDiskSpace;
		bs.store.speciesDesc = sizeof(SPECIESBINOUTINF) * Sce.numSpecies; // Species description
		// Species associated behaviors with the species description
		for(i=0; i<Sce.numSpecies; i++)
			bs.store.speciesDesc += pSpeciesList->Get(i)->GetNumBehaviorsModeled() * sizeof(BEHAVIOR_NAME);
		bs.totalDiskSpace += bs.store.speciesDesc;

		// Animat association with each species.
		bs.fp.animatAssoc = bs.totalDiskSpace;
		bs.store.animatAssoc = 0;
		for(i=0; i<Sce.numSpecies; i++)
			bs.store.animatAssoc += pSpeciesList->Get(i)->GetTotalAnimatCount() * sizeof(ANIMATASSCN);
		bs.totalDiskSpace += bs.store.animatAssoc;
	}

	// Animat State Information.
	bs.fp.animatState = bs.totalDiskSpace;
	bs.store.animatState = CalculateConfigurationBytesPerAnimatState(Sce.user.output);
	bs.totalAnimatStateBytes = (DWORDLONG)bs.store.animatState * (DWORDLONG)Sce.numSaveIterations * (DWORDLONG)Sce.totalNumAnimats;
	bs.totalDiskSpace += bs.totalAnimatStateBytes;

	// Acoustic location
	bs.fp.aeState = bs.totalDiskSpace; // this file pointer value will be incorrect if output is by time.
	bs.store.aeState = CalculateConfigurationBytesPerAcousticState(Sce.user.output);
	bs.totalAEStateBytes = (DWORDLONG)Sce.numSaveIterations * (DWORDLONG)bs.store.aeState;
	bs.totalDiskSpace += bs.totalAEStateBytes;
	return bs;
}

size_t CFileManagerStatic::CalculateConfigurationBytesPerAcousticState(BINARYOUTPUT BinOut)
{
	if(BinOut.enabled == FALSE || BinOut.AECoordinate == FALSE)
		return 0;
	return sizeof(_ACST_SRC_STATE_FILE);
}


size_t CFileManagerStatic::CalculateConfigurationBytesPerAnimatState(BINARYOUTPUT BinOut)
{
	size_t bytes = 0;
	ANIMATSTATE_FILEOUT as = {0};

	as.aeCmltve = 0; // quiet compiler warning.
	if(BinOut.enabled == FALSE)
		return 0;
	if(BinOut.animat.ID == TRUE)
		bytes += sizeof(as.animatID);
	if(BinOut.animat.timeOfDay == TRUE)
		bytes += sizeof(as.clock);
	if(BinOut.animat.coordinate == TRUE)
		bytes += (sizeof(as.lat) + sizeof(as.lon));
	if(BinOut.animat.depth == TRUE)
		bytes += sizeof(as.depth);
	if(BinOut.animat.bearing == TRUE)
		bytes += sizeof(as.bearing);
	if(BinOut.animat.diveRate == TRUE)
		bytes += sizeof(as.diveRate);
	if(BinOut.animat.travelRate == TRUE)
		bytes += sizeof(as.travelRate);
	if(BinOut.animat.aeCmltve == TRUE)
		bytes += sizeof(as.aeCmltve);
	if(BinOut.animat.aeMoment == TRUE)
		bytes += sizeof(as.aeMoment);
	if(BinOut.animat.aeRelAngle == TRUE)
		bytes += sizeof(as.aeRelAngle);
	if(BinOut.animat.aeTimeAvrt == TRUE)
		bytes += sizeof(as.aeTimeAvrt);
	if(BinOut.animat.bathyDepth == TRUE)
		bytes += sizeof(as.bathyDepth);
	if(BinOut.animat.salinity == TRUE)
		bytes += sizeof(as.salinity);
	if(BinOut.animat.temperature == TRUE)
		bytes += sizeof(as.temperature);

	if(BinOut.animat.packedData == TRUE)
		bytes += sizeof(as.packedData);

	if(BinOut.animat.targetDepth == TRUE)
		bytes += sizeof(as.targetDepth);
	if(BinOut.animat.calcDepth == TRUE) //30
		bytes += sizeof(as.calcDepth);
	if(BinOut.animat.xyDistance == TRUE) //41
		bytes += (sizeof(as.xDistance) + sizeof(as.yDistance));
	if(BinOut.animat.risk == TRUE) //42
		bytes += sizeof(as.risk);

	return bytes;
}

BOOL CFileManagerStatic::AssertBinaryOutFilePointers(HANDLE Hdl)
{
	DWORD readBytes = 0;
	DWORD bytes;
	DWORD totalReadBytes = 0;
	DWORD i;
	__int64 stateBytes;
	DWORD dwrd;
	size_t sizet;
	TAKE statAnalysis;
	SPECIESBINOUTINF speInf;
	TCHAR fileID[SIZE_16];
	CEnvironmentalDataStatic envDataStatic;
	

	_fSCENARIOPARAMS sceParams;
	BINARYOUTPUT bin;


	if(INVALID_SET_FILE_POINTER == m_staticLib.MySetFilePointer(Hdl, 0, FILE_BEGIN))
		return FALSE;

	// Check file ID.
	// Verify the first 16 bytes are the file identifier
	if(ReadFile(Hdl, fileID, sizeof(fileID), &bytes, NULL) == FALSE || bytes != sizeof(fileID))
		return FALSE;

	if(strcmp(fileID, SZ_OUTPUTFILEID) != 0)
		return FALSE;

	if(INVALID_SET_FILE_POINTER == m_staticLib.MySetFilePointer(Hdl, 0, FILE_BEGIN))
		return FALSE;

	// Scenario Params
	if(0 == ReadFile(Hdl, &sceParams, sizeof(_fSCENARIOPARAMS), &readBytes, NULL))
		return FALSE;
	totalReadBytes += readBytes;

	// STORAGE BYTES ASSERTION: Assert that calculated storage space for scenario
	// params is equivelent to the readBytes written for it.
	_ASSERT(sceParams.diskInf.store.scenarioParams == readBytes);
	if(sceParams.diskInf.store.scenarioParams != readBytes)
		return FALSE;

	bin = TranslateBinFileOutConfiguration(sceParams.binOutStateItemConfig);
	//----------------------------------------------------------------------------------//
	// Bathymetry map
	//---------------//
	if(bin.headerInf.bathyMap == TRUE)
	{
		// FILE POINTER ASSERTION: Assert that calculated file pointer location for
		// for the bathy map is equivelent to the total readBytes written so far
		_ASSERT(sceParams.diskInf.fp.bathyMap == totalReadBytes);
		if(sceParams.diskInf.fp.bathyMap != totalReadBytes)
			return FALSE;

		if(OK != envDataStatic.SkipOverInBinFile(Hdl, &readBytes))
			return FALSE;
		totalReadBytes += readBytes;

		// STORAGE BYTES ASSERTION: Assert that calculated storage space for bathy map
		// is equivelent to the readBytes written for it.
		_ASSERT(sceParams.diskInf.store.bathyMap == readBytes);
		if(sceParams.diskInf.store.bathyMap != readBytes)
			return FALSE;	
	}

	//----------------------------------------------------------------------------------//
	// Salinity map
	//---------------//
	if(bin.headerInf.salinityMap == TRUE)
	{
		// FILE POINTER ASSERTION: Assert that calculated file pointer location for
		// for the bathy map is equivelent to the total readBytes written so far
		_ASSERT(sceParams.diskInf.fp.salinityMap == totalReadBytes);
		if(sceParams.diskInf.fp.salinityMap != totalReadBytes)
			return FALSE;

		if(OK != envDataStatic.SkipOverInBinFile(Hdl, &readBytes))
			return FALSE;
		totalReadBytes += readBytes;

		// STORAGE BYTES ASSERTION: Assert that calculated storage space for bathy map
		// is equivelent to the readBytes written for it.
		_ASSERT(sceParams.diskInf.store.salinityMap == readBytes);
		if(sceParams.diskInf.store.salinityMap != readBytes)
			return FALSE;
	}

	//----------------------------------------------------------------------------------//
	// Temperature map
	//---------------//
	if(bin.headerInf.temperatureMap == TRUE)
	{
		// FILE POINTER ASSERTION: Assert that calculated file pointer location for
		// for the bathy map is equivelent to the total readBytes written so far
		_ASSERT(sceParams.diskInf.fp.temperatureMap == totalReadBytes);
		if(sceParams.diskInf.fp.temperatureMap != totalReadBytes)
			return FALSE;

		if(OK != envDataStatic.SkipOverInBinFile(Hdl, &readBytes))
			return FALSE;
		totalReadBytes += readBytes;

		// STORAGE BYTES ASSERTION: Assert that calculated storage space for bathy map
		// is equivelent to the readBytes written for it.
		_ASSERT(sceParams.diskInf.store.temperatureMap == readBytes);
		if(sceParams.diskInf.store.temperatureMap != readBytes)
			return FALSE;
	}

	//----------------------------------------------------------------------------------//
	// Statistical data
	//-----------------//
	if(bin.headerInf.postRunAnalysis == TRUE)
	{
		// FILE POINTER ASSERTION: Assert that calculated file pointer location for
		// for the post analysis is equivelent to the total readBytes read so far
		_ASSERT(sceParams.diskInf.fp.postAnalysis == totalReadBytes);
		if(sceParams.diskInf.fp.postAnalysis != totalReadBytes)
			return FALSE;

		//WritePostRunStatsResults(hd, &sceParams, &readBytes);
		ReadFile(Hdl, &statAnalysis, sizeof(TAKE), &readBytes, NULL);
		dwrd = m_staticLib.MySetFilePointer(Hdl, sizeof(TAKESTATS)*sceParams.numSpecies, FILE_CURRENT);
		if(INVALID_SET_FILE_POINTER == dwrd && GetLastError() != NO_ERROR)
		{
			_ASSERT(FALSE);
			return FALSE;
		}
		dwrd = readBytes + sizeof(TAKESTATS)*sceParams.numSpecies;
		totalReadBytes += dwrd;

		// STORAGE BYTES ASSERTION: Assert that calculated storage space for post
		// analysis stats region is equivelent to the readBytes written for it.
		sizet = (size_t)dwrd;
		if(sceParams.diskInf.store.postAnalysis != sizet)
		{
			_ASSERT(FALSE);
			return FALSE;
		}
	}

	//----------------------------------------------------------------------------------//
	// Animat to species association
	//------------------------------//
	if(bin.headerInf.speInfAndAnimatAsscn == TRUE)
	{
		// FILE POINTER ASSERTION: Assert that calculated file pointer location for
		// for the species description is equivelent to the total readBytes read.
		_ASSERT(sceParams.diskInf.fp.speciesDesc == totalReadBytes);
		for(i=0; (i < sceParams.numSpecies); i++)
		{
			if(FALSE == ReadFile(Hdl, &speInf, sizeof(SPECIESBINOUTINF), &readBytes, NULL))
				return FALSE;
			totalReadBytes += readBytes;

			// Read the names.
			m_staticLib.MySetFilePointer(Hdl, speInf.description.numBehaviors * sizeof(NAMES), FILE_CURRENT);
			totalReadBytes += speInf.description.numBehaviors * sizeof(NAMES);
		}

		// Animat Associations
		dwrd = m_staticLib.MySetFilePointer(Hdl, sceParams.totalNumAnimats * sizeof(ANIMATASSCN), FILE_CURRENT);
		if(INVALID_SET_FILE_POINTER == dwrd && GetLastError() != NO_ERROR)
		{
			_ASSERT(FALSE);
			return FALSE;
		}

		totalReadBytes += sceParams.totalNumAnimats * sizeof(ANIMATASSCN);

		// STORAGE BYTES ASSERTION: Assert that calculated storage space for the
		// animat association region is equivelent to the readBytes read in for it.
		sizet = (size_t)((DWORDLONG)totalReadBytes - sceParams.diskInf.fp.animatAssoc);
		_ASSERT(sceParams.diskInf.store.animatAssoc == sizet);
	}

	stateBytes = sceParams.saveIterationCnt *
				 sceParams.totalNumAnimats *
				 sceParams.diskInf.store.animatState;
	
	dwrd = m_staticLib.MySetFilePointer(Hdl, stateBytes, FILE_CURRENT);
	if(INVALID_SET_FILE_POINTER == dwrd && GetLastError() != NO_ERROR)
	{
		_ASSERT(FALSE);
		return FALSE;
	}

	// Need to add acoustic exposure state check here.
	return TRUE;
}


__int64 CFileManagerStatic::CalculateAnimatStateBufferLocation(CALCLOCINBUFF CLB/*int AnimatIndex, DWORD TotalAnimats, BOOL OutputByTime*/)
{
	__int64 bp = 0; // Byte position
	//DWORDLONG byAnimatMulitplier;

	if(CLB.outputByTime == TRUE) // format by iteration (new method requested by NUWC)
	{
		bp = (((CLB.iterationWriteNum * CLB.numAnimats) + CLB.animatIndex) * CLB.animatStateSize)
			+ (CLB.iterationWriteNum * CLB.acstSrcStateSize);
	}
	else // format by animat (original method)
	{
		// This is the original way of formating the file by animat.
		bp = ((CLB.bufferIterationLen * CLB.animatIndex) + CLB.iterationWriteNum) * CLB.animatStateSize;
		bp = bp;

#if 0
		// byAnimatMulitplier is the lesser of the number of iterations to be saved and the
		// buffer's iteration length.
		byAnimatMulitplier = CLB.numSaveIterations;
		if(byAnimatMulitplier > CLB.bufferIterationLen)
			byAnimatMulitplier = CLB.bufferIterationLen;

		bp = ((byAnimatMulitplier * CLB.animatIndex) + CLB.iterationWriteNum) * CLB.animatStateSize;
#endif

#if 0
		// This is the original way of formating the file by animat.
		bp = ((CLB.bufferIterationLen * CLB.animatIndex) + CLB.iterationWriteNum) * CLB.animatStateSize;
#endif
#if 0
		if(CLB.animatIndex == 0)
		{
			prevbp = bp;
		}
		else
		{

			//_ASSERT(bp == prevbp + 84*CLB.bufferIterationLen);
			if(bp != prevbp + 84*CLB.bufferIterationLen)
			{
				//TCHAR tempBuff[SIZE_128];
				//sprintf_s(tempBuff, sizeof(tempBuff), "animatIndex: %d, iteration: %d", CLB.animatIndex, CLB.iterationWriteNum);
				//MessageBox(NULL, tempBuff, "errored", 0);
				ExitProcess(8);
			}
			prevbp += 84*CLB.bufferIterationLen;
		}
#endif
	}
	return bp;
}


__int64 CFileManagerStatic::CalculateAcousticStateBufferLocation(CALCLOCINBUFF CLB)
{
	__int64 bytePos = 0;

	if(CLB.outputByTime == TRUE)
	{
		// Output by time.
		bytePos = (CLB.iterationWriteNum + 1) * CLB.numAnimats; // the +1 is based on animat states being written before acoustic states
		bytePos *= CLB.animatStateSize;
		bytePos += CLB.iterationWriteNum * CLB.acstSrcStateSize;
	}
	else
	{
		// Output by animat (the original binary out format)
		bytePos = (CLB.bufferIterationLen * CLB.numAnimats) * CLB.animatStateSize + 
			CLB.iterationWriteNum * CLB.acstSrcStateSize;
	}
	return bytePos;
}

