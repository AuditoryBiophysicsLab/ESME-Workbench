#include "ScenarioStatic.h"
#include "staticLib.h"
//--------------------------------------------------------------------------------------//
// Fomerly were statics
//--------------------------------------------------------------------------------------//
CStaticScenario::CStaticScenario()
{
}
CStaticScenario::~CStaticScenario(){}

double CStaticScenario::MetersBetweenCoords(double Lat1, double Lon1, double Lat2, double Lon2)
{
	return m_staticLib.MetersBetweenCoordinates(Lat1, Lon1, Lat2, Lon2);
}
COORD_DEPTH CStaticScenario::NewCoordFromOldPlusMeters(double Lat, double Lon, double LatitudeMetersY, double LongitudeMetersX)
{
	return m_staticLib.NewLatLonFromOldPlusMeters(Lat, Lon, LatitudeMetersY, LongitudeMetersX);
}

ANIMATASSCN_COMPACTINF CStaticScenario::CompctDecompctPodMembershipInf(UINT16 Inf)
{
	ANIMATASSCN_COMPACTINF c = {0};
	if(Inf & ANIMAT_RUNTIME_SPECIFCN_PODMEMSHIP){c.individual = TRUE;}
	if(Inf & ANIMAT_RUNTIME_SPECIFCN_PODLEADERTYPE){c.podLeaderType = CENTROID;}
	return c;
}
UINT16 CStaticScenario::CompctDecompctPodMembershipInf(ANIMATASSCN_COMPACTINF Inf)
{
	UINT16 c = 0;
	if(Inf.individual == TRUE){c |= ANIMAT_RUNTIME_SPECIFCN_PODMEMSHIP;}
	if(Inf.podLeaderType == CENTROID){c |= ANIMAT_RUNTIME_SPECIFCN_PODLEADERTYPE;}
	return c;
}

char *CStaticScenario::ActivityToString(SCEACTIVITY Activity, char *szBuffer, int BufferLen)
{
	_ASSERT(szBuffer != NULL && BufferLen > 0);
	if(szBuffer == NULL || BufferLen <= 0)
		return NULL;

	switch(Activity)
	{
	case __RUN_FINISHED: // no actiivty
		strncpy_s(szBuffer, BufferLen, "__RUN_FINISHED", BufferLen);
		break;
	case ___ALLOCOUTPUTBUFF: // allocating memory for animat output to file buffer
		strncpy_s(szBuffer, BufferLen, "___ALLOCOUTPUTBUFF", BufferLen);
		break;
	case ___SCE_INIT: // initializing scenaro
		strncpy_s(szBuffer, BufferLen, "___SCE_INIT", BufferLen);
		break;
	case ___SCE_INITANIMATS: // looping through each animat to initialize them
		strncpy_s(szBuffer, BufferLen, "___SCE_INITANIMATS", BufferLen);
		break;
	case ___SCE_RUNITERATING: // Iterating
		strncpy_s(szBuffer, BufferLen, "___SCE_RUNITERATING", BufferLen);
		break;
	case ___SCE_RUNBUFFERFLUSH: // flushing output buffer to file
		strncpy_s(szBuffer, BufferLen, "___SCE_RUNBUFFERFLUSH", BufferLen);
		break;
	case ___SCE_PAUSED: // running, but paused waiting on calling application input
		strncpy_s(szBuffer, BufferLen, "___SCE_PAUSED", BufferLen);
		break;
	}

	_ASSERT(0); // should never get here.
	return szBuffer;
}

BUILDINF CStaticScenario::GetBuildInformation()
{
	BUILDINF inf;
	size_t size = sizeof(&inf);
	memset(&inf, 0, sizeof(BUILDINF));
	GetBuildDateTimeString(inf.szBuildDate, sizeof(inf.szBuildDate), inf.szBuildTime, sizeof(inf.szBuildTime));

	inf.buildType = MBRELEASE;
	if(size == 4)
		inf.bitSize = 32;
	else if(size == 8)
		inf.bitSize = 64;

#ifdef _DEBUG
	inf.buildType = MBDEBUG;
#endif

	return inf;
}

void CStaticScenario::GetBuildDateTimeString(TCHAR *szDateBuffer, DWORD DateBuffSize, TCHAR *szTimeBuffer, DWORD TimeBuffSize)
{
	strncpy_s(szDateBuffer, DateBuffSize, __DATE__, DateBuffSize);
	strncpy_s(szTimeBuffer, TimeBuffSize, __TIME__, TimeBuffSize);
}


void CStaticScenario::SetAllBinOutFileConfiguration(BINARYOUTPUT *pConfig)
{
	pConfig->headerInf.bathyMap = TRUE;
	pConfig->headerInf.salinityMap = TRUE;
	pConfig->headerInf.temperatureMap = TRUE;
	pConfig->headerInf.postRunAnalysis = TRUE;
	pConfig->headerInf.speInfAndAnimatAsscn = TRUE;

	// Animat States Begin
	pConfig->animat.ID = TRUE;
	pConfig->animat.timeOfDay = TRUE;
	pConfig->animat.coordinate = TRUE;
	pConfig->animat.depth = TRUE;
	pConfig->animat.bearing = TRUE;
	pConfig->animat.diveRate = TRUE;
	pConfig->animat.travelRate = TRUE;
	pConfig->animat.aeCmltve = TRUE; // This become cumulative.
	pConfig->animat.aeMoment = TRUE;
	pConfig->animat.aeRelAngle = TRUE; // change this to acoustic source bearing or something
	pConfig->animat.aeTimeAvrt = TRUE;			// aversion cycle (~ 1/s) tally
	pConfig->animat.bathyDepth = TRUE;
	pConfig->animat.salinity = TRUE;
	pConfig->animat.temperature = TRUE;
	pConfig->animat.packedData = TRUE;
	pConfig->animat.targetDepth = TRUE;
	pConfig->animat.calcDepth = TRUE; //30
	pConfig->animat.xyDistance = TRUE; //41
	pConfig->animat.risk = TRUE; // 42

	// Acoustic Exposure State
	pConfig->AECoordinate = TRUE;
}

void CStaticScenario::SetMinBinOutFileConfiguration(BINARYOUTPUT *pConfig)
{
	// Does not alter structure members 'enabled' and 'outputByTime'.

	pConfig->headerInf.bathyMap = FALSE;
	pConfig->headerInf.salinityMap = FALSE;
	pConfig->headerInf.temperatureMap = FALSE;
	pConfig->headerInf.postRunAnalysis = FALSE;
	pConfig->headerInf.speInfAndAnimatAsscn = FALSE;

	// Animat States Begin
	pConfig->animat.ID = TRUE;
	pConfig->animat.timeOfDay = TRUE;
	pConfig->animat.coordinate = TRUE;
	pConfig->animat.depth = TRUE;
	pConfig->animat.bearing = FALSE;
	pConfig->animat.diveRate = FALSE;
	pConfig->animat.travelRate = FALSE;
	pConfig->animat.aeCmltve = FALSE; // This become cumulative.
	pConfig->animat.aeMoment = FALSE;
	pConfig->animat.aeRelAngle = FALSE; // change this to acoustic source bearing or something
	pConfig->animat.aeTimeAvrt = FALSE;			// aversion cycle (~ 1/s) tally
	pConfig->animat.bathyDepth = FALSE;
	pConfig->animat.salinity = FALSE;
	pConfig->animat.temperature = FALSE;
	pConfig->animat.packedData = TRUE;
	pConfig->animat.targetDepth = FALSE;
	pConfig->animat.calcDepth = FALSE; //30
	pConfig->animat.xyDistance = FALSE; //41
	pConfig->animat.risk = FALSE; // 42

	// Acoustic Exposure State
	pConfig->AECoordinate = FALSE;
}
