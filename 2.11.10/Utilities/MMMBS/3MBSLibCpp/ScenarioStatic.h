#ifndef SCEANRIOSTATIC_H
#define SCEANRIOSTATIC_H

#include "dataTypes.h"
#include "staticLib.h"
//#include "3mbsLib.h"

class CStaticScenario
{
public:
	CStaticScenario();
	virtual ~CStaticScenario();
private:
	C3mbStaticsLib m_staticLib;
public:

	double MetersBetweenCoords(double Lat1, double Lon1, double Lat2, double Lon2);
	COORD_DEPTH NewCoordFromOldPlusMeters(double Lat, double Lon, double LatitudeMetersY, double LongitudeMetersX);
	ANIMATASSCN_COMPACTINF CompctDecompctPodMembershipInf(UINT16 Inf);
	UINT16 CompctDecompctPodMembershipInf(ANIMATASSCN_COMPACTINF Inf);
	char *ActivityToString(SCEACTIVITY Activity, char *szBuffer, int BufferLenth);
	BUILDINF GetBuildInformation();
	void GetBuildDateTimeString(TCHAR *szDateBuffer, DWORD DateBuffSize, TCHAR *szTimeBuffer, DWORD TimeBuffSize);
	void SetAllBinOutFileConfiguration(BINARYOUTPUT *pConfig);
	void SetMinBinOutFileConfiguration(BINARYOUTPUT *pConfig);

};



#endif //SCEANRIOSTATIC_H