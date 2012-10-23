#pragma once
#include "environmentdata.h"
#include "listManager.h"
#include "staticLib.h"


class CBathymetry : public CEnvironmentData
{
public:
	CBathymetry(void);
	~CBathymetry(void);
private:
	C3mbStaticsLib m_staticLib;
public:

	RESLT LoadFromBinFile(HANDLE hd);
	RESLT LoadFromTextFile(TCHAR *szFileTitle, BOOL NegateMag);
	RESLT LoadFromTextFile(OPENFILENAME *Ofn, BOOL NegateMag);
	RESLT LoadFromTextFile(FILE *fd, BOOL NegateMag);
	void ClearData();

	RESLT CreateFakeData(double LatMin, double LatMax, double LonMin, double LonMax); 

	ENVDATAPOINTCOUNT GetDataPointCounts(BOOL GetSlope);
	RESLT GetRawDataCopy(RAWENVIRONMENTALDATA *RawEnv, BOOL Slope);
	double GetTotalSufaceAreaMeters();
	double GetLandSufaceAreaMeters();
	double GetWaterSurfaceAreaMeters();

	//----------------------------------------------------------------------------------//
	// Debugging and testing
	BATHYVALUE GetValueAtCoordinate(double Latitude, double Longitude, ENVDATA_INDEX *pLastVisitedSectorRef = NULL);
	BATHYEXTREMES GetExtremes();
	//----------------------------------------------------------------------------------//


private:
	ENVDATA_INDEX m_lastVisitedSector;
	CEnvironmentData m_slopeHeading;

	CListManager <CEnvironmentData> m_speciesSlopeList; // one for each species added.
};
