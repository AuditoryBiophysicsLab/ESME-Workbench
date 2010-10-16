#include "Bathymetry.h"
#include "3mbslib.h"

CBathymetry::CBathymetry(void)
{
	memset(&m_lastVisitedSector, 0, sizeof(m_lastVisitedSector));
}

CBathymetry::~CBathymetry(void)
{
}


void GenerateSpeciesStimulus(double StimulusAttenuation, RAWENVIRONMENTALDATA Sum, RAWENVIRONMENTALDATA EnvDat)
{
	// Quite compiler warnings until this is implemented.
	StimulusAttenuation = StimulusAttenuation;

	_ASSERT(Sum.Vlen == EnvDat.Vlen);
	_ASSERT(Sum.Xlen == EnvDat.Xlen);
	_ASSERT(Sum.Ylen == EnvDat.Ylen);
}


//--------------------------------------------------------------------------------------//
// Debugging and testing
//--------------------------------------------------------------------------------------//
BATHYVALUE CBathymetry::GetValueAtCoordinate(double Latitude, double Longitude, ENVDATA_INDEX *pLastVisitedSectorRef)
{
	BATHYVALUE bv = {0};

	if(pLastVisitedSectorRef == NULL)
		pLastVisitedSectorRef = &m_lastVisitedSector;
	// Note that slope heading is also retrieved in thsi function.  It is passed in as a
	// reference to the GetValueAtCoordinateSansExtrapolation() function.  Not the best
	// way to have implemented it...
	//bv.slope = m_slopeHeading.GetValueAtCoordinate(Latitude, Longitude, &m_lastVisitedSector, &bv.slopeHeading);
	bv.slope = m_slopeHeading.GetValueAtCoordinateSansExtrapolation(Latitude, Longitude, pLastVisitedSectorRef, &bv.slopeHeading);
	bv.depth = CEnvironmentData::GetValueAtCoordinate(Latitude, Longitude, &m_lastVisitedSector, NULL);
	return bv;
}

BATHYEXTREMES CBathymetry::GetExtremes()
{
	BATHYEXTREMES be;
	ENVMINMAX mmBath = CEnvironmentData::GetExtremes();
	ENVMINMAX mmSlHd = m_slopeHeading.GetExtremes();

	be.xMax = mmBath.xMax;
	be.xMin = mmBath.xMin;
	be.yMax = mmBath.yMax;
	be.yMin = mmBath.yMin;

	be.depthMax = mmBath.v1Max;
	be.depthMin = mmBath.v1Min;

	be.slopeMax = mmSlHd.v1Max;
	be.slopeMin = mmSlHd.v1Min;

	be.slopeHeadingMax = mmSlHd.v2Max;
	be.slopeHeadingMin = mmSlHd.v2Min;

	return be;
}

RESLT CBathymetry::GetRawDataCopy(RAWENVIRONMENTALDATA *RawEnv, BOOL Slope)
{
	if(Slope == false)
		return CEnvironmentData::GetRawDataCopy(RawEnv);
	return m_slopeHeading.GetRawDataCopy(RawEnv);
}

ENVDATAPOINTCOUNT CBathymetry::GetDataPointCounts(BOOL GetSlope)
{
	if(GetSlope == FALSE)
		return CEnvironmentData::GetDataPointCounts();

	return m_slopeHeading.GetDataPointCounts();
}

void CBathymetry::ClearData()
{
	CEnvironmentData::ClearData();
	m_slopeHeading.ClearData();
}

//--------------------------------------------------------------------------------------//


RESLT CBathymetry::LoadFromTextFile(TCHAR *szFileTitle, BOOL NegateMag)
{
	RESLT res;
	RAWENVIRONMENTALDATA slpHead;
	if(OK == (res = CEnvironmentData::LoadFromTextFile(szFileTitle, NegateMag)))
	{
		slpHead = GetSlopeHeadingFromCrossProduct(&res);

		// Do something, then delete slpHead dynamically allocated memory
		m_slopeHeading.NewEnvironmentFromData(slpHead);
	}
	return res;
}


RESLT CBathymetry::LoadFromTextFile(OPENFILENAME *Ofn, BOOL NegateMag)
{
	RESLT res;
	RAWENVIRONMENTALDATA slpHead;
	if(OK == (res = CEnvironmentData::LoadFromTextFile(Ofn, NegateMag)))
	{
		slpHead = GetSlopeHeadingFromCrossProduct(&res);
		// Do something, then delete slpHead dynamically allocated memory
		m_slopeHeading.NewEnvironmentFromData(slpHead);
	}
	return res;
}
RESLT CBathymetry::LoadFromTextFile(FILE *fd, BOOL NegateMag)
{
	RAWENVIRONMENTALDATA slpHead;
	RESLT res;
	if(OK == (res = CEnvironmentData::LoadFromTextFile(fd, NegateMag)))
	{
		slpHead = GetSlopeHeadingFromCrossProduct(&res);
		// Do something, then delete slpHead dynamically allocated memory
		m_slopeHeading.NewEnvironmentFromData(slpHead);
	}
	return res;
}

RESLT CBathymetry::CreateFakeData(double LatMin, double LatMax, double LonMin, double LonMax)
{
	RAWENVIRONMENTALDATA slpHead;
	RESLT res;
	if(OK == (res = CEnvironmentData::CreateFakeData(LatMin, LatMax, LonMin, LonMax)))
	{
		slpHead = GetSlopeHeadingFromCrossProduct(&res);
		// Do something, then delete slpHead dynamically allocated memory
		m_slopeHeading.NewEnvironmentFromData(slpHead);
	}
	return res;
}


RESLT CBathymetry::LoadFromBinFile(HANDLE hd)
{
	RESLT res;
	RAWENVIRONMENTALDATA slpHead;

	if(OK == (res = CEnvironmentData::LoadFromBinFile(hd)))
	{
		if(IsDataLoaded() == TRUE)
		{
			slpHead = GetSlopeHeadingFromCrossProduct(&res);
			// Do something, then delete slpHead dynamically allocated memory
			m_slopeHeading.NewEnvironmentFromData(slpHead);
		}
	}
	return res;
}

double CBathymetry::GetTotalSufaceAreaMeters()
{
	double b1, b2, h1;
	double latMin, lonMin, latMax, lonMax;

	ENVMINMAX Extremes = ((CEnvironmentData *)this)->GetExtremes();

	latMin = Extremes.xMin;
	lonMin = Extremes.yMin;
	latMax = Extremes.xMax;
	lonMax = Extremes.yMax;

	// Vary Lat, keep lon same. Should see a change in longitude distance returned except at small values for lat/lon.
	b1 = m_staticLib.MetersBetweenCoordinates(latMin, lonMin, latMin, lonMax); // returns distance in longitude at a specific latitude
	b2 = m_staticLib.MetersBetweenCoordinates(latMax, lonMin, latMax, lonMax); // returns distance in longitude at a specific latitude

	// Vary Lon, keep lat same. Shouldn't see a change in latitude distance (returned).
	h1 = m_staticLib.MetersBetweenCoordinates(latMin, lonMin, latMax, lonMin); // returns distance in latitudue at a specific longitude
#ifdef _DEBUG
	double h2 = m_staticLib.MetersBetweenCoordinates(latMin, lonMax, latMax, lonMax); // returns distance in latitudue at a specific longitude
	//_ASSERT(b1 != b2); often true normally, but not always true when region is very small.
	_ASSERT(h1 == h2);
#endif
	return 0.5*(b1 + b2)*h1;
}

double CBathymetry::GetWaterSurfaceAreaMeters()
{
	return GetTotalSufaceAreaMeters() - GetLandSufaceAreaMeters();
}


double CBathymetry::GetLandSufaceAreaMeters()
{
	double totalSurfArea, surfaceAreaUnitless;
//	double latMin, lonMin, latMax, lonMax;
	ENVMINMAX ex = CEnvironmentData::GetExtremes();

//	latMin = ex.xMin;
//	lonMin = ex.yMin;
//	latMax = ex.xMax;
//	lonMax = ex.yMax;

	//totalSurfArea = GetTotalSufaceAreaMeters(((CEnvironmentData *)GetBathymetryClassRef()));
	totalSurfArea = GetTotalSufaceAreaMeters();
	surfaceAreaUnitless = CalculateUnitlessSurfaceAreaAtPlane(ex.xMin, ex.yMin, ex.xMax, ex.yMax, -2.0);
	totalSurfArea *= surfaceAreaUnitless;
	return totalSurfArea;
}