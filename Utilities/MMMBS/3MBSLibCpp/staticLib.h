#ifndef STATICLIB_H
#define STATICLIB_H
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <Windows.h>
#include "RANDOM.H"
#include "dataTypes.h"

#ifndef _CRT_RAND_S
#define _CRT_RAND_S
#endif

#define TCHARBFLEN(X)(sizeof(X)/sizeof(TCHAR)) // Returns the length of a TCHAR buffer (not the size in bytes).


typedef struct _3DVector
{
	double x; // i
	double y; // j
	double z; // k
}_3DVECTOR;


typedef struct RawEnvironmentalData
{
	double *X;
	double *Y;
	double *Z;
	double *V1;
	double *V2;
	int Xlen;
	int Ylen;
	int Zlen;
	int Vlen;
}RAWENVIRONMENTALDATA;


enum ENV_DIMENSION
{
	ENVDAT_UNINITIALIZED = 0,	// This classes initial state before data is loaded
	ENVDAT_3D = 3,				// (lat, lon, value)
	ENVDAT_4D = 4,				// (lat, lon, depth, value)
	ENVDAT_3Dx2V = 5,				// (lat, lon, value1, value 2)
};


//--------------------------------------------------------------------------------------//
// struct ENVDATA_INDEX:
//	A structure to store the index of the sector (either in 2 or 3 dimensions) that the
//	animat's coordinates coorespond to in the environmental data.  This allows for faster
//	environmental value retreiveal because the function GetValueAtCoordinate() will first
//	check to see if the animit is still within the index values instead of search through
//	all indexes each time.

// struct members:
//	x: the last index data was retrieved from in the x-dimension 
//	y: the last index data was retrieved from in the y-dimension 
//	z: the last index data was retrieved from in the z-dimension 
//--------------------------------------------------------------//
typedef struct ENVDATA_INDEX
{
	int x;
	int y;
	int z;
}ENVDATA_INDEX;


typedef struct EnvironmentalDataPointCount
{
	__int32 x;
	__int32 y;
	__int32 z;
	__int32 v;
}ENVDATAPOINTCOUNT;


typedef struct ConsoleParamFileNameAndTitle
{
	TCHAR szFileName[SIZE_512];
	TCHAR szFileTitle[SIZE_256];
	BOOL set;
}CONSOLEPARAM_FILENAMEANDTITLE;

typedef struct ConsoleFileNameAndTitleParam
{
	TCHAR szDirectory[SIZE_512];
	BOOL set;
}CONSOLEPARAM_DIRECTORY;

typedef struct ConsoleInput
{
	CONSOLEPARAM_FILENAMEANDTITLE sce;
	CONSOLEPARAM_FILENAMEANDTITLE csv;
	CONSOLEPARAM_DIRECTORY outputDir;
	BOOL batch;
	BOOL abortBatchOnError;
	BOOL pingLimted;
	BOOL autoExtract;
	BOOL help;
	BOOL noSubDirectories;
	BOOL speedy;
}CONSOLEINPUT;

// Lat and Lon validity checks
#define INSIDE 0
#define OUTSIDE 1

enum BATHYCOMPARE
{
	DEEPER,
	SHALLOWER,
	EQUAL,
};

const _FINDEX_INFO_LEVELS gFXIL = FindExInfoStandard;
const _FINDEX_SEARCH_OPS gFXSO = FindExSearchNameMatch;

typedef struct
{
	WIN32_FIND_DATA data;
	HANDLE hdl;
	TCHAR sz[SIZE_256];
	//int cnt;
}FINDINF;


typedef struct GausianPDFCumulativeBuffStruct
{
	double *pCumulativeArray;
	double *pAnimatCntBin;
	int buffLen;
}GAUSS_CUMULATIVE_PDF_ARRAY;

const double INVERSEGAUSSIANPDF_RESOLUTION = 0.5;
const int INVERSEGAUSSIANPDF_MAX_STD_OUT = 10;


class C3mbStaticsLib
{
public:
	C3mbStaticsLib();
	~C3mbStaticsLib();


private:
	C3MBRandom m_3MBRandom;
	int InsidePolygon(COORD *polygon, int N, COORD p);

public:

	BOOL HasAnExtensionType(const TCHAR *szExtension, const TCHAR *szFileName);
	BOOL HasAnyExtension(const TCHAR *szFileName);

	//--------------------------------------------------------------------------------------//
	// Command prompt version functions
	//---------------------------------//
	void PrintScenarioExecutionFeedback(SCEACTIVITY Activity, int Animat, int Iteration, int TotalAnimats, int TotalDuration);
	void PrintExtractionFeedback(EXTRACTORACTIVITY Activity, int Animat, int Iteration, BOOL ByTime, int TotalAnimats, int TotalDuration);
	//--------------------------------------------------------------------------------------//

	double LinarInterpolation(double X1, double Y1, double X2, double Y2, double x);
	void DeallocateGausianPDFCumulativeBuffStruct(GAUSS_CUMULATIVE_PDF_ARRAY *pRefGaussCumPdfArray);
	double InverseGaussianPropabilityDistributionFunction(double Mean, double Std, double X);
	GAUSS_CUMULATIVE_PDF_ARRAY GenerateCumInvGaussPDF(double Mean, double Std);

	ENVMINMAX BathyExtremesToEnvExtemes(BATHYEXTREMES Bathy);
	BOOL MyFindFile(FINDINF *FindInf);
	RESLT MyDeleteAllFilesInDirectory(TCHAR *szDirectory, TCHAR *szFileType);
	RESLT IsFolderEmpty(TCHAR *szDirectory, BOOL *Result);
	BOOL FolderExists(TCHAR *szFolder);
	RESLT MyDeleteFolder(TCHAR *szFolder);

	double MyRound(double Val);

	TCHAR *MallocClearedBuffer(size_t Size);

	TCHAR *MemoryValueToString_bytes(DWORDLONG Value, TCHAR *szBuffer, DWORD BufferLen);
	TCHAR *MemoryValueToString_f(DWORDLONG Value, TCHAR *szBuffer, DWORD BufferLen);
	TCHAR *MemoryValueToString(DWORDLONG Value, TCHAR *szBuffer, DWORD BufferLen);

	int CalcAnimatQTYByDensity(double SurfaceArea_Meters, double AnimatPerSquareKilometer);

	DWORD MySetFilePointer(HANDLE Hdl, __int64 NumBytesToMove, DWORD MoveMethod);

	BOOL ClockTimeIsWithin(int ClockTime, double StartClockFractionHrs, double EndClockFractionHrs);

	TCHAR *GetLocalTimeAsHHMMSSString(TCHAR *szBuff, int BufferLength);
	TCHAR *GetLocalDateAsString(TCHAR *szBuff, int BufferLength);
	TCHAR *YesNoString(BOOL B, TCHAR *Buffer, int BufferLength);
	//TCHAR *ModelTypeToString(BEHAVIORAL_MODEL_TYPE M, TCHAR *Buffer, int BufferLength);
	TCHAR *ModelTypeToString(STANDARD_MODEL_TYPE M, TCHAR *Buffer, int BufferLength);
	TCHAR *ModelTypeToString(DIRECTIONAL_MODEL_TYPE M, TCHAR *Buffer, int BufferLength);

	double AddAngles(double AngleDeg1, double AngleDeg2);
	double AddAngles(double Weight1, double AngleDeg1, double Weight2, double AngleDeg2);

	double KeepWithin360(double Heading);
	HHMMSS Time_To24HrMinSec(int Seconds);
	HHMMSS Time_To24HrMinSec(HHMMSS HrMinSec);
	int Time_To24HrClockSeconds(int Seconds);
	int	Time_To24HrClockSeconds(HHMMSS HrMinSec);
	HHMMSS Time_ToHrMinSec(int Seconds);
	DDHHMMSS Time_ToDayHrMinSec(int Seconds);
	int	Time_ToSeconds(HHMMSS HrMinSec);
	void RemoveExtension(TCHAR *FileName);
	void GetPathAndFileTitleFromFileName(const TCHAR *szFileName, TCHAR *szFilePathBuffer, int PathBuffLen,
												TCHAR *szFileTitleBuffer, int TitleBuffLen);

	_3DVECTOR VectorCrossProduct(_3DVECTOR V1, _3DVECTOR V2);

	COORD_DEPTH	GetDefaultCoordinates();

	// These also determine distance.
	DISTANGL DetermineBearing(COORDINATE From, COORDINATE To);
	DISTANGL DetermineBearing(double lat1, double lon1, double lat2, double lon2);

	int GetExtension(const TCHAR *FileName, TCHAR *szBuffer, int BufferLength);

	//------------------//
	// Seeding Functions
	//------------------//
	//COORD_DEPTH NewLatLonFromAddingDistanceAndBearingToPrevious(COORD_DEPTH Coord, double DistanceMeters, double BearingRad);


	BATHYCOMPARE CompareBathyDepth(double CompareDepth, double ReferenceDepth);


	BOOL IsAValidSeedLatLon(double Lat, double Lon, double BathyDepth, ENVMINMAX BathyEx, double ShrFollwValue, double MinSeedDepth);
	int InsidePolygon(const COORDINATE *polygon, int N, double Lat, double Lon);
	BOOL CoordWithinCoordPolygonBoundaries(short X, short Y, COORD cArray[], int cArrayLen);
	BOOL LatLonWithinBoundaries(double Lat, double Lon, BATHYEXTREMES BathyEx);
	BOOL LatLonWithinBoundaries(double Lat, double Lon, ENVMINMAX BathyEx);
	BOOL BathyDepthAtLatLonValid(double BathyDepth, double ShoreFollowValue, double MinSeedDepth);

	double ContinueOtherSizeOfScreenValue(double V, double Min, double Max);
	COORDINATE ContinueOtherSideOfScreenLatLon(COORDINATE Current, BATHYEXTREMES BathyEx);

	// Lat and Lon calculation without validation
	COORDINATE RandomLatLon(BATHYEXTREMES BathyEx, C3MBRandom *p3MBRandomRef);
	COORDINATE RandomLatLon(ENVMINMAX BathyEx, C3MBRandom *p3MBRandomRef);
	COORD_DEPTH RandomLatLonAboutFocal(double Lat, double Lon, double AveDistMeters, double StdDevDistMeters, C3MBRandom *p3MBRandomRef);
	COORD_DEPTH NewLatLonFromOldPlusMeters(double Lat, double Lon, double LatitudeMetersY, double LongitudeMetersX);

	double MetersBetweenCoordinates(double Lat1, double Lon1, double Lat2, double Lon2);
	double GetTotalSufaceAreaMeters(ENVMINMAX Extremes);


	DWORD GetBit(DWORD Bit, DWORD _32bitVal);
	DWORD SetBit(DWORD Bit, DWORD _32bitVal, BOOL SET = TRUE);

	DWORD PackVersion(DWORD Super, DWORD Sub);
	void UnpackVersion(DWORD PackedVersion, DWORD *Super, DWORD *Sub);

	RESLT ConfigurationToTextFile(FILE *fd, USERPARAMS *Configuration);


	BOOL StringIsALegalNumber(TCHAR *szString);
	/*----------------------------------------------------------------------------------------
	  File I/O function prototypes
	/*--------------------------------------------------------------------------------------*/
	TCHAR *MbsResultToString(RESLT MbsResult, TCHAR *szMessageBuffer, int BufferLen,
							TCHAR *szCaptionBuffer = NULL, int CaptionBufferLength = 0);
};

#endif //STATICLIB_H