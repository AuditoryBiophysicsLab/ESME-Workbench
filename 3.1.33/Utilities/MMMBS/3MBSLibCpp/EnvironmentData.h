// EnvironmentData.h: interface for the CEnvironmentData class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_ENVIRONMENTDATA_H__C4F14EC9_9FBD_411F_BFDA_DCB7AC906755__INCLUDED_)
#define AFX_ENVIRONMENTDATA_H__C4F14EC9_9FBD_411F_BFDA_DCB7AC906755__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <windows.h>
#include <commdlg.h>
#include <stdio.h>
#include "datatypes.h"
#include "EnvironmentalDataStatic.h"
#include "staticLib.h"


class CEnvironmentData  
{
public:
	//-----------------------//
	// Constructor/Destructor
	//-----------------------//
	CEnvironmentData();
	virtual ~CEnvironmentData();

private:
	C3mbStaticsLib m_staticLib;
public:

	BOOL LoadScaleAndOutputTextFile(TCHAR *szInputFileTitle, TCHAR *szOutputFileTitle, double ScaleValue);
	ENVMINMAX GetExtremes();
	ENVDATAPOINTCOUNT GetDataPointCounts();
	BOOL CheckMetersPerCoord();

	void ClearData();

	//----------------------------------//
	// File Functions and File Variables
	//----------------------------------//
	RESLT LoadFromTextFile(TCHAR *szFileTitle, BOOL NegateMag);
	RESLT LoadFromTextFile(OPENFILENAME *Ofn, BOOL NegateMag);
	RESLT LoadFromTextFile(FILE *fd, BOOL NegateMag);
	RESLT LoadFromBinFile(HANDLE hd);
	RESLT SaveToBinFile(HANDLE hd, DWORD *pNumBytes);
	//Static RESLT SkipOverInBinFile(HANDLE hd, DWORD *pNumBytes);

	RESLT CreateFakeData(double LatMin, double LatMax, double LonMin, double LonMax); 


	//----------------------//
	// Data Access Functions
	//----------------------//
	double GetValueAtCoordinate(double X, double Y, ENVDATA_INDEX *Sector, double *SecondValue);
	double GetValueAtCoordinate(double X, double Y, double Z, ENVDATA_INDEX *Sector, double *SecondValue);

	double GetValueAtCoordinateSansExtrapolation(double X, double Y, ENVDATA_INDEX *Sector, double *SecondValue);
	double GetValueAtCoordinateSansExtrapolation(double X, double Y, double Z, ENVDATA_INDEX *Sector, double *SecondValue);

	double SetConstantValue(double Value);
	BOOL ConstantValueIsSet();

	//-----------------------------------//
	// Member Variable Accessor Functions
	//-----------------------------------//
	RESLT GetRawDataCopy(RAWENVIRONMENTALDATA *RawEnv);
	void DeallocateRawDataCopyMemory(RAWENVIRONMENTALDATA *RawEnv);
	int  GetNumDimensions();
	BOOL IsDataLoaded();
	void GetFileName(TCHAR *FileNameBuffer, int BufferLength);
	int CalculateStorageBytes(void);

	// Need to decide if/how this fits in.
	double CalculateUnitlessSurfaceAreaAtPlane(double Xi, double Yi, double Xf, double Yf, double Zplane);

	RAWENVIRONMENTALDATA GetSlopeHeadingFromCrossProduct(RESLT *pRes);
	RESLT NewEnvironmentFromData(RAWENVIRONMENTALDATA EnvData);

private:
	CEnvironmentalDataStatic m_EnvDataStatic;

	//----------------------------------------------------------------------------------//
	// These three originally public.  But never used and not sure what intent was so
	// moved to private until I look into them further.
	double GetXUnitlessResolution();
	double GetYUnitlessResolution();
	double xGetUnitlessUnitSquareResolution();
	//----------------------------------------------------------------------------------//


	//----------------------//
	// Data Access Functions
	//----------------------//
	int FindDimensionIndex(double Location, int StartSearchIndex, double *DataArr, int ArrayLen);
	int FindDimensionIndexSansInterpolation(double Location, double *DimensionArray, int ArrayLen);
	__int32	m_nXlen; // Dimension data 1
	__int32	m_nYlen; // Dimension data 2
	__int32	m_nZlen; // Dimension data 3
	__int32	m_nVlen; // Dimension data 4

	double *m_fX;	 // x-dimension
	double *m_fY;	 // y-dimension
	double *m_fZ;	 // z-dimension 
	double *m_fV1;	 // The value at coordinate (x,y,z)
	double *m_fV2;	 // The value at coordinate (x,y,z).  Not currently set up to read in dual V values from files.

	double  m_minX;  // Minimum x coordinate;
	double  m_maxX;  // Maximum x coordinate;

	double  m_minY;  // Minimum y coordinate;
	double  m_maxY;  // Maximum y coordinate;

	double  m_minZ;  // Minimum z coordinate;
	double  m_maxZ;  // Maximum z coordinate;

	double  m_minV1;  // Minimum environmental value;
	double  m_maxV1;  // Maximum environmental value;

	double  m_minV2;  // Minimum environmental value;
	double  m_maxV2;  // Maximum environmental value;

	double m_noDataLoadedValue; // Used when bathymetry not loaded.
	BOOL m_bathyConstantSet;  // indicates when no bathy loaded but a constant has been set.
	ENV_DIMENSION m_dimensionType;
	TCHAR m_szFileName[SIZE_128];

};
#endif // !defined(AFX_ENVIRONMENTDATA_H__C4F14EC9_9FBD_411F_BFDA_DCB7AC906755__INCLUDED_)
