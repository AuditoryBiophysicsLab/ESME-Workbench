// EnvironmentData.cpp: implementation of the CEnvironmentData class.
//
//////////////////////////////////////////////////////////////////////

#include "EnvironmentData.h"
#include <math.h>
#include "random.h"

// Make sure "3mbslib.h" is included here rather than in 
// "EnvironmentData.h"
#include "3mbslib.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////
CEnvironmentData::CEnvironmentData()
{
	m_nXlen = m_nYlen = m_nZlen = m_nVlen = 0;
	m_fX = m_fY = m_fZ = m_fV1 = m_fV2 = NULL;
	m_dimensionType = ENVDAT_UNINITIALIZED;
	memset(m_szFileName, 0, sizeof(m_szFileName));
	m_noDataLoadedValue = 0;
	m_bathyConstantSet = FALSE;

	m_minX = m_maxX = m_minY = m_maxY = m_minZ = m_maxZ = m_minV1 = m_maxV1 = m_minV2 = m_maxV2 = 0;
}

CEnvironmentData::~CEnvironmentData()
{
	ClearData();
}

//------------------------------------------------------------------------------------------//
//******************************************************************************************//
// New Stuff In progress
// Calling routine must deallocate memory.
RAWENVIRONMENTALDATA CEnvironmentData::GetSlopeHeadingFromCrossProduct(RESLT *pRes)
{
	int x, y;
	double theta, rho, slope, heading;
	RAWENVIRONMENTALDATA slpHead;
	*pRes = OK;

	memset(&slpHead, 0, sizeof(RAWENVIRONMENTALDATA));

	// Must be 3D data with only a single value dimension.
	_ASSERT(m_dimensionType == ENVDAT_3D);
	_ASSERT(m_fV2 == NULL);
	if(m_dimensionType != ENVDAT_3D)
		return slpHead; // will be zeroed out which isn't an error.

	// Dimensions of the slopeheading are less than the bathymetry data by 1 in each
	// dimension.
	slpHead.Xlen = m_nXlen-1;
	slpHead.Ylen = m_nYlen-1;
	slpHead.Vlen = slpHead.Xlen * slpHead.Ylen;

	slpHead.X = new double[slpHead.Xlen];
	slpHead.Y = new double[slpHead.Ylen];
	slpHead.V1 = new double[slpHead.Vlen];
	slpHead.V2 = new double[slpHead.Vlen];

	if(slpHead.X == NULL || slpHead.Y == NULL || slpHead.V1 == NULL || slpHead.V2 == NULL)
		*pRes = MEMALLOC_ERROR;

	COORD_DEPTH ptNW = {0}; // Northwest point/corner
	COORD_DEPTH ptSW = {0}; // Southwest point/corner
	COORD_DEPTH ptSE = {0}; // Southeast point/corner
	COORD_DEPTH ptNE = {0}; // Northeast point/corner

	// Postive Distances
	double dSW_NW; // Distance from the southwest corner to the northwest corner
	double dSW_SE; // Distance from the southwest corner to the southeast corner
	double dSE_NE; // Distance from the southeast corner to the northeast corner
	double dNW_NE; // Distance from the northwest corner to the northeast corner

	_3DVECTOR vNW_SW = {0};
	_3DVECTOR vNW_SE = {0};
	_3DVECTOR v1 = {0};

	_3DVECTOR vSW_SE = {0};
	_3DVECTOR vSW_NE = {0};
	_3DVECTOR v2 = {0};

	_3DVECTOR vSE_NE = {0};
	_3DVECTOR vSE_NW = {0};
	_3DVECTOR v3 = {0};

	_3DVECTOR vNE_NW = {0};
	_3DVECTOR vNE_SW = {0};
	_3DVECTOR v4 = {0};

	_3DVECTOR sum = {0};


	// X is latitude, y is longitude
	// Starts in lower left and left to right going upward

	for(x=0; x<m_nXlen-1 && *pRes == OK; x++)
	{
		for(y=0; y<m_nYlen-1 && *pRes == OK; y++)
		{
			_ASSERT(x+1 < m_nXlen);
			_ASSERT(y+1 < m_nYlen);
			_ASSERT((x+1)*m_nYlen + (y+1) < m_nVlen);

			if(x+1 >= m_nXlen || y+1 >= m_nYlen || (x+1)*m_nYlen + (y+1) >= m_nVlen)
			{
				 *pRes = INVALID_GENERAL_INDEX_ERROR;
				 break;
			}


			_ASSERT(x < slpHead.Xlen);
			_ASSERT(y < slpHead.Ylen);
			_ASSERT(x*slpHead.Ylen + y < slpHead.Vlen);

			if(x >= slpHead.Xlen || y >= slpHead.Ylen || x*slpHead.Ylen + y >= slpHead.Vlen)
			{
				 *pRes = INVALID_GENERAL_INDEX_ERROR;
				 break;
			}


			// Set up the points used for setting up distances.
			ptNW.lat = m_fX[x+1];
			ptNW.lon = m_fY[y];
			ptNW.depth = m_fV1[(x+1)*m_nYlen + (y)];

			ptSW.lat = m_fX[x];
			ptSW.lon = m_fY[y];
			ptSW.depth = m_fV1[(x)*m_nYlen + (y)];

			ptSE.lat = m_fX[x];
			ptSE.lon = m_fY[y+1];
			ptSE.depth = m_fV1[(x)*m_nYlen + (y+1)];

			ptNE.lat = m_fX[x+1];
			ptNE.lon = m_fY[y+1];
			ptNE.depth = m_fV1[(x+1)*m_nYlen + (y+1)];

			//--------------------------------------------------------------------------//
			// Set up distances (in meters) that will be used for seting up the vectors.
			//-------------------------------------------------------------------------//
			dSW_NW = m_staticLib.MetersBetweenCoordinates(ptSW.lat, ptSW.lon, ptNW.lat, ptNW.lon);
			dSW_SE = m_staticLib.MetersBetweenCoordinates(ptSW.lat, ptSW.lon, ptSE.lat, ptSE.lon);
			dSE_NE = m_staticLib.MetersBetweenCoordinates(ptSE.lat, ptSE.lon, ptNE.lat, ptNE.lon);
			dNW_NE = m_staticLib.MetersBetweenCoordinates(ptNW.lat, ptNW.lon, ptNE.lat, ptNE.lon);

			//--------------------------------------------------------------------------//
			// Set up the vectors and get the cross product.
			//-------------------//
			//---------------------------------------
			// First vector cross-product operation.
			// Northwest to southwest vector
			vNW_SW.x = 0;
			vNW_SW.y = -dSW_NW;
			vNW_SW.z = ptSW.depth - ptNW.depth;

			// Diagonal northwest to south east.
			vNW_SE.x = (dSW_SE + dNW_NE)/2.0; // take the average
			vNW_SE.y = -(dSE_NE + dSW_NW)/2.0;
			vNW_SE.z = ptSE.depth - ptNW.depth;

			v1 = m_staticLib.VectorCrossProduct(vNW_SW, vNW_SE);

			//---------------------------------------
			// Second vector cross-product operation.
			// Southwest to southeast vector
			vSW_SE.x = dSW_SE;
			vSW_SE.y = 0;
			vSW_SE.z = ptSE.depth - ptSW.depth;

			vSW_NE.x = dNW_NE;
			vSW_NE.y = (dSE_NE + dSW_NW)/2.0;
			vSW_NE.z = ptNE.depth - ptSW.depth;

			v2 = m_staticLib.VectorCrossProduct(vSW_SE, vSW_NE);

			//---------------------------------------
			// Third vector cross-product operation.
			vSE_NE.x = 0;
			vSE_NE.y = dSE_NE;
			vSE_NE.z = ptNE.depth - ptSE.depth;

			vSE_NW.x =  -dSE_NE;
			vSE_NW.y = (dSE_NE + dSW_NW)/2.0;
			vSE_NW.z = ptNW.depth - ptSE.depth;
			v3 = m_staticLib.VectorCrossProduct(vSE_NE, vSE_NW);

			//---------------------------------------
			// Fourth vector cross-product operation.
			vNE_NW.x = -dNW_NE;
			vNE_NW.y = 0;
			vNE_NW.z = ptNW.depth - ptNE.depth;

			vNE_SW.x = -dSE_NE;
			vNE_SW.y = -(dSE_NE + dSW_NW)/2.0;
			vNE_SW.z = ptSW.depth - ptNE.depth;
			v4 = m_staticLib.VectorCrossProduct(vNE_NW, vNE_SW);

			memset(&sum, 0, sizeof(_3DVECTOR));
			sum.x = v1.x + v2.x + v3.x + v4.x;
			sum.y = v1.y + v2.y + v3.y + v4.y;
			sum.z = v1.z + v2.z + v3.z + v4.z;

			// In radians
			theta = atan2(sum.y, sum.x);
			rho = sqrt(sum.x*sum.x + sum.y*sum.y);
			slope = atan2(rho, sum.z);

			// Convert to degrees.
			theta *=180/PI;

			slope *=180/PI;
			heading = -theta+90.0;

			// Set up the points used for setting up distances.
			ptNW.lat = m_fX[x+1];
			ptNW.lon = m_fY[y];
			ptNW.depth = m_fV1[(x+1)*m_nYlen + (y)];

			ptSW.lat = m_fX[x];
			ptSW.lon = m_fY[y];
			ptSW.depth = m_fV1[(x)*m_nYlen + (y)];

			ptSE.lat = m_fX[x];
			ptSE.lon = m_fY[y+1];
			ptSE.depth = m_fV1[(x)*m_nYlen + (y+1)];

			ptNE.lat = m_fX[x+1];
			ptNE.lon = m_fY[y+1];
			ptNE.depth = m_fV1[(x+1)*m_nYlen + (y+1)];
		
			slpHead.X[x] = ptSW.lat;// + (ptNW.lat - ptSW.lat)/2;
			slpHead.Y[y] = ptSW.lon;// + (ptSE.lon - ptSW.lon)/2;
			slpHead.V1[x*slpHead.Ylen + y] = slope;
			slpHead.V2[x*slpHead.Ylen + y] = heading;

			//CrossProduct[(x+1)*m_nYlen + (y)] 
		}
	}

	if(*pRes != OK)
		DeallocateRawDataCopyMemory(&slpHead);

	return slpHead;
}

// X is latitude and Y is longitude because in the bathymetry files it is listed as Lat Lon Depth.  But on the map
// Latitude is really a Y (Horizontal lines) value and longitude is an X (vertical lines) value.

void CEnvironmentData::DeallocateRawDataCopyMemory(RAWENVIRONMENTALDATA *RawEnv)
{
	if(RawEnv->X != NULL)
		delete [] RawEnv->X;
	if(RawEnv->Y != NULL)
		delete [] RawEnv->Y;
	if(RawEnv->Z != NULL)
		delete [] RawEnv->Z;
	if(RawEnv->V1 != NULL)
		delete [] RawEnv->V1;
	if(RawEnv->V2 != NULL)
		delete [] RawEnv->V2;
	memset(RawEnv, 0, sizeof(RAWENVIRONMENTALDATA));
}

RESLT CEnvironmentData::GetRawDataCopy(RAWENVIRONMENTALDATA *RawEnv)
{
	RESLT res = OK;

	//-------------------------------------------------------------------------------------//
	// Validate input parameters
	//--------------------------//
	// Data buffers must have been declared by the calling application and must be
	// the same length as is held in memory by the member variables of this class
	// or zero (meaning that data is not requested).
	_ASSERT(RawEnv != NULL);
	_ASSERT(RawEnv->Xlen == 0 || RawEnv->Xlen == m_nXlen);
	_ASSERT(RawEnv->Ylen == 0 || RawEnv->Ylen == m_nYlen);
	_ASSERT(RawEnv->Zlen == 0 || RawEnv->Zlen == m_nZlen);
	_ASSERT(RawEnv->Vlen == 0 || RawEnv->Vlen == m_nVlen);


	if(RawEnv == NULL)
		return PARAM_HAD_NULLREF_ERROR;
	if((RawEnv->Xlen != 0 && RawEnv->Xlen != m_nXlen) ||
		(RawEnv->Ylen != 0 && RawEnv->Ylen != m_nYlen) ||
		(RawEnv->Zlen != 0 && RawEnv->Zlen != m_nZlen) ||
		(RawEnv->Vlen != 0 && RawEnv->Vlen != m_nVlen))
		return BUFFERLENGTH_INADEQUATE_ERROR;
	//-------------------------------------------------------------------------------------//

	if(RawEnv->X != NULL && RawEnv->Xlen > 0 && m_nXlen > 0)
		memcpy_s(RawEnv->X, RawEnv->Xlen*sizeof(double), m_fX, RawEnv->Xlen*sizeof(double));
	if(RawEnv->Y != NULL && RawEnv->Ylen > 0 && m_nYlen > 0)
		memcpy_s(RawEnv->Y, RawEnv->Ylen*sizeof(double), m_fY, RawEnv->Ylen*sizeof(double));
	if(RawEnv->Z != NULL && RawEnv->Zlen > 0 && m_nZlen > 0)
		memcpy_s(RawEnv->Z, RawEnv->Zlen*sizeof(double), m_fZ, RawEnv->Zlen*sizeof(double));
	if(RawEnv->V1 != NULL && RawEnv->Vlen > 0 && m_nVlen > 0)
		memcpy_s(RawEnv->V1, RawEnv->Vlen*sizeof(double), m_fV1, RawEnv->Vlen*sizeof(double));
	if(RawEnv->V2 != NULL && RawEnv->Vlen > 0 && m_nVlen > 0)
		memcpy_s(RawEnv->V2, RawEnv->Vlen*sizeof(double), m_fV2, RawEnv->Vlen*sizeof(double));

	return res;
}
// End OF New Stuff In progress
//******************************************************************************************//
//------------------------------------------------------------------------------------------//



void CEnvironmentData::ClearData()
{
	m_nXlen = m_nYlen = m_nZlen = m_nVlen = 0;
	if(m_fX != NULL)
	{
		delete [] m_fX;
		m_fX = NULL;
	}
	if(m_fY != NULL)
	{
		delete [] m_fY;
		m_fY = NULL;
	}
	if(m_fZ != NULL)
	{
		delete [] m_fZ;
		m_fZ = NULL;
	}
	if(m_fV1 != NULL)
	{
		delete [] m_fV1;
		m_fV1 = NULL;
	}
	if(m_fV2 != NULL)
	{
		delete [] m_fV2;
		m_fV2 = NULL;
	}
	m_dimensionType = ENVDAT_UNINITIALIZED;
	memset(m_szFileName, 0, sizeof(m_szFileName));
}


BOOL CEnvironmentData::LoadScaleAndOutputTextFile(TCHAR *szInputFileTitle, TCHAR *szOutputFileTitle, double ScaleValue)
{
	int x,y,z,v;
	FILE *fd;
	LoadFromTextFile(szInputFileTitle, FALSE);
	errno_t t;


	_ASSERT(m_dimensionType == ENVDAT_UNINITIALIZED || m_dimensionType == ENVDAT_3D || m_dimensionType == ENVDAT_4D);
	if(m_dimensionType == ENVDAT_3Dx2V)
		return FALSE; // This enviornmental data type isn't allowed (currently) to load or save from/to file.

	if(0 != (t = fopen_s(&fd, szOutputFileTitle, "wt")))
		return FALSE;

	switch(m_dimensionType)
	{
	case ENVDAT_3D:
		if(m_nXlen * m_nYlen != m_nVlen)
			break;

		for(x=0; x<m_nXlen; x++)
			m_fX[x] *= ScaleValue;

		for(y=0; y<m_nYlen; y++)
			m_fY[y] *= ScaleValue;


		v = 0;
		for(x=0; x<m_nXlen; x++)
		{
			for(y=0; y<m_nYlen; y++)
			{
				// -000.0000000
				fprintf(fd, "%.7f %.7f %f\n", (float)m_fX[x], (float)m_fY[y], (float)m_fV1[v]);
				v++;
			}
		}
		break;

	case ENVDAT_4D:
		if(m_nXlen * m_nYlen * m_nZlen != m_nVlen)
			break;

		for(x=0; x<m_nXlen; x++)
			m_fX[x] *= ScaleValue;

		for(y=0; y<m_nYlen; y++)
			m_fY[y] *= ScaleValue;

		for(z=0; z<m_nZlen; z++)
			m_fZ[z] *= ScaleValue;

		v = 0;
		for(x=0; x<m_nXlen; x++)
		{
			for(y=0; y<m_nYlen; y++)
			{
				for(z=0; z<m_nZlen; z++)
				{
					// -000.0000000
					fprintf(fd, "Problem %12.7f %12.7f %10f\n", (float)m_fX[x], (float)m_fY[y], (float)m_fZ[z], (float)m_fV1[v]);
					v++;
				}
			}
		}
		break;
	}
	fclose(fd);
	return TRUE;
}


/*******************************************************************************
* MEMBER FUNCTION:
* 
* DESCRIPTION:
*
* ARGUMENTS:
*	None.
*
* MEMBER VARIABLES ALTERED:
*
* RETURN VALUE:
*
* REVISION HISTORY:
*     Date    Name   Change
*   --------  ----   ------
*   05/31/05  (MJC)  Initial Coding/Transfer
*******************************************************************************/
RESLT CEnvironmentData::LoadFromTextFile(TCHAR *szFileTitle, BOOL NegateMag)
{
	OPENFILENAME ofn;

	if(szFileTitle == NULL || strlen(szFileTitle) == 0)
		return FILENAME_ERROR;

	ofn.lpstrFileTitle = ofn.lpstrFile = szFileTitle;
	return LoadFromTextFile(&ofn, NegateMag);
}
RESLT CEnvironmentData::LoadFromTextFile(OPENFILENAME *Ofn, BOOL NegateMag)
{
	FILE	*fd;
	RESLT ret;

	fopen_s(&fd, Ofn->lpstrFile, "r");
	if(NULL == fd)
		return OPENFILEREAD_ERROR;
	strncpy_s(m_szFileName, sizeof(m_szFileName), Ofn->lpstrFileTitle, sizeof(m_szFileName));
	ret = LoadFromTextFile(fd, NegateMag);
	fclose(fd);
	return ret;
}

RESLT CEnvironmentData::NewEnvironmentFromData(RAWENVIRONMENTALDATA EnvData)
{
	int i;

	_ASSERT(m_dimensionType == ENVDAT_UNINITIALIZED);
	_ASSERT(EnvData.Xlen > 0 && EnvData.Ylen > 0 && EnvData.Vlen > 0);
	_ASSERT(EnvData.X != NULL && EnvData.Y != NULL && EnvData.V1 != NULL); // V2 is optional
	_ASSERT((EnvData.Zlen > 0 && EnvData.Z !=0) || (EnvData.Zlen == 0 && EnvData.Z == NULL));

	// Currently having a z-dimension and a second Value dimsions is not allowed.
	_ASSERT(EnvData.V2 == NULL || m_nZlen == 0);

	ClearData();

	if(EnvData.Xlen <= 0 || EnvData.Ylen <= 0 || EnvData.Vlen <= 0 || EnvData.X == NULL ||
		EnvData.Y == NULL || EnvData.V1 == NULL || (EnvData.Zlen > 0 && EnvData.Z == NULL) ||
		(EnvData.Zlen != 0 && EnvData.Z == NULL))
	{
		return PARAM_INVALID_ERROR;
	}


	// Currently having a z-dimension and a second Value dimsions is not allowed.
	if(m_nZlen != 0 && EnvData.V2 != NULL)
	{
		return PARAM_INVALID_ERROR;
	}


	// Copy x-dimension values then determine the max and mins
	m_nXlen = EnvData.Xlen;
	m_fX = new double[m_nXlen];
	memcpy(m_fX, EnvData.X, m_nXlen*sizeof(double));
	m_minX = m_maxX = EnvData.X[0];
	for(i=1; i<m_nXlen; i++)
	{
		if(m_maxX < EnvData.X[i])
			m_maxX = EnvData.X[i];

		if(m_minX > EnvData.X[i])
			m_minX = EnvData.X[i];
	}


	m_nYlen = EnvData.Ylen;
	m_fY = new double[m_nYlen];
	memcpy(m_fY, EnvData.Y, m_nYlen*sizeof(double));
	m_minY = m_maxY = EnvData.Y[0];
	for(i=1; i<m_nYlen; i++)
	{
		if(m_maxY < EnvData.Y[i])
			m_maxY = EnvData.Y[i];

		if(m_minY > EnvData.Y[i])
			m_minY = EnvData.Y[i];
	}

	m_nVlen = EnvData.Vlen;
	m_fV1 = new double[m_nVlen];
	memcpy(m_fV1, EnvData.V1, m_nVlen*sizeof(double));
	m_minV1 = m_maxV1 = m_fV1[0];
	for(i=1; i<m_nVlen; i++)
	{
		if(m_maxV1 < m_fV1[i])
			m_maxV1 = m_fV1[i];

		if(m_minV1 > m_fV1[i])
			m_minV1 = m_fV1[i];
	}




	m_dimensionType = ENVDAT_3D;

	m_nZlen = EnvData.Zlen;
	if(m_nZlen > 0)
	{
		m_fZ = new double[m_nZlen];
		m_dimensionType = ENVDAT_4D;
		memcpy(m_fZ, EnvData.Z, m_nZlen*sizeof(double));
		m_minZ = m_maxZ = m_fZ[0];
		for(i=1; i<m_nZlen; i++)
		{
			if(m_maxZ < m_fZ[i])
				m_maxZ = m_fZ[i];

			if(m_minZ > m_fZ[i])
				m_minZ = m_fZ[i];
		}

	}
	else if(EnvData.V2 != NULL)
	{
		m_fV2 = new double[m_nVlen];
		m_dimensionType = ENVDAT_3Dx2V;
		memcpy(m_fV2, EnvData.V2, m_nVlen*sizeof(double));

		m_minV2 = m_maxV2 = m_fV2[0];
		for(i=1; i<m_nVlen; i++)
		{
			if(m_maxV2 < m_fV2[i])
				m_maxV2 = m_fV2[i];

			if(m_minV2 > m_fV2[i])
				m_minV2 = m_fV2[i];
		}

	}
	m_noDataLoadedValue = 0;
	m_bathyConstantSet = FALSE;
	sprintf_s(m_szFileName, TCHARBFLEN(m_szFileName), "Slope, Heading");

	return OK; // add memory check error
}


RESLT CEnvironmentData::CreateFakeData(double LatMin, double LatMax, double LonMin, double LonMax)
{
	// Make sure this instance hasn't already loaded in data.
	if(m_dimensionType != ENVDAT_UNINITIALIZED)
		ClearData();

	m_dimensionType = ENVDAT_3D; // The data occupies a plane (3-Dimensional)
	m_nXlen = m_nYlen = 2;
	m_nVlen = 4;

	m_fX = new double[2];
	m_fY = new double[2];
	m_fV1 = new double[4];


	m_minX = m_fX[0] = LatMin;
	m_maxX = m_fX[1] = LatMax;
	m_minY = m_fY[0] = LonMin;
	m_maxY = m_fY[1] = LonMax;
	m_minV1 = m_maxV1 = m_fV1[0] = m_fV1[1] = m_fV1[2] = m_fV1[3] = -7000;

	return OK;
}


RESLT CEnvironmentData::LoadFromTextFile(FILE *fd, BOOL NegateMag)
{
	// Floats must be used for scanf.
	float x=0,   y=0,  z=0,  v=0;	// x,y,z and data values (v) read in.
	float prevX=0, prevY=0, prevZ=0;	// previous x,y,z and values read in.
	TCHAR  buff[256];
	int	  sscanfConvCnt, sscanfRequiredConvCnt;
	double dVal;
	int lineCount = 0;


	_ASSERT(m_dimensionType == ENVDAT_UNINITIALIZED || m_dimensionType == ENVDAT_3D || m_dimensionType == ENVDAT_4D);
	if(m_dimensionType == ENVDAT_3Dx2V)
		return WRONGENVDATATYPE_ERROR; // This enviornmental data type isn't allowed (currently) to load or save from/to file.

	// Make sure this instance hasn't already loaded in data.
	if(m_dimensionType != ENVDAT_UNINITIALIZED)
		ClearData();

	//----------------------------------------------------------------------------------//
	// (1) Determine dimensionality of the file.
	//------------------------------------------//
	// Scan in the first line of the file to determine if it is 3 dimension (as in lat,
	// lon, depth) or 4 dimension (as in lat, lon, depth, termperature (at depth).
	// Rewind after scaning first line because will restart the scan from the top after
	// determining the dimensionality of the file.
	// Return error if not 3-D or 4-D data.
	fgets(buff, 256, fd);
	sscanfConvCnt = sscanf_s(buff, "%f %f %f %f",&v, &v, &v, &v);
	rewind(fd);
	sscanfRequiredConvCnt = sscanfConvCnt;
	if(sscanfConvCnt < 3 || sscanfConvCnt > 4)
	{
		ClearData();
		return FILEFORMAT_ERROR;
	}

	//----------------------------------------------------------------------------------//
	// (2) Determine the length of each dimension
	//------------------------------------------//
	// Read in the first line of the environmental data file.
	if(sscanfConvCnt == 3)
	{
		m_dimensionType = ENVDAT_3D; // The data occupies a plane (3-Dimensional)
		if(3 != fscanf_s(fd, "%f %f %f",&x, &y, &v))
		{
			ClearData();
			return FILEFORMAT_ERROR;
		}
		lineCount = 1;
		prevZ = z = 0;
	}
	else if(sscanfConvCnt == 4)
	{
		m_dimensionType = ENVDAT_4D; // The data occupies a volume (4-Dimensional)
		if(4 != fscanf_s(fd, "%f %f %f %f",&x, &y, &z, &v))
		{
			ClearData();
			return FILEFORMAT_ERROR;
		}
		lineCount = 1;
		prevZ = z-1;
	}

	// make previous values different from just-read-in values so they count properly in
	// the following while-loop.
	prevX = x - 1; prevY = y - 1;
	while(sscanfConvCnt == sscanfRequiredConvCnt)
	{
		if(x != prevX)
		{
			m_nXlen++;
			m_nYlen = m_nZlen = 0;
		}
		
		if(y != prevY)
		{
			m_nYlen++;
			m_nZlen = 0;
		}

		if(z != prevZ)
		{
			m_nZlen++;
		}

		if(x != prevX || y != prevY || z != prevZ)
			m_nVlen++;
		else
			m_nVlen = m_nVlen;

		prevX = x; prevY = y; prevZ = z;


		switch(m_dimensionType)
		{
		case ENVDAT_3D:
			x = y = z = 0;
			sscanfConvCnt = fscanf_s(fd, "%f %f %f",&x, &y, &v);
			lineCount++;
			break;
		case ENVDAT_4D:
			sscanfConvCnt = fscanf_s(fd, "%f %f %f %f",&x, &y, &z, &v);
			lineCount++;
			break;
		}
	}

	// Verify proper file format.  The length of each dimension mulitplied together must
	// equate to the length of the value vector (m_nVlen).
	switch(m_dimensionType)
	{
	case ENVDAT_3D:
		if(m_nXlen * m_nYlen != m_nVlen)
		{
			ClearData();
			return FILEFORMAT_ERROR;
		}
		break;
	case ENVDAT_4D:
		if(m_nXlen * m_nYlen * m_nZlen != m_nVlen)
		{
			ClearData();
			return FILEFORMAT_ERROR;
		}
		break;
	}


	//----------------------------------------------------------------------------------//
	// (3) Allocate Memory for the arrays that hold the data read in.
	//--------------------------------------------------------------//
	// The length of each dimension is now known so required memory can now be allocated.
	if(NULL == (m_fX = new double[m_nXlen]))
	{
		ClearData();
		return MEMALLOC_ERROR;
	}

	if(NULL == (m_fY = new double[m_nYlen]))
	{
		ClearData();
		return MEMALLOC_ERROR;
	}

	if(NULL == (m_fV1 = new double[m_nVlen]))
	{
		ClearData();
		return MEMALLOC_ERROR;
	}

	if(m_nZlen > 0 && (NULL == (m_fZ = new double[m_nZlen])))
	{
		ClearData();
		return MEMALLOC_ERROR;
	}

	//----------------------------------------------------------------------------------//
	// (4) Read the data from the file into the allocated buffers.
	//-----------------------------------------------------------//
	rewind(fd);
	m_nXlen = m_nYlen = m_nZlen = m_nVlen = 0;

	// Read in the first line to start the while loop below.
	switch(m_dimensionType)
	{
	case ENVDAT_3D:
		sscanfConvCnt = fscanf_s(fd, "%f %f %f",&x, &y, &v);
		prevZ = z = 0;
		m_minX = m_maxX = x;
		m_minY = m_maxY = y;
		m_minZ = m_maxZ = 0;
		m_minV1 = m_maxV1 = v;
		break;

	case ENVDAT_4D:
		sscanfConvCnt = fscanf_s(fd, "%f %f %f %f",&x, &y, &z, &v);
		prevZ = z - 1;
		m_minX = m_maxX = x;
		m_minY = m_maxY = y;
		m_minZ = m_maxZ = z;
		m_minV1 = m_maxV1 = v;
		break;
	}

	// The following loop continues until the last line.  Before entering the loop, make
	// previous  (prevX, prevY, and prevZ) different from just-read-in values so they count
	// properly.
	prevX = x - 1; prevY = y - 1;
	while(sscanfConvCnt == sscanfRequiredConvCnt)
	{
		if(x != prevX)
		{
			m_fX[m_nXlen++] = x;
			m_nYlen = m_nZlen = 0;

			if(x < m_minX)
				m_minX = x;
			else if(x > m_maxX)
				m_maxX = x;
		}
		
		if(y != prevY)
		{
			m_fY[m_nYlen++] = y;
			m_nZlen = 0;

			if(y < m_minY)
				m_minY = y;
			else if(y > m_maxY)
				m_maxY = y;
		}

		if(z != prevZ)
		{
			m_fZ[m_nZlen++] = z;

			if(z < m_minZ)
				m_minZ = z;
			else if(z > m_maxZ)
				m_maxZ = z;

		}

		if(x != prevX || y != prevY || z != prevZ)
		{
			if(NegateMag == FALSE)
				m_fV1[m_nVlen] = v;
			else
				m_fV1[m_nVlen] = -v;

			m_nVlen++;

			if(v < m_minV1)
				m_minV1 = v;
			else if(v > m_maxV1)
				m_maxV1 = v;

		}

		prevX = x; prevY = y; prevZ = z;

		if(m_dimensionType == ENVDAT_3D)
			sscanfConvCnt = fscanf_s(fd, "%f %f %f", &x, &y, &v);
		else
			sscanfConvCnt = fscanf_s(fd, "%f %f %f %f", &x, &y, &z, &v);

	}

	if(NegateMag == TRUE)
	{
		dVal = m_maxV1;
		m_maxV1 = m_minV1;
		m_minV1 = dVal;

		m_minV1 = -m_minV1;
		m_maxV1 = -m_maxV1;

	}
	else
	{
		m_maxV1 = m_maxV1;
		m_minV1 = m_minV1;
	}

//	GetSlopeHeadingFromCrossProduct();
#if 0 // debug
	ENVMINMAX m = GetExtremes();
	CalculateSurfaceAreaAtPlane(m.xMin, m.yMin, m.xMax, m.yMax, -2.0);
#endif
	m_bathyConstantSet = FALSE;

	return OK;
}


RESLT CEnvironmentData::LoadFromBinFile(HANDLE hd)
{
	DWORD bytes;
	int i;
	int ioResult = 1;
	BYTE reserved1[SIZE_12];
	BYTE reserved2[SIZE_12];
	TCHAR szBuff[SIZE_16];
	DWORD xAdditionalBytes16byteAlign;
	DWORD yAdditionalBytes16byteAlign;
	DWORD zAdditionalBytes16byteAlign;
	DWORD vAdditionalBytes16byteAlign;
	DWORD totalBytes;
	__int32 intVar;

	_ASSERT(m_dimensionType == ENVDAT_UNINITIALIZED || m_dimensionType == ENVDAT_3D || m_dimensionType == ENVDAT_4D);
	if(m_dimensionType == ENVDAT_3Dx2V)
		return WRONGENVDATATYPE_ERROR; // This enviornmental data type isn't allowed (currently) to load or save from/to file.


	if(m_dimensionType != ENVDAT_UNINITIALIZED)
		ClearData();

	// Read the total bytes associated with this binary environmental data
	ioResult &= ReadFile(hd, &totalBytes, sizeof(totalBytes), &bytes, NULL);

	// Skip reserve buffer number 1
	ioResult &= ReadFile(hd, &reserved1, sizeof(reserved1), &bytes, NULL);

	// Read the name of the file into the file name buffer.
	ioResult &= ReadFile(hd, &m_szFileName, sizeof(m_szFileName), &bytes, NULL);

	// Write the dimension type (2-D or 3-D).
	ioResult &= ReadFile(hd, &intVar, sizeof(intVar), &bytes, NULL);
	m_dimensionType = (ENV_DIMENSION)intVar;
	ioResult &= ReadFile(hd, &reserved2, sizeof(reserved2), &bytes, NULL);

	// Read the array lengths
	ioResult &= ReadFile(hd, &m_nXlen, sizeof(m_nXlen), &bytes, NULL);
	ioResult &= ReadFile(hd, &xAdditionalBytes16byteAlign, sizeof(xAdditionalBytes16byteAlign), &bytes, NULL);
	ioResult &= ReadFile(hd, &m_nYlen, sizeof(m_nYlen), &bytes, NULL);
	ioResult &= ReadFile(hd, &yAdditionalBytes16byteAlign, sizeof(yAdditionalBytes16byteAlign), &bytes, NULL);
	ioResult &= ReadFile(hd, &m_nZlen, sizeof(m_nZlen), &bytes, NULL);
	ioResult &= ReadFile(hd, &zAdditionalBytes16byteAlign, sizeof(zAdditionalBytes16byteAlign), &bytes, NULL);
	ioResult &= ReadFile(hd, &m_nVlen, sizeof(m_nVlen), &bytes, NULL);
	ioResult &= ReadFile(hd, &vAdditionalBytes16byteAlign, sizeof(vAdditionalBytes16byteAlign), &bytes, NULL);
	if(ioResult == 0)
		return FILEREAD_ERROR;


	// Allocate Memory for the arrays.
	if(NULL == (m_fX = new double[m_nXlen]))
		return MEMALLOC_ERROR;

	if(NULL == (m_fY = new double[m_nYlen]))
	{
		delete [] m_fX;
		return MEMALLOC_ERROR;
	}

	if(NULL == (m_fV1 = new double[m_nVlen]))
	{
		delete [] m_fX;
		delete [] m_fY;
		return MEMALLOC_ERROR;
	}

	if(m_nZlen > 0 && (NULL == (m_fZ = new double[m_nZlen])))
	{
		delete [] m_fX;
		delete [] m_fY;
		delete [] m_fV1;
		return MEMALLOC_ERROR;
	}
	// Read the arrays.
	ioResult &= ReadFile(hd, m_fX, m_nXlen*sizeof(double), &bytes, NULL);
	ioResult &= ReadFile(hd, szBuff, xAdditionalBytes16byteAlign, &bytes, NULL);
	ioResult &= ReadFile(hd, m_fY, m_nYlen*sizeof(double), &bytes, NULL);
	ioResult &= ReadFile(hd, szBuff, yAdditionalBytes16byteAlign, &bytes, NULL);
	ioResult &= ReadFile(hd, m_fZ, m_nZlen*sizeof(double), &bytes, NULL);
	ioResult &= ReadFile(hd, szBuff, zAdditionalBytes16byteAlign, &bytes, NULL);
	ioResult &= ReadFile(hd, m_fV1, m_nVlen*sizeof(double), &bytes, NULL);
	ioResult &= ReadFile(hd, szBuff, vAdditionalBytes16byteAlign, &bytes, NULL);
	if(ioResult == 0)
	{
		delete [] m_fX;
		delete [] m_fY;
		delete [] m_fV1;
		if(m_fZ != NULL)
			delete [] m_fZ;
		m_fX = NULL;
		m_fY = NULL;
		m_fV1 = NULL;
		m_fZ = NULL;
		return FILEREAD_ERROR;
	}

	// Determine mins and maxes
	m_minX = m_maxX = m_fX[0];
	for(i=1; i<m_nXlen; i++)
	{
		if(m_fX[i] < m_minX)
			m_minX = m_fX[i];

		if(m_fX[i] > m_maxX)
			m_maxX = m_fX[i];
	}


	m_minY = m_maxY = m_fY[0];
	for(i=1; i<m_nYlen; i++)
	{
		if(m_fY[i] < m_minY)
			m_minY = m_fY[i];

		if(m_fY[i] > m_maxY)
			m_maxY = m_fY[i];
	}

	if(m_nZlen > 0)
	{
		m_minZ = m_maxZ = m_fZ[0];
		for(i=1; i<m_nZlen; i++)
		{
			if(m_fZ[i] < m_minZ)
				m_minZ = m_fZ[i];

			if(m_fZ[i] > m_maxZ)
				m_maxZ = m_fZ[i];
		}
	}

	m_minV1 = m_maxV1 = m_fV1[0];
	for(i=1; i<m_nVlen; i++)
	{
		if(m_fV1[i] < m_minV1)
			m_minV1 = m_fV1[i];

		if(m_fV1[i] > m_maxV1)
			m_maxV1 = m_fV1[i];
	}
	m_bathyConstantSet = FALSE;

	return OK;
}

int CEnvironmentData::CalculateStorageBytes(void)
{
	BYTE reserved1[SIZE_12];
	BYTE reserved2[SIZE_12];
	int xAdditionalBytes16byteAlign;
	int yAdditionalBytes16byteAlign;
	int zAdditionalBytes16byteAlign;
	int vAdditionalBytes16byteAlign;
	int totalBytes;


	_ASSERT(m_dimensionType == ENVDAT_UNINITIALIZED || m_dimensionType == ENVDAT_3D || m_dimensionType == ENVDAT_4D);
	if(m_dimensionType == ENVDAT_3Dx2V)
		return 0; // not stored to file.

	//----------------------------------------------------------------------------------//
	// Initialize local vars
	//----------------------//
	totalBytes = xAdditionalBytes16byteAlign = yAdditionalBytes16byteAlign = zAdditionalBytes16byteAlign = vAdditionalBytes16byteAlign = 0;

	// Values for keeping data 16-byte aligned
	// (value%16) equals 0 ~ 16.  That result %2 makes the value 0 or 1.  If it is one,
	// 8 bytes need to be written.  If 0, zero bytes need to be written.
	if(((m_nXlen%16)%2) != 0)
		xAdditionalBytes16byteAlign = 8;
	if(((m_nYlen%16)%2) != 0)
		yAdditionalBytes16byteAlign = 8;
	if(((m_nZlen%16)%2) != 0)
		zAdditionalBytes16byteAlign = 8;
	if(((m_nVlen%16)%2) != 0)
		vAdditionalBytes16byteAlign = 8;

	// Write the total bytes written (not determined yet, but will be)
	totalBytes += sizeof(totalBytes);

	// Write reserve buffer number 1
	totalBytes += sizeof(reserved1);

	// Write the name of the file into the file name buffer.
	totalBytes += sizeof(m_szFileName);

	// Write the dimension type (number of dimensions) (2-D or 3-D).
	totalBytes += sizeof(ENV_DIMENSION);
	totalBytes += sizeof(reserved2);

	// Write the array lengths
	totalBytes += sizeof(m_nXlen);
	totalBytes += sizeof(xAdditionalBytes16byteAlign);
	totalBytes += sizeof(m_nYlen);
	totalBytes += sizeof(yAdditionalBytes16byteAlign);
	totalBytes += sizeof(m_nZlen);
	totalBytes += sizeof(zAdditionalBytes16byteAlign);
	totalBytes += sizeof(m_nVlen);
	totalBytes += sizeof(vAdditionalBytes16byteAlign);

	// Write the arrays.
	totalBytes += m_nXlen*sizeof(double);
	totalBytes += xAdditionalBytes16byteAlign;
	totalBytes += m_nYlen*sizeof(double);
	totalBytes += yAdditionalBytes16byteAlign;
	totalBytes += m_nZlen*sizeof(double);
	totalBytes += zAdditionalBytes16byteAlign;
	totalBytes += m_nVlen*sizeof(double);
	totalBytes += vAdditionalBytes16byteAlign;
	return totalBytes;
}


RESLT CEnvironmentData::SaveToBinFile(HANDLE hd, DWORD *pNumBytes)
{
	DWORD bytes;
	BOOL ioResult = 1;
	BYTE reserved1[SIZE_12];
	BYTE reserved2[SIZE_12];
	TCHAR szBuff[SIZE_8];
	DWORD xAdditionalBytes16byteAlign;
	DWORD yAdditionalBytes16byteAlign;
	DWORD zAdditionalBytes16byteAlign;
	DWORD vAdditionalBytes16byteAlign;
	DWORD totalBytes;
	__int32 intVar;


	_ASSERT(m_dimensionType == ENVDAT_UNINITIALIZED || m_dimensionType == ENVDAT_3D || m_dimensionType == ENVDAT_4D);
	if(m_dimensionType == ENVDAT_3Dx2V)
		return WRONGENVDATATYPE_ERROR; // This enviornmental data type isn't allowed (currently) to load or save from/to file.

	//----------------------------------------------------------------------------------//
	// Initialize local vars
	//----------------------//
	memset(&reserved1, 0, sizeof(reserved1));
	memset(&reserved2, 0, sizeof(reserved2));
	memset(&szBuff, 0, sizeof(szBuff));
	totalBytes = xAdditionalBytes16byteAlign = yAdditionalBytes16byteAlign = zAdditionalBytes16byteAlign = vAdditionalBytes16byteAlign = 0;

	// Values for keeping data 16-byte aligned
	// (value%16) equals 0 ~ 16.  That result %2 makes the value 0 or 1.  If it is one,
	// 8 bytes need to be written.  If 0, zero bytes need to be written.
	if(((m_nXlen%16)%2) != 0)
		xAdditionalBytes16byteAlign = 8;
	if(((m_nYlen%16)%2) != 0)
		yAdditionalBytes16byteAlign = 8;
	if(((m_nZlen%16)%2) != 0)
		zAdditionalBytes16byteAlign = 8;
	if(((m_nVlen%16)%2) != 0)
		vAdditionalBytes16byteAlign = 8;

	// Write the total bytes written (not determined yet, but will be)
	ioResult &= WriteFile(hd, &totalBytes, sizeof(totalBytes), &bytes, NULL);
	totalBytes += bytes;

	// Write reserve buffer number 1
	ioResult &= WriteFile(hd, reserved1, sizeof(reserved1), &bytes, NULL);
	totalBytes += bytes;

	// Write the name of the file into the file name buffer.
	ioResult &= WriteFile(hd, &m_szFileName, sizeof(m_szFileName), &bytes, NULL);
	totalBytes += bytes;

	// Write the dimension type (2-D or 3-D).
	intVar = (__int32)m_dimensionType;
	ioResult &= WriteFile(hd, &intVar, sizeof(intVar), &bytes, NULL);
	totalBytes += bytes;
	ioResult &= WriteFile(hd, &reserved2, sizeof(reserved2), &bytes, NULL);
	totalBytes += bytes;

	// Write the array lengths
	ioResult &= WriteFile(hd, &m_nXlen, sizeof(m_nXlen), &bytes, NULL);
	totalBytes += bytes;
	ioResult &= WriteFile(hd, &xAdditionalBytes16byteAlign, sizeof(xAdditionalBytes16byteAlign), &bytes, NULL);
	totalBytes += bytes;
	ioResult &= WriteFile(hd, &m_nYlen, sizeof(m_nYlen), &bytes, NULL);
	totalBytes += bytes;
	ioResult &= WriteFile(hd, &yAdditionalBytes16byteAlign, sizeof(yAdditionalBytes16byteAlign), &bytes, NULL);
	totalBytes += bytes;
	ioResult &= WriteFile(hd, &m_nZlen, sizeof(m_nZlen), &bytes, NULL);
	totalBytes += bytes;
	ioResult &= WriteFile(hd, &zAdditionalBytes16byteAlign, sizeof(zAdditionalBytes16byteAlign), &bytes, NULL);
	totalBytes += bytes;
	ioResult &= WriteFile(hd, &m_nVlen, sizeof(m_nVlen), &bytes, NULL);
	totalBytes += bytes;
	ioResult &= WriteFile(hd, &vAdditionalBytes16byteAlign, sizeof(vAdditionalBytes16byteAlign), &bytes, NULL);
	totalBytes += bytes;

	// Write the arrays.
	ioResult &= WriteFile(hd, m_fX, m_nXlen*sizeof(double), &bytes, NULL);
	totalBytes += bytes;
	ioResult &= WriteFile(hd, szBuff, xAdditionalBytes16byteAlign, &bytes, NULL);
	totalBytes += bytes;
	ioResult &= WriteFile(hd, m_fY, m_nYlen*sizeof(double), &bytes, NULL);
	totalBytes += bytes;
	ioResult &= WriteFile(hd, szBuff, yAdditionalBytes16byteAlign, &bytes, NULL);
	totalBytes += bytes;
	ioResult &= WriteFile(hd, m_fZ, m_nZlen*sizeof(double), &bytes, NULL);
	totalBytes += bytes;
	ioResult &= WriteFile(hd, szBuff, zAdditionalBytes16byteAlign, &bytes, NULL);
	totalBytes += bytes;
	ioResult &= WriteFile(hd, m_fV1, m_nVlen*sizeof(double), &bytes, NULL);
	totalBytes += bytes;
	ioResult &= WriteFile(hd, szBuff, vAdditionalBytes16byteAlign, &bytes, NULL);
	totalBytes += bytes;


	// rewind to the start of where writing begain, and rewrite total bytes
	if(INVALID_SET_FILE_POINTER == m_staticLib.MySetFilePointer(hd, -1*__int64(totalBytes), FILE_CURRENT))
		return SETFILEPOINTER_ERROR;
//	if(INVALID_SET_FILE_POINTER == SetFilePointer(hd, -totalBytes, NULL, FILE_CURRENT))
//		return SETFILEPOINTER_ERROR;

	ioResult &= WriteFile(hd, &totalBytes, sizeof(totalBytes), &bytes, NULL);

	// Skip back ahead to where writting ended
	if(INVALID_SET_FILE_POINTER == m_staticLib.MySetFilePointer(hd, __int64(totalBytes)-sizeof(totalBytes), FILE_CURRENT))
		return SETFILEPOINTER_ERROR;
//	if(INVALID_SET_FILE_POINTER == SetFilePointer(hd, totalBytes-sizeof(totalBytes), NULL, FILE_CURRENT))
//		return SETFILEPOINTER_ERROR;

	if(ioResult == 0)
		return FILEWRITE_ERROR;

	if(pNumBytes != NULL)
		*pNumBytes = totalBytes;

	_ASSERTE(totalBytes == (DWORD)CalculateStorageBytes());

	return OK;
}



//----------------------//
// Data Access Functions
//----------------------//
int CEnvironmentData::
FindDimensionIndexSansInterpolation(double Location, double *DimensionArray, int ArrayLen)
{
	int   i = -1;
	float loc; // Keep this as a float for the resolution knock down.
	double delta;

	_ASSERT(ArrayLen >= 2);
	delta = DimensionArray[1]-DimensionArray[0];

	// Knock down the resolution to avoid comparisons being incorrectly true, as happens
	// when 2 < 2 being true when in memory its really
	// 1.9999999999999998 < 2.0000000000000000
	loc = float(Location);

	if(loc < DimensionArray[0]/* || loc > DimensionArray[ArrayLen-1]*/)
	{
		return -1;
	}
	else
	{
		i=0;
		while(i<ArrayLen && FALSE == (DimensionArray[i] <= loc && loc < DimensionArray[i]+delta))
			i++;
		if(i >= ArrayLen)
			return -1;
	}

	_ASSERTE(i >= 0 && i<ArrayLen);
	return i;
}
int CEnvironmentData::
FindDimensionIndex(double Location, int StartSearchIndex, double *DimensionArray, int ArrayLen)
{
	int   i = -1;
	float loc; // Keep this as a float for the resolution knock down. 

	// Knock down the resolution to avoid comparisons being incorrectly true, as happens
	// when 2 < 2 being true when in memory its really
	// 1.9999999999999998 < 2.0000000000000000
	loc = float(Location);

	// Test the following indexes (by incrementing i): 
	for(i = StartSearchIndex-1; i < StartSearchIndex+1; i++)
	{
		if((i >= 0 && i < ArrayLen-1) && (DimensionArray[i] <= loc && loc < DimensionArray[i+1]))
			return i;
	}

	if(loc < DimensionArray[0])
	{
		// Lower boundary test case.  If loc is less than the lowest entry in the DimensionArray
		// (array index [0]), return 0 as the array index
		i = 0;
	}
	else if(loc >= DimensionArray[ArrayLen-1])
	{
		// Upper boundary test case.  If loc is greater than or equal to the highest entry
		// in the DimensionArray (array index [ArrayLen-1]), return ArrayLen-2 as the
		// array index so calculations can be made.
		i = ArrayLen-2;
	}
	else
	{
		for(i=0; i<ArrayLen-1; i++)
			if(DimensionArray[i] <= loc && loc < DimensionArray[i+1])
				return i;
	}

	_ASSERTE(i >= 0);
	return i;
}


double CEnvironmentData::GetXUnitlessResolution()
{
	if(IsDataLoaded() == FALSE || m_dimensionType == ENVDAT_4D)
		return -1.0;

	// The following assumes regular spacing in the environment file
	return m_fX[1] - m_fX[0];

}
double CEnvironmentData::GetYUnitlessResolution()
{
	if(IsDataLoaded() == FALSE || m_dimensionType == ENVDAT_4D)
		return -1.0;

	// The following assumes regular spacing in the environment file
	return  m_fY[1] - m_fY[0];
}

double CEnvironmentData::xGetUnitlessUnitSquareResolution()
{
	if(IsDataLoaded() == FALSE || m_dimensionType == ENVDAT_4D)
		return -1.0;

	return GetXUnitlessResolution() * GetYUnitlessResolution();
}


double CEnvironmentData::CalculateUnitlessSurfaceAreaAtPlane(double Xi, double Yi, double Xf, double Yf, double Zplane)
{
	int xIndexStart, xIndexEnd, yIndexStart, yIndexEnd, i, j;
	double v00, v01, v10, v11 = 0;
	int cnt;
	//double squareArea=0;
	double surfaceArea = 0.0;
	double totalSquares = m_nYlen * m_nXlen;

	if(IsDataLoaded() == FALSE || m_dimensionType == ENVDAT_4D)
		return -1.0;

	if(Xf < Xi)
	{
		v00 = Xf;
		Xf = Xi;
		Xi = v00;
	}
	if(Yf < Yi)
	{
		v00 = Yf;
		Yf = Yi;
		Yi = v00;
	}


	// The following assumes regular spacing in the environment file
//	squareArea =  /*x*/ (m_fX[1] - m_fX[0]) * /*y*/ (m_fY[1] - m_fY[0]);

	//------------------------------------------------------------------------------------//
	// Find the x and y indexes for the given initial and final coordinates.
	//---------------------------------------------------------------------/
	xIndexStart = FindDimensionIndex(Xi, 0, m_fX, m_nXlen);
	yIndexStart = FindDimensionIndex(Yi, 0, m_fY, m_nYlen);
	xIndexEnd = FindDimensionIndex(Xf, 0, m_fX, m_nXlen);
	yIndexEnd = FindDimensionIndex(Yf, 0, m_fY, m_nYlen);

	// Look into reasoning for having to do this...
	if(xIndexStart == m_nXlen-2)
		xIndexStart += 2;
	if(xIndexEnd == m_nXlen-2)
		xIndexEnd += 2;
	if(yIndexStart == m_nYlen-2)
		yIndexStart += 2;
	if(yIndexEnd == m_nYlen-2)
		yIndexEnd += 2;


	for(i=xIndexStart; i<xIndexEnd-1; i++)
	{
		for(j=yIndexStart; j<yIndexEnd-1; j++)
		{
			cnt = 0;

			_ASSERT((i+0) * m_nYlen + (j+0) < m_nVlen);
			_ASSERT((i+0) * m_nYlen + (j+1) < m_nVlen);
			_ASSERT((i+1) * m_nYlen + (j+0) < m_nVlen);
			_ASSERT((i+1) * m_nYlen + (j+1) < m_nVlen);

			v00 = m_fV1[(i) * m_nYlen + (j)];
			if(v00 >= Zplane)
				cnt++;

			v01 = m_fV1[(i) * m_nYlen + (j+1)];
			if(v01 >= Zplane)
				cnt++;

			v10 = m_fV1[(i+1) * m_nYlen + (j)];
			if(v10 >= Zplane)
				cnt++;


			v11 = m_fV1[(i+1) * m_nYlen + (j+1)];
			if(v11 >= Zplane)
				cnt++;

			if(cnt == 3)
				surfaceArea += 0.5;// * squareArea;
			else if(cnt == 4)
				surfaceArea += 1;//squareArea;
		}
	}
	surfaceArea = surfaceArea/totalSquares;
	return surfaceArea;
}

BOOL CEnvironmentData::ConstantValueIsSet()
{
	return m_bathyConstantSet;
}


double CEnvironmentData::SetConstantValue(double Value)
{
	if(IsDataLoaded() == TRUE)
		ClearData();
	m_noDataLoadedValue = Value;

	//_ASSERT(0);
	m_bathyConstantSet = TRUE;
	return m_noDataLoadedValue;
}

// Lat is X
// Lon is Y
BOOL CEnvironmentData::CheckMetersPerCoord()
{
	int x,y;
	double deltaLat, deltaLon;
	double maxLatDelta, minLatDelta;
	double maxLonDelta, minLonDelta;

	if(IsDataLoaded() == FALSE)
		return TRUE;

	maxLatDelta = minLatDelta = fabs(m_staticLib.MetersBetweenCoordinates(m_fX[0], m_fY[0], m_fX[1], m_fY[0]));
	maxLonDelta = minLonDelta = fabs(m_staticLib.MetersBetweenCoordinates(m_fX[0], m_fY[0], m_fX[0], m_fY[1]));


	// Check each Lon for every Lat
	for(x=0; x<m_nXlen-1; x++)
	{
		for(y=0; y<m_nYlen-1; y++)
		{
			// Very Lat, keep lon constant
			deltaLat = fabs(m_staticLib.MetersBetweenCoordinates(m_fX[x], m_fY[y], m_fX[x+1], m_fY[y]));
			if(maxLatDelta < deltaLat)
				maxLatDelta = deltaLat;
			if(minLatDelta > deltaLat)
				minLatDelta = deltaLat;


			// Very Lon, keep lat constant
			deltaLon = fabs(m_staticLib.MetersBetweenCoordinates(m_fX[x], m_fY[y], m_fX[x], m_fY[y+1]));
			if(maxLonDelta < deltaLon)
				maxLonDelta = deltaLon;
			if(minLonDelta > deltaLon)
				minLonDelta = deltaLon;
		}
	}
	return TRUE;
}

double CEnvironmentData::GetValueAtCoordinateSansExtrapolation(double X, double Y, ENVDATA_INDEX *Sector, double *SecondValue)
{
	if(m_nXlen > 1 && m_nYlen > 1)
		return GetValueAtCoordinateSansExtrapolation(X, Y, -1, Sector, SecondValue);
	else
		return 0;
}


double CEnvironmentData::
GetValueAtCoordinateSansExtrapolation(double X, double Y, double Z, ENVDATA_INDEX *Sector, double *SecondValue)
{
	int x1, x2, y1, y2, z1 = -1, z2 = -1;
	int i1, i2, i3, i4;
	double val = 0;

	if(IsDataLoaded() == FALSE)
		return m_noDataLoadedValue;

	// Check that the Sector param passed in was initialized or that it has at the very
	// least reasonable values.  The values in Sector (x, y, and z) are only locations to
	// begin searching.  If set to zero, it searches at the begining of that dimension.
	// This is in place to speed up the retrieval of environmental data.
	if(Sector->x < 0)
		Sector->x = 0;
	if(Sector->x >= m_nXlen)
		Sector->x = m_nXlen-1;

	if(Sector->y < 0)
		Sector->y = 0;
	if(Sector->y >= m_nYlen)
		Sector->y = m_nYlen-1;

	if(GetNumDimensions() == 2 || Sector->z < 0)
		Sector->z = 0;
	else if(Sector->z >= m_nZlen)
		Sector->z = m_nZlen-1;

	//------------------------------------------------------------------------------------//
	// Seach the X dimension
	//************************************************************************************//
	// Begin the search starting near the last sector
	//************************************************************************************//
	x1 = FindDimensionIndexSansInterpolation(X, m_fX, m_nXlen);
	y1 = FindDimensionIndexSansInterpolation(Y, m_fY, m_nYlen);

	//------------------------------------------------------------------------------------//
	// Seach the Z dimension (if 3-D data)
	// Test boundary cases.
	if(m_dimensionType == ENVDAT_4D)
	{
		// Not immplemented.
		_ASSERT(FALSE);
		z1 = FindDimensionIndex(Z, Sector->z, m_fZ, m_nZlen);
	}

	x2 = Sector->x = x1;
	y2 = Sector->y = y1;
	z2 = Sector->z = z1;

	if(x2+1 < m_nXlen)
		x2++;
	if(y2+1 < m_nYlen)
		y2++;
	if(z2+1 < m_nZlen)
		z2++;

	switch(m_dimensionType)
	{
	case ENVDAT_3Dx2V:
		_ASSERT(m_fV2 != NULL);
		// No break.  Fall through to ENVDAT_3D

	case ENVDAT_3D:
		i1 = x1 * m_nYlen + y1;
		i2 = x1 * m_nYlen + y2;
		i3 = x2 * m_nYlen + y1;
		i4 = x2 * m_nYlen + y2;

		_ASSERT(i1 < m_nVlen);
		_ASSERT(i2 < m_nVlen);
		_ASSERT(i3 < m_nVlen);
		_ASSERT(i4 < m_nVlen);


		// This needs an improvement... specifically, if the indices are invalied (that's why
		// they'd be -1) there needs to be a return type that indicates this.
		if(x1 == -1 || y1 == -1)
			val = 0;
		else
			val = m_fV1[i1];

		if(m_dimensionType == ENVDAT_3Dx2V && SecondValue != NULL && m_fV2 != NULL)
		{
			if(x1 == -1 || y1 == -1)
				*SecondValue = 0;
			else
				*SecondValue = m_fV2[i1];
		}
		break;

	case ENVDAT_4D:
		// Do Trilinear interpolation
		// Not immplemented.
		_ASSERT(FALSE);
		val = 0;
		if(SecondValue != NULL)
			*SecondValue = 0;
	}
	return val;
}

double CEnvironmentData::GetValueAtCoordinate(double X, double Y, ENVDATA_INDEX *Sector, double *SecondValue)
{
	return GetValueAtCoordinate(X, Y, -1, Sector, SecondValue);
}


double CEnvironmentData::
GetValueAtCoordinate(double X, double Y, double Z, ENVDATA_INDEX *Sector, double *SecondValue)
{
	int x1, x2, y1, y2, z1 = -1, z2 = -1;
	int i1, i2, i3, i4;
	double t,u;
	double val = 0;

	if(IsDataLoaded() == FALSE)
		return m_noDataLoadedValue;

	// Check that the Sector param passed in was initialized or that it has at the very
	// least reasonable values.  The values in Sector (x, y, and z) are only locations to
	// begin searching.  If set to zero, it searches at the begining of that dimension.
	// This is in place to speed up the retrieval of environmental data.
	if(Sector->x < 0)
		Sector->x = 0;
	if(Sector->x >= m_nXlen)
		Sector->x = m_nXlen-1;

	if(Sector->y < 0)
		Sector->y = 0;
	if(Sector->y >= m_nYlen)
		Sector->y = m_nYlen-1;

	if(GetNumDimensions() == 2 || Sector->z < 0)
		Sector->z = 0;
	else if(Sector->z >= m_nZlen)
		Sector->z = m_nZlen-1;

	//------------------------------------------------------------------------------------//
	// Seach the X dimension
	//************************************************************************************//
	// Begin the search starting near the last sector
	//************************************************************************************//
	x1 = FindDimensionIndex(X, Sector->x, m_fX, m_nXlen);
	y1 = FindDimensionIndex(Y, Sector->y, m_fY, m_nYlen);

	//------------------------------------------------------------------------------------//
	// Seach the Z dimension (if 3-D data)
	// Test boundary cases.
	if(m_dimensionType == ENVDAT_4D)
		z1 = FindDimensionIndex(Z, Sector->z, m_fZ, m_nZlen);

	x2 = Sector->x = x1;
	y2 = Sector->y = y1;
	z2 = Sector->z = z1;

	if(x2+1 < m_nXlen)
		x2++;
	if(y2+1 < m_nYlen)
		y2++;
	if(z2+1 < m_nZlen)
		z2++;

	switch(m_dimensionType)
	{
	case ENVDAT_3Dx2V:
		_ASSERT(m_fV2 != NULL);
		// No break.  Fall through to ENVDAT_3D

	case ENVDAT_3D:
		// Do bilinear interpolation
		t = (Y - m_fY[y1])/(m_fY[y2] - m_fY[y1]);
		u = (X - m_fX[x1])/(m_fX[x2] - m_fX[x1]);
		i1 = x1 * m_nYlen + y1;
		i2 = x1 * m_nYlen + y2;
		i3 = x2 * m_nYlen + y1;
		i4 = x2 * m_nYlen + y2;

		_ASSERT(i1 < m_nVlen);
		_ASSERT(i2 < m_nVlen);
		_ASSERT(i3 < m_nVlen);
		_ASSERT(i4 < m_nVlen);


		if(m_fV1[i1] == m_fV1[i2] && m_fV1[i1] == m_fV1[i3] && m_fV1[i1] == m_fV1[i4])
		{
			val = m_fV1[i1];

			if(m_dimensionType == ENVDAT_3Dx2V && SecondValue != NULL && m_fV2 != NULL)
				*SecondValue = m_fV2[i1];
		}
		else
		{
			val =
				(1-t) * (1-u) * m_fV1[i1] +
				( t ) * (1-u) * m_fV1[i2] +
				(1-t) * ( u ) * m_fV1[i3] +
				( t ) * ( u ) * m_fV1[i4];

			if(m_dimensionType == ENVDAT_3Dx2V && SecondValue != NULL && m_fV2 != NULL)
			{
				*SecondValue =
					(1-t) * (1-u) * m_fV2[i1] +
					( t ) * (1-u) * m_fV2[i2] +
					(1-t) * ( u ) * m_fV2[i3] +
					( t ) * ( u ) * m_fV2[i4];
			}
		}
		break;

	case ENVDAT_4D:
		// Do Trilinear interpolation
		double xf, yf, zf;
		double i1, i2, j1, j2, w1, w2;
		int x1Index, x2Index, y1Index, y2Index;

		x1Index = x1*m_nYlen*m_nZlen;
		x2Index = x2*m_nYlen*m_nZlen;
		y1Index = y1*m_nZlen;
		y2Index = y2*m_nZlen;

		xf = X - floor(X);
		yf = Y - floor(Y);
		zf = Z - floor(Z);

		_ASSERT(x1Index + y1Index + z1 < m_nVlen);
		_ASSERT(x1Index + y1Index + z2 < m_nVlen);
		_ASSERT(x1Index + y2Index + z1 < m_nVlen);
		_ASSERT(x1Index + y2Index + z2 < m_nVlen);


		_ASSERT(x2Index + y1Index + z1 < m_nVlen);
		_ASSERT(x2Index + y1Index + z2 < m_nVlen);
		_ASSERT(x2Index + y2Index + z1 < m_nVlen);
		_ASSERT(x2Index + y2Index + z2 < m_nVlen);


		i1 = m_fV1[x1Index + y1Index + z1]*(1-zf) + m_fV1[x1Index + y1Index + z2]*zf;
		i2 = m_fV1[x1Index + y2Index + z1]*(1-zf) + m_fV1[x1Index + y2Index + z2]*zf;

		j1 = m_fV1[x2Index + y1Index + z1]*(1-zf) + m_fV1[x2Index + y1Index + z2]*zf;
		j2 = m_fV1[x2Index + y2Index + z1]*(1-zf) + m_fV1[x2Index + y2Index + z2]*zf;

		w1 = i1*(1-yf) + i2*yf;
		w2 = j1*(1-yf) + j2*yf;

		val = w1*(1-xf) + w2*xf;
		break;
	}
	return val;
}


//-----------------------------------//
// Member Variable Accessor Functions
//-----------------------------------//
int CEnvironmentData::GetNumDimensions()
{
	switch(m_dimensionType)
	{
	case ENVDAT_3D:
		return 2;
		break;
	case ENVDAT_4D:
		return 3;
		break;
	default:
		;
	}
	return 0;
}


BOOL CEnvironmentData::IsDataLoaded()
{
	if(m_dimensionType == ENVDAT_UNINITIALIZED)
		return FALSE;
	return TRUE;
}

void CEnvironmentData::GetFileName(TCHAR *FileNameBuffer, int BufferLength)
{
	strncpy_s(FileNameBuffer, BufferLength, m_szFileName, BufferLength);
}

ENVDATAPOINTCOUNT CEnvironmentData::GetDataPointCounts()
{
	ENVDATAPOINTCOUNT cnt = {0};
	cnt.x = m_nXlen;
	cnt.y = m_nYlen;
	cnt.z = m_nZlen;
	cnt.v = m_nVlen;
	return cnt;
}


ENVMINMAX CEnvironmentData::GetExtremes()
{
	ENVMINMAX mm;

	memset(&mm, 0, sizeof(ENVMINMAX));

	if(IsDataLoaded() == FALSE)
	{
		mm.xMax = 5000000000;
		mm.xMin = -5000000000;
		mm.yMax = 5000000000;
		mm.yMin = -5000000000;
		mm.v1Max = 5000000000;
		mm.v1Min = -5000000000;
		mm.v2Max = 5000000000;
		mm.v2Min = -5000000000;
		return mm;
	}

	mm.xMin = m_minX;
	mm.xMax = m_maxX;

	mm.yMin = m_minY;
	mm.yMax = m_maxY;

	mm.zMin = m_minZ;
	mm.zMax = m_maxZ;

	mm.v1Min = m_minV1;
	mm.v1Max = m_maxV1;

	mm.v2Min = m_minV2;
	mm.v2Max = m_maxV2;

	return mm;
}
