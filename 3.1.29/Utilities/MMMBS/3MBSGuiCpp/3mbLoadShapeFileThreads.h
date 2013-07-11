#ifndef _3MBLOADSHAPEFILETHREADS_H
#define _3MBLOADSHAPEFILETHREADS_H 

#include <windows.h>
#include "3mb.h"
#include "shapefil.h"
#include "dataTypes_general.h"


typedef struct characterBuffer
{
	char sz[SIZE_384];
}CHARBUFF;


typedef struct CoordData
{
	COORDINATE *vertexBuff;
	int numVertices;
	double density;
	double calculatedArea;
	double area;
	double latMax; // y max
	double latMin; // y min
	double lonMax; // x max
	double lonMin; // x min
}POLYGONEINF;

typedef struct ProbabilityDistributionFunctionInf
{
	// Shape file informaition

	// User selection combination from the database.
	char szSpeciesName1And2[SIZE_256];
	char szSeason[SIZE_32];
	char szStudy[SIZE_32];
	char szStock[SIZE_32];

	// Resulting polygon information from user database selection.
	int expectedAnimatCnt;
	char szSpeciesFileName[BUFFERED_MAX_PATH];
	char szSpeciesFileTitle[BUFFERED_MAX_PATH];

	int numPolygons;
	POLYGONEINF *pPolygonInfBuff;
}SHAPEFILEINF; // change name to shape file inf.

// Change name to PDF seeding inf.
typedef struct LoadShapeFileDlgProcessParam
{
	SHAPEFILEINF pdfInf;
	FILE_INFO fileInf;
	CScenario *pRefSce;
	HWND dlgHdl;
}SHAPE_PARAM;

typedef struct ShapeFileListSelection
{
	int species;
	int season;
	int study;
	int stock;
}SHAPEFILE_LISTBOX_INDICES;

typedef struct
{
	char *pSzDisp;
	char *pSz1;
	char *pSz2;
	char *pSz3;
}TRIPLE_STRING;


typedef struct
{
	char *pSzDisp;
	char *pSz1;
	char *pSz2;
}DUEL_STRING;

typedef struct UserChoiceLists 
{
	CListManager <DUEL_STRING> spe;
	CListManager <TRIPLE_STRING> seasons;
	CListManager <char *> study;
	CListManager <char *> stock;
}USERCHOICELISTS;

typedef struct shapeFileThreadParam
{
	THREAD_INF threadInf;
	FILE_INFO speciesFileInf;
	double mean;
	double std;
	SHAPE_PARAM *pShapeRef;
	C3MBRandom *p3mbRandomRef;
	HWND parentWin;
	SHPHandle shpHdl;
	DBFHandle dbfHdl;
	SHAPEFILE_LISTBOX_INDICES LBIndicesCpy;
	USERCHOICELISTS *pUserChoiceListRef;
	BOOL *pMbSpeFileMatchedRef; // Indicates if a species file match was found in the database with an actual 3mb species file on disk.
}SHAPE_FILE_THREAD_PARAM;

typedef struct dynamicString
{
	int len;
	char *szBuff;
}DYNAMICSTRING;


void ClearUserChoiceLists(USERCHOICELISTS *pListsRef);
BOOL AddIfNotAdded(CListManager <char *> *pListRef, const char *pSzAddRef);
BOOL AddIfNotAdded(CListManager <DUEL_STRING> *pListRef, const char *pSzRef1, const char *pSzRef2);
BOOL AddIfNotAdded(CListManager <TRIPLE_STRING> *pListRef, const char *pSzRef1, const char *pSzRef2, const char *pSzRef3);
void Deallocate3MBShapeFileInfStruct(SHAPEFILEINF *pShapeFileInfStruct);
#endif