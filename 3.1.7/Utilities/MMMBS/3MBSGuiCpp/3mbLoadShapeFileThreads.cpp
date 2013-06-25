#include "3mbLoadShapeFileThreads.h"
#include "random.h"
#include "3mbGuiFunctions.h"

char *DBFieldTypeToString(int DBFFieldType, char *szBuff, int BuffBytes);
char *ShapeTypeToString(int ShapeType, char *szBuff, int BuffBytes);
BOOL DBFFileFieldCheck(DBFHandle dbfHandle);


//SendMessage(GetDlgItem(hDlg, IDC_), LB_DELETESTRING, (WPARAM)X, (LPARAM)Y);
//SendMessage(GetDlgItem(hDlg, IDC_), LB_SETCURSEL,    (WPARAM)X, (LPARAM)Y);		
//SendMessage(GetDlgItem(hDlg, IDC_), LB_GETCURSEL, (WPARAM)X, (LPARAM)Y);
//SendMessage(GetDlgItem(hDlg, IDC_), LB_RESETCONTENT, (WPARAM)X, (LPARAM)Y);
//SendMessage(GetDlgItem(hDlg, IDC_), LB_INSERTSTRING, (WPARAM)X, (LPARAM)Y);			
// EnableWindow(GetDlgItem(hDlg, IDC_), TRUE or FALSE);
extern HWND g_hDlgSeed;
extern HWND g_hwndBathy;
DWORD WINAPI ShapeFileLoadFromFileThread(LPVOID lpParameter)
{
	SHAPE_FILE_THREAD_PARAM *param = (SHAPE_FILE_THREAD_PARAM *)lpParameter;
	int i;
	TCHAR szBuff0[SIZE_2048];
	TCHAR szBuff1[SIZE_1024];
	TCHAR szBuff2[SIZE_256];
	TCHAR szBuff3[SIZE_64];
	//TCHAR szBuff4[SIZE_64];
	int nEntities, nShapeType;
	int recordCnt, fieldCnt;
	C3mbStaticsLib staticLib;

	double dMinBound[4] = {0}, dMaxBound[4] = {0};
	errno_t err;
	size_t len;

	// Prevent the user from attempting to load a new file while the current file is being loaded.
	EnableWindow(GetDlgItem(param->parentWin, IDC_BUTTON_LOAD_SHAPEFILE), FALSE);


	//----------------------------------------------------------------------------------//
	// Open the shape file
	//--------------------//
	if(NULL == (param->shpHdl = SHPOpen(param->pShapeRef->fileInf.szFileName, "rb")))
	{
		// Report file open error
		sprintf_s(szBuff0, sizeof(szBuff0), "Load Shape File: %s  OPEN SHAPE FILE FAILURE!!!! -Biomimetica", param->pShapeRef->fileInf.szTitleName);
		SetWindowText(param->parentWin, szBuff0);
		sprintf_s(szBuff0, sizeof(szBuff0), "Open File [%s]......FAILED!!!!!!!!!!!!!!!!", param->pShapeRef->fileInf.szFileName);
		SetDlgItemText(param->parentWin, IDC_STATIC_FILENAME, szBuff0);
		goto shutdown_thread;
	}

	//----------------------------------------------------------------------------------//
	// Open the dbs data base file.
	//----------------------------//
	err = strcpy_s(szBuff1, sizeof(szBuff1), param->pShapeRef->fileInf.szFileName);
	len = strnlen_s(szBuff1, sizeof(szBuff1));
	szBuff1[len-4] = 0;
	strcat_s(szBuff1, sizeof(szBuff1), ".dbf");
	if(NULL == (param->dbfHdl = DBFOpen(szBuff1, "rb")))
	{
		sprintf_s(szBuff0, sizeof(szBuff0), "Load Shape File: %s  OPEN .dbs FILE FAILURE!!!! -Biomimetica", param->pShapeRef->fileInf.szTitleName);
		SetWindowText(param->parentWin, szBuff0);
		sprintf_s(szBuff0, sizeof(szBuff0), "Open File [%s]......FAILED!!!!!!!!!!!!!!!!", szBuff1);
		SetDlgItemText(param->parentWin, IDC_STATIC_FILENAME, szBuff0);

		// Close the shape file
		SHPClose(param->shpHdl);
		param->shpHdl = NULL;
		goto shutdown_thread;
	}

	//----------------------------------------------------------------------------------//
	// Get information about the shape and data base file.
	//----------------------------------------------------//
	SHPGetInfo(param->shpHdl, &nEntities, &nShapeType, dMinBound, dMaxBound);
	recordCnt = DBFGetRecordCount(param->dbfHdl);
	fieldCnt = DBFGetFieldCount(param->dbfHdl);

	sprintf_s(szBuff0, "%d", nEntities);
	SetDlgItemText(param->parentWin, IDC_STATIC_NUM_ENTRIES, szBuff0);

	sprintf_s(szBuff0, "(%f, %f)", dMinBound[1], dMinBound[0]);
	SetDlgItemText(param->parentWin, IDC_STATIC_SHAPE_BOUNDS_MIN, szBuff0);
	sprintf_s(szBuff0, "(%f, %f)", dMaxBound[1], dMaxBound[0]);
	SetDlgItemText(param->parentWin, IDC_STATIC_SHAPE_BOUNDS_MAX, szBuff0);


	//----------------------------------------------------------------------------------//
	// Check for invalid file format.
	//-------------------------------//
	szBuff0[0] = 0;
	szBuff1[0] = 0;
	i = 1;
	if(nShapeType != 5)
	{
		sprintf_s(szBuff0, sizeof(szBuff0), "(%d)%s", i++, "Incorrect shape type\r\n");
	}

	if(nEntities < 1)
	{
		strcpy_s(szBuff1, sizeof(szBuff1), szBuff0);
		sprintf_s(szBuff0, sizeof(szBuff0), "%s(%d)Zero entries\r\n", szBuff1, i++);
	}

	if(nEntities != recordCnt)
	{
		strcpy_s(szBuff1, sizeof(szBuff1), szBuff0);
		sprintf_s(szBuff0, sizeof(szBuff0), "%s(%d)Entry count does not equal record count\r\n", szBuff1, i++);
	}
	if(fieldCnt != 13)
	{
		strcpy_s(szBuff1, sizeof(szBuff1), szBuff0);
		sprintf_s(szBuff0, sizeof(szBuff0), "%s(%d)Improper field count in .dbs file for seeding via Inverse Gaussian PDF\r\n", szBuff1, i++);
	}
	if(FALSE==DBFFileFieldCheck(param->dbfHdl))
	{
		strcpy_s(szBuff1, sizeof(szBuff1), szBuff0);
		sprintf_s(szBuff0, sizeof(szBuff0), "%s(%d)Improper .dbs file format for seeding via Inverse Gaussian PDF\r\n", szBuff1, i++);
	}

	if(nShapeType!=5 || nEntities<1 || nEntities!=recordCnt || FALSE==DBFFileFieldCheck(param->dbfHdl))
	{
		// Print the generated error string into the feedback region.
		SetDlgItemText(param->parentWin, IDC_EDIT_LOADSPEFILEFEEDBACK, szBuff0);

		// Report problems
		sprintf_s(szBuff0, "Load Shape File: %s  OPEN SHAPE FILE FAILURE!!!! -Biomimetica", param->pShapeRef->fileInf.szTitleName);
		SetWindowText(param->parentWin, szBuff0);
		sprintf_s(szBuff0, "Read File [%s]......FAILED: File Format Error!!!!!!", param->pShapeRef->fileInf.szFileName);
		SetDlgItemText(param->parentWin, IDC_STATIC_FILENAME, szBuff0);

		// Close files.
		// Shape file handle
		SHPClose(param->shpHdl);
		param->shpHdl = NULL;

		// database file handle.
		DBFClose(param->dbfHdl);
		param->dbfHdl = NULL;
		goto shutdown_thread;
	}


	// Parse out the different field values the user may choose from.
	for(i=0; i<recordCnt && param->threadInf.exit == FALSE; i++)
	{
		//density = DBFReadDoubleAttribute(param->dbfHdl, i, 6);
		//area = DBFReadDoubleAttribute(param->dbfHdl, i, 7);
		//areaCalc = DBFReadDoubleAttribute(param->dbfHdl, i, 8);

		//strncpy_s(szBuff0, sizeof(szBuff0), DBFReadStringAttribute(param->dbfHdl, i, 0), 12); // Source FID
		strncpy_s(szBuff1, sizeof(szBuff1), DBFReadStringAttribute(param->dbfHdl, i, 1), sizeof(szBuff1)-1); // Species 1
		//AddIfNotAdded(&param->pUserChoiceListRef->speName, szBuff1);
		strncpy_s(szBuff2, sizeof(szBuff2), DBFReadStringAttribute(param->dbfHdl, i, 2), sizeof(szBuff2)-2); // Species 2
		//sprintf_s(szBuff0, sizeof(szBuff0), "%s (%s)", szBuff1, szBuff2);
		//AddIfNotAdded(&param->pUserChoiceListRef->speNameDisp, szBuff0);

		AddIfNotAdded(&param->pUserChoiceListRef->spe, szBuff1, szBuff2);

		strncpy_s(szBuff1, sizeof(szBuff1), DBFReadStringAttribute(param->dbfHdl, i, 3), sizeof(szBuff1)-1); // Season
		//AddIfNotAdded(&param->pUserChoiceListRef->season, szBuff1);
		strncpy_s(szBuff2, sizeof(szBuff2), DBFReadStringAttribute(param->dbfHdl, i, 4), sizeof(szBuff2)-1); // Begining Date
		//AddIfNotAdded(&param->pUserChoiceListRef->seasonBeg, szBuff2);
		strncpy_s(szBuff3, sizeof(szBuff3), DBFReadStringAttribute(param->dbfHdl, i, 5), sizeof(szBuff3)-1); // End Date
		//AddIfNotAdded(&param->pUserChoiceListRef->seasonEnd, szBuff3);
		//sprintf_s(szBuff0, sizeof(szBuff0), "%s (%s~%s)", szBuff1, szBuff2, szBuff3);
		//AddIfNotAdded(&param->pUserChoiceListRef->seasonDisp, szBuff0);

		AddIfNotAdded(&param->pUserChoiceListRef->seasons, szBuff1, szBuff2, szBuff3);

		//sprintf_s(szBuff0, sizeof(szBuff0), "%12.5f", DBFReadDoubleAttribute(param->bfHdl, i, 6)); // Density
		//sprintf_s(szBuff0, sizeof(szBuff0), "%12.5f", DBFReadDoubleAttribute(param->dbfHdl, i, 7)); // Area
		//sprintf_s(szBuff0, sizeof(szBuff0), "%12.5f", DBFReadDoubleAttribute(param->dbfHdl, i, 8)); // Area Calc
		strncpy_s(szBuff0, sizeof(szBuff0), DBFReadStringAttribute(param->dbfHdl, i, 9), 12); // Study
		AddIfNotAdded(&param->pUserChoiceListRef->study, szBuff0);
		strncpy_s(szBuff0, sizeof(szBuff0), DBFReadStringAttribute(param->dbfHdl, i, 10), 12); // Stock
		AddIfNotAdded(&param->pUserChoiceListRef->stock, szBuff0);

		//sprintf_s(szBuff0, sizeof(szBuff0), "%12.5f", DBFReadDoubleAttribute(param->dbfHdl, i, 11)); // Depth
		//strncpy_s(szBuff3, sizeof(szBuff3), DBFReadStringAttribute(threadParam->dbfHdl, i, 12), sizeof(szBuff3)-1); // File Path
	}


	for(i=0; i<param->pUserChoiceListRef->spe.Length() && param->threadInf.exit == FALSE; i++)
		SendMessage(GetDlgItem(param->parentWin, IDC_LIST_SPECIES), LB_INSERTSTRING, (WPARAM)i, (LPARAM)param->pUserChoiceListRef->spe.Get(i)->pSzDisp);

	for(i=0; i<param->pUserChoiceListRef->seasons.Length() && param->threadInf.exit == FALSE; i++)
		SendMessage(GetDlgItem(param->parentWin, IDC_LIST_SEASON), LB_INSERTSTRING, (WPARAM)i, (LPARAM)param->pUserChoiceListRef->seasons.Get(i)->pSzDisp);

	for(i=0; i<param->pUserChoiceListRef->study.Length() && param->threadInf.exit == FALSE; i++)
		SendMessage(GetDlgItem(param->parentWin, IDC_LIST_STUDY), LB_INSERTSTRING, (WPARAM)i, (LPARAM)*param->pUserChoiceListRef->study.Get(i));

	for(i=0; i<param->pUserChoiceListRef->stock.Length() && param->threadInf.exit == FALSE; i++)
		SendMessage(GetDlgItem(param->parentWin, IDC_LIST_STOCK), LB_INSERTSTRING, (WPARAM)i, (LPARAM)*param->pUserChoiceListRef->stock.Get(i));


shutdown_thread:
	EnableWindow(GetDlgItem(param->parentWin, IDC_BUTTON_LOAD_SHAPEFILE), TRUE);
	// Shut down this thread.  Threads need to both exit and have CloseHandle() called to fully go away.
	CloseHandle(param->threadInf.hdl);
	param->threadInf.hdl = NULL;
	param->threadInf.running = FALSE;
	return 0;
}


DWORD WINAPI ShapeFileSearchQueryThread(LPVOID lpParameter)
{
	SHAPE_FILE_THREAD_PARAM *param = (SHAPE_FILE_THREAD_PARAM *)lpParameter;
	int i,j;
	const int MAX_NUM_DISPLAY_POLYGONS = 100; // maximum number of polygons to display in the dialog box.
	TCHAR szBuff0[SIZE_2048];
	TCHAR szBuff1[SIZE_1024];
	TCHAR szBuff2[SIZE_256];
	TCHAR szBuff3[SIZE_64];
	TCHAR szBuff4[SIZE_64];
	FILE *fd;
	int hits;
	char *pSzStudyRef = NULL;
	char *pSzStockRef = NULL;
	CListManager <POLYGONEINF> polygonList;// = new CListManager();
	POLYGONEINF *pPolygonRef = NULL;
	double expectedAnimatCnt = 0;
	DUEL_STRING *pSzSpeRef = NULL;
	TRIPLE_STRING *pSzSeasonRef = NULL;
	int nEntities = 0;
	int recordCnt = 0;
	int fieldCnt = 0;
	CListManager <CHARBUFF> listMgr;// = new CListManager();
	int dynamicStringLength = 0;
	char *dynamicString = NULL;
	C3mbStaticsLib staticLib;	 

	// Shape file
	int nShapeType;
	double dMinBound[4] = {0};
	double dMaxBound[4] = {0};
	SHPObject *pShpObject = NULL;
	int totalNullObjects = 0;

	_ASSERT(param->threadInf.running == FALSE);
	param->threadInf.running = TRUE;
	param->threadInf.exit = FALSE;
	//_ASSERT(param->threadInf.exit == FALSE);
	EnableWindow(param->parentWin, TRUE); // prevent input

	//int numPolygons;

	// Prevent the user from attempting to load a new file while the current file is being loaded.
	EnableWindow(GetDlgItem(param->parentWin, IDC_BUTTON_LOAD_SHAPEFILE), FALSE);
	EnableWindow(GetDlgItem(param->parentWin, IDC_BUTTON_PDF_SEED), FALSE);
	EnableWindow(GetDlgItem(param->parentWin, IDC_EDIT_AVE_DIST), FALSE);
	EnableWindow(GetDlgItem(param->parentWin, IDC_STATIC_AVEDIST), FALSE);
	EnableWindow(GetDlgItem(param->parentWin, IDC_EDIT_STD_DIST), FALSE);
	EnableWindow(GetDlgItem(param->parentWin, IDC_STATIC_STDEVDIST), FALSE);


	ShowWindow(GetDlgItem(param->parentWin, IDC_BUTTON_ENVDLG_SPEC_ADD), SW_HIDE);

	//----------------------------------------------------------------------------------//
	// Run debug assertions.
	//----------------------//
	_ASSERT(param->pShapeRef != NULL);
	_ASSERT(param->parentWin != NULL);
	_ASSERT(strlen(param->pShapeRef->fileInf.szFileName) != 0);
	_ASSERT(strlen(param->pShapeRef->fileInf.szTitleName) != 0);
	_ASSERT(param->shpHdl != NULL);
	_ASSERT(param->dbfHdl != NULL);
	//_ASSERT(*param->pMbSpeFileMatchedRef == FALSE);


	*param->pMbSpeFileMatchedRef = FALSE;


	//----------------------------------------------------------------------------------//
	// Get information about the shape and data base file.
	//----------------------------------------------------//
	SHPGetInfo(param->shpHdl, &nEntities, &nShapeType, dMinBound, dMaxBound);
	recordCnt = DBFGetRecordCount(param->dbfHdl);
	fieldCnt = DBFGetFieldCount(param->dbfHdl);

	//----------------------------------------------------------------------------------//
	// Update the Search Query static Text
	//-------------------------------------//
	i = param->LBIndicesCpy.species;
	if(i == -1)
		strncpy_s(szBuff1, sizeof(szBuff1), "", sizeof(szBuff1)-1);
	else
		strncpy_s(szBuff1, sizeof(szBuff1), param->pUserChoiceListRef->spe.Get(i)->pSz1, sizeof(szBuff1)-1);

	// Season
	i = param->LBIndicesCpy.season;
	if(i == -1)
		strncpy_s(szBuff2, sizeof(szBuff2), "", sizeof(szBuff2)-1);
	else
		strncpy_s(szBuff2, sizeof(szBuff2), param->pUserChoiceListRef->seasons.Get(i)->pSzDisp, sizeof(szBuff2)-1);

	// Study
	i = param->LBIndicesCpy.study;
	if(i == -1)
		strncpy_s(szBuff3, sizeof(szBuff3), "", sizeof(szBuff3)-1);
	else
		strncpy_s(szBuff3, sizeof(szBuff3), *param->pUserChoiceListRef->study.Get(i), sizeof(szBuff3)-1);

	// Stock
	i = param->LBIndicesCpy.stock;
	if(i == -1)
		strncpy_s(szBuff4, sizeof(szBuff4), "", sizeof(szBuff4)-1);
	else
		strncpy_s(szBuff4, sizeof(szBuff4), *param->pUserChoiceListRef->stock.Get(i), sizeof(szBuff4)-1);

	sprintf_s(szBuff0, sizeof(szBuff0), "{SPECIES = { %s }, SEASON = { %s }, STUDY = { %s }, STOCK = { %s }}",
		szBuff1, szBuff2, szBuff3, szBuff4);

	SetDlgItemText(param->parentWin, IDC_STATIC_SEARCHQUERY, szBuff0);

	if(param->LBIndicesCpy.species == -1 || param->LBIndicesCpy.season == -1 ||
		param->LBIndicesCpy.study == -1 || param->LBIndicesCpy.stock == -1)
	{
		goto shutdown_thread;
	}


	sprintf_s(szBuff0, sizeof(szBuff0), "%9s   %7s   %9s        %9s    %9s",
		"Polygon", "Density", "Area", "Calc Area", "Vertices (first five)\r\n");
	SetDlgItemText(param->parentWin, IDC_EDIT_LOADSPEFILEFEEDBACK, szBuff0);
	dynamicStringLength = strlen(szBuff0);


	expectedAnimatCnt = 0;
	hits = 0;
	totalNullObjects = 0;
	param->pShapeRef->pdfInf.expectedAnimatCnt = 0;

	Deallocate3MBShapeFileInfStruct(&param->pShapeRef->pdfInf);

	param->pShapeRef->pdfInf.szSpeciesFileName[0] = 0;
	SetDlgItemText(param->parentWin, IDC_STATIC_SPECIES_FILE_NAME, "searching...");
	SetDlgItemText(param->parentWin, IDC_STATIC_SPECIES_FILE_NAME2, "");
	SetDlgItemText(param->parentWin, IDC_STATIC_SPECIES_FILE_NAME3, "");
	SetDlgItemText(param->parentWin, IDC_STATIC_NUM_POLYGONS, "");
	SetDlgItemText(param->parentWin, IDC_STATIC_EXPADDCNT, "");

	dynamicString = NULL;

	pSzSpeRef = param->pUserChoiceListRef->spe.Get(param->LBIndicesCpy.species);
	pSzSeasonRef = param->pUserChoiceListRef->seasons.Get(param->LBIndicesCpy.season);
	//pSzSeasonBegRef = *param->pUserChoiceListRef->seasonBeg.Get(param->LBIndicesCpy.season);
	//pSzSeasonEndRef = *param->pUserChoiceListRef->seasonEnd.Get(param->LBIndicesCpy.season);
	pSzStudyRef = *param->pUserChoiceListRef->study.Get(param->LBIndicesCpy.study);
	pSzStockRef = *param->pUserChoiceListRef->stock.Get(param->LBIndicesCpy.stock);
	for(i=0; i<recordCnt && param->threadInf.exit == FALSE; i++)
	{
		 // Species 1
		if(0 != strcmp(pSzSpeRef->pSz1, DBFReadStringAttribute(param->dbfHdl, i, 1)))
			continue;
		if(0 != strcmp(pSzSpeRef->pSz2, DBFReadStringAttribute(param->dbfHdl, i, 2)))
			continue;
		if(0 != strcmp(pSzSeasonRef->pSz1, DBFReadStringAttribute(param->dbfHdl, i, 3)))
			continue;
		if(0 != strcmp(pSzSeasonRef->pSz2, DBFReadStringAttribute(param->dbfHdl, i, 4)))
			continue;
		if(0 != strcmp(pSzSeasonRef->pSz3, DBFReadStringAttribute(param->dbfHdl, i, 5)))
			continue;
		if(0 != strcmp(pSzStudyRef, DBFReadStringAttribute(param->dbfHdl, i, 9)))
			continue;
		if(0 != strcmp(pSzStockRef, DBFReadStringAttribute(param->dbfHdl, i, 10)))
			continue;


		if(hits == 0)
		{
			_ASSERT(strlen(param->pShapeRef->pdfInf.szSpeciesFileName) == 0);

			// File Path
			sprintf_s(param->pShapeRef->pdfInf.szSpeciesFileName,
				sizeof(param->pShapeRef->pdfInf.szSpeciesFileName),
				"%s", DBFReadStringAttribute(param->dbfHdl, i, 12));

			SetDlgItemText(param->parentWin, IDC_STATIC_SPECIES_FILE_NAME,
				param->pShapeRef->pdfInf.szSpeciesFileName);


			// Possible species file 1
			//strncpy_s(szBuff0, sizeof(szBuff0), param->pShapeRef->pdfInf.szSpeciesFileName, sizeof(szBuff0)-1);
			staticLib.RemoveExtension(param->pShapeRef->pdfInf.szSpeciesFileName);
			strncat_s(param->pShapeRef->pdfInf.szSpeciesFileName,
				sizeof(param->pShapeRef->pdfInf.szSpeciesFileName), ".spe", 4);
			SetDlgItemText(param->parentWin, IDC_STATIC_SPECIES_FILE_NAME2, param->pShapeRef->pdfInf.szSpeciesFileName);

			// Attempt to open the file 1 to see if it exists and can be opened.
			if(0!= fopen_s(&fd, param->pShapeRef->pdfInf.szSpeciesFileName, "rb") || fd == NULL)
			{
				sprintf_s(szBuff0, sizeof(szBuff0), "%s...UNABLE TO LOCATE", param->pShapeRef->pdfInf.szSpeciesFileName);
				SetDlgItemText(param->parentWin, IDC_STATIC_SPECIES_FILE_NAME2, szBuff0);


				// Possible species file 2
				for(j=strlen(param->pShapeRef->pdfInf.szSpeciesFileName)-1; j>0; j--)
				{
					if((char)param->pShapeRef->pdfInf.szSpeciesFileName[j] == '\\')
					{
						param->pShapeRef->pdfInf.szSpeciesFileName[j] = 0;
						break;
					}
				}
				staticLib.RemoveExtension(param->pShapeRef->pdfInf.szSpeciesFileName);
				strncat_s(param->pShapeRef->pdfInf.szSpeciesFileName, sizeof(param->pShapeRef->pdfInf.szSpeciesFileName), ".spe", 4);
				SetDlgItemText(param->parentWin, IDC_STATIC_SPECIES_FILE_NAME3, param->pShapeRef->pdfInf.szSpeciesFileName);


				if(0!= fopen_s(&fd, param->pShapeRef->pdfInf.szSpeciesFileName, "rb") || fd == NULL)
				{
					sprintf_s(szBuff0, sizeof(szBuff0), "%s...UNABLE TO LOCATE", param->pShapeRef->pdfInf.szSpeciesFileName);
					SetDlgItemText(param->parentWin, IDC_STATIC_SPECIES_FILE_NAME3, szBuff0);
					param->pShapeRef->pdfInf.szSpeciesFileName[0] = 0;
				}
			}

			if(fd != NULL)
			{
				staticLib.GetPathAndFileTitleFromFileName(
					param->pShapeRef->pdfInf.szSpeciesFileName,
					NULL, 0, param->pShapeRef->pdfInf.szSpeciesFileTitle,
					sizeof(param->pShapeRef->pdfInf.szSpeciesFileTitle));
				*param->pMbSpeFileMatchedRef = TRUE;
				fclose(fd);
				fd = NULL;
			}

			SetDlgItemText(param->parentWin, IDC_STATIC_NUM_POLYGONS, "counting...");
			SetDlgItemText(param->parentWin, IDC_STATIC_EXPADDCNT, "calculating...");

		}
		else
		{
			_ASSERT(strlen(param->pShapeRef->pdfInf.szSpeciesFileName) >= 0);

			if(i%10 == 0)
			{
				sprintf_s(szBuff0, sizeof(szBuff0), "counting %d...", polygonList.Length());
				SetDlgItemText(param->parentWin, IDC_STATIC_NUM_POLYGONS, szBuff0);

				sprintf_s(szBuff0, sizeof(szBuff0), "calculating %d...", (int)expectedAnimatCnt);
				SetDlgItemText(param->parentWin, IDC_STATIC_EXPADDCNT, szBuff0);
			}
		}

		hits++;

		_ASSERT(pShpObject == NULL);
		pShpObject = SHPReadObject(param->shpHdl, i);
		if(pShpObject == NULL || pShpObject->nSHPType != 5 || pShpObject->nParts != 1||
			pShpObject->panPartStart[0] != 0)
		{
			SHPDestroyObject(pShpObject);
			pShpObject = NULL;
			totalNullObjects++;
			continue;
		}

		

		pPolygonRef = polygonList.Add();
		pPolygonRef->numVertices = pShpObject->nVertices;
		pPolygonRef->vertexBuff = (COORDINATE *)malloc(pPolygonRef->numVertices * sizeof(COORDINATE));
		pPolygonRef->latMax = pShpObject->dfYMax;
		pPolygonRef->lonMax = pShpObject->dfXMax;
		pPolygonRef->latMin = pShpObject->dfYMin;
		pPolygonRef->lonMin = pShpObject->dfXMin;
		
		//------------------------------------------------------------------------------//
		// Read in the verticies and set up display string to display first 5.
		//-------------------------------------------------------------------//
		// Only interested in lat and lon.  Lat and lon are listed in lon-lat (x-y) order,
		// so correct it here to lat-lon.  While five
		// verticies are listed the first and last are identical so only copy four.  Not
		// interested in the Z or M values.
		szBuff1[0] = 0;
		for(j=0; j<pShpObject->nVertices && param->threadInf.exit == FALSE; j++)
		{
			//param->pShapeRef->pdfInf.pPolygonInfBuff
			// Gets saved so the bitmap window may draw the coordinates.
			pPolygonRef->vertexBuff[j].lat = pShpObject->padfY[j];
			pPolygonRef->vertexBuff[j].lon = pShpObject->padfX[j];

			// Only want to display first MAX_NUM_DISPLAY_POLYGONS of valid polygons.
			if(hits <= MAX_NUM_DISPLAY_POLYGONS)
			{
				if(j<5)
				{
					sprintf_s(szBuff2, sizeof(szBuff2), "(%10.5f, %10.5f) ",
						pPolygonRef->vertexBuff[j].lat, pPolygonRef->vertexBuff[j].lon);

					if(strlen(szBuff2) + strlen(szBuff1) + 1 >= sizeof(szBuff1))
						continue;
					strcat_s(szBuff1, sizeof(szBuff1), szBuff2);
				}
			}

		}
		pPolygonRef->density = DBFReadDoubleAttribute(param->dbfHdl, i, 6);
		pPolygonRef->area = DBFReadDoubleAttribute(param->dbfHdl, i, 7);
		pPolygonRef->calculatedArea = DBFReadDoubleAttribute(param->dbfHdl, i, 8);

		expectedAnimatCnt += pPolygonRef->density*pPolygonRef->calculatedArea;

		//sprintf_s(szBuff0, sizeof(szBuff0), "%s\n", "X");
		//size_t zzzzz = strlen(szBuff0);

		if(hits <= MAX_NUM_DISPLAY_POLYGONS)
		{
			sprintf_s(szBuff0, sizeof(szBuff0), "%07d   %7.4f   %9.4f %10.4f %5d   %s\r\n",
				polygonList.Length(), pPolygonRef->density, pPolygonRef->area, pPolygonRef->calculatedArea, pShpObject->nVertices, szBuff1);
			
			//strncat_s(szBuff0, sizeof(szBuff0), szFill, 190-strlen(szBuff0));

			dynamicStringLength += strlen(szBuff0) + 1; // Dymnamic string's length only grows, plus 1 for terminating null character.
			_ASSERT(dynamicString == NULL);
			dynamicString = (char *)malloc(dynamicStringLength);
			GetDlgItemText(param->parentWin, IDC_EDIT_LOADSPEFILEFEEDBACK, dynamicString, dynamicStringLength);
			strncat_s(dynamicString, dynamicStringLength, szBuff0, dynamicStringLength);
			SetDlgItemText(param->parentWin, IDC_EDIT_LOADSPEFILEFEEDBACK, dynamicString);
			free(dynamicString);
			dynamicString = NULL;
		}

		SHPDestroyObject(pShpObject);
		pShpObject = NULL;
	}


	//----------------------------------------------------------------------------------//
	// Copy the information in the local linked list of vertices into the passed in Shape
	// File Information struct
	//------------------------//
	// Start the num poloygon count off at zero.  It will be incremented with each
	// sucessfull copy.
	param->pShapeRef->pdfInf.numPolygons = 0;

	// this is needed though.
	param->pShapeRef->pdfInf.numPolygons = polygonList.Length();
	if(param->pShapeRef->pdfInf.numPolygons > 0)
	{
		param->pShapeRef->pdfInf.pPolygonInfBuff =
			(POLYGONEINF *)malloc(param->pShapeRef->pdfInf.numPolygons * sizeof(POLYGONEINF));
		memset(param->pShapeRef->pdfInf.pPolygonInfBuff, 0, param->pShapeRef->pdfInf.numPolygons*sizeof(POLYGONEINF));
	}
	for(i=0; i<param->pShapeRef->pdfInf.numPolygons && param->threadInf.exit == FALSE; i++)
	{
		// copy the references 
		pPolygonRef = polygonList.Get(i);
		param->pShapeRef->pdfInf.pPolygonInfBuff[i] = *pPolygonRef;

		// Set the reference inside the linked list to NULL.
		pPolygonRef->vertexBuff = NULL;
		pPolygonRef->numVertices = 0;
	}
	//_ASSERT(numPolygons == param->pShapeRef->pdfInf.numPolygons || param->threadInf.exit == TRUE);

	if(hits == 0)
	{
		SetDlgItemText(param->parentWin, IDC_STATIC_SPECIES_FILE_NAME, "No match found");
		SetDlgItemText(param->parentWin, IDC_STATIC_NUM_POLYGONS, "No match found");
		SetDlgItemText(param->parentWin, IDC_STATIC_EXPADDCNT, "No match found");
	}
	else
	{
		if(hits != param->pShapeRef->pdfInf.numPolygons)
		{
			sprintf_s(szBuff0, sizeof(szBuff0), "(%d entries - %d null entries) = %d actual",
				hits, totalNullObjects, param->pShapeRef->pdfInf.numPolygons);
		}
		else
		{
			sprintf_s(szBuff0, sizeof(szBuff0), "%d", hits);
		}

		SetDlgItemText(param->parentWin, IDC_STATIC_NUM_POLYGONS, szBuff0);

		param->pShapeRef->pdfInf.expectedAnimatCnt = (int)staticLib.MyRound(expectedAnimatCnt);
		sprintf_s(szBuff0, sizeof(szBuff0), "%d", param->pShapeRef->pdfInf.expectedAnimatCnt);
		SetDlgItemText(param->parentWin, IDC_STATIC_EXPADDCNT, szBuff0);


		if(*param->pMbSpeFileMatchedRef == TRUE)
			SetDlgItemText(param->parentWin, IDC_BUTTON_ENVDLG_SPEC_ADD, "Confirm 3MB Species File Match");
		else
			SetDlgItemText(param->parentWin, IDC_BUTTON_ENVDLG_SPEC_ADD, "Manually Select 3MB Species File");

		ShowWindow(GetDlgItem(param->parentWin, IDC_BUTTON_ENVDLG_SPEC_ADD), SW_SHOW);



	}

	
	//----------------------------------------------------------------------------------//



	//----------------------------------------------------------------------------------//
	// Deallocate Memory, Close the files
	//-----------------------------------//
shutdown_thread:

	if(param->threadInf.exit == TRUE)
		Deallocate3MBShapeFileInfStruct(&param->pShapeRef->pdfInf);

	for(i=0; i<polygonList.Length(); i++)
	{
		pPolygonRef = polygonList.Get(i);

		_ASSERT((pPolygonRef->numVertices == 0 && pPolygonRef->vertexBuff == NULL) ||
			(pPolygonRef->numVertices != 0 && pPolygonRef->vertexBuff != NULL));

		if(pPolygonRef->vertexBuff != NULL)
			free(pPolygonRef->vertexBuff);
		pPolygonRef->vertexBuff = NULL;
		pPolygonRef->numVertices = 0;
	}
	polygonList.DeleteAll();

	// Shape object (SHPObject) dynamically allocated buffer
	if(pShpObject != NULL)
		SHPDestroyObject(pShpObject);
	pShpObject = NULL;

	listMgr.DeleteAll();


	// Enable the load shape file button.
	EnableWindow(GetDlgItem(param->parentWin, IDC_BUTTON_LOAD_SHAPEFILE), TRUE);
	// Shut down this thread.  Threads need to both exit and have CloseHandle() called to fully go away.
	CloseHandle(param->threadInf.hdl);
	param->threadInf.hdl = NULL;
	param->threadInf.exit = FALSE;
	param->threadInf.running = FALSE;
	return 0;
}



DWORD WINAPI RunPDFThread(LPVOID lpParameter)
{
	char szBuff0[SIZE_512];
	char szBuff1[SIZE_512];
	SHAPE_FILE_THREAD_PARAM *param = (SHAPE_FILE_THREAD_PARAM *)lpParameter;
	GAUSS_CUMULATIVE_PDF_ARRAY pdf;
	LinkedList <int> populationGroupList;
	int animatPoolSize = param->pShapeRef->pdfInf.expectedAnimatCnt;
	int *pGrpSize;
	double randdbl;
	double lower, upper;
	double densitySum = 0;
	int i, n, j;
	int *pIndexMapSubsetBuff = NULL;
	double *pCumltveProbSubsetBuff = NULL;
	double y;
	int bin;
	C3mbStaticsLib staticLib;

	// For seeding
	INHABITINF inhabInf;

	double candidateLat, candidateLon;

	int relationToPolygon;
	BATHYEXTREMES bathyextremes;
	BOOL bVal;
	RECT rect;
	int seedAttemptCnt;
	int polygonAnimatAdd;
	int totalAnimatAdd;
	DWORD tickCnt = GetTickCount();
	int iSpe = -1;
	RESLT res;
	int popIndex;
	double latMin, latMax, lonMin, lonMax;
	COORDINATE *pPolygonRef;
	int numVertices;

	//----------------------//
	// Testing and debugging
	//----------------------//
	//----------------------//


	 // Non-zero indices for buffers based on the non-zero density subset of the polygons.
	int numNonZeroDensityPolygons;


	_ASSERT(param->threadInf.exit == FALSE);
	_ASSERT(param->threadInf.running == FALSE);


	GetClientRect(g_hwndBathy, &rect);

	// Prevent the user from attempting to load a new file while the current file is being loaded.
	EnableWindow(GetDlgItem(param->parentWin, IDC_BUTTON_LOAD_SHAPEFILE), FALSE);
	EnableWindow(GetDlgItem(param->parentWin, IDC_EDIT_AVE_DIST), FALSE);
	EnableWindow(GetDlgItem(param->parentWin, IDC_BUTTON_LOAD_SHAPEFILE), FALSE);
	EnableWindow(GetDlgItem(param->parentWin, IDC_EDIT_STD_DIST), FALSE);
	EnableWindow(GetDlgItem(param->parentWin, IDCANCEL), FALSE);
	EnableWindow(GetDlgItem(param->parentWin, IDC_BUTTON_ENVDLG_SPEC_ADD), FALSE);

	EnableWindow(GetDlgItem(param->parentWin, IDC_LIST_SPECIES), FALSE);
	EnableWindow(GetDlgItem(param->parentWin, IDC_LIST_SEASON), FALSE);
	EnableWindow(GetDlgItem(param->parentWin, IDC_LIST_STUDY), FALSE);
	EnableWindow(GetDlgItem(param->parentWin, IDC_LIST_STOCK), FALSE);


	// Enable the seeding button should the user need to cancel the seeding process.
	SetDlgItemText(param->parentWin, IDC_BUTTON_PDF_SEED, "Cancel PDF Seeding");
	EnableWindow(GetDlgItem(param->parentWin, IDC_BUTTON_PDF_SEED), TRUE);

	param->threadInf.running = TRUE;

	// Generate the cumulative Gaussian Probability Distribution Function.
	_ASSERT(param->mean > 0 &&  param->std > 0);
	pdf = staticLib.GenerateCumInvGaussPDF(param->mean, param->std);
	_ASSERT(pdf.buffLen > 1);

	// Added 2010/11/02 for BU's ESME seeding.
	bathyextremes = param->pShapeRef->pRefSce->GetBathymetryClassRef()->GetExtremes();

	//-------------------------------------------------------------------------------------//
	// Use results of the Cumulative Gaussian Probability Distribution Function to make 
	// groups of population of varying size.
	//-------------------------------------------------------------------------------------//
	_ASSERT(animatPoolSize > 0);
	while(animatPoolSize > 0)
	{
		// Add a group, draw a random number.
		pGrpSize = populationGroupList.Add();
		randdbl = param->p3mbRandomRef->randomperc();

		y = -1.0;

		//LinarInterpolation
		// Less than or equal to on both upper and lower is allowed because linear interpolation
		// is used to determine the actual y value and if it is equal to the end points y will
		// come out equal to the end point using linear interpolation.
		if(pdf.pCumulativeArray[0] <= randdbl && randdbl <= pdf.pCumulativeArray[pdf.buffLen-1])
		{
			// See a 
			for(i=0; i<pdf.buffLen-1; i++)
			{
				if(pdf.pCumulativeArray[i] <= randdbl && randdbl <= pdf.pCumulativeArray[i+1])
				{
					y = staticLib.LinarInterpolation(
						pdf.pCumulativeArray[i],
						pdf.pAnimatCntBin[i],
						pdf.pCumulativeArray[i+1],
						pdf.pAnimatCntBin[i+1],
						randdbl);
					break;
				}
			}
		}
		else if(randdbl < pdf.pCumulativeArray[0])
		{
			// Handles a random number lower than the lowest (unlikely but not impossible).
			// Use linear extrapolation
			_ASSERT(pdf.buffLen > 1);
			y = staticLib.LinarInterpolation(
				pdf.pCumulativeArray[0],
				pdf.pAnimatCntBin[0],
				pdf.pCumulativeArray[1],
				pdf.pAnimatCntBin[1],
				randdbl);
		}
		else if(randdbl > pdf.pCumulativeArray[0])
		{
			_ASSERT(pdf.buffLen > 1);
			// Handles a random number higher than the highest (unlikely but not impossible).
			// Use linear extrapolation
			y = staticLib.LinarInterpolation(
				pdf.pCumulativeArray[pdf.buffLen-2],
				pdf.pAnimatCntBin[pdf.buffLen-2],
				pdf.pCumulativeArray[pdf.buffLen-1],
				pdf.pAnimatCntBin[pdf.buffLen-1],
				randdbl);
		}

		// Make sure Y didn't go negative on a extrapolation.
		_ASSERT(y >= 0);
		if(y < 0)
			y = 0;

		*pGrpSize = (int)staticLib.MyRound(y);

		if(*pGrpSize > animatPoolSize)
			*pGrpSize = animatPoolSize;
		animatPoolSize -= *pGrpSize;
	}


	//----------------------------------------------------------------------------------//
	// Compile (in an index buffer) density polygons that have an animal density greater
	// than zero.
	//-----------//
	pIndexMapSubsetBuff = (int *)malloc(param->pShapeRef->pdfInf.numPolygons * sizeof(int));
	memset(pIndexMapSubsetBuff, -1, param->pShapeRef->pdfInf.numPolygons * sizeof(int));
	numNonZeroDensityPolygons = 0;
	for(i=0; i<param->pShapeRef->pdfInf.numPolygons; i++)
	{
		if(param->pShapeRef->pdfInf.pPolygonInfBuff[i].density == 0)
			continue;
		pIndexMapSubsetBuff[numNonZeroDensityPolygons] = i;
		numNonZeroDensityPolygons++;
	}

	if(numNonZeroDensityPolygons == 0)
	{
		MessageBox(param->parentWin, "No Non-Zero Density Polygons Found", "File Problem", 0);
		goto shutdown_thread;
	}

	//----------------------------------------------------------------------------------//
	// Generate The Cumulative Probability of Occurence
	//--------------------------------------------------//
	// This is a "Guess-at-how-it-works" region and will have to be verified or explained
	// how to do it by NUWC or Dorian.
	//--------------------------------//
	// Allocate an array to hold the cumulative probabilties for each non-zero density
	// polygon
	pCumltveProbSubsetBuff = (double *)malloc(numNonZeroDensityPolygons * sizeof(double));
	memset(pCumltveProbSubsetBuff, 0, numNonZeroDensityPolygons * sizeof(double));

	// First index of a non-zero density polygon.  'n' is an index into the entire polygon
	// buffer that corresponds to 0.  If n equals -1, a programming error was made.
	n = pIndexMapSubsetBuff[0];
	_ASSERT(n != -1);
	densitySum = pCumltveProbSubsetBuff[0] = param->pShapeRef->pdfInf.pPolygonInfBuff[n].density;
	// Remaining non-zero density polygon cumulative probability sum indices.
	for(i = 1; i < numNonZeroDensityPolygons; i++)
	{
		// 'n' is an index into the entire polygon buffer that corresponds to i.
		n = pIndexMapSubsetBuff[i];
		_ASSERT(n != -1);
		pCumltveProbSubsetBuff[i] = pCumltveProbSubsetBuff[i-1] + param->pShapeRef->pdfInf.pPolygonInfBuff[n].density;
		densitySum += param->pShapeRef->pdfInf.pPolygonInfBuff[n].density;
	}

	// Normalize all the non-zero density polygon cumulative probability sums.
	for(i=0; i<numNonZeroDensityPolygons; i++)
		pCumltveProbSubsetBuff[i] /= densitySum;
	_ASSERT(pCumltveProbSubsetBuff[numNonZeroDensityPolygons-1] == 1.0);
	pCumltveProbSubsetBuff[numNonZeroDensityPolygons-1] = 1.0;

	// End of "Guess-at-how-it-works" region
	//----------------------------------------------------------------------------------//

	//----------------------------------------------------------------------------------//
	// Populate the animat groups of varying sizes into the non-zero density polygons.
	//-------------------------------------------------------------------------------//

	// Add the selected species.
	if(OK != (res = param->pShapeRef->pRefSce->AddSpecies(param->speciesFileInf.szFileName, &iSpe)))
	{	
		iSpe = -1;
		sprintf_s(szBuff1, sizeof(szBuff1), "Load Species File \"%s\" Error", param->speciesFileInf.szTitleName);
		MessageBox(param->parentWin, szBuff1, staticLib.MbsResultToString(res, szBuff0, sizeof(szBuff0)), 0);
		goto shutdown_thread;
	}

	strncpy_s(szBuff0, sizeof(szBuff0), param->speciesFileInf.szTitleName, sizeof(szBuff0)-1);
	staticLib.RemoveExtension(szBuff0);
	param->pShapeRef->pRefSce->SetSpeciesDisplayTitle(iSpe, szBuff0);

	// Some initialization.
	polygonAnimatAdd = 0;
	totalAnimatAdd = 0;
	tickCnt = 0;
	SetDlgItemText(param->parentWin, IDC_STATIC_ACTUALCNT, "0");
	SetDlgItemText(param->parentWin, IDC_STATIC_ADD_STATUS, "");



	lower = upper = 0; // quiet compiler warning.
	for(popIndex = 0; popIndex < populationGroupList.Length() && param->threadInf.exit == FALSE; popIndex++)
	{
		// Get the group size to be seeded and draw a random number towards finding a
		// polygon to seed it in.
		pGrpSize = populationGroupList.Get(popIndex);
		randdbl = param->p3mbRandomRef->randomperc();
		_ASSERT(0 <= randdbl && randdbl <=1.0);


		// Find a polygon for population group popIndex.
		bin = 0;
		if(randdbl > pCumltveProbSubsetBuff[0])
		{
			for(n=0; n<numNonZeroDensityPolygons-1 && param->threadInf.exit == FALSE; n++)
			{
				lower = pCumltveProbSubsetBuff[n];
				upper = pCumltveProbSubsetBuff[n+1];
				if(lower <= randdbl && randdbl <= upper)
					break; // found it.
			}
			// Seed non-zero desnity polygon at index bin.
			bin = (int)staticLib.MyRound(staticLib.LinarInterpolation(lower, n, upper, n+1, randdbl));
			_ASSERT(bin == n || bin == n+1);
		}


		// Status update every 1/2 second.
		if(GetTickCount() - tickCnt > 500)
		{
			sprintf_s(szBuff0, sizeof(szBuff0), "Population group %d of %d: %d animats to polygon %d",
				popIndex+1, populationGroupList.Length(), *pGrpSize, pIndexMapSubsetBuff[bin]);
			SetDlgItemText(param->parentWin, IDC_STATIC_ADD_STATUS, szBuff0);

			sprintf_s(szBuff0, sizeof(szBuff0), "%d", totalAnimatAdd);
			SetDlgItemText(param->parentWin, IDC_STATIC_ACTUALCNT, szBuff0);
			tickCnt = GetTickCount();
		}

		// pIndexMapSubsetBuff[] contains the subset of the polygones that have a non-zero
		// density
		n = pIndexMapSubsetBuff[bin];
		latMin = param->pShapeRef->pdfInf.pPolygonInfBuff[n].latMin;
		latMax = param->pShapeRef->pdfInf.pPolygonInfBuff[n].latMax;
		lonMin = param->pShapeRef->pdfInf.pPolygonInfBuff[n].lonMin;
		lonMax = param->pShapeRef->pdfInf.pPolygonInfBuff[n].lonMax;
		pPolygonRef = param->pShapeRef->pdfInf.pPolygonInfBuff[n].vertexBuff;
		numVertices = param->pShapeRef->pdfInf.pPolygonInfBuff[n].numVertices;


		// Find a seeding location for each animat in the group.
		for(j=0; j<*pGrpSize && param->threadInf.exit == FALSE; j++)
		{
			seedAttemptCnt = 0;
			memset(&inhabInf, 0, sizeof(inhabInf));

			while(seedAttemptCnt < 3000 && param->threadInf.exit == FALSE)
			{
				candidateLat = param->p3mbRandomRef->rndreal(latMin, latMax);
				candidateLon = param->p3mbRandomRef->rndreal(lonMin, lonMax);

				// Add animats to scenario around polygon y.
				relationToPolygon = staticLib.InsidePolygon(pPolygonRef, numVertices, candidateLat, candidateLon); 
				if(INSIDE == relationToPolygon)
				{
					inhabInf.coord.lat = candidateLat;
					inhabInf.coord.lon = candidateLon;
					//param->pShapeRef->pRefSce->AddIndividual(iSpe, inhabInf);

					//--------------------------------------------------------------------//
					// Added 2010/11/02 for BU's ESME seeding.
					bVal  = inhabInf.coord.lat <= bathyextremes.xMax;
					bVal &= inhabInf.coord.lat >= bathyextremes.xMin;
					bVal &= inhabInf.coord.lon <= bathyextremes.yMax;
					bVal &= inhabInf.coord.lon >= bathyextremes.yMin;

					if(bVal == TRUE)
					{
						param->pShapeRef->pRefSce->AddIndividual(iSpe, inhabInf);
						totalAnimatAdd++;
					}

#ifdef _DEBUG
					if(bVal != TRUE)
						totalAnimatAdd = totalAnimatAdd;

#endif
					//--------------------------------------------------------------------//

					//totalAnimatAdd++;
					break;
				}
				seedAttemptCnt++;
			}
#ifdef _DEBUG
			if(param->threadInf.exit == FALSE)
				_ASSERT(relationToPolygon == INSIDE && seedAttemptCnt <= 3000);
#endif
		}
	}




	if(param->threadInf.exit == FALSE)
	{
		sprintf_s(szBuff0, sizeof(szBuff0), "All Population groups Added, %d total animats", totalAnimatAdd);
		SetDlgItemText(param->parentWin, IDC_STATIC_ADD_STATUS, szBuff0);
		sprintf_s(szBuff0, sizeof(szBuff0), "%d", totalAnimatAdd);
		SetDlgItemText(param->parentWin, IDC_STATIC_ACTUALCNT, szBuff0);
	}
	else
	{

		if(iSpe >= 0)
		{
			sprintf_s(szBuff0, sizeof(szBuff0), "Removing %d animats and aborting", totalAnimatAdd);
			SetDlgItemText(param->parentWin, IDC_STATIC_ADD_STATUS, szBuff0);
			SetDlgItemText(param->parentWin, IDC_STATIC_ACTUALCNT, "0");
			//param->pShapeRef->pRefSce->DeleteSpecies(iSpe);
		}
		else
		{
			sprintf_s(szBuff0, sizeof(szBuff0), "aborting", totalAnimatAdd);
			SetDlgItemText(param->parentWin, IDC_STATIC_ADD_STATUS, szBuff0);

		}
	}



	// Shut down the thread.
shutdown_thread:

	if(param->threadInf.exit == TRUE && iSpe != -1)
	{
		SendMessage(GetDlgItem(g_hDlgSeed, IDC_LIST_SPECIES), LB_DELETESTRING, (WPARAM)iSpe, (LPARAM)0);
		param->pShapeRef->pRefSce->DeleteSpecies(iSpe);
	}


	if(param->threadInf.exit == FALSE && iSpe != -1)
	{
		UpdateGuiSpeciesListWindow(param->pShapeRef->pRefSce, GetDlgItem(g_hDlgSeed, IDC_LIST_SPECIES) ,-1);
		//PostMessage(GetDlgItem(g_hDlgSeed, IDC_LIST_SPECIES), LB_ADDSTRING, (WPARAM)0,
		//	(LPARAM)param->speciesFileInf.szTitleName);
	}


	staticLib.DeallocateGausianPDFCumulativeBuffStruct(&pdf);

	populationGroupList.DeleteAll();

	if(pCumltveProbSubsetBuff != NULL)
		free(pCumltveProbSubsetBuff);
	pCumltveProbSubsetBuff = NULL;

	if(pIndexMapSubsetBuff != NULL)
		free(pIndexMapSubsetBuff);
	pIndexMapSubsetBuff = NULL;

	SetDlgItemText(param->parentWin, IDC_BUTTON_PDF_SEED, "Seed Via Gauss PDF");
	EnableWindow(GetDlgItem(param->parentWin, IDC_BUTTON_PDF_SEED), TRUE);

	EnableWindow(GetDlgItem(param->parentWin, IDC_BUTTON_LOAD_SHAPEFILE), TRUE);
	EnableWindow(GetDlgItem(param->parentWin, IDC_EDIT_AVE_DIST), TRUE);
	EnableWindow(GetDlgItem(param->parentWin, IDC_BUTTON_LOAD_SHAPEFILE), TRUE);
	EnableWindow(GetDlgItem(param->parentWin, IDC_EDIT_STD_DIST), TRUE);
	EnableWindow(GetDlgItem(param->parentWin, IDCANCEL), TRUE);
	EnableWindow(GetDlgItem(param->parentWin, IDC_BUTTON_ENVDLG_SPEC_ADD), TRUE);

	EnableWindow(GetDlgItem(param->parentWin, IDC_LIST_SPECIES), TRUE);
	EnableWindow(GetDlgItem(param->parentWin, IDC_LIST_SEASON), TRUE);
	EnableWindow(GetDlgItem(param->parentWin, IDC_LIST_STUDY), TRUE);
	EnableWindow(GetDlgItem(param->parentWin, IDC_LIST_STOCK), TRUE);


	CloseHandle(param->threadInf.hdl);
	param->threadInf.hdl = NULL;
	param->threadInf.running = FALSE;
	param->threadInf.exit = FALSE;

	return 0;
}


char *ShapeTypeToString(int ShapeType, char *szBuff, int BuffBytes)
{
	switch(ShapeType)
	{
	case SHPT_NULL:
		sprintf_s(szBuff, BuffBytes, "(NULL)");
		return szBuff;

	//2D Shape Types (pre ArcView 3.x):

	case SHPT_POINT: //Points
		sprintf_s(szBuff, BuffBytes, "2D Points");
		return szBuff;
	case SHPT_ARC: //Arcs (Polylines, possible in parts)
		sprintf_s(szBuff, BuffBytes, "2D Arcs");
		return szBuff;
	case SHPT_POLYGON: //Polygons (possible in parts)
		sprintf_s(szBuff, BuffBytes, "2D Polygons");
		return szBuff;
	case SHPT_MULTIPOINT: //MultiPoint (related points)
		sprintf_s(szBuff, BuffBytes, "2D MultiPoint");
		return szBuff;

	//3D Shape Types (may include "measure" values for vertices):
	case SHPT_POINTZ:
		sprintf_s(szBuff, BuffBytes, "3D Points");
		return szBuff;
	case SHPT_ARCZ:
		sprintf_s(szBuff, BuffBytes, "3D Arc");
		return szBuff;
	case SHPT_POLYGONZ:
		sprintf_s(szBuff, BuffBytes, "3D Polygons");
		return szBuff;
	case SHPT_MULTIPOINTZ:
		sprintf_s(szBuff, BuffBytes, "3D Multi Point");
		return szBuff;

	//2D + Measure Types:

	case SHPT_POINTM:
		sprintf_s(szBuff, BuffBytes, "2D Measure Point");
		return szBuff;
	case SHPT_ARCM:
		sprintf_s(szBuff, BuffBytes, "2D Measure Arch");
		return szBuff;
	case SHPT_POLYGONM:
		sprintf_s(szBuff, BuffBytes, "2D Measure Polygon");
		return szBuff;
	case SHPT_MULTIPOINTM:
		sprintf_s(szBuff, BuffBytes, "2D Measure Point");
		return szBuff;

	//Complex (TIN-like) with Z, and Measure:
	case SHPT_MULTIPATCH:
		sprintf_s(szBuff, BuffBytes, "Complex (TIN-like) with Z, and Measure: Multi Patch");
		return szBuff;
	}
	return "unknown";
}


char *DBFieldTypeToString(int DBFFieldType, char *szBuff, int BuffBytes)
{
	switch(DBFFieldType)
	{
	case FTString:
		sprintf_s(szBuff, BuffBytes, "string");
		return szBuff;
	case FTInteger:
		sprintf_s(szBuff, BuffBytes, "integer");
		return szBuff;
	case FTDouble:
		sprintf_s(szBuff, BuffBytes, "double");
		return szBuff;
	case FTLogical: //Polygons (possible in parts)
		sprintf_s(szBuff, BuffBytes, "logical");
		return szBuff;
	case FTInvalid: //MultiPoint (related points)
		sprintf_s(szBuff, BuffBytes, "invalid");
		return szBuff;
	}
	return "unknown";
}



BOOL DBFFileFieldCheck(DBFHandle dbfHdl)
{
	//int i;
	int fieldCnt;
	DBFFieldType dbfFieldType;
	char szFieldName[13];

	fieldCnt = DBFGetFieldCount(dbfHdl);

	// There should be 13 fields
	if(13 != fieldCnt)
		return FALSE;

	dbfFieldType = DBFGetFieldInfo(dbfHdl, 0, szFieldName, NULL, NULL); //SourceFID (don't care)
	if(strcmp(szFieldName, "SourceFID") != 0)
		return FALSE;

	dbfFieldType = DBFGetFieldInfo(dbfHdl, 1, szFieldName, NULL, NULL); //"Species"
	if(strcmp(szFieldName, "Species") != 0)
		return FALSE;
	dbfFieldType = DBFGetFieldInfo(dbfHdl, 2, szFieldName, NULL, NULL); //Species_2
	if(strcmp(szFieldName, "Species_2") != 0)
		return FALSE;

	dbfFieldType = DBFGetFieldInfo(dbfHdl, 3, szFieldName, NULL, NULL); //Season
	if(strcmp(szFieldName, "Season") != 0)
		return FALSE;
	dbfFieldType = DBFGetFieldInfo(dbfHdl, 4, szFieldName, NULL, NULL); // BegDate
	if(strcmp(szFieldName, "BegDate") != 0)
		return FALSE;
	dbfFieldType = DBFGetFieldInfo(dbfHdl, 5, szFieldName, NULL, NULL); //EndDate
	if(strcmp(szFieldName, "EndDate") != 0)
		return FALSE;


	dbfFieldType = DBFGetFieldInfo(dbfHdl, 6, szFieldName, NULL, NULL); //Density
	if(strcmp(szFieldName, "Density") != 0)
		return FALSE;
	dbfFieldType = DBFGetFieldInfo(dbfHdl, 7, szFieldName, NULL, NULL); // Area
	if(strcmp(szFieldName, "Area") != 0)
		return FALSE;
	dbfFieldType = DBFGetFieldInfo(dbfHdl, 8, szFieldName, NULL, NULL); // Area_Calc
	if(strcmp(szFieldName, "Area_Calc") != 0)
		return FALSE;
	//dbfFieldType = DBFGetFieldInfo(dbfHdl, 9, szFieldName, NULL, NULL); // Study (but NULL)
	dbfFieldType = DBFGetFieldInfo(dbfHdl, 10, szFieldName, NULL, NULL); // Stock
	if(strcmp(szFieldName, "Stock") != 0)
		return FALSE;
	dbfFieldType = DBFGetFieldInfo(dbfHdl, 11, szFieldName, NULL, NULL); // Depth
	if(strcmp(szFieldName, "Depth") != 0)
		return FALSE;
	dbfFieldType = DBFGetFieldInfo(dbfHdl, 12, szFieldName, NULL, NULL); // FilePath
	if(strcmp(szFieldName, "FilePath") != 0)
		return FALSE;

	return TRUE;
}

void ClearUserChoiceLists(USERCHOICELISTS *pListsRef)
{
	int i;
	DUEL_STRING *duel;
	TRIPLE_STRING *trip;

	for(i=0; i<pListsRef->spe.Length(); i++)
	{
		duel = pListsRef->spe.Get(i);

		if(duel->pSzDisp != NULL)
			free(duel->pSzDisp);
		duel->pSzDisp = NULL;


		if(duel->pSz1 != NULL)
			free(duel->pSz1);
		duel->pSz1 = NULL;

		if(duel->pSz2 != NULL)
			free(duel->pSz2);
		duel->pSz2 = NULL;
	}
	pListsRef->spe.DeleteAll();
	
	for(i=0; i<pListsRef->seasons.Length(); i++)
	{
		trip = pListsRef->seasons.Get(i);

		if(trip->pSzDisp != NULL)
			free(trip->pSzDisp);
		trip->pSzDisp = NULL;

		if(trip->pSz1 != NULL)
			free(trip->pSz1);
		trip->pSz1 = NULL;

		if(trip->pSz2 != NULL)
			free(trip->pSz2);
		trip->pSz2 = NULL;

		if(trip->pSz3 != NULL)
			free(trip->pSz3);
		trip->pSz3 = NULL;
	}
	pListsRef->seasons.DeleteAll();

//	for(i=0; i<pListsRef->speName.Length(); i++)
//		free(*pListsRef->speName.Get(i));
//	pListsRef->speName.DeleteAll();

//	for(i=0; i<pListsRef->speNameDisp.Length(); i++)
//		free(*pListsRef->speNameDisp.Get(i));
//	pListsRef->speNameDisp.DeleteAll();

//	for(i=0; i<pListsRef->season.Length(); i++)
//		free(*pListsRef->season.Get(i));
//	pListsRef->season.DeleteAll();

//	for(i=0; i<pListsRef->seasonBeg.Length(); i++)
//		free(*pListsRef->seasonBeg.Get(i));
//	pListsRef->seasonBeg.DeleteAll();

//	for(i=0; i<pListsRef->seasonEnd.Length(); i++)
//		free(*pListsRef->seasonEnd.Get(i));
//	pListsRef->seasonEnd.DeleteAll();

//	for(i=0; i<pListsRef->seasonDisp.Length(); i++)
//		free(*pListsRef->seasonDisp.Get(i));
//	pListsRef->seasonDisp.DeleteAll();


	for(i=0; i<pListsRef->study.Length(); i++)
		free(*pListsRef->study.Get(i));
	pListsRef->study.DeleteAll();

	for(i=0; i<pListsRef->stock.Length(); i++)
		free(*pListsRef->stock.Get(i));
	pListsRef->stock.DeleteAll();

}

BOOL AddIfNotAdded(CListManager <DUEL_STRING> *pListRef, const char *pSzRef1, const char *pSzRef2)
{
	int i;
	char sz[SIZE_256];
	int bytes;
	//char *pSz = NULL;
	DUEL_STRING *strRef;

	sprintf_s(sz, sizeof(sz), "%s (%s)", pSzRef1, pSzRef2);

	// Compare with the last string in the list first so that currently read in strings
	// may be compared with themselves first.
	if(pListRef->Length() > 0)
	{
		strRef = pListRef->Get(pListRef->Length()-1);
		if(0 == strncmp(sz, strRef->pSzDisp, sizeof(sz)))
			return FALSE; // not added
	}

	// Need to check each previoiusly added string.
	for(i=0; i<pListRef->Length(); i++)
	{
		strRef = pListRef->Get(i);
		if(0==strcmp(sz, strRef->pSzDisp))
			return FALSE; // not added
	}
	
	strRef = pListRef->Add();

	// Display string
	bytes = strlen(sz) + 1;
	strRef->pSzDisp = (char *)malloc(bytes);
	strncpy_s(strRef->pSzDisp, bytes, sz, bytes-1);

	// String 1
	bytes = strlen(pSzRef1) + 1;
	strRef->pSz1 = (char *)malloc(bytes);
	strncpy_s(strRef->pSz1, bytes, pSzRef1, bytes-1);

	// String 2
	bytes = strlen(pSzRef2) + 1;
	strRef->pSz2 = (char *)malloc(bytes);
	strncpy_s(strRef->pSz2, bytes, pSzRef2, bytes-1);

	return TRUE; // added
}

BOOL AddIfNotAdded(CListManager <TRIPLE_STRING> *pListRef, const char *pSzRef1, const char *pSzRef2, const char *pSzRef3)
{
	int i;
	char sz[SIZE_256];
	int bytes;
	//char *pSz = NULL;
	TRIPLE_STRING *strRef;

	sprintf_s(sz, sizeof(sz), "%s (%s~%s)", pSzRef1, pSzRef2, pSzRef3);

	// Compare with the last string in the list first so that currently read in strings
	// may be compared with themselves first.
	if(pListRef->Length() > 0)
	{
		strRef = pListRef->Get(pListRef->Length()-1);
		if(0 == strncmp(sz, strRef->pSzDisp, sizeof(sz)))
			return FALSE; // not added
	}

	// Need to check each previoiusly added string.
	for(i=0; i<pListRef->Length(); i++)
	{
		strRef = pListRef->Get(i);
		if(0==strcmp(sz, strRef->pSzDisp))
			return FALSE; // not added
	}
	
	strRef = pListRef->Add();

	// Display string
	bytes = strlen(sz) + 1;
	strRef->pSzDisp = (char *)malloc(bytes);
	strncpy_s(strRef->pSzDisp, bytes, sz, bytes-1);

	// String 1
	bytes = strlen(pSzRef1) + 1;
	strRef->pSz1 = (char *)malloc(bytes);
	strncpy_s(strRef->pSz1, bytes, pSzRef1, bytes-1);

	// String 2
	bytes = strlen(pSzRef2) + 1;
	strRef->pSz2 = (char *)malloc(bytes);
	strncpy_s(strRef->pSz2, bytes, pSzRef2, bytes-1);

	// String 3
	bytes = strlen(pSzRef3) + 1;
	strRef->pSz3 = (char *)malloc(bytes);
	strncpy_s(strRef->pSz3, bytes, pSzRef3, bytes-1);

	return TRUE; // added
}


BOOL AddIfNotAdded(CListManager <char *> *pListRef, const char *pSzAddRef)
{
	int i;
	//char sz[SIZE_256];
	char *pSz;
	char **ppSz;
	int bytes;

	for(i=0; i<pListRef->Length(); i++)
	{
		pSz = *pListRef->Get(i);
		if(0==strcmp(pSz, pSzAddRef))
			return FALSE; // not added
	}

	bytes = strlen(pSzAddRef) + 1; // +1 for terminating null
	pSz = (char *)malloc(bytes); 
	strncpy_s(pSz, bytes, pSzAddRef, bytes-1);
	ppSz = pListRef->Add();
	*ppSz = pSz;
	pSz = NULL;

	return TRUE; // added a string
}


void HandlePreviousDataLoaded()
{
#if 0
		if(strlen(shapeFileInf.szFileName) != 0)
		{

			EnableWindow(GetDlgItem(hDlg, IDC_BUTTON_PDF_SEED), FALSE);

			sprintf_s(szBuff0, sizeof(szBuff0), "Shape File: %s -Biomimetica", shapeFileInf.szTitleName);
			SetWindowText(hDlg, szBuff0);
			sprintf_s(szBuff0, sizeof(szBuff0), "%s (previously loaded shape file)", shapeFileInf.szFileName);
			SetDlgItemText(hDlg, IDC_STATIC_FILENAME, szBuff0);

			SetDlgItemText(hDlg, IDC_STATIC_NUM_ENTRIES, "(unknown)");
			//SetDlgItemText(hDlg, IDC_STATIC_SHAPE_TYPE, "(unknown)");
			SetDlgItemText(hDlg, IDC_STATIC_SHAPE_BOUNDS_MIN, "(unknown)");
			SetDlgItemText(hDlg, IDC_STATIC_SHAPE_BOUNDS_MAX, "(unknown)");
			SetDlgItemText(hDlg, IDC_STATIC_SPECIES_FILE_NAME, "(unknown)");
			SetDlgItemText(hDlg, IDC_STATIC_SPECIES_FILE_NAME2, "(unknown)");
			SetDlgItemText(hDlg, IDC_STATIC_SPECIES_FILE_NAME3, "(unknown)");


			SetDlgItemText(hDlg, IDC_STATIC_NUM_POLYGONS, "(unknown)");
			SetDlgItemText(hDlg, IDC_STATIC_EXPADDCNT, "(unknown)");
			SetDlgItemText(hDlg, IDC_STATIC_SEARCHQUERY, "{ }");

			

			// Fill in the list boxes
			_ASSERT(strlen(pParam->pdfInf.szSpeciesName1And2) != 0);
			_ASSERT(strlen(pParam->pdfInf.szSeason) != 0);
			_ASSERT(strlen(pParam->pdfInf.szStudy) != 0);
			_ASSERT(strlen(pParam->pdfInf.szStock) != 0);
			SendMessage(GetDlgItem(hDlg, IDC_LIST_SPECIES), LB_INSERTSTRING, 0, (LPARAM)pParam->pdfInf.szSpeciesName1And2);
			SendMessage(GetDlgItem(hDlg, IDC_LIST_SEASON), LB_INSERTSTRING, 0, (LPARAM)pParam->pdfInf.szSeason);
			SendMessage(GetDlgItem(hDlg, IDC_LIST_STUDY), LB_INSERTSTRING, 0, (LPARAM)pParam->pdfInf.szStudy);
			SendMessage(GetDlgItem(hDlg, IDC_LIST_STOCK), LB_INSERTSTRING, 0, (LPARAM)pParam->pdfInf.szStock);
			SendMessage(GetDlgItem(hDlg, IDC_LIST_SPECIES), LB_SETCURSEL, (WPARAM)0, (LPARAM)0);
			SendMessage(GetDlgItem(hDlg, IDC_LIST_SEASON), LB_SETCURSEL, (WPARAM)0, (LPARAM)0);
			SendMessage(GetDlgItem(hDlg, IDC_LIST_STUDY), LB_SETCURSEL, (WPARAM)0, (LPARAM)0);
			SendMessage(GetDlgItem(hDlg, IDC_LIST_STOCK), LB_SETCURSEL, (WPARAM)0, (LPARAM)0);

			s_listBoxes.species = s_listBoxes.season = s_listBoxes.study = s_listBoxes.stock = 0;


			// Species file name
			SetDlgItemText(hDlg, IDC_STATIC_SPECIES_FILE_NAME, pParam->pdfInf.szSpeciesFileName);

			// Polygon count
			sprintf_s(szBuff0, sizeof(szBuff0), "%d", pParam->pdfInf.numPolygons);
			SetDlgItemText(hDlg, IDC_STATIC_NUM_POLYGONS, szBuff0);

			// Expected Animat count
			sprintf_s(szBuff0, sizeof(szBuff0), "%d", pParam->pdfInf.expectedAnimatCnt);
			SetDlgItemText(hDlg, IDC_STATIC_EXPADDCNT, szBuff0);


			// Vertices list
			for(i=0; i<pParam->pdfInf.numPolygons; i++)
			{
				
				sprintf_s(szBuff0, sizeof(szBuff0), "Density:%12.10f Area:%7.2 AreaCalc:9.5f",
					pParam->pdfInf.pPolygonInfBuff[i].density, pParam->pdfInf.pPolygonInfBuff[i].area,
					pParam->pdfInf.pPolygonInfBuff[i].calculatedArea);

				//pSz = s_dynamicStr.szBuff;
				s_dynamicStr.len += strlen(szBuff0);
				if(s_dynamicStr.szBuff == NULL)
					 s_dynamicStr.len++;// + 1 for NULL termination.

				_ASSERT(pSz == NULL);
				pSz = (char *)malloc(s_dynamicStr.len);
				if(s_dynamicStr.szBuff == NULL)
				{
					strncpy_s(pSz, s_dynamicStr.len, szBuff0, s_dynamicStr.len-1);
				}
				else
				{
					sprintf_s(pSz, s_dynamicStr.len, "%s\n%s", s_dynamicStr.szBuff, szBuff0);
					free(s_dynamicStr.szBuff);
				}
				s_dynamicStr.szBuff = pSz;
				pSz = NULL;
			}

			SetDlgItemText(hDlg, IDC_EDIT_LOADSPEFILEFEEDBACK, s_dynamicStr.szBuff);
			break;
		}
#endif
}

void Deallocate3MBShapeFileInfStruct(SHAPEFILEINF *pShapeFileInfStruct)
{
	int i;
	POLYGONEINF *pPolygonRef;
	_ASSERT((pShapeFileInfStruct->numPolygons > 0 && pShapeFileInfStruct->pPolygonInfBuff != NULL) ||
		(pShapeFileInfStruct->numPolygons == 0 && pShapeFileInfStruct->pPolygonInfBuff == NULL));
	

	if(pShapeFileInfStruct->pPolygonInfBuff == NULL)
	{
		pShapeFileInfStruct->numPolygons = 0;
		return;
	}

	for(i=0; i<pShapeFileInfStruct->numPolygons; i++)
	{
		pPolygonRef = &pShapeFileInfStruct->pPolygonInfBuff[i];
		_ASSERT(pPolygonRef != NULL);
		_ASSERT((pPolygonRef->numVertices > 0 && pPolygonRef->vertexBuff != NULL) ||
			(pPolygonRef->numVertices == 0 && pPolygonRef->vertexBuff == NULL));

		if(pPolygonRef->vertexBuff != NULL)
			free(pPolygonRef->vertexBuff);
		pPolygonRef->vertexBuff = NULL;
	}
	free(pShapeFileInfStruct->pPolygonInfBuff);
	pShapeFileInfStruct->numPolygons = 0;
	pShapeFileInfStruct->pPolygonInfBuff = NULL;
}