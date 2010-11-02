#include <math.h>
#include "staticLib.h"


C3mbStaticsLib::C3mbStaticsLib()
{
}
C3mbStaticsLib::~C3mbStaticsLib(){}


	/*
	// Normal

	mean = 15
	std = 3;

	12 ~ 18 @.5
	12.0, 12.5, 13.0, 13.5, 14.0, 14.5, 15.0, 15.5, 16.0, 16.5, 17.0, 17.5, 18.0

	(Mean + NUM_STANDARD_DEVS_OUT*Std)/Resolution) + 1 - (Mean - NUM_STANDARD_DEVS_OUT*Std)/Resolution

	(NUM_STANDARD_DEVS_OUT*Std)/Resolution + 1 + (NUM_STANDARD_DEVS_OUT*Std)/Resolution
	2*(NUM_STANDARD_DEVS_OUT*Std)/Resolution + 1


	// Std takes it below zero
	mean = 2
	std = 3;
	0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 5.5, 6.0, 6.5, 7.0, 7.5, 8.0, 8.5, 9.0, 9.5, 10.0, 10.5, 11.0

	(Mean + NUM_STANDARD_DEVS_OUT*Std)/Resolution + 1 - Resolution;


	Mean = 0.2, Std = 1, Res = 0.5
	Len =  ;                                M
	-X.X, -X.X, -X.X, -X.X, -X.X, -X.X,    0.2  0.7, 1.2, 1.7, 2.2, 2.7, 3.2
	-2.8, -2.3, -1.8, -1.3, -0.8, -0.3,    0.2  0.7, 1.2, 1.7, 2.2, 2.7, 3.2

	upper: (Mean + NUM_STANDARD_DEVS_OUT*Std) = (0 + 3*1) =  3.0
	lower: (Mean - NUM_STANDARD_DEVS_OUT*Std) = (0 - 3*1) = -3.0
	if(lower < 0)
		Lower = Resolution
		lower = 0.5

	(Upper - lower) =
	(Upper - lower)/resolution = array size.



	Mean = 0, Std = 1, Res = 0.5
	Len = 6;                              M
	-X.X, -X.X, -X.X, -X.X, -X.X, -X.X, -X.X    0.5, 1.0, 1.5, 2.0, 2.5, 3.0
	-3.0, -2.5, -2.0, -1.5, -1.0, -0.5, -0.0    0.5, 1.0, 1.5, 2.0, 2.5, 3.0

	upper: (Mean + NUM_STANDARD_DEVS_OUT*Std) = (0 + 3*1) =  3.0
	lower: (Mean - NUM_STANDARD_DEVS_OUT*Std) = (0 - 3*1) = -3.0
	if(lower < 0)
		Lower = Resolution
		lower = 0.5

	(Upper - lower) =
	(Upper - lower)/resolution = array size.




	Mean = 0.5, Std = 1, Res = 0.5
	Len = 7                                 M
	-X.X, -X.X, -X.X, -X.X, -X.X, -X.X,    0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5
	-2.5, -2.0, -1.5, -1.0, -0.5, -0.0,    0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5

	upper: (Mean + NUM_STANDARD_DEVS_OUT*Std) =
	lower: (Mean - NUM_STANDARD_DEVS_OUT*Std) =
	if(lower < 0)
		Lower = Resolution

	(Upper - lower) =
	(Upper - lower)/resolution = array size.


	Mean = 1.0, Std = 1, Res = 0.5
	Len = 8                                M
	-X.X, -X.X, -X.X, -X.X, -X.X,    0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5 4.0
	-2.0, -1.5, -1.0, -0.5, -0.0,    0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5 4.0


	--------------------------------------------------------------------------------------

	Mean = 3.0, Std = 1, Res = 0.5
	Len = 12                        M
	-X.X, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5 4.0 4.5, 5.0, 5.5, 6.0
	-0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5 4.0 4.5, 5.0, 5.5, 6.0
	       0    1    2    3    4    5    6   7   8    9    10   11 
	upper: (Mean + NUM_STANDARD_DEVS_OUT*Std) = 3.0+3 = 6.0;
	lower: (Mean - NUM_STANDARD_DEVS_OUT*Std) = 3.0-3 = 0.0;
	if(lower < 0)
		Lower = Resolution

	(Upper - lower) = 6.0 - 0.5 = 5.5
	(Upper - lower)/resolution = array size 11.
	if(mean > 0)
		Array size++ = 12 
	--------------------------------------------------------------------------------------

	Mean = 3.3, Std = 1, Res = 0.5
	Len = 12                       M
	0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5 4.0 4.5, 5.0, 5.5, 6.0 6.5
	0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5 4.0 4.5, 5.0, 5.5, 6.0 6.5
	 0    1    2    3    4    5    6   7   8    9    10   11  12

	upper val: (Mean + NUM_STANDARD_DEVS_OUT*Std) = 3.5+3 = 6.5;
	lower val: (Mean - NUM_STANDARD_DEVS_OUT*Std) = 3.5-3 = 0.5;
	(Upper - lower) = 6.0
	(Upper - lower)/resolution = array size 12
	*/

void C3mbStaticsLib::DeallocateGausianPDFCumulativeBuffStruct(GAUSS_CUMULATIVE_PDF_ARRAY *pRefGaussCumPdfArray)
{
	if(pRefGaussCumPdfArray->pAnimatCntBin != NULL)
		free(pRefGaussCumPdfArray->pAnimatCntBin);
	pRefGaussCumPdfArray->pAnimatCntBin = NULL;

	if(pRefGaussCumPdfArray->pCumulativeArray != NULL)
		free(pRefGaussCumPdfArray->pCumulativeArray);
	pRefGaussCumPdfArray->pCumulativeArray = NULL;

	pRefGaussCumPdfArray->buffLen = 0;
}

GAUSS_CUMULATIVE_PDF_ARRAY C3mbStaticsLib::GenerateCumInvGaussPDF(double Mean, double Std)
{
	// Calling routine must deallocate memory.
	int n;
	double val = 0;
	double x; // x population
	const int numStd = INVERSEGAUSSIANPDF_MAX_STD_OUT;
	double res = INVERSEGAUSSIANPDF_RESOLUTION;
	int initLen = 0;
	double *pBuffX = NULL;
	double *pBuffY = NULL;
	GAUSS_CUMULATIVE_PDF_ARRAY pdf = {0};


	if(Mean == 0 || Std == 0)
		return pdf;

	// Make sure the resolution is less than the standard deviation by a factor of 4.
	if(res >= Std/4)
		res = Std/4;

	// Guess a resonable initial length and allocate memory.  Set the starting value.
	initLen = (int)(2*Std*numStd/res + 1); // +1 for mean value
	_ASSERT(initLen > 0);
	if(initLen == 0)
		initLen = 1;
	pBuffX = (double *)malloc(initLen * sizeof(double));
	pBuffY = (double *)malloc(initLen * sizeof(double));
	memset(pBuffX, 0, initLen * sizeof(double));
	memset(pBuffY, 0, initLen * sizeof(double));

	x = Mean - Std*numStd;

	//-----------------------//
	// Set initial conditions.
	//-----------------------//
	// Adjust x (if needed so it is positive and greater than 0) and determine initial y.
	while(x <= 0)
		x += res;
	n = 0;
	val = InverseGaussianPropabilityDistributionFunction(Mean, Std, x);
	pBuffX[n] = x;
	pBuffY[n] = val;

	// Additional values
	for(n=1; n<initLen && val<1.0 && x <= Mean + numStd*Std; n++)
	{
		x += res;
		val = InverseGaussianPropabilityDistributionFunction(Mean, Std, x)*res;
		pBuffX[n] = x;
		_ASSERT(n > 0);
		pBuffY[n] = pBuffY[n-1]+ val;
		// for the for-loop comparison
		val = pBuffY[n];
	}

	pdf.buffLen = n;
	if(pdf.buffLen != initLen)
	{
		pdf.pCumulativeArray = (double *)malloc(n * sizeof(double));
		pdf.pAnimatCntBin = (double *)malloc(n * sizeof(double));
	}
	memcpy(pdf.pCumulativeArray, pBuffY, (n * sizeof(double)));
	memcpy(pdf.pAnimatCntBin, pBuffX, (n * sizeof(double)));

	if(pBuffY != NULL)
		free(pBuffY);
	if(pBuffX != NULL)
		free(pBuffX);
	return pdf;
}


double C3mbStaticsLib::InverseGaussianPropabilityDistributionFunction(double Mean, double Std, double X)
{
	double u = Mean; //  u for mu
	double lam; // lam for lamda

	lam = pow(Mean, 3)/pow(Std, 2);

	if(X == 0 || Mean == 0 || Std == 0)
		return 0;

	return pow((lam/(2*PI*pow(X,3))),0.5) * exp((-lam*pow((X-u),2))/(2*pow(u,2)*X));
}


double C3mbStaticsLib::LinarInterpolation(double X1, double Y1, double X2, double Y2, double x)
{
	double m = (Y2-Y1)/(X2-X1);
	double y = Y1 + m*(x - X1);
	return y;
}

ENVMINMAX C3mbStaticsLib::BathyExtremesToEnvExtemes(BATHYEXTREMES Bathy)
{
	ENVMINMAX env;

	memset(&env, 0, sizeof(env));
	env.xMin = Bathy.xMin;
	env.xMax = Bathy.xMax;
	env.yMin = Bathy.yMin;
	env.yMax = Bathy.yMax;
	//env.zMin;
	//env.zMax;
	env.v1Min = Bathy.depthMin;
	env.v1Max = Bathy.depthMax;
	//env.v2Min = ;
	//env.v2Max = ;
	return env;
}




BOOL C3mbStaticsLib::HasAnExtensionType(const TCHAR *szExtension, const TCHAR *szFileName)
{
	TCHAR szBuff[SIZE_16];
	TCHAR szExtBuff[SIZE_16];

	if(szExtension == NULL || szFileName == NULL)
		return FALSE;

	if(strlen(szExtension) == 4) // for .ext
		strcpy_s(szExtBuff, szExtension);
	else if(strlen(szExtension) == 3) // for no dot (ext)
		sprintf_s(szExtBuff, sizeof(szExtBuff), ".%s", szExtension);
	else
		return FALSE;

	GetExtension(szFileName, szBuff, TCHARBFLEN(szBuff));
	if(0 == strcmp(szBuff, szExtBuff))
		return TRUE;
	return FALSE;
}

BOOL C3mbStaticsLib::HasAnyExtension(const TCHAR *szFileName)
{
	TCHAR szBuff[SIZE_16];

	if(szFileName == NULL)
		return FALSE;

	GetExtension(szFileName, szBuff, TCHARBFLEN(szBuff));
	if(0 == strlen(szBuff))
		return FALSE;
	return TRUE;;
}




void C3mbStaticsLib::PrintScenarioExecutionFeedback(SCEACTIVITY Activity, int Animat, int Iteration, int TotalAnimats, int TotalDuration)
{
	static int pinwheelcnt = 0;

	Animat++; // to account of zero indexing;
	switch(Activity)
	{
	case __RUN_FINISHED:
		printf("Idle %s                                         \r", PINWHEEL[pinwheelcnt]);
		break;
	case ___ALLOCOUTPUTBUFF: // allocating memory for animat output to file buffer
		printf("Allocating output buffer %s                     \r", PINWHEEL[pinwheelcnt]);
		break;
	case ___SCE_INIT: // initializing scenaro
		printf("Initializing scenario %s                        \r", PINWHEEL[pinwheelcnt]);
		break;
	case ___SCE_INITANIMATS: // looping through each animat to initialize them
		printf("Initializing animats %s                         \r", PINWHEEL[pinwheelcnt]);
		break;
	case ___SCE_RUNITERATING: // Iterating
		printf("Iteration %d of %d, Animat %5d of %d\r", Iteration, TotalDuration, Animat, TotalAnimats);
		break;
	case ___SCE_RUNBUFFERFLUSH: // flushing output buffer to file
		printf("Flushing output buffer %s                       \r", PINWHEEL[pinwheelcnt]);
		break;
	case ___SCE_PAUSED: // running, but paused waiting on calling application input
		printf("Paused %c                                       \r", PINWHEEL[pinwheelcnt]);
		break;
	}
	++pinwheelcnt;
	pinwheelcnt %= 4;
}
void C3mbStaticsLib::PrintExtractionFeedback(EXTRACTORACTIVITY Activity, int Animat, int Iteration, BOOL ByTime, int TotalAnimats, int TotalDuration)
{
	static int pinwheelcnt = 0;
	Animat++; // to account of zero indexing;

	switch(Activity)
	{
	case EXTRACTOR_IDLE_PRERUN:
		printf("Data extraction at pre-run idle... %s                      \r", PINWHEEL[pinwheelcnt]);
		break;
	case EXTRACTOR_INIT: // allocating memory for animat output to file buffer
		printf("Data extraction initializing... %s                      \r", PINWHEEL[pinwheelcnt]);
		break;
	case EXTRACTOR_EXTRACTING: // initializing scenaro
		if(ByTime == FALSE)
			printf("Extracting animat %5d of %d, iteration %d of %d\r", Animat, TotalAnimats, Iteration, TotalDuration);
		else
			printf("Extracting iteration %d of %d, animat %5d of %d\r", Iteration, TotalDuration, Animat, TotalAnimats);
		break;
	case EXTRACTOR_IDLE_POSTRUN: // looping through each animat to initialize them
		printf("Data extraction finished  %s                               \r", PINWHEEL[pinwheelcnt]);
		break;
	}
	++pinwheelcnt;
	pinwheelcnt %= 4;
}



// Returns TRUE if a matching file is found.  Returns FALSE otherwise.  Calling process must close the handle.
BOOL C3mbStaticsLib::MyFindFile(FINDINF *FindInf)
{
	if(INVALID_HANDLE_VALUE == FindInf->hdl || FindInf->hdl == NULL)
	{
		if(INVALID_HANDLE_VALUE == (FindInf->hdl = FindFirstFileEx(FindInf->sz, gFXIL, &FindInf->data, gFXSO, NULL, 0)))
			return FALSE; // no file found.
		return TRUE;
	}
	return FindNextFile(FindInf->hdl, &FindInf->data);
}


RESLT C3mbStaticsLib::MyDeleteAllFilesInDirectory(TCHAR *szDirectory, TCHAR *szFileType)
{
	FINDINF fndInf = {0};
	RESLT res = OK;

	strcpy_s(fndInf.sz, sizeof(fndInf.sz), szFileType);

	// Set the directory to the one that is to have its files deleted.
	if(FALSE == SetCurrentDirectory(szDirectory))
		return SET_DIRECTORY_ERROR;

	// Delete each file of type 'szFileType' found.
	while(TRUE == MyFindFile(&fndInf))
	{
		if(FALSE == DeleteFile(fndInf.data.cAlternateFileName))
			res = DELETE_FILE_ERROR;
	}

	// Close the handle to the FindFile.
	FindClose(fndInf.hdl);
	fndInf.hdl = NULL;

	// Return if there was a problem deleting files.
	if(res != OK)
		return res;

	// Return to the parent directory.
	if(FALSE== SetCurrentDirectory(".."))
		return SET_DIRECTORY_ERROR;

	return OK;
}

RESLT C3mbStaticsLib::IsFolderEmpty(TCHAR *szDirectory, BOOL *Result)
{
	FINDINF fndInf = {0};
	*Result = FALSE;

	strcpy_s(fndInf.sz, sizeof(fndInf.sz), "*.*");

	// Set the directory to the one that is to have its files deleted.
	if(FALSE == SetCurrentDirectory(szDirectory))
		return SET_DIRECTORY_ERROR;

	// See if any file exists in the folder.
	*Result = !MyFindFile(&fndInf);

	// Close the handle to the FindFile.
	FindClose(fndInf.hdl);
	fndInf.hdl = NULL;

	// Return to the parent directory.
	if(FALSE == SetCurrentDirectory(".."))
		return SET_DIRECTORY_ERROR;

	return OK;
}


BOOL C3mbStaticsLib::FolderExists(TCHAR *szFolder)
{
	FINDINF findInf;
	BOOL bRes = OK;

	// Initialize the FINDINF struct
	memset(&findInf, 0, sizeof(FINDINF));
	strcpy_s(findInf.sz, sizeof(findInf.sz), szFolder);

	// Determine if the folder exists
	bRes = MyFindFile(&findInf);

	// Close the handle to the find directory
	if(findInf.hdl != NULL)
		FindClose(findInf.hdl);

	// Return the results
	return bRes;
}


RESLT C3mbStaticsLib::MyDeleteFolder(TCHAR *szFolder)
{
	FINDINF findInf;
	RESLT res = OK;
	BOOL bRes = TRUE;

	// Initialize the FINDINF struct
	memset(&findInf, 0, sizeof(FINDINF));

	// Copy the folder name.
	strcpy_s(findInf.sz, sizeof(findInf.sz), szFolder);

	// Determine if the folder exists
	if(FALSE == MyFindFile(&findInf))
		res = INVALID_DIRECTORY; // Folder does not exist.	

	//----------------------------------------------------------------------------------//
	// Folder must be empty to delete.
	//-------------------------------//
	// Delete all track (.trk) files
	if(res == OK && FALSE == MyDeleteAllFilesInDirectory(szFolder, "*.trk"))
		res = DELETE_FILE_ERROR;

	// Delete all take (.sts) files.
	if(res == OK && FALSE == MyDeleteAllFilesInDirectory(szFolder, "*.sts"))
		res = DELETE_FILE_ERROR;

	// Verify the folder is empty.
	if(res == OK)
		res = IsFolderEmpty(szFolder, &bRes);
	if(res == OK && bRes != TRUE)
		res = DIRECTORY_NOT_EMPTY_TO_DELETE;
	//----------------------------------------------------------------------------------//

	// Folder is now empty.  Attempt to delete it.
	if(res == OK && FALSE == RemoveDirectory(findInf.sz))
		res = DELETE_DIRECTORY_ERROR;

	// Close the handle to the find directory
	if(findInf.hdl != NULL)
		FindClose(findInf.hdl);
	return res;
}


double C3mbStaticsLib::MyRound(double Val)
{
	if(Val - floor(Val) >= 0.5)
		Val = (float)ceil(Val);
	else
		Val = (float)floor(Val);
	return Val;
}

BOOL C3mbStaticsLib::ClockTimeIsWithin(int ClockTime, double StartClockFractionHrs, double EndClockFractionHrs)
{
	int startClk = Time_To24HrClockSeconds((int)floor(StartClockFractionHrs * 60 * 60)); // in seconds
	int endClk = Time_To24HrClockSeconds((int)floor(EndClockFractionHrs * 60 * 60));
	int clockTime = Time_To24HrClockSeconds(ClockTime);

	if(startClk == endClk && clockTime == startClk)
	{
		return TRUE;
	}
	else if(startClk < endClk)
	{
		// Normal case.
		if(startClk <= clockTime && clockTime < endClk)
			return TRUE;
	}
	else
	{
		// Case where time span crosses over midnight from one day into the next
		if(startClk <= clockTime && clockTime < 24 * 60 * 60 || 0 <= clockTime && clockTime < endClk)
			return TRUE;
	}
	return FALSE;
}


TCHAR *C3mbStaticsLib::MallocClearedBuffer(size_t Size)
{
	TCHAR *sz = (TCHAR *)malloc(Size);
	if(sz == NULL)
		return NULL;
	memset(sz, 0, Size);
	return sz;
}


TCHAR *C3mbStaticsLib::MemoryValueToString(DWORDLONG Value, TCHAR *szBuffer, DWORD BufferLen)
{
	if(Value >= GIGA)
	{
		Value /= 1024;
		Value /= 1024;
		Value /= 1024;

		sprintf_s(szBuffer, BufferLen, "%u GB", Value);
	}
	else if(Value >= MEGA)
	{
		Value /= 1024;
		Value /= 1024;

		sprintf_s(szBuffer, BufferLen, "%u MB", Value);
	}
	else if(Value >= KILO)
	{
		Value /= 1024;
		sprintf_s(szBuffer, BufferLen, "%u KB", Value);
	}
	else
		sprintf_s(szBuffer, BufferLen, "%u bytes", Value);

	return szBuffer;
}

TCHAR *C3mbStaticsLib::MemoryValueToString_bytes(DWORDLONG Value, TCHAR *szBuffer, DWORD BufferLen)
{
	DWORDLONG twoGiga = GIGA;
	twoGiga *= 2;

	if(Value > twoGiga)
		return MemoryValueToString(Value, szBuffer, BufferLen);
	sprintf_s(szBuffer, BufferLen, "%u bytes", Value);
	return szBuffer;
}


TCHAR *C3mbStaticsLib::MemoryValueToString_f(DWORDLONG Value, TCHAR *szBuffer, DWORD BufferLen)
{
	double v;
	double div;
	if(Value >= GIGA)
	{
		div = (double)GIGA;
		v = ((double)Value)/div;
		sprintf_s(szBuffer, BufferLen, "%.2f GB", v);
	}
	else if(Value >= MEGA)
	{
		div = (double)MEGA;
		v = ((double)Value)/div;
		sprintf_s(szBuffer, BufferLen, "%.2f MB", v);
	}
	else if(Value >= KILO)
	{
		div = (double)KILO;
		v = ((double)Value)/div;
		sprintf_s(szBuffer, BufferLen, "%.2f KB", v);
	}
	else
		return MemoryValueToString(Value, szBuffer, BufferLen);


	return szBuffer;
}



DWORD C3mbStaticsLib::MySetFilePointer(HANDLE Hdl, __int64 NumBytesToMove, DWORD MoveMethod)
{
	//__int64 filePointer		= __int64(SizeOf) * __int64(NumberOf);
	long    filePointerLow	= long(NumBytesToMove & 0x00000000ffffffff);
	long    filePointerHigh	= long((NumBytesToMove >> 32) & 0x00000000ffffffff);
	return SetFilePointer(Hdl, filePointerLow, &filePointerHigh, MoveMethod);
}

double C3mbStaticsLib::AddAngles(double AngleDeg1, double AngleDeg2)
{
	return AddAngles(1, AngleDeg1, 1, AngleDeg2);
}


double C3mbStaticsLib::AddAngles(double Weight1, double AngleDeg1, double Weight2, double AngleDeg2)
{

	double x1,y1,x2,y2, angRad1, angRad2;

	angRad1 = AngleDeg1*PI/180;
	angRad2 = AngleDeg2*PI/180;

	x1 = cos(angRad1);
	y1 = sin(angRad1);
	x2 = cos(angRad2);
	y2 = sin(angRad2);

	return atan2((Weight1*y1 + Weight2*y2),(Weight1*x1 + Weight2*x2)) * 180/PI;
	
/*
atan2((b1 + b2), (a1 + a2)) * 180/pi

atan2((b1 + 2*b2), (a1 + 2*a2)) * 180/pi

atan2((b1 + 1.01*b2), (a1 + 1.01*a2)) * 180/pi

*/

}

double C3mbStaticsLib::KeepWithin360(double Heading)
{
	while(Heading >= 360.0)
		Heading -= 360.0;
	while(Heading < 0)
		Heading += 360.0;
	return Heading;
}


BOOL C3mbStaticsLib::StringIsALegalNumber(TCHAR *szString)
{
	TCHAR *pC;
	int strLen;
	BOOL decimalPointEncountered = FALSE;

	if(szString == NULL)
		return FALSE;

	strLen = strlen(szString);
	if(strLen == 0)
		return FALSE;

	// Remove white spaces at the end of the string.
	pC = &szString[strLen-1];
	while(*pC == 32 && strLen > 0)
	{
		szString[strLen-1] = NULL;
		strLen--;
		pC--;
	}

	// After removing the white spaces verify there is something left of the string.
	if(strLen == 0)
		return FALSE;

	// Set the character pointer to point to the begining of the string.
	pC = &szString[0];

	// Skip past any white spaces in the begining of the string.
	while(*pC == 32)
		pC++;

	// Skip past the negative sign at the front, the only one allowed
	if(*pC == '-')
		pC++;


	// Verify remaining characters are valid.  Only a single decimal point allowed.
	do
	{
		if(*pC >= 48 && *pC <=57)
		{
			continue;
		}
		else if(*pC == '.' && decimalPointEncountered == FALSE)
		{
			decimalPointEncountered = TRUE;
			continue;
		}
		return FALSE; // Invalid character encountered.
	}while(*(++pC) != NULL);

	return TRUE;
}

#pragma message("examine test inputs for GetPathAndFileTitleFromFileName()")
#pragma message("Have GetPathAndFileTitleFromFileName() return a result")
// 
// m
// m.ext 
// m:
// m:\
// m:\test\
// m:\test
// m:\test.ext
// m:\test\dog
// m:\test\dog.ext
// m:\test.dog.ext, etc...
void C3mbStaticsLib::GetPathAndFileTitleFromFileName(const TCHAR *szFileName, TCHAR *szFilePathBuffer, int PathBuffLen,
									 TCHAR *szFileTitleBuffer, int TitleBuffLen)
{
	int index;
	int strLen = strlen(szFileName);
	TCHAR fileCopy[SIZE_256];


	// Clear out the buffers passed in and the one declared here.
	if(szFilePathBuffer != NULL)
		memset(szFilePathBuffer, 0, PathBuffLen);
		
	if(szFileTitleBuffer != NULL)
		memset(szFileTitleBuffer, 0, TitleBuffLen);
	memset(fileCopy, 0, SIZE_256);


	// Check for possible input erros.
	if(szFileName == NULL || strLen > SIZE_256-1 || strLen == 0)
		return;


	// Set the index for the end of the string and copy the szFileName string into fileCopy.
	index = strLen - 1;
	strcpy_s(fileCopy, sizeof(fileCopy), szFileName);

	// Back up the index until first '\' (92). or 0th index.
	while(index >= 0)
	{
		if(szFileName[index] == 92)
			break;
		index--;
	}

	// Set szFilePathBuffer and title based upon the outcome of index.
	if(index == -1)
	{
		// 0
		// \mmmb.in
		if(szFileTitleBuffer != NULL)
			strncpy_s(szFileTitleBuffer, TitleBuffLen, &fileCopy[index+1], strlen(&fileCopy[index+1]));
		if(szFilePathBuffer != NULL)
			GetCurrentDirectory(PathBuffLen, szFilePathBuffer);
	}
	else if(index == 0)
	{

		// 0
		// mmmb.in
		if(szFileTitleBuffer != NULL)
			strncpy_s(szFileTitleBuffer, TitleBuffLen, &fileCopy[index], strlen(&fileCopy[index+1]));
		if(szFilePathBuffer != NULL)
			GetCurrentDirectory(PathBuffLen, szFilePathBuffer);
	}
	else // szFileName[index] equals 92 ('\').
	{
		// 0
		// d:\mmmb.in
		// d:\MMMBS\esme\mmmb.in
		// f\MMMBS\esme\mmmb.in
		// MMMBS\esme\mmmb.in
		// \MMMBS\esme\mmmb.in
		if(szFileTitleBuffer != NULL)
			strncpy_s(szFileTitleBuffer, TitleBuffLen, &fileCopy[index+1], strlen(&fileCopy[index+1]));
		fileCopy[index] = 0; // put in a terminating zero.
		if(szFilePathBuffer != NULL)
			strncpy_s(szFilePathBuffer, PathBuffLen, fileCopy, strlen(fileCopy)); 
	}
}


// Time Functions
int C3mbStaticsLib::Time_ToSeconds(HHMMSS HrMinSec)
{
	if(HrMinSec.hour < 0)
		HrMinSec.hour = 0;
	if(HrMinSec.min < 0)
		HrMinSec.min = 0;
	if(HrMinSec.sec < 0)
		HrMinSec.sec = 0;
	return HrMinSec.hour * 3600 + HrMinSec.min * 60 + HrMinSec.sec;
}

DDHHMMSS C3mbStaticsLib::Time_ToDayHrMinSec(int Seconds)
{
	DDHHMMSS absoluteDHMS;
	int numSecondsPerDay = 60*60*24;

	// If a negative time entered, convert to a positive time.
	while(Seconds < 0 )
		Seconds += numSecondsPerDay;

	// Determine the number of seconds.  
	absoluteDHMS.sec = Seconds % 60;
	Seconds -= Seconds % 60; // Remove seconds from original value passed in.

	// Determine the number of minutes.
	absoluteDHMS.min = (Seconds % 3600) / 60;
	Seconds -= Seconds % 3600; // Remove mins from original value passed in.

	absoluteDHMS.hour = (Seconds % numSecondsPerDay) / (60*60);
	Seconds -= Seconds % numSecondsPerDay;

	absoluteDHMS.day = Seconds / numSecondsPerDay;
	return absoluteDHMS;

}

HHMMSS C3mbStaticsLib::Time_ToHrMinSec(int Seconds)
{
	HHMMSS absoluteHMS;
	int numSecondsPerDay = 60*60*24;

	// If a negative time entered, convert to a positive time.
	while(Seconds < 0 )
		Seconds += numSecondsPerDay;

	absoluteHMS.sec = Seconds % 60;
	Seconds -= Seconds % 60;
	absoluteHMS.min = (Seconds % 3600) / 60;
	Seconds -= Seconds % 3600;

	absoluteHMS.hour = Seconds / 3600;
	return absoluteHMS;
}

int C3mbStaticsLib::Time_To24HrClockSeconds(int Seconds)
{
	int numSecondsPerDay = 60*60*24;

	// If a negative time entered, convert to a positive time.
	while(Seconds < 0 )
		Seconds += numSecondsPerDay;

	return Seconds % numSecondsPerDay;
}


int C3mbStaticsLib::Time_To24HrClockSeconds(HHMMSS HrMinSec)
{
	HrMinSec.hour %= 24;
	return Time_ToSeconds(HrMinSec);
}

HHMMSS C3mbStaticsLib::Time_To24HrMinSec(int Seconds)
{
	int numSecondsPerDay = 60*60*24;

	// If a negative time entered, convert to a positive time.
	while(Seconds < 0 )
		Seconds += numSecondsPerDay;

	Seconds %= 24*3600;
	return Time_ToHrMinSec(Seconds);
}

HHMMSS C3mbStaticsLib::Time_To24HrMinSec(HHMMSS HrMinSec)
{
	HrMinSec.hour %= 24;
	return HrMinSec;
}



void C3mbStaticsLib::RemoveExtension(TCHAR *FileName)
{
	DWORD strLen = strlen(FileName);

	while(strLen > 0)
	{
		if(FileName[strLen-1] == '.')
			break;
		strLen--;
	}

	// if no period (.) found, no extension so just retrun.
	if(strLen == 0)
		return;

	FileName[strLen-1] = NULL;
}

int C3mbStaticsLib::CalcAnimatQTYByDensity(double SurfaceArea_Meters, double AnimatPerSquareKilometer)
{
	return (int)MyRound(SurfaceArea_Meters/(1000*1000)*AnimatPerSquareKilometer);
}


int C3mbStaticsLib::GetExtension(const TCHAR *FileName, TCHAR *szBuffer, int BufferLength)
{
	DWORD strLen = strlen(FileName);
	int extensionLength = 1;

	memset(szBuffer, 0, BufferLength);

	while(strLen > 0)
	{
		if(FileName[strLen-1] == '.')
			break;
		strLen--;
		extensionLength++;
	}

	if(strLen == 0)
	{
		return 0;
	}

	strncpy_s(szBuffer,  BufferLength, &FileName[strLen-1], strlen(&FileName[strLen-1]));
	return extensionLength;
}



COORD_DEPTH C3mbStaticsLib::GetDefaultCoordinates()
{
	COORD_DEPTH c;
	c.lat = 33.25;
	c.lon = -119.5;
	c.depth = 0;
	return c;
}


#if 0
void C3mbStaticsLib::DistributeAnimalsAround(COORD_DEPTH Coord, ANIMAT_LIST *AnimatList, double MeanDistance, double StdDistance)
{
	int i;
	COORD_DEPTH coord;

	// Assign the first animat directly to the coordinate.
	*AnimatList->Get(0) = Coord;

	// Place the remainder animats about the first animat.
	for(i=1; i<AnimatList->Length(); i++)
	{
		coord = RandomLatLonAboutFocal(Coord.lat, Coord.lon, MeanDistance, StdDistance);
		*AnimatList->Get(i) = coord;
	}
}
#endif


// Determine the bearing of "to" relative to "from"
///////////////////////////////////////////////////////////////////////////
DISTANGL C3mbStaticsLib::DetermineBearing(COORDINATE From, COORDINATE To)
{
	return DetermineBearing(From.lat, From.lon, To.lat, To.lon);
}

// Determine the bearing of 2 relative to 1
DISTANGL C3mbStaticsLib::DetermineBearing(double lat1, double lon1, double lat2, double lon2)
{
	// The sign convention used in the following equations is that latitude is
	// negative in the South, positive in the North, and that longitude is positive in 
	// the West and negative in the East.  This is opposite of normal convention and 
	// requires some temporary sign manipulation.
	
	// The following labeling conventions are used: lat1 and lon1 are the latitude and
	// longitude of the sound source, lat2 and lon2 are the latitude and longitude of the 
	// animat.
	
	double bearing; 	// bearing from animat to sound source
	double d_rads;			// distance from animat to sound source in radians
	double rad_lon1, rad_lon2, rad_lat1, rad_lat2;	// radian conversions of latitude and longitude
	DISTANGL distBear;
	double pi = PI;
	
	// Signs are temporarily changed for longitude to be used in the following equations which 
	// follow the convention of W being a positive value and E being a negative value
	rad_lon1 = (pi / 180) * (-lon1);
	rad_lon2 = (pi / 180) * (-lon2);
	rad_lat1 = (pi / 180) * (lat1);
	rad_lat2 = (pi / 180) * (lat2);
	
	
	// Calculate distance between points
	d_rads = 2 * asin(sqrt(pow((sin((rad_lat1 - rad_lat2) / 2)),2) + 	
		cos(rad_lat1) * cos(rad_lat2) * pow((sin((rad_lon1 - rad_lon2) / 2)),2)));
	
	// Determine the bearing in radians
	if(sin(rad_lon2 - rad_lon1) == 0)
	{
		if(rad_lat1 == rad_lat2 || rad_lat1 < rad_lat2)
		{
			distBear.angle = 0;
			distBear.distance = d_rads * 1852 * (180 * 60)/pi; // convert to meters.
			return distBear;
		}
		else
		{
			distBear.angle = 180;
			distBear.distance = d_rads * 1852 * (180 * 60)/pi; // convert to meters.
			return distBear;
		}
	}
	else if (sin(rad_lon2 - rad_lon1) < 0)
		bearing = acos((sin(rad_lat2) - sin(rad_lat1) * cos(d_rads)) / (sin(d_rads) * cos(rad_lat1)));
	else
		bearing = 2 * pi - acos((sin(rad_lat2) - sin(rad_lat1) * cos(d_rads)) / (sin(d_rads) * cos(rad_lat1)));
	
	// Convert bearing back to degrees
	bearing = (180 / pi) * bearing;
	
	distBear.angle = bearing;
	distBear.distance = d_rads * 1852 * (180 * 60)/pi; // convert to meters.

	return distBear;
}

//------------------//
// Seeding Functions
//------------------//
// Lat and Lon validity checks
#define MIN(x,y) (x < y ? x : y)
#define MAX(x,y) (x > y ? x : y)
typedef struct {
double x,y;
} Point;
int C3mbStaticsLib::InsidePolygon(COORD *polygon, int N, COORD p)
{
	int counter = 0;
	int i;
	double xinters;
	COORD p1,p2;
	p1 = polygon[0];

	for(i=1;i<=N;i++)
	{
		p2 = polygon[i % N];
		if(p.Y > MIN(p1.Y,p2.Y))
		{
			if(p.Y <= MAX(p1.Y,p2.Y))
			{
				if(p.X <= MAX(p1.X,p2.X))
				{
					if(p1.Y != p2.Y)
					{
						xinters = (p.Y-p1.Y)*(p2.X-p1.X)/(p2.Y-p1.Y)+p1.X;
						if(p1.X == p2.X || p.X <= xinters)
							counter++;
					}
				}
			}
		}
		p1 = p2;
	}
	if(counter % 2 == 0)
		return(OUTSIDE);

	return(INSIDE);
}

BATHYCOMPARE C3mbStaticsLib::CompareBathyDepth(double CompareDepth, double ReferenceDepth)
{
	// Considers input 'CompareDepth' to input 'ReferenceDepth'.
	// Important to remember that depth values below sea level are negative and those
	// above are postive.

	if(CompareDepth > ReferenceDepth)
		return SHALLOWER;
	if(CompareDepth < ReferenceDepth)
		return DEEPER;
	return EQUAL;
}



int C3mbStaticsLib::InsidePolygon(const COORDINATE *polygon, int N, double Lat /*y*/, double Lon /*x*/)
{
	int counter = 0;
	int i;
	double xinters;
	COORDINATE p1,p2;
	p1 = polygon[0];

	for(i=1; i<=N; i++)
	{
		p2 = polygon[i % N];
		if(Lat > MIN(p1.lat,p2.lat))
		{
			if(Lat <= MAX(p1.lat,p2.lat))
			{
				if(Lon <= MAX(p1.lon,p2.lon))
				{
					if(p1.lat != p2.lat)
					{
						xinters = (Lat-p1.lat)*(p2.lon-p1.lon)/(p2.lat-p1.lat)+p1.lon;
						if(p1.lon == p2.lon || Lon <= xinters)
							counter++;
					}
				}
			}
		}
		p1 = p2;
	}
	if(counter % 2 == 0)
		return(OUTSIDE);

	return(INSIDE);
}

BOOL C3mbStaticsLib::CoordWithinCoordPolygonBoundaries(short X, short Y, COORD cArray[], int cArrayLen)
{
	COORD c;
	c.X = X;
	c.Y = Y;
	if(OUTSIDE == InsidePolygon(cArray, cArrayLen, c))
		return FALSE;

	return TRUE;
}

BOOL C3mbStaticsLib::LatLonWithinBoundaries(double Lat, double Lon, BATHYEXTREMES BathyEx)
{
	if(Lat > BathyEx.xMax || Lat < BathyEx.xMin || Lon > BathyEx.yMax || Lon < BathyEx.yMin)
		return FALSE;
	return TRUE;
}

BOOL C3mbStaticsLib::LatLonWithinBoundaries(double Lat, double Lon, ENVMINMAX BathyEx)
{
	if(Lat > BathyEx.xMax || Lat < BathyEx.xMin || Lon > BathyEx.yMax || Lon < BathyEx.yMin)
		return FALSE;
	return TRUE;
}

BOOL C3mbStaticsLib::BathyDepthAtLatLonValid(double BathyDepth, double ShoreFollowValue, double MinSeedDepth)
{
	if(BathyDepth >= BATHY_MIN_SEED_DEPTH || BathyDepth >= ShoreFollowValue || BathyDepth >= MinSeedDepth)
		return FALSE;
	return TRUE;
}

BOOL C3mbStaticsLib::IsAValidSeedLatLon(double Lat, double Lon, double BathyDepth, ENVMINMAX BathyEx, double ShoreFollowValue,
						double MinSeedDepth)
{
	if(LatLonWithinBoundaries(Lat, Lon, BathyEx) == FALSE ||
		BathyDepthAtLatLonValid(BathyDepth, ShoreFollowValue, MinSeedDepth) == FALSE)
		return FALSE;
	return TRUE;
}
_3DVECTOR C3mbStaticsLib::VectorCrossProduct(_3DVECTOR V1, _3DVECTOR V2)
{
	// +: i->j->k / x->y->z->
	// -: k->j->i / z->y->x->
	_3DVECTOR V = {0};
	V.z =  (V1.x * V2.y) - (V1.y * V2.x);
	V.y =  (V1.z * V2.x) - (V1.x * V2.z);
	V.x =  (V1.y * V2.z) - (V1.z * V2.y);
	return V;
}


double C3mbStaticsLib::MetersBetweenCoordinates(double Lat1, double Lon1, double Lat2, double Lon2)
{
	double rad_lon1, rad_lon2, rad_lat1, rad_lat2;	// radian conversions of latitude and longitude
	double d_rads; // distance in radians.
	double d_meters; // distance in meters;
	double pi = PI;

	// Signs are temporarily changed for longitude to be used in the following equations which 
	// follow the convention of W being a positive value and E being a negative value
	rad_lon1 = (pi / 180) * (-Lon1);
	rad_lon2 = (pi / 180) * (-Lon2);
	rad_lat1 = (pi / 180) * (Lat1);
	rad_lat2 = (pi / 180) * (Lat2);

	// Calculate distance between points
	d_rads = 2 * asin(sqrt(pow((sin((rad_lat1 - rad_lat2) / 2)),2) + 	
		cos(rad_lat1) * cos(rad_lat2) * pow((sin((rad_lon1 - rad_lon2) / 2)),2)));

	d_meters = d_rads * 1852 * (180 * 60)/pi; // convert to meters.
	return d_meters;
}

double C3mbStaticsLib::ContinueOtherSizeOfScreenValue(double V, double Min, double Max)
{
	double modVal = fmod(V-Min + (Max-Min), Max-Min);
	double ret = modVal + Min;
	return  ret;
}

COORDINATE C3mbStaticsLib::ContinueOtherSideOfScreenLatLon(COORDINATE Current, BATHYEXTREMES BathyEx)
{
	COORDINATE cd = {0};
	cd.lat = ContinueOtherSizeOfScreenValue(Current.lat, BathyEx.xMin, BathyEx.xMax); // Y (lat)
	cd.lon = ContinueOtherSizeOfScreenValue(Current.lon, BathyEx.yMin, BathyEx.yMax); // X (lon)
	return cd;
}


// Lat and Lon calculation without validation
COORDINATE C3mbStaticsLib::RandomLatLon(BATHYEXTREMES BathyEx, C3MBRandom *p3MBRandomRef)
{
	COORDINATE cd = {0};

	C3MBRandom *MBRandomRef = p3MBRandomRef;
	if(MBRandomRef == NULL)
		MBRandomRef = &m_3MBRandom;

	double rangeLat = BathyEx.xMax-BathyEx.xMin;
	double rangeLon = BathyEx.yMax-BathyEx.yMin;

	cd.lat = BathyEx.xMin + rangeLat * MBRandomRef->myrand();
	cd.lon = BathyEx.yMin + rangeLon * MBRandomRef->myrand();

	return cd;
}
COORDINATE C3mbStaticsLib::RandomLatLon(ENVMINMAX BathyEx, C3MBRandom *p3MBRandomRef)
{
	COORDINATE cd = {0};

	C3MBRandom *MBRandomRef = p3MBRandomRef;
	if(MBRandomRef == NULL)
		MBRandomRef = &m_3MBRandom;

	double rangeLat = BathyEx.xMax-BathyEx.xMin;
	double rangeLon = BathyEx.yMax-BathyEx.yMin;

	cd.lat = BathyEx.xMin + rangeLat * MBRandomRef->myrand();
	cd.lon = BathyEx.yMin + rangeLon * MBRandomRef->myrand();

	return cd;
}

COORD_DEPTH C3mbStaticsLib::RandomLatLonAboutFocal(double Lat, double Lon, double AveDistMeters, double StdDevDistMeters, C3MBRandom *p3MBRandomRef)
{
	double	   bear;
	double	   bearDeg;
	double	   meters;
	double	   temp_x, temp_y;
	double	   lat, lon, rads;
	COORD_DEPTH coord;

	C3MBRandom *MBRandomRef = p3MBRandomRef;
	if(MBRandomRef == NULL)
		MBRandomRef = &m_3MBRandom;


	memset(&coord, 0, sizeof(COORD_DEPTH));

	bear	= MBRandomRef->myrand()*2*PI;
	bearDeg = bear*180/PI;
	meters	= MBRandomRef->noise(AveDistMeters, StdDevDistMeters);
	lon		= (PI / 180) * (-Lon);
	lat		= (PI / 180) * (Lat);
	rads	= (PI / (180 * 60)) * (meters / 1852);
	
	// Calculate intermediates
	temp_y = ((lon - asin(sin(bear) * sin(rads) / cos(lat)) + PI));
	temp_x = (2 * PI);

	// latitude
	coord.lat = (180/PI)*(asin(sin(lat) * cos(rads) + cos(lat)*sin(rads)*cos(bear)));
	// longitude
	coord.lon = -(180/PI) * ((temp_y - (temp_x * floor(temp_y / temp_x))) - PI);
	return coord;
}

// Adds LatitudeMeters to Lat and LongitudeMeters to Lon to get a new Lat Lon.
COORD_DEPTH C3mbStaticsLib::NewLatLonFromOldPlusMeters(double Lat, double Lon, double LatitudeMetersY, double LongitudeMetersX)
{
#pragma message("!!!!!!!!!!!!!!  NewLatLonFromOldPlusMeters needs to be verified  !!!!!!!!!!!!!!!!!")
	double	   bear;
	double	   bearDeg;
	double	   meters;
	double	   temp_x, temp_y;
	double	   lat, lon, rads;
	COORD_DEPTH coord;

	memset(&coord, 0, sizeof(COORD_DEPTH));

	//bear	= myrand()*2*PI;
	bear = atan2(LatitudeMetersY, LongitudeMetersX);
	bearDeg = bear*180/PI;
	//meters	= noise(AveDistMeters, StdDevDistMeters);
	meters = sqrt(LatitudeMetersY * LongitudeMetersX);
	lon		= (PI / 180) * (-Lon);
	lat		= (PI / 180) * (Lat);
	rads	= (PI / (180 * 60)) * (meters / 1852);
	
	// Calculate intermediates
	temp_y = ((lon - asin(sin(bear) * sin(rads) / cos(lat)) + PI));
	temp_x = (2 * PI);

	// latitude
	coord.lat = (180/PI)*(asin(sin(lat) * cos(rads) + cos(lat)*sin(rads)*cos(bear)));
	// longitude
	coord.lon = -(180/PI) * ((temp_y - (temp_x * floor(temp_y / temp_x))) - PI);
	return coord;
}


DWORD C3mbStaticsLib::PackVersion(DWORD Super, DWORD Sub)
{
	return (Super << 16) | (Sub & 0x0000ffff);
}
void C3mbStaticsLib::UnpackVersion(DWORD PackedVersion, DWORD *Super, DWORD *Sub)
{
	*Super = PackedVersion >> 16;
	*Sub = PackedVersion & 0x0000ffff;
}



DWORD C3mbStaticsLib::GetBit(DWORD Bit, DWORD _32bitVal)
{
	_ASSERTE((Bit < 32) && (Bit >= 0));

	return (_32bitVal >> Bit) & 0x00000001;
}

DWORD C3mbStaticsLib::SetBit(DWORD Bit, DWORD _32bitVal, BOOL Set)
{

	if(Set == TRUE) // Set the bit
		_32bitVal = _32bitVal | (1 << Bit);
	else // Set == FALSE, clear the bit
		_32bitVal = _32bitVal & ~(1 << Bit);

	return _32bitVal;
}


RESLT C3mbStaticsLib::ConfigurationToTextFile(FILE *fd, USERPARAMS *Configuration)
{
#pragma message("need to implement ConfigurationToTextFile")
	fd = fd;
	Configuration = Configuration;
#if 0
	HHMMSS hhmmss;

	if(-1 == fprintf(fd, "\nCONFIGURATION\n"))
		return FILEWRITE_ERROR;
	if(-1 ==
		fprintf(fd,
		"------------------------------------------------------------------------------------------------\n"))
	{
		return FILEWRITE_ERROR;
	}
	if(-1 == fprintf(fd, "Randomizer Seeding\n"))
		return FILEWRITE_ERROR;
	if(cfg->seedWithCurrentTick == TRUE)
	{
		if(-1 == fprintf(fd, "   use current tick:  YES\n"))
			return FILEWRITE_ERROR;
	}
	else
	{
		if(-1 == fprintf(fd, "   use current tick:  NO\n"))
			return FILEWRITE_ERROR;
	}
	if(-1 == fprintf(fd, "          use value:  %d\n\n", cfg->seedValue))
		return FILEWRITE_ERROR;


	if(-1 == fprintf(fd, "Distance Calculation\n"))
		return FILEWRITE_ERROR;
	if(cfg->distCalcMethod == PLANAR_GEOMETRY)
	{
		if(-1 == fprintf(fd, "             method:  PLANAR_GEOMETRY\n\n"))
			return FILEWRITE_ERROR;
	}
	else
	{
		if(-1 == fprintf(fd, "             method:  LAT_LON\n\n"))
			return FILEWRITE_ERROR;
	}


	if(-1 == fprintf(fd, "Output Files\n"))
		return FILEWRITE_ERROR;
	if(cfg->outputText == TRUE)
	{
		if(-1 == fprintf(fd, "          text file:  YES\n"))
			return FILEWRITE_ERROR;
	}
	else
	{
		if(-1 == fprintf(fd, "          text file:  NO\n"))
			return FILEWRITE_ERROR;
	}


	if(-1 == fprintf(fd, "Text Output\n"))
		return FILEWRITE_ERROR;
	if(cfg->splitTextOutput == TRUE)
	{
		if(-1 == fprintf(fd, "       multiple files:  YES\n"))
			return FILEWRITE_ERROR;
	}
	else
	{
		if(-1 == fprintf(fd, "       multiple files:  NO\n"))
			return FILEWRITE_ERROR;
	}
		
	hhmmss = Time_ToHrMinSec(cfg->iterationsPerFile);
	if(-1 == fprintf(fd, "  iterations per file:  %d (%dhr %dmin %dsec per file)\n\n", cfg->iterationsPerFile,
			hhmmss.hour, hhmmss.min, hhmmss.sec))
			return FILEWRITE_ERROR;

	if(-1 == fprintf(fd, "------------------------------------------------------------------------------------------------\n\n\n"))
		return FILEWRITE_ERROR;
#endif
	return OK;
}

#if 0
TCHAR *C3mbStaticsLib::ModelTypeToString(BEHAVIORAL_MODEL_TYPE M, TCHAR *Buffer, int BufferLength)
{
	if(M == NON_VECTOR_MODEL_OR_NOT_USED)
		strcpy_s(Buffer, BufferLength, "Not Used");
	else if(M == VECTOR_MODEL)
	{
		strcpy_s(Buffer, BufferLength, "Vector Model");
	}
	else
		strcpy_s(Buffer, BufferLength, "User Defined Vector Model");
	return Buffer;
}
#endif

TCHAR *C3mbStaticsLib::ModelTypeToString(STANDARD_MODEL_TYPE M, TCHAR *Buffer, int BufferLength)
{
	switch(M)
	{
	case GAUSSIAN:
		strcpy_s(Buffer, BufferLength, "Gaussian");
		break;
	case UNIFORM:
		strcpy_s(Buffer, BufferLength, "Uniform Random");
		break;
	case VECTOR:
		strcpy_s(Buffer, BufferLength, "Vector Model");
		break;
	}
	return Buffer;
}

TCHAR *C3mbStaticsLib::ModelTypeToString(DIRECTIONAL_MODEL_TYPE M, TCHAR *Buffer, int BufferLength)
{
	switch(M)
	{
	case RANDOM_WALK:
		strcpy_s(Buffer, BufferLength, "Random Walk");
		break;

	case VECTOR_MODEL_DIRECTIONAL_NO_BIASING:
		strcpy_s(Buffer, BufferLength, "Vector Model");
		break;

	case VECTOR_MODEL_DIRECTIONAL_WITH_VECTOR_MODEL_BIASING:
		strcpy_s(Buffer, BufferLength, "User Defined Vector Model with Vector Modeled Biasing");
		break;

	case CORRELATED_RANDOM_WALK:
		strcpy_s(Buffer, BufferLength, "Correlated Random Walk");
		break;

	case CORRELATED_RANDOM_WALK_WITH_DIR_BIASING:
		strcpy_s(Buffer, BufferLength, "Correlated Random Walk with Directional Biasing");
		break;
	}
	return Buffer;
}


TCHAR *C3mbStaticsLib::YesNoString(BOOL B, TCHAR *Buffer, int BufferLength)
{
	if(B == FALSE)
		strcpy_s(Buffer, BufferLength, "NO");
	else
		strcpy_s(Buffer, BufferLength, "Yes");
	return Buffer;
}


TCHAR *C3mbStaticsLib::GetLocalDateAsString(TCHAR *szBuff, int BufferLength)
{
	SYSTEMTIME sysTime;
	GetLocalTime(&sysTime);
	_snprintf_s(szBuff, BufferLength, BufferLength, "%04d/%02d/%02d", sysTime.wYear, sysTime.wMonth, sysTime.wDay);
	return szBuff;
}


TCHAR *C3mbStaticsLib::GetLocalTimeAsHHMMSSString(TCHAR *szBuff, int BufferLength)
{
	SYSTEMTIME sysTime;
	GetLocalTime(&sysTime);
	_snprintf_s(szBuff, BufferLength, BufferLength, "%02d:%02d:%02d", sysTime.wHour, sysTime.wMinute, sysTime.wSecond);
	return szBuff;
}


TCHAR *C3mbStaticsLib::MbsResultToString(RESLT MbsResult, TCHAR *szMessageBuffer, int BufferLen,
						TCHAR *szCaptionBuffer, int CaptionBufferLength)
{

	switch(MbsResult)
	{
	// Non-errors.
	case OK:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "No Error");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "OK");
		break;

	case OK_EOFREACHED:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "End Of File Reached");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "OK");
		break;

	case OK_FILESNOTMATCH:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "Files Do Not Match");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "OK");
		break;

	// Errors.
	case ALREADYRUNNING_ERROR:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "3MB Is Busy And Cannot Start Another Process Now");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "3MB Already Busy");
		break;

	case MEMALLOC_ERROR:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "3MB Failed To Allocate Needed Memory");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "Memory Allocation Error");
		break;

	case FILEFORMAT_ERROR: 	// Error with the file... as in not formatted properly.
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "While Loading File A Problem With Its Format Was Detected");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "File Format Error");
		break;

	case FILENAME_ERROR:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "File Name Error");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "File Name Error");
		break;

	case OPENFILEREAD_ERROR:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "Unable To Open File For Reading");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "Open File Error");
		break;

	case OPENFILEWRITE_ERROR:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "Unable To Open File For Writing");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "Open File Error");
		break;

	case CREATEBINARYOUTPUT_ERROR:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "3MB Unable To Create/Open Binary Output File");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "Open File Error");
		break;

	case OPENTEXTOUTPUTFILE_ERROR:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "3MB Unable To Create/Open Animat Track Text Output File");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "Open File Error");
		break;

	case FILEREAD_ERROR:	// can be either file read error or buffer read error.
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "3MB Encountered Problems While Reading From A File");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "File Read Error");
		break;

	case FILEWRITE_ERROR:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "3MB Encountered Problems While Writing To A File");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "File Write Error");
		break;

	case WRONGFILETYPE_ERROR:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "The Wrong File Type Was Passed Into 3MB");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "Incorrect File Type");
		break;

	case INVALIDHANDLE_ERROR:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "A Function In 3MB Recieved An Invalid File Handle");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "Invalid File Handle");
		break;

	case USERMODELLINELENGTHEXCEEDED_ERROR:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "Line Length Exceeded In Matrix Model");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "Matrix Model Text File Error");
		break;

	case UNRECOGNIZED_SPECIES_MATRIX_PARAM_ERROR:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "A Species Param In Matrix Model Is Not Recognized");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "Matrix Model Error");
		break;

	// Setup Errors
	case NOSPECIESLOADED_ERROR:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "No Species Loaded Has Been Loaded Into 3MB");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "3MB Setup Error");
		break;

	case NOANIMATPOPULATION_ERROR:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "Scenario Is Unpopulated");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "Simulation Population Error");
		break;

	case UNPOPULATEDSPECIES_ERROR:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "Unpopulated Species Present");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "Simulation Population Error");
		break;

	case POPLIMITEXCEEDED_ERROR:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "The Scenario's Population Limit Is Exceeded");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "Simulation Population Error");
		break;


	case BUFFERLENGTH_INADEQUATE_ERROR:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "Inadequate Buffer Length");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "Programming Error");
		break;

	case INVALID_SPECIES_INDEX_ERROR:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "Invalid Species Index");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "Programming Error");
		break;
	case INVALID_POD_INDEX_ERROR:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "Invalid Pod Index");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "Programming Error");
		break;
	case INVALID_INDIVIDUAL_INDEX_ERROR:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "Invalid Individual Index");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "Programming Error");
		break;
	case INVALID_ANIMAT_INDEX_ERROR:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "Invalid Animat Index");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "Programming Error");
		break;

	case PARAM_HAD_NULLREF_ERROR:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "Parameter Had A NULL Value");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "Programming Error");
		break;
	case PARAM_INVALID_ERROR:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "A Parameter Was Invalid");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "Programming Error");
		break;

	case UNPOPULATED_POD_ERROR:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "A Pod Is Unpopulated");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "Seeding Error");
		break;
	case FILE_NOT_OPEN_ERROR:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "File Not Open");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "File Not Open");
		break;
	case INVALID_ITERATION_INDEX:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "Invalid Iteration Index");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "Programming Error");
		break;

	// Directory Errors
	case INVALID_DIRECTORY:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "Invalid Directory");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "Directory Error");
		break;
	case SET_DIRECTORY_ERROR:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "Set Directory Error");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "Directory Error");
		break;
	case DELETE_FILE_ERROR:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "Failed To Delete File");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "File Delete Error");
		break;
	case DIRECTORY_NOT_EMPTY_TO_DELETE:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "Unable To Delete Folder Because Not Empty");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "Delete Folder Error");
		break;
	case DELETE_DIRECTORY_ERROR:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "Problem While Attempting To Delete Directory");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "Delete Directory Error");
		break;

	case OBSOLETE_SPECIES_VERSION:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "Species Version Obsolete");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "Obsolete Species Version");
		break;
	case OBSOLETE_3MBS_VERSION:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "Obsolete 3MB Version");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "This 3MB Version Obsolete");
		break;

	case FILEMANAGER_SPECIESLIST_UNSET:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "Species List Unset");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "Unset Species List");
		break;

	case SCEPARAMS_NOT_MATCH_FILEMANAGER_PARAMS:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "Scenario Parameters Do Not Match File Manager Parameters");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "Parameter Mismatch Error");
		break;
	case FILE_NOT_OPEN: // File isn't open.
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "File Not Open");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "File Not Open");
		break;


	// Open/Save file dialog procedures.
	case FILE_EXTENSION_TOO_LARGE:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "File Extension Too Large");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "File Extension Too Large");
		break;
	case FILE_FILTER_TOO_LARGE:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "File Filter Too Large");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "File Filter Too Large");
		break;
	case FILE_PATH_TOO_LONG:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, TEXT("File Path Exceeds %d characters"), MAX_PATH-1);
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "File Path Error");
		break;

	case FILESIZE_TOO_LARGE:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, TEXT("File Too Large To Load"));
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "File Too Large");
		break;

	case MAX_NUM_PLAYBACK_STATES_EXCEEDED:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, TEXT("Max number of states (ANIMAT QTY times SAVED STATES <= %d) exceed"), MAX_NUM_PLAYBACK_STATES);
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "Too many states");
		break;


	case INVALID_SEEEDING_DEPTH:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, TEXT("Invalided Seeding Location For All Animats"));
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "Seed Location Error");
		break;

	case INVALID_SPECIES_SEEEDING_DEPTH:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, TEXT("Invalided Seeding Location For Specific Species"));
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "Seed Location Error");
		break;

	case COORDINATE_OFF_MAP:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, TEXT("Coordinates Off Map"));
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "Bad Location Error");
		break;

	case NULL_POINTER_RETUNRED_ERROR:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, TEXT("Null Pointer Returned"));
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "NULL Pointer");
		break;

	default:
		_snprintf_s(szMessageBuffer, BufferLen, BufferLen, "Unknown Error");
		if(szCaptionBuffer != NULL && CaptionBufferLength >0)
			_snprintf_s(szCaptionBuffer, CaptionBufferLength, CaptionBufferLength, "Unknown Error");

	}
	return szMessageBuffer;
}
