#include "EnvironmentalDataStatic.h"
#include "staticLib.h"

CEnvironmentalDataStatic::CEnvironmentalDataStatic(){}
CEnvironmentalDataStatic::~CEnvironmentalDataStatic(){}

RESLT CEnvironmentalDataStatic::SkipOverInBinFile(HANDLE hd, DWORD *pNumBytes)
{
	int   totalBytes;
	BOOL  ioResult = TRUE;
	DWORD bytes;
	C3mbStaticsLib staticLib;


	// Read the total bytes associated with this binary environmental data
	ioResult &= ReadFile(hd, &totalBytes, sizeof(totalBytes), &bytes, NULL);

	// Skip back ahead to where writting ended
	if(INVALID_SET_FILE_POINTER == staticLib.MySetFilePointer(hd, __int64(totalBytes)-sizeof(totalBytes), FILE_CURRENT))
		return SETFILEPOINTER_ERROR;

	*pNumBytes = totalBytes;
	return OK;
}
