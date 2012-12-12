#ifndef ENVIRONMENTALDATASTATIC_H	
#define ENVIRONMENTALDATASTATIC_H

#include "dataTypes.h"


class CEnvironmentalDataStatic
{
public:
	CEnvironmentalDataStatic();
	virtual ~CEnvironmentalDataStatic();

	RESLT SkipOverInBinFile(HANDLE hd, DWORD *pNumBytes);
};
#endif//ENVIRONMENTALDATASTATIC_H	