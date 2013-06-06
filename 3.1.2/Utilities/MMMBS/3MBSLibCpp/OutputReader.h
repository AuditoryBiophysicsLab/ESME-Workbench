#pragma once

#include "datatypes.h"

class COutputReader
{
public:
	COutputReader(void);
	~COutputReader(void);
	//RESLT OpenOutputFile(TCHAR *FileName);
	RESLT OpenOutputFile(TCHAR *FileName);
	void CloseOutputFile();
	_fSCENARIOPARAMS GetSceParams();
	RESLT GetAnimatState(DWORD AnimatNumber, DWORD IterationIndex, ANIMATSTATE_FILEOUT *pState);
	RESLT GetAnimatStates(DWORD AnimatNumber, ANIMATSTATE_FILEOUT *pStateArray, DWORD ArrayLen);
	BOOL IsOpen();
	unsigned int GetLibraryVersionSuper();
	unsigned int GetLibraryVersionSub();
	int GetTotalAnimats();
	int GetNumSpecies();
	int GetNumStates();
	int GetStartTime();

private:
	HANDLE	m_hdl;
	_fSCENARIOPARAMS m_sceParams;
	BINARYOUTPUT m_binOutputConfig;
	DWORD m_filePointerAnimats;
	DWORD m_filePointerAE;
	SPECIESBINOUTINF *m_speInf; // dynamically allocated.
	ANIMATASSCN *m_aniAssoc;
	//SPECIES_MDL *m_speciesModel;

};
