
#include "OutputReader.h"
#include "EnvironmentData.h"
#include "3mbslib.h"

COutputReader::COutputReader(void)
{
	m_hdl = NULL;
	m_filePointerAnimats = 0;
	memset(&m_sceParams, 0, sizeof(_fSCENARIOPARAMS));
	m_speInf = NULL;
	m_aniAssoc = NULL;

}

COutputReader::~COutputReader(void)
{
}

BOOL COutputReader::IsOpen()
{
	if(m_hdl == NULL || m_hdl == INVALID_HANDLE_VALUE)
		return FALSE;
	return TRUE;
}


void COutputReader::CloseOutputFile()
{
	if(m_hdl != NULL && m_hdl != INVALID_HANDLE_VALUE)
		CloseHandle(m_hdl);

	m_hdl = NULL;
	m_filePointerAnimats = 0;
	memset(&m_sceParams, 0, sizeof(_fSCENARIOPARAMS));
}


unsigned int COutputReader::GetLibraryVersionSuper()
{
	return m_sceParams.libVerSuper;
}

unsigned int COutputReader::GetLibraryVersionSub()
{
	return m_sceParams.libVerSub;
}


int COutputReader::GetTotalAnimats()
{
	return m_sceParams.totalNumAnimats;
}

int COutputReader::GetNumSpecies()
{
	return m_sceParams.numSpecies;
}
int COutputReader::GetNumStates()
{
	return m_sceParams.duration;
}
int COutputReader::GetStartTime()
{
	return m_sceParams.startTime;
}



_fSCENARIOPARAMS COutputReader::GetSceParams()
{
	return m_sceParams;
}


RESLT COutputReader::OpenOutputFile(TCHAR *FileName)
{
	DWORD bytes;
	//int i;
	//SPECIESBINOUTINF speInf;
	CEnvironmentData bath;
	CEnvironmentData salt;
	CEnvironmentData temp;
	//DWORD filePointer;
	//RESLT res;
	//BOOL rslt;

	if(m_hdl != NULL && m_hdl != INVALID_HANDLE_VALUE)
		CloseOutputFile();

	m_hdl = CreateFile(FileName, GENERIC_READ, NULL, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL | FILE_FLAG_RANDOM_ACCESS, 0);
	if(m_hdl == INVALID_HANDLE_VALUE)
		return OPENFILEREAD_ERROR;


	if(FALSE == ReadFile(m_hdl, &m_sceParams, sizeof(_fSCENARIOPARAMS), &bytes, NULL))
	{
		CloseOutputFile();
		return FILEREAD_ERROR;
	}

	//CloseOutputFile();
	return OK;

	// Not sure about what follows
# if 0

	m_binOutputConfig = CFileManager::TranslateBinFileOutConfiguration(m_sceParams.binOutStateItemConfig);

	// Read in the bathymetry file (if the scenario was configured to save it).
	if(m_binOutputConfig.headerInf.bathyMap == TRUE && OK != (res = bath.LoadFromBinFile(m_hdl)))
	{
		CloseOutputFile();
		return res;
	}

	// Read in the salinity file (if the scenario was configured to save it).
	if(m_binOutputConfig.headerInf.salinityMap == TRUE && OK != (res = salt.LoadFromBinFile(m_hdl)))
	{
		CloseOutputFile();
		return res;
	}

	// Read in the temperature file (if the scenario was configured to save it).
	if(m_binOutputConfig.headerInf.temperatureMap == TRUE && OK != (res = temp.LoadFromBinFile(m_hdl)))
	{
		CloseOutputFile();
		return res;
	}

	// Skip past statistical data if scenario was configured to save it.
	if(m_binOutputConfig.headerInf.postRunAnalysis == TRUE)
	{
		filePointer = MySetFilePointer(m_hdl, sizeof(TAKE), FILE_CURRENT);
		filePointer = MySetFilePointer(m_hdl, sizeof(TAKESTATS)*m_sceParams.numSpecies, FILE_CURRENT);
	}

	// Read in species model information (if the scenario was configured to save it).
	if(m_binOutputConfig.headerInf.speInfAndAnimatAsscn == TRUE)
	{
		_ASSERT(m_speInf == NULL);
		_ASSERT(m_aniAssoc == NULL);
		m_speInf = new SPECIESBINOUTINF[m_sceParams.numSpecies];
		m_aniAssoc = new ANIMATASSCN[m_sceParams.totalNumAnimats];

		// Skip over minor species information
		for(i=0; i<(int)m_sceParams.numSpecies && rslt == TRUE; i++)
		{
			if(FALSE == (rslt = ReadFile(m_hdl, &m_speInf[i], sizeof(SPECIESBINOUTINF), &bytes, NULL)))
				return FILEREAD_ERROR;

			// Skip past the behavior names that belong to this species.
			filePointer = MySetFilePointer(m_hdl, __int64(m_speInf[i].description.numBehaviors) * sizeof(BEHAVIOR_NAME), FILE_CURRENT);
			if(INVALID_SET_FILE_POINTER == filePointer)
				return SETFILEPOINTER_ERROR;
		}

		// Read in animat summaries/association to species.
		rslt &= ReadFile(m_hdl, m_aniAssoc, m_sceParams.totalNumAnimats * sizeof(ANIMATASSCN), &bytes, NULL);
			return FILEREAD_ERROR;
	}

	// Skip over acoustic exposures, get the file pointer for animat states (- 1 
	// to number of iterations because because an initial AE isn't saved).




	m_filePointerAnimats =
		MySetFilePointer(m_hdl, __int64(m_sceParams.duration-1) * sizeof(ACST_SRC_STATE), FILE_CURRENT);
	if(INVALID_SET_FILE_POINTER == filePointer)
		return SETFILEPOINTER_ERROR;

	return OK;
#endif
}


RESLT COutputReader::GetAnimatState(DWORD AnimatIndex, DWORD IterationIndex, ANIMATSTATE_FILEOUT *pState)
{
	long fileptr;
	DWORD bytes;
	C3mbStaticsLib staticLib;


	if(m_hdl == NULL)
		return FILE_NOT_OPEN_ERROR;
	if(AnimatIndex < 0 || AnimatIndex >= m_sceParams.totalNumAnimats)
		return INVALID_ANIMAT_INDEX_ERROR;
	if(IterationIndex < 0 || IterationIndex > m_sceParams.duration)
		return INVALID_ITERATION_INDEX;
	if(pState == NULL)
		return PARAM_HAD_NULLREF_ERROR;

	fileptr = m_filePointerAnimats + ((m_sceParams.duration) * AnimatIndex + IterationIndex);
	if(INVALID_SET_FILE_POINTER == staticLib.MySetFilePointer(m_hdl, fileptr, FILE_BEGIN))
		return SETFILEPOINTER_ERROR;

	if(FALSE == ReadFile(m_hdl, pState, sizeof(ANIMATSTATE_FILEOUT), &bytes, NULL))
		return FILEREAD_ERROR;

	return OK;
}
RESLT COutputReader::GetAnimatStates(DWORD AnimatIndex, ANIMATSTATE_FILEOUT *pStateArray, DWORD ArrayLen)
{
	long fileptr;
	DWORD bytes;
	DWORD readCount;
	C3mbStaticsLib staticLib;


	if(m_hdl == NULL)
		return FILE_NOT_OPEN_ERROR;

	if(m_hdl == NULL)
		return FILE_NOT_OPEN_ERROR;
	if(AnimatIndex < 0 || AnimatIndex >= m_sceParams.totalNumAnimats)
		return INVALID_ANIMAT_INDEX_ERROR;
	if(pStateArray == NULL)
		return PARAM_HAD_NULLREF_ERROR;

	fileptr = m_filePointerAnimats + ((m_sceParams.duration) * AnimatIndex);
	if(INVALID_SET_FILE_POINTER == staticLib.MySetFilePointer(m_hdl, fileptr, FILE_BEGIN))
		return SETFILEPOINTER_ERROR;

	readCount = (m_sceParams.duration);
	if(readCount > ArrayLen)
		readCount = ArrayLen;
	
	if(FALSE == ReadFile(m_hdl, pStateArray, sizeof(ANIMATSTATE_FILEOUT) * readCount, &bytes, NULL))
		return FILEREAD_ERROR;


	return OK;
}

