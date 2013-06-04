#pragma once
#include "Mutex.h"
#include "ListManager.h"
#include "dataTypes.h"

typedef CListManager <int> CWorkingList;


class COutputManager
{
public: // member functions
	COutputManager(void);
	~COutputManager(void);

	//---------------------------------------------//
	// Comma separated value list output functions
	//---------------------------------------------//
	RESLT ReadCSVListFromFile(TCHAR * szFileName);
	void ClearCSVList();
	BOOL CSVFileLoaded();
	//-------------------------------------//

	//---------------------------------------//
	// Acoustic Source Limit Output Functions
	//---------------------------------------//
	RESLT ResetAcousticSourceInf(ACOUSTICSRCEINF *AcstcSrcInfBuff, int AcstcSrcInfBuffLen);
	int GetAcousticSourceCount();
	void DeleteAllAcousticSourceInf();
	//--------------------------//

	//-------------------------------------//
	// Limit Output By Iteration Functions
	//-------------------------------------//
	//-------------------------------------//

	// Returns the number of iterations that will be saved to file based upon the current
	// state of this class's m_completeList size and 
	int GetFileReadListSaveStateCount(int StartTime, int Duration, const OUTPTINTRVLLMT *pOutputLim);

	// Simulation Active Routines
	RESLT Save(HANDLE Hd, BOOL AcousticPingLimt);
	RESLT Load(HANDLE Hd, BOOL AcousticPingLimt /*this param will be saved in the sce config and passed in here*/);

	//--------------------------------------------------------------------------//
	// Working list functions.
	// Note the working list is not maintained by this class but is updated by it
	// with the call to IsaSaveClockState()
	//--------------------------------------------------------------------------//
	BOOL IsaSaveClockState(CWorkingList *pList,
						   int Clock,
						   BOOL DeleteClockState = TRUE);
	CWorkingList *GetWorkingList(int StartTime, int Duration, const OUTPTINTRVLLMT *pOutputLim);
	void DeallocateWorkingList(CWorkingList *pList);
	//--------------------------------------------------------------------------//

private:
	// member functions
	//RESLT AddAcousticSrceToWorkingList(int StartTime, int Duration, CWorkingList *List, ACOUSTICSRCEINF *Ping);

	//-----------------//
	// member variables
	//-----------------//
	// The complete set of desired save states from user inputted CSV file minus duplicats
	// and erros
	CListManager <int> m_completeCSVList;

	// The set of acoustic sources added by the user.
	CListManager <ACOUSTICSRCEINF> m_acousticSrcList;

	// Describes the set up for limiting output by interation.
	//OUTPTINTRVLLMT m_intervalLimit;
};
