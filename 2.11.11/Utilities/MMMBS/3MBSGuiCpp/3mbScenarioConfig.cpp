#include "3mbScenarioConfig.h"
#include "3mbGuiFunctions.h"
#include "Scenario.h"
#include "resource.h"
#include "BrowseFolder.h"
#include "params.h"

extern C3mbStaticsLib staticLib;


DWORD WINAPI UpdateMemoryProc(LPVOID lpParameter);
void UpdateCalculations(CScenario *Sce, HWND hwndParent, USERPARAMS *pUserParams = NULL);
void UpdateBinOutConfigDlgState(USERPARAMS *pUserConfig, HWND hwndParent);

void UpdateCalculations(CScenario *Sce, HWND hwndParent, USERPARAMS *pUserParams)
{
	TCHAR szBuff1[SIZE_128];
	TCHAR szBuff2[SIZE_128];
	TCHAR szBuff3[SIZE_128];
	int popSize = Sce->GetAnimatCount();
	DWORDLONG val, val2;
	FM_MEMALLOCINF mallcInf = Sce->GetMemoryAllocationDetails(pUserParams);

	// IDC_STATIC_TOTAL_ANIMATS
	sprintf_s(szBuff1, sizeof(szBuff1), "animats: %d", popSize);
	SetDlgItemText(hwndParent, IDC_STATIC_TOTAL_ANIMATS, szBuff1);

	// IDC_STATIC_ANIMAT_STATE_SIZE
	//staticLib.MemoryValueToString_f(mallcInf.animatStateSize, szBuff2, sizeof(szBuff2));
	//sprintf_s(szBuff1, sizeof(szBuff1), "%s bytes per animat state = ", szBuff2);
	sprintf_s(szBuff1, sizeof(szBuff1), "bytes per state: %d", mallcInf.animatStateSize);
	SetDlgItemText(hwndParent, IDC_STATIC_ANIMAT_STATE_SIZE, szBuff1);

	// IDC_STATIC_ANIMAT_BYTESPERITERATION
	//staticLib.MemoryValueToString_f(popSize*mallcInf.animatStateSize, szBuff2, sizeof(szBuff2));
	//sprintf_s(szBuff1, sizeof(szBuff1), "%s per output iteration (animats)", szBuff2);
	sprintf_s(szBuff1, sizeof(szBuff1), "bytes per iteration: %d*%d = %d", popSize, mallcInf.animatStateSize, popSize*mallcInf.animatStateSize);
	SetDlgItemText(hwndParent, IDC_STATIC_ANIMAT_BYTESPERITERATION, szBuff1);

	// IDC_STATIC_PING_COORD_ENABLED
	if(mallcInf.acousticSrcStateSize > 0)
		sprintf_s(szBuff1, sizeof(szBuff1), "ping response coordinate: ENABLED");
	else
		sprintf_s(szBuff1, sizeof(szBuff1), "ping response coordinate: DISABLED");

	// IDC_STATIC_SOURCE_STATE_SIZE
	//staticLib.MemoryValueToString_f(mallcInf.acousticSrcStateSize, szBuff2, sizeof(szBuff2));
	//sprintf_s(szBuff1, sizeof(szBuff1), "+ %s bytes (acstc src)", szBuff2);
	sprintf_s(szBuff1, sizeof(szBuff1), "bytes per state: %d", mallcInf.acousticSrcStateSize);
	SetDlgItemText(hwndParent, IDC_STATIC_SOURCE_STATE_SIZE, szBuff1);

	// IDC_STATIC_STORAGE_PER_ITERATION
	val = popSize*mallcInf.animatStateSize + mallcInf.acousticSrcStateSize;
//	staticLib.MemoryValueToString_f(val, szBuff2, sizeof(szBuff2));
//	sprintf_s(szBuff1, sizeof(szBuff1), "%s per output iteration ", szBuff2);
	sprintf_s(szBuff1, sizeof(szBuff1), "tot. bytes per iteration %d+%d = %d", popSize*mallcInf.animatStateSize, mallcInf.acousticSrcStateSize, val);
	SetDlgItemText(hwndParent, IDC_STATIC_STORAGE_PER_ITERATION, szBuff1);



	// IDC_STATIC_TOTAL_ITERATIONS (total iterations)
	sprintf_s(szBuff1, sizeof(szBuff1), "save iteration count: %d", Sce->GetSaveStatesCount());
	SetDlgItemText(hwndParent, IDC_STATIC_TOTAL_ITERATIONS, szBuff1);

	// IDC_STATIC_TOTALBYTESPER_ITERATIONS (repeat of total bytes per outputted iteration)
	val = (popSize*mallcInf.animatStateSize + mallcInf.acousticSrcStateSize);
	sprintf_s(szBuff1, sizeof(szBuff1), "approx output file size:%d*", val);
	sprintf_s(szBuff2, sizeof(szBuff2), "%s%d =", szBuff1, Sce->GetSaveStatesCount());
	SetDlgItemText(hwndParent, IDC_STATIC_TEXTX10, szBuff2);


	// IDC_STATIC_TOTAL_BYTES (total bytes to be outputted to file)
//	val = (popSize*mallcInf.animatStateSize + mallcInf.acousticSrcStateSize)* Sce->GetSaveStatesCount();
	val = (popSize*mallcInf.animatStateSize + mallcInf.acousticSrcStateSize);
	val2 = val*Sce->GetSaveStatesCount();
	staticLib.MemoryValueToString_f(val2, szBuff2, sizeof(szBuff2));
	sprintf_s(szBuff3, sizeof(szBuff3), "%I64d", val2);
	//sprintf_s(szBuff1, sizeof(szBuff1), "%s", szBuff2);
	//sprintf_s(szBuff1, sizeof(szBuff1), "%d, %s", 5, szBuff2);
	sprintf_s(szBuff1, sizeof(szBuff1), "%s bytes (%s)", szBuff3, szBuff2);
	//sprintf_s(szBuff1, sizeof(szBuff1), "%d bytes %s", val, "cw=");
	SetDlgItemText(hwndParent, IDC_STATIC_TOTALBYTESPER_ITERATIONS, szBuff1);



	// 	IDC_STATIC_AVAILMEM
	staticLib.MemoryValueToString_f(mallcInf.memoryAvailablePhysical, szBuff2, sizeof(szBuff2));
	sprintf_s(szBuff1, sizeof(szBuff1), "physical memory available: %s", szBuff2);
	SetDlgItemText(hwndParent, IDC_STATIC_AVAILMEM, szBuff1);

	// Actual expected buffer size.
	// IDC_STATIC_OUTPUT_BUFF_SIZE
	staticLib.MemoryValueToString_f(mallcInf.memoryMaximumAllocPhysical, szBuff2, sizeof(szBuff2));
	sprintf_s(szBuff1, sizeof(szBuff1), "        max allocate attempt: %s", szBuff2);
	SetDlgItemText(hwndParent, IDC_STATIC_OUTPUT_BUFF_SIZE, szBuff1);


	// Actual expected buffer iteration size.
	// IDC_STATIC_OUTPUT_BUFF_SIZE
	sprintf_s(szBuff1, sizeof(szBuff1), "buffer length (interations): %d",  mallcInf.bufferIterationCapacity);
	SetDlgItemText(hwndParent, IDC_STATIC_OUTPUT_BUFF_LEN, szBuff1);

	// IDC_STATIC_NUMOUTPUT_ITERATIONS
	sprintf_s(szBuff1, sizeof(szBuff1), "iterations to file count: %d", Sce->GetSaveStatesCount());
	SetDlgItemText(hwndParent, IDC_STATIC_NUMOUTPUT_ITERATIONS, szBuff1);

	//val = (popSize*binDisInf.store.animatState + binDisInf.store.aeState)* sp.saveIterationCnt;


	/*
	val *= (Sce->GetDurationSeconds()+1);
	staticLib.MemoryValueToString_f(val, szBuff2, sizeof(szBuff2));
	sprintf_s(szBuff1, sizeof(szBuff1), "acoustic src: %s", szBuff2);
	SetDlgItemText(hwndParent, IDC_STATIC_STORAGE_PER_ITERATION, szBuff1);
*/

	/*
	IDC_STATIC_ANIMAT_STATE_SIZE
	IDC_STATIC_TOTAL_ANIMATS
	IDC_STATIC_SOURCE_STATE_SIZE
	IDC_STATIC_STORAGE_PER_ITERATION
	IDC_STATIC_OUTPUT_BUFF_SIZE
	IDC_STATIC_OUTPUT_BUFF_LEN
	IDC_STATIC_NUMOUTPUT_ITERATIONS
	*/

}

void UpdateBinOutConfigDlgState(USERPARAMS *pUserConfig, HWND hwndParent)
{
//	DWORD configDword = CScenario::TranslateBinFileOutConfiguration(pUserConfig);
	if(pUserConfig->output.enabled == FALSE)
	{

		EnableWindow(GetDlgItem(hwndParent, IDC_BUTTON_STATE_FULL), FALSE);
		EnableWindow(GetDlgItem(hwndParent, IDC_BUTTON_STATE_MIN), FALSE);

		EnableWindow(GetDlgItem(hwndParent, IDC_RADIO_STATEDATAFORMAT_ANIMAT), FALSE);
		CheckDlgButton(hwndParent, IDC_RADIO_STATEDATAFORMAT_ANIMAT, BST_UNCHECKED);
		
		EnableWindow(GetDlgItem(hwndParent, IDC_RADIO_STATEDATAFORMAT_ITERATION), FALSE);
		CheckDlgButton(hwndParent, IDC_RADIO_STATEDATAFORMAT_ITERATION, BST_UNCHECKED);


		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_BATHYMAP), FALSE);
		CheckDlgButton(hwndParent, IDC_CHECK_BATHYMAP, BST_UNCHECKED);
		
		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_SALINITYMAP), FALSE);
		CheckDlgButton(hwndParent, IDC_CHECK_SALINITYMAP, BST_UNCHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_TEMPMAP), FALSE);
		CheckDlgButton(hwndParent, IDC_CHECK_TEMPMAP, BST_UNCHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_AESTATS), FALSE);
		CheckDlgButton(hwndParent, IDC_CHECK_AESTATS, BST_UNCHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_ANIMATSPEASSOCIATION), FALSE);
		CheckDlgButton(hwndParent, IDC_CHECK_ANIMATSPEASSOCIATION, BST_UNCHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_CLOCK), FALSE);
		CheckDlgButton(hwndParent, IDC_CHECK_CLOCK, BST_UNCHECKED);
		
		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_ID), FALSE);
		CheckDlgButton(hwndParent, IDC_CHECK_ID, BST_UNCHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_COORD), FALSE);
		CheckDlgButton(hwndParent, IDC_CHECK_COORD, BST_UNCHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_DEPTH), FALSE);
		CheckDlgButton(hwndParent, IDC_CHECK_DEPTH, BST_UNCHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_PACKED), FALSE);
		CheckDlgButton(hwndParent, IDC_CHECK_PACKED, BST_UNCHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_BEARING), FALSE);
		CheckDlgButton(hwndParent, IDC_CHECK_BEARING, BST_UNCHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_TRAVELRATE), FALSE);
		CheckDlgButton(hwndParent, IDC_CHECK_TRAVELRATE, BST_UNCHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_DIVERATE), FALSE);
		CheckDlgButton(hwndParent, IDC_CHECK_DIVERATE, BST_UNCHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_TARGETDEPTH), FALSE);
		CheckDlgButton(hwndParent, IDC_CHECK_TARGETDEPTH, BST_UNCHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_BATHYVALUE), FALSE);
		CheckDlgButton(hwndParent, IDC_CHECK_BATHYVALUE, BST_UNCHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_SALINITYVALUE), FALSE);
		CheckDlgButton(hwndParent, IDC_CHECK_SALINITYVALUE, BST_UNCHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_TEMPVALUE), FALSE);
		CheckDlgButton(hwndParent, IDC_CHECK_TEMPVALUE, BST_UNCHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_AECUMTV), FALSE);
		CheckDlgButton(hwndParent, IDC_CHECK_AECUMTV, BST_UNCHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_AEINST), FALSE);
		CheckDlgButton(hwndParent, IDC_CHECK_AEINST, BST_UNCHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_AEANGLE), FALSE);
		CheckDlgButton(hwndParent, IDC_CHECK_AEANGLE, BST_UNCHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_AETIME), FALSE);
		CheckDlgButton(hwndParent, IDC_CHECK_AETIME, BST_UNCHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_AECOORDSTATE), FALSE);
		CheckDlgButton(hwndParent, IDC_CHECK_AECOORDSTATE, BST_UNCHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_XYDISTANCEMETERS), FALSE);
		CheckDlgButton(hwndParent, IDC_CHECK_XYDISTANCEMETERS, BST_UNCHECKED);

		// Risk
		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_RISK), FALSE);
		CheckDlgButton(hwndParent, IDC_CHECK_RISK, BST_UNCHECKED);
	}
	else
	{
		EnableWindow(GetDlgItem(hwndParent, IDC_BUTTON_STATE_FULL), TRUE);
		EnableWindow(GetDlgItem(hwndParent, IDC_BUTTON_STATE_MIN), TRUE);


		EnableWindow(GetDlgItem(hwndParent, IDC_RADIO_STATEDATAFORMAT_ANIMAT), TRUE);
		EnableWindow(GetDlgItem(hwndParent, IDC_RADIO_STATEDATAFORMAT_ITERATION), TRUE);

		if(pUserConfig->output.outputByTime == FALSE)
		{
			CheckDlgButton(hwndParent, IDC_RADIO_STATEDATAFORMAT_ANIMAT, BST_CHECKED);
			CheckDlgButton(hwndParent, IDC_RADIO_STATEDATAFORMAT_ITERATION, BST_UNCHECKED);
		}
		else
		{
			CheckDlgButton(hwndParent, IDC_RADIO_STATEDATAFORMAT_ANIMAT, BST_UNCHECKED);
			CheckDlgButton(hwndParent, IDC_RADIO_STATEDATAFORMAT_ITERATION, BST_CHECKED);
		}


		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_BATHYMAP), TRUE);
		if(pUserConfig->output.headerInf.bathyMap == FALSE)
			CheckDlgButton(hwndParent, IDC_CHECK_BATHYMAP, BST_UNCHECKED);
		else
			CheckDlgButton(hwndParent, IDC_CHECK_BATHYMAP, BST_CHECKED);
		
		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_SALINITYMAP), TRUE);
		if(pUserConfig->output.headerInf.salinityMap == FALSE)
			CheckDlgButton(hwndParent, IDC_CHECK_SALINITYMAP, BST_UNCHECKED);
		else
			CheckDlgButton(hwndParent, IDC_CHECK_SALINITYMAP, BST_CHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_TEMPMAP), TRUE);
		if(pUserConfig->output.headerInf.temperatureMap == FALSE)
			CheckDlgButton(hwndParent, IDC_CHECK_TEMPMAP, BST_UNCHECKED);
		else
			CheckDlgButton(hwndParent, IDC_CHECK_TEMPMAP, BST_CHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_AESTATS), TRUE);
		if(pUserConfig->output.headerInf.postRunAnalysis == FALSE)
			CheckDlgButton(hwndParent, IDC_CHECK_AESTATS, BST_UNCHECKED);
		else
			CheckDlgButton(hwndParent, IDC_CHECK_AESTATS, BST_CHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_ANIMATSPEASSOCIATION), TRUE);
		if(pUserConfig->output.headerInf.speInfAndAnimatAsscn == FALSE)
			CheckDlgButton(hwndParent, IDC_CHECK_ANIMATSPEASSOCIATION, BST_UNCHECKED);
		else
			CheckDlgButton(hwndParent, IDC_CHECK_ANIMATSPEASSOCIATION, BST_CHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_CLOCK), FALSE);
		if(pUserConfig->output.animat.timeOfDay == FALSE)
			CheckDlgButton(hwndParent, IDC_CHECK_CLOCK, BST_UNCHECKED);
		else
			CheckDlgButton(hwndParent, IDC_CHECK_CLOCK, BST_CHECKED);
		
		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_ID), FALSE);
		if(pUserConfig->output.animat.ID == FALSE)
			CheckDlgButton(hwndParent, IDC_CHECK_ID, BST_UNCHECKED);
		else
			CheckDlgButton(hwndParent, IDC_CHECK_ID, BST_CHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_COORD), FALSE);
		if(pUserConfig->output.animat.coordinate == FALSE)
			CheckDlgButton(hwndParent, IDC_CHECK_COORD, BST_UNCHECKED);
		else
			CheckDlgButton(hwndParent, IDC_CHECK_COORD, BST_CHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_DEPTH), FALSE);
		if(pUserConfig->output.animat.depth == FALSE)
			CheckDlgButton(hwndParent, IDC_CHECK_DEPTH, BST_UNCHECKED);
		else
			CheckDlgButton(hwndParent, IDC_CHECK_DEPTH, BST_CHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_PACKED), FALSE);
		if(pUserConfig->output.animat.packedData == FALSE)
			CheckDlgButton(hwndParent, IDC_CHECK_PACKED, BST_UNCHECKED);
		else
			CheckDlgButton(hwndParent, IDC_CHECK_PACKED, BST_CHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_BEARING), TRUE);
		if(pUserConfig->output.animat.bearing == FALSE)
			CheckDlgButton(hwndParent, IDC_CHECK_BEARING, BST_UNCHECKED);
		else
			CheckDlgButton(hwndParent, IDC_CHECK_BEARING, BST_CHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_TRAVELRATE), TRUE);
		if(pUserConfig->output.animat.travelRate == FALSE)
			CheckDlgButton(hwndParent, IDC_CHECK_TRAVELRATE, BST_UNCHECKED);
		else
			CheckDlgButton(hwndParent, IDC_CHECK_TRAVELRATE, BST_CHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_DIVERATE), TRUE);
		if(pUserConfig->output.animat.diveRate == FALSE)
			CheckDlgButton(hwndParent, IDC_CHECK_DIVERATE, BST_UNCHECKED);
		else
			CheckDlgButton(hwndParent, IDC_CHECK_DIVERATE, BST_CHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_TARGETDEPTH), TRUE);
		if(pUserConfig->output.animat.targetDepth == FALSE)
			CheckDlgButton(hwndParent, IDC_CHECK_TARGETDEPTH, BST_UNCHECKED);
		else
			CheckDlgButton(hwndParent, IDC_CHECK_TARGETDEPTH, BST_CHECKED);

		// XY distance in meters (41)
		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_XYDISTANCEMETERS), TRUE);
		if(pUserConfig->output.animat.xyDistance == FALSE)
			CheckDlgButton(hwndParent, IDC_CHECK_XYDISTANCEMETERS, BST_UNCHECKED);
		else
			CheckDlgButton(hwndParent, IDC_CHECK_XYDISTANCEMETERS, BST_CHECKED);

		// Risk (42)
		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_RISK), TRUE);
		if(pUserConfig->output.animat.risk == FALSE)
			CheckDlgButton(hwndParent, IDC_CHECK_RISK, BST_UNCHECKED);
		else
			CheckDlgButton(hwndParent, IDC_CHECK_RISK, BST_CHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_BATHYVALUE), TRUE);
		if(pUserConfig->output.animat.bathyDepth == FALSE)
			CheckDlgButton(hwndParent, IDC_CHECK_BATHYVALUE, BST_UNCHECKED);
		else
			CheckDlgButton(hwndParent, IDC_CHECK_BATHYVALUE, BST_CHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_SALINITYVALUE), TRUE);
		if(pUserConfig->output.animat.salinity == FALSE)
			CheckDlgButton(hwndParent, IDC_CHECK_SALINITYVALUE, BST_UNCHECKED);
		else
			CheckDlgButton(hwndParent, IDC_CHECK_SALINITYVALUE, BST_CHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_TEMPVALUE), TRUE);
		if(pUserConfig->output.animat.temperature == FALSE)
			CheckDlgButton(hwndParent, IDC_CHECK_TEMPVALUE, BST_UNCHECKED);
		else
			CheckDlgButton(hwndParent, IDC_CHECK_TEMPVALUE, BST_CHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_AECUMTV), TRUE);
		if(pUserConfig->output.animat.aeCmltve == FALSE)
			CheckDlgButton(hwndParent, IDC_CHECK_AECUMTV, BST_UNCHECKED);
		else
			CheckDlgButton(hwndParent, IDC_CHECK_AECUMTV, BST_CHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_AEINST), TRUE);
		if(pUserConfig->output.animat.aeMoment == FALSE)
			CheckDlgButton(hwndParent, IDC_CHECK_AEINST, BST_UNCHECKED);
		else
			CheckDlgButton(hwndParent, IDC_CHECK_AEINST, BST_CHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_AEANGLE), TRUE);
		if(pUserConfig->output.animat.aeRelAngle == FALSE)
			CheckDlgButton(hwndParent, IDC_CHECK_AEANGLE, BST_UNCHECKED);
		else
			CheckDlgButton(hwndParent, IDC_CHECK_AEANGLE, BST_CHECKED);

		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_AETIME), TRUE);
		if(pUserConfig->output.animat.aeTimeAvrt == FALSE)
			CheckDlgButton(hwndParent, IDC_CHECK_AETIME, BST_UNCHECKED);
		else
			CheckDlgButton(hwndParent, IDC_CHECK_AETIME, BST_CHECKED);

		// Acoustic exposure coodinate
		EnableWindow(GetDlgItem(hwndParent, IDC_CHECK_AECOORDSTATE), TRUE);
		if(pUserConfig->output.AECoordinate == FALSE)
			CheckDlgButton(hwndParent, IDC_CHECK_AECOORDSTATE, BST_UNCHECKED);
		else
			CheckDlgButton(hwndParent, IDC_CHECK_AECOORDSTATE, BST_CHECKED);
	}
}


LRESULT CALLBACK ScenarioConfigProc(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
	static FILE_INFO fi;
		   TCHAR szBuff[SIZE_512];
		   TCHAR szBuff2[SIZE_512];
		   TCHAR szSmallBuff[SIZE_16];
		   int iterationVal, clockVal, startTime;
		   CStaticScenario m_staticScenario;

		   //HHMMSS hms;
		   int i;
		   int savedStatesCnt;
//		   int dur;
		   TCHAR *szClock = NULL;
		   int dynamicSzLen1;
		   TCHAR *szIteration = NULL;
		   int dynamicSzLen2;
		   TCHAR *pSz;
		   
	//static TCHAR *szSaveDir = NULL;

	static FILE_INFO fiOutput;
	static TCHAR szOutputFilter[] = TEXT("Output Files (*.TXT)\0*.txt\0"); 
	static TCHAR szOutputDefExt[] = TEXT("txt") ;
	static USERPARAMS userParms;
	static BOOL multiSoures = MULTISOUNDSOURCEALLOWED; // Here to quite compiler warning.
	//static CScenario *sce = NULL;

	static UPDATETHREADINF memUpdateThread = {0};
	static SCECONFIGWINPARAMS *params = NULL;
	CWorkingList *list;

	//static TCHAR szCurDir[MAX_PATH]; 


	switch (message)
	{
		case WM_INITDIALOG:
			//szSaveDir = (TCHAR *)lParam;
			params = (SCECONFIGWINPARAMS *)lParam;
			userParms = params->sce->GetConfiguration();

//			fi.szFilter = szOutputFilter;
//			fi.szDefExt = szOutputDefExt;
//			fi.hwnd = hDlg;

			if(userParms.output.enabled == FALSE)
				SetDlgItemText(hDlg, IDC_BUTTON_STATE_NONE, "Enable Output");


			if(*params->outputFolderSet == FALSE)
				GetCurrentDirectory(params->outputFolderBufferSize, params->szOutputFolder);

			SetDlgItemText(hDlg, IDC_STATIC_STATE_OUTPUTFOLDER, params->szOutputFolder);

			// Launch thread that continuously updates memory usage.
//			memUpdateThread.hWnd = hDlg;
//			memUpdateThread.threadInf.hdl = 
//				CreateThread(NULL, 0, &UpdateMemoryProc, &memUpdateThread, 0, &memUpdateThread.threadInf.id);

			_stprintf_s(szBuff, sizeof(szBuff), "%u", userParms.seed.value);
			SetDlgItemText(hDlg, IDC_RANDOM_SET_SEED_VALUE,  szBuff);

			if(userParms.seed.useCurrentTick)
			{
				CheckRadioButton(hDlg, IDC_RANDOM_SEED_AUTO, IDC_RANDOM_SEED_SET, IDC_RANDOM_SEED_AUTO);
				EnableWindow(GetDlgItem(hDlg, IDC_RANDOM_SET_SEED_VALUE), FALSE);
			}
			else
			{
				CheckRadioButton(hDlg, IDC_RANDOM_SEED_AUTO, IDC_RANDOM_SEED_SET, IDC_RANDOM_SEED_SET);
				EnableWindow(GetDlgItem(hDlg, IDC_RANDOM_SET_SEED_VALUE), TRUE);
			}

			// The independent random number generation for animats.
			if(userParms.seed.independentAnimatRandomGen)
				CheckDlgButton(hDlg, IDC_ORTHOGONAL_RANDOM_VARS, BST_CHECKED);
			else
				CheckDlgButton(hDlg, IDC_ORTHOGONAL_RANDOM_VARS, BST_UNCHECKED);


			//--------------------------------------------------------------------------//
			// The Acoustic Source Default Values MULTISOUNDSOURCEALLOWED, 
			//----------------------------------//
			if(multiSoures == TRUE)
				SetDlgItemText(hDlg, IDC_ACSTICSRCE_MULTI_ENABLED, "Enabled");
			else
				SetDlgItemText(hDlg, IDC_ACSTICSRCE_MULTI_ENABLED, "Disabled");

			_stprintf_s(szBuff, sizeof(szBuff), "%.1f", (float)ACSTC_SOURCE_LEVEL_DB);
			SetDlgItemText(hDlg, IDC_ACSTICSRCE_OUTPUTDB,  szBuff);

			_stprintf_s(szBuff, sizeof(szBuff), "%u", ACSTC_SOURCE_BEGINS_ITERATION);
			SetDlgItemText(hDlg, IDC_ACSTICSRCE_START_ITRNTN,  szBuff);

			_stprintf_s(szBuff, sizeof(szBuff), "%.3f", (float)(1.0/ACSTC_SOURCE_DUTY_PERIOD));
			SetDlgItemText(hDlg, IDC_ACSTICSRCE_DUTYCYCLE,  szBuff);

			
			//IDC_STATIC_ITERATION_CNT
			szBuff[0] = 0;
			szBuff2[0] = 0;
			savedStatesCnt = params->sce->GetSaveStatesCount();

			// List will be NULL if no sound sources present AND no CSV files loaded.
			startTime = staticLib.Time_To24HrClockSeconds(params->sce->GetStartTime());
			list = params->sce->GetWorkingList(startTime, params->sce->GetDurationSeconds());

			szClock = (TCHAR *)malloc(1*sizeof(TCHAR));
			szClock[0] = 0;
			szIteration = (TCHAR *)malloc(1*sizeof(TCHAR));
			szIteration[0] = 0;
			for(i=0; i < 100 && i < savedStatesCnt; i++)
			{
				if(list == NULL)
					iterationVal = i;
				else
					iterationVal = *list->Get(i); // Clock Time

				clockVal = iterationVal + startTime;

				// Iteration
				_stprintf_s(szSmallBuff, sizeof(szSmallBuff), "%d, ", iterationVal);
				pSz = szIteration;
				dynamicSzLen2 = _tcslen(szIteration) + _tcslen(szSmallBuff) + 1; // +1 for terminating NULL.
				szIteration = (TCHAR *)malloc(dynamicSzLen2 * sizeof(TCHAR));
				_stprintf_s(szIteration, dynamicSzLen2*sizeof(TCHAR), "%s%s", pSz, szSmallBuff);
				if(pSz!= NULL)
					free(pSz);


				// Clock
				_stprintf_s(szSmallBuff, sizeof(szSmallBuff), "%d, ", clockVal);
				pSz = szClock;
				dynamicSzLen1 = _tcslen(szClock) + _tcslen(szSmallBuff) + 1; // +1 for terminating NULL.
				szClock = (TCHAR *)malloc(dynamicSzLen1 * sizeof(TCHAR));
				_stprintf_s(szClock, dynamicSzLen1*sizeof(TCHAR), "%s%s", pSz, szSmallBuff);
				if(pSz!= NULL)
					free(pSz);

			}
			SetDlgItemText(hDlg, IDC_EDIT_ACSTCSRC_ITRNLIST,  szIteration);

			SetDlgItemText(hDlg, IDC_EDIT_ACSTCSRC_CLKLIST,  szClock);

			if(szClock != NULL)
				free(szClock);
			szClock = NULL;
			if(szIteration != NULL)
				free(szIteration);
			szIteration = NULL;
			// Clear out the list
			if(list != NULL)
			{
				list->DeleteAll();
				delete list;
			}

			//dur = params->sce->GetDurationSeconds();
			if(savedStatesCnt > 100 || i <savedStatesCnt)
				_stprintf_s(szBuff, sizeof(szBuff), "First %d of %d Iterations To Be Saved", i, savedStatesCnt);
			else
				_stprintf_s(szBuff, sizeof(szBuff),"First %d of %d Iterations To Be Saved", i, savedStatesCnt);

			SetDlgItemText(hDlg, IDC_STATIC_ITERATION_CNT, szBuff);
			//--------------------------------------------------------------------------//


			// Text output option
			if(params->texConfig->enabled == TRUE)
				CheckDlgButton(hDlg, IDC_OUTPUT_TEXT, BST_CHECKED);
			else
				CheckDlgButton(hDlg, IDC_OUTPUT_TEXT, BST_UNCHECKED);

			// Distance Calculation option
			if(userParms.distCalcMethod == PLANAR_GEOMETRY)
				CheckRadioButton(hDlg, IDC_RADIO_DISTCALC_LATLO, IDC_RADIO_DISTCALC_PGEOM, IDC_RADIO_DISTCALC_PGEOM);
			else
				CheckRadioButton(hDlg, IDC_RADIO_DISTCALC_LATLO, IDC_RADIO_DISTCALC_PGEOM, IDC_RADIO_DISTCALC_LATLO);

			// Single, Multiple text file output
			if(params->texConfig->splitTextOutput == FALSE)
			{
				CheckRadioButton(hDlg, IDC_RADIO_OUTPUT_TXT_SINGLE, IDC_RADIO_OUTPUT_TXT_MULTI, IDC_RADIO_OUTPUT_TXT_SINGLE);
				EnableWindow(GetDlgItem(hDlg, IDC_OUTPUT_TXT_ITSPERFILE), FALSE);
			}
			else
			{
				CheckRadioButton(hDlg, IDC_RADIO_OUTPUT_TXT_SINGLE, IDC_RADIO_OUTPUT_TXT_MULTI, IDC_RADIO_OUTPUT_TXT_MULTI);
				EnableWindow(GetDlgItem(hDlg, IDC_OUTPUT_TXT_ITSPERFILE), TRUE);
			}	
			sprintf_s(szBuff, sizeof(szBuff), "%u", params->texConfig->iterationsPerFile);
			SetDlgItemText(hDlg, IDC_OUTPUT_TXT_ITSPERFILE,  szBuff);
			UpdateBinOutConfigDlgState(&userParms, hDlg);
			UpdateCalculations(params->sce, hDlg, &userParms);
			PostMessage(hDlg, WM_UPDATE_GUI, NULL, NULL);
			break;

		case WM_UPDATE_GUI:

			if(memUpdateThread.threadInf.hdl == NULL)
			{
				// Launch thread that continuously updates memory usage.
				memUpdateThread.hWnd = hDlg;
				memUpdateThread.threadInf.hdl = 
					CreateThread(NULL, 0, &UpdateMemoryProc, &memUpdateThread, 0, &memUpdateThread.threadInf.id);
			}

			UpdateCalculations(params->sce, hDlg, &userParms);
			break;

		case WM_COMMAND:

			if (LOWORD(wParam) == IDOK) 
			{
/*
				length = _tcsclen(fi.szFileName);

				for(i=length-1; i>=0; i--)
				{
					if(fi.szFileName[i] != _TEXT('\\'))
						continue;

					fi.szFileName[i] = 0;
					break;
				}
				_tcscpy(szSaveDir, fi.szFileName);
*/

				memUpdateThread.threadInf.exit = TRUE;

				GetDlgItemText(hDlg, IDC_RANDOM_SET_SEED_VALUE, szBuff, 16);
				userParms.seed.value = _ttoi(szBuff);

				GetDlgItemText(hDlg, IDC_OUTPUT_TXT_ITSPERFILE, szBuff, 16);

				params->texConfig->iterationsPerFile = _ttoi(szBuff);
				params->sce->SetConfiguration(userParms);

				while(memUpdateThread.threadInf.running == TRUE)
					Sleep(1);
				Sleep(1);

				EndDialog(hDlg, LOWORD(wParam));
				return TRUE;
			}

			if(LOWORD(wParam) == IDCANCEL)
			{
				memUpdateThread.threadInf.exit = TRUE;
				while(memUpdateThread.threadInf.running == TRUE)
					Sleep(1);
				Sleep(1);
				EndDialog(hDlg, LOWORD(wParam));
				return FALSE;
			}

			switch(wParam)
			{

/*
			case IDC_BROWSER:
				GetSaveFileName(&fi.ofn);
				SetDlgItemText(hDlg, IDC_STATIC_OUTPUT,  fi.szFileName);
				break;
*/

			case IDC_RADIO_STATEDATAFORMAT_ANIMAT:
				userParms.output.outputByTime = FALSE;
				CheckDlgButton(hDlg, IDC_RADIO_STATEDATAFORMAT_ANIMAT, BST_CHECKED);
				CheckDlgButton(hDlg, IDC_RADIO_STATEDATAFORMAT_ITERATION, BST_UNCHECKED);
				break;

			case IDC_RADIO_STATEDATAFORMAT_ITERATION:
				userParms.output.outputByTime = TRUE;
				CheckDlgButton(hDlg, IDC_RADIO_STATEDATAFORMAT_ANIMAT, BST_UNCHECKED);
				CheckDlgButton(hDlg, IDC_RADIO_STATEDATAFORMAT_ITERATION, BST_CHECKED);
				break;

			case IDC_BUTTON_STATE_NONE:
				// Doesn't alter output configuration other than to set .enabled to FALSE
				userParms.output.enabled = !userParms.output.enabled;

				if(userParms.output.enabled == FALSE)
					SetDlgItemText(hDlg, IDC_BUTTON_STATE_NONE, "Enable Output");
				else
					SetDlgItemText(hDlg, IDC_BUTTON_STATE_NONE, "No Output");
				UpdateBinOutConfigDlgState(&userParms, hDlg);
				UpdateCalculations(params->sce, hDlg, &userParms);
				break;
			case IDC_BUTTON_STATE_MIN:
				// Configures the binary output minimum
				m_staticScenario.SetMinBinOutFileConfiguration(&userParms.output);
				UpdateBinOutConfigDlgState(&userParms, hDlg);
				UpdateCalculations(params->sce, hDlg, &userParms);
				break;

			case IDC_BUTTON_STATE_FULL:
				// Configures the binary output to output everything
				m_staticScenario.SetAllBinOutFileConfiguration(&userParms.output);
				UpdateBinOutConfigDlgState(&userParms, hDlg);
				UpdateCalculations(params->sce, hDlg, &userParms);
				break;

			case IDC_RANDOM_SEED_AUTO:
				CheckRadioButton(hDlg, IDC_RANDOM_SEED_AUTO, IDC_RANDOM_SEED_SET, IDC_RANDOM_SEED_AUTO);
				userParms.seed.useCurrentTick = TRUE;
				EnableWindow(GetDlgItem(hDlg, IDC_RANDOM_SET_SEED_VALUE), FALSE);
				break;

			case IDC_ORTHOGONAL_RANDOM_VARS:
				userParms.seed.independentAnimatRandomGen = !userParms.seed.independentAnimatRandomGen;
				break;

			case IDC_RANDOM_SEED_SET:
				CheckRadioButton(hDlg, IDC_RANDOM_SEED_AUTO, IDC_RANDOM_SEED_SET, IDC_RANDOM_SEED_SET);
				userParms.seed.useCurrentTick = FALSE;
				EnableWindow(GetDlgItem(hDlg, IDC_RANDOM_SET_SEED_VALUE), TRUE);
				break;

			case IDC_BUTTON_OUTPUTFOLDER:
				if(TRUE == BrowseFolder("Select Output Folder...", params->szOutputFolder))
				{
					*params->outputFolderSet = TRUE;
					SetDlgItemText(hDlg, IDC_STATIC_STATE_OUTPUTFOLDER, params->szOutputFolder);
				}
				break;


			case IDC_OUTPUT_TEXT:
				if(params->texConfig->enabled == TRUE)
					params->texConfig->enabled = FALSE;
				else
					params->texConfig->enabled = TRUE;
				break;

			case IDC_RADIO_DISTCALC_LATLO:
				CheckRadioButton(hDlg, IDC_RADIO_DISTCALC_LATLO, IDC_RADIO_DISTCALC_PGEOM, IDC_RADIO_DISTCALC_LATLO);
				userParms.distCalcMethod = LAT_LON;
				break;

			case IDC_RADIO_DISTCALC_PGEOM:
				CheckRadioButton(hDlg, IDC_RADIO_DISTCALC_LATLO, IDC_RADIO_DISTCALC_PGEOM, IDC_RADIO_DISTCALC_PGEOM);
				userParms.distCalcMethod = PLANAR_GEOMETRY;
				break;

			case IDC_RADIO_OUTPUT_TXT_SINGLE:
				CheckRadioButton(hDlg, IDC_RADIO_OUTPUT_TXT_SINGLE, IDC_RADIO_OUTPUT_TXT_MULTI, IDC_RADIO_OUTPUT_TXT_SINGLE);
				params->texConfig->splitTextOutput = FALSE;
				EnableWindow(GetDlgItem(hDlg, IDC_OUTPUT_TXT_ITSPERFILE), FALSE);
				break;

			case IDC_RADIO_OUTPUT_TXT_MULTI:
				CheckRadioButton(hDlg, IDC_RADIO_OUTPUT_TXT_SINGLE, IDC_RADIO_OUTPUT_TXT_MULTI, IDC_RADIO_OUTPUT_TXT_MULTI);
				params->texConfig->splitTextOutput = TRUE;
				EnableWindow(GetDlgItem(hDlg, IDC_OUTPUT_TXT_ITSPERFILE), TRUE);
				break;

			case IDC_CHECK_BATHYMAP:
				userParms.output.headerInf.bathyMap = !userParms.output.headerInf.bathyMap;
				CheckDlgButton(hDlg, IDC_CHECK_BATHYMAP, userParms.output.headerInf.bathyMap);
				break;

			case IDC_CHECK_SALINITYMAP:
				userParms.output.headerInf.salinityMap = !userParms.output.headerInf.salinityMap;
				CheckDlgButton(hDlg, IDC_CHECK_SALINITYMAP, userParms.output.headerInf.salinityMap);
				break;
			case IDC_CHECK_TEMPMAP:
				userParms.output.headerInf.temperatureMap = !userParms.output.headerInf.temperatureMap;
				CheckDlgButton(hDlg, IDC_CHECK_TEMPMAP, userParms.output.headerInf.temperatureMap);
				break;
			case IDC_CHECK_AESTATS:
				userParms.output.headerInf.postRunAnalysis = !userParms.output.headerInf.postRunAnalysis;
				CheckDlgButton(hDlg, IDC_CHECK_AESTATS, userParms.output.headerInf.postRunAnalysis);
				break;
			case IDC_CHECK_ANIMATSPEASSOCIATION:
				userParms.output.headerInf.speInfAndAnimatAsscn = !userParms.output.headerInf.speInfAndAnimatAsscn;
				CheckDlgButton(hDlg, IDC_CHECK_ANIMATSPEASSOCIATION, userParms.output.headerInf.speInfAndAnimatAsscn);
				break;

				
			case IDC_CHECK_CLOCK: // Not user configurable
//				userParms.output.animat.timeOfDay = !userParms.output.animat.timeOfDay;
//				CheckDlgButton(hDlg, IDC_CHECK_CLOCK, userParms.output.animat.timeOfDay);
				break;
			case IDC_CHECK_ID:
//				userParms.output.animat.ID = !userParms.output.animat.ID;
//				CheckDlgButton(hDlg, IDC_CHECK_ID, userParms.output.animat.ID);
				break;
			case IDC_CHECK_COORD:
//				userParms.output.animat.coordinate = !userParms.output.animat.coordinate;
//				CheckDlgButton(hDlg, IDC_CHECK_COORD, userParms.output.animat.coordinate);
				break;
			case IDC_CHECK_DEPTH:
				//userParms.output.animat.depth = !userParms.output.animat.depth;
				//CheckDlgButton(hDlg, IDC_CHECK_DEPTH, userParms.output.animat.depth);
				break;
			case IDC_CHECK_PACKED:
//				userParms.output.animat.packedData = !userParms.output.animat.packedData;
//				CheckDlgButton(hDlg, IDC_CHECK_PACKED, userParms.output.animat.packedData);
				break;
			case IDC_CHECK_BEARING:
				userParms.output.animat.bearing = !userParms.output.animat.bearing;
				CheckDlgButton(hDlg, IDC_CHECK_BEARING, userParms.output.animat.bearing);
				UpdateCalculations(params->sce, hDlg, &userParms);
				break;
			case IDC_CHECK_TRAVELRATE:
				userParms.output.animat.travelRate = !userParms.output.animat.travelRate;
				CheckDlgButton(hDlg, IDC_CHECK_TRAVELRATE, userParms.output.animat.travelRate);
				UpdateCalculations(params->sce, hDlg, &userParms);
				break;
			case IDC_CHECK_DIVERATE:
				userParms.output.animat.diveRate = !userParms.output.animat.diveRate;
				CheckDlgButton(hDlg, IDC_CHECK_DIVERATE, userParms.output.animat.diveRate);
				UpdateCalculations(params->sce, hDlg, &userParms);
				break;
			case IDC_CHECK_TARGETDEPTH:
				userParms.output.animat.targetDepth = !userParms.output.animat.targetDepth;
				CheckDlgButton(hDlg, IDC_CHECK_TARGETDEPTH, userParms.output.animat.targetDepth);
				UpdateCalculations(params->sce, hDlg, &userParms);
				break;
			case IDC_CHECK_XYDISTANCEMETERS: // (41)
				userParms.output.animat.xyDistance = !userParms.output.animat.xyDistance;
				CheckDlgButton(hDlg, IDC_CHECK_XYDISTANCEMETERS, userParms.output.animat.xyDistance);
				UpdateCalculations(params->sce, hDlg, &userParms);
				break;
			case IDC_CHECK_RISK: // (42)
				userParms.output.animat.risk = !userParms.output.animat.risk;
				CheckDlgButton(hDlg, IDC_CHECK_BEARING, userParms.output.animat.risk);
				UpdateCalculations(params->sce, hDlg, &userParms);
				break;
			case IDC_CHECK_BATHYVALUE:
				userParms.output.animat.bathyDepth = !userParms.output.animat.bathyDepth;
				CheckDlgButton(hDlg, IDC_CHECK_BATHYVALUE, userParms.output.animat.bathyDepth);
				UpdateCalculations(params->sce, hDlg, &userParms);
				break;
			case IDC_CHECK_SALINITYVALUE:
				userParms.output.animat.salinity = !userParms.output.animat.salinity;
				CheckDlgButton(hDlg, IDC_CHECK_SALINITYVALUE, userParms.output.animat.salinity);
				UpdateCalculations(params->sce, hDlg, &userParms);
				break;
			case IDC_CHECK_TEMPVALUE:
				userParms.output.animat.temperature = !userParms.output.animat.temperature;
				CheckDlgButton(hDlg, IDC_CHECK_TEMPVALUE, userParms.output.animat.temperature);
				UpdateCalculations(params->sce, hDlg, &userParms);
				break;
			case IDC_CHECK_AECUMTV:
				userParms.output.animat.aeCmltve = !userParms.output.animat.aeCmltve;
				CheckDlgButton(hDlg, IDC_CHECK_AECUMTV, userParms.output.animat.aeCmltve);
				UpdateCalculations(params->sce, hDlg, &userParms);
				break;
			case IDC_CHECK_AEINST:
				userParms.output.animat.aeMoment = !userParms.output.animat.aeMoment;
				CheckDlgButton(hDlg, IDC_CHECK_AEINST, userParms.output.animat.aeMoment);
				UpdateCalculations(params->sce, hDlg, &userParms);
				break;
			case IDC_CHECK_AEANGLE:
				userParms.output.animat.aeRelAngle = !userParms.output.animat.aeRelAngle;
				CheckDlgButton(hDlg, IDC_CHECK_AEANGLE, userParms.output.animat.aeRelAngle);
				UpdateCalculations(params->sce, hDlg, &userParms);
				break;
			case IDC_CHECK_AETIME:
				userParms.output.animat.aeTimeAvrt = !userParms.output.animat.aeTimeAvrt;
				CheckDlgButton(hDlg, IDC_CHECK_AETIME, userParms.output.animat.aeTimeAvrt);
				UpdateCalculations(params->sce, hDlg, &userParms);
				break;
			case IDC_CHECK_AECOORDSTATE:
				userParms.output.AECoordinate = !userParms.output.AECoordinate;
				CheckDlgButton(hDlg, IDC_CHECK_AECOORDSTATE, userParms.output.AECoordinate);
				UpdateCalculations(params->sce, hDlg, &userParms);
				break;
			}
			break;
	}
    return FALSE;
}

