
// Windows Header Files:
#include <windows.h>

// C RunTime Header Files
#include <stdlib.h>
#include <malloc.h>
#include <memory.h>
#include <tchar.h>
#include <stdio.h>
#include <time.h>
#include <sys/types.h>
#include <sys/timeb.h>
#include <commctrl.h>
#include <io.h>

#include "3mbsLib.h"
#include "staticLib.h"


CScenario sce;


#define MMMBS_VERFICTN_APP_VER	1.0
#define SZTESTCASE "TestCase"
#define szScenarioFileNameTESTCASE "ScenarioTestCase"
#define TICKUPDATE 200
#define NUMVALIDSWITCHES 3

int		 failCount			= 0;
int		 successCount		= 0;
int		 generateNewCount	= 0;
int		 skippedCount		= 0;
int		 totalCount			= 0;
int		 errorCount			= 0;

const TCHAR *initMess[4];

void RunScenario(TCHAR *szScenarioFileName, BOOL GenerateNew, BOOL AeOn, BOOL DeleteOuputFile, double AeStart, int LineNumber)
{
	TCHAR szScenarioPath[MAX_PATH];
	TCHAR szScenarioFileTitle[MAX_PATH];
	TCHAR szSetStoredFileTitle[MAX_PATH];
	TCHAR szSetCompareFileTitle[MAX_PATH];
	TCHAR szBuffer[MAX_PATH];
	USERPARAMS mbsSceConfiguration;
	WIN32_FIND_DATA	findFileData;
	HANDLE hndFindFile = NULL;
	RESLT mbsRes;
	int numAnimats;
	double *aeArray = NULL;
	BATHYEXTREMES envMinMax;
	double aeLat, aeLon;
	INHABITINF *animatCoordArray = NULL;
	BOOL aeCycleOn = FALSE;
	int i;
	int initMesIndex = 0;
	DWORD lastUpdateTick = GetTickCount();
	int halfDuration = 0;
	int secsElapsed = 0;
	int	halfDutyCycleSeconds = 120; // run with AE off for 120 seconds, then on for 120 seconds.
	C3mbStaticsLib staticLib;


	// Set up string buffers: Get the input scenario file title and build the generate
	// and compare mbs binary file output titles.
	staticLib.GetPathAndFileTitleFromFileName(szScenarioFileName,
									szScenarioPath,
									TCHARBFLEN(szScenarioPath),
									szScenarioFileTitle,
									TCHARBFLEN(szScenarioFileTitle));
	strcpy_s(szBuffer, TCHARBFLEN(szBuffer), szScenarioFileTitle);
	staticLib.RemoveExtension(szBuffer);
	sprintf_s(szSetStoredFileTitle, TCHARBFLEN(szSetStoredFileTitle), "%s", szBuffer);
	sprintf_s(szSetCompareFileTitle, TCHARBFLEN(szSetCompareFileTitle), "%sCompare", szBuffer);

	// Prevent class scenario from appending a run count to the outputted binary files.
	sce.ResetRunCount();

	// Temporarily change the working directory to the path specified by the string
	// 'szScenarioPath' so outputted data will be saved here.
	if((strlen(szScenarioPath) > 0) && (FALSE == SetCurrentDirectory(szScenarioPath)))
	{
		printf("Invalid path for test scenario %s.  Skipping...\n", szScenarioFileName);
		skippedCount++;
		return;
	}

	// Load the scenario file into the scenario instance.
	if(OK != (mbsRes = sce.LoadFromBinFile(szScenarioFileName)))
	{
		printf("Failed to load scenario %s on line %d, skipping...\n", szScenarioFileTitle, LineNumber);
		sce.ClearScenario();
		errorCount++;
		return;
	}

	// Either the validation application is creatin new data to compare against in the
	// future or is doing a comparison.
	if(GenerateNew == TRUE)
	{
		// Set the title.
		sce.SetScenarioTitle(szSetStoredFileTitle);

		// Save a text printout of the contents of the scenario to a text file so that
		// there is documentation of all the parameters should a visual (human)
		// comparision ever be needed.
		strcat_s(szSetStoredFileTitle, TCHARBFLEN(szSetStoredFileTitle), ".txt");
		sce.ScenarioToText(szSetStoredFileTitle);
		staticLib.RemoveExtension(szSetStoredFileTitle);

		// Set the configuration.
		mbsSceConfiguration = sce.GetConfiguration();
//		mbsSceConfiguration.output.text.enabled = TRUE;
//		if(DeleteOuputFile == TRUE)
//			mbsSceConfiguration.output.text.enabled = FALSE;
	}
	else
	{
		//------------------------------------------------------------------------------//
		// Comparing current results against a previously generated file.
		//--------------------------------------------------------------//
		// Make sure the original file exists that is being compared to.  If not, skip
		// this comparision.
		strcat_s(szSetStoredFileTitle, TCHARBFLEN(szSetStoredFileTitle), MMMBS_OUTPUT_EXT);
		if(INVALID_HANDLE_VALUE == (hndFindFile = FindFirstFile(szSetStoredFileTitle, &findFileData)))
		{
			printf("Missing file %s to compare scenario %s line %d. Skipping...\n",
				szBuffer,  szScenarioFileTitle, LineNumber);
			sce.ClearScenario();
			skippedCount++;
			return;
		}
		FindClose(hndFindFile);

		// Set the title of the newly generated file being compared to the old.
		sce.SetScenarioTitle(szSetCompareFileTitle);

		// Set the configuration of the newly generated scenario file so that it DOESN'T
		// output text, then run the scenario.
		mbsSceConfiguration = sce.GetConfiguration();
//		mbsSceConfiguration.output.text.enabled = FALSE;
	}

	// Set the configuration
	sce.SetConfiguration(mbsSceConfiguration);


	//----------------------------------------------------------------------------------//
	// Start up the scenario.
	//----------------------//
	if(AeOn == TRUE) // Handle acoustic exposure startup
	{

		//------------------------------------------------------------------------------------//
		// Determine when to bring acoustic expoure values up to full
		halfDuration = sce.GetDurationSeconds() / 2;
		//------------------------------------------//

		//------------------------------------------------------------------------------------//
		// Initialization for scenarios with acoustic exposure on
		//-------------------------------------------------------//
		printf("Initializing:                         ");

		// Initialize local variables needed for acoustic exposure scenario.  Scenarios
		// using acoustic exposure must provide and set the location and values of each
		// animat in the scenario.

		//------------------------------------------------------------------------------------//
		// Allocate needed memory buffers for acoustic exposure.
		//-----------------------------------------------------..
		// GetAnimatCoordinates() with NULL passed in will allocate memory for the
		// COORD_DEPTH buffer.  This must be deleted locally when finished.
		numAnimats = sce.GetAnimatCount();
		animatCoordArray = sce.GetAnimatPopulationCurrentCoordinates(animatCoordArray); 
		if(NULL == (aeArray = new double[numAnimats]))
		{
			printf("\n\nMemory Allocation Error... exiting\n");
			ExitProcess(1);
		}
		
		// The acoustic exposure used here doesn't represent anything truely being
		// simulated.  Simply place a location for it that is in the middle of the
		// x,y coordinates.
		memset(&envMinMax, 0, sizeof(envMinMax));
		if(sce.BathymetryLoaded() == TRUE)
			envMinMax = sce.GetBathymetryExtremes();
		aeLat = (envMinMax.xMax-envMinMax.xMin)/2;
		aeLon = (envMinMax.yMax-envMinMax.yMin)/2;

		// Initialize the scenario.  Scenario member function RunScenario() launches the
		// scenario execuation in its own thread, so the code following this line must be
		// treated as happening simultaneously with the running scenario.
		if(OK != (mbsRes = sce.RunScenario(1)))
		{
			printf("%s\n\n", staticLib.MbsResultToString(mbsRes, szBuffer, SIZE_128));
			errorCount++;

			if(aeArray != NULL) delete [] aeArray;
			aeArray = NULL;

			if(animatCoordArray != NULL) delete [] animatCoordArray;
			animatCoordArray = NULL;
			return;
		}

		printf("Initializing: %s                      \r", initMess[initMesIndex++]);
		lastUpdateTick = GetTickCount();
		while(sce.GetRunState_old() == INITIALIZING)
		{
			Sleep(25);
			if(GetTickCount() - lastUpdateTick >= TICKUPDATE)
			{
				initMesIndex %= 4;
				printf("Initializing: %s                      \r", initMess[initMesIndex++]);
				lastUpdateTick = GetTickCount();
			}
		}


//		printf("\nRun Progress: %d percent complete   %s              \r", sce.GetPercentDone(), initMess[initMesIndex++]);
		lastUpdateTick = GetTickCount();
		do
		{
			Sleep(25);
			if(GetTickCount() - lastUpdateTick >= TICKUPDATE)
			{
				if(sce.GetRunState_old() == RUNNING || sce.GetRunState_old() == RUNPAUSED)
				{
					initMesIndex %= 4;
//					printf("Run Progress: %d percent complete   %s              \r",
//						sce.GetPercentDone(), initMess[initMesIndex++]);
					lastUpdateTick = GetTickCount();
				}
			}

			if(sce.GetRunState_old() == RUNPAUSED)
			{
				if(aeCycleOn == TRUE)
				{
					if(secsElapsed >= halfDuration)
						AeStart = 20;
	
					// If this were a true acoutic exposure run, we'd want to get the
					// current position of every animat in the scenario in order to 
					// calculate it's acoustic exposure.  Not needed here, but do it
					// anyway.
					sce.GetAnimatPopulationCurrentCoordinates(animatCoordArray); // reuses memory, error checking

					// Process the ae based upon the animat coordinates.
					for(i=0; i<numAnimats; i++)
					{
						// Calculate ae based upon the animat's coordinates.
						// (do something with animatCoordArray).
						// ... calculation on animatCoordArray ...
						// ... another calculation on animatCoordArray...

						// Then assign acoustic exposures
						aeArray[i] = AeStart;
					}

					if(AeStart > 12)
						AeStart = 12;

					aeCycleOn = FALSE;
				}
				else
				{
					memset(aeArray, 0, numAnimats*sizeof(double));
					aeCycleOn = TRUE;
				}

				sce.SetAnimatAcousticExposure(aeLat, aeLon, aeArray);
				if(OK != (mbsRes =sce.RunScenario(halfDutyCycleSeconds)))
				{
					printf("%s\n\n", staticLib.MbsResultToString(mbsRes, szBuffer, SIZE_128));
					errorCount++;

					if(aeArray != NULL) delete [] aeArray;
					aeArray = NULL;

					if(animatCoordArray != NULL) delete [] animatCoordArray;
					animatCoordArray = NULL;
					return;
				}
				secsElapsed += halfDutyCycleSeconds;
			}
		}while(sce.GetRunState_old() == RUNNING || sce.GetRunState_old() == RUNPAUSED);

		if(aeArray != NULL) delete [] aeArray;
		aeArray = NULL;

		if(animatCoordArray != NULL) delete [] animatCoordArray;
		animatCoordArray = NULL;

	}
	else  // Not an acoustic exposure scenario.
	{
		if(OK != (mbsRes = sce.RunScenario()))
		{
			printf("%s\n\n", staticLib.MbsResultToString(mbsRes, szBuffer, SIZE_128));
			errorCount++;
			return;
		}
		printf("Initializing: %s                      \r", initMess[initMesIndex++]);
		lastUpdateTick = GetTickCount();
		while(sce.GetRunState_old() == INITIALIZING)
		{
			Sleep(25);
			if(GetTickCount() - lastUpdateTick >= TICKUPDATE)
			{
				initMesIndex %= 4;
				printf("Initializing: %s                      \r", initMess[initMesIndex++]);
				lastUpdateTick = GetTickCount();
			}
		}
		printf("Initializing:                         ");
//		printf("\nRun Progress: %d percent complete   %s              \r", sce.GetPercentDone(), initMess[initMesIndex++]);
		lastUpdateTick = GetTickCount();
	}

	// Allow for data extraction.
	while((OK == (mbsRes = sce.GetErrorStatus())) && sce.GetRunState_old() == RUNNING)
	{
		Sleep(25);
		if(GetTickCount() - lastUpdateTick < TICKUPDATE)
			continue;

		lastUpdateTick = GetTickCount();
		initMesIndex %= 4;
//		printf("Run Progress: %d percent complete   %s            \r", sce.GetPercentDone(), initMess[initMesIndex++]);
	}
//	printf("Run Progress: %d percent complete                  \n", sce.GetPercentDone());
	lastUpdateTick = GetTickCount();

	while((OK == (mbsRes = sce.GetErrorStatus())) && sce.GetRunState_old() == DATAEXTRACTING)
	{
		Sleep(25);
		if(GetTickCount() - lastUpdateTick < TICKUPDATE)
			continue;
		lastUpdateTick = GetTickCount();
		initMesIndex %= 4;
		printf("Extracting binary data into text files... %s\r", initMess[initMesIndex++]);
	}

	if(OK != mbsRes)
	{
		printf("%s\n\n", staticLib.MbsResultToString(mbsRes, szBuffer, SIZE_128));
		errorCount++;
		return;
	}

	if(GenerateNew == TRUE)
	{
		printf("Extracting binary data into text files... done\n");
		generateNewCount++;

		if(DeleteOuputFile == FALSE)
			return;

		// Delete the binary output file.
		strcat_s(szSetStoredFileTitle, sizeof(szSetStoredFileTitle), MMMBS_OUTPUT_EXT);
		DeleteFile(szSetStoredFileTitle);
	}
	else
	{
		// Do the comparision.
		strcat_s(szSetCompareFileTitle, sizeof(szSetStoredFileTitle), MMMBS_OUTPUT_EXT);
		if(sce.CompareFileOutput(szSetStoredFileTitle, szSetCompareFileTitle) == OK)
		{
			printf("Success!!\n\n");
			successCount++;
			DeleteFile(szSetCompareFileTitle);
		}
		else
		{
			printf("Fail!!\n\n");
			failCount++;
			staticLib.RemoveExtension(szSetCompareFileTitle);
			strcat_s(szSetCompareFileTitle, sizeof(szSetCompareFileTitle), ".txt");
			sce.ScenarioToText(szSetCompareFileTitle);

			staticLib.RemoveExtension(szSetCompareFileTitle);
			strcat_s(szSetCompareFileTitle, sizeof(szSetCompareFileTitle), MMMBS_OUTPUT_EXT);
#if 0
			sce.ExtractBinaryResultsIntoTextFiles(szSetCompareFileTitle);
#endif
			while(sce.GetRunState_old() != FINISHED)
				Sleep(1);
		}
	}

}


void main(int argc, TCHAR *argv[])
{
	FILE	*fd = NULL; // The inputted list of scenarios to examine.
	HANDLE	 hd = NULL;		 // Handle to scenarios.
	TCHAR	 szCmdPmtPath[SIZE_256];
	TCHAR	 szCmdPmtInputFileName[SIZE_256];
	TCHAR	 szCmdPmtInputTitleName[SIZE_128];
	DWORD	 pathLength;
	C3mbStaticsLib staticLib;
	int		 numLineConverstions;
	int		 lineNumber;
	int		 i;
	BOOL	 generateNew;
	BOOL	 aeOn = FALSE;
	BOOL	 deleteOuputFile = FALSE;
	TCHAR	 szBuffer[SIZE_256];
	TCHAR	 szLine[SIZE_256];

	TCHAR	 szScenarioFileName[SIZE_128];

	TCHAR	 szSwitch[NUMVALIDSWITCHES][SIZE_16];

	double aeDb;
	BOOL  aeCycleOn;


	//----------------------------------------------------------------------------------//
	// Print Banner
	//-------------//
	printf("3MBS Verfication Application ver %3.1f\n\n", MMMBS_VERFICTN_APP_VER);
	if(argc < 2)
	{
		printf("Missing input file name containing list of scenarios to examine\n\n");
		printf("Usage: 3MBSValidation [listfile.txt]\n");
		Sleep(5000);
		return;
	}

	strncpy_s(szCmdPmtInputFileName,			// Destination buffer
		TCHARBFLEN(szCmdPmtInputFileName),	// Length of the destination bufffer
		argv[1],								// String to be copied
		TCHARBFLEN(szCmdPmtInputFileName));	// Max count

	staticLib.GetPathAndFileTitleFromFileName(szCmdPmtInputFileName,					// Inputted file name
									szCmdPmtPath,							// Buffer for file path
									TCHARBFLEN(szCmdPmtPath),				// Length of the file path buffer
									szCmdPmtInputTitleName,					// Buffer for file title
									TCHARBFLEN(szCmdPmtInputTitleName));	// Length of the file title buffer

	// Set the path if one was specified.  Otherwise, get the current directory.
	if((strlen(szCmdPmtPath) > 0) && (FALSE == SetCurrentDirectory(szCmdPmtPath)))
	{
		printf("Invalid path (%s) specified in input: %s\n", szCmdPmtPath, szCmdPmtInputFileName);
		Sleep(5000);
		return;
	}
	else
	{
		pathLength = GetCurrentDirectory(SIZE_256, szCmdPmtPath);
		if(pathLength == 0 || pathLength > SIZE_256)
		{
			printf("Current directory is too deep\n");
			Sleep(5000);
			return;
		}
	}
		
	printf("Working Directory: %s\n  ", szCmdPmtPath);
	printf("Input File:        %s\n", szCmdPmtInputTitleName);

	fopen_s(&fd, szCmdPmtInputFileName, "r");
	if(fd == NULL)
	{
		printf("Unable to open file %s\n\n", szCmdPmtInputFileName);
		return;
	}

	// Scan the file.
	lineNumber = 0;
	while(NULL != fgets(szLine, SIZE_256, fd))
	{

		// Reset to the current file directory.
		SetCurrentDirectory(szCmdPmtPath);

		// Initialize/update variables.
		lineNumber++;

		// Skip comments and irrelevant tokens in the text file describing the test cases.
		if(szLine[0] == '%' || szLine[0] == ' ' || strlen(szLine) <= 1)
			continue;

		totalCount++;
		// A scenario will have, at a minimum, 5 characters (the name plus ".sce").
		if(strlen(szLine) < 5)
		{
			printf("\"%s\" on line %d is an invalid token, skipping...\n", szLine, lineNumber);
			skippedCount++;
			continue;
		}

		// Read in the name of the scenario to run and optional switches.
		memset(szSwitch, 0, NUMVALIDSWITCHES*SIZE_16);
		numLineConverstions = sscanf_s(szLine, "%s %s %s", szScenarioFileName, szSwitch[0], szSwitch[1], szSwitch[2]);
		if(numLineConverstions > NUMVALIDSWITCHES+1)
		{
			printf("\"%s\" on line %d has too many tokens, skipping...\n", szLine, lineNumber);
			skippedCount++;
			continue;
		}

		if(szLine[strlen(szLine) - 1] == '\n')
			szLine[strlen(szLine) - 1] = 0;
		printf("\n%s            \n", szLine);

		// Verify the extension is correct.
		staticLib.GetExtension(szScenarioFileName, szBuffer, SIZE_256);
		if(strcmp(szBuffer, ".sce") != 0)
		{
			printf("\"%s\" on line %d has invalid extension, skipping...\n", szScenarioFileName, lineNumber);
			skippedCount++;
			continue;
		}

		// Default values
		aeDb = 0.25f;
		aeCycleOn = FALSE;
		generateNew = FALSE;
		aeOn = FALSE;

		// Determine switches (magic number 3 because at this time there can be only 
		// three switches).
		for(i=0; i<NUMVALIDSWITCHES && strlen(szSwitch[i]); i++)
		{
			if(strcmp(szSwitch[i], "-g") == 0)
				generateNew = TRUE;
			else if(strcmp(szSwitch[i], "-a") == 0)
				aeOn = TRUE;
			else if(strcmp(szSwitch[i], "-d") == 0)
			{
				generateNew = TRUE;
				deleteOuputFile = TRUE;
			}
			else
			{
				printf("\"%s\" on line %d has an invalid switch, skipping...\n", szLine, lineNumber);
				skippedCount++;
			}
		}
		RunScenario(szScenarioFileName, generateNew, aeOn, deleteOuputFile, .001, lineNumber);
	}
	fclose(fd);

	printf("\n");

	printf("Summary Results:\n");
	printf("Total Files:%d  Total Errors:%d\n", totalCount, errorCount);
	printf("\tGenerate New: %d\n", generateNewCount);
	printf("\tComparisons: Success:%d  Fail:%d  Skipped:%d\n", successCount, failCount, skippedCount);
}
