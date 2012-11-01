// 3MBAutoTest.cpp : Defines the entry point for the console application.
//

#include <stdio.h>
#include <conio.h>
#include <tchar.h>
#include <windows.h>
#include "3mbslib.h"
#include "FileExtracter.h"

__int64 ValidateTrackFileFormat(int AnimatIndex, int Duration, TCHAR *szFileName, CWorkingList *pWorkingList = NULL);
int ParseCommandLineInput_autoTest(CONSOLEINPUT *Input, int argc, TCHAR *argv[]);
void EndProcessOnError(CWorkingList *pWorkingList = NULL);
void DelayNextAttempt();
void CheckForUserPause();
void DisplayError(DWORD Error);

TCHAR *SZHELPSTRING_AUTOTEST = "\
\n\
Usage:\n\n\
 3MBc [sce] [csv] [dir] [flags: -b -a -p -e -f]\n\n\
     Input Options:\n\
      []      (no command line entries) runs all scenarios across first\n\
              first csv file encountered and across ping cycles.\n\
      [sce]   Specifies a 3MB scenario file (.sce) to run.  Overrides the -b\n\
              flag.\n\
      [csv]   Specifies a comma-separated values (.csv) file that lists a sub\n\
              set of iterations to be written to the 3MB binary output file\n\
              (.3mb).  If both this option and the -p flag are set and an\n\
              acoustic source seeded in the scenario then the outputted\n\
              iterations are a composite of both the [csv] file and the\n\
              acoustic source ping iterations.\n\
      -b      Batch processing.  Run all scenarios in the current directory.\n\
              This option overriden if the [sce] file option is set.\n\
      -p      Output limited to the acoustic source ping cycles.  Ignored if\n\
              (not run) if the scenario not populated with a sound source.\n\
              See also the [csv] option.\n\
      -f      Fast execution.  Checks only first and last 250 track files.\n\
              files for verification\n\
              \n";

/*
No args: runs normal
[sce]
-n:   runs normal
[csv]: runs csv
-p:   runs ping cycle
*/

enum RUNTESTTYPE
{
	NORMAL = 0,
	CSV = 1,
	PING = 2,
	CSVPING = 3,
};

TCHAR *RunTestTypeToString(RUNTESTTYPE Type, TCHAR *szBuffer, int BufferLength)
{
	if(szBuffer == NULL || BufferLength == 0)
		return NULL;

	switch(Type)
	{
	case NORMAL:
		strcpy_s(szBuffer, BufferLength, "NRM");
		break;
	case CSV:
		strcpy_s(szBuffer, BufferLength, "CSV");
		break;
	case PING:
		strcpy_s(szBuffer, BufferLength, "PNG");
		break;
	case CSVPING:
		strcpy_s(szBuffer, BufferLength, "C&P");
		break;
	}
	return szBuffer;
}


int main(int argc, _TCHAR* argv[])
{
	TCHAR szFolder[SIZE_256];
	TCHAR szBuff[SIZE_256];
	TCHAR szBuff2[SIZE_256];
	TCHAR szScenarioNameBuff[SIZE_256];
	FINDINF findSce = {0}, mbs = {0}, trk = {0}, sts = {0}, mem = {0}, csv = {0}, rpt = {0};
	int trackFilesExtractedCnt;
	DWORD numTrackFileGroups;
	DWORD feedbackTick = GetTickCount();
	DWORD scenarioRunTimeTick;
	HHMMSS hhmmss;
	SYSTEMTIME sysTime;
	RUNTESTTYPE runType;
	C3mbStaticsLib staticLib;

	int runNum;

	//----------------------//
	// Running Scenario vars
	CScenario sce;
	CFileExtracter fileExtracter;
	CWorkingList *pWorkingList = NULL;
	SCESTATE state;
	FESTATE feState;
	USERPARAMS config;
	//SCEACTIVITY activity;
	FILE *fdRunTimeReport = NULL;
	FILE *fdIterationReport = NULL;
	TCHAR szReportName[SIZE_256];

	DWORD trackFileGroupNum;
	__int64 animatCount;
	__int64 duration;
	__int64 totalInterationsRead;
	__int64 interationsRead;
	DWORD trackFileCount;
	FESETUP feSetup;


	// Report start time.
	GetLocalTime(&sysTime);
	printf("StartTime: %02d:%02d:%02d\n\n", sysTime.wHour, sysTime.wMinute, sysTime.wSecond);


	// Parse command line arguments
	CONSOLEINPUT input;
	memset(&input, 0, sizeof(input));
	BOOL fullRun = FALSE;
	if(argc == 1) // user didn't enter any command line arguments.
		fullRun = TRUE;
	else if(0 == ParseCommandLineInput_autoTest(&input, argc, argv))
		fullRun = TRUE;

	if(input.help == TRUE)
	{
		printf_s(SZHELPSTRING_AUTOTEST);
		ExitProcess(0);
	}


	//----------------------//

	// Intialize variables
	strcpy_s(findSce.sz, sizeof(findSce.sz), "*.findSce");
	strcpy_s(mbs.sz, sizeof(mbs.sz), "*.3mb");
	strcpy_s(trk.sz, sizeof(trk.sz), "*.trk");
	strcpy_s(sts.sz, sizeof(sts.sz), "*.sts");
	strcpy_s(csv.sz, sizeof(csv.sz), "*.csv");
	strcpy_s(mem.sz, sizeof(mem.sz), "*.mem");
	strcpy_s(rpt.sz, sizeof(rpt.sz), "*.rprt");

	//----------------------------------------------------------------------------------//
	// 1. Delete any .3mb files present and associated folders so they can be built from
	// scratch and to save disk space.
	//----------------------------------------------------------------------------------//
	while(TRUE == staticLib.MyFindFile(&mbs))
	{
		// Form the name of the folder associated with a .3mb file by copying the name of
		// the .3mb file into the 'szFolder' string buffer then removing the extension.
		strcpy_s(szFolder, sizeof(szFolder), mbs.data.cFileName);
		staticLib.RemoveExtension(szFolder);

		// If it is found, delete the folder associated with the found .3mb file
		printf("Deleting folder %s and contents in it\n", szFolder);
		staticLib.MyDeleteFolder(szFolder);

		// Now delete the .3mb file
		printf("Deleting file %s\n", mbs.data.cFileName);
		if(0 == DeleteFile(mbs.data.cFileName))
		{
			printf("Delete file %s FAILED\n", mbs.data.cFileName);
			while(0 == _kbhit())
				Sleep(500);
			ExitProcess(0);
		}
	}

	FindClose(mbs.hdl);
	mbs.hdl = NULL;


	// Delete the track .trk  files.
	while(TRUE == staticLib.MyFindFile(&trk))
	{
		//printf("deleting %s\n", trk.data.cFileName);
		DeleteFile(trk.data.cFileName);
		Sleep(10);
	}
	FindClose(trk.hdl);
	trk.hdl = NULL;


	// Delete the memory files that show buffer placement.
	while(TRUE == staticLib.MyFindFile(&mem))
	{
		//printf("deleting %s\n", trk.data.cFileName);
		DeleteFile(mem.data.cFileName);
		Sleep(10);
	}
	FindClose(mem.hdl);
	mem.hdl = NULL;

	
	// Delete the report (.rprt) files that iterations read in counts.
	while(TRUE == staticLib.MyFindFile(&rpt))
	{
		//printf("deleting %s\n", trk.data.cFileName);
		DeleteFile(rpt.data.cFileName);
		Sleep(10);
	}
	FindClose(rpt.hdl);
	mem.hdl = NULL;

	//----------------------------------------------------------------------------------//
	// One scenario or multiple scenarios?
	//------------------------------------//
	// Intially set the FINDINF struct to handle all scenario (.sce) files in the current
	// directory for batch processing.  If input params set for a single scenario change
	// the FINDINF struct accordingly.
	strcpy_s(findSce.sz, sizeof(findSce.sz), "*.sce");
	if(input.sce.set == TRUE) // single scenario
	{
		strcpy_s(findSce.sz, sizeof(findSce.sz), input.sce.szFileName);
		if(FALSE == staticLib.MyFindFile(&findSce))
		{
			printf("Did not locate scenario:\n\t\"%s\"\n", findSce.sz);
			EndProcessOnError(pWorkingList);
		}
		FindClose(findSce.hdl);
		findSce.hdl = NULL;
	}



	// Specific CSV file or first one encountered.
	memset(&csv, 0, sizeof(csv));
	if(input.csv.set == TRUE)
	{
		 // A specific CSV file was entered as part of the command line arguments.
		strcpy_s(csv.sz, sizeof(csv.sz), input.csv.szFileName);
		if(FALSE == staticLib.MyFindFile(&csv))
		{
			printf("Did not locate cs file:\n\t\"%s\"\n", csv.sz);
			memset(&csv, 0, sizeof(csv));
			EndProcessOnError(pWorkingList);
		}
		FindClose(csv.hdl);
		csv.hdl = NULL;
	}
	else if(fullRun == TRUE)
	{
		strcpy_s(csv.sz, sizeof(csv.sz), "*.csv");

		// No specific CSV file was entered in the command line arguments so find the
		// first in the current folder.  Not an error condition if non found.
		if(FALSE == staticLib.MyFindFile(&csv))
			memset(&csv, 0, sizeof(csv));
		FindClose(csv.hdl);
		csv.hdl = NULL;
	}


	// Run the scenario or scenarios
	while(TRUE == staticLib.MyFindFile(&findSce))
	{
		// Load the sce file into the sce instance
		if(OK != sce.LoadFromBinFile(findSce.data.cFileName))
		{
			// If set to run only a single scenario file (via command line arguments)
			// then exit.  Otherwise delay and beep to notify user there was a problem
			// then continue if no user response.
			printf("Failed to load sce %s\n", findSce.data.cFileName);
			if(input.sce.set == TRUE)
				EndProcessOnError(pWorkingList);
			else
				EndProcessOnError(pWorkingList); //DelayNextAttempt();
		}

		// Intialize sce-related variables
		config = sce.GetConfiguration();
		animatCount = (__int64)sce.GetAnimatCount();
		duration = (__int64)sce.GetDurationSeconds();


		// Print a "\n" in the time report file to separate specific scenarios.
		_ASSERT(fdRunTimeReport == NULL);
		fopen_s(&fdRunTimeReport, "RunTimeReport.txt", "a");
		fprintf(fdRunTimeReport, "\n");
		fclose(fdRunTimeReport);
		fdRunTimeReport = NULL;


		for(runNum=0; runNum<4; runNum++)
		{
			if(runNum == 0 && fullRun == TRUE)
			{
				 // Normal scenario run, only get to do normal runs when set up for full runs.
				runType = NORMAL;

				// Clear any potential CSV loaded files and potential to run with ping-
				// limited output.
				sce.ClearCSVList();
				sce.SetAcousticSrceLimitOutput(FALSE);
			}
			else if(runNum == 1)
			{
				// Considers running scenario with CSV file
				runType = CSV;

				// A .csv file needs to have been found in the current folder for this
				// option to be run.  If the string length of the file name is zero no
				// .csv file was found.
				if(0 == strlen(csv.data.cFileName))
					continue;

				// Otherwise, set to run with a scenario file and clear any potential for
				// running ping-limited output.
				if(OK != sce.ReadCSVListFromFile(csv.data.cFileName))
				{
					printf("Failed to load CSV iteration save list from file:\n  %s\n", csv.data.cFileName);
					EndProcessOnError(pWorkingList);//DelayNextAttempt();
					continue; // try the next sceanario.
				}
				sce.SetAcousticSrceLimitOutput(FALSE);

			}
			else if(runNum == 2) // Considers limiting output by acoustic source ping cycle.
			{
				runType = PING;


				// A sound source needs to have been populated into the scenario for this
				// to matter
				if(sce.SoundSourceSpeciesPresent() == FALSE)
					continue;

				// Either a complete (full) run needs to have been set or specifically a
				// ping-limited run needs to have been specified.
				if(fullRun == FALSE && input.pingLimted == FALSE)
					continue;

				// Otherwise clear any potential CSV list and set to run with ping-limited output.
				sce.ClearCSVList();
				sce.SetAcousticSrceLimitOutput(TRUE);
			}
			else if(runNum == 3) // Rus the scenario with CSV file and acoustic source ping-limited output
			{
				runType = CSVPING;

				// A .csv file needs to have been found (won't be if not specified or
				// non in the current folder)
				if(0 == strlen(csv.data.cFileName))
					continue;

				// A sound source needs to be present.
				if(sce.SoundSourceSpeciesPresent() == FALSE)
					continue;

				// Full run needs to be the set up or ping limited needs to have been set
				// (csv file needs to have been found too but wont' be if not specifically
				// set or fullrun enabled).
				if(fullRun == FALSE && input.pingLimted == FALSE)
					continue; // loop around.

				// Run the scenario with both CSV and ping-limited output limitations.
				if(OK != sce.ReadCSVListFromFile(csv.data.cFileName))
				{
					printf("Failed to load CSV iteration save list from file:\n  %s\n", csv.data.cFileName);
					EndProcessOnError(pWorkingList);//DelayNextAttempt();
					continue;
				}
				sce.SetAcousticSrceLimitOutput(TRUE);
			}
			else
			{
				continue;
			}

			// Get the working list if this was an iteration-limited scenario.
			pWorkingList = NULL;
			if(runNum == 1 || runNum == 2 || runNum == 3)
				pWorkingList = sce.GetWorkingList(staticLib.Time_ToSeconds(sce.GetStartTime()), (int)duration);


			totalInterationsRead = 0;
			trackFilesExtractedCnt = 0;
			trackFileCount = 0;


			// Set up the scearnio's title for the output file and set it.
			strcpy_s(szScenarioNameBuff, sizeof(szScenarioNameBuff), findSce.data.cFileName);
			staticLib.RemoveExtension(szScenarioNameBuff);

			RunTestTypeToString(runType, szBuff2, sizeof(szBuff2));
			sprintf_s(szBuff, sizeof(szBuff), "%d%s_%s", runNum, szBuff2, szScenarioNameBuff);
			strcpy_s(szScenarioNameBuff, sizeof(szScenarioNameBuff), szBuff);
			sce.SetScenarioTitle(szScenarioNameBuff);

			// Display select sce information
			printf("\n\n");
			printf("**********************************************************************\n");
			printf("%s\n", findSce.data.cFileName);
			if(config.output.outputByTime == TRUE)
				printf("\t Output by:     time\n");
			else
				printf("\t Output by:     animat\n");
			printf("Animat count:     %I64d\n", animatCount);
			printf("Iteration count:: %I64d\n", duration);


#if 1
			//------------------------------------------------------------------------------------//
			// Run the sce and print feedback once a second while running
			//----------------------------------------------------------------//
			scenarioRunTimeTick = GetTickCount();
			sce.RunScenario();
			while(sce.IsActive() == TRUE)
			{
				if(GetTickCount() - feedbackTick > 1000)
				{
					state = sce.GetState();
					staticLib.PrintScenarioExecutionFeedback(state.activity,
												   state.currentAnimat,
												   state.currentIteration,
												   (int)animatCount,
												   (int)duration);
					feedbackTick = GetTickCount();
				}
				Sleep(10);
			}
			printf("\n"); // This is create a line after all the "\r"s that were printed.

			// Calcuate run time and print.
			scenarioRunTimeTick = GetTickCount() - scenarioRunTimeTick;
			hhmmss = staticLib.Time_ToHrMinSec(scenarioRunTimeTick/1000);
			printf("Scenario completed, run time was %02d:%02d:%02d (%d seconds)\n",
				hhmmss.hour, hhmmss.min, hhmmss.sec, scenarioRunTimeTick/1000);

			// Set up the report file to be generated for this sce then open and close
			// the file to overwrite any previously existing file of the same name.  Write
			// runtime into the report file.
			GetLocalTime(&sysTime);
			_ASSERT(fdRunTimeReport == NULL);
			fopen_s(&fdRunTimeReport, "RunTimeReport.txt", "a");
			fprintf(fdRunTimeReport, "Run time: %02d:%02d:%02d (%08d seconds), %s %02d/%02d/%04d at %02d:%02d:%02d\n",
				hhmmss.hour, hhmmss.min, hhmmss.sec, scenarioRunTimeTick/1000, szScenarioNameBuff, sysTime.wMonth,
				sysTime.wDay, sysTime.wYear, sysTime.wHour, sysTime.wMinute, sysTime.wSecond);
			fclose(fdRunTimeReport);
			fdRunTimeReport = NULL;

#endif

			//--------------------------------------------------------------------------//
			// Extract animat track files
			//-----------------------------//
			trackFileGroupNum = 0;
			numTrackFileGroups = (int)ceil((double)animatCount/(double)MAXANIMATFILESOPEN);
			sprintf_s(szReportName, sizeof(szReportName), "%s.rprt", szScenarioNameBuff);
			while(trackFileGroupNum < numTrackFileGroups)
			{
				fopen_s(&fdIterationReport, szReportName, "a");

				// If the speedy option is set and the application has verified the first
				// group of animts then move onto the last group.
				if(input.speedy == TRUE && trackFileGroupNum == 1)
				{
					trackFileGroupNum++;

					while(trackFileGroupNum < numTrackFileGroups)
					{
						trackFileGroupNum++;
						trackFileCount += MAXANIMATFILESOPEN;
						//trackFileCount += MAXANIMATFILESOPEN;
						totalInterationsRead += MAXANIMATFILESOPEN*sce.GetSaveStatesCount();
					}

					// The internal -1 is so the animat count is set based on the number
					// of animats that would have been read in right before the last group
					// and the second -1 is accounts for zero-based indexing of the
					// animats.
					//trackFileCount = ((trackFileGroupNum-1)*MAXANIMATFILESOPEN);
				}
				else
				{
					trackFileGroupNum++; // starts at 1 in class FileExtractor
				}
				fileExtracter.SetSpecificAnimatGroup(trackFileGroupNum);
				sprintf_s(szBuff, sizeof(szBuff), "Extracting sce %s group %d\n", szScenarioNameBuff, trackFileGroupNum);
				printf(szBuff);

				//------------------------------------------------------------------------------//
				// Begin the extraction process.
				//------------------------------//
				sprintf_s(szBuff, sizeof(szBuff), "%s.3mb", szScenarioNameBuff);
 				if(OK != fileExtracter.ExtractBinaryResultsIntoTextFiles(szBuff, FALSE, FALSE, -1))
				{
					printf("\nProblem opening file %s\n for data extraction\n", szBuff);
					EndProcessOnError(pWorkingList);
				}

				feSetup = fileExtracter.GetSetup();
				//------------------------------------------------------------------------------//
				// Provide feedback about the extraction process.
				//-----------------------------------------------//
				feedbackTick = GetTickCount();
				do
				{
					feState = fileExtracter.GetState();
					if(GetTickCount() - feedbackTick > 1000)
					{
						staticLib.PrintExtractionFeedback(feState.activity,
												feState.currentAnimat,
												feState.currentIteration,
												feSetup.bin.outputByTime,
												feSetup.sceParams.totalNumAnimats,
												feSetup.sceParams.duration);
						feedbackTick = GetTickCount();
					}
					Sleep(1);
				}while(feState.activity != EXTRACTOR_IDLE_POSTRUN);
				printf("\n"); // This is create a line after all the "\r"s that were printed.


				//--------------------------------------------------------------------------//
				// Animat extraction for current animat is finished.  Verify track file
				// format.  Multiple track files possible if animat went off map.
				//-------------------------------------------------------------------//
				while(TRUE == staticLib.MyFindFile(&trk))
				{
					// Verify the animat track file is properly formatted
					if(0 >= (interationsRead = ValidateTrackFileFormat(trackFileCount, (int)duration, trk.data.cFileName, pWorkingList)))
					{
						printf("Format Error: %s\n", trk.data.cFileName);
						EndProcessOnError(pWorkingList);
					}

					// Advance the trak file count.
					totalInterationsRead += interationsRead;
					fprintf_s(fdIterationReport, "Animat Track %07d, ", trackFileCount);
					fprintf_s(fdIterationReport, "iterations read %07I64d, ", interationsRead);
					fprintf_s(fdIterationReport, "total iterations read %07I64d\n", totalInterationsRead);
					trackFileCount++;
					Sleep(100);
				}
				FindClose(trk.hdl);
				trk.hdl = NULL;

				//------------------------------------------------------------------------------//
				// Verify there is at least one track .trk files for the current animat
				//---------------------------------------------------------------------//
				if(trackFileCount == 0)
				{
					printf("\nFailed to create or locate animat tracks for data extraction\n");
					printf("Press any key to exit\n");
					while(0 == _kbhit())
						Sleep(500);
					ExitProcess(0);
				}

				//--------------------------------------------------------------------------//
				// Delete the track .trk files for the current animat
				//---------------------------------------------------//
				while(TRUE == staticLib.MyFindFile(&trk))
				{
					DeleteFile(trk.data.cFileName);
					Sleep(100);
				}

				// Close the FindFile handle for the track files.  A new set of track files is
				// potentially about to be generated.
				FindClose(trk.hdl);
				trk.hdl = NULL;

				// Close the report file for this round of trak files.  Will be reopened for
				// appending if more track files are generated.
				fclose(fdIterationReport);
				fdIterationReport = NULL;
			}


			//----------------------------------------------------------------------------------//
			// Verify that the proper number of animat states were observed
			//-------------------------------------------------------------//
			__int64 totalIterationsExpected = animatCount * (duration + 1);
			if(pWorkingList != NULL)
				totalIterationsExpected = animatCount * pWorkingList->Length();

			if(totalInterationsRead != totalIterationsExpected)
			{
				printf("\nFormat Error in sce %s\n", szScenarioNameBuff);
				printf("Incorrect quantity of animat states counted\n\t %I64d expected\n\t,", totalIterationsExpected);
				printf("%I64d read\n\t", totalInterationsRead);
				printf("based on %I64d animats *\n", animatCount);
				printf("                     (%I64d + 1)\n\n", (duration));
				EndProcessOnError(pWorkingList);
			}

			//----------------------------------------------------------------------------------//
			// Delete the .3mb File.
			//----------------------//
			while(TRUE == staticLib.MyFindFile(&mbs))
			{
				printf("deleting %s                                   \r", mbs.data.cFileName);
				DeleteFile(mbs.data.cFileName);
			}
			FindClose(mbs.hdl);
			mbs.hdl = NULL;

			//----------------------------------------------------------------------------------//
			// Delete the .sts take file if created.
			//-------------------------------------//
			while(TRUE == staticLib.MyFindFile(&sts))
			{
				printf("deleting %s                                 \r", sts.data.cFileName);
				DeleteFile(sts.data.cFileName);
			}
			FindClose(sts.hdl);
			sts.hdl = NULL;

			//----------------------------------------------------------------------------------//
			// Delete the memory files that show buffer placement.
			//---------------------------------------------------//
			printf("deleting %s files                                \r", mem.sz);
			while(TRUE == staticLib.MyFindFile(&mem))
			{
				//printf("deleting %s\n", trk.data.cFileName);
				DeleteFile(mem.data.cFileName);
				Sleep(10);
			}
			FindClose(mem.hdl);
			mem.hdl = NULL;

			
			//----------------------------------------------------------------------------------//
			// Delete the report (.rprt) files that iterations read in counts.
			//---------------------------------------------------------------//
			printf("deleting %s files                                \r", rpt.sz);
			while(TRUE == staticLib.MyFindFile(&rpt))
			{
				//printf("deleting %s\n", trk.data.cFileName);
				DeleteFile(rpt.data.cFileName);
				Sleep(10);
			}
			FindClose(rpt.hdl);
			rpt.hdl = NULL;


			// Deallocate memory for the working list if used in the just-completed scenario.
			if(pWorkingList != NULL)
			{
				pWorkingList->DeleteAll();
				delete pWorkingList;
				pWorkingList = NULL;
			}
			printf("\n");
		}



		/////////////////////////////////////////////////////////////////////////
		CheckForUserPause();
	}
	// Close up the sce files.
	FindClose(findSce.hdl);
	findSce.hdl = NULL;

	GetLocalTime(&sysTime);
	printf("\n\nEnd Normally Time: %02d:%02d:%02d\n", sysTime.wHour, sysTime.wMinute, sysTime.wSecond);

	return 0;
}

// Returns the number of valid command line entries.
int ParseCommandLineInput_autoTest(CONSOLEINPUT *Input, int argc, TCHAR *argv[])
{
	int i;
	int validEntries = 0;
	C3mbStaticsLib staticLib;

	memset(Input, 0, sizeof(CONSOLEINPUT));
	if(argc == 1)
		return 0;

	for(i=1; i<argc && Input->help == FALSE; i++)
	{
		if(TRUE == staticLib.HasAnExtensionType(".sce", argv[i]) && Input->sce.set == FALSE)
		{
			strcpy_s(Input->sce.szFileName, sizeof(Input->sce.szFileName), argv[i]);
			staticLib.GetPathAndFileTitleFromFileName(Input->sce.szFileName, NULL, 0, Input->sce.szFileTitle, sizeof(Input->sce.szFileTitle));
			Input->sce.set = TRUE;
			validEntries++;

			// Overrides -b (batch) option.
			if(Input->batch == TRUE)
			{
				Input->batch = FALSE;
				validEntries--;
			}

			// Overrides -a (abort batch on error) option
			if(Input->abortBatchOnError == TRUE)
			{
				Input->abortBatchOnError = FALSE;
				validEntries--;
			}
		}
		else if(TRUE == staticLib.HasAnExtensionType(".csv", argv[i]) && Input->csv.set == FALSE)
		{
			strcpy_s(Input->csv.szFileName, sizeof(Input->csv.szFileName), argv[i]);
			staticLib.GetPathAndFileTitleFromFileName(Input->csv.szFileName, NULL, 0, Input->csv.szFileTitle, sizeof(Input->csv.szFileTitle));
			Input->csv.set = TRUE;
			validEntries++;
		}
		else if(0 == strcmp("-b", argv[i]) && Input->sce.set == FALSE && Input->batch == FALSE)
		{
			Input->batch = TRUE;
			validEntries++;
		}
		else if(0 == strcmp("-p", argv[i]) && Input->pingLimted == FALSE)
		{
			Input->pingLimted = TRUE;
			validEntries++;
		}
		else if(0 == strcmp("-?", argv[i]) && Input->help == FALSE)
		{
			memset(Input, 0, sizeof(CONSOLEINPUT));
			Input->help = TRUE; // will cause for-loop to exit.
			validEntries = 1;
		}
		else if(0 == strcmp("-f", argv[i]) && Input->speedy == FALSE)
		{
			Input->speedy = TRUE;
			//validEntries++;
		}
	}

	if(Input->batch == FALSE)
		Input->abortBatchOnError = FALSE; // no need for this.

	return validEntries;
}



// returns the number of iterations read.
__int64 ValidateTrackFileFormat(int AnimatIndex, int Duration, TCHAR *szFileName, CWorkingList *pWorkingList)
{
	FILE *fp;
	int i;
	TCHAR szBuff[SIZE_2048] = {0};
	TCHAR szPrintLn[SIZE_16] = {0};
	errno_t err;
	static DWORD pauseTick = 0;

	int totalIterationsRead;
	int totalIterationsExpected;
	int animatNum;
	int animatRead;
	int lastIteration;
	int iteration;
	int expectedIteration;

	CheckForUserPause();


	//----------------------------------------------------------------------------------//
	// Attempt to open the track file (text-based) to verify.
	//-------------------------------------------------------//
	if(0 != (err = fopen_s(&fp, szFileName, "r")))
	{
		printf("\nUnable to open file:\n\t%s\n", szFileName);
		EndProcessOnError(pWorkingList);
	}

	// Determine the number of iterations expected to be read in from the text file.
	totalIterationsExpected = Duration + 1;
	if(pWorkingList != NULL)
		totalIterationsExpected = pWorkingList->Length();


	//----------------------------------------------------------------------------------//
	// Skip header lines (first six lines of the track files).
	//--------------------------------------------------------//
	for(i=0; i<6; i++)
	{
		if(NULL != fgets(szBuff, sizeof(szBuff), fp))
			continue;

		printf("\nError reading track file header lines, file:\n\t%s\n", szFileName);
		EndProcessOnError(pWorkingList);
	}


	//----------------------------------------------------------------------------------//
	// Read first data line in the track file.
	// There should always be at least one iteration unless the output was limited by
	// either ping cycle, a csv file, or both for which there may be no output.
	//
	// For the first line of iterations it is OK for fgets() to return NULL if there is a
	// working list and the working list length is zero.
	//---------------------------------------------------//
	totalIterationsRead = 0;
	if(NULL == fgets(szBuff, sizeof(szBuff), fp) && totalIterationsExpected != 0)
	{
		printf("\nError reading data line index %d in file:\n%s\n", totalIterationsRead, szFileName);
		EndProcessOnError(pWorkingList);
	}
	else
	{
		// The fist iteration was successfully read in.  Copy it into the print-line
		// buffer (no specific reason for doing this...?) then parse the buffer. 
		strncpy_s(szPrintLn, sizeof(szPrintLn), szBuff, sizeof(szPrintLn)-1);
		if(2 != sscanf_s(szPrintLn, "%6d %7d\n", &animatRead, &iteration))
		{
			printf("\nError parsing data line index %d in file:\n%s\n", totalIterationsRead, szFileName);
			EndProcessOnError(pWorkingList);
		}

		// Buffer sucessfully parsed.  Set total iterations read in to 1 and set animat
		// number to the animat number read in and last iteration.  The iteration value
		// read in can be anything and is dependent upon the starting time of the
		// scenario and if output is limited by a .csv file or by acoustic ping cycles.
		totalIterationsRead = 1;
		animatNum = animatRead;
		lastIteration = iteration;

		// Verify proper animat index read in.
		if(animatNum != AnimatIndex)
		{
			printf("\nError: Animat index %d doesn't match animat number %d read in, file:\n%s\n", AnimatIndex,
				animatNum, szFileName);
			EndProcessOnError(pWorkingList);
		}

		// Verify proper iteration read in.
		// If output is limited by either .csv file or acoustic ping make sure that the first
		// iteration read in matches the first one expected.  pWorkingList is zero indexed.
		if(pWorkingList != NULL)
		{
			expectedIteration = *pWorkingList->Get(0);
			if(expectedIteration != iteration)
			{
				printf("\nError: Iteration read in %d does not match expected iteration %d, file:\n%s\n",
					iteration, expectedIteration, szFileName);
				EndProcessOnError(pWorkingList);
			}
		}
	}


	// Read iterations in until the end of the scenario.
	while(NULL != fgets(szBuff, sizeof(szBuff), fp))
	{
		strncpy_s(szPrintLn, sizeof(szPrintLn), szBuff, sizeof(szPrintLn)-1);
		if(2 != sscanf_s(szPrintLn, "%6d %7d\n", &animatRead, &iteration))
		{
			printf("\nError parsing data line index %d in file:\n%s\n", totalIterationsRead, szFileName);
			EndProcessOnError(pWorkingList);
		}

		// Determine the expected iteration.
		expectedIteration = lastIteration+1;
		if(pWorkingList != NULL)
			expectedIteration = *pWorkingList->Get(totalIterationsRead); // zero-based indexing

		if(expectedIteration != iteration)
		{
			printf("\nError: Iteration read in %d does not match expected iteration %d, file:\n%s\n",
				iteration, expectedIteration, szFileName);
			EndProcessOnError(pWorkingList);
		}

		// Verify the correct animat read in.
		if(animatRead != animatNum || iteration != expectedIteration)
		{
			printf("\nData format error in data at line index %d in file:\n%s\n", totalIterationsRead, szFileName);
			printf("Expected animat %d at iteration %d, got animat %d at iteration %d\n", animatNum, expectedIteration,
				animatRead, iteration);
			EndProcessOnError(pWorkingList);
		}

		lastIteration = iteration;
		totalIterationsRead++;

		// Pause once a second.
		if(GetTickCount() - pauseTick > 100)
		{
			pauseTick = GetTickCount();
			printf("Animat %07d Iteration %07d\r", animatRead, iteration);
			Sleep(10);
		}
	}

	//if(totalIterationsRead != Duration + 1)
	if(totalIterationsRead != totalIterationsExpected)
	{
		printf("\nIncorrect number of iterations read in file:\n%s\n", szFileName);
		printf("Expected %d iterations, read in %d iterations\n\n\n", totalIterationsExpected, totalIterationsRead);
		EndProcessOnError(pWorkingList);
	}
	
	fclose(fp);
	return totalIterationsRead;
}

void EndProcessOnError(CWorkingList *pWorkingList)
{
	SYSTEMTIME sysTime;
	GetLocalTime(&sysTime);
	printf("\n\nEnd On Error Time: %02d:%02d:%02d\n", sysTime.wHour, sysTime.wMinute, sysTime.wSecond);


	if(pWorkingList != NULL)
	{
		pWorkingList->DeleteAll();
		delete pWorkingList;
		pWorkingList = NULL;
	}

	printf("Press any key to exit\n");
	while(0 == _kbhit())
		Sleep(500);
	_getch();
	fflush(NULL);
	ExitProcess(0);
}


void CheckForUserPause()
{
	//int i;
	return;
#if 0
	printf("\n\nPause Begin\n");

	for(i=0; i<100; i++)
	{
		Sleep(10);
		if(0 != _kbhit() && 'p' != _getch())
		{
			printf("\n\nPress p key to continue processing\n");
			while(0 == _kbhit() || 'p' != _getch())
				Sleep(2000);
		}
	}
	printf("\n\nPause End\n");
#endif
}

void DelayNextAttempt()
{
	DWORD tickCnt = GetTickCount();
	DWORD delay = 3*60*1000; // 3 minute delay.
	//DWORD error;
	do
	{
		printf("Press any key to continue (continuing in %d seconds)\r", (delay - (GetTickCount() - tickCnt))/1000);
		Sleep(500);
	}while(0 == _kbhit() && (GetTickCount() - tickCnt) < delay);
	_getch();
	printf("\n");
}


void DisplayError(DWORD Error)
{
    TCHAR szBuffer[SIZE_256];
    //LPVOID lpDisplayBuf;
	LPTSTR lpszFunction = "dog";

    FormatMessage( 
        FORMAT_MESSAGE_FROM_SYSTEM, // dw Flags
        NULL,						// lpSource
        Error,						// error message (dwMessageID)
        MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
        (LPTSTR) szBuffer,
        sizeof(szBuffer),
		NULL );


#if 0
	StringCchPrintf((LPTSTR)lpDisplayBuf, 
        LocalSize(lpDisplayBuf),
        TEXT("%s failed with error %d: %s"), 
        lpszFunction, dw, lpMsgBuf); 
#endif
    MessageBox(NULL, (LPCTSTR)szBuffer, TEXT("Error"), MB_OK); 

    //LocalFree(lpMsgBuf);
    //LocalFree(lpDisplayBuf);
    //ExitProcess(dw); 

}

enum LIMITOUTPUTTRIAL
{
	AUTOTEST_NORMAL, // No limiting of output
	AUTOTEST_CSV_LIMITED, // limited by comma separated values.
	AUTOTEST_ACSTCSRCPING_LIMITED, //limited to acoustic source pings
	AUTOTEST_CSV_ACSTCSRCPING_LIMITED, // limited by both CSV and acoustic source pings.
};

TCHAR *SZLIMITOUTPUTTRIAL[4] = {"1nrml", "2csv", "3ping", "4csvPing"};