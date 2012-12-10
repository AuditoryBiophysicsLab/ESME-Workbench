
// Windows Header Files:
#include <windows.h>

// C RunTime Header Files
#include <conio.h>
#include <stdlib.h>
#include <malloc.h>
#include <memory.h>
#include <tchar.h>
#include <stdio.h>
#include <time.h>
#include <sys/types.h>
#include <sys/timeb.h>
#include <commctrl.h>

#include "3mbsLib.h"
#include "FileExtracter.h"
#include "staticLib.h"

#define MMMBS_CMDPRMPT_VERSION_SUPER 2
#define MMMBS_CMDPRMPT_VERSION_SUB 1

#ifndef TRUE
#define FALSE 0
#define TRUE !FALSE
#endif

#ifdef _DEBUG
#define DEBUG_VERSION_BOOL TRUE
#else 
#define DEBUG_VERSION_BOOL FALSE
#endif

CStaticScenario m_staticScenario;

void Run3mb(int argc, TCHAR *argv[], int *RunNum = NULL);
int ParseCommandLineInput_cmdPrmt(CONSOLEINPUT *Input, int argc, TCHAR *argv[]);
void HandleFailure(CONSOLEINPUT *Input = NULL, TCHAR *szInitialDirectory = NULL);
void PrintMainBanner();
void PrintBanner(TCHAR *szSceFileNameParam, BOOL PingCycleLimit, TCHAR *szCSVFileNameParam);

void InputParamTest();

/* Input: argv[0] - name of application
          argv[1] - name of scenario file or -a for all scenarios in the current directory
		  argv[2-3] - [no argument] or 
		  [[name of comma separated values file with .csv extension or "ping"]] or both
*/

/*
TCHAR *SZHELPSTRING = "\
\n\
Usage:\n\n\
 3MBc -?                Help \n\
 3MBc [sce] [csv] [dir] [opt] Run a specific .sce scenario file.\n\
 3MBc [sce] [opt]       Run a specific .sce scenario file.\n\
 3MBc [sce] [opt] [csv] Run a specific .sce scenario file.  Binary output\n\
                        iterations limited by .csv file.\n\
 3MBc  -a   [opt]       Run all scenario .sce files in the current directory.\n\
 3MBc  -a   [opt] [csv] Run all scenario .sce files in the current directory.\n\
                        Binary output iterations limited by .csv file\n\n\
  [opt] is a combination of all or none of the following:\n\
       -p   Output limited to the acoustic source ping cycles.  Ignored if the\n\
            scenario not populated with a sound source.  Output combined with\n\
            .csv file if .csv file loaded\n\
       -e   Extract the binary output (.3mb) file into track files.  Preserve\n\
            the binary output file\n\
       -E   Extract the binary output (.3mb) file into track files.  Delete\n\
            the binary output file afterwards.  Overriden by -e option.\n\n";

*/

TCHAR *SZHELPSTRING = "\
\n\
Usage:\n\n\
 3MBc [sce] [csv] [dir] [flags: -b -a -p -e]\n\n\
     Input Options:\n\
      [sce]   Specifies a 3MB scenario file (.sce) to run.  Overrides the -b\n\
              flag.\n\
      [csv]   Specifies a comma-separated values (.csv) file that lists a sub\n\
              set of iterations to be written to the 3MB binary output file\n\
              (.3mb).  If both this option and the -p flag are set and an\n\
              acoustic source seeded in the scenario then the outputted\n\
              iterations are a composite of both the [csv] file and the\n\
              acoustic source ping iterations.\n\
      [dir]   Specifies a path to a folder into which 3MB outputs data.  This\n\
              option overriden by the -n flag.\n\
      -b      Batch processing.  Run all scenarios in the current directory.\n\
              This option overriden if the [sce] file option is set.\n\
      -a      Abort batch processing on error.  This option has no meaning\n\
              if the -b option is not set.\n\
      -p      Output limited to the acoustic source ping cycles.  Ignored if\n\
              the scenario not populated with a sound source.  See also the\n\
              [csv] option.\n\
      -n      No subfolders.  All data stored in running directory.\n\
              Overrides [dir] option.\n\
      -e      Extract the binary output (.3mb) file into track files.\n\
              Preserves the binary output file.\n\n";            
				        


/*
    i =                   j =       
    0 0 0 0 0 0 0 0 = 0   0 0 0 0 0 0 0 0 = 0
	                      0 0 0 0 0 0 0 1 = 1
						  0 0 0 0 0 0 1 0 = 2
						  0 0 0 0 0 0 1 1 = 3



	0 0 0 0 0 0 0 1 = 1   0 0 0 0 0 0 0 0 = 0
	                      0 0 0 0 0 0 0 1 = 1
						  0 0 0 0 0 0 1 0 = 2
						  0 0 0 0 0 0 1 1 = 3
	0 0 0 0 0 0 1 0 = 2   0 0 0 0 0 0 0 0 = 0
	                      0 0 0 0 0 0 0 1 = 1
						  0 0 0 0 0 0 1 0 = 2
						  0 0 0 0 0 0 1 1 = 3
	0 0 0 0 0 0 1 1 = 3
	0 0 0 0 0 1 0 0 = 4
*/



void main(int argc, TCHAR *argv[])
{
	//int i;
	//InputParamTest();

//	for(i=0; i<argc; i++)
//		printf("%s ", argv[i]);
//	printf("\n");

	Run3mb(argc, argv);
}


void InputParamTest()
{
	TCHAR *sz[] = {"3MBc.exe", "-a", "-b", "dog", "-e", "30Sec_52hr.csv", "-p", "-n"};
	TCHAR **szCopy;
	int szSize = sizeof(sz)/sizeof(TCHAR *);
	int numPermutations = (int)pow((double)2, (int)szSize);
	int i, j;
	int size;
	int mask = 0;
	int val = 0;
	int runNum = 0;


	szCopy = new TCHAR *[szSize];
	for(i=0; i<szSize; i++)
	{
		szCopy[i] = new TCHAR[SIZE_32];
		size = sizeof(szCopy[i]);
		memset(szCopy[i], 0, sizeof(TCHAR)*SIZE_32);
	}

	for(i=1; i<numPermutations; i++)
	{
		if(i%2 == 0)
			continue;

		for(j=0; j<szSize; j++)
		{
			if(((1<<j) & i) == (1<<j))
				strcpy_s(szCopy[j], sizeof(TCHAR)*SIZE_32, sz[j]);
			else
				strcpy_s(szCopy[j], sizeof(TCHAR)*SIZE_32, "-k");

		}

		printf("%04d ", runNum);
		for(j=0; j< szSize; j++)
			printf("%s ", szCopy[j]);
		printf("\n");
		Run3mb(szSize, szCopy, &runNum);
		printf("\n");
		runNum++;

		for(j=0; j<szSize; j++)
			memset(szCopy[j], 0, sizeof(TCHAR)*SIZE_32);

	}

	for(i=0; i<szSize; i++)
		delete [] szCopy[i];

	delete [] szCopy;
}

void Run3mb(int argc, TCHAR *argv[], int *RunNum)
{
	int i = 0;
	CONSOLEINPUT input;
	TCHAR szCurrentDirectory[SIZE_512];
	TCHAR szSceTitle[SIZE_128];
	C3mbStaticsLib staticLib;

	TCHAR *szArg = NULL;

	TCHAR szBuff[SIZE_1024] = {0};

	DWORD elapsedTimeTick;
	HHMMSS hms;
	RESLT res;

	CScenario sce;
	SCESTATE state;
	CFileExtracter fileExtracter;
	FESTATE feState;
	FESETUP feSetup;
	FINDINF findSce;

	int animatCount;
	int duration;
	BOOL byTime;


	// Intialize vars.
	memset(&findSce, 0, sizeof(FINDINF));
	memset(&input, 0, sizeof(CONSOLEINPUT));

#ifdef _DEBUG
	printf("DEBUG VERSION\n");
#endif
	//SCENARIOPARAMS sceParams;

	//SPECIESSUMMARY		  clusterSummary;
	// Display the list of entered params as long as it wasn't a "-?"

	// If no valid command line parameters entered print an error message.  If a -? was
	// entered at any point print the help string.
	if(0 == ParseCommandLineInput_cmdPrmt(&input, argc, argv))
	{
		printf("\n  Error. No valid parameter entered.\n\tType \"3MBc -?\"\n\tfor help.\n");
		return;
	}
	else if(input.help == TRUE)
	{
		printf(SZHELPSTRING);
		return;
	}
	else if(input.batch == FALSE && input.sce.set == FALSE)
	{
		printf("\n  Error. No scenario parameters (.sce file or -a option) entered.\n\tType \"3MBc -?\"\n\tfor help.\n");
		return;		
	}

	// Print the main banner for this application
	PrintMainBanner();


	// Record the current directory.  At the end of the run or when loading in subseqent
	// scenarios the application returns to the directory it was launched from. 
	GetCurrentDirectory(sizeof(szCurrentDirectory), szCurrentDirectory);

	//----------------------------------------------------------------------------------//
	// One scenario or multiple scenarios?
	//------------------------------------//
	// Intially set the FINDINF struct to handle all scenario (.sce) files in the current
	// directory for batch processing.  If input params set for a single scenario change
	// the FINDINF struct accordinly.
	strcpy_s(findSce.sz, sizeof(findSce.sz), "*.sce");
	if(input.sce.set == TRUE) // single scenario
	{
		strcpy_s(findSce.sz, sizeof(findSce.sz), input.sce.szFileName);
		if(FALSE == staticLib.MyFindFile(&findSce))
		{
			printf("Did not locate scenario:\n\t\"%s\"\n", findSce.sz);
			HandleFailure(&input, szCurrentDirectory); 
			return; // Failure here stops this routine.
		}
		FindClose(findSce.hdl);
		findSce.hdl = NULL;
	}

	// Run the scenario or scenarios.
	while(TRUE == staticLib.MyFindFile(&findSce))
	{
		PrintBanner(findSce.data.cFileName, input.pingLimted, input.csv.szFileTitle);
		
		// Load the sce file into the sce instance
		if(OK != sce.LoadFromBinFile(findSce.data.cFileName))
		{
			printf("Failed to load sce %s\n", findSce.data.cFileName);
			HandleFailure(&input, szCurrentDirectory);
			continue; // try the next sceanario.
		}

		// Set the scenario up to handle the CSV file, if the user set the input parameters
		// up to do so.
		if(input.csv.set == TRUE && OK != sce.ReadCSVListFromFile(input.csv.szFileName))
		{
			printf("Failed to load CSV iteration save list from file:\n  %s\n", input.csv.szFileTitle);
			HandleFailure(&input, szCurrentDirectory);
			continue; // try the next sceanario.
		}

		// Set the scenario to limit output to acoutic pings if both the user entered a
		// parameter to do so and there is an acoustic source present in the scenario
		if(sce.GetTotalSoundSourceCount() > 0 && input.pingLimted == TRUE)
			sce.SetAcousticSrceLimitOutput(TRUE);

		// If the user specificed an output directory create that directory then change into
		// it.
		if(input.outputDir.set == TRUE && input.noSubDirectories == FALSE)
		{
			// Creating the directory isn't important... it may already have been created
			// so error checking not needed.
			if(input.noSubDirectories == FALSE)
				CreateDirectory(input.outputDir.szDirectory, NULL);

			// Failure to set the directory causes the current scenario to be skipped.
			if(FALSE == SetCurrentDirectory(input.outputDir.szDirectory))
			{
				printf("Failed to set directory to:\n\t %s\n", input.outputDir.szDirectory);
				HandleFailure(&input, szCurrentDirectory);
				continue;
			}

		}

		//Create a folder to output data into.  Don't check if the folder was successfully
		// created since it may have already been created. 
		staticLib.GetPathAndFileTitleFromFileName(findSce.data.cFileName, NULL, 0,szSceTitle, sizeof(szSceTitle));
		staticLib.RemoveExtension(szSceTitle);

		// Append the run number at the end.
		if(RunNum != NULL)
		{
			sprintf_s(szBuff, sizeof(szBuff), "%04d_%s", *RunNum, szSceTitle);
			strcpy_s(szSceTitle, sizeof(szSceTitle), szBuff);
		}

		if(input.noSubDirectories == FALSE)
		{
			CreateDirectory(szSceTitle, NULL);
			sce.SetOutputDirectory(szSceTitle);
		}

		// Set the scenario title and the output folder
		sce.SetScenarioTitle(szSceTitle);

		// Get information abou the scenario to be used for feedback.
		animatCount = sce.GetAnimatCount();
		duration = sce.GetDurationSeconds();
		byTime = sce.GetConfiguration().output.outputByTime;

		//------------------------------------------------------------------------------//
		// Launch the scenario.
		//---------------------//
		// Get the current tick count then launch the scenario.
		elapsedTimeTick = GetTickCount();
		if(OK != (res = sce.RunScenario()))
		{
			// Handle scenario launch failure.
			printf("Scenario failed to launch\n");
			HandleFailure(&input, szCurrentDirectory);
			continue; // try the next scenario.

		}

		while(sce.IsActive() == TRUE)
		{
			// Provide feedback about the running scenario.
			state = sce.GetState();
			staticLib.PrintScenarioExecutionFeedback(state.activity,
										   state.currentAnimat,
										   state.currentIteration,
										   animatCount,
										   duration);
			Sleep(100);
		}

		// Handle scenario failure
		if(res != OK)
		{
			printf("Scenario %s did not run sucessfully...\n", input.sce.szFileTitle);
			HandleFailure(&input, szCurrentDirectory);
			continue; // try the next scenario.
		}

		//------------------------------------------------------------------------------//
		// Scenario finished.  Provide feeback about total time it took to run scenario.
		//------------------------------------------------------------------------------//
		elapsedTimeTick = GetTickCount() - elapsedTimeTick;
		hms = staticLib.Time_ToHrMinSec(elapsedTimeTick/1000);
		printf("\nScenario %s run time:\n\t %02d:%02d:%02d (%d secs)\n",
			szSceTitle, hms.hour, hms.min, hms.sec, elapsedTimeTick/1000);


		//------------------------------------------------------------------------------//
		// Run the extraction process.
		//----------------------------//
		if(input.autoExtract == TRUE)
		{
			// Create the directory the track files are to be extracted into then set
			// the procss into it.
			if(input.noSubDirectories == FALSE)
			{
				CreateDirectory(szSceTitle, NULL);
				SetCurrentDirectory(szSceTitle);
			}

			// Construct the string for the binary file (.3mb) that is to be extracted.
			sprintf_s(szBuff, sizeof(szBuff), "%s.3mb", szSceTitle);

			if(OK != fileExtracter.ExtractBinaryResultsIntoTextFiles(szBuff, FALSE, FALSE, -1))
			{
				printf("\nProblem opening file %s\n for data extraction\n", szBuff);
				HandleFailure(&input, szCurrentDirectory);
				continue;
			}
			feSetup = fileExtracter.GetSetup();
			
			do
			{
				// Provide feedback about the extraction process.
				feState = fileExtracter.GetState();
				staticLib.PrintExtractionFeedback(feState.activity,
										feState.currentAnimat,
										feState.currentIteration,
										feSetup.bin.outputByTime,
										feSetup.sceParams.totalNumAnimats,
										feSetup.sceParams.duration);
				Sleep(100);
			}while(feState.activity != EXTRACTOR_IDLE_POSTRUN);
			printf("\n"); // This is create a line after all the "\r"s that were printed.
			
			// Return to the directory above the extraction directory.
			if(input.noSubDirectories == FALSE)
				SetCurrentDirectory("..");
		}

		// Return to the original directory above the output directory if an output
		// directory was set.
		if(input.outputDir.set == TRUE && input.noSubDirectories == FALSE)
			SetCurrentDirectory("..");

	}
}



// Returns the number of valid command line entries.
int ParseCommandLineInput_cmdPrmt(CONSOLEINPUT *Input, int argc, TCHAR *argv[])
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
		else if(FALSE == staticLib.HasAnyExtension(argv[i]) && argv[i][0] != '-' && Input->outputDir.set == FALSE && Input->noSubDirectories == FALSE)
		{
			strcpy_s(Input->outputDir.szDirectory, sizeof(Input->outputDir.szDirectory), argv[i]);
			Input->outputDir.set = TRUE;
			validEntries++;
		}
		else if(0 == strcmp("-b", argv[i]) && Input->sce.set == FALSE && Input->batch == FALSE)
		{
			Input->batch = TRUE;
			validEntries++;
		}
		else if(0 == strcmp("-a", argv[i]) && Input->sce.set == FALSE && Input->abortBatchOnError == FALSE)
		{
			Input->abortBatchOnError = TRUE;
			validEntries++;
		}
		else if(0 == strcmp("-p", argv[i]) && Input->pingLimted == FALSE)
		{
			Input->pingLimted = TRUE;
			validEntries++;
		}
		else if(0 == strcmp("-n", argv[i]) && Input->noSubDirectories == FALSE)
		{
			Input->noSubDirectories = TRUE;
			validEntries++;

			if(Input->outputDir.set == TRUE)
			{
				memset(Input->outputDir.szDirectory, 0, sizeof(Input->outputDir.szDirectory));
				Input->outputDir.set = FALSE;
				validEntries--;
			}
		}
		else if(0 == strcmp("-e", argv[i]) && Input->autoExtract == FALSE)
		{
			Input->autoExtract = TRUE;
			validEntries++;
		}
		else if(0 == strcmp("-?", argv[i]))
		{
			memset(Input, 0, sizeof(CONSOLEINPUT));
			Input->help = TRUE;
			validEntries = 1;
		}
	}

	if(Input->batch == FALSE)
		Input->abortBatchOnError = FALSE; // no need for this.

	return validEntries;
}

/* Exits the process only if in batch processing mode or inable to set the current
   directory back to the initial directory.*/
void HandleFailure(CONSOLEINPUT *Input, TCHAR *szInitialDirectory)
{
	DWORD countUpTick;
	DWORD respondTime = 3*60*1000;

#ifdef _DEBUG
	respondTime = 1000; // 1 second 
#endif

	_ASSERT(Input != NULL);
	_ASSERT(szInitialDirectory != NULL);
	// Considers: 1) If user specified to abort on batch error when batch processing
	//            2) If this application no set to run in batch mode (running a single
	//               scenario so condition (Input->batch == FALSE) is true
	//            3) ForceAbort is set to true.
	if(Input != NULL && Input->abortBatchOnError == TRUE && Input->batch == TRUE)
	{
		printf("Press any key to exit\n");
		while(0 == _kbhit())
			Sleep(500);
		ExitProcess(0);
	}

	// Return to the initial directory when an error is detected.  Inability to do so is
	// a critical error.
	if(strlen(szInitialDirectory) > 0 && FALSE == SetCurrentDirectory(szInitialDirectory) && Input->noSubDirectories == FALSE)
	{
		printf("Unable to return to initial directory:\n\t %s\n", szInitialDirectory);
		printf("Press any key to exit\n");
		while(0 == _kbhit())
			Sleep(500);
		ExitProcess(0);
	}

	countUpTick = GetTickCount(); // 3 minuites

	// 3 minutes to respond to error condition
	while(countUpTick - GetTickCount() > respondTime && 0 == _kbhit())
	{
		Beep(750, 100);
		Sleep(500);
	}
}



void PrintMainBanner()
{
	TCHAR szDate[SIZE_32];
	TCHAR szTime[SIZE_32];
	CStaticScenario sceStatic;

	sceStatic.GetBuildDateTimeString(szDate, sizeof(szDate), szTime, sizeof(szTime));
	printf("---------------------------------------------------------------------------\n");
	if(DEBUG_VERSION_BOOL == TRUE)
		printf("* DEBUG VERSION  DEBUG VERSION DEBUG VERSION DEBUG VERSION DEBUG VERSION DE\n");
//	printf("\n");
	printf(" 3MBc (cmd prompt)   ver %d.%02d ", MMMBS_CMDPRMPT_VERSION_SUPER, MMMBS_CMDPRMPT_VERSION_SUB);
	printf("build %s, %s\n", __DATE__, __TIME__);
	printf("   built on 3MB lib  ver %d.%02d ", MMBSLIB_VERSION_SUPER, MMBSLIB_VERSION_SUB);
	printf("build %s, %s\n", szDate, szTime);
	if(DEBUG_VERSION_BOOL == TRUE)
		printf("* DEBUG VERSION  DEBUG VERSION DEBUG VERSION DEBUG VERSION DEBUG VERSION DE\n");
	printf("---------------------------------------------------------------------------\n");
	printf("\n");
}

void PrintBanner(TCHAR *szSceFileNameParam, BOOL PingCycleLimit, TCHAR *szCSVFileNameParam)
{
	static int callCnt = 0;

	// Separate the previous run.
	if(callCnt > 0)
	{
		//printf("***************************************************************************\n");
		printf("\n");
	}

	printf("***************************************************************************\n");
	printf("(%d) Running Scenario: %s\n", callCnt, szSceFileNameParam);
	if(PingCycleLimit == TRUE || (szCSVFileNameParam != NULL && strlen(szCSVFileNameParam) > 4)) // 4 for the .csv
	{
		printf(" Output limited by:");
		if(szCSVFileNameParam != NULL && strlen(szCSVFileNameParam) > 4)
			printf("%s ", szCSVFileNameParam);
		if(PingCycleLimit == TRUE && (szCSVFileNameParam != NULL && strlen(szCSVFileNameParam)))
			printf("and ");
		if(PingCycleLimit == TRUE)
			printf(" Acoustic Source Ping Cycle");
		printf("\n");
	}

	callCnt++;
}