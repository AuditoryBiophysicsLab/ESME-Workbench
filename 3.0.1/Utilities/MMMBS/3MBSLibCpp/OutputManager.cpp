#include "OutputManager.h"
#include <stdio.h>
#include <math.h>

COutputManager::COutputManager(void)
{
	m_completeCSVList.Initialize("COutputManager", "completeList");
	m_acousticSrcList.Initialize("COutputManager", "acousticSrcList");
}

COutputManager::~COutputManager(void)
{
	m_completeCSVList.DeleteAll();
	m_acousticSrcList.DeleteAll();
}

RESLT COutputManager::ResetAcousticSourceInf(ACOUSTICSRCEINF *AcstcSrcInfBuff, int AcstcSrcInfBuffLen)
{
	int i;
	ACOUSTICSRCEINF *acf;
	m_acousticSrcList.DeleteAll();

	_ASSERT((AcstcSrcInfBuffLen > 0 && AcstcSrcInfBuff != NULL) || (AcstcSrcInfBuffLen == 0));

	for(i=0; i<AcstcSrcInfBuffLen; i++)
	{
		if(NULL == (acf = m_acousticSrcList.Add()))
			return MEMALLOC_ERROR;
		*acf = AcstcSrcInfBuff[i];
	}
	return OK;
}



int COutputManager::GetAcousticSourceCount(){return m_acousticSrcList.Length();}

// Reads in from file a list of states to be saved.  Returns OK if successful
RESLT COutputManager::ReadCSVListFromFile(TCHAR *szFileName)
{
	FILE *fd;
	int read;
	int numConverted;
	errno_t error;
	int i;
	LinkedList <int> fReadList;

	if(0 != (error = fopen_s(&fd, szFileName, "rt")))
		return OPENFILEREAD_ERROR;

	numConverted = fscanf_s(fd, "%d\n", &read);
	while(numConverted != EOF && numConverted !=0)
	{
		// Check for duplicates.
		for(i=0; i<fReadList.Length(); i++)
		{
			if(read == *fReadList.Get(i))
				break; // A duplicate was found.
		}
		// If no duplicates found i will equal the length of the list.  Add a new
		// node to the list.
		if(i==fReadList.Length())
			*fReadList.Add() = read;

		// Read in the next value.
		numConverted = fscanf_s(fd, "%d\n", &read);
	}
	fclose(fd);

	if(numConverted == 0)
		return FILEFORMAT_ERROR;

	// Copy the temporary read list into the class member list.
	m_completeCSVList.DeleteAll();
	for(i=0; i<fReadList.Length(); i++)
		*m_completeCSVList.Add() = *fReadList.Get(i);
	fReadList.DeleteAll();
	return OK;
}

// Clears out the current list of states to save, returns the current
void COutputManager::ClearCSVList()
{
	m_completeCSVList.DeleteAll();
}

BOOL COutputManager::CSVFileLoaded()
{
	if(m_completeCSVList.Length() != 0)
		return TRUE;
	return FALSE;
}


// Creates a work list based upon the complete list read in from file and the values in 
// passed in parameters StartTime and Duration.  Returns the dynamically allocated and 
// created list.  User can either delete all list members and deallocate list or pass
// the list to DeallocateWorkingList() member function.
typedef struct ClockBufferArray
{
	int index;
	int *ptr;
	int length;
	int *clkBuffer;
}CLKBUFFERARRAY;

// Returns the number of valid states to be saved based upon duration, start time, and
// list generated from a read in file.
int COutputManager::GetFileReadListSaveStateCount(int StartTime, int Duration, const OUTPTINTRVLLMT *pOutputLim)
{
	int retVal = 0;
	CWorkingList *pList = GetWorkingList(StartTime, Duration, pOutputLim);

	// If no list is generated then return the duration of the scenario plus 1 (plus 1 is
	// for the initial animat state).
	if(pList == NULL)
		return Duration+1;

	_ASSERT(pList != NULL);
	retVal = pList->Length();
	DeallocateWorkingList(pList);

	return retVal;
}


CWorkingList *COutputManager::GetWorkingList(int StartTime, int Duration, const OUTPTINTRVLLMT *pOutputLim)
{
	int i,a;
	//int clockTime;
	int csvListLen = m_completeCSVList.Length();
	int acstcSrcLen = m_acousticSrcList.Length();
	CLKBUFFERARRAY *binArray = NULL;
	CLKBUFFERARRAY outputBin = {0};
	CLKBUFFERARRAY *lowestBinArrayPtr;
	int numBins = 0;
	RESLT res = OK;
	ACOUSTICSRCEINF *acSrc;
	int intVal;
	int outputBinCnt = 0;
	CWorkingList *pSaveList = NULL;
	OUTPTINTRVLLMT limOutCpy;// = m_intervalLimit;
	int limOutBuffLen;

	memcpy(&limOutCpy, pOutputLim, sizeof(OUTPTINTRVLLMT));
	if(limOutCpy.interval <= 0)
		limOutCpy.enabled = FALSE;

	//--------------------------------------------------------------------------------//
	// Check if user entered a list or both set the scenario to have acoustic sorces and
	// output only during acoustic pings.
	if(csvListLen == 0 && acstcSrcLen == 0 && limOutCpy.enabled == FALSE)
		return NULL;
	//--------------------------------------------------------------------------------//

	//--------------------------------------------------------------------------------//
	// Determine how many bins will be needed to temporarly hold the lists of iterations
	// to be saved.  There will be one bin for each acoustic source plus one for the CSV
	// list if it is loaed.
	numBins = acstcSrcLen; // 
	if(csvListLen > 0)
		numBins += 1;
	if(limOutCpy.enabled == TRUE)
		numBins++;
	//--------------------------------------------------------------------------------//

	//--------------------------------------------------------------------------------//
	// Allocate memory for the bins that hold the information contained in the lists then
	// zero out the allocated memory.
	if(NULL == (binArray = new CLKBUFFERARRAY[numBins]))
		return NULL;
	memset(binArray, 0, numBins * sizeof(CLKBUFFERARRAY));
	//--------------------------------------------------------------------------------//

	//--------------------------------------------------------------------------------//
	// Fill in the bins from the data stored in the linked lists.
	outputBin.length = 0;

	// The acoustic sources
	for(a=0; a<acstcSrcLen && a<numBins && res == OK; a++)
	{
		acSrc = m_acousticSrcList.Get(a);
		binArray[a].length = 1 + (int)floor(double(Duration)/double(acSrc->dutyPeriod));
		binArray[a].clkBuffer = new int[binArray[a].length];
		
		// Zero out the memory.
		binArray[a].index = 0;
		binArray[a].ptr = &binArray[a].clkBuffer[0];

		// Initialize the bin's buffe with -1's.  -1's indicate an invalid index.
		memset(binArray[a].clkBuffer, -1, sizeof(int)*binArray[a].length);
		for(i=0; i<binArray[a].length; i++)
		{	
			intVal = StartTime + acSrc->beginIteration + i*acSrc->dutyPeriod;
			if(intVal > StartTime+Duration)
				break;
			if(intVal == StartTime)
				continue; // not allowed.
			*binArray[a].ptr = intVal;
			binArray[a].ptr++;
#ifdef _DEBUG
			binArray[a].index++; // not neccesary here... good to hace for debugging.
#endif
		}

		// Reset pointers to begining of list.
		binArray[a].index = 0;
		binArray[a].ptr = &binArray[a].clkBuffer[0];


		// track the needed size of the final output bin.
		outputBin.length += binArray[a].length; 											
	}

	// The CSV values.
	if(csvListLen > 0 && res == OK)
	{
		// Function GetListDataInBuffer() sets the length of the allocated int buffer in
		// the param passed into it.
		if(NULL == (binArray[a].clkBuffer = m_completeCSVList.GetListDataInBuffer(&binArray[a].length)))
			res = MEMALLOC_ERROR;

		_ASSERT(binArray[a].length == m_completeCSVList.Length());

		// Reset pointers to begining of list.
		binArray[a].index = 0;
		binArray[a].ptr = &binArray[a].clkBuffer[0];

		// Adjust the CSV list information for the start time.
		for(i=0; i<binArray[a].length; i++)
		{
			if(binArray[a].clkBuffer[i] > Duration)
				binArray[a].clkBuffer[i] = -1;
			else
				binArray[a].clkBuffer[i] += StartTime;
		}

		// track the needed size of the final output bin.
		outputBin.length += binArray[a].length;

		// Incrment variable 'a' for the next output limiting factor.
		a++;
	}

	// Formula: floor((Duration-start)/interval) + 1
	// Duration = 16, iterations = 17
	// start = 0.
	//	interval =  1: 0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16 = 17 saved iterations floor(16/1)+1  = 16 + 1 = 17
	//	interval =  2: 0, 2,4,6,8,10,12,14,16					 = 9  saved iterations floor(16/2)+1  =  8 + 1 = 9
	//	interval =  3: 0, 3,6,9,12,15							 = 6  saved iterations floor(16/3)+1  =  5 + 1 = 6
	//	interval =  4: 0, 4,8,12,16						         = 5  saved iterations floor(16/4)+1  =  4 + 1 = 5 
	//	interval =  5: 0, 5,10,15								 = 4  saved iterations floor(16/5)+1  =  3 + 1 = 4 
	//	interval =  6: 0, 6,12					  			     = 3  saved iterations floor(16/6)+1  =  2 + 1 = 3 
	//	interval =  7: 0, 7,14									 = 3  saved iterations floor(16/7)+1  =  2 + 1 = 3
	//  interval =  8: 0, 8,16									 = 3  saved iterations floor(16/8)+1  =  2 + 1 = 3
	//  interval =  9: 0, 9										 = 2  saved iterations floor(16/9)+1  =  1 + 1 = 2
	//  interval = 10: 0, 10									 = 2  saved iterations
	//  interval = 11: 0, 11									 = 2  saved iterations
	//  interval = 12: 0, 12									 = 2  saved iterations
	//  interval = 13: 0, 13									 = 2  saved iterations
	//  interval = 14: 0, 14									 = 2  saved iterations
	//  interval = 15: 0, 15									 = 2  saved iterations
	//  interval = 16: 0, 16									 = 2  saved iterations floor(16/16)+1 =  1 + 1 = 2
	//  interval = 17: 0, 										 = 1  saved iterations floor(16/17)+1 =  0 + 1 = 1
	//  interval = 18: 0,										 = 1  saved iterations floor(16/18)+1 =  0 + 1 = 1
	//  ...

	// start = 1    
	//	interval =  2: 1, 3,5,7,9,11,13,15,						 = 8  saved iterations floor(15/2)+1  = 7 + 1 = 8
	//	interval =  3: 1, 4,7,10,13,16,							 = 6  saved iterations floor(15/3)+1  = 5 + 1 = 6
	//	interval =  4: 1, 5,9,13,								 = 4  saved iterations floor(15/4)+1  = 3 + 1 = 4
	//	interval =  5: 1, 6,11,16								 = 4  saved iterations floor(15/5)+1  = 3 + 1 = 4
	//	interval =  6: 1, 7,13									 = 3  saved iterations floor(15/6)+1  = 2 + 1 = 3
	//	interval =  7: 1, 8,15									 = 3  saved iterations floor(15/7)+1  = 2 + 1 = 3
	//  interval =  8: 1, 9,									 = 2  saved iterations floor(15/8)+1  = 2 + 1 = 3
	//  interval =  9: 1, 10									 = 2  saved iterations floor(15/9)+1  = 1 + 1 = 2
	//  interval = 10: 1, 11									 = 2  saved iterations
	//  interval = 11: 1, 12									 = 2  saved iterations
	//  interval = 12: 1, 13									 = 2  saved iterations
	//  interval = 13: 1, 14									 = 2  saved iterations
	//  interval = 14: 1, 15									 = 2  saved iterations
	//  interval = 15: 1, 16									 = 2  saved iterations
	//  interval = 16: 1,										 = 2  saved iterations floor(15/16)+1 = 0 + 1 = 1
	//  interval = 17: 1,										 = 1  saved iterations floor(15/17)+1 = 0 + 1 = 1
	//  interval = 18: 1,										 = 1  saved iterations floor(15/18)+1 = 0 + 1 = 1.
	//			...

	// start = 16.
	//	interval =  1: 16,                                       = 1  saved iterations floor(0/1)+1  =  0 + 1 = 1
	//	interval =  2: 16, 										 = 1  saved iterations floor(0/2)+1  =  0 + 1 = 1


	// The Limit by interval bin.
	if(limOutCpy.enabled == TRUE && res == OK)
	{
		_ASSERT(limOutCpy.interval > 0); // start <= duration because, for example, start = 0 and duration = 0 means only output initial state.

		if((int)limOutCpy.start <= Duration)
		{
			limOutBuffLen = ((int)(floor(((float)Duration-limOutCpy.start)/(float)limOutCpy.interval))) + 1;
		}
		else
		{
			limOutBuffLen = 0;
		}

		// Function GetListDataInBuffer() sets the length of the allocated int buffer in
		// the param passed into it.
		// (Duration+1) for iteration count
		// 
		if(NULL == (binArray[a].clkBuffer = new int[limOutBuffLen]))
			res = MEMALLOC_ERROR;

		binArray[a].length = limOutBuffLen;

		// Reset pointers to begining of list.
		binArray[a].index = 0;
		binArray[a].ptr = &binArray[a].clkBuffer[0];

		// Adjust the interval list information for the start time.
		if(binArray[a].length >= 1)
		{
			binArray[a].clkBuffer[0] = limOutCpy.start;
			for(i=1; i<binArray[a].length; i++)
			{
				binArray[a].clkBuffer[i] = binArray[a].clkBuffer[i-1] + limOutCpy.interval;
			}
		}

		// track the needed size of the final output bin.
		outputBin.length += binArray[a].length;

		a++;
	}
	//--------------------------------------------------------------------------------//

	// Allocate the bin buffer and fill with -1's.  -1's indicate an invalid index.
	if(res == OK && NULL == (outputBin.clkBuffer = new int[outputBin.length]))
		res = MEMALLOC_ERROR;
	memset(outputBin.clkBuffer, -1, outputBin.length*sizeof(int));

	//----------------------------------------------------------------------------------//
	// Build the working list.
	for(i=0; i<outputBin.length; i++)
	{
		//------------------------------------------------------------------------------//
		// Find a non-zero value.  Once a zero has been hit in a bin's buffer that bin
		// is not longer eligible for consideration.
		a = 0;
		while(a < numBins)
		{
			_ASSERT(binArray[a].index <= binArray[a].length);

			//----------------//
			// Test conditions
			//----------------//
			// If bin 'a's index is at the end it is not eligible to be considered or
			// reached -1's then it is no longer eligible to be considered.
			if((binArray[a].index == binArray[a].length) || (*binArray[a].ptr == -1))
				a++;
			else
				break; // break out of the while loop.
		}

		if(a == numBins)
			break; // all -1's found so all done done here.
		//------------------------------------------------------------------------------//

		// -----------------------------------------------------------------------------//
		// Find the lowest non -1 value.
		// -----------------------------------------------------------------------------//
		// Set the bin pointer to bin a then start the for loop by advancing a by 1.  The
		// pointer is now the initial bin that other bins are compared against.
		lowestBinArrayPtr = &binArray[a];
		_ASSERT(binArray[a].index < binArray[a].length);
		
		// Assert that the next entry in bin a is greater than the current entry.
		if(binArray[a].index < binArray[a].length-2 && (binArray[a].clkBuffer[binArray[a].index+1] != -1))
			_ASSERT(binArray[a].clkBuffer[binArray[a].index] < binArray[a].clkBuffer[binArray[a].index+1]);

		for(a=a+1; a<numBins; a++)
		{
			_ASSERT(binArray[a].index <= binArray[a].length);

			//----------------------------------//
			// Verify bin 'a' is not at its end
			//----------------------------------//
			// If bin 'a's index is at the end it is not eligible to be considered.
			if(binArray[a].index == binArray[a].length || (*binArray[a].ptr == -1))
				continue;


			//------------------------//
			// Check for equal values.
			//------------------------//
			// Compare the value the currently lowest bin points to to the value bin a
			// points to.  If they are equal, then this entry in bin a need not be
			// considered so advance bin a to its next entry.
			if(*lowestBinArrayPtr->ptr == *binArray[a].ptr)
			{
				// These two must alwayes be advanced together.
				binArray[a].ptr++;
				binArray[a].index++;
				
				// Assert that the next entry in bin a is greater than the current entry.
				if(binArray[a].index < binArray[a].length-2 && (binArray[a].clkBuffer[binArray[a].index+1] != -1))
					_ASSERT(binArray[a].clkBuffer[binArray[a].index] < binArray[a].clkBuffer[binArray[a].index+1]);

				continue; // continue onto the next bin.
			}

			// If bin a's current entry is discovered to have a smaller value (other than
			// -1) than the compare bin's current entry then make bin a the compare bin.
			if(*lowestBinArrayPtr->ptr > *binArray[a].ptr)
				lowestBinArrayPtr = &binArray[a];
		}

		//if(a == numBins)
		//	break; // all bins searched.
		
		// Place the lowest found bin entry into the output bin's current entry.
		outputBin.clkBuffer[outputBin.index] = *lowestBinArrayPtr->ptr;
		outputBin.index++;
		outputBin.ptr++;
		outputBinCnt++;

		// Advanced the bin that had the lowest pointer
		lowestBinArrayPtr->ptr++;
		lowestBinArrayPtr->index++;
	}
	//----------------------------------------------------------------------------------//

	// Make the save list.
	pSaveList = new CWorkingList();
	pSaveList->SetBufferDataIntoList(outputBinCnt, outputBin.clkBuffer);

	// Delete allocated memory.
	delete [] outputBin.clkBuffer;

	for(a=0; a<numBins; a++)
		delete [] binArray[a].clkBuffer;

	delete [] binArray;

	return pSaveList;
}


RESLT COutputManager::Save(HANDLE Hd, BOOL AcousticPingLimt)
{
	BOOL bRes = TRUE;
	int i;
	int listLen = m_completeCSVList.Length();
	DWORD bytes;

	// Write the length of the CSV list to file
	if(0 == WriteFile(Hd, &listLen, sizeof(listLen), &bytes, NULL))
		return FILEWRITE_ERROR;

	// Write the individual list values to file.
	for(i=0; i<listLen && bRes == TRUE; i++)
		bRes = WriteFile(Hd, m_completeCSVList.Get(i), sizeof(int), &bytes, NULL);

	if(AcousticPingLimt == TRUE)
	{
		// Write the length of the acoustic source list to file
		listLen = m_acousticSrcList.Length();
		if((bRes == TRUE) && (0 == WriteFile(Hd, &listLen, sizeof(listLen), &bytes, NULL)))
			return FILEWRITE_ERROR;

		// Write the individual acoustic source information values to file.
		for(i=0; i<listLen && bRes == TRUE; i++)
			bRes = WriteFile(Hd, m_acousticSrcList.Get(i), sizeof(ACOUSTICSRCEINF), &bytes, NULL);
	}

	if(bRes == FALSE)
		return FILEWRITE_ERROR;

	return OK;
}

RESLT COutputManager::Load(HANDLE Hd, BOOL AcousticPingLimt)
{
	BOOL bRes = TRUE;
	int i;
	int listLen;
	DWORD bytes;

	// Read in the size of the CSV list from file
	if(0 == ReadFile(Hd, &listLen, sizeof(listLen), &bytes, NULL))
		return FILEREAD_ERROR;

	// Read the individual list values from file.
	for(i=0; i<listLen && bRes == TRUE; i++)
		bRes = ReadFile(Hd, m_completeCSVList.Add(), sizeof(int), &bytes, NULL);

	if(AcousticPingLimt == TRUE)
	{
		// Read in the length of the acoustic source list from file
		if((bRes == TRUE) && (0 == ReadFile(Hd, &listLen, sizeof(listLen), &bytes, NULL)))
			return FILEREAD_ERROR;

		// Read the individual acoustic source information values to file.
		for(i=0; i<listLen && bRes == TRUE; i++)
			bRes = ReadFile(Hd, m_acousticSrcList.Add(), sizeof(ACOUSTICSRCEINF), &bytes, NULL);
	}

	if(bRes == FALSE)
		return FILEREAD_ERROR;

	return OK;
}


void COutputManager::DeallocateWorkingList(CWorkingList *pList)
{
	if(pList == NULL)
		return;
	pList->DeleteAll();
	delete pList;
}


// Returns TRUE if the passed in 'Clock' parameter is a state to be saved.  If so then the
// link in the list associated with 'Clock' is removed.
BOOL COutputManager::IsaSaveClockState(CWorkingList *pList, int Clock, BOOL DeleteClockState)
{
	int i;
	int listLength;

	// If no list was ever read in from file then every state is a valid state
	if(/*m_completeCSVList.Length() == 0 || */pList == NULL)
		return TRUE;

	listLength = pList->Length();

	// Determine if the clock value passed in is included in the active list.
	for(i=0; i<listLength; i++)
	{
		if(Clock == *pList->Get(i))
		{
			if(DeleteClockState == TRUE)
				pList->Delete(i);
			return TRUE;
		}
	}
	// Clock value not found, so return FALSE.
	return FALSE;
}



#if 0
RESLT COutputManager::AddAcousticSrceToWorkingList(int StartTime, int Duration, CWorkingList *List, ACOUSTICSRCEINF *Ping)
{
	int i;
	int listPositionIndex = -1;
	int clockTime;
	int value;
	int listLen;

	// i starts at StartTime+1 because the initial state isn't allowed to have an active
	// acoustic source
	_ASSERT(Ping->dutyPeriod > 0);
	if(Ping->dutyPeriod == 0)
		Ping->dutyPeriod = 1; // every second.

	_ASSERT(Ping->dutyPeriod != 0);
	if(Ping->beginIteration == 0)
		Ping->beginIteration = 1;

	// Determine the first clock time to be saved to file from this acoustic source.
	clockTime = StartTime + Ping->beginIteration;

	// +1 because there are duration+1 number of iterations saved to file, as in
	// if the duration is zero then there is a single iteration (the initial) saved
	// to file).
	if(clockTime > StartTime + Duration + 1)
		return OK; // nothing to do.  Scenario duration too short for this duty period.

	//----------------------------------------------------------------------------------//
	// Insert or Add the first link in the acoustic source list.
	// Finds the initial location in the list to begin adds/insertions.
	//----------------------------------------------------------------//
	listLen = List->Length();
	if(listLen == 0)
	{
		// If list length is zero simply add the determined clock time.
		*List->Add() = clockTime;
		listPositionIndex = 0; // for the next iteration
	}
	// If list length is 1
	else if(listLen == 1)
	{
		value = *List->Get(0);
		if(clockTime != value)
		{
			if(clockTime < value)
			{
				*List->Insert(0) = clockTime;
				listPositionIndex = 0; // for the next iteration
			}
			else
			{
				*List->Add() = clockTime;
				listPositionIndex = 1; // for the next iteration
			}
		}
	}
	// If list length is 2 or more
	else
	{
		i = 0;
		value = 0; // Just to quiet the compiler warning.
		listLen = List->Length();
		while(i < listLen && clockTime > (value = *List->Get(i)))
			i++;

		// 'clockTime' is either less than or equal to 'value' from the list or the
		// end of the list was reached.  If it is not equal, then either insert 
		// 'clockTime" into the list if it is less than value or add it to the list
		// if the end of the list was reached.
		if(clockTime != value)
		{
			if(i == listLen)
				*List->Add() = clockTime;
			else
				*List->Insert(i) = clockTime;
			listPositionIndex = i;
		}
	}
	//----------------------------------------------------------------------------------//

	//----------------------------------------------------------------------------------//
	// Insert or Add the subsequent links in the acoustic source list.
	//----------------------------------------------------------------//
	clockTime += Ping->dutyPeriod;
	listLen = List->Length(); // here
	while(clockTime < StartTime + Duration + 1)
	{
		value = clockTime; // here just to quiet compiler warning

		i = listPositionIndex;// + 1;
		//listLen = List->Length(); // here
		while(i < listLen && clockTime > (value = *List->Get(i)))
			i++;

		// 'clockTime' is either less than or equal to 'value' from the list or the
		// end of the list was reached.  If it is not equal, then either insert 
		// 'clockTime" into the list if it is less than value or add it to the list
		// if the end of the list was reached.
		if(clockTime != value)
		{
			if(i == listLen)
				*List->Add() = clockTime;
			else
				*List->Insert(i) = clockTime;
			listPositionIndex = i;
			listLen = List->Length(); // here
		}
		clockTime += Ping->dutyPeriod;
	}
	//----------------------------------------------------------------------------------//

	return OK;
}
#endif