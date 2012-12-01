// This is the main DLL file.

#include "3mbsWrapper.h"
#include "3mbsLib.h"
#include "dataTypes.h"
#include "ScenarioStatic.h"

//#include "3mbsDataTypes.h"

#if 0
/**
<summary>
The constructor for class C3mbs.
</summary>
*/
#endif


mbs::C3mbs::C3mbs()
{
	m_sce = new CScenario;
	m_binOutReader = new COutputReader;
	m_staticScenario = new CStaticScenario;

//	mbsSPECIES_MDL d;
}



String^ mbs::C3mbs::MbsRunStateToString(mbs::mbsRUNSTATE RunState)
{
	switch(RunState)
	{
	case mbsRUNSTATE::DATAEXTRACTING:
		return gcnew String("DATAEXTRACTING");
	case mbsRUNSTATE::INITIALIZING:
		return gcnew String("INITIALIZING");
	case mbsRUNSTATE::FINISHED:
		return gcnew String("FINISHED");
	case mbsRUNSTATE::RUNNING:
		return gcnew String("RUNNING");
	case mbsRUNSTATE::RUNPAUSED:
		return gcnew String("RUNPAUSED");
	case mbsRUNSTATE::UNKNOWN:
		return gcnew String("UNKNOWN");
	}
	return gcnew String("unknown");
}


String^ mbs::C3mbs::MbsResultToString(mbs::mbsRESULT Result)
{
	switch(Result)
	{
	// Non-errors.
	case mbsRESULT::OK: //0
		return gcnew String("OK") ;
	case mbsRESULT::OK_EOFREACHED: // 1
		return gcnew String("OK_EOFREACHED");
	case mbsRESULT::OK_FILESNOTMATCH: // 2
		return gcnew String("OK_FILESNOTMATCH");

	case mbsRESULT::ALREADYRUNNING_ERROR: // 3
		return gcnew String("ALREADYRUNNING_ERROR");
	case mbsRESULT::MEMALLOC_ERROR: // 4
		return gcnew String("MEMALLOC_ERROR");
	case mbsRESULT::MEM_ALREADY_ALLOC_ERROR: // 5
		return gcnew String("MEM_ALREADY_ALLOC_ERROR");

	// Error with the file... as in not formatted properly.
	case mbsRESULT::FILEFORMAT_ERROR: // 6
		return gcnew String("FILEFORMAT_ERROR");
	case mbsRESULT::FILENAME_ERROR: // 7
		return gcnew String("FILENAME_ERROR");
	case mbsRESULT::OPENFILEREAD_ERROR: // 8
		return gcnew String("OPENFILEREAD_ERROR");
	case mbsRESULT::OPENFILEWRITE_ERROR: // 9
		return gcnew String("OPENFILEWRITE_ERROR");
	case mbsRESULT::OPENTEXTOUTPUTFILE_ERROR: // 10
		return gcnew String("OPENTEXTOUTPUTFILE_ERROR");
	case mbsRESULT::CREATEBINARYOUTPUT_ERROR: // 11
		return gcnew String("CREATEBINARYOUTPUT_ERROR");
	case mbsRESULT::OPEN_ESME_EXCHANGE_ERROR: // 12
		return gcnew String("OPEN_ESME_EXCHANGE_ERROR");
	case mbsRESULT::SETFILEPOINTER_ERROR: // 13
		return gcnew String("SETFILEPOINTER_ERROR");
	case mbsRESULT::FILEREAD_ERROR: // 14
		return gcnew String("FILEREAD_ERROR");
	case mbsRESULT::FILEWRITE_ERROR: // 15
		return gcnew String("FILEWRITE_ERROR");
	case mbsRESULT::WRONGFILETYPE_ERROR: // 16
		return gcnew String("WRONG FILE TYPE");
	case mbsRESULT::WRONGENVDATATYPE_ERROR: // 17
		return gcnew String("WRONG ENVIRONMENT DATA TYPE");
	case mbsRESULT::INVALIDHANDLE_ERROR: // 18
		return gcnew String("INVALID HANDLE");
	case mbsRESULT::USERMODELLINELENGTHEXCEEDED_ERROR: // 19
		return gcnew String("User Model Line Length Exceeded");
	case mbsRESULT::UNRECOGNIZED_SPECIES_MATRIX_PARAM_ERROR: // 20
		return gcnew String("UNRECOGNIZED_SPECIES_MATRIX_PARAM_ERROR");

	case mbsRESULT::NOSPECIESLOADED_ERROR: // 21
		return gcnew String("NOSPECIESLOADED_ERROR");
	case mbsRESULT::NOANIMATPOPULATION_ERROR: // 22
		return gcnew String("NOANIMATPOPULATION_ERROR");
	case mbsRESULT::UNPOPULATEDSPECIES_ERROR: // 23
		return gcnew String("UNPOPULATEDSPECIES_ERROR");
	case mbsRESULT::POPLIMITEXCEEDED_ERROR: // 24
		return gcnew String("POPLIMITEXCEEDED_ERROR");
	case mbsRESULT::BUFFERLENGTH_INADEQUATE_ERROR: // 25
		return gcnew String("BUFFERLENGTH_INADEQUATE_ERROR");
	case mbsRESULT::INVALID_GENERAL_INDEX_ERROR: // 26
		return gcnew String("INVALID_GENERAL_INDEX_ERROR");
	case mbsRESULT::INVALID_SPECIES_INDEX_ERROR: // 27
		return gcnew String("INVALID_SPECIES_INDEX_ERROR");
	case mbsRESULT::INVALID_POD_INDEX_ERROR: // 28
		return gcnew String("INVALID_POD_INDEX_ERROR");
	case mbsRESULT::INVALID_INDIVIDUAL_INDEX_ERROR: // 29
		return gcnew String("INVALID_INDIVIDUAL_INDEX_ERROR");
	case mbsRESULT::INVALID_ANIMAT_INDEX_ERROR: // 30
		return gcnew String("INVALID_ANIMAT_INDEX_ERROR");

	case mbsRESULT::PARAM_HAD_NULLREF_ERROR: // 31
		return gcnew String("PARAM_HAD_NULLREF_ERROR");
	case mbsRESULT::PARAM_INVALID_ERROR: // 32
		return gcnew String("PARAM_INVALID_ERROR");
	case mbsRESULT::UNPOPULATED_POD_ERROR: // 33
		return gcnew String("UNPOPULATED_POD_ERROR");
	case mbsRESULT::FILE_NOT_OPEN_ERROR: // 34
		return gcnew String("FILE_NOT_OPEN_ERROR");
	case mbsRESULT::INVALID_ITERATION_INDEX: // 35
		return gcnew String("INVALID_ITERATION_INDEX");
	case mbsRESULT::INVALID_DIRECTORY: // 36
		return gcnew String("INVALID_DIRECTORY");

	case mbsRESULT::SET_DIRECTORY_ERROR: // 37
		return gcnew String("SET_DIRECTORY_ERROR");
	case mbsRESULT::DELETE_FILE_ERROR: // 38
		return gcnew String("DELETE_FILE_ERROR");
	case mbsRESULT::DIRECTORY_NOT_EMPTY_TO_DELETE: // 39
		return gcnew String("DIRECTORY_NOT_EMPTY_TO_DELETE");
	case mbsRESULT::DELETE_DIRECTORY_ERROR: // 40
		return gcnew String("DELETE_DIRECTORY_ERROR");


	case mbsRESULT::OBSOLETE_SPECIES_VERSION: // 41
		return gcnew String("Obsolete Species Version");
	case mbsRESULT::OBSOLETE_3MBS_VERSION: // 42
		return gcnew String("Obsolete 3MB and/or Species Builder Version");
	case mbsRESULT::FILEMANAGER_SPECIESLIST_UNSET: // 43
		return gcnew String("FILEMANAGER_SPECIESLIST_UNSET");
	case mbsRESULT::SCEPARAMS_NOT_MATCH_FILEMANAGER_PARAMS: // 44
		return gcnew String("SCENARIO_PARAMS_NOT_MATCH_FILEMANAGER_PARAMS");
	case mbsRESULT::FILE_NOT_OPEN: // 45
		return gcnew String("FILE_NOT_OPEN");

	case mbsRESULT::FILE_EXTENSION_TOO_LARGE: // 46
		return gcnew String("FILE_EXTENSION_TOO_LARGE");
	case mbsRESULT::FILE_FILTER_TOO_LARGE: // 47
		return gcnew String("FILE_FILTER_TOO_LARGE");
	case mbsRESULT::FILE_PATH_TOO_LONG: // 48
		return gcnew String("FILE_PATH_TOO_LONG");

	case mbsRESULT::FILESIZE_TOO_LARGE: // 49
		return gcnew String("FILESIZE_TOO_LARGE");
	case mbsRESULT::MAX_NUM_PLAYBACK_STATES_EXCEEDED: // 50
		return gcnew String("MAX_NUM_PLAYBACK_STATES_EXCEEDED");
	case mbsRESULT::INVALID_SEEEDING_DEPTH : // 51
		return gcnew String("INVALID_SEEEDING_DEPTH ");
	case mbsRESULT::INVALID_SPECIES_SEEEDING_DEPTH: // 52
		return gcnew String("INVALID_SPECIES_SEEEDING_DEPTH");
	case mbsRESULT::COORDINATE_OFF_MAP: // 52
		return gcnew String("COORDINATE_OFF_MAP");	
	case mbsRESULT::NULL_POINTER_RETUNRED_ERROR:
		return gcnew String("NULL_POINTER_RETURNED_ERROR");	
	default:
		return gcnew String("unlisted error....");
	}
	return gcnew String("unlisted error....");
}

mbs::mbsBUILDINF mbs::C3mbs::GetBuildInformation()
{
	mbs::mbsBUILDINF infCpy;
	BUILDINF inf;

	inf = m_staticScenario->GetBuildInformation();

	if(inf.buildType == MBRELEASE)
		infCpy.buildType = mbs::mbsBUILDTYPE::MBRELEASE;
	else
		infCpy.buildType = mbs::mbsBUILDTYPE::MBDEBUG;

	infCpy.bitSize = inf.bitSize;

	//SpeMdl->behavior[i].szName = C3mbs::CharBufferToString(behSrc->szName);
	infCpy.szBuildDate = C3mbs::CharBufferToString(inf.szBuildDate);
	infCpy.szBuildTime = C3mbs::CharBufferToString(inf.szBuildTime);
	return infCpy;
}


/**
<summary>
	<shortname> CharBufferFromString </shortname>
	<purpose> Translates a class String into a null terminated character string. </purpose>
	<param name='Sz'> The instance of class string to translate into a character string</param>
	<param name='szBuffer'> A reference to a character array that will hold the translated string</param>
	<param name='BufferLen'> The array size of szBuffer</param>
	<returns>A string representation of the input parameter 'Result'.</returns>
</summary>
*/
char *mbs::C3mbs::CharBufferFromString(String^ Sz, char *szBuffer, int BufferLen)
{
	int i;
	array<Char>^ input = Sz->ToCharArray();
	memset(szBuffer, 0, BufferLen);

	for(i=0; i<Sz->Length && i<BufferLen-1; i++)
		szBuffer[i] = (char)input[i];

	delete input;
	return szBuffer;
}

String ^mbs::C3mbs::CharBufferToString(char *szBuffer)
{
//	String ^sz = gcnew String(strlen(szBuffer)); //new String(strlen(szBuffer));
	return gcnew String(szBuffer); //new String(strlen(szBuffer));
	//sz->
}


mbs::mbsRESULT mbs::C3mbs::SetSpeciesDef(/*mbs::mbsSPECIES_MDL^ SpeMdl*/)
{
	return mbsRESULT::OK;
}

double *mbs::C3mbs::DoubleArrayToDoubleBuffer(array<double>^ D)
{
	int i;
#if 0
	array<double>^ darray = gcnew array<double>(5);
	darray[0] = .5;
#endif

//	array<array<double>^>^ darray = gcnew array<array<double>>(5);
	//array<array^>^ dog = gcnew array<array^>(3);
	double *d = new double[D->Length];
	for(i=0; i<D->Length; i++)
		d[i] = D[i];
	return d;
}


mbs::mbsRESULT mbs::C3mbs::SimpleMbsResult(RESLT Res)
{
	if(Res == OK_EOFREACHED || Res == OK_FILESNOTMATCH)
		return mbsRESULT::OK;

	return mbsRESULT(Res);
}

#if 0
/**
<summary> Determines if the input parameter references a valid species index in the 3MBS animat population.
<param name='Index'> The species index in question.</param>
<returns> A mbs::mbsRESULT, either:
<para> OK if valid, or </para>
<para> INVALID_SPECIES_INDEX_ERROR if invalid </para>
</returns>
</summary>
*/
#endif
mbs::mbsRESULT mbs::C3mbs::ValidSpeciesIndex(int Index)
{
	if(Index <0 || Index >= GetSpeciesCount())
		return mbsRESULT::INVALID_SPECIES_INDEX_ERROR;
	return mbsRESULT::OK;
}

#if 0
/**
<summary> Determines if the two input parameters reference a valid pod index of a valid species in the 3MBS animat population.
<param name='SpeciesIndex'> The species index the pod belongs to.</param>
<param name='Index'> The pod index in question.</param>
<returns> A mbs::mbsRESULT:
<para> OK if valid, or </para>
<para> INVALID_SPECIES_INDEX_ERROR if species index was invalid </para>
<para> INVALID_POD_INDEX_ERROR if the pod index was invalid </para>
</returns>
</summary>
*/
#endif
mbs::mbsRESULT mbs::C3mbs::ValidPodIndex(int SpeciesIndex, int Index)
{
	mbsRESULT res;
	int podCount;
	if(mbsRESULT::OK != (res = ValidSpeciesIndex(SpeciesIndex)))
		return res;
	podCount = GetPodCount(SpeciesIndex);
	if(Index < 0 || Index >= podCount)
		return mbsRESULT::INVALID_POD_INDEX_ERROR;
	return mbsRESULT::OK;
}

#if 0
/**
<summary> Determines if the three input params reference a valid animat index of a valid pod index of a valid species index in the 3MBS animat population.
<param name='SpeciesIndex'> The species index the pod belongs to.</param>
<param name='PodIndex'> The pod index the animat belongs to.</param>
<param name='Index'> An animat index in question.</param>
<returns> A mbs::mbsRESULT:
<para> OK if valid, or </para>
<para> INVALID_SPECIES_INDEX_ERROR if species index is invalid </para>
<para> INVALID_POD_INDEX_ERROR if the pod index is invalid </para>
<para> INVALID_ANIMAT_INDEX_ERROR if the pod index is invalid </para>
</returns>
</summary>
*/
#endif
mbs::mbsRESULT mbs::C3mbs::ValidPodAnimatIndex(int SpeciesIndex, int PodIndex, int Index)
{
	mbsRESULT res;
	if(mbsRESULT::OK != (res = ValidPodIndex(SpeciesIndex, PodIndex)))
		return res;
	if(Index < 0 || Index >= GetPodMemberCount(SpeciesIndex, PodIndex))
		return mbsRESULT::INVALID_ANIMAT_INDEX_ERROR;
	return mbsRESULT::OK;
}
#if 0
/**
<summary> Determines if the two input parameters reference a valid animat index, that is an individual of the specified species, in the 3MBS animat population.
<param name='SpeciesIndex'> The species the individual animat belongs to.</param>
<param name='Index'> The individual animat index in question.</param>
<returns> A mbs::mbsRESULT:
<para> OK if valid, or </para>
<para> INVALID_SPECIES_INDEX_ERROR if species index is invalid </para>
<para> INVALID_INDIVIDUAL_INDEX_ERROR if the individual index is invalid </para>
</returns>
</summary>
*/
#endif
mbs::mbsRESULT mbs::C3mbs::ValidIndividualIndex(int SpeciesIndex, int Index)
{
	mbsRESULT res;
	if(mbsRESULT::OK != (res = ValidSpeciesIndex(SpeciesIndex)))
		return res;
	if(Index < 0 || Index >= GetIndivdualCount(SpeciesIndex))
		return mbsRESULT::INVALID_INDIVIDUAL_INDEX_ERROR;
	return mbsRESULT::OK;
}

#if 0
/**
<summary>
  Determines if the input parameter references a valid animat within the entire
  population.
<remarks>
  Note that animats may be indexed based upon where there index in reference to the
  animat population as
  a whole without specifying species or pod membership indices or specifying if the
  animat is an
  individual belonging to a particular species.  This indexing is based soley on the
  total number of animats in the population.  Therefore, if the total animat population
  were 100, regardless of species membership, pod membership, etc., valid animat indices
  would be 0-99.
<para>
  This type of indexing is most useful when running a scenario when needing get 
  animat location, state, etc and setting environmental data such as bathymetry
  or acoustic exposure.  This type of indexing shouldn't be used for populating
  a scenario when pod membership, species membership, and the like are considered.
</para>
</remarks>
<param name='Index'>
  The animat index in question.
</param>
<returns>
  A mbs::mbsRESULT:
<para> OK if valid, or </para>
<para> INVALID_SPECIES_INDEX_ERROR if species index is invalid </para>
<para> INVALID_INDIVIDUAL_INDEX_ERROR if the individual index is invalid </para>
</returns>
</summary>
*/
#endif
mbs::mbsRESULT mbs::C3mbs::ValidAnimatIndex(int Index)
{
	if(Index < 0 || Index >= GetAnimatCount())
		return mbsRESULT::INVALID_ANIMAT_INDEX_ERROR;
	return mbsRESULT::OK;
}

mbs::mbsRESULT mbs::C3mbs::CoordStructToPositionArray(INHABITINF *C, int NumAnimats, array<mbs::mbsPosition>^ P)
{
	int i;

	if(P == nullptr)
		return mbs::mbsRESULT::PARAM_HAD_NULLREF_ERROR;
	if(P->Length < NumAnimats)
		return mbs::mbsRESULT::BUFFERLENGTH_INADEQUATE_ERROR;

	// Copy the location/position/coordinate data over
	for(i=0; i<NumAnimats; i++)
	{
		P[i].latitude = C[i].coord.lat;
		P[i].longitude = C[i].coord.lon;
		P[i].depth = C[i].coord.depth;
	}
	for(; i<P->Length; i++)
		P[i].latitude = P[i].longitude = P[i].depth = 0;
	return mbs::mbsRESULT::OK;
}

// User must deallocate C, C must be passed in set to NULL.
mbs::mbsRESULT mbs::C3mbs::PositionArrayToInitialConditionStruct(INHABITINF **IC, int NumAnimats, array<mbs::mbsPosition>^ P)
{
	int i;
	INHABITINF *c;
	if(NumAnimats < 0)
		return mbsRESULT::PARAM_INVALID_ERROR;
	if(NumAnimats == 0)
	{
		*IC = NULL;
		return mbsRESULT::OK;
	}
	if(NumAnimats > 0)
	{
		if(P == nullptr)
			return mbsRESULT::PARAM_HAD_NULLREF_ERROR;
		if(P->Length < NumAnimats)
			return mbsRESULT::BUFFERLENGTH_INADEQUATE_ERROR;
	}

	if(NULL == (c = new INHABITINF[NumAnimats]))
		return mbsRESULT::MEMALLOC_ERROR;

	for(i=0; i<NumAnimats; i++)
	{
		c[i].coord.lat = P[i].latitude;
		c[i].coord.lon = P[i].longitude;
		c[i].coord.depth = P[i].depth;

		// Other non-positional initial or set conditions.
		// No acoustic sources are (currently) allowed to be set up through the wrapper 
		// class interface but maybe later for testing purposes.
		c[i].acstcSrc.beginIteration = 0;
		c[i].acstcSrc.dutyPeriod = 0;
		c[i].acstcSrc.outputValue = 0;
	}
	*IC = c;
	return mbs::mbsRESULT::OK;;
}


mbs::mbsRESULT mbs::C3mbs::CoordToPosition(COORD_DEPTH *C, mbs::mbsPosition^ P)
{
	mbsRESULT res = mbs::mbsRESULT::OK;
	if(P == nullptr)
		return mbsRESULT::PARAM_HAD_NULLREF_ERROR;
	if(C == NULL)
	{
		P->latitude = P->longitude = P->depth = 0;
		return mbsRESULT::PARAM_HAD_NULLREF_ERROR;
	}
	P->latitude = C->lat; 	P->longitude = C->lon; 	P->depth = C->depth;
	return res;
}

#if 0
/**
<summary>
  Loads a 3mbs scenario (.sce) file.
<param name = 'FileName'> Name of the 3mbs scenario (.sce) file to load in.</param>
<returns>   A mbs::mbsRESULT:  OK if succesfull. </returns>
</summary>
*/
#endif
mbs::mbsRESULT mbs::C3mbs::LoadScenario(String ^FileName)
{
	char sz[SIZE_512];
	RESLT res;
	if(FileName == nullptr)
		return mbsRESULT::PARAM_HAD_NULLREF_ERROR;

	if(OK != (res = m_sce->LoadFromBinFile(CharBufferFromString(FileName, sz, SIZE_512))))
		return SimpleMbsResult(res);

	m_sce->SetScenarioTitle(CharBufferFromString(FileName, sz, SIZE_512));
	return SimpleMbsResult(res);
}
#if 0
/**
<summary>
  Saves a 3mbs scenario (.sce) file.
<param name = 'FileName'> Name of the 3mbs scenario (.sce) file to save to.</param>
<returns>   A mbs::mbsRESULT:  OK if succesfull. </returns>
</summary>
*/
#endif
mbs::mbsRESULT mbs::C3mbs::SaveScenario(String ^FileName)
{
	char sz[SIZE_512];
	if(FileName == nullptr)
		return mbsRESULT::PARAM_HAD_NULLREF_ERROR;
	return SimpleMbsResult(m_sce->SaveToBinFile(CharBufferFromString(FileName, sz, SIZE_512)));
}


#if 0
/**
<summary>
  Saves a 3mbs scenario details to a text file.
<param name = 'FileName'> Name of the text file to save to.</param>
<returns>   A mbs::mbsRESULT:  OK if succesfull. </returns>
</summary>
*/
#endif
mbs::mbsRESULT mbs::C3mbs::ScenarioToText(String^ FileName)
{
	char sz[SIZE_512];
	if(FileName == nullptr)
		return mbsRESULT::PARAM_HAD_NULLREF_ERROR;
	return SimpleMbsResult(m_sce->ScenarioToText(CharBufferFromString(FileName, sz, SIZE_512)));
}

#if 0
/**
<summary>
  Saves a 3mbs species's details to a text file.
<param name = 'SpeciesIndex'> The index of the species to save.</param>
<param name = 'FileName'> Name of the text file to save the species to.</param>
<returns>   A mbs::mbsRESULT:  OK if succesfull. </returns>
</summary>
*/
#endif
mbs::mbsRESULT mbs::C3mbs::SpeciesToText(int SpeciesIndex, String^ FileName)
{
	char sz[SIZE_512];
	mbsRESULT res;
	if(mbsRESULT::OK != (res = ValidSpeciesIndex(SpeciesIndex)))
		return res;
	if(FileName == nullptr)
		return mbsRESULT::PARAM_HAD_NULLREF_ERROR;
	return SimpleMbsResult(m_sce->SpeciesToText(SpeciesIndex, CharBufferFromString(FileName, sz, SIZE_512)));
}

mbs::mbsRESULT mbs::C3mbs::SetOutputDirectory(String^ Directory)
{
	char sz[SIZE_512];
	return mbsRESULT(m_sce->SetOutputDirectory(CharBufferFromString(Directory, sz, SIZE_512)));
}


mbs::mbsRESULT mbs::C3mbs::CompareFileOutput(String^ FileName1, String^ FileName2)
{
	char sz1[SIZE_512];
	char sz2[SIZE_512];
	if(FileName1 == nullptr)
		return mbsRESULT::PARAM_HAD_NULLREF_ERROR;
	if(FileName2 == nullptr)
		return mbsRESULT::PARAM_HAD_NULLREF_ERROR;
	CharBufferFromString(FileName1, sz1, SIZE_512);
	CharBufferFromString(FileName2, sz2, SIZE_512);
	return mbsRESULT(m_sce->CompareFileOutput(sz1, sz2));
}


#if 0
/**
<summary>
  Sets the animat acoustic exposures.
<param name = 'SourceLat'> Acoustic source latitudal position</param>
<param name = 'SourceLon'> Acoustic source longitudal position</param>
<param name = 'AcousticExposureArray'>
  The array containing acoustic exosure values for each animat
</param>
<remarks>
  1) The length of the acoustic exposure array parameter must have a length equal to or
  greater than the number of animats in the scenario.
  2) If the acoustic exposure value isn't set the previous set value is retained.
  Therefore, if acoustic exposure drops to a very low value or zero, explicity set it.
  3) Zero is currently the lowest acoustic exposure value that may be set.
  4) Each animat maintain a memory of the last previous acoustic exposure position
  (lat/lon) until a value greater than zero is passed in.  This is necessary because
  one an animat's threshold is exceeded it stays in an adversive state fleeing away
  from the acoustic exposure's source.  
</remarks>
<returns>   A mbs::mbsRESULT:  OK if succesfull. </returns>
</summary>
*/
#endif
mbs::mbsRESULT mbs::C3mbs::SetAnimatAcousticExposure(double SourceLat, double SourceLon, array<double>^ AcousticExposureArray)
{
	int numAnimats = m_sce->GetAnimatCount();
	double *db = NULL;
	mbsRUNSTATE rs = GetRunState();


#pragma message("Make this an error later.  Just Returns for now.")
	// If there scenario isn't running there's nothing to do here.  This really should be an error.
	if(rs != mbsRUNSTATE::RUNNING && rs != mbsRUNSTATE::RUNPAUSED)
		return mbs::mbsRESULT::OK;

	//printf("%s\n",mbs::C3mbs::MbsRunStateToString(rs));

	if(AcousticExposureArray->Length < numAnimats)
		return mbs::mbsRESULT::BUFFERLENGTH_INADEQUATE_ERROR;

	if(NULL == (db = DoubleArrayToDoubleBuffer(AcousticExposureArray)))
		return mbs::mbsRESULT::MEMALLOC_ERROR;

	m_sce->SetAnimatAcousticExposure(SourceLat, SourceLon, db);
	delete [] db;

	return mbs::mbsRESULT::OK;
}

double mbs::C3mbs::SetBaythyConstantDepth(double Depth) // must be negative and deeper than beaching depth
{
	return m_sce->SetBaythyConstantDepth(Depth);
}


mbs::mbsRESULT mbs::C3mbs::SetAnimatBathymetry(array<double>^ BathyArray)
{
	int numAnimats = m_sce->GetAnimatCount();
	double *db = NULL;

	if(BathyArray->Length < numAnimats)
		return mbs::mbsRESULT::BUFFERLENGTH_INADEQUATE_ERROR;

	if(NULL == (db = DoubleArrayToDoubleBuffer(BathyArray)))
		return mbs::mbsRESULT::MEMALLOC_ERROR;

	m_sce->SetAnimatBathymetry(db);
	delete [] db;

	return mbs::mbsRESULT::OK;
}

bool mbs::C3mbs::IsActive()
{
	if(m_sce->IsActive() == TRUE)
		return true;
	return false;
}

mbs::mbsRESULT mbs::C3mbs::GetIndividualInitialCoordinate(int SpeciesIndex, int IndividualIndex, mbsPosition^ Position)
{
	mbsRESULT res;
	INHABITINF c = {0};

	if(mbsRESULT::OK != (res = ValidIndividualIndex(SpeciesIndex, IndividualIndex)))
		return res;
	c = m_sce->GetIndividualInitialCoordinate(SpeciesIndex, IndividualIndex);
	res = CoordToPosition(&c.coord, Position);
	return res;
}

mbs::mbsRESULT mbs::C3mbs::GetPodMemberInitialCoordinate(int SpeciesIndex, int PodIndex, int PodMemberIndex, mbsPosition^ Position)
{
	mbsRESULT res;
	INHABITINF c = {0};

	if(mbsRESULT::OK != (res = ValidPodAnimatIndex(SpeciesIndex, PodIndex, PodMemberIndex)))
		return res;
	c = m_sce->GetPodMemberInitialCoordinate(SpeciesIndex, PodIndex, PodMemberIndex);
	res = CoordToPosition(&c.coord, Position);
	return res;
}

void mbs::C3mbs::ResetRunCount()
{
	m_sce->ResetRunCount();
}

mbs::mbsSCESTATE mbs::C3mbs::GetScenarioState()
{
	mbs::mbsSCESTATE r;
	SCESTATE s = m_sce->GetState();

	r.activity = (mbs::mbsSCEACTIVITY)s.activity;
	r.runClock = s.runClock;
	r.currentAnimat = s.currentAnimat;
	r.currentIteration = s.currentIteration;
	r.runNumber = s.runNumber;
	r.errorStatus = (mbsRESULT)s.errorStatus;

	// Acoustic Source state
	r.acousticSrc.lat = s.acousticSrc.lat;
	r.acousticSrc.lon =s.acousticSrc.lon;
	r.acousticSrc.active = s.acousticSrc.active;


	r.bufferState.numBytes = s.bufferState.numBytes;
	r.bufferState.bufferCycleCount = s.bufferState.bufferCycleCount;
	r.bufferState.bufferIterationLevel = s.bufferState.bufferIterationCapacity;
	r.bufferState.fileIterationRWCount = s.bufferState.fileIterationRWCount;
	r.bufferState.animatStateSize = s.bufferState.animatStateSize;
	r.bufferState.acousticSrcStateSize = s.bufferState.acousticSrcStateSize;
	r.bufferState.currentAnimatFlush = s.bufferState.currentAnimatFlush;
	r.bufferState.bufferIterationCapacity = s.bufferState.bufferIterationCapacity;

	return r;
}


mbs::mbsRUNSTATE mbs::C3mbs::GetRunState()
{
	return mbs::mbsRUNSTATE(m_sce->GetRunState_old());
}


//mbs::mbsRESULT mbs::C3mbs::GetAnimatCoordinates(array<mbs::mbsPosition>^ Position)
mbs::mbsRESULT mbs::C3mbs::GetAnimatCoordinates(array<mbs::mbsPosition>^ Position)
{
	mbsRESULT res = mbs::mbsRESULT::OK;
	int numAnimats = GetAnimatCount();
	INHABITINF *c;

	// Allocate Memory
	if(NULL == (c = new INHABITINF[numAnimats]))
		return mbs::mbsRESULT::MEMALLOC_ERROR;

	// Get the animat coordinates, copy them into the position array
	m_sce->GetAnimatPopulationCurrentCoordinates(c);
	res = CoordStructToPositionArray(c, numAnimats, Position);

	// Free allocated memory and return result
	delete [] c;
	return res;
}


mbs::mbsRESULT mbs::C3mbs::GetAnimatCoordinates(int SpeciesIndex, array<mbs::mbsPosition>^ Position)
{
	int numAnimats;
	INHABITINF *c;
	mbsRESULT res = mbs::mbsRESULT::OK;

	// Verify a valid species index passed in.
	if(mbsRESULT::OK != (res = ValidSpeciesIndex(SpeciesIndex)))
		return res;

	// Get the animat count, allocate memory
	numAnimats = GetAnimatCount(SpeciesIndex);
	if(NULL == (c = new INHABITINF[numAnimats]))
		return mbs::mbsRESULT::MEMALLOC_ERROR;

	// Get the animat coordinates, copy them into the position array
	m_sce->GetAnimatCurrentCoordinates(SpeciesIndex, numAnimats, c);
	res = CoordStructToPositionArray(c, numAnimats, Position);

	// Free allocated memory and return result
	delete [] c;
	return res;
}
mbs::mbsRESULT mbs::C3mbs::GetPodMemberCoordinates(int SpeciesIndex, int PodIndex, array<mbs::mbsPosition>^ Position)
{
	int numAnimats;
	INHABITINF *c;
	mbsRESULT res = mbs::mbsRESULT::OK;

	// Verify a valid species index passed in.
	if(mbsRESULT::OK != (res = ValidPodIndex(SpeciesIndex, PodIndex)))
		return res;

	// Get the animat count, allocate memory
	numAnimats = GetPodMemberCount(SpeciesIndex, PodIndex);
	if(NULL == (c = new INHABITINF[numAnimats]))
		return mbs::mbsRESULT::MEMALLOC_ERROR;

	// Get the animat coordinates, copy them into the position array
	m_sce->GetPodMemberCoordinates(SpeciesIndex, PodIndex, c);
	res = CoordStructToPositionArray(c, numAnimats, Position);

	// Free allocated memory and return result
	delete [] c;
	return res;
}


mbs::mbsRESULT mbs::C3mbs::GetPodMemberCoordinate(int SpeciesIndex, int PodIndex, int PodMemberIndex, mbsPosition^ Position)
{
	mbsRESULT res;
	INHABITINF c;
	if(mbsRESULT::OK != (res = ValidPodAnimatIndex(SpeciesIndex, PodIndex, PodMemberIndex)))
		return res;
	c = m_sce->GetPodMemberCoordinate(SpeciesIndex, PodIndex, PodMemberIndex);
	res = CoordToPosition(&c.coord, Position);
	return res;
}
mbs::mbsRESULT mbs::C3mbs::GetIndividualCoordinates(int SpeciesIndex, array<mbs::mbsPosition>^ Position)
{
	int numAnimats;
	INHABITINF *c;
	mbsRESULT res = mbs::mbsRESULT::OK;

	// Verify a valid species index passed in.
	if(mbsRESULT::OK != (res = ValidSpeciesIndex(SpeciesIndex)))
		return res;

	// Get the animat count, allocate memory
	numAnimats = GetIndivdualCount(SpeciesIndex);
	if(NULL == (c = new INHABITINF[numAnimats]))
		return mbs::mbsRESULT::MEMALLOC_ERROR;

	// Get the animat coordinates, copy them into the position array
	m_sce->GetIndividualCoordinates(SpeciesIndex, c);
	res = CoordStructToPositionArray(c, numAnimats, Position);

	// Free allocated memory and return result
	delete [] c;
	return res;
}
mbs::mbsRESULT mbs::C3mbs::GetIndividualCoordinate(int SpeciesIndex, int IndividualIndex, mbsPosition^ Position)
{
	mbsRESULT res;
	INHABITINF c;
	if(mbsRESULT::OK != (res = ValidIndividualIndex(SpeciesIndex, IndividualIndex)))
		return res;
	c = m_sce->GetIndividualCoordinate(SpeciesIndex, IndividualIndex);
	res = CoordToPosition(&c.coord, Position);
	return res;
}

//--------//
// Adds
//--------//
mbs::mbsRESULT mbs::C3mbs::SeedingCoordinateIsValid(int SpeciesIndex, mbsPosition^ Position)
{
	mbsRESULT mbsres;
	mbsres = SimpleMbsResult(m_sce->SeedingCoordinateIsValid(SpeciesIndex, Position->latitude, Position->longitude));
	return mbsres;
}

double mbs::C3mbs::GetShoreFollowingDepth(int SpeciesIndex)
{
	return m_sce->GetShoreFollowingDepth((unsigned int)SpeciesIndex);
}

double mbs::C3mbs::GetMinimumSeededingDepth(int SpeciesIndex)
{
	return m_sce->GetMinimumSeededingDepth((unsigned int)SpeciesIndex);
}


mbs::mbsRESULT mbs::C3mbs::AddSpecies(String^ FileName)
{
	char sz[SIZE_512];
	if(FileName == nullptr)
		return mbsRESULT::PARAM_HAD_NULLREF_ERROR;
	return SimpleMbsResult(m_sce->AddSpecies(CharBufferFromString(FileName, sz, sizeof(sz))));
}
mbs::mbsRESULT mbs::C3mbs::AddPod(int SpeciesIndex)
{
	return AddPod(SpeciesIndex, mbsPODLEADERTYPE::ANIMAT, 0, 0, nullptr);
}
mbs::mbsRESULT mbs::C3mbs::AddPod(int SpeciesIndex, mbsPODLEADERTYPE LeaderType, double FocalDistance, int NumAnimats, array<mbs::mbsPosition>^ Position)
{
	INHABITINF *c = NULL;
	PODLEADERTYPE plt;
	mbsRESULT res;
	int i;

	res = ValidSpeciesIndex(SpeciesIndex);
	if(mbsRESULT::OK != res)
		return res;


	for(i=0; i<NumAnimats && mbsRESULT::OK == res; i++)
		res = SeedingCoordinateIsValid(SpeciesIndex, Position[i]);
	if(mbsRESULT::OK != res)
		return res;

	res = PositionArrayToInitialConditionStruct(&c, NumAnimats, Position);
	if(mbsRESULT::OK != res)
		return res;


	if(LeaderType == mbsPODLEADERTYPE::ANIMAT)
		plt = ANIMAT;
	else
		plt = CENTROID;
	//res = SimpleMbsResult(m_sce->AddPod(SpeciesIndex, *plt, FocalDistance, NumAnimats, c));
	m_sce->AddPod(SpeciesIndex, plt, FocalDistance, NumAnimats, c);
	delete [] c;
	return res;
}
mbs::mbsRESULT mbs::C3mbs::AddPodAnimat(int SpeciesIndex, int PodIndex, mbsPosition Position)
{
	INHABITINF c = {0};
	mbsRESULT res;

	res = ValidPodIndex(SpeciesIndex, PodIndex);
	if(mbsRESULT::OK != res)
		return res;

	res = SeedingCoordinateIsValid(SpeciesIndex, Position);
	if(mbsRESULT::OK != res)
		return res;

	c.coord.lat = Position.latitude;
	c.coord.lon = Position.longitude;
	res = SimpleMbsResult(m_sce->AddPodMember(SpeciesIndex, PodIndex, c));
	return res;
}
mbs::mbsRESULT mbs::C3mbs::AddPodAnimats(int SpeciesIndex, int PodIndex, int NumAnimats, array<mbsPosition>^ Position)
{
	mbsRESULT res;
	INHABITINF *c = NULL;
	int i;

	if(NumAnimats == 0)
		return mbsRESULT::OK;

	if(mbsRESULT::OK != (res = ValidPodIndex(SpeciesIndex, PodIndex)))
		return res;

	for(i=0; i<NumAnimats && mbsRESULT::OK == res; i++)
		res = SeedingCoordinateIsValid(SpeciesIndex, Position[i]);
	if(mbsRESULT::OK != res)
		return res;

	if(mbsRESULT::OK != (res = PositionArrayToInitialConditionStruct(&c, NumAnimats, Position)))
		return res;

	res = SimpleMbsResult(m_sce->AddPodMembers(SpeciesIndex, PodIndex, c, NumAnimats));
	delete [] c;
	return res;
}

mbs::mbsRESULT mbs::C3mbs::AddIndividualAnimat(int SpeciesIndex, mbsPosition Position)
{
	INHABITINF c = {0};
	mbsRESULT res;

	res = ValidSpeciesIndex(SpeciesIndex);
	if(mbsRESULT::OK != res)
		return res;

	res = SeedingCoordinateIsValid(SpeciesIndex, Position);
	if(mbsRESULT::OK != res)
		return res;

	c.coord.lat = Position.latitude;
	c.coord.lon = Position.longitude;

	res = SimpleMbsResult(m_sce->AddIndividual(SpeciesIndex, c));
	return res;
}
mbs::mbsRESULT mbs::C3mbs::AddIndividualAnimats(int SpeciesIndex, int NumAnimats, array<mbsPosition>^ Position)
{
	int i;
	INHABITINF *c = NULL;
	mbsRESULT res;

	if(NumAnimats == 0)
		return mbsRESULT::OK;

	res = ValidSpeciesIndex(SpeciesIndex);
	if(res != mbsRESULT::OK)
		return res;

	for(i=0; i<NumAnimats && mbsRESULT::OK == res; i++)
		res = SeedingCoordinateIsValid(SpeciesIndex, Position[i]);
	if(mbsRESULT::OK != res)
		return res;

	res = PositionArrayToInitialConditionStruct(&c, NumAnimats, Position);
	if(mbsRESULT::OK != res)
		return res;

	res = SimpleMbsResult(m_sce->AddIndividuals(SpeciesIndex, c, NumAnimats));
	delete [] c;
	return res;
}

//----------//
// Deletions
//----------//
void mbs::C3mbs::DeleteSpecies() // Deletes all species from scenario.
{
	m_sce->DeleteSpecies();
}
mbs::mbsRESULT mbs::C3mbs::DeleteSpecies(int Index) // Deletes specific species from scenario.
{
	mbsRESULT res;
	if(mbsRESULT::OK != (res = ValidSpeciesIndex(Index)))
		return res;
	m_sce->DeleteSpecies(Index);
	return mbsRESULT::OK;
}
mbs::mbsRESULT mbs::C3mbs::DeletePods(int SpeciesIndex) // Deletes all pods from specific species
{
	mbsRESULT res;
	if(mbsRESULT::OK != (res = ValidSpeciesIndex(SpeciesIndex)))
		return res;
	m_sce->DeletePods(SpeciesIndex);
	return mbsRESULT::OK;
}
mbs::mbsRESULT mbs::C3mbs::DeletePod(int SpeciesIndex, int PodIndex) // Deletes specific pod from specific species.
{
	mbsRESULT res;
	if(mbsRESULT::OK != (res = ValidPodIndex(SpeciesIndex, PodIndex)))
		return res;
	m_sce->DeletePod(SpeciesIndex, PodIndex);
	return mbsRESULT::OK;
}
mbs::mbsRESULT mbs::C3mbs::DeletePodMember(int SpeciesIndex, int PodIndex, int AnimatIndex) // Deletes specific animat from specific pod in specific species.
{
	mbsRESULT res;
	if(mbsRESULT::OK != (res = ValidPodAnimatIndex(SpeciesIndex, PodIndex, AnimatIndex)))
		return res;
	m_sce->DeletePodMember(SpeciesIndex, PodIndex, AnimatIndex);
	return mbsRESULT::OK;
}
mbs::mbsRESULT mbs::C3mbs::DeleteIndividuals(int SpeciesIndex) // Deletes all individuals from specific species.
{
	mbsRESULT res;
	if(mbsRESULT::OK != (res = ValidSpeciesIndex(SpeciesIndex)))
		return res;
	m_sce->DeleteIndividuals(SpeciesIndex);
	return mbsRESULT::OK;
}

// all zero indexed.
mbs::mbsRESULT mbs::C3mbs::DeleteIndividual(int SpeciesIndex, int IndividualIndex)// Deletes specific individual from specific species.
{
	mbsRESULT res;
	if(mbsRESULT::OK != (res = ValidIndividualIndex(SpeciesIndex, IndividualIndex)))
		return res;

	m_sce->DeleteIndividual(SpeciesIndex, IndividualIndex);
	return mbsRESULT::OK;
}

//-------------//
// Counts
//-------------//
int mbs::C3mbs::GetSpeciesCount()
{
	return m_sce->GetSpeciesCount();
}
int mbs::C3mbs::GetAnimatCount()// Count of all animats in the scenario.
{
	return m_sce->GetAnimatCount();
}
int mbs::C3mbs::GetAnimatCount(int SpeciesIndex)// Count of all animats in a specific species
{
	if(mbs::mbsRESULT::OK != ValidSpeciesIndex(SpeciesIndex))
		return 0;
	return m_sce->GetAnimatCount(SpeciesIndex);
}
int mbs::C3mbs::GetPodCount() // Count of all pods in the scenario.
{
	return m_sce->GetPodCount();
}
int mbs::C3mbs::GetPodCount(int SpeciesIndex)
{
	int podCount;
	if(mbs::mbsRESULT::OK != ValidSpeciesIndex(SpeciesIndex))
		return 0;
	podCount = m_sce->GetPodCount(SpeciesIndex);
	return podCount;
}
int mbs::C3mbs::GetPodMemberCount(int SpeciesIndex, int PodIndex)
{
	if(mbs::mbsRESULT::OK != ValidPodIndex(SpeciesIndex, PodIndex))
		return 0;
	return m_sce->GetPodMemberCount(SpeciesIndex, PodIndex);
}
int mbs::C3mbs::GetIndivdualCount()
{
	return m_sce->GetIndivdualCount();
}
int mbs::C3mbs::GetIndivdualCount(int SpeciesIndex)
{
	if(mbs::mbsRESULT::OK != ValidSpeciesIndex(SpeciesIndex))
		return 0;
	return m_sce->GetIndivdualCount(SpeciesIndex);
}

//-------//
// Access
//-------//
mbs::mbsRESULT mbs::C3mbs::SetPodLeaderType(int SpeciesIndex, int PodIndex, mbsPODLEADERTYPE Type)
{
	PODLEADERTYPE plt;
	mbsRESULT res;
	if(mbsRESULT::OK != (res = ValidPodIndex(SpeciesIndex, PodIndex)))
		return res;

	if(Type == mbsPODLEADERTYPE::ANIMAT)
		plt = ANIMAT;
	else
		plt = CENTROID;

	m_sce->SetPodLeaderType(SpeciesIndex, PodIndex, plt);
	return mbsRESULT::OK;
}
mbs::mbsRESULT mbs::C3mbs::GetPodLeaderType(int SpeciesIndex, int PodIndex, mbsPODLEADERTYPE^ Type)
{
	mbsRESULT res;
	PODLEADERTYPE plt;

	if(mbsRESULT::OK != (res = ValidPodIndex(SpeciesIndex, PodIndex)))
		return res;
	if(Type == nullptr)
		return mbsRESULT::PARAM_HAD_NULLREF_ERROR;

	Type = mbsPODLEADERTYPE::ANIMAT;

	if(CENTROID == (plt = m_sce->GetPodLeaderType(SpeciesIndex, PodIndex)))
		Type = mbsPODLEADERTYPE::CENTROID;

	return mbsRESULT::OK;
}

mbs::mbsRESULT mbs::C3mbs::SetPodLeaderFocalDistance(int SpeciesIndex, int PodIndex, double FocalDistance)
{
	mbsRESULT res;
	if(mbsRESULT::OK != (res = ValidPodIndex(SpeciesIndex, PodIndex)))
		return res;
	m_sce->SetPodLeaderFocalDistance(SpeciesIndex, PodIndex, FocalDistance);
	return mbsRESULT::OK;
}
double mbs::C3mbs::GetPodLeaderFocalDistance(int SpeciesIndex, int PodIndex)
{
	mbsRESULT res;
	if(mbsRESULT::OK != (res = ValidPodIndex(SpeciesIndex, PodIndex)))
		return -1;
	return m_sce->GetPodLeaderFocalDistance(SpeciesIndex, PodIndex);
}

mbs::mbsANIMAT_STATE mbs::C3mbs::RetrieveFirstAnimatStateAtIndex(DWORD Index)
{
	mbsANIMAT_STATE mas = {0};
	ANIMATSTATE as = m_sce->RetrieveFirstAnimatStateAtIndex(Index);

	mas.behavior = as.behState;
	mas.nextBehavior = as.nextBehState;
	mas.nextBehaviorSelected = as.behTransActive;
	mas.lat = as.coord.lat;
	mas.lon = as.coord.lon;
	mas.depth = as.depth;
	mas.bearing = as.setHeading;
	mas.diveRate = as.setDiveRate;
	mas.travelRate = as.setTravelRate;
	mas.aeCmltve = as.acstcExp.cumulativeValue;
	mas.aeMoment = as.acstcExp.actualSrcInstantValue;
	mas.aeRelAngle = as.acstcExp.responseSrcAngle; // change this to acoustic source bearing or something
	mas.aeTimeAvrt = as.acstcExp.timeLapsed;			// aversion cycle (~ 1/s) tally
	mas.bathyDepth = as.bathyDepth;
	//mas.salinity = as.;
	//mas.temperature = as;
	//mas.packedData =
	mas.targetDepth = as.submdl.dive.targetDepth;
	mas.calcDepth = as.submdl.dive.calcDepth;
	mas.xDistance = as.deltaXY.x; // 41
	mas.yDistance = as.deltaXY.y; // 41

	return mas;
}


mbs::mbsRESULT mbs::C3mbs::GetPodFocalCoordinate(int SpeciesIndex, int PodIndex, mbsPosition^ Position)
{
	COORD_DEPTH c;
	mbsRESULT res;
	if(mbsRESULT::OK != (res = ValidPodIndex(SpeciesIndex, PodIndex)))
		return res;
	c = m_sce->GetPodFocalCoordinate(SpeciesIndex, PodIndex);
	res = CoordToPosition(&c, Position);
	return res;
}

// Unitialized pointer to an array of Strings.
String^ mbs::C3mbs::GetBehaviorName(int SpeciesIndex, int Index)
{
	mbsRESULT res;
	BEHAVIOR_NAME *bn;
	int numBehaviors;

	if(mbsRESULT::OK != (res = ValidSpeciesIndex(SpeciesIndex)))
		return gcnew String("") ;
	numBehaviors = GetNumberOfBehaviorsModeledInSpecies(SpeciesIndex);

	if(Index >= numBehaviors)
		return gcnew String("") ;

	if(NULL == (bn = new BEHAVIOR_NAME[numBehaviors]))
		return gcnew String("") ;

	m_sce->GetCopyOfBehaviorNamesModeledInSpecies(SpeciesIndex, bn, numBehaviors);
	return gcnew String(bn[Index].sz) ;
}
int mbs::C3mbs::GetNumberOfBehaviorsModeledInSpecies(int SpeciesIndex)
{
	mbsRESULT res;
	if(mbsRESULT::OK != (res = ValidSpeciesIndex(SpeciesIndex)))
		return -1;
	return (int)m_sce->GetNumberOfBehaviorsModeledInSpecies(SpeciesIndex);
}

//-------------------//
// Species Titles
//-------------------//
mbs::mbsRESULT mbs::C3mbs::SetSpeciesDisplayTitle(int Index, String^ Title)
{
	char sz[SIZE_512];
	mbsRESULT res;
	if(mbsRESULT::OK != (res = ValidSpeciesIndex(Index)))
		return res;
	m_sce->SetSpeciesDisplayTitle(Index, CharBufferFromString(Title, sz, sizeof(sz)));
	return mbsRESULT::OK;
}
String^ mbs::C3mbs::GetSpeciesDisplayTitle(int Index)
{
	char sz[SIZE_512];
	mbsRESULT res;

	if(mbsRESULT::OK != (res = ValidSpeciesIndex(Index)))
		return gcnew String("invalid species index");
	m_sce->GetSpeciesDisplayTitle(Index, sz, sizeof(sz));
	return gcnew String(sz, 0, sizeof(sz));
}


//----------------------------------//
// Simulation Routines and Variables
//----------------------------------//
mbs::mbsRESULT mbs::C3mbs::RunScenarioEntireDuration()
{
	m_sce->EnableEsmeAcousticExposureSetTracking();
	RESLT res = m_sce->RunScenario();  // RunScenario in Scenario.cpp
	if(res == OK)
		res = m_sce->GetErrorStatus();
	return SimpleMbsResult(res);
}
mbs::mbsRESULT mbs::C3mbs::RunScenarioNumIterations(int NumIterations)
{
	m_sce->EnableEsmeAcousticExposureSetTracking();
	return SimpleMbsResult(m_sce->RunScenario(NumIterations));
}

mbs::mbsRESULT mbs::C3mbs::InitializeRun()
{
	RESLT res = m_sce->InitializeRun();
	return SimpleMbsResult(res);
}
mbs::mbsRESULT mbs::C3mbs::StepRun(unsigned int NumIterations)
{
	RESLT res = m_sce->StepRun((int)NumIterations);
	return SimpleMbsResult(res);
}
mbs::mbsRESULT mbs::C3mbs::FinishRun()
{
	RESLT res = m_sce->FinishRun();
	return SimpleMbsResult(res);
}

/* dog comment
 *
 */
void mbs::C3mbs::ClearScenario()
{
	m_sce->ClearScenario();
}

#if 0
mbs::mbsRESULT mbs::C3mbs::ExtractBinaryResultsIntoTextFiles(OPENFILENAME *ofn)
{
	return SimpleMbsResult(m_sce->ExtractBinaryResultsIntoTextFiles(ofn));
}
#endif
mbs::mbsRESULT mbs::C3mbs::ExtractBinaryResultsIntoTextFiles(String^ FileName)
{
	char sz[SIZE_512];
	RESLT res = OK;
	if(FileName == nullptr)
		return mbsRESULT::PARAM_HAD_NULLREF_ERROR;

	CharBufferFromString(FileName, sz, sizeof(sz));
#if 0
	res = m_sce->ExtractBinaryResultsIntoTextFiles(sz);
#endif
	return SimpleMbsResult(res);
}

void mbs::C3mbs::AbortRun()
{
	return m_sce->AbortRun();
}

//--------------------------------------------------------------//
// Scenario Setup And User Interface (GUI or Console)
//--------------------------------------------------------------//
mbs::mbsRESULT mbs::C3mbs::SetScenarioTitle(String^ Title)
{
	char sz[SIZE_512];
	if(Title == nullptr)
		return mbsRESULT::PARAM_HAD_NULLREF_ERROR;
	m_sce->SetScenarioTitle(CharBufferFromString(Title, sz, sizeof(sz)));
	return mbsRESULT::OK;
}
void mbs::C3mbs::SetConfiguration(mbsCONFIG Configuration)
{

	USERPARAMS c = m_sce->GetConfiguration();

	// 1-7 cannot be set directly
	c.output.enabled = Configuration.enabled; // 8
	c.output.headerInf.bathyMap = Configuration.bathyMap; //9
	c.output.headerInf.salinityMap = Configuration.salinityMap; //10
	c.output.headerInf.temperatureMap = Configuration.temperatureMap; //11
	c.output.headerInf.postRunAnalysis = Configuration.postRunAnalysis; // 12
	c.output.headerInf.speInfAndAnimatAsscn = Configuration.speInfAndAnimatAsscn; //13

	// 14 - 17 (ID, time of day, coordinate, and depth) and 28 (packed data) not configurable

	c.output.animat.bearing = Configuration.bearing; //18
	c.output.animat.diveRate = Configuration.diveRate; //19
	c.output.animat.travelRate = Configuration.travelRate; //20
	c.output.animat.aeCmltve = Configuration.aeCmltve; // 21
	c.output.animat.aeMoment = Configuration.aeMoment; // 22
	c.output.animat.aeRelAngle = Configuration.aeRelAngle; //23
	c.output.animat.aeTimeAvrt = Configuration.aeTimeAvrt; // 24
	c.output.animat.bathyDepth = Configuration.bathyDepth; // 25
	c.output.animat.salinity = Configuration.salinity; // 26
	c.output.animat.temperature = Configuration.temperature; // 27
	c.output.animat.targetDepth = Configuration.targetDepth; // 29
	c.output.animat.calcDepth = Configuration.calcDepth; //30 // Saved only if everything is saved.
	c.output.animat.xyDistance = Configuration.xyDistance; //41
	c.output.animat.risk = Configuration.risk; //41

	c.output.AECoordinate = Configuration.AECoordinate; //31
	c.output.outputByTime = Configuration.outputByTime; //32

	// 33-35 cannot be directly set.

	c.durationless = Configuration.durationLess; // 36

	// 37 (acoustic animat active) cannot be set

	if(Configuration.distCalcMethod == mbs::mbsDISTCALC::LAT_LON) //38
		c.distCalcMethod = LAT_LON;
	else
		c.distCalcMethod = PLANAR_GEOMETRY;


	// Set to passed in values.
	c.seed.useCurrentTick = Configuration.seedWithCurrentTick; //39
	c.seed.independentAnimatRandomGen = Configuration.indpdentAnimatRandomGen;
	c.seed.value = Configuration.seedValue; //40

	c.maintainFirstAnimatState = FALSE;
	if(Configuration.maintainFirstAnimatState == true) // (C), tells 3mb to hold the first animat's state for the entire run.
		c.maintainFirstAnimatState = TRUE;

	_ASSERT(m_sce != NULL);
	if(m_sce == NULL)
		return;
	return m_sce->SetConfiguration(c);
}
mbs::mbsCONFIG mbs::C3mbs::GetConfiguration()
{
	mbsCONFIG m;
	USERPARAMS c = m_sce->GetConfiguration();

	if(c.output.enabled == TRUE) // 8
		m.enabled = true;
	if(c.output.headerInf.bathyMap == TRUE) //9
		m.bathyMap = true;
	if(c.output.headerInf.salinityMap == TRUE) //10
		m.salinityMap = true;
	if(c.output.headerInf.temperatureMap == TRUE) //11
		m.temperatureMap = true;
	if(c.output.headerInf.postRunAnalysis == TRUE) //12
		m.postRunAnalysis = true;
	if(c.output.headerInf.speInfAndAnimatAsscn == TRUE) //13
		m.speInfAndAnimatAsscn = true;

	// 14 - 17 (ID, time of day, coordinate, and depth) and 28 (packed data) not configurable

	if(c.output.animat.bearing == TRUE) // 18
		m.bearing = true;
	if(c.output.animat.diveRate == TRUE)// 19
		m.diveRate = true;
	if(c.output.animat.travelRate == TRUE)// 20
		m.travelRate = true;
	if(c.output.animat.aeCmltve == TRUE)// 21
		m.aeCmltve = true;
	if(c.output.animat.aeMoment == TRUE)// 22
		m.aeMoment = true;
	if(c.output.animat.aeRelAngle == TRUE)// 23
		m.aeRelAngle = true;
	if(c.output.animat.aeTimeAvrt == TRUE)// 24
		m.aeTimeAvrt = true;
	if(c.output.animat.bathyDepth == TRUE)// 25
		m.bathyDepth = true;
	if(c.output.animat.salinity == TRUE)// 26
		m.salinity = true;
	if(c.output.animat.temperature == TRUE)// 27
		m.temperature = true;
	// 28, packed data, not configurable.
	if(c.output.animat.targetDepth == TRUE)// 29
		m.targetDepth = true;
	if(c.output.animat.calcDepth == TRUE)// 30
		m.calcDepth = true;
	if(c.output.animat.xyDistance == TRUE) // 41
		m.xyDistance = true;

	if(c.output.animat.risk == TRUE) // 42
		m.risk = true;

	if(c.output.AECoordinate == TRUE) // 31
		m.AECoordinate = true;
	if(c.output.outputByTime == TRUE) // 32
		m.outputByTime = true;

	// 33-35 cannot be directly set.

	if(c.durationless == TRUE) // 36
		m.durationLess = true;

	// 37 (acoustic animat active) cannot be set

	m.distCalcMethod = mbsDISTCALC(c.distCalcMethod); // 38

	if(c.seed.useCurrentTick == TRUE) // 39
		m.seedWithCurrentTick = true;

	if(c.seed.independentAnimatRandomGen == TRUE)
		m.indpdentAnimatRandomGen = true;

	m.seedValue = c.seed.value; //40


	m.maintainFirstAnimatState = FALSE;
	if(c.maintainFirstAnimatState == TRUE)
		m.maintainFirstAnimatState = true;; // (C), tells 3mb to hold the first animat's state for the entire run.

	return m;
}
void mbs::C3mbs::SetDuration(mbsHHMMSS Duration)
{
	HHMMSS d;
	d.hour = Duration.hour;
	d.min = Duration.min;
	d.sec = Duration.sec;
	m_sce->SetDuration(d);
}
void mbs::C3mbs::SetDuration(int Seconds)
{
	m_sce->SetDuration(Seconds);
}
mbs::mbsHHMMSS mbs::C3mbs::GetDuration()
{
	mbsHHMMSS md;
	HHMMSS d = m_sce->GetDuration();

	md.hour = d.hour;
	md.min = d.min;
	md.sec = d.sec;
	return md;
}
int	mbs::C3mbs::GetDurationSeconds()
{
	return m_sce->GetDurationSeconds();
}
void mbs::C3mbs::SetStartTime(mbsHHMMSS StartTime)
{
	HHMMSS d;
	d.hour = StartTime.hour;
	d.min = StartTime.min;
	d.sec = StartTime.sec;
	return m_sce->SetStartTime(d);
}
mbs::mbsHHMMSS mbs::C3mbs::GetStartTime()
{
	mbsHHMMSS md;
	HHMMSS d = m_sce->GetStartTime();
	md.hour = d.hour;
	md.min = d.min;
	md.sec = d.sec;
	return md;
}
BOOL mbs::C3mbs::CalculateRequiredDiskSpace(DWORD *BinStorage, DWORD *TextStorage)
{
	DWORDLONG d;
#pragma message("NEEDS UPDATING: mbs::C3mbs::CalculateRequiredDiskSpace(DWORD *BinStorage, DWORD *TextStorage)")
	return m_sce->CalculateRequiredDiskSpace(&d, &d);
}


//-------------------//
// Environmental Data
//-------------------//
double mbs::C3mbs::GetBathymetryDepth(double lat, double lon)
{
	return m_sce->GetBathymetryDepth(lat, lon).depth;
}

mbs::mbsPosition mbs::C3mbs::NewLatLonFromOldPlusMeters(double Lat, double Lon, double MetersY, double MetersX)
{
	mbs::mbsPosition pos;
	COORD_DEPTH c = m_staticScenario->NewCoordFromOldPlusMeters(Lat, Lon, MetersY, MetersX);

	pos.latitude = c.lat;
	pos.longitude = c.lon;
	pos.depth = 0; // doesn't matter
	return pos;
}
double mbs::C3mbs::MetersBetweenCoordinates(double Lat1, double Lon1, double Lat2, double Lon2)
{
	return m_staticScenario->MetersBetweenCoords(Lat1, Lon1, Lat2, Lon2);
}

mbs::mbsRESULT mbs::C3mbs::LoadBathymetryFromTextFile(String^ FileName)
{
	char sz[SIZE_512];
	if(FileName == nullptr)
		return mbsRESULT::PARAM_HAD_NULLREF_ERROR;
	return SimpleMbsResult(m_sce->LoadBathymetryFromTextFile(CharBufferFromString(FileName, sz, sizeof(sz))));
}

void mbs::C3mbs::ClearBathymetry()
{
	return m_sce->ClearBathymetry();
}
BOOL mbs::C3mbs::BathymetryLoaded()
{
	return m_sce->BathymetryLoaded();
}
String^ mbs::C3mbs::GetBathymetryFileName()
{
	char sz[SIZE_512];
	m_sce->GetBathymetryFileName(sz, sizeof(sz));
	//return gcnew String(sz, 0, sizeof(sz));
	return CharBufferToString(sz);
}

#if 0
void mbs::C3mbs::BathymetryToTextFile(char *FileName)
{
	return m_sce->BathymetryToTextFile(FileName);
}
#endif
ENVMINMAX mbs::C3mbs::GetBathymetryExtremes()
{
	ENVMINMAX em = {0};
	BATHYEXTREMES be = m_sce->GetBathymetryExtremes();

	em.xMin = be.xMin;
	em.xMax = be.xMax;

	em.yMin = be.yMin;
	em.yMax = be.yMax;

	em.v1Min = be.depthMin;
	em.v1Max = be.depthMax;

	return em;
}

mbs::mbsRESULT mbs::C3mbs::LoadTemperatureFromTextFile(String^ FileName)
{
	char sz[SIZE_512];
	if(FileName == nullptr)
		return mbsRESULT::PARAM_HAD_NULLREF_ERROR;
	return SimpleMbsResult(m_sce->LoadTemperatureFromTextFile(CharBufferFromString(FileName, sz, sizeof(sz))));
}

void mbs::C3mbs::ClearTemperature()
{
	return m_sce->ClearTemperature();
}
BOOL mbs::C3mbs::TemperatureLoaded()
{
	return m_sce->TemperatureLoaded();
}
String^ mbs::C3mbs::GetTemperatureFileName()
{
	char sz[SIZE_512];
	m_sce->GetTemperatureFileName(sz, sizeof(sz));
	return gcnew String(sz, 0, sizeof(sz));
}

mbs::mbsRESULT mbs::C3mbs::LoadSalinityFromTextFile(String^ FileName)
{
	char sz[SIZE_512];
	if(FileName == nullptr)
		return mbsRESULT::PARAM_HAD_NULLREF_ERROR;
	return SimpleMbsResult(m_sce->LoadSalinityFromTextFile(CharBufferFromString(FileName, sz, sizeof(sz))));
}

void mbs::C3mbs::ClearSalinity()
{
	return m_sce->ClearSalinity();
}
BOOL mbs::C3mbs::SalinityLoaded()
{
	return m_sce->SalinityLoaded();
}
String^ mbs::C3mbs::GetSalinityFileName()
{
	char sz[SIZE_512];
	m_sce->GetSalinityFileName(sz, sizeof(sz));
	return gcnew String(sz, 0, sizeof(sz));
}

//------------------------------------------------------------//
// File Functions, Functions Related to File IO, and Variables
//------------------------------------------------------------//
mbs::mbsRESULT mbs::C3mbs::LoadFromBinFile(String^ FileName)
{
	char sz[SIZE_512];
	if(FileName == nullptr)
		return mbsRESULT::PARAM_HAD_NULLREF_ERROR;

	return SimpleMbsResult(m_sce->LoadFromBinFile(CharBufferFromString(FileName, sz, sizeof(sz))));
}
mbs::mbsRESULT mbs::C3mbs::SaveToBinFile(String^ FileName)
{
	char sz[SIZE_512];
	if(FileName == nullptr)
		return mbsRESULT::PARAM_HAD_NULLREF_ERROR;

	return SimpleMbsResult(m_sce->SaveToBinFile(CharBufferFromString(FileName, sz, sizeof(sz))));
}

//------------------//
// Accesor Functions
//------------------//

//---------------------//
// Scenario Description
//---------------------//
mbs::mbsRESULT mbs::C3mbs::RunParamsToTextFile(FILE *fd)
{
	return SimpleMbsResult(m_sce->RunParamsToTextFile(fd));
}
mbs::mbsRESULT mbs::C3mbs::GetErrorStatus()
{
	return SimpleMbsResult(m_sce->GetErrorStatus());
}



//-------------------//
// Binary File Reader
//-------------------//
/*
mbs::mbsRESULT mbs::C3mbBathy::LoadFromTextFile(String^ FileName)
{
	char sz[SIZE_512];
	if(FileName == nullptr)
		return mbsRESULT::PARAM_HAD_NULLREF_ERROR;
	mbsInstance.CharBufferFromString(FileName, sz, SIZE_512);
	return mbsInstance.SimpleMbsResult(m_bathy->LoadFromTextFile(sz, false));
}
double mbs::C3mbBathy::GetDepthAtCoordinate(double Lat, double Lon)
{
	return GetValueAtCoordinate(Lat, Lon).depth;
}

mbs::mbsBATHYEXTREMES mbs::C3mbBathy::GetExtremes()
{
	mbsBATHYEXTREMES mbe;
	BATHYEXTREMES be = m_bathy->GetExtremes();
	mbe.depthMax = be.depthMax;
	mbe.depthMin = be.depthMin;
	mbe.slopeHeadingMax = be.slopeHeadingMax;
	mbe.slopeHeadingMin = be.slopeHeadingMin;
	mbe.slopeMax = be.slopeMax;
	mbe.slopeMin = be.slopeMin;
	mbe.slopeMax = be.slopeMax;
	mbe.slopeHeadingMin = be.slopeHeadingMin;
	mbe.slopeHeadingMax = be.slopeHeadingMax;

	return mbe;
}

mbs::mbsBATHYVALUE mbs::C3mbBathy::GetValueAtCoordinate(double Lat, double Lon)
{
	mbs::mbsBATHYVALUE mbBv;
	BATHYVALUE bv = m_bathy->GetValueAtCoordinate(Lat, Lon);
	mbBv.depth = bv.depth;
	mbBv.slope = bv.slope;
	mbBv.slopeHeading = bv.slopeHeading;
	return mbBv;
}
*/

mbs::mbsRESULT mbs::C3mbs::BinFileOpen(String^ FileName)
{
	char sz[SIZE_512];
	if(FileName == nullptr)
		return mbsRESULT::PARAM_HAD_NULLREF_ERROR;
	return SimpleMbsResult(m_binOutReader->OpenOutputFile(CharBufferFromString(FileName, sz, SIZE_512)));
}

void mbs::C3mbs::BinFileClose()
{
	m_binOutReader->CloseOutputFile();
}

bool mbs::C3mbs::BinFileIsOpen()
{
	if(m_binOutReader->IsOpen() == TRUE)
		return true;
	return false;
}


mbs::mbsSCENARIO_PARAMS mbs::C3mbs::GetSceParams()
{
	mbsSCENARIO_PARAMS m;
	_fSCENARIOPARAMS p = m_binOutReader->GetSceParams();

	m.libVerSuper = p.libVerSuper;
	m.libVerSub = p.libVerSub;
	m.numberOfIterations = p.duration;
	m.numSpecies = p.numSpecies;
	m.startTime = p.startTime;
	m.totalNumAnimats = p.totalNumAnimats;
	return m;
}

unsigned int mbs::C3mbs::LibraryVersionSuper()
{
	_ASSERT(m_sce != NULL);
	if(m_sce == NULL)
		return 0;

	return m_sce->GetScenarioParamsCopy().libVerSuper;
}

unsigned int mbs::C3mbs::LibraryVersionSub()
{
	_ASSERT(m_sce != NULL);
	if(m_sce == NULL)
		return 0;

	return m_sce->GetScenarioParamsCopy().libVerSub;
}

unsigned int mbs::C3mbs::BinFileLibraryVersionSuper()
{
	return m_binOutReader->GetLibraryVersionSuper();
}

unsigned int mbs::C3mbs::BinFileLibraryVersionSub()
{
	return m_binOutReader->GetLibraryVersionSub();
}
int mbs::C3mbs::BinFileSpeciesCount()
{
	return m_binOutReader->GetNumSpecies();
}
int mbs::C3mbs::BinFileAnimatCount()
{
	return m_binOutReader->GetTotalAnimats();
}

int mbs::C3mbs::BinFileSpeciesStateCount()
{
	return m_binOutReader->GetNumStates();
}
int mbs::C3mbs::BinFileStartTime()
{
	return m_binOutReader->GetStartTime();
}

void mbs::C3mbs::AnimatStateToMbsAnimatState(ANIMATSTATE_FILEOUT *s, int sLength, array<mbs::mbsANIMAT_STATE>^ m)
{
	//mbs::mbsANIMAT_STATE *n; // allowed.
	//mbs::mbsANIMAT_STATE^ k; // allowed.
	//array<mbs::mbsANIMAT_STATE>^ z; // allowed

	/* almost worked
	mbs::mbsANIMAT_STATE **nn; // allowed.
	nn = new mbs::mbsANIMAT_STATE*[5];
	nn[3] = new mbs::mbsANIMAT_STATE[4];
	*/

//	mbs::mbsANIMAT_STATE^ nn[]; // allowed.
	//nn = new mbs::mbsANIMAT_STATE*[5];
	//nn[3] = new mbs::mbsANIMAT_STATE[4];


	//array<mbs::mbsANIMAT_STATE>* v; // not allowed
	//mbs::mbsANIMAT_STATE[]^ v; // not allowed.
	//mbs::mbsANIMAT_STATE[] v; // not allowed.
	//mbs::mbsANIMAT_STATE^[] v; // not allowed.
	//array<mbs::mbsANIMAT_STATE> z; // not allowed

	//n = new mbs::mbsANIMAT_STATE; // allowed
	//n->depth = 5; // allowed
	//n = new mbs::mbsANIMAT_STATE[5]; // not allowed, a native array cannot contain this managed type.
	//k = new mbs::mbsANIMAT_STATE;
	//k = gcnew mbs::mbsANIMAT_STATE; // allowed
	//k->depth = 5; // allowed
	//k = gcnew mbs::mbsANIMAT_STATE[2]; // not allowed  a native array cannot contain this managed type.
	
	//m[2].depth = 5; // allowed

	//z[0].depth = 5; // allowed w/o initialization.

	//z = gcnew array<mbs::mbsANIMAT_STATE>
	//z = new array<mbs::mbsANIMAT_STATE>[2];// not allowed, cannot convert from 'cli::array<Type> *' to 'cli::array<Type> ^
	//z = gcnew array<mbs::mbsANIMAT_STATE>[2];// not allowed, a native array cannot contain this managed type
	//z = gcnew array<mbs::mbsANIMAT_STATE>; // not allowed, managed array creation must have array size or array initializer
	//z = array<mbs::mbsANIMAT_STATE>^; // not allowed
	//z = array<mbs::mbsANIMAT_STATE>;; // not allowed
	//z = gcnew mbs::mbsANIMAT_STATE[5];// not allowed, a native array cannot contain this managed type
	//z = gcnew mbs::mbsANIMAT_STATE;// not allowed, cannot convert from 'mbs::mbsANIMAT_STATE ^' to 'cli::array<Type> ^
	//z = new mbs::mbsANIMAT_STATE; // not allowed
	//z = new mbs::mbsANIMAT_STATE[5]; // not allowed, a native array cannot contain this managed type

	for(int i=0; i<m->Length && i<sLength; i++)
	{
		m[i] = AnimatStateToMbsAnimatState(&s[i]);
	}
}


mbs::mbsANIMAT_STATE mbs::C3mbs::AnimatStateToMbsAnimatState(ANIMATSTATE_FILEOUT *s)
{
	mbsANIMAT_STATE m;
	m.lat = s->lat;
	m.lon = s->lon;
	m.depth = s->depth;
	m.bearing = s->bearing;
	m.diveRate = s->diveRate;
	m.travelRate = s->travelRate;
	m.aeCmltve = s->aeCmltve;
	m.aeMoment = s->aeMoment;
	m.aeRelAngle = s->aeRelAngle;
	m.aeTimeAvrt = s->aeTimeAvrt;
	m.bathyDepth = s->bathyDepth;
	m.salinity = s->salinity;
	m.temperature = s->temperature;
	m.packedData = s->packedData;
	m.targetDepth = s->targetDepth;
	m.calcDepth = s->calcDepth;
	m.xDistance = s->xDistance;
	m.yDistance = s->yDistance;
	return m;
}

mbs::mbsANIMAT_STATE mbs::C3mbs::BinFileGetAnimatState(int AnimatIndex, int StateIndex, mbs::mbsRESULT^ pResult)
{
	ANIMATSTATE_FILEOUT a;
	mbsANIMAT_STATE ret = {0};
	RESLT r;

	*pResult = mbsRESULT::OK;

	if(AnimatIndex >= m_binOutReader->GetTotalAnimats() || AnimatIndex < 0)
	{
		*pResult = mbsRESULT::INVALID_ANIMAT_INDEX_ERROR;
		return ret;
	}
	if(StateIndex >= m_binOutReader->GetNumStates() || StateIndex < 0)
	{
		*pResult = mbsRESULT::INVALID_ITERATION_INDEX;
		return ret;
	}


	if(OK != (r = m_binOutReader->GetAnimatState(AnimatIndex, StateIndex, &a)))
	{
		if(pResult != nullptr)
			*pResult = mbsRESULT(r);
		return ret;
	}
	ret = AnimatStateToMbsAnimatState(&a);
	return ret;
}
mbs::mbsRESULT mbs::C3mbs::BinFileGetAnimatStates(int AnimatIndex, array<mbs::mbsANIMAT_STATE>^ pAnimatStateBuffer)
{
	int i;
	RESLT r;
	ANIMATSTATE_FILEOUT *s;
	int arraySize;

	if(pAnimatStateBuffer == nullptr)
		return mbsRESULT::PARAM_HAD_NULLREF_ERROR;
	if(AnimatIndex >= m_binOutReader->GetTotalAnimats() || AnimatIndex < 0)
		return mbsRESULT::INVALID_ANIMAT_INDEX_ERROR;

	arraySize = pAnimatStateBuffer->Length;
	s = new ANIMATSTATE_FILEOUT[arraySize];
	if(NULL == s)
		return mbsRESULT::MEMALLOC_ERROR;

	if(OK != (r = m_binOutReader->GetAnimatStates(AnimatIndex, s, pAnimatStateBuffer->Length)))
	{
		delete [] s;
		return mbsRESULT(r);
	}

	for(i=0; i<pAnimatStateBuffer->Length; i++)
		pAnimatStateBuffer[i] = AnimatStateToMbsAnimatState(&s[i]);

	delete [] s;

	return mbsRESULT::OK;
}




mbs::C3mbsWrapperSpeciesModel::C3mbsWrapperSpeciesModel(){}
void mbs::C3mbsWrapperSpeciesModel::VoidFnc(){}


// Copying a matrix from C++ code to C# code (loading into C# GUI)
mbs::mbsMATRIX mbs::C3mbsWrapperSpeciesModel::CopyMatrix(const MATRIX *Src)
{
	int i, j;
	mbsMATRIX Des;
	Des.a = gcnew array< mbsARRAY >(Src->rowCnt);
	Des.colCnt = Src->colCnt;
	Des.rowCnt = Src->rowCnt;

	for(i=0; i<Src->rowCnt; i++)
	{
		Des.a[i].a = gcnew array< mbsELEMENT >(Src->colCnt);
		for(j=0; j<Src->colCnt; j++)
			Des.a[i].a[j].a = Src->p.ppa[i][j];
	}
	return Des;
}

// Copying an array/vector from C++ code to C# code (loading into C# GUI)
mbs::mbsARRAY mbs::C3mbsWrapperSpeciesModel::CopyMatrix(const ARRAY *Src)
{
	int i;
	mbsARRAY Des;
	Des.a = gcnew array< mbsELEMENT >(Src->colCnt);
	Des.colCnt = Src->colCnt;
	Des.rowCnt = 1;

	for(i=0; i<Src->colCnt; i++)
		Des.a[i].a = Src->p.pa[i];
	return Des;
}

// Copying an element from C++ code to C# code (loading into C# GUI)
mbs::mbsELEMENT mbs::C3mbsWrapperSpeciesModel::CopyMatrix(const ELEMENT *Src)
{
	mbsELEMENT Des;
	Des.colCnt = Des.rowCnt = 1;
	Des.a = Src->a;
	return Des;
}

// Copying a matrix from C# code to C++ code (saving to C++ libraries)
void mbs::C3mbsWrapperSpeciesModel::CopyMatrix(MATRIX *Des,  const mbsMATRIX ^Src)
{
	int i, j;
	CSpeciesModel speMdl;// = new CSpeciesModel();
	if(FALSE == speMdl.AllocateMatrix(Des, Src->rowCnt, Src->colCnt))
		return;

	for(i=0; i<Src->rowCnt; i++)
		for(j=0; j<Src->colCnt; j++)
			Des->p.ppa[i][j] = Src->a[i].a[j].a; //Src->a[i][j];
}

// Copying an array/vector from C# code to C++ code (saving to C++ libraries)
void mbs::C3mbsWrapperSpeciesModel::CopyMatrix(ARRAY *Des, const mbsARRAY ^Src)
{
	int i;
	CSpeciesModel speMdl;
//	mbsELEMENT e;
	if(FALSE == speMdl.AllocateMatrix(Des, Src->rowCnt, Src->colCnt))
		return;

	for(i=0; i<Src->colCnt; i++)
		Des->p.pa[i] = Src->a[i].a;
}

// Copying an element from C# code to C++ code (saving to C++ libraries)
void mbs::C3mbsWrapperSpeciesModel::CopyMatrix(ELEMENT *Des,  const mbsELEMENT ^Src)
{
	CSpeciesModel speMdl;
	if(FALSE == speMdl.AllocateMatrix(Des, Src->rowCnt, Src->colCnt))
		return;
	Des->a = Src->a;
}

mbs::mbsSNGLBEHTRANSTRUCT mbs::C3mbsWrapperSpeciesModel::RunBehaviorTransitionControlTest(mbsMATRIX ^M, int BehaviorIndex, int NumTrials)
{
	int i,j;
	MATRIX m = {0};
	SNGLBEHTRANSTRUCT behTrans;
	mbsSNGLBEHTRANSTRUCT mbsBehTrans;
	CSpeciesModel speMdl;

//	CSpeciesModel::AllocateMatrix(&m, M->rowCnt, M->colCnt);
	CopyMatrix(&m, M);
	speMdl.RunBehaviorTransitionControlTest(&behTrans, &m, BehaviorIndex, NumTrials);
	speMdl.FreeMatrix(&m);
	
	mbsBehTrans.timePeriodArray = gcnew array< mbsCNTBINARRAY >(M->rowCnt);

	for(i=0; i<M->rowCnt; i++)
	{
		mbsBehTrans.timePeriodArray[i].trialArray = gcnew array <mbsCNTBIN>(NumTrials);
		mbsBehTrans.timePeriodArray[i].start = M->a[i].a[0].a;
		mbsBehTrans.timePeriodArray[i].end = M->a[i].a[1].a;

		for(j=0; j<NumTrials; j++)
		{
			mbsBehTrans.timePeriodArray[i].trialArray[j].trans = behTrans.cnt[i][j].trans;
			mbsBehTrans.timePeriodArray[i].trialArray[j].sec = behTrans.cnt[i][j].sec;
		}
	}


	for(i=0; i<M->rowCnt; i++)
		delete [] behTrans.cnt[i];

	delete [] behTrans.cnt;

	return mbsBehTrans;
}


mbs::mbsSNGLBEHTRANSTRUCT mbs::C3mbsWrapperSpeciesModel::InitialBehaviorControlTest(mbsMATRIX ^M, int NumTrials)
{
	int i,j;
	MATRIX m = {0};
	SNGLBEHTRANSTRUCT behTrans;
	mbsSNGLBEHTRANSTRUCT mbsBehTrans;
	CSpeciesModel speMdl;

//	CSpeciesModel::AllocateMatrix(&m, M->rowCnt, M->colCnt);
	CopyMatrix(&m, M);
	speMdl.InitialBehaviorControlTest(&behTrans, &m, NumTrials);
	speMdl.FreeMatrix(&m);

	mbsBehTrans.timePeriodArray = gcnew array< mbsCNTBINARRAY >(M->rowCnt);

	for(i=0; i<M->rowCnt; i++)
	{
		mbsBehTrans.timePeriodArray[i].trialArray = gcnew array <mbsCNTBIN>(NumTrials);
		mbsBehTrans.timePeriodArray[i].start = M->a[i].a[0].a;
		mbsBehTrans.timePeriodArray[i].end = M->a[i].a[1].a;

		for(j=0; j<NumTrials; j++)
		{
			mbsBehTrans.timePeriodArray[i].trialArray[j].trans = behTrans.cnt[i][j].trans;
			mbsBehTrans.timePeriodArray[i].trialArray[j].sec = behTrans.cnt[i][j].sec;
		}
	}


	for(i=0; i<M->rowCnt; i++)
		delete [] behTrans.cnt[i];

	delete [] behTrans.cnt;

	return mbsBehTrans;
}


int mbs::C3mbsWrapperSpeciesModel::GetShortDescriptionAllowedLength(){return SPECIES_DESCRPTN_MAX_LEN;}
int mbs::C3mbsWrapperSpeciesModel::GetLongCommentAllowedLength(){return SPECIES_COMMENT_MAX_LEN; }
int mbs::C3mbsWrapperSpeciesModel::GetSpeciesGroupCount(){ return NUM_SPEGROUPS_INUSE;}
int mbs::C3mbsWrapperSpeciesModel::GetSpeciesNameListCount(){ return SPECIESNAMELENGTH;}

unsigned int mbs::C3mbsWrapperSpeciesModel::GetSpeSuperVersion(){return MMMBLIB_SPECIES_VERSION_SUPER;};
unsigned int mbs::C3mbsWrapperSpeciesModel::GetSpeSubVersion(){return MMMBLIB_SPECIES_VERSION_SUB;};


String ^mbs::C3mbsWrapperSpeciesModel::GetSpeciesGroupString(int SpeciesGroupType)
{
	return mbsInstance.CharBufferToString((char *)SZSPECIESGROUPNAMEBUFFER[SpeciesGroupType]);
}

String ^mbs::C3mbsWrapperSpeciesModel::GetSpeciesLatinNameString(int SpeciesNameIndex)
{
	return mbsInstance.CharBufferToString((char *)SZSPECIESNAMEBUFFER_LATIN[SpeciesNameIndex]);
}

String ^mbs::C3mbsWrapperSpeciesModel::GetSpeciesEnglishNameString(int SpeciesNameIndex)
{
	return mbsInstance.CharBufferToString((char *)SZSPECIESNAMEBUFFER_ENGLISH[SpeciesNameIndex]);
}



void mbs::C3mbsWrapperSpeciesModel::SpeciesModelToC3mbsWrapperSpeModel(CSpeciesModel *CSpeMdl, mbsSPECIESMODEL ^SpeMdl)
{
	int i;
	int j;
	NRMLBEHMDL *behSrc;

	//----------------------//
	// Acoustic Aversion
	// 39 parameters
	//----------------------//
	SpeMdl->acousticAversion.ascentRateAffected = CSpeMdl->m_speciesModel.acousticAversion.ascentRateAffected;
	SpeMdl->acousticAversion.ascentRate.coeff = CSpeMdl->m_speciesModel.acousticAversion.ascentRate.coeff;
	SpeMdl->acousticAversion.ascentRate.mean = CSpeMdl->m_speciesModel.acousticAversion.ascentRate.mean;
	SpeMdl->acousticAversion.ascentRate.std = CSpeMdl->m_speciesModel.acousticAversion.ascentRate.std;
	SpeMdl->acousticAversion.descentRateAffected = CSpeMdl->m_speciesModel.acousticAversion.descentRateAffected;
	SpeMdl->acousticAversion.descentRate.coeff = CSpeMdl->m_speciesModel.acousticAversion.descentRate.coeff;
	SpeMdl->acousticAversion.descentRate.mean = CSpeMdl->m_speciesModel.acousticAversion.descentRate.mean;
	SpeMdl->acousticAversion.descentRate.std = CSpeMdl->m_speciesModel.acousticAversion.descentRate.std;
	SpeMdl->acousticAversion.beaches = CSpeMdl->m_speciesModel.acousticAversion.beaches;
	SpeMdl->acousticAversion.beachingDepth = CSpeMdl->m_speciesModel.acousticAversion.beachingDepth;
	SpeMdl->acousticAversion.depth.coeff = CSpeMdl->m_speciesModel.acousticAversion.depth.coeff;
	SpeMdl->acousticAversion.depth.mean = CSpeMdl->m_speciesModel.acousticAversion.depth.mean;
	SpeMdl->acousticAversion.depth.std = CSpeMdl->m_speciesModel.acousticAversion.depth.std;
	SpeMdl->acousticAversion.depthAffected = CSpeMdl->m_speciesModel.acousticAversion.depthAffected;
	SpeMdl->acousticAversion.flatBottomDiveAffected = CSpeMdl->m_speciesModel.acousticAversion.flatBottomDiveAffected;
	SpeMdl->acousticAversion.flatBottomDives = CSpeMdl->m_speciesModel.acousticAversion.flatBottomDives;
	SpeMdl->acousticAversion.podBreaksUp = CSpeMdl->m_speciesModel.acousticAversion.podBreaksUp;
	SpeMdl->acousticAversion.reversal.count.coeff = CSpeMdl->m_speciesModel.acousticAversion.reversal.count.coeff;
	SpeMdl->acousticAversion.reversal.count.mean = CSpeMdl->m_speciesModel.acousticAversion.reversal.count.mean;
	SpeMdl->acousticAversion.reversal.count.std = CSpeMdl->m_speciesModel.acousticAversion.reversal.count.std;
	SpeMdl->acousticAversion.reversal.probOfReversal = CSpeMdl->m_speciesModel.acousticAversion.reversal.probOfReversal;
	SpeMdl->acousticAversion.reversal.time.coeff = CSpeMdl->m_speciesModel.acousticAversion.reversal.time.coeff;
	SpeMdl->acousticAversion.reversal.time.mean = CSpeMdl->m_speciesModel.acousticAversion.reversal.time.mean;
	SpeMdl->acousticAversion.reversal.time.std = CSpeMdl->m_speciesModel.acousticAversion.reversal.time.std;
	SpeMdl->acousticAversion.reversalAffected = CSpeMdl->m_speciesModel.acousticAversion.reversalAffected;
	SpeMdl->acousticAversion.surfaceInterval.coeff = CSpeMdl->m_speciesModel.acousticAversion.surfaceInterval.coeff;
	SpeMdl->acousticAversion.surfaceInterval.mean = CSpeMdl->m_speciesModel.acousticAversion.surfaceInterval.mean;
	SpeMdl->acousticAversion.surfaceInterval.std = CSpeMdl->m_speciesModel.acousticAversion.surfaceInterval.std;
	SpeMdl->acousticAversion.surfaceIntervalAffected = CSpeMdl->m_speciesModel.acousticAversion.surfaceIntervalAffected;
	SpeMdl->acousticAversion.travel.arcStep = CSpeMdl->m_speciesModel.acousticAversion.travel.arcStep;
	SpeMdl->acousticAversion.travel.bias = CSpeMdl->m_speciesModel.acousticAversion.travel.bias;
	SpeMdl->acousticAversion.travel.directionOfBias = CSpeMdl->m_speciesModel.acousticAversion.travel.directionOfBias;
	SpeMdl->acousticAversion.travel.perturbation = CSpeMdl->m_speciesModel.acousticAversion.travel.perturbation;
	SpeMdl->acousticAversion.travel.termCoeff = CSpeMdl->m_speciesModel.acousticAversion.travel.termCoeff;
	SpeMdl->acousticAversion.travelDirectionAffected = CSpeMdl->m_speciesModel.acousticAversion.travelDirectionAffected;
	SpeMdl->acousticAversion.travelRate.coeff = CSpeMdl->m_speciesModel.acousticAversion.travelRate.coeff;
	SpeMdl->acousticAversion.travelRate.mean = CSpeMdl->m_speciesModel.acousticAversion.travelRate.mean;
	SpeMdl->acousticAversion.travelRate.std = CSpeMdl->m_speciesModel.acousticAversion.travelRate.std;
	SpeMdl->acousticAversion.travelRateAffected = CSpeMdl->m_speciesModel.acousticAversion.travelRateAffected;


	//----------------------------------------------------------------------------------//
	// Species Description
	// 7 parameters: 3mbs lib version, species version, species group, short description,
	//				 long comment, shore following depth, and number of behaviors
	//----------------------------------------------------------------------------------//
	SpeMdl->mbsLibVerSuper = CSpeMdl->m_speciesModel.description.mbsVerSuper;
	SpeMdl->mbsLibVerSub = CSpeMdl->m_speciesModel.description.mbsVerSub;
	SpeMdl->speVerSuper = CSpeMdl->m_speciesModel.description.speVerSuper;
	SpeMdl->speVerSub = CSpeMdl->m_speciesModel.description.speVerSub;
	SpeMdl->speciesGroup = (int)CSpeMdl->m_speciesModel.description.group;
	SpeMdl->szSpeciesShrtDscrptn = mbsInstance.CharBufferToString(CSpeMdl->m_speciesModel.description.speciesShrtDscrptn);
	SpeMdl->szSpeciesLongComment = mbsInstance.CharBufferToString(CSpeMdl->m_speciesModel.description.speciesComment);
	SpeMdl->shoreFollowDepth = CSpeMdl->m_speciesModel.description.shoreFollowDepth;
	SpeMdl->minSeedDepth = CSpeMdl->m_speciesModel.description.minSeedingDepth;
	SpeMdl->behaviorCount = CSpeMdl->m_speciesModel.description.numBehaviors;

	SpeMdl->deepWaterSeedingDepth = CSpeMdl->m_speciesModel.description.deepWaterSeedingDepth;
	SpeMdl->deepWaterSeedingDepthEnabled = CSpeMdl->m_speciesModel.description.deepWaterSeedingDepthEnabled;

	SpeMdl->speciesName = CSpeMdl->m_speciesModel.description.name;

	SpeMdl->year = CSpeMdl->m_speciesModel.description.year;
	SpeMdl->month = CSpeMdl->m_speciesModel.description.month;
	SpeMdl->day = CSpeMdl->m_speciesModel.description.day;
	SpeMdl->hour = CSpeMdl->m_speciesModel.description.hour;
	SpeMdl->min = CSpeMdl->m_speciesModel.description.min;
	SpeMdl->sec = CSpeMdl->m_speciesModel.description.sec;
	SpeMdl->id = CSpeMdl->m_speciesModel.description.id;


	//---------------------//
	// Initial Behavior
	//---------------------//
	//SpeMdl->initialBehavior = CopyMatrix(&CSpeMdl->m_speciesModel.initialBehavior);
	SpeMdl->initBehSpanCnt = CSpeMdl->m_speciesModel.initBehSpanCnt;
	SpeMdl->initialBehavior = gcnew array<mbsBEHTRAN>(SpeMdl->initBehSpanCnt);
	for(i=0; i<SpeMdl->initBehSpanCnt; i++)
	{
		SpeMdl->initialBehavior[i].depthSpan.shallow = CSpeMdl->m_speciesModel.initialBehavior.arr[i].depthSpan.shallow;
		SpeMdl->initialBehavior[i].depthSpan.deep = CSpeMdl->m_speciesModel.initialBehavior.arr[i].depthSpan.deep;
		SpeMdl->initialBehavior[i].m = CopyMatrix(&CSpeMdl->m_speciesModel.initialBehavior.arr[i].m);
	}


	//------------------------//
	// Behavior Allocation
	//------------------------//
	SpeMdl->behaviorCount = CSpeMdl->m_speciesModel.description.numBehaviors;
	SpeMdl->behavior = gcnew array<mbsNRMLBEHMDL>(CSpeMdl->m_speciesModel.description.numBehaviors);

	for(i=0; i<SpeMdl->behaviorCount; i++)
	{
		behSrc = &CSpeMdl->m_speciesModel.p.behavior[i];

		//------------------//
		// Behavior Name
		//------------------//
		SpeMdl->behavior[i].szName = mbsInstance.CharBufferToString(behSrc->szName);

		switch(behSrc->nrmlBehTermType)
		{
		case T50_K_TERM:
			SpeMdl->behavior[i].behTransTermFormula = mbsBehTermFormula::T50_K;
			break;
		case GAUSSIAN_TERM:
			SpeMdl->behavior[i].behTransTermFormula = mbsBehTermFormula::GAUSSIAN;
			break;
		}

		//-------------------------------------//
		// Environmental Attractor Priority
		//-------------------------------------//
		SpeMdl->behavior[i].depthHasPriorityOverTemp = behSrc->depthHasPriorityOverTemp;

		//----------------------------------------//
		// Depth environmental attractor
		// 8 parameters, including 2 vector models
		//----------------------------------------//
		SpeMdl->behavior[i].depthEnvAtt.shelfIsEnabled = behSrc->depthEnvAtt.shelfIsEnabled;
		SpeMdl->behavior[i].depthEnvAtt.shelfDepth = behSrc->depthEnvAtt.shelfDepth;
		SpeMdl->behavior[i].depthEnvAtt.shelfSlope = behSrc->depthEnvAtt.shelfSlope;

		SpeMdl->behavior[i].depthEnvAtt.basinIsEnabled = behSrc->depthEnvAtt.basinIsEnabled;
		SpeMdl->behavior[i].depthEnvAtt.basinDepth = behSrc->depthEnvAtt.basinDepth;
		SpeMdl->behavior[i].depthEnvAtt.basinSlope = behSrc->depthEnvAtt.basinSlope;

		SpeMdl->behavior[i].depthEnvAtt.slopeIsEnabled = behSrc->depthEnvAtt.slopeIsEnabled;
		SpeMdl->behavior[i].depthEnvAtt.slopeDepth = behSrc->depthEnvAtt.slopeDepth;
		SpeMdl->behavior[i].depthEnvAtt.slopeSlope = behSrc->depthEnvAtt.slopeSlope;

		SpeMdl->behavior[i].depthEnvAttBehTrans.meanTimeInBeh = behSrc->depthEnvAttBehTrans.meanTimeInBeh;
		SpeMdl->behavior[i].depthEnvAttBehTrans.slopeCoefficient = behSrc->depthEnvAttBehTrans.slopeCoefficient;

		SpeMdl->behavior[i].depthEnvAttBehTrans.behavior = CopyMatrix(&behSrc->depthEnvAttBehTrans.behavior);
		SpeMdl->behavior[i].depthEnvAttBehTrans.terminate = CopyMatrix(&behSrc->depthEnvAttBehTrans.terminate);


		//-----------------------------------------//
		// Temperature Environmental Attractor
		// 8 parameters, including 2 vector models
		//-----------------------------------------//
		SpeMdl->behavior[i].tempEnvAtt.delta = behSrc->tempEnvAtt.delta;
		SpeMdl->behavior[i].tempEnvAtt.deltaIsEnabled = behSrc->tempEnvAtt.deltaIsEnabled;
		SpeMdl->behavior[i].tempEnvAtt.max = behSrc->tempEnvAtt.max;
		SpeMdl->behavior[i].tempEnvAtt.maxIsEnabled = behSrc->tempEnvAtt.maxIsEnabled;
		SpeMdl->behavior[i].tempEnvAtt.min = behSrc->tempEnvAtt.min;
		SpeMdl->behavior[i].tempEnvAtt.minIsEnabled = behSrc->tempEnvAtt.minIsEnabled;
		SpeMdl->behavior[i].tempEnvAttBehTrans.meanTimeInBeh = behSrc->tempEnvAttBehTrans.meanTimeInBeh;
		SpeMdl->behavior[i].tempEnvAttBehTrans.slopeCoefficient = behSrc->tempEnvAttBehTrans.slopeCoefficient;

		SpeMdl->behavior[i].tempEnvAttBehTrans.behavior = CopyMatrix(&behSrc->tempEnvAttBehTrans.behavior);
		SpeMdl->behavior[i].tempEnvAttBehTrans.terminate = CopyMatrix(&behSrc->tempEnvAttBehTrans.terminate);

		//-----------------------------------------//
		// Travel Direction
		// 12 parameters, including 3 vector models
		//-----------------------------------------//
		SpeMdl->behavior[i].travelDirection.crRndWalk.perturbation = behSrc->travelDirection.crRndWalk.perturbation;
		SpeMdl->behavior[i].travelDirection.crRndWalk.termCoeff = behSrc->travelDirection.crRndWalk.termCoeff;
		SpeMdl->behavior[i].travelDirection.crRndWalkDb.arcStep = behSrc->travelDirection.crRndWalkDb.arcStep;
		SpeMdl->behavior[i].travelDirection.crRndWalkDb.bias = behSrc->travelDirection.crRndWalkDb.bias;
		SpeMdl->behavior[i].travelDirection.crRndWalkDb.directionOfBias = behSrc->travelDirection.crRndWalkDb.directionOfBias;
		SpeMdl->behavior[i].travelDirection.crRndWalkDb.perturbation = behSrc->travelDirection.crRndWalkDb.perturbation;
		SpeMdl->behavior[i].travelDirection.crRndWalkDb.termCoeff = behSrc->travelDirection.crRndWalkDb.termCoeff;
		SpeMdl->behavior[i].travelDirection.modelType = (mbsDIRECTIONAL_MODEL_TYPE)behSrc->travelDirection.modelType;
		SpeMdl->behavior[i].travelDirection.rndWalk.termCoeff = behSrc->travelDirection.rndWalk.termCoeff;
		SpeMdl->behavior[i].travelDirection.vm.direction = CopyMatrix(&behSrc->travelDirection.vm.direction);
		SpeMdl->behavior[i].travelDirection.vm.directionalBias = CopyMatrix(&behSrc->travelDirection.vm.directionalBias);
		SpeMdl->behavior[i].travelDirection.vm.terminate = CopyMatrix(&behSrc->travelDirection.vm.terminate);

		//-----------------------------------------//
		// Travel Rate
		// 10 parameters, including 3 vector models
		//-----------------------------------------//
		SpeMdl->behavior[i].travelRate.gauss.coeff = behSrc->travelRate.gauss.coeff ;
		SpeMdl->behavior[i].travelRate.gauss.mean = behSrc->travelRate.gauss.mean;
		SpeMdl->behavior[i].travelRate.gauss.std = behSrc->travelRate.gauss.std;
		SpeMdl->behavior[i].travelRate.modelType = (mbsSTANDARD_MODEL_TYPE)behSrc->travelRate.modelType;
		SpeMdl->behavior[i].travelRate.rnd.coeff = behSrc->travelRate.rnd.coeff;
		SpeMdl->behavior[i].travelRate.rnd.max = behSrc->travelRate.rnd.max;
		SpeMdl->behavior[i].travelRate.rnd.min = behSrc->travelRate.rnd.min;
		SpeMdl->behavior[i].travelRate.vm.step = CopyMatrix(&behSrc->travelRate.vm.step);
		SpeMdl->behavior[i].travelRate.vm.terminate = CopyMatrix(&behSrc->travelRate.vm.terminate);
		SpeMdl->behavior[i].travelRate.vm.vector = CopyMatrix(&behSrc->travelRate.vm.vector);


		//-----------------------------------------//
		// Dive Ascent Rate
		// 10 parameters, including 3 vector models
		//-----------------------------------------//
		SpeMdl->behavior[i].dive.ascentRate.gauss.coeff = behSrc->dive.ascentRate.gauss.coeff;
		SpeMdl->behavior[i].dive.ascentRate.gauss.mean = behSrc->dive.ascentRate.gauss.mean;
		SpeMdl->behavior[i].dive.ascentRate.gauss.std = behSrc->dive.ascentRate.gauss.std;
		SpeMdl->behavior[i].dive.ascentRate.modelType = (mbsSTANDARD_MODEL_TYPE)behSrc->dive.ascentRate.modelType;
		SpeMdl->behavior[i].dive.ascentRate.rnd.coeff = behSrc->dive.ascentRate.rnd.coeff;
		SpeMdl->behavior[i].dive.ascentRate.rnd.max = behSrc->dive.ascentRate.rnd.max;
		SpeMdl->behavior[i].dive.ascentRate.rnd.min = behSrc->dive.ascentRate.rnd.min;
		SpeMdl->behavior[i].dive.ascentRate.vm.step = CopyMatrix(&behSrc->dive.ascentRate.vm.step);
		SpeMdl->behavior[i].dive.ascentRate.vm.terminate = CopyMatrix(&behSrc->dive.ascentRate.vm.terminate);
		SpeMdl->behavior[i].dive.ascentRate.vm.vector = CopyMatrix(&behSrc->dive.ascentRate.vm.vector);

		//-----------------------------------------//
		// Dive Descent Rate
		// 10 parameters, including 3 vector models
		//-----------------------------------------//
		SpeMdl->behavior[i].dive.descentRate.gauss.coeff = behSrc->dive.descentRate.gauss.coeff;
		SpeMdl->behavior[i].dive.descentRate.gauss.mean = behSrc->dive.descentRate.gauss.mean;
		SpeMdl->behavior[i].dive.descentRate.gauss.std = behSrc->dive.descentRate.gauss.std;
		SpeMdl->behavior[i].dive.descentRate.modelType = (mbsSTANDARD_MODEL_TYPE)behSrc->dive.descentRate.modelType;
		SpeMdl->behavior[i].dive.descentRate.rnd.coeff = behSrc->dive.descentRate.rnd.coeff;
		SpeMdl->behavior[i].dive.descentRate.rnd.max = behSrc->dive.descentRate.rnd.max;
		SpeMdl->behavior[i].dive.descentRate.rnd.min = behSrc->dive.descentRate.rnd.min;
		SpeMdl->behavior[i].dive.descentRate.vm.step = CopyMatrix(&behSrc->dive.descentRate.vm.step);
		SpeMdl->behavior[i].dive.descentRate.vm.terminate = CopyMatrix(&behSrc->dive.descentRate.vm.terminate);
		SpeMdl->behavior[i].dive.descentRate.vm.vector = CopyMatrix(&behSrc->dive.descentRate.vm.vector);

		//---------------------------//
		// Dive Bottom Following
		//---------------------------//
//		SpeMdl->behavior[i].dive.bottomFollows = behSrc->dive.bottomFollows;
		// Modified 7-7-09 for species version 5.0 with enhanced bottom following
		//behDst->dive.bottomFollows = behSrc->dive.bottomFollows;
		// Bottom following type.
		switch(behSrc->dive.bttmFollow.type)
		{
		case NO_BTTMFLLWNG:
			SpeMdl->behavior[i].dive.bttmFollow.bttmFollowType = mbs::mbsBTTMFLLW_MDL_TYPE::mbsNO_BTTMFLLWNG;
			break;
		case BOTTOMFOLLOWS_NORML_MDL:
			SpeMdl->behavior[i].dive.bttmFollow.bttmFollowType = mbs::mbsBTTMFLLW_MDL_TYPE::mbsBOTTOMFOLLOWS_CURRENT_VELOCITY;
			break;
		case BOTTOMFOLLOWS_REPLACEMENT_MDL:
			if(behSrc->dive.bttmFollow.rateMdl.modelType == GAUSSIAN)
			{
				SpeMdl->behavior[i].dive.bttmFollow.bttmFollowType = 
					mbs::mbsBTTMFLLW_MDL_TYPE::mbsBOTTOMFOLLOWS_GAUSSIAN_VELOCITY;
			}
			else if(behSrc->dive.bttmFollow.rateMdl.modelType == UNIFORM)
			{
				SpeMdl->behavior[i].dive.bttmFollow.bttmFollowType =
					mbs::mbsBTTMFLLW_MDL_TYPE::mbsBOTTOMFOLLOWS_UNIFORM_VELOCITY;
			}
			else // VECTOR
			{
				_ASSERT(0); // no vector models allowed
			}
			break;
		}

		SpeMdl->behavior[i].dive.bttmFollow.gauss.mean = behSrc->dive.bttmFollow.rateMdl.gauss.mean;
		SpeMdl->behavior[i].dive.bttmFollow.gauss.std = behSrc->dive.bttmFollow.rateMdl.gauss.std;
		SpeMdl->behavior[i].dive.bttmFollow.gauss.coeff = behSrc->dive.bttmFollow.rateMdl.gauss.coeff;

		SpeMdl->behavior[i].dive.bttmFollow.rnd.max = behSrc->dive.bttmFollow.rateMdl.rnd.max;
		SpeMdl->behavior[i].dive.bttmFollow.rnd.min = behSrc->dive.bttmFollow.rateMdl.rnd.min;
		SpeMdl->behavior[i].dive.bttmFollow.rnd.coeff = behSrc->dive.bttmFollow.rateMdl.rnd.coeff;

		//-----------------------------------------//
		// Dive Depth
		// 9 parameters, including 2 vector models
		//-----------------------------------------//
		SpeMdl->behavior[i].dive.depth.gauss.coeff = behSrc->dive.depth.gauss.coeff;
		SpeMdl->behavior[i].dive.depth.gauss.mean = behSrc->dive.depth.gauss.mean;
		SpeMdl->behavior[i].dive.depth.gauss.std = behSrc->dive.depth.gauss.std;
		SpeMdl->behavior[i].dive.depth.modelType = (mbsSTANDARD_MODEL_TYPE)behSrc->dive.depth.modelType;
		SpeMdl->behavior[i].dive.depth.rnd.coeff = behSrc->dive.depth.rnd.coeff;
		SpeMdl->behavior[i].dive.depth.rnd.max = behSrc->dive.depth.rnd.max;
		SpeMdl->behavior[i].dive.depth.rnd.min = behSrc->dive.depth.rnd.min;
		SpeMdl->behavior[i].dive.depth.vm.step = CopyMatrix(&behSrc->dive.depth.vm.step);
		SpeMdl->behavior[i].dive.depth.vm.vector = CopyMatrix(&behSrc->dive.depth.vm.vector);

		//-----------------------------------------//
		// Dive Reversals
		// 23 parameters, including 4 vector models
		//-----------------------------------------//
		SpeMdl->behavior[i].dive.reversal.modelType = (mbsSTANDARD_MODEL_TYPE)behSrc->dive.reversal.modelType;
		SpeMdl->behavior[i].dive.reversal.reverses = behSrc->dive.reversal.reverses;
		SpeMdl->behavior[i].dive.reversal.gauss.count.coeff = behSrc->dive.reversal.gauss.count.coeff;
		SpeMdl->behavior[i].dive.reversal.gauss.count.mean = behSrc->dive.reversal.gauss.count.mean;
		SpeMdl->behavior[i].dive.reversal.gauss.count.std = behSrc->dive.reversal.gauss.count.std;
		SpeMdl->behavior[i].dive.reversal.gauss.probOfReversal = behSrc->dive.reversal.gauss.probOfReversal;
		SpeMdl->behavior[i].dive.reversal.gauss.time.coeff = behSrc->dive.reversal.gauss.time.coeff;
		SpeMdl->behavior[i].dive.reversal.gauss.time.mean = behSrc->dive.reversal.gauss.time.mean;
		SpeMdl->behavior[i].dive.reversal.gauss.time.std = behSrc->dive.reversal.gauss.time.std;
		SpeMdl->behavior[i].dive.reversal.rnd.count.max = behSrc->dive.reversal.rnd.count.max;
		SpeMdl->behavior[i].dive.reversal.rnd.count.min = behSrc->dive.reversal.rnd.count.min;
		SpeMdl->behavior[i].dive.reversal.rnd.probOfReversal = behSrc->dive.reversal.rnd.probOfReversal;
		SpeMdl->behavior[i].dive.reversal.rnd.time.coeff = behSrc->dive.reversal.rnd.time.coeff;
		SpeMdl->behavior[i].dive.reversal.rnd.time.mean = behSrc->dive.reversal.rnd.time.mean;
		SpeMdl->behavior[i].dive.reversal.rnd.time.std = behSrc->dive.reversal.rnd.time.std;
		//SpeMdl->behavior[i].dive.reversal.hasaIndependentDiveRate = behSrc->dive.reversal.hasaIndependentDiveRate;
		SpeMdl->behavior[i].dive.reversal.diveRateType = (mbsREVERSAL_DIVE_RATE_TYPE)behSrc->dive.reversal.diveRateType;
		SpeMdl->behavior[i].dive.reversal.diveRate.coeff = behSrc->dive.reversal.diveRate.coeff ;
		SpeMdl->behavior[i].dive.reversal.diveRate.mean = behSrc->dive.reversal.diveRate.mean;
		SpeMdl->behavior[i].dive.reversal.diveRate.std = behSrc->dive.reversal.diveRate.std;

		SpeMdl->behavior[i].dive.reversal.ascentRate.coeff = behSrc->dive.reversal.ascentRate.coeff ;
		SpeMdl->behavior[i].dive.reversal.ascentRate.mean = behSrc->dive.reversal.ascentRate.mean;
		SpeMdl->behavior[i].dive.reversal.ascentRate.std = behSrc->dive.reversal.ascentRate.std;

		SpeMdl->behavior[i].dive.reversal.vm.count = CopyMatrix(&behSrc->dive.reversal.vm.count);
		SpeMdl->behavior[i].dive.reversal.vm.probOfReversal = CopyMatrix(&behSrc->dive.reversal.vm.probOfReversal);
		SpeMdl->behavior[i].dive.reversal.vm.time = CopyMatrix(&behSrc->dive.reversal.vm.time);
		SpeMdl->behavior[i].dive.reversal.vm.timeStep = CopyMatrix(&behSrc->dive.reversal.vm.timeStep);

		//-----------------------------------------//
		// Dive Surface interval
		// 6 parameters, including 4 vector models
		//-----------------------------------------//
		SpeMdl->behavior[i].dive.srfInv.modelType = (mbsSTANDARD_MODEL_TYPE)behSrc->dive.srfInv.modelType;
		SpeMdl->behavior[i].dive.srfInv.gauss.coeff = behSrc->dive.srfInv.gauss.coeff;
		SpeMdl->behavior[i].dive.srfInv.gauss.mean = behSrc->dive.srfInv.gauss.mean;
		SpeMdl->behavior[i].dive.srfInv.gauss.std = behSrc->dive.srfInv.gauss.std;
		SpeMdl->behavior[i].dive.srfInv.vm.step = CopyMatrix(&behSrc->dive.srfInv.vm.step);
		SpeMdl->behavior[i].dive.srfInv.vm.vector = CopyMatrix(&behSrc->dive.srfInv.vm.vector);


        //-----------------------------------------//
        // Behavior Transition
        // 4 parameters, including 2 vector models
        //-----------------------------------------//
		//SpeMdl->behavior[i].nrmlBehTransMatrix = CopyMatrix(&behSrc->nrmlBehTransMatrix);
		SpeMdl->behavior[i].nrmlBehTransCnt = behSrc->nrmlBehTransCnt;
		SpeMdl->behavior[i].nrmlBehTrans = gcnew array<mbsBEHTRAN>(SpeMdl->behavior[i].nrmlBehTransCnt);
		for(j=0; j<behSrc->nrmlBehTransCnt; j++)
		{

			SpeMdl->behavior[i].nrmlBehTrans[j].depthSpan.shallow = behSrc->nrmlBehTrans.arr[j].depthSpan.shallow;
			SpeMdl->behavior[i].nrmlBehTrans[j].depthSpan.deep = behSrc->nrmlBehTrans.arr[j].depthSpan.deep;
			SpeMdl->behavior[i].nrmlBehTrans[j].m = CopyMatrix(&behSrc->nrmlBehTrans.arr[j].m);
		}
	}
}


void mbs::C3mbsWrapperSpeciesModel::C3mbsWrapperSpeModelToSpeciesModel(mbsSPECIESMODEL ^SpeMdl, CSpeciesModel *CSpeMdl)
{
	// For saving...
	UINT32 i;
	INT32 j;
	NRMLBEHMDL *behDst;
	mbsNRMLBEHMDL ^behSrc;

	//----------------------//
	// Acoustic Aversion
	// 39 parameters
	//----------------------//
	CSpeMdl->m_speciesModel.acousticAversion.ascentRateAffected = SpeMdl->acousticAversion.ascentRateAffected;
	CSpeMdl->m_speciesModel.acousticAversion.ascentRate.coeff = SpeMdl->acousticAversion.ascentRate.coeff;
	CSpeMdl->m_speciesModel.acousticAversion.ascentRate.mean = SpeMdl->acousticAversion.ascentRate.mean;
	CSpeMdl->m_speciesModel.acousticAversion.ascentRate.std = SpeMdl->acousticAversion.ascentRate.std;
	CSpeMdl->m_speciesModel.acousticAversion.descentRateAffected = SpeMdl->acousticAversion.descentRateAffected;
	CSpeMdl->m_speciesModel.acousticAversion.descentRate.coeff = SpeMdl->acousticAversion.descentRate.coeff;
	CSpeMdl->m_speciesModel.acousticAversion.descentRate.mean = SpeMdl->acousticAversion.descentRate.mean;
	CSpeMdl->m_speciesModel.acousticAversion.descentRate.std = SpeMdl->acousticAversion.descentRate.std;
	CSpeMdl->m_speciesModel.acousticAversion.beaches = SpeMdl->acousticAversion.beaches;
	CSpeMdl->m_speciesModel.acousticAversion.beachingDepth = SpeMdl->acousticAversion.beachingDepth;
	CSpeMdl->m_speciesModel.acousticAversion.depth.coeff = SpeMdl->acousticAversion.depth.coeff;
	CSpeMdl->m_speciesModel.acousticAversion.depth.mean = SpeMdl->acousticAversion.depth.mean;
	CSpeMdl->m_speciesModel.acousticAversion.depth.std = SpeMdl->acousticAversion.depth.std;
	CSpeMdl->m_speciesModel.acousticAversion.depthAffected = SpeMdl->acousticAversion.depthAffected;
	CSpeMdl->m_speciesModel.acousticAversion.flatBottomDiveAffected = SpeMdl->acousticAversion.flatBottomDiveAffected;
	CSpeMdl->m_speciesModel.acousticAversion.flatBottomDives = SpeMdl->acousticAversion.flatBottomDives;
	CSpeMdl->m_speciesModel.acousticAversion.podBreaksUp = SpeMdl->acousticAversion.podBreaksUp;
	CSpeMdl->m_speciesModel.acousticAversion.reversal.count.coeff = SpeMdl->acousticAversion.reversal.count.coeff;
	CSpeMdl->m_speciesModel.acousticAversion.reversal.count.mean = SpeMdl->acousticAversion.reversal.count.mean;
	CSpeMdl->m_speciesModel.acousticAversion.reversal.count.std = SpeMdl->acousticAversion.reversal.count.std;
	CSpeMdl->m_speciesModel.acousticAversion.reversal.probOfReversal = SpeMdl->acousticAversion.reversal.probOfReversal;
	CSpeMdl->m_speciesModel.acousticAversion.reversal.time.coeff = SpeMdl->acousticAversion.reversal.time.coeff;
	CSpeMdl->m_speciesModel.acousticAversion.reversal.time.mean = SpeMdl->acousticAversion.reversal.time.mean;
	CSpeMdl->m_speciesModel.acousticAversion.reversal.time.std = SpeMdl->acousticAversion.reversal.time.std;
	CSpeMdl->m_speciesModel.acousticAversion.reversalAffected = SpeMdl->acousticAversion.reversalAffected;
	CSpeMdl->m_speciesModel.acousticAversion.surfaceInterval.coeff = SpeMdl->acousticAversion.surfaceInterval.coeff;
	CSpeMdl->m_speciesModel.acousticAversion.surfaceInterval.mean = SpeMdl->acousticAversion.surfaceInterval.mean;
	CSpeMdl->m_speciesModel.acousticAversion.surfaceInterval.std = SpeMdl->acousticAversion.surfaceInterval.std;
	CSpeMdl->m_speciesModel.acousticAversion.surfaceIntervalAffected = SpeMdl->acousticAversion.surfaceIntervalAffected;
	CSpeMdl->m_speciesModel.acousticAversion.travel.arcStep = SpeMdl->acousticAversion.travel.arcStep;
	CSpeMdl->m_speciesModel.acousticAversion.travel.bias = SpeMdl->acousticAversion.travel.bias;
	CSpeMdl->m_speciesModel.acousticAversion.travel.directionOfBias = SpeMdl->acousticAversion.travel.directionOfBias;
	CSpeMdl->m_speciesModel.acousticAversion.travel.perturbation = SpeMdl->acousticAversion.travel.perturbation;
	CSpeMdl->m_speciesModel.acousticAversion.travel.termCoeff = SpeMdl->acousticAversion.travel.termCoeff;
	CSpeMdl->m_speciesModel.acousticAversion.travelDirectionAffected = SpeMdl->acousticAversion.travelDirectionAffected;
	CSpeMdl->m_speciesModel.acousticAversion.travelRate.coeff = SpeMdl->acousticAversion.travelRate.coeff;
	CSpeMdl->m_speciesModel.acousticAversion.travelRate.mean = SpeMdl->acousticAversion.travelRate.mean;
	CSpeMdl->m_speciesModel.acousticAversion.travelRate.std = SpeMdl->acousticAversion.travelRate.std;
	CSpeMdl->m_speciesModel.acousticAversion.travelRateAffected = SpeMdl->acousticAversion.travelRateAffected;

	//----------------------------------------------------------------------------------//
	// Species Description
	// 7 parameters: 3mbs lib version, species version, species group, short description,
	//				 long comment, shore following depth, and number of behaviors
	//----------------------------------------------------------------------------------//
	CSpeMdl->m_speciesModel.description.mbsVerSuper = SpeMdl->mbsLibVerSuper;
	CSpeMdl->m_speciesModel.description.mbsVerSub = SpeMdl->mbsLibVerSub;
	CSpeMdl->m_speciesModel.description.speVerSuper = SpeMdl->speVerSuper;
	CSpeMdl->m_speciesModel.description.speVerSub = SpeMdl->speVerSub;
	CSpeMdl->m_speciesModel.description.group = (SPECIESGROUP)SpeMdl->speciesGroup;
	mbsInstance.CharBufferFromString(SpeMdl->szSpeciesShrtDscrptn, CSpeMdl->m_speciesModel.description.speciesShrtDscrptn, SPECIES_DESCRPTN_MAX_LEN);
	mbsInstance.CharBufferFromString(SpeMdl->szSpeciesLongComment, CSpeMdl->m_speciesModel.description.speciesComment, SPECIES_COMMENT_MAX_LEN);
	CSpeMdl->m_speciesModel.description.shoreFollowDepth = SpeMdl->shoreFollowDepth;
	CSpeMdl->m_speciesModel.description.minSeedingDepth = SpeMdl->minSeedDepth;
	CSpeMdl->m_speciesModel.description.numBehaviors = SpeMdl->behaviorCount;
	CSpeMdl->m_speciesModel.description.deepWaterSeedingDepth = SpeMdl->deepWaterSeedingDepth;
	CSpeMdl->m_speciesModel.description.deepWaterSeedingDepthEnabled = SpeMdl->deepWaterSeedingDepthEnabled;

	CSpeMdl->m_speciesModel.description.name = (SPECIESNAME)SpeMdl->speciesName;

#if 0
	CSpeMdl->m_speciesModel.description.year = SpeMdl->year;
	CSpeMdl->m_speciesModel.description.month = SpeMdl->month;
	CSpeMdl->m_speciesModel.description.day = SpeMdl->day;

	CSpeMdl->m_speciesModel.description.hour = SpeMdl->hour;
	CSpeMdl->m_speciesModel.description.min = SpeMdl->min;
	CSpeMdl->m_speciesModel.description.sec = SpeMdl->sec;
	CSpeMdl->m_speciesModel.description.id = SpeMdl->id;
#endif


	//---------------------//
	// Initial Behavior
	//---------------------//
	//CopyMatrix(&CSpeMdl->m_speciesModel.initialBehavior, SpeMdl->initialBehavior);
	CSpeMdl->m_speciesModel.initBehSpanCnt = SpeMdl->initBehSpanCnt;
	CSpeMdl->m_speciesModel.initialBehavior.arr = new BEHTRAN[CSpeMdl->m_speciesModel.initBehSpanCnt];
	memset(CSpeMdl->m_speciesModel.initialBehavior.arr, 0, sizeof(BEHTRAN)* CSpeMdl->m_speciesModel.initBehSpanCnt);
	for(i=0; (int)i<CSpeMdl->m_speciesModel.initBehSpanCnt; i++)
	{
		CSpeMdl->m_speciesModel.initialBehavior.arr[i].depthSpan.shallow = (int)SpeMdl->initialBehavior[i].depthSpan.shallow;
		CSpeMdl->m_speciesModel.initialBehavior.arr[i].depthSpan.deep = (int)SpeMdl->initialBehavior[i].depthSpan.deep;
		//CSpeMdl->m_speciesModel.initialBehavior.arr[i].m = CopyMatrix(SpeMdl->initialBehavior[i]);
		CopyMatrix(&CSpeMdl->m_speciesModel.initialBehavior.arr[i].m, SpeMdl->initialBehavior[i].m);

	}


	//------------------------//
	// Behavior Allocation
	//------------------------//
	CSpeMdl->m_speciesModel.description.numBehaviors = SpeMdl->behaviorCount;
	CSpeMdl->m_speciesModel.p.behavior = (NRMLBEHMDL *)malloc(SpeMdl->behaviorCount * sizeof(NRMLBEHMDL));
	memset(CSpeMdl->m_speciesModel.p.behavior, 0, SpeMdl->behaviorCount * sizeof(NRMLBEHMDL));

	for(i=0; i<CSpeMdl->m_speciesModel.description.numBehaviors; i++)
	{
		// Make code lines shorter...
		behDst = &CSpeMdl->m_speciesModel.p.behavior[i];
		behSrc = SpeMdl->behavior[i];

		//------------------//
		// Behavior Name
		//------------------//
		mbsInstance.CharBufferFromString(behSrc->szName, behDst->szName, sizeof(behDst->szName));

		switch(behSrc->behTransTermFormula)
		{
		case mbsBehTermFormula::T50_K:
			behDst->nrmlBehTermType = T50_K_TERM;
			break;
		case mbsBehTermFormula::GAUSSIAN:
			behDst->nrmlBehTermType = GAUSSIAN_TERM;
			break;
		}

		//-------------------------------------//
		// Environmental Attractor Priority
		//-------------------------------------//
		behDst->depthHasPriorityOverTemp = behSrc->depthHasPriorityOverTemp;

		//----------------------------------------//
		// Depth environmental attractor
		// 8 parameters, including 2 vector models
		//------------------------------------------//
		behDst->depthEnvAtt.shelfIsEnabled = behSrc->depthEnvAtt.shelfIsEnabled;
		behDst->depthEnvAtt.shelfDepth = behSrc->depthEnvAtt.shelfDepth;
		behDst->depthEnvAtt.shelfSlope = behSrc->depthEnvAtt.shelfSlope;

		behDst->depthEnvAtt.basinIsEnabled = behSrc->depthEnvAtt.basinIsEnabled;
		behDst->depthEnvAtt.basinDepth = behSrc->depthEnvAtt.basinDepth;
		behDst->depthEnvAtt.basinSlope = behSrc->depthEnvAtt.basinSlope;

		behDst->depthEnvAtt.slopeIsEnabled = behSrc->depthEnvAtt.slopeIsEnabled;
		behDst->depthEnvAtt.slopeDepth = behSrc->depthEnvAtt.slopeDepth;
		behDst->depthEnvAtt.slopeSlope = behSrc->depthEnvAtt.slopeSlope;
		
		behDst->depthEnvAttBehTrans.meanTimeInBeh = behSrc->depthEnvAttBehTrans.meanTimeInBeh;
		behDst->depthEnvAttBehTrans.slopeCoefficient = behSrc->depthEnvAttBehTrans.slopeCoefficient;

		CopyMatrix(&behDst->depthEnvAttBehTrans.behavior, behSrc->depthEnvAttBehTrans.behavior);
		CopyMatrix(&behDst->depthEnvAttBehTrans.terminate, behSrc->depthEnvAttBehTrans.terminate);

		//-----------------------------------------//
		// Temperature Environmental Attractor
		// 8 parameters, including 2 vector models
		//-----------------------------------------//
		behDst->tempEnvAtt.delta = behSrc->tempEnvAtt.delta;
		behDst->tempEnvAtt.deltaIsEnabled = behSrc->tempEnvAtt.deltaIsEnabled;
		behDst->tempEnvAtt.max = behSrc->tempEnvAtt.max;
		behDst->tempEnvAtt.maxIsEnabled = behSrc->tempEnvAtt.maxIsEnabled;
		behDst->tempEnvAtt.min = behSrc->tempEnvAtt.min;
		behDst->tempEnvAtt.minIsEnabled = behSrc->tempEnvAtt.minIsEnabled;

		behDst->tempEnvAttBehTrans.meanTimeInBeh = behSrc->tempEnvAttBehTrans.meanTimeInBeh;
		behDst->tempEnvAttBehTrans.slopeCoefficient = behSrc->tempEnvAttBehTrans.slopeCoefficient;

		CopyMatrix(&behDst->tempEnvAttBehTrans.behavior, behSrc->tempEnvAttBehTrans.behavior);
		CopyMatrix(&behDst->tempEnvAttBehTrans.terminate, behSrc->tempEnvAttBehTrans.terminate);

		//-----------------------------------------//
		// Travel Direction
		// 12 parameters, including 3 vector models
		//-----------------------------------------//
		behDst->travelDirection.crRndWalk.perturbation = behSrc->travelDirection.crRndWalk.perturbation;
		behDst->travelDirection.crRndWalk.termCoeff = behSrc->travelDirection.crRndWalk.termCoeff;
		behDst->travelDirection.crRndWalkDb.arcStep = behSrc->travelDirection.crRndWalkDb.arcStep;
		behDst->travelDirection.crRndWalkDb.bias = behSrc->travelDirection.crRndWalkDb.bias;
		behDst->travelDirection.crRndWalkDb.directionOfBias = behSrc->travelDirection.crRndWalkDb.directionOfBias;
		behDst->travelDirection.crRndWalkDb.perturbation = behSrc->travelDirection.crRndWalkDb.perturbation;
		behDst->travelDirection.crRndWalkDb.termCoeff = behSrc->travelDirection.crRndWalkDb.termCoeff;
		behDst->travelDirection.modelType = (DIRECTIONAL_MODEL_TYPE)behSrc->travelDirection.modelType;
		behDst->travelDirection.rndWalk.termCoeff = behSrc->travelDirection.rndWalk.termCoeff;
		CopyMatrix(&behDst->travelDirection.vm.direction, behSrc->travelDirection.vm.direction);
		CopyMatrix(&behDst->travelDirection.vm.directionalBias, behSrc->travelDirection.vm.directionalBias);
		CopyMatrix(&behDst->travelDirection.vm.terminate, behSrc->travelDirection.vm.terminate);

		//-----------------------------------------//
		// Travel Rate
		// 10 parameters, including 3 vector models
		//-----------------------------------------//
		behDst->travelRate.gauss.coeff = behSrc->travelRate.gauss.coeff ;
		behDst->travelRate.gauss.mean = behSrc->travelRate.gauss.mean;
		behDst->travelRate.gauss.std = behSrc->travelRate.gauss.std;
		behDst->travelRate.modelType = (STANDARD_MODEL_TYPE)behSrc->travelRate.modelType;
		behDst->travelRate.rnd.coeff = behSrc->travelRate.rnd.coeff;
		behDst->travelRate.rnd.max = behSrc->travelRate.rnd.max;
		behDst->travelRate.rnd.min = behSrc->travelRate.rnd.min;
		CopyMatrix(&behDst->travelRate.vm.step, behSrc->travelRate.vm.step);
		CopyMatrix(&behDst->travelRate.vm.terminate, behSrc->travelRate.vm.terminate);
		CopyMatrix(&behDst->travelRate.vm.vector, behSrc->travelRate.vm.vector);


		//-----------------------------------------//
		// Dive Ascent Rate
		// 10 parameters, including 3 vector models
		//-----------------------------------------//
		behDst->dive.ascentRate.gauss.coeff = behSrc->dive.ascentRate.gauss.coeff;
		behDst->dive.ascentRate.gauss.mean = behSrc->dive.ascentRate.gauss.mean;
		behDst->dive.ascentRate.gauss.std = behSrc->dive.ascentRate.gauss.std;
		behDst->dive.ascentRate.modelType = (STANDARD_MODEL_TYPE)behSrc->dive.ascentRate.modelType;
		behDst->dive.ascentRate.rnd.coeff = behSrc->dive.ascentRate.rnd.coeff;
		behDst->dive.ascentRate.rnd.max = behSrc->dive.ascentRate.rnd.max;
		behDst->dive.ascentRate.rnd.min = behSrc->dive.ascentRate.rnd.min;
		CopyMatrix(&behDst->dive.ascentRate.vm.step, behSrc->dive.ascentRate.vm.step);
		CopyMatrix(&behDst->dive.ascentRate.vm.terminate, behSrc->dive.ascentRate.vm.terminate);
		CopyMatrix(&behDst->dive.ascentRate.vm.vector, behSrc->dive.ascentRate.vm.vector);

		//-------------------------------------------//
		// Dive Descent Rate
		// 10 parameters, including 3 vector models
		//-------------------------------------------//
		behDst->dive.descentRate.gauss.coeff = behSrc->dive.descentRate.gauss.coeff;
		behDst->dive.descentRate.gauss.mean = behSrc->dive.descentRate.gauss.mean;
		behDst->dive.descentRate.gauss.std = behSrc->dive.descentRate.gauss.std;
		behDst->dive.descentRate.modelType = (STANDARD_MODEL_TYPE)behSrc->dive.descentRate.modelType;
		behDst->dive.descentRate.rnd.coeff = behSrc->dive.descentRate.rnd.coeff;
		behDst->dive.descentRate.rnd.max = behSrc->dive.descentRate.rnd.max;
		behDst->dive.descentRate.rnd.min = behSrc->dive.descentRate.rnd.min;
		CopyMatrix(&behDst->dive.descentRate.vm.step, behSrc->dive.descentRate.vm.step);
		CopyMatrix(&behDst->dive.descentRate.vm.terminate, behSrc->dive.descentRate.vm.terminate);
		CopyMatrix(&behDst->dive.descentRate.vm.vector, behSrc->dive.descentRate.vm.vector);

		//---------------------------//
		// Dive Bottom Following
		//---------------------------//
		// Modified 7-7-09 for species version 5.0 with enhanced bottom following
		//behDst->dive.bottomFollows = behSrc->dive.bottomFollows;
		// Bottom following type.
		switch(behSrc->dive.bttmFollow.bttmFollowType)
		{
		case mbs::mbsBTTMFLLW_MDL_TYPE::mbsNO_BTTMFLLWNG:
			behDst->dive.bttmFollow.type = NO_BTTMFLLWNG;
			break;
		case mbs::mbsBTTMFLLW_MDL_TYPE::mbsBOTTOMFOLLOWS_CURRENT_VELOCITY:
			behDst->dive.bttmFollow.type = BOTTOMFOLLOWS_NORML_MDL;
			break;
		case mbs::mbsBTTMFLLW_MDL_TYPE::mbsBOTTOMFOLLOWS_GAUSSIAN_VELOCITY:
			behDst->dive.bttmFollow.type = BOTTOMFOLLOWS_REPLACEMENT_MDL;
			behDst->dive.bttmFollow.rateMdl.modelType = GAUSSIAN;
			break;
		case mbs::mbsBTTMFLLW_MDL_TYPE::mbsBOTTOMFOLLOWS_UNIFORM_VELOCITY:
			behDst->dive.bttmFollow.type = BOTTOMFOLLOWS_REPLACEMENT_MDL;
			behDst->dive.bttmFollow.rateMdl.modelType = UNIFORM;
			break;
		}

		behDst->dive.bttmFollow.rateMdl.gauss.mean = behSrc->dive.bttmFollow.gauss.mean;
		behDst->dive.bttmFollow.rateMdl.gauss.std = behSrc->dive.bttmFollow.gauss.std;
		behDst->dive.bttmFollow.rateMdl.gauss.coeff = behSrc->dive.bttmFollow.gauss.coeff;

		behDst->dive.bttmFollow.rateMdl.rnd.max = behSrc->dive.bttmFollow.rnd.max;
		behDst->dive.bttmFollow.rateMdl.rnd.min = behSrc->dive.bttmFollow.rnd.min;
		behDst->dive.bttmFollow.rateMdl.rnd.coeff = behSrc->dive.bttmFollow.rnd.coeff;


		//------------------------------------------//
		// Dive Depth
		// 9 parameters, including 2 vector models
		//------------------------------------------//
		behDst->dive.depth.gauss.coeff = behSrc->dive.depth.gauss.coeff;
		behDst->dive.depth.gauss.mean = behSrc->dive.depth.gauss.mean;
		behDst->dive.depth.gauss.std = behSrc->dive.depth.gauss.std;
		behDst->dive.depth.modelType = (STANDARD_MODEL_TYPE)behSrc->dive.depth.modelType;
		behDst->dive.depth.rnd.coeff = behSrc->dive.depth.rnd.coeff;
		behDst->dive.depth.rnd.max = behSrc->dive.depth.rnd.max;
		behDst->dive.depth.rnd.min = behSrc->dive.depth.rnd.min;
		CopyMatrix(&behDst->dive.depth.vm.step, behSrc->dive.depth.vm.step);
		CopyMatrix(&behDst->dive.depth.vm.vector, behSrc->dive.depth.vm.vector);

		//-------------------------------------------//
		// Dive Reversals
		// 23 parameters, including 4 vector models
		//-------------------------------------------//
		behDst->dive.reversal.modelType = (STANDARD_MODEL_TYPE)behSrc->dive.reversal.modelType;
		behDst->dive.reversal.reverses = behSrc->dive.reversal.reverses;
		behDst->dive.reversal.gauss.count.coeff = behSrc->dive.reversal.gauss.count.coeff;
		behDst->dive.reversal.gauss.count.mean = behSrc->dive.reversal.gauss.count.mean;
		behDst->dive.reversal.gauss.count.std = behSrc->dive.reversal.gauss.count.std;
		behDst->dive.reversal.gauss.probOfReversal = behSrc->dive.reversal.gauss.probOfReversal;
		behDst->dive.reversal.gauss.time.coeff = behSrc->dive.reversal.gauss.time.coeff;
		behDst->dive.reversal.gauss.time.mean = behSrc->dive.reversal.gauss.time.mean;
		behDst->dive.reversal.gauss.time.std = behSrc->dive.reversal.gauss.time.std;
		behDst->dive.reversal.rnd.count.max = behSrc->dive.reversal.rnd.count.max;
		behDst->dive.reversal.rnd.count.min = behSrc->dive.reversal.rnd.count.min;
		behDst->dive.reversal.rnd.probOfReversal = behSrc->dive.reversal.rnd.probOfReversal;
		behDst->dive.reversal.rnd.time.coeff = behSrc->dive.reversal.rnd.time.coeff;
		behDst->dive.reversal.rnd.time.mean = behSrc->dive.reversal.rnd.time.mean;
		behDst->dive.reversal.rnd.time.std = behSrc->dive.reversal.rnd.time.std;

		//behDst->dive.reversal.hasaIndependentDiveRate = behSrc->dive.reversal.hasaIndependentDiveRate;
		behDst->dive.reversal.diveRateType = (REVERSAL_DIVE_RATE_TYPE)behSrc->dive.reversal.diveRateType;

		behDst->dive.reversal.diveRate.coeff = behSrc->dive.reversal.diveRate.coeff ;
		behDst->dive.reversal.diveRate.mean = behSrc->dive.reversal.diveRate.mean;
		behDst->dive.reversal.diveRate.std = behSrc->dive.reversal.diveRate.std;

		behDst->dive.reversal.ascentRate.coeff = behSrc->dive.reversal.ascentRate.coeff ;
		behDst->dive.reversal.ascentRate.mean = behSrc->dive.reversal.ascentRate.mean;
		behDst->dive.reversal.ascentRate.std = behSrc->dive.reversal.ascentRate.std;

		CopyMatrix(&behDst->dive.reversal.vm.count, behSrc->dive.reversal.vm.count);
		CopyMatrix(&behDst->dive.reversal.vm.probOfReversal, behSrc->dive.reversal.vm.probOfReversal);
		CopyMatrix(&behDst->dive.reversal.vm.time, behSrc->dive.reversal.vm.time);
		CopyMatrix(&behDst->dive.reversal.vm.timeStep, behSrc->dive.reversal.vm.timeStep);

		//-------------------------------------------//
		// Dive Surface interval
		// 6 parameters, including 4 vector models
		//-------------------------------------------//
		behDst->dive.srfInv.modelType = (STANDARD_MODEL_TYPE)behSrc->dive.srfInv.modelType;
		behDst->dive.srfInv.gauss.coeff = behSrc->dive.srfInv.gauss.coeff;
		behDst->dive.srfInv.gauss.mean = behSrc->dive.srfInv.gauss.mean;
		behDst->dive.srfInv.gauss.std = behSrc->dive.srfInv.gauss.std;
		CopyMatrix(&behDst->dive.srfInv.vm.step, behSrc->dive.srfInv.vm.step);
		CopyMatrix(&behDst->dive.srfInv.vm.vector, behSrc->dive.srfInv.vm.vector);

        //-----------------------------------------//
        // Behavior Transition
        // 4 parameters, including 2 vector models
        //-----------------------------------------//
		behDst->nrmlBehTransCnt = behSrc->nrmlBehTransCnt;
		behDst->nrmlBehTrans.arr = new BEHTRAN[behDst->nrmlBehTransCnt];
		memset(behDst->nrmlBehTrans.arr, 0, sizeof(BEHTRAN)*behDst->nrmlBehTransCnt);
		for(j=0; j<behDst->nrmlBehTransCnt; j++)
		{
			behDst->nrmlBehTrans.arr[j].depthSpan.shallow = (int)behSrc->nrmlBehTrans[j].depthSpan.shallow;
			behDst->nrmlBehTrans.arr[j].depthSpan.deep = (int)behSrc->nrmlBehTrans[j].depthSpan.deep;
			CopyMatrix(&behDst->nrmlBehTrans.arr[j].m, behSrc->nrmlBehTrans[j].m);
		}

		//CopyMatrix(&behDst->nrmlBehTransMatrix, behSrc->nrmlBehTransMatrix);


//		CopyMatrix(&behDst->nrmlBehTrans.behavior, behSrc->nrmlBehTrans.behavior);
//		CopyMatrix(&behDst->nrmlBehTrans.terminate, behSrc->nrmlBehTrans.terminate);
//		behDst->nrmlBehTrans.meanTimeInBeh = behSrc->nrmlBehTrans.meanTimeInBeh;
//		behDst->nrmlBehTrans.slopeCoefficient = behSrc->nrmlBehTrans.slopeCoefficient;
	}
}



mbs::mbsRESULT mbs::C3mbsWrapperSpeciesModel::LoadFromBinFile(String ^FileName,  mbsSPECIESMODEL ^SpeMdl)
{
	//mbs::C3mbs::mbsResult res;
	RESLT res;
	//mbs::mbsRESULT res;
	CSpeciesModel CSpeMdl;
	char fileName[256];
	mbsInstance.CharBufferFromString(FileName, fileName, 256);
	if(OK != (res = CSpeMdl.LoadFromBinFile(fileName)))
		return mbs::mbsRESULT(res);

	SpeciesModelToC3mbsWrapperSpeModel(&CSpeMdl, SpeMdl);

	return mbs::mbsRESULT(res);
}

int mbs::C3mbsWrapperSpeciesModel::ModelToTextFile(String ^FileName,  mbsSPECIESMODEL ^SpeMdl)
{
	CSpeciesModel CSpeMdl;
	char fileName[256];
	mbsInstance.CharBufferFromString(FileName, fileName, 256);
	C3mbsWrapperSpeModelToSpeciesModel(SpeMdl, &CSpeMdl);
	CSpeMdl.ModelToText(fileName);
	return 0;
}


int mbs::C3mbsWrapperSpeciesModel::SaveToBinFile(String ^FileName,  mbsSPECIESMODEL ^SpeMdl)
{
	CSpeciesModel CSpeMdl;
	char fileName[256];
	mbsInstance.CharBufferFromString(FileName, fileName, 256);
	C3mbsWrapperSpeModelToSpeciesModel(SpeMdl, &CSpeMdl);
	CSpeMdl.SaveToBinFile(fileName);

	// This following line isn't necessary but helps to ensure information translated
	// properly.  Helps determine that reading information back from file by a
	// separate call to LoadFromBinFile is most likely a problem with the file
	// saving rather than the exchange of information through the wrapper classes
	// (but no guarantee there isn't a problem with information exchange).
	SpeciesModelToC3mbsWrapperSpeModel(&CSpeMdl, SpeMdl);

	// The following would be needed if call to SpeciesModelToC3mbsWrapperSpeModel()
	// isn't made.
#if 0
	// Get the updated date, time, and unique ID values.
	SpeMdl->year = CSpeMdl.m_speciesModel.description.year;
	SpeMdl->month = CSpeMdl.m_speciesModel.description.month;
	SpeMdl->day = CSpeMdl.m_speciesModel.description.day;
	SpeMdl->hour = CSpeMdl.m_speciesModel.description.hour;
	SpeMdl->min = CSpeMdl.m_speciesModel.description.min;
	SpeMdl->sec = CSpeMdl.m_speciesModel.description.sec;
	SpeMdl->id = CSpeMdl.m_speciesModel.description.id;

	SpeMdl->speVerSuper = CSpeMdl.m_speciesModel.description.speVerSuper;
	SpeMdl->speVerSub = CSpeMdl.m_speciesModel.description.speVerSub;
#endif
	return 0;
}
