#pragma once

//#include "3mbslib.h"
#include "scenario.h"
#include "ScenarioStatic.h"
#include "3mbsWrapperDataTypes.h"
#include "OutputReader.h"

using namespace System;


namespace mbs {

	public ref class C3mbs
	{
	public:
		C3mbs();

		// TODO: Add your methods for this class here.
	private:
		CScenario *m_sce;
		CStaticScenario *m_staticScenario;
		COutputReader *m_binOutReader;
	
		//mbs::mbsRESULT SimpleMbsResult(RESLT Res);
		mbs::mbsRESULT ValidSpeciesIndex(int Index);
		mbs::mbsRESULT ValidPodIndex(int SpeciesIndex, int Index);
		mbs::mbsRESULT ValidPodAnimatIndex(int SpeciesIndex, int PodIndex, int Index);
		mbs::mbsRESULT ValidIndividualIndex(int SpeciesIndex, int Index);
		mbs::mbsRESULT ValidAnimatIndex(int Index);
		mbs::mbsRESULT CoordStructToPositionArray(INHABITINF *C, int NumAnimats, array<mbs::mbsPosition>^ P);
		mbs::mbsRESULT PositionArrayToInitialConditionStruct(INHABITINF **IC, int NumAnimats, array<mbs::mbsPosition>^ P);
		mbs::mbsRESULT CoordToPosition(COORD_DEPTH *C, mbs::mbsPosition^ P);

	public: // Binary File Reader
		mbsBUILDINF GetBuildInformation();

		mbs::mbsRESULT SimpleMbsResult(RESLT Res);

		char *CharBufferFromString(String^ Sz, char *szBuffer, int BufferLen);
		String ^CharBufferToString(char *szBuffer);
		double *DoubleArrayToDoubleBuffer(array<double>^ D);
		double SetBaythyConstantDepth(double Depth); // must be negative and deeper than beaching depth
		mbs::mbsRESULT SetSpeciesDef(/*mbs::mbsSPECIES_MDL^ SpeMdl*/);


		mbs::mbsRESULT BinFileOpen(String^ FileName);
		void BinFileClose();
		bool BinFileIsOpen();
		unsigned int BinFileLibraryVersionSuper();
		unsigned int BinFileLibraryVersionSub();
		unsigned int LibraryVersionSuper();
		unsigned int LibraryVersionSub();
		int BinFileSpeciesCount();
		int BinFileAnimatCount();
		int BinFileSpeciesStateCount();
		int BinFileStartTime();

		mbs::mbsANIMAT_STATE BinFileGetAnimatState(int AnimatIndex, int StateIndex, mbs::mbsRESULT^ pResult);
		mbs::mbsRESULT BinFileGetAnimatStates(int AnimatIndex, array<mbs::mbsANIMAT_STATE>^ pAnimatStateBuffer);

	private:
		mbs::mbsRESULT CompareFileOutput(String^ FileName1, String^ FileName2);
		mbs::mbsSCENARIO_PARAMS GetSceParams();
		mbs::mbsANIMAT_STATE AnimatStateToMbsAnimatState(ANIMATSTATE_FILEOUT *s);
		void AnimatStateToMbsAnimatState(ANIMATSTATE_FILEOUT *s, int sLength, array<mbs::mbsANIMAT_STATE>^ m);
		//array<mbs::mbsANIMAT_STATE>^ AnimatStateToMbsAnimatState(ANIMATSTATE_FILEOUT *s, int ArraryLength);

	public:
		mbs::mbsRESULT ScenarioToText(String^ FileName);
		mbs::mbsRESULT SpeciesToText(int SpeciesIndex, String^ FileName);
		mbs::mbsRESULT SetOutputDirectory(String^ Directory);

		void ResetRunCount();

	public:
		mbs::mbsRESULT SetAnimatAcousticExposure(double SourceLat, double SourceLon, array<double>^ AcousticExposureArray);
		mbs::mbsRESULT SetAnimatBathymetry(array<double>^ BathyArray);
		bool IsActive();
		mbs::mbsRUNSTATE GetRunState();
		mbs::mbsSCESTATE GetScenarioState();


		mbs::mbsANIMAT_STATE RetrieveFirstAnimatStateAtIndex(DWORD Index);


		//---------//
		// Species
		//---------//
		mbs::mbsRESULT AddSpecies(String^ szFileName);
		void DeleteSpecies(); // Deletes all species from scenario.
		mbs::mbsRESULT DeleteSpecies(int Index); // Deletes specific species from scenario.
		int GetSpeciesCount();
		mbs::mbsRESULT GetAnimatCoordinates(int SpeciesIndex, array<mbs::mbsPosition>^ Position);

		//-------------//
		// All Animats
		//-------------//
		int GetAnimatCount();// Count of all animats in the scenario.
		int GetAnimatCount(int SpeciesIndex);// Count of all animats in a specific species
		mbsRESULT GetAnimatCoordinates(array<mbs::mbsPosition>^ Position);


		mbsRESULT SeedingCoordinateIsValid(int SpeciesIndex, mbsPosition^ Position);
		double GetShoreFollowingDepth(int SpeciesIndex);
		double GetMinimumSeededingDepth(int SpeciesIndex);

		//mbs::mbsRESULT GetAnimatCoordinates(array<mbs::mbsPosition>^ Position);

		//------//
		// Pods
		//------//
		int GetPodCount(); // Count of all pods in the scenario.
		int GetPodCount(int SpeciesIndex);// Count of all pods in a specific species
		int GetPodMemberCount(int SpeciesIndex, int PodIndex);
		mbs::mbsRESULT AddPod(int SpeciesIndex);
		mbs::mbsRESULT AddPod(int SpeciesIndex, mbsPODLEADERTYPE LeaderType, double FocalDistance, int NumAnimats, array<mbs::mbsPosition>^ Position);
		mbs::mbsRESULT AddPodAnimat(int SpeciesIndex, int PodIndex, mbsPosition Position);
		mbs::mbsRESULT AddPodAnimats(int SpeciesIndex, int PodIndex, int NumAnimats, array<mbsPosition>^ Position);
		mbs::mbsRESULT DeletePods(int SpeciesIndex); // Deletes all pods from specific species
		mbs::mbsRESULT DeletePod(int SpeciesIndex, int PodIndex); // Deletes specific pod from specific species.
		mbs::mbsRESULT DeletePodMember(int SpeciesIndex, int PodIndex, int AnimatIndex); // Deletes specific animat from specific pod in specific species.
		mbs::mbsRESULT GetPodMemberInitialCoordinate(int SpeciesIndex, int PodIndex, int PodMemberIndex, mbsPosition^ Position);
		mbs::mbsRESULT GetPodMemberCoordinates(int SpeciesIndex, int PodIndex, array<mbs::mbsPosition>^ Position);
		mbs::mbsRESULT GetPodMemberCoordinate(int SpeciesIndex, int PodIndex, int PodMemberIndex, mbsPosition^ Position);
		mbs::mbsRESULT SetPodLeaderType(int SpeciesIndex, int PodIndex, mbsPODLEADERTYPE Type);
		mbs::mbsRESULT GetPodLeaderType(int SpeciesIndex, int PodIndex, mbsPODLEADERTYPE^ Type);
		mbs::mbsRESULT SetPodLeaderFocalDistance(int SpeciesIndex, int PodIndex, double FocalDistance);
		double GetPodLeaderFocalDistance(int SpeciesIndex, int PodIndex);
		mbs::mbsRESULT GetPodFocalCoordinate(int SpeciesIndex, int PodIndex, mbsPosition^ Position);

		//------------//
		// Individuals
		//------------//
		int GetIndivdualCount();// Count of all individuals in the scenario.
		int GetIndivdualCount(int SpeciesIndex);// Count of all individuals in a specific species
		mbs::mbsRESULT AddIndividualAnimat(int SpeciesIndex, mbsPosition Position);
		mbs::mbsRESULT AddIndividualAnimats(int SpeciesIndex, int NumAnimats, array<mbsPosition>^ Position);
		mbs::mbsRESULT DeleteIndividuals(int SpeciesIndex); // Deletes all individuals from specific species.
		mbs::mbsRESULT DeleteIndividual(int SpeciesIndex, int IndividualIndex);// Deletes specific individual from specific species.
		mbs::mbsRESULT GetIndividualInitialCoordinate(int SpeciesIndex, int IndividualIndex, mbsPosition^ Position);
		mbs::mbsRESULT GetIndividualCoordinates(int SpeciesIndex, array<mbs::mbsPosition>^ Position);
		mbs::mbsRESULT GetIndividualCoordinate(int SpeciesIndex, int IndividualIndex, mbsPosition^ Position);

		//-------//
		// Access
		//-------//
		String^ GetBehaviorName(int SpeciesIndex, int Index);
		int GetNumberOfBehaviorsModeledInSpecies(int SpeciesIndex);

		//-------------------//
		// Species Titles
		//-------------------//
		mbs::mbsRESULT SetSpeciesDisplayTitle(int Index, String^ Title);
		String^ GetSpeciesDisplayTitle(int Index);

		//----------------------------------//
		// Simulation Routines and Variables
		//----------------------------------//
		mbs::mbsRESULT RunScenarioEntireDuration(); //RunScenario(int NumIterations=-1);
		mbs::mbsRESULT RunScenarioNumIterations(int NumIterations); //RunScenario(int NumIterations=-1); // this goes away

		mbs::mbsRESULT InitializeRun();
		mbs::mbsRESULT StepRun(unsigned int NumIterations);
		mbs::mbsRESULT FinishRun();


		// Deallocates memory, closes files, resets 3MB.
		void ClearScenario();// Deallocates memory, closes files, resets 3MB.

		//mbs::mbsRESULT ExtractBinaryResultsIntoTextFiles(OPENFILENAME *ofn);
		mbs::mbsRESULT ExtractBinaryResultsIntoTextFiles(String^ FileName);

		void AbortRun();

		//--------------------------------------------------------------//
		// Scenario Setup And User Interface (GUI or Console)
		//--------------------------------------------------------------//
		mbs::mbsRESULT SetScenarioTitle(String^ Title);
		void SetConfiguration(mbsCONFIG Configuration);
		mbsCONFIG GetConfiguration();
		void SetDuration(mbsHHMMSS Duration);
		void SetDuration(int Seconds);
		mbsHHMMSS GetDuration();
		int	GetDurationSeconds();
		void SetStartTime(mbsHHMMSS StartTime);
		mbsHHMMSS GetStartTime();
		BOOL CalculateRequiredDiskSpace(DWORD *BinStorage, DWORD *TextStorage);
		mbs::mbsRESULT LoadScenario(String ^FileName);
		mbs::mbsRESULT SaveScenario(String ^FileName);



		//-------------------//
		// Environmental Data
		//-------------------//
		double GetBathymetryDepth(double lat, double lon);
		mbs::mbsPosition NewLatLonFromOldPlusMeters(double Lat, double Lon, double MetersY, double Metersx);
		double MetersBetweenCoordinates(double Lat1, double Lon1, double Lat2, double Lon2);

		mbs::mbsRESULT LoadBathymetryFromTextFile(String^ FileName);
		void ClearBathymetry();
		BOOL BathymetryLoaded();
		String^ GetBathymetryFileName();
		//void BathymetryToTextFile(String^ FileName);/**/ //code//
		ENVMINMAX GetBathymetryExtremes();

		mbs::mbsRESULT LoadTemperatureFromTextFile(String^ FileName);
		void ClearTemperature();
		BOOL TemperatureLoaded();
		String^ GetTemperatureFileName();

		mbs::mbsRESULT LoadSalinityFromTextFile(String^ FileName);
		void ClearSalinity();
		BOOL SalinityLoaded();
		String^ GetSalinityFileName();

		//------------------------------------------------------------//
		// File Functions, Functions Related to File IO, and Variables
		//------------------------------------------------------------//
		mbs::mbsRESULT LoadFromBinFile(String^ FileName);
		mbs::mbsRESULT SaveToBinFile(String^ FileName);

		//------------------//
		// Accesor Functions
		//------------------//

		//---------------------//
		// Scenario Description
		//---------------------//
		mbs::mbsRESULT RunParamsToTextFile(FILE *fd);
		mbs::mbsRESULT GetErrorStatus();
	public:
		// Translates a mbsRESULT into a String
		String^ MbsResultToString(mbs::mbsRESULT Result);
		String^ MbsRunStateToString(mbs::mbsRUNSTATE RunState);
	};

	/*
	public ref class C3mbBathy
	{
	private:
		CBathymetry *m_bathy;
		C3mbs mbsInstance;
	public:
		mbs::mbsRESULT LoadFromTextFile(String^ FileName);
		double GetDepthAtCoordinate(double Lat, double Lon);
		mbs::mbsBATHYVALUE GetValueAtCoordinate(double Latitude, double Longitude);
		mbs::mbsBATHYEXTREMES GetExtremes();
		
	};
*/
	public ref class C3mbsWrapperSpeciesModel
	{
	public:
		C3mbsWrapperSpeciesModel();
	private:
		C3mbs mbsInstance;
	public:
		void VoidFnc();
		void CopyMatrix(MATRIX *Des,  const mbsMATRIX ^Src);
		void CopyMatrix(ARRAY *Des,  const mbsARRAY ^Src);
		void CopyMatrix(ELEMENT *Des,  const mbsELEMENT ^Src);

		mbsMATRIX CopyMatrix(const MATRIX *Src);
		mbsARRAY CopyMatrix(const ARRAY *Src);
		mbsELEMENT CopyMatrix(const ELEMENT *Src);

		int GetShortDescriptionAllowedLength();
		int GetLongCommentAllowedLength();
		unsigned int GetSpeSuperVersion();
		unsigned int GetSpeSubVersion();

		int SaveToBinFile(String ^FileName,  mbsSPECIESMODEL ^SpeMdl);
		int ModelToTextFile(String ^FileName,  mbsSPECIESMODEL ^SpeMdl);
		mbs::mbsRESULT LoadFromBinFile(String ^FileName,  mbsSPECIESMODEL ^SpeMdl);

		String ^GetSpeciesGroupString(int SpeciesGroupType);
		String ^GetSpeciesLatinNameString(int SpeciesNameIndex);
		String ^GetSpeciesEnglishNameString(int SpeciesNameIndex);
		int GetSpeciesGroupCount();
		int GetSpeciesNameListCount();


		mbsSNGLBEHTRANSTRUCT RunBehaviorTransitionControlTest(mbsMATRIX ^M, int BehaviorIndex, int NumTrials);
		mbsSNGLBEHTRANSTRUCT InitialBehaviorControlTest(mbsMATRIX ^M, int NumTrials);


	private:
		void C3mbsWrapperSpeModelToSpeciesModel(mbsSPECIESMODEL ^SpeMdl, CSpeciesModel *CSpeMdl);
		void SpeciesModelToC3mbsWrapperSpeModel(CSpeciesModel *CSpeMdl, mbsSPECIESMODEL ^SpeMdl);
	};
}
