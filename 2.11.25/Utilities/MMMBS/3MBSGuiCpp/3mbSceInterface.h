#ifndef _3MBENVIRONMENTFUNCTIONS_H
#define _3MBENVIRONMENTFUNCTIONS_H
//#include "Bathymetry.h"
#include "3mb.h"
#include "Scenario.h"

typedef struct AnimatStateFileOutPointer
{
	ANIMATSTATE_FILEOUT *p;
}ANIMATSTATE_FILEOUT_PTR;

typedef struct SimplePod // simple pod
{
	//LinkedList <ANIMATSTATE_FILEOUT> a; // a for animat
	LinkedList <ANIMATSTATE_FILEOUT_PTR> a; // a for animat
}SIMPLEPOD;

/*
typedef struct SimpleInd // simple individual
{
	// Single animat
	//ANIMATSTATE_FILEOUT a;
	ANIMATSTATE_FILEOUT_PTR a;
}SIMPLEINDIVIDUAL;
*/
typedef struct SimpleSpe
{
	// Pod list
	LinkedList <SIMPLEPOD> p;
	// Individual list
	LinkedList <ANIMATSTATE_FILEOUT_PTR> i;
}SIMPLESPECIES; // simple species

typedef struct SimplePop
{
	LinkedList <SIMPLESPECIES> s; // i for individual
}SIMPLEPOP;  // simple population

#if 0
typedef struct UniqueIDMap
{
	int uniqueID;
	int species;
	int podID;
	int podMemberNumber;
	int individualNumber;
}UNIQUEIDMAP;
#endif

class CBitmapEnvFunctions
{
private:
	static void DeallocateSimplePop();
	static CScenario *m_sce;
	static SCENARIOUTPUTFILEINF *m_playback;
	static CFileManager m_fileMgr;

	static BATHYUSESTATE m_state;
	static SIMPLEPOP m_pop;
	static int m_numSoundSourcePresent;

	static void BogusFunction();
public:
	CBitmapEnvFunctions(void);
	~CBitmapEnvFunctions(void);

	static SIMPLEPOP *GetPopRef();
	static CScenario *GetSceRef();

	static double GetMinimumSeededingDepth(int SpeIndex);
	static double GetShoreFollowingDepth(int SpeIndex);
	static BOOL SoundSourceIsPresent();
	static void InitializeSeeding(CScenario *pScenario);
	static RESLT InitializePlayback(TCHAR *szFileName, SCENARIOUTPUTFILEINF *pPlaybackInf);
	static void Uninitialize();
	static BATHYVALUE GetValueAtCoordinate(double Lat, double Lon);
	static BOOL BathyFileLoaded();
	static BOOL SaltinityFileLoaded();
	static BOOL TemperatureFileLoaded();
	static BOOL AnyEnvFileLoaded();
	static BATHYUSESTATE GetUsageState();


	static double GetBathymtryWaterSurfaceAreaMeters();
	static double GetTotalBathySufaceAreaMeters();
	static double GetBathymetryLandSufaceAreaMeters();

	static int GetSpeciesCount();
	static int GetAnimatCount();
	static double GetAnimatDensity();
	static int GetAnimatCount(int SpeciesIndex);
	static int GetIndividualCount();
	static int GetIndividualCount(int SpeciesIndex);
	static int GetPodCount(int SpeciesIndex);
	static int GetPodMemberCount(int SpeciesIndex, int PodIndex);

	static PODLEADERTYPE GetPodLeaderType(int SpeciesIndex, int PodIndex);
	static double GetPodLeaderFocalDistance(int SpeciesIndex, int PodIndex);

	static BATHYVALUE GetBathymetryValues(double Lat, double Lon);
	static void GetSpeciesDisplayTitle(int Index, char *szBuff, DWORD BufferSize);

	static ENVDATAPOINTCOUNT GetDataPointCounts(BOOL GetSlope);
	static RESLT GetRawDataCopy(RAWENVIRONMENTALDATA *pEnvironmentalData, BOOL GetSlope);
	static void DeallocateRawDataCopyMemory(RAWENVIRONMENTALDATA *pEnvironmentalData);
	static BATHYEXTREMES GetBathyExtremes();

	static INHABITINF *GetAnimatInitialCoordinates(INHABITINF *IC = NULL);
	static INHABITINF *GetAnimatInitialCoordinates(int SpeciesIndex, int BufferLen, INHABITINF *IC = NULL);
	static INHABITINF *GetPodInitialCoordinates(int SpeciesIndex, int PodIndex, INHABITINF *IC = NULL);
	static INHABITINF GetPodMemberInitialCoordinate(int SpeciesIndex, int PodIndex, int MemberIndex);
	static INHABITINF GetIndividualInitialCoordinate(int SpeciesIndex, int IndividualIndex);
	static INHABITINF *GetIndividualInitialCoordinates(int SpeciesIndex,  INHABITINF *IC = NULL);


	static BOOL SpeciesIsASoundSourceModel(int SpeciesIndex);
	static BOOL SetSpeciesAsASoundSource(int SpeciesIndex);
	static BOOL AnimatIsASoundSource(int AnimatIndex);
	static int GetTotalSoundSourceCount();
};
#endif	