// SpeciesModel.h: interface for the CSpeciesModel class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_SPECIES_H__B72C8383_0C24_45C8_BC8D_767648DED8AA__INCLUDED_)
#define AFX_SPECIES_H__B72C8383_0C24_45C8_BC8D_767648DED8AA__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "datatypes.h"
#include "staticLib.h"

/*----------------------------------------------------------------------------------------
	STRUCT: CRWDB_PARAM

	DESCRIPTION:
		Used for passing initialization parameters to each animat during calls to
		function ().

	STRUCT MEMBERS:
		M:
			A pointer to the direction model governing the animat. 

		Bearing:
			Animat's bearing.

		DirBiasOverride:
			A pointer to a biasing override.  If NULL, this is ignored.  If set, the
			value pointed to overrides the value contained in crwdbDirectionOfBias in the
			model M.
----------------------------------------------------------------------------------------*/
class CSpeciesModel  
{
public:
	//-------------//
	// Constructors
	//-------------//
	CSpeciesModel();
	virtual ~CSpeciesModel();

private:
	C3mbStaticsLib m_staticLib;
	C3MBRandom m_C3MBRandomDefault;

public:

	//-----------------//
	// Member Variables
	//-----------------//
	SPECIES_MDL m_speciesModel;
	BOOL m_needSave;
	C3MBRandom *m_pC3MBRandomRef;

	//---------------------------------------------//
	// memory deallocation
	//---------------------------------------------//
	void  OverrideRandomInstance(C3MBRandom *pC3MBRandomRef);
	void  ClearMemberVariables();
	BOOL  DefaultMemberVariables();

	void CopyModel(CSpeciesModel *pCpyMdl, const CSpeciesModel *pMdl);
	void CopyMatrix(MATRIX *pCpyM, const MATRIX *pM);
	void CopyMatrix(ARRAY *pCpyM, const ARRAY *pM);
	void CopyMatrix(ELEMENT *pCpyM, const ELEMENT *pM);


	//-----------------------------//
	// Simulation/Modeling Routines
	//-----------------------------//
	double ProbTermination(double TermCoeff, double TimeLapse);

	double GetMaxRate(const RATEMDL *R);
	double GetMaxRate(const RATEVCTRMDLPARAM *R);
	double GetMaxRate(const RANDOM *R);
	double GetMaxRate(const GAUSS *R);

	double RateModel(const RATEMDL *R);
	double RateModel(const RATEVCTRMDLPARAM *R);
	double RateModel(const RANDOM *R);
	double RateModel(const GAUSS *R);
	BOOL RateTerminates(const RANDOM *Rp, const RATESTATE *Rs);
	BOOL RateTerminates(const GAUSS *Rp, const RATESTATE *Rs);
	BOOL RateTerminates(const RATEVCTRMDLPARAM *Rp, const RATESTATE *Rs);
	BOOL RateTerminates(const RATEMDL *Rp, const RATESTATE *Rs);

	int IntialBehavior(int AbsClock, MATRIX *Md);

	// Behavior transiton functions //
private:
	void TestBehaviorTransitionControlTest(MATRIX *M, int BehaviorIndex);
	void RunInitialBehaviorControlTest(MATRIX *M);
public:
	void RunBehaviorTransitionControlTest(SNGLBEHTRANSTRUCT *pBehTrans, MATRIX *M, int BehaviorIndex, int NumTrials);
	void InitialBehaviorControlTest(SNGLBEHTRANSTRUCT *pBehTrans, MATRIX *M, int NumTrials);
	int BehaviorTransition(BEHTRANS_TERM_MODEL ModelType, const TRANSITNSTATE *Bs, const MATRIX *Btm, int AbsClock);

	double DepthModel(const DEPTHPARAM *D);
	double DepthModel(const RANDOM *D);
	double DepthModel(const GAUSS *D);
	double DepthModel(const VCTRMDLPARAM *D);

	int NumberOfReversals(const REVERSAL_DEF *Rp);
	int NumberOfReversals(const REVVCTRMDLPARAM *R);
	int NumberOfReversals(const REVERSAL_GAUSS *R);
	int NumberOfReversals(const REVERSAL_RND *R);

	BOOL DirectionTerminates(const DIRCTNMDL *Dp, const DIRECTIONSTATE *Ds);
	double Direction(const DIRCTNMDL *Dp, const DIRECTIONSTATE *Ds);
	double Direction(const RANDOMWALK *M);
	double Direction(const CORRANDWALK *M, double Bearing);
	double Direction(const CORRANDWALKDB *M, double Bearing);
	double Direction(const DIRVCTRMDLPARAM *M, const DIRECTIONAL_MODEL_TYPE Type, double Bearing);

	int SurfaceInterval(const SURFINTRVLPARAM *Sp);
	int SurfaceInterval(const GAUSS *S);
	int SurfaceInterval(const VCTRMDLPARAM *S);

	int ReversalDuration(const REVERSAL_DEF *Rp);
	int ReversalDuration(const REVERSAL_RND *Rp);
	int ReversalDuration(const REVERSAL_GAUSS *Rp);
	int ReversalDuration(const REVVCTRMDLPARAM *Rp);

	//SUBMODEL_VECTOR_STATUS GetSubModelVectorStatus(BEHTRANSMDL* Mdl, MATRIX *Initial);
	//SUBMODEL_VECTOR_STATUS GetSubModelVectorStatus(BEHAVIOR_MODEL_GETSREPLACEd *Mdl);
	//SUBMODEL_VECTOR_STATUS GetSubModelVectorStatus(RATE_MODEL* Mdl);
	//SUBMODEL_VECTOR_STATUS GetSubModelVectorStatus(DIRECTION_MODEL_thisGoesAway* Mdl);
	//SUBMODEL_VECTOR_STATUS GetSubModelVectorStatus(DEPTH_MODEL_goesAway* Mdl);
	//SUBMODEL_VECTOR_STATUS GetSubModelVectorStatus(SURFACE_INTERVAL_MODEL_goesAway* Mdl);
	//SUBMODEL_VECTOR_STATUS GetSubModelVectorStatus(REVERSAL_MODEL_goesAway* Mdl);


	//----------------------//
	// Public File Functions
	//----------------------//
	RESLT SaveToBinFile(TCHAR *FileName);
	RESLT SaveToBinFile(HANDLE hdl);
	RESLT SaveToBinFile(TCHAR *FileName, SPECIES_MDL* pSpeMdl, BOOL UpdateSaveDateTime = TRUE);
	RESLT SaveToBinFile(HANDLE hdl, SPECIES_MDL* pSpeMdl, BOOL UpdateSaveDateTime = TRUE);
	RESLT LoadFromBinFile(TCHAR *FileName);
	RESLT LoadFromBinFile(HANDLE Hdl);
	RESLT LoadFromFile(TCHAR *FileName);
	RESLT ModelToText(TCHAR *FileName);
	RESLT ModelToText(FILE *Fd);
	
	int MatrixToText(MATRIX *M, TCHAR *szBuffer, int BufferSize);
	int MatrixToText(ARRAY *M, TCHAR *szBuffer, int BufferSize);
	int MatrixToText(ELEMENT *M, TCHAR *szBuffer, int BufferSize);

	int TextToMatrix(MATRIX *M, TCHAR *szBuffer);
	int TextToMatrix(ARRAY *M, TCHAR *szBuffer);
	int TextToMatrix(ELEMENT *M, TCHAR *szBuffer);


	//--------------------//
	// Resource Allocation
	//--------------------//
	BOOL AllocateMatrix(MATRIX *M, int NumRows, int NumCols);
	BOOL AllocateMatrix(ARRAY *M, int NumRows, int NumCols);
	BOOL AllocateMatrix(ELEMENT *M, int NumRows, int NumCols);
    
	//----------------------//
	// Resource Deallocation
	//----------------------//
	void FreeMatrix(MATRIX *M);
	void FreeMatrix(ARRAY *M);
	void FreeMatrix(ELEMENT *M);


	void ClearMatrix(MATRIX *M);
	void ClearMatrix(ARRAY *M);
	void ClearMatrix(ELEMENT *M);

	RESLT WriteVector(HANDLE hd, ELEMENT *M);
	RESLT WriteVector(HANDLE hd, MATRIX *M);
	RESLT WriteVector(HANDLE hd, ARRAY *M);
	RESLT ReadVector(HANDLE hd, ELEMENT *M);
	RESLT ReadVector(HANDLE hd, MATRIX *M);
	RESLT ReadVector(HANDLE hd, ARRAY *M);

private:

	//---------------//
	// File Functions
	//---------------//
	RESLT ReadTextVectorModel(TCHAR *FileName);
	RESLT ProcessTextVectorModel(FILE *Fd, ARRAY *M);
	RESLT ProcessTextVectorModel(FILE *Fd, MATRIX *M);
	RESLT ProcessTextVectorModel(FILE *Fd, ELEMENT *M);
	RESLT ProcessNextLine(FILE *Fd, TCHAR *Buffer, int MaxLength, int *SringLength);

	//------------------//
	// Parsing functions
	//------------------//
	void RemoveUnneededCharacters(TCHAR *szBuff);
	int  CountNumRows(TCHAR *szBuff);
	int  CountNumColums(TCHAR *szBuff);

	//----------------//
	// Output Routines
	//----------------//
	RESLT MatrixToText(MATRIX *M, TCHAR *MatrixTitle, FILE *Fd);
	RESLT MatrixToText(ARRAY *M, TCHAR *MatrixTitle, FILE *Fd);
	RESLT MatrixToText(ELEMENT *M, TCHAR *MatrixTitle, FILE *Fd);

	//-------------------------//
	// Private Member Variables
	//-------------------------//
	//TCHAR   m_szFileName[SIZE_128]; // The display name of this file (for the GUI).
};
#endif // !defined(AFX_SPECIES_H__B72C8383_0C24_45C8_BC8D_767648DED8AA__INCLUDED_)