using System;
using System.Collections.Generic;
using System.Text;

namespace MMMBSLib
{
    public class CDirectionalParameters
    {
        //-----------------------//
        // Class Member Variables
        //-----------------------//
        CParamsRndmWalk m_rndmWalk = new CParamsRndmWalk();
        CParamsDirectionCRndmWalk m_corrRndmWlk = new CParamsDirectionCRndmWalk();
        CParamsDirectionCRndmWalkDirBias m_corrRndWlkDb = new CParamsDirectionCRndmWalkDirBias();
        CParamsDirectionVectMdl m_matrx = new CParamsDirectionVectMdl();
        protected CParamsDirectionBase m_base = null;
        DIRECTIONMODELTYPE m_modelType;

        //-----------------------//
        // Class Properties
        //-----------------------//
        public CParamsRndmWalk rndmWalk { get { return m_rndmWalk; } }
        public CParamsDirectionCRndmWalk correlatedRndmWalk { get { return m_corrRndmWlk; } }
        public CParamsDirectionCRndmWalkDirBias correlatedRndmWalkDirBias { get { return m_corrRndWlkDb; } }
        public CParamsDirectionVectMdl matrix { get { return m_matrx; } }

        public DIRECTIONMODELTYPE type
        {
            get { return m_modelType; }
            set
            {
                m_modelType = value;
                switch(m_modelType)
                {
                case DIRECTIONMODELTYPE.VECTOR:
                    m_base = m_matrx;
                    m_matrx.biasVectorEnabled = false;
                    break;
                case DIRECTIONMODELTYPE.VECTOR_DIRBIAS:
                    m_base = m_matrx;
                    m_matrx.biasVectorEnabled = true;
                    break;
                case DIRECTIONMODELTYPE.RANDOM_WALK:
                    m_base = m_rndmWalk;
                    break;
                case DIRECTIONMODELTYPE.CORR_RAND_WALK:
                    m_base = m_corrRndmWlk;
                    break;
                case DIRECTIONMODELTYPE.CORR_RAND_WALK_DIR_BIAS:
                    m_base = m_corrRndWlkDb;
                    break;
                }
            }
        }
        public string sz { get { return m_base.sz; } }
        public string szType { get { return m_base.szType; } }


        //-----------------------//
        // Class Methods
        //-----------------------//
        public CDirectionalParameters GetCopy()
        {
            CDirectionalParameters c = new CDirectionalParameters();
            c.type = type;
            c.m_rndmWalk = m_rndmWalk.GetCopy();
            c.m_corrRndmWlk = m_corrRndmWlk.GetCopy();
            c.m_corrRndWlkDb = m_corrRndWlkDb.GetCopy();
            c.m_matrx = m_matrx.GetCopy();

            switch(c.m_modelType)
            {
            case DIRECTIONMODELTYPE.RANDOM_WALK:
                c.m_base = c.m_rndmWalk;
                break;
            case DIRECTIONMODELTYPE.CORR_RAND_WALK:
                c.m_base = c.m_corrRndmWlk;
                break;
            case DIRECTIONMODELTYPE.CORR_RAND_WALK_DIR_BIAS:
                c.m_base = c.m_corrRndWlkDb;
                break;
            case DIRECTIONMODELTYPE.VECTOR:
            case DIRECTIONMODELTYPE.VECTOR_DIRBIAS:
                c.m_base = c.m_matrx;
                break;
            }
            return c;
        }
    }


    abstract public class CParamsDirectionBase
    {
        //-----------------------//
        // Class Properties
        //-----------------------//
        public abstract string sz { get;}
        public abstract string szType { get;}
    }

    public class CParamsRndmWalk: CParamsDirectionBase
    {
        //-----------------------//
        // Class Member Variables
        //-----------------------//
        double m_coeff = 0;

        //-----------------------//
        // Class Properties
        //-----------------------//
        public override string sz { get { return "term coeff: " + m_coeff; } }
        public override string szType { get { return STRINGCONSTANTS.SZ_RANDOMWALK; } }
        public double coeff { get { return m_coeff; } set { m_coeff = CUtil.Min0OrKeep(value); } }

        //-----------------------//
        // Class Methods
        //-----------------------//
        public CParamsRndmWalk GetCopy()
        {
            CParamsRndmWalk c = new CParamsRndmWalk();
            c.m_coeff = m_coeff;
            return c;
        }
    }

    public class CParamsDirectionCRndmWalk: CParamsDirectionBase
    {
        //-----------------------//
        // Class Member Variables
        //-----------------------//
        double m_pert = 0, m_coeff = 0;

        //-----------------------//
        // Class Properties
        //-----------------------//
        public override string sz { get { return "pert: " + m_pert + "\nterm coeff: " + m_coeff; } }
        public override string szType { get { return STRINGCONSTANTS.SZ_CORR_RAND_WALK; } }
        public double pert { get { return m_pert; } set { m_pert = CUtil.Min0OrKeep(value); } }
        public double coeff { get { return m_coeff; } set { m_coeff = CUtil.Min0OrKeep(value); } }

        //-----------------------//
        // Class Methods
        //-----------------------//
        public CParamsDirectionCRndmWalk GetCopy()
        {
            CParamsDirectionCRndmWalk c = new CParamsDirectionCRndmWalk();
            c.m_pert = m_pert;
            c.m_coeff = m_coeff;
            return c;
        }
    }

    public class CParamsDirectionCRndmWalkDirBias: CParamsDirectionBase
    {
        //-----------------------//
        // Class Member Variables
        //-----------------------//
        double m_pert = 0, m_biasDir = 0, m_bias = 0, m_arcStep = 0, m_coeff = 0;

        //-----------------------//
        // Class Properties
        //-----------------------//
        public override string sz
        {
            get
            {
                return "pert: " + m_pert + "\ndirectional bias:" + m_biasDir + "  bias:" + m_bias +
                    "\narc step: " + m_arcStep + "\nterm coeff: " + m_coeff;
            }
        }
        public override string szType { get { return STRINGCONSTANTS.SZ_CORR_RAND_WALKDB; } }
        public double pert { get { return m_pert; } set { m_pert = CUtil.Min0OrKeep(value); } }
        public double biasDir { get { return m_biasDir; } set { m_biasDir = CUtil.Min0OrKeep(value); } }
        public double bias { get { return m_bias; } set { m_bias = CUtil.Min0OrKeep(value); } }
        public double arcStep { get { return m_arcStep; } set { m_arcStep = CUtil.Min0OrKeep(value); } }
        public double coeff { get { return m_coeff; } set { m_coeff = CUtil.Min0OrKeep(value); } }


        //-----------------------//
        // Class Methods
        //-----------------------//
        public CParamsDirectionCRndmWalkDirBias GetCopy()
        {
            CParamsDirectionCRndmWalkDirBias c = new CParamsDirectionCRndmWalkDirBias();
            c.m_pert = m_pert;
            c.m_biasDir = m_biasDir;
            c.m_bias = m_bias;
            c.m_arcStep = m_arcStep;
            c.m_coeff = m_coeff;
            return c;
        }
    }



    public class CParamsDirectionVectMdl: CParamsDirectionBase
    {
        //-----------------------//
        // Class Member Variables
        //-----------------------//
        CVector m_directionVector = new CVector();
        CMatrix m_bias = new CMatrix();
        CElement m_term = new CElement();
        bool m_biasVectorEnabled = true;

        //------------------//
        // Class Constructor
        //------------------//
        public CParamsDirectionVectMdl() { SetDefaultModel(); }

        //-----------------//
        // Class Properties
        //-----------------//
        public Boolean isValid { get { return true; } }
        public Boolean biasVectorEnabled { get { return m_biasVectorEnabled; } set { m_biasVectorEnabled = value; } }
        public CVector directionVector { get { return m_directionVector; } set { m_directionVector = value; } }
        public CMatrix directionBiasMatrix { get { return m_bias; } set { m_bias = value; } }
        public CElement term { get { return m_term; } set { m_term = value; } }

        public override string sz
        {
            get
            {
                if(m_biasVectorEnabled == true)
                    return "direction: " + m_directionVector.sz + "\nbias: " + m_bias.sz + "\nterminate " + m_term.sz;
                return "direction: " + m_directionVector.sz + "\nterminate " + m_term.sz;
            }
        }
        public override string szType
        {
            get
            {
                if(m_biasVectorEnabled == true)
                    return STRINGCONSTANTS.SZ_DIRECTIONMATRIXWITHBIAS;
                return STRINGCONSTANTS.SZ_DIRECTIONMATRIXNOBIAS;
            }
        }

        //--------------//
        // Class Methods
        //--------------//
        public CParamsDirectionVectMdl GetCopy()
        {
            CParamsDirectionVectMdl c = new CParamsDirectionVectMdl();
            c.m_directionVector = m_directionVector.GetCopy();
            c.m_bias = m_bias.GetCopy();
            c.m_term = m_term.GetCopy();
            c.m_biasVectorEnabled = m_biasVectorEnabled;
            return c;
        }

        public void SetDefaultModel()
        {
            m_directionVector.a = MBSDEFAULTS.DIRECTIONAL_PROB_VECTOR0;
            m_bias.a = MBSDEFAULTS.DIRECTIONAL_PROB_BIAS;
            m_term.a = MBSDEFAULTS.DIRECTIONAL_TERMINATE;
            m_biasVectorEnabled = true;
        }
    }
}