using System;
using System.Collections.Generic;
using System.Text;

namespace MMMBSLib
{
    public class CRateParameters
    {
        //-----------------------//
        // Class Member Variables
        //-----------------------//
        CParamsRateBase m_base = null;
        MODELTYPE m_modelType;
        CParamsRateGaussn m_gauss = new CParamsRateGaussn();
        CParamsRateRandom m_randm = new CParamsRateRandom();
        CParamsRateVectorModel m_vectorMdl = new CParamsRateVectorModel();
        CParamsRateDisabled m_disabled = new CParamsRateDisabled();
        CParamsExternallyModeledRate m_externallyModeled = new CParamsExternallyModeledRate();

        //-----------------//
        // Class Properties
        //-----------------//
        public CParamsRateGaussn gauss { get { return m_gauss; } }
        public CParamsRateRandom randm { get { return m_randm; } }
        public CParamsRateVectorModel vectorMdl { get { return m_vectorMdl; } }
        public string sz { get { return m_base.sz; } }
        public string szType { get { return m_base.szType; } }
        public MODELTYPE modelType
        {
            get { return m_modelType; }
            set
            {
                m_modelType = value;
                switch(m_modelType)
                {
                case MODELTYPE.DISABLED:
                    m_base = m_disabled;
                    break;
                case MODELTYPE.EXTERNALLYMODELED:
                    m_base = m_externallyModeled;
                    break;
                case MODELTYPE.RANDOM:
                    m_base = m_randm;
                    break;
                case MODELTYPE.GAUSSIAN:
                    m_base = m_gauss;
                    break;
                case MODELTYPE.MATRIX:
                    m_base = m_vectorMdl;
                    break;
                }
            }
        }

        //--------------//
        // Class Methods
        //--------------//
        public CRateParameters GetCopy()
        {
            CRateParameters c = new CRateParameters();
            c.m_gauss = m_gauss.GetCopy();
            c.m_randm = m_randm.GetCopy();
            c.m_vectorMdl = m_vectorMdl.GetCopy();
            c.modelType = modelType;
            return c;
        }
    }

    abstract public class CParamsRateBase
    {
        //-----------------//
        // Class Properties
        //-----------------//
        public abstract string sz { get;}
        public abstract string szType { get;}
    }

    public class CParamsRateGaussn: CParamsRateBase
    {
        //-----------------------//
        // Class Member Variables
        //-----------------------//
        double m_mean, m_std, m_coeff;

        //-----------------//
        // Class Properties
        //-----------------//
        public override string sz { get { return "mean: " + m_mean + "\nstd: " + m_std + "\nterm coeff: " + m_coeff; } }
        public override string szType { get { return STRINGCONSTANTS.SZ_GAUSSIAN; } }
        public double mean { get { return m_mean; } set { m_mean = CUtil.Min0OrKeep(value); } }
        public double std { get { return m_std; } set { m_std = CUtil.Min0OrKeep(value); } }
        public double coeff { get { return m_coeff; } set { m_coeff = CUtil.Min0OrKeep(value); } }


        //--------------//
        // Class Methods
        //--------------//
        public CParamsRateGaussn GetCopy()
        {
            CParamsRateGaussn c = new CParamsRateGaussn();
            c.m_mean = m_mean;
            c.m_std = m_std;
            c.m_coeff = m_coeff;
            return c;
        }
    }
    public class CParamsRateRandom: CParamsRateBase
    {
        //-----------------------//
        // Class Member Variables
        //-----------------------//
        double m_max, m_min, m_coeff;

        //-----------------//
        // Class Properties
        //-----------------//
        public override string sz { get { return "max: " + m_max + "\nmin: " + m_min + "\nterm coeff: " + m_coeff; } }
        public override string szType { get { return STRINGCONSTANTS.SZ_RANDOM; } }
        public double max { get { return m_max; } set { m_max = CUtil.Min0OrKeep(value); } }
        public double min { get { return m_min; } set { m_min = CUtil.Min0OrKeep(value); } }
        public double coeff { get { return m_coeff; } set { m_coeff = CUtil.Min0OrKeep(value); } }

        //--------------//
        // Class Methods
        //--------------//
        public CParamsRateRandom GetCopy()
        {
            CParamsRateRandom c = new CParamsRateRandom();
            c.m_max = m_max;
            c.m_min = m_min;
            c.m_coeff = m_coeff;
            return c;
        }
    }

    public class CParamsRateVectorModel: CParamsRateBase
    {
        //-----------------------//
        // Class Member Variables
        //-----------------------//
        CVector m_mtx = new CVector();
        CElement m_step = new CElement();
        CElement m_term = new CElement();


        //-----------------//
        // Class Properties
        //-----------------//
        public Boolean isValid { get { return true; } }
        public CVector vector { get { return m_mtx; } set { m_mtx = value; } }
        public CElement step { get { return m_step; } set { m_step = value; } }
        public CElement termination { get { return m_term; } set { m_term = value; } }

        public override string sz
        {
            get
            {
                return "rate: " + m_mtx.sz + "\nstep: " + m_step.sz + "\nterminate " + m_term.sz;
            }
        }
        public override string szType { get { return STRINGCONSTANTS.SZ_MATRIX; } }


        //--------------//
        // Class Methods
        //--------------//
        public CParamsRateVectorModel GetCopy()
        {
            CParamsRateVectorModel c = new CParamsRateVectorModel();
            if(m_mtx.a == null)
                c.m_mtx.a = null;
            else
                c.m_mtx = m_mtx.GetCopy();
            c.m_step = m_step.GetCopy();
            c.m_term = m_term.GetCopy();
            return c;
        }
    }

    public class CParamsRateDisabled : CParamsRateBase
    {
        //-----------------//
        // Class Properties
        //-----------------//
        public override string sz { get { return "Disabled"; } }
        public override string szType { get { return STRINGCONSTANTS.SZ_DISABLED; } }

        //--------------//
        // Class Methods
        //--------------//
        public CParamsRateDisabled GetCopy()
        {
            // Nothing to copy, really...
            return new CParamsRateDisabled();
        }
    }

    public class CParamsExternallyModeledRate : CParamsRateBase
    {
        //-----------------//
        // Class Properties
        //-----------------//
        public override string sz { get { return "Externally Modeled"; } }
        public override string szType { get { return STRINGCONSTANTS.SZ_EXTRNLLYMDLED; } }

        //--------------//
        // Class Methods
        //--------------//
        public CParamsExternallyModeledRate GetCopy()
        {
            // Nothing to copy, really...
            return new CParamsExternallyModeledRate();
        }
    }


}