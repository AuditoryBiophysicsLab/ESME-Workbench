using System;
using System.Collections.Generic;
using System.Text;

namespace MMMBSLib
{
    public class CDepthParameters
    {
        //-----------------------//
        // Class Member Variables
        //-----------------------//
        CParamsDepthGaussn m_gauss = new CParamsDepthGaussn();
        CParamsDepthRandom m_randm = new CParamsDepthRandom();
        CParamsDepthVectrMdl m_vectorMdl = new CParamsDepthVectrMdl();
        CParamsDepthBase m_base = null;
        MODELTYPE m_modelType;

        //-----------------//
        // Class Properties
        //-----------------//
        public CParamsDepthGaussn gauss { get { return m_gauss; } }
        public CParamsDepthRandom randm { get { return m_randm; } }
        public CParamsDepthVectrMdl vectorMdl { get { return m_vectorMdl; } }
        public string sz { get { return m_base.sz; } }
        public string szType { get { return m_base.szType; } }
        public MODELTYPE type
        {
            get { return m_modelType; }
            set
            {
                m_modelType = value;
                switch(m_modelType)
                {
                case MODELTYPE.MATRIX:
                    m_base = m_vectorMdl;
                    break;
                case MODELTYPE.RANDOM:
                    m_base = m_randm;
                    break;
                case MODELTYPE.GAUSSIAN:
                    m_base = m_gauss;
                    break;
                }
            }
        }
        
        //--------------//
        // Class Methods
        //--------------//
        public CDepthParameters GetCopy()
        {
            CDepthParameters c = new CDepthParameters();
            c.m_gauss = m_gauss.GetCopy();
            c.m_randm = m_randm.GetCopy();
            c.m_vectorMdl = m_vectorMdl.GetCopy();
            c.m_modelType = m_modelType;

            switch(m_modelType)
            {
            case MODELTYPE.GAUSSIAN:
                c.m_base = c.m_gauss;
                break;
            case MODELTYPE.RANDOM:
                c.m_base = c.m_randm;
                break;
            case MODELTYPE.MATRIX:
                c.m_base = c.m_vectorMdl;
                break;
            }
            return c;
        }
    }

    abstract public class CParamsDepthBase
    {
        //-----------------//
        // Class Properties
        //-----------------//
        public abstract string sz { get;}
        public abstract string szType { get;}
    }


    public class CParamsDepthGaussn: CParamsDepthBase
    {
        //-----------------------//
        // Class Member Variables
        //-----------------------//
        double m_mean = 0, m_std = 0;

        //-----------------//
        // Class Properties
        //-----------------//
        public override string sz { get { return "mean: " + m_mean + "\nstddev: " + m_std; } }
        public override string szType { get { return STRINGCONSTANTS.SZ_GAUSSIAN; } }
        public double mean { get { return m_mean; } set { m_mean = CUtil.Min0OrKeep(value); } }
        public double std { get { return m_std; } set { m_std = CUtil.Min0OrKeep(value); } }

        //--------------//
        // Class Methods
        //--------------//
        public CParamsDepthGaussn GetCopy()
        {
            CParamsDepthGaussn c = new CParamsDepthGaussn();
            c.m_mean = m_mean;
            c.m_std = m_std;
            return c;
        }

    }
    public class CParamsDepthRandom: CParamsDepthBase
    {
        //-----------------------//
        // Class Member Variables
        //-----------------------//
        double m_max = 0;

        //-----------------//
        // Class Properties
        //-----------------//
        public override string sz { get { return "max: " + m_max; } }
        public override string szType { get { return STRINGCONSTANTS.SZ_RANDOM; } }
        public double max { get { return m_max; } set { m_max = CUtil.Min0OrKeep(value); } }

        //--------------//
        // Class Methods
        //--------------//        
        public CParamsDepthRandom GetCopy()
        {
            CParamsDepthRandom c = new CParamsDepthRandom();
            c.m_max = m_max;
            return c;
        }
    }

    public class CParamsDepthVectrMdl: CParamsDepthBase
    {
        //-----------------------//
        // Class Member Variables
        //-----------------------//
        CVector m_vctr = new CVector();
        CElement m_step = new CElement();

        //-----------------//
        // Class Properties
        //-----------------//
        public override string sz { get { return "depth: " + m_vctr.sz + "\nstep: " + m_step.sz; } }
        public override string szType { get { return STRINGCONSTANTS.SZ_MATRIX; } }
        public Boolean isValid { get { return true; } }
        public CVector vector { get { return m_vctr; } set { m_vctr = value; } }
        public CElement step { get { return m_step; } set { m_step = value; } }

        //--------------//
        // Class Methods
        //--------------//        
        public CParamsDepthVectrMdl GetCopy()
        {
            CParamsDepthVectrMdl c = new CParamsDepthVectrMdl();
            c.m_vctr = m_vctr.GetCopy();
            c.m_step = m_step.GetCopy();
            return c;
        }
    }

}