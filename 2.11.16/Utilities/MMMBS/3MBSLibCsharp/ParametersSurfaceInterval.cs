using System;
using System.Collections.Generic;
using System.Text;

namespace MMMBSLib
{
    public class CSurfaceIntervalParams
    {
        //-----------------------//
        // Class Member Variables
        //-----------------------//
        CParamsSurfaceIntervalGaussn m_gauss = new CParamsSurfaceIntervalGaussn();
        CParamsSurfaceIntervalVectorMdl m_vctr = new CParamsSurfaceIntervalVectorMdl();
        CParamsSurfaceIntervalBase m_base = null;
        SRFINVMODELTYPE m_modelType;

        //-----------------//
        // Class Properties
        //-----------------//
        public CParamsSurfaceIntervalGaussn gauss { get { return m_gauss; } }
        public CParamsSurfaceIntervalVectorMdl vectorMdl { get { return m_vctr; }}
        public string sz { get { return m_base.sz; } }
        public string szType { get { return m_base.szType; } }
        public SRFINVMODELTYPE type
        {
            get { return m_modelType; }
            set
            {
                m_modelType = value;
                switch(m_modelType)
                {
                case SRFINVMODELTYPE.MATRIX:
                    m_base = m_vctr;
                    break;
                case SRFINVMODELTYPE.GAUSSIAN:
                    m_base = m_gauss;
                    break;
                }
            }
        }

        //--------------//
        // Class Methods
        //--------------//
        public CSurfaceIntervalParams GetCopy()
        {
            CSurfaceIntervalParams c = new CSurfaceIntervalParams();
            c.m_gauss = m_gauss.GetCopy();
            c.m_vctr = m_vctr.GetCopy();
            c.type = type;
            return c;
        }
    }


    abstract public class CParamsSurfaceIntervalBase
    {
        //-----------------//
        // Class Properties
        //-----------------//
        public abstract string sz { get;}
        public abstract string szType { get;}
    }

    public class CParamsSurfaceIntervalGaussn: CParamsSurfaceIntervalBase
    {
        //-----------------------//
        // Class Member Variables
        //-----------------------//
        double m_mean = 0, m_std = 0;

        //-----------------//
        // Class Properties
        //-----------------//
        public override string sz { get { return "mean: " + m_mean + "\nstd: " + m_std; } }
        public override string szType { get { return STRINGCONSTANTS.SZ_GAUSSIAN; } }
        public double mean { get { return m_mean; } set { m_mean = CUtil.Min0OrKeep(value); } }
        public double std { get { return m_std; } set { m_std = CUtil.Min0OrKeep(value); } }

        //--------------//
        // Class Methods
        //--------------//
        public CParamsSurfaceIntervalGaussn GetCopy()
        {
            CParamsSurfaceIntervalGaussn c = new CParamsSurfaceIntervalGaussn();
            c.m_mean = m_mean;
            c.m_std = m_std;
            return c;
        }
    }

    public class CParamsSurfaceIntervalVectorMdl: CParamsSurfaceIntervalBase
    {
        //-----------------------//
        // Class Member Variables
        //-----------------------//
        CVector m_vctr = new CVector();
        CElement m_step = new CElement();

        //-----------------//
        // Class Properties
        //-----------------//
        public Boolean isValid { get { return true; } }
        public CVector vector { get { return m_vctr; } set { m_vctr = value; } }
        public CElement step { get { return m_step; } set { m_step = value; } }
        public override string sz { get { return "surface_interval: " + m_vctr.sz + "\nstep: " + m_step.sz; } }
        public override string szType { get { return STRINGCONSTANTS.SZ_MATRIX; } }

        //--------------//
        // Class Methods
        //--------------//
        public CParamsSurfaceIntervalVectorMdl GetCopy()
        {
            CParamsSurfaceIntervalVectorMdl c = new CParamsSurfaceIntervalVectorMdl();
            c.m_vctr = m_vctr.GetCopy();
            c.m_step = m_step.GetCopy();
            return c;
        }
    }
}