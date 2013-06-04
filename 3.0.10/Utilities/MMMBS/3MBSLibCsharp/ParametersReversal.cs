using System;
using System.Collections.Generic;
using System.Text;

namespace MMMBSLib
{
    public class CReversalParameters
    {
        //-----------------------//
        // Class Member Variables
        //-----------------------//
        Boolean m_enabled = false;
        //Boolean m_diveRateEnabled = false;
        REVERSAL_DIVE_RATE_TYPE m_reversalDiveRateType;
        CParamsRateGaussn m_diveRate = new CParamsRateGaussn();
        CParamsRateGaussn m_ascentRate = new CParamsRateGaussn();
        CParamsReversalGaussn m_gauss = new CParamsReversalGaussn();
        CParamsReversalRandom m_randm = new CParamsReversalRandom();
        CParamsReversalVectorMdl m_vctr = new CParamsReversalVectorMdl();
        CParamsReversalBase m_base;
        MODELTYPE m_modelType;

        //-----------------//
        // Class Properties
        //-----------------//
        public Boolean enabled { get { return m_enabled; } set { m_enabled = value; } }
        //public Boolean diveRateEnabled { get { return m_diveRateEnabled; } set { m_diveRateEnabled = value; } }
        public REVERSAL_DIVE_RATE_TYPE reversalDiveRateType { get { return m_reversalDiveRateType; } set { m_reversalDiveRateType = value; } }
        public CParamsRateGaussn diveRate { get { return m_diveRate; } }
        public CParamsRateGaussn ascentRate { get { return m_ascentRate; } }
        public CParamsReversalGaussn gauss { get { return m_gauss; } }
        public CParamsReversalRandom randm { get { return m_randm; } }
        public CParamsReversalVectorMdl vector { get { return m_vctr; } }


        public string sz
        { 
            get
            { 
                string sz = m_base.sz;
                //sz = sz + "\nOverride nrml dive rates \n";m_ascentRate
                switch(m_reversalDiveRateType)
                {
                    case REVERSAL_DIVE_RATE_TYPE.NO_INDEPENDENT:
                        sz = sz + "\nUses behavior dive rates"; // "\nRev Dive Rate: disabled";
                        break;
                    case REVERSAL_DIVE_RATE_TYPE.INDEPENDENT:
                        sz = sz + "\nDive rate  mean:" + m_diveRate.mean + " std:" + m_diveRate.std;
                        break;
                    case REVERSAL_DIVE_RATE_TYPE.INDEPENDENT_DIVE_AND_ASCENT:
                        sz = sz + "\nDescent rate mean:" + m_diveRate.mean + " std:" + m_diveRate.std;
                        sz = sz + "\nAscent rate  mean:" + m_ascentRate.mean + " std:" + m_ascentRate.std;
                        break;
                }
                return sz;
            }
        }


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
                    m_base = m_vctr;
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
        public CReversalParameters GetCopy()
        {
            CReversalParameters c = new CReversalParameters();
            c.m_gauss = m_gauss.GetCopy();
            c.m_randm = m_randm.GetCopy();
            c.m_vctr = m_vctr.GetCopy();
            c.m_enabled = m_enabled;
            //c.m_diveRateEnabled = m_diveRateEnabled;
            c.reversalDiveRateType = m_reversalDiveRateType;
            //c.m_diveRate
            c.m_diveRate = m_diveRate.GetCopy();
            c.m_ascentRate = m_ascentRate.GetCopy();
            c.type = type;
            return c;
        }
    }
    abstract public class CParamsReversalBase
    {
        //-----------------//
        // Class Properties
        //-----------------//
        public abstract string sz { get;}
        public abstract string szType { get;}

    }


    public class CParamsReversalGaussn: CParamsReversalBase
    {
        //-----------------------//
        // Class Member Variables
        //-----------------------//
        double m_meanCnt = 0, m_stdCnt = 0, m_prob = 0, m_meanTime = 0, m_stdTime = 0;

        //-----------------------//
        // Class Properties
        //-----------------------//
        public override string sz
        {
            get
            {
                return "Count: (mean:" + m_meanCnt + ",std:" + m_stdCnt + ")\nProb:" + m_prob +
                    "\nDur: (mean:" + m_meanTime + ",std:" + m_stdTime + ")";
            }
        }
        public override string szType { get { return STRINGCONSTANTS.SZ_GAUSSIAN; } }
        public double meanCnt { get { return m_meanCnt; } set { m_meanCnt = CUtil.Min0OrKeep(value); } }
        public double stdCnt { get { return m_stdCnt; } set { m_stdCnt = CUtil.Min0OrKeep(value); } }
        public double prob { get { return m_prob; } set { m_prob = CUtil.Min0OrKeep(value); } }
        public double meanTime { get { return m_meanTime; } set { m_meanTime = CUtil.Min0OrKeep(value); } }
        public double stdTime { get { return m_stdTime; } set { m_stdTime = CUtil.Min0OrKeep(value); } }

        //-----------------------//
        // Class Methods
        //-----------------------//
        public CParamsReversalGaussn GetCopy()
        {
            CParamsReversalGaussn c = new CParamsReversalGaussn();
            c.m_meanCnt = m_meanCnt;
            c.m_stdCnt = m_stdCnt;
            c.m_prob = m_prob;
            c.m_meanTime = m_meanTime;
            c.m_stdTime = m_stdTime;
            return c;
        }
    }

    public class CParamsReversalRandom: CParamsReversalBase
    {
        //-----------------------//
        // Class Member Variables
        //-----------------------//
        int m_minCnt = 0, m_maxCnt = 0;
        double m_prob = 0, m_meanTime = 0, m_stdTime = 0;

        //-----------------//
        // Class Properties
        //-----------------//
        public override string sz
        {
            get
            {
                return "Count: (min: " + m_minCnt + ",max: " + m_maxCnt + ")\nProb: " + m_prob +
                    "\nDur: (mean: " + m_meanTime + ",std: " + m_stdTime + ")";
            }
        }
        public override string szType { get { return STRINGCONSTANTS.SZ_RANDOM; } }
        public int minCnt { get { return m_minCnt; } set { m_minCnt = CUtil.Min0OrKeep(value); } }
        public int maxCnt { get { return m_maxCnt; } set { m_maxCnt = CUtil.Min0OrKeep(value); } }
        public double prob { get { return m_prob; } set { m_prob = CUtil.Min0OrKeep(value); } }
        public double meanTime { get { return m_meanTime; } set { m_meanTime = CUtil.Min0OrKeep(value); } }
        public double stdTime { get { return m_stdTime; } set { m_stdTime = CUtil.Min0OrKeep(value); } }

        //--------------//
        // Class Methods
        //--------------//
        public CParamsReversalRandom GetCopy()
        {
            CParamsReversalRandom c = new CParamsReversalRandom();
            c.m_maxCnt = m_maxCnt;
            c.m_minCnt = m_minCnt;
            c.m_prob = m_prob;
            c.m_meanTime = m_meanTime;
            c.m_stdTime = m_stdTime;
            return c;
        }
    }

    public class CParamsReversalVectorMdl: CParamsReversalBase
    {
        //-----------------------//
        // Class Member Variables
        //-----------------------//
        CElement m_prob = new CElement(); // reversal probability array
        CVector m_vector = new CVector(); // reversal matrix
        CVector m_timeVctr = new CVector();
        CElement m_timeStep = new CElement();

        //-----------------------//
        // Class Properties
        //-----------------------//
        public override string sz
        {
            get
            {
                return "Count: " + m_vector.sz + ", Prob: " + m_prob.sz + 
                    "\nDur: " + m_timeVctr.sz + ", Step: " + m_timeStep.sz;
            }
        }
        public override string szType { get { return STRINGCONSTANTS.SZ_MATRIX; } }

        public Boolean isValid
        {
            get { return true; }
        }
        public CElement probabilityElement { get { return m_prob; } set { m_prob = value; } }
        public CVector countVector { get { return m_vector; } set { m_vector = value; } }
        public CVector durationVector { get { return m_timeVctr; } set { m_timeVctr = value; } }
        public CElement durationStepElement { get { return m_timeStep; } set { m_timeStep = value; } }

        //-----------------------//
        // Class Methods
        //-----------------------//
        public CParamsReversalVectorMdl GetCopy()
        {
            CParamsReversalVectorMdl c = new CParamsReversalVectorMdl();
            c.m_vector = m_vector.GetCopy();
            c.m_prob = m_prob.GetCopy();
            c.m_timeVctr = m_timeVctr.GetCopy();
            c.m_timeStep = m_timeStep.GetCopy();
            return c;
        }
    }

}