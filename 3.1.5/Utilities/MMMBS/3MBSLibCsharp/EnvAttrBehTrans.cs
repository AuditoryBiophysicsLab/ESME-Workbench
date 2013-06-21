using System;
using System.Collections.Generic;
using System.Collections;
using System.Text;
using System.Diagnostics;

namespace MMMBSLib
{
    public class CEnvAttrBehTrans
    {
        // This class is managed by the class that instantiates it.  No management or
        // modification occurs here with the exception of calls to get a copy of this
        // class and to reset it.
        //-----------------------//
        // Class Member Variables
        //-----------------------//

        CVector m_vector = new CVector();
        double m_meanTimeInBeh;
        double m_slopeCoefficient;
        //--------------------------------------------------------------------------------------------------//

        //--------------------------------------------------------------------------------------------------//
        // This doesn't seem to be used any longer so is expected to go away but must confirm and coordinate with
        // Wrapper class and C++ libraries first.
        CElement m_element = new CElement(); // see about removing this.
        //--------------------------------------------------------------------------------------------------//


        //-----------------------//
        // Class Properties
        //-----------------------//
        public CVector vector { get { return m_vector; } set { m_vector = value; } }
        public CElement element { get { return m_element; } set { m_element = value; } } //see about removing this.

        public double meanTimeInBehMinuites { get { return m_meanTimeInBeh; } set { m_meanTimeInBeh = CUtil.Min0OrKeep(value); } }
        public double slopeCoeff_goesAway { get { return m_slopeCoefficient; } set { m_slopeCoefficient = CUtil.Min0OrKeep(value); } }

        //-----------------------//
        // Class Methods
        //-----------------------//
        public CEnvAttrBehTrans GetCopy()
        {
            CEnvAttrBehTrans c = new CEnvAttrBehTrans();

            c.m_vector = m_vector.GetCopy();
            c.meanTimeInBehMinuites = meanTimeInBehMinuites;
            c.slopeCoeff_goesAway = slopeCoeff_goesAway;
            //--------------------------------------------------------------------------------------------------//

            //--------------------------------------------------------------------------------------------------//
            // This doesn't seem to be used any longer so is expected to go away but must confirm and coordinate with
            // Wrapper class and C++ libraries first.
            c.m_element = m_element.GetCopy();
            //--------------------------------------------------------------------------------------------------//
            return c;
        }

        public void Reset()
        {
            m_vector.Reset();
            //--------------------------------------------------------------------------------------------------//
            // This doesn't seem to be used any longer so is expected to go away but must confirm and coordinate with
            // Wrapper class and C++ libraries first.
            m_element.Reset();
            //--------------------------------------------------------------------------------------------------//
        }
    }
}