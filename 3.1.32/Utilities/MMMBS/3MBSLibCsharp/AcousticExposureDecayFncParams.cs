using System;
using System.Collections.Generic;
using System.Collections;
using System.Text;

namespace MMMBSLib
{
    public class CAcousticExposureDecayFncParams
    {
        // Concept class for the decay function.  The doubles below are meant represent
        // function parameters, the spacefiller represents space to be saved in the
        // modeling file reserved for future changes.
        DECAYFUNCTIONS m_decayFncType;

        // For now, save 512 bytes in binary file for decay funcion.

        // double = 8 bytes
        double m_A1 = 4.5;
        double m_B1 = 2.3;
        double m_B2 = 1.11;
        double m_C1 = 5.4;
        double m_C2 = 3.3;
        double m_C3 = 1.67;

        public DECAYFUNCTIONS decayFunctionType { get { return m_decayFncType; } set { m_decayFncType = value; } }

        public double A1 { get { return m_A1; } set { m_A1 = value; } }
        public double B1 { get { return m_B1; } set { m_B1 = value; } }
        public double B2 { get { return m_B2; } set { m_B2 = value; } }
        public double C1 { get { return m_C1; } set { m_C1 = value; } }
        public double C2 { get { return m_C2; } set { m_C2 = value; } }
        public double C3 { get { return m_C3; } set { m_C3 = value; } }
        // 6*8 = 128 bytes
        // have spacefiller be 512 total bytes, 512 - 128 - 4 = 380
        char[] m_spacefiller = new char[380];
    }
}