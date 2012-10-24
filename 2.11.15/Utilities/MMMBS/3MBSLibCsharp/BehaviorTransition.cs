using System;
using System.Collections.Generic;
using System.Collections;
using System.Text;
using System.Diagnostics;
using mbs;
namespace MMMBSLib
{
    abstract public class CBehaviorMatrix
    {
        // This class is managed by the class that instantiates it.  No management or
        // modification occurs here with the exception of calls to get a copy of this
        // class and to reset it.
        //-----------------------//
        // Class Member Variables
        //-----------------------//
        // Member variables are created but left uninitialized.
        protected internal CMatrix m_matrix = new CMatrix(); // added 1-14-09 to allow for transition as a function of time of day

        //-------------------//
        // Class Constructors
        //-------------------//

        //-----------------------//
        // Class Properties
        //-----------------------//
        public abstract int transitionCount { get; }
        public CMatrix matrix { get { return m_matrix; } set { m_matrix = value; } }
 
        //-----------------------//
        // Class Methods
        //-----------------------//
        public void Reset() { m_matrix.Reset(); }
        public abstract CBehaviorMatrix GetCopy();
    }

    public class CBehaviorTransMatrix : CBehaviorMatrix
    {
        public override int transitionCount { get { return m_matrix.ColumnCount - 5; } } // number of behaviors in the matrix

        public override CBehaviorMatrix GetCopy()
        {
            CBehaviorMatrix c = new CBehaviorTransMatrix();
            c.m_matrix = m_matrix.GetCopy();
            return c;
        }

    }

    public class CInitBehaviorMatrix : CBehaviorMatrix
    {
        public override int transitionCount { get { return m_matrix.ColumnCount - 3; } } // number of behaviors in the matrix

        public override CBehaviorMatrix GetCopy()
        {
            CBehaviorMatrix c = new CInitBehaviorMatrix();
            c.m_matrix = m_matrix.GetCopy();
            return c;
        }
    
    }
}