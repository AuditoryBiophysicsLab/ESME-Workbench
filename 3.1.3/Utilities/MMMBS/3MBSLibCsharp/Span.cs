using System;
using System.Collections.Generic;
using System.Collections;
using System.Text;
using System.Diagnostics;


namespace MMMBSLib
{
    abstract public class CSpan
    {
        //-----------------------//
        // Class Member Variables
        //-----------------------//
        protected CBehaviorMatrix m_behTrans;
        protected double m_shallow = 0;
        protected double m_deep = 0;

        //-----------------------//
        // Class Properties
        //-----------------------//
        public double shallow { get { return m_shallow; } set { m_shallow = CUtil.ForceNegative(value); } }
        public double deep { get { return m_deep; } set { m_deep = CUtil.ForceNegative(value); } }
        public int behaviorTransCount { get { return m_behTrans.transitionCount; } } // The nubmer of behaviors in the transition matrix
        public int timeTransCount { get { return m_behTrans.matrix.RowCount; } } // the number of time rows
        public CBehaviorMatrix behTrans { get { return m_behTrans; } }

        //--------------//
        // Class Methods
        //--------------//
        protected void Setup(double Shallow, double Deep, double[][] TransMatrix)
        {
            m_shallow = Shallow;
            m_deep = Deep;
            m_behTrans.matrix.a = TransMatrix;
        }

        public abstract CSpan GetCopy();
        public abstract void IncrementSpanBehaviorTransitionVectorElement();       
        public abstract Boolean DecrmentSpanBehaviorTransitionVectorElement(int BehaviorIndex);
    }

    // A class for the behavior transition depth span
    public class CSpanBehTrans : CSpan
    {
        public CSpanBehTrans()
        {
            m_behTrans = new CBehaviorTransMatrix();
            Setup(0, 0, MBSDEFAULTS.BEHAVIOR_INITIAL_TRANSLATION_MATRIX);
        }
        public CSpanBehTrans(double Shallow, double Deep)
        {
            m_behTrans = new CBehaviorTransMatrix();
            Setup(Shallow, Deep, MBSDEFAULTS.BEHAVIOR_INITIAL_TRANSLATION_MATRIX);
        }
        public CSpanBehTrans(double Shallow, double Deep, CBehaviorMatrix BehaviorTransition)
        {
            m_behTrans = new CBehaviorTransMatrix();
            Setup(Shallow, Deep, BehaviorTransition.matrix.a);
        }


        public override CSpan GetCopy()
        {
            CSpanBehTrans c = new CSpanBehTrans();
            c.shallow = m_shallow;
            c.deep = m_deep;
            c.m_behTrans = m_behTrans.GetCopy();
            return c;
        }

        public override void IncrementSpanBehaviorTransitionVectorElement()
        {
            // Overides Class CMatrix's method to add a column so that mean time in
            // behavior and slope are preserved at the end of the row (last two columns)
            int i, j;
            int rowCnt = m_behTrans.matrix.RowCount;
            int colCnt = m_behTrans.matrix.ColumnCount;
            double[][] aa = m_behTrans.matrix.a;

            m_behTrans.matrix.a = new double[rowCnt][];

            for(i=0; i<rowCnt; i++)
            {
                m_behTrans.matrix.a[i] = new double[colCnt+1];

                // The first 2 columns correspond to the transitional period.  The third
                for(j=0; j<2 && j<colCnt; j++)
                    m_behTrans.matrix.a[i][j] = aa[i][j];

                // The next columns minus 2 correspond to the transitional probabilities
                // for the previous behaviors present.
                for(; j<colCnt-2; j++)
                    m_behTrans.matrix.a[i][j] = aa[i][j];
                m_behTrans.matrix.a[i][j] = 1.0;

                // The remaining columns correspond to the original mean time in behavior
                // and slope coefficient
                for(; j<colCnt; j++)
                    m_behTrans.matrix.a[i][j+1] = aa[i][j];
            }
        }

        public override Boolean DecrmentSpanBehaviorTransitionVectorElement(int BehaviorIndex)
        {
            if(false == CUtil.CheckIndex(m_behTrans.transitionCount, BehaviorIndex))
                return false;

            //m_nrmlBehTrans.matrix_new.DeleteColumn(Index);
            // Overides Class CMatrix's method to delete a column so that mean time in
            // behavior and slope are preserved at the end of the row (last two columns)
            int i, j;
            int col;
            int rowCnt = m_behTrans.matrix.RowCount;
            int colCnt = m_behTrans.matrix.ColumnCount;
            double[][] aa = m_behTrans.matrix.a;
            int column = BehaviorIndex = BehaviorIndex + 3;

            m_behTrans.matrix.a = new double[rowCnt][];

            for(i=0; i<rowCnt; i++)
            {
                col = 0;
                m_behTrans.matrix.a[i] = new double[colCnt-1];

                // The first 2 columns correspond to the transitional period.
                for(j=0; j<colCnt; j++)
                {
                    if(j != column)
                    {
                        m_behTrans.matrix.a[i][col] = aa[i][j];
                        // Set the last element in the row to 1.000
                        if(col == colCnt - 4) // -2 for indexing, -3 for T and coefficient
                        {
                            m_behTrans.matrix.a[i][col] = 1.0;
                        }
                        col++;
                    }
                }
            }
            return true;
        }    
    }

    // A class for the initial behavior depth span
    public class CSpanInitialBeh : CSpan
    {
        public CSpanInitialBeh()
        {
            m_behTrans = new CInitBehaviorMatrix();
            Setup(0, 0, MBSDEFAULTS.BEHAVIOR_INITIAL_MATRIX);
        }
        public CSpanInitialBeh(double Shallow, double Deep)
        {
            m_behTrans = new CInitBehaviorMatrix();
            Setup(Shallow, Deep, MBSDEFAULTS.BEHAVIOR_INITIAL_MATRIX);
        }
        public CSpanInitialBeh(double Shallow, double Deep, CInitBehaviorMatrix InitBehMatrix)
        {
            m_behTrans = new CInitBehaviorMatrix();
            Setup(Shallow, Deep, InitBehMatrix.matrix.a);
        }


        public override CSpan GetCopy()
        {
            CSpanInitialBeh c = new CSpanInitialBeh();
            c.shallow = m_shallow;
            c.deep = m_deep;
            c.m_behTrans = m_behTrans.GetCopy();
            return c;
        }

        public override void IncrementSpanBehaviorTransitionVectorElement()
        {
            // Overides Class CMatrix's method to add a column so that mean time in
            // behavior and slope are preserved at the end of the row (last two columns)
            int i, j;
            int rowCnt = m_behTrans.matrix.RowCount;
            int colCnt = m_behTrans.matrix.ColumnCount;
            double[][] aa = m_behTrans.matrix.a;

            m_behTrans.matrix.a = new double[rowCnt][];

            for(i=0; i<rowCnt; i++)
            {
                m_behTrans.matrix.a[i] = new double[colCnt+1];

                // The first 2 columns correspond to the transitional period.  The third
                for(j=0; j<2 && j<colCnt; j++)
                    m_behTrans.matrix.a[i][j] = aa[i][j];

                // The next columns minus 2 correspond to the transitional probabilities
                // for the previous behaviors present.
                for(; j<colCnt; j++)
                    m_behTrans.matrix.a[i][j] = aa[i][j];
                m_behTrans.matrix.a[i][j] = 1.0;

            }
        }


        public override Boolean DecrmentSpanBehaviorTransitionVectorElement(int BehaviorIndex)
        {
            if(false == CUtil.CheckIndex(m_behTrans.transitionCount, BehaviorIndex))
                return false;

            //m_nrmlBehTrans.matrix_new.DeleteColumn(Index);
            // Overides Class CMatrix's method to delete a column so that mean time in
            // behavior and slope are preserved at the end of the row (last two columns)
            int i, j;
            int col;
            int rowCnt = m_behTrans.matrix.RowCount;
            int colCnt = m_behTrans.matrix.ColumnCount;
            double[][] aa = m_behTrans.matrix.a;
            int column = BehaviorIndex + 3;

            m_behTrans.matrix.a = new double[rowCnt][];

            for(i=0; i<rowCnt; i++)
            {
                col = 0;
                m_behTrans.matrix.a[i] = new double[colCnt-1];

                // The first 2 columns correspond to the transitional period.
                for(j=0; j<colCnt; j++)
                {
                    if(j != column)
                    {
                        m_behTrans.matrix.a[i][col] = aa[i][j];
                        // Set the last element in the row to 1.000
                        if(col == colCnt - 2) // -2 for the two clocks... essentially is asking "is this the last column?"
                        {
                            m_behTrans.matrix.a[i][col] = 1.0;
                        }
                        col++;
                    }
                }
            }
            return true;
        }

    }
}