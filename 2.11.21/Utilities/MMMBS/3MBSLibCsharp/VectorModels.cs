    using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Runtime.InteropServices;
using System.Diagnostics;

namespace MMMBSLib
{
    //[StructLayout(LayoutKind.Sequential, Pack=1)]
    public class CMatrix
    {
        // Class Member Variables
        private double[][] m_a = null; // Elements in the Matrix 
        private int m_lastColumnCount;

        // Class Constructors
        public CMatrix() { }
        public CMatrix(double[][] InitialMatrix) { m_a = InitialMatrix; }

        // Class Properties
        public double[][] a { get { return m_a; } set { m_a = value; } }
        public int RowCount
        {
            get
            {
                if(m_a == null || m_a.Length < 1)
                    return 0;
                return m_a.Length;
            } 
        }
        public int ColumnCount
        {
            get
            {
                if(m_a == null || m_a.Length < 1)
                    return 0;
                return m_a[0].Length;
            } 
        }
        public string sz { get { return "[" + RowCount + "x" + ColumnCount + "]"; } }

        // Class Methods
        public void SetInitialDimensions(int RowCount, int ColCount)
        {
            int i;
            if(RowCount < 1 || ColCount < 1)
                return;
            m_a = new double[RowCount][];
            for(i=0; i<RowCount; i++)
                m_a[i] = new double[ColCount];

            m_lastColumnCount = ColCount;
        }

        public void AddColumn()
        {
            int i, j;
            int rowCnt = RowCount;
            int colCnt = ColumnCount;
            double[][] aa = m_a;
            m_a = new double[rowCnt][];
            for(i=0; i<rowCnt; i++)
            {
                m_a[i] = new double[colCnt+1];
                for(j=0; j<colCnt; j++)
                    m_a[i][j] = aa[i][j];
                m_a[i][j] = 1.0;
            }

            m_lastColumnCount = ColumnCount;
        }

        public Boolean DeleteColumn(int Index)
        {
            int i, j, col;
            CMatrix cpy = new CMatrix();

            // Precheck.
            if(false == CUtil.CheckIndex(ColumnCount, Index))
                return false;

            if(ColumnCount == 1)
            {
                m_a = null;
                m_lastColumnCount = 0;
                return true;
            }

            // Create the matrix
            cpy.m_a = new double[m_a.Length][];
            for(i=0; i<RowCount; i++)
                cpy.m_a[i] = new double[ColumnCount-1];

            // Set the matrix
            for(i=0; i<RowCount; i++)
            {
                for(j=0, col=0; j<ColumnCount; j++)
                {
                    if(j != Index)
                    {
                        cpy.m_a[i][col] = m_a[i][j];

                        // The last behavior's probability must be 1.0
                        if(col == cpy.m_a[i].Length-1)
                            cpy.m_a[i][col] = 1.0;
                        col++;
                    }
                }
            }
            m_a = cpy.m_a;
            m_lastColumnCount = ColumnCount-1;

            return true;
        }

        public void AddRow(double[] RowData)
        {
            int i;
            AddRow();
            for(i=0; i<RowData.Length && i<m_a[m_a.Length-1].Length; i++)
                m_a[m_a.Length-1][i] = RowData[i];
        }

        public void SetRow(int Index, double[] Data)
        {
            Debug.Assert(Index < RowCount);
            Debug.Assert(ColumnCount < Data.Length);
            for(int i=0; i<ColumnCount && i<Data.Length; i++)
                m_a[Index][i] = Data[i];
        }

        public void GetRowCopy(int Index)
        {
            double[] cpy = new double[ColumnCount];
            Debug.Assert(Index < RowCount);

            for(int i=0; i< ColumnCount; i++)
                cpy[i] = m_a[Index][i];
        }

        public void AddRow()
        {
            int i, j;
            int rowCnt = RowCount;
            int colCnt = ColumnCount;
            double[][] aa = m_a;

            m_a = new double[rowCnt+1][];
            for(i=0; i<rowCnt; i++)
            {
                m_a[i] = new double[colCnt];
                for(j=0; j<colCnt; j++)
                    m_a[i][j] = aa[i][j];
            }

            //--------------------------------------------------------------------------//
            // This, combined with 'm_lastColumnCout', is here in case the code allowed
            // the user to delete every row which causes column count to ahve also gone to
            // zero.  With this, if the code permit the user to add a row back in the
            // column count will be resotred to the correct value.
            if(colCnt == 0)
                colCnt = m_lastColumnCount;
            //--------------------------------------------------------------------------//

            m_a[i] = new double[colCnt];
            for(j=0; j<colCnt; j++)
                m_a[i][j] = 1.00;

        }

        public void DeleteRow(int Index)
        {
            // Fix: 02/10/09: m_a.Length to m_a[i].Length in inner for loop.
            int i, j, row;
            CMatrix cpy = new CMatrix();
            if(Index < 0 || Index >= RowCount)
                return;

            m_lastColumnCount = ColumnCount;

            if(RowCount == 1)
            {
                m_a = null;
                return;
            }

            // Create the matrix
            cpy.m_a = new double[m_a.Length-1][];
            for(i=0; i<m_a.Length-1; i++)
                cpy.m_a[i] = new double[m_a[i].Length];

            // Set the matrix
            for(i=0, row = 0; i<m_a.Length; i++)
            {
                if(i != Index)
                {
                    for(j=0; j<m_a[i].Length; j++) // here was the fix
                    {
                        cpy.m_a[row][j] = m_a[i][j];
                    }
                    row++;
                }
            }
            m_a = cpy.m_a;
        }

        public CMatrix GetCopy()
        {
            int i, j;
            CMatrix cpy = new CMatrix();
            cpy.m_a = new double[m_a.Length][];

            for(i=0; i<RowCount; i++)
                cpy.m_a[i] = new double[ColumnCount];
            for(i=0; i<RowCount; i++)
            {
                for(j=0; j<ColumnCount; j++)
                    cpy.m_a[i][j] = m_a[i][j];
            }
            return cpy;
        }
        public void Reset() { m_a = null; }

        public string ConvertToStringA()
        {
            int i, j;
            string sz = "";
            string szFmat = "";

            if(RowCount == 0 || ColumnCount == 0)
                return "";

            for(i=0; i<RowCount; i++)
            {
                // The first element in row i.

                if(m_a[i][0] >= 0)
                    szFmat = string.Format("{0:0.000}", m_a[i][0]);
                else
                    szFmat = "[   ]";
                sz = string.Concat(sz + szFmat);

                for(j=1; j<ColumnCount; j++)
                {
                    if(m_a[i][j] >= 0)
                        szFmat = string.Format("{0: 0.000}", m_a[i][j]);
                    else
                        szFmat = "[   ]";
                    sz = string.Concat(sz + szFmat);
                }
                sz = sz + "\r\n";
            }

            // Remove the last "\r\n"
            sz = sz.Remove(sz.Length-2, 2);
            return sz;
        }


        public string ConvertToStringB()
        {
            int i, j;
            string sz = "";
            string szFmat = "";

            if(RowCount == 0 || ColumnCount == 0)
                return "";

            for(i=0; i<RowCount; i++)
            {
                // The first element in row i.

                if(m_a[i][0] >= 0)
                {
                    if(m_a[i][0] < 10)
                        szFmat = string.Format("{0: 0.00}", m_a[i][0]);
                    else
                        szFmat = string.Format("{0:00.00}", m_a[i][0]);
                    //szFmat = string.Format("{0:00.000;0.00#}", m_a[i][0]);
                    //szFmat = string.Format("{0,5:0.000;0.00#}", m_a[i][0]);
                }
                else
                    szFmat = "[   ]";
                sz = string.Concat(sz + szFmat);

                // Second column
                j=1;
                {
                    if(m_a[i][j] >= 0)
                    {
                        if(m_a[i][j] < 10)
                            szFmat = string.Format("{0:  0.00}", m_a[i][j]);
                        else
                            szFmat = string.Format("{0: 00.00}", m_a[i][j]);
                        //szFmat = string.Format("{0: 00.000;0.00#}", m_a[i][j]);
                        //szFmat = string.Format("{0,6:0.000;0.00#}", m_a[i][j]);
                    }
                    else
                        szFmat = "[   ]";
                    sz = string.Concat(sz + szFmat);
                }

                // Remaining elements in row i.
                for(j=2; j<ColumnCount; j++)
                {
                    if(m_a[i][j] >= 0)
                        szFmat = string.Format("{0: 0.000}", m_a[i][j]);
                    else
                        szFmat = "[   ]";
                    sz = string.Concat(sz + szFmat);
                }
                sz = sz + "\r\n";
            }

            // Remove the last "\r\n"
            sz = sz.Remove(sz.Length-2, 2);
            return sz;
        }
    }


    public class CVector
    {
        // Class Member Variables
        private double[] m_a = null; // Elements in the Matrix 
        
        // Class Properties
        public double[] a { get { return m_a; } set { m_a = value; } }
        public int rowCount
        {
            get
            {
                if(m_a == null || m_a.Length < 1)
                    return 0;
                return 1;
            }
        }
        public int columnCount
        {
            get
            {
                if(m_a == null || m_a.Length < 1)
                    return 0;
                return m_a.Length;
            }
        }
        public string sz { get { return "[" + rowCount + "x" + columnCount + "]"; } }

        // Class Methods
        public CVector GetCopy()
        {
            int i;
            CVector cpy = new CVector();
            cpy.m_a = new double[m_a.Length];

            for(i=0; i<columnCount; i++)
                cpy.m_a[i] = m_a[i];
            return cpy;
        }
        public void Reset() { m_a = null; }

        public void SetInitialDimensions(int RowCount, int ColCount)
        {
            if(RowCount < 1 || ColCount < 1)
                return;
            m_a = new double[ColCount];

        }


        public void AddColumn()
        {
            int i;
            int colCnt = columnCount;
            double[] aa = m_a;

            m_a = new double[colCnt+1];
            for(i=0; i<colCnt; i++)
                m_a[i] = aa[i];
            m_a[i] = 1.0;
        }
        public void DeleteColumn(int Index)
        {
            int i, col;
            double[] d = new double[m_a.Length-1];

            // Precheck.
            if(Index < 0 || Index >= columnCount)
                return;
            if(columnCount == 1)
            {
                m_a = null;
                return;
            }

            // Set the matrix
            for(i=0, col=0; i<columnCount; i++)
            {
                if(i != Index)
                    d[col++] = m_a[i];
            }
            m_a = d;
        }

        public string ConvertToString()
        {
            int i;
            string sz = ""; // default if vector not defined
            string szFmat;

            if(columnCount>0)
            {
                if(m_a[0] >= 0)
                    szFmat = string.Format("{0:0.000;0.00#}", m_a[0]);
                    //szFmat = string.Format("{0,5:0.000;0.00#}", m_a[0]);
                else
                    szFmat = "[   ]";
                //szFmat = string.Format("{0,5:-0.000;-0.00#}", m_a[0]);
                sz = string.Concat(sz + szFmat);
            }

            for(i=1; i<columnCount; i++)
            {
                if(m_a[i] >= 0)
                    szFmat = string.Format("{0: 0.000;0.00#}", m_a[i]);
                    //szFmat = string.Format("{0,6:0.000;0.00#}", m_a[i]);
                else
                    szFmat = " [   ]";
                sz = string.Concat(sz + szFmat);
                //sz = sz + m_a[i] + "   ";
            }
            return sz;
        }
    }

    public class CElement
    {
        // Class Member Variables
        private double m_a = 0; // Elements in the Matrix 

        // Class Constructors
        // Class Properties
        public double a { get {return m_a;} set {m_a = value;}} 
        public int rowCount {get { return 1; }}
        public int columnCount {get { return 1; }}
        public string sz { get { return "[1x1]" ; } }

        // Static Class Methods
        // Class Methods
        public CElement GetCopy()
        {
            CElement e = new CElement();
            e.m_a = m_a;
            return e;
        }
        public void SetInitialDimensions(int RowCount, int ColCount)
        {
            Debug.Assert(RowCount > 0 && ColCount > 0);
            if(RowCount < 1 || ColCount < 1)
                return;
        }

        public string ConvertToString() { return "" + m_a; }
        public void Reset() { m_a = 0; }
    }
}