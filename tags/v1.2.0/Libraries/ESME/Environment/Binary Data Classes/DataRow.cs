using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace ESME.Environment
{
    public class DataRow : Datum
    {
        internal DataRow(DataLayer DataLayer, long RowIndex) : base(DataLayer, RowIndex, -1) { }

        public long RowIndex { get { return _rowIndex; } }

        public IEnumerable<DataPoint> GetPoints(long StartColumn, long EndColumn, long ColumnStepSize)
        {
            for (long column = StartColumn; column < EndColumn; column += ColumnStepSize)
            {
                yield return new DataPoint(_dataLayer, _rowIndex, column);
            }
        }
        public IEnumerable<DataPoint> Points { get { return GetPoints(0, _dataLayer.ColumnCount, 1); } }
    }
}
