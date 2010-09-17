using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace ESME.Environment
{
    public class DataColumn : Datum
    {
        internal DataColumn(DataLayer DataLayer, long ColumnIndex) : base(DataLayer, -1, ColumnIndex) { }

        public long ColumnIndex { get { return _columnIndex; } }

        public IEnumerable<DataPoint> GetPoints(long StartRow, long EndRow, long RowStepSize)
        {
            for (long row = StartRow; row < EndRow; row += RowStepSize)
            {
                yield return new DataPoint(_dataLayer, row, _columnIndex);
            }
        }
        public IEnumerable<DataPoint> Points { get { return GetPoints(0, _dataLayer.RowCount, 1); } }
    }
}
