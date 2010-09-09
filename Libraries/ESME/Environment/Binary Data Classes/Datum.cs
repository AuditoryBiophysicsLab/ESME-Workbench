using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace ESME.Environment
{
    public class Datum
    {
        protected Datum(DataLayer DataLayer, long RowIndex, long ColumnIndex)
        {
            _dataLayer = DataLayer;
            _rowIndex = RowIndex;
            _columnIndex = ColumnIndex;
            _rowStep = _columnStep = 0;

            if (RowIndex > -1)
            {
                if (DataLayer.RowCount <= RowIndex)
                    throw new IndexOutOfRangeException(String.Format("Requested row index ({0}) out of range in DataLayer \"{1}\" ({2} rows, {3} cols) in DataFile \"{4}\"",
                        RowIndex, DataLayer.Name, DataLayer.RowCount, DataLayer.ColumnCount, DataLayer.DataFile.FileName));
                _latitude = DataLayer.LatitudeAxis[(int)RowIndex];
                _rowStep = DataLayer.ColumnCount * DataLayer.DepthCount * sizeof(float);
            }
            else
                _latitude = float.NaN;

            if (ColumnIndex > -1)
            {
                if (DataLayer.ColumnCount <= ColumnIndex)
                    throw new IndexOutOfRangeException(String.Format("Requested column index ({0}) out of range in DataLayer \"{1}\" ({2} rows, {3} cols) in DataFile \"{4}\"",
                        ColumnIndex, DataLayer.Name, DataLayer.RowCount, DataLayer.ColumnCount, DataLayer.DataFile.FileName));
                _longitude = DataLayer.LongitudeAxis[(int)ColumnIndex];
                _columnStep = DataLayer.DepthCount * sizeof(float);
            }
            else
                _longitude = float.NaN;

            _dataStartOffset = DataLayer.DataStartOffset;
        }

        protected long _rowIndex, _columnIndex, _rowStep, _columnStep, _dataStartOffset;
        protected float _latitude, _longitude;
        protected DataLayer _dataLayer;
    }

}
