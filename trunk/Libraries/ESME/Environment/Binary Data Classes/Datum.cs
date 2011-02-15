using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace ESME.Environment
{
    public class Datum
    {
        protected Datum(DataLayer dataLayer, long rowIndex, long columnIndex)
        {
            _dataLayer = dataLayer;
            _rowIndex = rowIndex;
            _columnIndex = columnIndex;
            _rowStep = _columnStep = 0;

            if (rowIndex > -1)
            {
                if (dataLayer.RowCount <= rowIndex)
                    throw new IndexOutOfRangeException(String.Format("Requested row index ({0}) out of range in DataLayer \"{1}\" ({2} rows, {3} cols) in DataFile \"{4}\"",
                        rowIndex, dataLayer.Name, dataLayer.RowCount, dataLayer.ColumnCount, dataLayer.DataFile.FileName));
                _latitude = dataLayer.LatitudeAxis[(int)rowIndex];
                _rowStep = dataLayer.ColumnCount * dataLayer.DepthCount * sizeof(float);
            }
            else
                _latitude = float.NaN;

            if (columnIndex > -1)
            {
                if (dataLayer.ColumnCount <= columnIndex)
                    throw new IndexOutOfRangeException(String.Format("Requested column index ({0}) out of range in DataLayer \"{1}\" ({2} rows, {3} cols) in DataFile \"{4}\"",
                        columnIndex, dataLayer.Name, dataLayer.RowCount, dataLayer.ColumnCount, dataLayer.DataFile.FileName));
                _longitude = dataLayer.LongitudeAxis[(int)columnIndex];
                _columnStep = dataLayer.DepthCount * sizeof(float);
            }
            else
                _longitude = float.NaN;

            _dataStartOffset = dataLayer.DataStartOffset;
        }

        protected long _rowIndex, _columnIndex, _rowStep, _columnStep, _dataStartOffset;
        protected float _latitude, _longitude;
        protected DataLayer _dataLayer;
    }

}
