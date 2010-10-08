using System.Collections.Generic;
using System.Linq;
using ESME.Model;

namespace ESME.Environment
{
    public class DataColumn : Datum
    {
        internal DataColumn(DataLayer dataLayer, long columnIndex) : base(dataLayer, -1, columnIndex) { }

        public long ColumnIndex
        {
            get { return _columnIndex; }
        }

        public IEnumerable<DataPoint> GetPoints(long startRow, long endRow, long rowStepSize)
        {
            for (var row = startRow; row < endRow; row += rowStepSize)
            {
                yield return new DataPoint(_dataLayer, row, _columnIndex);
            }
        }

        public IEnumerable<DataPoint> GetPoints(float southLatitude, float northLatitude)
        {
            if ((southLatitude < -90) || (southLatitude > 90)) throw new LatitudeOutOfRangeException("DataLayer: southLatitude must be between -90 and +90");
            if ((northLatitude < -90) || (northLatitude > 90)) throw new LatitudeOutOfRangeException("DataLayer: northLatitude must be between -90 and +90");
            if (southLatitude >= northLatitude) throw new LatitudeOutOfRangeException("DataLayer: southLatitude must be less than northLatitude");
            return GetPoints(0, _dataLayer.ColumnCount, 1).Where(point => (southLatitude <= point.EarthCoordinate.Latitude_degrees) && (point.EarthCoordinate.Latitude_degrees <= northLatitude));
        }

        public IEnumerable<DataPoint> Points
        {
            get { return GetPoints(0, _dataLayer.RowCount, 1); }
        }
    }
}