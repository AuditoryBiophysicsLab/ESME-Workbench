using System.Collections.Generic;
using System.Linq;
using ESME.Model;
using ESME.NEMO;

namespace ESME.Environment
{
    public class DataRow : Datum
    {
        internal DataRow(DataLayer dataLayer, long rowIndex) : base(dataLayer, rowIndex, -1) { }

        public long RowIndex
        {
            get { return _rowIndex; }
        }

        public IEnumerable<DataPoint> GetPoints(long startColumn, long endColumn, long columnStepSize)
        {
            for (var column = startColumn; column < endColumn; column += columnStepSize)
            {
                yield return new DataPoint(_dataLayer, _rowIndex, column);
            }
        }

        public IEnumerable<DataPoint> GetPoints(float westLongitude, float eastLongitude)
        {
            if ((westLongitude < -180) || (westLongitude > 180)) throw new LongitudeOutOfRangeException("GetPoints: westLongitude must be between -180 and +180");
            if ((eastLongitude < -180) || (eastLongitude > 180)) throw new LongitudeOutOfRangeException("GetPoints: eastLongitude must be between -180 and +180");
            if (westLongitude >= eastLongitude) throw new LongitudeOutOfRangeException("GetPoints: westLongitude must be less than eastLongitude");
            return GetPoints(0, _dataLayer.ColumnCount, 1).Where(point => (westLongitude <= point.EarthCoordinate.Longitude) && (point.EarthCoordinate.Longitude <= eastLongitude));
        }

        public IEnumerable<DataPoint> Points
        {
            get { return GetPoints(0, _dataLayer.ColumnCount, 1); }
        }
    }
}