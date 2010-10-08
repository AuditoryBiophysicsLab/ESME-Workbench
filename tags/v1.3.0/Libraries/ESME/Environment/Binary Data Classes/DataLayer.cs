using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using ESME.Model;

namespace ESME.Environment
{
    public class DataLayer
    {
        public IEnumerable<DataRow> GetRows(float southLatitude, float northLatitude)
        {
            if ((southLatitude < -90) || (southLatitude > 90)) throw new LatitudeOutOfRangeException("DataLayer: southLatitude must be between -90 and +90");
            if ((northLatitude < -90) || (northLatitude > 90)) throw new LatitudeOutOfRangeException("DataLayer: northLatitude must be between -90 and +90");
            if (southLatitude >= northLatitude) throw new LatitudeOutOfRangeException("DataLayer: southLatitude must be less than northLatitude");
            long startLatIndex = 0;
            long endLatIndex = 0;
            for (var latIndex = 0; latIndex < LatitudeAxis.Values.Length; latIndex++)
            {
                if (LatitudeAxis.Values[latIndex] < southLatitude) startLatIndex = latIndex;
                if (LatitudeAxis.Values[latIndex] < northLatitude) endLatIndex = latIndex;
            }
            return GetRows(startLatIndex, endLatIndex, 1);
        }

        public IEnumerable<DataRow> GetRows(long startRow, long endRow, long rowStepSize) { for (var row = startRow; row <= endRow; row += rowStepSize) yield return new DataRow(this, row); }

        public IEnumerable<DataRow> Rows
        {
            get { return GetRows(0, RowCount - 1, 1); }
        }

        public IEnumerable<DataColumn> GetColumns(float westLongitude, float eastLongitude)
        {
            if ((westLongitude < -180) || (westLongitude > 180)) throw new LongitudeOutOfRangeException("DataLayer: westLongitude must be between -180 and +180");
            if ((eastLongitude < -180) || (eastLongitude > 180)) throw new LongitudeOutOfRangeException("DataLayer: eastLongitude must be between -180 and +180");
            if (westLongitude >= eastLongitude) throw new LongitudeOutOfRangeException("DataLayer: westLongitude must be less than eastLongitude");
            long startLonIndex = 0;
            long endLonIndex = 0;
            for (var lonIndex = 0; lonIndex < LongitudeAxis.Values.Length; lonIndex++)
            {
                if (LongitudeAxis.Values[lonIndex] < westLongitude) startLonIndex = lonIndex;
                if (LongitudeAxis.Values[lonIndex] < eastLongitude) endLonIndex = lonIndex;
            }
            return GetColumns(startLonIndex, endLonIndex, 1);
        }

        public IEnumerable<DataColumn> GetColumns(long startColumn, long endColumn, long columnStepSize) { for (var column = startColumn; column <= endColumn; column += columnStepSize) yield return new DataColumn(this, column); }

        public IEnumerable<DataColumn> Columns
        {
            get { return GetColumns(0, ColumnCount - 1, 1); }
        }

        public float[,] Get2DData(long startRow, long endRow, long requestedRows, long startColumn, long endColumn, long requestedColumns) { return Get2DData(startRow, endRow, requestedRows, startColumn, endColumn, requestedColumns, 0); }
        public float[,] Get2DData(long startRow, long endRow, long startColumn, long endColumn) { return Get2DData(startRow, endRow, endRow - startRow + 1, startColumn, endColumn, endColumn - startColumn + 1, 0); }

        public DataPoint this[long row, long column]
        {
            get { return new DataPoint(this, row, column); }
        }

        public float[,] Get2DData(long startRow, long endRow, long requestedRows, long startColumn, long endColumn, long requestedColumns, int depthIndex)
        {
            var retval = new float[requestedRows,requestedColumns];

            var rowStep = (endRow - startRow)/(float) requestedRows;
            var columnStep = (endColumn - startColumn)/(float) requestedColumns;

            var curPoint = new DataPoint(this);
            for (long row = 0; row < requestedRows; row++)
            {
                var rowIndex = (long) (startRow + (row*rowStep));
                curPoint.RowIndex = rowIndex;
                for (long column = 0; column < requestedColumns; column++)
                {
                    var columnIndex = (long) (startColumn + (column*columnStep));
                    curPoint.ColumnIndex = columnIndex;
                    retval[row, column] = curPoint.Data[depthIndex];
                }
            }
            return retval;
        }

        public float[,][] Get3DData(long startRow, long endRow, long requestedRows, long startColumn, long endColumn, long requestedColumns)
        {
            var retval = new float[requestedRows,requestedColumns][];

            var rowStep = (endRow - startRow)/(float) requestedRows;
            var columnStep = (endColumn - startColumn)/(float) requestedColumns;

            var curPoint = new DataPoint(this);
            for (long row = 0; row < requestedRows; row++)
            {
                var rowIndex = (long) (startRow + (row*rowStep));
                curPoint.RowIndex = rowIndex;
                for (long column = 0; column < requestedColumns; column++)
                {
                    var columnIndex = (long) (startColumn + (column*columnStep));
                    curPoint.ColumnIndex = columnIndex;
                    retval[row, column] = curPoint.Data;
                }
            }
            return retval;
        }

        public DataLayer(BinaryReader stream)
        {
            LayerStartOffset = stream.BaseStream.Position;
            var expectedMagic = stream.ReadUInt32();
            if (Magic != expectedMagic) throw new FormatException(string.Format("DataLayer: Invalid input file format.  Expected magic number {0:x} not found.  Instead saw {1:x}.", Magic, expectedMagic));
            Name = stream.ReadString();
            TimePeriod = stream.ReadString();
            OriginalFilename = stream.ReadString();
            Metadata = stream.ReadString();
            _axisCount = stream.ReadUInt16();
            if ((_axisCount == 2) || (_axisCount == 3))
            {
                _longitudeAxis = new DataAxis(stream);
                _longitudeAxis.Normalize(-180, 180);
                _latitudeAxis = new DataAxis(stream);
                if (_axisCount == 3) _depthAxis = new DataAxis(stream);
            }
            else throw new FormatException("DataLayer: Invalid input file format. This code currently supports only 2 or 3 axes, not " + _axisCount);
            RowCount = stream.ReadInt64();
            ColumnCount = stream.ReadInt64();
            DepthCount = stream.ReadInt64();
            DataStartOffset = stream.BaseStream.Position;
        }

        public DataLayer(string name, string timePeriod, string originalFilename, string metadata, DataAxis latitudeAxis, DataAxis longitudeAxis, DataAxis depthAxis)
        {
            LayerStartOffset = -1;
            Name = name;
            TimePeriod = timePeriod;
            OriginalFilename = originalFilename;
            Metadata = metadata;
            _latitudeAxis = latitudeAxis;
            _depthAxis = depthAxis;
            _longitudeAxis = longitudeAxis;
            _longitudeAxis.Normalize(-180, 180);
            _axisCount = _depthAxis != null ? (ushort) 3 : (ushort) 2;
            RowCount = latitudeAxis.Length;
            ColumnCount = longitudeAxis.Length;
            DepthCount = _depthAxis != null ? depthAxis.Length : 1;
            _isNewLayer = true;
        }

        public DataLayer(DataLayer source, float northLatitude, float westLongitude, float southLatitude, float eastLongitude)
        {
            LayerStartOffset = -1;
            // Construct the new DataLayer from an existing one, given Row and Column constraints
            if (source == null) throw new ApplicationException("DataLayer: Cannot construct new DataLayer from an existing DataLayer when the existing DataLayer is null");

            if (!source._latitudeAxis.ContainsRange(northLatitude, southLatitude)) throw new ApplicationException("DataLayer: Requested latitude range is not fully contained in this layer");
            if (!source._longitudeAxis.ContainsRange(westLongitude, eastLongitude))
            {
                var newWest = westLongitude + 360;
                var newEast = eastLongitude + 360;
                if (!source._longitudeAxis.ContainsRange(newWest, newEast))
                {
                    newWest = westLongitude - 360;
                    newEast = eastLongitude - 360;
                    if (!source._longitudeAxis.ContainsRange(newWest, newEast)) // Here is where we should check to see if the requested data range straddles the wrap point of the source layer
                        throw new ApplicationException("DataLayer: Requested longitude range is not fully contained in this layer");
                }
                westLongitude = newWest;
                eastLongitude = newEast;
            }

            Name = source.Name;
            TimePeriod = source.TimePeriod;
            OriginalFilename = source.OriginalFilename;
            Metadata = source.Metadata;

            // Construct the lat/lon axes
            _latitudeAxis = new DataAxis(source._latitudeAxis, northLatitude, southLatitude);

            _longitudeAxis = new DataAxis(source._longitudeAxis, westLongitude, eastLongitude);
            _longitudeAxis.Normalize(-180, 180);

            ColumnCount = _longitudeAxis.Length;

            // If there's a depth axis, just copy it
            _axisCount = 2;
            if (source._depthAxis != null)
            {
                _axisCount = 3;
                _depthAxis = new DataAxis(source._depthAxis);
                DepthCount = _depthAxis.Length;
            }
            else DepthCount = 1;

            long sourceStartRow = source._latitudeAxis[southLatitude];
            long sourceEndRow = source._latitudeAxis[northLatitude];
            long sourceStartColumn = source._longitudeAxis[westLongitude];
            long sourceEndColumn = source._longitudeAxis[eastLongitude];
            RowCount = sourceEndRow - sourceStartRow;
            ColumnCount = sourceEndColumn - sourceStartColumn;

            _sourceDataByRow = source.SelectedDataByRow(sourceStartRow, sourceEndRow, sourceStartColumn, sourceEndColumn);
            //array = new DataArray(Source.ReadStream, DestFile.WriteStream, Source.DataArray, _longitudeAxis, _latitudeAxis);
        }

        IEnumerable<DataPoint> SelectedDataByRow(long startRow, long endRow, long startColumn, long endColumn) { return GetRows(startRow, endRow, 1).SelectMany(selectedRow => selectedRow.GetPoints(startColumn, endColumn, 1)); }

        public void Save(BinaryWriter stream)
        {
            if (LayerStartOffset != -1) stream.BaseStream.Position = LayerStartOffset;
            else LayerStartOffset = stream.BaseStream.Position;

            stream.Write(Magic);
            stream.Write(Name);
            stream.Write(TimePeriod);
            stream.Write(OriginalFilename);
            stream.Write(Metadata);
            stream.Write(_axisCount);
            _longitudeAxis.Save(stream);
            _latitudeAxis.Save(stream);
            if (_depthAxis != null) _depthAxis.Save(stream);
            stream.Write(RowCount);
            stream.Write(ColumnCount);
            stream.Write(DepthCount);
            DataStartOffset = stream.BaseStream.Position;
            if (_isNewLayer)
            {
                DataStartOffset = stream.BaseStream.Position;
                for (long row = 0; row < RowCount; row++) for (long col = 0; col < ColumnCount; col++) for (long dep = 0; dep < DepthCount; dep++) stream.Write(_defaultValue);
                _isNewLayer = false;
            }
            if (_sourceDataByRow != null)
            {
                var sourcePoints = _sourceDataByRow.GetEnumerator();
                DataStartOffset = stream.BaseStream.Position;
                for (long row = 0; row < RowCount; row++)
                    for (long col = 0; col < ColumnCount; col++)
                    {
                        sourcePoints.MoveNext();
                        for (long dep = 0; dep < DepthCount; dep++) stream.Write(sourcePoints.Current.Data[dep]);
                    }
                _sourceDataByRow = null;
            }
        }

        public override string ToString() { return string.Format("  DataLayer\n" + "    Name             : {0}\n" + "    Time Period      : {1}\n" + "    Original Filename: {2}\n" + "    Metadata         : {3}\n" + "    Latitude Axis    : {4}\n" + "    Longitude Axis   : {5}\n" + "    Depth Axis       : {6}\n", Name, TimePeriod, Path.GetFileName(OriginalFilename), Metadata, LatitudeAxis, LongitudeAxis, DepthAxis == null ? "(none)" : DepthAxis.ToString()); }

        internal DataFile DataFile { get; set; }
        internal long DataStartOffset { get; private set; }
        long LayerStartOffset { get; set; }

        public string Name { get; set; }
        public string TimePeriod { get; set; }
        public string OriginalFilename { get; set; }
        public string Metadata { get; set; }

        public DataAxis LatitudeAxis
        {
            get { return _latitudeAxis; }
        }

        public DataAxis LongitudeAxis
        {
            get { return _longitudeAxis; }
        }

        public DataAxis DepthAxis
        {
            get { return _depthAxis; }
        }

        public long RowCount { get; private set; }
        public long ColumnCount { get; private set; }
        public long DepthCount { get; private set; }

        public float DefaultValue
        {
            set { _defaultValue = value; }
        }

        const uint Magic = 0x8ef22a9e;
        readonly UInt16 _axisCount;
        readonly DataAxis _latitudeAxis;
        readonly DataAxis _longitudeAxis;
        readonly DataAxis _depthAxis;
        IEnumerable<DataPoint> _sourceDataByRow;
        bool _isNewLayer;
        float _defaultValue;
    }
}