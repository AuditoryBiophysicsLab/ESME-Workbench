using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using HRC.Navigation;

namespace ESME.Environment
{
    public class DataLayer
    {
        public IEnumerable<DataRow> GetRows(long StartRow, long EndRow, long RowStepSize)
        {
            for (long row = StartRow; row <= EndRow; row += RowStepSize)
                yield return new DataRow(this, row);
        }
        public IEnumerable<DataRow> Rows { get {return GetRows(0, RowCount - 1, 1); } }

        public IEnumerable<DataColumn> GetColumns(long StartColumn, long EndColumn, long ColumnStepSize)
        {
            for (long column = StartColumn; column <= EndColumn; column += ColumnStepSize)
                yield return new DataColumn(this, column);
        }
        public IEnumerable<DataColumn> Columns { get { return GetColumns(0, ColumnCount - 1, 1); } }

        public float[,] Get2DData(long StartRow, long EndRow, long RowCount, long StartColumn, long EndColumn, long ColumnCount)
        {
            return Get2DData(StartRow, EndRow, RowCount, StartColumn, EndColumn, ColumnCount, 0);
        }

        public DataPoint this[long Row, long Column] { get { return new DataPoint(this, Row, Column); } }

        public float[,] Get2DData(long StartRow, long EndRow, long RequestedRows, long StartColumn, long EndColumn, long RequestedColumns, int DepthIndex)
        {
            float RowStep, ColumnStep;
            float[,] retval = new float[RequestedRows, RequestedColumns];

            RowStep = (float)(EndRow - StartRow) / (float)RequestedRows;
            ColumnStep = (float)(EndColumn - StartColumn) / (float)RequestedColumns;

            DataPoint curPoint = new DataPoint(this);
            for (long row = 0; row < RequestedRows; row++)
            {
                long RowIndex = (long)(StartRow + (row * RowStep));
                curPoint.RowIndex = RowIndex;
                for (long column = 0; column < RequestedColumns; column++)
                {
                    long ColumnIndex = (long)(StartColumn + (column * ColumnStep));
                    curPoint.ColumnIndex = ColumnIndex;
                    retval[row, column] = curPoint.Data[DepthIndex];
                }
            }
            return retval;
        }

        public float[,][] Get3DData(long StartRow, long EndRow, long RequestedRows, long StartColumn, long EndColumn, long RequestedColumns)
        {
            float RowStep, ColumnStep;
            float[,][] retval = new float[RequestedRows, RequestedColumns][];

            RowStep = (float)(EndRow - StartRow) / (float)RequestedRows;
            ColumnStep = (float)(EndColumn - StartColumn) / (float)RequestedColumns;

            DataPoint curPoint = new DataPoint(this);
            for (long row = 0; row < RequestedRows; row++)
            {
                long RowIndex = (long)(StartRow + (row * RowStep));
                curPoint.RowIndex = RowIndex;
                for (long column = 0; column < RequestedColumns; column++)
                {
                    long ColumnIndex = (long)(StartColumn + (column * ColumnStep));
                    curPoint.ColumnIndex = ColumnIndex;
                    retval[row, column] = curPoint.Data;
                }
            }
            return retval;
        }

        public DataLayer(BinaryReader stream)
        {
            LayerStartOffset = stream.BaseStream.Position;
            UInt32 expectedMagic = stream.ReadUInt32();
            if (_magic != expectedMagic)
                throw new FormatException(string.Format("DataLayer: Invalid input file format.  Expected magic number {0:x} not found.  Instead saw {1:x}.", _magic, expectedMagic));
            Name = stream.ReadString();
            TimePeriod = stream.ReadString();
            OriginalFilename = stream.ReadString();
            Metadata = stream.ReadString();
            _axisCount = stream.ReadUInt16();
            if ((_axisCount == 2) || (_axisCount == 3))
            {
                _longitudeAxis = new DataAxis(stream);
                _latitudeAxis = new DataAxis(stream);
                if (_axisCount == 3)
                    _depthAxis = new DataAxis(stream);
            }
            else
                throw new FormatException("DataLayer: Invalid input file format. This code currently supports only 2 or 3 axes, not " + _axisCount);
            RowCount = stream.ReadInt64();
            ColumnCount = stream.ReadInt64();
            DepthCount = stream.ReadInt64();
            DataStartOffset = stream.BaseStream.Position;
        }

        public DataLayer(string Name, string TimePeriod, string OriginalFilename, string Metadata,
            DataAxis LatitudeAxis, DataAxis LongitudeAxis, DataAxis DepthAxis)
        {
            LayerStartOffset = -1;
            this.Name = Name;
            this.TimePeriod = TimePeriod;
            this.OriginalFilename = OriginalFilename;
            this.Metadata = Metadata;
            _latitudeAxis = LatitudeAxis;
            _depthAxis = DepthAxis;
            _longitudeAxis = LongitudeAxis;
            _axisCount = _depthAxis != null ? (ushort)3 : (ushort)2;
            RowCount = LatitudeAxis.Length;
            ColumnCount = LongitudeAxis.Length;
            if (_depthAxis != null)
                DepthCount = DepthAxis.Length;
            else
                DepthCount = 1;
            _isNewLayer = true;
        }

        public DataLayer(DataLayer Source, DataFile DestFile, float NorthLatitude, float WestLongitude, float SouthLatitude, float EastLongitude)
        {
            LayerStartOffset = -1;
            // Construct the new DataLayer from an existing one, given Row and Column constraints
            if (Source == null)
                throw new ApplicationException("DataLayer: Cannot construct new DataLayer from an existing DataLayer when the existing DataLayer is null");

            if (!Source._latitudeAxis.ContainsRange(NorthLatitude, SouthLatitude))
                throw new ApplicationException("DataLayer: Requested latitude range is not fully contained in this layer");
            if (!Source._longitudeAxis.ContainsRange(WestLongitude, EastLongitude))
            {
                float NewWest, NewEast;
                NewWest = WestLongitude + 360;
                NewEast = EastLongitude + 360;
                if (!Source._longitudeAxis.ContainsRange(NewWest, NewEast))
                {
                    NewWest = WestLongitude - 360;
                    NewEast = EastLongitude - 360;
                    if (!Source._longitudeAxis.ContainsRange(NewWest, NewEast))
                        // Here is where we should check to see if the requested data range straddles the wrap point of the source layer
                        throw new ApplicationException("DataLayer: Requested longitude range is not fully contained in this layer");
                }
                WestLongitude = NewWest;
                EastLongitude = NewEast;
            }

            Name = Source.Name;
            TimePeriod = Source.TimePeriod;
            OriginalFilename = Source.OriginalFilename;
            Metadata = Source.Metadata;

            // Construct the lat/lon axes
            _latitudeAxis = new DataAxis(Source._latitudeAxis, NorthLatitude, SouthLatitude);

            _longitudeAxis = new DataAxis(Source._longitudeAxis, WestLongitude, EastLongitude);
            _longitudeAxis.Normalize();

            ColumnCount = _longitudeAxis.Length;
            
            // If there's a depth axis, just copy it
            _axisCount = 2;
            if (Source._depthAxis != null)
            {
                _axisCount = 3;
                _depthAxis = new DataAxis(Source._depthAxis);
                DepthCount = _depthAxis.Length;
            }
            else
                DepthCount = 1;

            long SourceStartRow = Source._latitudeAxis[SouthLatitude];
            long SourceEndRow = Source._latitudeAxis[NorthLatitude];
            long SourceStartColumn = Source._longitudeAxis[WestLongitude];
            long SourceEndColumn = Source._longitudeAxis[EastLongitude];
            RowCount = SourceEndRow - SourceStartRow;
            ColumnCount = SourceEndColumn - SourceStartColumn;

            _sourceDataByRow = Source.SelectedDataByRow(SourceStartRow, SourceEndRow, SourceStartColumn, SourceEndColumn);
            //array = new DataArray(Source.ReadStream, DestFile.WriteStream, Source.DataArray, _longitudeAxis, _latitudeAxis);
        }

        private IEnumerable<DataPoint> SelectedDataByRow(long StartRow, long EndRow, long StartColumn, long EndColumn)
        {
            foreach (DataRow selectedRow in GetRows(StartRow, EndRow, 1))
                foreach (DataPoint selectedPoint in selectedRow.GetPoints(StartColumn, EndColumn, 1))
                    yield return selectedPoint;
        }

        public void Save(BinaryWriter stream)
        {
            if (LayerStartOffset != -1)
                stream.BaseStream.Position = LayerStartOffset;
            else
                LayerStartOffset = stream.BaseStream.Position;

            stream.Write(_magic);
            stream.Write(Name);
            stream.Write(TimePeriod);
            stream.Write(OriginalFilename);
            stream.Write(Metadata);
            stream.Write(_axisCount);
            _longitudeAxis.Save(stream);
            _latitudeAxis.Save(stream);
            if (_depthAxis != null)
                _depthAxis.Save(stream);
            stream.Write(RowCount);
            stream.Write(ColumnCount);
            stream.Write(DepthCount);
            DataStartOffset = stream.BaseStream.Position;
            if (_isNewLayer)
            {
                DataStartOffset = stream.BaseStream.Position;
                for (long row = 0; row < RowCount; row++)
                    for (long col = 0; col < ColumnCount; col++)
                        for (long dep = 0; dep < DepthCount; dep++)
                            stream.Write(_defaultValue);
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
                        for (long dep = 0; dep < DepthCount; dep++)
                            stream.Write(sourcePoints.Current.Data[dep]);
                    }
                _sourceDataByRow = null;
            }
        }

        public override string ToString()
        {
            return string.Format("  DataLayer\n" +
                                 "    Name             : {0}\n" +
                                 "    Time Period      : {1}\n" +
                                 "    Original Filename: {2}\n" +
                                 "    Metadata         : {3}\n" +
                                 "    Latitude Axis    : {4}\n" +
                                 "    Longitude Axis   : {5}\n" +
                                 "    Depth Axis       : {6}\n",
                                 Name, TimePeriod, Path.GetFileName(OriginalFilename), Metadata, LatitudeAxis.ToString(), LongitudeAxis.ToString(),
                                 DepthAxis == null ? "(none)" : DepthAxis.ToString());
        }

        internal DataFile DataFile { get; set; }
        internal long DataStartOffset { get; private set; }
        private long LayerStartOffset { get; set; }

        public string Name { get; set; }
        public string TimePeriod { get; set; }
        public string OriginalFilename { get; set; }
        public string Metadata { get; set; }
        public DataAxis LatitudeAxis { get { return _latitudeAxis; } }
        public DataAxis LongitudeAxis { get { return _longitudeAxis; } }
        public DataAxis DepthAxis { get { return _depthAxis; } }
        public long RowCount { get; private set; }
        public long ColumnCount { get; private set; }
        public long DepthCount { get; private set; }
        public float DefaultValue { set { _defaultValue = value; } }

        const uint _magic = 0x8ef22a9e;
        UInt16 _axisCount;
        DataAxis _latitudeAxis, _longitudeAxis, _depthAxis;
        IEnumerable<DataPoint> _sourceDataByRow = null;
        bool _isNewLayer = false;
        float _defaultValue = 0.0f;
    }
}
