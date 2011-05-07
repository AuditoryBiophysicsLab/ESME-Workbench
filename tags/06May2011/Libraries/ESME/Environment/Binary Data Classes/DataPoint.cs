using System;
using HRC.Navigation;

namespace ESME.Environment
{
    public class DataPoint : Datum
    {
        internal DataPoint(DataLayer dataLayer, long rowIndex, long columnIndex) : base(dataLayer, rowIndex, columnIndex) { }
        public DataPoint(DataLayer dataLayer) : base(dataLayer, 0, 0) { }

        public EarthCoordinate EarthCoordinate { get { return new EarthCoordinate(_latitude, _longitude); } }
        public long RowIndex { get { return _rowIndex; } set { _rowIndex = value; } }
        public long ColumnIndex { get { return _columnIndex; } set { _columnIndex = value; } }

        public float[] Data
        {
            get
            {
                if (!_dataLayer.DataFile.CanRead)
                    throw new ApplicationException("Unable to read");
                SetPosition();
                var retval = new float[_dataLayer.DepthCount];
                for (var i = 0; i < retval.Length; i++)
                    retval[i] = _dataLayer.DataFile.ReadStream.ReadSingle();
                return retval;
            }

            set
            {
                if (!_dataLayer.DataFile.CanWrite)
                    throw new ApplicationException("Unable to write");
                if (value.Length != _dataLayer.DepthCount)
                    throw new IndexOutOfRangeException("Depth dimension mismatch");
                SetPosition();
                foreach (double t in value) _dataLayer.DataFile.WriteStream.Write(t);
            }
        }

        private void SetPosition()
        {
            var desiredPosition = _dataStartOffset + (_rowStep * _rowIndex) + (_columnStep * _columnIndex);
            _dataLayer.DataFile.BaseStream.Position = desiredPosition;
        }
    }
}
