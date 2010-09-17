using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using HRC.Navigation;

namespace ESME.Environment
{
    public class DataPoint : Datum
    {
        internal DataPoint(DataLayer DataLayer, long RowIndex, long ColumnIndex) : base(DataLayer, RowIndex, ColumnIndex) { }
        public DataPoint(DataLayer DataLayer) : base(DataLayer, 0, 0) { }

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
                float[] retval = new float[_dataLayer.DepthCount];
                for (int i = 0; i < retval.Length; i++)
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
                for (int i = 0; i < value.Length; i++)
                    _dataLayer.DataFile.WriteStream.Write(value[i]);
            }
        }

        private void SetPosition()
        {
            long DesiredPosition = _dataStartOffset + (_rowStep * _rowIndex) + (_columnStep * _columnIndex);
            if (_dataLayer.DataFile.BaseStream.Position != DesiredPosition)
                _dataLayer.DataFile.BaseStream.Position = DesiredPosition;
        }
    }
}
