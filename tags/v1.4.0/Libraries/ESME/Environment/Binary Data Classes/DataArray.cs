using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

namespace ESME.Environment
{
    public class DimensionStep
    {
        public int Stride_bytes { get; internal set; }
    }

    public class DataArray_DEPRECATED
    {
        internal const uint _magic = 0x109b168e;
        internal long _dataStartOffset = -1;
        private uint rowcount, colcount, depthcount;
        private float[,,] array;
        private DataFile dataFile;
        private bool dataChanged;
        private bool initialSave = true;
        private DataArraySourceInfo mSourceInfo = null;


        public IEnumerable<uint> DimensionOffsets(int DimensionNumber)
        {
            uint stepsize = 0;
            uint dimcount = 0;
            switch (DimensionNumber)
            {
                case 2: // depths
                    stepsize = sizeof(float);
                    dimcount = depthcount;
                    break;
                case 1: // rows
                    stepsize = sizeof(float) * depthcount;
                    dimcount = rowcount;
                    break;
                case 0: // columns
                    stepsize = sizeof(float) * depthcount * rowcount;
                    dimcount = colcount;
                    break;
            }
            for (uint i = 0; i < dimcount; i++)
                yield return dimcount * i;
        }

        public void SaveAsAscii(string FileName)
        {
            LoadAllData();
            using (StreamWriter sw = new StreamWriter(FileName))
            {
                for (int row = 0; row < rowcount; row++)
                {
                    for (int col = 0; col < colcount; col++)
                        for (int dep = 0; dep < depthcount; dep++)
                            sw.Write("{0} ", array[col, row, dep]);
                    sw.WriteLine();
                }
            }
        }

        public DataArray_DEPRECATED(uint Rows, uint Columns, uint Depths)
        {
            //System.Diagnostics.PerformanceCounter ramCounter = new System.Diagnostics.PerformanceCounter("Memory", "Available MBytes");
            //Console.WriteLine("Memory available before allocating data array: {0}MB", ramCounter.NextValue());
            //array = new float[Columns, Rows, Depths];
            //Console.WriteLine("Memory available after allocating data array: {0}MB", ramCounter.NextValue());
            rowcount = Rows;
            colcount = Columns;
            depthcount = Depths;
        }

        public DataArray_DEPRECATED(BinaryReader stream)
        {
            if (_magic != stream.ReadUInt32())
                throw new FormatException("DataArray: Invalid input file format.  Magic number not found.");
            rowcount = stream.ReadUInt32();
            colcount = stream.ReadUInt32();
            depthcount = stream.ReadUInt32();
            _dataStartOffset = (int)stream.BaseStream.Seek(0, SeekOrigin.Current);
            initialSave = false;
            stream.BaseStream.Seek(_dataStartOffset + (colcount * rowcount * depthcount * sizeof(float)), SeekOrigin.Begin);
        }

        public DataFile DataFile { set { dataFile = value; } }


        public DataArray_DEPRECATED(BinaryReader SourceStream, BinaryWriter DestStream, DataArray_DEPRECATED Source, DataAxis ColumnAxis, DataAxis RowAxis)
        {
            if (Source == null)
                throw new ApplicationException("DataArray: Cannot construct new DataArray from an existing array when the existing array is null");
            if (ColumnAxis == null)
                throw new ApplicationException("DataArray: Cannot construct new DataArray from an existing array when the column axis is null");
            if (RowAxis == null)
                throw new ApplicationException("DataArray: Cannot construct new DataArray from an existing array when the row axis is null");

            depthcount = Source.Depths;
            colcount = (uint)ColumnAxis.Length;
            rowcount = (uint)RowAxis.Length;
            mSourceInfo = new DataArraySourceInfo
            {
                Stream = SourceStream,
                DataArray = Source,
                StartColumn = (uint)ColumnAxis.Map[0].Index,
                StartRow = (uint)RowAxis.Map[0].Index,
                RowCount = rowcount
            };
        }

        public float[, ,] Data 
        { 
            set 
            {
                // TODO: Bounds checking on the array - does it have the right number of rows/cols/depths?
                array = value;
                initialSave = false;
            }
            get
            {
                if (array == null)
                    LoadAllData();
                return array;
            }
        }

        public void Clear()
        {
            array = null;
        }

        public void Save(BinaryWriter stream, DataAxis ColumnMap)
        {
            uint row, col, dep;

            if (array != null)
            {
                stream.Write(_magic);
                stream.Write(rowcount);
                stream.Write(colcount);
                stream.Write(depthcount);
                _dataStartOffset = (int)stream.Seek(0, SeekOrigin.Current);
                if (array != null)
                {
                    for (col = 0; col < colcount; col++)
                        for (row = 0; row < rowcount; row++)
                            for (dep = 0; dep < depthcount; dep++)
                                if (ColumnMap != null)
                                    stream.Write(array[ColumnMap.Map[col].Index, row, dep]);
                                else
                                    stream.Write(array[col, row, dep]);
                    initialSave = false;
                }
            }
            else if (initialSave)
            {
                stream.Write(_magic);
                stream.Write(rowcount);
                stream.Write(colcount);
                stream.Write(depthcount);
                int ArrayOffset = (int)stream.BaseStream.Position;
                if (mSourceInfo != null)
                {
                    for (col = 0; col < colcount; col++)
                        SetColumn(stream, col,
                            mSourceInfo.DataArray.GetRowRange(mSourceInfo.Stream,
                                mSourceInfo.StartColumn,
                                mSourceInfo.StartRow,
                                mSourceInfo.StartRow + mSourceInfo.RowCount - 1));
                }
                else
                {
                    for (int i = 0; i < rowcount * colcount * depthcount; i++)
                        stream.Write(float.NaN);
                    //stream.Seek(dataStartOffset, SeekOrigin.Begin);
                }
                _dataStartOffset = ArrayOffset;
                initialSave = false;
            }
        }

        private long SeekOffset(uint Column, uint Row)
        {
            if ((Column < 0) || (Column >= colcount))
                throw new IndexOutOfRangeException("DataArray.SeekTo: Column index out of range");
            if ((Row < 0) || (Row >= rowcount))
                throw new IndexOutOfRangeException("DataArray.SeekTo: Row index out of range");
            long offset = sizeof(float);
            offset *= depthcount;
            offset *= ((Column * rowcount) + Row);
            offset += _dataStartOffset;
            return offset;
        }

        public void LoadAllData()
        {
            if (dataFile != null)
            {
                LoadAllData(dataFile.ReadStream);
            }
            else
            {
                if (array == null)
                    array = new float[colcount, rowcount, depthcount];
            }
        }

        public void LoadAllData(BinaryReader reader)
        {
            int row, col, dep;

            if (array == null)
                array = new float[colcount, rowcount, depthcount];
            reader.BaseStream.Seek(SeekOffset(0, 0), SeekOrigin.Begin);
            if ((reader == null) || (reader.BaseStream.CanRead == false))
                throw new ApplicationException("DataArray: Attempt to call LoadAllData when the data file is closed or not set");
            for (col = 0; col < colcount; col++)
                for (row = 0; row < rowcount; row++)
                    for (dep = 0; dep < depthcount; dep++)
                        array[col, row, dep] = reader.ReadSingle();
        }

        public float[] GetDataPoint(BinaryReader reader, uint Column, uint Row)
        {
            if ((reader == null) || (reader.BaseStream.CanRead == false))
                throw new ApplicationException("DataArray: Attempt to call GetData when the data file is closed or not set");
            reader.BaseStream.Seek(SeekOffset(Column, Row), SeekOrigin.Begin);
            float[] retval = new float[depthcount];
            for (int i = 0; i < retval.Length; i++)
                retval[i] = reader.ReadSingle();
            return retval;
        }

        public float[,] GetRowRange(BinaryReader reader, uint Column, uint StartRow, uint EndRow)
        {
            if ((reader == null) || (reader.BaseStream.CanRead == false))
                throw new ApplicationException("DataArray: Attempt to call GetRowRange when the data file is closed or not set");
            if ((EndRow < StartRow) || (EndRow >= rowcount))
                throw new IndexOutOfRangeException("DataArray.GetRowRange: EndRow index out of range");
            reader.BaseStream.Seek(SeekOffset(Column, StartRow), SeekOrigin.Begin);
            float[,] retval = new float[EndRow - StartRow, depthcount];
            for (int i = 0; i < retval.GetLength(0); i++)
                for (int j = 0; j < retval.GetLength(1); j++)
                    retval[i, j] = dataFile.ReadStream.ReadSingle();
            return retval;
        }

        public float[,] GetColumn(BinaryReader reader, uint Column)
        {
            return GetRowRange(reader, Column, 0, rowcount - 1);
        }

        public uint Rows { get { return rowcount; } }
        public uint Columns { get { return colcount; } }
        public uint Depths { get { return depthcount; } }
        public bool DataChanged { get { return dataChanged; } }
        public float[] this[uint col, uint row] 
        { 
            get 
            {
                float[] temp;
                if (array != null)
                {
                    temp = new float[depthcount];
                    for (uint depth = 0; depth < depthcount; depth++)
                        temp[depth] = array[col, row, depth];
                    return temp;
                }
                else
                    throw new ApplicationException("DataArray: Array not loaded or initialized!");
            }
            set
            {
                if (array != null)
                {
                    if (value.Length > depthcount)
                        throw new ArrayTypeMismatchException("DataArray: Tried to store value with " + value.Length + " depth points, when the array can hold max of " + depthcount + " points.");
                    for (uint depth = 0; depth < depthcount; depth++)
                        array[col, row, depth] = value[depth];
                }
                else
                    throw new ApplicationException("DataArray: Array not loaded or initialized!");
                dataChanged = true;
            }
        }

        public void SetDataPoint(BinaryWriter stream, uint Column, uint Row, float[] DataValues)
        {
            if (DataValues.Length != depthcount)
                throw new ArrayTypeMismatchException("Number of depths being stored does not match size of DataLayer");

            if (_dataStartOffset != -1)
                stream.Seek((int)SeekOffset(Column, Row), SeekOrigin.Begin);

            for (uint dep = 0; dep < depthcount; dep++)
                stream.Write(DataValues[dep]);
        }

        public void SetColumnRange(BinaryWriter stream, uint RowIndex, uint StartColumn, float[,] DataValues)
        {
            if (DataValues.GetLength(1) != depthcount)
                throw new ArrayTypeMismatchException("Number of depths being stored does not match size of DataLayer");
            if (colcount < StartColumn + DataValues.GetLength(0))
                throw new ArrayTypeMismatchException("Number of columns being stored exceeds the size of the DataLayer");

            for (uint CurCol = 0; CurCol < DataValues.GetLength(0); CurCol++)
                for (uint dep = 0; dep < depthcount; dep++)
                {
                    stream.BaseStream.Position = SeekOffset(CurCol, RowIndex);
                    stream.Write(DataValues[CurCol, dep]);
                }
        }

        public void SetRowRange(BinaryWriter stream, uint ColumnIndex, uint StartRow, float[,] DataValues)
        {
            if (DataValues.GetLength(1) != depthcount)
                throw new ArrayTypeMismatchException("Number of depths being stored does not match size of DataLayer");
            if (rowcount < StartRow + DataValues.GetLength(0))
                throw new ArrayTypeMismatchException("Number of columns being stored exceeds the size of the DataLayer");

            if (_dataStartOffset != -1)
                stream.BaseStream.Position = SeekOffset(ColumnIndex, StartRow);
            for (uint CurRow = 0; CurRow < DataValues.GetLength(0); CurRow++)
                for (uint dep = 0; dep < depthcount; dep++)
                    stream.Write(DataValues[CurRow, dep]);
        }

        public void SetRow(BinaryWriter stream, uint RowIndex, float[,] DataValues)
        {
            SetColumnRange(stream, RowIndex, 0, DataValues);
        }

        public void SetColumn(BinaryWriter stream, uint ColIndex, float[,] DataValues)
        {
            SetRowRange(stream, ColIndex, 0, DataValues);
        }

        public override string ToString()
        {
            return string.Format("Longitudes: {0}\n " +
                                 "                       Latitudes: {1}\n " +
                                 "                          Depths: {2}",
                                 colcount, rowcount, depthcount);
        }
    }
}
