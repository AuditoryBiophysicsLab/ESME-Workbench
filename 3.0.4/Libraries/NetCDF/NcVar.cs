using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace NetCDF
{
    public abstract class NcVar : NcComponent
    {
        protected int[] DimIds, Dims, Strides;
        protected long size;
        protected long Elements;
        protected int Varid;
        readonly NcDimList _ncDims;
        readonly NcAttList _ncAtts;

        protected NcVar(int ncID, int variableID, IList<NcDim> dims)
            : base(ncID, variableID)
        {
            StringBuilder variableName;
            int attributeCount, dimCount;
            var dimIDs = new int[100];
            NcType variableType;

            NcInqVar(ncID, variableID, out variableName, out variableType, out dimCount, ref dimIDs, out attributeCount);

            Varid = variableID;
            DimIds = new int[dimCount];
            Dims = new int[dimCount];
            Strides = new int[dimCount];
            ComponentName = variableName.ToString();
            _ncDims = new NcDimList(dimCount);
            _ncAtts = new NcAttList(attributeCount);
            for (var i = 0; i < dimCount; i++)
                DimIds[i] = dimIDs[i];
            for (var i = 0; i < dimCount; i++)
            {
                _ncDims.Add(dims[DimIds[i]]);
                Dims[i] = (int)_ncDims[i].Size;
            }
            Elements = 1;
            for (var i = 0; i < dimCount; i++)
                Elements *= Dims[i];

            switch (dimCount)
            {
                case 1:
                    Strides[0] = 1;
                    break;
                case 2:
                    Strides[1] = 1;
                    Strides[0] = Dims[1];
                    break;
                default:
                    Strides[dimCount - 1] = 1;
                    for (var i = dimCount - 2; i >= 0; i--)
                        Strides[i] = Dims[i + 1] * Strides[i + 1];
                    break;
            }

            for (var i = 0; i < attributeCount; i++)
                _ncAtts.Add(GetAttribute(ncID, variableID, i));
        }

        public int[] DimensionIDs { get { return DimIds; } }
        public NcDimList Dimensions { get { return _ncDims; } }
        public NcAttList Attributes { get { return _ncAtts; } }
        public abstract void ReadData();

        /// <summary>
        /// Size of the variable, in bytes
        /// </summary>
        public long Size { get { return size * Elements; } }

        /// <summary>
        /// Total number of elements in the variable (all dimensions multiplied together)
        /// </summary>
        public long ElementCount { get { return Elements; } }

        public NcType NcType { get; protected set; }
        public virtual short GetShort(params int[] dimensions) { return 0; }
        public virtual float GetFloat(params int[] dimensions) { return 0.0f; }
        public virtual double Min { get; protected set; }
        public virtual double Max { get; protected set; }
    }

    public abstract class NcVarTyped<T> : NcVar
    {
        protected NcVarTyped(int ncid, int varID, IList<NcDim> dims)
            : base(ncid, varID, dims)
        {
        }

        public void ReleaseData()
        {
            Data = null;
        }

        public override void ReadData()
        {
            if (Data == null)
                Data = new T[Elements];
        }

        public T[] Data { get; protected set; }
        protected abstract void FindMinMax();

        public int GetIndex(params int[] dimensions)
        {
            return Dims.Select((t, i) => dimensions[i] * Strides[i]).Sum();
        }

        public T Element(params int[] dimensions)
        {
            if (Data == null)
                throw new ArgumentNullException("Data", "NcVarTyped<T>.Element(...) failed because data has not been loaded.  Call ReadData() method for this object first.");
            if (dimensions.Length != Dims.Length)
                throw new IndexOutOfRangeException(String.Format("NcVarTyped<T>.Element(...) exception.  Request was for {0} dimensions but variable has {1} dimensions", dimensions.Length, Dims.Length));
            for (var i = 0; i < Dims.Length; i++)
            {
                if (dimensions[i] >= Dims[i])
                    throw new IndexOutOfRangeException(String.Format("NcVarTyped<T>.Element(...) exception.  Dimension {0} requested index {1} but size of dimension {0} for variable is {2}", i, dimensions[i], Dims[i]));
                if (dimensions[i] < 0)
                    throw new IndexOutOfRangeException(String.Format("NcVarTyped<T>.Element(...) exception.  Dimension {0} requested index {1}.  Negative indices are not supported.", i, dimensions[i]));
            }
            return Data[GetIndex(dimensions)];
        }

        public override double Min { get; protected set; }
        public override double Max { get; protected set; }
    }

    public class NcVarDouble : NcVarTyped<double>
    {
        public NcVarDouble(int ncid, int varID, IList<NcDim> dims)
            : base(ncid, varID, dims)
        {
            size = 8;
            NcType = NcType.ncDouble;
        }

        protected override void FindMinMax()
        {
            Max = double.MinValue;
            Min = double.MaxValue;
            foreach (var cur in Data)
            {
                Min = Math.Min(cur, Min);
                Max = Math.Max(cur, Max);
            }
        }

        public override short GetShort(params int[] dimensions) { return (short)Element(dimensions); }
        public override float GetFloat(params int[] dimensions) { return (float)Element(dimensions); }

        public override void ReadData()
        {
            base.ReadData();
            nc_get_var_double(Ncid, Varid, Data);
            FindMinMax();
        }
    }

    public class NcVarFloat : NcVarTyped<float>
    {
        public NcVarFloat(int ncid, int varID, IList<NcDim> dims)
            : base(ncid, varID, dims)
        {
            size = 4;
            NcType = NcType.ncFloat;
        }

        protected override void FindMinMax()
        {
            Max = float.MinValue;
            Min = float.MaxValue;
            foreach (var cur in Data)
            {
                Min = Math.Min(cur, Min);
                Max = Math.Max(cur, Max);
            }
        }

        public override short GetShort(params int[] dimensions) { return (short)Element(dimensions); }
        public override float GetFloat(params int[] dimensions) { return Element(dimensions); }

        public override void ReadData()
        {
            base.ReadData();
            nc_get_var_float(Ncid, Varid, Data);
            FindMinMax();
        }
    }

    public class NcVarInt : NcVarTyped<int>
    {
        public NcVarInt(int ncid, int varID, IList<NcDim> dims)
            : base(ncid, varID, dims)
        {
            size = 4;
            NcType = NcType.ncInt;
        }

        protected override void FindMinMax()
        {
            Max = int.MinValue;
            Min = int.MaxValue;
            foreach (var cur in Data)
            {
                Min = Math.Min(cur, Min);
                Max = Math.Max(cur, Max);
            }
        }

        public override short GetShort(params int[] dimensions) { return (short)Element(dimensions); }
        public override float GetFloat(params int[] dimensions) { return Element(dimensions); }

        public override void ReadData()
        {
            base.ReadData();
            nc_get_var_int(Ncid, Varid, Data);
            FindMinMax();
        }
    }

    public class NcVarShort : NcVarTyped<short>
    {
        public NcVarShort(int ncid, int varID, IList<NcDim> dims)
            : base(ncid, varID, dims)
        {
            size = 2;
            NcType = NcType.ncShort;
        }

        protected override void FindMinMax()
        {
            Max = short.MinValue;
            Min = short.MaxValue;
            foreach (var cur in Data)
            {
                Min = Math.Min(cur, Min);
                Max = Math.Max(cur, Max);
            }
        }

        public override short GetShort(params int[] dimensions) { return Element(dimensions); }
        public override float GetFloat(params int[] dimensions) { return Element(dimensions); }

        public override void ReadData()
        {
            base.ReadData();
            nc_get_var_short(Ncid, Varid, Data);
            FindMinMax();
        }
    }

    public class NcVarByte : NcVarTyped<byte>
    {
        public NcVarByte(int ncid, int varID, IList<NcDim> dims)
            : base(ncid, varID, dims)
        {
            size = 1;
            NcType = NcType.ncByte;
        }

        protected override void FindMinMax()
        {
            Max = byte.MinValue;
            Min = byte.MaxValue;
            foreach (var cur in Data)
            {
                Min = Math.Min(cur, Min);
                Max = Math.Max(cur, Max);
            }
        }

        public override void ReadData()
        {
            base.ReadData();
            nc_get_var_byte(Ncid, Varid, Data);
            FindMinMax();
        }
    }

    public class NcVarChar : NcVarTyped<char>
    {
        public NcVarChar(int ncid, int varID, IList<NcDim> dims)
            : base(ncid, varID, dims)
        {
            size = 1;
            NcType = NcType.ncChar;
        }

        protected override void FindMinMax()
        {
            Max = char.MinValue;
            Min = char.MaxValue;
            foreach (var cur in Data)
            {
                Min = Math.Min(cur, Min);
                Max = Math.Max(cur, Max);
            }
        }

        public override void ReadData()
        {
            base.ReadData();
            nc_get_var_char(Ncid, Varid, Data);
            FindMinMax();
        }

        public override string ToString()
        {
            return new string(Data);
        }
    }
}
