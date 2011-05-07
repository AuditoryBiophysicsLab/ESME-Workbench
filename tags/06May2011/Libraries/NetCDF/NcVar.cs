using System;
using System.Collections.Generic;
using System.Text;

namespace NetCDF
{
    public abstract class NcVar : NcComponent
    {
        protected NcType type;
        protected int[] dim_ids, dims, strides;
        protected long size;
        protected long elements;
        protected int varid;
        NcDimList nc_dims;
        NcAttList nc_atts;

        public NcVar(int NcID, int VariableID, NcDimList Dims)
            : base(NcID, VariableID)
        {
            StringBuilder VariableName;
            int AttributeCount, DimCount;
            int[] DimIDs = new int[100];
            int[] dimlist = new int[100];
            NcType VariableType;

            NC_inq_var(NcID, VariableID, out VariableName, out VariableType, out DimCount, ref DimIDs, out AttributeCount);

            varid = VariableID;
            dim_ids = new int[DimCount];
            dims = new int[DimCount];
            strides = new int[DimCount];
            component_name = VariableName.ToString();
            nc_dims = new NcDimList(DimCount);
            nc_atts = new NcAttList(AttributeCount);
            for (int i = 0; i < DimCount; i++)
                dim_ids[i] = DimIDs[i];
            for (int i = 0; i < DimCount; i++)
            {
                nc_dims.Add(Dims[dim_ids[i]]);
                dims[i] = (int)nc_dims[i].Size;
            }
            elements = 1;
            for (int i = 0; i < DimCount; i++)
                elements *= dims[i];

            switch (DimCount)
            {
                case 1:
                    strides[0] = 1;
                    break;
                case 2:
                    strides[1] = 1;
                    strides[0] = dims[1];
                    break;
                default:
                    strides[DimCount - 1] = 1;
                    for (int i = DimCount - 2; i >= 0; i--)
                        strides[i] = (int)dims[i + 1] * strides[i + 1];
                    break;
            }

            for (int i = 0; i < AttributeCount; i++)
                nc_atts.Add(NcComponent.GetAttribute(NcID, VariableID, i));
        }

        public int[] DimensionIDs { get { return dim_ids; } }
        public NcDimList Dimensions { get { return nc_dims; } }
        public NcAttList Attributes { get { return nc_atts; } }
        public abstract void ReadData();

        /// <summary>
        /// Size of the variable, in bytes
        /// </summary>
        public long Size { get { return size * elements; } }

        /// <summary>
        /// Total number of elements in the variable (all dimensions multiplied together)
        /// </summary>
        public long ElementCount { get { return elements; } }

        public NcType Type { get { return type; } }
        public virtual short GetShort(params int[] dimensions) { return 0; }
        public virtual float GetFloat(params int[] dimensions) { return 0.0f; }
        public virtual double Min { get { return 0.0; } }
        public virtual double Max { get { return 0.0; } }
    }

    public abstract class NcVarTyped<T> : NcVar
    {
        protected T[] data;
        protected double min, max;

        public NcVarTyped(int NCID, int VarID, NcDimList Dims)
            : base(NCID, VarID, Dims)
        {
        }

        public void ReleaseData()
        {
            data = null;
        }

        public override void ReadData()
        {
            if (data == null)
                data = new T[elements];
        }

        public T[] Data { get { return data; } }
        protected abstract void FindMinMax();

        public int GetIndex(params int[] dimensions)
        {
            int offset = 0;
            for (int i = 0; i < dims.Length; i++)
                offset += dimensions[i] * strides[i];
            return offset;
        }

        public T Element(params int[] dimensions)
        {
            if (data == null)
                throw new ArgumentNullException("NcVarTyped<T>.Element(...) failed because data has not been loaded.  Call ReadData() method for this object first.");
            if (dimensions.Length != dims.Length)
                throw new IndexOutOfRangeException(String.Format("NcVarTyped<T>.Element(...) exception.  Request was for {0} dimensions but variable has {1} dimensions", dimensions.Length, dims.Length));
            for (int i = 0; i < dims.Length; i++)
            {
                if (dimensions[i] >= dims[i])
                    throw new IndexOutOfRangeException(String.Format("NcVarTyped<T>.Element(...) exception.  Dimension {0} requested index {1} but size of dimension {0} for variable is {2}", i, dimensions[i], dims[i]));
                if (dimensions[i] < 0)
                    throw new IndexOutOfRangeException(String.Format("NcVarTyped<T>.Element(...) exception.  Dimension {0} requested index {1}.  Negative indices are not supported.", i, dimensions[i], dims[i]));
            }
            return data[GetIndex(dimensions)];
        }
        public override double Min { get { return min; } }
        public override double Max { get { return max; } }
    }

    public class NcVarDouble : NcVarTyped<double>
    {
        public NcVarDouble(int NCID, int VarID, NcDimList Dims)
            : base(NCID, VarID, Dims)
        {
            size = 8;
            type = NcType.ncDouble;
        }

        protected override void FindMinMax()
        {
            max = double.MinValue;
            min = double.MaxValue;
            foreach (double cur in data)
            {
                min = Math.Min(cur, min);
                max = Math.Max(cur, max);
            }
        }

        public override short GetShort(params int[] dimensions) { return (short)Element(dimensions); }
        public override float GetFloat(params int[] dimensions) { return (float)Element(dimensions); }

        public override void ReadData()
        {
            base.ReadData();
            NcResult result = nc_get_var_double(ncid, varid, data);
            FindMinMax();
        }
    }

    public class NcVarFloat : NcVarTyped<float>
    {
        public NcVarFloat(int NCID, int VarID, NcDimList Dims)
            : base(NCID, VarID, Dims)
        {
            size = 4;
            type = NcType.ncFloat;
        }

        protected override void FindMinMax()
        {
            max = float.MinValue;
            min = float.MaxValue;
            foreach (float cur in data)
            {
                min = Math.Min(cur, min);
                max = Math.Max(cur, max);
            }
        }

        public override short GetShort(params int[] dimensions) { return (short)Element(dimensions); }
        public override float GetFloat(params int[] dimensions) { return (float)Element(dimensions); }

        public override void ReadData()
        {
            base.ReadData();
            nc_get_var_float(ncid, varid, data);
            FindMinMax();
        }
    }

    public class NcVarInt : NcVarTyped<int>
    {
        public NcVarInt(int NCID, int VarID, NcDimList Dims)
            : base(NCID, VarID, Dims)
        {
            size = 4;
            type = NcType.ncInt;
        }

        protected override void FindMinMax()
        {
            max = int.MinValue;
            min = int.MaxValue;
            foreach (int cur in data)
            {
                min = Math.Min(cur, min);
                max = Math.Max(cur, max);
            }
        }

        public override short GetShort(params int[] dimensions) { return (short)Element(dimensions); }
        public override float GetFloat(params int[] dimensions) { return (float)Element(dimensions); }

        public override void ReadData()
        {
            base.ReadData();
            nc_get_var_int(ncid, varid, data);
            FindMinMax();
        }
    }

    public class NcVarShort : NcVarTyped<short>
    {
        public NcVarShort(int NCID, int VarID, NcDimList Dims)
            : base(NCID, VarID, Dims)
        {
            size = 2;
            type = NcType.ncShort;
        }

        protected override void FindMinMax()
        {
            max = short.MinValue;
            min = short.MaxValue;
            foreach (short cur in data)
            {
                min = Math.Min(cur, min);
                max = Math.Max(cur, max);
            }
        }

        public override short GetShort(params int[] dimensions) { return (short)Element(dimensions); }
        public override float GetFloat(params int[] dimensions) { return (float)Element(dimensions); }

        public override void ReadData()
        {
            base.ReadData();
            nc_get_var_short(ncid, varid, data);
            FindMinMax();
        }
    }

    public class NcVarByte : NcVarTyped<byte>
    {
        public NcVarByte(int NCID, int VarID, NcDimList Dims)
            : base(NCID, VarID, Dims)
        {
            size = 1;
            type = NcType.ncByte;
        }

        protected override void FindMinMax()
        {
            max = byte.MinValue;
            min = byte.MaxValue;
            foreach (byte cur in data)
            {
                min = Math.Min(cur, min);
                max = Math.Max(cur, max);
            }
        }

        public override void ReadData()
        {
            base.ReadData();
            nc_get_var_byte(ncid, varid, data);
            FindMinMax();
        }
    }

    public class NcVarChar : NcVarTyped<char>
    {
        public NcVarChar(int NCID, int VarID, NcDimList Dims)
            : base(NCID, VarID, Dims)
        {
            size = 1;
            type = NcType.ncChar;
        }

        protected override void FindMinMax()
        {
            max = char.MinValue;
            min = char.MaxValue;
            foreach (char cur in data)
            {
                min = (char)Math.Min(cur, min);
                max = (char)Math.Max(cur, max);
            }
        }

        public override void ReadData()
        {
            base.ReadData();
            nc_get_var_char(ncid, varid, data);
            FindMinMax();
        }

        public override string ToString()
        {
            return new string(data);
        }
    }
}
