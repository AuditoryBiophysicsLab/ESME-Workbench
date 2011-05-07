using System;
using System.Collections.Generic;
using System.Text;

namespace NetCDF
{
    public abstract class NcAtt : NcComponent
    {
        protected int varid;

        protected NcAtt(int NcID, int VariableID, string AttributeName)
            : base(NcID, AttributeName)
        {
            varid = VariableID;
        }

        protected NcAtt(int NcID, int VariableID, string AttributeName, int Length)
            : base(NcID, AttributeName)
        {
            varid = VariableID;
        }

        public virtual short GetShort(int Index) { return 0; }
        public virtual float GetFloat(int Index) { return 0.0f; }

        public int VarID { get { return varid; } }
    }

    public abstract class NcAttGeneric<T> : NcAtt
    {
        protected T[] values;
        NcType type;

        public NcAttGeneric(int NcId, int VariableID, string AttributeName, NcType Type)
            : base(NcId, VariableID, AttributeName)
        {
            type = Type;
        }

        public T[] Value { get { return values; } }
        protected int Length
        {
            get { return values.Length; }
            set { values = new T[value]; }
        }
    }

    public class NcAttString : NcAttGeneric<string>
    {
        private string strval;
        public NcAttString(int NcId, int VariableID, string AttributeName, string Value)
            : base(NcId, VariableID, AttributeName, NcType.ncChar)
        {
            strval = Value;
        }

        public NcAttString(int NcId, int VariableID, string AttributeName, int Length)
            : base(NcId, VariableID, AttributeName, NcType.ncChar)
        {
            StringBuilder outval = new StringBuilder(Length);
            NcResult status = NC_get_att_text(ncid, VariableID, AttributeName, ref outval);
            strval = outval.ToString(0, Length);
            //strval = outval.ToString(0, outval.Length);
        }

        public override short GetShort(int Index) { return short.Parse(strval.Substring(Index)); }
        public override float GetFloat(int Index) { return float.Parse(strval.Substring(Index)); }

        public new string Value { get { return strval; } }
        public override string ToString() { return string.Format("{0}: '{1}'", ComponentName, strval); }
    }

    public class NcAttInt : NcAttGeneric<int>
    {
        public NcAttInt(int NcId, int VariableID, string AttributeName, int Length)
            : base(NcId, VariableID, AttributeName, NcType.ncInt)
        {
            this.Length = Length;
            NcResult status = NC_get_att_int(ncid, VariableID, AttributeName, ref values);
        }

        public override short GetShort(int Index) { return (short)values[Index]; }
        public override float GetFloat(int Index) { return (float)values[Index]; }

        public override string ToString() { return string.Format("{0}: int[{1}]", ComponentName, values[0]); }
    }

    public class NcAttShort : NcAttGeneric<short>
    {
        public NcAttShort(int NcId, int VariableID, string AttributeName, int Length)
            : base(NcId, VariableID, AttributeName, NcType.ncShort)
        {
            this.Length = Length;
            NcResult status = NC_get_att_short(ncid, VariableID, AttributeName, ref values);
        }

        public override short GetShort(int Index) { return (short)values[Index]; }
        public override float GetFloat(int Index) { return (float)values[Index]; }

        public override string ToString() { return string.Format("{0}: short[{1}]", ComponentName, values[0]); }
    }

    public class NcAttFloat : NcAttGeneric<float>
    {
        public NcAttFloat(int NcId, int VariableID, string AttributeName, int Length)
            : base(NcId, VariableID, AttributeName, NcType.ncFloat)
        {
            this.Length = Length;
            NcResult status = NC_get_att_float(ncid, VariableID, AttributeName, ref values);
        }

        public override short GetShort(int Index) { return (short)values[Index]; }
        public override float GetFloat(int Index) { return (float)values[Index]; }

        public override string ToString() { return string.Format("{0}: float[{1}]", ComponentName, values[0]); }
    }

    public class NcAttDouble : NcAttGeneric<double>
    {
        public NcAttDouble(int NcId, int VariableID, string AttributeName, int Length)
            : base(NcId, VariableID, AttributeName, NcType.ncDouble)
        {
            this.Length = Length;
            NcResult status = NC_get_att_double(ncid, VariableID, AttributeName, ref values);
        }

        public override short GetShort(int Index) { return (short)values[Index]; }
        public override float GetFloat(int Index) { return (float)values[Index]; }

        public override string ToString() { return string.Format("{0}: double[{1}]", ComponentName, values[0]); }
    }
}

