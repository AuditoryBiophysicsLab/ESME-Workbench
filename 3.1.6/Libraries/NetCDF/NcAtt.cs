using System.Text;

namespace NetCDF
{
    public abstract class NcAtt : NcComponent
    {
        protected NcAtt(int ncID, int variableID, string attributeName)
            : base(ncID, attributeName)
        {
            VarID = variableID;
        }

        public virtual short GetShort(int index) { return 0; }
        public virtual float GetFloat(int index) { return 0.0f; }

        public int VarID { get; protected set; }
    }

    public abstract class NcAttGeneric<T> : NcAtt
    {
        protected T[] Values;

        protected NcAttGeneric(int ncId, int variableID, string attributeName, NcType type)
            : base(ncId, variableID, attributeName) { NcType = type; }

        public T[] Value { get { return Values; } }
        protected NcType NcType { get; set; }
        protected int Length
        {
            get { return Values.Length; }
            set { Values = new T[value]; }
        }
    }

    public class NcAttString : NcAttGeneric<string>
    {
        private readonly string _strval;
        public NcAttString(int ncId, int variableID, string attributeName, string value)
            : base(ncId, variableID, attributeName, NcType.ncChar)
        {
            _strval = value;
        }

        public NcAttString(int ncId, int variableID, string attributeName, int length)
            : base(ncId, variableID, attributeName, NcType.ncChar)
        {
            var outval = new StringBuilder(length);
            NcGetAttText(Ncid, variableID, attributeName, ref outval);
            _strval = outval.ToString(0, length);
            //strval = outval.ToString(0, outval.Length);
        }

        public override short GetShort(int index) { return short.Parse(_strval.Substring(index)); }
        public override float GetFloat(int index) { return float.Parse(_strval.Substring(index)); }

        public new string Value { get { return _strval; } }
        public override string ToString() { return string.Format("{0}: '{1}'", ComponentName, _strval); }
    }

    public class NcAttInt : NcAttGeneric<int>
    {
        public NcAttInt(int ncId, int variableID, string attributeName, int length)
            : base(ncId, variableID, attributeName, NcType.ncInt)
        {
            Length = length;
            NcGetAttInt(Ncid, variableID, attributeName, ref Values);
        }

        public override short GetShort(int index) { return (short)Values[index]; }
        public override float GetFloat(int index) { return Values[index]; }

        public override string ToString() { return string.Format("{0}: int[{1}]", ComponentName, Values[0]); }
    }

    public class NcAttShort : NcAttGeneric<short>
    {
        public NcAttShort(int ncId, int variableID, string attributeName, int length)
            : base(ncId, variableID, attributeName, NcType.ncShort)
        {
            Length = length;
            NcGetAttShort(Ncid, variableID, attributeName, ref Values);
        }

        public override short GetShort(int index) { return Values[index]; }
        public override float GetFloat(int index) { return Values[index]; }

        public override string ToString() { return string.Format("{0}: short[{1}]", ComponentName, Values[0]); }
    }

    public class NcAttFloat : NcAttGeneric<float>
    {
        public NcAttFloat(int ncId, int variableID, string attributeName, int length)
            : base(ncId, variableID, attributeName, NcType.ncFloat)
        {
            Length = length;
            NcGetAttFloat(Ncid, variableID, attributeName, ref Values);
        }

        public override short GetShort(int index) { return (short)Values[index]; }
        public override float GetFloat(int index) { return Values[index]; }

        public override string ToString() { return string.Format("{0}: float[{1}]", ComponentName, Values[0]); }
    }

    public class NcAttDouble : NcAttGeneric<double>
    {
        public NcAttDouble(int ncId, int variableID, string attributeName, int length)
            : base(ncId, variableID, attributeName, NcType.ncDouble)
        {
            Length = length;
            NcGetAttDouble(Ncid, variableID, attributeName, ref Values);
        }

        public override short GetShort(int index) { return (short)Values[index]; }
        public override float GetFloat(int index) { return (float)Values[index]; }

        public override string ToString() { return string.Format("{0}: double[{1}]", ComponentName, Values[0]); }
    }
}

