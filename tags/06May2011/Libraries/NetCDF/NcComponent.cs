using System;
using System.Collections.Generic;
using System.Text;

namespace NetCDF
{
    public abstract class NcComponent: NetCDF
    {
        protected int component_id;
        protected string component_name;

        public NcComponent(int NcId, int ComponentID, string ComponentName)
            : base(NcId)
        {
            component_id = ComponentID;
            component_name = ComponentName;
        }

        public NcComponent(int NcId, string ComponentName)
            : base(NcId)
        {
            component_name = ComponentName;
        }

        public NcComponent(int NcId, int ComponentID)
            : base(NcId)
        {
            component_id = ComponentID;
        }

        public int ComponentID { get { return component_id; } }
        public string ComponentName { get { return component_name; } }

        /// <summary>
        /// Get the global attribute with the specified AttributeID
        /// </summary>
        /// <param name="NcID">ID returned by NC_open()</param>
        /// <param name="AttributeID">Zero-based global attribute ID</param>
        /// <returns>NcAtt with the specified attribute properties</returns>
        public static NcAtt GetAttribute(int NcID, int AttributeID)
        {
            return NcComponent.GetAttribute(NcID, -1, AttributeID);
        }

        /// <summary>
        /// Retrieve the specified attribute ID for a given variable ID
        /// </summary>
        /// <param name="NcID">ID returned by NC_open()</param>
        /// <param name="VariableID">Zero-based global variable ID</param>
        /// <param name="AttributeID">Zero-based attribute ID specific to the given VariableID</param>
        /// <returns>NcAtt with the specified attribute properties</returns>
        public static NcAtt GetAttribute(int NcID, int VariableID, int AttributeID)
        {
            NcResult status;
            StringBuilder curatt;
            NcType atttype;
            int curlen;

            status = NC_inq_attname(NcID, VariableID, AttributeID, out curatt);
            if (status != NcResult.NC_NOERR)
                throw new ApplicationException(String.Format("Error constructing NcAtt for VariableID {0} and AttributeID {1}. NC_inq_attname returned {2}", VariableID, AttributeID, status.ToString()));
            status = NC_inq_att(NcID, VariableID, curatt.ToString(), out atttype, out curlen);
            if (status != NcResult.NC_NOERR)
                throw new ApplicationException(String.Format("Error constructing NcAtt for VariableID {0} and Attribute {1}. NC_inq_att returned {2}", VariableID, curatt.ToString(), status.ToString()));

            switch (atttype)
            {
                case NcType.ncByte:
                    throw new Exception("ncByte attributes not implemented yet");
                case NcType.ncChar:
                    return new NcAttString(NcID, VariableID, curatt.ToString(), curlen);
                case NcType.ncDouble:
                    return new NcAttDouble(NcID, VariableID, curatt.ToString(), curlen);
                case NcType.ncFloat:
                    return new NcAttFloat(NcID, VariableID, curatt.ToString(), curlen);
                case NcType.ncInt:
                    return new NcAttInt(NcID, VariableID, curatt.ToString(), curlen);
                case NcType.ncNat:
                    return new NcAttString(NcID, VariableID, curatt.ToString(), "");
                case NcType.ncShort:
                    return new NcAttShort(NcID, VariableID, curatt.ToString(), curlen);
                default:
                    throw new ApplicationException("Unknown type " + atttype.ToString() + " passed to NcAttFactory.Create()");
            }
        }

        public static NcDim GetDimension(int NcID, int DimensionID, bool IsUnlimited)
        {
            return new NcDim(NcID, DimensionID, IsUnlimited);
        }

        public static NcVar GetVariable(int NcID, int VariableID, NcDimList Dims)
        {
            NcType VariableType;

            NC_inq_vartype(NcID, VariableID, out VariableType);
            switch (VariableType)
            {
                case NcType.ncDouble:
                    return new NcVarDouble(NcID, VariableID, Dims);
                case NcType.ncShort:
                    return new NcVarShort(NcID, VariableID, Dims);
                case NcType.ncFloat:
                    return new NcVarFloat(NcID, VariableID, Dims);
                case NcType.ncInt:
                    return new NcVarInt(NcID, VariableID, Dims);
                case NcType.ncByte:
                    return new NcVarByte(NcID, VariableID, Dims);
                case NcType.ncChar:
                    return new NcVarChar(NcID, VariableID, Dims);
                case NcType.ncNat:
                default:
                    throw new Exception(VariableType.ToString() + " factory not yet implemented!");
            }
        }
    }

    public class NcList<T> : List<T> where T: NcComponent
    {
        public NcList() : base() { }
        public NcList(int capacity) : base(capacity) { }

        public T this[string index]
        {
            get
            {
                foreach (T cur in this)
                    if (cur.ComponentName == index)
                        return cur;
                return null;
            }
        }
    }

    public class NcDimList : NcList<NcDim>
    {
        public NcDimList() : base() { }
        public NcDimList(int capacity) : base(capacity) { }
    }

    public class NcAttList : NcList<NcAtt>
    {
        public NcAttList() : base() { }
        public NcAttList(int capacity) : base(capacity) { }
    }

    public class NcVarList : NcList<NcVar>
    {
        public NcVarList() : base() { }
        public NcVarList(int capacity) : base(capacity) { }
    }
}
