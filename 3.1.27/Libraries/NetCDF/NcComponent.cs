using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace NetCDF
{
    public abstract class NcComponent: NetCdf
    {
        protected NcComponent(int ncId, int componentID, string componentName)
            : base(ncId)
        {
            ComponentID = componentID;
            ComponentName = componentName;
        }

        protected NcComponent(int ncId, string componentName)
            : base(ncId)
        {
            ComponentName = componentName;
        }

        protected NcComponent(int ncId, int componentID)
            : base(ncId)
        {
            ComponentID = componentID;
        }

        public int ComponentID { get; protected set; }
        public string ComponentName { get; protected set; }

        /// <summary>
        /// Get the global attribute with the specified AttributeID
        /// </summary>
        /// <param name="ncID">ID returned by NC_open()</param>
        /// <param name="attributeID">Zero-based global attribute ID</param>
        /// <returns>NcAtt with the specified attribute properties</returns>
        public static NcAtt GetAttribute(int ncID, int attributeID)
        {
            return GetAttribute(ncID, -1, attributeID);
        }

        /// <summary>
        /// Retrieve the specified attribute ID for a given variable ID
        /// </summary>
        /// <param name="ncID">ID returned by NC_open()</param>
        /// <param name="variableID">Zero-based global variable ID</param>
        /// <param name="attributeID">Zero-based attribute ID specific to the given VariableID</param>
        /// <returns>NcAtt with the specified attribute properties</returns>
        public static NcAtt GetAttribute(int ncID, int variableID, int attributeID)
        {
            StringBuilder curatt;
            NcType atttype;
            int curlen;

            var status = NcInqAttname(ncID, variableID, attributeID, out curatt);
            if (status != NcResult.NC_NOERR)
                throw new ApplicationException(String.Format("Error constructing NcAtt for VariableID {0} and AttributeID {1}. NC_inq_attname returned {2}", variableID, attributeID, status.ToString()));
            status = NcInqAtt(ncID, variableID, curatt.ToString(), out atttype, out curlen);
            if (status != NcResult.NC_NOERR)
                throw new ApplicationException(String.Format("Error constructing NcAtt for VariableID {0} and Attribute {1}. NC_inq_att returned {2}", variableID, curatt, status.ToString()));

            switch (atttype)
            {
                case NcType.ncByte:
                    throw new Exception("ncByte attributes not implemented yet");
                case NcType.ncChar:
                    return new NcAttString(ncID, variableID, curatt.ToString(), curlen);
                case NcType.ncDouble:
                    return new NcAttDouble(ncID, variableID, curatt.ToString(), curlen);
                case NcType.ncFloat:
                    return new NcAttFloat(ncID, variableID, curatt.ToString(), curlen);
                case NcType.ncInt:
                    return new NcAttInt(ncID, variableID, curatt.ToString(), curlen);
                case NcType.ncNat:
                    return new NcAttString(ncID, variableID, curatt.ToString(), "");
                case NcType.ncShort:
                    return new NcAttShort(ncID, variableID, curatt.ToString(), curlen);
                default:
                    throw new ApplicationException("Unknown type " + atttype.ToString() + " passed to NcAttFactory.Create()");
            }
        }

        public static NcDim GetDimension(int ncID, int dimensionID, bool isUnlimited)
        {
            return new NcDim(ncID, dimensionID, isUnlimited);
        }

        public static NcVar GetVariable(int ncID, int variableID, NcDimList dims)
        {
            NcType variableType;

            NcInqVartype(ncID, variableID, out variableType);
            switch (variableType)
            {
                case NcType.ncDouble:
                    return new NcVarDouble(ncID, variableID, dims);
                case NcType.ncShort:
                    return new NcVarShort(ncID, variableID, dims);
                case NcType.ncFloat:
                    return new NcVarFloat(ncID, variableID, dims);
                case NcType.ncInt:
                    return new NcVarInt(ncID, variableID, dims);
                case NcType.ncByte:
                    return new NcVarByte(ncID, variableID, dims);
                case NcType.ncChar:
                    return new NcVarChar(ncID, variableID, dims);
                default:
                    throw new Exception(variableType.ToString() + " factory not yet implemented!");
            }
        }
    }

    public class NcList<T> : List<T> where T: NcComponent
    {
        public NcList() { }
        public NcList(int capacity) : base(capacity) { }

        public T this[string index] { get { return this.FirstOrDefault(cur => cur.ComponentName == index); } }
    }

    public class NcDimList : NcList<NcDim>
    {
        public NcDimList() { }
        public NcDimList(int capacity) : base(capacity) { }
    }

    public class NcAttList : NcList<NcAtt>
    {
        public NcAttList() { }
        public NcAttList(int capacity) : base(capacity) { }
    }

    public class NcVarList : NcList<NcVar>
    {
        public NcVarList() { }
        public NcVarList(int capacity) : base(capacity) { }
    }
}
