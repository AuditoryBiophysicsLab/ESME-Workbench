using System;
using System.Collections.Generic;
using System.Text;

namespace NetCDF
{
    public class NcFile : NetCDF
    {
        private int ndims, nvars, natts, unlimdimid;
        private NcDimList dims;
        private NcVarList vars;
        private NcAttList atts;

#region Constructors
        public NcFile(string Filename)
        {
            NcResult status;
            int i, j;

            filename = Filename;
            status = NC_open(filename, 0, out ncid);
            if (status != NcResult.NC_NOERR)
                throw new ApplicationException(String.Format("Error constructing NcFile object for file \"{0}\".  NC_open returned {1}", Filename, status.ToString()));

            status = NC_inq(ncid, out ndims, out nvars, out natts, out unlimdimid);
            if (status != NcResult.NC_NOERR)
                throw new ApplicationException(String.Format("Error constructing NcFile object for file \"{0}\".  NC_inq returned {1}", Filename, status.ToString()));

            atts = new NcAttList(natts);
            for (j = 0; j < natts; j++)
                atts.Add(NcComponent.GetAttribute(ncid, j));

            dims = new NcDimList(ndims);
            // Get the list of dimensions
            for (i = 0; i < ndims; i++)
                dims.Add(NcComponent.GetDimension(ncid, i, (unlimdimid == i)));

            vars = new NcVarList(nvars);
            // Get the list of variables
            for (i = 0; i < nvars; i++)
                vars.Add(NcComponent.GetVariable(ncid, i, dims));
        }
        #endregion

        public void LoadAllData()
        {
            foreach (NcVar cur in vars)
                cur.ReadData();
        }

        public NcResult CheckYerself(int VarID, int X, int Y, out short[] results)
        {
            uint[] start = new uint[3];
            uint[] count = new uint[3];

            start[0] = 0;
            start[2] = (uint)X;
            start[1] = (uint)Y;
            count[0] = (uint)Variables[VarID].Dimensions["depth"].Size;
            count[1] = count[2] = 1;

            results = new short[Variables[VarID].Dimensions["depth"].Size];

            return nc_get_vara_short(NcID, VarID, start, count, results);
        }

        public int DimensionCount { get { return ndims; } }
        public int VariableCount { get { return nvars; } }
        public int AttributeCount { get { return natts; } }
        public NcDimList Dimensions { get { return dims; } }
        public NcVarList Variables { get { return vars; } }
        public NcAttList Attributes { get { return atts; } }
        public string Filename { get { return filename; } }
    }
}
